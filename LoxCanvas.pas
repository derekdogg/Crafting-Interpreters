unit LoxCanvas;

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics, Classes, Chunk_Types;

type
  // Fired by the game canvas when a recognised key is pressed/released
  // while the canvas has focus. KeyName uses the same vocabulary as
  // the keyHeld() native: 'left','right','up','down','space','enter',
  // 'escape','a'..'z'. Empty string is never sent.
  TGameKeyEvent = procedure(Sender: TObject; const KeyName: string) of object;

  // Fired by the game canvas on mouse activity. Button is 0=left, 1=right,
  // 2=middle. LX/LY are in LOGICAL coordinates (0..319 / 0..239), with the
  // host-to-logical scale and letterbox offset already applied and clamped
  // to the visible playfield. For mouse-move, Button is ignored (-1).
  TGameMouseEvent = procedure(Sender: TObject; Button: Integer;
    LX, LY: Integer) of object;

  // Runtime-created control that hosts the software-rendered game frame.
  // Owns its own keyboard input (focusable, consumes game keys) and
  // its own repaint fallback (re-blits the last presented frame on
  // WM_PAINT when the game loop is idle). Not registered with the IDE
  // component palette — instantiate from code via InitCanvas().
  TLoxGameCanvas = class(TCustomControl)
  private
    FOnGameKeyDown: TGameKeyEvent;
    FOnGameKeyUp: TGameKeyEvent;
    FOnGameMouseDown: TGameMouseEvent;
    FOnGameMouseUp: TGameMouseEvent;
    FOnGameMouseMove: TGameMouseEvent;
    function MapVKToName(Key: Word): string;
    // Translate a host client-coord point to logical (320x240) space,
    // inverting the present-time integer scale + letterbox offset.
    // Returns clamped logical coords; LX is in [0..LOGICAL_W-1] and
    // LY is in [0..LOGICAL_H-1].
    procedure HostToLogical(HX, HY: Integer; out LX, LY: Integer);
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property OnGameKeyDown: TGameKeyEvent read FOnGameKeyDown write FOnGameKeyDown;
    property OnGameKeyUp: TGameKeyEvent read FOnGameKeyUp write FOnGameKeyUp;
    property OnGameMouseDown: TGameMouseEvent
      read FOnGameMouseDown write FOnGameMouseDown;
    property OnGameMouseUp: TGameMouseEvent
      read FOnGameMouseUp write FOnGameMouseUp;
    property OnGameMouseMove: TGameMouseEvent
      read FOnGameMouseMove write FOnGameMouseMove;
  end;

procedure InitCanvas(AParent: TWinControl);
procedure FreeCanvas;
procedure RegisterCanvasNatives;
procedure SetKeyState(const KeyName: string; Down: Boolean);
procedure ClearAllKeyState;
function GameCanvas: TLoxGameCanvas;

implementation

{$POINTERMATH ON}

uses
  SysUtils, Types, Generics.Collections, Math, IOUtils, Vcl.Imaging.pngimage;

type
  TRGBQuadRec = packed record
    rgbBlue, rgbGreen, rgbRed, rgbReserved: Byte;
  end;
  TRGBQuadArray = array[0..MaxInt div SizeOf(TRGBQuadRec) - 1] of TRGBQuadRec;
  PRGBQuadArray = ^TRGBQuadArray;

  TTilemap = class
    Cols, Rows: Integer;
    TileW, TileH: Integer;
    Tiles: array of Integer;
    constructor Create(ACols, ARows, ATileW, ATileH: Integer);
  end;

const
  SRCCOPY = $00CC0020;
  COLORONCOLOR = 3;  // StretchBlt mode: nearest-neighbor for color bitmaps

  // Fixed logical resolution. All Lox draw calls operate in this
  // coordinate space. At present() time the back buffer is scaled by
  // the largest integer factor that fits the host control, centered,
  // with black letterbox/pillarbox bars filling any remainder.
  LOGICAL_W = 320;
  LOGICAL_H = 240;

function SetStretchBltMode(DC: HDC; iStretchMode: Integer): Integer; stdcall;
  external 'gdi32.dll' name 'SetStretchBltMode';
function StretchBlt(DestDC: HDC; X, Y, W, H: Integer; SrcDC: HDC;
  XSrc, YSrc, WSrc, HSrc: Integer; Rop: DWORD): LongBool; stdcall;
  external 'gdi32.dll' name 'StretchBlt';
function BitBlt(DestDC: HDC; X, Y, W, H: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; Rop: DWORD): LongBool; stdcall;
  external 'gdi32.dll' name 'BitBlt';
function PatBlt(DC: HDC; X, Y, W, H: Integer; Rop: DWORD): LongBool; stdcall;
  external 'gdi32.dll' name 'PatBlt';

const
  BLACKNESS = $00000042;  // PatBlt: fill with black

// DwmFlush blocks until the Desktop Window Manager has finished compositing
// queued updates for the next refresh cycle. Used after present() to pace
// frames with the monitor refresh rate. Returns S_OK on success; we ignore
// the return code since failure (e.g. DWM disabled) is non-fatal.
function DwmFlush: Integer; stdcall;
  external 'dwmapi.dll' name 'DwmFlush';

var
  FGameSurface: TLoxGameCanvas;
  FBackBuffer: TBitmap;
  FFrontBuffer: TBitmap;   // presented frame â€” never partially drawn
  FPresentBuffer: TBitmap; // client-sized staging buffer for one-shot blits
  FRenderTarget: TBitmap;  // current draw target (FBackBuffer or a surface)
  FRenderTargetId: Integer; // -1 = back buffer, >= 0 = surface index
  FCurrentPixel: Cardinal;  // precomputed BGRA pixel for ScanLine writes
  FCameraX: Integer;
  FCameraY: Integer;
  FSprites: TObjectList<TBitmap>;
  FSpriteFreelist: TList<Integer>;
  FSurfaces: TObjectList<TBitmap>;
  FSurfaceFreelist: TList<Integer>;
  FTilemaps: TObjectList<TTilemap>;
  FHasFrontFrame: Boolean;  // true once present() has been called at least once
  FClipX1, FClipY1, FClipX2, FClipY2: Integer;  // clip rect (inclusive x1,y1 to exclusive x2,y2)
  FClipActive: Boolean;  // true when user has set a clip rect
  FPalette: array[0..255] of Cardinal;   // char -> BGRA color
  FPaletteUsed: array[0..255] of Boolean;
  FKeysHeld: TDictionary<string, Boolean>;
  // Latest known mouse position in LOGICAL pixel coords (clamped to the
  // 320x240 playfield) and held state for the three primary buttons.
  // Updated by TLoxGameCanvas.MouseMove/Down/Up and queried by the
  // mouseX/mouseY/mouseDown natives.
  FMouseX: Integer;
  FMouseY: Integer;
  FMouseBtn: array[0..2] of Boolean;

procedure ResetClipToTarget; forward;

procedure EnsurePresentBuffer(cw, ch: Integer);
begin
  if (cw <= 0) or (ch <= 0) then Exit;
  if FPresentBuffer = nil then
  begin
    FPresentBuffer := TBitmap.Create;
    FPresentBuffer.PixelFormat := pf32bit;
  end;
  if (FPresentBuffer.Width <> cw) or (FPresentBuffer.Height <> ch) then
    FPresentBuffer.SetSize(cw, ch);
end;

procedure EnsureBackBuffer;
begin
  // Back buffer is always the fixed logical resolution. We never resize
  // it to match the host control — that's what the present() scale step
  // is for. This guarantees pixel-perfect art and stable fill cost.
  if FBackBuffer = nil then
  begin
    FBackBuffer := TBitmap.Create;
    FBackBuffer.PixelFormat := pf32bit;
  end;
  if (FBackBuffer.Width <> LOGICAL_W) or (FBackBuffer.Height <> LOGICAL_H) then
  begin
    FBackBuffer.SetSize(LOGICAL_W, LOGICAL_H);
    FBackBuffer.Canvas.Brush.Color := clBlack;
    FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  end;
  if FRenderTargetId < 0 then
    FRenderTarget := FBackBuffer;
  ResetClipToTarget;
end;

procedure ResetClipToTarget;
begin
  if not FClipActive then
  begin
    FClipX1 := 0;
    FClipY1 := 0;
    FClipX2 := FRenderTarget.Width;
    FClipY2 := FRenderTarget.Height;
  end;
end;

// PresentScaled draws FFrontBuffer (always LOGICAL_W x LOGICAL_H) onto the
// supplied DC, integer-scaled to fit the host control, centered, with
// black letterbox/pillarbox bars on any leftover area.
//
// Scale rule: the largest positive integer N such that N*LOGICAL_W <= cw
// AND N*LOGICAL_H <= ch. If the host is smaller than the logical size on
// any axis, we still use scale=1 and clip — the result is acceptable for
// a tiny window and avoids the visual disaster of fractional scaling.
procedure PresentScaled(DC: HDC; cw, ch: Integer);
var
  scale, dw, dh, dx, dy: Integer;
  stageDC: HDC;
begin
  if (FFrontBuffer = nil) or (cw <= 0) or (ch <= 0) then Exit;
  EnsurePresentBuffer(cw, ch);
  if FPresentBuffer = nil then Exit;
  scale := cw div LOGICAL_W;
  if (ch div LOGICAL_H) < scale then scale := ch div LOGICAL_H;
  if scale < 1 then scale := 1;
  dw := LOGICAL_W * scale;
  dh := LOGICAL_H * scale;
  dx := (cw - dw) div 2;
  dy := (ch - dh) div 2;
  stageDC := FPresentBuffer.Canvas.Handle;
  // Compose the full client image off-screen, then copy it in one blit.
  PatBlt(stageDC, 0, 0, cw, ch, BLACKNESS);
  SetStretchBltMode(stageDC, COLORONCOLOR);
  StretchBlt(stageDC, dx, dy, dw, dh,
             FFrontBuffer.Canvas.Handle, 0, 0, LOGICAL_W, LOGICAL_H, SRCCOPY);
  BitBlt(DC, 0, 0, cw, ch, stageDC, 0, 0, SRCCOPY);
end;

{ TLoxGameCanvas }

constructor TLoxGameCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Opaque: tells VCL we paint every pixel, so it skips background prep.
  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse];
  TabStop := True;          // allow keyboard focus / tab traversal
  DoubleBuffered := False;  // we manage our own buffering
end;

function TLoxGameCanvas.MapVKToName(Key: Word): string;
begin
  case Key of
    VK_LEFT:   Result := 'left';
    VK_RIGHT:  Result := 'right';
    VK_UP:     Result := 'up';
    VK_DOWN:   Result := 'down';
    VK_SPACE:  Result := 'space';
    VK_RETURN: Result := 'enter';
    VK_ESCAPE: Result := 'escape';
  else
    if (Key >= Ord('A')) and (Key <= Ord('Z')) then
      Result := LowerCase(Char(Key))
    else
      Result := '';
  end;
end;

procedure TLoxGameCanvas.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  // Tell the dialog manager we want all keys ourselves — including
  // arrows and Tab — so they don't trigger focus traversal or other
  // default UI behaviour while a game is running.
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
end;

procedure TLoxGameCanvas.HostToLogical(HX, HY: Integer; out LX, LY: Integer);
var
  cw, ch, scale, dw, dh, dx, dy: Integer;
begin
  cw := ClientWidth;
  ch := ClientHeight;
  if (cw <= 0) or (ch <= 0) then
  begin
    LX := 0; LY := 0; Exit;
  end;
  // Mirror PresentScaled's geometry exactly so a click on a presented
  // pixel returns that same logical pixel.
  scale := cw div LOGICAL_W;
  if (ch div LOGICAL_H) < scale then scale := ch div LOGICAL_H;
  if scale < 1 then scale := 1;
  dw := LOGICAL_W * scale;
  dh := LOGICAL_H * scale;
  dx := (cw - dw) div 2;
  dy := (ch - dh) div 2;
  LX := (HX - dx) div scale;
  LY := (HY - dy) div scale;
  if LX < 0 then LX := 0
  else if LX > LOGICAL_W - 1 then LX := LOGICAL_W - 1;
  if LY < 0 then LY := 0
  else if LY > LOGICAL_H - 1 then LY := LOGICAL_H - 1;
end;

function MouseTButtonToIndex(Button: TMouseButton): Integer;
begin
  case Button of
    mbLeft:   Result := 0;
    mbRight:  Result := 1;
    mbMiddle: Result := 2;
  else
    Result := -1;
  end;
end;

procedure TLoxGameCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  btnIdx, lx, ly: Integer;
begin
  if CanFocus then SetFocus;
  HostToLogical(X, Y, lx, ly);
  FMouseX := lx;
  FMouseY := ly;
  btnIdx := MouseTButtonToIndex(Button);
  if (btnIdx >= 0) and (btnIdx <= 2) then
    FMouseBtn[btnIdx] := True;
  if Assigned(FOnGameMouseDown) and (btnIdx >= 0) then
    FOnGameMouseDown(Self, btnIdx, lx, ly);
  inherited;
end;

procedure TLoxGameCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  btnIdx, lx, ly: Integer;
begin
  HostToLogical(X, Y, lx, ly);
  FMouseX := lx;
  FMouseY := ly;
  btnIdx := MouseTButtonToIndex(Button);
  if (btnIdx >= 0) and (btnIdx <= 2) then
    FMouseBtn[btnIdx] := False;
  if Assigned(FOnGameMouseUp) and (btnIdx >= 0) then
    FOnGameMouseUp(Self, btnIdx, lx, ly);
  inherited;
end;

procedure TLoxGameCanvas.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lx, ly: Integer;
begin
  HostToLogical(X, Y, lx, ly);
  FMouseX := lx;
  FMouseY := ly;
  if Assigned(FOnGameMouseMove) then
    FOnGameMouseMove(Self, -1, lx, ly);
  inherited;
end;

procedure TLoxGameCanvas.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyName: string;
begin
  KeyName := MapVKToName(Key);
  if KeyName <> '' then
  begin
    SetKeyState(KeyName, True);
    if Assigned(FOnGameKeyDown) then FOnGameKeyDown(Self, KeyName);
    Key := 0;  // consume so it doesn't bubble to the form / editor
  end else
    inherited;
end;

procedure TLoxGameCanvas.KeyUp(var Key: Word; Shift: TShiftState);
var
  KeyName: string;
begin
  KeyName := MapVKToName(Key);
  if KeyName <> '' then
  begin
    SetKeyState(KeyName, False);
    if Assigned(FOnGameKeyUp) then FOnGameKeyUp(Self, KeyName);
    Key := 0;
  end else
    inherited;
end;

procedure TLoxGameCanvas.WMPaint(var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  // Repaint fallback: the active game loop presents directly via GetDC
  // and ValidateRect, so WM_PAINT only fires when the window genuinely
  // needs to redraw (uncover, restore, alt-tab, resize). Re-blit the
  // last presented frame from FFrontBuffer so the user sees something
  // sensible even when the script is paused or finished.
  DC := BeginPaint(Handle, PS);
  try
    if FHasFrontFrame and (FFrontBuffer <> nil) then
      PresentScaled(DC, ClientWidth, ClientHeight)
    else
    begin
      // No frame yet — fill black so we don't show garbage.
      Canvas.Brush.Color := clBlack;
      Canvas.FillRect(ClientRect);
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

procedure TLoxGameCanvas.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;  // suppress background erase entirely (no flicker)
end;

function GameCanvas: TLoxGameCanvas;
begin
  Result := FGameSurface;
end;

{ TTilemap }

constructor TTilemap.Create(ACols, ARows, ATileW, ATileH: Integer);
var
  i: Integer;
begin
  inherited Create;
  Cols := ACols;
  Rows := ARows;
  TileW := ATileW;
  TileH := ATileH;
  SetLength(Tiles, ACols * ARows);
  for i := 0 to Length(Tiles) - 1 do
    Tiles[i] := -1;
end;

procedure InitCanvas(AParent: TWinControl);
begin
  FCurrentPixel := $FFFFFFFF;  // opaque white in BGRA
  FreeAndNil(FGameSurface);
  FGameSurface := TLoxGameCanvas.Create(AParent);
  FGameSurface.Parent := AParent;
  FGameSurface.Align := alClient;
  FreeAndNil(FBackBuffer);
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackBuffer.SetSize(LOGICAL_W, LOGICAL_H);
  FBackBuffer.Canvas.Brush.Color := clBlack;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  FRenderTarget := FBackBuffer;
  FRenderTargetId := -1;
  FClipActive := False;
  ResetClipToTarget;
  FHasFrontFrame := False;
  FreeAndNil(FPresentBuffer);
  FreeAndNil(FKeysHeld);
  FKeysHeld := TDictionary<string, Boolean>.Create;
end;

procedure FreeCanvas;
begin
  FreeAndNil(FTilemaps);
  FreeAndNil(FSpriteFreelist);
  FreeAndNil(FSprites);
  FreeAndNil(FSurfaceFreelist);
  FreeAndNil(FSurfaces);
  FreeAndNil(FFrontBuffer);
  FreeAndNil(FPresentBuffer);
  FreeAndNil(FBackBuffer);
  FRenderTarget := nil;
  FRenderTargetId := -1;
  FreeAndNil(FKeysHeld);
  FreeAndNil(FGameSurface);
end;

// -- Native functions exposed to Lox --

function canvasWidthNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(LOGICAL_W);
end;

function canvasHeightNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(LOGICAL_H);
end;

procedure ClearBuffer(ABmp: TBitmap);
var
  y: Integer;
begin
  for y := 0 to ABmp.Height - 1 do
    FillChar(ABmp.ScanLine[y]^, ABmp.Width * SizeOf(Cardinal), 0);
end;

// Unchecked pixel write â€” caller must guarantee x,y are in bounds.
// Use this in internal native blitters where clipping is done at a higher level.
procedure PutPixelUnchecked(x, y: Integer; pixel: Cardinal); inline;
begin
  PCardinal(FRenderTarget.ScanLine[y])[x] := pixel;
end;

function clearCanvasNative(argCount: integer; args: pValue): TValue;
var
  r, g, b: Integer;
  pixel: Cardinal;
  x, y, w, h: Integer;
  row: PCardinal;
begin
  EnsureBackBuffer;
  if argCount = 0 then
    ClearBuffer(FRenderTarget)
  else if argCount = 3 then
  begin
    if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
       (args[2].ValueKind <> vkNumber) then
    begin
      RuntimeError('clearCanvas() color arguments must be numbers.');
      Exit(CreateNilValue);
    end;
    r := Trunc(args[0].NumberValue) and $FF;
    g := Trunc(args[1].NumberValue) and $FF;
    b := Trunc(args[2].NumberValue) and $FF;
    pixel := Cardinal(b or (g shl 8) or (r shl 16) or $FF000000);
    w := FRenderTarget.Width;
    h := FRenderTarget.Height;
    for y := 0 to h - 1 do
    begin
      row := FRenderTarget.ScanLine[y];
      for x := 0 to w - 1 do
        row[x] := pixel;
    end;
  end else
  begin
    RuntimeError('clearCanvas() takes 0 or 3 arguments.');
    Exit(CreateNilValue);
  end;
  Result := CreateNilValue;
end;

function setColorNative(argCount: integer; args: pValue): TValue;
var
  r, g, b: Integer;
begin
  if argCount <> 3 then
  begin
    RuntimeError('setColor() takes 3 arguments (r, g, b).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('setColor() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  r := Trunc(args[0].NumberValue);
  g := Trunc(args[1].NumberValue);
  b := Trunc(args[2].NumberValue);
  if (r < 0) or (r > 255) or (g < 0) or (g > 255) or (b < 0) or (b > 255) then
  begin
    RuntimeError('setColor() values must be in the range 0..255.');
    Exit(CreateNilValue);
  end;
  // Precompute native BGRA pixel: TRGBQuad layout is Blue, Green, Red, Reserved
  // Alpha must be $FF (opaque) so 32-bit alpha-aware GDI paths don't treat
  // colored pixels as transparent.
  FCurrentPixel := Cardinal(b or (g shl 8) or (r shl 16) or $FF000000);
  Result := CreateNilValue;
end;

function setCameraNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('setCamera() takes 2 arguments (x, y).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('setCamera() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  FCameraX := Trunc(args[0].NumberValue);
  FCameraY := Trunc(args[1].NumberValue);
  Result := CreateNilValue;
end;

function setClipRectNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h: Integer;
begin
  if argCount <> 4 then
  begin
    RuntimeError('setClipRect() takes 4 arguments (x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('setClipRect() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue);
  y := Trunc(args[1].NumberValue);
  w := Trunc(args[2].NumberValue);
  h := Trunc(args[3].NumberValue);
  // Clamp to target bounds
  if x < 0 then begin w := w + x; x := 0; end;
  if y < 0 then begin h := h + y; y := 0; end;
  if x + w > FRenderTarget.Width then w := FRenderTarget.Width - x;
  if y + h > FRenderTarget.Height then h := FRenderTarget.Height - y;
  if (w <= 0) or (h <= 0) then
  begin
    // Degenerate clip — nothing will draw
    FClipX1 := 0; FClipY1 := 0; FClipX2 := 0; FClipY2 := 0;
  end else
  begin
    FClipX1 := x;
    FClipY1 := y;
    FClipX2 := x + w;
    FClipY2 := y + h;
  end;
  FClipActive := True;
  Result := CreateNilValue;
end;

function clearClipRectNative(argCount: integer; args: pValue): TValue;
begin
  FClipActive := False;
  EnsureBackBuffer;
  FClipX1 := 0;
  FClipY1 := 0;
  FClipX2 := FRenderTarget.Width;
  FClipY2 := FRenderTarget.Height;
  Result := CreateNilValue;
end;

function fillRectNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h: Integer;
  x1, y1, x2, y2, py, px: Integer;
  row: PCardinal;
begin
  if argCount <> 4 then
  begin
    RuntimeError('fillRect() takes 4 arguments (x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('fillRect() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue) - FCameraX;
  y := Trunc(args[1].NumberValue) - FCameraY;
  w := Trunc(args[2].NumberValue);
  h := Trunc(args[3].NumberValue);
  // Clamp to clip rect
  x1 := x; y1 := y; x2 := x + w; y2 := y + h;
  if x1 < FClipX1 then x1 := FClipX1;
  if y1 < FClipY1 then y1 := FClipY1;
  if x2 > FClipX2 then x2 := FClipX2;
  if y2 > FClipY2 then y2 := FClipY2;
  if (x1 >= x2) or (y1 >= y2) then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  for py := y1 to y2 - 1 do
  begin
    row := FRenderTarget.ScanLine[py];
    for px := x1 to x2 - 1 do
      row[px] := FCurrentPixel;
  end;
  Result := CreateNilValue;
end;

function drawTextNative(argCount: integer; args: pValue): TValue;
const
  // Built-in 5Ã—7 bitmap font â€” printable ASCII 32..126
  // Each glyph = 7 bytes (rows top-to-bottom), bit4=leftmost, bit0=rightmost
  FONT_W = 5;
  FONT_H = 7;
  FONT_ADVANCE = 6;  // 5px glyph + 1px spacing
  FONT_FIRST = 32;
  FONT_LAST  = 126;
  FONT_DATA: array[0..664] of Byte = (
    // 32 space
    $00,$00,$00,$00,$00,$00,$00,
    // 33 !
    $04,$04,$04,$04,$00,$04,$00,
    // 34 "
    $0A,$0A,$00,$00,$00,$00,$00,
    // 35 #
    $0A,$1F,$0A,$0A,$1F,$0A,$00,
    // 36 $
    $04,$0E,$14,$0E,$05,$0E,$04,
    // 37 %
    $18,$19,$02,$04,$08,$13,$03,
    // 38 &
    $0C,$12,$0C,$0A,$11,$12,$0D,
    // 39 '
    $04,$04,$00,$00,$00,$00,$00,
    // 40 (
    $02,$04,$08,$08,$08,$04,$02,
    // 41 )
    $08,$04,$02,$02,$02,$04,$08,
    // 42 *
    $00,$04,$15,$0E,$15,$04,$00,
    // 43 +
    $00,$04,$04,$1F,$04,$04,$00,
    // 44 ,
    $00,$00,$00,$00,$00,$04,$08,
    // 45 -
    $00,$00,$00,$1F,$00,$00,$00,
    // 46 .
    $00,$00,$00,$00,$00,$04,$00,
    // 47 /
    $01,$02,$02,$04,$08,$08,$10,
    // 48 0
    $0E,$11,$13,$15,$19,$11,$0E,
    // 49 1
    $04,$0C,$04,$04,$04,$04,$0E,
    // 50 2
    $0E,$11,$01,$02,$04,$08,$1F,
    // 51 3
    $0E,$11,$01,$06,$01,$11,$0E,
    // 52 4
    $02,$06,$0A,$12,$1F,$02,$02,
    // 53 5
    $1F,$10,$1E,$01,$01,$11,$0E,
    // 54 6
    $06,$08,$10,$1E,$11,$11,$0E,
    // 55 7
    $1F,$01,$02,$04,$04,$04,$04,
    // 56 8
    $0E,$11,$11,$0E,$11,$11,$0E,
    // 57 9
    $0E,$11,$11,$0F,$01,$02,$0C,
    // 58 :
    $00,$00,$04,$00,$04,$00,$00,
    // 59 ;
    $00,$00,$04,$00,$04,$04,$08,
    // 60 <
    $02,$04,$08,$10,$08,$04,$02,
    // 61 =
    $00,$00,$1F,$00,$1F,$00,$00,
    // 62 >
    $08,$04,$02,$01,$02,$04,$08,
    // 63 ?
    $0E,$11,$01,$06,$04,$00,$04,
    // 64 @
    $0E,$11,$17,$15,$17,$10,$0E,
    // 65 A
    $04,$0A,$11,$11,$1F,$11,$11,
    // 66 B
    $1E,$11,$11,$1E,$11,$11,$1E,
    // 67 C
    $0E,$11,$10,$10,$10,$11,$0E,
    // 68 D
    $1E,$11,$11,$11,$11,$11,$1E,
    // 69 E
    $1F,$10,$10,$1E,$10,$10,$1F,
    // 70 F
    $1F,$10,$10,$1C,$10,$10,$10,
    // 71 G
    $0E,$11,$10,$17,$11,$11,$0E,
    // 72 H
    $11,$11,$11,$1F,$11,$11,$11,
    // 73 I
    $0E,$04,$04,$04,$04,$04,$0E,
    // 74 J
    $01,$01,$01,$01,$01,$11,$0E,
    // 75 K
    $11,$12,$14,$18,$14,$12,$11,
    // 76 L
    $10,$10,$10,$10,$10,$10,$1F,
    // 77 M
    $11,$1B,$15,$11,$11,$11,$11,
    // 78 N
    $11,$19,$15,$13,$11,$11,$11,
    // 79 O
    $0E,$11,$11,$11,$11,$11,$0E,
    // 80 P
    $1E,$11,$11,$1E,$10,$10,$10,
    // 81 Q
    $0E,$11,$11,$11,$15,$12,$0D,
    // 82 R
    $1E,$11,$11,$1E,$14,$12,$11,
    // 83 S
    $0E,$11,$10,$0E,$01,$11,$0E,
    // 84 T
    $1F,$04,$04,$04,$04,$04,$04,
    // 85 U
    $11,$11,$11,$11,$11,$11,$0E,
    // 86 V
    $11,$11,$11,$11,$0A,$0A,$04,
    // 87 W
    $11,$11,$11,$11,$15,$15,$0A,
    // 88 X
    $11,$11,$0A,$04,$0A,$11,$11,
    // 89 Y
    $11,$11,$0A,$04,$04,$04,$04,
    // 90 Z
    $1F,$01,$02,$04,$08,$10,$1F,
    // 91 [
    $0E,$08,$08,$08,$08,$08,$0E,
    // 92 backslash
    $10,$08,$08,$04,$02,$02,$01,
    // 93 ]
    $0E,$02,$02,$02,$02,$02,$0E,
    // 94 ^
    $04,$0A,$11,$00,$00,$00,$00,
    // 95 _
    $00,$00,$00,$00,$00,$00,$1F,
    // 96 `
    $08,$04,$00,$00,$00,$00,$00,
    // 97 a
    $00,$00,$0E,$01,$0F,$11,$0F,
    // 98 b
    $10,$10,$1E,$11,$11,$11,$1E,
    // 99 c
    $00,$00,$0E,$11,$10,$11,$0E,
    // 100 d
    $01,$01,$0F,$11,$11,$11,$0F,
    // 101 e
    $00,$00,$0E,$11,$1F,$10,$0E,
    // 102 f
    $06,$08,$08,$1C,$08,$08,$08,
    // 103 g
    $00,$00,$0F,$11,$0F,$01,$0E,
    // 104 h
    $10,$10,$1E,$11,$11,$11,$11,
    // 105 i
    $04,$00,$0C,$04,$04,$04,$0E,
    // 106 j
    $02,$00,$02,$02,$02,$12,$0C,
    // 107 k
    $10,$10,$12,$14,$18,$14,$12,
    // 108 l
    $0C,$04,$04,$04,$04,$04,$0E,
    // 109 m
    $00,$00,$1A,$15,$15,$15,$15,
    // 110 n
    $00,$00,$1E,$11,$11,$11,$11,
    // 111 o
    $00,$00,$0E,$11,$11,$11,$0E,
    // 112 p
    $00,$00,$1E,$11,$1E,$10,$10,
    // 113 q
    $00,$00,$0F,$11,$0F,$01,$01,
    // 114 r
    $00,$00,$16,$19,$10,$10,$10,
    // 115 s
    $00,$00,$0E,$10,$0E,$01,$1E,
    // 116 t
    $08,$08,$1C,$08,$08,$08,$06,
    // 117 u
    $00,$00,$11,$11,$11,$11,$0F,
    // 118 v
    $00,$00,$11,$11,$0A,$0A,$04,
    // 119 w
    $00,$00,$11,$11,$15,$15,$0A,
    // 120 x
    $00,$00,$11,$0A,$04,$0A,$11,
    // 121 y
    $00,$00,$11,$11,$0F,$01,$0E,
    // 122 z
    $00,$00,$1F,$02,$04,$08,$1F,
    // 123 {
    $02,$04,$04,$08,$04,$04,$02,
    // 124 |
    $04,$04,$04,$04,$04,$04,$04,
    // 125 }
    $08,$04,$04,$02,$04,$04,$08,
    // 126 ~
    $00,$00,$08,$15,$02,$00,$00
  );
var
  x, y, scale, i, gx, gy, charIdx: Integer;
  ax, ay, px, py, bw, bh: Integer;
  s: AnsiString;
  ch: Byte;
  rowBits: Byte;
  dstRow: PCardinal;
begin
  if (argCount < 3) or (argCount > 4) then
  begin
    RuntimeError('drawText() takes 3 or 4 arguments (x, y, text [, scale]).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawText() x and y must be numbers.');
    Exit(CreateNilValue);
  end;
  if not isString(args[2]) then
  begin
    RuntimeError('drawText() third argument must be a string.');
    Exit(CreateNilValue);
  end;
  scale := 1;
  if (argCount = 4) then
  begin
    if args[3].ValueKind <> vkNumber then
    begin
      RuntimeError('drawText() scale must be a number.');
      Exit(CreateNilValue);
    end;
    scale := Trunc(args[3].NumberValue);
    if scale < 1 then scale := 1;
  end;
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue) - FCameraX;
  y := Trunc(args[1].NumberValue) - FCameraY;
  s := ObjStringToAnsiString(pObjString(args[2].ObjValue));
  bw := FClipX2;
  bh := FClipY2;

  for i := 1 to Length(s) do
  begin
    ch := Ord(s[i]);
    if (ch < FONT_FIRST) or (ch > FONT_LAST) then
      ch := FONT_FIRST;  // unknown â†’ space
    charIdx := (ch - FONT_FIRST) * FONT_H;

    for gy := 0 to FONT_H - 1 do
    begin
      rowBits := FONT_DATA[charIdx + gy];
      if rowBits = 0 then Continue;  // skip empty rows
      for gx := 0 to FONT_W - 1 do
      begin
        if (rowBits and ($10 shr gx)) <> 0 then
        begin
          // Plot scaled pixel block
          for py := 0 to scale - 1 do
          begin
            ay := y + gy * scale + py;
            if (ay < FClipY1) or (ay >= bh) then Continue;
            dstRow := FRenderTarget.ScanLine[ay];
            for px := 0 to scale - 1 do
            begin
              ax := x + gx * scale + px;
              if (ax >= FClipX1) and (ax < bw) then
                dstRow[ax] := FCurrentPixel;
            end;
          end;
        end;
      end;
    end;
    x := x + FONT_ADVANCE * scale;
  end;
  Result := CreateNilValue;
end;

function measureTextNative(argCount: integer; args: pValue): TValue;
var
  textLen, scale: Integer;
  s: string;
begin
  if (argCount < 1) or (argCount > 2) then
  begin
    RuntimeError('measureText() takes 1 or 2 arguments (text [, scale]).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkObject then
  begin
    RuntimeError('measureText() first argument must be a string.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(args[0].ObjValue));
  textLen := Length(s);
  scale := 1;
  if argCount = 2 then
  begin
    if args[1].ValueKind <> vkNumber then
    begin
      RuntimeError('measureText() scale must be a number.');
      Exit(CreateNilValue);
    end;
    scale := Trunc(args[1].NumberValue);
    if scale < 1 then scale := 1;
  end;
  Result := CreateNumber(textLen * 6 * scale);
end;

procedure SetKeyState(const KeyName: string; Down: Boolean);
begin
  if FKeysHeld = nil then
    FKeysHeld := TDictionary<string, Boolean>.Create;
  FKeysHeld.AddOrSetValue(KeyName, Down);
end;

procedure ClearAllKeyState;
begin
  if FKeysHeld <> nil then
    FKeysHeld.Clear;
end;

function keyHeldNative(argCount: integer; args: pValue): TValue;
var
  name: string;
  held: Boolean;
begin
  if argCount <> 1 then
  begin
    RuntimeError('keyHeld() takes 1 argument (key name).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkObject then
  begin
    RuntimeError('keyHeld() argument must be a string.');
    Exit(CreateNilValue);
  end;
  name := ObjStringToAnsiString(pObjString(args[0].ObjValue));
  held := False;
  if (FKeysHeld <> nil) then
    FKeysHeld.TryGetValue(name, held);
  Result := CreateBoolean(held);
end;

function mouseXNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 0 then
  begin
    RuntimeError('mouseX() takes no arguments.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FMouseX);
end;

function mouseYNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 0 then
  begin
    RuntimeError('mouseY() takes no arguments.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FMouseY);
end;

function mouseDownNative(argCount: integer; args: pValue): TValue;
var
  btn: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('mouseDown() takes 1 argument (button 0=left, 1=right, 2=middle).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('mouseDown() argument must be a number.');
    Exit(CreateNilValue);
  end;
  btn := Trunc(args[0].NumberValue);
  if (btn < 0) or (btn > 2) then
  begin
    RuntimeError('mouseDown() button must be 0..2.');
    Exit(CreateNilValue);
  end;
  Result := CreateBoolean(FMouseBtn[btn]);
end;

function spriteWidthNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('spriteWidth() takes 1 argument (sprite id).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('spriteWidth() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) or (FSprites[id] = nil) then
  begin
    RuntimeError('spriteWidth() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FSprites[id].Width);
end;

function spriteHeightNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('spriteHeight() takes 1 argument (sprite id).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('spriteHeight() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) or (FSprites[id] = nil) then
  begin
    RuntimeError('spriteHeight() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FSprites[id].Height);
end;

function presentNative(argCount: integer; args: pValue): TValue;
var
  Temp: TBitmap;
begin
  EnsureBackBuffer;
  // Swap buffers: promote back buffer to front, reuse old front as new back.
  if FFrontBuffer = nil then
  begin
    FFrontBuffer := TBitmap.Create;
    FFrontBuffer.PixelFormat := pf32bit;
    FFrontBuffer.SetSize(FBackBuffer.Width, FBackBuffer.Height);
  end;
  Temp := FFrontBuffer;
  FFrontBuffer := FBackBuffer;
  FBackBuffer := Temp;
  // Ensure new back buffer matches dimensions and is cleared
  if (FBackBuffer.Width <> FFrontBuffer.Width) or
     (FBackBuffer.Height <> FFrontBuffer.Height) then
    FBackBuffer.SetSize(FFrontBuffer.Width, FFrontBuffer.Height);
  ClearBuffer(FBackBuffer);
  // Keep render target in sync after swap
  if FRenderTargetId < 0 then
    FRenderTarget := FBackBuffer;
  FHasFrontFrame := True;
  // Direct DC presentation: bypass WM_PAINT round-trip entirely.
  // The composed staging bitmap is copied to the window in one BitBlt.
  if (FGameSurface <> nil) and FGameSurface.HandleAllocated then
  begin
    var dc := GetDC(FGameSurface.Handle);
    PresentScaled(dc, FGameSurface.ClientWidth, FGameSurface.ClientHeight);
    ReleaseDC(FGameSurface.Handle, dc);
    ValidateRect(FGameSurface.Handle, nil);
  end;
  // Pace presentation to the DWM composition cycle for effective vsync.
  DwmFlush;
  Result := CreateNilValue;
end;

function drawLineNative(argCount: integer; args: pValue): TValue;
var
  x1, y1, x2, y2: Integer;
  dx, dy, sx, sy, err, e2: Integer;
  bw, bh: Integer;
begin
  if argCount <> 4 then
  begin
    RuntimeError('drawLine() takes 4 arguments (x1, y1, x2, y2).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawLine() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x1 := Trunc(args[0].NumberValue) - FCameraX;
  y1 := Trunc(args[1].NumberValue) - FCameraY;
  x2 := Trunc(args[2].NumberValue) - FCameraX;
  y2 := Trunc(args[3].NumberValue) - FCameraY;
  bw := FClipX2;
  bh := FClipY2;
  // Bresenham's line algorithm â€” crisp single-pixel line
  dx := Abs(x2 - x1);
  dy := -Abs(y2 - y1);
  if x1 < x2 then sx := 1 else sx := -1;
  if y1 < y2 then sy := 1 else sy := -1;
  err := dx + dy;
  while True do
  begin
    if (x1 >= FClipX1) and (x1 < bw) and (y1 >= FClipY1) and (y1 < bh) then
      PutPixelUnchecked(x1, y1, FCurrentPixel);
    if (x1 = x2) and (y1 = y2) then Break;
    e2 := 2 * err;
    if e2 >= dy then begin err := err + dy; x1 := x1 + sx; end;
    if e2 <= dx then begin err := err + dx; y1 := y1 + sy; end;
  end;
  Result := CreateNilValue;
end;

function drawRectNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h: Integer;
  bw, bh, px, py: Integer;
  row: PCardinal;
begin
  if argCount <> 4 then
  begin
    RuntimeError('drawRect() takes 4 arguments (x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawRect() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue) - FCameraX;
  y := Trunc(args[1].NumberValue) - FCameraY;
  w := Trunc(args[2].NumberValue);
  h := Trunc(args[3].NumberValue);
  if (w <= 0) or (h <= 0) then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Top edge
  if (y >= FClipY1) and (y < bh) then
  begin
    row := FRenderTarget.ScanLine[y];
    for px := x to x + w - 1 do
      if (px >= FClipX1) and (px < bw) then
        row[px] := FCurrentPixel;
  end;
  // Bottom edge
  if (y + h - 1 >= FClipY1) and (y + h - 1 < bh) and (h > 1) then
  begin
    row := FRenderTarget.ScanLine[y + h - 1];
    for px := x to x + w - 1 do
      if (px >= FClipX1) and (px < bw) then
        row[px] := FCurrentPixel;
  end;
  // Left edge
  if (x >= FClipX1) and (x < bw) then
    for py := y + 1 to y + h - 2 do
      if (py >= FClipY1) and (py < bh) then
        PCardinal(FRenderTarget.ScanLine[py])[x] := FCurrentPixel;
  // Right edge
  if (x + w - 1 >= FClipX1) and (x + w - 1 < bw) then
    for py := y + 1 to y + h - 2 do
      if (py >= FClipY1) and (py < bh) then
        PCardinal(FRenderTarget.ScanLine[py])[x + w - 1] := FCurrentPixel;
  Result := CreateNilValue;
end;

function drawCircleNative(argCount: integer; args: pValue): TValue;
var
  cx, cy, r: Integer;
  bw, bh: Integer;
  x, y, d: Integer;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawCircle() takes 3 arguments (x, y, radius).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawCircle() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  cx := Trunc(args[0].NumberValue) - FCameraX;
  cy := Trunc(args[1].NumberValue) - FCameraY;
  r  := Trunc(args[2].NumberValue);
  if r < 0 then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Midpoint circle â€” plot outline pixels in all 8 octants
  x := 0;
  y := r;
  d := 1 - r;
  while x <= y do
  begin
    // 8-way symmetry
    if (cx + x >= FClipX1) and (cx + x < bw) and (cy + y >= FClipY1) and (cy + y < bh) then
      PutPixelUnchecked(cx + x, cy + y, FCurrentPixel);
    if (cx - x >= FClipX1) and (cx - x < bw) and (cy + y >= FClipY1) and (cy + y < bh) then
      PutPixelUnchecked(cx - x, cy + y, FCurrentPixel);
    if (cx + x >= FClipX1) and (cx + x < bw) and (cy - y >= FClipY1) and (cy - y < bh) then
      PutPixelUnchecked(cx + x, cy - y, FCurrentPixel);
    if (cx - x >= FClipX1) and (cx - x < bw) and (cy - y >= FClipY1) and (cy - y < bh) then
      PutPixelUnchecked(cx - x, cy - y, FCurrentPixel);
    if (cx + y >= FClipX1) and (cx + y < bw) and (cy + x >= FClipY1) and (cy + x < bh) then
      PutPixelUnchecked(cx + y, cy + x, FCurrentPixel);
    if (cx - y >= FClipX1) and (cx - y < bw) and (cy + x >= FClipY1) and (cy + x < bh) then
      PutPixelUnchecked(cx - y, cy + x, FCurrentPixel);
    if (cx + y >= FClipX1) and (cx + y < bw) and (cy - x >= FClipY1) and (cy - x < bh) then
      PutPixelUnchecked(cx + y, cy - x, FCurrentPixel);
    if (cx - y >= FClipX1) and (cx - y < bw) and (cy - x >= FClipY1) and (cy - x < bh) then
      PutPixelUnchecked(cx - y, cy - x, FCurrentPixel);
    Inc(x);
    if d < 0 then
      d := d + 2 * x + 1
    else
    begin
      Dec(y);
      d := d + 2 * (x - y) + 1;
    end;
  end;
  Result := CreateNilValue;
end;

function fillCircleNative(argCount: integer; args: pValue): TValue;
var
  cx, cy, r: Integer;
  bw, bh: Integer;
  x, y, d: Integer;
  x0, x1, yy: Integer;
  row: PCardinal;

  procedure HLine(hx0, hx1, hy: Integer);
  var
    px: Integer;
  begin
    if (hy < FClipY1) or (hy >= bh) then Exit;
    if hx0 < FClipX1 then hx0 := FClipX1;
    if hx1 >= bw then hx1 := bw - 1;
    if hx0 > hx1 then Exit;
    row := FRenderTarget.ScanLine[hy];
    for px := hx0 to hx1 do
      row[px] := FCurrentPixel;
  end;

begin
  if argCount <> 3 then
  begin
    RuntimeError('fillCircle() takes 3 arguments (x, y, radius).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('fillCircle() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  cx := Trunc(args[0].NumberValue) - FCameraX;
  cy := Trunc(args[1].NumberValue) - FCameraY;
  r  := Trunc(args[2].NumberValue);
  if r < 0 then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Midpoint circle â€” fill with horizontal spans
  x := 0;
  y := r;
  d := 1 - r;
  // Draw initial horizontal line through center
  HLine(cx - r, cx + r, cy);
  while x < y do
  begin
    Inc(x);
    if d < 0 then
      d := d + 2 * x + 1
    else
    begin
      // Fill the wider span pair before shrinking y
      HLine(cx - x + 1, cx + x - 1, cy - y);
      HLine(cx - x + 1, cx + x - 1, cy + y);
      Dec(y);
      d := d + 2 * (x - y) + 1;
    end;
    if x <= y then
    begin
      HLine(cx - y, cx + y, cy - x);
      HLine(cx - y, cx + y, cy + x);
    end;
  end;
  Result := CreateNilValue;
end;

function drawPixelNative(argCount: integer; args: pValue): TValue;
var
  x, y: Integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('drawPixel() takes 2 arguments (x, y).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawPixel() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue) - FCameraX;
  y := Trunc(args[1].NumberValue) - FCameraY;
  if (x >= FClipX1) and (x < FClipX2) and
     (y >= FClipY1) and (y < FClipY2) then
    PCardinal(FRenderTarget.ScanLine[y])[x] := FCurrentPixel;
  Result := CreateNilValue;
end;

// -- Sprite functions --

function AllocSpriteSlot(bmp: TBitmap): Integer;
var
  idx: Integer;
begin
  if FSprites = nil then
    FSprites := TObjectList<TBitmap>.Create(True);
  if FSpriteFreelist = nil then
    FSpriteFreelist := TList<Integer>.Create;
  if FSpriteFreelist.Count > 0 then
  begin
    idx := FSpriteFreelist[FSpriteFreelist.Count - 1];
    FSpriteFreelist.Delete(FSpriteFreelist.Count - 1);
    FSprites[idx] := bmp;
    Result := idx;
  end
  else
    Result := FSprites.Add(bmp);
end;

function createSpriteNative(argCount: integer; args: pValue): TValue;
var
  w, h, x, y, i: Integer;
  s: AnsiString;
  bmp: TBitmap;
  transColor: TColor;
  row: PRGBQuadArray;
begin
  if argCount <> 3 then
  begin
    RuntimeError('createSprite() takes 3 arguments (width, height, pixels).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('createSprite() width and height must be numbers.');
    Exit(CreateNilValue);
  end;
  if not isString(args[2]) then
  begin
    RuntimeError('createSprite() third argument must be a string.');
    Exit(CreateNilValue);
  end;
  w := Trunc(args[0].NumberValue);
  h := Trunc(args[1].NumberValue);
  if (w <= 0) or (h <= 0) then
  begin
    RuntimeError('createSprite() width and height must be positive.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(args[2].ObjValue));
  if Length(s) <> w * h then
  begin
    RuntimeError('createSprite() pixel string length must equal width * height.');
    Exit(CreateNilValue);
  end;

  transColor := TColor($FF00FF); // magenta transparency key
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(w, h);
    // Fill with transparency key and stamp colored pixels via ScanLine
    i := 1;
    for y := 0 to h - 1 do
    begin
      row := bmp.ScanLine[y];
      for x := 0 to w - 1 do
      begin
        if s[i] <> '.' then
          PCardinal(@row[x])^ := FCurrentPixel
        else
          PCardinal(@row[x])^ := $00FF00FF; // magenta in BGRA
        Inc(i);
      end;
    end;
    bmp.Transparent := True;
    bmp.TransparentColor := transColor;
  except
    bmp.Free;
    raise;
  end;

  Result := CreateNumber(AllocSpriteSlot(bmp));
end;

function drawSpriteNative(argCount: integer; args: pValue): TValue;
var
  id, x, y, sx, sy, bw, bh: Integer;
  bmp: TBitmap;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawSprite() takes 3 arguments (id, x, y).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawSprite() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('drawSprite() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[1].NumberValue) - FCameraX;
  y := Trunc(args[2].NumberValue) - FCameraY;
  bmp := FSprites[id];
  if bmp = nil then
  begin
    RuntimeError('drawSprite() sprite has been freed.');
    Exit(CreateNilValue);
  end;
  bw := FClipX2;
  bh := FClipY2;
  for sy := 0 to bmp.Height - 1 do
  begin
    if (y + sy < FClipY1) or (y + sy >= bh) then Continue;
    srcRow := bmp.ScanLine[sy];
    dstRow := FRenderTarget.ScanLine[y + sy];
    for sx := 0 to bmp.Width - 1 do
    begin
      if (x + sx < FClipX1) or (x + sx >= bw) then Continue;
      pixel := PCardinal(@srcRow[sx])^;
      if pixel <> $00FF00FF then
        dstRow[x + sx] := pixel;
    end;
  end;
  Result := CreateNilValue;
end;

function drawSpriteScaledNative(argCount: integer; args: pValue): TValue;
var
  id, x, y, scale, sw, sh: Integer;
  bmp: TBitmap;
  sx, sy, dx, dy, bw, bh: Integer;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 4 then
  begin
    RuntimeError('drawSpriteScaled() takes 4 arguments (id, x, y, scale).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawSpriteScaled() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('drawSpriteScaled() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[1].NumberValue) - FCameraX;
  y := Trunc(args[2].NumberValue) - FCameraY;
  scale := Trunc(args[3].NumberValue);
  if scale < 1 then
  begin
    RuntimeError('drawSpriteScaled() scale must be >= 1.');
    Exit(CreateNilValue);
  end;
  bmp := FSprites[id];
  if bmp = nil then
  begin
    RuntimeError('drawSpriteScaled() sprite has been freed.');
    Exit(CreateNilValue);
  end;
  sw := bmp.Width * scale;
  sh := bmp.Height * scale;
  bw := FClipX2;
  bh := FClipY2;
  // Manual scanline blit: skip transparent pixels ($00FF00FF), copy opaque ones.
  // Single pass, no GDI mask operations, deterministic rendering.
  for dy := 0 to sh - 1 do
  begin
    if (y + dy < FClipY1) or (y + dy >= bh) then Continue;
    sy := dy div scale;
    srcRow := bmp.ScanLine[sy];
    dstRow := FRenderTarget.ScanLine[y + dy];
    for dx := 0 to sw - 1 do
    begin
      if (x + dx < FClipX1) or (x + dx >= bw) then Continue;
      sx := dx div scale;
      pixel := PCardinal(@srcRow[sx])^;
      if pixel <> $00FF00FF then
        dstRow[x + dx] := pixel;
    end;
  end;
  Result := CreateNilValue;
end;

function drawSpriteRotatedNative(argCount: integer; args: pValue): TValue;
var
  id, x, y, scale, sw, sh, dw, dh: Integer;
  bmp: TBitmap;
  angle, cosA, sinA, cx, cy, srcCx, srcCy: Double;
  dx, dy, sx, sy, bw, bh: Integer;
  fx, fy: Double;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 5 then
  begin
    RuntimeError('drawSpriteRotated() takes 5 arguments (id, x, y, scale, angle).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) or
     (args[4].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawSpriteRotated() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('drawSpriteRotated() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[1].NumberValue) - FCameraX;
  y := Trunc(args[2].NumberValue) - FCameraY;
  scale := Trunc(args[3].NumberValue);
  if scale < 1 then scale := 1;
  angle := args[4].NumberValue * Pi / 180.0; // degrees to radians
  cosA := Cos(angle);
  sinA := Sin(angle);

  bmp := FSprites[id];
  if bmp = nil then
  begin
    RuntimeError('drawSpriteRotated() sprite has been freed.');
    Exit(CreateNilValue);
  end;
  sw := bmp.Width * scale;
  sh := bmp.Height * scale;
  // Bounding box of rotated sprite
  dw := Ceil(Abs(sw * cosA) + Abs(sh * sinA));
  dh := Ceil(Abs(sw * sinA) + Abs(sh * cosA));
  // Center of destination bounding box (x, y is center of output)
  cx := dw / 2.0;
  cy := dh / 2.0;
  // Center of source (scaled) sprite
  srcCx := sw / 2.0;
  srcCy := sh / 2.0;
  bw := FClipX2;
  bh := FClipY2;

  // Inverse-mapping: for each destination pixel, find source pixel
  for dy := 0 to dh - 1 do
  begin
    if (y - Trunc(cy) + dy < FClipY1) or (y - Trunc(cy) + dy >= bh) then Continue;
    dstRow := FRenderTarget.ScanLine[y - Trunc(cy) + dy];
    for dx := 0 to dw - 1 do
    begin
      if (x - Trunc(cx) + dx < FClipX1) or (x - Trunc(cx) + dx >= bw) then Continue;
      // Rotate destination pixel back to source space
      fx := (dx - cx) * cosA + (dy - cy) * sinA + srcCx;
      fy := -(dx - cx) * sinA + (dy - cy) * cosA + srcCy;
      // Map back through scale to original sprite coords
      sx := Trunc(fx) div scale;
      sy := Trunc(fy) div scale;
      if (sx >= 0) and (sx < bmp.Width) and (sy >= 0) and (sy < bmp.Height) and
         (Trunc(fx) >= 0) and (Trunc(fx) < sw) and (Trunc(fy) >= 0) and (Trunc(fy) < sh) then
      begin
        srcRow := bmp.ScanLine[sy];
        pixel := PCardinal(@srcRow[sx])^;
        if pixel <> $00FF00FF then
          dstRow[x - Trunc(cx) + dx] := pixel;
      end;
    end;
  end;
  Result := CreateNilValue;
end;

// -- Palette sprite functions --

function setPaletteColorNative(argCount: integer; args: pValue): TValue;
var
  s: AnsiString;
  ch: Byte;
  r, g, b: Integer;
begin
  if argCount <> 4 then
  begin
    RuntimeError('setPaletteColor() takes 4 arguments (char, r, g, b).');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('setPaletteColor() first argument must be a single-character string.');
    Exit(CreateNilValue);
  end;
  if (args[1].ValueKind <> vkNumber) or (args[2].ValueKind <> vkNumber) or
     (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('setPaletteColor() r, g, b must be numbers.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(args[0].ObjValue));
  if Length(s) <> 1 then
  begin
    RuntimeError('setPaletteColor() first argument must be a single character.');
    Exit(CreateNilValue);
  end;
  ch := Ord(s[1]);
  r := Trunc(args[1].NumberValue);
  g := Trunc(args[2].NumberValue);
  b := Trunc(args[3].NumberValue);
  if (r < 0) or (r > 255) or (g < 0) or (g > 255) or (b < 0) or (b > 255) then
  begin
    RuntimeError('setPaletteColor() color values must be 0..255.');
    Exit(CreateNilValue);
  end;
  FPalette[ch] := Cardinal(b or (g shl 8) or (r shl 16) or $FF000000);
  FPaletteUsed[ch] := True;
  Result := CreateNilValue;
end;

function clearPaletteNative(argCount: integer; args: pValue): TValue;
begin
  FillChar(FPalette, SizeOf(FPalette), 0);
  FillChar(FPaletteUsed, SizeOf(FPaletteUsed), 0);
  Result := CreateNilValue;
end;

function createPaletteSpriteNative(argCount: integer; args: pValue): TValue;
var
  w, h, x, y, i: Integer;
  s: AnsiString;
  bmp: TBitmap;
  row: PRGBQuadArray;
  ch: Byte;
begin
  if argCount <> 3 then
  begin
    RuntimeError('createPaletteSprite() takes 3 arguments (width, height, pixels).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('createPaletteSprite() width and height must be numbers.');
    Exit(CreateNilValue);
  end;
  if not isString(args[2]) then
  begin
    RuntimeError('createPaletteSprite() third argument must be a string.');
    Exit(CreateNilValue);
  end;
  w := Trunc(args[0].NumberValue);
  h := Trunc(args[1].NumberValue);
  if (w <= 0) or (h <= 0) then
  begin
    RuntimeError('createPaletteSprite() width and height must be positive.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(args[2].ObjValue));
  if Length(s) <> w * h then
  begin
    RuntimeError('createPaletteSprite() pixel string length must equal width * height.');
    Exit(CreateNilValue);
  end;

  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(w, h);
    i := 1;
    for y := 0 to h - 1 do
    begin
      row := bmp.ScanLine[y];
      for x := 0 to w - 1 do
      begin
        ch := Ord(s[i]);
        if s[i] = '.' then
          PCardinal(@row[x])^ := $00FF00FF
        else if FPaletteUsed[ch] then
          PCardinal(@row[x])^ := FPalette[ch]
        else
          PCardinal(@row[x])^ := FCurrentPixel;  // fallback to current color
        Inc(i);
      end;
    end;
    bmp.Transparent := True;
    bmp.TransparentColor := TColor($FF00FF);
  except
    bmp.Free;
    raise;
  end;

  Result := CreateNumber(AllocSpriteSlot(bmp));
end;

// -- PNG loader --
// Loads a PNG file from disk and returns a sprite ID. Fully-transparent
// pixels (alpha < 128) are mapped to the magenta colour key so the
// resulting sprite blits with transparency just like createSprite()
// sprites. Partially-transparent pixels are written as opaque RGB
// (no alpha blending; this engine is paletted/keyed by design).
//
// loadSpriteFromPNG(path)
//   path: string. If relative, resolved against the current working
//         directory.
//
// loadSpriteFromPNGRegion(path, sx, sy, sw, sh)
//   Same as above but extracts a sub-rectangle from the PNG. Useful
//   for picking a single character out of a packed sprite sheet.
function LoadPngRegionAsSprite(const APath: string;
  sx, sy, sw, sh: Integer; UseRegion: Boolean;
  out ErrMsg: string): Integer;
var
  png: TPngImage;
  bmp: TBitmap;
  pw, ph, x, y, srcX, srcY: Integer;
  row: PRGBQuadArray;
  alphaScan: PByteArray;
  hasAlpha: Boolean;
  c: TColor;
  r, g, b: Byte;
  transparent: Boolean;
begin
  Result := -1;
  ErrMsg := '';
  if not FileExists(APath) then
  begin
    ErrMsg := 'PNG file not found: ' + APath;
    Exit;
  end;

  png := TPngImage.Create;
  try
    try
      png.LoadFromFile(APath);
    except
      on E: Exception do
      begin
        ErrMsg := 'Failed to load PNG (' + APath + '): ' + E.Message;
        Exit;
      end;
    end;

    if not UseRegion then
    begin
      sx := 0; sy := 0;
      sw := png.Width; sh := png.Height;
    end;
    if (sw <= 0) or (sh <= 0) then
    begin
      ErrMsg := 'PNG region width/height must be positive.';
      Exit;
    end;
    if (sx < 0) or (sy < 0) or
       (sx + sw > png.Width) or (sy + sh > png.Height) then
    begin
      ErrMsg := Format(
        'PNG region (%d,%d %dx%d) outside image bounds (%dx%d).',
        [sx, sy, sw, sh, png.Width, png.Height]);
      Exit;
    end;

    pw := sw; ph := sh;
    hasAlpha := png.TransparencyMode = ptmPartial;

    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32bit;
      bmp.SetSize(pw, ph);
      for y := 0 to ph - 1 do
      begin
        srcY := sy + y;
        row := bmp.ScanLine[y];
        if hasAlpha then
          alphaScan := PByteArray(png.AlphaScanline[srcY])
        else
          alphaScan := nil;
        for x := 0 to pw - 1 do
        begin
          srcX := sx + x;
          transparent := False;
          if alphaScan <> nil then
            transparent := alphaScan[srcX] < 128
          else if png.TransparencyMode = ptmBit then
            transparent := png.Pixels[srcX, srcY] = png.TransparentColor;
          if transparent then
            PCardinal(@row[x])^ := $00FF00FF
          else
          begin
            c := png.Pixels[srcX, srcY]; // TColor: $00BBGGRR
            r := c and $FF;
            g := (c shr 8) and $FF;
            b := (c shr 16) and $FF;
            // Force exact-magenta source pixels off the key by nudging
            // green up by 1, otherwise they'd blit as holes.
            if (r = $FF) and (g = $00) and (b = $FF) then g := 1;
            PCardinal(@row[x])^ :=
              Cardinal(b) or (Cardinal(g) shl 8) or (Cardinal(r) shl 16);
          end;
        end;
      end;
      bmp.Transparent := True;
      bmp.TransparentColor := TColor($FF00FF);
    except
      bmp.Free;
      raise;
    end;

    Result := AllocSpriteSlot(bmp);
  finally
    png.Free;
  end;
end;

function ResolveAssetPath(const APath: string): string;
begin
  if TPath.IsPathRooted(APath) then
    Result := APath
  else
    Result := TPath.Combine(GetCurrentDir, APath);
end;

function loadSpriteFromPNGNative(argCount: integer; args: pValue): TValue;
var
  path: string;
  id: Integer;
  err: string;
begin
  if argCount <> 1 then
  begin
    RuntimeError('loadSpriteFromPNG() takes 1 argument (path).');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('loadSpriteFromPNG() path must be a string.');
    Exit(CreateNilValue);
  end;
  path := ResolveAssetPath(string(ObjStringToAnsiString(pObjString(args[0].ObjValue))));
  id := LoadPngRegionAsSprite(path, 0, 0, 0, 0, False, err);
  if id < 0 then
  begin
    RuntimeError(err);
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(id);
end;

function loadSpriteFromPNGRegionNative(argCount: integer; args: pValue): TValue;
var
  path: string;
  sx, sy, sw, sh, id, i: Integer;
  err: string;
begin
  if argCount <> 5 then
  begin
    RuntimeError('loadSpriteFromPNGRegion() takes 5 arguments (path, x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('loadSpriteFromPNGRegion() path must be a string.');
    Exit(CreateNilValue);
  end;
  for i := 1 to 4 do
    if args[i].ValueKind <> vkNumber then
    begin
      RuntimeError('loadSpriteFromPNGRegion() x, y, w, h must be numbers.');
      Exit(CreateNilValue);
    end;
  path := ResolveAssetPath(string(ObjStringToAnsiString(pObjString(args[0].ObjValue))));
  sx := Trunc(args[1].NumberValue);
  sy := Trunc(args[2].NumberValue);
  sw := Trunc(args[3].NumberValue);
  sh := Trunc(args[4].NumberValue);
  id := LoadPngRegionAsSprite(path, sx, sy, sw, sh, True, err);
  if id < 0 then
  begin
    RuntimeError(err);
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(id);
end;

// -- Tilemap functions --

function createTilemapNative(argCount: integer; args: pValue): TValue;
var
  cols, rows, tw, th: Integer;
begin
  if argCount <> 4 then
  begin
    RuntimeError('createTilemap() takes 4 arguments (cols, rows, tileW, tileH).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('createTilemap() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  cols := Trunc(args[0].NumberValue);
  rows := Trunc(args[1].NumberValue);
  tw := Trunc(args[2].NumberValue);
  th := Trunc(args[3].NumberValue);
  if (cols <= 0) or (rows <= 0) or (tw <= 0) or (th <= 0) then
  begin
    RuntimeError('createTilemap() all arguments must be positive.');
    Exit(CreateNilValue);
  end;
  if FTilemaps = nil then
    FTilemaps := TObjectList<TTilemap>.Create(True);
  Result := CreateNumber(FTilemaps.Add(TTilemap.Create(cols, rows, tw, th)));
end;

function setTileNative(argCount: integer; args: pValue): TValue;
var
  mapId, col, row, spriteId: Integer;
  tm: TTilemap;
begin
  if argCount <> 4 then
  begin
    RuntimeError('setTile() takes 4 arguments (mapId, col, row, spriteId).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) or (args[3].ValueKind <> vkNumber) then
  begin
    RuntimeError('setTile() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  mapId := Trunc(args[0].NumberValue);
  if (FTilemaps = nil) or (mapId < 0) or (mapId >= FTilemaps.Count) then
  begin
    RuntimeError('setTile() invalid tilemap id.');
    Exit(CreateNilValue);
  end;
  tm := FTilemaps[mapId];
  col := Trunc(args[1].NumberValue);
  row := Trunc(args[2].NumberValue);
  spriteId := Trunc(args[3].NumberValue);
  if (col < 0) or (col >= tm.Cols) or (row < 0) or (row >= tm.Rows) then
  begin
    RuntimeError('setTile() col/row out of range.');
    Exit(CreateNilValue);
  end;
  // spriteId -1 = empty, otherwise validate
  if (spriteId >= 0) and ((FSprites = nil) or (spriteId >= FSprites.Count) or (FSprites[spriteId] = nil)) then
  begin
    RuntimeError('setTile() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  tm.Tiles[row * tm.Cols + col] := spriteId;
  Result := CreateNilValue;
end;

function getTileNative(argCount: integer; args: pValue): TValue;
var
  mapId, col, row: Integer;
  tm: TTilemap;
begin
  if argCount <> 3 then
  begin
    RuntimeError('getTile() takes 3 arguments (mapId, col, row).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('getTile() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  mapId := Trunc(args[0].NumberValue);
  if (FTilemaps = nil) or (mapId < 0) or (mapId >= FTilemaps.Count) then
  begin
    RuntimeError('getTile() invalid tilemap id.');
    Exit(CreateNilValue);
  end;
  tm := FTilemaps[mapId];
  col := Trunc(args[1].NumberValue);
  row := Trunc(args[2].NumberValue);
  if (col < 0) or (col >= tm.Cols) or (row < 0) or (row >= tm.Rows) then
    Result := CreateNumber(-1)
  else
    Result := CreateNumber(tm.Tiles[row * tm.Cols + col]);
end;

function drawTilemapNative(argCount: integer; args: pValue): TValue;
var
  mapId, scrollX, scrollY: Integer;
  tm: TTilemap;
  startCol, startRow, endCol, endRow: Integer;
  c, r, tileIdx, dx, dy: Integer;
  bmp: TBitmap;
  sx, sy, px, py, bw, bh: Integer;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawTilemap() takes 3 arguments (mapId, scrollX, scrollY).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawTilemap() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  mapId := Trunc(args[0].NumberValue);
  if (FTilemaps = nil) or (mapId < 0) or (mapId >= FTilemaps.Count) then
  begin
    RuntimeError('drawTilemap() invalid tilemap id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  tm := FTilemaps[mapId];
  scrollX := Trunc(args[1].NumberValue) + FCameraX;
  scrollY := Trunc(args[2].NumberValue) + FCameraY;

  // Calculate visible tile range
  startCol := scrollX div tm.TileW;
  startRow := scrollY div tm.TileH;
  endCol := (scrollX + FRenderTarget.Width) div tm.TileW;
  endRow := (scrollY + FRenderTarget.Height) div tm.TileH;
  if startCol < 0 then startCol := 0;
  if startRow < 0 then startRow := 0;
  if endCol >= tm.Cols then endCol := tm.Cols - 1;
  if endRow >= tm.Rows then endRow := tm.Rows - 1;

  if FSprites = nil then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  for r := startRow to endRow do
    for c := startCol to endCol do
    begin
      tileIdx := tm.Tiles[r * tm.Cols + c];
      if (tileIdx >= 0) and (tileIdx < FSprites.Count) then
      begin
        bmp := FSprites[tileIdx];
        if bmp = nil then Continue;  // freed sprite â€” skip tile
        dx := c * tm.TileW - scrollX;
        dy := r * tm.TileH - scrollY;
        for py := 0 to tm.TileH - 1 do
        begin
          if (dy + py < FClipY1) or (dy + py >= bh) then Continue;
          sy := (py * bmp.Height) div tm.TileH;
          srcRow := bmp.ScanLine[sy];
          dstRow := FRenderTarget.ScanLine[dy + py];
          for px := 0 to tm.TileW - 1 do
          begin
            if (dx + px < FClipX1) or (dx + px >= bw) then Continue;
            sx := (px * bmp.Width) div tm.TileW;
            pixel := PCardinal(@srcRow[sx])^;
            if pixel <> $00FF00FF then
              dstRow[dx + px] := pixel;
          end;
        end;
      end;
    end;
  Result := CreateNilValue;
end;

// -- Sprite utility functions --

function flipSpriteNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
  src, bmp: TBitmap;
  sx, sy, w, h: Integer;
  srcRow, dstRow: PRGBQuadArray;
begin
  if argCount <> 2 then
  begin
    RuntimeError('flipSprite() takes 2 arguments (id, direction).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) then
  begin
    RuntimeError('flipSprite() first argument must be a number.');
    Exit(CreateNilValue);
  end;
  if not isString(args[1]) then
  begin
    RuntimeError('flipSprite() second argument must be "h" or "v".');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('flipSprite() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  src := FSprites[id];
  if src = nil then
  begin
    RuntimeError('flipSprite() sprite has been freed.');
    Exit(CreateNilValue);
  end;
  var dirStr := ObjStringToAnsiString(pObjString(args[1].ObjValue));
  if (dirStr <> 'h') and (dirStr <> 'v') then
  begin
    RuntimeError('flipSprite() direction must be "h" or "v".');
    Exit(CreateNilValue);
  end;
  w := src.Width;
  h := src.Height;
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(w, h);
    if dirStr = 'v' then
    begin
      // Vertical flip
      for sy := 0 to h - 1 do
      begin
        srcRow := src.ScanLine[sy];
        dstRow := bmp.ScanLine[h - 1 - sy];
        Move(srcRow^, dstRow^, w * SizeOf(TRGBQuadRec));
      end;
    end
    else
    begin
      // Horizontal flip (default)
      for sy := 0 to h - 1 do
      begin
        srcRow := src.ScanLine[sy];
        dstRow := bmp.ScanLine[sy];
        for sx := 0 to w - 1 do
          PCardinal(@dstRow[sx])^ := PCardinal(@srcRow[w - 1 - sx])^;
      end;
    end;
    bmp.Transparent := True;
    bmp.TransparentColor := TColor($FF00FF);
  except
    bmp.Free;
    raise;
  end;
  Result := CreateNumber(AllocSpriteSlot(bmp));
end;

function freeSpriteNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('freeSprite() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('freeSprite() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('freeSprite() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  if FSprites[id] = nil then
  begin
    RuntimeError('freeSprite() sprite already freed.');
    Exit(CreateNilValue);
  end;
  FSprites[id] := nil;  // OwnsObjects frees the TBitmap
  if FSpriteFreelist = nil then
    FSpriteFreelist := TList<Integer>.Create;
  FSpriteFreelist.Add(id);
  Result := CreateNilValue;
end;

// --- Offscreen render surfaces ---

function AllocSurfaceSlot(bmp: TBitmap): Integer;
var
  idx: Integer;
begin
  if FSurfaces = nil then
    FSurfaces := TObjectList<TBitmap>.Create(True);
  if FSurfaceFreelist = nil then
    FSurfaceFreelist := TList<Integer>.Create;
  if FSurfaceFreelist.Count > 0 then
  begin
    idx := FSurfaceFreelist[FSurfaceFreelist.Count - 1];
    FSurfaceFreelist.Delete(FSurfaceFreelist.Count - 1);
    FSurfaces[idx] := bmp;
    Result := idx;
  end else
    Result := FSurfaces.Add(bmp);
end;

function createSurfaceNative(argCount: integer; args: pValue): TValue;
var
  w, h: Integer;
  bmp: TBitmap;
begin
  if argCount <> 2 then
  begin
    RuntimeError('createSurface() takes 2 arguments (width, height).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('createSurface() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  w := Trunc(args[0].NumberValue);
  h := Trunc(args[1].NumberValue);
  if (w <= 0) or (h <= 0) or (w > 4096) or (h > 4096) then
  begin
    RuntimeError('createSurface() dimensions must be 1..4096.');
    Exit(CreateNilValue);
  end;
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.SetSize(w, h);
  ClearBuffer(bmp);
  Result := CreateNumber(AllocSurfaceSlot(bmp));
end;

function setRenderTargetNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('setRenderTarget() takes 1 argument (surface id, or -1 for back buffer).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('setRenderTarget() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if id < 0 then
  begin
    // Target main back buffer
    EnsureBackBuffer;
    FRenderTarget := FBackBuffer;
    FRenderTargetId := -1;
  end else
  begin
    if (FSurfaces = nil) or (id >= FSurfaces.Count) or (FSurfaces[id] = nil) then
    begin
      RuntimeError('setRenderTarget() invalid surface id.');
      Exit(CreateNilValue);
    end;
    FRenderTarget := FSurfaces[id];
    FRenderTargetId := id;
  end;
  // Reset clip to new target bounds
  FClipActive := False;
  ResetClipToTarget;
  Result := CreateNilValue;
end;

function drawSurfaceNative(argCount: integer; args: pValue): TValue;
var
  id, x, y, sx, sy, ax, ay: Integer;
  bw, bh: Integer;
  src: TBitmap;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawSurface() takes 3 arguments (id, x, y).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawSurface() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSurfaces = nil) or (id < 0) or (id >= FSurfaces.Count) or (FSurfaces[id] = nil) then
  begin
    RuntimeError('drawSurface() invalid surface id.');
    Exit(CreateNilValue);
  end;
  src := FSurfaces[id];
  EnsureBackBuffer;
  x := Trunc(args[1].NumberValue) - FCameraX;
  y := Trunc(args[2].NumberValue) - FCameraY;
  bw := FClipX2;
  bh := FClipY2;
  for sy := 0 to src.Height - 1 do
  begin
    ay := y + sy;
    if (ay < FClipY1) or (ay >= bh) then Continue;
    srcRow := src.ScanLine[sy];
    dstRow := FRenderTarget.ScanLine[ay];
    for sx := 0 to src.Width - 1 do
    begin
      ax := x + sx;
      if (ax < FClipX1) or (ax >= bw) then Continue;
      pixel := PCardinal(@srcRow[sx])^;
      // Skip fully transparent pixels (alpha = 0, i.e. cleared buffer)
      if (pixel and $FF000000) = 0 then Continue;
      dstRow[ax] := pixel;
    end;
  end;
  Result := CreateNilValue;
end;

function freeSurfaceNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('freeSurface() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('freeSurface() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(args[0].NumberValue);
  if (FSurfaces = nil) or (id < 0) or (id >= FSurfaces.Count) then
  begin
    RuntimeError('freeSurface() invalid surface id.');
    Exit(CreateNilValue);
  end;
  if FSurfaces[id] = nil then
  begin
    RuntimeError('freeSurface() surface already freed.');
    Exit(CreateNilValue);
  end;
  // If currently targeting this surface, reset to back buffer
  if FRenderTargetId = id then
  begin
    EnsureBackBuffer;
    FRenderTarget := FBackBuffer;
    FRenderTargetId := -1;
  end;
  FSurfaces[id] := nil;
  if FSurfaceFreelist = nil then
    FSurfaceFreelist := TList<Integer>.Create;
  FSurfaceFreelist.Add(id);
  Result := CreateNilValue;
end;

procedure RegisterCanvasNatives;
begin
  // Clear sprites, tilemaps, surfaces, and palette from previous run
  if FSprites <> nil then
    FSprites.Clear;
  if FSpriteFreelist <> nil then
    FSpriteFreelist.Clear;
  if FSurfaces <> nil then
    FSurfaces.Clear;
  if FSurfaceFreelist <> nil then
    FSurfaceFreelist.Clear;
  if FTilemaps <> nil then
    FTilemaps.Clear;
  FillChar(FPalette, SizeOf(FPalette), 0);
  FillChar(FPaletteUsed, SizeOf(FPaletteUsed), 0);
  FCameraX := 0;
  FCameraY := 0;
  FRenderTargetId := -1;
  FClipActive := False;
  ClearAllKeyState;
  defineNative('canvasWidth', canvasWidthNative, 0);
  defineNative('canvasHeight', canvasHeightNative, 0);
  defineNative('clearCanvas', clearCanvasNative, -1);
  defineNative('setColor', setColorNative, 3);
  defineNative('fillRect', fillRectNative, 4);
  defineNative('drawText', drawTextNative, -1);
  defineNative('present', presentNative, 0);
  defineNative('drawLine', drawLineNative, 4);
  defineNative('drawRect', drawRectNative, 4);
  defineNative('drawCircle', drawCircleNative, 3);
  defineNative('fillCircle', fillCircleNative, 3);
  defineNative('drawPixel', drawPixelNative, 2);
  defineNative('createSprite', createSpriteNative, 3);
  defineNative('drawSprite', drawSpriteNative, 3);
  defineNative('drawSpriteScaled', drawSpriteScaledNative, 4);
  defineNative('createTilemap', createTilemapNative, 4);
  defineNative('setTile', setTileNative, 4);
  defineNative('drawTilemap', drawTilemapNative, 3);
  defineNative('setPaletteColor', setPaletteColorNative, 4);
  defineNative('clearPalette', clearPaletteNative, 0);
  defineNative('createPaletteSprite', createPaletteSpriteNative, 3);
  defineNative('loadSpriteFromPNG', loadSpriteFromPNGNative, 1);
  defineNative('loadSpriteFromPNGRegion', loadSpriteFromPNGRegionNative, 5);
  defineNative('getTile', getTileNative, 3);
  defineNative('flipSprite', flipSpriteNative, 2);
  defineNative('freeSprite', freeSpriteNative, 1);
  defineNative('drawSpriteRotated', drawSpriteRotatedNative, 5);
  defineNative('setCamera', setCameraNative, 2);
  defineNative('createSurface', createSurfaceNative, 2);
  defineNative('setRenderTarget', setRenderTargetNative, 1);
  defineNative('drawSurface', drawSurfaceNative, 3);
  defineNative('freeSurface', freeSurfaceNative, 1);
  defineNative('setClipRect', setClipRectNative, 4);
  defineNative('clearClipRect', clearClipRectNative, 0);
  defineNative('measureText', measureTextNative, -1);
  defineNative('keyHeld', keyHeldNative, 1);
  defineNative('mouseX', mouseXNative, 0);
  defineNative('mouseY', mouseYNative, 0);
  defineNative('mouseDown', mouseDownNative, 1);
  defineNative('spriteWidth', spriteWidthNative, 1);
  defineNative('spriteHeight', spriteHeightNative, 1);
end;

end.
