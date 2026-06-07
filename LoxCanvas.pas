unit LoxCanvas;

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Forms, Vcl.Graphics, Classes, Chunk_Types;

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
  // component palette � instantiate from code via InitCanvas().
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
procedure DetachCanvasFromHost;
procedure RegisterCanvasNatives;
procedure SetKeyState(const KeyName: string; Down: Boolean);
procedure ClearAllKeyState;
// Reset the per-frame edge-trigger state (FKeysPressed,
// FMouseBtnClicked). Called by processMessagesNative immediately before
// pumping Windows messages so that keyPressed() / mouseClicked() report
// only transitions that occurred during the current frame's pump.
procedure ClearFrameEdges;
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

  // Logical resolution. All Lox draw calls operate in this coordinate
  // space. Defaults to 320x240 but a script can mutate it at startup
  // via setCanvasSize(w, h). At present() time the back buffer is scaled
  // by the largest integer factor that fits the host control, centered,
  // with black letterbox/pillarbox bars filling any remainder.
  //
  // These are intentionally module-level vars (not consts) so that
  // setCanvasSize can change them between frames; EnsureBackBuffer +
  // present's swap path both check and resize the underlying bitmaps
  // to match on the next frame.

// (moved out of const block - see var block below)

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
  LOGICAL_W: Integer = 320;
  LOGICAL_H: Integer = 240;
  FGameSurface: TLoxGameCanvas;
  FBackBuffer: TBitmap;
  FFrontBuffer: TBitmap;   // presented frame — never partially drawn
  FPresentBuffer: TBitmap; // client-sized staging buffer for one-shot blits
  FRenderTarget: TBitmap;  // current draw target (FBackBuffer or a surface)
  FRenderTargetId: Integer; // -1 = back buffer, >= 0 = surface index
  FCurrentPixel: Cardinal;  // precomputed BGRA pixel for ScanLine writes
  FCameraX: Integer;
  FCameraY: Integer;
  FSprites: TObjectList<TBitmap>;
  FSpriteFreelist: TList<Integer>;
  // Parallel to FSprites: true when the sprite contains no magenta
  // ($00FF00FF) transparency-key pixels. Set once at slot allocation
  // time and used by drawSprite to take a per-row Move() fast path.
  FSpriteOpaque: TList<Boolean>;
  FSurfaces: TObjectList<TBitmap>;
  FSurfaceFreelist: TList<Integer>;
  FTilemaps: TObjectList<TTilemap>;
  FHasFrontFrame: Boolean;  // true once present() has been called at least once
  FClipX1, FClipY1, FClipX2, FClipY2: Integer;  // clip rect (inclusive x1,y1 to exclusive x2,y2)
  FClipActive: Boolean;  // true when user has set a clip rect
  FPalette: array[0..255] of Cardinal;   // char -> BGRA color
  FPaletteUsed: array[0..255] of Boolean;
  FKeysHeld: TDictionary<string, Boolean>;
  // Edge-trigger set: keys that transitioned from up -> down since the
  // last ClearFrameEdges. Cleared at the top of each processMessages()
  // tick so keyPressed(name) returns true exactly once per physical
  // key press, no matter how many times the script polls it within
  // that frame.
  FKeysPressed: TDictionary<string, Boolean>;
  // Latest known mouse position in LOGICAL pixel coords (clamped to the
  // 320x240 playfield) and held state for the three primary buttons.
  // Updated by TLoxGameCanvas.MouseMove/Down/Up and queried by the
  // mouseX/mouseY/mouseDown natives.
  FMouseX: Integer;
  FMouseY: Integer;
  FMouseBtn: array[0..2] of Boolean;
  // Edge-trigger flags for the three mouse buttons. Set in MouseDown
  // when the button transitions from up -> down; cleared in
  // ClearFrameEdges. mouseClicked(btn) reads these.
  FMouseBtnClicked: array[0..2] of Boolean;
  // -- Glyph cache for drawText() --
  // Built once from the 5x7 font table. For each printable ASCII char
  // (32..126) and each of the 7 rows, we pre-extract which of the 5
  // columns have pixels set. drawText then iterates only those set
  // pixels instead of testing all 5 columns with shift-and-mask, and
  // sparse rows (the common case) need just 0-3 writes instead of 5
  // bit tests + branches.
  FGlyphCount: array[32..126, 0..6] of Byte;            // pixels per row
  FGlyphCol:   array[32..126, 0..6, 0..4] of Byte;      // column indices
  FGlyphCacheBuilt: Boolean;

procedure ResetClipToTarget; forward;

// Fast 32-bit fill: rep stosd. Bit-identical to a scalar `for i := 0 to
// Count-1 do P[i] := Value` loop but avoids per-pixel loop overhead on
// the hot rasterizer paths (fillRect / clearCanvas / fillCircle.HLine).
{$IFDEF CPUX64}
procedure FillDWord(var Dest; Count: Integer; Value: LongWord);
asm
  // Win64 ABI: RCX=@Dest, EDX=Count, R8D=Value
  PUSH RDI
  MOV  RDI, RCX
  MOV  EAX, R8D
  MOV  ECX, EDX
  TEST ECX, ECX
  JLE  @@done
  REP  STOSD
@@done:
  POP  RDI
end;
{$ELSE}
procedure FillDWord(var Dest; Count: Integer; Value: LongWord);
asm
  // Win32 fastcall: EAX=@Dest, EDX=Count, ECX=Value.
  PUSH EDI
  MOV  EDI, EAX
  MOV  EAX, ECX
  MOV  ECX, EDX
  TEST ECX, ECX
  JLE  @@done
  REP  STOSD
@@done:
  POP  EDI
end;
{$ENDIF}

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
  // it to match the host control � that's what the present() scale step
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
  // If the current render target is a surface that no longer exists
  // (freed, surfaces list cleared, etc.), fall back to the back buffer.
  // This closes a small dangling-pointer window after RegisterCanvasNatives
  // or any other path that mutates FSurfaces without rebinding.
  if (FRenderTargetId >= 0) and
     ((FSurfaces = nil) or (FRenderTargetId >= FSurfaces.Count) or
      (FSurfaces[FRenderTargetId] = nil)) then
  begin
    FRenderTarget := FBackBuffer;
    FRenderTargetId := -1;
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
// any axis, we still use scale=1 and clip � the result is acceptable for
// a tiny window and avoids the visual disaster of fractional scaling.
//
// Pixel-art invariants (please preserve when changing this routine):
//   1. All drawing into FBackBuffer/FFrontBuffer happens at integer
//      logical coordinates. Every native (drawSprite, fillRect, etc.)
//      Trunc()s its Lox-double arguments before writing, so there is no
//      sub-pixel sampling anywhere upstream of this point.
//   2. The scale step uses StretchBlt with SetStretchBltMode(COLORONCOLOR),
//      which on Windows is nearest-neighbor for 32-bit blits. HALFTONE
//      would smooth � DO NOT use it. The final stage->client copy is a
//      1:1 BitBlt, so no resampling happens there.
//   3. cw/ch should be PHYSICAL client pixels. Callers pass values from
//      Windows.GetClientRect (true device pixels) rather than
//      TControl.ClientWidth, so per-monitor DPI virtualization can't
//      insert a hidden resample between us and the screen. The EXE's
//      DPI-awareness in the manifest still matters: if Windows is auto-
//      scaling a non-DPI-aware app, it bitmap-stretches our output AFTER
//      we deliver it. For pixel-art crispness, declare the EXE
//      per-monitor DPI-aware (or System DPI aware) in the manifest, and
//      either set the host form's Scaled := False or leave it on with
//      integer scale factors only.
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
  // COLORONCOLOR == nearest-neighbor for 32-bit blits. Required for
  // pixel-art crispness; HALFTONE would smooth and ruin the look.
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
  // Tell the dialog manager we want all keys ourselves � including
  // arrows and Tab � so they don't trigger focus traversal or other
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
  begin
    // Edge-detect: only the up->down transition counts as a click.
    // Spurious repeat-downs (e.g. WM_LBUTTONDOWN delivered twice) won't
    // re-fire mouseClicked() within the same frame.
    if not FMouseBtn[btnIdx] then
      FMouseBtnClicked[btnIdx] := True;
    FMouseBtn[btnIdx] := True;
  end;
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
    Key := 0;  // consume so it doesn't bubble to the form / editor --need to revisit this.
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
    begin
      // Use physical client pixels (see PresentScaled docs, invariant #3).
      var rc: TRect;
      Winapi.Windows.GetClientRect(Handle, rc);
      PresentScaled(DC, rc.Right - rc.Left, rc.Bottom - rc.Top);
    end
    else
    begin
      // No frame yet � fill black so we don't show garbage. Use the
      // BeginPaint DC directly (not the VCL Canvas wrapper, which can
      // lazily acquire its own DC and drift from the paint lifecycle).
      Winapi.Windows.FillRect(DC, ClientRect, HBRUSH(GetStockObject(BLACK_BRUSH)));
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
var
  hostW, hostH: Integer;
begin
  FCurrentPixel := $FFFFFFFF;  // opaque white in BGRA
  FreeAndNil(FGameSurface);
  // Adopt the host's current client size as the logical canvas
  // resolution. Resize the host (e.g. the game form) before pressing
  // Run to change resolution. Clamp to sane bounds.
  hostW := AParent.ClientWidth;
  hostH := AParent.ClientHeight;
  if hostW < 32   then hostW := 32;
  if hostH < 32   then hostH := 32;
  if hostW > 4096 then hostW := 4096;
  if hostH > 4096 then hostH := 4096;
  LOGICAL_W := hostW;
  LOGICAL_H := hostH;
  // Owner = nil so FreeCanvas owns lifetime; otherwise the host form
  // would destroy FGameSurface first and leave a dangling pointer for
  // FreeCanvas (Form4.OnDestroy runs AFTER frmGame on shutdown).
  FGameSurface := TLoxGameCanvas.Create(nil);
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
  FreeAndNil(FKeysPressed);
  FKeysPressed := TDictionary<string, Boolean>.Create;
end;

procedure DetachCanvasFromHost;
begin
  // Drop the parent pointer so the canvas can outlive the host form.
  // Application destroys frmGame BEFORE Form4, but FGameSurface is freed
  // by Form4.OnDestroy (FreeCanvas). Without this, FGameSurface.FParent
  // would dangle and FreeAndNil would AV inside the inherited
  // TWinControl destructor.
  if Assigned(FGameSurface) then
    FGameSurface.Parent := nil;
end;

procedure FreeCanvas;
begin
  FreeAndNil(FTilemaps);
  FreeAndNil(FSpriteFreelist);
  FreeAndNil(FSpriteOpaque);
  FreeAndNil(FSprites);
  FreeAndNil(FSurfaceFreelist);
  FreeAndNil(FSurfaces);
  FreeAndNil(FFrontBuffer);
  FreeAndNil(FPresentBuffer);
  FreeAndNil(FBackBuffer);
  FRenderTarget := nil;
  FRenderTargetId := -1;
  FreeAndNil(FKeysHeld);
  FreeAndNil(FKeysPressed);
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

// setCanvasSize(w, h) - change the logical drawing surface dimensions.
// Sizes are clamped to [32..4096] in each axis. Safe to call between
// frames; the back buffer is resized on the next EnsureBackBuffer pass
// and the front buffer is resized lazily on the next present(). All
// existing sprites, surfaces, and tilemaps are preserved unchanged.
// Typical use: call once at the top of a script before reading
// canvasWidth()/canvasHeight().
function setCanvasSizeNative(argCount: integer; args: pValue): TValue;
var
  newW, newH: Integer;
  host: TWinControl;
  hostForm: TCustomForm;
begin
  Result := CreateNilValue;
  if argCount < 2 then Exit;
  newW := Trunc(GetNumber(args[0]));
  newH := Trunc(GetNumber(args[1]));
  if newW < 32 then newW := 32 else if newW > 4096 then newW := 4096;
  if newH < 32 then newH := 32 else if newH > 4096 then newH := 4096;
  LOGICAL_W := newW;
  LOGICAL_H := newH;
  // Resize the host window (the game form) so the visible area matches
  // the new logical dimensions. The canvas itself is alClient and will
  // follow automatically. We walk up to the parent form rather than
  // resizing FGameSurface directly so window chrome stays correct.
  if FGameSurface <> nil then
  begin
    host := FGameSurface.Parent;
    if host <> nil then
    begin
      hostForm := GetParentForm(host);
      if hostForm <> nil then
      begin
        hostForm.ClientWidth  := newW;
        hostForm.ClientHeight := newH;
      end
      else
      begin
        host.Width  := newW;
        host.Height := newH;
      end;
    end;
  end;
  // Recreate / resize the back buffer immediately so subsequent draw
  // calls in the same frame see the new dimensions.
  EnsureBackBuffer;
  // Resize front buffer too if it exists; present() will allocate one
  // at the right size if it doesn't.
  if (FFrontBuffer <> nil) and (FFrontBuffer <> FBackBuffer) and
     ((FFrontBuffer.Width <> LOGICAL_W) or (FFrontBuffer.Height <> LOGICAL_H)) then
    FFrontBuffer.SetSize(LOGICAL_W, LOGICAL_H);
  // Drop any user clip rect; old coordinates may be out of bounds.
  FClipActive := False;
  ResetClipToTarget;
end;

procedure ClearBuffer(ABmp: TBitmap);
var
  y: Integer;
begin
  for y := 0 to ABmp.Height - 1 do
    FillChar(ABmp.ScanLine[y]^, ABmp.Width * SizeOf(Cardinal), 0);
end;

// Unchecked pixel write — caller must guarantee x,y are in bounds.
// Use this in internal native blitters where clipping is done at a higher level.
procedure PutPixelUnchecked(x, y: Integer; pixel: Cardinal); inline;
begin
  PCardinal(FRenderTarget.ScanLine[y])[x] := pixel;
end;

function clearCanvasNative(argCount: integer; args: pValue): TValue;
var
  r, g, b: Integer;
  pixel: Cardinal;
  y, w, h: Integer;
  row: PCardinal;
begin
  EnsureBackBuffer;
  if argCount = 0 then
    ClearBuffer(FRenderTarget)
  else if argCount = 3 then
  begin
    if (not isNumber(args[0])) or (not isNumber(args[1])) or
       (not isNumber(args[2])) then
    begin
      RuntimeError('clearCanvas() color arguments must be numbers.');
      Exit(CreateNilValue);
    end;
    r := Trunc(GetNumber(args[0])) and $FF;
    g := Trunc(GetNumber(args[1])) and $FF;
    b := Trunc(GetNumber(args[2])) and $FF;
    pixel := Cardinal(b or (g shl 8) or (r shl 16) or $FF000000);
    w := FRenderTarget.Width;
    h := FRenderTarget.Height;
    // FillDWord compiles to `rep stosd`: one 32-bit store per pixel
    // with no loop overhead in the Pascal code.
    for y := 0 to h - 1 do
    begin
      row := FRenderTarget.ScanLine[y];
      FillDWord(row^, w, pixel);
    end;
  end
  else
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('setColor() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  r := Trunc(GetNumber(args[0]));
  g := Trunc(GetNumber(args[1]));
  b := Trunc(GetNumber(args[2]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) then
  begin
    RuntimeError('setCamera() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  FCameraX := Trunc(GetNumber(args[0]));
  FCameraY := Trunc(GetNumber(args[1]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('setClipRect() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[0]));
  y := Trunc(GetNumber(args[1]));
  w := Trunc(GetNumber(args[2]));
  h := Trunc(GetNumber(args[3]));
  // Clamp to target bounds
  if x < 0 then begin w := w + x; x := 0; end;
  if y < 0 then begin h := h + y; y := 0; end;
  if x + w > FRenderTarget.Width then w := FRenderTarget.Width - x;
  if y + h > FRenderTarget.Height then h := FRenderTarget.Height - y;
  if (w <= 0) or (h <= 0) then
  begin
    // Degenerate clip � nothing will draw
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
  x1, y1, x2, y2, py: Integer;
  row: PCardinal;
begin
  if argCount <> 4 then
  begin
    RuntimeError('fillRect() takes 4 arguments (x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('fillRect() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[0])) - FCameraX;
  y := Trunc(GetNumber(args[1])) - FCameraY;
  w := Trunc(GetNumber(args[2]));
  h := Trunc(GetNumber(args[3]));
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
  // FillDWord (rep stosd) writes one 32-bit pixel per cycle with no
  // per-pixel loop overhead. Bit-identical to the prior scalar loop.
  for py := y1 to y2 - 1 do
  begin
    row := FRenderTarget.ScanLine[py];
    FillDWord(row[x1], x2 - x1, FCurrentPixel);
  end;
  Result := CreateNilValue;
end;

function drawTextNative(argCount: integer; args: pValue): TValue;
const
  // Built-in 5x7 bitmap font - printable ASCII 32..126.
  // Each glyph = 7 bytes (rows top-to-bottom), bit4=leftmost, bit0=rightmost.
  // Used only by BuildGlyphCache below to populate FGlyphCount/FGlyphCol.
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
  x, y, scale, i, gx, gy, charIdx, k: Integer;
  ax, ay, px, py, bw, bh: Integer;
  s: AnsiString;
  ch: Byte;
  rowBits: Byte;
  cnt: Byte;
  dstRow: PCardinal;
  fullyVisible: Boolean;
begin
  if (argCount < 3) or (argCount > 4) then
  begin
    RuntimeError('drawText() takes 3 or 4 arguments (x, y, text [, scale]).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) then
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
    if not isNumber(args[3]) then
    begin
      RuntimeError('drawText() scale must be a number.');
      Exit(CreateNilValue);
    end;
    scale := Trunc(GetNumber(args[3]));
    if scale < 1 then scale := 1;
  end;
  EnsureBackBuffer;

  // Lazily build the glyph cache on first call.
  if not FGlyphCacheBuilt then
  begin
    for i := FONT_FIRST to FONT_LAST do
    begin
      charIdx := (i - FONT_FIRST) * FONT_H;
      for gy := 0 to FONT_H - 1 do
      begin
        rowBits := FONT_DATA[charIdx + gy];
        cnt := 0;
        for gx := 0 to FONT_W - 1 do
          if (rowBits and ($10 shr gx)) <> 0 then
          begin
            FGlyphCol[i, gy, cnt] := gx;
            Inc(cnt);
          end;
        FGlyphCount[i, gy] := cnt;
      end;
    end;
    FGlyphCacheBuilt := True;
  end;

  x := Trunc(GetNumber(args[0])) - FCameraX;
  y := Trunc(GetNumber(args[1])) - FCameraY;
  s := ObjStringToAnsiString(pObjString(GetObject(args[2])));
  bw := FClipX2;
  bh := FClipY2;

  if scale = 1 then
  begin
    // Hot path: scale=1. For each glyph, branch once on whether it is
    // fully inside the clip rect. If so, the inner loop has zero clip
    // branches and writes only the set pixels via FGlyphCol.
    for i := 1 to Length(s) do
    begin
      ch := Ord(s[i]);
      if (ch < FONT_FIRST) or (ch > FONT_LAST) then
        ch := FONT_FIRST;

      // Skip fully off-screen glyphs entirely.
      if (x + FONT_W <= FClipX1) or (x >= bw) or
         (y + FONT_H <= FClipY1) or (y >= bh) then
      begin
        x := x + FONT_ADVANCE;
        Continue;
      end;

      fullyVisible := (x >= FClipX1) and (x + FONT_W <= bw) and
                      (y >= FClipY1) and (y + FONT_H <= bh);

      if fullyVisible then
      begin
        for gy := 0 to FONT_H - 1 do
        begin
          cnt := FGlyphCount[ch, gy];
          if cnt = 0 then Continue;
          dstRow := FRenderTarget.ScanLine[y + gy];
          for k := 0 to cnt - 1 do
            dstRow[x + FGlyphCol[ch, gy, k]] := FCurrentPixel;
        end;
      end
      else
      begin
        // Partially clipped glyph: per-pixel checks.
        for gy := 0 to FONT_H - 1 do
        begin
          cnt := FGlyphCount[ch, gy];
          if cnt = 0 then Continue;
          ay := y + gy;
          if (ay < FClipY1) or (ay >= bh) then Continue;
          dstRow := FRenderTarget.ScanLine[ay];
          for k := 0 to cnt - 1 do
          begin
            ax := x + FGlyphCol[ch, gy, k];
            if (ax >= FClipX1) and (ax < bw) then
              dstRow[ax] := FCurrentPixel;
          end;
        end;
      end;
      x := x + FONT_ADVANCE;
    end;
  end
  else
  begin
    // Scaled path. Each set glyph pixel becomes a scale x scale block.
    for i := 1 to Length(s) do
    begin
      ch := Ord(s[i]);
      if (ch < FONT_FIRST) or (ch > FONT_LAST) then
        ch := FONT_FIRST;
      for gy := 0 to FONT_H - 1 do
      begin
        cnt := FGlyphCount[ch, gy];
        if cnt = 0 then Continue;
        for py := 0 to scale - 1 do
        begin
          ay := y + gy * scale + py;
          if (ay < FClipY1) or (ay >= bh) then Continue;
          dstRow := FRenderTarget.ScanLine[ay];
          for k := 0 to cnt - 1 do
          begin
            for px := 0 to scale - 1 do
            begin
              ax := x + FGlyphCol[ch, gy, k] * scale + px;
              if (ax >= FClipX1) and (ax < bw) then
                dstRow[ax] := FCurrentPixel;
            end;
          end;
        end;
      end;
      x := x + FONT_ADVANCE * scale;
    end;
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
  if not isObject(args[0]) then
  begin
    RuntimeError('measureText() first argument must be a string.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(GetObject(args[0])));
  textLen := Length(s);
  scale := 1;
  if argCount = 2 then
  begin
    if not isNumber(args[1]) then
    begin
      RuntimeError('measureText() scale must be a number.');
      Exit(CreateNilValue);
    end;
    scale := Trunc(GetNumber(args[1]));
    if scale < 1 then scale := 1;
  end;
  Result := CreateNumber(textLen * 6 * scale);
end;

procedure SetKeyState(const KeyName: string; Down: Boolean);
var
  was: Boolean;
begin
  if FKeysHeld = nil then
    FKeysHeld := TDictionary<string, Boolean>.Create;
  was := False;
  FKeysHeld.TryGetValue(KeyName, was);
  FKeysHeld.AddOrSetValue(KeyName, Down);
  // Edge-detect up->down so that auto-repeat (Windows resends WM_KEYDOWN
  // while a key is held) doesn't keep re-flagging keyPressed.
  if Down and not was then
  begin
    if FKeysPressed = nil then
      FKeysPressed := TDictionary<string, Boolean>.Create;
    FKeysPressed.AddOrSetValue(KeyName, True);
  end;
end;

procedure ClearAllKeyState;
var
  i: Integer;
begin
  if FKeysHeld <> nil then
    FKeysHeld.Clear;
  if FKeysPressed <> nil then
    FKeysPressed.Clear;
  for i := 0 to High(FMouseBtn) do
  begin
    FMouseBtn[i] := False;
    FMouseBtnClicked[i] := False;
  end;
end;

procedure ClearFrameEdges;
var
  i: Integer;
begin
  if FKeysPressed <> nil then
    FKeysPressed.Clear;
  for i := 0 to High(FMouseBtnClicked) do
    FMouseBtnClicked[i] := False;
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
  if not isObject(args[0]) then
  begin
    RuntimeError('keyHeld() argument must be a string.');
    Exit(CreateNilValue);
  end;
  name := ObjStringToAnsiString(pObjString(GetObject(args[0])));
  held := False;
  if (FKeysHeld <> nil) then
    FKeysHeld.TryGetValue(name, held);
  Result := CreateBoolean(held);
end;

function keyPressedNative(argCount: integer; args: pValue): TValue;
var
  name: string;
  pressed: Boolean;
begin
  if argCount <> 1 then
  begin
    RuntimeError('keyPressed() takes 1 argument (key name).');
    Exit(CreateNilValue);
  end;
  if not isObject(args[0]) then
  begin
    RuntimeError('keyPressed() argument must be a string.');
    Exit(CreateNilValue);
  end;
  name := ObjStringToAnsiString(pObjString(GetObject(args[0])));
  pressed := False;
  if (FKeysPressed <> nil) then
    FKeysPressed.TryGetValue(name, pressed);
  Result := CreateBoolean(pressed);
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
  if not isNumber(args[0]) then
  begin
    RuntimeError('mouseDown() argument must be a number.');
    Exit(CreateNilValue);
  end;
  btn := Trunc(GetNumber(args[0]));
  if (btn < 0) or (btn > 2) then
  begin
    RuntimeError('mouseDown() button must be 0..2.');
    Exit(CreateNilValue);
  end;
  Result := CreateBoolean(FMouseBtn[btn]);
end;

function mouseClickedNative(argCount: integer; args: pValue): TValue;
var
  btn: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('mouseClicked() takes 1 argument (button 0=left, 1=right, 2=middle).');
    Exit(CreateNilValue);
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('mouseClicked() argument must be a number.');
    Exit(CreateNilValue);
  end;
  btn := Trunc(GetNumber(args[0]));
  if (btn < 0) or (btn > 2) then
  begin
    RuntimeError('mouseClicked() button must be 0..2.');
    Exit(CreateNilValue);
  end;
  Result := CreateBoolean(FMouseBtnClicked[btn]);
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
  if not isNumber(args[0]) then
  begin
    RuntimeError('spriteWidth() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
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
  if not isNumber(args[0]) then
  begin
    RuntimeError('spriteHeight() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
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
    // Use Windows.GetClientRect to read true physical client pixels.
    // TControl.ClientWidth can be affected by VCL per-monitor DPI scaling;
    // GetClientRect is always device pixels, which is what BitBlt needs.
    var rc: TRect;
    Winapi.Windows.GetClientRect(FGameSurface.Handle, rc);
    PresentScaled(dc, rc.Right - rc.Left, rc.Bottom - rc.Top);
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('drawLine() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x1 := Trunc(GetNumber(args[0])) - FCameraX;
  y1 := Trunc(GetNumber(args[1])) - FCameraY;
  x2 := Trunc(GetNumber(args[2])) - FCameraX;
  y2 := Trunc(GetNumber(args[3])) - FCameraY;
  bw := FClipX2;
  bh := FClipY2;
  // Bresenham's line algorithm — crisp single-pixel line
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
  bw, bh, py: Integer;
  px0, px1, py0, py1: Integer;
begin
  if argCount <> 4 then
  begin
    RuntimeError('drawRect() takes 4 arguments (x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('drawRect() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[0])) - FCameraX;
  y := Trunc(GetNumber(args[1])) - FCameraY;
  w := Trunc(GetNumber(args[2]));
  h := Trunc(GetNumber(args[3]));
  if (w <= 0) or (h <= 0) then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Pre-clip the horizontal x-range once for the top/bottom edges so
  // the per-pixel loop has no clip branches; FillDWord (rep stosd)
  // writes one 32-bit pixel per cycle.
  px0 := x;       if px0 < FClipX1 then px0 := FClipX1;
  px1 := x + w;   if px1 > bw      then px1 := bw;
  // Top edge
  if (px0 < px1) and (y >= FClipY1) and (y < bh) then
    FillDWord(PCardinal(FRenderTarget.ScanLine[y])[px0], px1 - px0, FCurrentPixel);
  // Bottom edge (skip if h = 1; the top edge already covered it)
  if (px0 < px1) and (h > 1) and (y + h - 1 >= FClipY1) and (y + h - 1 < bh) then
    FillDWord(PCardinal(FRenderTarget.ScanLine[y + h - 1])[px0], px1 - px0, FCurrentPixel);
  // Pre-clip the vertical y-range once for the left/right edges so the
  // per-row loop has no clip branches.
  py0 := y + 1;     if py0 < FClipY1 then py0 := FClipY1;
  py1 := y + h - 2; if py1 >= bh     then py1 := bh - 1;
  // Left edge
  if (x >= FClipX1) and (x < bw) then
    for py := py0 to py1 do
      PCardinal(FRenderTarget.ScanLine[py])[x] := FCurrentPixel;
  // Right edge
  if (x + w - 1 >= FClipX1) and (x + w - 1 < bw) then
    for py := py0 to py1 do
      PCardinal(FRenderTarget.ScanLine[py])[x + w - 1] := FCurrentPixel;
  Result := CreateNilValue;
end;

function drawCircleNative(argCount: integer; args: pValue): TValue;
var
  cx, cy, r: Integer;
  bw, bh: Integer;
  x, y, d: Integer;
  fullyInside: Boolean;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawCircle() takes 3 arguments (x, y, radius).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('drawCircle() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  cx := Trunc(GetNumber(args[0])) - FCameraX;
  cy := Trunc(GetNumber(args[1])) - FCameraY;
  r  := Trunc(GetNumber(args[2]));
  if r < 0 then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Trivial reject: circle's bounding box is entirely outside the clip rect.
  if (cx + r < FClipX1) or (cx - r >= bw) or
     (cy + r < FClipY1) or (cy - r >= bh) then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  // Trivial accept: bounding box entirely inside clip; the per-pixel
  // clip checks (4 conditions x 8 octants per Bresenham step) can be
  // skipped, leaving a tight outline plot.
  fullyInside := (cx - r >= FClipX1) and (cx + r < bw) and
                 (cy - r >= FClipY1) and (cy + r < bh);
  x := 0;
  y := r;
  d := 1 - r;
  if fullyInside then
  begin
    while x <= y do
    begin
      PutPixelUnchecked(cx + x, cy + y, FCurrentPixel);
      PutPixelUnchecked(cx - x, cy + y, FCurrentPixel);
      PutPixelUnchecked(cx + x, cy - y, FCurrentPixel);
      PutPixelUnchecked(cx - x, cy - y, FCurrentPixel);
      PutPixelUnchecked(cx + y, cy + x, FCurrentPixel);
      PutPixelUnchecked(cx - y, cy + x, FCurrentPixel);
      PutPixelUnchecked(cx + y, cy - x, FCurrentPixel);
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
  end
  else
  begin
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
  begin
    if (hy < FClipY1) or (hy >= bh) then Exit;
    if hx0 < FClipX1 then hx0 := FClipX1;
    if hx1 >= bw then hx1 := bw - 1;
    if hx0 > hx1 then Exit;
    row := FRenderTarget.ScanLine[hy];
    // FillDWord: rep stosd, one 32-bit store per pixel.
    FillDWord(row[hx0], hx1 - hx0 + 1, FCurrentPixel);
  end;

begin
  if argCount <> 3 then
  begin
    RuntimeError('fillCircle() takes 3 arguments (x, y, radius).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('fillCircle() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  cx := Trunc(GetNumber(args[0])) - FCameraX;
  cy := Trunc(GetNumber(args[1])) - FCameraY;
  r  := Trunc(GetNumber(args[2]));
  if r < 0 then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Midpoint circle — fill with horizontal spans
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) then
  begin
    RuntimeError('drawPixel() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[0])) - FCameraX;
  y := Trunc(GetNumber(args[1])) - FCameraY;
  if (x >= FClipX1) and (x < FClipX2) and
     (y >= FClipY1) and (y < FClipY2) then
    PCardinal(FRenderTarget.ScanLine[y])[x] := FCurrentPixel;
  Result := CreateNilValue;
end;

// drawPixels(xs, ys): batch-plot N pixels using the current setColor()
// pen colour. Collapses N x (setColor + drawPixel) RTTI crossings into
// one call. Mismatched array lengths use min(Count). Per-pixel clip +
// camera + ScanLine cache by Y. Used by particle/starfield code.
function drawPixelsNative(argCount: integer; args: pValue): TValue;
var
  xs, ys: pObjArray;
  n, i, x, y, lastY: Integer;
  row: PCardinal;
  px: Cardinal;
begin
  if argCount <> 2 then
  begin
    RuntimeError('drawPixels() takes 2 arguments (xs, ys).');
    Exit(CreateNilValue);
  end;
  if (not isArray(args[0])) or (not isArray(args[1])) then
  begin
    RuntimeError('drawPixels() arguments must be arrays.');
    Exit(CreateNilValue);
  end;
  xs := pObjArray(GetObject(args[0]));
  ys := pObjArray(GetObject(args[1]));
  n := xs^.Count;
  if ys^.Count < n then n := ys^.Count;
  if n <= 0 then Exit(CreateNilValue);
  EnsureBackBuffer;
  px := FCurrentPixel;
  row := nil;
  lastY := -$7FFFFFFF;
  for i := 0 to n - 1 do
  begin
    if (not isNumber(xs^.Elements[i])) or
       (not isNumber(ys^.Elements[i])) then Continue;
    x := Trunc(GetNumber(xs^.Elements[i])) - FCameraX;
    y := Trunc(GetNumber(ys^.Elements[i])) - FCameraY;
    if (x < FClipX1) or (x >= FClipX2) or
       (y < FClipY1) or (y >= FClipY2) then Continue;
    if y <> lastY then
    begin
      row := FRenderTarget.ScanLine[y];
      lastY := y;
    end;
    row[x] := px;
  end;
  Result := CreateNilValue;
end;

// drawPixelsGray(xs, ys, brights): batch-plot N grayscale pixels with
// per-pixel brightness (0..255). Identical inner loop to drawPixels
// but packs (b,b,b) for each point. Designed for starfields where
// every star has its own brightness.
function drawPixelsGrayNative(argCount: integer; args: pValue): TValue;
var
  xs, ys, bs: pObjArray;
  n, i, x, y, b, lastY: Integer;
  row: PCardinal;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawPixelsGray() takes 3 arguments (xs, ys, brights).');
    Exit(CreateNilValue);
  end;
  if (not isArray(args[0])) or (not isArray(args[1])) or
     (not isArray(args[2])) then
  begin
    RuntimeError('drawPixelsGray() arguments must be arrays.');
    Exit(CreateNilValue);
  end;
  xs := pObjArray(GetObject(args[0]));
  ys := pObjArray(GetObject(args[1]));
  bs := pObjArray(GetObject(args[2]));
  n := xs^.Count;
  if ys^.Count < n then n := ys^.Count;
  if bs^.Count < n then n := bs^.Count;
  if n <= 0 then Exit(CreateNilValue);
  EnsureBackBuffer;
  row := nil;
  lastY := -$7FFFFFFF;
  for i := 0 to n - 1 do
  begin
    if (not isNumber(xs^.Elements[i])) or
       (not isNumber(ys^.Elements[i])) or
       (not isNumber(bs^.Elements[i])) then Continue;
    x := Trunc(GetNumber(xs^.Elements[i])) - FCameraX;
    y := Trunc(GetNumber(ys^.Elements[i])) - FCameraY;
    if (x < FClipX1) or (x >= FClipX2) or
       (y < FClipY1) or (y >= FClipY2) then Continue;
    b := Trunc(GetNumber(bs^.Elements[i]));
    if b < 0 then b := 0 else if b > 255 then b := 255;
    if y <> lastY then
    begin
      row := FRenderTarget.ScanLine[y];
      lastY := y;
    end;
    row[x] := Cardinal($FF000000 or (b shl 16) or (b shl 8) or b);
  end;
  Result := CreateNilValue;
end;

// -- Sprite functions --

// One-time scan: returns true if the bitmap has no magenta ($00FF00FF)
// transparency-key pixels, i.e. every pixel is opaque. Called once per
// sprite at slot-allocation time so drawSprite can skip per-pixel
// transparency tests for fully opaque sprites.
function BitmapIsFullyOpaque(bmp: TBitmap): Boolean;
var
  y, x: Integer;
  row: PRGBQuadArray;
begin
  if (bmp = nil) or (bmp.Width <= 0) or (bmp.Height <= 0) then
    Exit(False);
  for y := 0 to bmp.Height - 1 do
  begin
    row := bmp.ScanLine[y];
    for x := 0 to bmp.Width - 1 do
      if PCardinal(@row[x])^ = $00FF00FF then
        Exit(False);
  end;
  Result := True;
end;

function AllocSpriteSlot(bmp: TBitmap): Integer;
var
  idx: Integer;
  opaque: Boolean;
begin
  if FSprites = nil then
    FSprites := TObjectList<TBitmap>.Create(True);
  if FSpriteFreelist = nil then
    FSpriteFreelist := TList<Integer>.Create;
  if FSpriteOpaque = nil then
    FSpriteOpaque := TList<Boolean>.Create;
  opaque := BitmapIsFullyOpaque(bmp);
  if FSpriteFreelist.Count > 0 then
  begin
    idx := FSpriteFreelist[FSpriteFreelist.Count - 1];
    FSpriteFreelist.Delete(FSpriteFreelist.Count - 1);
    FSprites[idx] := bmp;
    FSpriteOpaque[idx] := opaque;
    Result := idx;
  end
  else
  begin
    Result := FSprites.Add(bmp);
    // Keep FSpriteOpaque strictly parallel to FSprites.
    while FSpriteOpaque.Count <= Result do
      FSpriteOpaque.Add(False);
    FSpriteOpaque[Result] := opaque;
  end;
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) then
  begin
    RuntimeError('createSprite() width and height must be numbers.');
    Exit(CreateNilValue);
  end;
  if not isString(args[2]) then
  begin
    RuntimeError('createSprite() third argument must be a string.');
    Exit(CreateNilValue);
  end;
  w := Trunc(GetNumber(args[0]));
  h := Trunc(GetNumber(args[1]));
  if (w <= 0) or (h <= 0) then
  begin
    RuntimeError('createSprite() width and height must be positive.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(GetObject(args[2])));
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
  sx0, sx1, sy0, sy1: Integer;
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('drawSprite() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('drawSprite() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[1])) - FCameraX;
  y := Trunc(GetNumber(args[2])) - FCameraY;
  bmp := FSprites[id];
  if bmp = nil then
  begin
    RuntimeError('drawSprite() sprite has been freed.');
    Exit(CreateNilValue);
  end;
  bw := FClipX2;
  bh := FClipY2;
  // Pre-clip: compute the visible sub-rectangle in SOURCE coordinates,
  // then iterate only those pixels. This removes the per-pixel clip
  // branches from the inner loop (correctness identical to the prior
  // per-pixel check version; the transparency-key test still runs).
  sx0 := FClipX1 - x; if sx0 < 0 then sx0 := 0;
  sy0 := FClipY1 - y; if sy0 < 0 then sy0 := 0;
  sx1 := bw - x;      if sx1 > bmp.Width  then sx1 := bmp.Width;
  sy1 := bh - y;      if sy1 > bmp.Height then sy1 := bmp.Height;
  if (sx0 >= sx1) or (sy0 >= sy1) then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  // Fast path: sprite has no transparent pixels, so each visible row
  // is just a contiguous copy. Move() compiles to an SSE/AVX-optimized
  // memcpy and is several times faster than the branchy per-pixel loop.
  if (FSpriteOpaque <> nil) and (id < FSpriteOpaque.Count) and FSpriteOpaque[id] then
  begin
    for sy := sy0 to sy1 - 1 do
    begin
      srcRow := bmp.ScanLine[sy];
      dstRow := FRenderTarget.ScanLine[y + sy];
      Move(srcRow[sx0], dstRow[x + sx0], (sx1 - sx0) * SizeOf(Cardinal));
    end;
    Result := CreateNilValue;
    Exit;
  end;
  for sy := sy0 to sy1 - 1 do
  begin
    srcRow := bmp.ScanLine[sy];
    dstRow := FRenderTarget.ScanLine[y + sy];
    for sx := sx0 to sx1 - 1 do
    begin
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
  sxFrac: Integer;
  dx0, dx1, dy0, dy1: Integer;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 4 then
  begin
    RuntimeError('drawSpriteScaled() takes 4 arguments (id, x, y, scale).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('drawSpriteScaled() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('drawSpriteScaled() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[1])) - FCameraX;
  y := Trunc(GetNumber(args[2])) - FCameraY;
  scale := Trunc(GetNumber(args[3]));
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
  // Pre-clip in DESTINATION coordinates so the inner loop has no
  // per-pixel clip branches. Source pixel is then sx = dx div scale.
  dx0 := FClipX1 - x; if dx0 < 0 then dx0 := 0;
  dy0 := FClipY1 - y; if dy0 < 0 then dy0 := 0;
  dx1 := bw - x;      if dx1 > sw then dx1 := sw;
  dy1 := bh - y;      if dy1 > sh then dy1 := sh;
  if (dx0 >= dx1) or (dy0 >= dy1) then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  // Manual scanline blit: skip transparent pixels ($00FF00FF), copy opaque ones.
  // Single pass, no GDI mask operations, deterministic rendering.
  // Inner-loop optimization: replace per-pixel `sx := dx div scale` with an
  // incremental counter that increments sx every `scale` destination pixels.
  // Eliminates an integer division per destination pixel.
  for dy := dy0 to dy1 - 1 do
  begin
    sy := dy div scale;
    srcRow := bmp.ScanLine[sy];
    dstRow := FRenderTarget.ScanLine[y + dy];
    sx := dx0 div scale;
    sxFrac := dx0 mod scale;
    for dx := dx0 to dx1 - 1 do
    begin
      pixel := PCardinal(@srcRow[sx])^;
      if pixel <> $00FF00FF then
        dstRow[x + dx] := pixel;
      Inc(sxFrac);
      if sxFrac >= scale then
      begin
        sxFrac := 0;
        Inc(sx);
      end;
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
  baseX, baseY: Integer;
  dx0, dx1, dy0, dy1: Integer;
  fxc, fyc, fxcRow, fycRow: Double;
  // 2x2 supersample offsets in source space (precomputed)
  ofsXa, ofsXb, ofsYa, ofsYb: Double;
  fxOff, fyOff: array[0..3] of Double;
  // Per-sample iteration
  i: Integer;
  sumR, sumG, sumB, cnt: Integer;
  ifx, ify: Integer;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel, dstPx: Cardinal;
  outR, outG, outB, missing: Integer;
  scaleIsOne: Boolean;
  lastSy: Integer;
begin
  if argCount <> 5 then
  begin
    RuntimeError('drawSpriteRotated() takes 5 arguments (id, x, y, scale, angle).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) or
     (not isNumber(args[4])) then
  begin
    RuntimeError('drawSpriteRotated() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
  if (FSprites = nil) or (id < 0) or (id >= FSprites.Count) then
  begin
    RuntimeError('drawSpriteRotated() invalid sprite id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[1])) - FCameraX;
  y := Trunc(GetNumber(args[2])) - FCameraY;
  scale := Trunc(GetNumber(args[3]));
  if scale < 1 then scale := 1;
  angle := GetNumber(args[4]) * Pi / 180.0; // degrees to radians
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

  // Hoist destination origin so the inner loop just adds dx/dy.
  baseX := x - Trunc(cx);
  baseY := y - Trunc(cy);

  // Pre-clip the destination bounding box against the clip rect so the
  // inner loop has no per-pixel screen-clip branches.
  dx0 := FClipX1 - baseX; if dx0 < 0 then dx0 := 0;
  dy0 := FClipY1 - baseY; if dy0 < 0 then dy0 := 0;
  dx1 := bw - baseX;      if dx1 > dw then dx1 := dw;
  dy1 := bh - baseY;      if dy1 > dh then dy1 := dh;
  if (dx0 >= dx1) or (dy0 >= dy1) then
  begin
    Result := CreateNilValue;
    Exit;
  end;

  // 2x2 supersampling: sample destination pixel at (+/-0.25, +/-0.25)
  // offsets, average opaque samples, alpha-blend partial coverage with
  // the existing destination pixel. Anti-aliases rotated edges while
  // preserving the magenta ($00FF00FF) transparency key exactly.
  ofsXa := 0.25 * cosA;   // dest-dx -> source fx contribution
  ofsXb := 0.25 * sinA;   // dest-dy -> source fx contribution
  ofsYa := -0.25 * sinA;  // dest-dx -> source fy contribution
  ofsYb := 0.25 * cosA;   // dest-dy -> source fy contribution

  // Precompute the 4 (fx, fy) sub-sample deltas relative to the pixel
  // center so the inner sample loop is a straight `for i := 0 to 3`
  // (no `case` jump-table branch per sub-sample).
  fxOff[0] := -ofsXa - ofsXb; fyOff[0] := -ofsYa - ofsYb;
  fxOff[1] :=  ofsXa - ofsXb; fyOff[1] :=  ofsYa - ofsYb;
  fxOff[2] := -ofsXa + ofsXb; fyOff[2] := -ofsYa + ofsYb;
  fxOff[3] :=  ofsXa + ofsXb; fyOff[3] :=  ofsYa + ofsYb;

  // scale=1 is the overwhelmingly common case; hoist the test out of
  // the inner sub-sample loop so the per-pixel branch is just a load.
  scaleIsOne := (scale = 1);

  // DDA: the original code recomputed
  //   fxc = (dx - cx)*cosA + (dy - cy)*sinA + srcCx
  //   fyc = -(dx - cx)*sinA + (dy - cy)*cosA + srcCy
  // for every destination pixel. Both forms are linear in dx and dy,
  // so we step them incrementally instead: per-column adds {cosA, -sinA},
  // per-row adds {sinA, cosA}. Removes 4 multiplies + 4 adds per pixel.
  fxcRow := (dx0 - cx) * cosA + (dy0 - cy) * sinA + srcCx;
  fycRow := -(dx0 - cx) * sinA + (dy0 - cy) * cosA + srcCy;

  // Initialize so the "did we already fetch this row?" cache is unset.
  srcRow := nil;

  for dy := dy0 to dy1 - 1 do
  begin
    dstRow := FRenderTarget.ScanLine[baseY + dy];
    fxc := fxcRow;
    fyc := fycRow;
    for dx := dx0 to dx1 - 1 do
    begin
      sumR := 0; sumG := 0; sumB := 0; cnt := 0;
      // ScanLine cache: most rotations have shallow angles, so all 4
      // sub-samples within a pixel usually fall on the same source row.
      // Reset per output pixel so the cache reflects reality.
      lastSy := -1;

      for i := 0 to 3 do
      begin
        ifx := Trunc(fxc + fxOff[i]);
        ify := Trunc(fyc + fyOff[i]);
        if (ifx < 0) or (ifx >= sw) or (ify < 0) or (ify >= sh) then Continue;
        if scaleIsOne then
        begin
          sx := ifx;
          sy := ify;
        end
        else
        begin
          sx := ifx div scale;
          sy := ify div scale;
        end;
        if sy <> lastSy then
        begin
          srcRow := bmp.ScanLine[sy];
          lastSy := sy;
        end;
        pixel := PCardinal(@srcRow[sx])^;
        if pixel = $00FF00FF then Continue;
        sumB := sumB + Integer(pixel and $FF);
        sumG := sumG + Integer((pixel shr 8) and $FF);
        sumR := sumR + Integer((pixel shr 16) and $FF);
        cnt := cnt + 1;
      end;

      if cnt <> 0 then
      begin
        if cnt = 4 then
        begin
          // Full coverage: pure average, no blend with destination.
          outB := sumB shr 2;
          outG := sumG shr 2;
          outR := sumR shr 2;
        end
        else
        begin
          // Partial coverage: alpha-blend averaged source over existing
          // destination pixel. cnt samples contribute source color,
          // (4 - cnt) samples contribute destination color.
          missing := 4 - cnt;
          dstPx := dstRow[baseX + dx];
          outB := (sumB + Integer(dstPx and $FF) * missing) shr 2;
          outG := (sumG + Integer((dstPx shr 8) and $FF) * missing) shr 2;
          outR := (sumR + Integer((dstPx shr 16) and $FF) * missing) shr 2;
        end;
        dstRow[baseX + dx] := (Cardinal(outR) shl 16) or
                              (Cardinal(outG) shl 8) or
                               Cardinal(outB);
      end;

      // Step source coords by one destination column.
      fxc := fxc + cosA;
      fyc := fyc - sinA;
    end;
    // Step source coords by one destination row.
    fxcRow := fxcRow + sinA;
    fycRow := fycRow + cosA;
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
  if (not isNumber(args[1])) or (not isNumber(args[2])) or
     (not isNumber(args[3])) then
  begin
    RuntimeError('setPaletteColor() r, g, b must be numbers.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(GetObject(args[0])));
  if Length(s) <> 1 then
  begin
    RuntimeError('setPaletteColor() first argument must be a single character.');
    Exit(CreateNilValue);
  end;
  ch := Ord(s[1]);
  r := Trunc(GetNumber(args[1]));
  g := Trunc(GetNumber(args[2]));
  b := Trunc(GetNumber(args[3]));
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
const
  // Fixed, opaque-white fallback for palette characters that haven't been
  // defined via setPaletteColor(). Using a constant (rather than
  // FCurrentPixel) keeps the sprite's appearance deterministic and
  // independent of whatever setColor() value happens to be active when
  // the sprite is created.
  PALETTE_FALLBACK_PIXEL: Cardinal = $FFFFFFFF;
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) then
  begin
    RuntimeError('createPaletteSprite() width and height must be numbers.');
    Exit(CreateNilValue);
  end;
  if not isString(args[2]) then
  begin
    RuntimeError('createPaletteSprite() third argument must be a string.');
    Exit(CreateNilValue);
  end;
  w := Trunc(GetNumber(args[0]));
  h := Trunc(GetNumber(args[1]));
  if (w <= 0) or (h <= 0) then
  begin
    RuntimeError('createPaletteSprite() width and height must be positive.');
    Exit(CreateNilValue);
  end;
  s := ObjStringToAnsiString(pObjString(GetObject(args[2])));
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
          PCardinal(@row[x])^ := PALETTE_FALLBACK_PIXEL;  // undefined char -> opaque white
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
  path := ResolveAssetPath(string(ObjStringToAnsiString(pObjString(GetObject(args[0])))));
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
    if not isNumber(args[i]) then
    begin
      RuntimeError('loadSpriteFromPNGRegion() x, y, w, h must be numbers.');
      Exit(CreateNilValue);
    end;
  path := ResolveAssetPath(string(ObjStringToAnsiString(pObjString(GetObject(args[0])))));
  sx := Trunc(GetNumber(args[1]));
  sy := Trunc(GetNumber(args[2]));
  sw := Trunc(GetNumber(args[3]));
  sh := Trunc(GetNumber(args[4]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('createTilemap() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  cols := Trunc(GetNumber(args[0]));
  rows := Trunc(GetNumber(args[1]));
  tw := Trunc(GetNumber(args[2]));
  th := Trunc(GetNumber(args[3]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) or (not isNumber(args[3])) then
  begin
    RuntimeError('setTile() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  mapId := Trunc(GetNumber(args[0]));
  if (FTilemaps = nil) or (mapId < 0) or (mapId >= FTilemaps.Count) then
  begin
    RuntimeError('setTile() invalid tilemap id.');
    Exit(CreateNilValue);
  end;
  tm := FTilemaps[mapId];
  col := Trunc(GetNumber(args[1]));
  row := Trunc(GetNumber(args[2]));
  spriteId := Trunc(GetNumber(args[3]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('getTile() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  mapId := Trunc(GetNumber(args[0]));
  if (FTilemaps = nil) or (mapId < 0) or (mapId >= FTilemaps.Count) then
  begin
    RuntimeError('getTile() invalid tilemap id.');
    Exit(CreateNilValue);
  end;
  tm := FTilemaps[mapId];
  col := Trunc(GetNumber(args[1]));
  row := Trunc(GetNumber(args[2]));
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
  px0, px1, py0, py1: Integer;
  oneToOne, opaque: Boolean;
  srcRow: PRGBQuadArray;
  dstRow: PCardinal;
  pixel: Cardinal;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawTilemap() takes 3 arguments (mapId, scrollX, scrollY).');
    Exit(CreateNilValue);
  end;
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('drawTilemap() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  mapId := Trunc(GetNumber(args[0]));
  if (FTilemaps = nil) or (mapId < 0) or (mapId >= FTilemaps.Count) then
  begin
    RuntimeError('drawTilemap() invalid tilemap id.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  tm := FTilemaps[mapId];
  scrollX := Trunc(GetNumber(args[1])) + FCameraX;
  scrollY := Trunc(GetNumber(args[2])) + FCameraY;

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
      if (tileIdx < 0) or (tileIdx >= FSprites.Count) then Continue;
      bmp := FSprites[tileIdx];
      if bmp = nil then Continue;  // freed sprite - skip tile
      dx := c * tm.TileW - scrollX;
      dy := r * tm.TileH - scrollY;

      // Pre-clip the tile's visible rectangle in tile-local coords so
      // the inner loops have no per-pixel screen-clip branches.
      px0 := FClipX1 - dx; if px0 < 0 then px0 := 0;
      py0 := FClipY1 - dy; if py0 < 0 then py0 := 0;
      px1 := bw - dx;      if px1 > tm.TileW then px1 := tm.TileW;
      py1 := bh - dy;      if py1 > tm.TileH then py1 := tm.TileH;
      if (px0 >= px1) or (py0 >= py1) then Continue;

      // 1:1 sprite-to-tile dimensions enable a much faster inner loop:
      // no per-pixel scaling math, and (if opaque) a Move() memcpy.
      oneToOne := (bmp.Width = tm.TileW) and (bmp.Height = tm.TileH);
      opaque := (FSpriteOpaque <> nil) and (tileIdx < FSpriteOpaque.Count)
                and FSpriteOpaque[tileIdx];

      if oneToOne and opaque then
      begin
        // Fast path: opaque tile, no scaling - one Move() per row.
        for py := py0 to py1 - 1 do
        begin
          srcRow := bmp.ScanLine[py];
          dstRow := FRenderTarget.ScanLine[dy + py];
          Move(srcRow[px0], dstRow[dx + px0], (px1 - px0) * SizeOf(Cardinal));
        end;
      end
      else if oneToOne then
      begin
        // No scaling, but transparency-keyed: tight per-pixel copy
        // without the div math.
        for py := py0 to py1 - 1 do
        begin
          srcRow := bmp.ScanLine[py];
          dstRow := FRenderTarget.ScanLine[dy + py];
          for px := px0 to px1 - 1 do
          begin
            pixel := PCardinal(@srcRow[px])^;
            if pixel <> $00FF00FF then
              dstRow[dx + px] := pixel;
          end;
        end;
      end
      else
      begin
        // Sprite is not the tile's native size: stretch/shrink to fit.
        // Inner loop uses a fractional-counter (Bresenham-style) advance
        // for sx so we avoid an integer division per destination pixel.
        for py := py0 to py1 - 1 do
        begin
          sy := (py * bmp.Height) div tm.TileH;
          srcRow := bmp.ScanLine[sy];
          dstRow := FRenderTarget.ScanLine[dy + py];
          // Initial sx for px0 (one div, hoisted out of the inner loop).
          sx := (px0 * bmp.Width) div tm.TileW;
          // Fractional accumulator over tm.TileW units.
          // After each px, advance by bmp.Width; while >= tm.TileW, Inc(sx).
          // (Works for both stretching and shrinking ratios.)
          var sxFrac2: Integer := (px0 * bmp.Width) mod tm.TileW;
          for px := px0 to px1 - 1 do
          begin
            pixel := PCardinal(@srcRow[sx])^;
            if pixel <> $00FF00FF then
              dstRow[dx + px] := pixel;
            Inc(sxFrac2, bmp.Width);
            while sxFrac2 >= tm.TileW do
            begin
              Dec(sxFrac2, tm.TileW);
              Inc(sx);
            end;
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
  if (not isNumber(args[0])) then
  begin
    RuntimeError('flipSprite() first argument must be a number.');
    Exit(CreateNilValue);
  end;
  if not isString(args[1]) then
  begin
    RuntimeError('flipSprite() second argument must be "h" or "v".');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
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
  var dirStr := ObjStringToAnsiString(pObjString(GetObject(args[1])));
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
  if not isNumber(args[0]) then
  begin
    RuntimeError('freeSprite() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) then
  begin
    RuntimeError('createSurface() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  w := Trunc(GetNumber(args[0]));
  h := Trunc(GetNumber(args[1]));
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
  if not isNumber(args[0]) then
  begin
    RuntimeError('setRenderTarget() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
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
  if (not isNumber(args[0])) or (not isNumber(args[1])) or
     (not isNumber(args[2])) then
  begin
    RuntimeError('drawSurface() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
  if (FSurfaces = nil) or (id < 0) or (id >= FSurfaces.Count) or (FSurfaces[id] = nil) then
  begin
    RuntimeError('drawSurface() invalid surface id.');
    Exit(CreateNilValue);
  end;
  src := FSurfaces[id];
  EnsureBackBuffer;
  x := Trunc(GetNumber(args[1])) - FCameraX;
  y := Trunc(GetNumber(args[2])) - FCameraY;
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
  if not isNumber(args[0]) then
  begin
    RuntimeError('freeSurface() argument must be a number.');
    Exit(CreateNilValue);
  end;
  id := Trunc(GetNumber(args[0]));
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
  // Unbind the current render target BEFORE clearing the surfaces list,
  // otherwise FRenderTarget could point at a TBitmap that FSurfaces.Clear
  // is about to free. EnsureBackBuffer (called by the next draw native)
  // will repoint FRenderTarget at FBackBuffer; until then, nil is the
  // only safe value if FBackBuffer hasn't been created yet.
  FRenderTargetId := -1;
  FRenderTarget := FBackBuffer;  // may be nil; EnsureBackBuffer will fix
  // Clear sprites, tilemaps, surfaces, and palette from previous run
  if FSprites <> nil then
    FSprites.Clear;
  if FSpriteFreelist <> nil then
    FSpriteFreelist.Clear;
  if FSpriteOpaque <> nil then
    FSpriteOpaque.Clear;
  if FSurfaces <> nil then
    FSurfaces.Clear;
  if FSurfaceFreelist <> nil then
    FSurfaceFreelist.Clear;
  if FTilemaps <> nil then
    FTilemaps.Clear;
  FillChar(FPalette, SizeOf(FPalette), 0);
  FillChar(FPaletteUsed, SizeOf(FPaletteUsed), 0);
  FCurrentPixel := $FFFFFFFF;  // opaque white; safe default if InitCanvas wasn't run
  FCameraX := 0;
  FCameraY := 0;
  FRenderTargetId := -1;
  FClipActive := False;
  ClearAllKeyState;
  defineNative('canvasWidth', canvasWidthNative, 0);
  defineNative('canvasHeight', canvasHeightNative, 0);
  defineNative('setCanvasSize', setCanvasSizeNative, 2);
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
  defineNative('drawPixels', drawPixelsNative, 2);
  defineNative('drawPixelsGray', drawPixelsGrayNative, 3);
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
  defineNative('keyPressed', keyPressedNative, 1);
  defineNative('mouseX', mouseXNative, 0);
  defineNative('mouseY', mouseYNative, 0);
  defineNative('mouseDown', mouseDownNative, 1);
  defineNative('mouseClicked', mouseClickedNative, 1);
  defineNative('spriteWidth', spriteWidthNative, 1);
  defineNative('spriteHeight', spriteHeightNative, 1);
end;

end.
