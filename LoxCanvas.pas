unit LoxCanvas;

interface

uses
  Vcl.ExtCtrls, Vcl.Graphics, Chunk_Types;

procedure InitCanvas(APaintBox: TPaintBox);
procedure FreeCanvas;
procedure RegisterCanvasNatives;

implementation

{$POINTERMATH ON}

uses
  Vcl.Controls, Winapi.Messages,
  SysUtils, Types, Classes, Generics.Collections, Math;

type
  TRGBQuadRec = packed record
    rgbBlue, rgbGreen, rgbRed, rgbReserved: Byte;
  end;
  TRGBQuadArray = array[0..MaxInt div SizeOf(TRGBQuadRec) - 1] of TRGBQuadRec;
  PRGBQuadArray = ^TRGBQuadArray;

  TGameSurface = class(TCustomControl)
  protected
    procedure Paint; override;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

  TTilemap = class
    Cols, Rows: Integer;
    TileW, TileH: Integer;
    Tiles: array of Integer;
    constructor Create(ACols, ARows, ATileW, ATileH: Integer);
  end;

const
  SRCCOPY = $00CC0020;

function BitBlt(DestDC: THandle; X, Y, Width, Height: Integer;
  SrcDC: THandle; XSrc, YSrc: Integer; Rop: Cardinal): LongBool; stdcall;
  external 'gdi32.dll' name 'BitBlt';

var
  FPaintBox: TPaintBox;
  FGameSurface: TGameSurface;
  FBackBuffer: TBitmap;
  FFrontBuffer: TBitmap;   // presented frame — never partially drawn
  FCurrentColor: TColor;
  FCurrentPixel: Cardinal;  // precomputed BGRA pixel for ScanLine writes
  FCameraX: Integer;
  FCameraY: Integer;
  FSprites: TObjectList<TBitmap>;
  FTilemaps: TObjectList<TTilemap>;
  FHasFrontFrame: Boolean;  // true once present() has been called at least once
  FPalette: array[0..255] of Cardinal;   // char -> BGRA color
  FPaletteUsed: array[0..255] of Boolean;

procedure EnsureBackBuffer;
begin
  if FBackBuffer = nil then
  begin
    FBackBuffer := TBitmap.Create;
    FBackBuffer.PixelFormat := pf32bit;
  end;
  if (FBackBuffer.Width <> FGameSurface.Width) or
     (FBackBuffer.Height <> FGameSurface.Height) then
  begin
    FBackBuffer.SetSize(FGameSurface.Width, FGameSurface.Height);
    FBackBuffer.Canvas.Brush.Color := clBlack;
    FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  end;
end;

{ TGameSurface }

procedure TGameSurface.Paint;
begin
  if FHasFrontFrame and (FFrontBuffer <> nil) then
    BitBlt(Canvas.Handle, 0, 0, FFrontBuffer.Width, FFrontBuffer.Height,
           FFrontBuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TGameSurface.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;  // suppress background erase entirely
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

procedure InitCanvas(APaintBox: TPaintBox);
begin
  FPaintBox := APaintBox;
  FCurrentColor := clWhite;
  // Create a TGameSurface to replace the TPaintBox — it owns its window
  // handle, suppresses WM_ERASEBKGND, and has a single paint path.
  FreeAndNil(FGameSurface);
  FGameSurface := TGameSurface.Create(FPaintBox.Owner);
  FGameSurface.Parent := FPaintBox.Parent;
  FGameSurface.Align := FPaintBox.Align;
  FGameSurface.BoundsRect := FPaintBox.BoundsRect;
  FPaintBox.Visible := False;
  FreeAndNil(FBackBuffer);
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackBuffer.SetSize(FGameSurface.Width, FGameSurface.Height);
  FBackBuffer.Canvas.Brush.Color := clBlack;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  FHasFrontFrame := False;
end;

procedure FreeCanvas;
begin
  FreeAndNil(FTilemaps);
  FreeAndNil(FSprites);
  FreeAndNil(FFrontBuffer);
  FreeAndNil(FBackBuffer);
  FreeAndNil(FGameSurface);
  if FPaintBox <> nil then
    FPaintBox.Visible := True;
  FPaintBox := nil;
end;

// -- Native functions exposed to Lox --

function canvasWidthNative(argCount: integer; args: pValue): TValue;
begin
  if FGameSurface = nil then
  begin
    RuntimeError('Canvas not initialized.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FGameSurface.Width);
end;

function canvasHeightNative(argCount: integer; args: pValue): TValue;
begin
  if FGameSurface = nil then
  begin
    RuntimeError('Canvas not initialized.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FGameSurface.Height);
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
  PCardinal(FBackBuffer.ScanLine[y])[x] := pixel;
end;

function clearCanvasNative(argCount: integer; args: pValue): TValue;
begin
  EnsureBackBuffer;
  ClearBuffer(FBackBuffer);
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
  FCurrentColor := TColor(r or (g shl 8) or (b shl 16));
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

function fillRectNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h: Integer;
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
  FBackBuffer.Canvas.Brush.Color := FCurrentColor;
  FBackBuffer.Canvas.FillRect(Rect(x, y, x + w, y + h));
  Result := CreateNilValue;
end;

function drawTextNative(argCount: integer; args: pValue): TValue;
var
  oldBrushStyle: TBrushStyle;
  x, y: Integer;
  s: AnsiString;
begin
  if argCount <> 3 then
  begin
    RuntimeError('drawText() takes 3 arguments (x, y, text).');
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
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue) - FCameraX;
  y := Trunc(args[1].NumberValue) - FCameraY;
  s := ObjStringToAnsiString(pObjString(args[2].ObjValue));
  oldBrushStyle := FBackBuffer.Canvas.Brush.Style;
  try
    FBackBuffer.Canvas.Brush.Style := bsClear;
    FBackBuffer.Canvas.Font.Color := FCurrentColor;
    FBackBuffer.Canvas.Font.Size := 14;
    FBackBuffer.Canvas.TextOut(x, y, String(s));
  finally
    FBackBuffer.Canvas.Brush.Style := oldBrushStyle;
  end;
  Result := CreateNilValue;
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
  FHasFrontFrame := True;
  // Async repaint: let Windows coalesce and
  // paint at its own pace for smoother frame pacing.
  FGameSurface.Invalidate;
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
  bw := FBackBuffer.Width;
  bh := FBackBuffer.Height;
  // Bresenham's line algorithm — crisp single-pixel line
  dx := Abs(x2 - x1);
  dy := -Abs(y2 - y1);
  if x1 < x2 then sx := 1 else sx := -1;
  if y1 < y2 then sy := 1 else sy := -1;
  err := dx + dy;
  while True do
  begin
    if (x1 >= 0) and (x1 < bw) and (y1 >= 0) and (y1 < bh) then
      PutPixelUnchecked(x1, y1, FCurrentPixel);
    if (x1 = x2) and (y1 = y2) then Break;
    e2 := 2 * err;
    if e2 >= dy then begin err := err + dy; x1 := x1 + sx; end;
    if e2 <= dx then begin err := err + dx; y1 := y1 + sy; end;
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
    if (hy < 0) or (hy >= bh) then Exit;
    if hx0 < 0 then hx0 := 0;
    if hx1 >= bw then hx1 := bw - 1;
    if hx0 > hx1 then Exit;
    row := FBackBuffer.ScanLine[hy];
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
  bw := FBackBuffer.Width;
  bh := FBackBuffer.Height;
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
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('drawPixel() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  EnsureBackBuffer;
  x := Trunc(args[0].NumberValue) - FCameraX;
  y := Trunc(args[1].NumberValue) - FCameraY;
  if (x >= 0) and (x < FBackBuffer.Width) and
     (y >= 0) and (y < FBackBuffer.Height) then
    PCardinal(FBackBuffer.ScanLine[y])[x] := FCurrentPixel;
  Result := CreateNilValue;
end;

// -- Sprite functions --

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

  if FSprites = nil then
    FSprites := TObjectList<TBitmap>.Create(True);
  Result := CreateNumber(FSprites.Add(bmp));
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
  bw := FBackBuffer.Width;
  bh := FBackBuffer.Height;
  for sy := 0 to bmp.Height - 1 do
  begin
    if (y + sy < 0) or (y + sy >= bh) then Continue;
    srcRow := bmp.ScanLine[sy];
    dstRow := FBackBuffer.ScanLine[y + sy];
    for sx := 0 to bmp.Width - 1 do
    begin
      if (x + sx < 0) or (x + sx >= bw) then Continue;
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
  sw := bmp.Width * scale;
  sh := bmp.Height * scale;
  bw := FBackBuffer.Width;
  bh := FBackBuffer.Height;
  // Manual scanline blit: skip transparent pixels ($00FF00FF), copy opaque ones.
  // Single pass, no GDI mask operations, deterministic rendering.
  for dy := 0 to sh - 1 do
  begin
    if (y + dy < 0) or (y + dy >= bh) then Continue;
    sy := dy div scale;
    srcRow := bmp.ScanLine[sy];
    dstRow := FBackBuffer.ScanLine[y + dy];
    for dx := 0 to sw - 1 do
    begin
      if (x + dx < 0) or (x + dx >= bw) then Continue;
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
  bw := FBackBuffer.Width;
  bh := FBackBuffer.Height;

  // Inverse-mapping: for each destination pixel, find source pixel
  for dy := 0 to dh - 1 do
  begin
    if (y - Trunc(cy) + dy < 0) or (y - Trunc(cy) + dy >= bh) then Continue;
    dstRow := FBackBuffer.ScanLine[y - Trunc(cy) + dy];
    for dx := 0 to dw - 1 do
    begin
      if (x - Trunc(cx) + dx < 0) or (x - Trunc(cx) + dx >= bw) then Continue;
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

  if FSprites = nil then
    FSprites := TObjectList<TBitmap>.Create(True);
  Result := CreateNumber(FSprites.Add(bmp));
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
  if (spriteId >= 0) and ((FSprites = nil) or (spriteId >= FSprites.Count)) then
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
  endCol := (scrollX + FBackBuffer.Width) div tm.TileW;
  endRow := (scrollY + FBackBuffer.Height) div tm.TileH;
  if startCol < 0 then startCol := 0;
  if startRow < 0 then startRow := 0;
  if endCol >= tm.Cols then endCol := tm.Cols - 1;
  if endRow >= tm.Rows then endRow := tm.Rows - 1;

  bw := FBackBuffer.Width;
  bh := FBackBuffer.Height;
  for r := startRow to endRow do
    for c := startCol to endCol do
    begin
      tileIdx := tm.Tiles[r * tm.Cols + c];
      if (tileIdx >= 0) and (tileIdx < FSprites.Count) then
      begin
        bmp := FSprites[tileIdx];
        dx := c * tm.TileW - scrollX;
        dy := r * tm.TileH - scrollY;
        for py := 0 to tm.TileH - 1 do
        begin
          if (dy + py < 0) or (dy + py >= bh) then Continue;
          sy := (py * bmp.Height) div tm.TileH;
          srcRow := bmp.ScanLine[sy];
          dstRow := FBackBuffer.ScanLine[dy + py];
          for px := 0 to tm.TileW - 1 do
          begin
            if (dx + px < 0) or (dx + px >= bw) then Continue;
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
  w := src.Width;
  h := src.Height;
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(w, h);
    if ObjStringToAnsiString(pObjString(args[1].ObjValue)) = 'v' then
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
  Result := CreateNumber(FSprites.Add(bmp));
end;

procedure RegisterCanvasNatives;
begin
  // Clear sprites, tilemaps, and palette from previous run
  if FSprites <> nil then
    FSprites.Clear;
  if FTilemaps <> nil then
    FTilemaps.Clear;
  FillChar(FPalette, SizeOf(FPalette), 0);
  FillChar(FPaletteUsed, SizeOf(FPaletteUsed), 0);
  FCameraX := 0;
  FCameraY := 0;
  defineNative('canvasWidth', canvasWidthNative, 0);
  defineNative('canvasHeight', canvasHeightNative, 0);
  defineNative('clearCanvas', clearCanvasNative, 0);
  defineNative('setColor', setColorNative, 3);
  defineNative('fillRect', fillRectNative, 4);
  defineNative('drawText', drawTextNative, 3);
  defineNative('present', presentNative, 0);
  defineNative('drawLine', drawLineNative, 4);
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
  defineNative('getTile', getTileNative, 3);
  defineNative('flipSprite', flipSpriteNative, 2);
  defineNative('drawSpriteRotated', drawSpriteRotatedNative, 5);
  defineNative('setCamera', setCameraNative, 2);
end;

end.
