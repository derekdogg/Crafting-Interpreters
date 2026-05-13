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
  SysUtils, Types, Classes, Generics.Collections;

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
  FSprites: TObjectList<TBitmap>;
  FTilemaps: TObjectList<TTilemap>;
  FHasFrontFrame: Boolean;  // true once present() has been called at least once

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
  x := Trunc(args[0].NumberValue);
  y := Trunc(args[1].NumberValue);
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
  x := Trunc(args[0].NumberValue);
  y := Trunc(args[1].NumberValue);
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
  x1 := Trunc(args[0].NumberValue);
  y1 := Trunc(args[1].NumberValue);
  x2 := Trunc(args[2].NumberValue);
  y2 := Trunc(args[3].NumberValue);
  FBackBuffer.Canvas.Pen.Color := FCurrentColor;
  FBackBuffer.Canvas.MoveTo(x1, y1);
  FBackBuffer.Canvas.LineTo(x2, y2);
  Result := CreateNilValue;
end;

function fillCircleNative(argCount: integer; args: pValue): TValue;
var
  oldPenStyle: TPenStyle;
  cx, cy, r: Integer;
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
  cx := Trunc(args[0].NumberValue);
  cy := Trunc(args[1].NumberValue);
  r  := Trunc(args[2].NumberValue);
  oldPenStyle := FBackBuffer.Canvas.Pen.Style;
  try
    FBackBuffer.Canvas.Brush.Color := FCurrentColor;
    FBackBuffer.Canvas.Pen.Style := psClear;
    FBackBuffer.Canvas.Ellipse(cx - r, cy - r, cx + r, cy + r);
  finally
    FBackBuffer.Canvas.Pen.Style := oldPenStyle;
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
  x := Trunc(args[0].NumberValue);
  y := Trunc(args[1].NumberValue);
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
  x := Trunc(args[1].NumberValue);
  y := Trunc(args[2].NumberValue);
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
  x := Trunc(args[1].NumberValue);
  y := Trunc(args[2].NumberValue);
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

function drawTilemapNative(argCount: integer; args: pValue): TValue;
var
  mapId, scrollX, scrollY: Integer;
  tm: TTilemap;
  startCol, startRow, endCol, endRow: Integer;
  c, r, tileIdx, dx, dy: Integer;
  bmp: TBitmap;
  scaleX, scaleY, sx, sy, px, py, bw, bh: Integer;
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
  scrollX := Trunc(args[1].NumberValue);
  scrollY := Trunc(args[2].NumberValue);

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
        scaleX := tm.TileW div bmp.Width;
        scaleY := tm.TileH div bmp.Height;
        if scaleX < 1 then scaleX := 1;
        if scaleY < 1 then scaleY := 1;
        for py := 0 to tm.TileH - 1 do
        begin
          if (dy + py < 0) or (dy + py >= bh) then Continue;
          sy := py div scaleY;
          if sy >= bmp.Height then sy := bmp.Height - 1;
          srcRow := bmp.ScanLine[sy];
          dstRow := FBackBuffer.ScanLine[dy + py];
          for px := 0 to tm.TileW - 1 do
          begin
            if (dx + px < 0) or (dx + px >= bw) then Continue;
            sx := px div scaleX;
            if sx >= bmp.Width then sx := bmp.Width - 1;
            pixel := PCardinal(@srcRow[sx])^;
            if pixel <> $00FF00FF then
              dstRow[dx + px] := pixel;
          end;
        end;
      end;
    end;
  Result := CreateNilValue;
end;

procedure RegisterCanvasNatives;
begin
  // Clear sprites and tilemaps from previous run
  if FSprites <> nil then
    FSprites.Clear;
  if FTilemaps <> nil then
    FTilemaps.Clear;
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
end;

end.
