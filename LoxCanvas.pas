unit LoxCanvas;

interface

uses
  Vcl.ExtCtrls, Vcl.Graphics, Chunk_Types;

procedure InitCanvas(APaintBox: TPaintBox);
procedure FreeCanvas;
procedure RegisterCanvasNatives;

implementation

uses
  SysUtils, Types, Classes, Generics.Collections;

type
  TCanvasHelper = class
    procedure PaintBoxPaint(Sender: TObject);
  end;

var
  FPaintBox: TPaintBox;
  FBackBuffer: TBitmap;
  FCurrentColor: TColor;
  FCanvasHelper: TCanvasHelper;
  FSprites: TObjectList<TBitmap>;

procedure EnsureBackBuffer;
begin
  if FBackBuffer = nil then
  begin
    FBackBuffer := TBitmap.Create;
    FBackBuffer.PixelFormat := pf32bit;
  end;
  if (FBackBuffer.Width <> FPaintBox.Width) or
     (FBackBuffer.Height <> FPaintBox.Height) then
  begin
    FBackBuffer.SetSize(FPaintBox.Width, FPaintBox.Height);
    FBackBuffer.Canvas.Brush.Color := clBlack;
    FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  end;
end;

{ TCanvasHelper }

procedure TCanvasHelper.PaintBoxPaint(Sender: TObject);
begin
  if FBackBuffer <> nil then
    FPaintBox.Canvas.Draw(0, 0, FBackBuffer);
end;

procedure InitCanvas(APaintBox: TPaintBox);
begin
  FPaintBox := APaintBox;
  FCurrentColor := clWhite;
  // Clear sprites from previous run
  if FSprites <> nil then
    FSprites.Clear;
  if FCanvasHelper = nil then
    FCanvasHelper := TCanvasHelper.Create;
  FreeAndNil(FBackBuffer);
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackBuffer.SetSize(FPaintBox.Width, FPaintBox.Height);
  FBackBuffer.Canvas.Brush.Color := clBlack;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  FPaintBox.OnPaint := FCanvasHelper.PaintBoxPaint;
end;

procedure FreeCanvas;
begin
  if FPaintBox <> nil then
    FPaintBox.OnPaint := nil;
  FreeAndNil(FSprites);
  FreeAndNil(FBackBuffer);
  FreeAndNil(FCanvasHelper);
  FPaintBox := nil;
end;

// -- Native functions exposed to Lox --

function canvasWidthNative(argCount: integer; args: pValue): TValue;
begin
  if FPaintBox = nil then
  begin
    RuntimeError('Canvas not initialized.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FPaintBox.Width);
end;

function canvasHeightNative(argCount: integer; args: pValue): TValue;
begin
  if FPaintBox = nil then
  begin
    RuntimeError('Canvas not initialized.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(FPaintBox.Height);
end;

function clearCanvasNative(argCount: integer; args: pValue): TValue;
begin
  EnsureBackBuffer;
  FBackBuffer.Canvas.Brush.Color := clBlack;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
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
begin
  EnsureBackBuffer;
  FPaintBox.Canvas.Draw(0, 0, FBackBuffer);
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

// -- Sprite functions --

function createSpriteNative(argCount: integer; args: pValue): TValue;
var
  w, h, x, y, i: Integer;
  s: AnsiString;
  bmp: TBitmap;
  transColor: TColor;
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
    bmp.Canvas.Brush.Color := transColor;
    bmp.Canvas.FillRect(Rect(0, 0, w, h));
    i := 1;
    for y := 0 to h - 1 do
      for x := 0 to w - 1 do
      begin
        if s[i] <> '.' then
          bmp.Canvas.Pixels[x, y] := FCurrentColor;
        Inc(i);
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
  id, x, y: Integer;
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
  FBackBuffer.Canvas.Draw(x, y, FSprites[id]);
  Result := CreateNilValue;
end;

function drawSpriteScaledNative(argCount: integer; args: pValue): TValue;
var
  id, x, y, scale, sw, sh: Integer;
  destRect: TRect;
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
  sw := FSprites[id].Width * scale;
  sh := FSprites[id].Height * scale;
  destRect := Rect(x, y, x + sw, y + sh);
  FBackBuffer.Canvas.StretchDraw(destRect, FSprites[id]);
  Result := CreateNilValue;
end;

procedure RegisterCanvasNatives;
begin
  defineNative('canvasWidth', canvasWidthNative, 0);
  defineNative('canvasHeight', canvasHeightNative, 0);
  defineNative('clearCanvas', clearCanvasNative, 0);
  defineNative('setColor', setColorNative, 3);
  defineNative('fillRect', fillRectNative, 4);
  defineNative('drawText', drawTextNative, 3);
  defineNative('present', presentNative, 0);
  defineNative('drawLine', drawLineNative, 4);
  defineNative('fillCircle', fillCircleNative, 3);
  defineNative('createSprite', createSpriteNative, 3);
  defineNative('drawSprite', drawSpriteNative, 3);
  defineNative('drawSpriteScaled', drawSpriteScaledNative, 4);
end;

end.
