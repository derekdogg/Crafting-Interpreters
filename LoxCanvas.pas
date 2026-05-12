unit LoxCanvas;

interface

uses
  Vcl.ExtCtrls, Vcl.Graphics, Chunk_Types;

procedure InitCanvas(APaintBox: TPaintBox);
procedure FreeCanvas;
procedure RegisterCanvasNatives;

implementation

uses
  SysUtils, Types, Classes;

type
  TCanvasHelper = class
    procedure PaintBoxPaint(Sender: TObject);
  end;

var
  FPaintBox: TPaintBox;
  FBackBuffer: TBitmap;
  FCurrentColor: TColor;
  FCanvasHelper: TCanvasHelper;

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
  FCanvasHelper := TCanvasHelper.Create;
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackBuffer.SetSize(FPaintBox.Width, FPaintBox.Height);
  FBackBuffer.Canvas.Brush.Color := clBlack;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));
  FPaintBox.OnPaint := FCanvasHelper.PaintBoxPaint;
end;

procedure FreeCanvas;
begin
  FreeAndNil(FBackBuffer);
  FreeAndNil(FCanvasHelper);
  FPaintBox := nil;
end;

// -- Native functions exposed to Lox --

function canvasWidthNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(FPaintBox.Width);
end;

function canvasHeightNative(argCount: integer; args: pValue): TValue;
begin
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
  r := Trunc(args[0].NumberValue) and $FF;
  g := Trunc(args[1].NumberValue) and $FF;
  b := Trunc(args[2].NumberValue) and $FF;
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
  FBackBuffer.Canvas.Pen.Style := psClear;
  FBackBuffer.Canvas.FillRect(Rect(x, y, x + w, y + h));
  FBackBuffer.Canvas.Pen.Style := psSolid;
  Result := CreateNilValue;
end;

function drawTextNative(argCount: integer; args: pValue): TValue;
var
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
  FBackBuffer.Canvas.Brush.Style := bsClear;
  FBackBuffer.Canvas.Font.Color := FCurrentColor;
  FBackBuffer.Canvas.Font.Size := 14;
  FBackBuffer.Canvas.TextOut(x, y, String(s));
  FBackBuffer.Canvas.Brush.Style := bsSolid;
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
  FBackBuffer.Canvas.Brush.Color := FCurrentColor;
  FBackBuffer.Canvas.Pen.Style := psClear;
  FBackBuffer.Canvas.Ellipse(cx - r, cy - r, cx + r, cy + r);
  FBackBuffer.Canvas.Pen.Style := psSolid;
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
end;

end.
