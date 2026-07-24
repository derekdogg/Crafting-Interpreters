unit MathNatives;

interface

procedure RegisterMathNatives;

implementation

uses
  Math, Suto, NativeRegistry;

// ---- Math native functions ----

function absNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('abs() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('abs() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Abs(GetNumber(args[0])));
end;

function floorNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('floor() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('floor() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Math.Floor(GetNumber(args[0])));
end;

function ceilNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('ceil() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('ceil() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Math.Ceil(GetNumber(args[0])));
end;

function roundNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('round() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('round() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Round(GetNumber(args[0])));
end;

function minNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('min() takes exactly 2 arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) or not isNumber(args[1]) then
  begin
    RuntimeError('min() arguments must be numbers.');
    Result := CreateNilValue;
    Exit;
  end;
  if GetNumber(args[0]) <= GetNumber(args[1]) then
    Result := CreateNumber(GetNumber(args[0]))
  else
    Result := CreateNumber(GetNumber(args[1]));
end;

function maxNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('max() takes exactly 2 arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) or not isNumber(args[1]) then
  begin
    RuntimeError('max() arguments must be numbers.');
    Result := CreateNilValue;
    Exit;
  end;
  if GetNumber(args[0]) >= GetNumber(args[1]) then
    Result := CreateNumber(GetNumber(args[0]))
  else
    Result := CreateNumber(GetNumber(args[1]));
end;

function sqrtNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('sqrt() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('sqrt() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  if GetNumber(args[0]) < 0 then
  begin
    RuntimeError('sqrt() argument must not be negative.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Sqrt(GetNumber(args[0])));
end;

function powNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('pow() takes exactly 2 arguments (base, exponent).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) or not isNumber(args[1]) then
  begin
    RuntimeError('pow() arguments must be numbers.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Math.Power(GetNumber(args[0]), GetNumber(args[1])));
end;

function sinNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('sin() takes exactly 1 argument (radians).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('sin() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Sin(GetNumber(args[0])));
end;

function cosNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('cos() takes exactly 1 argument (radians).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('cos() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Cos(GetNumber(args[0])));
end;

function randomNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 0 then
  begin
    RuntimeError('random() takes no arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(Random);
end;

// rectsOverlap(ax, ay, aw, ah, bx, by, bw, bh) -> bool
// Axis-aligned bounding-box overlap test. Ported from the identical
// hand-written Lox helper used by every game demo's collision detection
// (e.g. samples/demos/1942.lox's rectsOverlap) - called many times per
// frame in tight nested loops (every bullet against every enemy, every
// enemy against the player, etc.), so moving it to a native cuts out a
// Lox function-call + 4 comparisons' worth of VM dispatch per check.
// Same edge case as the original: touching-but-not-crossing edges
// (e.g. ax+aw = bx) do NOT count as overlapping.
function rectsOverlapNative(argCount: integer; args: pValue): TValue;
var
  ax, ay, aw, ah, bx, bY, bw, bh: Double;
  i: Integer;
begin
  if argCount <> 8 then
  begin
    RuntimeError('rectsOverlap() takes exactly 8 arguments (ax, ay, aw, ah, bx, by, bw, bh).');
    Result := CreateNilValue;
    Exit;
  end;
  for i := 0 to 7 do
    if not isNumber(args[i]) then
    begin
      RuntimeError('rectsOverlap() arguments must be numbers.');
      Result := CreateNilValue;
      Exit;
    end;
  ax := GetNumber(args[0]); ay := GetNumber(args[1]);
  aw := GetNumber(args[2]); ah := GetNumber(args[3]);
  bx := GetNumber(args[4]); bY := GetNumber(args[5]);
  bw := GetNumber(args[6]); bh := GetNumber(args[7]);
  Result := CreateBoolean((ax + aw > bx) and (bx + bw > ax) and
                          (ay + ah > bY) and (bY + bh > ay));
end;

procedure RegisterMathNatives;
begin
  defineNative('abs', absNative, 1);
  defineNative('floor', floorNative, 1);
  defineNative('ceil', ceilNative, 1);
  defineNative('round', roundNative, 1);
  defineNative('min', minNative, 2);
  defineNative('max', maxNative, 2);
  defineNative('sqrt', sqrtNative, 1);
  defineNative('pow', powNative, 2);
  defineNative('sin', sinNative, 1);
  defineNative('cos', cosNative, 1);
  defineNative('random', randomNative, 0);
  defineNative('rectsOverlap', rectsOverlapNative, 8);
end;

initialization
  RegisterNativeModule(RegisterMathNatives);

end.
