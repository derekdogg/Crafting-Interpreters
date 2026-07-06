unit ArrayNatives;

interface

procedure RegisterArrayNatives;

implementation

uses
  System.SysUtils, Suto, NativeRegistry;

// ---- Array native functions ----

function arrayNewNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
begin
  if argCount <> 0 then
  begin
    RuntimeError('newArray() takes no arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := newArray(VM.MemTracker);
  Result := CreateObject(pObj(arr));
end;

function arrayPushNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
begin
  if argCount <> 2 then
  begin
    RuntimeError('arrayPush() takes 2 arguments (array, value).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arrayPush() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := pObjArray(GetObject(args[0]));
  EnsureArrayCapacity(arr, VM.MemTracker);
  arr^.Elements[arr^.Count] := args[1];
  Inc(arr^.Count);
  Result := args[0]; // return the array for chaining
end;

function arrayPopNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
begin
  if argCount <> 1 then
  begin
    RuntimeError('arrayPop() takes 1 argument (array).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arrayPop() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := pObjArray(GetObject(args[0]));
  if arr^.Count = 0 then
  begin
    RuntimeError('Cannot pop from an empty array.');
    Result := CreateNilValue;
    Exit;
  end;
  Dec(arr^.Count);
  Result := arr^.Elements[arr^.Count];
end;

function arrayGetNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
  idx : integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('arrayGet() takes 2 arguments (array, index).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arrayGet() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[1]) then
  begin
    RuntimeError('Index argument to arrayGet() must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := pObjArray(GetObject(args[0]));
  idx := AsInteger(args[1]);
  if (idx < 0) or (idx >= arr^.Count) then
  begin
    RuntimeError('Array index ' + IntToStr(idx) + ' out of bounds [0, ' + IntToStr(arr^.Count - 1) + '].');
    Result := CreateNilValue;
    Exit;
  end;
  Result := arr^.Elements[idx];
end;

function arraySetNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
  idx : integer;
begin
  if argCount <> 3 then
  begin
    RuntimeError('arraySet() takes 3 arguments (array, index, value).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arraySet() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[1]) then
  begin
    RuntimeError('Index argument to arraySet() must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := pObjArray(GetObject(args[0]));
  idx := AsInteger(args[1]);
  if (idx < 0) or (idx >= arr^.Count) then
  begin
    RuntimeError('Array index ' + IntToStr(idx) + ' out of bounds [0, ' + IntToStr(arr^.Count - 1) + '].');
    Result := CreateNilValue;
    Exit;
  end;
  arr^.Elements[idx] := args[2];
  Result := args[2];
end;

function arrayLenNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
begin
  if argCount <> 1 then
  begin
    RuntimeError('arrayLen() takes 1 argument (array).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arrayLen() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := pObjArray(GetObject(args[0]));
  Result := CreateNumber(arr^.Count);
end;

function arrayRemoveNative(argCount: integer; args: pValue): TValue;
var
  arr : pObjArray;
  idx, j : integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('arrayRemove() takes 2 arguments (array, index).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arrayRemove() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[1]) then
  begin
    RuntimeError('Index argument to arrayRemove() must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  arr := pObjArray(GetObject(args[0]));
  idx := AsInteger(args[1]);
  if (idx < 0) or (idx >= arr^.Count) then
  begin
    RuntimeError('Array index ' + IntToStr(idx) + ' out of bounds [0, ' + IntToStr(arr^.Count - 1) + '].');
    Result := CreateNilValue;
    Exit;
  end;
  Result := arr^.Elements[idx];
  // Shift elements left
  for j := idx to arr^.Count - 2 do
    arr^.Elements[j] := arr^.Elements[j + 1];
  Dec(arr^.Count);
end;

procedure RegisterArrayNatives;
begin
  defineNative('newArray', arrayNewNative, 0);
  defineNative('arrayPush', arrayPushNative, 2);
  defineNative('arrayPop', arrayPopNative, 1);
  defineNative('arrayGet', arrayGetNative, 2);
  defineNative('arraySet', arraySetNative, 3);
  defineNative('arrayLen', arrayLenNative, 1);
  defineNative('arrayRemove', arrayRemoveNative, 2);
end;

initialization
  RegisterNativeModule(RegisterArrayNatives);

end.
