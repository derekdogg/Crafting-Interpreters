unit CallbackNatives;

interface

procedure RegisterCallbackNatives;

implementation

uses
  Chunk_Types, NativeRegistry;

// ---- Callback test natives ----

// callWith(fn, arg) — invokes fn(arg) and returns its result.
// Used to verify InvokeCallback works correctly from within a native.
function callWithNative(argCount: integer; args: pValue): TValue;
var
  ret: TValue;
  res: TInterpretResult;
  callArgs: array[0..0] of TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('callWith() takes exactly 2 arguments (fn, arg).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isClosure(args[0]) then
  begin
    RuntimeError('callWith() first argument must be a function.');
    Result := CreateNilValue;
    Exit;
  end;

  callArgs[0] := args[1];
  res := InvokeCallback(args[0], callArgs, ret);
  if res.code <> INTERPRET_OK then
  begin
    // Error already set by InvokeCallback/Run
    Result := CreateNilValue;
    Exit;
  end;
  Result := ret;
end;

// invoke(fn, ...) — invokes fn with the remaining arguments.
// Variadic: first arg is the closure, rest are passed through.
function invokeNative(argCount: integer; args: pValue): TValue;
var
  ret: TValue;
  res: TInterpretResult;
  callArgs: array of TValue;
  i: Integer;
begin
  if argCount < 1 then
  begin
    RuntimeError('invoke() takes at least 1 argument (fn, ...).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isClosure(args[0]) then
  begin
    RuntimeError('invoke() first argument must be a function.');
    Result := CreateNilValue;
    Exit;
  end;

  SetLength(callArgs, argCount - 1);
  for i := 1 to argCount - 1 do
    callArgs[i - 1] := args[i];

  res := InvokeCallback(args[0], callArgs, ret);
  if res.code <> INTERPRET_OK then
  begin
    Result := CreateNilValue;
    Exit;
  end;
  Result := ret;
end;

procedure RegisterCallbackNatives;
begin
  defineNative('callWith', callWithNative, 2);
  defineNative('invoke', invokeNative, -1);  // variadic
end;

initialization
  RegisterNativeModule(RegisterCallbackNatives);

end.
