unit EventNatives;

// ============================================================
// Native functions for the Lox event engine.
// Registers: onKeyPressed, onKeyReleased, onKeyHeld,
//            onMouseDown, onMouseUp, onMouseMove, processEvents
// Also provides simulation natives for scripted testing:
//            simulateKeyDown, simulateKeyUp,
//            simulateMouseDown, simulateMouseUp, simulateMouseMove
// ============================================================

interface

uses
  Chunk_Types;

procedure RegisterEventNatives;

implementation

uses
  Vcl.Forms, System.SysUtils, LoxEventEngine;

// --- Callback registration natives ---

function onKeyPressedNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isClosure(args[0]) then
  begin
    RuntimeError('onKeyPressed() takes 1 function argument.');
    Exit(CreateNilValue);
  end;
  ActiveEngine.SetCallback(eckKeyPressed, args[0]);
  Result := CreateNilValue;
end;

function onKeyReleasedNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isClosure(args[0]) then
  begin
    RuntimeError('onKeyReleased() takes 1 function argument.');
    Exit(CreateNilValue);
  end;
  ActiveEngine.SetCallback(eckKeyReleased, args[0]);
  Result := CreateNilValue;
end;

function onKeyHeldNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isClosure(args[0]) then
  begin
    RuntimeError('onKeyHeld() takes 1 function argument.');
    Exit(CreateNilValue);
  end;
  ActiveEngine.SetCallback(eckKeyHeld, args[0]);
  Result := CreateNilValue;
end;

function onMouseDownNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isClosure(args[0]) then
  begin
    RuntimeError('onMouseDown() takes 1 function argument.');
    Exit(CreateNilValue);
  end;
  ActiveEngine.SetCallback(eckMouseDown, args[0]);
  Result := CreateNilValue;
end;

function onMouseUpNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isClosure(args[0]) then
  begin
    RuntimeError('onMouseUp() takes 1 function argument.');
    Exit(CreateNilValue);
  end;
  ActiveEngine.SetCallback(eckMouseUp, args[0]);
  Result := CreateNilValue;
end;

function onMouseMoveNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isClosure(args[0]) then
  begin
    RuntimeError('onMouseMove() takes 1 function argument.');
    Exit(CreateNilValue);
  end;
  ActiveEngine.SetCallback(eckMouseMove, args[0]);
  Result := CreateNilValue;
end;

function processEventsNative(argCount: integer; args: pValue): TValue;
begin
  // Flush print output
  ActiveEngine.FlushOutput;
  // Pump Windows messages (delivers key/mouse to VCL handlers)
  Application.ProcessMessages;
  // Dispatch any pending events as callbacks
  ActiveEngine.DispatchPendingEvents;
  // Check for abort (engine stopped or host app closing)
  if not ActiveEngine.Running then
  begin
    RuntimeError('Script aborted: window closed.');
    Exit(CreateNilValue);
  end;
  if Assigned(ActiveEngine.OnCheckAbort) and ActiveEngine.OnCheckAbort() then
  begin
    RuntimeError('Script aborted: application closing.');
    Exit(CreateNilValue);
  end;
  Result := CreateNilValue;
end;

// --- Simulation natives (for scripted testing) ---

function simulateKeyDownNative(argCount: integer; args: pValue): TValue;
var
  key: string;
begin
  if (argCount <> 1) or not isString(args[0]) then
  begin
    RuntimeError('simulateKeyDown() takes 1 string argument.');
    Exit(CreateNilValue);
  end;
  key := string(AsAnsiString(args[0]));
  ActiveEngine.QueueKeyDown(key);
  Result := CreateNilValue;
end;

function simulateKeyUpNative(argCount: integer; args: pValue): TValue;
var
  key: string;
begin
  if (argCount <> 1) or not isString(args[0]) then
  begin
    RuntimeError('simulateKeyUp() takes 1 string argument.');
    Exit(CreateNilValue);
  end;
  key := string(AsAnsiString(args[0]));
  ActiveEngine.QueueKeyUp(key);
  Result := CreateNilValue;
end;

function simulateMouseDownNative(argCount: integer; args: pValue): TValue;
var
  btn, x, y: Integer;
begin
  if (argCount <> 3) or not isNumber(args[0]) or not isNumber(args[1]) or not isNumber(args[2]) then
  begin
    RuntimeError('simulateMouseDown() takes 3 number arguments (btn, x, y).');
    Exit(CreateNilValue);
  end;
  btn := Trunc(GetNumber(args[0]));
  x := Trunc(GetNumber(args[1]));
  y := Trunc(GetNumber(args[2]));
  ActiveEngine.QueueMouseDown(btn, x, y);
  Result := CreateNilValue;
end;

function simulateMouseUpNative(argCount: integer; args: pValue): TValue;
var
  btn, x, y: Integer;
begin
  if (argCount <> 3) or not isNumber(args[0]) or not isNumber(args[1]) or not isNumber(args[2]) then
  begin
    RuntimeError('simulateMouseUp() takes 3 number arguments (btn, x, y).');
    Exit(CreateNilValue);
  end;
  btn := Trunc(GetNumber(args[0]));
  x := Trunc(GetNumber(args[1]));
  y := Trunc(GetNumber(args[2]));
  ActiveEngine.QueueMouseUp(btn, x, y);
  Result := CreateNilValue;
end;

function simulateMouseMoveNative(argCount: integer; args: pValue): TValue;
var
  x, y: Integer;
begin
  if (argCount <> 2) or not isNumber(args[0]) or not isNumber(args[1]) then
  begin
    RuntimeError('simulateMouseMove() takes 2 number arguments (x, y).');
    Exit(CreateNilValue);
  end;
  x := Trunc(GetNumber(args[0]));
  y := Trunc(GetNumber(args[1]));
  ActiveEngine.QueueMouseMove(x, y);
  Result := CreateNilValue;
end;

// --- Registration ---

procedure RegisterEventNatives;
begin
  defineNative('onKeyPressed', onKeyPressedNative, 1);
  defineNative('onKeyReleased', onKeyReleasedNative, 1);
  defineNative('onKeyHeld', onKeyHeldNative, 1);
  defineNative('onMouseDown', onMouseDownNative, 1);
  defineNative('onMouseUp', onMouseUpNative, 1);
  defineNative('onMouseMove', onMouseMoveNative, 1);
  defineNative('processEvents', processEventsNative, 0);
  // Simulation natives (for scripted testing — queue events without VCL)
  defineNative('simulateKeyDown', simulateKeyDownNative, 1);
  defineNative('simulateKeyUp', simulateKeyUpNative, 1);
  defineNative('simulateMouseDown', simulateMouseDownNative, 3);
  defineNative('simulateMouseUp', simulateMouseUpNative, 3);
  defineNative('simulateMouseMove', simulateMouseMoveNative, 2);
end;

end.
