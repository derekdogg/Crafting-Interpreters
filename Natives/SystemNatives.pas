unit SystemNatives;

interface

procedure RegisterSystemNatives;

implementation

uses
  System.SysUtils, Classes, Windows, Chunk_Types, NativeRegistry;

function clockNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(GetTickCount / 1000.0);
end;

function collectGarbageNative(argCount: integer; args: pValue): TValue;
begin
  CollectGarbage;
  Result := CreateNilValue;
end;

function assertNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount < 1) or (argCount > 2) then
  begin
    RuntimeError('assert() takes 1 or 2 arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  if IsFalsey(args[0]) then
  begin
    if (argCount = 2) and IsString(args[1]) then
      RuntimeError('Assertion failed: ' + String(AsAnsiString(args[1])))
    else
      RuntimeError('Assertion failed.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNilValue;
end;

function bytesAllocatedNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(VM.MemTracker.BytesAllocated);
end;

function objectsAllocatedNative(argCount: integer; args: pValue): TValue;
var
  obj: pObj;
  count: integer;
begin
  count := 0;
  obj := VM.MemTracker.CreatedObjects;
  while obj <> nil do
  begin
    count := count + 1;
    obj := obj.Next;
  end;
  Result := CreateNumber(count);
end;

function vmStackDepthNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber( (NativeUInt(VM.Stack.StackTop) - NativeUInt(VM.Stack.Values)) div SizeOf(TValue) );
end;

function vmStackCapacityNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(VM.Stack.CapacityEnd - VM.Stack.Values);
end;

function vmCallDepthNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(VM.FrameCount);
end;

function vmOpenUpvaluesNative(argCount: integer; args: pValue): TValue;
var
  upval: pObjUpvalue;
  count: integer;
begin
  count := 0;
  upval := VM.OpenUpvalues;
  while upval <> nil do
  begin
    count := count + 1;
    upval := upval^.next;
  end;
  Result := CreateNumber(count);
end;

function vmFramesMaxNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(FRAMES_MAX);
end;

function vmStackMaxNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(STACK_MAX);
end;

function gcNextThresholdNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(VM.MemTracker.NextGC);
end;

function gcCollectionCountNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(VM.MemTracker.GCCollections);
end;

function internTableStatsNative(argCount: integer; args: pValue): TValue;
var
  dict: pObjDictionary;
  liveCount, capacity, tombstones, i: integer;
  keyStr: pObjString;
  dictVal: TValue;
begin
  capacity := VM.Strings.CurrentCapacity;
  // Count live entries and tombstones separately
  liveCount := 0;
  tombstones := 0;
  if (VM.Strings.Entries <> nil) and (capacity > 0) then
    for i := 0 to capacity - 1 do
      if VM.Strings.Entries[i].key <> nil then
        Inc(liveCount)
      else if not isNill(VM.Strings.Entries[i].value) then
        Inc(tombstones);
  // Create dictionary and push onto stack to protect from GC during allocations
  dict := newDictionary(VM.MemTracker);
  dictVal := CreateObject(pObj(dict));
  pushStack(VM.Stack, dictVal);
  // Root each key string before DictSet ? DictGrow can trigger GC
  keyStr := CreateString('liveStrings', VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(keyStr)));
  DictSet(dict, CreateObject(pObj(keyStr)), CreateNumber(liveCount), VM.MemTracker);
  Dec(VM.Stack.StackTop);

  keyStr := CreateString('slots', VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(keyStr)));
  DictSet(dict, CreateObject(pObj(keyStr)), CreateNumber(capacity), VM.MemTracker);
  Dec(VM.Stack.StackTop);

  keyStr := CreateString('tombstones', VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(keyStr)));
  DictSet(dict, CreateObject(pObj(keyStr)), CreateNumber(tombstones), VM.MemTracker);
  Dec(VM.Stack.StackTop);

  // Pop the dict protection slot ? caller will push result onto stack
  Dec(VM.Stack.StackTop);
  Result := dictVal;
end;

function envNative(argCount: integer; args: pValue): TValue;
var
  name, val : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('env() takes exactly 1 argument.');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('env() argument must be a string.');
    Exit(CreateNilValue);
  end;
  name := AsAnsiString(args[0]);
  val := AnsiString(GetEnvironmentVariable(String(name)));
  if val = '' then
    Result := CreateNilValue
  else
    Result := CreateStringValue(val);
end;

function loadEnvNative(argCount: integer; args: pValue): TValue;
var
  filePath : AnsiString;
  lines : TStringList;
  i, eqPos : integer;
  line, key, value : string;
begin
  if argCount > 1 then
  begin
    RuntimeError('loadEnv() takes 0 or 1 arguments.');
    Exit(CreateNilValue);
  end;

  if argCount = 1 then
  begin
    if not isString(args[0]) then
    begin
      RuntimeError('loadEnv() argument must be a string (file path).');
      Exit(CreateNilValue);
    end;
    filePath := AsAnsiString(args[0]);
  end
  else
    filePath := AnsiString(ExtractFilePath(ParamStr(0))) + '.env';

  if not FileExists(String(filePath)) then
  begin
    RuntimeError('loadEnv() file not found: ' + String(filePath));
    Exit(CreateNilValue);
  end;

  lines := TStringList.Create;
  try
    lines.LoadFromFile(String(filePath));
    for i := 0 to lines.Count - 1 do
    begin
      line := Trim(lines[i]);
      if (line = '') or (line[1] = '#') then
        Continue;
      eqPos := Pos('=', line);
      if eqPos = 0 then
        Continue;
      key := Trim(Copy(line, 1, eqPos - 1));
      value := Trim(Copy(line, eqPos + 1, MaxInt));
      // Strip surrounding quotes if present
      if (Length(value) >= 2) and
         ((value[1] = '"') or (value[1] = '''')) and
         (value[Length(value)] = value[1]) then
        value := Copy(value, 2, Length(value) - 2);
      SetEnvironmentVariable(PChar(key), PChar(value));
    end;
  finally
    lines.Free;
  end;
  Result := CreateNilValue;
end;

// abort(message) or abort(message, exitCode)
// Immediately halts the VM. Default exit code is 1.
function abortNative(argCount: integer; args: pValue): TValue;
var
  msg: string;
  code: Integer;
begin
  if (argCount < 1) or (argCount > 2) then
  begin
    RuntimeError('abort() takes 1 or 2 arguments (message, [exitCode]).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('abort() first argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  msg := String(AsAnsiString(args[0]));
  code := 1;
  if argCount = 2 then
  begin
    if not isNumber(args[1]) then
    begin
      RuntimeError('abort() second argument must be a number (exit code).');
      Result := CreateNilValue;
      Exit;
    end;
    code := AsInteger(args[1]);
  end;
  raise ELoxHalt.Create(msg, code);
end;

// getCwd() -> current working directory as string
function getCwdNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateStringValue(AnsiString(GetCurrentDir));
end;

// chdir(path) -> true on success, runtime error on failure
function chdirNative(argCount: integer; args: pValue): TValue;
var
  path: string;
begin
  if argCount <> 1 then
  begin
    RuntimeError('chdir() takes exactly 1 argument (path).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('chdir() argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  path := String(AsAnsiString(args[0]));
  if not DirectoryExists(path) then
  begin
    RuntimeError('chdir(): directory does not exist: ' + path);
    Result := CreateNilValue;
    Exit;
  end;
  SetCurrentDir(path);
  Result := CreateBoolean(True);
end;

procedure RegisterSystemNatives;
begin
  defineNative('clock', clockNative, 0);
  defineNative('collectGarbage', collectGarbageNative, 0);
  defineNative('assert', assertNative);           // 1-2 args, self-validates
  defineNative('bytesAllocated', bytesAllocatedNative, 0);
  defineNative('objectsAllocated', objectsAllocatedNative, 0);
  defineNative('vmStackDepth', vmStackDepthNative, 0);
  defineNative('vmStackCapacity', vmStackCapacityNative, 0);
  defineNative('vmCallDepth', vmCallDepthNative, 0);
  defineNative('vmOpenUpvalues', vmOpenUpvaluesNative, 0);
  defineNative('vmFramesMax', vmFramesMaxNative, 0);
  defineNative('vmStackMax', vmStackMaxNative, 0);
  defineNative('gcNextThreshold', gcNextThresholdNative, 0);
  defineNative('gcCollectionCount', gcCollectionCountNative, 0);
  defineNative('internTableStats', internTableStatsNative, 0);
  defineNative('env', envNative, 1);
  defineNative('loadEnv', loadEnvNative, -1);
  defineNative('abort', abortNative, -1);       // 1-2 args, self-validates
  defineNative('getCwd', getCwdNative, 0);
  defineNative('chdir', chdirNative, 1);
end;

initialization
  RegisterNativeModule(RegisterSystemNatives);

end.
