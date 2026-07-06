unit DictNatives;

interface

procedure RegisterDictNatives;

implementation

uses
  System.SysUtils, Suto, NativeRegistry;

// ---- Dictionary native functions ----

function dictNewNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
begin
  if argCount <> 0 then
  begin
    RuntimeError('dictNew() takes no arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := newDictionary(VM.MemTracker);
  Result := CreateObject(pObj(dict));
end;

function dictSetNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
begin
  if argCount <> 3 then
  begin
    RuntimeError('dictSet() takes 3 arguments (dict, key, value).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictSet() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  DictSet(dict, args[1], args[2], VM.MemTracker);
  Result := args[2];
end;

function dictGetNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
  value : TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('dictGet() takes 2 arguments (dict, key).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictGet() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  if not DictGet(dict, args[1], value) then
  begin
    RuntimeError('Key not found in dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := value;
end;

function dictHasNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
  value : TValue;
begin
  if argCount <> 2 then
  begin
    RuntimeError('dictHas() takes 2 arguments (dict, key).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictHas() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  Result := CreateBoolean(DictGet(dict, args[1], value));
end;

function dictDeleteNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
begin
  if argCount <> 2 then
  begin
    RuntimeError('dictDelete() takes 2 arguments (dict, key).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictDelete() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  Result := CreateBoolean(DictDelete(dict, args[1]));
end;

function dictKeysNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
  arr : pObjArray;
  i, newCap, oldSize, newSize : integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('dictKeys() takes 1 argument (dict).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictKeys() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  arr := newArray(VM.MemTracker);
  // Pre-allocate elements for all keys
  if dict^.Count > 0 then
  begin
    newCap := dict^.Count;
    oldSize := 0;
    newSize := newCap * SizeOf(TValue);
    // Push arr to stack for GC safety before allocating
    PushStack(vm.stack, CreateObject(pObj(arr)));
    Allocate(Pointer(arr^.Elements), oldSize, newSize, VM.MemTracker);
    arr^.Capacity := newCap;
    for i := 0 to dict^.Capacity - 1 do
    begin
      if dict^.Entries[i].occupied and not dict^.Entries[i].tombstone then
      begin
        arr^.Elements[arr^.Count] := dict^.Entries[i].key;
        Inc(arr^.Count);
      end;
    end;
    Dec(VM.Stack.StackTop);
  end;
  Result := CreateObject(pObj(arr));
end;

function dictSizeNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
begin
  if argCount <> 1 then
  begin
    RuntimeError('dictSize() takes 1 argument (dict).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictSize() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  Result := CreateNumber(dict^.Count);
end;

function dictValuesNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
  arr : pObjArray;
  i, newCap, oldSize, newSize : integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('dictValues() takes 1 argument (dict).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('First argument to dictValues() must be a dictionary.');
    Result := CreateNilValue;
    Exit;
  end;
  dict := pObjDictionary(GetObject(args[0]));
  arr := newArray(VM.MemTracker);
  if dict^.Count > 0 then
  begin
    newCap := dict^.Count;
    oldSize := 0;
    newSize := newCap * SizeOf(TValue);
    PushStack(vm.stack, CreateObject(pObj(arr)));
    Allocate(Pointer(arr^.Elements), oldSize, newSize, VM.MemTracker);
    arr^.Capacity := newCap;
    for i := 0 to dict^.Capacity - 1 do
    begin
      if dict^.Entries[i].occupied and not dict^.Entries[i].tombstone then
      begin
        arr^.Elements[arr^.Count] := dict^.Entries[i].value;
        Inc(arr^.Count);
      end;
    end;
    Dec(VM.Stack.StackTop);
  end;
  Result := CreateObject(pObj(arr));
end;

procedure RegisterDictNatives;
begin
  defineNative('dictNew', dictNewNative, 0);
  defineNative('dictSet', dictSetNative, 3);
  defineNative('dictGet', dictGetNative, 2);
  defineNative('dictHas', dictHasNative, 2);
  defineNative('dictDelete', dictDeleteNative, 2);
  defineNative('dictKeys', dictKeysNative, 1);
  defineNative('dictSize', dictSizeNative, 1);
  defineNative('dictValues', dictValuesNative, 1);
end;

initialization
  RegisterNativeModule(RegisterDictNatives);

end.
