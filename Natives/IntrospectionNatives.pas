unit IntrospectionNatives;

interface

procedure RegisterIntrospectionNatives;

implementation

uses
  System.SysUtils, System.Rtti, System.TypInfo, Classes, Chunk_Types, NativeRegistry;

// ---- StringList wrapper ----

function SL_Add(instance: Pointer; argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('add() takes 1 argument.');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('add() argument must be a string.');
    Exit(CreateNilValue);
  end;
  TStringList(instance).Add(String(AsAnsiString(args[0])));
  Result := CreateNilValue;
end;

function SL_Get(instance: Pointer; argCount: integer; args: pValue): TValue;
var
  idx : integer;
  s : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('get() takes 1 argument.');
    Exit(CreateNilValue);
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('get() argument must be a number.');
    Exit(CreateNilValue);
  end;
  idx := AsInteger(args[0]);
  if (idx < 0) or (idx >= TStringList(instance).Count) then
  begin
    RuntimeError('StringList index ' + IntToStr(idx) + ' out of bounds.');
    Exit(CreateNilValue);
  end;
  s := AnsiString(TStringList(instance)[idx]);
  Result := StringToValue(CreateString(s, VM.MemTracker));
end;

function SL_Count(instance: Pointer; argCount: integer; args: pValue): TValue;
begin
  if argCount <> 0 then
  begin
    RuntimeError('count() takes no arguments.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(TStringList(instance).Count);
end;

function SL_Remove(instance: Pointer; argCount: integer; args: pValue): TValue;
var
  idx : integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('remove() takes 1 argument.');
    Exit(CreateNilValue);
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('remove() argument must be a number.');
    Exit(CreateNilValue);
  end;
  idx := AsInteger(args[0]);
  if (idx < 0) or (idx >= TStringList(instance).Count) then
  begin
    RuntimeError('StringList index ' + IntToStr(idx) + ' out of bounds.');
    Exit(CreateNilValue);
  end;
  TStringList(instance).Delete(idx);
  Result := CreateNilValue;
end;

procedure SL_Destroy(instance: Pointer);
begin
  TStringList(instance).Free;
end;

function stringListNative(argCount: integer; args: pValue): TValue;
var
  classInfo : pNativeClassInfo;
  obj : pObjNativeObject;
begin
  if argCount <> 0 then
  begin
    RuntimeError('StringList() takes no arguments.');
    Exit(CreateNilValue);
  end;
  classInfo := findNativeClass('StringList');
  Assert(classInfo <> nil, 'StringList class not registered');
  obj := newNativeObject(TStringList.Create, classInfo, VM.MemTracker);
  Result := CreateObject(pObj(obj));
end;

// ---- Class discovery ----

function HasAttributeLocal(const attrs: TArray<TCustomAttribute>; attrClass: TClass): Boolean;
var
  a: TCustomAttribute;
begin
  for a in attrs do
    if a is attrClass then
      Exit(True);
  Result := False;
end;

function loxClassesNative(argCount: integer; args: pValue): TValue;
var
  ctx: TRttiContext;
  t: TRttiType;
  attr: TCustomAttribute;
  arr: pObjArray;
  nameStr: pObjString;
  names: TStringList;
  i: Integer;
begin
  ctx := TRttiContext.Create;
  names := TStringList.Create;
  try
    for t in ctx.GetTypes do
      if t is TRttiInstanceType then
        for attr in t.GetAttributes do
          if attr is LoxClassAttribute then
            names.Add(LoxClassAttribute(attr).Name);
    names.Sort;

    arr := newArray(VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(arr)));
    for i := 0 to names.Count - 1 do
    begin
      nameStr := CreateString(AnsiString(names[i]), VM.MemTracker);
      pushStack(VM.Stack, StringToValue(nameStr));
      EnsureArrayCapacity(arr, VM.MemTracker);
      arr^.Elements[arr^.Count] := StringToValue(nameStr);
      Inc(arr^.Count);
      Dec(VM.Stack.StackTop);
    end;
    Dec(VM.Stack.StackTop); // pop the array guard
    Result := CreateObject(pObj(arr));
  finally
    names.Free;
    ctx.Free;
  end;
end;

function loxClassInfoNative(argCount: integer; args: pValue): TValue;
var
  ctx: TRttiContext;
  t: TRttiType;
  attr: TCustomAttribute;
  className: string;
  foundClass: TClass;
  rt: TRttiType;
  prop: TRttiProperty;
  method: TRttiMethod;
  dict: pObjDictionary;
  dictVal: TValue;
  propsArr, methodsArr: pObjArray;
  nameStr: pObjString;
  keyStr: pObjString;
  i: Integer;
  seen: TStringList;
  mName: string;
begin
  if not isString(args[0]) then
  begin
    RuntimeError('loxClassInfo() argument must be a string.');
    Exit(CreateNilValue);
  end;
  className := String(AsAnsiString(args[0]));

  // Find the attributed class
  foundClass := nil;
  ctx := TRttiContext.Create;
  try
    for t in ctx.GetTypes do
      if t is TRttiInstanceType then
        for attr in t.GetAttributes do
          if (attr is LoxClassAttribute) and SameText(LoxClassAttribute(attr).Name, className) then
          begin
            foundClass := TRttiInstanceType(t).MetaclassType;
            Break;
          end;

    if foundClass = nil then
    begin
      RuntimeError('loxClassInfo(): class "' + className + '" not found.');
      Exit(CreateNilValue);
    end;

    rt := ctx.GetType(foundClass);

    // Build result dict ? push everything for GC safety
    dict := newDictionary(VM.MemTracker);
    dictVal := CreateObject(pObj(dict));
    pushStack(VM.Stack, dictVal);

    // --- Properties array (sorted for deterministic output) ---
    propsArr := newArray(VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(propsArr)));
    seen := TStringList.Create;
    try
      seen.CaseSensitive := False;
      seen.Sorted := True;
      seen.Duplicates := dupIgnore;
      for prop in rt.GetProperties do
      begin
        if not prop.IsReadable then Continue;
        if not HasAttributeLocal(prop.GetAttributes, LoxPropertyAttribute) then Continue;
        seen.Add(prop.Name);
      end;
      for i := 0 to seen.Count - 1 do
      begin
        nameStr := CreateString(AnsiString(seen[i]), VM.MemTracker);
        pushStack(VM.Stack, StringToValue(nameStr));
        EnsureArrayCapacity(propsArr, VM.MemTracker);
        propsArr^.Elements[propsArr^.Count] := StringToValue(nameStr);
        Inc(propsArr^.Count);
        Dec(VM.Stack.StackTop);
      end;
    finally
      seen.Free;
    end;
    // Set "properties" key
    keyStr := CreateString('properties', VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(keyStr)));
    DictSet(dict, CreateObject(pObj(keyStr)), CreateObject(pObj(propsArr)), VM.MemTracker);
    Dec(VM.Stack.StackTop);
    Dec(VM.Stack.StackTop); // pop propsArr guard

    // --- Methods array (attribute-filtered, deduplicated, sorted) ---
    methodsArr := newArray(VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(methodsArr)));
    seen := TStringList.Create;
    try
      seen.CaseSensitive := False;
      seen.Sorted := True;
      seen.Duplicates := dupIgnore;
      for method in rt.GetMethods do
      begin
        mName := method.Name;
        if not HasAttributeLocal(method.GetAttributes, LoxMethodAttribute) then
          Continue;
        if seen.IndexOf(mName) >= 0 then Continue;
        seen.Add(mName);
      end;
      for i := 0 to seen.Count - 1 do
      begin
        nameStr := CreateString(AnsiString(seen[i]), VM.MemTracker);
        pushStack(VM.Stack, StringToValue(nameStr));
        EnsureArrayCapacity(methodsArr, VM.MemTracker);
        methodsArr^.Elements[methodsArr^.Count] := StringToValue(nameStr);
        Inc(methodsArr^.Count);
        Dec(VM.Stack.StackTop);
      end;
    finally
      seen.Free;
    end;
    // Set "methods" key
    keyStr := CreateString('methods', VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(keyStr)));
    DictSet(dict, CreateObject(pObj(keyStr)), CreateObject(pObj(methodsArr)), VM.MemTracker);
    Dec(VM.Stack.StackTop);
    Dec(VM.Stack.StackTop); // pop methodsArr guard

    Dec(VM.Stack.StackTop); // pop dict guard
    Result := dictVal;
  finally
    ctx.Free;
  end;
end;

function loxObjectsNative(argCount: integer; args: pValue): TValue;
var
  i: Integer;
  entry: pEntry;
  arr: pObjArray;
  nameStr: pObjString;
  names: TStringList;
begin
  names := TStringList.Create;
  try
    // Walk VM.Globals, find entries whose value is a native object
    for i := 0 to VM.Globals.CurrentCapacity - 1 do
    begin
      entry := @VM.Globals.Entries[i];
      if (entry^.key = nil) then Continue;
      if not isNativeObject(entry^.value) then Continue;
      names.Add(String(ObjStringToAnsiString(entry^.key)));
    end;
    names.Sort;

    arr := newArray(VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(arr)));
    for i := 0 to names.Count - 1 do
    begin
      nameStr := CreateString(AnsiString(names[i]), VM.MemTracker);
      pushStack(VM.Stack, StringToValue(nameStr));
      EnsureArrayCapacity(arr, VM.MemTracker);
      arr^.Elements[arr^.Count] := StringToValue(nameStr);
      Inc(arr^.Count);
      Dec(VM.Stack.StackTop);
    end;
    Dec(VM.Stack.StackTop); // pop arr guard
    Result := CreateObject(pObj(arr));
  finally
    names.Free;
  end;
end;

function loxObjectInfoNative(argCount: integer; args: pValue): TValue;
var
  objName: AnsiString;
  i: Integer;
  entry: pEntry;
  nativeObj: pObjNativeObject;
  rt: TRttiType;
  prop: TRttiProperty;
  method: TRttiMethod;
  attr: TCustomAttribute;
  hasLoxClass: Boolean;
  dict: pObjDictionary;
  dictVal: TValue;
  propsArr, methodsArr: pObjArray;
  nameStr, keyStr: pObjString;
  classNameStr: pObjString;
  seen: TStringList;
  mName: string;
  ctx: TRttiContext;
begin
  if not isString(args[0]) then
  begin
    RuntimeError('loxObjectInfo() argument must be a string.');
    Exit(CreateNilValue);
  end;
  objName := AsAnsiString(args[0]);

  // Find the named native object in globals
  nativeObj := nil;
  for i := 0 to VM.Globals.CurrentCapacity - 1 do
  begin
    entry := @VM.Globals.Entries[i];
    if (entry^.key = nil) then Continue;
    if not isNativeObject(entry^.value) then Continue;
    if ObjStringToAnsiString(entry^.key) = objName then
    begin
      nativeObj := pObjNativeObject(GetObject(entry^.value));
      Break;
    end;
  end;

  if nativeObj = nil then
  begin
    RuntimeError('loxObjectInfo(): object "' + String(objName) + '" not found.');
    Exit(CreateNilValue);
  end;

  if (nativeObj^.classInfo = nil) or (not nativeObj^.classInfo^.rttiEnabled) then
  begin
    RuntimeError('loxObjectInfo(): object "' + String(objName) + '" has no RTTI.');
    Exit(CreateNilValue);
  end;

  ctx := TRttiContext.Create;
  try
    rt := ctx.GetType(nativeObj^.classInfo^.rttiClass);
    if rt = nil then
    begin
      RuntimeError('loxObjectInfo(): RTTI not available for "' + String(objName) + '".');
      Exit(CreateNilValue);
    end;

    // Check if this class has [LoxClass] ? if so, only show attributed members
    hasLoxClass := False;
    for attr in rt.GetAttributes do
      if attr is LoxClassAttribute then
      begin
        hasLoxClass := True;
        Break;
      end;

    // Build result dict
    dict := newDictionary(VM.MemTracker);
    dictVal := CreateObject(pObj(dict));
    pushStack(VM.Stack, dictVal);

    // --- "class" key ---
    classNameStr := CreateString(nativeObj^.classInfo^.name, VM.MemTracker);
    pushStack(VM.Stack, StringToValue(classNameStr));
    keyStr := CreateString('class', VM.MemTracker);
    pushStack(VM.Stack, StringToValue(keyStr));
    DictSet(dict, StringToValue(keyStr), StringToValue(classNameStr), VM.MemTracker);
    Dec(VM.Stack.StackTop);
    Dec(VM.Stack.StackTop);

    // --- Properties array ---
    propsArr := newArray(VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(propsArr)));
    for prop in rt.GetProperties do
    begin
      if not prop.IsReadable then Continue;
      if hasLoxClass then
      begin
        // Only include properties with [LoxProperty]
        if not HasAttributeLocal(prop.GetAttributes, LoxPropertyAttribute) then Continue;
      end;
      nameStr := CreateString(AnsiString(prop.Name), VM.MemTracker);
      pushStack(VM.Stack, StringToValue(nameStr));
      EnsureArrayCapacity(propsArr, VM.MemTracker);
      propsArr^.Elements[propsArr^.Count] := StringToValue(nameStr);
      Inc(propsArr^.Count);
      Dec(VM.Stack.StackTop);
    end;
    keyStr := CreateString('properties', VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(keyStr)));
    DictSet(dict, CreateObject(pObj(keyStr)), CreateObject(pObj(propsArr)), VM.MemTracker);
    Dec(VM.Stack.StackTop);
    Dec(VM.Stack.StackTop); // pop propsArr guard

    // --- Methods array ---
    methodsArr := newArray(VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(methodsArr)));
    seen := TStringList.Create;
    try
      seen.CaseSensitive := False;
      seen.Sorted := True;
      seen.Duplicates := dupIgnore;
      for method in rt.GetMethods do
      begin
        mName := method.Name;
        if hasLoxClass then
        begin
          // Only include methods with [LoxMethod]
          if not HasAttributeLocal(method.GetAttributes, LoxMethodAttribute) then Continue;
        end
        else
        begin
          // No [LoxClass] ? use denylist (legacy behavior)
          if SameText(mName, 'Free') or SameText(mName, 'Destroy') or
             SameText(mName, 'DisposeOf') or SameText(mName, 'FreeInstance') or
             SameText(mName, 'CleanupInstance') or SameText(mName, 'AfterConstruction') or
             SameText(mName, 'BeforeDestruction') or SameText(mName, 'Create') or
             SameText(mName, 'GetHashCode') or SameText(mName, 'Equals') or
             SameText(mName, 'ToString') or SameText(mName, 'ClassName') or
             SameText(mName, 'ClassType') or SameText(mName, 'ClassParent') or
             SameText(mName, 'ClassInfo') or SameText(mName, 'InstanceSize') or
             SameText(mName, 'InheritsFrom') or SameText(mName, 'DefaultHandler') or
             SameText(mName, 'SafeCallException') or SameText(mName, 'NewInstance') or
             SameText(mName, 'UnitName') or SameText(mName, 'QualifiedClassName') then
            Continue;
        end;
        if seen.IndexOf(mName) >= 0 then Continue;
        seen.Add(mName);

        nameStr := CreateString(AnsiString(mName), VM.MemTracker);
        pushStack(VM.Stack, StringToValue(nameStr));
        EnsureArrayCapacity(methodsArr, VM.MemTracker);
        methodsArr^.Elements[methodsArr^.Count] := StringToValue(nameStr);
        Inc(methodsArr^.Count);
        Dec(VM.Stack.StackTop);
      end;
    finally
      seen.Free;
    end;
    keyStr := CreateString('methods', VM.MemTracker);
    pushStack(VM.Stack, CreateObject(pObj(keyStr)));
    DictSet(dict, CreateObject(pObj(keyStr)), CreateObject(pObj(methodsArr)), VM.MemTracker);
    Dec(VM.Stack.StackTop);
    Dec(VM.Stack.StackTop); // pop methodsArr guard

    Dec(VM.Stack.StackTop); // pop dict guard
    Result := dictVal;
  finally
    ctx.Free;
  end;
end;

function vmHashNative(argCount: integer; args: pValue): TValue;
// Exposes the dictionary's HashValue() function to Lox so distribution
// regression tests can pin hash quality directly (independent of dict probe
// behaviour). Returns the 32-bit hash as a Lox number.
begin
  if argCount <> 1 then
  begin
    RuntimeError('vmHash(value) takes 1 argument.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(HashValue(args[0]));
end;

procedure RegisterIntrospectionNatives;
var
  slMethods : array[0..3] of TNativeMethod;
begin
  // StringList native class
  slMethods[0].name := 'add';    slMethods[0].fn := SL_Add;
  slMethods[1].name := 'get';    slMethods[1].fn := SL_Get;
  slMethods[2].name := 'count';  slMethods[2].fn := SL_Count;
  slMethods[3].name := 'remove'; slMethods[3].fn := SL_Remove;
  registerNativeClass('StringList', slMethods, SL_Destroy);
  findNativeClass('StringList')^.rttiEnabled := True;
  findNativeClass('StringList')^.rttiClass := TStringList;
  defineNative('StringList', stringListNative, 0);

  // Class discovery
  defineNative('loxClasses', loxClassesNative, 0);
  defineNative('loxClassInfo', loxClassInfoNative, 1);

  // Object introspection
  defineNative('loxObjects', loxObjectsNative, 0);
  defineNative('loxObjectInfo', loxObjectInfoNative, 1);

  // VM hash exposure (regression-test hook for hash distribution)
  defineNative('vmHash', vmHashNative, 1);
end;

initialization
  RegisterNativeModule(RegisterIntrospectionNatives);

end.
