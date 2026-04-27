program NativeObjectTest;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Generics.Collections;

// ============================================================================
// Core types — what would live in Chunk_Types.pas
// ============================================================================

type
  // Simulated TValue (simplified — real interpreter has tagged union)
  TValueKind = (vkNil, vkNumber, vkString, vkNativeObject);

  PNativeClassInfo = ^TNativeClassInfo;

  TNativeObject = record
    Instance   : Pointer;         // the Delphi object
    ClassInfo  : PNativeClassInfo; // method table + destructor
  end;
  PNativeObject = ^TNativeObject;

  TValue = record
    case Kind: TValueKind of
      vkNil:          ();
      vkNumber:       (NumberVal: Double);
      vkString:       (StringVal: PChar);          // simplified
      vkNativeObject: (NativeObj: PNativeObject);
  end;

  PValue = ^TValue;

  // Method signature: instance pointer + args → return value
  TNativeMethodFn = function(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;

  TNativeMethod = record
    Name : string;
    Fn   : TNativeMethodFn;
  end;

  TNativeDestructor = procedure(Instance: Pointer);

  TNativeClassInfo = record
    Name       : string;
    Methods    : array of TNativeMethod;
    Destructor_: TNativeDestructor;
  end;

// ============================================================================
// Value constructors
// ============================================================================

function NilVal: TValue;
begin
  Result.Kind := vkNil;
end;

function NumberVal(n: Double): TValue;
begin
  Result.Kind := vkNumber;
  Result.NumberVal := n;
end;

function StringVal(s: PChar): TValue;
begin
  Result.Kind := vkString;
  Result.StringVal := s;
end;

function NativeObjVal(obj: PNativeObject): TValue;
begin
  Result.Kind := vkNativeObject;
  Result.NativeObj := obj;
end;

function ValueToStr(v: TValue): string;
begin
  case v.Kind of
    vkNil:          Result := 'nil';
    vkNumber:       Result := FloatToStr(v.NumberVal);
    vkString:       Result := string(v.StringVal);
    vkNativeObject: Result := Format('<native %s>', [v.NativeObj^.ClassInfo^.Name]);
  end;
end;

// ============================================================================
// Registry — stores class info so it persists
// ============================================================================

var
  ClassRegistry: TList<PNativeClassInfo>;

procedure InitRegistry;
begin
  ClassRegistry := TList<PNativeClassInfo>.Create;
end;

procedure FreeRegistry;
var
  i: Integer;
begin
  for i := 0 to ClassRegistry.Count - 1 do
    Dispose(ClassRegistry[i]);
  ClassRegistry.Free;
end;

function RegisterNativeClass(const AName: string;
  const AMethods: array of TNativeMethod;
  ADestructor: TNativeDestructor): PNativeClassInfo;
var
  info: PNativeClassInfo;
  i: Integer;
begin
  New(info);
  info^.Name := AName;
  SetLength(info^.Methods, Length(AMethods));
  for i := 0 to High(AMethods) do
    info^.Methods[i] := AMethods[i];
  info^.Destructor_ := ADestructor;
  ClassRegistry.Add(info);
  Result := info;
end;

// ============================================================================
// Instance creation + method dispatch
// ============================================================================

function CreateNativeObject(Instance: Pointer; ClassInfo: PNativeClassInfo): PNativeObject;
begin
  New(Result);
  Result^.Instance := Instance;
  Result^.ClassInfo := ClassInfo;
end;

procedure FreeNativeObject(obj: PNativeObject);
begin
  if Assigned(obj^.ClassInfo^.Destructor_) then
    obj^.ClassInfo^.Destructor_(obj^.Instance);
  Dispose(obj);
end;

function FindMethod(ClassInfo: PNativeClassInfo; const MethodName: string;
  out Found: TNativeMethodFn): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(ClassInfo^.Methods) do
    if ClassInfo^.Methods[i].Name = MethodName then
    begin
      Found := ClassInfo^.Methods[i].Fn;
      Exit(True);
    end;
  Found := nil;
  Result := False;
end;

function CallMethod(obj: PNativeObject; const MethodName: string;
  ArgCount: Integer; Args: PValue): TValue;
var
  method: TNativeMethodFn;
begin
  if not FindMethod(obj^.ClassInfo, MethodName, method) then
  begin
    WriteLn(Format('ERROR: %s has no method "%s"', [obj^.ClassInfo^.Name, MethodName]));
    Result := NilVal;
    Exit;
  end;
  Result := method(obj^.Instance, ArgCount, Args);
end;

// ============================================================================
// TStringList wrapper
// ============================================================================

function SL_Add(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
begin
  if (ArgCount <> 1) or (Args^.Kind <> vkString) then
  begin
    WriteLn('ERROR: add() expects 1 string argument');
    Exit(NilVal);
  end;
  TStringList(Instance).Add(string(Args^.StringVal));
  Result := NilVal;
end;

function SL_Get(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
var
  idx: Integer;
  s: string;
begin
  if (ArgCount <> 1) or (Args^.Kind <> vkNumber) then
  begin
    WriteLn('ERROR: get() expects 1 number argument');
    Exit(NilVal);
  end;
  idx := Trunc(Args^.NumberVal);
  if (idx < 0) or (idx >= TStringList(Instance).Count) then
  begin
    WriteLn(Format('ERROR: index %d out of bounds (count=%d)', [idx, TStringList(Instance).Count]));
    Exit(NilVal);
  end;
  s := TStringList(Instance)[idx];
  // In real interpreter this would be copyString — here we cheat with a global buffer
  Result := StringVal(PChar(s));
end;

function SL_Count(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
begin
  Result := NumberVal(TStringList(Instance).Count);
end;

function SL_Remove(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
var
  idx: Integer;
begin
  if (ArgCount <> 1) or (Args^.Kind <> vkNumber) then
  begin
    WriteLn('ERROR: remove() expects 1 number argument');
    Exit(NilVal);
  end;
  idx := Trunc(Args^.NumberVal);
  TStringList(Instance).Delete(idx);
  Result := NilVal;
end;

procedure SL_Destroy(Instance: Pointer);
begin
  WriteLn('  [destructor] Freeing TStringList');
  TStringList(Instance).Free;
end;

// ============================================================================
// TList<Integer> wrapper (demonstrates a second class)
// ============================================================================

type
  TIntList = TList<Integer>;

function IL_Add(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
begin
  TIntList(Instance).Add(Trunc(Args^.NumberVal));
  Result := NilVal;
end;

function IL_Get(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
begin
  Result := NumberVal(TIntList(Instance)[Trunc(Args^.NumberVal)]);
end;

function IL_Count(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
begin
  Result := NumberVal(TIntList(Instance).Count);
end;

function IL_Sum(Instance: Pointer; ArgCount: Integer; Args: PValue): TValue;
var
  i: Integer;
  total: Integer;
begin
  total := 0;
  for i := 0 to TIntList(Instance).Count - 1 do
    total := total + TIntList(Instance)[i];
  Result := NumberVal(total);
end;

procedure IL_Destroy(Instance: Pointer);
begin
  WriteLn('  [destructor] Freeing TList<Integer>');
  TIntList(Instance).Free;
end;

// ============================================================================
// Main — simulates what the interpreter would do
// ============================================================================

var
  slClass, ilClass: PNativeClassInfo;
  slObj, ilObj: PNativeObject;
  arg: TValue;
  ret: TValue;
  slMethods: array[0..3] of TNativeMethod;
  ilMethods: array[0..3] of TNativeMethod;
  foo : TStringList;
begin
  ReportMemoryLeaksOnShutdown := True;
  InitRegistry;
  try
    // --- Register classes (done once at interpreter startup) ---

    slMethods[0].Name := 'add';     slMethods[0].Fn := @SL_Add;
    slMethods[1].Name := 'get';     slMethods[1].Fn := @SL_Get;
    slMethods[2].Name := 'count';   slMethods[2].Fn := @SL_Count;
    slMethods[3].Name := 'remove';  slMethods[3].Fn := @SL_Remove;
    slClass := RegisterNativeClass('StringList', slMethods, @SL_Destroy);

    ilMethods[0].Name := 'add';     ilMethods[0].Fn := @IL_Add;
    ilMethods[1].Name := 'get';     ilMethods[1].Fn := @IL_Get;
    ilMethods[2].Name := 'count';   ilMethods[2].Fn := @IL_Count;
    ilMethods[3].Name := 'sum';     ilMethods[3].Fn := @IL_Sum;
    ilClass := RegisterNativeClass('IntList', ilMethods, @IL_Destroy);

    // --- Simulate: var list = StringList(); ---
    WriteLn('=== StringList test ===');
    slObj := CreateNativeObject(TStringList.Create, slClass);

    // list.add("hello")
    arg := StringVal('hello');
    CallMethod(slObj, 'add', 1, @arg);

    // list.add("world")
    arg := StringVal('world');
    CallMethod(slObj, 'add', 1, @arg);

    // list.add("foo")
    arg := StringVal('foo');
    CallMethod(slObj, 'add', 1, @arg);

    // print list.count()
    ret := CallMethod(slObj, 'count', 0, nil);
    WriteLn(Format('  count = %s', [ValueToStr(ret)]));

    // print list.get(0)
    arg := NumberVal(0);
    ret := CallMethod(slObj, 'get', 1, @arg);
    WriteLn(Format('  get(0) = %s', [ValueToStr(ret)]));

    // print list.get(1)
    arg := NumberVal(1);
    ret := CallMethod(slObj, 'get', 1, @arg);
    WriteLn(Format('  get(1) = %s', [ValueToStr(ret)]));

    // list.remove(0)
    arg := NumberVal(0);
    CallMethod(slObj, 'remove', 1, @arg);

    // print list.get(0) — should be "world" now
    arg := NumberVal(0);
    ret := CallMethod(slObj, 'get', 1, @arg);
    WriteLn(Format('  after remove, get(0) = %s', [ValueToStr(ret)]));

    // Error test: unknown method
    CallMethod(slObj, 'sort', 0, nil);

    // --- Simulate: var nums = IntList(); ---
    WriteLn('');
    WriteLn('=== IntList test ===');
    ilObj := CreateNativeObject(TIntList.Create, ilClass);

    arg := NumberVal(10); CallMethod(ilObj, 'add', 1, @arg);
    arg := NumberVal(20); CallMethod(ilObj, 'add', 1, @arg);
    arg := NumberVal(30); CallMethod(ilObj, 'add', 1, @arg);

    ret := CallMethod(ilObj, 'count', 0, nil);
    WriteLn(Format('  count = %s', [ValueToStr(ret)]));

    arg := NumberVal(1);
    ret := CallMethod(ilObj, 'get', 1, @arg);
    WriteLn(Format('  get(1) = %s', [ValueToStr(ret)]));

    ret := CallMethod(ilObj, 'sum', 0, nil);
    WriteLn(Format('  sum = %s', [ValueToStr(ret)]));

    // --- Cleanup (GC's FreeObject would do this) ---
    WriteLn('');
    WriteLn('=== Cleanup ===');
    FreeNativeObject(slObj);
    FreeNativeObject(ilObj);

    WriteLn('');
    WriteLn('All tests passed.');


   foo := TObject.create;

  finally
    FreeRegistry;
  end;
end.
