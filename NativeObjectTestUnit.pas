unit NativeObjectTestUnit;

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  TNotValueKind = (vkNil, vkNumber, vkString, vkNativeObject);

  PNativeClassInfo = ^TNativeClassInfo;

  TNativeObject = record
    Instance   : Pointer;
    ClassInfo  : PNativeClassInfo;
  end;
  PNativeObject = ^TNativeObject;

  TNotValue = record
    case Kind: TNotValueKind of
      vkNil:          ();
      vkNumber:       (NumberVal: Double);
      vkString:       (StringVal: PChar);
      vkNativeObject: (NativeObj: PNativeObject);
  end;
  PNotValue = ^TNotValue;

  TNativeMethodFn = function(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;

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

// Core API
procedure InitRegistry;
procedure FreeRegistry;
function  RegisterNativeClass(const AName: string;
            const AMethods: array of TNativeMethod;
            ADestructor: TNativeDestructor): PNativeClassInfo;
function  CreateNativeObject(Instance: Pointer; ClassInfo: PNativeClassInfo): PNativeObject;
procedure FreeNativeObject(obj: PNativeObject);
function  CallMethod(obj: PNativeObject; const MethodName: string;
            ArgCount: Integer; Args: PNotValue): TNotValue;

// Value constructors
function NilVal: TNotValue;
function NumberVal(n: Double): TNotValue;
function StringVal(s: PChar): TNotValue;
function NativeObjVal(obj: PNativeObject): TNotValue;
function ValueToStr(v: TNotValue): string;

// Test runner — logs output to a TStrings (e.g. Memo.Lines)
procedure RunNativeObjectTest(Log: TStrings);

implementation

var
  ClassRegistry: TList<PNativeClassInfo>;

// ============================================================================
// Value constructors
// ============================================================================

function NilVal: TNotValue;
begin
  Result.Kind := vkNil;
end;

function NumberVal(n: Double): TNotValue;
begin
  Result.Kind := vkNumber;
  Result.NumberVal := n;
end;

function StringVal(s: PChar): TNotValue;
begin
  Result.Kind := vkString;
  Result.StringVal := s;
end;

function NativeObjVal(obj: PNativeObject): TNotValue;
begin
  Result.Kind := vkNativeObject;
  Result.NativeObj := obj;
end;

function ValueToStr(v: TNotValue): string;
begin
  case v.Kind of
    vkNil:          Result := 'nil';
    vkNumber:       Result := FloatToStr(v.NumberVal);
    vkString:       Result := string(v.StringVal);
    vkNativeObject: Result := Format('<native %s>', [v.NativeObj^.ClassInfo^.Name]);
  else
    Result := '?';
  end;
end;

// ============================================================================
// Registry
// ============================================================================

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
  ClassRegistry := nil;
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
  ArgCount: Integer; Args: PNotValue): TNotValue;
var
  method: TNativeMethodFn;
begin
  if not FindMethod(obj^.ClassInfo, MethodName, method) then
  begin
    Result := NilVal;
    Exit;
  end;
  Result := method(obj^.Instance, ArgCount, Args);
end;

// ============================================================================
// TStringList wrapper
// ============================================================================

function SL_Add(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
begin
  TStringList(Instance).Add(string(Args^.StringVal));
  Result := NilVal;
end;

function SL_Get(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
var
  s: string;
begin
  s := TStringList(Instance)[Trunc(Args^.NumberVal)];
  Result := StringVal(PChar(s));
end;

function SL_Count(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
begin
  Result := NumberVal(TStringList(Instance).Count);
end;

function SL_Remove(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
begin
  TStringList(Instance).Delete(Trunc(Args^.NumberVal));
  Result := NilVal;
end;

procedure SL_Destroy(Instance: Pointer);
begin
  TStringList(Instance).Free;
end;

// ============================================================================
// TList<Integer> wrapper
// ============================================================================

type
  TIntList = TList<Integer>;

function IL_Add(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
begin
  TIntList(Instance).Add(Trunc(Args^.NumberVal));
  Result := NilVal;
end;

function IL_Get(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
begin
  Result := NumberVal(TIntList(Instance)[Trunc(Args^.NumberVal)]);
end;

function IL_Count(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
begin
  Result := NumberVal(TIntList(Instance).Count);
end;

function IL_Sum(Instance: Pointer; ArgCount: Integer; Args: PNotValue): TNotValue;
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
  TIntList(Instance).Free;
end;

// ============================================================================
// Test runner
// ============================================================================

procedure RunNativeObjectTest(Log: TStrings);
var
  slClass, ilClass: PNativeClassInfo;
  slObj, ilObj: PNativeObject;
  arg, ret: TNotValue;
  slMethods: array[0..3] of TNativeMethod;
  ilMethods: array[0..3] of TNativeMethod;
begin
  InitRegistry;
  try
    // Register StringList
    slMethods[0].Name := 'add';     slMethods[0].Fn := @SL_Add;
    slMethods[1].Name := 'get';     slMethods[1].Fn := @SL_Get;
    slMethods[2].Name := 'count';   slMethods[2].Fn := @SL_Count;
    slMethods[3].Name := 'remove';  slMethods[3].Fn := @SL_Remove;
    slClass := RegisterNativeClass('StringList', slMethods, @SL_Destroy);

    // Register IntList
    ilMethods[0].Name := 'add';     ilMethods[0].Fn := @IL_Add;
    ilMethods[1].Name := 'get';     ilMethods[1].Fn := @IL_Get;
    ilMethods[2].Name := 'count';   ilMethods[2].Fn := @IL_Count;
    ilMethods[3].Name := 'sum';     ilMethods[3].Fn := @IL_Sum;
    ilClass := RegisterNativeClass('IntList', ilMethods, @IL_Destroy);

    // --- StringList test ---
    Log.Add('=== StringList test ===');
    slObj := CreateNativeObject(TStringList.Create, slClass);

    arg := StringVal('hello');  CallMethod(slObj, 'add', 1, @arg);
    arg := StringVal('world');  CallMethod(slObj, 'add', 1, @arg);
    arg := StringVal('foo');    CallMethod(slObj, 'add', 1, @arg);

    ret := CallMethod(slObj, 'count', 0, nil);
    Log.Add(Format('  count = %s', [ValueToStr(ret)]));

    arg := NumberVal(0);
    ret := CallMethod(slObj, 'get', 1, @arg);
    Log.Add(Format('  get(0) = %s', [ValueToStr(ret)]));

    arg := NumberVal(1);
    ret := CallMethod(slObj, 'get', 1, @arg);
    Log.Add(Format('  get(1) = %s', [ValueToStr(ret)]));

    arg := NumberVal(0);
    CallMethod(slObj, 'remove', 1, @arg);

    arg := NumberVal(0);
    ret := CallMethod(slObj, 'get', 1, @arg);
    Log.Add(Format('  after remove, get(0) = %s', [ValueToStr(ret)]));

    // --- IntList test ---
    Log.Add('');
    Log.Add('=== IntList test ===');
    ilObj := CreateNativeObject(TIntList.Create, ilClass);

    arg := NumberVal(10); CallMethod(ilObj, 'add', 1, @arg);
    arg := NumberVal(20); CallMethod(ilObj, 'add', 1, @arg);
    arg := NumberVal(30); CallMethod(ilObj, 'add', 1, @arg);

    ret := CallMethod(ilObj, 'count', 0, nil);
    Log.Add(Format('  count = %s', [ValueToStr(ret)]));

    arg := NumberVal(1);
    ret := CallMethod(ilObj, 'get', 1, @arg);
    Log.Add(Format('  get(1) = %s', [ValueToStr(ret)]));

    ret := CallMethod(ilObj, 'sum', 0, nil);
    Log.Add(Format('  sum = %s', [ValueToStr(ret)]));

    // --- Cleanup ---
    Log.Add('');
    Log.Add('=== Cleanup ===');
    FreeNativeObject(slObj);
    Log.Add('  StringList freed');
    FreeNativeObject(ilObj);
    Log.Add('  IntList freed');

    Log.Add('');
    Log.Add('All native object tests passed.');
  finally
    FreeRegistry;
  end;
end;

end.
