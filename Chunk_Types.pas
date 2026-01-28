unit Chunk_Types;
{$POINTERMATH ON}
interface

uses
  Classes, dialogs;


const

  START_CAPACITY = 256;
  MAX_SIZE       = 256 * 256;  // 65,536
  GROWTH_FACTOR = 2;

  //scanner
  CHAR_SPACE    = ' ';
  CHAR_TAB      = #9;   // '\t'
  CHAR_LF       = #10;  // '\n'
  CHAR_CR       = #13;  // '\r'
  CHAR_SLASH    = '/';
  CHAR_NUL      = #0;
  CHAR_QUOTE  = '"';
  CHAR_DOT = '.';

  //opcodes within byte range;
  OP_CONSTANT = 0;
  OP_NEGATE   = 1;
  OP_ADD      = 2;
  OP_SUBTRACT = 3;
  OP_MULTIPLY = 4;
  OP_DIVIDE   = 5;
  OP_RETURN   = 6;
  OP_TRUE     = 7;
  OP_FALSE    = 8;
  OP_NIL      = 9;
  OP_NOT      = 10;
  OP_EQUAL    = 11;
  OP_GREATER  = 12;
  OP_LESS     = 13;

  OP_STRINGS : array[0..13] of string = (
    'OP_CONSTANT',
    'OP_NEGATE',
    'OP_ADD',
    'OP_SUBTRACT',
    'OP_MULTIPLY',
    'OP_DIVIDE',
    'OP_RETURN',
    'OP_TRUE',
    'OP_FALSE',
    'OP_NIL',
    'OP_NOT',
    'OP_EQUAL',
    'OP_GREATER',
    'OP_LESS');


type
  //Alias
  TCode = byte;



  //Enums
  TValueKind = (vkNumber, vkBoolean, vkNull, vkObject);
  TObjectKind = (okString);
  TBinaryOperation = (boAdd, boSubtract, boMultiply, boDivide, boGreater, boLess);
  TTokenType = (
    // Single-character tokens
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,

    // One or two character tokens
    TOKEN_BANG, TOKEN_BANG_EQUAL,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_LESS, TOKEN_LESS_EQUAL,

    // Literals
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

    // Keywords
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
    TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
    TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
    TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

    TOKEN_ERROR, TOKEN_EOF
  );

  TPrecedence = (
      PREC_NONE,
      PREC_ASSIGNMENT,  // =
      PREC_OR,          // or
      PREC_AND,         // and
      PREC_EQUALITY,    // == !=
      PREC_COMPARISON,  // < > <= >=
      PREC_TERM,        // + -
      PREC_FACTOR,      // * /
      PREC_UNARY,       // ! -
      PREC_CALL,        // . ()
      PREC_PRIMARY
    );

   TLogLevel = (Debug);


  //pointers
  pChunk          = ^TChunk;
  pValueArray     = ^TValueArray;
  pCode           = ^TCode;
  pValue          = ^TValue;

  pObj            = ^TObj;
  pObjString      = ^TObjString;
  pStackRecord    = ^TStackRecord;
  pVirtualMachine = ^TVirtualMachine;
  pAnsiCharArray = ^TAnsiCharArray;
  pLogs     = ^TLogs; //Note : Don't think we need this (https://www.danieleteti.it/loggerpro/) maybe add later? We use a stupidly simple approach for now



  //Arrays
  TAnsiCharArray = Array[0..0] of AnsiChar;


  //Records
    TLogRecord = record
    msg   : string;
    level : TLogLevel;
  end;

  TLogs = record
    Logs : Array[0..0] of TLogRecord;
  end;


  TObj = record
    ObjectKind : TObjectKind;
    Next       : pObj; //for GC collection
  end;

  TObjString = record
    Obj     : TObj;
    length  : integer;
    chars   : TAnsiCharArray;
  end;

  TValue = record
    ValueKind: TValueKind;
    case TValueKind of
      vkNumber:  (NumberValue: Double);
      vkBoolean: (BooleanValue: Boolean);
      vkNull:    (NullValue: Byte);
      vkObject:  (ObjValue : pObj);  //note here that this pointer is to TObjectKind.However, since all objects are derived from it, we can cast the pointer to the actual object (and back).
  end;

  TChunk = record
    Count       : Integer;
    Capacity    : Integer;
    Code        : pCode;
    Constants   : pValueArray;
    Lines       : pInteger;
    Initialised : boolean;
  end;

  TValueArray = record
    Count     : Integer;
    Capacity  : Integer;
    Values    : pValue;
  end;

  TStackRecord = record
    Count     : Integer;
    Capacity  : Integer;
    Values    : pValue;
    StackTop  : pValue;
  end;

  //Virtual Machine result
  TInterpretResult = record
    code  : (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);
    value : TValue;
  end;


  //Virtual Machine
  TVirtualMachine = record
    Chunk           : pChunk;
    ip              : pCode;
    Stack           : pStackRecord;
    CreatedObjects  : pObj;
    ownObjects      : boolean;
    BytesAllocated  : integer;
  end;

  TScanner = record
   start    : pAnsiChar;
   current  : pAnsiChar;
   line     : integer;
  end;

   TToken = record
      tokenType : TTokenType;
      start : pAnsiChar;
      length : integer;
      line : integer;
   end;

   TParser = record
      Current   : TToken;
      Previous  : TToken;
      HadError  : boolean;
      PanicMode : boolean;
      ErrorStr  : String;
   end;


  //Prat parsing structs
  TParseFn = procedure;

  TParseRule = record
    prefix : TParseFn;
    infix  : TParseFn;
    precedence : TPrecedence;
  end;


//Memory creation routines
procedure Allocate(var p : pointer; oldsize,newSize : integer);
function AllocateArray(var list: Pointer; var CurrentCapacity: Integer; Count: Integer; ElemSize: Integer) : boolean;

//object creation routines
procedure AddToCreatedObjects(p : pObj);
function CreateString(const S: AnsiString): PObjString;

//Log routines
(*procedure InitLogs(var Logs : pLogs);
procedure FreeLogs(var logs : pLogs);
procedure WriteLog(const logs : pLogs; LogRecord : TLogRecord); *)

//Log routines
(*procedure InitLogs(var Logs : pLogs);
procedure FreeLogs(var logs : pLogs);
procedure WriteLog(const logs : pLogs; LogRecord : TLogRecord); *)

//string routines
function GetChar(const str : pObjString; index : integer) : AnsiChar;
procedure FreeString(var obj : pObjString);
function StringsEqual(a, b: PObjString): Boolean;
function ValuesEqual(a, b : TValue) : boolean;
function TokenToString(const Token: TToken): AnsiString;
function ObjStringToAnsiString(S: PObjString): AnsiString;
function ValueToString(const value : TValue) : pObjString;
function StringToValue(const value : pObjString) : TValue;


//Chunk routines
procedure initChunk(var chunk: pChunk);
procedure freeChunk(var chunk: pChunk);
procedure writeChunk(chunk: pChunk; value: byte; Line : Integer);
procedure AddConstant(chunk : pChunk; const value : TValue; Line : Integer);
procedure initValueArray(var ValueArray : pValueArray);
procedure writeValueArray(ValueArray : pValueArray; Value : TValue);
procedure FreeValues(var Values : pValue; Capacity : integer);
procedure freeValueArray(var ValueArray : pValueArray);
procedure printValueArray(ValueArray: pValueArray; strings: TStrings);


//Virtual Machine
procedure InitVM();
function InterpretResult(source : pAnsiChar) : TInterpretResult;
procedure FreeObjects();
procedure CollectGarbage();
procedure FreeVM();

//Stack
procedure InitStack(var Stack : pStackRecord);
procedure FreeStack(var Stack : pStackRecord);
procedure ResetStack(var stack : pStackRecord);
procedure pushStack(var stack : pStackRecord;const value : TValue);
function peekStack(stack : pStackRecord) : TValue; overload;
function peekStack(stack : pStackRecord; distanceFromTop : integer) : TValue;overload;
function  popStack(var stack : pStackRecord) : TValue;

//Scanner
procedure InitScanner(source : pAnsiChar);
function  advance : ansichar;
function  isAtEnd : boolean;

//compilation
procedure BinaryOp(Op: TBinaryOperation);
function compile(source : pAnsiChar; chunk : pChunk) : boolean;
procedure Number();
procedure grouping();
procedure unary();
procedure binary();
procedure literal();
procedure ParseString();

//prat parsing rule table used in compilation
const
  Rules: array[TTokenType] of TParseRule = (
    { TOKEN_LEFT_PAREN }
    (Prefix: grouping; Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_RIGHT_PAREN }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_LEFT_BRACE }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_RIGHT_BRACE }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_COMMA }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_DOT }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_MINUS }
    (Prefix: unary;    Infix: binary;  Precedence: PREC_TERM),

    { TOKEN_PLUS }
    (Prefix: nil;      Infix: binary;  Precedence: PREC_TERM),

    { TOKEN_SEMICOLON }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_SLASH }
    (Prefix: nil;      Infix: binary;  Precedence: PREC_FACTOR),

    { TOKEN_STAR }
    (Prefix: nil;      Infix: binary;  Precedence: PREC_FACTOR),

    { TOKEN_BANG }
    (Prefix: unary;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_BANG_EQUAL }
    (Prefix: nil;      Infix: binary;     Precedence: PREC_EQUALITY),

    { TOKEN_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_EQUAL_EQUAL }
    (Prefix: nil;      Infix: binary;     Precedence: PREC_EQUALITY),

    { TOKEN_GREATER }
    (Prefix: nil;      Infix: binary;     Precedence: PREC_COMPARISON),

    { TOKEN_GREATER_EQUAL }
    (Prefix: nil;      Infix: binary;     Precedence: PREC_COMPARISON),

    { TOKEN_LESS }
    (Prefix: nil;      Infix: binary;     Precedence: PREC_COMPARISON),

    { TOKEN_LESS_EQUAL }
    (Prefix: nil;      Infix: binary;     Precedence: PREC_COMPARISON),

    { TOKEN_IDENTIFIER }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_STRING }
    (Prefix: ParseString; Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_NUMBER }
    (Prefix: number;   Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_AND }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_CLASS }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_ELSE }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_FALSE }
    (Prefix: literal;  Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_FOR }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_FUN }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_IF }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_NIL }
    (Prefix: literal;  Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_OR }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_PRINT }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_RETURN }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_SUPER }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_THIS }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_TRUE }
    (Prefix: literal;  Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_VAR }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_WHILE }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_ERROR }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_EOF }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE)
  );


//global variables
var
  VM : pVirtualMachine;
  Scanner : TScanner;
  Parser  : TParser;
  CompilingChunk : pChunk;
  //Output : TStrings;

implementation

uses
  sysutils, Math,strUtils, typinfo;


//memory creation routines

function IsInCreatedObjects(obj: PObj): Boolean;
var
  current: PObj;
begin
  current := vm.CreatedObjects; // head of VM object list
  while Assigned(current) do
  begin
    if current = obj then
      Exit(True);
    current := current^.Next;
  end;
  Result := False;
end;


procedure Allocate(var p: Pointer; OldSize, NewSize: Integer);
begin
  // --------------------------------------------------------------------------
  // Basic sanity checks on sizes
  // --------------------------------------------------------------------------
  Assert(NewSize >= 0, 'New size underflow');   // New allocation cannot be negative
  Assert(OldSize >= 0, 'Old size underflow');  // Old allocation cannot be negative
  Assert(OldSize <> NewSize, 'Old size = new Size - invalid allocation');


  // --------------------------------------------------------------------------
  // Pointer consistency invariant
  // --------------------------------------------------------------------------
  // Enforce that OldSize = 0 means pointer must be nil
  // and OldSize > 0 means pointer must be valid
  if OldSize = 0 then Assert(p = nil, 'OldSize = 0 but pointer is not nil');
  if OldSize > 0 then Assert(p <> nil, 'OldSize > 0 but pointer is nil');



  // --------------------------------------------------------------------------
  // Reallocate memory
  // --------------------------------------------------------------------------
  // ReallocMem preserves the first OldSize bytes (if >0) and may raise an exception
  // if allocation fails. Memory beyond OldSize is uninitialized.
  ReallocMem(p, NewSize);

  // --------------------------------------------------------------------------
  // Zero out any newly allocated memory
  // --------------------------------------------------------------------------
  // Ensures deterministic state for GC, pointers, or arrays
  // Only affects the "new" portion; old memory remains unchanged
  if NewSize > OldSize then
    FillChar(PByte(p)[OldSize], NewSize - OldSize, #0);

  // --------------------------------------------------------------------------
  // Update VM memory accounting
  // --------------------------------------------------------------------------
  // Tracks total allocated bytes for GC / memory management


  Inc(vm.BytesAllocated, NewSize - OldSize);

  // Ensure bytes allocated never underflows (extra safety)
  Assert(vm.BytesAllocated >= 0, 'VM bytes underflow');


  // --------------------------------------------------------------------------
  // Zero-size check
  // --------------------------------------------------------------------------
  // If allocation shrinks to zero, the pointer must now be nil
  if NewSize = 0 then
    Assert(p = nil, 'Pointer not nil after zero-size allocation');

  // --------------------------------------------------------------------------
  // Non-Zero-size check
  // --------------------------------------------------------------------------
  // If allocation shrinks to zero, the pointer must now be nil
  if NewSize > 0 then
    Assert(p <> nil, 'Pointer nil after new size > 0 allocation');

end;


function AllocateArray(
  var List: Pointer;
  var CurrentCapacity: Integer;
  Count, ElemSize: Integer
): Boolean;
var
  NewCapacity: Integer;
  OldSize, NewSize: Integer;
begin
  // ---- Global invariants ---------------------------------------------------

  Assert(START_CAPACITY > 0,
    'START_CAPACITY must be greater than zero');
  Assert(GROWTH_FACTOR > 1,
    'GROWTH_FACTOR must be greater than 1');
  Assert(ElemSize > 0,
    'ElemSize must be greater than zero');
  Assert(CurrentCapacity >= 0,
    'CurrentCapacity underflow');
  Assert(Count >= 0,
    'Count underflow');
  Assert(Count <= CurrentCapacity,
    'Count exceeds CurrentCapacity');

  // ---- Initial allocation --------------------------------------------------

  if CurrentCapacity = 0 then
  begin
    Assert(List = nil,
      'List must be nil when CurrentCapacity is zero');

    // Ensure capacity * element size will not overflow
    Assert(START_CAPACITY <= MaxInt div ElemSize,
      'Initial allocation size exceeds addressable memory');

    CurrentCapacity := START_CAPACITY;
    NewSize := CurrentCapacity * ElemSize;

    Allocate(List, 0, NewSize);
    Exit(True);
  end;

  // ---- Growth path ---------------------------------------------------------

  Assert(Assigned(List),
    'List is nil with non-zero CurrentCapacity');

  if Count < CurrentCapacity then
    Exit(False);

  // Ensure capacity growth itself cannot overflow Integer
  Assert(CurrentCapacity <= MaxInt div GROWTH_FACTOR,
    'Array capacity multiplication overflows Integer');

  // Ensure logical size limit is not exceeded
  Assert(CurrentCapacity <= MAX_SIZE div GROWTH_FACTOR,
    'Array capacity growth would exceed MAX_SIZE');

  NewCapacity := CurrentCapacity * GROWTH_FACTOR;

  // Ensure byte-size multiplications are safe
  Assert(CurrentCapacity <= MaxInt div ElemSize,
    'Current array byte size exceeds Integer range');
  Assert(NewCapacity <= MaxInt div ElemSize,
    'Grown array byte size exceeds Integer range');

  OldSize := CurrentCapacity * ElemSize;
  NewSize := NewCapacity * ElemSize;

  Allocate(List, OldSize, NewSize);
  CurrentCapacity := NewCapacity;

  Result := True;
end;





//note : we do not add a null terminator #0 since we track the length
function CreateString(const S: AnsiString): PObjString;
var
  Len: Integer;
  Size: NativeInt;
begin
  Result := nil;
  Len := Length(S);
  Size := SizeOf(TObjString) + Max(0, Len - 1);  // avoid negative size

  Allocate(Pointer(Result),0, Size);

  Result^.Obj.ObjectKind := okString;
  Result^.Obj.Next := nil;
  Result^.Length := Len;

  if Len > 0 then
  begin
    Move(PAnsiChar(S)^, Result^.Chars[0], Len);
  end;

  //track creation in the vm list of obj.
  AddToCreatedObjects(PObj(Result));

end;

procedure FreeString(var obj : pObjString);
var
 objSize : integer;
begin
  // ---- preconditions ----
  assert(assigned(obj), 'string to free is nil');
  assert(obj^.Length >= 0, 'string length is negative');
  assert(obj^.Obj.ObjectKind = okString, 'Type mismatch, expected a string object but object kind is not a string');

  // ---- resize now ----
  objSize := Sizeof(TObjString) + Max(0, obj^.length - 1);  // mimc here size from CreateString (We don't have the string but we do have the length now)
  Allocate(pointer(obj), objSize , 0);
  obj := nil;

  // ---- postconditions ----
  assert(obj = nil, 'string pointer not cleared after free');
  assert(vm.BytesAllocated >= 0, 'VM bytes allocated underflow after freeing string.');
end;


procedure initChunk(var chunk: pChunk);
begin
  assert(Chunk = nil,'Chunk initialization failuyre. Chunk is not nil');
  Allocate(pointer(chunk),0, Sizeof(TChunk));

  chunk.Count := 0;
  chunk.Capacity := 0;
  chunk.Code := nil;
  chunk.Lines := nil;
  chunk.Constants := nil;
  InitValueArray(chunk.Constants);
  chunk.Initialised := true;
end;

procedure freeChunk(var chunk: pChunk);
begin
  assert(assigned(Chunk),'Chunk is not assigned');
  assert(chunk.Initialised = true, 'Chunk is not initialised');

  if (chunk.Capacity) > 0 then
  begin
    Allocate(pointer(chunk.Code), Chunk.Capacity * sizeof(TCode), 0);

    Assert(Chunk.Code = nil, 'Expected Chunk Code to be nil');

    Allocate(pointer(Chunk.Lines), Chunk.Capacity * Sizeof(Integer),0);

    Assert(Chunk.Lines = nil, 'Expected Chunk Lines to be nil');
  end;


  freeValueArray(chunk.Constants);

  Assert(Chunk.Constants = nil, 'Expected chunk Constants to be nil');

  Allocate(pointer(chunk),Sizeof(TChunk),0);

  Assert(Chunk = nil, 'Expected chunk to be nil');

end;

procedure writeChunk(chunk: pChunk; value: byte; Line : Integer);
var
  currentCap : integer;
begin
  assert(assigned(chunk),'Chunk is not assigned');
  assert(chunk.Initialised = true, 'Chunk is not initialised');

  currentCap := Chunk.Capacity;
  if AllocateArray(Pointer(chunk.Code),  Chunk.Capacity, Chunk.Count, sizeof(TCode)) then
  begin
    //we have to do it like this because we can't pass Chunk.Capacity to grow the line array (since it will be altered)
    AllocateArray(Pointer(chunk.Lines), currentCap, Chunk.Count, sizeof(Integer));
  end;

  chunk.Code[chunk.Count] := value;
  chunk.Lines[chunk.count] := Line;
  Inc(chunk.Count);
end;


procedure initValueArray(var ValueArray : pValueArray);
begin
  assert(ValueArray = nil,'values is not nil');

  allocate(pointer(ValueArray),0,Sizeof(TValueArray));

  ValueArray.Count := 0;

  ValueArray.Capacity := 0;

  ValueArray.Values := nil;
end;

procedure writeValueArray(ValueArray : pValueArray; Value : TValue);
begin
  assert(assigned(ValueArray),'ValueArray is not assigned');

  AllocateArray(pointer(ValueArray.Values), ValueArray.Capacity, ValueArray.Count,sizeof(TValue));

  ValueArray.Values[ValueArray.Count] := value;

  Inc(ValueArray.Count);
end;

procedure FreeValues(var Values : pValue; Capacity : integer);
begin
  assert(assigned(Values),'Values is not assigned');
  assert(Capacity > 0, 'Capacity is < 0');
  Allocate(pointer(Values), Capacity * Sizeof(TValue),0);  //Note here that the references to objects will be free'd externally
  Assert(Values = nil, 'values is not nil');
end;

procedure freeValueArray(var ValueArray : pValueArray);
begin
  assert(assigned(ValueArray),'ValueArray is not assigned');
  if ValueArray.Values <> nil then
    FreeValues(ValueArray.Values,ValueArray.Capacity);
  Allocate(pointer(ValueArray),Sizeof(TValueArray),0);
  ValueArray := nil;
end;

procedure InitStack(var Stack : pStackRecord);
begin
  Assert(Stack = nil, 'Stack initialization failure - stack record is not nil');
  Allocate(pointer(Stack),0, Sizeof(TStackRecord));
  Stack.Count := 0;
  Stack.Capacity := 0;
  Stack.Values := nil;
  AllocateArray(pointer(Stack.Values),Stack.Capacity,Stack.Count,Sizeof(TValue));
  Stack.StackTop := Stack.Values;
end;

procedure FreeStack(var Stack : pStackRecord);
begin
  assert(Assigned(Stack),'Stack is not assigned - free stack');
  assert(Assigned(Stack.Values), 'Stack values is not assigned - free stack');
  if (stack.Capacity > 0) then
  begin
    Allocate(pointer(Stack.Values), stack.Capacity * sizeof(TValue), 0);
    Stack.Values := nil;
  end;

  Allocate(pointer(Stack),sizeof(TStackRecord),0);
  Stack := nil;
end;

procedure pushStack(var stack : pStackRecord;const value : TValue);
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
  Assert(Assigned(Stack.values), 'Stack values is not assigned');
  if AllocateArray(pointer(Stack.Values),Stack.Capacity,Stack.Count,Sizeof(TValue)) then
  begin
    ResetStack(stack);
    Stack.StackTop := Stack.Values + Stack.Count;  //move stack top to next pointer available (at count)
  end;
  Stack.StackTop^ := Value;
  Inc(Stack.StackTop);
  inc(Stack.Count);
end;


//End memory creation routines






function isObject(value : TValue) : boolean;
begin
  result := value.valueKind = vkObject;
end;

function GetObject(const value : TValue) : pObj; inline;
begin
  assert(isObject(Value), 'value is not an object');
  result := value.ObjValue;
end;

function CreateObject(value : pObj) : TValue;
begin
  assert(assigned(value), 'object value is nil');
  result.ValueKind := vkObject;
  result.ObjValue := value;
end;


function isString(value : TValue) : boolean;
begin
  result := isObject(Value) and (value.ObjValue.ObjectKind = okString);
end;

function ObjStringToAnsiString(S: PObjString): AnsiString;
begin
  SetString(Result, PAnsiChar(@S^.chars[0]), S^.length);
end;

function GetChar(const str : pObjString; index : integer) : AnsiChar;
var
  ptr : pAnsichar;
begin
  assert(assigned(str),'str is not assigned');
  assert(index >= 0, 'Index underflow');
  assert(index < str.length, 'Index overflow');

  ptr := str.chars;
  inc(ptr,index);
  result := ptr^;
end;



procedure AddToCreatedObjects(p : pObj);
begin
  p^.Next := vm.CreatedObjects;
  vm.CreatedObjects := p;
end;

//Chars: array[0..0] of AnsiChar;
function AddString(const a, b : AnsiString) : PObjString;
begin
  result := CreateString(a+b);
end;

function ObjStringSize(const p : pObjString) : integer;
begin
  result := Sizeof(TObjString) + Max(0, p^.length - 1);  // avoid negative size
end;



function ValueToString(const value : TValue) : pObjString;
begin
  assert(isString(value), 'value is not a string value');
  assert(Assigned(value.ObjValue), 'string pointer is not assigned');
  result := pObjString(value.ObjValue);
end;

function StringToValue(const value : pObjString) : TValue;
begin
  assert(Assigned(value), 'string to value is not assigned');
  result.ValueKind := vkObject;
  result.ObjValue := pObj(value);
end;

procedure Concatenate();
var
  top, below, resultStr: PObjString;
  strTop, strBelow: AnsiString;
begin
  Assert(IsString(peekStack(vm.stack)),'Value at top of stack to concatenate is not a string');
  Assert(IsString(peekStack(vm.stack,1)),'Value at position -1 of stack to concatenate is not a string');

  top := ValueToString(popStack(vm.stack));       // top of stack ("B")
  below := ValueToString(popStack(vm.stack));     // below top ("A")

  strTop := ObjStringToAnsiString(top);
  strBelow := ObjStringToAnsiString(below);

  resultStr := AddString(strBelow, strTop);   // "A" + "B"

  PushStack(vm.stack, StringToValue(resultStr));
end;


function CreateBoolean(Value: Boolean): TValue; inline;
begin
  Result.ValueKind := vkBoolean;
  Result.BooleanValue := Value;
end;

function isBoolean(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkBoolean;
end;

function GetBoolean(const Value : TValue) : Boolean; inline;
begin
  assert(isBoolean(Value),'Value is not boolean');
  result := Value.BooleanValue;
end;

function isNumber(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkNumber;
end;

function CreateNumber(Value: Double): TValue; inline;
begin
  Result.ValueKind := vkNumber;
  Result.NumberValue := Value;
end;

function GetNumber(Value : TValue) : double;
begin
  assert(isNumber(Value), 'Value is not a number');
  result := value.NumberValue;
end;

function CreateNilValue : TValue; inline;
begin
  Result.ValueKind := vkNull;
  Result.NullValue := 0;
end;

function isNill(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkNull;
end;

function GetNil(const value : TValue) : byte;
begin
  assert(isNill(value), 'value is not a nil value');
  result := Value.NullValue;
end;

function IsFalsey(const Value: TValue): Boolean; inline;
begin
  Result :=
    (Value.ValueKind = vkNull) or
    ((Value.ValueKind = vkBoolean) and (not Value.BooleanValue));
end;


function StringsEqual(a, b: PObjString): Boolean;
begin
  Assert(Assigned(a), 'Value A is not assigned for string compare');
  Assert(Assigned(b), 'Value B is not assigned for string compare');
  if a.length <> b.length then
    exit(false);

  Result :=
    (a^.Length = b^.Length) and
    CompareMem(@a^.Chars, @b^.Chars, a^.Length);
end;

function ValuesEqual(a, b : TValue) : boolean;
begin
  if a.ValueKind <> b.ValueKind then exit(False);

  case a.ValueKind of
    vkBoolean : result := GetBoolean(a) = GetBoolean(b);
    vkNull    : result := true;
    vkNumber  : result := GetNumber(a) = GetNumber(b);
    vkObject  : begin
                  assert(assigned(a.ObjValue),'A value is not assigned in value Equals');
                  assert(assigned(b.ObjValue),'B value is not assigned in value Equals');
                  case a.ObjValue.ObjectKind of
                    okString : begin
                       result := StringsEqual(ValueToString(a),ValueToString(b));
                    end
                    else
                    begin
                      result := false;
                    end;
                  end
    end
    else
      result := false;
  end
end;





function AddValueConstant(ValueArray: pValueArray; const value: TValue): Integer;
begin
  Assert(Assigned(ValueArray), 'ValueArray is not assigned');

  // Add the value to the ValueArray
  writeValueArray(ValueArray, value);

  // Return the index of the newly added value
  Result := ValueArray.Count - 1;
end;

//we now write an integer into the byte array
procedure WriteConstantIndex(chunk : pChunk; index : integer);
begin
  Assert(Assigned(Chunk.Code), 'Chunk code is not assigned');
  Assert(Index >= 0, ' index is negative');
  Assert(Index <= Chunk.Constants.Capacity, 'index is > constants array');
  Assert(Chunk.Count + SizeOf(Integer) <= Chunk.Capacity,' writing chunk index will exceed chunk capacity');

  PInteger(Chunk.Code + Chunk.Count)^ := index;
  Inc(Chunk.Count, SizeOf(Integer));
end;


procedure AddConstant(chunk : pChunk; const value : TValue; Line : Integer);
var
  idx : integer;
begin
  Assert(Assigned(chunk), 'Chunk is not assigned');
  Assert(Assigned(chunk.Constants), 'ValueArray is not assigned');

  //add constant, 1st into value's array of the value record
  idx := AddValueConstant(chunk.Constants,value);
  //add constant op code into the chunk array
  writeChunk(Chunk, OP_CONSTANT,Line);
  //followed by the index of the value inserted into the value array
  WriteConstantIndex(Chunk,idx);
end;





procedure printValueArray(ValueArray: pValueArray; strings: TStrings);
const
  VR_Header = 'ValueArray starts at address $%p';
  VALUE_FIELD_WIDTH = 50; // fixed width for the value column
var
  i: Integer;
  valStr: string;
  valuePtr: pValue;
begin
  Assert(Assigned(ValueArray), 'ValueArray is not assigned');
  Assert(Assigned(strings), 'Output strings is not assigned');

  strings.Clear;
  strings.Add(Format(VR_Header, [Pointer(ValueArray)]));

  ValuePtr := ValueArray.Values;



    for i := 0 to ValueArray.Count - 1 do
    begin
      inc(ValuePtr,i);

      // format value as general format with up to 12 significant digits
      valStr := Format('%.12g', [ValuePtr^.NumberValue]);

      // pad to fixed width
      valStr := valStr + StringOfChar(' ', VALUE_FIELD_WIDTH - Length(valStr));

      // Index + fixed-width value
      strings.Add(Format('%.6d  %s', [i, valStr]));
    end;

end;




procedure RunTimeError(const msg : string);
begin
  //TODO
end;

function Run : TInterpretResult;

  function ReadByte: Byte; inline;
  begin
     result := vm.ip^;
     inc(vm.Ip);
  end;

  function ReadConstant : TValue; inline;
  var idx : integer;
  begin
    idx := PInteger(vm.IP)^;
    Inc(vm.IP, SizeOf(Integer));
    result := vm.Chunk.Constants.Values[idx];
  end;


var
  instruction: Byte;
  value,ValueB : TValue;

begin

    Assert(Assigned(VM),'VM is not assigned');
    Assert(Assigned(VM.Chunk),'VM Chunk is not assigned');
    Assert(Assigned(VM.Chunk.Code),'VM chunk code is not assigned');

    while True do
    begin
      instruction := ReadByte();
      case instruction of

        OP_CONSTANT : begin
          value := ReadConstant;
          pushStack(vm.Stack,value);
        end;

        OP_NEGATE : begin

          if not isNumber(peekStack(vm.stack)) then
          begin
             result.code := INTERPRET_RUNTIME_ERROR;
             runtimeError('Operand must be a number.');
             exit;
          end;

          value := popStack(vm.Stack);
          value.NumberValue := -Value.NumberValue;

          PushStack(vm.Stack,value);

        end;

        OP_NIL      : pushStack(vm.stack, CreateNilValue);
        OP_TRUE     : pushStack(vm.Stack, CreateBoolean(true));
        OP_FALSE    : pushStack(vm.stack, CreateBoolean(false));

        OP_EQUAL: begin
          Value := popStack(vm.Stack);
          ValueB := popStack(vm.Stack);
          pushStack(vm.stack,CreateBoolean(valuesEqual(Value, ValueB)));
        end;

        OP_GREATER  : binaryOp(boGreater);

        OP_LESS     : binaryOp(boLess);

        OP_ADD      : begin
                        if isNumber(peekStack(vm.stack,0)) and isNumber(peekStack(vm.stack,1)) then
                        begin
                          BinaryOp(boAdd);
                        end
                        else
                        if isString(peekStack(vm.stack,0)) and isString(peekStack(vm.stack,1)) then
                        begin
                          Concatenate();
                        end
                        else
                        begin
                          runtimeError('Operands must be two numbers or two strings.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          runtimeError('Operand must be a number.');
                          exit;
                        end;




        end;

        OP_SUBTRACT : BinaryOp(boSubtract);

        OP_MULTIPLY : BinaryOP(boMultiply);

        OP_DIVIDE   : BinaryOp(boDivide);

        OP_NOT      : pushStack(vm.Stack,CreateBoolean(isFalsey(popStack(vm.Stack))));

        OP_RETURN: begin
           if vm.Stack.count > 0 then
           begin
             value := popStack(vm.Stack); //pop the last thing on stack and exit -- this unsafely assumes something is on the stack at this point..
           end
           else
           begin
             value.ValueKind := vknull;
           end;


          //TODO output.Add(floattostr(value.));

           Result.Code := INTERPRET_OK;
           Result.value := value;
           Exit(Result);
        end;
      end;
    end;

end;




procedure ResetStack(var stack : pStackRecord);
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
  Assert(Assigned(Stack.values), 'Stack values is not assigned');
  Stack.StackTop := Stack.Values;
end;



function peekStack(Stack: pStackRecord; DistanceFromTop: Integer): TValue;
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
  Assert(Assigned(Stack.Values), 'Stack values is not assigned');
  Assert(DistanceFromTop >= 0, 'Distance from top is negative');
  Assert(DistanceFromTop < Stack.Count, 'Distance from top is >= Stack.Count');

  Result := Stack.Values[Stack.Count - 1 - DistanceFromTop];
end;

function peekStack(Stack: pStackRecord): TValue;
begin
  Result := peekStack(Stack, 0);
end;

function popStack(var stack : pStackRecord) : TValue;
begin
   Assert(Assigned(Stack), 'Stack is not assigned');
   Assert(Assigned(Stack.values), 'Stack values is not assigned');
   Assert(Stack.Count > 0, 'Stack underflow error on stack pop zero count');
   Dec(Stack.StackTop);
   result := Stack.StackTop^;
   Dec(Stack.Count);
end;

function CheckBinaryNumbers: Boolean;
begin
  Result := False;
  if (not isNumber(PeekStack(vm.stack,0))) or
     (not isNumber(PeekStack(vm.stack,1))) then
  begin
    RuntimeError('Operands must be numbers.');
    exit;
  end;

  Result := True;
end;


function BinaryOpNumber_Add: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A + B));
  Result := true;
end;

function BinaryOpNumber_Subtract: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A - B));
  Result := true;
end;

function BinaryOpNumber_Multiply: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A * B));
  Result := true;
end;

function BinaryOpNumber_Divide: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A / B));
  Result := true;
end;

function BinaryOpNumber_Greater: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateBoolean(A > B));
  Result := true;
end;

function BinaryOpNumber_Less: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateBoolean(A < B));
  Result := true;
end;


procedure BinaryOp(Op: TBinaryOperation);
begin
  case Op of
    boAdd       : BinaryOpNumber_Add;
    boSubtract  : BinaryOpNumber_subtract;
    boMultiply  : BinaryOpNumber_Multiply;
    boDivide    : BinaryOpNumber_Divide;
    boGreater   : BinaryOpNumber_Greater;
    boLess      : BinaryOpNumber_Less;
  else
    raise Exception.Create('Unknown operator');
  end;
end;


//entry point into vm
function InterpretResult(source : pAnsiChar) : TInterpretResult;
var
  Chunk : pChunk;
begin
  // Assert(Assigned(output),'strings is not assigned');
   Assert(Assigned(VM),'VM is not assigned');
   chunk := nil;
   try
     InitChunk(Chunk);
     if not compile(source,chunk) then
     begin
       Result.code :=  INTERPRET_COMPILE_ERROR;
       Exit;
     end;
     vm.chunk := chunk;
     vm.ip := vm.chunk.Code;
     Result := Run;
   finally
     freeChunk(chunk);
   end;
end;

procedure FreeObject(obj : pObj);
begin
  assert(assigned(obj), 'Object is not assigned to free');
  case obj.ObjectKind of
    okString : begin
      FreeString(pObjString(obj));
    end;
  end;
end;

procedure FreeObjects();
var
  obj : pObj;
  next : pObj;
begin
  if not vm.ownObjects then exit;

  obj := vm.CreatedObjects;
  while (obj <> nil) do
  begin
    next := obj.Next;
    freeObject(obj);
    obj := next;
  end;


  (*
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
  } *)
end;


procedure CollectGarbage();
begin

end;

procedure InitVM;
begin
  new(VM); //we don't care about making this route through allocate
  VM.Chunk := nil;
  VM.Stack := nil;
  vm.CreatedObjects := nil;
  vm.ownObjects := true;
  vm.BytesAllocated := 0;
  InitStack(VM.Stack);
  ResetStack(vm.Stack);
end;

procedure FreeVM;
begin
  FreeStack(VM.Stack);
  FreeObjects();
  dispose(VM); //and therefore (see above comment in initVM) we just dispose here
end;

procedure InitScanner(source : pAnsiChar);
begin
  scanner.start := source;
  scanner.current := source;
  scanner.line := 1;
end;


function advance : ansichar;
begin
  inc(scanner.Current);
  result := scanner.current[-1];
end;

function isAtEnd : boolean;
begin
  result := scanner.current^ = #0;
end;

function MakeToken(const tokenType : TTokenType)  : TToken;
begin
  result.tokenType := tokenType;
  result.start := scanner.start;
  result.length := scanner.current - scanner.start;
  result.line := scanner.line;
end;


function ErrorToken(msg : pAnsiChar) : TToken;
begin

  result.Tokentype := TOKEN_ERROR;
  result.start := msg;
  result.length := strlen(msg);
  result.line := scanner.line;
end;




function match(expected : Ansichar) : boolean;
begin
  if isAtEnd then exit(false);

  if scanner.current^ <> expected then exit(false);

  inc(scanner.current);

  result := true;
end;

function peek : ansichar;
begin
  result := scanner.current^;
end;

function PeekNext: ansiChar;
begin
  if IsAtEnd then
    Exit(#0);

  Result := (scanner.current + 1)^;
end;

procedure SkipWhitespace;
var
  c: AnsiChar;
begin
  while True do
  begin
    c := Peek;

    case c of
      CHAR_SPACE,
      CHAR_TAB,
      CHAR_CR:
        Advance;

      CHAR_LF:
        begin
          Inc(scanner.Line);
          Advance;
        end;

      CHAR_SLASH:
        begin
          // Line comment "//"
          if PeekNext = CHAR_SLASH then
          begin
            // Consume until end of line or EOF
            while (Peek <> CHAR_LF) and (not IsAtEnd) do
              Advance;
          end
          else
            Exit;
        end;

    else
      Exit;
    end;
  end;
end;


function isDigit(c : Ansichar) : boolean;
begin
  Result := (c >= '0') and (c <= '9');
end;

(*
static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
} *)


function isAlpha(c : AnsiChar) : boolean;
begin
  result := ((c >= 'a') and (c <= 'z')) or
            ((c >= 'A') and (c <= 'Z')) or
            (c = '_');
end;

function ScanString: TToken;
begin
  while (Peek <> CHAR_QUOTE) and (not IsAtEnd) do
  begin
    if Peek = CHAR_LF then
      Inc(scanner.Line);

    Advance;
  end;

  if IsAtEnd then
    Exit(ErrorToken('Unterminated string.'));

  // Consume the closing quote
  Advance;

  Result := MakeToken(TOKEN_STRING);
end;


function ScanNumber: TToken;
begin
  while IsDigit(Peek) do
    Advance;

  // Look for a fractional part
  if (Peek = CHAR_DOT) and IsDigit(PeekNext) then
  begin
    // Consume the '.'
    Advance;

    while IsDigit(Peek) do
      Advance;
  end;

  Result := MakeToken(TOKEN_NUMBER);
end;


function CheckKeyword(start, length: Integer; const rest: pAnsiChar;
  tokenType: TTokenType): TTokenType;
begin
  if (scanner.current - scanner.start = start + length) and
     (CompareMem(scanner.start + start, rest, length)) then
    Exit(tokenType);

  Result := TOKEN_IDENTIFIER;
end;

function identifierType : TTokenType;
begin
    case scanner.start^ of
      'a': Exit(CheckKeyword(1, 2, 'nd',    TOKEN_AND));
      'c': Exit(CheckKeyword(1, 4, 'lass',  TOKEN_CLASS));
      'e': Exit(CheckKeyword(1, 3, 'lse',   TOKEN_ELSE));
      'f':
      begin
        if (scanner.current - scanner.start > 1) then
        begin
          case (scanner.start + 1)^ of
            'a': Exit(CheckKeyword(2, 3, 'lse', TOKEN_FALSE));
            'o': Exit(CheckKeyword(2, 1, 'r',   TOKEN_FOR));
            'u': Exit(CheckKeyword(2, 1, 'n',   TOKEN_FUN));
          end;
        end;
      end;
      'i': Exit(CheckKeyword(1, 1, 'f',     TOKEN_IF));
      'n': Exit(CheckKeyword(1, 2, 'il',    TOKEN_NIL));
      'o': Exit(CheckKeyword(1, 1, 'r',     TOKEN_OR));
      'p': Exit(CheckKeyword(1, 4, 'rint',  TOKEN_PRINT));
      'r': Exit(CheckKeyword(1, 5, 'eturn', TOKEN_RETURN));
      's': Exit(CheckKeyword(1, 4, 'uper',  TOKEN_SUPER));
      't':
      begin
        if (scanner.current - scanner.start > 1) then
        begin
          case (scanner.start + 1)^ of
            'h': Exit(CheckKeyword(2, 2, 'is', TOKEN_THIS));
            'r': Exit(CheckKeyword(2, 2, 'ue', TOKEN_TRUE));
          end;
        end;
      end;
      'v': Exit(CheckKeyword(1, 2, 'ar',    TOKEN_VAR));
      'w': Exit(CheckKeyword(1, 4, 'hile',  TOKEN_WHILE));
    end;
    result := TOKEN_IDENTIFIER;
end;


function Identifier : TToken;
begin
  while (isAlpha(peek()) or isDigit(peek())) do advance();
  result := makeToken(identifierType());
end;

function ScanToken : TToken;
var
  c : Ansichar;
begin

  skipWhitespace;

  Scanner.start := Scanner.Current;

  if (isAtEnd()) then
  begin
    result := makeToken(TOKEN_EOF);
    exit;
  end;

  c := Advance;


  if (isAlpha(c)) then Exit(identifier);

  if (isDigit(c)) then Exit(ScanNumber);


  case (c) of
      '(': result :=  makeToken(TOKEN_LEFT_PAREN);

      ')': result :=  makeToken(TOKEN_RIGHT_PAREN);

      '{': result :=  makeToken(TOKEN_LEFT_BRACE);

      '}': result :=  makeToken(TOKEN_RIGHT_BRACE);

      ';': result :=  makeToken(TOKEN_SEMICOLON);

      ',': result :=  makeToken(TOKEN_COMMA);

      '.': result :=  makeToken(TOKEN_DOT);

      '-': result :=  makeToken(TOKEN_MINUS);

      '+': result :=  makeToken(TOKEN_PLUS);

      '/': result :=  makeToken(TOKEN_SLASH);

      '*': result :=  makeToken(TOKEN_STAR);

     '!':
        begin
          if Match('=') then
            Result := MakeToken(TOKEN_BANG_EQUAL)
          else
            Result := MakeToken(TOKEN_BANG);
        end;

      '=':
        begin
          if Match('=') then
            Result := MakeToken(TOKEN_EQUAL_EQUAL)
          else
            Result := MakeToken(TOKEN_EQUAL);
        end;

      '<':
        begin
          if Match('=') then
            Result := MakeToken(TOKEN_LESS_EQUAL)
          else
            Result := MakeToken(TOKEN_LESS);
        end;

      '>':
        begin
          if Match('=') then
            Result := MakeToken(TOKEN_GREATER_EQUAL)
          else
            Result := MakeToken(TOKEN_GREATER);
        end;

        CHAR_QUOTE :
        begin
           Result := ScanString;
        end

        else
          result := errorToken('Unexpected character.');
  end

end;

function TokenToString(const Token: TToken): AnsiString;
begin
  SetString(Result, PAnsiChar(Token.Start), Token.Length);
end;

procedure DumpTokens(const output : TStrings);
var
  token: TToken;
  lexeme: string;
  lastLine: Integer;
begin
  lastLine := -1;

  while True do
  begin
    token := ScanToken();

    // Convert pointer+length to a proper Delphi string
    lexeme := TokenToString(token);

    // Print line number only once per line
    if token.Line <> lastLine then
    begin
      output.Add(Format('%4d: %-15s %s', [
        token.Line,
        GetEnumName(TypeInfo(TTokenType), Ord(token.TokenType)),
        lexeme
      ]));
      lastLine := token.Line;
    end
    else
    begin
      output.Add(Format('     | %-15s %s', [
        GetEnumName(TypeInfo(TTokenType), Ord(token.TokenType)),
        lexeme
      ]));
    end;

    if token.TokenType = TOKEN_EOF then
      Break;
  end;
end;



procedure ErrorAt(const Token: TToken; const Msg: PAnsiChar);
var
  s, tokenText, msgText: string;
begin
  if Parser.PanicMode then
    Exit;

  s := Format('[line %d] Error', [Token.Line]);

  case Token.TokenType of
    TOKEN_EOF:
      s := s + ' at end';

    TOKEN_ERROR:
      ; // nothing

  else
    SetString(tokenText, Token.Start, Token.Length);
    s := s + ' at ''' + tokenText + '''';
  end;

  Parser.ErrorStr := s;
  Parser.HadError := True;
end;



procedure Error(const Msg: pAnsiChar);
begin
  parser.panicMode := true;
  ErrorAt(Parser.Previous, Msg);
end;


procedure errorAtCurrent(const msg : pAnsiChar);
begin
  errorAt(parser.current,msg);
end;

procedure AdvanceParser();
begin
  Parser.Previous := Parser.Current;
  while true do
  begin
    parser.current := ScanToken;
    if (parser.Current.tokenType <> TOKEN_ERROR) then
      break;

      errorAtCurrent(Parser.Current.start);
  end;
end;

procedure Consume(TokenKind: TTokenType; const Msg);
begin
  if Parser.Current.TokenType = TokenKind then
  begin
    AdvanceParser;
    Exit;
  end;

  ErrorAtCurrent(pAnsiChar(Msg));
end;

function currentChunk : pChunk;
begin
  result := CompilingChunk;
end;


procedure emitByte(value : byte);
begin
  writeChunk(CurrentChunk,value,parser.previous.line);
end;

procedure EmitReturn;
begin
  writeChunk(CurrentChunk,OP_RETURN,parser.previous.line);
end;

procedure emitConstant(value : TValue);
begin
  AddConstant(CurrentChunk,value,parser.previous.line);
end;

function  getRule(tokenType : TTokenType) : TParseRule;
begin
  result := Rules[tokenType];
end;

procedure FatalError;
begin


end;

procedure parsePrecedence(precedence : TPrecedence);
var
  prefixRule : procedure;
  infixRule  : procedure;
begin
  prefixRule := nil;
  infixRule  := nil;
  advanceParser();

  //do prefix
  prefixRule := getRule(parser.previous.tokenType).prefix;
  Assert(assigned(prefixRule),'Expect expression. Prefix rule is not assigned. Halting execution');

  try
    prefixRule();
  Except
    on E:Exception do
    begin
      showmessage('prefix rule failure' + e.Message);
      FatalError;
    end;
  end;

  //now do infix
  while (precedence <= (getRule(parser.current.tokenType).precedence)) do
  begin
    advanceParser;
    infixRule := getRule(parser.previous.tokentype).infix;
    infixRule();
  end;
end;


procedure Number();
var
  Value  : TValue;
  lexeme : string;
begin
  lexeme := TokenToString(parser.previous);
  Value := CreateNumber(StrToFloat(lexeme));
  EmitConstant(Value);
end;

procedure Expression();
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure grouping;
begin
  expression;
  consume(TOKEN_RIGHT_PAREN,'Expect '')'' after expression.');
end;

procedure unary();
var
  operatorType : TTokenType;
begin
  //TODO what asserts go here?
  operatorType := parser.Previous.tokenType;
  expression;
  case operatortype of
    TOKEN_MINUS: emitByte(OP_NEGATE);
    TOKEN_BANG: emitByte(OP_NOT);   //TODO finish all the code for token_bang pg 569
  end;
end;


procedure ParseString();
var
  lexeme  : ansiString;
  strObj : pObjString;
  value  : TValue;
begin
  lexeme := TokenToString(parser.Previous);
  // strip leading and trailing quotes
  if (Length(lexeme) >= 2) and (lexeme[1] = '"') and (lexeme[Length(lexeme)] = '"') then
    lexeme := Copy(lexeme, 2, Length(lexeme) - 2);
  strObj := CreateString(lexeme);
  value := StringToValue(strObj);
  emitConstant(value);
end;

procedure binary();
var
  tokenType : TTokenType;
  rule : TParseRule;
begin
  tokenType := parser.Previous.tokenType;
  rule := getRule(tokenType);
  parsePrecedence(TPrecedence(Ord(Rule.Precedence) + 1));

  case tokenType of

      TOKEN_BANG_EQUAL: begin
        emitByte(OP_EQUAL);
        emitByte(OP_NOT);
      end;

      TOKEN_EQUAL_EQUAL: begin
        emitByte(OP_EQUAL);
      end;

      TOKEN_GREATER:    begin
         emitByte(OP_GREATER);
      end;

      TOKEN_GREATER_EQUAL: begin
        emitByte(OP_LESS);
        emitByte(OP_NOT);
      end;

      TOKEN_LESS: begin
        emitByte(OP_LESS);
      end;

      TOKEN_LESS_EQUAL: begin
          emitByte(OP_GREATER);
          emitByte(OP_NOT);
      end;

      TOKEN_PLUS : begin
        emitByte(OP_ADD);
      end;

      TOKEN_MINUS : begin
        emitByte(OP_SUBTRACT);
      end;

      TOKEN_STAR : begin
        emitByte(OP_MULTIPLY);
      end;

      TOKEN_SLASH : begin
        emitByte(OP_DIVIDE);
      end
      else
      begin
        Showmessage('Invalid token -- not implemented yet!');
        FatalError;
      end;
  end;
end;


procedure literal();
begin
  case parser.previous.tokenType of
      TOKEN_FALSE : EmitByte(OP_FALSE);
      TOKEN_TRUE  : EmitByte(OP_TRUE);
      TOKEN_NIL   : EmitByte(OP_NIL);
  end;
end;

procedure endCompiler;
begin
  emitReturn;
end;

function compile(source : pAnsiChar; chunk : pChunk) : boolean;
var
  line : integer;
  token : TToken;
begin
   assert(assigned(chunk), 'Chunk is not assigned');
   assert(assigned(source), 'Source code is not assigned');

   initScanner(source);
   compilingChunk := chunk;
   parser.hadError := false;
   parser.panicMode := false;
   advanceParser();
   Expression();
   consume(TOKEN_EOF, 'Expect end of expression.');
   endCompiler;
   result := parser.HadError = false;
end;


initialization
  InitVM;

finalization
  FreeVM;
  Assert(VM.BytesAllocated = 0, 'VM has not disposed of all mem allocation');

end.
