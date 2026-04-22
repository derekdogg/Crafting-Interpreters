unit Chunk_Types;
{$POINTERMATH ON}
{$ASSERTIONS ON}
interface

uses
  Classes, dialogs;

const

  START_CAPACITY =  256;
  MAX_SIZE       =  START_CAPACITY * 256;
  GROWTH_FACTOR  =  2;

  //Table
  TABLE_MAX_LOAD = 0.75;

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
  OP_CONSTANT_LONG = 14;

  OP_STRINGS : array[0..14] of string = (
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
    'OP_LESS',
    'OP_CONSTANT_LONG');


type

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
  pValue          = ^TValue;
  pObj            = ^TObj;
  pObjString      = ^TObjString;
  pStack          = ^TStack;
  pVirtualMachine = ^TVirtualMachine;
  pMemTracker     = ^TMemTracker;
  pLogs           = ^TLogs; //Note : Don't think we need this (https://www.danieleteti.it/loggerpro/) maybe add later? We use a stupidly simple approach for now
  pEntry          = ^TEntry;
  pTable          = ^TTable;
  //pRoots          = ^TRoots;

  //Arrays
  TAnsiCharArray = Array[0..0] of AnsiChar;


  //container for all root level memory
  TRoots = record
     Stack : pStack;
  end;

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
    IsMarked   : boolean;
    Next       : pObj; //for GC collection
  end;

  TObjString = record
    Obj     : TObj; //note here is has the same header as TObj, to allow cast back and forth
    length  : integer;
    hash    : uint32;
    chars   : TAnsiCharArray;
  end;

  TIntToByteResult = record
     byte0 : byte;
     byte1 : byte;
     byte2 : byte;
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
    Count              : Integer;
    CurrentCapacity    : Integer;
    Code               : pByte;
    Constants          : pValueArray;
    Lines              : pInteger;
  end;

  TValueArray = record
    Count             : Integer;
    CurrentCapacity   : Integer;
    Values            : pValue;
  end;

  TStack  = record
    Count             : Integer;
    CurrentCapacity   : Integer;
    Values            : pValue;
    StackTop          : pValue;
  end;

  //Virtual Machine result
  TInterpretResult = record
    code  : (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);
    value : TValue;
    ErrorStr : String;
  end;

  TMemTracker = record
    CreatedObjects  : pObj;
    BytesAllocated  : integer;
    Roots           : TRoots;
  end;

  //Virtual Machine
  TVirtualMachine = record
    Chunk           : pChunk;
    Stack           : pStack;
    MemTracker      : PMemTracker;
    RuntimeErrorStr : String;
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


  TEntry = record
    key     : pObjString;
    value   : TValue;
  end;

  TTable = record
    Count           : integer;
    CurrentCapacity : integer;
    Entries         : pEntry;
  end;


//Assertions
procedure AssertMemTrackerIsNotNil(MemTracker : pMemTracker);
procedure AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker : pMemTracker);
procedure AssertNewStringIsNillBeforeAllocation(ObjString : pObjString);

//Stack assertions
procedure AssertStackIsAssigned(Stack : pStack);
procedure AssertStackValuesIsAssigned(Stack : pStack);
procedure AssertStackIsNotEmpty(Stack : pStack);
procedure AssertStackTopIsNotNil(Stack : pStack);
procedure AssertStackIsNilBeforeInit(Stack : pStack);

//Chunk assertions
procedure AssertChunkIsAssigned(Chunk : pChunk);
procedure AssertChunkCodeIsAssigned(Chunk : pChunk);
procedure AssertChunkHasInstructions(Chunk : pChunk);
procedure AssertChunkEndsWithReturn(Chunk : pChunk);
procedure AssertChunkConstantsIsAssigned(Chunk : pChunk);

//ValueArray assertions
procedure AssertValueArrayIsAssigned(ValueArray : pValueArray);
procedure AssertValueArrayIsNilBeforeInit(ValueArray : pValueArray);
procedure AssertValuesIsAssigned(Values : pValue);
procedure AssertValueArrayCount(ValueArray : pValueArray);

//Object assertions
procedure AssertObjectIsAssigned(Obj : pObj);
procedure AssertObjStringIsAssigned(ObjString : pObjString);

//Index and range assertions
procedure AssertIndexIsNotNegative(Index : integer);
procedure AssertIndexInRange(Index : integer; MaxValue : integer);
procedure AssertCapacityIsPositive(Capacity : integer);
procedure AssertCountIsNotNegative(Count : integer);
procedure AssertSizeIsNotNegative(Size : integer);
procedure AssertLineIsNotNegative(Line : integer);

//VM assertions
procedure AssertVMIsAssigned;
procedure AssertVMChunkIsAssigned;
procedure AssertVMChunkCodeIsAssigned;

//Pointer assertions
procedure AssertPointerIsNotNil(p : pointer; const context : string);
procedure AssertPointerIsNil(p : pointer; const context : string);
procedure AssertCodePointerIsAssigned(Code : pByte);

//Source code assertions
procedure AssertSourceCodeIsAssigned(Source : pAnsiChar);

//String object assertions
procedure AssertStringLengthIsNotNegative(ObjString : pObjString);
procedure AssertObjectKindIsString(Obj : pObj);

//Value type assertions
procedure AssertValueIsBoolean(const Value : TValue);
procedure AssertValueIsNumber(const Value : TValue);
procedure AssertValueIsNil(const Value : TValue);
procedure AssertValueIsObject(const Value : TValue);
procedure AssertValueIsString(const Value : TValue);

//Size comparison assertions
procedure AssertOldSizeNotEqualNewSize(OldSize, NewSize : integer);
procedure AssertCountDoesNotExceedCapacity(Count, Capacity : integer);

//Parser assertions
procedure AssertParseFnIsAssigned(ParseFn : TParseFn; const context : string);

//Distance/offset assertions
procedure AssertDistanceIsNotNegative(Distance : integer);
procedure AssertDistanceInRange(Distance, MaxValue : integer);




//Memory creation routines
procedure ClearMem(p: PByte; FromIndex, Count: Integer);
procedure Allocate(var p : pointer; oldsize,newSize : integer; MemTracker : pMemTracker);
function AllocateArray(var List: Pointer;  var CurrentCapacity: Integer;  Count, ElemSize: Integer; MemTracker : pMemTracker): Boolean;

//object creation routines
procedure AddToCreatedObjects(p : pObj; MemTracker : pMemTracker);
function CreateString(const S: AnsiString; MemTracker : pMemTracker): PObjString;
procedure FreeString(var obj : pObjString; MemTracker : pMemTracker);

//string routines
function GetChar(const str : pObjString; index : integer) : AnsiChar;
function StringsEqual(a, b: PObjString): Boolean;
function ValuesEqual(a, b : TValue) : boolean;
function TokenToString(const Token: TToken): AnsiString;
function ObjStringToAnsiString(S: PObjString): AnsiString;
function ValueToString(const value : TValue) : pObjString;
function StringToValue(const value : pObjString) : TValue;
procedure Concatenate(stack : pStack; MemTracker : pMemTracker);


//Chunk routines
procedure initChunk(var chunk: pChunk;MemTracker : pMemTracker);
procedure freeChunk(var chunk: pChunk;MemTracker : pMemTracker);
procedure emitByte(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker );
procedure EmitReturn(Chunk : pChunk; Line : integer; MemTracker : pMemTracker);

procedure writeChunk(chunk: pChunk; value: byte; Line : Integer;MemTracker : pMemTracker);
procedure AddConstant(chunk : pChunk; const value : TValue; Line : Integer; MemTracker : pMemTracker);
procedure initValueArray(var ValueArray : pValueArray;MemTracker : pMemTracker);
function AddValueConstant(ValueArray: pValueArray; const value: TValue;Memtracker : pMemTracker): Integer;
procedure writeValueArray(ValueArray : pValueArray; Value : TValue;MemTracker : pMemTracker);
procedure FreeValues(var Values : pValue; Capacity : integer;MemTracker : pMemTracker);
procedure freeValueArray(var ValueArray : pValueArray;MemTracker : pMemTracker);
procedure printValueArray(ValueArray: pValueArray; strings: TStrings);
function IntToBytes(const value : integer) : TIntToByteResult;
function ByteToInt(const value : TIntToByteResult) : integer;
function ReadByte(var code : pByte): Byte;
function ReadConstant(var code : pByte; constants : pValueArray) : TValue;
function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue;
//Memtracker
procedure InitMemTracker(var MemTracker : pMemTracker);
procedure FreeMemTracker(var MemTracker : pMemTracker);
procedure MarkObject(obj : pObj);
procedure MarkValue(value : pValue);
procedure MarkRoots(Stack : pStack);
procedure CollectGarbage(MemTracker : pMemTracker);


//Virtual Machine
procedure InitVM();
function InterpretResult(source : pAnsiChar) : TInterpretResult;
procedure FreeObjects(objects: pObj);
procedure FreeVM();

//Stack
procedure InitStack(var Stack : pStack;MemTracker : pMemTracker);
procedure FreeStack(var Stack : pStack;MemTracker : pMemTracker);
procedure ResetStack(var stack : pStack);
procedure pushStack(var stack : pStack;const value : TValue;MemTracker : pMemTracker);
function peekStack(stack : pStack) : TValue; overload;
function peekStack(stack : pStack; distanceFromTop : integer) : TValue;overload;
function  popStack(var stack : pStack) : TValue;

//Scanner
procedure InitScanner(source : pAnsiChar);
function  advance : ansichar;
function  isAtEnd : boolean;

//compilation
function BinaryOp(Op: TBinaryOperation): boolean;
function compile(source : pAnsiChar; chunk : pChunk) : boolean;
procedure Number();
procedure grouping();
procedure unary();
procedure binary();
procedure literal();
procedure ParseString();

//Hash
function HashString(const Key: PAnsiChar; Length: Integer): UInt32;

//Value constructors
function CreateNumber(Value: Double): TValue;
function CreateBoolean(Value: Boolean): TValue;
function CreateNilValue: TValue;

//Table
procedure InitTable(var Table : pTable; memTracker : pMemTracker);
function TableSet(var Table : pTable; key : pObjString; value : TValue;MemTracker : pMemTracker): boolean;
function TableGet(Table : pTable; key : pObjString; var value : TValue): boolean;
procedure FreeTable(var Table : pTable; memTracker : pMemTracker);

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
  CompilingChunk : pChunk; //related to the pratt parser table because we have to have a global chunk in there.
  //Output : TStrings;

implementation

uses
  sysutils, Math,strUtils, typinfo;

procedure AssertTable(Table : pTable);
begin
  Assert(assigned(Table), 'Table is not assigned');
end;

procedure AssertTableEntries(Table : pTable);
begin
  AssertTable(Table);
  Assert(assigned(Table.Entries), 'Table Entries is not assigned');
end;

procedure AssertMemTrackerIsNotNil(MemTracker : pMemTracker);
begin
  Assert(MemTracker <> nil, 'Memory tracker is nil');
end;

procedure AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker : pMemTracker);
begin
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(Memtracker.BytesAllocated >= 0, 'VM bytes underflow');
end;

procedure AssertNewStringIsNillBeforeAllocation(ObjString : pObjString);
begin
  Assert(ObjString = nil, 'new obj string is not nil before alloc');
end;

//Stack assertions
procedure AssertStackIsAssigned(Stack : pStack);
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
end;

procedure AssertStackValuesIsAssigned(Stack : pStack);
begin
  Assert(Assigned(Stack.Values), 'Stack values is not assigned');
end;

procedure AssertStackIsNotEmpty(Stack : pStack);
begin
  Assert(Stack.Count > 0, 'Stack underflow - count is zero');
end;

procedure AssertStackTopIsNotNil(Stack : pStack);
begin
  Assert(Stack.StackTop <> nil, 'Stack top is nil');
end;

procedure AssertStackIsNilBeforeInit(Stack : pStack);
begin
  Assert(Stack = nil, 'Stack initialization failure - stack record is not nil');
end;

//Chunk assertions
procedure AssertChunkIsAssigned(Chunk : pChunk);
begin
  Assert(Assigned(Chunk), 'Chunk is not assigned');
end;

procedure AssertChunkCodeIsAssigned(Chunk : pChunk);
begin
  Assert(Assigned(Chunk.Code), 'Chunk code is not assigned');
end;

procedure AssertChunkHasInstructions(Chunk : pChunk);
begin
  Assert(Chunk.Count > 0, 'No chunks to interpret');
end;

procedure AssertChunkEndsWithReturn(Chunk : pChunk);
begin
  Assert(Chunk.Code[Chunk.Count-1] = OP_RETURN, 'Expected return otherwise will loop infinitely');
end;

procedure AssertChunkConstantsIsAssigned(Chunk : pChunk);
begin
  Assert(Assigned(Chunk.Constants), 'Chunk constants is not assigned');
end;

//ValueArray assertions
procedure AssertValueArrayIsAssigned(ValueArray : pValueArray);
begin
  Assert(Assigned(ValueArray), 'ValueArray is not assigned');
end;

procedure AssertValueArrayIsNilBeforeInit(ValueArray : pValueArray);
begin
  Assert(ValueArray = nil, 'ValueArray is not nil before initialization');
end;

procedure AssertValuesIsAssigned(Values : pValue);
begin
  Assert(Assigned(Values), 'Values is not assigned');
end;

procedure AssertValueArrayCount(ValueArray : pValueArray);
begin
  Assert(ValueArray.Count >= 0, 'Value array count is less than zero');
end;

//Object assertions
procedure AssertObjectIsAssigned(Obj : pObj);
begin
  Assert(Assigned(Obj), 'Object is not assigned');
end;

procedure AssertObjStringIsAssigned(ObjString : pObjString);
begin
  Assert(Assigned(ObjString), 'ObjString is not assigned');
end;

//Index and range assertions
procedure AssertIndexIsNotNegative(Index : integer);
begin
  Assert(Index >= 0, 'Index underflow');
end;

procedure AssertIndexInRange(Index : integer; MaxValue : integer);
begin
  Assert(Index < MaxValue, 'Index overflow');
end;

procedure AssertCapacityIsPositive(Capacity : integer);
begin
  Assert(Capacity > 0, 'Capacity must be positive');
end;

procedure AssertCountIsNotNegative(Count : integer);
begin
  Assert(Count >= 0, 'Count underflow');
end;

procedure AssertSizeIsNotNegative(Size : integer);
begin
  Assert(Size >= 0, 'Size underflow');
end;

procedure AssertLineIsNotNegative(Line : integer);
begin
  Assert(Line >= 0, 'Line is < 0');
end;

//VM assertions
procedure AssertVMIsAssigned;
begin
  Assert(Assigned(VM), 'VM is not assigned');
end;

procedure AssertVMChunkIsAssigned;
begin
  Assert(Assigned(VM.Chunk), 'VM Chunk is not assigned');
end;

procedure AssertVMChunkCodeIsAssigned;
begin
  Assert(Assigned(VM.Chunk.Code), 'VM chunk code is not assigned');
end;

//Pointer assertions
procedure AssertPointerIsNotNil(p : pointer; const context : string);
begin
  Assert(p <> nil, 'Pointer is nil: ' + context);
end;

procedure AssertPointerIsNil(p : pointer; const context : string);
begin
  Assert(p = nil, 'Pointer is not nil: ' + context);
end;

procedure AssertCodePointerIsAssigned(Code : pByte);
begin
  Assert(Assigned(Code), 'Code is not assigned');
end;

//Source code assertions
procedure AssertSourceCodeIsAssigned(Source : pAnsiChar);
begin
  Assert(Assigned(Source), 'Source code is not assigned');
end;

//String object assertions
procedure AssertStringLengthIsNotNegative(ObjString : pObjString);
begin
  Assert(ObjString.Length >= 0, 'String length is negative');
end;

procedure AssertObjectKindIsString(Obj : pObj);
begin
  Assert(Obj.ObjectKind = okString, 'Type mismatch, expected a string object but object kind is not a string');
end;

//Size comparison assertions
procedure AssertOldSizeNotEqualNewSize(OldSize, NewSize : integer);
begin
  Assert(OldSize <> NewSize, 'Old size = new Size - invalid allocation');
end;

procedure AssertCountDoesNotExceedCapacity(Count, Capacity : integer);
begin
  Assert(Count <= Capacity, 'Count exceeds CurrentCapacity');
end;

//Parser assertions
procedure AssertParseFnIsAssigned(ParseFn : TParseFn; const context : string);
begin
  Assert(Assigned(ParseFn), 'Expect expression. Parse function is not assigned: ' + context);
end;

//Distance/offset assertions
procedure AssertDistanceIsNotNegative(Distance : integer);
begin
  Assert(Distance >= 0, 'Distance from top is negative');
end;

procedure AssertDistanceInRange(Distance, MaxValue : integer);
begin
  Assert(Distance < MaxValue, 'Distance from top is >= maximum value');
end;


function IntToBytes(const value: Integer): TIntToByteResult;
begin
  Assert((value >= 0) and (value <= $FFFFFF),
    'IntToBytes: value out of 24-bit range');
  Result.byte0 := Byte(value and $FF);
  Result.byte1 := Byte((value shr 8) and $FF);
  Result.byte2 := Byte((value shr 16) and $FF);
end;

function ByteToInt(const value: TIntToByteResult): Integer;
begin
  // Structural sanity (documents intent)
  Assert(SizeOf(TIntToByteResult) = 3,
    'TIntToByteResult must be exactly 3 bytes');

  // Logical sanity (24-bit unsigned result)
  Result :=
    Integer(value.byte0) or
    (Integer(value.byte1) shl 8) or
    (Integer(value.byte2) shl 16);

  Assert((Result >= 0) and (Result <= $FFFFFF),
    'ByteToInt: reconstructed value out of 24-bit range');
end;


//memory creation routines

function IsInCreatedObjects(obj: PObj): Boolean;
var
  current: PObj;
begin
  current := vm.MemTracker.CreatedObjects; // head of VM object list
  while Assigned(current) do
  begin
    if current = obj then
      Exit(True);
    current := current^.Next;
  end;
  Result := False;
end;


procedure IncrementBytesAllocated(memTracker : pMemTracker; Amount : integer);
begin
  AssertMemTrackerIsNotNil(MemTracker);
  AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
  Inc(memtracker.BytesAllocated, Amount);
  AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
end;

procedure ClearMem(p: PByte; FromIndex, Count: Integer);
begin
  AssertPointerIsNotNil(p, 'ClearMem');
  AssertIndexIsNotNegative(FromIndex);
  AssertCountIsNotNegative(Count);
  Assert(FromIndex <= High(Integer) - Count, 'mem buffer overflow');
  FillChar(p[FromIndex], Count, 0);
end;


procedure Allocate(var p: Pointer; OldSize, NewSize: Integer; MemTracker : pMemTracker);
begin
  AssertMemTrackerIsNotNil(MemTracker);
  AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
  // --------------------------------------------------------------------------
  // Basic sanity checks on sizes
  // --------------------------------------------------------------------------
  AssertSizeIsNotNegative(NewSize);   // New allocation cannot be negative
  AssertSizeIsNotNegative(OldSize);  // Old allocation cannot be negative
  AssertOldSizeNotEqualNewSize(OldSize, NewSize);

  // --------------------------------------------------------------------------
  // Pointer consistency invariant
  // --------------------------------------------------------------------------
  // Enforce that OldSize = 0 means pointer must be nil
  // and OldSize > 0 means pointer must be valid
  if OldSize = 0 then AssertPointerIsNil(p, 'OldSize = 0 but pointer is not nil');
  if OldSize > 0 then AssertPointerIsNotNil(p, 'OldSize > 0 but pointer is nil');


  // --------------------------------------------------------------------------
  // Garbage Collection run
  // --------------------------------------------------------------------------
  if (NewSize > OldSize) then
  begin
    CollectGarbage(MemTracker);
  end;

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
  begin
    ClearMem(pByte(p),OldSize,NewSize - OldSize);
    //FillChar(PByte(p)[OldSize], NewSize - OldSize, 0);
  end;

  // --------------------------------------------------------------------------
  // Tracks total allocated bytes for GC / memory management
  // --------------------------------------------------------------------------
  IncrementBytesAllocated(Memtracker, NewSize - OldSize);

  // --------------------------------------------------------------------------
  // Zero-size check
  // --------------------------------------------------------------------------
  // If allocation shrinks to zero, the pointer must now be nil
  if NewSize = 0 then
    AssertPointerIsNil(p, 'Pointer not nil after zero-size allocation');

  // --------------------------------------------------------------------------
  // Non-Zero-size check
  // --------------------------------------------------------------------------
  // If allocation shrinks to zero, the pointer must now be nil
  if NewSize > 0 then
    AssertPointerIsNotNil(p, 'Pointer nil after new size > 0 allocation');

end;


function AllocateArray(var List: Pointer;  var CurrentCapacity: Integer;  Count, ElemSize: Integer; MemTracker : pMemTracker): Boolean;
var
  NewCapacity: Integer;
  OldSize, NewSize: Integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
  // ---- Global invariants ---------------------------------------------------

  Assert(START_CAPACITY > 0, 'START_CAPACITY must be greater than zero');
  Assert(GROWTH_FACTOR > 1, 'GROWTH_FACTOR must be greater than 1');
  Assert(ElemSize > 0,'ElemSize must be greater than zero');
  AssertCountIsNotNegative(CurrentCapacity);
  AssertCountIsNotNegative(Count);
  AssertCountDoesNotExceedCapacity(Count, CurrentCapacity);

  // ---- Initial allocation --------------------------------------------------
  if CurrentCapacity = 0 then
  begin
    AssertPointerIsNil(List, 'List must be nil when CurrentCapacity is zero');

    // Ensure capacity * element size will not overflow
    Assert(START_CAPACITY <= MaxInt div ElemSize,
      'Initial allocation size exceeds addressable memory');

    CurrentCapacity := START_CAPACITY;
    NewSize := CurrentCapacity * ElemSize;

    Allocate(List, 0, NewSize, MemTracker);
    Exit(True);
  end;

  // ---- Growth path ---------------------------------------------------------
  AssertPointerIsNotNil(List, 'List is nil with non-zero CurrentCapacity');
  if Count < CurrentCapacity then
    Exit(False);

  // Ensure capacity growth itself cannot overflow Integer
  Assert(CurrentCapacity <= MaxInt div GROWTH_FACTOR,
    'Array capacity multiplication overflows Integer : ' + inttostr(CurrentCapacity));

  // Ensure logical size limit is not exceeded
  Assert(CurrentCapacity <= MAX_SIZE div GROWTH_FACTOR,
    'Array capacity growth would exceed MAX_SIZE + : ' + inttostr(CurrentCapacity));

  NewCapacity := CurrentCapacity * GROWTH_FACTOR;

  // Ensure byte-size multiplications are safe
  Assert(CurrentCapacity <= MaxInt div ElemSize,
    'Current array byte size exceeds Integer range');
  Assert(NewCapacity <= MaxInt div ElemSize,
    'Grown array byte size exceeds Integer range');

  OldSize := CurrentCapacity * ElemSize;
  NewSize := NewCapacity * ElemSize;

  Allocate(List, OldSize, NewSize, MemTracker);
  CurrentCapacity := NewCapacity;

  Result := True;
end;


procedure AllocateString(var p: PObjString; OldSize, NewSize: NativeInt; MemTracker : pMemTracker);
begin
  AssertNewStringIsNillBeforeAllocation(p);
  Allocate(Pointer(p), OldSize, NewSize, MemTracker);
end;

//note : we do not add a null terminator #0 since we track the length
function CreateString(const S: AnsiString; MemTracker : pMemTracker): PObjString;
var
  Len: Integer;
  NewSize: NativeInt;
begin

  AssertMemTrackerIsNotNil(MemTracker);
  Result := nil;
  Len := Length(S);
  NewSize := SizeOf(TObjString) + Max(0, Len - 1);  // avoid negative size

  AllocateString(Result,0, NewSize,MemTracker);

  Result^.Obj.ObjectKind := okString;
  Result^.Obj.Next := nil;
  Result^.Length := Len;
  Result^.hash := HashString(pAnsiChar(s),len);
  if Len > 0 then
  begin
    Move(PAnsiChar(S)^, Result^.Chars[0], Len);
    //Move(Pointer(S)^, Result^.Chars[0], Len);
  end;

  //track creation in the vm list of obj.
  AddToCreatedObjects(PObj(Result),MemTracker);
  
  // ---- Exit assertions ----
  AssertObjStringIsAssigned(Result);
  Assert(Result^.Length = Len, 'CreateString exit: length mismatch');
  Assert(Result^.Obj.ObjectKind = okString, 'CreateString exit: not a string object');
end;

procedure FreeString(var obj : pObjString; MemTracker : pMemTracker);
var
 objSize : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);

  // ---- preconditions ----
  AssertObjStringIsAssigned(obj);
  AssertStringLengthIsNotNegative(obj);
  AssertObjectKindIsString(@obj^.Obj);

  // ---- resize now ----
  objSize := Sizeof(TObjString) + Max(0, obj^.length - 1);  // mimc here size from CreateString (We don't have the string but we do have the length now)
  Allocate(pointer(obj), objSize , 0, MemTracker);
  obj := nil;

  // ---- postconditions ----
  AssertPointerIsNil(obj, 'string pointer not cleared after free');
  assert(Memtracker.BytesAllocated >= 0, 'VM bytes allocated underflow after freeing string.');
end;


procedure initChunk(var chunk: pChunk; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);

  AssertPointerIsNil(Chunk, 'Chunk initialization - chunk is not nil');
  Allocate(pointer(chunk),0, Sizeof(TChunk),MemTracker);

  chunk.Count := 0;
  chunk.CurrentCapacity := 0;
  chunk.Code := nil;
  chunk.Lines := nil;
  chunk.Constants := nil;
  InitValueArray(chunk.Constants,MemTracker);

  // ---- Exit assertions ----
  AssertChunkIsAssigned(chunk);
  Assert(chunk.Count = 0, 'initChunk exit: count should be 0');
  Assert(chunk.CurrentCapacity = 0, 'initChunk exit: capacity should be 0');
  Assert(chunk.Code = nil, 'initChunk exit: code should be nil');
  Assert(chunk.Lines = nil, 'initChunk exit: lines should be nil');
  AssertChunkConstantsIsAssigned(chunk);
end;

procedure freeChunk(var chunk: pChunk; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertChunkIsAssigned(Chunk);


  if (chunk.CurrentCapacity) > 0 then
  begin
    Allocate(pointer(chunk.Code), Chunk.CurrentCapacity * sizeof(Byte), 0,MemTracker);
    AssertPointerIsNil(Chunk.Code, 'Expected Chunk Code to be nil');
    Allocate(pointer(Chunk.Lines), Chunk.CurrentCapacity * Sizeof(Integer),0,MemTracker);
    AssertPointerIsNil(Chunk.Lines, 'Expected Chunk Lines to be nil');
  end;

  freeValueArray(chunk.Constants,MemTracker);
  AssertPointerIsNil(Chunk.Constants, 'Expected chunk Constants to be nil');
  Allocate(pointer(chunk),Sizeof(TChunk),0,MemTracker);
  AssertPointerIsNil(Chunk, 'Expected chunk to be nil');
end;

procedure writeChunk(chunk: pChunk; value: byte; Line : Integer; MemTracker : pMemTracker);
var
  currentCap : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertChunkIsAssigned(chunk);
  AssertLineIsNotNegative(Line);

  currentCap := Chunk.CurrentCapacity;
  if AllocateArray(Pointer(chunk.Code),  Chunk.CurrentCapacity, Chunk.Count, sizeof(Byte) , MemTracker) then
  begin
    //we have to do it like this because we can't pass Chunk.Capacity to grow the line array (since it will be altered)
    //and we keep the capacity of chunk and lines always equal.
    AllocateArray(Pointer(chunk.Lines), currentCap, Chunk.Count, sizeof(Integer),MemTracker);
  end;

  chunk.Code[chunk.Count] := value;
  chunk.Lines[chunk.count] := Line;
  Inc(chunk.Count);
  
  // ---- Exit assertions ----
  AssertChunkIsAssigned(chunk);
  AssertChunkCodeIsAssigned(chunk);
  Assert(chunk.Count > 0, 'writeChunk exit: chunk count should be > 0');
  Assert(chunk.Code[chunk.Count - 1] = value, 'writeChunk exit: value not written correctly');
end;


procedure initValueArray(var ValueArray : pValueArray; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsNilBeforeInit(ValueArray);

  allocate(pointer(ValueArray),0,Sizeof(TValueArray),MemTracker);

  ValueArray.Count := 0;

  ValueArray.CurrentCapacity := 0;

  ValueArray.Values := nil;

  // ---- Exit assertions ----
  AssertValueArrayIsAssigned(ValueArray);
  Assert(ValueArray.Count = 0, 'initValueArray exit: count should be 0');
  Assert(ValueArray.CurrentCapacity = 0, 'initValueArray exit: capacity should be 0');
  Assert(ValueArray.Values = nil, 'initValueArray exit: values should be nil');
end;

procedure writeValueArray(ValueArray : pValueArray; Value : TValue; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsAssigned(ValueArray);
  AssertValueArrayCount(ValueArray);

  AllocateArray(pointer(ValueArray.Values), ValueArray.CurrentCapacity, ValueArray.Count,sizeof(TValue),MemTracker);

  ValueArray.Values[ValueArray.Count] := value;

  Inc(ValueArray.Count);

  // ---- Exit assertions ----
  AssertValueArrayIsAssigned(ValueArray);
  AssertValuesIsAssigned(ValueArray.Values);
  Assert(ValueArray.Count > 0, 'writeValueArray exit: count should be > 0');
end;

procedure FreeValues(var Values : pValue; Capacity : integer; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValuesIsAssigned(Values);
  AssertCapacityIsPositive(Capacity);
  Allocate(pointer(Values), Capacity * Sizeof(TValue),0,MemTracker);  //Note here that the references to objects will be free'd externally
  AssertPointerIsNil(Values, 'FreeValues - values not nil after free');
end;

procedure freeValueArray(var ValueArray : pValueArray; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsAssigned(ValueArray);
  if ValueArray.Values <> nil then
    FreeValues(ValueArray.Values,ValueArray.CurrentCapacity,MemTracker);
  Allocate(pointer(ValueArray),Sizeof(TValueArray),0,MemTracker);
  ValueArray := nil;

  // ---- Exit assertions ----
  AssertPointerIsNil(ValueArray, 'freeValueArray exit: ValueArray should be nil');
end;

procedure InitStack(var Stack : pStack;MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsNilBeforeInit(Stack);
  Allocate(pointer(Stack),0, Sizeof(TStack),Memtracker);
  Stack.Count := 0;
  Stack.CurrentCapacity := 0;
  Stack.Values := nil;
  AllocateArray(pointer(Stack.Values),Stack.CurrentCapacity,Stack.Count,Sizeof(TValue),Memtracker);
  Stack.StackTop := Stack.Values;
  
  // ---- Exit assertions ----
  AssertStackIsAssigned(Stack);
  Assert(Stack.Count = 0, 'InitStack exit: count should be 0');
  Assert(Stack.CurrentCapacity > 0, 'InitStack exit: capacity should be > 0');
  AssertStackValuesIsAssigned(Stack);
  Assert(Stack.StackTop = Stack.Values, 'InitStack exit: StackTop should equal Values');
end;

procedure FreeStack(var Stack : pStack;MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  if (stack.CurrentCapacity > 0) then
  begin
    Allocate(pointer(Stack.Values), stack.CurrentCapacity * sizeof(TValue), 0,Memtracker);
    Stack.Values := nil;
  end;

  Allocate(pointer(Stack),sizeof(TStack),0,Memtracker);
  Stack := nil;
  
  // ---- Exit assertions ----
  AssertPointerIsNil(Stack, 'FreeStack exit: Stack should be nil');
end;

procedure pushStack(var stack : pStack;const value : TValue;MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  if AllocateArray(pointer(Stack.Values),Stack.CurrentCapacity,Stack.Count,Sizeof(TValue),Memtracker) then
  begin
    ResetStack(stack);
    Stack.StackTop := Stack.Values + Stack.Count;  //move stack top to next pointer available (at count)
  end;
  Stack.StackTop^ := Value;
  Inc(Stack.StackTop);
  inc(Stack.Count);
  
  // ---- Exit assertions ----
  AssertStackIsAssigned(Stack);
  Assert(Stack.Count > 0, 'pushStack exit: count should be > 0');
  AssertStackTopIsNotNil(Stack);
end;


//End memory creation routines






function isObject(value : TValue) : boolean;
begin
  result := value.valueKind = vkObject;
end;

function GetObject(const value : TValue) : pObj; inline;
begin
  AssertValueIsObject(value);
  result := value.ObjValue;
end;

function CreateObject(value : pObj) : TValue;
begin
  AssertPointerIsNotNil(value, 'CreateObject - object value');
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
  AssertObjStringIsAssigned(str);
  AssertIndexIsNotNegative(index);
  AssertIndexInRange(index, str.length);

  ptr := str.chars;
  inc(ptr,index);
  result := ptr^;
end;



procedure AddToCreatedObjects(p : pObj; MemTracker : pMemTracker);
begin
  AssertPointerIsNotNil(p, 'object');
  AssertMemTrackerIsNotNil(MemTracker);
  // Assert(Memtracker.CreatedObjects <> nil, 'Mem tracker created objects is nil'); this can be nil

  p^.Next := Memtracker.CreatedObjects;
  Memtracker.CreatedObjects := p;
  
  // ---- Exit assertions ----
  Assert(MemTracker.CreatedObjects = p, 'AddToCreatedObjects exit: object should be at head of list');
end;

//Chars: array[0..0] of AnsiChar;
function AddString(const a, b : AnsiString; MemTracker : pMemTracker) : PObjString;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  result := CreateString(a+b,Memtracker);
end;

function ObjStringSize(const p : pObjString) : integer;
begin
  AssertPointerIsNotNil(p, 'ObjString');
  result := Sizeof(TObjString) + Max(0, p^.length - 1);  // avoid negative size
end;

function ValueToString(const value : TValue) : pObjString;
begin
  AssertValueIsString(value);
  AssertPointerIsNotNil(value.ObjValue, 'string value');
  result := pObjString(value.ObjValue);
end;

function StringToValue(const value : pObjString) : TValue;
begin
  AssertObjStringIsAssigned(value);
  AssertObjectKindIsString(@value.Obj);
  result.ValueKind := vkObject;
  result.ObjValue := pObj(value);
end;

procedure Concatenate(Stack : pStack; MemTracker : pMemTracker);
var
  top, below, resultStr: PObjString;
  strTop, strBelow: AnsiString;
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsAssigned(Stack);
  Assert(IsString(peekStack(stack)),'Value at top of stack to concatenate is not a string');
  Assert(IsString(peekStack(stack,1)),'Value at position -1 of stack to concatenate is not a string');

  oldCount := Stack.Count;
  
  top := ValueToString(popStack(stack));       // top of stack ("B")
  below := ValueToString(popStack(stack));     // below top ("A")

  strTop := ObjStringToAnsiString(top);
  strBelow := ObjStringToAnsiString(below);

  resultStr := AddString(strBelow, strTop, Memtracker);   // "A" + "B"

  PushStack(stack, StringToValue(resultStr),Memtracker);
  
  // ---- Exit assertions ----
  Assert(Stack.Count = oldCount - 1, 'Concatenate exit: should pop 2, push 1 (net -1)');
  Assert(isString(peekStack(stack)), 'Concatenate exit: top of stack should be a string');
end;


function CreateBoolean(Value: Boolean): TValue;
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
  AssertValueIsBoolean(Value);
  result := Value.BooleanValue;
end;

function isNumber(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkNumber;
end;

function CreateNumber(Value: Double): TValue;
begin
  Result.ValueKind := vkNumber;
  Result.NumberValue := Value;
end;

function GetNumber(Value : TValue) : double;
begin
  AssertValueIsNumber(Value);
  result := value.NumberValue;
end;

function CreateNilValue : TValue;
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
  AssertValueIsNil(value);
  result := Value.NullValue;
end;

function IsFalsey(const Value: TValue): Boolean; inline;
begin
  Result :=
    (Value.ValueKind = vkNull) or
    ((Value.ValueKind = vkBoolean) and (not Value.BooleanValue));
end;

//Value type assertions (placed here after type checking functions are defined)
procedure AssertValueIsBoolean(const Value : TValue);
begin
  Assert(isBoolean(Value), 'Value is not boolean');
end;

procedure AssertValueIsNumber(const Value : TValue);
begin
  Assert(isNumber(Value), 'Value is not a number');
end;

procedure AssertValueIsNil(const Value : TValue);
begin
  Assert(isNill(Value), 'Value is not a nil value');
end;

procedure AssertValueIsObject(const Value : TValue);
begin
  Assert(isObject(Value), 'Value is not an object');
end;

procedure AssertValueIsString(const Value : TValue);
begin
  Assert(isString(Value), 'Value is not a string value');
end;


function StringsEqual(a, b: PObjString): Boolean;
begin
  AssertObjStringIsAssigned(a);
  AssertObjStringIsAssigned(b);
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
                  AssertPointerIsNotNil(a.ObjValue, 'A value in ValuesEqual');
                  AssertPointerIsNotNil(b.ObjValue, 'B value in ValuesEqual');
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



function AddValueConstant(ValueArray: pValueArray; const value: TValue;Memtracker : pMemTracker): Integer;
var
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsAssigned(ValueArray);

  oldCount := ValueArray.Count;
  
  // Add the value to the ValueArray -- note here the value array can grow
  writeValueArray(ValueArray, value,Memtracker);

  // Return the index of the newly added value
  Result := ValueArray.Count - 1;
  
  // ---- Exit assertions ----
  Assert(ValueArray.Count = oldCount + 1, 'AddValueConstant exit: count should increase by 1');
  Assert(Result >= 0, 'AddValueConstant exit: result index should be >= 0');
end;


procedure AddConstant(chunk : pChunk; const value : TValue; Line : Integer; MemTracker : pMemTracker);
var
  idx : integer;
  IntBytes : TIntToByteResult;
  oldChunkCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertChunkIsAssigned(chunk);
  AssertChunkConstantsIsAssigned(chunk);

  oldChunkCount := chunk.Count;
  
  //add constant, 1st into value's array of the value record
  idx := AddValueConstant(chunk.Constants,value,MemTracker);
  //add constant op code into the chunk array
  AssertIndexIsNotNegative(idx);
  if idx <= high(Byte) then
  begin
    writeChunk(Chunk, OP_CONSTANT,Line,MemTracker);
    WriteChunk(Chunk,idx,Line,memTracker);
  end
  else
  begin
    writeChunk(Chunk, OP_CONSTANT_LONG,Line,MemTracker);
    IntBytes := IntToBytes(idx);     //we can't store an int in a byte array, so we split it now.
    WriteChunk(Chunk, IntBytes.byte0 , Line, MemTracker);
    WriteChunk(Chunk, IntBytes.byte1 , Line, MemTracker);
    WriteChunk(Chunk, IntBytes.byte2 , Line, MemTracker);
  end;
  
  // ---- Exit assertions ----
  Assert(chunk.Count > oldChunkCount, 'AddConstant exit: chunk count should have increased');
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
  AssertValueArrayIsAssigned(ValueArray);
  AssertPointerIsNotNil(strings, 'output strings');

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
  VM.RuntimeErrorStr := msg;
end;


function ReadByte(var code : pByte): Byte; inline;
begin
   AssertCodePointerIsAssigned(Code);
   result := Code^;
   inc(Code);
end;

function ReadConstant(var code : pByte; constants : pValueArray) : TValue; inline;
var idx : Byte;
begin
  idx := ReadByte(code);
  result := Constants.Values[idx];
end;

function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue; inline;
var
  idx : integer;
  Bytes: TIntToByteResult;
begin
  Bytes.byte0 := ReadByte(code);
  Bytes.byte1 := ReadByte(code);
  Bytes.byte2 := ReadByte(code);
  idx := ByteToInt(Bytes);
  result := Constants.Values[idx];
end;

function Run : TInterpretResult;
var
  InstructionPtr : pByte;
  instruction: Byte;
  value,ValueB : TValue;

begin
    AssertVMIsAssigned;
    AssertVMChunkIsAssigned;
    AssertVMChunkCodeIsAssigned;
    AssertChunkHasInstructions(vm.Chunk);
    AssertChunkEndsWithReturn(vm.Chunk);
    InstructionPtr := Vm.Chunk.Code;
    while True do
    begin
      instruction := ReadByte(InstructionPtr);
      case instruction of

        OP_CONSTANT : begin
          value := ReadConstant(InstructionPtr,vm.Chunk.Constants);
          pushStack(vm.Stack,value,vm.MemTracker);
        end;

        OP_CONSTANT_LONG : begin
          value := ReadConstantLong(InstructionPtr,vm.chunk.Constants);
          pushStack(vm.Stack,value,vm.MemTracker);
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

          PushStack(vm.Stack,value,vm.MemTracker);

        end;

        OP_NIL      : pushStack(vm.stack, CreateNilValue,vm.MemTracker);
        OP_TRUE     : pushStack(vm.Stack, CreateBoolean(true),vm.MemTracker);
        OP_FALSE    : pushStack(vm.stack, CreateBoolean(false),vm.MemTracker);

        OP_EQUAL: begin
          Value := popStack(vm.Stack);
          ValueB := popStack(vm.Stack);
          pushStack(vm.stack,CreateBoolean(valuesEqual(Value, ValueB)),vm.MemTracker);
        end;

        OP_GREATER  : begin
                        if not binaryOp(boGreater) then
                        begin
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_LESS     : begin
                        if not binaryOp(boLess) then
                        begin
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_ADD      : begin
                        if isNumber(peekStack(vm.stack,0)) and isNumber(peekStack(vm.stack,1)) then
                        begin
                          if not BinaryOp(boAdd) then
                          begin
                            result.code := INTERPRET_RUNTIME_ERROR;
                            exit;
                          end;
                        end
                        else
                        if isString(peekStack(vm.stack,0)) and isString(peekStack(vm.stack,1)) then
                        begin
                          Concatenate(vm.stack,vm.MemTracker);
                        end
                        else
                        begin
                          runtimeError('Operands must be two numbers or two strings.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_SUBTRACT : begin
                        if not BinaryOp(boSubtract) then
                        begin
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_MULTIPLY : begin
                        if not BinaryOp(boMultiply) then
                        begin
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_DIVIDE   : begin
                        if not BinaryOp(boDivide) then
                        begin
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_NOT      : pushStack(vm.Stack,CreateBoolean(isFalsey(popStack(vm.Stack))),vm.MemTracker);

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




procedure ResetStack(var stack : pStack);
begin
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  Stack.StackTop := Stack.Values;
  
  // ---- Exit assertions ----
  Assert(Stack.StackTop = Stack.Values, 'ResetStack exit: StackTop should equal Values');
end;



function peekStack(Stack: pStack; DistanceFromTop: Integer): TValue;
begin
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  AssertStackIsNotEmpty(Stack);
  AssertDistanceIsNotNegative(DistanceFromTop);
  AssertDistanceInRange(DistanceFromTop, Stack.Count);

  Result := Stack.Values[Stack.Count - 1 - DistanceFromTop];
end;

function peekStack(Stack: pStack): TValue;
begin
  Result := peekStack(Stack, 0);
end;

function popStack(var stack : pStack) : TValue;
var
  oldCount : integer;
begin
   AssertStackIsAssigned(Stack);
   AssertStackTopIsNotNil(Stack);
   AssertStackValuesIsAssigned(Stack);
   AssertStackIsNotEmpty(Stack);
   
   oldCount := Stack.Count;
   
   Dec(Stack.StackTop);
   result := Stack.StackTop^;
   Dec(Stack.Count);
   
   // ---- Exit assertions ----
   Assert(Stack.Count = oldCount - 1, 'popStack exit: count should decrease by 1');
   Assert(Stack.Count >= 0, 'popStack exit: count should not be negative');
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
  oldCount : integer;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  oldCount := vm.stack.Count;
  
  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A + B),vm.MemTracker);
  
  // ---- Exit assertions ----
  Assert(vm.stack.Count = oldCount - 1, 'BinaryOpNumber_Add exit: should pop 2, push 1 (net -1)');
  Assert(isNumber(peekStack(vm.stack)), 'BinaryOpNumber_Add exit: result should be a number');
  
  Result := true;
end;

function BinaryOpNumber_Subtract: boolean;
var
  A, B: Double;
  oldCount : integer;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  oldCount := vm.stack.Count;
  
  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A - B),vm.MemTracker);
  
  // ---- Exit assertions ----
  Assert(vm.stack.Count = oldCount - 1, 'BinaryOpNumber_Subtract exit: should pop 2, push 1 (net -1)');
  Assert(isNumber(peekStack(vm.stack)), 'BinaryOpNumber_Subtract exit: result should be a number');
  
  Result := true;
end;

function BinaryOpNumber_Multiply: boolean;
var
  A, B: Double;
  oldCount : integer;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  oldCount := vm.stack.Count;
  
  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateNumber(A * B),vm.MemTracker);
  
  // ---- Exit assertions ----
  Assert(vm.stack.Count = oldCount - 1, 'BinaryOpNumber_Multiply exit: should pop 2, push 1 (net -1)');
  Assert(isNumber(peekStack(vm.stack)), 'BinaryOpNumber_Multiply exit: result should be a number');
  
  Result := true;
end;

function BinaryOpNumber_Divide: boolean;
var
  A, B: Double;
  oldCount : integer;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  oldCount := vm.stack.Count;
  
  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  if B = 0 then
  begin
    RuntimeError('Division by zero.');
    Exit(false);
  end;

  PushStack(vm.stack,CreateNumber(A / B),vm.MemTracker);
  
  // ---- Exit assertions ----
  Assert(vm.stack.Count = oldCount - 1, 'BinaryOpNumber_Divide exit: should pop 2, push 1 (net -1)');
  Assert(isNumber(peekStack(vm.stack)), 'BinaryOpNumber_Divide exit: result should be a number');
  
  Result := true;
end;

function BinaryOpNumber_Greater: boolean;
var
  A, B: Double;
  oldCount : integer;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  oldCount := vm.stack.Count;
  
  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateBoolean(A > B),vm.MemTracker);
  
  // ---- Exit assertions ----
  Assert(vm.stack.Count = oldCount - 1, 'BinaryOpNumber_Greater exit: should pop 2, push 1 (net -1)');
  Assert(isBoolean(peekStack(vm.stack)), 'BinaryOpNumber_Greater exit: result should be a boolean');
  
  Result := true;
end;

function BinaryOpNumber_Less: boolean;
var
  A, B: Double;
  oldCount : integer;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  oldCount := vm.stack.Count;
  
  B := GetNumber(PopStack(vm.Stack));
  A := GetNumber(PopStack(vm.stack));

  PushStack(vm.stack,CreateBoolean(A < B),vm.MemTracker);
  
  // ---- Exit assertions ----
  Assert(vm.stack.Count = oldCount - 1, 'BinaryOpNumber_Less exit: should pop 2, push 1 (net -1)');
  Assert(isBoolean(peekStack(vm.stack)), 'BinaryOpNumber_Less exit: result should be a boolean');
  
  Result := true;
end;


function BinaryOp(Op: TBinaryOperation): boolean;
begin
  case Op of
    boAdd       : Result := BinaryOpNumber_Add;
    boSubtract  : Result := BinaryOpNumber_subtract;
    boMultiply  : Result := BinaryOpNumber_Multiply;
    boDivide    : Result := BinaryOpNumber_Divide;
    boGreater   : Result := BinaryOpNumber_Greater;
    boLess      : Result := BinaryOpNumber_Less;
  else
    raise Exception.Create('Unknown operator');
  end;
end;


//entry point into vm
function InterpretResult(source : pAnsiChar) : TInterpretResult;
begin
   initVM;
   try
     if not compile(source,Vm.chunk) then
     begin
       Result.code :=  INTERPRET_COMPILE_ERROR;
       Result.ErrorStr := Parser.ErrorStr;
       Exit;
     end;
     Result := Run;
     if Result.code = INTERPRET_RUNTIME_ERROR then
       Result.ErrorStr := VM.RuntimeErrorStr;
   finally
     FreeVM;
   end;
end;

procedure FreeObject(obj : pObj);
begin
  AssertObjectIsAssigned(obj);
  case obj.ObjectKind of
    okString : begin
      FreeString(pObjString(obj),vm.MemTracker);
    end;
  end;
end;

procedure FreeObjects(objects : pObj);
var
  obj : pObj;
  next : pObj;
begin
  AssertPointerIsNotNil(objects, 'FreeObjects - objects');
  obj := Objects;
  while (obj <> nil) do
  begin
    next := obj.Next;
    freeObject(obj);
    obj := next;
  end;
end;


procedure MarkObject(obj : pObj);
begin
   AssertPointerIsNotNil(obj, 'MarkObject - object');
   Obj.IsMarked := true;
end;

procedure MarkValue(value : pValue);
begin
  AssertPointerIsNotNil(Value, 'MarkValue - value');
  if (isObject(value^)) then
  begin
    markObject(GetObject(value^));
  end;
end;

procedure MarkRoots(Stack : pStack);
var
  slot : pValue;
begin
  AssertPointerIsNotNil(Stack, 'MarkRoots - stack');
  if Stack.Count = 0 then exit;
  AssertStackValuesIsAssigned(Stack);
  slot := stack.Values;   //first elemement in stack array
  while slot < Stack.StackTop do
  begin
    markValue(slot);
    Inc(slot);
  end;
end;

procedure CollectGarbage(MemTracker : pMemTracker);
begin
  AssertMemTrackerIsNotNil(MemTracker);
  //Assert(Assigned(MemTracker.Roots.Stack), 'Mem tracker roots is nil');
  if MemTracker.Roots.Stack <> nil then
    MarkRoots(MemTracker.Roots.Stack);
end;

procedure InitMemTracker(var MemTracker : pMemTracker);
begin
  AssertPointerIsNil(MemTracker, 'InitMemTracker - MemTracker not nil before initialization');
  new(MemTracker); //Allocate(pointer(MemTracker),0, SizeOf(MemTracker),MemTracker);
  MemTracker.CreatedObjects := nil;
  MemTracker.Roots.Stack := nil;
  MemTracker.BytesAllocated := 0;
  
  // ---- Exit assertions ----
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(MemTracker.Roots.Stack = nil, 'InitMemTracker exit: Roots.Stack should be nil');
  Assert(MemTracker.BytesAllocated = 0, 'InitMemTracker exit: BytesAllocated should be 0');
end;

procedure FreeMemTracker(var MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  dispose(MemTracker);
  MemTracker := nil;  // Note here we don't dispose of the roots.stack reference.
  
  // ---- Exit assertions ----
  AssertPointerIsNil(MemTracker, 'FreeMemTracker exit: MemTracker should be nil');
end;

procedure InitVM;
begin
  new(VM); //we don't care about making this route through allocate
  VM.Chunk := nil;
  VM.Stack := nil;
  VM.MemTracker := nil;
  VM.RuntimeErrorStr := '';
  InitMemTracker(VM.MemTracker);
  InitChunk(Vm.Chunk,vm.MemTracker);
  InitStack(VM.Stack,Vm.MemTracker);
  ResetStack(vm.Stack);
  //GC set up
  VM.MemTracker.Roots.Stack := Vm.Stack;
  
  // ---- Exit assertions ----
  AssertVMIsAssigned;
  AssertVMChunkIsAssigned;
  Assert(VM.Stack <> nil, 'InitVM exit: Stack should not be nil');
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(VM.MemTracker.Roots.Stack = VM.Stack, 'InitVM exit: GC roots should point to stack');
end;

procedure FreeVM;
begin
  FreeStack(VM.Stack,Vm.MemTracker);
  if (Vm.MemTracker.CreatedObjects <> nil) then
  begin
    FreeObjects(Vm.MemTracker.CreatedObjects);
  end;
  freeChunk(vm.chunk,vm.MemTracker);
  Assert(VM.MemTracker.BytesAllocated = 0, 'VM has not disposed of all mem allocation');
  FreeMemTracker(VM.MemTracker);
  dispose(VM); //and therefore (see above comment in initVM) we just dispose here
end;

procedure InitScanner(source : pAnsiChar);
begin
  scanner.start := source;
  scanner.current := source;
  scanner.line := 1;
  
  // ---- Exit assertions ----
  Assert(scanner.start = source, 'InitScanner exit: start should equal source');
  Assert(scanner.current = source, 'InitScanner exit: current should equal source');
  Assert(scanner.line = 1, 'InitScanner exit: line should be 1');
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
  result := false;

  if isAtEnd then exit;

  if scanner.current^ <> expected then exit;

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
  s, tokenText: string;
begin
  if Parser.PanicMode then
    Exit;

  Parser.PanicMode := True;

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

  s := s + ': ' + string(AnsiString(Msg));

  Parser.ErrorStr := s;
  Parser.HadError := True;
end;



procedure Error(const Msg: pAnsiChar);
begin
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

procedure Consume(TokenKind: TTokenType; const Msg: PAnsiChar);
begin
  if Parser.Current.TokenType = TokenKind then
  begin
    AdvanceParser;
    Exit;
  end;

  ErrorAtCurrent(Msg);
end;

function currentChunk : pChunk;
begin
  result := CompilingChunk;
end;


procedure emitByte(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker );
var
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  oldCount := Chunk.Count;
  writeChunk(Chunk,value,line,MemTracker);
  
  // ---- Exit assertions ----
  Assert(Chunk.Count = oldCount + 1, 'emitByte exit: chunk count should increase by 1');
end;

procedure EmitReturn(Chunk : pChunk; Line : integer; MemTracker : pMemTracker);
var
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  oldCount := Chunk.Count;
  writeChunk(Chunk,OP_RETURN,line,Memtracker);
  
  // ---- Exit assertions ----
  Assert(Chunk.Count = oldCount + 1, 'EmitReturn exit: chunk count should increase by 1');
  AssertChunkEndsWithReturn(Chunk);
end;

procedure emitConstant(value : TValue);
begin
  AddConstant(CurrentChunk,value,parser.previous.line,VM.MemTracker);
end;

function  getRule(tokenType : TTokenType) : TParseRule;
begin
  result := Rules[tokenType];
end;

procedure FatalError;
begin
  Abort;
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
  if not Assigned(prefixRule) then
  begin
    Error('Expect expression.');
    Exit;
  end;

  prefixRule();

  //now do infix
  while (precedence <= (getRule(parser.current.tokenType).precedence)) do
  begin
    advanceParser;
    infixRule := getRule(parser.previous.tokentype).infix;
    if not Assigned(infixRule) then
    begin
      Error('Expect expression.');
      Exit;
    end;

    infixRule();
  end;
end;


procedure Number();
var
  Value  : TValue;
  lexeme : AnsiString;
  fs : TFormatSettings;
begin
  lexeme := TokenToString(parser.previous);
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Value := CreateNumber(StrToFloat(string(lexeme), fs));
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
  parsePrecedence(PREC_UNARY);
  case operatortype of
    TOKEN_MINUS: emitByte(OP_NEGATE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_BANG: emitByte(OP_NOT,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
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
  strObj := CreateString(lexeme,VM.MemTracker);
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
        emitByte(OP_EQUAL,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
        emitByte(OP_NOT,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_EQUAL_EQUAL: begin
        emitByte(OP_EQUAL,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_GREATER:    begin
        emitByte(OP_GREATER,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_GREATER_EQUAL: begin
        emitByte(OP_LESS,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
        emitByte(OP_NOT,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_LESS: begin
        emitByte(OP_LESS,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_LESS_EQUAL: begin
          emitByte(OP_GREATER,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
          emitByte(OP_NOT,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_PLUS : begin
        emitByte(OP_ADD,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_MINUS : begin
        emitByte(OP_SUBTRACT,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_STAR : begin
        emitByte(OP_MULTIPLY,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_SLASH : begin
        emitByte(OP_DIVIDE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
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
    TOKEN_FALSE : emitByte(OP_FALSE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_TRUE  : emitByte(OP_TRUE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_NIL   : emitByte(OP_NIL,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
  end;
end;


function compile(source : pAnsiChar; chunk : pChunk) : boolean;
begin
  AssertChunkIsAssigned(chunk);
  AssertSourceCodeIsAssigned(source);
  initScanner(source);
  compilingChunk := chunk; //the reason this is done is so it is not passed around.
  parser.hadError := false;
  parser.panicMode := false;
  advanceParser();
  Expression();
  consume(TOKEN_EOF, 'Expect end of expression.');
  emitReturn(CurrentChunk,parser.previous.line,VM.Memtracker);
  result := parser.HadError = false;

  // ---- Exit assertions ----
  if result then
  begin
    AssertChunkHasInstructions(chunk);
    AssertChunkEndsWithReturn(chunk);
  end;
end;


procedure InitTable(var Table : pTable; memTracker : pMemTracker);
begin
   AssertMemTrackerIsNotNil(memTracker);
   AssertPointerIsNil(Table, ' Table is not nil');
   Allocate(pointer(Table),0,Sizeof(TTable),MemTracker);
   Table.Count := 0;
   Table.CurrentCapacity := 0;
   Table.Entries  := nil;
end;

procedure FreeEntries(var Entries : pEntry; Capacity : integer; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(Assigned(Entries), 'Entries is not assigned');
  AssertCapacityIsPositive(Capacity);
  Allocate(pointer(Entries), Capacity * Sizeof(TEntry),0,MemTracker);  //Note here that the references to objects will be free'd externally
  AssertPointerIsNil(Entries, 'FreeValues - Entries not nil after free');
end;

function FindEntry(Entries: pEntry; Capacity: integer; Key: pObjString): pEntry;
var
  index: uint32;
begin
  Assert(Assigned(Entries), 'FindEntry: entries not assigned');
  Assert(Capacity > 0, 'FindEntry: capacity must be > 0');
  AssertObjStringIsAssigned(key);

  index := Key.hash mod uint32(Capacity);
  while true do
  begin
    Result := @Entries[index];
    if (Result.key = nil) or (Result.key = key) then
      Exit;
    index := (index + 1) mod uint32(Capacity);
  end;
end;


procedure AdjustCapacity(Table: pTable; NewCapacity: integer; MemTracker: pMemTracker);
var
  NewEntries, OldEntries: pEntry;
  OldCapacity, i: integer;
  dest: pEntry;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  AssertTable(Table);
  Assert(NewCapacity > 0, 'AdjustCapacity: new capacity must be > 0');

  // Allocate fresh array
  NewEntries := nil;
  Allocate(pointer(NewEntries), 0, NewCapacity * SizeOf(TEntry), MemTracker);

  // Initialize all slots to empty
  for i := 0 to NewCapacity - 1 do
  begin
    NewEntries[i].key := nil;
    NewEntries[i].value := CreateNilValue;
  end;

  // Rehash existing entries into new array
  OldEntries := Table.Entries;
  OldCapacity := Table.CurrentCapacity;
  Table.Count := 0;

  if OldEntries <> nil then
  begin
    for i := 0 to OldCapacity - 1 do
    begin
      if OldEntries[i].key = nil then Continue;
      dest := FindEntry(NewEntries, NewCapacity, OldEntries[i].key);
      dest.key := OldEntries[i].key;
      dest.value := OldEntries[i].value;
      Inc(Table.Count);
    end;

    // Free old array
    Allocate(pointer(OldEntries), OldCapacity * SizeOf(TEntry), 0, MemTracker);
  end;

  Table.Entries := NewEntries;
  Table.CurrentCapacity := NewCapacity;
end;


function TableSet(var Table: pTable; key: pObjString; value: TValue; MemTracker: pMemTracker): boolean;
var
  Entry: pEntry;
  NewCapacity: integer;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  AssertTable(Table);
  AssertObjStringIsAssigned(key);

  if (Table.Count + 1 > Table.CurrentCapacity * TABLE_MAX_LOAD) then
  begin
    if Table.CurrentCapacity < 8 then
      NewCapacity := 8
    else
      NewCapacity := Table.CurrentCapacity * GROWTH_FACTOR;
    AdjustCapacity(Table, NewCapacity, MemTracker);
  end;

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  Result := Entry.key = nil;
  if Result then
    Inc(Table.Count);

  Entry.key := key;
  Entry.value := value;
end;


function TableGet(Table: pTable; key: pObjString; var value: TValue): boolean;
var
  Entry: pEntry;
begin
  if Table.Count = 0 then
    Exit(false);

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  if Entry.key = nil then
    Exit(false);

  value := Entry.value;
  Result := true;
end;

procedure FreeTable(var Table : pTable; memTracker : pMemTracker);
begin
   // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertTable(Table);
  if Table.Entries <> nil then
    FreeEntries(Table.Entries,Table.CurrentCapacity,MemTracker);
  Allocate(pointer(Table),Sizeof(TTable),0,MemTracker);
  Table := nil;

  // ---- Exit assertions ----
  AssertPointerIsNil(Table, 'freeValueArray exit: ValueArray should be nil');

end;


function HashString(const Key: PAnsiChar; Length: Integer): UInt32;
const
  FNVOffset = 2166136261;
  FNVPrime = 16777619;
var
  i: Integer;
begin
  {$Q-} // disable overflow checking for this function
  Result := FNVOffset;
  for i := 0 to Length - 1 do
  begin
    Result := Result xor UInt8(Key[i]);
    Result := Result * FNVPrime;
  end;
  {$Q+} // disable overflow checking for this function
end;




initialization
  VM := nil;
  CompilingChunk := nil;

finalization

end.
