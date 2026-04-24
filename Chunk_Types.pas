unit Chunk_Types;
{$POINTERMATH ON}
{$ASSERTIONS ON}
{$DEFINE DEBUG_LOG_GC}
interface

uses
  Classes, dialogs;

const

  START_CAPACITY =  256;
  MAX_SIZE       =  START_CAPACITY * 256;
  GROWTH_FACTOR  =  2;
  GC_HEAP_GROW_FACTOR = 2;

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
  OP_PRINT    = 15;
  OP_POP      = 16;
  OP_DEFINE_GLOBAL = 17;
  OP_GET_GLOBAL = 18;
  OP_SET_GLOBAL = 19;
  OP_GET_LOCAL = 20;
  OP_SET_LOCAL = 21;
  OP_JUMP = 22;
  OP_JUMP_IF_FALSE = 23;
  OP_LOOP = 24;
  OP_CALL = 25;
  OP_CLOSURE = 26;
  OP_GET_UPVALUE = 27;
  OP_SET_UPVALUE = 28;
  OP_CLOSE_UPVALUE = 29;

  OP_STRINGS : array[0..29] of string = (
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
    'OP_CONSTANT_LONG',
    'OP_PRINT',
    'OP_POP',
    'OP_DEFINE_GLOBAL',
    'OP_GET_GLOBAL',
    'OP_SET_GLOBAL',
    'OP_GET_LOCAL',
    'OP_SET_LOCAL',
    'OP_JUMP',
    'OP_JUMP_IF_FALSE',
    'OP_LOOP',
    'OP_CALL',
    'OP_CLOSURE',
    'OP_GET_UPVALUE',
    'OP_SET_UPVALUE',
    'OP_CLOSE_UPVALUE');

  UINT8_COUNT = 256;
  FRAMES_MAX = 64;
  STACK_MAX = FRAMES_MAX * UINT8_COUNT;


type

  //Enums
  TValueKind = (vkNumber, vkBoolean, vkNull, vkObject);
  TObjectKind = (okString, okFunction, okNative, okClosure, okUpvalue);
  TBinaryOperation = (boAdd, boSubtract, boMultiply, boDivide, boGreater, boLess);
  TFunctionType = (TYPE_FUNCTION, TYPE_SCRIPT);
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
  pObjFunction    = ^TObjFunction;
  pObjNative      = ^TObjNative;
  pObjClosure     = ^TObjClosure;
  pObjUpvalue     = ^TObjUpvalue;
  pUpvalueArray   = ^TUpvalueArray;
  pCompiler       = ^TCompiler;
  pStack          = ^TStack;
  pVirtualMachine = ^TVirtualMachine;
  pMemTracker     = ^TMemTracker;
  pLogs           = ^TLogs; //Note : Don't think we need this (https://www.danieleteti.it/loggerpro/) maybe add later? We use a stupidly simple approach for now
  pEntry          = ^TEntry;
  pTable          = ^TTable;
  //pRoots          = ^TRoots;

  //Arrays
  TAnsiCharArray = Array[0..0] of AnsiChar;
  TUpvalueArray  = Array[0..UINT8_COUNT - 1] of pObjUpvalue;


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

  TObjFunction = record
    Obj          : TObj;
    arity        : integer;
    upvalueCount : integer;
    chunk        : pChunk;
    name         : pObjString;
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

  TNativeFn = function(argCount: integer; args: pValue): TValue;

  TObjNative = record
    Obj      : TObj;
    func     : TNativeFn;
  end;

  TObjUpvalue = record
    Obj      : TObj;
    location : pValue;  // points to stack slot when open
    closed   : TValue;  // holds value after closing
    next     : pObjUpvalue; // intrusive list for open upvalues
  end;

  TObjClosure = record
    Obj         : TObj;
    func        : pObjFunction;
    upvalues    : pUpvalueArray;
    upvalueCount: integer;
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
    ResultStr : String;
    OutputStr : String;
  end;

  TMemTracker = record
    CreatedObjects  : pObj;
    BytesAllocated  : integer;
    NextGC          : integer;
    GrayCount       : integer;
    GrayCapacity    : integer;
    GrayStack       : ^pObj;
    Roots           : TRoots;
  end;

  TCallFrame = record
    closure : pObjClosure;
    ip      : pByte;
    slots   : pValue; // points into VM stack
  end;

  //Virtual Machine
  TVirtualMachine = record
    Frames          : array[0..FRAMES_MAX - 1] of TCallFrame;
    FrameCount      : integer;
    Stack           : pStack;
    MemTracker      : PMemTracker;
    Strings         : pTable;
    Globals         : pTable;
    OpenUpvalues    : pObjUpvalue;
    RuntimeErrorStr : String;
    PrintOutput     : String;
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

  TLocal = record
    name       : TToken;
    depth      : integer;
    isCaptured : boolean;
  end;

  TUpvalue = record
    index   : byte;
    isLocal : boolean;
  end;

  TCompiler = record
    enclosing    : pCompiler;
    func         : pObjFunction;
    funcType     : TFunctionType;
    locals       : array[0..UINT8_COUNT - 1] of TLocal;
    localCount   : integer;
    upvalues     : array[0..UINT8_COUNT - 1] of TUpvalue;
    scopeDepth   : integer;
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
function ValueToStr(const value : TValue) : String;
function StringToValue(const value : pObjString) : TValue;
function isString(value : TValue) : boolean;
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
procedure MarkValue(value : TValue);
procedure MarkRoots;
procedure MarkTable(Table : pTable);
procedure MarkArray(ValueArray : pValueArray);
procedure MarkCompilerRoots;
procedure BlackenObject(obj : pObj);
procedure TraceReferences;
procedure TableRemoveWhite(Table : pTable);
procedure Sweep;
procedure CollectGarbage;


//Virtual Machine
procedure InitVM();
function InterpretResult(source : pAnsiChar) : TInterpretResult;
function Run : TInterpretResult;
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
function compile(source : pAnsiChar) : pObjClosure;
procedure declaration();
procedure varDeclaration();
procedure funDeclaration();
procedure returnStatement();
procedure statement();
procedure block();
procedure printStatement();
procedure ifStatement();
procedure whileStatement();
procedure forStatement();
procedure expressionStatement();
procedure Number();
procedure grouping();
procedure unary();
procedure binary();
procedure literal();
procedure ParseString();
procedure variable();
procedure call();
procedure and_();
procedure or_();
procedure beginScope();
procedure endScope();
function newFunction(MemTracker : pMemTracker) : pObjFunction;
function newNative(func : TNativeFn; MemTracker : pMemTracker) : pObjNative;
function newClosure(func : pObjFunction; MemTracker : pMemTracker) : pObjClosure;
function newUpvalue(slot : pValue; MemTracker : pMemTracker) : pObjUpvalue;
procedure defineNative(const name : AnsiString; func : TNativeFn);

//Hash
function HashString(const Key: PAnsiChar; Length: Integer): UInt32;

//Value constructors
function CreateNumber(Value: Double): TValue;
function CreateBoolean(Value: Boolean): TValue;
function CreateNilValue: TValue;
function CreateObject(value : pObj) : TValue;

//Table
procedure InitTable(var Table : pTable; memTracker : pMemTracker);
function TableSet(var Table : pTable; key : pObjString; value : TValue;MemTracker : pMemTracker): boolean;
function TableGet(Table : pTable; key : pObjString; var value : TValue): boolean;
function TableDelete(Table : pTable; key : pObjString): boolean;
function TableFindString(Table : pTable; const chars : PAnsiChar; length : integer; hash : uint32): pObjString;
procedure FreeTable(var Table : pTable; memTracker : pMemTracker);

//prat parsing rule table used in compilation
const
  Rules: array[TTokenType] of TParseRule = (
    { TOKEN_LEFT_PAREN }
    (Prefix: grouping; Infix: call;    Precedence: PREC_CALL),

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
    (Prefix: variable; Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_STRING }
    (Prefix: ParseString; Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_NUMBER }
    (Prefix: number;   Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_AND }
    (Prefix: nil;      Infix: and_;    Precedence: PREC_AND),

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
    (Prefix: nil;      Infix: or_;     Precedence: PREC_OR),

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
  Current : pCompiler;
  //Output : TStrings;
  {$IFDEF DEBUG_LOG_GC}
  GCLogFile : TextFile;
  GCLogAllocations : integer;
  GCLogFrees : integer;
  GCLogMarks : integer;
  GCLogBlackens : integer;
  GCLogCycles : integer;
  {$ENDIF}

implementation

uses
  sysutils, Math, strUtils, typinfo, Windows;

{$IFDEF DEBUG_LOG_GC}
function ObjectKindStr(kind : TObjectKind) : string;
begin
  Result := GetEnumName(TypeInfo(TObjectKind), Ord(kind));
end;

procedure GCLog(const msg : string);
begin
  WriteLn(GCLogFile, msg);
  Flush(GCLogFile);
end;
{$ENDIF}

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
  // No longer applicable: VM uses call frames instead of a single chunk
end;

procedure AssertVMChunkCodeIsAssigned;
begin
  // No longer applicable: VM uses call frames instead of a single chunk
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
    {$IFDEF DEBUG_STRESS_GC}
    CollectGarbage;
    {$ENDIF}
    if MemTracker.BytesAllocated > MemTracker.NextGC then
      CollectGarbage;
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
  hash: UInt32;
begin

  AssertMemTrackerIsNotNil(MemTracker);
  Len := Length(S);
  hash := HashString(PAnsiChar(S), Len);

  // String interning: if VM is active, check intern table first
  if (VM <> nil) and (VM.Strings <> nil) then
  begin
    Result := TableFindString(VM.Strings, PAnsiChar(S), Len, hash);
    if Result <> nil then
      Exit;
  end;

  Result := nil;
  NewSize := SizeOf(TObjString) + Max(0, Len - 1);  // avoid negative size

  AllocateString(Result,0, NewSize,MemTracker);

  Result^.Obj.ObjectKind := okString;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.Length := Len;
  Result^.hash := hash;
  if Len > 0 then
  begin
    Move(PAnsiChar(S)^, Result^.Chars[0], Len);
  end;

  //track creation in the vm list of obj.
  AddToCreatedObjects(PObj(Result),MemTracker);

  // String interning: add to intern table if VM is active
  // Push string onto stack to protect from GC during TableSet.
  if (VM <> nil) and (VM.Strings <> nil) then
  begin
    pushStack(VM.Stack, StringToValue(Result), VM.MemTracker);
    TableSet(VM.Strings, Result, CreateNilValue, MemTracker);
    popStack(VM.Stack);
  end;
  
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
  // Use raw memory — stack must never trigger GC (same pattern as GrayStack)
  GetMem(Stack, SizeOf(TStack));
  Stack.Count := 0;
  Stack.CurrentCapacity := START_CAPACITY;
  GetMem(Stack.Values, START_CAPACITY * SizeOf(TValue));
  FillChar(Stack.Values^, START_CAPACITY * SizeOf(TValue), 0);
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
  // Raw memory — matches InitStack/pushStack (not tracked by BytesAllocated)
  if Stack.Values <> nil then
    FreeMem(Stack.Values);
  Stack.Values := nil;
  FreeMem(Stack);
  Stack := nil;
  
  // ---- Exit assertions ----
  AssertPointerIsNil(Stack, 'FreeStack exit: Stack should be nil');
end;

procedure pushStack(var stack : pStack;const value : TValue;MemTracker : pMemTracker);
var
  NewCapacity : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  // Grow using raw ReallocMem — must never trigger GC so push/pop
  // can safely protect unrooted objects (same pattern as book's fixed stack)
  if Stack.Count >= Stack.CurrentCapacity then
  begin
    NewCapacity := Stack.CurrentCapacity * GROWTH_FACTOR;
    ReallocMem(Stack.Values, NewCapacity * SizeOf(TValue));
    // Zero new portion
    FillChar(Stack.Values[Stack.CurrentCapacity],
      (NewCapacity - Stack.CurrentCapacity) * SizeOf(TValue), 0);
    Stack.CurrentCapacity := NewCapacity;
    // Rebase StackTop after realloc (pointer may have moved)
    Stack.StackTop := Stack.Values + Stack.Count;
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

  // ---- Exit assertions ----
  Assert(Result.ValueKind = vkObject, 'CreateObject exit: ValueKind should be vkObject');
end;


function isString(value : TValue) : boolean;
begin
  result := isObject(Value) and (value.ObjValue <> nil) and (value.ObjValue.ObjectKind = okString);
end;

function isFunction(value : TValue) : boolean;
begin
  result := isObject(Value) and (value.ObjValue <> nil) and (value.ObjValue.ObjectKind = okFunction);
end;

function isNative(value : TValue) : boolean;
begin
  result := isObject(Value) and (value.ObjValue <> nil) and (value.ObjValue.ObjectKind = okNative);
end;

function isClosure(value : TValue) : boolean;
begin
  result := isObject(Value) and (value.ObjValue <> nil) and (value.ObjValue.ObjectKind = okClosure);
end;

function newFunction(MemTracker : pMemTracker) : pObjFunction;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjFunction), MemTracker);
  Result^.Obj.ObjectKind := okFunction;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.arity := 0;
  Result^.upvalueCount := 0;
  Result^.name := nil;
  Result^.chunk := nil;
  InitChunk(Result^.chunk, MemTracker);
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
  Assert(Result <> nil, 'newFunction exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okFunction, 'newFunction exit: ObjectKind should be okFunction');
  AssertChunkIsAssigned(Result^.chunk);
  Assert(Result^.arity = 0, 'newFunction exit: arity should be 0');
end;

function newNative(func : TNativeFn; MemTracker : pMemTracker) : pObjNative;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(Assigned(func), 'newNative: func is not assigned');
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjNative), MemTracker);
  Result^.Obj.ObjectKind := okNative;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.func := func;
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
  Assert(Result <> nil, 'newNative exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okNative, 'newNative exit: ObjectKind should be okNative');
end;

function newClosure(func : pObjFunction; MemTracker : pMemTracker) : pObjClosure;
var
  upvals : pUpvalueArray;
  i : integer;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(func <> nil, 'newClosure: func is nil');
  // Allocate the upvalue pointer array
  upvals := nil;
  if func^.upvalueCount > 0 then
  begin
    Allocate(Pointer(upvals), 0, func^.upvalueCount * SizeOf(pObjUpvalue), MemTracker);
    for i := 0 to func^.upvalueCount - 1 do
      upvals^[i] := nil;
  end;

  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjClosure), MemTracker);
  Result^.Obj.ObjectKind := okClosure;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.func := func;
  Result^.upvalues := upvals;
  Result^.upvalueCount := func^.upvalueCount;
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
  Assert(Result <> nil, 'newClosure exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okClosure, 'newClosure exit: ObjectKind should be okClosure');
  Assert(Result^.func = func, 'newClosure exit: func does not match input');
end;

function newUpvalue(slot : pValue; MemTracker : pMemTracker) : pObjUpvalue;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(slot <> nil, 'newUpvalue: slot is nil');
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjUpvalue), MemTracker);
  Result^.Obj.ObjectKind := okUpvalue;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.location := slot;
  Result^.closed := CreateNilValue;
  Result^.next := nil;
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
  Assert(Result <> nil, 'newUpvalue exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okUpvalue, 'newUpvalue exit: ObjectKind should be okUpvalue');
  Assert(Result^.location = slot, 'newUpvalue exit: location does not match input slot');
end;

function ObjStringToAnsiString(S: PObjString): AnsiString;
begin
  AssertObjStringIsAssigned(S);
  AssertStringLengthIsNotNegative(S);
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

  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('allocate %p kind=%s', [Pointer(p), ObjectKindStr(p^.ObjectKind)]));
  Inc(GCLogAllocations);
  {$ENDIF}
  
  // ---- Exit assertions ----
  Assert(MemTracker.CreatedObjects = p, 'AddToCreatedObjects exit: object should be at head of list');
end;

//Chars: array[0..0] of AnsiChar;
function AddString(const a, b : AnsiString; MemTracker : pMemTracker) : PObjString;
begin
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  result := CreateString(a+b,Memtracker);

  // ---- Exit assertions ----
  AssertObjStringIsAssigned(Result);
  Assert(Result^.Obj.ObjectKind = okString, 'AddString exit: result is not a string object');
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

function ValueToStr(const value : TValue) : String;
begin
  case value.ValueKind of
    vkNumber:  Result := FloatToStr(value.NumberValue);
    vkBoolean: if value.BooleanValue then Result := 'true' else Result := 'false';
    vkNull:    Result := 'nil';
    vkObject:  begin
      Assert(value.ObjValue <> nil, 'ValueToStr: ObjValue is nil for object value');
      case value.ObjValue.ObjectKind of
        okString: Result := String(ObjStringToAnsiString(pObjString(value.ObjValue)));
        okFunction: begin
          if pObjFunction(value.ObjValue)^.name = nil then
            Result := '<script>'
          else
            Result := '<fn ' + String(ObjStringToAnsiString(pObjFunction(value.ObjValue)^.name)) + '>';
        end;
        okNative: Result := '<native fn>';
        okClosure: begin
          if pObjClosure(value.ObjValue)^.func^.name = nil then
            Result := '<script>'
          else
            Result := '<fn ' + String(ObjStringToAnsiString(pObjClosure(value.ObjValue)^.func^.name)) + '>';
        end;
        okUpvalue: Result := 'upvalue';
      else
        Result := '<object>';
      end;
    end;
  else
    Result := '<unknown>';
  end;
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

  // Peek instead of pop — keep on stack to protect from GC during allocation
  top := ValueToString(peekStack(stack, 0));
  below := ValueToString(peekStack(stack, 1));

  strTop := ObjStringToAnsiString(top);
  strBelow := ObjStringToAnsiString(below);

  resultStr := AddString(strBelow, strTop, Memtracker);   // "A" + "B"

  // Now safe to pop — result is allocated
  popStack(stack);
  popStack(stack);
  PushStack(stack, StringToValue(resultStr),Memtracker);
  
  // ---- Exit assertions ----
  Assert(Stack.Count = oldCount - 1, 'Concatenate exit: should pop 2, push 1 (net -1)');
  Assert(isString(peekStack(stack)), 'Concatenate exit: top of stack should be a string');
end;


function CreateBoolean(Value: Boolean): TValue;
begin
  Result.ValueKind := vkBoolean;
  Result.BooleanValue := Value;

  // ---- Exit assertions ----
  Assert(Result.ValueKind = vkBoolean, 'CreateBoolean exit: ValueKind should be vkBoolean');
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

  // ---- Exit assertions ----
  Assert(Result.ValueKind = vkNumber, 'CreateNumber exit: ValueKind should be vkNumber');
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

  // ---- Exit assertions ----
  Assert(Result.ValueKind = vkNull, 'CreateNilValue exit: ValueKind should be vkNull');
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

  // Push value onto stack to protect from GC during potential array growth.
  pushStack(VM.Stack, value, VM.MemTracker);
  
  // Add the value to the ValueArray -- note here the value array can grow
  writeValueArray(ValueArray, value,Memtracker);

  popStack(VM.Stack);

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
  AssertVMIsAssigned;
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
  AssertValueArrayIsAssigned(constants);
  AssertValuesIsAssigned(constants.Values);
  idx := ReadByte(code);
  AssertIndexInRange(idx, constants.Count);
  result := Constants.Values[idx];
end;

function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue; inline;
var
  idx : integer;
  Bytes: TIntToByteResult;
begin
  AssertValueArrayIsAssigned(constants);
  AssertValuesIsAssigned(constants.Values);
  Bytes.byte0 := ReadByte(code);
  Bytes.byte1 := ReadByte(code);
  Bytes.byte2 := ReadByte(code);
  idx := ByteToInt(Bytes);
  AssertIndexIsNotNegative(idx);
  AssertIndexInRange(idx, constants.Count);
  result := Constants.Values[idx];
end;

function captureUpvalue(local : pValue) : pObjUpvalue;
var
  prevUpvalue : pObjUpvalue;
  upvalue : pObjUpvalue;
  createdUpvalue : pObjUpvalue;
begin
  // ---- Entry assertions ----
  Assert(local <> nil, 'captureUpvalue: local is nil');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);

  prevUpvalue := nil;
  upvalue := VM.OpenUpvalues;
  while (upvalue <> nil) and (NativeUInt(upvalue^.location) > NativeUInt(local)) do
  begin
    prevUpvalue := upvalue;
    upvalue := upvalue^.next;
  end;

  if (upvalue <> nil) and (upvalue^.location = local) then
    Exit(upvalue);

  createdUpvalue := newUpvalue(local, VM.MemTracker);
  createdUpvalue^.next := upvalue;

  if prevUpvalue = nil then
    VM.OpenUpvalues := createdUpvalue
  else
    prevUpvalue^.next := createdUpvalue;

  // ---- Exit assertions ----
  Assert(createdUpvalue <> nil, 'captureUpvalue exit: result is nil');
  Assert(createdUpvalue^.location = local, 'captureUpvalue exit: location does not match local');
  Result := createdUpvalue;
end;

procedure closeUpvalues(last : pValue);
var
  upvalue : pObjUpvalue;
begin
  // ---- Entry assertions ----
  Assert(last <> nil, 'closeUpvalues: last is nil');
  AssertVMIsAssigned;

  while (VM.OpenUpvalues <> nil) and
        (NativeUInt(VM.OpenUpvalues^.location) >= NativeUInt(last)) do
  begin
    upvalue := VM.OpenUpvalues;
    upvalue^.closed := upvalue^.location^;
    upvalue^.location := @upvalue^.closed;
    VM.OpenUpvalues := upvalue^.next;
  end;
end;

function Run : TInterpretResult;
var
  frame : ^TCallFrame;
  instruction: Byte;
  slot : Byte;
  offset : Word;
  value,ValueB : TValue;
  argCount : byte;
  func : pObjFunction;
  closure : pObjClosure;
  native : TNativeFn;
  nativeResult : TValue;
  isLocal : byte;
  index : byte;
  i : integer;

  function ReadByteFr: Byte;
  begin
    Result := frame^.ip^;
    Inc(frame^.ip);
  end;

  function ReadConstantFr: TValue;
  begin
    Result := frame^.closure^.func^.chunk^.Constants^.Values[ReadByteFr];
  end;

  function CallValue(callee : TValue; argCnt : byte) : boolean;
  begin
    if isClosure(callee) then
    begin
      if argCnt <> pObjClosure(callee.ObjValue)^.func^.arity then
      begin
        runtimeError('Expected ' + IntToStr(pObjClosure(callee.ObjValue)^.func^.arity) +
          ' arguments but got ' + IntToStr(argCnt) + '.');
        Exit(false);
      end;
      if VM.FrameCount = FRAMES_MAX then
      begin
        runtimeError('Stack overflow.');
        Exit(false);
      end;
      frame := @VM.Frames[VM.FrameCount];
      frame^.closure := pObjClosure(callee.ObjValue);
      frame^.ip := pObjClosure(callee.ObjValue)^.func^.chunk^.Code;
      // slots points to the function value on the stack (argCnt args above it)
      frame^.slots := VM.Stack.StackTop;
      Dec(frame^.slots, argCnt + 1);
      Inc(VM.FrameCount);
      Result := true;
    end
    else if isNative(callee) then
    begin
      native := pObjNative(callee.ObjValue)^.func;
      nativeResult := native(argCnt, pValue(NativeUInt(VM.Stack.StackTop) - NativeUInt(argCnt) * SizeOf(TValue)));
      // Check if native signalled a runtime error
      if VM.RuntimeErrorStr <> '' then
      begin
        Result := false;
        Exit;
      end;
      // pop args + callee
      VM.Stack.StackTop := pValue(NativeUInt(VM.Stack.StackTop) - NativeUInt(argCnt + 1) * SizeOf(TValue));
      VM.Stack.Count := VM.Stack.Count - (argCnt + 1);
      pushStack(VM.Stack, nativeResult, VM.MemTracker);
      Result := true;
    end
    else
    begin
      runtimeError('Can only call functions and classes.');
      Result := false;
    end;
  end;

begin
    AssertVMIsAssigned;
    AssertStackIsAssigned(vm.Stack);
    AssertMemTrackerIsNotNil(vm.MemTracker);
    Assert(VM.FrameCount > 0, 'Run: no call frames');

    frame := @VM.Frames[VM.FrameCount - 1];
    Assert(frame^.closure <> nil, 'Run: initial frame closure is nil');
    Assert(frame^.ip <> nil, 'Run: initial frame ip is nil');

    while True do
    begin
      instruction := ReadByteFr;
      case instruction of

        OP_CONSTANT : begin
          value := ReadConstantFr;
          pushStack(vm.Stack,value,vm.MemTracker);
        end;

        OP_CONSTANT_LONG : begin
          // 3-byte constant index (little-endian: byte0 | byte1<<8 | byte2<<16)
          i := ReadByteFr;
          i := i or (ReadByteFr shl 8);
          i := i or (ReadByteFr shl 16);
          value := frame^.closure^.func^.chunk^.Constants^.Values[i];
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

        OP_PRINT: begin
          value := popStack(vm.Stack);
          if VM.PrintOutput <> '' then
            VM.PrintOutput := VM.PrintOutput + sLineBreak;
          VM.PrintOutput := VM.PrintOutput + ValueToStr(value);
        end;

        OP_POP: begin
          popStack(vm.Stack);
        end;

        OP_GET_GLOBAL: begin
          value := ReadConstantFr;
          AssertValueIsString(value);
          if not TableGet(vm.Globals, ValueToString(value), ValueB) then
          begin
            runtimeError('Undefined variable ''' + String(ObjStringToAnsiString(ValueToString(value))) + '''.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          pushStack(vm.Stack, ValueB, vm.MemTracker);
        end;

        OP_SET_GLOBAL: begin
          value := ReadConstantFr;
          AssertValueIsString(value);
          if TableSet(vm.Globals, ValueToString(value), peekStack(vm.Stack), vm.MemTracker) then
          begin
            TableDelete(vm.Globals, ValueToString(value));
            runtimeError('Undefined variable ''' + String(ObjStringToAnsiString(ValueToString(value))) + '''.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_GET_LOCAL: begin
          slot := ReadByteFr;
          pushStack(vm.Stack, frame^.slots[slot], vm.MemTracker);
        end;

        OP_SET_LOCAL: begin
          slot := ReadByteFr;
          frame^.slots[slot] := peekStack(vm.Stack);
        end;

        OP_JUMP: begin
          offset := ReadByteFr shl 8;
          offset := offset or ReadByteFr;
          Inc(frame^.ip, offset);
        end;

        OP_JUMP_IF_FALSE: begin
          offset := ReadByteFr shl 8;
          offset := offset or ReadByteFr;
          if isFalsey(peekStack(vm.Stack)) then
            Inc(frame^.ip, offset);
        end;

        OP_LOOP: begin
          offset := ReadByteFr shl 8;
          offset := offset or ReadByteFr;
          Dec(frame^.ip, offset);
        end;

        OP_DEFINE_GLOBAL: begin
          value := ReadConstantFr;
          AssertValueIsString(value);
          TableSet(vm.Globals, ValueToString(value), peekStack(vm.Stack), vm.MemTracker);
          popStack(vm.Stack);
        end;

        OP_CALL: begin
          argCount := ReadByteFr;
          if not CallValue(peekStack(vm.Stack, argCount), argCount) then
          begin
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          frame := @VM.Frames[VM.FrameCount - 1];
        end;

        OP_CLOSURE: begin
          func := pObjFunction(ReadConstantFr.ObjValue);
          closure := newClosure(func, VM.MemTracker);
          pushStack(vm.Stack, CreateObject(pObj(closure)), vm.MemTracker);
          for i := 0 to closure^.upvalueCount - 1 do
          begin
            isLocal := ReadByteFr;
            index := ReadByteFr;
            if isLocal = 1 then
              closure^.upvalues^[i] := captureUpvalue(
                pValue(NativeUInt(frame^.slots) + NativeUInt(index) * SizeOf(TValue)))
            else
              closure^.upvalues^[i] := frame^.closure^.upvalues^[index];
          end;
        end;

        OP_GET_UPVALUE: begin
          slot := ReadByteFr;
          pushStack(vm.Stack, frame^.closure^.upvalues^[slot]^.location^, vm.MemTracker);
        end;

        OP_SET_UPVALUE: begin
          slot := ReadByteFr;
          frame^.closure^.upvalues^[slot]^.location^ := peekStack(vm.Stack);
        end;

        OP_CLOSE_UPVALUE: begin
          closeUpvalues(pValue(NativeUInt(VM.Stack.StackTop) - SizeOf(TValue)));
          popStack(vm.Stack);
        end;

        OP_RETURN: begin
          value := popStack(vm.Stack);
          closeUpvalues(frame^.slots);

          Dec(VM.FrameCount);
          if VM.FrameCount = 0 then
          begin
            popStack(vm.Stack); // pop the script function
            Result.Code := INTERPRET_OK;
            Result.value := value;
            Exit(Result);
          end;

          // Discard the returning function's stack window
          VM.Stack.StackTop := frame^.slots;
          VM.Stack.Count := (NativeUInt(VM.Stack.StackTop) - NativeUInt(VM.Stack.Values)) div SizeOf(TValue);
          pushStack(vm.Stack, value, vm.MemTracker);

          frame := @VM.Frames[VM.FrameCount - 1];
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
var
  closure : pObjClosure;
begin
   AssertSourceCodeIsAssigned(source);
   initVM;
   try
     closure := compile(source);
     if closure = nil then
     begin
       Result.code :=  INTERPRET_COMPILE_ERROR;
       Result.ErrorStr := Parser.ErrorStr;
       Exit;
     end;

     // Push the script closure onto the stack
     pushStack(VM.Stack, CreateObject(pObj(closure)), VM.MemTracker);

     // Set up the first call frame
     VM.Frames[0].closure := closure;
     VM.Frames[0].ip := closure^.func^.chunk^.Code;
     VM.Frames[0].slots := VM.Stack.Values;
     VM.FrameCount := 1;

     Result := Run;
     if Result.code = INTERPRET_RUNTIME_ERROR then
       Result.ErrorStr := VM.RuntimeErrorStr
     else if (Result.code = INTERPRET_OK) and isString(Result.value) then
       Result.ResultStr := ObjStringToAnsiString(ValueToString(Result.value));
     Result.OutputStr := VM.PrintOutput;
   finally
     FreeVM;
   end;
end;

procedure FreeObject(obj : pObj);
begin
  AssertObjectIsAssigned(obj);
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('free %p kind=%s', [Pointer(obj), ObjectKindStr(obj^.ObjectKind)]));
  Inc(GCLogFrees);
  {$ENDIF}
  case obj.ObjectKind of
    okString : begin
      FreeString(pObjString(obj),vm.MemTracker);
    end;
    okFunction : begin
      Assert(pObjFunction(obj)^.chunk <> nil, 'FreeObject: function chunk is nil');
      freeChunk(pObjFunction(obj)^.chunk, vm.MemTracker);
      Allocate(Pointer(obj), SizeOf(TObjFunction), 0, vm.MemTracker);
    end;
    okNative : begin
      Allocate(Pointer(obj), SizeOf(TObjNative), 0, vm.MemTracker);
    end;
    okClosure : begin
      if pObjClosure(obj)^.upvalues <> nil then
        Allocate(Pointer(pObjClosure(obj)^.upvalues),
                 pObjClosure(obj)^.upvalueCount * SizeOf(pObjUpvalue), 0, vm.MemTracker);
      Allocate(Pointer(obj), SizeOf(TObjClosure), 0, vm.MemTracker);
    end;
    okUpvalue : begin
      Allocate(Pointer(obj), SizeOf(TObjUpvalue), 0, vm.MemTracker);
    end;
  end;
end;

procedure FreeObjects(objects : pObj);
var
  obj : pObj;
  next : pObj;
begin
  AssertPointerIsNotNil(objects, 'FreeObjects - objects');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  obj := Objects;
  while (obj <> nil) do
  begin
    next := obj.Next;
    freeObject(obj);
    obj := next;
  end;
end;


procedure MarkObject(obj : pObj);
var
  newCapacity : integer;
  oldGrayCount : integer;
begin
  // ---- Entry assertions ----
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);

  if obj = nil then Exit;
  if obj^.IsMarked then Exit;

  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('mark %p kind=%s', [Pointer(obj), ObjectKindStr(obj^.ObjectKind)]));
  Inc(GCLogMarks);
  {$ENDIF}

  obj^.IsMarked := true;

  // Add to gray stack
  if VM.MemTracker.GrayCount + 1 > VM.MemTracker.GrayCapacity then
  begin
    if VM.MemTracker.GrayCapacity < 8 then
      newCapacity := 8
    else
      newCapacity := VM.MemTracker.GrayCapacity * 2;
    VM.MemTracker.GrayCapacity := newCapacity;
    // Use raw ReallocMem to avoid recursive GC trigger
    ReallocMem(VM.MemTracker.GrayStack, SizeOf(pObj) * VM.MemTracker.GrayCapacity);
    // ---- Mid assertions ----
    Assert(VM.MemTracker.GrayStack <> nil, 'MarkObject: GrayStack is nil after realloc');
    Assert(VM.MemTracker.GrayCapacity > 0, 'MarkObject: GrayCapacity should be > 0 after growth');
  end;

  oldGrayCount := VM.MemTracker.GrayCount;
  VM.MemTracker.GrayStack[VM.MemTracker.GrayCount] := obj;
  Inc(VM.MemTracker.GrayCount);

  // ---- Exit assertions ----
  Assert(obj^.IsMarked, 'MarkObject exit: obj should be marked');
  Assert(VM.MemTracker.GrayCount = oldGrayCount + 1, 'MarkObject exit: GrayCount should have increased by 1');
end;

procedure MarkValue(value : TValue);
begin
  if isObject(value) then
    MarkObject(GetObject(value));
end;

procedure MarkArray(ValueArray : pValueArray);
var
  i : integer;
begin
  if ValueArray = nil then Exit;
  Assert(ValueArray^.Count >= 0, 'MarkArray: Count is negative');
  for i := 0 to ValueArray^.Count - 1 do
    MarkValue(ValueArray^.Values[i]);
end;

procedure MarkTable(Table : pTable);
var
  i : integer;
begin
  if Table = nil then Exit;
  if Table^.Entries = nil then Exit;
  for i := 0 to Table^.CurrentCapacity - 1 do
  begin
    if Table^.Entries[i].key <> nil then
    begin
      MarkObject(pObj(Table^.Entries[i].key));
      MarkValue(Table^.Entries[i].value);
    end;
  end;
end;

procedure MarkCompilerRoots;
var
  compiler : pCompiler;
begin
  compiler := Current;
  while compiler <> nil do
  begin
    MarkObject(pObj(compiler^.func));
    compiler := compiler^.enclosing;
  end;
end;

procedure MarkRoots;
var
  slot : pValue;
  i : integer;
  upvalue : pObjUpvalue;
begin
  // ---- Entry assertions ----
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);

  // Mark the value stack
  if (VM.Stack <> nil) and (VM.Stack.Count > 0) then
  begin
    slot := VM.Stack.Values;
    while NativeUInt(slot) < NativeUInt(VM.Stack.StackTop) do
    begin
      MarkValue(slot^);
      Inc(slot);
    end;
  end;

  // Mark call frame closures
  // ---- Mid assertions ----
  Assert(VM.FrameCount >= 0, 'MarkRoots: FrameCount is negative');
  Assert(VM.FrameCount <= FRAMES_MAX, 'MarkRoots: FrameCount exceeds FRAMES_MAX');
  for i := 0 to VM.FrameCount - 1 do
    MarkObject(pObj(VM.Frames[i].closure));

  // Mark open upvalues
  upvalue := VM.OpenUpvalues;
  while upvalue <> nil do
  begin
    MarkObject(pObj(upvalue));
    upvalue := upvalue^.next;
  end;

  // Mark globals table
  MarkTable(VM.Globals);

  // Mark compiler roots
  MarkCompilerRoots;

  // ---- Exit assertions ----
  Assert(VM.MemTracker.GrayCount >= 0, 'MarkRoots exit: GrayCount is negative');
end;

procedure BlackenObject(obj : pObj);
var
  i : integer;
begin
  // ---- Entry assertions ----
  Assert(obj <> nil, 'BlackenObject: obj is nil');
  Assert(obj^.IsMarked, 'BlackenObject: obj is not marked (should be gray)');

  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('blacken %p kind=%s', [Pointer(obj), ObjectKindStr(obj^.ObjectKind)]));
  Inc(GCLogBlackens);
  {$ENDIF}

  case obj^.ObjectKind of
    okUpvalue:
      MarkValue(pObjUpvalue(obj)^.closed);
    okFunction: begin
      Assert(pObjFunction(obj)^.chunk <> nil, 'BlackenObject: function chunk is nil');
      MarkObject(pObj(pObjFunction(obj)^.name));
      MarkArray(pObjFunction(obj)^.chunk^.Constants);
    end;
    okClosure: begin
      Assert(pObjClosure(obj)^.func <> nil, 'BlackenObject: closure func is nil');
      if pObjClosure(obj)^.upvalueCount > 0 then
        Assert(pObjClosure(obj)^.upvalues <> nil, 'BlackenObject: closure upvalues is nil with count > 0');
      MarkObject(pObj(pObjClosure(obj)^.func));
      for i := 0 to pObjClosure(obj)^.upvalueCount - 1 do
        MarkObject(pObj(pObjClosure(obj)^.upvalues^[i]));
    end;
    okNative: ;
    okString: ;
  end;
end;

procedure TraceReferences;
var
  obj : pObj;
begin
  // ---- Entry assertions ----
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  if VM.MemTracker.GrayCount > 0 then
    Assert(VM.MemTracker.GrayStack <> nil, 'TraceReferences: GrayStack is nil with GrayCount > 0');

  while VM.MemTracker.GrayCount > 0 do
  begin
    Dec(VM.MemTracker.GrayCount);
    obj := VM.MemTracker.GrayStack[VM.MemTracker.GrayCount];
    // ---- Mid assertion ----
    Assert(obj <> nil, 'TraceReferences: nil object popped from gray stack');
    BlackenObject(obj);
  end;

  // ---- Exit assertions ----
  Assert(VM.MemTracker.GrayCount = 0, 'TraceReferences exit: GrayCount should be 0');
end;

procedure TableRemoveWhite(Table : pTable);
var
  i : integer;
begin
  if Table = nil then Exit;
  if Table^.Entries = nil then Exit;
  for i := 0 to Table^.CurrentCapacity - 1 do
  begin
    if (Table^.Entries[i].key <> nil) and
       (not Table^.Entries[i].key^.Obj.IsMarked) then
      TableDelete(Table, Table^.Entries[i].key);
  end;
end;

procedure Sweep;
var
  previous, obj, unreached : pObj;
begin
  // ---- Entry assertions ----
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);

  previous := nil;
  obj := VM.MemTracker.CreatedObjects;
  while obj <> nil do
  begin
    if obj^.IsMarked then
    begin
      obj^.IsMarked := false;
      previous := obj;
      obj := obj^.Next;
    end
    else
    begin
      unreached := obj;
      obj := obj^.Next;
      if previous <> nil then
        previous^.Next := obj
      else
        VM.MemTracker.CreatedObjects := obj;
      FreeObject(unreached);
    end;
  end;

  // ---- Exit assertions ----
  Assert(VM.MemTracker.BytesAllocated >= 0, 'Sweep exit: BytesAllocated is negative');
end;

procedure CollectGarbage;
{$IFDEF DEBUG_LOG_GC}
var
  bytesBefore : integer;
{$ENDIF}
begin
  if VM = nil then Exit;
  if VM.MemTracker = nil then Exit;

  {$IFDEF DEBUG_LOG_GC}
  GCLog('-- gc begin');
  bytesBefore := VM.MemTracker.BytesAllocated;
  Inc(GCLogCycles);
  {$ENDIF}

  MarkRoots;
  TraceReferences;
  TableRemoveWhite(VM.Strings);
  Sweep;

  VM.MemTracker.NextGC := VM.MemTracker.BytesAllocated * GC_HEAP_GROW_FACTOR;

  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('-- gc end   collected %d bytes (from %d to %d) next at %d',
    [bytesBefore - VM.MemTracker.BytesAllocated, bytesBefore,
     VM.MemTracker.BytesAllocated, VM.MemTracker.NextGC]));
  {$ENDIF}

  // ---- Exit assertions ----
  Assert(VM.MemTracker.BytesAllocated >= 0, 'CollectGarbage exit: BytesAllocated is negative');
  Assert(VM.MemTracker.NextGC > 0, 'CollectGarbage exit: NextGC should be > 0');
end;

procedure InitMemTracker(var MemTracker : pMemTracker);
begin
  AssertPointerIsNil(MemTracker, 'InitMemTracker - MemTracker not nil before initialization');
  new(MemTracker); //Allocate(pointer(MemTracker),0, SizeOf(MemTracker),MemTracker);
  MemTracker.CreatedObjects := nil;
  MemTracker.Roots.Stack := nil;
  MemTracker.BytesAllocated := 0;
  MemTracker.NextGC := 1024 * 1024;
  MemTracker.GrayCount := 0;
  MemTracker.GrayCapacity := 0;
  MemTracker.GrayStack := nil;
  
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

procedure defineNative(const name : AnsiString; func : TNativeFn);
var
  native : pObjNative;
  nameStr : pObjString;
begin
  // ---- Entry assertions ----
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(Length(name) > 0, 'defineNative: name is empty');
  Assert(Assigned(func), 'defineNative: func is not assigned');

  // Push nameStr and native onto stack to protect from GC (book pattern).
  nameStr := CreateString(name, VM.MemTracker);
  pushStack(VM.Stack, StringToValue(nameStr), VM.MemTracker);
  native := newNative(func, VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(native)), VM.MemTracker);
  TableSet(VM.Globals, nameStr, CreateObject(pObj(native)), VM.MemTracker);
  popStack(VM.Stack);
  popStack(VM.Stack);
end;

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
      RuntimeError('Assertion failed: ' + String(ObjStringToAnsiString(pObjString(args[1].ObjValue))))
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

procedure InitVM;
begin
  {$IFDEF DEBUG_LOG_GC}
  AssignFile(GCLogFile, 'gc.log');
  Rewrite(GCLogFile);
  GCLog('== GC log started ==');
  GCLogAllocations := 0;
  GCLogFrees := 0;
  GCLogMarks := 0;
  GCLogBlackens := 0;
  GCLogCycles := 0;
  {$ENDIF}

  new(VM); //we don't care about making this route through allocate
  VM.FrameCount := 0;
  VM.Stack := nil;
  VM.MemTracker := nil;
  VM.Strings := nil;
  VM.Globals := nil;
  VM.OpenUpvalues := nil;
  VM.RuntimeErrorStr := '';
  VM.PrintOutput := '';
  InitMemTracker(VM.MemTracker);
  InitTable(VM.Strings, VM.MemTracker);
  InitTable(VM.Globals, VM.MemTracker);
  InitStack(VM.Stack,Vm.MemTracker);
  ResetStack(vm.Stack);
  //GC set up
  VM.MemTracker.Roots.Stack := Vm.Stack;

  // Define native functions
  defineNative('clock', clockNative);
  defineNative('collectGarbage', collectGarbageNative);
  defineNative('assert', assertNative);
  defineNative('bytesAllocated', bytesAllocatedNative);
  defineNative('objectsAllocated', objectsAllocatedNative);

  // ---- Exit assertions ----
  AssertVMIsAssigned;
  Assert(VM.Stack <> nil, 'InitVM exit: Stack should not be nil');
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(VM.Strings <> nil, 'InitVM exit: Strings table should not be nil');
  Assert(VM.Globals <> nil, 'InitVM exit: Globals table should not be nil');
  Assert(VM.MemTracker.Roots.Stack = VM.Stack, 'InitVM exit: GC roots should point to stack');
end;

procedure FreeVM;
begin
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  FreeStack(VM.Stack,Vm.MemTracker);
  FreeTable(VM.Globals, VM.MemTracker);
  FreeTable(VM.Strings, VM.MemTracker);
  if (Vm.MemTracker.CreatedObjects <> nil) then
  begin
    FreeObjects(Vm.MemTracker.CreatedObjects);
  end;
  // Free gray stack (raw memory, not via Allocate)
  if VM.MemTracker.GrayStack <> nil then
    FreeMem(VM.MemTracker.GrayStack);
  VM.MemTracker.GrayStack := nil;
  Assert(VM.MemTracker.BytesAllocated = 0, 'VM has not disposed of all mem allocation');
  FreeMemTracker(VM.MemTracker);
  dispose(VM);
  VM := nil;

  {$IFDEF DEBUG_LOG_GC}
  GCLog('');
  GCLog('== Summary ==');
  GCLog(Format('  GC cycles:    %d', [GCLogCycles]));
  GCLog(Format('  Allocations:  %d', [GCLogAllocations]));
  GCLog(Format('  Frees:        %d', [GCLogFrees]));
  GCLog(Format('  Marks:        %d', [GCLogMarks]));
  GCLog(Format('  Blackens:     %d', [GCLogBlackens]));
  if GCLogAllocations = GCLogFrees then
    GCLog('  Leak check:   OK (allocations == frees)')
  else
    GCLog(Format('  Leak check:   LEAK (%d objects unaccounted)', [GCLogAllocations - GCLogFrees]));
  if GCLogMarks = GCLogBlackens then
    GCLog('  Trace check:  OK (marks == blackens)')
  else
    GCLog(Format('  Trace check:  MISMATCH (marks=%d, blackens=%d)', [GCLogMarks, GCLogBlackens]));
  GCLog('== GC log finished ==');
  CloseFile(GCLogFile);
  {$ENDIF}

  // ---- Exit assertions ----
  Assert(VM = nil, 'FreeVM exit: VM should be nil');
end;

procedure InitScanner(source : pAnsiChar);
begin
  AssertSourceCodeIsAssigned(source);
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
  Assert(not isAtEnd, 'advance: already at end');
  inc(scanner.Current);
  result := scanner.current[-1];
end;

function isAtEnd : boolean;
begin
  result := scanner.current^ = #0;
end;

function MakeToken(const tokenType : TTokenType)  : TToken;
begin
  Assert(NativeUInt(scanner.current) >= NativeUInt(scanner.start), 'MakeToken: current before start');
  result.tokenType := tokenType;
  result.start := scanner.start;
  result.length := scanner.current - scanner.start;
  result.line := scanner.line;
  Assert(result.length >= 0, 'MakeToken: negative token length');
end;


function ErrorToken(msg : pAnsiChar) : TToken;
begin
  Assert(Assigned(msg), 'ErrorToken: msg is nil');
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
  Assert(Assigned(scanner.current), 'ScanString: scanner.current is nil');
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
  Assert(Assigned(scanner.current), 'ScanNumber: scanner.current is nil');
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
  Assert(Assigned(rest), 'CheckKeyword: rest is nil');
  Assert(start >= 0, 'CheckKeyword: start is negative');
  Assert(length > 0, 'CheckKeyword: length must be > 0');
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
  Assert(Assigned(scanner.current), 'ScanToken: scanner.current is nil');

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
  Assert(Assigned(Token.Start), 'TokenToString: Token.Start is nil');
  Assert(Token.Length >= 0, 'TokenToString: Token.Length is negative');
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
  Assert(Assigned(Msg), 'ErrorAt: Msg is nil');
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
  Assert(Assigned(Msg), 'Consume: Msg is nil');
  if Parser.Current.TokenType = TokenKind then
  begin
    AdvanceParser;
    Exit;
  end;

  ErrorAtCurrent(Msg);
end;

function currentChunk : pChunk;
begin
  Assert(Current <> nil, 'Current compiler is nil');
  Assert(Current^.func <> nil, 'Current compiler function is nil');
  Assert(Current^.func^.chunk <> nil, 'Current compiler function chunk is nil');
  result := Current^.func^.chunk;
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
  writeChunk(Chunk, OP_NIL, line, Memtracker);
  writeChunk(Chunk, OP_RETURN, line, Memtracker);

  // ---- Exit assertions ----
  Assert(Chunk.Count = oldCount + 2, 'EmitReturn exit: chunk count should increase by 2');
  AssertChunkEndsWithReturn(Chunk);
end;

procedure emitConstant(value : TValue);
begin
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
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

function check(tokenType : TTokenType) : boolean;
begin
  Result := parser.current.tokenType = tokenType;
end;

function matchToken(tokenType : TTokenType) : boolean;
begin
  if not check(tokenType) then
    Exit(false);
  advanceParser();
  Result := true;
end;


procedure parsePrecedence(precedence : TPrecedence);
var
  prefixRule : procedure;
  infixRule  : procedure;
  canAssign  : boolean;
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

  canAssign := precedence <= PREC_ASSIGNMENT;
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

  if canAssign and matchToken(TOKEN_EQUAL) then
  begin
    Error('Invalid assignment target.');
  end;
end;


procedure Number();
var
  Value  : TValue;
  lexeme : AnsiString;
  fs : TFormatSettings;
begin
  Assert(parser.previous.tokenType = TOKEN_NUMBER, 'Number: expected TOKEN_NUMBER');
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
  Assert((parser.Previous.tokenType = TOKEN_MINUS) or (parser.Previous.tokenType = TOKEN_BANG),
    'unary: expected TOKEN_MINUS or TOKEN_BANG');
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
  Assert(parser.previous.tokenType = TOKEN_STRING, 'ParseString: expected TOKEN_STRING');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  lexeme := TokenToString(parser.Previous);
  // strip leading and trailing quotes
  if (Length(lexeme) >= 2) and (lexeme[1] = '"') and (lexeme[Length(lexeme)] = '"') then
    lexeme := Copy(lexeme, 2, Length(lexeme) - 2);
  // Push string onto stack to protect from GC until stored in constants.
  strObj := CreateString(lexeme,VM.MemTracker);
  pushStack(VM.Stack, StringToValue(strObj), VM.MemTracker);
  value := StringToValue(strObj);
  emitConstant(value);
  popStack(VM.Stack);
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
  Assert((parser.previous.tokenType = TOKEN_FALSE) or
         (parser.previous.tokenType = TOKEN_TRUE) or
         (parser.previous.tokenType = TOKEN_NIL),
    'literal: expected TOKEN_FALSE, TOKEN_TRUE, or TOKEN_NIL');
  case parser.previous.tokenType of
    TOKEN_FALSE : emitByte(OP_FALSE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_TRUE  : emitByte(OP_TRUE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_NIL   : emitByte(OP_NIL,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
  end;
end;


procedure printStatement();
begin
  Expression();
  consume(TOKEN_SEMICOLON, 'Expect '';'' after value.');
  emitByte(OP_PRINT, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

function emitJump(instruction : byte) : integer;
begin
  emitByte(instruction, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
  Result := CurrentChunk.count - 2;
  // ---- Exit assertions ----
  Assert(Result >= 0, 'emitJump exit: offset should be >= 0');
end;

procedure patchJump(offset : integer);
var
  jump : integer;
begin
  // ---- Entry assertions ----
  Assert(offset >= 0, 'patchJump: offset is negative');
  Assert(offset + 1 < CurrentChunk.Count, 'patchJump: offset out of bounds');

  // -2 to adjust for the two-byte jump operand itself
  jump := CurrentChunk.count - offset - 2;
  if jump > $FFFF then
    Error('Too much code to jump over.');
  CurrentChunk.code[offset]     := (jump shr 8) and $FF;
  CurrentChunk.code[offset + 1] := jump and $FF;
end;

procedure emitLoop(loopStart : integer);
var
  offset : integer;
begin
  Assert(loopStart >= 0, 'emitLoop: loopStart is negative');
  emitByte(OP_LOOP, CurrentChunk, parser.previous.line, vm.MemTracker);
  offset := CurrentChunk.count - loopStart + 2;
  if offset > $FFFF then
    Error('Loop body too large.');
  emitByte((offset shr 8) and $FF, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte(offset and $FF, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

procedure ifStatement();
var
  thenJump, elseJump : integer;
begin
  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after ''if''.');
  Expression();
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after condition.');

  thenJump := emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  statement();

  elseJump := emitJump(OP_JUMP);
  patchJump(thenJump);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);

  if matchToken(TOKEN_ELSE) then
    statement();
  patchJump(elseJump);
end;

procedure whileStatement();
var
  loopStart, exitJump : integer;
begin
  loopStart := CurrentChunk.count;
  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after ''while''.');
  Expression();
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after condition.');

  exitJump := emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

procedure forStatement();
var
  loopStart, exitJump, bodyJump, incrementStart : integer;
begin
  beginScope();
  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after ''for''.');

  // Initializer
  if matchToken(TOKEN_SEMICOLON) then
    // No initializer
  else if matchToken(TOKEN_VAR) then
    varDeclaration()
  else
    expressionStatement();

  loopStart := CurrentChunk.count;

  // Condition
  exitJump := -1;
  if not matchToken(TOKEN_SEMICOLON) then
  begin
    Expression();
    consume(TOKEN_SEMICOLON, 'Expect '';'' after loop condition.');
    exitJump := emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

  // Increment
  if not matchToken(TOKEN_RIGHT_PAREN) then
  begin
    bodyJump := emitJump(OP_JUMP);
    incrementStart := CurrentChunk.count;
    Expression();
    emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after for clauses.');
    emitLoop(loopStart);
    loopStart := incrementStart;
    patchJump(bodyJump);
  end;

  statement();
  emitLoop(loopStart);

  if exitJump <> -1 then
  begin
    patchJump(exitJump);
    emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

  endScope();
end;

procedure and_();
var
  endJump : integer;
begin
  endJump := emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
end;

procedure or_();
var
  elseJump, endJump : integer;
begin
  elseJump := emitJump(OP_JUMP_IF_FALSE);
  endJump := emitJump(OP_JUMP);
  patchJump(elseJump);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  parsePrecedence(PREC_OR);
  patchJump(endJump);
end;

procedure initCompiler(var compiler : pCompiler; funcType : TFunctionType);
var
  local : ^TLocal;
begin
  New(compiler);
  compiler^.enclosing := Current;
  compiler^.func := nil;
  compiler^.funcType := funcType;
  compiler^.localCount := 0;
  compiler^.scopeDepth := 0;
  compiler^.func := newFunction(VM.MemTracker);
  Current := compiler;

  if funcType <> TYPE_SCRIPT then
  begin
    Current^.func^.name := CreateString(
      Copy(AnsiString(parser.previous.start), 1, parser.previous.length),
      VM.MemTracker);
  end;

  // The first slot is claimed for the VM's internal use (the function itself)
  local := @Current^.locals[Current^.localCount];
  Inc(Current^.localCount);
  local^.depth := 0;
  local^.name.start := nil;
  local^.name.length := 0;
end;

procedure beginScope();
begin
  Assert(Current <> nil, 'beginScope: Current compiler is nil');
  Inc(Current^.scopeDepth);

  // ---- Exit assertions ----
  Assert(Current^.scopeDepth > 0, 'beginScope exit: scopeDepth should be > 0');
end;

procedure endScope();
begin
  // ---- Entry assertions ----
  Assert(Current <> nil, 'endScope: Current compiler is nil');
  Assert(Current^.scopeDepth > 0, 'endScope: scopeDepth is already 0');

  Dec(Current^.scopeDepth);
  while (Current^.localCount > 0) and
        (Current^.locals[Current^.localCount - 1].depth > Current^.scopeDepth) do
  begin
    if Current^.locals[Current^.localCount - 1].isCaptured then
      emitByte(OP_CLOSE_UPVALUE, CurrentChunk, parser.previous.line, vm.MemTracker)
    else
      emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    Dec(Current^.localCount);
  end;

  // ---- Exit assertions ----
  Assert(Current^.scopeDepth >= 0, 'endScope exit: scopeDepth is negative');
  Assert(Current^.localCount >= 0, 'endScope exit: localCount is negative');
end;

procedure addLocal(name : TToken);
begin
  Assert(Current <> nil, 'addLocal: Current compiler is nil');
  if Current^.localCount = UINT8_COUNT then
  begin
    Error('Too many local variables in function.');
    Exit;
  end;
  Current^.locals[Current^.localCount].name := name;
  Current^.locals[Current^.localCount].depth := -1; // sentinel: not yet initialized
  Current^.locals[Current^.localCount].isCaptured := false;
  Inc(Current^.localCount);
end;

function identifiersEqual(const a, b : TToken) : boolean;
begin
  if a.length <> b.length then
    Exit(false);
  Result := CompareMem(a.start, b.start, a.length);
end;

procedure declareVariable();
var
  i : integer;
begin
  Assert(Current <> nil, 'declareVariable: Current compiler is nil');
  if Current^.scopeDepth = 0 then Exit; // globals handled differently

  for i := Current^.localCount - 1 downto 0 do
  begin
    if Current^.locals[i].depth <> -1 then
      if Current^.locals[i].depth < Current^.scopeDepth then
        Break;
    if identifiersEqual(parser.previous, Current^.locals[i].name) then
    begin
      Error('Already a variable with this name in this scope.');
      Exit;
    end;
  end;

  addLocal(parser.previous);
end;

procedure markInitialized();
begin
  if Current^.scopeDepth = 0 then Exit;
  Current^.locals[Current^.localCount - 1].depth := Current^.scopeDepth;
end;

function resolveLocal(compiler : pCompiler; const name : TToken) : integer;
var
  i : integer;
begin
  Assert(compiler <> nil, 'resolveLocal: compiler is nil');
  for i := compiler^.localCount - 1 downto 0 do
  begin
    if identifiersEqual(name, compiler^.locals[i].name) then
    begin
      if compiler^.locals[i].depth = -1 then
        Error('Can''t read local variable in its own initializer.');
      Exit(i);
    end;
  end;
  Result := -1; // not found = global
end;

function addUpvalue(compiler : pCompiler; index : byte; isLocal : boolean) : integer;
var
  upvalueCount : integer;
  i : integer;
begin
  upvalueCount := compiler^.func^.upvalueCount;

  for i := 0 to upvalueCount - 1 do
  begin
    if (compiler^.upvalues[i].index = index) and (compiler^.upvalues[i].isLocal = isLocal) then
      Exit(i);
  end;

  if upvalueCount = UINT8_COUNT then
  begin
    Error('Too many closure variables in function.');
    Exit(0);
  end;

  compiler^.upvalues[upvalueCount].isLocal := isLocal;
  compiler^.upvalues[upvalueCount].index := index;
  Inc(compiler^.func^.upvalueCount);
  Result := upvalueCount;
end;

function resolveUpvalue(compiler : pCompiler; const name : TToken) : integer;
var
  local : integer;
  upvalue : integer;
begin
  Assert(compiler <> nil, 'resolveUpvalue: compiler is nil');
  if compiler^.enclosing = nil then
    Exit(-1);

  local := resolveLocal(compiler^.enclosing, name);
  if local <> -1 then
  begin
    compiler^.enclosing^.locals[local].isCaptured := true;
    Exit(addUpvalue(compiler, byte(local), true));
  end;

  upvalue := resolveUpvalue(compiler^.enclosing, name);
  if upvalue <> -1 then
    Exit(addUpvalue(compiler, byte(upvalue), false));

  Result := -1;
end;

function identifierConstant(const name : TToken) : byte;
var
  strObj : pObjString;
  idx : integer;
begin
  // Push string onto stack to protect from GC until stored in constants.
  strObj := CreateString(TokenToString(name), VM.MemTracker);
  pushStack(VM.Stack, StringToValue(strObj), VM.MemTracker);
  idx := AddValueConstant(CurrentChunk.Constants, StringToValue(strObj), VM.MemTracker);
  popStack(VM.Stack);
  Assert(idx <= High(Byte), 'Too many constants in one chunk');
  Result := Byte(idx);
end;

function parseVariable(const errorMsg : PAnsiChar) : byte;
begin
  consume(TOKEN_IDENTIFIER, errorMsg);

  declareVariable();
  if Current^.scopeDepth > 0 then
    Exit(0); // locals don't get stored in constant table

  Result := identifierConstant(parser.previous);
end;

procedure defineVariable(global : byte);
begin
  if Current^.scopeDepth > 0 then
  begin
    markInitialized();
    Exit; // local is already on the stack
  end;
  emitByte(OP_DEFINE_GLOBAL, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte(global, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

procedure varDeclaration();
var
  global : byte;
begin
  global := parseVariable('Expect variable name.');

  if matchToken(TOKEN_EQUAL) then
    Expression()
  else
    emitByte(OP_NIL, CurrentChunk, parser.previous.line, vm.MemTracker);

  consume(TOKEN_SEMICOLON, 'Expect '';'' after variable declaration.');
  defineVariable(global);
end;

procedure namedVariable(name : TToken);
var
  arg : integer;
  getOp, setOp : byte;
begin
  arg := resolveLocal(Current, name);
  if arg <> -1 then
  begin
    getOp := OP_GET_LOCAL;
    setOp := OP_SET_LOCAL;
  end
  else
  begin
    arg := resolveUpvalue(Current, name);
    if arg <> -1 then
    begin
      getOp := OP_GET_UPVALUE;
      setOp := OP_SET_UPVALUE;
    end
    else
    begin
      arg := identifierConstant(name);
      getOp := OP_GET_GLOBAL;
      setOp := OP_SET_GLOBAL;
    end;
  end;

  if matchToken(TOKEN_EQUAL) then
  begin
    Expression();
    emitByte(setOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(arg), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitByte(getOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(arg), CurrentChunk, parser.previous.line, vm.MemTracker);
  end;
end;

procedure variable();
begin
  namedVariable(parser.previous);
end;

procedure expressionStatement();
begin
  Expression();
  consume(TOKEN_SEMICOLON, 'Expect '';'' after expression.');
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

function argumentList() : byte;
var
  argCount : byte;
begin
  argCount := 0;
  if not check(TOKEN_RIGHT_PAREN) then
  begin
    repeat
      Expression();
      if argCount = 255 then
        Error('Can''t have more than 255 arguments.');
      Inc(argCount);
    until not matchToken(TOKEN_COMMA);
  end;
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after arguments.');
  Result := argCount;
end;

procedure call();
var
  argCount : byte;
begin
  argCount := argumentList();
  emitByte(OP_CALL, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte(argCount, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

procedure functionBody(funcType : TFunctionType);
var
  compiler : pCompiler;
  func : pObjFunction;
  i : byte;
  j : integer;
begin
  // ---- Entry assertions ----
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);

  compiler := nil;
  initCompiler(compiler, funcType);
  beginScope();

  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after function name.');
  if not check(TOKEN_RIGHT_PAREN) then
  begin
    repeat
      Inc(Current^.func^.arity);
      if Current^.func^.arity > 255 then
        errorAtCurrent('Can''t have more than 255 parameters.');
      i := parseVariable('Expect parameter name.');
      defineVariable(i);
    until not matchToken(TOKEN_COMMA);
  end;
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after parameters.');
  consume(TOKEN_LEFT_BRACE, 'Expect ''{'' before function body.');
  block();

  // End compiler
  emitReturn(CurrentChunk, parser.previous.line, VM.MemTracker);
  func := Current^.func;
  Current := Current^.enclosing;

  // Push func onto stack to protect from GC until stored in constants.
  pushStack(VM.Stack, CreateObject(pObj(func)), VM.MemTracker);

  emitByte(OP_CLOSURE, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte(AddValueConstant(CurrentChunk.Constants, CreateObject(pObj(func)), VM.MemTracker),
    CurrentChunk, parser.previous.line, vm.MemTracker);

  for j := 0 to func^.upvalueCount - 1 do
  begin
    if compiler^.upvalues[j].isLocal then
      emitByte(1, CurrentChunk, parser.previous.line, vm.MemTracker)
    else
      emitByte(0, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(compiler^.upvalues[j].index, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

  popStack(VM.Stack);

  Dispose(compiler);
end;

procedure funDeclaration();
var
  global : byte;
begin
  Assert(Current <> nil, 'funDeclaration: Current compiler is nil');
  global := parseVariable('Expect function name.');
  markInitialized();
  functionBody(TYPE_FUNCTION);
  defineVariable(global);
end;

procedure returnStatement();
begin
  Assert(Current <> nil, 'returnStatement: Current compiler is nil');
  if Current^.funcType = TYPE_SCRIPT then
  begin
    Error('Can''t return from top-level code.');
  end;

  if matchToken(TOKEN_SEMICOLON) then
  begin
    emitReturn(CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    Expression();
    consume(TOKEN_SEMICOLON, 'Expect '';'' after return value.');
    emitByte(OP_RETURN, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;
end;

procedure block();
begin
  while (not check(TOKEN_RIGHT_BRACE)) and (not check(TOKEN_EOF)) do
    declaration();
  consume(TOKEN_RIGHT_BRACE, 'Expect ''}'' after block.');
end;

procedure statement();
begin
  if matchToken(TOKEN_PRINT) then
    printStatement()
  else if matchToken(TOKEN_IF) then
    ifStatement()
  else if matchToken(TOKEN_WHILE) then
    whileStatement()
  else if matchToken(TOKEN_FOR) then
    forStatement()
  else if matchToken(TOKEN_RETURN) then
    returnStatement()
  else if matchToken(TOKEN_LEFT_BRACE) then
  begin
    beginScope();
    block();
    endScope();
  end
  else
    expressionStatement();
end;

procedure declaration();
begin
  Assert(Current <> nil, 'declaration: Current compiler is nil');
  if matchToken(TOKEN_FUN) then
    funDeclaration()
  else if matchToken(TOKEN_VAR) then
    varDeclaration()
  else
    statement();
end;

function compile(source : pAnsiChar) : pObjClosure;
var
  compiler : pCompiler;
  func : pObjFunction;
begin
  AssertSourceCodeIsAssigned(source);
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  initScanner(source);
  compiler := nil;
  Current := nil;
  initCompiler(compiler, TYPE_SCRIPT);
  parser.hadError := false;
  parser.panicMode := false;
  advanceParser();
  while not check(TOKEN_EOF) do
  begin
    declaration();
  end;
  consume(TOKEN_EOF, 'Expect end of input.');
  emitReturn(CurrentChunk, parser.previous.line, VM.Memtracker);
  func := Current^.func;

  // Restore enclosing compiler
  Current := Current^.enclosing;
  Dispose(compiler);

  if parser.HadError then
    Result := nil
  else
  begin
    // Push func onto stack to protect from GC during newClosure allocation.
    pushStack(VM.Stack, CreateObject(pObj(func)), VM.MemTracker);
    Result := newClosure(func, VM.MemTracker);
    popStack(VM.Stack);
  end;

  // ---- Exit assertions ----
  Assert(Current = nil, 'compile exit: Current compiler should be nil');
end;


procedure InitTable(var Table : pTable; memTracker : pMemTracker);
begin
   AssertMemTrackerIsNotNil(memTracker);
   AssertPointerIsNil(Table, ' Table is not nil');
   Allocate(pointer(Table),0,Sizeof(TTable),MemTracker);
   Table.Count := 0;
   Table.CurrentCapacity := 0;
   Table.Entries  := nil;

   // ---- Exit assertions ----
   AssertTable(Table);
   Assert(Table.Count = 0, 'InitTable exit: count should be 0');
   Assert(Table.CurrentCapacity = 0, 'InitTable exit: capacity should be 0');
   Assert(Table.Entries = nil, 'InitTable exit: entries should be nil');
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
  tombstone: pEntry;
  entry: pEntry;
begin
  Assert(Assigned(Entries), 'FindEntry: entries not assigned');
  Assert(Capacity > 0, 'FindEntry: capacity must be > 0');
  AssertObjStringIsAssigned(key);

  index := Key.hash mod uint32(Capacity);
  tombstone := nil;
  while true do
  begin
    entry := @Entries[index];
    if entry.key = nil then
    begin
      if isNill(entry.value) then
      begin
        // Empty slot: return tombstone if we passed one, else this empty slot
        if tombstone <> nil then
          Result := tombstone
        else
          Result := entry;
        Exit;
      end
      else
      begin
        // Tombstone: remember first one
        if tombstone = nil then
          tombstone := entry;
      end;
    end
    else if entry.key = key then
    begin
      // Found the key
      Result := entry;
      Exit;
    end;
    index := (index + 1) mod uint32(Capacity);
  end;

  // ---- Exit assertions ----
  Assert(Result <> nil, 'FindEntry exit: result should not be nil');
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

  // ---- Exit assertions ----
  Assert(Table.Entries <> nil, 'AdjustCapacity exit: entries should not be nil');
  Assert(Table.CurrentCapacity = NewCapacity, 'AdjustCapacity exit: capacity mismatch');
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
  if Result and isNill(Entry.value) then
    Inc(Table.Count);

  Entry.key := key;
  Entry.value := value;

  // ---- Exit assertions ----
  Assert(Entry.key = key, 'TableSet exit: key not written');
end;


function TableDelete(Table: pTable; key: pObjString): boolean;
var
  Entry: pEntry;
begin
  AssertTable(Table);
  AssertObjStringIsAssigned(key);

  if Table.Count = 0 then
    Exit(false);

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  if Entry.key = nil then
    Exit(false);

  // Place a tombstone
  Entry.key := nil;
  Entry.value := CreateBoolean(true);
  Result := true;
end;


function TableFindString(Table: pTable; const chars: PAnsiChar; length: integer; hash: uint32): pObjString;
var
  index: uint32;
  entry: pEntry;
begin
  AssertTable(Table);
  Assert(length >= 0, 'TableFindString: length must be >= 0');

  Result := nil;
  if Table.Count = 0 then Exit;

  AssertTableEntries(Table);

  index := hash mod uint32(Table.CurrentCapacity);
  while true do
  begin
    entry := @Table.Entries[index];
    if entry.key = nil then
    begin
      // Stop if we find an empty non-tombstone entry
      if isNill(entry.value) then Exit;
    end
    else if (entry.key.length = length) and
            (entry.key.hash = hash) and
            CompareMem(@entry.key.chars[0], chars, length) then
    begin
      Result := entry.key;
      Exit;
    end;
    index := (index + 1) mod uint32(Table.CurrentCapacity);
  end;
end;


function TableGet(Table: pTable; key: pObjString; var value: TValue): boolean;
var
  Entry: pEntry;
begin
  AssertTable(Table);
  AssertObjStringIsAssigned(key);

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
  AssertPointerIsNil(Table, 'FreeTable exit: Table should be nil');

end;


function HashString(const Key: PAnsiChar; Length: Integer): UInt32;
const
  FNVOffset = 2166136261;
  FNVPrime = 16777619;
var
  i: Integer;
begin
  Assert(Assigned(Key), 'HashString: Key is nil');
  Assert(Length >= 0, 'HashString: Length must be >= 0');
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
  Current := nil;

finalization

end.
