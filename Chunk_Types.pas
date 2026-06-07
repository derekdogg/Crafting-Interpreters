unit Chunk_Types;
{$POINTERMATH ON}
{$ASSERTIONS OFF}
{..$DEFINE DEBUG_LOG_GC}
{..$DEFINE DEBUG_STRESS_GC}
{..$DEFINE DEBUG_STRESS_TABLE}
{..$DEFINE OPCODE_PROFILING}
interface
 
uses
  Classes, dialogs, System.Rtti, Math, System.TypInfo, System.SysUtils, System.AnsiStrings;

const

  START_CAPACITY =  8;
  MAX_SIZE       =  MaxInt div 2;
  GROWTH_FACTOR  =  2;
  GC_HEAP_GROW_FACTOR = 2;
  GC_NEXT_MIN = 1024;              // floor for NextGC after collection
  GC_NEXT_INITIAL = 1024 * 1024;   // initial threshold before first GC
  GRAY_STACK_INITIAL_CAPACITY = 8;
  ARRAY_START_CAPACITY = 8;        // initial element capacity for ObjArray
  MAX_BYTE_OPERAND = 255;          // max value encodable in a single byte operand
  MAX_DICT_LITERAL_PAIRS = 127;    // max pairs in dict literal (pair * 2 must fit in byte)
  MAX_JUMP_OFFSET = $FFFF;         // max 16-bit jump distance

  //Table
  TABLE_START_CAPACITY = 16;
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
  OP_MODULO = 30;
  OP_DEFINE_GLOBAL_LONG = 31;
  OP_GET_GLOBAL_LONG = 32;
  OP_SET_GLOBAL_LONG = 33;
  OP_CLOSURE_LONG = 34;
  OP_RECORD = 35;
  OP_GET_PROPERTY = 36;
  OP_SET_PROPERTY = 37;
  OP_INVOKE = 38;
  OP_INVOKE_LONG = 39;
  OP_GET_PROPERTY_LONG = 40;
  OP_SET_PROPERTY_LONG = 41;
  OP_GET_SUBSCRIPT = 42;
  OP_SET_SUBSCRIPT = 43;
  OP_ARRAY_LITERAL = 44;
  OP_DICT_LITERAL = 45;
  OP_RECORD_LONG = 46;
  OP_POP_N = 47;  // 1-byte operand N: bulk-pop N stack slots (N >= 1). Compiler emits OP_POP for N=1, OP_POP_N for N>=2.
  OP_JUMP_IF_FALSE_POP = 48;  // 2-byte offset: always pops, jumps if popped value was falsy. Fused replacement for OP_JUMP_IF_FALSE+OP_POP on if/while/for. NOT used by and_/or_ (they need value preservation).
  // Fast-slot variants for the 8 most common local slots. No operand byte ?
  // slot index is encoded in the opcode itself. Emitted by namedVariable when
  // resolveLocal returns a slot in 0..7. Saves 1 bytecode byte per access and
  // one ReadByteFr per dispatch (~3 instructions). Slot 0 is the reserved
  // function-self slot and is never produced by user code today, but the
  // opcode is kept for uniformity (future this-binding etc.).
  OP_GET_LOCAL_0 = 49;
  OP_GET_LOCAL_1 = 50;
  OP_GET_LOCAL_2 = 51;
  OP_GET_LOCAL_3 = 52;
  OP_GET_LOCAL_4 = 53;
  OP_GET_LOCAL_5 = 54;
  OP_GET_LOCAL_6 = 55;
  OP_GET_LOCAL_7 = 56;
  OP_SET_LOCAL_0 = 57;
  OP_SET_LOCAL_1 = 58;
  OP_SET_LOCAL_2 = 59;
  OP_SET_LOCAL_3 = 60;
  OP_SET_LOCAL_4 = 61;
  OP_SET_LOCAL_5 = 62;
  OP_SET_LOCAL_6 = 63;
  OP_SET_LOCAL_7 = 64;
  OP_LESS_JUMP_IF_FALSE = 65;  // Fused OP_LESS + OP_JUMP_IF_FALSE_POP. 2-byte offset operand. Pops both number operands, jumps if NOT (a < b).
  OP_GET_LOCAL_CONST_SUBTRACT = 66;  // Fused GET_LOCAL_N + CONSTANT + SUBTRACT. Operands: <slot> <const_idx>. Pushes locals[slot] - constants[const_idx].
  OP_ADD_SET_LOCAL_POP = 67;  // Fused ADD + SET_LOCAL_N + POP. Operand: <slot>. Adds top two stack values, stores to slot, pops both.
  OP_ADD_RETURN = 68;  // Fused ADD + RETURN. No operands. Adds top two, returns result to caller.
  OP_DUP = 69;  // Duplicate top of stack. Push copy of stack[-1].
  OP_DUP2 = 70;  // Duplicate top two stack slots. Push copies of stack[-2] and stack[-1].

  OP_STRINGS : array[0..70] of string = (
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
    'OP_CLOSE_UPVALUE',
    'OP_MODULO',
    'OP_DEFINE_GLOBAL_LONG',
    'OP_GET_GLOBAL_LONG',
    'OP_SET_GLOBAL_LONG',
    'OP_CLOSURE_LONG',
    'OP_RECORD',
    'OP_GET_PROPERTY',
    'OP_SET_PROPERTY',
    'OP_INVOKE',
    'OP_INVOKE_LONG',
    'OP_GET_PROPERTY_LONG',
    'OP_SET_PROPERTY_LONG',
    'OP_GET_SUBSCRIPT',
    'OP_SET_SUBSCRIPT',
    'OP_ARRAY_LITERAL',
    'OP_DICT_LITERAL',
    'OP_RECORD_LONG',
    'OP_POP_N',
    'OP_JUMP_IF_FALSE_POP',
    'OP_GET_LOCAL_0','OP_GET_LOCAL_1','OP_GET_LOCAL_2','OP_GET_LOCAL_3',
    'OP_GET_LOCAL_4','OP_GET_LOCAL_5','OP_GET_LOCAL_6','OP_GET_LOCAL_7',
    'OP_SET_LOCAL_0','OP_SET_LOCAL_1','OP_SET_LOCAL_2','OP_SET_LOCAL_3',
    'OP_SET_LOCAL_4','OP_SET_LOCAL_5','OP_SET_LOCAL_6','OP_SET_LOCAL_7',
    'OP_LESS_JUMP_IF_FALSE',
    'OP_GET_LOCAL_CONST_SUBTRACT',
    'OP_ADD_SET_LOCAL_POP',
    'OP_ADD_RETURN',
    'OP_DUP',
    'OP_DUP2');

  UINT8_COUNT = 256;
  FRAMES_MAX = 512;
  STACK_MAX = FRAMES_MAX * UINT8_COUNT;  // 65536 slots — hard upper bound on Stack growth

  // NaN boxing constants
  // Finite IEEE 754 doubles do not satisfy (bits & QNAN) = QNAN.
  // Some IEEE NaN payloads can satisfy this mask. Arithmetic operations
  // that propagate NaNs are allowed to preserve or modify payload bits,
  // so an externally supplied or computed NaN could otherwise be mistaken
  // for a tagged value. CreateNumber canonicalizes such NaNs to CANON_NAN
  // so they are never misclassified as nil/boolean/object values.
  //
  // Non-number Lox values are encoded as quiet NaN payloads:
  //   Nil:    QNAN | TAG_NIL
  //   False:  QNAN | TAG_FALSE
  //   True:   QNAN | TAG_TRUE
  //   Object: SIGN_BIT | QNAN | pointer
  //            (assumes object pointers fit within the low 48 bits
  //             of a 64-bit virtual address)
  QNAN      = UInt64($7FFC000000000000);
  SIGN_BIT  = UInt64($8000000000000000);
  OBJ_TAG   = QNAN or SIGN_BIT;            // $FFFC000000000000
  CANON_NAN = UInt64($7FF8000000000000);
  // Canonical quiet NaN chosen so (bits and QNAN) <> QNAN,
  // therefore it is classified as a number rather than a tagged value.

  TAG_NIL   = 1;
  TAG_FALSE = 2;
  TAG_TRUE  = 3;

  NIL_VAL   = QNAN or TAG_NIL;    // $7FFC000000000001
  FALSE_VAL = QNAN or TAG_FALSE;  // $7FFC000000000002
  TRUE_VAL  = QNAN or TAG_TRUE;   // $7FFC000000000003


type
  // Raised by the slow paths of stack growth (pushStackGrow) and other
  // VM-internal unwinders when an unrecoverable condition is hit mid-
  // bytecode-dispatch. Caught at the CompileAndRun boundary and translated
  // into INTERPRET_RUNTIME_ERROR so deep recursion / runaway pushes surface
  // as a clean script error instead of crashing the host app.
  ELoxRuntimeError = class(Exception);
  //Enums
  TValueKind = (vkNumber, vkBoolean, vkNull, vkObject);
  TObjectKind = (okString, okFunction, okNative, okClosure, okUpvalue, okArray, okRecordType, okRecord, okNativeObject, okDictionary);
  TFunctionType = (TYPE_FUNCTION, TYPE_SCRIPT);
  TTokenType = (
    // Single-character tokens
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_LEFT_BRACKET, TOKEN_RIGHT_BRACKET,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR, TOKEN_PERCENT,
    TOKEN_COLON,

    // One or two character tokens
    TOKEN_BANG, TOKEN_BANG_EQUAL,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_LESS, TOKEN_LESS_EQUAL,
    TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL,
    TOKEN_STAR_EQUAL, TOKEN_SLASH_EQUAL,

    // Literals
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

    // Keywords
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
    TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
    TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
    TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE, TOKEN_RECORD,

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
  pObjArray       = ^TObjArray;
  pObjRecordType  = ^TObjRecordType;
  pObjRecord      = ^TObjRecord;
  pObjNativeObject = ^TObjNativeObject;
  pNativeClassInfo = ^TNativeClassInfo;
  pObjDictionary  = ^TObjDictionary;
  pDictEntry      = ^TDictEntry;
  ppObjString     = ^pObjString;
  pUpvalueArray   = ^TUpvalueArray;
  pCompiler       = ^TCompiler;
  pStack          = ^TStack;
  pVirtualMachine = ^TVirtualMachine;
  pMemTracker     = ^TMemTracker;


  pLogs           = ^TLogs; //Note : Don't think we need this (https://www.danieleteti.it/loggerpro/) maybe add later? We use a stupidly simple approach for now
  pEntry          = ^TEntry;
  pTable          = ^TTable;
  pLocal          = ^TLocal;
  pCallFrame      = ^TCallFrame;


  //Arrays
  TAnsiCharArray = Array[0..0] of AnsiChar;
  TUpvalueArray  = Array[0..UINT8_COUNT - 1] of pObjUpvalue;

  { Attribute to mark a Delphi class as visible to Lox scripts.
    Usage: [LoxClass('MyName')] on a class declaration.
    The name is what appears in Lox ? e.g. [LoxClass('StringList')]. }
  LoxClassAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  { Marks a published/public property as accessible from Lox scripts.
    Usage: [LoxProperty] or [LoxProperty('readonly')] }
  LoxPropertyAttribute = class(TCustomAttribute)
  private
    FReadOnly: Boolean;
  public
    constructor Create; overload;
    constructor Create(const AMode: string); overload;
    property IsReadOnly: Boolean read FReadOnly;
  end;

  { Marks a public method as callable from Lox scripts.
    Usage: [LoxMethod] }
  LoxMethodAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

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

  TValue = UInt64;

  // `args` points into VM.Stack.Values. Natives MUST read args[i] into local
  // Pascal vars before any pushStack / CreateString / allocation call ? those
  // can ReallocMem the stack buffer and dangle the pointer. See CallValue.
  TNativeFn = function(argCount: integer; args: pValue): TValue;

  TObjNative = record
    Obj      : TObj;
    func     : TNativeFn;
    arity    : Integer;
    name     : PAnsiChar;  // points to static string literal ? no alloc, no leak
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

  TObjArray = record
    Obj      : TObj;
    Count    : integer;
    Capacity : integer;
    Elements : pValue;
  end;

  TObjRecordType = record
    Obj        : TObj;
    name       : pObjString;
    fieldCount : integer;
    fieldNames : ppObjString;  // array of pObjString
  end;

  TObjRecord = record
    Obj        : TObj;
    recordType : pObjRecordType;
    fields     : pValue;  // flat array indexed by field position
  end;

  TNativeMethodFn = function(instance: Pointer; argCount: integer; args: pValue): TValue;

  TNativeMethod = record
    name : AnsiString;
    fn   : TNativeMethodFn;
  end;

  TNativeDestructor = procedure(instance: Pointer);

  TNativeClassInfo = record
    name       : AnsiString;
    methods    : array of TNativeMethod;
    destructor_: TNativeDestructor;
    rttiEnabled: Boolean;
    rttiClass  : TClass;
  end;

  TObjNativeObject = record
    Obj          : TObj;
    instance     : Pointer;
    classInfo    : pNativeClassInfo;
    ownsInstance : Boolean;  // if true, destructor_ is called on sweep
  end;

  TDictEntry = record
    key       : TValue;
    value     : TValue;
    occupied  : boolean;  // true if slot holds a live entry
    tombstone : boolean;  // true if slot is a deleted entry
  end;

  TObjDictionary = record
    Obj        : TObj;
    Count      : integer;   // live entries (excludes tombstones)
    Tombstones : integer;   // number of tombstone slots
    Capacity   : integer;   // total slots in Entries array
    Entries    : pDictEntry;
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
    Values            : pValue;
    StackTop          : pValue;
    CapacityEnd       : pValue;   // = Values + capacity. Pre-computed sentinel for pushStack.
  end;

  //Virtual Machine result
  TInterpretResult = record
    code  : (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR, INTERPRET_HALTED);
    value : TValue;
    ErrorStr : String;
    ResultStr : String;
    OutputStr : String;
    ExitCode : Integer;
  end;

  TMemTracker = record
    CreatedObjects  : pObj;
    BytesAllocated  : integer;
    NextGC          : integer;
    GrayCount       : integer;
    GrayCapacity    : integer;
    GrayStack       : ^pObj;
    Roots           : TRoots;
    GCCollections   : integer;
  end;

  TCallFrame = record
    closure : pObjClosure;
    ip      : pByte;
    slots   : pValue; // points into VM stack
  end;

  // Callback fired on each print statement for real-time output.
  // If assigned, called with the printed text (one line per call).
  TLoxPrintEvent = procedure(const Text: string) of object;

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
    // External GC roots: Delphi-side TValue slots that hold Lox objects
    // (e.g. closures stored by native libraries for callbacks). The mark
    // phase walks these so the GC doesn't collect live callback closures.
    ExtraRoots      : array of pValue;
    ExtraRootCount  : Integer;
    // Print output is accumulated through a TStringBuilder to avoid the O(n^2)
    // copy cost of repeated `s := s + ...` on a managed string. Callers that
    // need the materialised text read VM.PrintBuilder.ToString; resets call
    // VM.PrintBuilder.Clear. The builder is owned by initVM/FreeVM.
    PrintBuilder    : TStringBuilder;
    OnPrint         : TLoxPrintEvent;
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
    locals       : array[0..UINT8_COUNT - 1] of TLocal; // hard limit: 256 per function, guarded by addLocal and asserted at access sites
    localCount   : integer;
    upvalues     : array[0..UINT8_COUNT - 1] of TUpvalue; // hard limit: 256 per function, guarded by addUpvalue and asserted at access sites
    scopeDepth   : integer;
  end;


  //Prat parsing structs
  TParseFn = procedure(canAssign: Boolean);

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

function findNativeClass(const name : AnsiString) : pNativeClassInfo;
procedure RunTimeError(const msg : string);

type
  ELoxHalt = class(Exception)
  public
    HaltExitCode: Integer;
    constructor Create(const AMsg: string; AExitCode: Integer);
  end;

//Assertions
{$IFOPT C+}
procedure AssertMemTrackerIsNotNil(MemTracker : pMemTracker);
procedure AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker : pMemTracker);
procedure AssertNewStringIsNillBeforeAllocation(ObjString : pObjString);

//Stack assertions
procedure AssertStackIsAssigned(Stack : pStack);
procedure AssertStackValuesIsAssigned(Stack : pStack);
procedure AssertStackCountIsZero(Stack : pStack);
procedure AssertStackCapacityIsGreaterThanZero(Stack : pStack);
procedure AssertStackTopEqualsValues(Stack : pStack);
procedure AssertStackCapacityCanGrow(Stack : pStack);
procedure AssertStackByteSizeIsSafe(NewCapacity : integer);
procedure AssertStackIsNotEmpty(Stack : pStack);
procedure AssertStackTopIsNotNil(Stack : pStack);
procedure AssertStackIsNilBeforeInit(Stack : pStack);
procedure AssertStackTopCountConsistent(Stack : pStack);
procedure AssertRebaseOffsetIsNonZero(Offset : NativeInt);
procedure AssertFrameCountInBounds(FrameCount : integer);

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
procedure AssertValidObjPointer(obj : pObj; const context : string);

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
procedure AssertTable(Table : pTable);inline;
{$ENDIF}



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
function TokenToString(const Token: TToken): AnsiString; inline;
function ObjStringToAnsiString(S: PObjString): AnsiString;
function ValueToString(const value : TValue) : pObjString; inline;
function ValueToStr(const value : TValue) : String;
function StringToValue(const value : pObjString) : TValue; inline;
function isString(value : TValue) : boolean; inline;
function ObjStringEqualsAnsi(s: PObjString; const a: AnsiString): Boolean; inline;
function ObjStringToWideStr(s: PObjString): string;
procedure Concatenate(stack : pStack; MemTracker : pMemTracker); inline;


//Chunk routines
procedure initChunk(var chunk: pChunk;MemTracker : pMemTracker);inline;
procedure freeChunk(var chunk: pChunk;MemTracker : pMemTracker);inline
procedure emitByte(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker );inline;
procedure EmitReturn(Chunk : pChunk; Line : integer; MemTracker : pMemTracker);inline;

procedure writeChunk(chunk: pChunk; value: byte; Line : Integer;MemTracker : pMemTracker);inline;
procedure AddConstant(chunk : pChunk; const value : TValue; Line : Integer; MemTracker : pMemTracker);inline;
procedure initValueArray(var ValueArray : pValueArray;MemTracker : pMemTracker);inline;
function AddValueConstant(ValueArray: pValueArray; const value: TValue;Memtracker : pMemTracker): Integer;inline;
procedure writeValueArray(ValueArray : pValueArray; Value : TValue;MemTracker : pMemTracker);inline;
procedure FreeValues(var Values : pValue; Capacity : integer;MemTracker : pMemTracker);inline;
procedure freeValueArray(var ValueArray : pValueArray;MemTracker : pMemTracker);inline;
procedure printValueArray(ValueArray: pValueArray; strings: TStrings);
function IntToBytes(const value : integer) : TIntToByteResult; inline;
function ByteToInt(const value : TIntToByteResult) : integer;inline;
function ReadByte(var code : pByte): Byte; inline;
function ReadConstant(var code : pByte; constants : pValueArray) : TValue; inline;
function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue;inline;
//Memtracker
procedure InitMemTracker(var MemTracker : pMemTracker);inline;
procedure FreeMemTracker(var MemTracker : pMemTracker);inline;
procedure MarkObject(obj : pObj);inline;
procedure MarkValue(value : TValue); inline;
procedure MarkRoots; inline;
procedure MarkTable(Table : pTable);inline;
procedure MarkArray(ValueArray : pValueArray);inline;
procedure MarkCompilerRoots;inline;
procedure BlackenObject(obj : pObj);inline;
procedure TraceReferences;inline;
procedure TableRemoveWhite(Table : pTable);inline;
procedure Sweep;inline;
procedure CollectGarbage;inline;


//Virtual Machine
procedure InitVM();
function CompileAndRun(source : pAnsiChar) : TInterpretResult;
function InterpretResult(source : pAnsiChar) : TInterpretResult;
procedure InjectNativeObject(const name : AnsiString; instance : Pointer; const className : AnsiString);
function Run(baselineFrameCount: Integer = 0) : TInterpretResult;
function InvokeCallback(closure: TValue; const args: array of TValue;
  out returnVal: TValue): TInterpretResult;
procedure RegisterGCRoot(var slot: TValue);
procedure UnregisterGCRoot(var slot: TValue);
procedure FreeObjects(objects: pObj);
procedure FreeVM();

//Stack
procedure FillNilValues(p: pValue; Count: NativeInt);
procedure InitStack(var Stack : pStack;MemTracker : pMemTracker); inline;
procedure FreeStack(var Stack : pStack;MemTracker : pMemTracker);inline;
procedure ResetStack(var stack : pStack);inline;
procedure pushStack(var stack : pStack;const value : TValue); inline;
procedure pushStackGrow(var stack : pStack;const value : TValue);inline;
//function peekStack(stack : pStack) : TValue; overload; inline;
function peekStack(stack : pStack; distanceFromTop : integer) : TValue; inline;
function  popStack(stack : pStack) : TValue; inline;

//Scanner
procedure InitScanner(source : pAnsiChar);
function  advance : ansichar; inline;
function  isAtEnd : boolean; inline;
function isDigit(c : Ansichar) : boolean;inline;
function CheckKeyword(start, length: Integer; const rest: pAnsiChar;
  tokenType: TTokenType): TTokenType;inline;
  function ScanNumber: TToken;inline;
procedure Consume(TokenKind: TTokenType; const Msg: PAnsiChar);inline;
procedure AdvanceParser(); inline;
function ScanToken : TToken; inline;
function ScanString: TToken;inline;
function ErrorToken(msg : pAnsiChar) : TToken;inline;
procedure errorAtCurrent(const msg : pAnsiChar);
procedure Expression(); inline;
procedure parsePrecedence(precedence : TPrecedence); inline;
procedure Error(const Msg: pAnsiChar);


function identifierType : TTokenType; inline;

//compilation
procedure initCompiler(var compiler : pCompiler; funcType : TFunctionType);inline;
function compile(source : pAnsiChar) : pObjClosure;
procedure declaration();
procedure declareVariable(); inline;
procedure markInitialized();inline;
function resolveLocal(compiler : pCompiler; const name : TToken) : integer; inline;
procedure synchronize();
procedure varDeclaration();
procedure funDeclaration();
procedure functionBody(funcType : TFunctionType);
procedure returnStatement();
procedure statement();
procedure block();
procedure printStatement();
procedure ifStatement();
procedure whileStatement();
procedure forStatement();
procedure expressionStatement();
procedure emitPopWithPeephole;
procedure Number(canAssign: Boolean);
procedure grouping(canAssign: Boolean);
procedure unary(canAssign: Boolean);
procedure binary(canAssign: Boolean);
procedure literal(canAssign: Boolean);
procedure ParseString(canAssign: Boolean);
procedure variable(canAssign: Boolean);
procedure call(canAssign: Boolean);
procedure dot_(canAssign: Boolean);
procedure subscript_(canAssign: Boolean);
procedure arrayLiteral(canAssign: Boolean);
procedure lambda(canAssign: Boolean);
procedure and_(canAssign: Boolean);
procedure or_(canAssign: Boolean);
procedure beginScope(); inline;
procedure endScope();
function newFunction(MemTracker : pMemTracker) : pObjFunction; inline;
function newNative(func : TNativeFn; arity: Integer; aName: PAnsiChar; MemTracker : pMemTracker) : pObjNative;
function newClosure(func : pObjFunction; MemTracker : pMemTracker) : pObjClosure;
function newUpvalue(slot : pValue; MemTracker : pMemTracker) : pObjUpvalue;
function newArray(MemTracker : pMemTracker) : pObjArray;
function isArray(value : TValue) : boolean; inline;
procedure EnsureArrayCapacity(arr : pObjArray; MemTracker : pMemTracker);inline;
function newDictionary(MemTracker : pMemTracker) : pObjDictionary; inline;
function isDictionary(value : TValue) : boolean; inline;
function HashValue(const value : TValue) : UInt32; inline;
function DictGet(dict : pObjDictionary; const key : TValue; var outValue : TValue) : boolean;inline;
procedure DictSet(dict : pObjDictionary; const key : TValue; const val : TValue; MemTracker : pMemTracker);inline;
function DictDelete(dict : pObjDictionary; const key : TValue) : boolean;inline;
function DictFindEntry(entries : pDictEntry; capacity : integer; const key : TValue) : pDictEntry;inline;
function InvokeDictMethod(dict: pObjDictionary; methodName: pObjString;
  argCount: integer; args: pValue; var outResult: TValue): boolean;
function newRecordType(name : pObjString; fieldCount : integer; fieldNames : ppObjString; MemTracker : pMemTracker) : pObjRecordType;
function newRecord(recType : pObjRecordType; MemTracker : pMemTracker) : pObjRecord;
function isRecordType(value : TValue) : boolean; inline;
function isRecord(value : TValue) : boolean; inline;
function isNativeObject(value : TValue) : boolean; inline;
function newNativeObject(instance : Pointer; classInfo : pNativeClassInfo; MemTracker : pMemTracker) : pObjNativeObject;
function findNativeMethod(classInfo : pNativeClassInfo; methodName : pObjString; out found : TNativeMethodFn) : boolean;
procedure recordDeclaration();
procedure defineNative(const name : AnsiString; func : TNativeFn; arity: Integer = -1);

procedure registerNativeClass(const AName : AnsiString; const AMethods : array of TNativeMethod; ADestructor : TNativeDestructor);
procedure registerNativeClassRTTI(const AName : AnsiString; AClass : TClass; ADestructor : TNativeDestructor = nil);
procedure InjectObject(const name : AnsiString; instance : TObject);
function identifierConstant(const name : TToken) : integer;inline;
function parseVariable(const errorMsg : PAnsiChar) : integer;inline;
procedure defineVariable(global : integer);inline;

//Hash
function HashString(const Key: PAnsiChar; Length: Integer): UInt32;inline;

//Value constructors
function CreateNumber(Value: Double): TValue; inline;
function CreateBoolean(Value: Boolean): TValue; inline;
function CreateNilValue: TValue; inline;
function CreateObject(value : pObj) : TValue; inline;

//Value accessors
function isNumber(const Value: TValue): Boolean; inline;
function isBoolean(const Value: TValue): Boolean; inline;
function isObject(value : TValue) : boolean; inline;
function isNill(const Value: TValue): Boolean; inline;
function isClosure(value : TValue) : boolean; inline;
function GetNumber(Value : TValue) : double; inline;
function GetBoolean(const Value : TValue) : Boolean; inline;
function GetObject(const value : TValue) : pObj; inline;
function IsFalsey(const Value: TValue): Boolean; inline;

//Native helper functions — convenience wrappers for native function authors
function AsAnsiString(value : TValue) : AnsiString; inline;
function AsWideString(value : TValue) : string; inline;
function AsInteger(value : TValue) : Integer; inline;
function CreateStringValue(const s : AnsiString) : TValue;inline;

//Table
procedure InitTable(var Table : pTable; memTracker : pMemTracker); inline;
function TableSet(var Table : pTable; key : pObjString; value : TValue;MemTracker : pMemTracker): boolean;
function TableGet(Table : pTable; key : pObjString; var value : TValue): boolean; inline;
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

    { TOKEN_LEFT_BRACKET }
    (Prefix: arrayLiteral; Infix: subscript_;  Precedence: PREC_CALL),

    { TOKEN_RIGHT_BRACKET }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_COMMA }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_DOT }
    (Prefix: nil;      Infix: dot_;    Precedence: PREC_CALL),

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

    { TOKEN_PERCENT }
    (Prefix: nil;      Infix: binary;  Precedence: PREC_FACTOR),

    { TOKEN_COLON }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

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

    { TOKEN_PLUS_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_MINUS_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_STAR_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_SLASH_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

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
    (Prefix: lambda;   Infix: nil;     Precedence: PREC_NONE),

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

    { TOKEN_RECORD }
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
  lastPatchTarget : integer = -1;
  lastAddOffset   : integer = -1;
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

  NativeRegistry;

{$IFDEF OPCODE_PROFILING}
var
  OpPairCounts: array[0..255, 0..255] of UInt64;
  OpPrevOp: Byte = 255;

procedure DumpOpcodePairs;
type
  TPairEntry = record
    A, B: Byte;
    Count: UInt64;
  end;
var
  pairs: array of TPairEntry;
  i, j, n: Integer;
  temp: TPairEntry;
  nameA, nameB: string;
begin
  // Collect non-zero pairs
  SetLength(pairs, 0);
  for i := 0 to 255 do
    for j := 0 to 255 do
      if OpPairCounts[i, j] > 0 then
      begin
        n := Length(pairs);
        SetLength(pairs, n + 1);
        pairs[n].A := i;
        pairs[n].B := j;
        pairs[n].Count := OpPairCounts[i, j];
      end;

  // Simple insertion sort top-30 (descending by count)
  for i := 0 to High(pairs) do
    for j := i + 1 to High(pairs) do
      if pairs[j].Count > pairs[i].Count then
      begin
        temp := pairs[i];
        pairs[i] := pairs[j];
        pairs[j] := temp;
      end;

  VM.PrintBuilder.Append(sLineBreak);
  VM.PrintBuilder.Append('=== OPCODE PAIR PROFILE (top 30) ===');
  VM.PrintBuilder.Append(sLineBreak);
  if Assigned(VM.OnPrint) then
    VM.OnPrint('=== OPCODE PAIR PROFILE (top 30) ===');
  for i := 0 to Min(29, High(pairs)) do
  begin
    if pairs[i].A <= High(OP_STRINGS) then nameA := OP_STRINGS[pairs[i].A]
    else nameA := 'OP_' + IntToStr(pairs[i].A);
    if pairs[i].B <= High(OP_STRINGS) then nameB := OP_STRINGS[pairs[i].B]
    else nameB := 'OP_' + IntToStr(pairs[i].B);
    VM.PrintBuilder.Append(Format('  %-28s -> %-28s : %d',
      [nameA, nameB, pairs[i].Count]));
    VM.PrintBuilder.Append(sLineBreak);
    if Assigned(VM.OnPrint) then
      VM.OnPrint(Format('  %-28s -> %-28s : %d',
        [nameA, nameB, pairs[i].Count]));
  end;
  VM.PrintBuilder.Append('====================================');
  if Assigned(VM.OnPrint) then
    VM.OnPrint('====================================');
end;
{$ENDIF}



{ ELoxHalt }

constructor ELoxHalt.Create(const AMsg: string; AExitCode: Integer);
begin
  inherited Create(AMsg);
  HaltExitCode := AExitCode;
end;

{ LoxClassAttribute }

constructor LoxClassAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ LoxPropertyAttribute }

constructor LoxPropertyAttribute.Create;
begin
  inherited Create;
  FReadOnly := False;
end;

constructor LoxPropertyAttribute.Create(const AMode: string);
begin
  inherited Create;
  FReadOnly := SameText(AMode, 'readonly');
end;

{ LoxMethodAttribute }

constructor LoxMethodAttribute.Create;
begin
  inherited Create;
end;

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

 {$IFOPT C+}
procedure AssertTable(Table : pTable);
begin
  Assert(assigned(Table), 'Table is not assigned');
end;
{$ENDIF}

{$IFOPT C+}
procedure AssertTableConsistency(Table : pTable);

var
  i, liveCount, tombstoneCount: integer;
  entry: pEntry;

begin

  AssertTable(Table);
  if Table.CurrentCapacity = 0 then
  begin
    Assert(Table.Count = 0, 'AssertTableConsistency: capacity=0 but count<>0');
    Assert(Table.Entries = nil, 'AssertTableConsistency: capacity=0 but entries<>nil');
    Exit;
  end;

  Assert(Table.Entries <> nil, 'AssertTableConsistency: capacity>0 but entries=nil');
  Assert(Table.CurrentCapacity >= TABLE_START_CAPACITY, 'AssertTableConsistency: capacity below minimum');
  Assert(Table.Count >= 0, 'AssertTableConsistency: negative count');
  Assert(Table.Count <= Table.CurrentCapacity,
    'AssertTableConsistency: count exceeds capacity');

  // Walk entries and count live + tombstone
  liveCount := 0;
  tombstoneCount := 0;
  for i := 0 to Table.CurrentCapacity - 1 do
  begin
    entry := @Table.Entries[i];
    if entry.key <> nil then
    begin
      // Live entry ? key must be a valid string object
      Assert(entry.key^.Obj.ObjectKind = okString,
        'AssertTableConsistency: entry key is not a string object');
      Assert(entry.key^.length >= 0,
        'AssertTableConsistency: entry key has negative length');
      Inc(liveCount);
    end
    else
    begin
      // key=nil: either empty (value=nil) or tombstone (value=true boolean)
      if not isNill(entry.value) then
        Inc(tombstoneCount);
    end;
  end;

  Assert(liveCount + tombstoneCount = Table.Count,
    'AssertTableConsistency: live (' + IntToStr(liveCount) +
    ') + tombstones (' + IntToStr(tombstoneCount) +
    ') <> Table.Count (' + IntToStr(Table.Count) + ')');

  // Count + tombstones must not exceed capacity (no overflows possible)
  Assert(liveCount + tombstoneCount <= Table.CurrentCapacity,
    'AssertTableConsistency: live + tombstones exceed capacity');

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

procedure AssertStackCountIsZero(Stack : pStack);
begin
  Assert(Stack.StackTop = Stack.Values, 'Stack count should be 0');
end;

procedure AssertStackCapacityIsGreaterThanZero(Stack : pStack);
begin
  Assert(Stack.CapacityEnd > Stack.Values, 'Stack capacity is zero, cannot grow');
end;

procedure AssertStackTopEqualsValues(Stack : pStack);
begin
  Assert(Stack.StackTop = Stack.Values, 'StackTop should equal Values');
end;

procedure AssertStackCapacityCanGrow(Stack : pStack);
begin
  Assert((Stack.CapacityEnd - Stack.Values) <= MaxInt div GROWTH_FACTOR, 'Stack capacity multiplication would overflow');
end;

procedure AssertStackByteSizeIsSafe(NewCapacity : integer);
begin
  Assert(NewCapacity <= MaxInt div SizeOf(TValue), 'Stack byte size would overflow');
end;

procedure AssertStackIsNotEmpty(Stack : pStack);
begin
  Assert(Stack.StackTop > Stack.Values, 'Stack underflow - count is zero');
end;

procedure AssertStackTopCountConsistent(Stack : pStack);
begin
  Assert(Stack.StackTop >= Stack.Values, 'StackTop below Values');
  Assert(Stack.StackTop <= Stack.CapacityEnd, 'StackTop beyond capacity');
end;

procedure AssertRebaseOffsetIsNonZero(Offset : NativeInt);
begin
  Assert(Offset <> 0, 'pushStack rebase: offset is zero but pointers differ');
end;

procedure AssertFrameCountInBounds(FrameCount : integer);
begin
  Assert((FrameCount >= 0) and (FrameCount <= FRAMES_MAX),
    'pushStack rebase: FrameCount out of bounds');
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

procedure AssertValidObjPointer(obj : pObj; const context : string);
begin
  if obj = nil then Exit;
  Assert(Ord(obj^.ObjectKind) <= Ord(High(TObjectKind)),
    context + ': invalid ObjectKind (corrupt or freed pointer)');
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

{$ENDIF}


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


procedure IncrementBytesAllocated(memTracker : pMemTracker; Amount : integer);inline;
begin
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
  {$ENDIF}
  Inc(memtracker.BytesAllocated, Amount);
  {$IFOPT C+}
    AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
  {$ENDIF}
end;

procedure ClearMem(p: PByte; FromIndex, Count: Integer);
begin
  {$IFOPT C+}
  AssertPointerIsNotNil(p, 'ClearMem');
  AssertIndexIsNotNegative(FromIndex);
  AssertCountIsNotNegative(Count);
  Assert(FromIndex <= High(Integer) - Count, 'mem buffer overflow');
 {$ENDIF}
  FillChar(p[FromIndex], Count, 0);
end;


procedure Allocate(var p: Pointer; OldSize, NewSize: Integer; MemTracker : pMemTracker); inline;
begin
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
  {$ENDIF}
  // --------------------------------------------------------------------------
  // Basic sanity checks on sizes
  // --------------------------------------------------------------------------
   {$IFOPT C+}
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
  {$ENDIF}

  // --------------------------------------------------------------------------
  // Garbage Collection run
  // --------------------------------------------------------------------------
  if (NewSize > OldSize) then
  begin
    {$IFDEF DEBUG_STRESS_GC}
    CollectGarbage;
    AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
    {$ENDIF}
    if MemTracker.BytesAllocated > MemTracker.NextGC then
    begin
      CollectGarbage;
      {$IFOPT C+}
        AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker);
      {$ENDIF}
    end;
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
  {$IFOPT C+}
  if NewSize = 0 then
    AssertPointerIsNil(p, 'Pointer not nil after zero-size allocation');

  // --------------------------------------------------------------------------
  // Non-Zero-size check
  // --------------------------------------------------------------------------
  // If allocation shrinks to zero, the pointer must now be nil
  if NewSize > 0 then
    AssertPointerIsNotNil(p, 'Pointer nil after new size > 0 allocation');
  {$ENDIF}

end;


function AllocateArray(var List: Pointer;  var CurrentCapacity: Integer;  Count, ElemSize: Integer; MemTracker : pMemTracker): Boolean;
var
  NewCapacity: Integer;
  OldSize, NewSize: Integer;
begin
  {$IFOPT C+}
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
  {$ENDIF}

  // ---- Initial allocation --------------------------------------------------
  if CurrentCapacity = 0 then
  begin
    {$IFOPT C+}
    AssertPointerIsNil(List, 'List must be nil when CurrentCapacity is zero');
    // Ensure capacity * element size will not overflow
    Assert(START_CAPACITY <= MaxInt div ElemSize,
      'Initial allocation size exceeds addressable memory');
    {$ENDIF}
    CurrentCapacity := START_CAPACITY;
    NewSize := CurrentCapacity * ElemSize;

    Allocate(List, 0, NewSize, MemTracker);
    Exit(True);
  end;

  // ---- Growth path ---------------------------------------------------------
  {$IFOPT C+}
  AssertPointerIsNotNil(List, 'List is nil with non-zero CurrentCapacity');
  {$ENDIF}

  if Count < CurrentCapacity then
    Exit(False);

   {$IFOPT C+}
  // Ensure capacity growth itself cannot overflow Integer
  Assert(CurrentCapacity <= MaxInt div GROWTH_FACTOR,
    'Array capacity multiplication overflows Integer : ' + inttostr(CurrentCapacity));

  // Ensure logical size limit is not exceeded
  Assert(CurrentCapacity <= MAX_SIZE div GROWTH_FACTOR,
    'Array capacity growth would exceed MAX_SIZE + : ' + inttostr(CurrentCapacity));
  {$ENDIF}
  NewCapacity := CurrentCapacity * GROWTH_FACTOR;

  // Ensure byte-size multiplications are safe
  {$IFOPT C+}
  Assert(CurrentCapacity <= MaxInt div ElemSize,
    'Current array byte size exceeds Integer range');
  Assert(NewCapacity <= MaxInt div ElemSize,
    'Grown array byte size exceeds Integer range');
  {$ENDIF}

  OldSize := CurrentCapacity * ElemSize;
  NewSize := NewCapacity * ElemSize;

  Allocate(List, OldSize, NewSize, MemTracker);
  CurrentCapacity := NewCapacity;

  Result := True;
end;


procedure AllocateString(var p: PObjString; OldSize, NewSize: NativeInt; MemTracker : pMemTracker); inline;
begin
  {$IFOPT C+}
  AssertNewStringIsNillBeforeAllocation(p);
  {$ENDIF}
  Allocate(Pointer(p), OldSize, NewSize, MemTracker);
end;

//note : we do not add a null terminator #0 since we track the length
function CreateString(const S: AnsiString; MemTracker : pMemTracker): PObjString;
var
  Len: Integer;
  NewSize: NativeInt;
  hash: UInt32;
begin
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  {$ENDIF}
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
    pushStack(VM.Stack, StringToValue(Result));
    TableSet(VM.Strings, Result, CreateNilValue, MemTracker);
    Dec(VM.Stack.StackTop);
  end;
  
  // ---- Exit assertions ----
   {$IFOPT C+}
   AssertObjStringIsAssigned(Result);
  Assert(Result^.Length = Len, 'CreateString exit: length mismatch');
  Assert(Result^.Obj.ObjectKind = okString, 'CreateString exit: not a string object');
  {$ENDIF}
end;

procedure FreeString(var obj : pObjString; MemTracker : pMemTracker);
var
 objSize : integer;
begin
    {$IFOPT C+}
  // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);

  // ---- preconditions ----
  AssertObjStringIsAssigned(obj);
  AssertStringLengthIsNotNegative(obj);
  AssertObjectKindIsString(@obj^.Obj);
  {$ENDIF}

  // ---- resize now ----
  objSize := Sizeof(TObjString) + Max(0, obj^.length - 1);  // mimc here size from CreateString (We don't have the string but we do have the length now)
  Allocate(pointer(obj), objSize , 0, MemTracker);
  obj := nil;

  // ---- postconditions ----
   {$IFOPT C+}
   AssertPointerIsNil(obj, 'string pointer not cleared after free');
  assert(Memtracker.BytesAllocated >= 0, 'VM bytes allocated underflow after freeing string.');
  {$ENDIF}
end;


procedure initChunk(var chunk: pChunk; MemTracker : pMemTracker); inline;
begin
  // ---- Test MemTracker ---------------------------------------------------
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);

  AssertPointerIsNil(Chunk, 'Chunk initialization - chunk is not nil');
  {$ENDIF}
  Allocate(pointer(chunk),0, Sizeof(TChunk),MemTracker);

  chunk.Count := 0;
  chunk.CurrentCapacity := 0;
  chunk.Code := nil;
  chunk.Lines := nil;
  chunk.Constants := nil;
  InitValueArray(chunk.Constants,MemTracker);

  // ---- Exit assertions ----
  {$IFOPT C+}
  AssertChunkIsAssigned(chunk);
  Assert(chunk.Count = 0, 'initChunk exit: count should be 0');
  Assert(chunk.CurrentCapacity = 0, 'initChunk exit: capacity should be 0');
  Assert(chunk.Code = nil, 'initChunk exit: code should be nil');
  Assert(chunk.Lines = nil, 'initChunk exit: lines should be nil');
  AssertChunkConstantsIsAssigned(chunk);
  {$ENDIF}
end;

procedure freeChunk(var chunk: pChunk; MemTracker : pMemTracker); inline;
begin
  // ---- Test MemTracker ---------------------------------------------------
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertChunkIsAssigned(Chunk);
  {$ENDIF}


  if (chunk.CurrentCapacity) > 0 then
  begin
    Allocate(pointer(chunk.Code), Chunk.CurrentCapacity * sizeof(Byte), 0,MemTracker);
    {$IFOPT C+}
    AssertPointerIsNil(Chunk.Code, 'Expected Chunk Code to be nil');
    {$ENDIF}
    Allocate(pointer(Chunk.Lines), Chunk.CurrentCapacity * Sizeof(Integer),0,MemTracker);
    {$IFOPT C+}
    AssertPointerIsNil(Chunk.Lines, 'Expected Chunk Lines to be nil');
    {$ENDIF}
  end;

  freeValueArray(chunk.Constants,MemTracker);
  {$IFOPT C+}
   AssertPointerIsNil(Chunk.Constants, 'Expected chunk Constants to be nil');
  {$ENDIF}
  Allocate(pointer(chunk),Sizeof(TChunk),0,MemTracker);
  {$IFOPT C+}
  AssertPointerIsNil(Chunk, 'Expected chunk to be nil');
  {$ENDIF}
end;

procedure writeChunk(chunk: pChunk; value: byte; Line : Integer; MemTracker : pMemTracker);
var
  currentCap : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertChunkIsAssigned(chunk);
  AssertLineIsNotNegative(Line);
  {$ENDIF}

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
   {$IFOPT C+}
  AssertChunkIsAssigned(chunk);
  AssertChunkCodeIsAssigned(chunk);
  Assert(chunk.Count > 0, 'writeChunk exit: chunk count should be > 0');
  Assert(chunk.Code[chunk.Count - 1] = value, 'writeChunk exit: value not written correctly');
  {$ENDIF}
end;


procedure initValueArray(var ValueArray : pValueArray; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsNilBeforeInit(ValueArray);
  {$ENDIF}

  allocate(pointer(ValueArray),0,Sizeof(TValueArray),MemTracker);

  ValueArray.Count := 0;

  ValueArray.CurrentCapacity := 0;

  ValueArray.Values := nil;

  // ---- Exit assertions ----
  {$IFOPT C+}
  AssertValueArrayIsAssigned(ValueArray);
  Assert(ValueArray.Count = 0, 'initValueArray exit: count should be 0');
  Assert(ValueArray.CurrentCapacity = 0, 'initValueArray exit: capacity should be 0');
  Assert(ValueArray.Values = nil, 'initValueArray exit: values should be nil');
  {$ENDIF}
end;

procedure writeValueArray(ValueArray : pValueArray; Value : TValue; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsAssigned(ValueArray);
  AssertValueArrayCount(ValueArray);
  {$ENDIF}

  AllocateArray(pointer(ValueArray.Values), ValueArray.CurrentCapacity, ValueArray.Count,sizeof(TValue),MemTracker);

  ValueArray.Values[ValueArray.Count] := value;

  Inc(ValueArray.Count);

  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertValueArrayIsAssigned(ValueArray);
  AssertValuesIsAssigned(ValueArray.Values);
  Assert(ValueArray.Count > 0, 'writeValueArray exit: count should be > 0');
  {$ENDIF}
end;

procedure FreeValues(var Values : pValue; Capacity : integer; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  {$IFOPT C+}
   AssertMemTrackerIsNotNil(MemTracker);
  AssertValuesIsAssigned(Values);
  AssertCapacityIsPositive(Capacity);
  {$ENDIF}
  Allocate(pointer(Values), Capacity * Sizeof(TValue),0,MemTracker);  //Note here that the references to objects will be free'd externally
   {$IFOPT C+}
  AssertPointerIsNil(Values, 'FreeValues - values not nil after free');
  {$ENDIF}
end;

procedure freeValueArray(var ValueArray : pValueArray; MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsAssigned(ValueArray);
  {$ENDIF}
  if ValueArray.Values <> nil then
    FreeValues(ValueArray.Values,ValueArray.CurrentCapacity,MemTracker);
  Allocate(pointer(ValueArray),Sizeof(TValueArray),0,MemTracker);
  ValueArray := nil;

  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertPointerIsNil(ValueArray, 'freeValueArray exit: ValueArray should be nil');
  {$ENDIF}
end;

procedure FillNilValues(p: pValue; Count: NativeInt); inline;
var
  i: NativeInt;
begin
  for i := 0 to Count - 1 do
    p[i] := NIL_VAL;
end;

procedure InitStack(var Stack : pStack;MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsNilBeforeInit(Stack);
  {$ENDIF}
  // Use raw memory ? stack must never trigger GC (same pattern as GrayStack)
  GetMem(Stack, SizeOf(TStack));
  GetMem(Stack.Values, START_CAPACITY * SizeOf(TValue));
  FillNilValues(Stack.Values, START_CAPACITY);
  Stack.StackTop := Stack.Values;
  Stack.CapacityEnd := Stack.Values + START_CAPACITY;
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertStackIsAssigned(Stack);
  AssertStackCountIsZero(Stack);
  AssertStackCapacityIsGreaterThanZero(Stack);
  AssertStackValuesIsAssigned(Stack);
  AssertStackTopEqualsValues(Stack);
  {$ENDIF}
end;

procedure FreeStack(var Stack : pStack;MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  {$ENDIF}
  // Raw memory ? matches InitStack/pushStack (not tracked by BytesAllocated)
  if Stack.Values <> nil then
    FreeMem(Stack.Values);
  Stack.Values := nil;
  FreeMem(Stack);
  Stack := nil;
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertPointerIsNil(Stack, 'FreeStack exit: Stack should be nil');
  {$ENDIF}
end;

// pushStackGrow - slow path for pushStack.
// Called only when StackTop >= Values + CurrentCapacity. Handles the realloc,
// rebase of frame.slots and open upvalues, and finally writes the value and
// increments. Kept out-of-line so the fast path (pushStack) stays tiny enough
// for Delphi to inline at every call site without code bloat.
//
// Defensive `LocalValue := value` copy lives HERE because only this path can
// realloc the stack buffer ? callers routinely pass references into
// Stack.Values itself (e.g. OP_GET_LOCAL), so we must snapshot before the
// ReallocMem moves the buffer.
procedure pushStackGrow(var stack : pStack;const value : TValue);
var
  NewCapacity : integer;
  OldCapacity : integer;
  OldValues   : pValue;
  Offset      : NativeInt;
  i           : integer;
  upval       : pObjUpvalue;
  LocalValue  : TValue;
begin

  {$IFOPT C+}
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  AssertStackCapacityIsGreaterThanZero(Stack);
  {$ENDIF}

  // Snapshot value before any realloc (see header comment).
  LocalValue := value;

  // Grow using raw ReallocMem ? must never trigger GC so push/pop
  // can safely protect unrooted objects (same pattern as book's fixed stack)
  // Enforce hard upper bound. Asserts compile out in release; this raise
  // ensures runaway recursion / infinite push surfaces as a clean
  // "Stack overflow" instead of an eventual OOM crash. Caught at the
  // CompileAndRun boundary and converted to INTERPRET_RUNTIME_ERROR.
  OldCapacity := Stack.CapacityEnd - Stack.Values;  // element count
  //if OldCapacity >= STACK_MAX then
    //raise ELoxRuntimeError.CreateFmt('Stack overflow (max %d slots).', [STACK_MAX]);
  {$IFOPT C+}
  AssertStackCapacityCanGrow(Stack);
  {$ENDIF}
  NewCapacity := OldCapacity * GROWTH_FACTOR;
  if NewCapacity > STACK_MAX then
    NewCapacity := STACK_MAX;
  {$IFOPT C+}
  AssertStackByteSizeIsSafe(NewCapacity);
  {$ENDIF}
  OldValues := Stack.Values;
  ReallocMem(Stack.Values, NewCapacity * SizeOf(TValue));
  {$IFOPT C+}
  AssertStackValuesIsAssigned(Stack);
  {$ENDIF}
  // Zero new portion
  FillNilValues(@Stack.Values[OldCapacity], NewCapacity - OldCapacity);
  Stack.CapacityEnd := Stack.Values + NewCapacity;
  {$IFOPT C+}
  AssertStackCapacityIsGreaterThanZero(Stack);
  {$ENDIF}
  // If the buffer moved, rebase StackTop, frame.slots and open upvalue locations
  if Stack.Values <> OldValues then
  begin
    Offset := NativeInt(Stack.Values) - NativeInt(OldValues);
    {$IFOPT C+}
    AssertRebaseOffsetIsNonZero(Offset);
    {$ENDIF}
    // Rebase StackTop
    Inc(PByte(Stack.StackTop), Offset);
    // Rebase call frame slot pointers
    if (VM <> nil) and (Stack = VM.Stack) then
    begin
      {$IFOPT C+}
      AssertFrameCountInBounds(VM.FrameCount);
      {$ENDIF}
      for i := 0 to VM.FrameCount - 1 do
        Inc(PByte(VM.Frames[i].slots), Offset);
      // Rebase open upvalue location pointers
      upval := VM.OpenUpvalues;
      while upval <> nil do
      begin
        Inc(PByte(upval^.location), Offset);
        upval := upval^.next;
      end;
    end;
  end;

  Stack.StackTop^ := LocalValue;
  Inc(Stack.StackTop);

  {$IFOPT C+}
  AssertStackIsAssigned(Stack);
  AssertStackIsNotEmpty(Stack);
  AssertStackTopIsNotNil(Stack);
  {$ENDIF}
end;

// pushStack - fast-path inline wrapper.
// Hot path (StackTop < capacity end): 2 stores + 1 pointer inc +
// 1 predicted-not-taken branch. No realloc, so `value` cannot alias a
// memory region that's about to move - direct write is safe without a
// defensive copy. Cold path delegates to pushStackGrow.
procedure pushStack(var stack : pStack;const value : TValue);
begin
  if Stack.StackTop < Stack.CapacityEnd then
  begin
    Stack.StackTop^ := value;
    Inc(Stack.StackTop);
  end
  else
    pushStackGrow(Stack, value);
end;


//End memory creation routines






function isObject(value : TValue) : boolean; inline;
begin
  result := (value and OBJ_TAG) = OBJ_TAG;
end;

function GetObject(const value : TValue) : pObj; inline;
begin
  result := pObj(NativeUInt(value and not OBJ_TAG));
end;

function CreateObject(value : pObj) : TValue; inline;
var
  ptr: UInt64;
begin
  {$IFOPT C+}
  AssertPointerIsNotNil(value, 'CreateObject - object value');
  {$ENDIF}

  ptr := UInt64(NativeUInt(value));

  {$IFOPT C+}
  // NaN-boxing assumes canonical lower-half userspace pointers (lower 48 bits).
  // Object pointers must not overlap the reserved tag region (upper 16 bits).
  Assert((ptr and OBJ_TAG) = 0,
    'CreateObject: pointer incompatible with NaN boxing ($' +
    IntToHex(ptr, 16) + ')');
  {$ENDIF}

  Result := OBJ_TAG or ptr;
end;


function isString(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okString;
end;

function isFunction(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okFunction;
end;

function isNative(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okNative;
end;

function isClosure(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okClosure;
end;

function newFunction(MemTracker : pMemTracker) : pObjFunction; inline;
begin
   {$IFOPT C+}
   AssertMemTrackerIsNotNil(MemTracker);
   {$ENDIF}
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
   {$IFOPT C+}
  Assert(Result <> nil, 'newFunction exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okFunction, 'newFunction exit: ObjectKind should be okFunction');
  AssertChunkIsAssigned(Result^.chunk);
  Assert(Result^.arity = 0, 'newFunction exit: arity should be 0');
  {$ENDIF}
end;

function newNative(func : TNativeFn; arity: Integer; aName: PAnsiChar; MemTracker : pMemTracker) : pObjNative;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(Assigned(func), 'newNative: func is not assigned');
  {$ENDIF}
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjNative), MemTracker);
  Result^.Obj.ObjectKind := okNative;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.func := func;
  Result^.arity := arity;
  Result^.name := aName;
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Result <> nil, 'newNative exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okNative, 'newNative exit: ObjectKind should be okNative');
  {$ENDIF}
end;

function newClosure(func : pObjFunction; MemTracker : pMemTracker) : pObjClosure;
var
  upvals : pUpvalueArray;
  i : integer;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(func <> nil, 'newClosure: func is nil');
  {$ENDIF}
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
  {$IFOPT C+}
  Assert(Result <> nil, 'newClosure exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okClosure, 'newClosure exit: ObjectKind should be okClosure');
  Assert(Result^.func = func, 'newClosure exit: func does not match input');
  {$ENDIF}
end;

function newUpvalue(slot : pValue; MemTracker : pMemTracker) : pObjUpvalue;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(slot <> nil, 'newUpvalue: slot is nil');
  {$ENDIF}
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
   {$IFOPT C+}
  Assert(Result <> nil, 'newUpvalue exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okUpvalue, 'newUpvalue exit: ObjectKind should be okUpvalue');
  Assert(Result^.location = slot, 'newUpvalue exit: location does not match input slot');
  {$ENDIF}
end;

function newArray(MemTracker : pMemTracker) : pObjArray;
begin
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  {$ENDIF}
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjArray), MemTracker);
  Result^.Obj.ObjectKind := okArray;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.Count := 0;
  Result^.Capacity := 0;
  Result^.Elements := nil;
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Result <> nil, 'newArray exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okArray, 'newArray exit: ObjectKind should be okArray');
  Assert(Result^.Count = 0, 'newArray exit: Count should be 0');
  {$ENDIF}
end;

function isArray(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okArray;
end;

procedure EnsureArrayCapacity(arr : pObjArray; MemTracker : pMemTracker);
var
  newCap, oldSize, newSize: Integer;
begin
  if arr^.Count < arr^.Capacity then Exit;
  if arr^.Capacity = 0 then newCap := ARRAY_START_CAPACITY else newCap := arr^.Capacity * GROWTH_FACTOR;
  oldSize := arr^.Capacity * SizeOf(TValue);
  newSize := newCap * SizeOf(TValue);
  Allocate(Pointer(arr^.Elements), oldSize, newSize, MemTracker);
  arr^.Capacity := newCap;
end;

function newDictionary(MemTracker : pMemTracker) : pObjDictionary;
begin
   {$IFOPT C+}
   AssertMemTrackerIsNotNil(MemTracker);
   {$ENDIF}
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjDictionary), MemTracker);
  Assert(Result <> nil, 'newDictionary: allocation returned nil');
  Result^.Obj.ObjectKind := okDictionary;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.Count := 0;
  Result^.Tombstones := 0;
  Result^.Capacity := 0;
  Result^.Entries := nil;
  AddToCreatedObjects(pObj(Result), MemTracker);
end;

function isDictionary(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okDictionary;
end;

function HashValue(const value : TValue) : UInt32;
var
  bits : UInt64;
begin
  if isNumber(value) then
  begin
    // Canonicalize -0.0 to +0.0 so they hash identically (both compare equal via =)
    if GetNumber(value) = 0.0 then
      bits := 0
    else
      bits := value;  // raw bits of the double
    Result := UInt32(bits xor (bits shr 32));
  end
  else if isBoolean(value) then
  begin
    if GetBoolean(value) then Result := 1 else Result := 0;
  end
  else if isNill(value) then
    Result := 2
  else if isObject(value) then
  begin
    Assert(GetObject(value) <> nil, 'HashValue: ObjValue is nil');
    case GetObject(value)^.ObjectKind of
      okString: Result := pObjString(GetObject(value))^.hash;
    else
      // Identity hash for other objects (pointer-based)
      Result := UInt32(NativeUInt(GetObject(value)) xor (NativeUInt(GetObject(value)) shr 32));
    end;
  end
  else
    Result := 0;
end;

function DictFindEntry(entries : pDictEntry; capacity : integer; const key : TValue) : pDictEntry;inline;
var
  index : UInt32;
  entry : pDictEntry;
  tombstone : pDictEntry;
  probeCount : integer;
begin
  // ---- Entry assertions ----
  Assert(entries <> nil, 'DictFindEntry: entries is nil');
  Assert(capacity > 0, 'DictFindEntry: capacity must be > 0');
  Assert((capacity and (capacity - 1)) = 0, 'DictFindEntry: capacity must be a power of 2');

  index := HashValue(key) and UInt32(capacity - 1);
  tombstone := nil;
  for probeCount := 0 to capacity - 1 do
  begin
    entry := @entries[index];
    if not entry^.occupied and not entry^.tombstone then
    begin
      // Empty slot ? return tombstone if we passed one, else this slot
      if tombstone <> nil then
        Result := tombstone
      else
        Result := entry;
      Exit;
    end
    else if entry^.tombstone then
    begin
      Assert(not entry^.occupied, 'DictFindEntry: tombstone has occupied=true');
      // Remember first tombstone
      if tombstone = nil then
        tombstone := entry;
    end
    else if ValuesEqual(entry^.key, key) then
    begin
      // Found the key
      Result := entry;
      Exit;
    end;
    index := (index + 1) and UInt32(capacity - 1);
  end;
  // Should never reach here ? load factor guarantees an empty slot exists
  Assert(false, 'DictFindEntry: probed entire table without finding slot ? table is full');
  Result := nil;
end;

procedure DictGrow(dict : pObjDictionary; MemTracker : pMemTracker); inline;
var
  newCapacity, i, oldCount : integer;
  newEntries : pDictEntry;
  entry, dest : pDictEntry;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  Assert(dict <> nil, 'DictGrow: dict is nil');
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(dict^.Count >= 0, 'DictGrow: count is negative');
  Assert(dict^.Tombstones >= 0, 'DictGrow: tombstones is negative');
  Assert(dict^.Count + dict^.Tombstones <= dict^.Capacity,
    'DictGrow: count + tombstones exceeds capacity');
  {$ENDIF}

  oldCount := dict^.Count;

  if dict^.Capacity < TABLE_START_CAPACITY then
    newCapacity := TABLE_START_CAPACITY
  else
    newCapacity := dict^.Capacity * GROWTH_FACTOR;

   {$IFOPT C+}
  Assert(newCapacity > dict^.Count, 'DictGrow: new capacity must exceed current count');
  {$ENDIF}

  newEntries := nil;
  Allocate(Pointer(newEntries), 0, newCapacity * SizeOf(TDictEntry), MemTracker);

   {$IFOPT C+}
  Assert(newEntries <> nil, 'DictGrow: allocation returned nil');
  {$ENDIF}

  // Rehash existing entries (skip tombstones ? they are discarded)
  dict^.Count := 0;
  for i := 0 to dict^.Capacity - 1 do
  begin
    entry := @dict^.Entries[i];
    if not entry^.occupied then Continue;

    dest := DictFindEntry(newEntries, newCapacity, entry^.key);
    dest^.key := entry^.key;
    dest^.value := entry^.value;
    dest^.occupied := true;
    dest^.tombstone := false;
    Inc(dict^.Count);
  end;

  // ---- Post-rehash assertions ----
   {$IFOPT C+}
  Assert(dict^.Count = oldCount, 'DictGrow: count mismatch after rehash');
  {$ENDIF}

  // Free old entries
  if dict^.Entries <> nil then
    Allocate(Pointer(dict^.Entries), dict^.Capacity * SizeOf(TDictEntry), 0, MemTracker);

  dict^.Entries := newEntries;
  dict^.Capacity := newCapacity;
  dict^.Tombstones := 0;

  // ---- Exit assertions ----
  {$IFOPT C+}
  Assert(dict^.Count + dict^.Tombstones <= dict^.Capacity,
    'DictGrow exit: count + tombstones exceeds new capacity');
  {$ENDIF}
end;

function DictGet(dict : pObjDictionary; const key : TValue; var outValue : TValue) : boolean; inline;
var
  entry : pDictEntry;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  Assert(dict <> nil, 'DictGet: dict is nil');
  Assert(dict^.Count >= 0, 'DictGet: count is negative');
  Assert(dict^.Tombstones >= 0, 'DictGet: tombstones is negative');
  {$ENDIF}

  if dict^.Count = 0 then
  begin
    Result := false;
    Exit;
  end;

   {$IFOPT C+}
  Assert(dict^.Entries <> nil, 'DictGet: entries nil with count > 0');
  Assert(dict^.Capacity > 0, 'DictGet: capacity 0 with count > 0');
  {$ENDIF}

  entry := DictFindEntry(dict^.Entries, dict^.Capacity, key);
  if not entry^.occupied then
  begin
    Result := false;
    Exit;
  end;
  outValue := entry^.value;
  Result := true;
end;

procedure DictSet(dict : pObjDictionary; const key : TValue; const val : TValue; MemTracker : pMemTracker);
var
  entry : pDictEntry;
  isNewKey : boolean;
  oldCount : integer;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  Assert(dict <> nil, 'DictSet: dict is nil');
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(dict^.Count >= 0, 'DictSet: count is negative');
  Assert(dict^.Tombstones >= 0, 'DictSet: tombstones is negative');
  {$ENDIF}

  oldCount := dict^.Count;

  // Grow if load factor (count + tombstones) > 75%
  if dict^.Count + dict^.Tombstones + 1 > Integer(Trunc(dict^.Capacity * TABLE_MAX_LOAD)) then
    DictGrow(dict, MemTracker);

  Assert(dict^.Entries <> nil, 'DictSet: entries nil after grow');
  Assert(dict^.Capacity > 0, 'DictSet: capacity 0 after grow');

  entry := DictFindEntry(dict^.Entries, dict^.Capacity, key);
  isNewKey := not entry^.occupied;
  if isNewKey then
  begin
    if entry^.tombstone then
      Dec(dict^.Tombstones);  // reusing a tombstone slot
    Inc(dict^.Count);
  end;
  entry^.key := key;
  entry^.value := val;
  entry^.occupied := true;
  entry^.tombstone := false;

  // ---- Exit assertions ----
  Assert(dict^.Count >= oldCount, 'DictSet exit: count decreased');
  Assert(dict^.Count <= oldCount + 1, 'DictSet exit: count increased by more than 1');
  Assert(dict^.Count + dict^.Tombstones <= dict^.Capacity,
    'DictSet exit: count + tombstones exceeds capacity');
  Assert(dict^.Tombstones >= 0, 'DictSet exit: tombstones went negative');
end;

function DictDelete(dict : pObjDictionary; const key : TValue) : boolean;
var
  entry : pDictEntry;
begin
  // ---- Entry assertions ----
  Assert(dict <> nil, 'DictDelete: dict is nil');
  Assert(dict^.Count >= 0, 'DictDelete: count is negative');
  Assert(dict^.Tombstones >= 0, 'DictDelete: tombstones is negative');

  if dict^.Count = 0 then
  begin
    Result := false;
    Exit;
  end;

  Assert(dict^.Entries <> nil, 'DictDelete: entries nil with count > 0');
  Assert(dict^.Capacity > 0, 'DictDelete: capacity 0 with count > 0');

  entry := DictFindEntry(dict^.Entries, dict^.Capacity, key);
  if not entry^.occupied then
  begin
    Result := false;
    Exit;
  end;
  // Place a tombstone
  entry^.occupied := false;
  entry^.tombstone := true;
  Dec(dict^.Count);
  Inc(dict^.Tombstones);
  Result := true;

  // ---- Exit assertions ----
  Assert(dict^.Count >= 0, 'DictDelete exit: count went negative');
  Assert(dict^.Count + dict^.Tombstones <= dict^.Capacity,
    'DictDelete exit: count + tombstones exceeds capacity');
end;

function isRecordType(value : TValue) : boolean; inline;
begin
  result := isObject(Value) and (GetObject(Value) <> nil) and (GetObject(Value).ObjectKind = okRecordType);
end;

function isRecord(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okRecord;
end;

function newRecordType(name : pObjString; fieldCount : integer; fieldNames : ppObjString; MemTracker : pMemTracker) : pObjRecordType;
begin
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(name <> nil, 'newRecordType: name is nil');
  Assert(fieldCount >= 0, 'newRecordType: fieldCount is negative');
  {$ENDIF}
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjRecordType), MemTracker);
  Result^.Obj.ObjectKind := okRecordType;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.name := name;
  Result^.fieldCount := fieldCount;
  Result^.fieldNames := fieldNames;
  AddToCreatedObjects(pObj(Result), MemTracker);

   {$IFOPT C+}
  Assert(Result <> nil, 'newRecordType exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okRecordType, 'newRecordType exit: ObjectKind should be okRecordType');
  {$ENDIF}
end;

function newRecord(recType : pObjRecordType; MemTracker : pMemTracker) : pObjRecord;
var
  i : integer;
  fields : pValue;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(recType <> nil, 'newRecord: recType is nil');
  {$ENDIF}
  // Allocate the fields array first (like newClosure allocates upvals first)
  // so the second Allocate cannot trigger GC with an untracked parent object
  fields := nil;
  if recType^.fieldCount > 0 then
  begin
    Allocate(Pointer(fields), 0, recType^.fieldCount * SizeOf(TValue), MemTracker);
    for i := 0 to recType^.fieldCount - 1 do
      fields[i] := CreateNilValue;
  end;
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjRecord), MemTracker);
  Result^.Obj.ObjectKind := okRecord;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.recordType := recType;
  Result^.fields := fields;
  AddToCreatedObjects(pObj(Result), MemTracker);

   {$IFOPT C+}
  Assert(Result <> nil, 'newRecord exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okRecord, 'newRecord exit: ObjectKind should be okRecord');
  {$ENDIF}
end;

function findFieldIndex(recType : pObjRecordType; name : pObjString) : integer;
var
  i : integer;
begin
  Assert(recType <> nil, 'findFieldIndex: recType is nil');
  Assert(name <> nil, 'findFieldIndex: name is nil');
  for i := 0 to recType^.fieldCount - 1 do
  begin
    if StringsEqual(recType^.fieldNames[i], name) then
      Exit(i);
  end;
  Result := -1;
end;

function isNativeObject(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okNativeObject;
end;

function newNativeObject(instance : Pointer; classInfo : pNativeClassInfo; MemTracker : pMemTracker) : pObjNativeObject;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(instance <> nil, 'newNativeObject: instance is nil');
  Assert(classInfo <> nil, 'newNativeObject: classInfo is nil');
  {$ENDIF}
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjNativeObject), MemTracker);
  Result^.Obj.ObjectKind := okNativeObject;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.instance := instance;
  Result^.classInfo := classInfo;
  Result^.ownsInstance := true;  // VM-created objects are owned by default
  AddToCreatedObjects(pObj(Result), MemTracker);

   {$IFOPT C+}
  Assert(Result <> nil, 'newNativeObject exit: Result is nil');
  {$ENDIF}
end;

function findNativeMethod(classInfo : pNativeClassInfo; methodName : pObjString; out found : TNativeMethodFn) : boolean;
var
  i : integer;
begin
  for i := 0 to High(classInfo^.methods) do
    if ObjStringEqualsAnsi(methodName, classInfo^.methods[i].name) then
    begin
      found := classInfo^.methods[i].fn;
      Exit(True);
    end;
  found := nil;
  Result := False;
end;

// ---- RTTI value marshaling ----

var
  RttiCtx : TRttiContext;



function DelphiValueToLox(const V: System.Rtti.TValue; MemTracker: pMemTracker): TValue;
var
  s: AnsiString;
  childObj: TObject;
  childClassName: AnsiString;
  childClassInfo: pNativeClassInfo;
  childNative: pObjNativeObject;
  // Set marshaling
  setData: PByte;
  setByteIndex, setBitIndex: Integer;
  baseTypeInfo: PTypeInfo;
  baseTypeData: PTypeData;
  bit: Integer;
  arr: pObjArray;
  elemStr: pObjString;
  // Dynamic array marshaling
  dynLen: Integer;
  dynArr: pObjArray;
  dynElem: System.Rtti.TValue;
  dynI: Integer;
begin
  if V.IsEmpty then
  begin
    // A nil dynamic array has valid TypeInfo but nil data ? return empty Lox array
    if (V.TypeInfo <> nil) and (V.Kind = tkDynArray) then
    begin
      Result := CreateObject(pObj(newArray(MemTracker)));
      Exit;
    end;
    Exit(CreateNilValue);
  end;

  case V.Kind of
    tkInteger, tkInt64:
      Result := CreateNumber(V.AsExtended);
    tkFloat:
      Result := CreateNumber(V.AsExtended);
    tkEnumeration:
    begin
      if V.TypeInfo = TypeInfo(Boolean) then
        Result := CreateBoolean(V.AsBoolean)
      else
      begin
        // Return the symbolic enum name as a string (e.g. 'bsSizeable')
        s := AnsiString(GetEnumName(V.TypeInfo, V.AsOrdinal));
        Result := StringToValue(CreateString(s, MemTracker));
      end;
    end;
    tkSet:
    begin
      // Marshal a Delphi set to a Lox array of enum-name strings.
      // We read raw bytes to handle sets of any size (up to 256 bits / 32 bytes).
      setData := V.GetReferenceToRawData;

      baseTypeInfo := GetTypeData(V.TypeInfo)^.CompType^;
      baseTypeData := GetTypeData(baseTypeInfo);

      arr := newArray(MemTracker);
      // Protect array from GC during element allocation
      pushStack(VM.Stack, CreateObject(pObj(arr)));

      for bit := baseTypeData^.MinValue to baseTypeData^.MaxValue do
      begin
        setByteIndex := bit div 8;
        setBitIndex  := bit mod 8;
        if (setData[setByteIndex] and (1 shl setBitIndex)) <> 0 then
        begin
          s := AnsiString(GetEnumName(baseTypeInfo, bit));
          elemStr := CreateString(s, MemTracker);
          EnsureArrayCapacity(arr, VM.MemTracker);
          arr^.Elements[arr^.Count] := StringToValue(elemStr);
          Inc(arr^.Count);
        end;
      end;

      Dec(VM.Stack.StackTop); // pop GC protection
      Result := CreateObject(pObj(arr));
    end;
    tkDynArray:
    begin
      // Marshal a Delphi dynamic array to a Lox array.
      dynLen := V.GetArrayLength;
      dynArr := newArray(MemTracker);
      // Protect array from GC during element allocation
      pushStack(VM.Stack, CreateObject(pObj(dynArr)));

      for dynI := 0 to dynLen - 1 do
      begin
        dynElem := V.GetArrayElement(dynI);
        EnsureArrayCapacity(dynArr, VM.MemTracker);
        dynArr^.Elements[dynArr^.Count] := DelphiValueToLox(dynElem, MemTracker);
        Inc(dynArr^.Count);
      end;

      Dec(VM.Stack.StackTop); // pop GC protection
      Result := CreateObject(pObj(dynArr));
    end;
    tkString, tkLString, tkWString, tkUString:
    begin
      s := AnsiString(V.AsString);
      Result := StringToValue(CreateString(s, MemTracker));
    end;
    tkChar, tkWChar:
    begin
      s := AnsiString(V.AsString);
      Result := StringToValue(CreateString(s, MemTracker));
    end;
    tkClass:
    begin
      childObj := V.AsObject;
      if childObj = nil then
        Exit(CreateNilValue);
      // Auto-register the class if not already known
      childClassName := AnsiString(childObj.ClassName);
      childClassInfo := findNativeClass(childClassName);
      if childClassInfo = nil then
      begin
        registerNativeClassRTTI(childClassName, childObj.ClassType);
        childClassInfo := findNativeClass(childClassName);
      end;
      childNative := newNativeObject(Pointer(childObj), childClassInfo, MemTracker);
      childNative^.ownsInstance := false;  // parent object owns the child
      Result := CreateObject(pObj(childNative));
    end;
  else
  begin
    runtimeError(Format('Cannot marshal Delphi type ''%s'' (kind %s) to Lox.',
      [String(V.TypeInfo.Name), GetEnumName(TypeInfo(TTypeKind), Ord(V.Kind))]));
    Result := CreateNilValue;
  end;
  end;
end;

function LoxValueToDelphi(const V: TValue; TargetType: PTypeInfo): System.Rtti.TValue;
var
  s: string;
  // Enum marshaling
  enumOrd: Integer;
  // Set marshaling (single string)
  singleOrd: Integer;
  setBaseInfo: PTypeInfo;
  // Set marshaling (array of strings)
  setArr: pObjArray;
  setBaseInfo2: PTypeInfo;
  setI: Integer;
  setElemOrd: Integer;
  setElemName: string;
  // Set marshaling (shared byte buffer for arbitrary-size sets)
  setBuf: array[0..31] of Byte;  // 256 bits max
  // Dynamic array marshaling
  srcArr: pObjArray;
  dynArrLen: NativeInt;
  dynResult: System.Rtti.TValue;
  dynElemType: PTypeInfo;
  dynJ: Integer;
begin
  if isNumber(V) then
  begin
    if TargetType = nil then
      Exit(System.Rtti.TValue.From<Double>(GetNumber(V)));
    case TargetType^.Kind of
      tkInteger:
        Result := System.Rtti.TValue.From<Integer>(Trunc(GetNumber(V)));
      tkInt64:
        Result := System.Rtti.TValue.From<Int64>(Trunc(GetNumber(V)));
      tkFloat:
        Result := System.Rtti.TValue.From<Double>(GetNumber(V));
      tkEnumeration:
        System.Rtti.TValue.Make(Trunc(GetNumber(V)), TargetType, Result);
    else
      Result := System.Rtti.TValue.From<Double>(GetNumber(V));
    end;
  end
  else if isBoolean(V) then
  begin
    if (TargetType <> nil) and (TargetType^.Kind = tkEnumeration) and
       (TargetType = TypeInfo(Boolean)) then
      Result := System.Rtti.TValue.From<Boolean>(GetBoolean(V))
    else
      Result := System.Rtti.TValue.From<Boolean>(GetBoolean(V));
  end
  else if isNill(V) then
    Result := System.Rtti.TValue.Empty
  else if isObject(V) then
  begin
      if isString(V) then
      begin
        s := String(ObjStringToAnsiString(pObjString(GetObject(V))));
        // String -> enum by name (e.g. 'bsSizeable' -> TBorderStyle)
        if (TargetType <> nil) and (TargetType^.Kind = tkEnumeration) then
        begin
          enumOrd := GetEnumValue(TargetType, s);
          if enumOrd < 0 then
          begin
            runtimeError(Format('Invalid enum value ''%s'' for type ''%s''.',
              [s, String(TargetType^.Name)]));
            Result := System.Rtti.TValue.Empty;
          end
          else
            System.Rtti.TValue.Make(enumOrd, TargetType, Result);
        end
        else if (TargetType <> nil) and (TargetType^.Kind = tkSet) then
        begin
          // Single string -> set with one element
          setBaseInfo := GetTypeData(TargetType)^.CompType^;
          singleOrd := GetEnumValue(setBaseInfo, s);
          if singleOrd < 0 then
          begin
            runtimeError(Format('Invalid set element ''%s'' for type ''%s''.',
              [s, String(TargetType^.Name)]));
            Result := System.Rtti.TValue.Empty;
          end
          else
          begin
            FillChar(setBuf, SizeOf(setBuf), 0);
            setBuf[singleOrd div 8] := setBuf[singleOrd div 8] or (1 shl (singleOrd mod 8));
            System.Rtti.TValue.Make(@setBuf, TargetType, Result);
          end;
        end
        else
          Result := System.Rtti.TValue.From<string>(s);
      end
      else if isArray(V) and (TargetType <> nil) and (TargetType^.Kind = tkSet) then
      begin
        // Lox array of enum-name strings -> Delphi set
        setArr := pObjArray(GetObject(V));
        setBaseInfo2 := GetTypeData(TargetType)^.CompType^;
        FillChar(setBuf, SizeOf(setBuf), 0);

        for setI := 0 to setArr^.Count - 1 do
        begin
          if not isString(setArr^.Elements[setI]) then
          begin
            runtimeError('Set elements must be strings (enum names).');
            Exit(System.Rtti.TValue.Empty);
          end;
          setElemName := String(ObjStringToAnsiString(pObjString(GetObject(setArr^.Elements[setI]))));
          setElemOrd := GetEnumValue(setBaseInfo2, setElemName);
          if setElemOrd < 0 then
          begin
            runtimeError(Format('Invalid set element ''%s'' for type ''%s''.',
              [setElemName, String(TargetType^.Name)]));
            Exit(System.Rtti.TValue.Empty);
          end;
          setBuf[setElemOrd div 8] := setBuf[setElemOrd div 8] or (1 shl (setElemOrd mod 8));
        end;
        System.Rtti.TValue.Make(@setBuf, TargetType, Result);
      end
      else if isArray(V) and (TargetType <> nil) and (TargetType^.Kind = tkDynArray) then
      begin
        // Lox array -> Delphi dynamic array
        srcArr := pObjArray(GetObject(V));
        dynArrLen := srcArr^.Count;
        dynElemType := GetTypeData(TargetType)^.DynArrElType^;

        dynResult := System.Rtti.TValue.Empty;
        System.Rtti.TValue.Make(nil, TargetType, dynResult);
        DynArraySetLength(PPointer(dynResult.GetReferenceToRawData)^,
          TargetType, 1, @dynArrLen);

        for dynJ := 0 to dynArrLen - 1 do
          dynResult.SetArrayElement(dynJ,
            LoxValueToDelphi(srcArr^.Elements[dynJ], dynElemType));

        Result := dynResult;
      end
      else if isNativeObject(V) then
        Result := System.Rtti.TValue.From<TObject>(TObject(pObjNativeObject(GetObject(V))^.instance))
      else
      begin
        runtimeError(Format('Cannot marshal Lox %s to Delphi.',
          [GetEnumName(TypeInfo(TObjectKind), Ord(GetObject(V)^.ObjectKind))]));
        Result := System.Rtti.TValue.Empty;
      end;
  end
  else
    Result := System.Rtti.TValue.Empty;
end;

function ClassHasLoxClassAttr(rt: TRttiType): Boolean;
var
  a: TCustomAttribute;
begin
  for a in rt.GetAttributes do
    if a is LoxClassAttribute then Exit(True);
  Result := False;
end;

function HasAttribute(const attrs: TArray<TCustomAttribute>; attrClass: TClass): Boolean;
var
  a: TCustomAttribute;
begin
  for a in attrs do
    if a is attrClass then Exit(True);
  Result := False;
end;

function RttiGetProperty(instance: Pointer; classInfo: pNativeClassInfo;
  propName: pObjString; MemTracker: pMemTracker; out loxVal: TValue): Boolean;
var
  rt: TRttiType;
  prop: TRttiProperty;
  field: TRttiField;
  dv: System.Rtti.TValue;
  useLoxAttrs: Boolean;
  propNameStr: string;
begin
  Result := False;
  if instance = nil then
  begin
    runtimeError('Cannot access property of a destroyed native object.');
    Exit;
  end;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then Exit;

  rt := RttiCtx.GetType(classInfo^.rttiClass);
  if rt = nil then Exit;

  useLoxAttrs := ClassHasLoxClassAttr(rt);
  propNameStr := ObjStringToWideStr(propName);

  try
    prop := rt.GetProperty(propNameStr);
    if (prop <> nil) and (prop.IsReadable) then
    begin
      if useLoxAttrs and not HasAttribute(prop.GetAttributes, LoxPropertyAttribute) then Exit;
      dv := prop.GetValue(TObject(instance));
      loxVal := DelphiValueToLox(dv, MemTracker);
      Exit(True);
    end;

    field := rt.GetField(propNameStr);
    if field <> nil then
    begin
      dv := field.GetValue(TObject(instance));
      loxVal := DelphiValueToLox(dv, MemTracker);
      Exit(True);
    end;
  except
    on E: Exception do
    begin
      runtimeError(Format('RTTI get ''%s'' failed: %s', [propNameStr, E.Message]));
      Result := False;
    end;
  end;
end;

function RttiSetProperty(instance: Pointer; classInfo: pNativeClassInfo;
  propName: pObjString; const loxVal: TValue): Boolean;
var
  rt: TRttiType;
  prop: TRttiProperty;
  field: TRttiField;
  dv: System.Rtti.TValue;
  useLoxAttrs: Boolean;
  lpAttr: TCustomAttribute;
  propNameStr: string;
begin
  Result := False;
  if instance = nil then
  begin
    runtimeError('Cannot set property on a destroyed native object.');
    Exit;
  end;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then Exit;

  rt := RttiCtx.GetType(classInfo^.rttiClass);
  if rt = nil then Exit;

  useLoxAttrs := ClassHasLoxClassAttr(rt);
  propNameStr := ObjStringToWideStr(propName);

  try
    prop := rt.GetProperty(propNameStr);
    if (prop <> nil) and (prop.IsWritable) then
    begin
      if useLoxAttrs then
      begin
        if not HasAttribute(prop.GetAttributes, LoxPropertyAttribute) then Exit;
        // Check for readonly
        for lpAttr in prop.GetAttributes do
          if (lpAttr is LoxPropertyAttribute) and LoxPropertyAttribute(lpAttr).IsReadOnly then
          begin
            runtimeError(Format('Property ''%s'' is read-only.', [propNameStr]));
            Exit;
          end;
      end;
      dv := LoxValueToDelphi(loxVal, prop.PropertyType.Handle);
      if VM.RuntimeErrorStr <> '' then Exit;
      prop.SetValue(TObject(instance), dv);
      Exit(True);
    end;

    field := rt.GetField(propNameStr);
    if field <> nil then
    begin
      dv := LoxValueToDelphi(loxVal, field.FieldType.Handle);
      if VM.RuntimeErrorStr <> '' then Exit;
      field.SetValue(TObject(instance), dv);
      Exit(True);
    end;
  except
    on E: Exception do
    begin
      runtimeError(Format('RTTI set ''%s'' failed: %s', [propNameStr, E.Message]));
      Result := False;
    end;
  end;
end;

function RttiInvokeMethod(instance: Pointer; classInfo: pNativeClassInfo;
  methodName: pObjString; argCount: integer; args: pValue;
  MemTracker: pMemTracker; out loxResult: TValue): Boolean;
var
  rt: TRttiType;
  method: TRttiMethod;
  methods: TArray<TRttiMethod>;
  params: TArray<TRttiParameter>;
  delphiArgs: array of System.Rtti.TValue;
  dv: System.Rtti.TValue;
  i: integer;
  mName: string;
  useLoxAttrs: Boolean;
  rejectedByAttr: Boolean;
begin
  Result := False;
  if instance = nil then
  begin
    runtimeError('Cannot invoke method on a destroyed native object.');
    Exit;
  end;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then Exit;

  rt := RttiCtx.GetType(classInfo^.rttiClass);
  if rt = nil then Exit;

  mName := ObjStringToWideStr(methodName);
  useLoxAttrs := ClassHasLoxClassAttr(rt);

  if not useLoxAttrs then
  begin
    // Legacy denylist: prevent scripts from invoking lifecycle/dangerous TObject methods
    if SameText(mName, 'Free') or SameText(mName, 'Destroy') or
       SameText(mName, 'DisposeOf') or SameText(mName, 'FreeInstance') or
       SameText(mName, 'CleanupInstance') or SameText(mName, 'AfterConstruction') or
       SameText(mName, 'BeforeDestruction') then
    begin
      runtimeError(Format('Method ''%s'' is not callable from script.', [mName]));
      Exit;
    end;
  end;

  methods := rt.GetMethods(mName);
  rejectedByAttr := False;

  try
    for method in methods do
    begin
      params := method.GetParameters;
      if Length(params) = argCount then
      begin
        // Attribute mode: gate the matched overload, not methods[0]
        if useLoxAttrs and not HasAttribute(method.GetAttributes, LoxMethodAttribute) then
        begin
          rejectedByAttr := True;
          Continue;
        end;
        SetLength(delphiArgs, argCount);
        for i := 0 to argCount - 1 do
          delphiArgs[i] := LoxValueToDelphi(args[i], params[i].ParamType.Handle);

        // Bail out if marshaling set a runtime error
        if VM.RuntimeErrorStr <> '' then
          Exit;

        dv := method.Invoke(TObject(instance), delphiArgs);

        if method.ReturnType <> nil then
          loxResult := DelphiValueToLox(dv, MemTracker)
        else
          loxResult := CreateNilValue;
        Exit(True);
      end;
    end;
  except
    on E: Exception do
    begin
      runtimeError(Format('RTTI invoke ''%s'' failed: %s', [mName, E.Message]));
      Result := False;
    end;
  end;

  if (not Result) and rejectedByAttr then
    runtimeError(Format('Method ''%s'' is not callable from script.', [mName]));
end;

function ObjStringToAnsiString(S: PObjString): AnsiString;
begin
  {$IFOPT C+}
  AssertObjStringIsAssigned(S);
  AssertStringLengthIsNotNegative(S);
  {$ENDIF}
  SetString(Result, PAnsiChar(@S^.chars[0]), S^.length);
end;

function ObjStringEqualsAnsi(s: PObjString; const a: AnsiString): Boolean; inline;
begin
  Result := (s <> nil) and (s^.length = Length(a)) and
            ((s^.length = 0) or CompareMem(@s^.chars[0], PAnsiChar(a), s^.length));
end;

function ObjStringToWideStr(s: PObjString): string;
begin
  {$IFOPT C+}
  AssertObjStringIsAssigned(s);
  AssertStringLengthIsNotNegative(s);
  {$ENDIF}
  if s^.length = 0 then
    Exit('');
  SetString(Result, PAnsiChar(@s^.chars[0]), s^.length);
end;

function GetChar(const str : pObjString; index : integer) : AnsiChar;
var
  ptr : pAnsichar;
begin
  {$IFOPT C+}
  AssertObjStringIsAssigned(str);
  AssertIndexIsNotNegative(index);
  AssertIndexInRange(index, str.length);
  {$ENDIF}

  ptr := str.chars;
  inc(ptr,index);
  result := ptr^;
end;



procedure AddToCreatedObjects(p : pObj; MemTracker : pMemTracker);
begin
   {$IFOPT C+}
  AssertPointerIsNotNil(p, 'object');
  AssertMemTrackerIsNotNil(MemTracker);
  // Assert(Memtracker.CreatedObjects <> nil, 'Mem tracker created objects is nil'); this can be nil
  {$ENDIF}

  p^.Next := Memtracker.CreatedObjects;
  Memtracker.CreatedObjects := p;

  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('allocate %p kind=%s', [Pointer(p), ObjectKindStr(p^.ObjectKind)]));
  Inc(GCLogAllocations);
  {$ENDIF}
  
  // ---- Exit assertions ----
  {$IFOPT C+}
  Assert(MemTracker.CreatedObjects = p, 'AddToCreatedObjects exit: object should be at head of list');
  {$ENDIF}
end;

//Chars: array[0..0] of AnsiChar;
function AddString(const a, b : AnsiString; MemTracker : pMemTracker) : PObjString;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
   AssertMemTrackerIsNotNil(MemTracker);
   {$ENDIF}
  result := CreateString(a+b,Memtracker);

  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertObjStringIsAssigned(Result);
  Assert(Result^.Obj.ObjectKind = okString, 'AddString exit: result is not a string object');
  {$ENDIF}
end;

function ObjStringSize(const p : pObjString) : integer; inline;
begin
  {$IFOPT C+}
  AssertPointerIsNotNil(p, 'ObjString');
  {$ENDIF}
  result := Sizeof(TObjString) + Max(0, p^.length - 1);  // avoid negative size
end;

function ValueToString(const value : TValue) : pObjString;
begin
  {$IFOPT C+}
  AssertValueIsString(value);
  AssertPointerIsNotNil(GetObject(value), 'string value');
  {$ENDIF}
  result := pObjString(GetObject(value));
end;

function StringToValue(const value : pObjString) : TValue;
begin
  {$IFOPT C+}
  AssertObjStringIsAssigned(value);
  AssertObjectKindIsString(@value.Obj);
  {$ENDIF}
  result := CreateObject(pObj(value));
end;

function ValueToStr(const value : TValue) : String;
var
  i : integer;
  d : Double;
begin
  if isNumber(value) then
  begin
    d := GetNumber(value);
    if IsNan(d) then
      Result := 'nan'
    else if IsInfinite(d) then
    begin
      if d > 0 then Result := 'inf' else Result := '-inf';
    end
    else if (d = 0) and (IsInfinite(1 / d)) and ((1 / d) < 0) then
      Result := '-0'
    else
      Result := FloatToStr(d);
  end
  else if isBoolean(value) then
  begin
    if GetBoolean(value) then Result := 'true' else Result := 'false';
  end
  else if isNill(value) then
    Result := 'nil'
  else if isObject(value) then
  begin
    Assert(GetObject(value) <> nil, 'ValueToStr: ObjValue is nil for object value');
    case GetObject(value).ObjectKind of
      okString: Result := String(ObjStringToAnsiString(pObjString(GetObject(value))));
      okFunction: begin
        if pObjFunction(GetObject(value))^.name = nil then
          Result := '<script>'
        else
          Result := '<fn ' + String(ObjStringToAnsiString(pObjFunction(GetObject(value))^.name)) + '>';
      end;
      okNative: Result := '<native fn>';
      okClosure: begin
        if pObjClosure(GetObject(value))^.func^.name = nil then
          Result := '<script>'
        else
          Result := '<fn ' + String(ObjStringToAnsiString(pObjClosure(GetObject(value))^.func^.name)) + '>';
      end;
      okUpvalue: Result := 'upvalue';
      okArray: begin
        Result := '[';
        for i := 0 to pObjArray(GetObject(value))^.Count - 1 do
        begin
          if i > 0 then Result := Result + ', ';
          Result := Result + ValueToStr(pObjArray(GetObject(value))^.Elements[i]);
        end;
        Result := Result + ']';
      end;
      okRecordType: begin
        Result := '<record ' + String(ObjStringToAnsiString(pObjRecordType(GetObject(value))^.name)) + '>';
      end;
      okRecord: begin
        Result := String(ObjStringToAnsiString(pObjRecord(GetObject(value))^.recordType^.name)) + '(';
        for i := 0 to pObjRecord(GetObject(value))^.recordType^.fieldCount - 1 do
        begin
          if i > 0 then Result := Result + ', ';
          Result := Result + String(ObjStringToAnsiString(pObjRecord(GetObject(value))^.recordType^.fieldNames[i]))
            + ': ' + ValueToStr(pObjRecord(GetObject(value))^.fields[i]);
        end;
        Result := Result + ')';
      end;
      okNativeObject: begin
        Result := '<' + String(pObjNativeObject(GetObject(value))^.classInfo^.name) + '>';
      end;
      okDictionary: begin
        Result := '{';
        var first := true;
        for i := 0 to pObjDictionary(GetObject(value))^.Capacity - 1 do
        begin
          if pObjDictionary(GetObject(value))^.Entries[i].occupied and
             not pObjDictionary(GetObject(value))^.Entries[i].tombstone then
          begin
            if not first then Result := Result + ', ';
            first := false;
            Result := Result + ValueToStr(pObjDictionary(GetObject(value))^.Entries[i].key)
              + ': ' + ValueToStr(pObjDictionary(GetObject(value))^.Entries[i].value);
          end;
        end;
        Result := Result + '}';
      end;
    else
      Result := '<object>';
    end;
  end
  else
    Result := '<unknown>';
end;

procedure Concatenate(Stack : pStack; MemTracker : pMemTracker);
var
  top, below, resultStr: PObjString;
  totalLen: Integer;
  newSize: NativeInt;
  hash: UInt32;
  buf: PAnsiChar;
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertStackIsAssigned(Stack);
  Assert(IsString(peekStack(stack,0)),'Value at top of stack to concatenate is not a string');
  Assert(IsString(peekStack(stack,1)),'Value at position -1 of stack to concatenate is not a string');
  {$ENDIF}

  oldCount := Stack.StackTop - Stack.Values;  // element count via pointer arithmetic

  // Peek instead of pop ? keep on stack to protect from GC during allocation
  top := ValueToString(peekStack(stack, 0));
  below := ValueToString(peekStack(stack, 1));

  // Overflow guard: prevent integer wrap on malformed/corrupt string lengths
  if (below^.length > MaxInt - top^.length) then
    raise ELoxRuntimeError.Create('String concatenation overflow.');
  totalLen := below^.length + top^.length;

  // Empty-string fast path: if both operands are empty, return one of them
  // directly. Avoids GetMem, hash, intern lookup, and any allocation.
  if totalLen = 0 then
  begin
    resultStr := below;  // both are empty strings; either is fine
    popStack(stack);
    popStack(stack);
    PushStack(stack, StringToValue(resultStr));
    Assert(Stack.StackTop - Stack.Values = oldCount - 1, 'Concatenate exit: should pop 2, push 1 (net -1)');
    Assert(isString(peekStack(stack,0)), 'Concatenate exit: top of stack should be a string');
    Exit;
  end;

  // Build contiguous char buffer (raw heap ? not GC-tracked) so we can
  // hash and check the intern table BEFORE any GC-triggering allocation.
  GetMem(buf, totalLen);
  try
    if below^.length > 0 then
      Move(below^.chars[0], buf[0], below^.length);
    if top^.length > 0 then
      Move(top^.chars[0], buf[below^.length], top^.length);

    hash := HashString(buf, totalLen);

    // Fast path: check intern table before allocating (same pattern as CreateString)
    resultStr := nil;
    if (VM <> nil) and (VM.Strings <> nil) then
      resultStr := TableFindString(VM.Strings, buf, totalLen, hash);

    // Intern hit ? early exit, no GC allocation needed
    if resultStr <> nil then
    begin
      FreeMem(buf);
      buf := nil;
      popStack(stack);
      popStack(stack);
      PushStack(stack, StringToValue(resultStr));
      Assert(Stack.StackTop - Stack.Values = oldCount - 1, 'Concatenate exit: should pop 2, push 1 (net -1)');
      Assert(isString(peekStack(stack,0)), 'Concatenate exit: top of stack should be a string');
      Exit;
    end;

    // Not interned ? allocate via GC and follow CreateString's safe pattern
    newSize := SizeOf(TObjString) + Max(0, totalLen - 1);
    resultStr := nil;
    AllocateString(resultStr, 0, newSize, MemTracker);

    resultStr^.Obj.ObjectKind := okString;
    resultStr^.Obj.IsMarked := false;
    resultStr^.Obj.Next := nil;
    resultStr^.length := totalLen;
    resultStr^.hash := hash;

    // Copy from our raw buffer (immune to GC)
    if totalLen > 0 then
      Move(buf^, resultStr^.chars[0], totalLen);

    // Track in GC object list
    AddToCreatedObjects(PObj(resultStr), MemTracker);

    // Intern: push to protect from GC during TableSet (same pattern as CreateString)
    if (VM <> nil) and (VM.Strings <> nil) then
    begin
      pushStack(stack, StringToValue(resultStr));
      TableSet(VM.Strings, resultStr, CreateNilValue, MemTracker);
      popStack(stack);
    end;
  finally
    if buf <> nil then
      FreeMem(buf);
  end;

  // Pop sources and push result
  popStack(stack);
  popStack(stack);
  PushStack(stack, StringToValue(resultStr));

  // ---- Exit assertions ----
  Assert(Stack.StackTop - Stack.Values = oldCount - 1, 'Concatenate exit: should pop 2, push 1 (net -1)');
  Assert(isString(peekStack(stack,0)), 'Concatenate exit: top of stack should be a string');
end;


function CreateBoolean(Value: Boolean): TValue; inline;
begin
  if Value then Result := TRUE_VAL else Result := FALSE_VAL;
end;

function isBoolean(const Value: TValue): Boolean; inline;
begin
  Result := (Value or 1) = TRUE_VAL;
end;

function GetBoolean(const Value : TValue) : Boolean; inline;
begin
  Result := Value = TRUE_VAL;
end;

function isNumber(const Value: TValue): Boolean; inline;
begin
  Result := (Value and QNAN) <> QNAN;
end;

function CreateNumber(Value: Double): TValue; inline;
var
  bits: UInt64;
begin
  Move(Value, bits, SizeOf(UInt64));
  // Canonicalize ALL NaN payloads to CANON_NAN. This serves two purposes:
  // 1. Prevents misclassification: NaN payloads that satisfy (bits & QNAN)=QNAN
  //    would be mistaken for nil/boolean/object tagged values.
  // 2. Guarantees ValuesEqual's fast path: the only NaN at runtime is CANON_NAN,
  //    so "a = b implies equality" holds for all values except CANON_NAN.
  // Check: mask off sign bit, compare to infinity. Any value > +inf is NaN.
  if (bits and $7FFFFFFFFFFFFFFF) > $7FF0000000000000 then
    Result := CANON_NAN
  else
    Result := bits;
end;

function GetNumber(Value : TValue) : double; inline;
begin
  Move(Value, Result, SizeOf(Double));
end;

function CreateNilValue : TValue; inline;
begin
  Result := NIL_VAL;
end;

function isNill(const Value: TValue): Boolean; inline;
begin
  Result := Value = NIL_VAL;
end;

function GetNil(const value : TValue) : byte;
begin
  Result := 0;
end;

function IsFalsey(const Value: TValue): Boolean; inline;
begin
  Result := (Value = NIL_VAL) or (Value = FALSE_VAL);
end;

// --- Native helper functions ---

function AsAnsiString(value : TValue) : AnsiString; inline;
begin
  Result := ObjStringToAnsiString(pObjString(GetObject(value)));
end;

function AsWideString(value : TValue) : string; inline;
begin
  Result := ObjStringToWideStr(pObjString(GetObject(value)));
end;

function AsInteger(value : TValue) : Integer; inline;
begin
  Result := Trunc(GetNumber(value));
end;

function CreateStringValue(const s : AnsiString) : TValue;
var
  objStr : pObjString;
begin
  objStr := CreateString(s, VM.MemTracker);
  Result := CreateObject(pObj(objStr));
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
   {$IFOPT C+}
   AssertObjStringIsAssigned(a);
  AssertObjStringIsAssigned(b);
  {$ENDIF}
  if a.length <> b.length then
    exit(false);

  Result :=
    (a^.Length = b^.Length) and
    CompareMem(@a^.Chars, @b^.Chars, a^.Length);
end;



function ValuesEqual(a, b : TValue) : boolean;
var
  objA, objB : pObj;
begin
  // Fast path: identical bit representations imply equality for all value
  // types except NaN (IEEE-754 defines NaN ≠ NaN).
  // Safe because: (1) all runtime strings are interned — equal content means
  // same pointer, (2) every NaN is normalized to CANON_NAN by CreateNumber,
  // so no non-canonical NaN payloads exist at runtime.
  if a = b then
    exit(a <> CANON_NAN);

  if isNumber(a) and isNumber(b) then
    result := GetNumber(a) = GetNumber(b)
  else if isObject(a) and isObject(b) then
  begin
    objA := GetObject(a);
    objB := GetObject(b);
     {$IFOPT C+}
     AssertPointerIsNotNil(objA, 'A value in ValuesEqual');
    AssertPointerIsNotNil(objB, 'B value in ValuesEqual');
    {$ENDIF}
    case objA.ObjectKind of
      okString : begin
        result := (objB.ObjectKind = okString) and (objA = objB);
      end;
      okNativeObject : begin
        if objB.ObjectKind = okNativeObject then
          result := pObjNativeObject(objA)^.instance =
                    pObjNativeObject(objB)^.instance
        else
          result := false;
      end;
      // All remaining object kinds use identity (pointer) equality.
      // Listed explicitly so adding a new TObjectKind forces a decision here.
      okFunction,
      okNative,
      okClosure,
      okUpvalue,
      okArray,
      okRecordType,
      okRecord,
      okDictionary : begin
        result := objA = objB;
      end;
    else
      // Exhaustiveness guard: if a new TObjectKind is added but not handled
      // above, this raises immediately rather than returning an undefined result.
      raise Exception.CreateFmt('Unhandled ObjectKind %d in ValuesEqual',
        [Ord(objA.ObjectKind)]);
    end
  end
  else
    // For booleans, nil, and mixed types: bitwise equality works
    // (same encoding = same value, different encoding = different value)
    result := a = b;
end;



function AddValueConstant(ValueArray: pValueArray; const value: TValue;Memtracker : pMemTracker): Integer;
var
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertValueArrayIsAssigned(ValueArray);
  {$ENDIF}

  oldCount := ValueArray.Count;

  // Push value onto stack to protect from GC during potential array growth.
  pushStack(VM.Stack, value);
  
  // Add the value to the ValueArray -- note here the value array can grow
  writeValueArray(ValueArray, value,Memtracker);

  Dec(VM.Stack.StackTop);

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
   {$IFOPT C+}
   AssertMemTrackerIsNotNil(MemTracker);
  AssertChunkIsAssigned(chunk);
  AssertChunkConstantsIsAssigned(chunk);
  {$ENDIF}

  oldChunkCount := chunk.Count;
  
  //add constant, 1st into value's array of the value record
  idx := AddValueConstant(chunk.Constants,value,MemTracker);
  //add constant op code into the chunk array
   {$IFOPT C+}
  AssertIndexIsNotNegative(idx);
  {$ENDIF}

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
  {$IFOPT C+}
  Assert(chunk.Count > oldChunkCount, 'AddConstant exit: chunk count should have increased');
  {$ENDIF}
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
   {$IFOPT C+}
  AssertValueArrayIsAssigned(ValueArray);
  AssertPointerIsNotNil(strings, 'output strings');
  {$ENDIF}

  strings.Clear;
  strings.Add(Format(VR_Header, [Pointer(ValueArray)]));

  ValuePtr := ValueArray.Values;

    for i := 0 to ValueArray.Count - 1 do
    begin
      // format value as general format with up to 12 significant digits
      valStr := Format('%.12g', [GetNumber(ValuePtr^)]);

      // pad to fixed width
      valStr := valStr + StringOfChar(' ', VALUE_FIELD_WIDTH - Length(valStr));

      // Index + fixed-width value
      strings.Add(Format('%.6d  %s', [i, valStr]));
      Inc(ValuePtr);
    end;

end;




procedure RunTimeError(const msg : string);
begin
   {$IFOPT C+}
  AssertVMIsAssigned;
  {$ENDIF}
  VM.RuntimeErrorStr := msg;
end;


function ReadByte(var code : pByte): Byte; inline;
begin
   {$IFOPT C+}
   AssertCodePointerIsAssigned(Code);
   {$ENDIF}
   result := Code^;
   inc(Code);
end;

function ReadConstant(var code : pByte; constants : pValueArray) : TValue; inline;
var idx : Byte;
begin
  {$IFOPT C+}
  AssertValueArrayIsAssigned(constants);
  AssertValuesIsAssigned(constants.Values);
  {$ENDIF}
  idx := ReadByte(code);


  {$IFOPT C+}
    AssertIndexInRange(idx, constants.Count);
  {$ENDIF}
  result := Constants.Values[idx];
end;

function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue; inline;
var
  idx : integer;
  Bytes: TIntToByteResult;
begin
  {$IFOPT C+}
  AssertValueArrayIsAssigned(constants);
  AssertValuesIsAssigned(constants.Values);
  {$ENDIF}
  Bytes.byte0 := ReadByte(code);
  Bytes.byte1 := ReadByte(code);
  Bytes.byte2 := ReadByte(code);
  idx := ByteToInt(Bytes);
  {$IFOPT C+}
  AssertIndexIsNotNegative(idx);
  AssertIndexInRange(idx, constants.Count);
  {$ENDIF}
  result := Constants.Values[idx];
end;

function captureUpvalue(local : pValue) : pObjUpvalue;
var
  prevUpvalue : pObjUpvalue;
  upvalue : pObjUpvalue;
  createdUpvalue : pObjUpvalue;
begin
  // ---- Entry assertions ----
  {$IFOPT C+}
  Assert(local <> nil, 'captureUpvalue: local is nil');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}

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
  {$IFOPT C+}
  Assert(createdUpvalue <> nil, 'captureUpvalue exit: result is nil');
  Assert(createdUpvalue^.location = local, 'captureUpvalue exit: location does not match local');
  {$ENDIF}
  Result := createdUpvalue;
end;

procedure closeUpvalues(last : pValue);
var
  upvalue : pObjUpvalue;
begin
  // ---- Entry assertions ----
  {$IFOPT C+}
  Assert(last <> nil, 'closeUpvalues: last is nil');
  AssertVMIsAssigned;
  {$ENDIF}

  while (VM.OpenUpvalues <> nil) and
        (NativeUInt(VM.OpenUpvalues^.location) >= NativeUInt(last)) do
  begin
    upvalue := VM.OpenUpvalues;
    upvalue^.closed := upvalue^.location^;
    upvalue^.location := @upvalue^.closed;
    VM.OpenUpvalues := upvalue^.next;
  end;
end;

function InvokeDictMethod(dict: pObjDictionary; methodName: pObjString;
  argCount: integer; args: pValue; var outResult: TValue): boolean;
var
  val: TValue;
  arr: pObjArray;
  i, newCap, oldSize, newSize: integer;
begin
  Result := true;
  if ObjStringEqualsAnsi(methodName, 'Set') then
  begin
    if argCount <> 2 then
    begin
      RuntimeError('Set() takes 2 arguments (key, value).');
      Exit(false);
    end;
    DictSet(dict, args[0], args[1], VM.MemTracker);
    outResult := args[1];
  end
  else if ObjStringEqualsAnsi(methodName, 'Get') then
  begin
    if argCount <> 1 then
    begin
      RuntimeError('Get() takes 1 argument (key).');
      Exit(false);
    end;
    if not DictGet(dict, args[0], val) then
    begin
      RuntimeError('Key not found in dictionary.');
      Exit(false);
    end;
    outResult := val;
  end
  else if ObjStringEqualsAnsi(methodName, 'Has') then
  begin
    if argCount <> 1 then
    begin
      RuntimeError('Has() takes 1 argument (key).');
      Exit(false);
    end;
    outResult := CreateBoolean(DictGet(dict, args[0], val));
  end
  else if ObjStringEqualsAnsi(methodName, 'Delete') then
  begin
    if argCount <> 1 then
    begin
      RuntimeError('Delete() takes 1 argument (key).');
      Exit(false);
    end;
    outResult := CreateBoolean(DictDelete(dict, args[0]));
  end
  else if ObjStringEqualsAnsi(methodName, 'Keys') then
  begin
    if argCount <> 0 then
    begin
      RuntimeError('Keys() takes no arguments.');
      Exit(false);
    end;
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
          arr^.Elements[arr^.Count] := dict^.Entries[i].key;
          Inc(arr^.Count);
        end;
      end;
      Dec(VM.Stack.StackTop);
    end;
    outResult := CreateObject(pObj(arr));
  end
  else if ObjStringEqualsAnsi(methodName, 'Values') then
  begin
    if argCount <> 0 then
    begin
      RuntimeError('Values() takes no arguments.');
      Exit(false);
    end;
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
    outResult := CreateObject(pObj(arr));
  end
  else if ObjStringEqualsAnsi(methodName, 'Size') then
  begin
    if argCount <> 0 then
    begin
      RuntimeError('Size() takes no arguments.');
      Exit(false);
    end;
    outResult := CreateNumber(dict^.Count);
  end
  else if ObjStringEqualsAnsi(methodName, 'Equal') or ObjStringEqualsAnsi(methodName, 'Equals') then
  begin
    if argCount <> 1 then
    begin
      RuntimeError('Equal() takes exactly 1 argument.');
      Exit(false);
    end;
    if not isDictionary(args[0]) then
      outResult := CreateBoolean(false)
    else if pObjDictionary(GetObject(args[0])) = dict then
      outResult := CreateBoolean(true)
    else
    begin
      outResult := CreateBoolean(false);
      if pObjDictionary(GetObject(args[0]))^.Count = dict^.Count then
      begin
        outResult := CreateBoolean(true);
        for i := 0 to dict^.Capacity - 1 do
        begin
          if not dict^.Entries[i].occupied or dict^.Entries[i].tombstone then
            Continue;
          if not DictGet(pObjDictionary(GetObject(args[0])), dict^.Entries[i].key, val) then
          begin
            outResult := CreateBoolean(false);
            Break;
          end;
          if not ValuesEqual(dict^.Entries[i].value, val) then
          begin
            outResult := CreateBoolean(false);
            Break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    RuntimeError('Undefined dictionary method ''' + ObjStringToWideStr(methodName) + '''.');
    Exit(false);
  end;
end;



function Run(baselineFrameCount: Integer = 0) : TInterpretResult;
var
  frame : pCallFrame;  // Cached pointer to current executing frame.
                        // Derived from VM.Frames[VM.FrameCount - 1].
                        // Never authoritative state ? recomputed after call/return.

  ip : pByte;           // Cached frame^.ip. Eliminates one pointer indirection
                        // on every byte read (hottest path in the VM).
                        // Stored back to frame^.ip before OP_CALL (so the
                        // caller's return address is preserved) and reloaded
                        // from the new frame after OP_CALL / OP_RETURN.

  constants : pValue;  // Cached frame^.closure^.func^.chunk^.Constants^.Values.
                        // Eliminates 5-deep pointer chain on every constant load.
                        // Rebased alongside `frame` after OP_CALL / OP_RETURN.
  stack : pStack;      // Cached vm.Stack. Never changes during execution
                        // (allocated once in InitVM). Saves one pointer load
                        // on every stack access vs going through the global `vm`.
  instruction: Byte;
  slot : Byte;
  offset : Word;
  value,ValueB,ValueC : TValue;
  argCount : byte;
  func : pObjFunction;
  closure : pObjClosure;
  native : TNativeFn;
  nativeResult : TValue;
  isLocal : byte;
  index : byte;
  i, j : integer;
  // record support
  recFieldNames : array[0..MAX_BYTE_OPERAND] of pObjString;
  recNameIdx : integer;
  recTypeName : pObjString;
  recType : pObjRecordType;
  recFieldNamesCopy : ppObjString;
  rec : pObjRecord;
  fieldName : pObjString;
  fieldIdx : integer;
  // native object invoke support
  nativeObj : pObjNativeObject;
  nativeMethod : TNativeMethodFn;
  invokeNameIdx : integer;
  invokeArgCount : byte;
  invokeMethodName : pObjString;
  // RTTI property support
  rttiPropName : pObjString;
  rttiResult : TValue;
  // RTTI indexed property support
  idxProp : TRttiIndexedProperty;
  idxParams : TArray<TRttiParameter>;
  idxDelphiArgs : array[0..0] of System.Rtti.TValue;
  idxDelphiVal  : System.Rtti.TValue;

  function CurrentFrame: pCallFrame;
  begin
    Result := @VM.Frames[VM.FrameCount - 1];
  end;


  {$IFOPT C+}
  procedure AssertFrameConstants(const context: string);
  begin
    Assert(frame <> nil, context + ': frame is nil');
    Assert(frame^.closure <> nil, context + ': frame^.closure is nil');
    Assert(frame^.closure^.func <> nil, context + ': frame^.closure^.func is nil');
    Assert(frame^.closure^.func^.chunk <> nil, context + ': frame^.closure^.func^.chunk is nil');
    Assert(frame^.closure^.func^.chunk^.Constants <> nil, context + ': chunk^.Constants is nil');
  end;

  procedure AssertFrameValues(const context: string);
  var
    cl: pObjClosure;
    fn: pObjFunction;
    ch: pChunk;
    constants: pValueArray;
  begin
    Assert(frame <> nil, context + ': frame is nil');
    cl := frame^.closure;
    Assert(cl <> nil, context + ': frame^.closure is nil');
    fn := cl^.func;
    Assert(fn <> nil, context + ': closure^.func is nil');
    ch := fn^.chunk;
    Assert(ch <> nil, context + ': func^.chunk is nil');
    constants := ch^.Constants;
    Assert(constants <> nil, context + ': chunk^.Constants is nil');
    Assert(constants^.Values <> nil, context + ': chunk^.Constants^.Values is nil');
  end;

  procedure AssertConstantIndex(idx: Integer; const context: string);
  begin
    AssertFrameConstants(context);
    Assert((idx >= 0) and (idx < frame^.closure^.func^.chunk^.Constants^.Count),
      context + ': constant index out of bounds');
  end;

  procedure AssertFrameCode(const context: string);
  begin
    Assert(frame <> nil, context + ': frame is nil');
    Assert(frame^.closure <> nil, context + ': frame^.closure is nil');
    Assert(frame^.closure^.func <> nil, context + ': frame^.closure^.func is nil');
    Assert(frame^.closure^.func^.chunk <> nil, context + ': frame^.closure^.func^.chunk is nil');
    Assert(frame^.closure^.func^.chunk^.Code <> nil, context + ': chunk^.Code is nil');
  end;

  procedure AssertUpvalueIndex(idx: Integer; const context: string);
  begin
    Assert(frame <> nil, context + ': frame is nil');
    Assert(frame^.closure <> nil, context + ': frame^.closure is nil');
    Assert((idx >= 0) and (idx < frame^.closure^.upvalueCount),
      context + ': upvalue index out of bounds');
  end;
  {$ENDIF}


begin
    //Assert(false, 'assertions are still compiled in');
    Result := Default(TInterpretResult);
     {$IFOPT C+}
     AssertVMIsAssigned;
    AssertStackIsAssigned(vm.Stack);
    AssertMemTrackerIsNotNil(vm.MemTracker);
    Assert(VM.FrameCount > 0, 'Run: no call frames');
    {$ENDIF}

    stack := vm.Stack;
    frame := CurrentFrame;
    ip := frame^.ip;

    constants := frame^.closure^.func^.chunk^.Constants^.Values;

    {$IFOPT C+}
    Assert(frame^.closure <> nil, 'Run: initial frame closure is nil');
    Assert(ip <> nil, 'Run: initial frame ip is nil');
    {$ENDIF}

    // FRAME OWNERSHIP:
    // `frame` always points to VM.Frames[VM.FrameCount - 1] (the current executing frame).
    // It MUST be rebased after any operation that changes VM.FrameCount:
    //   - After CallValue returns true (OP_CALL)
    //   - After OP_RETURN decrements FrameCount
    // Between ReadByteFr and CallValue, frame still points to the CALLER.
    // After CallValue, frame is stale until explicitly rebased.
    // No nested function may mutate frame.
    while True do
    begin
       {$IFOPT C+}
      Assert(VM.FrameCount > 0,'Run dispatch: FrameCount <= 0');
      Assert(frame = CurrentFrame, 'Run dispatch: frame is out of sync with FrameCount');
      {$ENDIF}
      instruction := ip^; Inc(ip);
      {$IFDEF OPCODE_PROFILING}
      Inc(OpPairCounts[OpPrevOp, instruction]);
      OpPrevOp := instruction;
      {$ENDIF}
      case instruction of
        OP_ADD_RETURN: begin
          // Fused ADD + RETURN. No operands.
          // Adds top two stack values, then returns the result to the caller.
          if isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2]) then
            value := CreateNumber(GetNumber(stack.StackTop[-2]) + GetNumber(stack.StackTop[-1]))
          else if isString(stack.StackTop[-1]) and isString(stack.StackTop[-2]) then
          begin
            Concatenate(vm.stack, vm.MemTracker);
            value := stack.StackTop[-1];
          end
          else
          begin
            runtimeError('Operands must be two numbers or two strings.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;

          if VM.OpenUpvalues <> nil then
            closeUpvalues(frame^.slots);

          Dec(VM.FrameCount);
          if VM.FrameCount <= baselineFrameCount then
          begin
            // Clean the stack window
            stack.StackTop := frame^.slots;
            if baselineFrameCount = 0 then
              Dec(stack.StackTop);
            // For re-entrant calls, push return value so InvokeCallback can pop it.
            if baselineFrameCount > 0 then
            begin
              stack.StackTop^ := value;
              Inc(stack.StackTop);
            end;
            Result.Code := INTERPRET_OK;
            Result.value := value;
            Exit(Result);
          end;

          stack.StackTop := frame^.slots;
          Assert(NativeUInt(stack.StackTop) >= NativeUInt(stack.Values), 'OP_ADD_RETURN: StackTop before Values');
          stack.StackTop^ := value;
          Inc(stack.StackTop);

          frame := @VM.Frames[VM.FrameCount - 1];
          ip := frame^.ip;
          constants := frame^.closure^.func^.chunk^.Constants^.Values;
        end;


        OP_RETURN: begin
          Dec(stack.StackTop);

          value := stack.StackTop^;
          if VM.OpenUpvalues <> nil then
            closeUpvalues(frame^.slots);

          Dec(VM.FrameCount);
          if VM.FrameCount <= baselineFrameCount then
          begin
            // Discard the returning function's stack window
            stack.StackTop := frame^.slots;
            if baselineFrameCount = 0 then
              Dec(stack.StackTop); // pop the script function
            // For re-entrant calls (baselineFrameCount > 0), push return
            // value so InvokeCallback can pop it.
            if baselineFrameCount > 0 then
            begin
              stack.StackTop^ := value;
              Inc(stack.StackTop);
            end;
            Result.Code := INTERPRET_OK;
            Result.value := value;
            Exit(Result);
          end;

          // Discard the returning function's stack window
          stack.StackTop := frame^.slots;
          Assert(NativeUInt(stack.StackTop) >= NativeUInt(stack.Values), 'OP_RETURN: StackTop before Values');
          // Push return value directly — safe because we just shrunk StackTop
          // to frame^.slots, which is well below CapacityEnd.
          stack.StackTop^ := value;
          Inc(stack.StackTop);

          frame := @VM.Frames[VM.FrameCount - 1];
          ip := frame^.ip;
          constants := frame^.closure^.func^.chunk^.Constants^.Values;
        end;

        OP_CALL: begin
          argCount := ip^; Inc(ip);
          // Store ip back to frame before entering new frame (preserves return address)
          frame^.ip := ip;
          value := stack.StackTop[-1-argCount];
          // Inline closure fast-path: avoids nested-function call overhead (331M calls for fib(40))
          if isClosure(value) then
          begin
            closure := pObjClosure(GetObject(value));
            if argCount <> closure^.func^.arity then
            begin
              runtimeError('Expected ' + IntToStr(closure^.func^.arity) +
                ' arguments but got ' + IntToStr(argCount) + '.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            if VM.FrameCount = FRAMES_MAX then
            begin
              runtimeError('Stack overflow (max ' + IntToStr(FRAMES_MAX) +
                ' frames). Last call: ' + String(ObjStringToAnsiString(closure^.func^.name)));
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            VM.Frames[VM.FrameCount].closure := closure;
            VM.Frames[VM.FrameCount].ip := closure^.func^.chunk^.Code;
            VM.Frames[VM.FrameCount].slots := stack.StackTop;
            Dec(VM.Frames[VM.FrameCount].slots, argCount + 1);
            Inc(VM.FrameCount);
          end
          else if isNative(value) then
          begin
            // Arity check (-1 means variadic, skip check)
            if (pObjNative(GetObject(value))^.arity >= 0) and
               (argCount <> pObjNative(GetObject(value))^.arity) then
            begin
              if pObjNative(GetObject(value))^.arity = 1 then
                runtimeError(String(AnsiString(pObjNative(GetObject(value))^.name)) +
                  '() takes exactly 1 argument.')
              else
                runtimeError(String(AnsiString(pObjNative(GetObject(value))^.name)) +
                  '() takes exactly ' +
                  IntToStr(pObjNative(GetObject(value))^.arity) + ' arguments.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            native := pObjNative(GetObject(value))^.func;
            nativeResult := native(argCount, pValue(NativeUInt(stack.StackTop) - NativeUInt(argCount) * SizeOf(TValue)));
            // Check if native signalled a runtime error
            if VM.RuntimeErrorStr <> '' then
            begin
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // pop args + callee, push result
            stack.StackTop := pValue(NativeUInt(stack.StackTop) - NativeUInt(argCount + 1) * SizeOf(TValue));
            pushStack(stack, nativeResult);
          end
          else if isRecordType(value) then
          begin
            // Construct a new record instance
            if argCount <> pObjRecordType(GetObject(value))^.fieldCount then
            begin
              runtimeError('Expected ' + IntToStr(pObjRecordType(GetObject(value))^.fieldCount) +
                ' arguments but got ' + IntToStr(argCount) + '.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Create the record instance - push onto stack to protect from GC
            value := CreateObject(pObj(newRecord(pObjRecordType(GetObject(value)), VM.MemTracker)));
            pushStack(stack, value);
            // Copy arguments into the record's fields (args are on stack below the new record)
            for j := 0 to argCount - 1 do
              pObjRecord(GetObject(value))^.fields[j] :=
                (stack.StackTop - 1 - argCount + j)^;
            // Pop the record instance we just pushed for protection
            Dec(stack.StackTop);
            // Pop args + callee, push result
            stack.StackTop := pValue(NativeUInt(stack.StackTop) - NativeUInt(argCount + 1) * SizeOf(TValue));
            pushStack(stack, value);
          end
          else
          begin
            runtimeError('Can only call functions and classes.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          // Rebase frame/ip/constants after call (closure pushes a new frame;
          // native/record don't, but rebasing is harmless and keeps code simple)
          frame := @VM.Frames[VM.FrameCount - 1];
          ip := frame^.ip;
          constants := frame^.closure^.func^.chunk^.Constants^.Values;
        end;


        OP_CONSTANT : begin
          if stack.StackTop < stack.CapacityEnd then
          begin
            stack.StackTop^ := constants[ip^];
            Inc(stack.StackTop);
          end
          else
            pushStackGrow(stack, constants[ip^]);

          Inc(ip);
        end;

        OP_CONSTANT_LONG : begin
          // 3-byte constant index (little-endian: byte0 | byte1<<8 | byte2<<16)
          i := ip^; Inc(ip);
          i := i or (ip^ shl 8); Inc(ip);
          i := i or (ip^ shl 16); Inc(ip);
           {$IFOPT C+}
          AssertConstantIndex(i, 'OP_CONSTANT_LONG');
          AssertFrameValues('OP_CONSTANT_LONG');
          {$ENDIF}
          value := constants[i];
          pushStack(stack,value);
        end;

        OP_NEGATE : begin
          // In-place negation: rewrite the top slot, no pop/push.
          if not isNumber(stack.StackTop[-1]) then
          begin
             result.code := INTERPRET_RUNTIME_ERROR;
             runtimeError('Operand must be a number.');
             exit;
          end;
          stack.StackTop[-1] := CreateNumber(-GetNumber(stack.StackTop[-1]));
        end;

        OP_NIL      : pushStack(stack, CreateNilValue);
        OP_TRUE     : pushStack(stack, CreateBoolean(true));
        OP_FALSE    : pushStack(stack, CreateBoolean(false));

        OP_EQUAL: begin
          stack.StackTop[-2] := CreateBoolean(valuesEqual(stack.StackTop[-2], stack.StackTop[-1]));
          Dec(stack.StackTop);
        end;

        OP_GREATER  : begin
                        // Inlined: peek-peek-typecheck, write boolean result, pop 2 push 1.
                        // Stack before: [..., A, B]   StackTop[-2]=A, StackTop[-1]=B
                        // Stack after:  [..., A>B]    boolean in old A slot
                        if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        stack.StackTop[-2] := CreateBoolean(
                          GetNumber(stack.StackTop[-2]) > GetNumber(stack.StackTop[-1]));
                        Dec(stack.StackTop);
                      end;

        OP_LESS     : begin
                        if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        stack.StackTop[-2] := CreateBoolean(
                          GetNumber(stack.StackTop[-2]) < GetNumber(stack.StackTop[-1]));
                        Dec(stack.StackTop);
                      end;

        OP_ADD      : begin
                        // Fast path: both numbers ? direct in-place add, no pop/push.
                        if isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2]) then
                        begin
                          stack.StackTop[-2] := CreateNumber(
                            GetNumber(stack.StackTop[-2]) + GetNumber(stack.StackTop[-1]));
                          Dec(stack.StackTop);
                        end
                        else if isString(stack.StackTop[-1]) and isString(stack.StackTop[-2]) then
                        begin
                          Concatenate(vm.stack, vm.MemTracker);
                        end
                        else
                        begin
                          runtimeError('Operands must be two numbers or two strings.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_SUBTRACT : begin
                        if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        stack.StackTop[-2] := CreateNumber(
                          GetNumber(stack.StackTop[-2]) - GetNumber(stack.StackTop[-1]));
                        Dec(stack.StackTop);
                      end;

        OP_MULTIPLY : begin
                        if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        stack.StackTop[-2] := CreateNumber(
                          GetNumber(stack.StackTop[-2]) * GetNumber(stack.StackTop[-1]));
                        Dec(stack.StackTop);
                      end;

        OP_DIVIDE   : begin
                        if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        stack.StackTop[-2] := CreateNumber(
                          GetNumber(stack.StackTop[-2]) / GetNumber(stack.StackTop[-1]));
                        Dec(stack.StackTop);
                      end;

        OP_MODULO   : begin
                        if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          result.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        // Match prior semantics: A - Trunc(A/B)*B
                        stack.StackTop[-2] := CreateNumber(
                          GetNumber(stack.StackTop[-2]) -
                          Trunc(GetNumber(stack.StackTop[-2]) / GetNumber(stack.StackTop[-1])) *
                          GetNumber(stack.StackTop[-1]));
                        Dec(stack.StackTop);
                      end;

        OP_NOT      : begin
          // In-place: rewrite top slot to boolean(isFalsey(top)).
          stack.StackTop[-1] := CreateBoolean(isFalsey(stack.StackTop[-1]));
        end;

        OP_PRINT: begin
          Dec(stack.StackTop);

          value := stack.StackTop^;
          if VM.PrintBuilder.Length > 0 then
            VM.PrintBuilder.Append(sLineBreak);
          VM.PrintBuilder.Append(ValueToStr(value));
          if Assigned(VM.OnPrint) then
            VM.OnPrint(ValueToStr(value));
        end;

        OP_POP: begin
          // Inlined popStack: just shrink top, no asserts.
          Dec(stack.StackTop);
        end;

        OP_DUP: begin
          // Push a copy of the top of stack.
          stack.StackTop^ := stack.StackTop[-1];
          Inc(stack.StackTop);
        end;

        OP_DUP2: begin
          // Duplicate top two stack slots.
          stack.StackTop[0] := stack.StackTop[-2];
          stack.StackTop[1] := stack.StackTop[-1];
          Inc(stack.StackTop, 2);
        end;

        OP_POP_N: begin
          // Bulk discard. 1-byte operand N (1..255). Emitted by endScope to
          // collapse runs of locals leaving scope into a single dispatch.
          argCount := ip^; Inc(ip);
          {$IFOPT C+}
          Assert(argCount > 0, 'OP_POP_N: operand is zero');
          Assert(stack.StackTop - stack.Values >= argCount, 'OP_POP_N: stack underflow');
          {$ENDIF}
          Dec(stack.StackTop, argCount);
        end;

        OP_GET_GLOBAL: begin
          value := constants[ip^];
          Inc(ip);
          {$IFOPT C+}
          AssertValueIsString(value);
          {$ENDIF}
          if not TableGet(vm.Globals, ValueToString(value), ValueB) then
          begin
            runtimeError('Undefined variable ''' + String(ObjStringToAnsiString(ValueToString(value))) + '''.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          pushStack(stack, ValueB);
        end;

        OP_SET_GLOBAL: begin
          value := constants[ip^];
          Inc(ip);
          {$IFOPT C+}
          AssertValueIsString(value);
          {$ENDIF}
          if TableSet(vm.Globals, ValueToString(value), stack.StackTop[-1], vm.MemTracker) then
          begin
            TableDelete(vm.Globals, ValueToString(value));
            runtimeError('Undefined variable ''' + String(ObjStringToAnsiString(ValueToString(value))) + '''.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_GET_LOCAL: begin
          slot := ip^; Inc(ip);
          {$IFOPT C+}
          Assert(NativeUInt(@frame^.slots[slot]) < NativeUInt(stack.StackTop), 'OP_GET_LOCAL: slot out of stack bounds');
          {$ENDIF}
          if stack.StackTop >= stack.CapacityEnd then
            pushStackGrow(stack, frame^.slots[slot])
          else begin
            stack.StackTop^ := frame^.slots[slot];
            Inc(stack.StackTop);
          end;
        end;

        OP_GET_LOCAL_0..OP_GET_LOCAL_7: begin
          // Fast-slot get: no operand byte, slot encoded in opcode.
          slot := instruction - OP_GET_LOCAL_0;
          Assert(NativeUInt(@frame^.slots[slot]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_N: slot out of stack bounds');
          if stack.StackTop >= stack.CapacityEnd then
            pushStackGrow(stack, frame^.slots[slot])
          else
          begin
            stack.StackTop^ := frame^.slots[slot];
            Inc(stack.StackTop);
          end;
        end;

        OP_SET_LOCAL: begin
          slot := ip^; Inc(ip);
          Assert(NativeUInt(@frame^.slots[slot]) < NativeUInt(stack.StackTop), 'OP_SET_LOCAL: slot out of stack bounds');
          frame^.slots[slot] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_0..OP_SET_LOCAL_7: begin
          // Fast-slot set: no operand byte; leaves value on stack (assignment
          // expression semantics, identical to OP_SET_LOCAL).
          slot := instruction - OP_SET_LOCAL_0;
          Assert(NativeUInt(@frame^.slots[slot]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_N: slot out of stack bounds');
          frame^.slots[slot] := stack.StackTop[-1];
        end;

        OP_JUMP: begin
          offset := ip^ shl 8; Inc(ip);
          offset := offset or ip^; Inc(ip);
          Inc(ip, offset);
          {$IFOPT C+}
          AssertFrameCode('OP_JUMP');
          Assert(NativeUInt(ip) <= NativeUInt(frame^.closure^.func^.chunk^.Code) + NativeUInt(frame^.closure^.func^.chunk^.Count), 'OP_JUMP: ip past end of code');
          {$ENDIF}
        end;

        OP_JUMP_IF_FALSE: begin
          offset := ip^ shl 8; Inc(ip);
          offset := offset or ip^; Inc(ip);
          if isFalsey(stack.StackTop[-1]) then
            Inc(ip, offset);
        end;

        OP_JUMP_IF_FALSE_POP: begin
          // Fused: pop top, then jump if popped value was falsy.
          // Replaces OP_JUMP_IF_FALSE + OP_POP pair in if/while/for.
          offset := ip^ shl 8; Inc(ip);
          offset := offset or ip^; Inc(ip);
           {$IFOPT C+}
           AssertStackIsNotEmpty(vm.Stack);
           {$ENDIF}
          Dec(stack.StackTop);
          if isFalsey(stack.StackTop^) then
            Inc(ip, offset);
        end;

        OP_LESS_JUMP_IF_FALSE: begin
          // Fused OP_LESS + OP_JUMP_IF_FALSE_POP: compare two numbers,
          // pop both, jump if NOT (a < b). Saves one dispatch cycle.
          offset := ip^ shl 8; Inc(ip);
          offset := offset or ip^; Inc(ip);
          if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
          begin
            runtimeError('Operands must be numbers.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          if not (GetNumber(stack.StackTop[-2]) < GetNumber(stack.StackTop[-1])) then
            Inc(ip, offset);
          Dec(stack.StackTop, 2);
        end;

        OP_GET_LOCAL_CONST_SUBTRACT: begin
          // Fused GET_LOCAL + CONSTANT + SUBTRACT. Operands: <slot> <const_idx>.
          // Pushes locals[slot] - constants[const_idx]. Saves two dispatch cycles.
          slot := ip^; Inc(ip);
          index := ip^; Inc(ip);
          if stack.StackTop >= stack.CapacityEnd then
            pushStackGrow(stack, CreateNumber(
              GetNumber(frame^.slots[slot]) - GetNumber(constants[index])))
          else begin
            stack.StackTop^ := CreateNumber(
              GetNumber(frame^.slots[slot]) - GetNumber(constants[index]));
            Inc(stack.StackTop);
          end;
        end;

        OP_ADD_SET_LOCAL_POP: begin
          // Fused ADD + SET_LOCAL + POP. Operand: <slot>.
          // Adds top two stack values, stores to local slot, pops both.
          // Saves two dispatch cycles per loop increment / accumulator update.
          slot := ip^; Inc(ip);
          if isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2]) then
          begin
            frame^.slots[slot] := CreateNumber(
              GetNumber(stack.StackTop[-2]) + GetNumber(stack.StackTop[-1]));
            Dec(stack.StackTop, 2);
          end
          else if isString(stack.StackTop[-1]) and isString(stack.StackTop[-2]) then
          begin
            Concatenate(vm.stack, vm.MemTracker);
            frame^.slots[slot] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else
          begin
            runtimeError('Operands must be two numbers or two strings.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;


        OP_LOOP: begin
          offset := ip^ shl 8; Inc(ip);
          offset := offset or ip^; Inc(ip);
          Dec(ip, offset);
          {$IFOPT C+}
          AssertFrameCode('OP_LOOP');
          Assert(NativeUInt(ip) >= NativeUInt(frame^.closure^.func^.chunk^.Code), 'OP_LOOP: ip before start of code');
          {$ENDIF}
        end;

        OP_DEFINE_GLOBAL: begin
          value := constants[ip^];
          Inc(ip);
          {$IFOPT C+}AssertValueIsString(value);{$ENDIF}
          TableSet(vm.Globals, ValueToString(value), stack.StackTop[-1], vm.MemTracker);
          Dec(stack.StackTop);
        end;


        OP_CLOSURE: begin
          value := constants[ip^];
          Inc(ip);
          Assert(isFunction(value), 'OP_CLOSURE: constant is not a function');
          func := pObjFunction(GetObject(value));
          closure := newClosure(func, VM.MemTracker);
          pushStack(stack, CreateObject(pObj(closure)));
          for i := 0 to closure^.upvalueCount - 1 do
          begin
            isLocal := ip^; Inc(ip);
            index := ip^; Inc(ip);
            if isLocal = 1 then
              closure^.upvalues^[i] := captureUpvalue(
                pValue(NativeUInt(frame^.slots) + NativeUInt(index) * SizeOf(TValue)))
            else
            begin
              {$IFOPT C+}AssertUpvalueIndex(index, 'OP_CLOSURE');{$ENDIF}
              closure^.upvalues^[i] := frame^.closure^.upvalues^[index];
            end;
          end;
        end;

        OP_GET_UPVALUE: begin
          slot := ip^; Inc(ip);
          {$IFOPT C+}
          AssertUpvalueIndex(slot, 'OP_GET_UPVALUE');
          Assert(frame^.closure^.upvalues^[slot] <> nil, 'OP_GET_UPVALUE: upvalue is nil');
          {$ENDIF}
          pushStack(stack, frame^.closure^.upvalues^[slot]^.location^);
        end;

        OP_SET_UPVALUE: begin
          slot := ip^; Inc(ip);
          {$IFOPT C+}
          AssertUpvalueIndex(slot, 'OP_SET_UPVALUE');
          Assert(frame^.closure^.upvalues^[slot] <> nil, 'OP_SET_UPVALUE: upvalue is nil');
          {$ENDIF}
          frame^.closure^.upvalues^[slot]^.location^ := stack.StackTop[-1];
        end;

        OP_CLOSE_UPVALUE: begin
          closeUpvalues(pValue(NativeUInt(stack.StackTop) - SizeOf(TValue)));
          Dec(stack.StackTop);
        end;


        OP_DEFINE_GLOBAL_LONG: begin
          i := ip^; Inc(ip);
          i := i or (ip^ shl 8); Inc(ip);
          i := i or (ip^ shl 16); Inc(ip);
          value := constants[i];
          AssertValueIsString(value);
          TableSet(vm.Globals, ValueToString(value), stack.StackTop[-1], vm.MemTracker);
          Dec(stack.StackTop);
        end;

        OP_GET_GLOBAL_LONG: begin
          i := ip^; Inc(ip);
          i := i or (ip^ shl 8); Inc(ip);
          i := i or (ip^ shl 16); Inc(ip);
          value := constants[i];
          {$IFOPT C+}
          AssertValueIsString(value);
          {$ENDIF}
          if not TableGet(vm.Globals, ValueToString(value), ValueB) then
          begin
            runtimeError('Undefined variable ''' + String(ObjStringToAnsiString(ValueToString(value))) + '''.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          pushStack(stack, ValueB);
        end;

        OP_SET_GLOBAL_LONG: begin
          i := ip^; Inc(ip);
          i := i or (ip^ shl 8); Inc(ip);
          i := i or (ip^ shl 16); Inc(ip);
          value := constants[i];
          AssertValueIsString(value);
          if TableSet(vm.Globals, ValueToString(value), stack.StackTop[-1], vm.MemTracker) then
          begin
            TableDelete(vm.Globals, ValueToString(value));
            runtimeError('Undefined variable ''' + String(ObjStringToAnsiString(ValueToString(value))) + '''.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_CLOSURE_LONG: begin
          i := ip^; Inc(ip);
          i := i or (ip^ shl 8); Inc(ip);
          i := i or (ip^ shl 16); Inc(ip);
          value := constants[i];
          Assert(isFunction(value), 'OP_CLOSURE_LONG: constant is not a function');
          func := pObjFunction(GetObject(value));
          closure := newClosure(func, VM.MemTracker);
          pushStack(stack, CreateObject(pObj(closure)));
          for i := 0 to closure^.upvalueCount - 1 do
          begin
            isLocal := ip^; Inc(ip);
            index := ip^; Inc(ip);
            if isLocal = 1 then
              closure^.upvalues^[i] := captureUpvalue(
                pValue(NativeUInt(frame^.slots) + NativeUInt(index) * SizeOf(TValue)))
            else
            begin
              {$IFOPT C+}AssertUpvalueIndex(index, 'OP_CLOSURE_LONG');{$ENDIF}
              closure^.upvalues^[i] := frame^.closure^.upvalues^[index];
            end;
          end;
        end;

        OP_RECORD: begin
          // Read field count and field name constant indices (1 byte each)
          argCount := ip^; Inc(ip); // fieldCount
          for i := 0 to argCount - 1 do
          begin
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_RECORD field name');
            AssertFrameValues('OP_RECORD field name');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_RECORD: field name constant is not a string');
            recFieldNames[i] := ValueToString(value);
          end;
          // Read the record type name (1 byte)
          recNameIdx := ip^; Inc(ip);
          {$IFOPT C+}
          AssertConstantIndex(recNameIdx, 'OP_RECORD type name');
          AssertFrameValues('OP_RECORD type name');
          {$ENDIF}
          value := constants[recNameIdx];
          Assert(isString(value), 'OP_RECORD: type name constant is not a string');
          recTypeName := ValueToString(value);
          // Allocate persistent field names array
          recFieldNamesCopy := nil;
          if argCount > 0 then
          begin
            Allocate(Pointer(recFieldNamesCopy), 0, argCount * SizeOf(pObjString), VM.MemTracker);
            for i := 0 to argCount - 1 do
              recFieldNamesCopy[i] := recFieldNames[i];
          end;
          recType := newRecordType(recTypeName, argCount, recFieldNamesCopy, VM.MemTracker);
          pushStack(stack, CreateObject(pObj(recType)));
        end;

        OP_RECORD_LONG: begin
          // Read field count and field name constant indices (3 bytes each, 24-bit LE)
          argCount := ip^; Inc(ip); // fieldCount
          for i := 0 to argCount - 1 do
          begin
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_RECORD_LONG field name');
            AssertFrameValues('OP_RECORD_LONG field name');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_RECORD_LONG: field name constant is not a string');
            recFieldNames[i] := ValueToString(value);
          end;
          // Read the record type name (3 bytes, 24-bit LE)
          recNameIdx := ip^; Inc(ip);
          recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
          recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
          {$IFOPT C+}
          AssertConstantIndex(recNameIdx, 'OP_RECORD_LONG type name');
          AssertFrameValues('OP_RECORD_LONG type name');
          {$ENDIF}
          value := constants[recNameIdx];
          Assert(isString(value), 'OP_RECORD_LONG: type name constant is not a string');
          recTypeName := ValueToString(value);
          // Allocate persistent field names array
          recFieldNamesCopy := nil;
          if argCount > 0 then
          begin
            Allocate(Pointer(recFieldNamesCopy), 0, argCount * SizeOf(pObjString), VM.MemTracker);
            for i := 0 to argCount - 1 do
              recFieldNamesCopy[i] := recFieldNames[i];
          end;
          recType := newRecordType(recTypeName, argCount, recFieldNamesCopy, VM.MemTracker);
          pushStack(stack, CreateObject(pObj(recType)));
        end;

        OP_GET_PROPERTY: begin
          if isRecord(stack.StackTop[-1]) then
          begin
            rec := pObjRecord(GetObject(stack.StackTop[-1]));
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_GET_PROPERTY');
            AssertFrameValues('OP_GET_PROPERTY');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_GET_PROPERTY: name constant is not a string');
            fieldName := ValueToString(value);
            fieldIdx := findFieldIndex(rec^.recordType, fieldName);
            if fieldIdx < 0 then
            begin
              runtimeError('Undefined property ''' + String(ObjStringToAnsiString(fieldName)) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop);
            pushStack(stack, rec^.fields[fieldIdx]);
          end
          else if isNativeObject(stack.StackTop[-1]) then
          begin
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-1]));
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_GET_PROPERTY');
            AssertFrameValues('OP_GET_PROPERTY');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_GET_PROPERTY: name constant is not a string');
            rttiPropName := ValueToString(value);
            if not RttiGetProperty(nativeObj^.instance, nativeObj^.classInfo, rttiPropName, vm.MemTracker, rttiResult) then
            begin
              if VM.RuntimeErrorStr = '' then
                runtimeError('Undefined property ''' + ObjStringToWideStr(rttiPropName) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop);
            pushStack(stack, rttiResult);
          end
          else
          begin
            recNameIdx := ip^; Inc(ip); // consume operand
            runtimeError('Only records and native objects have properties.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_SET_PROPERTY: begin
          if isRecord(stack.StackTop[-2]) then
          begin
            rec := pObjRecord(GetObject(stack.StackTop[-2]));
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_SET_PROPERTY');
            AssertFrameValues('OP_SET_PROPERTY');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_SET_PROPERTY: name constant is not a string');
            fieldName := ValueToString(value);
            fieldIdx := findFieldIndex(rec^.recordType, fieldName);
            if fieldIdx < 0 then
            begin
              runtimeError('Undefined property ''' + String(ObjStringToAnsiString(fieldName)) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            rec^.fields[fieldIdx] := stack.StackTop[-1];
            Dec(stack.StackTop);

            value := stack.StackTop^;
            Dec(stack.StackTop);
            pushStack(stack, value);
          end
          else if isNativeObject(stack.StackTop[-2]) then
          begin
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-2]));
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_SET_PROPERTY');
            AssertFrameValues('OP_SET_PROPERTY');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_SET_PROPERTY: name constant is not a string');
            rttiPropName := ValueToString(value);
            if not RttiSetProperty(nativeObj^.instance, nativeObj^.classInfo, rttiPropName, stack.StackTop[-1]) then
            begin
              if VM.RuntimeErrorStr = '' then
                runtimeError('Undefined or read-only property ''' + ObjStringToWideStr(rttiPropName) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop);

            value := stack.StackTop^;
            Dec(stack.StackTop);
            pushStack(stack, value);
          end
          else
          begin
            recNameIdx := ip^; Inc(ip); // consume operand
            runtimeError('Only records and native objects have properties.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_INVOKE: begin
          invokeNameIdx := ip^; Inc(ip);
          invokeArgCount := ip^; Inc(ip);
          {$IFOPT C+}
          AssertConstantIndex(invokeNameIdx, 'OP_INVOKE');
          AssertFrameValues('OP_INVOKE');
          {$ENDIF}
          // The receiver is on the stack below the arguments
          value := stack.StackTop[-1-invokeArgCount];
          // Get method name from constant pool
          ValueB := constants[invokeNameIdx];
          Assert(isString(ValueB), 'OP_INVOKE: method name constant is not a string');
          invokeMethodName := ValueToString(ValueB);
          if isDictionary(value) then
          begin
            if not InvokeDictMethod(pObjDictionary(GetObject(value)), invokeMethodName,
              invokeArgCount, pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount) * SizeOf(TValue)), nativeResult) then
            begin
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            stack.StackTop := pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount + 1) * SizeOf(TValue));
            pushStack(stack, nativeResult);
          end
          else if isNativeObject(value) then
          begin
            nativeObj := pObjNativeObject(GetObject(value));
            if findNativeMethod(nativeObj^.classInfo, invokeMethodName, nativeMethod) then
            begin
              // Call manual wrapper method
              nativeResult := nativeMethod(nativeObj^.instance, invokeArgCount,
                pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount) * SizeOf(TValue)));
              if VM.RuntimeErrorStr <> '' then
              begin
                Result.code := INTERPRET_RUNTIME_ERROR;
                Exit;
              end;
            end
            else if RttiInvokeMethod(nativeObj^.instance, nativeObj^.classInfo,
              invokeMethodName, invokeArgCount,
              pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount) * SizeOf(TValue)),
              VM.MemTracker, nativeResult) then
            begin
              // RTTI method call succeeded
            end
            else
            begin
              if VM.RuntimeErrorStr = '' then
                runtimeError('Undefined method ''' + ObjStringToWideStr(invokeMethodName) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Pop args + receiver, push result
            stack.StackTop := pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount + 1) * SizeOf(TValue));
            pushStack(stack, nativeResult);
          end
          else
          begin
            runtimeError('Only dictionaries and native objects have methods.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_INVOKE_LONG: begin
          invokeNameIdx := ip^; Inc(ip);
          invokeNameIdx := invokeNameIdx or (ip^ shl 8); Inc(ip);
          invokeNameIdx := invokeNameIdx or (ip^ shl 16); Inc(ip);
          invokeArgCount := ip^; Inc(ip);
          {$IFOPT C+}
          AssertConstantIndex(invokeNameIdx, 'OP_INVOKE_LONG');
          AssertFrameValues('OP_INVOKE_LONG');
          {$ENDIF}
          value := stack.StackTop[-1-invokeArgCount];
          ValueB := constants[invokeNameIdx];
          Assert(isString(ValueB), 'OP_INVOKE_LONG: method name constant is not a string');
          invokeMethodName := ValueToString(ValueB);
          if isDictionary(value) then
          begin
            if not InvokeDictMethod(pObjDictionary(GetObject(value)), invokeMethodName,
              invokeArgCount, pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount) * SizeOf(TValue)), nativeResult) then
            begin
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            stack.StackTop := pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount + 1) * SizeOf(TValue));
            pushStack(stack, nativeResult);
          end
          else if isNativeObject(value) then
          begin
            nativeObj := pObjNativeObject(GetObject(value));
            if findNativeMethod(nativeObj^.classInfo, invokeMethodName, nativeMethod) then
            begin
              nativeResult := nativeMethod(nativeObj^.instance, invokeArgCount,
                pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount) * SizeOf(TValue)));
              if VM.RuntimeErrorStr <> '' then
              begin
                Result.code := INTERPRET_RUNTIME_ERROR;
                Exit;
              end;
            end
            else if RttiInvokeMethod(nativeObj^.instance, nativeObj^.classInfo,
              invokeMethodName, invokeArgCount,
              pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount) * SizeOf(TValue)),
              VM.MemTracker, nativeResult) then
            begin
              // RTTI method call succeeded
            end
            else
            begin
              if VM.RuntimeErrorStr = '' then
                runtimeError('Undefined method ''' + ObjStringToWideStr(invokeMethodName) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            stack.StackTop := pValue(NativeUInt(stack.StackTop) - NativeUInt(invokeArgCount + 1) * SizeOf(TValue));
            pushStack(stack, nativeResult);
          end
          else
          begin
            runtimeError('Only dictionaries and native objects have methods.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_GET_PROPERTY_LONG: begin
          if isRecord(stack.StackTop[-1]) then
          begin
            rec := pObjRecord(GetObject(stack.StackTop[-1]));
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_GET_PROPERTY_LONG');
            AssertFrameValues('OP_GET_PROPERTY_LONG');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_GET_PROPERTY_LONG: name constant is not a string');
            fieldName := ValueToString(value);
            fieldIdx := findFieldIndex(rec^.recordType, fieldName);
            if fieldIdx < 0 then
            begin
              runtimeError('Undefined property ''' + String(ObjStringToAnsiString(fieldName)) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop);
            pushStack(stack, rec^.fields[fieldIdx]);
          end
          else if isNativeObject(stack.StackTop[-1]) then
          begin
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-1]));
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_GET_PROPERTY_LONG');
            AssertFrameValues('OP_GET_PROPERTY_LONG');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_GET_PROPERTY_LONG: name constant is not a string');
            rttiPropName := ValueToString(value);
            if not RttiGetProperty(nativeObj^.instance, nativeObj^.classInfo, rttiPropName, vm.MemTracker, rttiResult) then
            begin
              if VM.RuntimeErrorStr = '' then
                runtimeError('Undefined property ''' + ObjStringToWideStr(rttiPropName) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop);
            pushStack(stack, rttiResult);
          end
          else
          begin
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            runtimeError('Only records and native objects have properties.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_SET_PROPERTY_LONG: begin
          if isRecord(stack.StackTop[-2]) then
          begin
            rec := pObjRecord(GetObject(stack.StackTop[-2]));
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_SET_PROPERTY_LONG');
            AssertFrameValues('OP_SET_PROPERTY_LONG');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_SET_PROPERTY_LONG: name constant is not a string');
            fieldName := ValueToString(value);
            fieldIdx := findFieldIndex(rec^.recordType, fieldName);
            if fieldIdx < 0 then
            begin
              runtimeError('Undefined property ''' + String(ObjStringToAnsiString(fieldName)) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            rec^.fields[fieldIdx] := stack.StackTop[-1];
            Dec(stack.StackTop);

            value := stack.StackTop^;
            Dec(stack.StackTop);
            pushStack(stack, value);
          end
          else if isNativeObject(stack.StackTop[-2]) then
          begin
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-2]));
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_SET_PROPERTY_LONG');
            AssertFrameValues('OP_SET_PROPERTY_LONG');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_SET_PROPERTY_LONG: name constant is not a string');
            rttiPropName := ValueToString(value);
            if not RttiSetProperty(nativeObj^.instance, nativeObj^.classInfo, rttiPropName, stack.StackTop[-1]) then
            begin
              if VM.RuntimeErrorStr = '' then
                runtimeError('Undefined or read-only property ''' + ObjStringToWideStr(rttiPropName) + '''.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop);

            value := stack.StackTop^;
            Dec(stack.StackTop);
            pushStack(stack, value);
          end
          else
          begin
            recNameIdx := ip^; Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 8); Inc(ip);
            recNameIdx := recNameIdx or (ip^ shl 16); Inc(ip);
            runtimeError('Only records and native objects have properties.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_GET_SUBSCRIPT: begin
          // Stack: [... object, index/key] -> [... result]
          Dec(stack.StackTop);   // index/key

          value := stack.StackTop^;
          Dec(stack.StackTop);  // object

          ValueB := stack.StackTop^;
          if isArray(ValueB) then
          begin
            if not isNumber(value) then
            begin
              runtimeError('Array index must be a number.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            i := Trunc(GetNumber(value));
            if (i < 0) or (i >= pObjArray(GetObject(ValueB))^.Count) then
            begin
              runtimeError('Array index ' + IntToStr(i) + ' out of bounds [0, ' + IntToStr(pObjArray(GetObject(ValueB))^.Count - 1) + '].');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            pushStack(stack, pObjArray(GetObject(ValueB))^.Elements[i]);
          end
          else if isDictionary(ValueB) then
          begin
            if not DictGet(pObjDictionary(GetObject(ValueB)), value, ValueC) then
            begin
              runtimeError('Key not found in dictionary.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            pushStack(stack, ValueC);
          end
          else if isNativeObject(ValueB) then
          begin
            nativeObj := pObjNativeObject(GetObject(ValueB));
            if (nativeObj^.classInfo <> nil) and nativeObj^.classInfo^.rttiEnabled then
            begin
              idxProp := nil;
              for idxProp in RttiCtx.GetType(nativeObj^.classInfo^.rttiClass).GetIndexedProperties do
                if idxProp.IsDefault and idxProp.IsReadable then
                  Break;
              if (idxProp <> nil) and idxProp.IsDefault and idxProp.IsReadable then
              begin
                idxParams := idxProp.ReadMethod.GetParameters;
                if Length(idxParams) <> 1 then
                begin
                  runtimeError('Default indexed property requires exactly 1 index parameter.');
                  result.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
                try
                  idxDelphiArgs[0] := LoxValueToDelphi(value, idxParams[0].ParamType.Handle);
                  if VM.RuntimeErrorStr <> '' then begin result.code := INTERPRET_RUNTIME_ERROR; exit; end;
                  idxDelphiVal := idxProp.GetValue(TObject(nativeObj^.instance), [idxDelphiArgs[0]]);
                  pushStack(stack, DelphiValueToLox(idxDelphiVal, vm.MemTracker));
                except
                  on E: Exception do
                  begin
                    runtimeError('Indexed property get failed: ' + E.Message);
                    result.code := INTERPRET_RUNTIME_ERROR;
                    exit;
                  end;
                end;
              end
              else
              begin
                runtimeError('Native object has no default indexed property.');
                result.code := INTERPRET_RUNTIME_ERROR;
                exit;
              end;
            end
            else
            begin
              runtimeError('Native object does not support subscript access.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
          end
          else
          begin
            runtimeError('Only arrays and dictionaries support subscript access.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_SET_SUBSCRIPT: begin
          // Stack: [... object, index/key, value] -> [... value]
          // Keep values on stack during DictSet which may trigger GC
          if isArray(stack.StackTop[-3]) then
          begin
            Dec(stack.StackTop);    // value to assign

            value := stack.StackTop^;
            Dec(stack.StackTop);   // index/key

            ValueB := stack.StackTop^;
            if not isNumber(ValueB) then
            begin
              runtimeError('Array index must be a number.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            i := Trunc(GetNumber(ValueB));
            if (i < 0) or (i >= pObjArray(GetObject(stack.StackTop[-1]))^.Count) then
            begin
              runtimeError('Array index ' + IntToStr(i) + ' out of bounds [0, ' + IntToStr(pObjArray(GetObject(stack.StackTop[-1]))^.Count - 1) + '].');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            pObjArray(GetObject(stack.StackTop[-1]))^.Elements[i] := value;
            Dec(stack.StackTop); // pop the array
            pushStack(stack, value); // leave the assigned value
          end
          else if isDictionary(stack.StackTop[-3]) then
          begin
            // Stack: [... dict, key, value]
            // Leave all on stack during DictSet to protect from GC
            value := stack.StackTop[-1];   // value
            ValueB := stack.StackTop[-2];  // key
            DictSet(pObjDictionary(GetObject(stack.StackTop[-3])), ValueB, value, vm.MemTracker);
            // Pop value, key, dict; push value as result
            Dec(stack.StackTop);
            Dec(stack.StackTop);
            Dec(stack.StackTop);
            pushStack(stack, value);
          end
          else if isNativeObject(stack.StackTop[-3]) then
          begin
            // Stack: [... nativeObj, index, value]
            Dec(stack.StackTop);     // value to assign

            value := stack.StackTop^;
            Dec(stack.StackTop);    // index

            ValueB := stack.StackTop^;
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-1]));
            if (nativeObj^.classInfo <> nil) and nativeObj^.classInfo^.rttiEnabled then
            begin
              idxProp := nil;
              for idxProp in RttiCtx.GetType(nativeObj^.classInfo^.rttiClass).GetIndexedProperties do
                if idxProp.IsDefault and idxProp.IsWritable then
                  Break;
              if (idxProp <> nil) and idxProp.IsDefault and idxProp.IsWritable then
              begin
                idxParams := idxProp.WriteMethod.GetParameters;
                // WriteMethod params: index + value ? we only marshal the index here
                if Length(idxParams) < 1 then
                begin
                  runtimeError('Default indexed property has no index parameter.');
                  result.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
                try
                  idxDelphiArgs[0] := LoxValueToDelphi(ValueB, idxParams[0].ParamType.Handle);
                  if VM.RuntimeErrorStr <> '' then begin result.code := INTERPRET_RUNTIME_ERROR; exit; end;
                  idxDelphiVal := LoxValueToDelphi(value, idxProp.PropertyType.Handle);
                  if VM.RuntimeErrorStr <> '' then begin result.code := INTERPRET_RUNTIME_ERROR; exit; end;
                  idxProp.SetValue(TObject(nativeObj^.instance), [idxDelphiArgs[0]], idxDelphiVal);
                  Dec(stack.StackTop); // pop the native object
                  pushStack(stack, value); // leave the assigned value
                except
                  on E: Exception do
                  begin
                    runtimeError('Indexed property set failed: ' + E.Message);
                    result.code := INTERPRET_RUNTIME_ERROR;
                    exit;
                  end;
                end;
              end
              else
              begin
                runtimeError('Native object has no writable default indexed property.');
                result.code := INTERPRET_RUNTIME_ERROR;
                exit;
              end;
            end
            else
            begin
              runtimeError('Native object does not support subscript assignment.');
              result.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
          end
          else
          begin
            runtimeError('Only arrays and dictionaries support subscript assignment.');
            result.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_ARRAY_LITERAL: begin
          // Operand: 1-byte element count
          // Stack before: [... elem0, elem1, ..., elemN-1] -> [... array]
          argCount := ip^; Inc(ip);
          // Create the array and push it to protect from GC during Allocate
          value := CreateObject(pObj(newArray(vm.MemTracker)));
          pushStack(stack, value);
          // Pre-grow the array elements and copy in. Both the array (top of
          // stack) and the source elements (below it) are GC-rooted via Stack.
          if argCount > 0 then
          begin
            Allocate(Pointer(pObjArray(GetObject(value))^.Elements), 0, argCount * SizeOf(TValue), vm.MemTracker);
            pObjArray(GetObject(value))^.Capacity := argCount;
            pObjArray(GetObject(value))^.Count := argCount;
            // Elements live at StackTop[-1-argCount .. -2]; array is at [-1].
            for i := 0 to argCount - 1 do
              pObjArray(GetObject(value))^.Elements[i] := stack.StackTop[-1 - argCount + i];
          end;
          // Bulk-collapse the window: overwrite the first element slot with
          // the array and drop the remaining argCount slots in one step.
          // Replaces popStack x (argCount + 1) + pushStack with N+2 pointer/asserts.
          stack.StackTop[-1 - argCount] := value;
          Dec(stack.StackTop, argCount);
        end;

        OP_DICT_LITERAL: begin
          // Operand: 1-byte pair count
          // Stack before: [... key0, val0, key1, val1, ..., keyN-1, valN-1] -> [... dict]
          argCount := ip^; Inc(ip);
          // Create dictionary and push to stack for GC safety during DictSet.
          value := CreateObject(pObj(newDictionary(vm.MemTracker)));
          pushStack(stack, value);
          // Insert pairs from below the dict on the stack.
          // Pairs occupy StackTop[-1-2*argCount .. -2]; dict is at [-1].
          for i := 0 to argCount - 1 do
          begin
            ValueB := stack.StackTop[-1 - 2 * argCount + 2 * i];     // key
            ValueC := stack.StackTop[-1 - 2 * argCount + 2 * i + 1]; // value
            DictSet(pObjDictionary(GetObject(value)), ValueB, ValueC, vm.MemTracker);
          end;
          // Bulk-collapse: overwrite the first pair slot with the dict and
          // drop the remaining 2*argCount slots in one step.
          stack.StackTop[-1 - 2 * argCount] := value;
          Dec(stack.StackTop, 2 * argCount);
        end;

      else
        begin
          runtimeError('Unknown opcode: ' + IntToStr(instruction));
          result.code := INTERPRET_RUNTIME_ERROR;
          exit;
        end;
      end;
    end;

end;

function InvokeCallback(closure: TValue; const args: array of TValue;
  out returnVal: TValue): TInterpretResult;
var
  closurePtr: pObjClosure;
  i, argCount: Integer;
begin
  Result := Default(TInterpretResult);
  returnVal := CreateNilValue;

  // Validate closure
  if not isClosure(closure) then
  begin
    runtimeError('InvokeCallback: value is not a callable closure.');
    Result.code := INTERPRET_RUNTIME_ERROR;
    Exit;
  end;

  closurePtr := pObjClosure(GetObject(closure));
  argCount := Length(args);

  // Arity check
  if argCount <> closurePtr^.func^.arity then
  begin
    runtimeError('InvokeCallback: expected ' + IntToStr(closurePtr^.func^.arity) +
      ' arguments but got ' + IntToStr(argCount) + '.');
    Result.code := INTERPRET_RUNTIME_ERROR;
    Exit;
  end;

  // Stack overflow check
  if VM.FrameCount >= FRAMES_MAX then
  begin
    runtimeError('InvokeCallback: stack overflow (max ' + IntToStr(FRAMES_MAX) + ' frames).');
    Result.code := INTERPRET_RUNTIME_ERROR;
    Exit;
  end;

  // Push closure (slot 0 of new frame)
  pushStack(VM.Stack, closure);
  // Push arguments
  for i := 0 to argCount - 1 do
    pushStack(VM.Stack, args[i]);

  // Set up call frame (same as OP_CALL closure fast-path)
  VM.Frames[VM.FrameCount].closure := closurePtr;
  VM.Frames[VM.FrameCount].ip := closurePtr^.func^.chunk^.Code;
  VM.Frames[VM.FrameCount].slots := VM.Stack.StackTop;
  Dec(VM.Frames[VM.FrameCount].slots, argCount + 1);
  Inc(VM.FrameCount);

  // Run until this frame returns
  Result := Run(VM.FrameCount - 1);

  // Pop return value (Run leaves it on the stack for re-entrant calls)
  if Result.code = INTERPRET_OK then
  begin
    returnVal := Result.value;
    // Clean up: return value was pushed by OP_RETURN onto the caller's stack
    // position. For a re-entrant call, OP_RETURN already set StackTop to
    // frame^.slots and pushed the value. We need to pop it.
    Dec(VM.Stack.StackTop);
  end;
end;

procedure RegisterGCRoot(var slot: TValue);
begin
  // Grow dynamic array if needed
  if VM.ExtraRootCount >= Length(VM.ExtraRoots) then
  begin
    if Length(VM.ExtraRoots) = 0 then
      SetLength(VM.ExtraRoots, 8)
    else
      SetLength(VM.ExtraRoots, Length(VM.ExtraRoots) * 2);
  end;
  VM.ExtraRoots[VM.ExtraRootCount] := @slot;
  Inc(VM.ExtraRootCount);
end;

procedure UnregisterGCRoot(var slot: TValue);
var
  i: Integer;
begin
  for i := 0 to VM.ExtraRootCount - 1 do
  begin
    if VM.ExtraRoots[i] = @slot then
    begin
      // Swap with last and shrink
      VM.ExtraRoots[i] := VM.ExtraRoots[VM.ExtraRootCount - 1];
      Dec(VM.ExtraRootCount);
      Exit;
    end;
  end;
end;



procedure ResetStack(var stack : pStack);
begin
   {$IFOPT C+}
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  {$ENDIF}
  Stack.StackTop := Stack.Values;
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Stack.StackTop = Stack.Values, 'ResetStack exit: StackTop should equal Values');
  {$ENDIF}
end;



function peekStack(Stack: pStack; DistanceFromTop: Integer): TValue; inline;
begin
  {$IFOPT C+}
  AssertStackIsAssigned(Stack);
  AssertStackValuesIsAssigned(Stack);
  AssertStackIsNotEmpty(Stack);
  AssertDistanceIsNotNegative(DistanceFromTop);
  Assert(DistanceFromTop < (Stack.StackTop - Stack.Values), 'peekStack: distance out of range');
  {$ENDIF}
  Result := (Stack.StackTop - 1 - DistanceFromTop)^;
end;


function popStack(stack : pStack) : TValue; inline;
begin
   {$IFOPT C+}
   AssertStackIsAssigned(Stack);
   AssertStackTopIsNotNil(Stack);
   AssertStackValuesIsAssigned(Stack);
   AssertStackIsNotEmpty(Stack);
   {$ENDIF}
   Dec(Stack.StackTop);
   result := Stack.StackTop^;

   // ---- Exit assertions ----
   {$IFOPT C+}
   Assert(Stack.StackTop >= Stack.Values, 'popStack exit: StackTop below Values');
   {$ENDIF}
end;

//entry point into vm
function CompileAndRun(source : pAnsiChar) : TInterpretResult;
var
  closure : pObjClosure;
begin
    {$IFOPT C+}
   AssertSourceCodeIsAssigned(source);
   AssertVMIsAssigned;
   {$ENDIF}

   closure := compile(source);
   if closure = nil then
   begin
     Result.code :=  INTERPRET_COMPILE_ERROR;
     Result.ErrorStr := Parser.ErrorStr;
     Exit;
   end;

   // Push the script closure onto the stack
   pushStack(VM.Stack, CreateObject(pObj(closure)));

   // Set up the first call frame
   VM.Frames[0].closure := closure;
   VM.Frames[0].ip := closure^.func^.chunk^.Code;
   VM.Frames[0].slots := VM.Stack.Values;
   VM.FrameCount := 1;

   try
     Result := Run;
     {$IFDEF OPCODE_PROFILING}
     DumpOpcodePairs;
     {$ENDIF}
   except
     // Stack overflow (or any other VM-internal abort raised as
     // ELoxRuntimeError) unwinds out of Run mid-dispatch. Translate to
     // a clean runtime-error result and reset transient VM state so a
     // subsequent CompileAndRun on the same initVM session starts fresh.
     on E: ELoxHalt do
     begin
       VM.FrameCount := 0;
       VM.OpenUpvalues := nil;
       if VM.Stack <> nil then
         VM.Stack.StackTop := VM.Stack.Values;
       Result.code := INTERPRET_HALTED;
       Result.ExitCode := E.HaltExitCode;
       Result.ErrorStr := E.Message;
       Result.OutputStr := VM.PrintBuilder.ToString;
       Exit;
     end;
     on E: ELoxRuntimeError do
     begin
       VM.RuntimeErrorStr := E.Message;
       VM.FrameCount := 0;
       VM.OpenUpvalues := nil;
       if VM.Stack <> nil then
       begin
         VM.Stack.StackTop := VM.Stack.Values;
       end;
       Result.code := INTERPRET_RUNTIME_ERROR;
       Result.ErrorStr := E.Message;
       Result.OutputStr := VM.PrintBuilder.ToString;
       Exit;
     end;
   end;
   if Result.code = INTERPRET_RUNTIME_ERROR then
     Result.ErrorStr := VM.RuntimeErrorStr
   else if (Result.code = INTERPRET_OK) and isString(Result.value) then
     Result.ResultStr := String(ObjStringToAnsiString(ValueToString(Result.value)));
   Result.OutputStr := VM.PrintBuilder.ToString;
end;

function InterpretResult(source : pAnsiChar) : TInterpretResult;
begin
    {$IFOPT C+}
    AssertSourceCodeIsAssigned(source);
    {$ENDIF}
   initVM;
   try
     Result := CompileAndRun(source);
   finally
     FreeVM;
   end;
end;

procedure FreeObject(obj : pObj);inline;
begin
   {$IFOPT C+}
  AssertObjectIsAssigned(obj);
  AssertValidObjPointer(obj, 'FreeObject');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}
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
    okArray : begin
      if pObjArray(obj)^.Elements <> nil then
        Allocate(Pointer(pObjArray(obj)^.Elements),
                 pObjArray(obj)^.Capacity * SizeOf(TValue), 0, vm.MemTracker);
      Allocate(Pointer(obj), SizeOf(TObjArray), 0, vm.MemTracker);
    end;
    okRecordType : begin
      if pObjRecordType(obj)^.fieldNames <> nil then
        Allocate(Pointer(pObjRecordType(obj)^.fieldNames),
                 pObjRecordType(obj)^.fieldCount * SizeOf(pObjString), 0, vm.MemTracker);
      Allocate(Pointer(obj), SizeOf(TObjRecordType), 0, vm.MemTracker);
    end;
    okRecord : begin
      if pObjRecord(obj)^.fields <> nil then
        Allocate(Pointer(pObjRecord(obj)^.fields),
                 pObjRecord(obj)^.recordType^.fieldCount * SizeOf(TValue), 0, vm.MemTracker);
      Allocate(Pointer(obj), SizeOf(TObjRecord), 0, vm.MemTracker);
    end;
    okNativeObject : begin
      if pObjNativeObject(obj)^.ownsInstance and
         Assigned(pObjNativeObject(obj)^.classInfo^.destructor_) then
      begin
        // Contract: native destructors MUST NOT raise. They run mid-sweep,
        // so any raise here unwinds through Allocate -> Run and leaves the
        // GC cycle in an inconsistent state. If a destructor needs to fail,
        // fix it at the source ? do not wrap this call.
        pObjNativeObject(obj)^.classInfo^.destructor_(pObjNativeObject(obj)^.instance);
        pObjNativeObject(obj)^.instance := nil;
      end;
      Allocate(Pointer(obj), SizeOf(TObjNativeObject), 0, vm.MemTracker);
    end;
    okDictionary : begin
      if pObjDictionary(obj)^.Entries <> nil then
        Allocate(Pointer(pObjDictionary(obj)^.Entries),
                 pObjDictionary(obj)^.Capacity * SizeOf(TDictEntry), 0, vm.MemTracker);
      Allocate(Pointer(obj), SizeOf(TObjDictionary), 0, vm.MemTracker);
    end;
  else
    Assert(false, 'FreeObject: unknown or corrupt ObjectKind (possible double-free)');
  end;
end;

procedure FreeObjects(objects : pObj);
var
  obj : pObj;
  next : pObj;
begin
   {$IFOPT C+}
  AssertPointerIsNotNil(objects, 'FreeObjects - objects');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}
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
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}

  if obj = nil then Exit;
   {$IFOPT C+}
  AssertValidObjPointer(obj, 'MarkObject');
  {$ENDIF}
  if obj^.IsMarked then Exit;

  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('mark %p kind=%s', [Pointer(obj), ObjectKindStr(obj^.ObjectKind)]));
  Inc(GCLogMarks);
  {$ENDIF}

  obj^.IsMarked := true;

  // Add to gray stack
  if VM.MemTracker.GrayCount + 1 > VM.MemTracker.GrayCapacity then
  begin
    if VM.MemTracker.GrayCapacity < GRAY_STACK_INITIAL_CAPACITY then
      newCapacity := GRAY_STACK_INITIAL_CAPACITY
    else
      newCapacity := VM.MemTracker.GrayCapacity * 2;
    // Overflow check on the computed byte size before the realloc.
     {$IFOPT C+}
    Assert(newCapacity <= MaxInt div SizeOf(pObj), 'MarkObject: GrayStack byte size overflow');
    {$ENDIF}
    // Use raw ReallocMem to avoid recursive GC trigger.
    // Update GrayCapacity only AFTER a successful realloc, so an EOutOfMemory
    // exception leaves the tracker's capacity matching the still-valid old buffer.
    ReallocMem(VM.MemTracker.GrayStack, SizeOf(pObj) * newCapacity);
    VM.MemTracker.GrayCapacity := newCapacity;
    // ---- Mid assertions ----
     {$IFOPT C+}
    Assert(VM.MemTracker.GrayStack <> nil, 'MarkObject: GrayStack is nil after realloc');
    Assert(VM.MemTracker.GrayCapacity > 0, 'MarkObject: GrayCapacity should be > 0 after growth');
    {$ENDIF}
  end;

  oldGrayCount := VM.MemTracker.GrayCount;
  VM.MemTracker.GrayStack[VM.MemTracker.GrayCount] := obj;
  Inc(VM.MemTracker.GrayCount);

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(obj^.IsMarked, 'MarkObject exit: obj should be marked');
  Assert(VM.MemTracker.GrayCount = oldGrayCount + 1, 'MarkObject exit: GrayCount should have increased by 1');
  {$ENDIF}
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
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}

  // Mark the value stack
  if (VM.Stack <> nil) and (VM.Stack.StackTop > VM.Stack.Values) then
  begin
    slot := VM.Stack.Values;
    while NativeUInt(slot) < NativeUInt(VM.Stack.StackTop) do
    begin
      // Validate stack slot holds a valid value kind
      Assert(isNumber(slot^) or isBoolean(slot^) or isNill(slot^) or isObject(slot^),
        'MarkRoots: invalid value on stack');

       {$IFOPT C+}
      if isObject(slot^) then
        AssertValidObjPointer(GetObject(slot^), 'MarkRoots stack');
      {$ENDIF}


      MarkValue(slot^);
      Inc(slot);
    end;
  end;

  // Mark call frame closures
  // ---- Mid assertions ----
  Assert(VM.FrameCount >= 0, 'MarkRoots: FrameCount is negative');
  Assert(VM.FrameCount <= FRAMES_MAX, 'MarkRoots: FrameCount exceeds FRAMES_MAX');
  for i := 0 to VM.FrameCount - 1 do
  begin
     {$IFOPT C+}
    Assert(VM.Frames[i].closure <> nil, 'MarkRoots: frame closure is nil');
    AssertValidObjPointer(pObj(VM.Frames[i].closure), 'MarkRoots frame closure');
    {$ENDIF}
    MarkObject(pObj(VM.Frames[i].closure));
  end;

  // Mark open upvalues
  upvalue := VM.OpenUpvalues;
  while upvalue <> nil do
  begin
    {$IFOPT C+}
    AssertValidObjPointer(pObj(upvalue), 'MarkRoots open upvalue');

    // Verify open upvalue location points into the VM stack
    Assert(NativeUInt(upvalue^.location) >= NativeUInt(VM.Stack.Values),
      'MarkRoots: open upvalue location is below stack base');
    Assert(NativeUInt(upvalue^.location) < NativeUInt(VM.Stack.StackTop),
      'MarkRoots: open upvalue location is at or above stack top');
    {$ENDIF}
    MarkObject(pObj(upvalue));
    upvalue := upvalue^.next;
  end;

  // Mark globals table
  MarkTable(VM.Globals);

  // Mark extra roots (externally-held closures for callbacks)
  for i := 0 to VM.ExtraRootCount - 1 do
    MarkValue(VM.ExtraRoots[i]^);

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
  {$IFOPT C+}
  Assert(obj <> nil, 'BlackenObject: obj is nil');
  Assert(obj^.IsMarked, 'BlackenObject: obj is not marked (should be gray)');
  {$ENDIF}
  {$IFDEF DEBUG_LOG_GC}
  GCLog(Format('blacken %p kind=%s', [Pointer(obj), ObjectKindStr(obj^.ObjectKind)]));
  Inc(GCLogBlackens);
  {$ENDIF}

  case obj^.ObjectKind of
    okUpvalue:
      MarkValue(pObjUpvalue(obj)^.closed);
    okFunction: begin
      {$IFOPT C+}
      Assert(pObjFunction(obj)^.chunk <> nil, 'BlackenObject: function chunk is nil');
      AssertValidObjPointer(pObj(pObjFunction(obj)^.name), 'BlackenObject function name');
      {$ENDIF}
      MarkObject(pObj(pObjFunction(obj)^.name));
      MarkArray(pObjFunction(obj)^.chunk^.Constants);
    end;
    okClosure: begin
      {$IFOPT C+}
      Assert(pObjClosure(obj)^.func <> nil, 'BlackenObject: closure func is nil');
      AssertValidObjPointer(pObj(pObjClosure(obj)^.func), 'BlackenObject closure func');
      {$ENDIF}
      if pObjClosure(obj)^.upvalueCount > 0 then
        Assert(pObjClosure(obj)^.upvalues <> nil, 'BlackenObject: closure upvalues is nil with count > 0');
      MarkObject(pObj(pObjClosure(obj)^.func));
      for i := 0 to pObjClosure(obj)^.upvalueCount - 1 do
      begin
        {$IFOPT C+}
        AssertValidObjPointer(pObj(pObjClosure(obj)^.upvalues^[i]), 'BlackenObject closure upvalue');
        {$ENDIF}
        MarkObject(pObj(pObjClosure(obj)^.upvalues^[i]));
      end;
    end;
    okNative: ;
    okString: ;
    okArray: begin
      if pObjArray(obj)^.Count > 0 then
      begin
        Assert(pObjArray(obj)^.Elements <> nil, 'BlackenObject: array elements is nil with count > 0');
        for i := 0 to pObjArray(obj)^.Count - 1 do
        begin
          {$IFOPT C+}
          if isObject(pObjArray(obj)^.Elements[i]) then
            AssertValidObjPointer(GetObject(pObjArray(obj)^.Elements[i]), 'BlackenObject array element');
          {$ENDIF}
          MarkValue(pObjArray(obj)^.Elements[i]);
        end;
      end;
    end;
    okRecordType: begin
      {$IFOPT C+}
      AssertValidObjPointer(pObj(pObjRecordType(obj)^.name), 'BlackenObject recordType name');
      {$ENDIF}
      MarkObject(pObj(pObjRecordType(obj)^.name));
      for i := 0 to pObjRecordType(obj)^.fieldCount - 1 do
      begin
        {$IFOPT C+}
        AssertValidObjPointer(pObj(pObjRecordType(obj)^.fieldNames[i]), 'BlackenObject recordType fieldName');
        {$ENDIF}
        MarkObject(pObj(pObjRecordType(obj)^.fieldNames[i]));
      end;
    end;
    okRecord: begin
      {$IFOPT C+}
      AssertValidObjPointer(pObj(pObjRecord(obj)^.recordType), 'BlackenObject record type');
      {$ENDIF}
      MarkObject(pObj(pObjRecord(obj)^.recordType));
      for i := 0 to pObjRecord(obj)^.recordType^.fieldCount - 1 do
      begin
        {$IFOPT C+}
        if isObject(pObjRecord(obj)^.fields[i]) then
          AssertValidObjPointer(GetObject(pObjRecord(obj)^.fields[i]), 'BlackenObject record field');
        {$ENDIF}
        MarkValue(pObjRecord(obj)^.fields[i]);
      end;
    end;
    okNativeObject: ; // Delphi object not traced by GC
    okDictionary: begin
      if pObjDictionary(obj)^.Capacity > 0 then
      begin
        Assert(pObjDictionary(obj)^.Entries <> nil, 'BlackenObject: dictionary entries is nil with capacity > 0');
        for i := 0 to pObjDictionary(obj)^.Capacity - 1 do
        begin
          if pObjDictionary(obj)^.Entries[i].occupied then
          begin
            {$IFOPT C+}
            if isObject(pObjDictionary(obj)^.Entries[i].key) then
              AssertValidObjPointer(GetObject(pObjDictionary(obj)^.Entries[i].key), 'BlackenObject dict key');

            if isObject(pObjDictionary(obj)^.Entries[i].value) then
              AssertValidObjPointer(GetObject(pObjDictionary(obj)^.Entries[i].value), 'BlackenObject dict value');
              {$ENDIF}


            MarkValue(pObjDictionary(obj)^.Entries[i].key);
            MarkValue(pObjDictionary(obj)^.Entries[i].value);
          end;
        end;
      end;
    end;
  end;
end;

procedure TraceReferences;
var
  obj : pObj;
begin
  // ---- Entry assertions ----
  {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);

  if VM.MemTracker.GrayCount > 0 then
    Assert(VM.MemTracker.GrayStack <> nil, 'TraceReferences: GrayStack is nil with GrayCount > 0');
  {$ENDIF}
  while VM.MemTracker.GrayCount > 0 do
  begin
    Dec(VM.MemTracker.GrayCount);
    obj := VM.MemTracker.GrayStack[VM.MemTracker.GrayCount];
    // ---- Mid assertion ----
     {$IFOPT C+}
    Assert(obj <> nil, 'TraceReferences: nil object popped from gray stack');
    {$ENDIF}
    BlackenObject(obj);
  end;

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(VM.MemTracker.GrayCount = 0, 'TraceReferences exit: GrayCount should be 0');
  {$ENDIF}
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
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}

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
       {$IFOPT C+}
      Assert(not unreached^.IsMarked, 'Sweep: about to free a marked (live) object');
      {$ENDIF}

      obj := obj^.Next;
      if previous <> nil then
        previous^.Next := obj
      else
        VM.MemTracker.CreatedObjects := obj;
      FreeObject(unreached);
    end;
  end;

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(VM.MemTracker.BytesAllocated >= 0, 'Sweep exit: BytesAllocated is negative');
  {$ENDIF}
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

  Inc(VM.MemTracker.GCCollections);

  MarkRoots;
  TraceReferences;

   {$IFOPT C+}
  Assert(VM.MemTracker.GrayCount = 0, 'CollectGarbage: gray stack not drained after TraceReferences');
  {$ENDIF}

  {$IFDEF DEBUG_STRESS_GC}
  // Post-mark reachability verification: all stack roots must be marked
  if (VM.Stack <> nil) and (VM.Stack.StackTop > VM.Stack.Values) then
  begin
    var verifySlot : pValue := VM.Stack.Values;
    while NativeUInt(verifySlot) < NativeUInt(VM.Stack.StackTop) do
    begin
      if isObject(verifySlot^) and (GetObject(verifySlot^) <> nil) then
        Assert(GetObject(verifySlot^)^.IsMarked,
          'Post-mark verify: stack root object is not marked');
      Inc(verifySlot);
    end;
  end;
  var verifyIdx : integer;
  for verifyIdx := 0 to VM.FrameCount - 1 do
    Assert(pObj(VM.Frames[verifyIdx].closure)^.IsMarked,
      'Post-mark verify: frame closure is not marked');
  {$ENDIF}

  TableRemoveWhite(VM.Strings);
  Sweep;

  {$IFDEF DEBUG_STRESS_GC}
  // Tracked-allocation verification: all surviving objects must have valid structure
  begin
    var verifyObj : pObj := VM.MemTracker.CreatedObjects;
    while verifyObj <> nil do
    begin
      AssertValidObjPointer(verifyObj, 'Post-sweep verify surviving object');
      // After sweep, IsMarked is cleared for the next cycle ? do not check it
      verifyObj := verifyObj^.Next;
    end;
  end;
  // Interned string table consistency: every entry must point to a valid okString
  if (VM.Strings <> nil) and (VM.Strings.Entries <> nil) then
  begin
    var strIdx : integer;
    for strIdx := 0 to VM.Strings.CurrentCapacity - 1 do
    begin
      if VM.Strings.Entries[strIdx].key <> nil then
      begin
        AssertValidObjPointer(pObj(VM.Strings.Entries[strIdx].key), 'Post-sweep VM.Strings entry');
        Assert(VM.Strings.Entries[strIdx].key^.Obj.ObjectKind = okString,
          'Post-sweep VM.Strings: entry key is not okString');
      end;
    end;
  end;
  {$ENDIF}

  if VM.MemTracker.BytesAllocated <= MaxInt div GC_HEAP_GROW_FACTOR then
    VM.MemTracker.NextGC := VM.MemTracker.BytesAllocated * GC_HEAP_GROW_FACTOR
  else
    VM.MemTracker.NextGC := MaxInt;
  if VM.MemTracker.NextGC < GC_NEXT_MIN then
    VM.MemTracker.NextGC := GC_NEXT_MIN;

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
   {$IFOPT C+}
  AssertPointerIsNil(MemTracker, 'InitMemTracker - MemTracker not nil before initialization');
  {$ENDIF}
  new(MemTracker); //Allocate(pointer(MemTracker),0, SizeOf(MemTracker),MemTracker);
  MemTracker.CreatedObjects := nil;
  MemTracker.Roots.Stack := nil;
  MemTracker.BytesAllocated := 0;
  MemTracker.NextGC := GC_NEXT_INITIAL;
  MemTracker.GrayCount := 0;
  MemTracker.GrayCapacity := 0;
  MemTracker.GrayStack := nil;
  MemTracker.GCCollections := 0;
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(MemTracker.Roots.Stack = nil, 'InitMemTracker exit: Roots.Stack should be nil');
  Assert(MemTracker.BytesAllocated = 0, 'InitMemTracker exit: BytesAllocated should be 0');
  {$ENDIF}
end;

procedure FreeMemTracker(var MemTracker : pMemTracker);
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(MemTracker.BytesAllocated = 0, 'VM has not disposed of all mem allocation');
  Assert(MemTracker.GrayStack = nil, 'FreeMemTracker: GrayStack not freed before dispose');
  {$ENDIF}
  dispose(MemTracker);
  MemTracker := nil;  // Note here we don't dispose of the roots.stack reference.
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertPointerIsNil(MemTracker, 'FreeMemTracker exit: MemTracker should be nil');
  {$ENDIF}
end;

procedure defineNative(const name : AnsiString; func : TNativeFn; arity: Integer = -1);
var
  native : pObjNative;
  nameStr : pObjString;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(Length(name) > 0, 'defineNative: name is empty');
  Assert(Assigned(func), 'defineNative: func is not assigned');
  {$ENDIF}

  // Push nameStr and native onto stack to protect from GC (book pattern).
  nameStr := CreateString(name, VM.MemTracker);
  pushStack(VM.Stack, StringToValue(nameStr));
  native := newNative(func, arity, PAnsiChar(name), VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(native)));
  TableSet(VM.Globals, nameStr, CreateObject(pObj(native)), VM.MemTracker);
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);
end;

// ---- Native class registry ----

var
  NativeClassRegistry : array of pNativeClassInfo;
  NativeClassCount    : integer = 0;

procedure registerNativeClass(const AName : AnsiString; const AMethods : array of TNativeMethod; ADestructor : TNativeDestructor);
var
  i : integer;
  info : pNativeClassInfo;
begin
  if findNativeClass(AName) <> nil then Exit; // already registered
  New(info);
  info^.name := AName;
  SetLength(info^.methods, Length(AMethods));
  for i := 0 to High(AMethods) do
    info^.methods[i] := AMethods[i];
  info^.destructor_ := ADestructor;
  info^.rttiEnabled := False;
  info^.rttiClass := nil;
  Inc(NativeClassCount);
  SetLength(NativeClassRegistry, NativeClassCount);
  NativeClassRegistry[NativeClassCount - 1] := info;
end;

procedure registerNativeClassRTTI(const AName : AnsiString; AClass : TClass; ADestructor : TNativeDestructor = nil);
var
  info : pNativeClassInfo;
begin
  if findNativeClass(AName) <> nil then Exit; // already registered
  New(info);
  info^.name := AName;
  SetLength(info^.methods, 0);
  info^.destructor_ := ADestructor;
  info^.rttiEnabled := True;
  info^.rttiClass := AClass;
  Inc(NativeClassCount);
  SetLength(NativeClassRegistry, NativeClassCount);
  NativeClassRegistry[NativeClassCount - 1] := info;
end;

function findNativeClass(const name : AnsiString) : pNativeClassInfo;
var
  i : integer;
begin
  for i := 0 to NativeClassCount - 1 do
    if NativeClassRegistry[i]^.name = name then
      Exit(NativeClassRegistry[i]);
  Result := nil;
end;

procedure InjectNativeObject(const name : AnsiString; instance : Pointer; const className : AnsiString);
var
  classInfo : pNativeClassInfo;
  obj : pObjNativeObject;
  nameStr : pObjString;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(instance <> nil, 'InjectNativeObject: instance is nil');
  Assert(Length(name) > 0, 'InjectNativeObject: name is empty');
  Assert(Length(className) > 0, 'InjectNativeObject: className is empty');
  {$ENDIF}

  classInfo := findNativeClass(className);
  Assert(classInfo <> nil, 'InjectNativeObject: class not registered: ' + className);

  // GC-safe order: create string first, push it, then create object and push it.
  nameStr := CreateString(name, VM.MemTracker);
  pushStack(VM.Stack, StringToValue(nameStr));
  obj := newNativeObject(instance, classInfo, VM.MemTracker);
  obj^.ownsInstance := false;  // Delphi owns this object
  pushStack(VM.Stack, CreateObject(pObj(obj)));
  TableSet(VM.Globals, nameStr, CreateObject(pObj(obj)), VM.MemTracker);
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);
end;

procedure InjectObject(const name : AnsiString; instance : TObject);
var
  className : AnsiString;
  classInfo : pNativeClassInfo;
begin
  Assert(instance <> nil, 'InjectObject: instance is nil');
  className := AnsiString(instance.ClassName);
  classInfo := findNativeClass(className);
  if classInfo = nil then
    registerNativeClassRTTI(className, instance.ClassType);
  InjectNativeObject(name, Pointer(instance), className);
end;



procedure InitVM;
begin
  // Allow IEEE 754 semantics: NaN, Inf, -Inf instead of FPU exceptions
  SetExceptionMask(exAllArithmeticExceptions);
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

  Assert(VM = nil, 'InitVM: VM is not nil — previous instance was not freed');
  new(VM); //we don't care about making this route through allocate
  FillChar(VM^, SizeOf(TVirtualMachine), 0); // Zero all fields to prevent stale heap data (e.g. OnPrint)

  {$IFDEF OPCODE_PROFILING}
  FillChar(OpPairCounts, SizeOf(OpPairCounts), 0);
  OpPrevOp := 255;
  {$ENDIF}
  VM.FrameCount := 0;
  VM.Stack := nil;
  VM.MemTracker := nil;
  VM.Strings := nil;
  VM.Globals := nil;
  VM.OpenUpvalues := nil;
  VM.OnPrint := nil;
  VM.RuntimeErrorStr := '';
  VM.PrintBuilder := TStringBuilder.Create;

  RttiCtx := TRttiContext.Create;
  InitMemTracker(VM.MemTracker);
  InitTable(VM.Strings, VM.MemTracker);
  InitTable(VM.Globals, VM.MemTracker);
  InitStack(VM.Stack,Vm.MemTracker);
  ResetStack(vm.Stack);
  //GC set up
  VM.MemTracker.Roots.Stack := Vm.Stack;

  // Seed random number generator
  Randomize;

  // Register native functions
  RegisterAllNatives;

  // ---- Exit assertions ----
   {$IFOPT C+}
  AssertVMIsAssigned;
  Assert(VM.Stack <> nil, 'InitVM exit: Stack should not be nil');
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(VM.Strings <> nil, 'InitVM exit: Strings table should not be nil');
  Assert(VM.Globals <> nil, 'InitVM exit: Globals table should not be nil');
  Assert(VM.MemTracker.Roots.Stack = VM.Stack, 'InitVM exit: GC roots should point to stack');
  {$ENDIF}
end;

procedure FreeVM;
var
  i : integer;
begin
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}
  {$IFDEF DEBUG_LOG_GC}
  try
  {$ENDIF}
  VM.PrintBuilder.Free;
  VM.PrintBuilder := nil;
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
//  Assert(VM.MemTracker.BytesAllocated = 0, 'VM has not disposed of all mem allocation');
  if VM.MemTracker.BytesAllocated <> 0 then
  begin
    raise Exception.Create('Mem Tracker has not disposed all bytes');
  end;

  FreeMemTracker(VM.MemTracker);
  dispose(VM);
  VM := nil;
  // Reset native class registry
  for i := 0 to NativeClassCount - 1 do
    Dispose(NativeClassRegistry[i]);
  SetLength(NativeClassRegistry, 0);
  NativeClassCount := 0;

  RttiCtx.Free;
  {$IFDEF DEBUG_LOG_GC}
  finally
  try
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
  finally
    CloseFile(GCLogFile);
  end;
  end;
  {$ENDIF}

  // ---- Exit assertions ----
  Assert(VM = nil, 'FreeVM exit: VM should be nil');
end;

procedure InitScanner(source : pAnsiChar);
begin
   {$IFOPT C+}
  AssertSourceCodeIsAssigned(source);
  {$ENDIF}


  scanner.start := source;
  scanner.current := source;
  scanner.line := 1;
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(scanner.start = source, 'InitScanner exit: start should equal source');
  Assert(scanner.current = source, 'InitScanner exit: current should equal source');
  Assert(scanner.line = 1, 'InitScanner exit: line should be 1');
  {$ENDIF}
end;


function advance : ansichar;
begin
  {$IFOPT C+}
  Assert(not isAtEnd, 'advance: already at end');
  {$ENDIF}
  inc(scanner.Current);
  result := scanner.current[-1];
end;

function isAtEnd : boolean;
begin
  result := scanner.current^ = #0;
end;

function MakeToken(const tokenType : TTokenType)  : TToken;inline;
begin
  {$IFOPT C+}
  Assert(NativeUInt(scanner.current) >= NativeUInt(scanner.start), 'MakeToken: current before start');
  {$ENDIF}
  result.tokenType := tokenType;
  result.start := scanner.start;
  result.length := scanner.current - scanner.start;
  result.line := scanner.line;

  {$IFOPT C+}
  Assert(result.length >= 0, 'MakeToken: negative token length');
  {$ENDIF}
end;


function ErrorToken(msg : pAnsiChar) : TToken;
begin
   {$IFOPT C+}
  Assert(Assigned(msg), 'ErrorToken: msg is nil');
  {$ENDIF}
  result.Tokentype := TOKEN_ERROR;
  result.start := msg;
  result.length := System.AnsiStrings.StrLen(msg);
  result.line := scanner.line;
end;

function match(expected : Ansichar) : boolean;inline;
begin
  result := false;

  if isAtEnd then exit;

  if scanner.current^ <> expected then exit;

  inc(scanner.current);

  result := true;
end;

function peek : ansichar; inline;
begin
  result := scanner.current^;
end;

function PeekNext: ansiChar; inline;
begin
  if IsAtEnd then
    Exit(#0);

  Result := (scanner.current + 1)^;
end;

procedure SkipWhitespace; inline;
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
          // Block comment "/* ... */"
          else if PeekNext = '*' then
          begin
            Advance; // consume '/'
            Advance; // consume '*'
            while not IsAtEnd do
            begin
              if (Peek = '*') and (PeekNext = '/') then
              begin
                Advance; // consume '*'
                Advance; // consume '/'
                Break;
              end;
              if Peek = CHAR_LF then
                Inc(scanner.Line);
              Advance;
            end;
          end
          else
            Exit;
        end;

    else
      Exit;
    end;
  end;
end;


function isDigit(c : Ansichar) : boolean;inline;
begin
  Result := (c >= '0') and (c <= '9');
end;

(*
static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
} *)


function isAlpha(c : AnsiChar) : boolean; inline;
begin
  result := ((c >= 'a') and (c <= 'z')) or
            ((c >= 'A') and (c <= 'Z')) or
            (c = '_');
end;

function ScanString: TToken;inline;
begin
  Assert(Assigned(scanner.current), 'ScanString: scanner.current is nil');
  while not IsAtEnd do
  begin
    if Peek = CHAR_QUOTE then
    begin
      // Doubled quote ("") is an embedded quote - skip both and continue
      if PeekNext = CHAR_QUOTE then
      begin
        Advance;
        Advance;
        Continue;
      end;
      // Single quote ends the string
      Break;
    end;
    // Backslash escape: skip the next character so \" doesn't end the string
    if Peek = '\' then
    begin
      Advance; // consume the backslash
      if not IsAtEnd then
      begin
        if Peek = CHAR_LF then
          Inc(scanner.Line);
        Advance; // consume the escaped character
      end;
      Continue;
    end;
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


function ScanNumber: TToken;inline;
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
  tokenType: TTokenType): TTokenType; inline;
begin
  Assert(Assigned(rest), 'CheckKeyword: rest is nil');
  Assert(start >= 0, 'CheckKeyword: start is negative');
  Assert(length > 0, 'CheckKeyword: length must be > 0');
  if (scanner.current - scanner.start = start + length) and
     (CompareMem(scanner.start + start, rest, length)) then
    Exit(tokenType);

  Result := TOKEN_IDENTIFIER;
end;

function identifierType : TTokenType; inline;
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
      'r':
      begin
        if (scanner.current - scanner.start > 1) then
        begin
          case (scanner.start + 1)^ of
            'e':
            begin
              if (scanner.current - scanner.start > 2) then
              begin
                case (scanner.start + 2)^ of
                  't': Exit(CheckKeyword(2, 4, 'turn', TOKEN_RETURN));
                  'c': Exit(CheckKeyword(2, 4, 'cord', TOKEN_RECORD));
                end;
              end;
            end;
          end;
        end;
      end;
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


function Identifier : TToken; inline;
begin
  while (isAlpha(peek()) or isDigit(peek())) do advance();
  result := makeToken(identifierType());
end;

function ScanToken : TToken; inline;
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

      '[': result :=  makeToken(TOKEN_LEFT_BRACKET);

      ']': result :=  makeToken(TOKEN_RIGHT_BRACKET);

      ';': result :=  makeToken(TOKEN_SEMICOLON);

      ':': result :=  makeToken(TOKEN_COLON);

      ',': result :=  makeToken(TOKEN_COMMA);

      '.': result :=  makeToken(TOKEN_DOT);

      '-': begin
          if Match('=') then
            Result := MakeToken(TOKEN_MINUS_EQUAL)
          else
            Result := MakeToken(TOKEN_MINUS);
        end;

      '+': begin
          if Match('=') then
            Result := MakeToken(TOKEN_PLUS_EQUAL)
          else
            Result := MakeToken(TOKEN_PLUS);
        end;

      '/': begin
          if Match('=') then
            Result := MakeToken(TOKEN_SLASH_EQUAL)
          else
            Result := MakeToken(TOKEN_SLASH);
        end;

      '*': begin
          if Match('=') then
            Result := MakeToken(TOKEN_STAR_EQUAL)
          else
            Result := MakeToken(TOKEN_STAR);
        end;

      '%': result :=  makeToken(TOKEN_PERCENT);

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

function TokenToString(const Token: TToken): AnsiString; inline;
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
    lexeme := String(TokenToString(token));

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

  if Parser.ErrorStr <> '' then
    Parser.ErrorStr := Parser.ErrorStr + sLineBreak + s
  else
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

procedure AdvanceParser(); inline;
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

procedure Consume(TokenKind: TTokenType; const Msg: PAnsiChar);inline;
begin
  Assert(Assigned(Msg), 'Consume: Msg is nil');
  if Parser.Current.TokenType = TokenKind then
  begin
    AdvanceParser;
    Exit;
  end;

  ErrorAtCurrent(Msg);
end;

function currentChunk : pChunk; inline;
begin
    {$IFOPT C+}
  Assert(Current <> nil, 'Current compiler is nil');
  Assert(Current^.func <> nil, 'Current compiler function is nil');
  Assert(Current^.func^.chunk <> nil, 'Current compiler function chunk is nil');
  {$ENDIF}
  result := Current^.func^.chunk;
end;


procedure emitByte(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker );inline;
var
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  {$ENDIF}
  oldCount := Chunk.Count;
  writeChunk(Chunk,value,line,MemTracker);
  
  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Chunk.Count = oldCount + 1, 'emitByte exit: chunk count should increase by 1');
  {$ENDIF}
end;

procedure EmitReturn(Chunk : pChunk; Line : integer; MemTracker : pMemTracker);inline;
var
  oldCount : integer;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  {$ENDIF}
  oldCount := Chunk.Count;
  writeChunk(Chunk, OP_NIL, line, Memtracker);
  writeChunk(Chunk, OP_RETURN, line, Memtracker);

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Chunk.Count = oldCount + 2, 'EmitReturn exit: chunk count should increase by 2');
  AssertChunkEndsWithReturn(Chunk);
  {$ENDIF}
end;

procedure emitConstant(value : TValue);inline;
begin
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}
  AddConstant(CurrentChunk,value,parser.previous.line,VM.MemTracker);
end;

function  getRule(tokenType : TTokenType) : TParseRule; inline;
begin
  result := Rules[tokenType];
end;

procedure FatalError;
begin
  Abort;
end;

function check(tokenType : TTokenType) : boolean; inline;
begin
  Result := parser.current.tokenType = tokenType;
end;

function matchToken(tokenType : TTokenType) : boolean; inline;
begin
  if not check(tokenType) then
    Exit(false);
  advanceParser();
  Result := true;
end;


procedure parsePrecedence(precedence : TPrecedence); inline;
var
  prefixRule : TParseFn;
  infixRule  : TParseFn;
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
  prefixRule(canAssign);

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

    infixRule(canAssign);
  end;

  if canAssign and matchToken(TOKEN_EQUAL) then
  begin
    Error('Invalid assignment target.');
  end;
end;


procedure Number(canAssign: Boolean);
var
  Value  : TValue;
  lexeme : AnsiString;
  fs : TFormatSettings;
begin
  Assert(parser.previous.tokenType = TOKEN_NUMBER, 'Number: expected TOKEN_NUMBER');
  lexeme := TokenToString(parser.previous);
  fs := TFormatSettings.Create;   //record type, no need to free
  fs.DecimalSeparator := '.';
  Value := CreateNumber(StrToFloat(string(lexeme), fs));
  EmitConstant(Value);
end;

procedure Expression();
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure grouping(canAssign: Boolean);
begin
  expression;
  consume(TOKEN_RIGHT_PAREN,'Expect '')'' after expression.');
end;

procedure unary(canAssign: Boolean);
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


procedure ParseString(canAssign: Boolean);
var
  lexeme  : ansiString;
  strObj : pObjString;
  value  : TValue;

  function ProcessEscapes(const s: AnsiString): AnsiString;
  var
    i: Integer;
    ch: AnsiChar;
    buf: AnsiString;
  begin
    buf := '';
    i := 1;
    while i <= Length(s) do
    begin
      if (s[i] = '\') and (i < Length(s)) then
      begin
        Inc(i);
        ch := s[i];
        case ch of
          'n': buf := buf + #10;
          't': buf := buf + #9;
          'r': buf := buf + #13;
          '\': buf := buf + '\';
          '"': buf := buf + '"';
          '0': buf := buf + #0;
        else
          begin
            errorAt(parser.previous, PAnsiChar(AnsiString('Unknown escape sequence: \' + ch)));
            buf := buf + '\' + ch;
          end;
        end;
      end
      else
        buf := buf + s[i];
      Inc(i);
    end;
    Result := buf;
  end;

begin
  {$IFOPT C+}
  Assert(parser.previous.tokenType = TOKEN_STRING, 'ParseString: expected TOKEN_STRING');
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}
  lexeme := TokenToString(parser.Previous);
  // strip leading and trailing quotes
  if (Length(lexeme) >= 2) and (lexeme[1] = '"') and (lexeme[Length(lexeme)] = '"') then
    lexeme := Copy(lexeme, 2, Length(lexeme) - 2);
  // Collapse doubled quotes ("") into single quotes (")
  lexeme := AnsiString(StringReplace(string(lexeme), '""', '"', [rfReplaceAll]));
  // Process backslash escape sequences
  lexeme := ProcessEscapes(lexeme);
  // Push string onto stack to protect from GC until stored in constants.
  strObj := CreateString(lexeme,VM.MemTracker);
  pushStack(VM.Stack, StringToValue(strObj));
  value := StringToValue(strObj);
  emitConstant(value);
  Dec(VM.Stack.StackTop);
end;

procedure binary(canAssign: Boolean);
var
  tokenType : TTokenType;
  rule : TParseRule;
  slot, index : Byte;
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
        lastAddOffset := CurrentChunk.count - 1;
      end;

      TOKEN_MINUS : begin
        // Peephole: fuse GET_LOCAL_N + CONSTANT + SUBTRACT into one opcode.
        // Pattern in chunk: [GET_LOCAL_0..7 | GET_LOCAL,slot], OP_CONSTANT, idx
        if (CurrentChunk.count >= 3) and
           (CurrentChunk.code[CurrentChunk.count - 2] = OP_CONSTANT) and
           (CurrentChunk.code[CurrentChunk.count - 3] >= OP_GET_LOCAL_0) and
           (CurrentChunk.code[CurrentChunk.count - 3] <= OP_GET_LOCAL_7) then
        begin
          // Fast-slot variant: slot encoded in the opcode
          slot := CurrentChunk.code[CurrentChunk.count - 3] - OP_GET_LOCAL_0;
          index := CurrentChunk.code[CurrentChunk.count - 1]; // const idx
          Dec(CurrentChunk.count, 3);
          emitByte(OP_GET_LOCAL_CONST_SUBTRACT, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
          emitByte(slot, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
          emitByte(index, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
        end
        else if (CurrentChunk.count >= 4) and
                (CurrentChunk.code[CurrentChunk.count - 2] = OP_CONSTANT) and
                (CurrentChunk.code[CurrentChunk.count - 4] = OP_GET_LOCAL) then
        begin
          // General GET_LOCAL variant: GET_LOCAL, slot, OP_CONSTANT, idx
          slot := CurrentChunk.code[CurrentChunk.count - 3];
          index := CurrentChunk.code[CurrentChunk.count - 1];
          Dec(CurrentChunk.count, 4);
          emitByte(OP_GET_LOCAL_CONST_SUBTRACT, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
          emitByte(slot, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
          emitByte(index, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
        end
        else
          emitByte(OP_SUBTRACT, CurrentChunk, Parser.Previous.Line, vm.MemTracker);
      end;

      TOKEN_STAR : begin
        emitByte(OP_MULTIPLY,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_SLASH : begin
        emitByte(OP_DIVIDE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end;

      TOKEN_PERCENT : begin
        emitByte(OP_MODULO,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
      end
      else
      begin
        Showmessage('Invalid token -- not implemented yet!');
        FatalError;
      end;
  end;
end;


procedure literal(canAssign: Boolean);
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




function emitJump(instruction : byte) : integer; inline;
begin

  // Peephole: fuse OP_LESS + OP_JUMP_IF_FALSE_POP into OP_LESS_JUMP_IF_FALSE
  // Guard: don't fuse if a jump was just patched to target this position
  // (e.g. and_ short-circuit lands here — fusing would corrupt the target)
  if (instruction = OP_JUMP_IF_FALSE_POP) and (CurrentChunk.count > 0)
     and (CurrentChunk.code[CurrentChunk.count - 1] = OP_LESS)
     and (lastPatchTarget <> CurrentChunk.count) then
  begin
    // Overwrite the OP_LESS we already emitted with the fused opcode
    CurrentChunk.code[CurrentChunk.count - 1] := OP_LESS_JUMP_IF_FALSE;
    emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
    Result := CurrentChunk.count - 2;
    Exit;
  end;
  emitByte(instruction, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
  Result := CurrentChunk.count - 2;
  // ---- Exit assertions ----
  {$IFOPT C+}
  Assert(Result >= 0, 'emitJump exit: offset should be >= 0');
  {$ENDIF}
end;

procedure patchJump(offset : integer); inline;
var
  jump : integer;
begin
  // ---- Entry assertions ----
  Assert(offset >= 0, 'patchJump: offset is negative');
  Assert(offset + 1 < CurrentChunk.Count, 'patchJump: offset out of bounds');

  // -2 to adjust for the two-byte jump operand itself
  jump := CurrentChunk.count - offset - 2;
  // Zero is a legitimate (no-op) forward jump now that OP_JUMP_IF_FALSE_POP
  // can sit immediately before its patch target with no intervening pop byte
  // (e.g. `if (x) {}` or `if (x) {} else {}`). Negative is still a bug.
  Assert(jump >= 0, 'patchJump: jump distance is negative');
  if jump > MAX_JUMP_OFFSET then
    Error('Too much code to jump over.');
  CurrentChunk.code[offset]     := (jump shr 8) and $FF;
  CurrentChunk.code[offset + 1] := jump and $FF;
  lastPatchTarget := CurrentChunk.count;
end;

procedure emitLoop(loopStart : integer); inline;
var
  offset : integer;
begin
  Assert(loopStart >= 0, 'emitLoop: loopStart is negative');
  emitByte(OP_LOOP, CurrentChunk, parser.previous.line, vm.MemTracker);
  offset := CurrentChunk.count - loopStart + 2;
  if offset > MAX_JUMP_OFFSET then
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

  thenJump := emitJump(OP_JUMP_IF_FALSE_POP);
  statement();

  if matchToken(TOKEN_ELSE) then
  begin
    // Only emit the unconditional skip-over-else when there IS an else.
    // Without this guard, an `if` without else would call patchJump(elseJump)
    // immediately after emitting elseJump (jump distance = 0). The old code
    // accidentally avoided this because an OP_POP byte sat between them; the
    // new fused OP_JUMP_IF_FALSE_POP removes that byte.
    elseJump := emitJump(OP_JUMP);
    patchJump(thenJump);
    statement();
    patchJump(elseJump);
  end
  else
    patchJump(thenJump);
end;

procedure whileStatement();
var
  loopStart, exitJump : integer;
begin
  loopStart := CurrentChunk.count;
  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after ''while''.');
  Expression();
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after condition.');

  exitJump := emitJump(OP_JUMP_IF_FALSE_POP);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
end;

procedure forStatement();
var
  loopStart, exitJump, bodyJump, incrementStart : integer;
  loopVarSlot : integer;
  innerVarSlot : integer;
  loopVarName : TToken;
  hasLoopVar : boolean;
begin
  beginScope();
  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after ''for''.');

  // Initializer
  hasLoopVar := false;
  if matchToken(TOKEN_SEMICOLON) then
    // No initializer
  else if matchToken(TOKEN_VAR) then
  begin
    varDeclaration();
    // Remember the loop variable so we can copy it per iteration
    hasLoopVar := true;
    loopVarSlot := Current^.localCount - 1;
    loopVarName := Current^.locals[loopVarSlot].name;
  end
  else
    expressionStatement();

  loopStart := CurrentChunk.count;

  // Condition
  exitJump := -1;
  if not matchToken(TOKEN_SEMICOLON) then
  begin
    Expression();
    consume(TOKEN_SEMICOLON, 'Expect '';'' after loop condition.');
    exitJump := emitJump(OP_JUMP_IF_FALSE_POP);
  end;

  // Increment
  if not matchToken(TOKEN_RIGHT_PAREN) then
  begin
    bodyJump := emitJump(OP_JUMP);
    incrementStart := CurrentChunk.count;
    Expression();
    emitPopWithPeephole;
    consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after for clauses.');
    emitLoop(loopStart);
    loopStart := incrementStart;
    patchJump(bodyJump);
  end;

  // Per-iteration scope: copy the loop variable so closures get a fresh binding
  if hasLoopVar then
  begin
    beginScope();
    emitByte(OP_GET_LOCAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(loopVarSlot), CurrentChunk, parser.previous.line, vm.MemTracker);
    // Inline addLocal + markInitialized (declared later in unit)
    Current^.locals[Current^.localCount].name := loopVarName;
    Current^.locals[Current^.localCount].depth := Current^.scopeDepth;
    Current^.locals[Current^.localCount].isCaptured := false;
    innerVarSlot := Current^.localCount;
    Inc(Current^.localCount);
  end;

  statement();

  if hasLoopVar then
  begin
    // Write the (possibly mutated) inner copy back to the outer loop variable
    // so the increment/condition see updates made inside the body.
    emitByte(OP_GET_LOCAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(innerVarSlot), CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(OP_SET_LOCAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(loopVarSlot), CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    endScope();
  end;

  emitLoop(loopStart);

  if exitJump <> -1 then
  begin
    patchJump(exitJump);
  end;

  endScope();
end;

procedure and_(canAssign: Boolean);
var
  endJump : integer;
begin
  endJump := emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
end;

procedure or_(canAssign: Boolean);
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
  local : pLocal;
begin
  New(compiler);
  compiler^.enclosing := Current;
  compiler^.func := nil;
  compiler^.funcType := funcType;
  compiler^.localCount := 0;
  compiler^.scopeDepth := 0;
  lastAddOffset := -1;
  compiler^.func := newFunction(VM.MemTracker);
  Current := compiler;

  if funcType <> TYPE_SCRIPT then
  begin
    Current^.func^.name := CreateString(
      Copy(AnsiString(parser.previous.start), 1, parser.previous.length),
      VM.MemTracker);
  end;

  // The first slot is claimed for the VM's internal use (the function itself)
  Assert(Current^.localCount < UINT8_COUNT, 'initCompiler: localCount overflow');
  local := @Current^.locals[Current^.localCount];
  Inc(Current^.localCount);
  local^.depth := 0;
  local^.isCaptured := false;
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
var
  pendingPops : integer;

  // Flush a contiguous run of non-captured pops as a single OP_POP_N (or
  // plain OP_POP for run==1). Splits runs >255 across multiple OP_POP_N.
  procedure FlushPendingPops();
  var
    chunkN : integer;
  begin
    while pendingPops > 0 do
    begin
      if pendingPops = 1 then
      begin
        emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
        pendingPops := 0;
      end
      else
      begin
        if pendingPops > 255 then chunkN := 255 else chunkN := pendingPops;
        emitByte(OP_POP_N, CurrentChunk, parser.previous.line, vm.MemTracker);
        emitByte(Byte(chunkN), CurrentChunk, parser.previous.line, vm.MemTracker);
        Dec(pendingPops, chunkN);
      end;
    end;
  end;

begin
  // ---- Entry assertions ----
  Assert(Current <> nil, 'endScope: Current compiler is nil');
  Assert(Current^.scopeDepth > 0, 'endScope: scopeDepth is already 0');

  Dec(Current^.scopeDepth);
  pendingPops := 0;
  while (Current^.localCount > 0) and
        (Current^.locals[Current^.localCount - 1].depth > Current^.scopeDepth) do
  begin
    Assert(Current^.localCount - 1 < UINT8_COUNT, 'endScope: locals index out of bounds');
    if Current^.locals[Current^.localCount - 1].isCaptured then
    begin
      // Captured locals need their upvalue closed in order; flush any pending
      // run of plain pops first so the captured local sits at the top of
      // the stack when OP_CLOSE_UPVALUE fires.
      FlushPendingPops;
      emitByte(OP_CLOSE_UPVALUE, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      Inc(pendingPops);
    Dec(Current^.localCount);
  end;
  FlushPendingPops;

  // ---- Exit assertions ----
  Assert(Current^.scopeDepth >= 0, 'endScope exit: scopeDepth is negative');
  Assert(Current^.localCount >= 0, 'endScope exit: localCount is negative');
end;

procedure addLocal(name : TToken); inline;
begin
  Assert(Current <> nil, 'addLocal: Current compiler is nil');
  if Current^.localCount = UINT8_COUNT then
  begin
    Error('Too many local variables in function.');
    Exit;
  end;
  Assert(Current^.localCount < UINT8_COUNT, 'addLocal: locals index out of bounds');
  Current^.locals[Current^.localCount].name := name;
  Current^.locals[Current^.localCount].depth := -1; // sentinel: not yet initialized
  Current^.locals[Current^.localCount].isCaptured := false;
  Inc(Current^.localCount);
end;

function identifiersEqual(const a, b : TToken) : boolean; inline;
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
    Assert((i >= 0) and (i < UINT8_COUNT), 'declareVariable: locals index out of bounds');
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
  Assert((Current^.localCount > 0) and (Current^.localCount - 1 < UINT8_COUNT), 'markInitialized: locals index out of bounds');
  Current^.locals[Current^.localCount - 1].depth := Current^.scopeDepth;
end;

function resolveLocal(compiler : pCompiler; const name : TToken) : integer; inline;
var
  i : integer;
begin
  Assert(compiler <> nil, 'resolveLocal: compiler is nil');
  for i := compiler^.localCount - 1 downto 0 do
  begin
    Assert((i >= 0) and (i < UINT8_COUNT), 'resolveLocal: locals index out of bounds');
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
  Assert(compiler <> nil, 'addUpvalue: compiler is nil');
  Assert(compiler^.func <> nil, 'addUpvalue: compiler func is nil');
  upvalueCount := compiler^.func^.upvalueCount;
  Assert(upvalueCount >= 0, 'addUpvalue: upvalueCount is negative');

  for i := 0 to upvalueCount - 1 do
  begin
    Assert((i >= 0) and (i < UINT8_COUNT), 'addUpvalue: upvalues index out of bounds (search)');
    if (compiler^.upvalues[i].index = index) and (compiler^.upvalues[i].isLocal = isLocal) then
      Exit(i);
  end;

  if upvalueCount = UINT8_COUNT then
  begin
    Error('Too many closure variables in function.');
    Exit(0);
  end;

  Assert(upvalueCount < UINT8_COUNT, 'addUpvalue: upvalues index out of bounds (write)');
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

  local := resolveLocal(compiler^.enclosing, name); // -1 means not found locally, so we try the next enclosing scope
  if local <> -1 then
  begin
    Assert((local >= 0) and (local < UINT8_COUNT), 'resolveUpvalue: locals index out of bounds');
    compiler^.enclosing^.locals[local].isCaptured := true;
    Exit(addUpvalue(compiler, byte(local), true));
  end;

  upvalue := resolveUpvalue(compiler^.enclosing, name);
  if upvalue <> -1 then
    Exit(addUpvalue(compiler, byte(upvalue), false));

  Result := -1;
end;

function identifierConstant(const name : TToken) : integer;
var
  strObj : pObjString;
  idx : integer;
begin
  // Push string onto stack to protect from GC until stored in constants.
  strObj := CreateString(TokenToString(name), VM.MemTracker);
  pushStack(VM.Stack, StringToValue(strObj));
  idx := AddValueConstant(CurrentChunk.Constants, StringToValue(strObj), VM.MemTracker);
  Dec(VM.Stack.StackTop);
  Result := idx;
end;

function parseVariable(const errorMsg : PAnsiChar) : integer;
begin
  consume(TOKEN_IDENTIFIER, errorMsg);

  declareVariable();
  if Current^.scopeDepth > 0 then
    Exit(0); // locals don't get stored in constant table

  Result := identifierConstant(parser.previous);
end;

procedure defineVariable(global : integer);
var
  IntBytes : TIntToByteResult;
begin
  if Current^.scopeDepth > 0 then
  begin
    markInitialized();
    Exit; // local is already on the stack
  end;
  if global <= High(Byte) then
  begin
    emitByte(OP_DEFINE_GLOBAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(global), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitByte(OP_DEFINE_GLOBAL_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
    IntBytes := IntToBytes(global);
    emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;
end;

procedure varDeclaration();
var
  global : integer;
begin
  global := parseVariable('Expect variable name.');

  if matchToken(TOKEN_EQUAL) then
    Expression()
  else
    emitByte(OP_NIL, CurrentChunk, parser.previous.line, vm.MemTracker);

  consume(TOKEN_SEMICOLON, 'Expect '';'' after variable declaration.');
  defineVariable(global);
end;

procedure namedVariable(name : TToken; canAssign : Boolean);
var
  arg : integer;
  getOp, setOp : byte;
  IntBytes : TIntToByteResult;
  isGlobalLong : boolean;
  compoundOp : Byte;

  function MatchCompoundAssign(out ArithOp: Byte): Boolean;
  begin
    Result := True;
    if matchToken(TOKEN_PLUS_EQUAL) then ArithOp := OP_ADD
    else if matchToken(TOKEN_MINUS_EQUAL) then ArithOp := OP_SUBTRACT
    else if matchToken(TOKEN_STAR_EQUAL) then ArithOp := OP_MULTIPLY
    else if matchToken(TOKEN_SLASH_EQUAL) then ArithOp := OP_DIVIDE
    else Result := False;
  end;

begin
  arg := resolveLocal(Current, name);
  isGlobalLong := false;
  if arg <> -1 then
  begin
    // Fast-slot variants: slots 0..7 fit in the opcode itself, no operand byte.
    if (arg >= 0) and (arg <= 7) then
    begin
      if canAssign and matchToken(TOKEN_EQUAL) then
      begin
        Expression();
        emitByte(Byte(OP_SET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
      end
      else if canAssign and MatchCompoundAssign(compoundOp) then
      begin
        // GET, expression, arithmetic op, SET
        emitByte(Byte(OP_GET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
        Expression();
        emitByte(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
        emitByte(Byte(OP_SET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
      end
      else
        emitByte(Byte(OP_GET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
      Exit;
    end;
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
      if arg <= High(Byte) then
      begin
        getOp := OP_GET_GLOBAL;
        setOp := OP_SET_GLOBAL;
      end
      else
      begin
        getOp := OP_GET_GLOBAL_LONG;
        setOp := OP_SET_GLOBAL_LONG;
        isGlobalLong := true;
      end;
    end;
  end;

  if canAssign and matchToken(TOKEN_EQUAL) then
  begin
    Expression();
    emitByte(setOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    if isGlobalLong then
    begin
      IntBytes := IntToBytes(arg);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      emitByte(Byte(arg), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else if canAssign and MatchCompoundAssign(compoundOp) then
  begin
    // Emit GET, expression, arithmetic op, SET
    emitByte(getOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    if isGlobalLong then
    begin
      IntBytes := IntToBytes(arg);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      emitByte(Byte(arg), CurrentChunk, parser.previous.line, vm.MemTracker);
    Expression();
    emitByte(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(setOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    if isGlobalLong then
    begin
      IntBytes := IntToBytes(arg);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      emitByte(Byte(arg), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitByte(getOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    if isGlobalLong then
    begin
      IntBytes := IntToBytes(arg);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      emitByte(Byte(arg), CurrentChunk, parser.previous.line, vm.MemTracker);
  end;
end;

procedure variable(canAssign: Boolean);
begin
  namedVariable(parser.previous, canAssign);
end;

procedure emitPopWithPeephole;
var
  slot: Byte;
begin
  // Peephole: fuse ADD + SET_LOCAL_N + POP into OP_ADD_SET_LOCAL_POP
  // Guard: lastAddOffset must confirm the byte is genuinely OP_ADD, not an operand.
  if (CurrentChunk.count >= 2) and
     (lastAddOffset = CurrentChunk.count - 2) and
     (CurrentChunk.code[CurrentChunk.count - 1] >= OP_SET_LOCAL_0) and
     (CurrentChunk.code[CurrentChunk.count - 1] <= OP_SET_LOCAL_7) then
  begin
    slot := CurrentChunk.code[CurrentChunk.count - 1] - OP_SET_LOCAL_0;
    Dec(CurrentChunk.count, 2);
    emitByte(OP_ADD_SET_LOCAL_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(slot, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else if (CurrentChunk.count >= 3) and
          (lastAddOffset = CurrentChunk.count - 3) and
          (CurrentChunk.code[CurrentChunk.count - 2] = OP_SET_LOCAL) then
  begin
    slot := CurrentChunk.code[CurrentChunk.count - 1];
    Dec(CurrentChunk.count, 3);
    emitByte(OP_ADD_SET_LOCAL_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(slot, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
    emitByte(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
  lastAddOffset := -1;
end;

procedure expressionStatement();
begin
  Expression();
  consume(TOKEN_SEMICOLON, 'Expect '';'' after expression.');
  emitPopWithPeephole;
end;

function argumentList() : byte; inline;
var
  argCount : byte;
begin
  argCount := 0;
  if not check(TOKEN_RIGHT_PAREN) then
  begin
    repeat
      Expression();
      if argCount = MAX_BYTE_OPERAND then
        Error('Can''t have more than 255 arguments.')
      else
        Inc(argCount);
    until not matchToken(TOKEN_COMMA);
  end;
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after arguments.');
  Result := argCount;
end;

procedure call(canAssign: Boolean);
var
  argCount : byte;
begin
  argCount := argumentList();
  emitByte(OP_CALL, CurrentChunk, parser.previous.line, vm.MemTracker);
  emitByte(argCount, CurrentChunk, parser.previous.line, vm.MemTracker);
end;

procedure dot_(canAssign: Boolean);
var
  nameIdx : integer;
  argCount : byte;
  IntBytes : TIntToByteResult;
  compoundOp : Byte;

  function MatchCompoundAssign(out ArithOp: Byte): Boolean;
  begin
    Result := True;
    if matchToken(TOKEN_PLUS_EQUAL) then ArithOp := OP_ADD
    else if matchToken(TOKEN_MINUS_EQUAL) then ArithOp := OP_SUBTRACT
    else if matchToken(TOKEN_STAR_EQUAL) then ArithOp := OP_MULTIPLY
    else if matchToken(TOKEN_SLASH_EQUAL) then ArithOp := OP_DIVIDE
    else Result := False;
  end;

begin
  consume(TOKEN_IDENTIFIER, 'Expect property name after ''.''.');
  nameIdx := identifierConstant(parser.previous);

  if canAssign and matchToken(TOKEN_EQUAL) then
  begin
    Expression();
    if nameIdx <= High(Byte) then
    begin
      emitByte(OP_SET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitByte(OP_SET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
  end
  else if canAssign and MatchCompoundAssign(compoundOp) then
  begin
    // obj.field += expr → DUP obj, GET_PROPERTY, expr, op, SET_PROPERTY
    emitByte(OP_DUP, CurrentChunk, parser.previous.line, vm.MemTracker);
    if nameIdx <= High(Byte) then
    begin
      emitByte(OP_GET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitByte(OP_GET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
    Expression();
    emitByte(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    if nameIdx <= High(Byte) then
    begin
      emitByte(OP_SET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitByte(OP_SET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
  end
  else if matchToken(TOKEN_LEFT_PAREN) then
  begin
    argCount := argumentList();
    if nameIdx <= High(Byte) then
    begin
      emitByte(OP_INVOKE, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(argCount, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitByte(OP_INVOKE_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(argCount, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
  end
  else
  begin
    if nameIdx <= High(Byte) then
    begin
      emitByte(OP_GET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitByte(OP_GET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
  end;
end;

procedure arrayLiteral(canAssign: Boolean);
var
  elemCount : integer;
  pairCount : integer;
begin
  // Empty dict: [:]
  if matchToken(TOKEN_COLON) then
  begin
    consume(TOKEN_RIGHT_BRACKET, 'Expect '']'' after empty dictionary literal.');
    emitByte(OP_DICT_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(0, CurrentChunk, parser.previous.line, vm.MemTracker);
    Exit;
  end;

  // Empty array: []
  if check(TOKEN_RIGHT_BRACKET) then
  begin
    consume(TOKEN_RIGHT_BRACKET, 'Expect '']'' after array elements.');
    emitByte(OP_ARRAY_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(0, CurrentChunk, parser.previous.line, vm.MemTracker);
    Exit;
  end;

  // Parse first expression to determine if array or dict
  Expression();

  if matchToken(TOKEN_COLON) then
  begin
    // Dictionary: first expression was the first key, now parse value
    Expression();
    pairCount := 1;
    while matchToken(TOKEN_COMMA) do
    begin
      Expression();  // key
      consume(TOKEN_COLON, 'Expect '':'' after dictionary key.');
      Expression();  // value
      if pairCount = MAX_DICT_LITERAL_PAIRS then
        Error('Can''t have more than 127 entries in a dictionary literal.')
      else
        Inc(pairCount);
    end;
    consume(TOKEN_RIGHT_BRACKET, 'Expect '']'' after dictionary entries.');
    emitByte(OP_DICT_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(pairCount), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    // Array: first expression was the first element
    elemCount := 1;
    while matchToken(TOKEN_COMMA) do
    begin
      Expression();
      if elemCount = MAX_BYTE_OPERAND then
        Error('Can''t have more than 255 elements in an array literal.')
      else
        Inc(elemCount);
    end;
    consume(TOKEN_RIGHT_BRACKET, 'Expect '']'' after array elements.');
    emitByte(OP_ARRAY_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(elemCount), CurrentChunk, parser.previous.line, vm.MemTracker);
  end;
end;

procedure subscript_(canAssign: Boolean);
var
  compoundOp: Byte;

  function MatchCompoundAssign(out ArithOp: Byte): Boolean;
  begin
    Result := True;
    if matchToken(TOKEN_PLUS_EQUAL) then ArithOp := OP_ADD
    else if matchToken(TOKEN_MINUS_EQUAL) then ArithOp := OP_SUBTRACT
    else if matchToken(TOKEN_STAR_EQUAL) then ArithOp := OP_MULTIPLY
    else if matchToken(TOKEN_SLASH_EQUAL) then ArithOp := OP_DIVIDE
    else Result := False;
  end;

begin
  // The object being subscripted is already on the stack.
  // Parse the index/key expression between [ and ]
  Expression();
  consume(TOKEN_RIGHT_BRACKET, 'Expect '']'' after subscript.');

  if canAssign and matchToken(TOKEN_EQUAL) then
  begin
    // a[expr] = value
    Expression();
    emitByte(OP_SET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else if canAssign and MatchCompoundAssign(compoundOp) then
  begin
    // a[key] += expr → DUP2 [obj,key], GET_SUBSCRIPT, expr, op, SET_SUBSCRIPT
    emitByte(OP_DUP2, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(OP_GET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
    Expression();
    emitByte(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(OP_SET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    // a[expr]
    emitByte(OP_GET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;
end;

procedure functionBody(funcType : TFunctionType);
var
  compiler : pCompiler;
  func : pObjFunction;
  i : integer;
  j : integer;
  closureIdx : integer;
  closureBytes : TIntToByteResult;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}

  compiler := nil;
  initCompiler(compiler, funcType);
  beginScope();

  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after function name.');
  if not check(TOKEN_RIGHT_PAREN) then
  begin
    repeat
      Inc(Current^.func^.arity);
      if Current^.func^.arity > MAX_BYTE_OPERAND then
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
  pushStack(VM.Stack, CreateObject(pObj(func)));

  closureIdx := AddValueConstant(CurrentChunk.Constants, CreateObject(pObj(func)), VM.MemTracker);
  if closureIdx <= High(Byte) then
  begin
    emitByte(OP_CLOSURE, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(closureIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitByte(OP_CLOSURE_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
    closureBytes := IntToBytes(closureIdx);
    emitByte(closureBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(closureBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(closureBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

  for j := 0 to func^.upvalueCount - 1 do
  begin
    if compiler^.upvalues[j].isLocal then
      emitByte(1, CurrentChunk, parser.previous.line, vm.MemTracker)
    else
      emitByte(0, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(compiler^.upvalues[j].index, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

  Dec(VM.Stack.StackTop);

  Dispose(compiler);
end;

procedure funDeclaration();
var
  global : integer;
begin
  Assert(Current <> nil, 'funDeclaration: Current compiler is nil');
  global := parseVariable('Expect function name.');
  markInitialized();
  functionBody(TYPE_FUNCTION);
  defineVariable(global);
end;

procedure lambda(canAssign: Boolean);
begin
  // If followed by an identifier, this is a named function declaration
  // misplaced in expression position — not a valid anonymous function.
  if check(TOKEN_IDENTIFIER) then
  begin
    Error('Expect expression.');
    Exit;
  end;
  // Anonymous function expression: fun(params) { body }
  // Compiles exactly like a named function but without binding to a variable.
  // The closure value is left on the stack as an expression result.
  functionBody(TYPE_FUNCTION);
end;

procedure recordDeclaration();
var
  global : integer;
  fieldCount : integer;
  fieldNameIndices : array[0..MAX_BYTE_OPERAND] of integer;
  i : integer;
  nameStr : pObjString;
  nameToken : TToken;
begin
  Assert(Current <> nil, 'recordDeclaration: Current compiler is nil');
  global := parseVariable('Expect record name.');
  nameToken := parser.previous;  // save the record name token
  markInitialized();

  consume(TOKEN_LEFT_PAREN, 'Expect ''('' after record name.');
  fieldCount := 0;
  if not check(TOKEN_RIGHT_PAREN) then
  begin
    repeat
      consume(TOKEN_IDENTIFIER, 'Expect field name.');
      if fieldCount >= MAX_BYTE_OPERAND then
      begin
        Error('Can''t have more than 255 fields.');
        Break;
      end;
      nameStr := CreateString(TokenToString(parser.previous), VM.MemTracker);
      pushStack(VM.Stack, StringToValue(nameStr));
      fieldNameIndices[fieldCount] := AddValueConstant(CurrentChunk.Constants,
        StringToValue(nameStr), VM.MemTracker);
      Dec(VM.Stack.StackTop);
      Inc(fieldCount);
    until not matchToken(TOKEN_COMMA);
  end;
  consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after record fields.');
  consume(TOKEN_SEMICOLON, 'Expect '';'' after record declaration.');

  // Store the record type name as a constant
  nameStr := CreateString(TokenToString(nameToken), VM.MemTracker);
  pushStack(VM.Stack, StringToValue(nameStr));
  var typeNameIdx : integer := AddValueConstant(CurrentChunk.Constants,
    StringToValue(nameStr), VM.MemTracker);
  Dec(VM.Stack.StackTop);

  // Determine whether any constant index exceeds a single byte
  var needsLong : Boolean := (typeNameIdx > High(Byte));
  if not needsLong then
    for i := 0 to fieldCount - 1 do
      if fieldNameIndices[i] > High(Byte) then
      begin
        needsLong := True;
        Break;
      end;

  if not needsLong then
  begin
    // Short form: 1 byte per index
    emitByte(OP_RECORD, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(fieldCount), CurrentChunk, parser.previous.line, vm.MemTracker);
    for i := 0 to fieldCount - 1 do
      emitByte(Byte(fieldNameIndices[i]), CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(typeNameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    // Long form: 3 bytes per index (24-bit little-endian)
    var intBytes : TIntToByteResult;
    emitByte(OP_RECORD_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(fieldCount), CurrentChunk, parser.previous.line, vm.MemTracker);
    for i := 0 to fieldCount - 1 do
    begin
      intBytes := IntToBytes(fieldNameIndices[i]);
      emitByte(intBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(intBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(intBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
    intBytes := IntToBytes(typeNameIdx);
    emitByte(intBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(intBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(intBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

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
    // Peephole: fuse ADD + RETURN into OP_ADD_RETURN
    // Guard: don't fuse if a jump was just patched to target this position
    // (e.g. short-circuit and_/or_ lands here — fusing would corrupt the target)
    if (lastAddOffset = CurrentChunk.count - 1)
       and (lastPatchTarget <> CurrentChunk.count) then
    begin
      Dec(CurrentChunk.count); // remove the OP_ADD
      emitByte(OP_ADD_RETURN, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      emitByte(OP_RETURN, CurrentChunk, parser.previous.line, vm.MemTracker);
    lastAddOffset := -1;
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

procedure synchronize();
begin
  Parser.PanicMode := false;
  while Parser.Current.TokenType <> TOKEN_EOF do
  begin
    if Parser.Previous.TokenType = TOKEN_SEMICOLON then
      Exit;
    case Parser.Current.TokenType of
      TOKEN_CLASS, TOKEN_FUN, TOKEN_VAR, TOKEN_FOR,
      TOKEN_IF, TOKEN_WHILE, TOKEN_PRINT, TOKEN_RETURN,
      TOKEN_RECORD:
        Exit;
    end;
    advanceParser();
  end;
end;

procedure declaration();
begin
  Assert(Current <> nil, 'declaration: Current compiler is nil');
  if matchToken(TOKEN_FUN) then
    funDeclaration()
  else if matchToken(TOKEN_VAR) then
    varDeclaration()
  else if matchToken(TOKEN_RECORD) then
    recordDeclaration()
  else
    statement();

  if Parser.PanicMode then
    synchronize();

end;

function compile(source : pAnsiChar) : pObjClosure;
var
  compiler : pCompiler;
  func : pObjFunction;
begin
   {$IFOPT C+}
  AssertSourceCodeIsAssigned(source);
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  {$ENDIF}
  initScanner(source);
  compiler := nil;
  Current := nil;
  initCompiler(compiler, TYPE_SCRIPT);
  parser.hadError := false;
  parser.panicMode := false;
  parser.ErrorStr := '';
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
    pushStack(VM.Stack, CreateObject(pObj(func)));
    Result := newClosure(func, VM.MemTracker);
    Dec(VM.Stack.StackTop);
  end;

  // ---- Exit assertions ----
  Assert(Current = nil, 'compile exit: Current compiler should be nil');
end;


procedure InitTable(var Table : pTable; memTracker : pMemTracker);
begin
    {$IFOPT C+}
   AssertMemTrackerIsNotNil(memTracker);
   AssertPointerIsNil(Table, ' Table is not nil');
   {$ENDIF}
   Allocate(pointer(Table),0,Sizeof(TTable),MemTracker);
   Table.Count := 0;
   Table.CurrentCapacity := 0;
   Table.Entries  := nil;

   // ---- Exit assertions ----
    {$IFOPT C+}
   AssertTable(Table);
   Assert(Table.Count = 0, 'InitTable exit: count should be 0');
   Assert(Table.CurrentCapacity = 0, 'InitTable exit: capacity should be 0');
   Assert(Table.Entries = nil, 'InitTable exit: entries should be nil');
   {$ENDIF}
end;

procedure FreeEntries(var Entries : pEntry; Capacity : integer; MemTracker : pMemTracker); inline;
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
   AssertMemTrackerIsNotNil(MemTracker);
  Assert(Assigned(Entries), 'Entries is not assigned');
  AssertCapacityIsPositive(Capacity);
  {$ENDIF}
  Allocate(pointer(Entries), Capacity * Sizeof(TEntry),0,MemTracker);  //Note here that the references to objects will be free'd externally

   {$IFOPT C+}
  AssertPointerIsNil(Entries, 'FreeValues - Entries not nil after free');
   {$ENDIF}
end;

function FindEntry(Entries: pEntry; Capacity: integer; Key: pObjString): pEntry; inline;
var
  index: uint32;
  tombstone: pEntry;
  entry: pEntry;
  probeCount: integer;
begin
  // ---- Entry assertions ----
   {$IFOPT C+}
  Assert(Assigned(Entries), 'FindEntry: entries not assigned');
  Assert(Capacity > 0, 'FindEntry: capacity must be > 0');
  Assert((Capacity and (Capacity - 1)) = 0, 'FindEntry: capacity must be a power of 2');
  AssertObjStringIsAssigned(key);
  {$ENDIF}

  index := Key.hash and uint32(Capacity - 1);
  tombstone := nil;
  Result := nil;

  for probeCount := 0 to Capacity - 1 do
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
        Break;
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
      // Found the key (pointer identity ? strings are interned)
      Result := entry;
      Break;
    end;
    index := (index + 1) and uint32(Capacity - 1);
  end;

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Result <> nil, 'FindEntry: probed entire table without finding slot');
  {$ENDIF}
end;


procedure AdjustCapacity(Table: pTable; NewCapacity: integer; MemTracker: pMemTracker);
var
  NewEntries, OldEntries: pEntry;
  OldCapacity, i: integer;
  dest: pEntry;
begin
  // ---- Entry assertions ----
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertTable(Table);
  Assert(NewCapacity > 0, 'AdjustCapacity: new capacity must be > 0');
  Assert(NewCapacity >= Table.Count, 'AdjustCapacity: new capacity smaller than live count');
  {$ENDIF}
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

  {$IFOPT C+}
  // ---- Exit assertions ----
  Assert(Table.Entries <> nil, 'AdjustCapacity exit: entries should not be nil');
  Assert(Table.CurrentCapacity = NewCapacity, 'AdjustCapacity exit: capacity mismatch');


  AssertTableConsistency(Table);
  {$ENDIF}
end;


function TableSet(var Table: pTable; key: pObjString; value: TValue; MemTracker: pMemTracker): boolean;
var
  Entry: pEntry;
  NewCapacity: integer;
begin
  {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  AssertTable(Table);
  AssertObjStringIsAssigned(key);
  {$ENDIF}

  if (Table.Count + 1 > Table.CurrentCapacity * TABLE_MAX_LOAD) then
  begin
    if Table.CurrentCapacity < TABLE_START_CAPACITY then
      NewCapacity := TABLE_START_CAPACITY
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
  {$IFOPT C+}
  Assert(Entry.key = key, 'TableSet exit: key not written');
  AssertTableConsistency(Table);
  {$ENDIF}
end;


function TableDelete(Table: pTable; key: pObjString): boolean;
var
  Entry: pEntry;
begin
   {$IFOPT C+}
  AssertTable(Table);
  AssertObjStringIsAssigned(key);
  {$ENDIF}

  if Table.Count = 0 then
    Exit(false);

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  if Entry.key = nil then
    Exit(false);

  // Place a tombstone. Setting key to nil does not leak: the ObjString
  // remains on the VM's object linked list and will be freed by Sweep
  // if no other roots reference it. TableRemoveWhite handles the intern table.
  Entry.key := nil;
  Entry.value := CreateBoolean(true);
  Result := true;

   {$IFOPT C+}
  AssertTableConsistency(Table);
  {$ENDIF}
end;


function TableFindString(Table: pTable; const chars: PAnsiChar; length: integer; hash: uint32): pObjString;
var
  index, probes, capacity: uint32;
  entry: pEntry;
begin
   {$IFOPT C+}
  AssertTable(Table);
  Assert(length >= 0, 'TableFindString: length must be >= 0');
  {$ENDIF}

  Result := nil;
  if Table.Count = 0 then Exit;

   {$IFOPT C+}
  AssertTableEntries(Table);
  {$ENDIF}

  // Probe-count cap: a healthy table grows before it ever reaches full
  // saturation, so a single pass over every slot is always enough to
  // either locate the key or prove it isn't present. The cap protects
  // against pathological states (e.g. all slots tombstoned with no
  // empty slot remaining) where the original `while true do` could
  // spin forever.
  capacity := uint32(Table.CurrentCapacity);
  Assert((capacity and (capacity - 1)) = 0, 'TableFindString: capacity must be a power of 2');
  index := hash and (capacity - 1);
  probes := 0;
  while probes < capacity do
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
    index := (index + 1) and (capacity - 1);
    Inc(probes);
  end;
  // Walked the entire table without finding key or an empty slot.
  // Treat as "not present" rather than spinning indefinitely.
end;


function TableGet(Table: pTable; key: pObjString; var value: TValue): boolean;
var
  Entry: pEntry;
begin
  {$IFOPT C+}
  AssertTable(Table);
  AssertObjStringIsAssigned(key);
  {$ENDIF}

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
  {$IFOPT C+}
   // ---- Test MemTracker ---------------------------------------------------
  AssertMemTrackerIsNotNil(MemTracker);
  AssertTable(Table);
 {$ENDIF}

  if Table.Entries <> nil then
    FreeEntries(Table.Entries,Table.CurrentCapacity,MemTracker);
  Allocate(pointer(Table),Sizeof(TTable),0,MemTracker);
  Table := nil;

  // ---- Exit assertions ----
  {$IFOPT C+}
  AssertPointerIsNil(Table, 'FreeTable exit: Table should be nil');
  {$ENDIF}

end;


function HashString(const Key: PAnsiChar; Length: Integer): UInt32;inline;
const
  FNVOffset = 2166136261;
  FNVPrime = 16777619;
var
  i: Integer;
begin
  {$IFOPT C+}
  Assert(Assigned(Key), 'HashString: Key is nil');
  Assert(Length >= 0, 'HashString: Length must be >= 0');
  {$ENDIF}
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
