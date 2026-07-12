unit Suto;
{$POINTERMATH ON}
{$ASSERTIONS OFF}
{..$DEFINE DEBUG_LOG_GC}
{..$DEFINE DEBUG_STRESS_GC}
{..$DEFINE DEBUG_STRESS_TABLE}
{..$DEFINE OPCODE_PROFILING}
{..$DEFINE DEBUG_ASSERT_INVARIANTS}

interface

uses
  Classes, dialogs, System.Rtti, Math, System.TypInfo, System.SysUtils, System.AnsiStrings,
  Generics.Collections;

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
  // TAG_UNDEFINED marks a global slot that has been reserved (a name has been
  // referenced) but never DEFINE'd. Not producible from Lox source — the only
  // writer is resolveGlobalSlot. See global-slot-indexing-plan.md.
  TAG_UNDEFINED = 4;

  NIL_VAL       = QNAN or TAG_NIL;        // $7FFC000000000001
  FALSE_VAL     = QNAN or TAG_FALSE;      // $7FFC000000000002
  TRUE_VAL      = QNAN or TAG_TRUE;       // $7FFC000000000003
  UNDEFINED_VAL = QNAN or TAG_UNDEFINED;  // $7FFC000000000004


type
  // Raised by the slow paths of stack growth (pushStackGrow) and other
  // VM-internal unwinders when an unrecoverable condition is hit mid-
  // bytecode-dispatch. Caught at the CompileAndRun boundary and translated
  // into INTERPRET_RUNTIME_ERROR so deep recursion / runaway pushes surface
  // as a clean script error instead of crashing the host app.
  ELoxRuntimeError = class(Exception);
  //Enums
  TValueKind = (vkNumber, vkBoolean, vkNull, vkObject);
  TObjectKind = (okString, okFunction, okNative, okClosure, okUpvalue, okArray, okRecordType, okRecord, okNativeObject, okDictionary, okNativeClass);
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
    TOKEN_ARROW,                                  // =>

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
  pObjNativeClass  = ^TObjNativeClass;
  pNativeClassInfo = ^TNativeClassInfo;
  pObjDictionary  = ^TObjDictionary;
  pDictEntry      = ^TDictEntry;
  ppObjString     = ^pObjString;
  ppObjUpvalue    = ^pObjUpvalue;  // base of an array of pObjUpvalue, like ppObjString for fieldNames
  pCompiler       = ^TCompiler;
  pStack          = ^TStack;
  pVirtualMachine = ^TVirtualMachine;
  pMemTracker     = ^TMemTracker;

  pEntry          = ^TEntry;
  pTable          = ^TTable;
  pLocal          = ^TLocal;
  pCallFrame      = ^TCallFrame;


  //Arrays
  TAnsiCharArray = Array[0..0] of AnsiChar;

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
    upvalues    : ppObjUpvalue;  // base of upvalueCount pointers (POINTERMATH-indexed)
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

  { One script-visible property or field, resolved once at registration.
    Exactly one of prop/field is non-nil. `writable` already folds in
    [LoxProperty('readonly')], so dispatch never re-scans attributes. }
  TNativePropCache = record
    name     : AnsiString;
    prop     : TRttiProperty;
    field    : TRttiField;
    typeInfo : PTypeInfo;
    readable : Boolean;
    writable : Boolean;
  end;

  { One invokable overload with its parameter list pre-fetched
    (TRttiMethod.GetParameters allocates on every call). }
  TNativeOverload = record
    method : TRttiMethod;
    params : TArray<TRttiParameter>;
  end;

  { All script-visible overloads sharing one method name. }
  TNativeMethodCache = record
    name      : AnsiString;
    overloads : array of TNativeOverload;
  end;

  TNativeClassInfo = record
    name       : AnsiString;
    methods    : array of TNativeMethod;
    destructor_: TNativeDestructor;
    rttiEnabled: Boolean;
    rttiClass  : TClass;
    // RTTI member cache, built once by registerNativeClassRTTI. The cached
    // TRtti* handles stay valid because the global RttiCtx outlives the
    // registry (both are torn down in FreeVM, registry first).
    rttiProps    : array of TNativePropCache;
    rttiMethods  : array of TNativeMethodCache;
    rttiCtors    : array of TNativeOverload;  // public ctors of the most-derived declarer
    // Default indexed property (subscript access), resolved once.
    idxGet          : TRttiIndexedProperty;
    idxSet          : TRttiIndexedProperty;
    idxGetParamType : PTypeInfo;
    idxSetParamType : PTypeInfo;
    idxSetValueType : PTypeInfo;
  end;

  TObjNativeObject = record
    Obj          : TObj;
    instance     : Pointer;
    classInfo    : pNativeClassInfo;
    ownsInstance : Boolean;  // if true, destructor_ is called on sweep
  end;

  { A registered native class exposed as a callable Lox value: calling it
    runs a matching public Delphi constructor and returns an owned wrapper. }
  TObjNativeClass = record
    Obj       : TObj;
    classInfo : pNativeClassInfo;
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

  // Per-slot diagnostic metadata for the global slot table. Cold path only:
  // touched by runtime error reporting and by the GC name-marking walk.
  // ModuleDict is also read on the OP_DEFINE_GLOBAL / OP_SET_GLOBAL paths,
  // which are far cooler than the GET paths.
  TGlobalMeta = record
    Name : pObjString;  // interned name; used for 'Undefined variable X.' errors
    // require(): export dict of the module that owns this slot, nil for
    // main-script/builtin slots. When set, defines/assignments to the slot
    // are written through to the dict so it stays a live namespace.
    ModuleDict : pObjDictionary;
  end;

  //Virtual Machine
  TVirtualMachine = record
    Frames          : array[0..FRAMES_MAX - 1] of TCallFrame;
    FrameCount      : integer;
    Stack           : pStack;
    MemTracker      : PMemTracker;
    Strings         : pTable;
    // Global slot table — Phase 1 dormant, wired in Phase 2. Slot index replaces
    // hash-table lookup at OP_GET_GLOBAL / OP_SET_GLOBAL / OP_DEFINE_GLOBAL time.
    // GlobalValues is the hot fast-path array; GlobalMeta is cold diagnostic
    // storage. GlobalNameToSlot is the compile-time name->slot map (never touched
    // during dispatch). See global-slot-indexing-plan.md.
    GlobalValues     : array of TValue;
    GlobalMeta       : array of TGlobalMeta;
    GlobalCount      : Integer;
    GlobalCapacity   : Integer;
    GlobalNameToSlot : pTable;
    // require(): slot count at the moment of the first user compile. Slots
    // below this boundary are natives/builtins and are the only entries
    // seeded into a module's private name->slot map. -1 until first
    // CompileAndRun.
    BuiltinGlobalCount : Integer;
    // require(): while CompileModuleSource has swapped a module's private
    // map into GlobalNameToSlot, this holds the main map (and keeps it
    // GC-marked). nil outside module compiles.
    ModuleCompileSavedMap : pTable;
    // require(): export dict of the module currently being compiled. New
    // global slots allocated during that compile carry it in
    // GlobalMeta[].ModuleDict. nil outside module compiles.
    CompilingModuleDict : pObjDictionary;
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
    // Count is the number of *occupied* slots — both live entries and
    // tombstones. TableDelete deliberately does not decrement it (matches
    // the original Crafting Interpreters design) so tombstones still count
    // toward the load factor. The number of live entries is not tracked.
    Count           : integer;
    CurrentCapacity : integer;
    Entries         : pEntry;
  end;

  TRecFieldNames = array[0..MAX_BYTE_OPERAND] of pObjString;

type
  ELoxHalt = class(Exception)
  public
    HaltExitCode: Integer;
    constructor Create(const AMsg: string; AExitCode: Integer);
  end;

{$REGION 'Debug and Assertions'}
{$IFOPT C+}
procedure AssertMemTrackerIsNotNil(MemTracker : pMemTracker); inline;
procedure AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker : pMemTracker); inline;
procedure AssertNewStringIsNillBeforeAllocation(ObjString : pObjString); inline;

//Stack assertions
procedure AssertStackIsAssigned(Stack : pStack); inline;
procedure AssertStackValuesIsAssigned(Stack : pStack); inline;
procedure AssertStackCountIsZero(Stack : pStack); inline;
procedure AssertStackCapacityIsGreaterThanZero(Stack : pStack); inline;
procedure AssertStackTopEqualsValues(Stack : pStack); inline;
procedure AssertStackCapacityCanGrow(Stack : pStack); inline;
procedure AssertStackByteSizeIsSafe(NewCapacity : integer); inline;
procedure AssertStackIsNotEmpty(Stack : pStack); inline;
procedure AssertStackTopIsNotNil(Stack : pStack); inline;
procedure AssertStackIsNilBeforeInit(Stack : pStack); inline;
procedure AssertStackTopCountConsistent(Stack : pStack); inline;
procedure AssertRebaseOffsetIsNonZero(Offset : NativeInt); inline;
procedure AssertFrameCountInBounds(FrameCount : integer); inline;

//Chunk assertions
procedure AssertChunkIsAssigned(Chunk : pChunk); inline;
procedure AssertChunkCodeIsAssigned(Chunk : pChunk); inline;
procedure AssertChunkHasInstructions(Chunk : pChunk); inline;
procedure AssertChunkEndsWithReturn(Chunk : pChunk); inline;
procedure AssertChunkConstantsIsAssigned(Chunk : pChunk); inline;

//ValueArray assertions
procedure AssertValueArrayIsAssigned(ValueArray : pValueArray); inline;
procedure AssertValueArrayIsNilBeforeInit(ValueArray : pValueArray); inline;
procedure AssertValuesIsAssigned(Values : pValue); inline;
procedure AssertValueArrayCount(ValueArray : pValueArray); inline;

//Object assertions
procedure AssertObjectIsAssigned(Obj : pObj); inline;
procedure AssertObjStringIsAssigned(ObjString : pObjString); inline;
procedure AssertValidObjPointer(obj : pObj; const context : string); inline;

//Index and range assertions
procedure AssertIndexIsNotNegative(Index : integer); inline;
procedure AssertIndexInRange(Index : integer; MaxValue : integer); inline;
procedure AssertCapacityIsPositive(Capacity : integer); inline;
procedure AssertCountIsNotNegative(Count : integer); inline;
procedure AssertSizeIsNotNegative(Size : integer); inline;
procedure AssertLineIsNotNegative(Line : integer); inline;

//VM assertions
procedure AssertVMIsAssigned; inline;
procedure AssertVMChunkIsAssigned; inline;
procedure AssertVMChunkCodeIsAssigned; inline;

//Pointer assertions
procedure AssertPointerIsNotNil(p : pointer; const context : string); inline;
procedure AssertPointerIsNil(p : pointer; const context : string); inline;
procedure AssertCodePointerIsAssigned(Code : pByte); inline;

//Source code assertions
procedure AssertSourceCodeIsAssigned(Source : pAnsiChar); inline;

//String object assertions
procedure AssertStringLengthIsNotNegative(ObjString : pObjString); inline;
procedure AssertObjectKindIsString(Obj : pObj); inline;
procedure AssertInlineStringMoveBounds(const p : PObjString; ByteCount : Integer); inline;

//Value type assertions
procedure AssertValueIsBoolean(const Value : TValue); inline;
procedure AssertValueIsNumber(const Value : TValue); inline;
procedure AssertValueIsNil(const Value : TValue); inline;
procedure AssertValueIsObject(const Value : TValue); inline;
procedure AssertValueIsString(const Value : TValue); inline;

//Size comparison assertions
procedure AssertOldSizeNotEqualNewSize(OldSize, NewSize : integer); inline;
procedure AssertCountDoesNotExceedCapacity(Count, Capacity : integer); inline;

//Parser assertions
procedure AssertParseFnIsAssigned(ParseFn : TParseFn; const context : string); inline;

//Distance/offset assertions
procedure AssertDistanceIsNotNegative(Distance : integer); inline;
procedure AssertDistanceInRange(Distance, MaxValue : integer); inline;
procedure AssertTable(Table : pTable); inline;
{$ENDIF}
{$ENDREGION}

{$REGION 'Small Utilities'}
function IntToBytes(const value : integer) : TIntToByteResult; inline;
function ByteToInt(const value : TIntToByteResult) : integer; inline;
function HashString(const Key: PAnsiChar; Length: Integer): UInt32; inline;
procedure RunTimeError(const msg : string);
{$ENDREGION}

{$REGION 'Memory Management'}
procedure InitMemTracker(var MemTracker : pMemTracker); inline;
procedure FreeMemTracker(var MemTracker : pMemTracker); inline;
procedure AddToCreatedObjects(p : pObj; MemTracker : pMemTracker);
procedure ClearMem(p: PByte; FromIndex, Count: Integer);
procedure Allocate(var p : pointer; oldsize, newSize : integer; MemTracker : pMemTracker);
function AllocateArray(var List: Pointer; var CurrentCapacity: Integer; Count, ElemSize: Integer; MemTracker : pMemTracker): Boolean;
function InlineStringByteSize(Len : Integer) : NativeInt; inline;
function InlineClosureByteSize(upvalueCount : Integer) : NativeInt; inline;
{$ENDREGION}

{$REGION 'Strings (Allocation)'}
function CreateString(const S: AnsiString; MemTracker : pMemTracker): PObjString;
procedure FreeString(var obj : pObjString; MemTracker : pMemTracker);
{$ENDREGION}

{$REGION 'Chunk and ValueArray'}
procedure initChunk(var chunk: pChunk; MemTracker : pMemTracker); inline;
procedure freeChunk(var chunk: pChunk; MemTracker : pMemTracker); inline;
procedure writeChunk(chunk: pChunk; value: byte; Line : Integer; MemTracker : pMemTracker); inline;
procedure initValueArray(var ValueArray : pValueArray; MemTracker : pMemTracker); inline;
procedure writeValueArray(ValueArray : pValueArray; Value : TValue; MemTracker : pMemTracker); inline;
procedure FreeValues(var Values : pValue; Capacity : integer; MemTracker : pMemTracker); inline;
procedure freeValueArray(var ValueArray : pValueArray; MemTracker : pMemTracker); inline;
procedure FillNilValues(p: pValue; Count: NativeInt);
{$ENDREGION}

{$REGION 'Stack'}
procedure InitStack(var Stack : pStack; MemTracker : pMemTracker); inline;
procedure FreeStack(var Stack : pStack; MemTracker : pMemTracker); inline;
procedure ResetStack(stack : pStack); inline; //Stack is not passed as a var in the following rtns (as we had previously) because we're never actually changing what stack points to, just changing it's internal fields. I don't know why we had it like that to start with.
procedure pushStackGrow(stack : pStack; const value : TValue); inline;
procedure pushStack(stack : pStack; const value : TValue); inline;
function peekStack(stack : pStack; distanceFromTop : integer) : TValue; inline;
function popStack(stack : pStack) : TValue; inline;
{$ENDREGION}

{$REGION 'Values'}
function isObject(value : TValue) : boolean; inline;
function GetObject(const value : TValue) : pObj; inline;
function CreateObject(value : pObj) : TValue; inline;
function CreateBoolean(Value: Boolean): TValue; inline;
function isBoolean(const Value: TValue): Boolean; inline;
function GetBoolean(const Value : TValue) : Boolean; inline;
function isNumber(const Value: TValue): Boolean; inline;
function CreateNumber(Value: Double): TValue; inline;
function GetNumber(Value : TValue) : double; inline;
function CreateNilValue : TValue; inline;
function isNill(const Value: TValue): Boolean; inline;
function IsUndefined(const Value: TValue): Boolean; inline;
function IsFalsey(const Value: TValue): Boolean; inline;
function ValuesEqual(a, b : TValue) : boolean;
//Native helper functions -- convenience wrappers for native function authors
function AsAnsiString(value : TValue) : AnsiString; inline;
function AsWideString(value : TValue) : string; inline;
function AsInteger(value : TValue) : Integer; inline;
function CreateStringValue(const s : AnsiString) : TValue; inline;
{$ENDREGION}

{$REGION 'Table'}
procedure InitTable(var Table : pTable; memTracker : pMemTracker); inline;
function TableSet(var Table : pTable; key : pObjString; value : TValue; MemTracker : pMemTracker): boolean;
function TableGet(Table : pTable; key : pObjString; var value : TValue): boolean; inline;
function TableDelete(Table : pTable; key : pObjString): boolean;
function TableFindString(Table : pTable; const chars : PAnsiChar; length : integer; hash : uint32): pObjString;
procedure FreeTable(var Table : pTable; memTracker : pMemTracker);
{$ENDREGION}

{$REGION 'Objects'}
function isString(value : TValue) : boolean; inline;
function isClosure(value : TValue) : boolean; inline;
function newFunction(MemTracker : pMemTracker) : pObjFunction; inline;
function newNative(func : TNativeFn; arity: Integer; aName: PAnsiChar; MemTracker : pMemTracker) : pObjNative;
function newClosure(func : pObjFunction; MemTracker : pMemTracker) : pObjClosure;
function newUpvalue(slot : pValue; MemTracker : pMemTracker) : pObjUpvalue;
function newArray(MemTracker : pMemTracker) : pObjArray;
function isArray(value : TValue) : boolean; inline;
procedure EnsureArrayCapacity(arr : pObjArray; MemTracker : pMemTracker); inline;
function newDictionary(MemTracker : pMemTracker) : pObjDictionary; inline;
function isDictionary(value : TValue) : boolean; inline;
function HashValue(const value : TValue) : UInt32; inline;
function DictFindEntry(entries : pDictEntry; capacity : integer; const key : TValue) : pDictEntry; inline;
function DictGet(dict : pObjDictionary; const key : TValue; var outValue : TValue) : boolean; inline;
procedure DictSet(dict : pObjDictionary; const key : TValue; const val : TValue; MemTracker : pMemTracker); inline;
function DictDelete(dict : pObjDictionary; const key : TValue) : boolean; inline;
function isRecordType(value : TValue) : boolean; inline;
function isRecord(value : TValue) : boolean; inline;
function newRecordType(name : pObjString; fieldCount : integer; fieldNames : ppObjString; MemTracker : pMemTracker) : pObjRecordType;
function newRecord(recType : pObjRecordType; MemTracker : pMemTracker) : pObjRecord;
function isNativeObject(value : TValue) : boolean; inline;
function isNativeClass(value : TValue) : boolean; inline;
function newNativeObject(instance : Pointer; classInfo : pNativeClassInfo; MemTracker : pMemTracker; AOwnsInstance : Boolean = True) : pObjNativeObject;
function newNativeClass(classInfo : pNativeClassInfo; MemTracker : pMemTracker) : pObjNativeClass;
function findNativeMethod(classInfo : pNativeClassInfo; methodName : pObjString; out found : TNativeMethodFn) : boolean;
{$ENDREGION}

{$REGION 'String Utilities'}
function GetChar(const str : pObjString; index : integer) : AnsiChar;
function StringsEqual(a, b: PObjString): Boolean;
function TokenToString(const Token: TToken): AnsiString; inline;
function ObjStringToAnsiString(S: PObjString): AnsiString;
function ObjStringEqualsAnsi(s: PObjString; const a: AnsiString): Boolean; inline;
function ObjStringToWideStr(s: PObjString): string;
function ValueToString(const value : TValue) : pObjString; inline;
function StringToValue(const value : pObjString) : TValue; inline;
function ValueToStr(const value : TValue) : String;
procedure Concatenate(stack : pStack; MemTracker : pMemTracker);
{$ENDREGION}

{$REGION 'Chunk Emission'}
procedure emitByte(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker); inline;
procedure emitOpcode(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker); inline;
procedure EmitReturn(Chunk : pChunk; Line : integer; MemTracker : pMemTracker); inline;
procedure AddConstant(chunk : pChunk; const value : TValue; Line : Integer; MemTracker : pMemTracker); inline;
function AddValueConstant(ValueArray: pValueArray; const value: TValue; Memtracker : pMemTracker): Integer; inline;
procedure printValueArray(ValueArray: pValueArray; strings: TStrings);
{$ENDREGION}

{$REGION 'Bytecode Readers'}
function ReadByte(var code : pByte): Byte; inline;
function ReadLongIndex(var code : pByte) : Integer; inline;
function ReadJumpOffset(var code : pByte) : Word; inline;
function ReadConstant(var code : pByte; constants : pValueArray) : TValue; inline;
function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue; inline;
{$ENDREGION}

{$REGION 'Dictionary Methods'}
function InvokeDictMethod(dict: pObjDictionary; methodName: pObjString;
  argCount: integer; args: pValue; var outResult: TValue): boolean;
{$ENDREGION}

{$REGION 'VM Dispatch'}
function Run(baselineFrameCount: Integer = 0) : TInterpretResult;
{$ENDREGION}

{$REGION 'VM Callbacks'}
function InvokeCallback(closure: TValue; const args: array of TValue;
  out returnVal: TValue): TInterpretResult;
{$ENDREGION}

{$REGION 'GC Root API'}
procedure RegisterGCRoot(var slot: TValue);
function IsGCRootRegistered(var slot: TValue): Boolean;
procedure UnregisterGCRoot(var slot: TValue);
{$ENDREGION}

{$REGION 'Public Entry Points'}
function CompileAndRun(source : pAnsiChar) : TInterpretResult;
function InterpretResult(source : pAnsiChar) : TInterpretResult;
// require(): compiles a module body against its own private global
// name->slot namespace (seeded with builtins only) so top-level module
// definitions land in fresh slots tagged with moduleDict. The caller must
// keep moduleDict GC-reachable for the duration of the call.
function CompileModuleSource(source : pAnsiChar; moduleDict : pObjDictionary) : pObjClosure;
{$ENDREGION}

{$REGION 'Native Registration'}
procedure defineNative(const name : AnsiString; func : TNativeFn; arity: Integer = -1);
procedure defineGlobalNative(name : pObjString; value : TValue);
procedure registerNativeClass(const AName : AnsiString; const AMethods : array of TNativeMethod; ADestructor : TNativeDestructor);
procedure registerNativeClassRTTI(const AName : AnsiString; AClass : TClass; ADestructor : TNativeDestructor = nil);
function findNativeClass(const name : AnsiString) : pNativeClassInfo;
function findNativeClassByClass(AClass : TClass) : pNativeClassInfo;
// Rebuilds the reflected member cache. Only needed when rttiEnabled/rttiClass
// are toggled by hand after registration (see IntrospectionNatives' hybrid
// StringList); both register* functions call it themselves.
procedure BuildRttiMemberCache(info : pNativeClassInfo);
// Registers AClass under its [LoxClass('...')] name (falling back to the
// Delphi ClassName) if not yet registered, and returns its class info.
function ensureNativeClassRTTI(AClass : TClass) : pNativeClassInfo;
procedure InjectNativeObject(const name : AnsiString; instance : Pointer; const className : AnsiString);
procedure InjectObject(const name : AnsiString; instance : TObject);
// Defines a callable global (default: the class's Lox name) that constructs
// new instances via the class's public Delphi constructors. Instances built
// this way are owned by the VM and freed when the wrapper is collected.
procedure ExposeNativeClass(AClass : TClass; const AGlobalName : AnsiString = '');
// Host-side lifetime hook: call this just before Freeing a Delphi object that
// scripts may still reference. The live wrapper (if any) is disarmed so script
// access reports 'destroyed native object' instead of touching freed memory.
procedure ReleaseNativeInstance(instance : Pointer);
{$ENDREGION}

{$REGION 'VM Lifecycle'}
procedure InitVM();
procedure FreeVM();
procedure GrowGlobals; inline;
function  resolveGlobalSlot(name : pObjString; isDeclaration : Boolean = false) : Integer;
{$ENDREGION}

{$REGION 'Garbage Collection'}
procedure FreeObjects(objects: pObj);
procedure MarkObject(obj : pObj); inline;
procedure MarkValue(value : TValue); inline;
procedure MarkArray(ValueArray : pValueArray); inline;
procedure MarkTable(Table : pTable); inline;
procedure MarkCompilerRoots; inline;
procedure MarkRoots;
procedure BlackenObject(obj : pObj); inline;
procedure TraceReferences;
procedure TableRemoveWhite(Table : pTable); inline;
procedure Sweep;
procedure CollectGarbage;
{$ENDREGION}

{$REGION 'Scanner'}
procedure InitScanner(source : pAnsiChar);
function advance : ansichar; inline;
function isAtEnd : boolean; inline;
function isDigit(c : Ansichar) : boolean; inline;
function CheckKeyword(start, length: Integer; const rest: pAnsiChar;
  tokenType: TTokenType): TTokenType; inline;
function ScanNumber: TToken; inline;
function ScanToken : TToken; inline;
function ScanString: TToken; inline;
function ErrorToken(msg : pAnsiChar) : TToken; inline;
{$ENDREGION}

{$REGION 'Compiler'}
procedure Error(const Msg: pAnsiChar);
procedure errorAtCurrent(const msg : pAnsiChar);
procedure AdvanceParser(); inline;
procedure Consume(TokenKind: TTokenType; const Msg: PAnsiChar); inline;
procedure Expression(); inline;
procedure parsePrecedence(precedence : TPrecedence); inline;
function identifierType : TTokenType; inline;
procedure initCompiler(var compiler : pCompiler; funcType : TFunctionType); inline;
function compile(source : pAnsiChar) : pObjClosure;
procedure declaration();
procedure declareVariable(); inline;
procedure markInitialized(); inline;
function resolveLocal(compiler : pCompiler; const name : TToken) : integer; inline;
procedure synchronize();
procedure varDeclaration();
procedure funDeclaration();
procedure functionBody(funcType : TFunctionType);
procedure arrowFinish(hasBareParam : Boolean; const bareParam : TToken);
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
procedure recordDeclaration();
function identifierConstant(const name : TToken) : integer; inline;
function identifierGlobalSlot(const name : TToken; isDeclaration : Boolean = false) : integer; inline;
function parseVariable(const errorMsg : PAnsiChar) : integer; inline;
procedure defineVariable(global : integer); inline;
{$ENDREGION}

{$REGION 'Pratt Parsing Rule Table'}
//pratt parsing rule table used in compilation
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

    { TOKEN_ARROW }
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
{$ENDREGION}


//global variables
var
  VM : pVirtualMachine;
  Scanner : TScanner;
  Parser  : TParser;
  Current : pCompiler;
  lastPatchTarget : integer = -1;
  lastAddOffset   : integer = -1;
  // Tracks opcode boundaries so peepholes can verify the bytes they're
  // matching are genuinely at opcode offsets, not operand bytes. Both are
  // -1 when unknown (function entry, after a patched jump target lands here,
  // or after a fusion that erased the byte they pointed at).
  lastOpcodeOffset : integer = -1;
  prevOpcodeOffset : integer = -1;
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

{$REGION 'Debug and Assertions'}
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
procedure AssertTable(Table : pTable); inline;
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

procedure AssertTableEntries(Table : pTable); inline;
begin
  AssertTable(Table);
  Assert(assigned(Table.Entries), 'Table Entries is not assigned');
end;

procedure AssertMemTrackerIsNotNil(MemTracker : pMemTracker); inline;
begin
  Assert(MemTracker <> nil, 'Memory tracker is nil');
end;

procedure AssertMemTrackerBytesAllocatedIsGreaterOrEqualToZero(MemTracker : pMemTracker); inline;
begin
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(Memtracker.BytesAllocated >= 0, 'VM bytes underflow');
end;

procedure AssertNewStringIsNillBeforeAllocation(ObjString : pObjString); inline;
begin
  Assert(ObjString = nil, 'new obj string is not nil before alloc');
end;

//Stack assertions
procedure AssertStackIsAssigned(Stack : pStack); inline;
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
end;

procedure AssertStackValuesIsAssigned(Stack : pStack); inline;
begin
  Assert(Assigned(Stack.Values), 'Stack values is not assigned');
end;

procedure AssertStackCountIsZero(Stack : pStack); inline;
begin
  Assert(Stack.StackTop = Stack.Values, 'Stack count should be 0');
end;

procedure AssertStackCapacityIsGreaterThanZero(Stack : pStack); inline;
begin
  Assert(Stack.CapacityEnd > Stack.Values, 'Stack capacity is zero, cannot grow');
end;

procedure AssertStackTopEqualsValues(Stack : pStack); inline;
begin
  Assert(Stack.StackTop = Stack.Values, 'StackTop should equal Values');
end;

procedure AssertStackCapacityCanGrow(Stack : pStack); inline;
begin
  Assert((Stack.CapacityEnd - Stack.Values) <= MaxInt div GROWTH_FACTOR, 'Stack capacity multiplication would overflow');
end;

procedure AssertStackByteSizeIsSafe(NewCapacity : integer); inline;
begin
  Assert(NewCapacity <= MaxInt div SizeOf(TValue), 'Stack byte size would overflow');
end;

procedure AssertStackIsNotEmpty(Stack : pStack); inline;
begin
  Assert(Stack.StackTop > Stack.Values, 'Stack underflow - count is zero');
end;

procedure AssertStackTopCountConsistent(Stack : pStack); inline;
begin
  Assert(Stack.StackTop >= Stack.Values, 'StackTop below Values');
  Assert(Stack.StackTop <= Stack.CapacityEnd, 'StackTop beyond capacity');
end;

procedure AssertRebaseOffsetIsNonZero(Offset : NativeInt); inline;
begin
  Assert(Offset <> 0, 'pushStack rebase: offset is zero but pointers differ');
end;

procedure AssertFrameCountInBounds(FrameCount : integer); inline;
begin
  Assert((FrameCount >= 0) and (FrameCount <= FRAMES_MAX),
    'pushStack rebase: FrameCount out of bounds');
end;

procedure AssertStackTopIsNotNil(Stack : pStack); inline;
begin
  Assert(Stack.StackTop <> nil, 'Stack top is nil');
end;

procedure AssertStackIsNilBeforeInit(Stack : pStack); inline;
begin
  Assert(Stack = nil, 'Stack initialization failure - stack record is not nil');
end;

//Chunk assertions
procedure AssertChunkIsAssigned(Chunk : pChunk); inline;
begin
  Assert(Assigned(Chunk), 'Chunk is not assigned');
end;

procedure AssertChunkCodeIsAssigned(Chunk : pChunk); inline;
begin
  Assert(Assigned(Chunk.Code), 'Chunk code is not assigned');
end;

procedure AssertChunkHasInstructions(Chunk : pChunk); inline;
begin
  Assert(Chunk.Count > 0, 'No chunks to interpret');
end;

procedure AssertChunkEndsWithReturn(Chunk : pChunk); inline;
begin
  Assert(Chunk.Code[Chunk.Count-1] = OP_RETURN, 'Expected return otherwise will loop infinitely');
end;

procedure AssertChunkConstantsIsAssigned(Chunk : pChunk); inline;
begin
  Assert(Assigned(Chunk.Constants), 'Chunk constants is not assigned');
end;

//ValueArray assertions
procedure AssertValueArrayIsAssigned(ValueArray : pValueArray); inline;
begin
  Assert(Assigned(ValueArray), 'ValueArray is not assigned');
end;

procedure AssertValueArrayIsNilBeforeInit(ValueArray : pValueArray); inline;
begin
  Assert(ValueArray = nil, 'ValueArray is not nil before initialization');
end;

procedure AssertValuesIsAssigned(Values : pValue); inline;
begin
  Assert(Assigned(Values), 'Values is not assigned');
end;

procedure AssertValueArrayCount(ValueArray : pValueArray); inline;
begin
  Assert(ValueArray.Count >= 0, 'Value array count is less than zero');
end;

//Object assertions
procedure AssertObjectIsAssigned(Obj : pObj); inline;
begin
  Assert(Assigned(Obj), 'Object is not assigned');
end;

procedure AssertObjStringIsAssigned(ObjString : pObjString); inline;
begin
  Assert(Assigned(ObjString), 'ObjString is not assigned');
end;

procedure AssertValidObjPointer(obj : pObj; const context : string); inline;
begin
  if obj = nil then Exit;
  Assert(Ord(obj^.ObjectKind) <= Ord(High(TObjectKind)),
    context + ': invalid ObjectKind (corrupt or freed pointer)');
end;

//Index and range assertions
procedure AssertIndexIsNotNegative(Index : integer); inline;
begin
  Assert(Index >= 0, 'Index underflow');
end;

procedure AssertIndexInRange(Index : integer; MaxValue : integer); inline;
begin
  Assert(Index < MaxValue, 'Index overflow');
end;

procedure AssertCapacityIsPositive(Capacity : integer); inline;
begin
  Assert(Capacity > 0, 'Capacity must be positive');
end;

procedure AssertCountIsNotNegative(Count : integer); inline;
begin
  Assert(Count >= 0, 'Count underflow');
end;

procedure AssertSizeIsNotNegative(Size : integer); inline;
begin
  Assert(Size >= 0, 'Size underflow');
end;

procedure AssertLineIsNotNegative(Line : integer); inline;
begin
  Assert(Line >= 0, 'Line is < 0');
end;

//VM assertions
procedure AssertVMIsAssigned; inline;
begin
  Assert(Assigned(VM), 'VM is not assigned');
end;

procedure AssertVMChunkIsAssigned; inline;
begin
  // No longer applicable: VM uses call frames instead of a single chunk
end;

procedure AssertVMChunkCodeIsAssigned; inline;
begin
  // No longer applicable: VM uses call frames instead of a single chunk
end;

//Pointer assertions
procedure AssertPointerIsNotNil(p : pointer; const context : string); inline;
begin
  Assert(p <> nil, 'Pointer is nil: ' + context);
end;

procedure AssertPointerIsNil(p : pointer; const context : string); inline;
begin
  Assert(p = nil, 'Pointer is not nil: ' + context);
end;

procedure AssertCodePointerIsAssigned(Code : pByte); inline;
begin
  Assert(Assigned(Code), 'Code is not assigned');
end;

//Source code assertions
procedure AssertSourceCodeIsAssigned(Source : pAnsiChar); inline;
begin
  Assert(Assigned(Source), 'Source code is not assigned');
end;

//String object assertions
procedure AssertStringLengthIsNotNegative(ObjString : pObjString); inline;
begin
  Assert(ObjString.Length >= 0, 'String length is negative');
end;

procedure AssertObjectKindIsString(Obj : pObj); inline;
begin
  Assert(Obj.ObjectKind = okString, 'Type mismatch, expected a string object but object kind is not a string');
end;

//Size comparison assertions
procedure AssertOldSizeNotEqualNewSize(OldSize, NewSize : integer); inline;
begin
  Assert(OldSize <> NewSize, 'Old size = new Size - invalid allocation');
end;

procedure AssertCountDoesNotExceedCapacity(Count, Capacity : integer); inline;
begin
  Assert(Count <= Capacity, 'Count exceeds CurrentCapacity');
end;

//Parser assertions
procedure AssertParseFnIsAssigned(ParseFn : TParseFn; const context : string); inline;
begin
  Assert(Assigned(ParseFn), 'Expect expression. Parse function is not assigned: ' + context);
end;

//Distance/offset assertions
procedure AssertDistanceIsNotNegative(Distance : integer); inline;
begin
  Assert(Distance >= 0, 'Distance from top is negative');
end;

procedure AssertDistanceInRange(Distance, MaxValue : integer); inline;
begin
  Assert(Distance < MaxValue, 'Distance from top is >= maximum value');
end;

{$ENDIF}

{$ENDREGION}

{$REGION 'Small Utilities'}

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


procedure RunTimeError(const msg : string);
begin
   {$IFOPT C+}
  AssertVMIsAssigned;
  {$ENDIF}
  VM.RuntimeErrorStr := msg;
end;


//memory creation routines

{$ENDREGION}

{$REGION 'Memory Management'}
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
var
  Delta : Integer;
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

  Delta := NewSize - OldSize;

  // --------------------------------------------------------------------------
  // Garbage Collection run
  // --------------------------------------------------------------------------
  // Use projected post-allocation size so a single large grow that jumps
  // over NextGC still triggers a collection instead of silently overshooting.
  if Delta > 0 then
  begin
    {$IFDEF DEBUG_STRESS_GC}
    CollectGarbage;
    {$ENDIF}
    if MemTracker.BytesAllocated + Delta > MemTracker.NextGC then
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
  if Delta > 0 then
    ClearMem(pByte(p), OldSize, Delta);

  // --------------------------------------------------------------------------
  // Tracks total allocated bytes for GC / memory management
  // --------------------------------------------------------------------------
  IncrementBytesAllocated(Memtracker, Delta);

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

// SINGLE SOURCE OF TRUTH for the inline-string allocation size.
// TObjString.Chars is declared `array[0..0] of AnsiChar` (phantom array);
// the buffer is laid out inline at the tail of the record. SizeOf(TObjString)
// already counts the first byte of Chars[0..0]; add (Len-1) for the rest.
// CreateString, Concatenate, FreeString, and ObjStringSize MUST all call
// this helper -- never rederive the formula inline, or the three sites can
// drift and corrupt MemTracker.BytesAllocated when a string is freed.
function InlineStringByteSize(Len: Integer): NativeInt; inline;
begin
  Result := SizeOf(TObjString) + Max(0, Len - 1);
end;

// SINGLE SOURCE OF TRUTH for the closure + upvalues combined allocation size.
// TObjClosure carries an inline tail array of `upvalueCount` pObjUpvalue slots
// laid out immediately after the record. The record itself doesn't reserve any
// slots (unlike TObjString whose Chars is `array[0..0]`), so the tail size is
// exactly upvalueCount * SizeOf(pObjUpvalue). newClosure and FreeObject MUST
// route through this helper so free-time size matches alloc-time size and the
// MemTracker byte accounting stays consistent.
function InlineClosureByteSize(upvalueCount: Integer): NativeInt; inline;
begin
  Result := SizeOf(TObjClosure) + NativeInt(Max(0, upvalueCount)) * SizeOf(pObjUpvalue);
end;

{$IFOPT C+}
// Debug-only bounds check for raw `Move` reads/writes against the inline
// trailing buffer. We deliberately bypass the range checker via `Move`
// (Chars is declared length 1 but allocated longer); this assert is the
// only line of defence against an off-by-one Count argument.
procedure AssertInlineStringMoveBounds(const p: PObjString; ByteCount: Integer); inline;
begin
  AssertPointerIsNotNil(p, 'inline string');
  AssertStringLengthIsNotNegative(p);
  Assert(ByteCount >= 0, 'inline-string Move: negative byte count');
  Assert(ByteCount <= p^.length,
    Format('inline-string Move: %d bytes requested but length is %d',
      [ByteCount, p^.length]));
end;
{$ENDIF}

{$ENDREGION}

{$REGION 'Strings (Allocation)'}
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
  NewSize := InlineStringByteSize(Len);

  AllocateString(Result,0, NewSize,MemTracker);

  Result^.Obj.ObjectKind := okString;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.Length := Len;
  Result^.hash := hash;
  if Len > 0 then
  begin
    {$IFOPT C+} AssertInlineStringMoveBounds(Result, Len); {$ENDIF}
    // Raw write into the inline trailing buffer. Chars is declared
    // `array[0..0]` so a Pascal loop would trip the range checker; Move
    // is the deliberate escape hatch. See InlineStringByteSize.
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
  objSize := InlineStringByteSize(obj^.length);
  Allocate(pointer(obj), objSize , 0, MemTracker);
  obj := nil;

  // ---- postconditions ----
   {$IFOPT C+}
   AssertPointerIsNil(obj, 'string pointer not cleared after free');
  assert(Memtracker.BytesAllocated >= 0, 'VM bytes allocated underflow after freeing string.');
  {$ENDIF}
end;


{$ENDREGION}

{$REGION 'Chunk and ValueArray'}
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

procedure writeChunk(chunk: pChunk; value: byte; Line : Integer; MemTracker : pMemTracker); inline;
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

{$ENDREGION}

{$REGION 'Stack'}
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
procedure pushStackGrow(stack : pStack;const value : TValue);
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
  if OldCapacity >= STACK_MAX then
    raise ELoxRuntimeError.CreateFmt('Stack overflow (max %d slots).', [STACK_MAX]);
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
procedure pushStack(stack : pStack;const value : TValue); inline;
begin
  if Stack.StackTop < Stack.CapacityEnd then
  begin
    Stack.StackTop^ := value;
    Inc(Stack.StackTop);
  end
  else
    pushStackGrow(Stack, value);
end;


procedure ResetStack(stack : pStack);
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


//End memory creation routines






{$ENDREGION}

{$REGION 'Values'}
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
// `absolute` aliases bits over Value's storage (zero-cost type-pun;
// replaces a call Move that the optimizer couldn't inline and that
// was producing 30+ instructions per arithmetic op).
var
  bits: UInt64 absolute Value;
begin
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
var
  d: Double absolute Value;
begin
  Result := d;
end;

function CreateNilValue : TValue; inline;
begin
  Result := NIL_VAL;
end;

function isNill(const Value: TValue): Boolean; inline;
begin
  Result := Value = NIL_VAL;
end;

function IsUndefined(const Value: TValue): Boolean; inline;
begin
  Result := Value = UNDEFINED_VAL;
end;

function GetNil(const value : TValue) : byte;
begin
  Result := 0;
end;

function IsFalsey(const Value: TValue): Boolean; inline;
begin
  Result := (Value = NIL_VAL) or (Value = FALSE_VAL);
end;

//Value type assertions
procedure AssertValueIsBoolean(const Value : TValue); inline;
begin
  Assert(isBoolean(Value), 'Value is not boolean');
end;

procedure AssertValueIsNumber(const Value : TValue); inline;
begin
  Assert(isNumber(Value), 'Value is not a number');
end;

procedure AssertValueIsNil(const Value : TValue); inline;
begin
  Assert(isNill(Value), 'Value is not a nil value');
end;

procedure AssertValueIsObject(const Value : TValue); inline;
begin
  Assert(isObject(Value), 'Value is not an object');
end;

procedure AssertValueIsString(const Value : TValue); inline;
begin
  Assert(isString(Value), 'Value is not a string value');
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
    // Objects of different kinds are never equal; hoisting this check lets
    // every same-kind branch below use identity comparison without repeating
    // the ObjectKind test.
    if objA.ObjectKind <> objB.ObjectKind then
      exit(false);
    case objA.ObjectKind of
      // Intentional semantic asymmetry: okNativeObject compares by the wrapped
      // Delphi instance so two distinct VM wrappers around the same TObject are
      // considered equal. Every other kind uses pointer identity (strings are
      // interned, all others are identity-typed by design).
      okNativeObject : begin
        result := pObjNativeObject(objA)^.instance =
                  pObjNativeObject(objB)^.instance;
      end;
      // All remaining object kinds use identity (pointer) equality.
      // Listed explicitly so adding a new TObjectKind forces a decision here.
      okString,
      okFunction,
      okNative,
      okClosure,
      okUpvalue,
      okArray,
      okRecordType,
      okRecord,
      okDictionary,
      okNativeClass : begin
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

// --- Value → native Delphi conversions ---

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


{$ENDREGION}

{$REGION 'Table'}
// --- Hash table (open-addressed, string-keyed) ---

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
      // Found the key (pointer identity – strings are interned)
      Result := entry;
      Break;
    end;
    index := (index + 1) and uint32(Capacity - 1);
  end;

  // Pathological fallback: if we walked the whole table without hitting an
  // empty slot or a matching key (only possible if every slot is a live
  // entry or a tombstone), reuse the first tombstone we saw. In a healthy
  // table the 0.75 load-factor cap makes this unreachable, but this keeps
  // FindEntry defined under a corrupted state instead of returning nil.
  if (Result = nil) and (tombstone <> nil) then
    Result := tombstone;

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
  // Table.Count includes tombstones. This assert holds today because
  // AdjustCapacity is only called to grow (see TableSet), and growth
  // always picks NewCapacity >= current occupancy. If the caller ever
  // shrinks to reclaim tombstone slack this bound may need loosening.
  Assert(NewCapacity >= Table.Count, 'AdjustCapacity: new capacity smaller than occupied slot count');
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
  {$IFOPT C+}
  Assert(Entry <> nil, 'TableSet: FindEntry returned nil');
  {$ENDIF}
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
  // Defensive guard: Count > 0 with CurrentCapacity = 0 should be impossible
  // (inserts always grow first), but in release the AssertTableEntries below
  // and the power-of-2 check don't catch a zero capacity — `capacity - 1`
  // would wrap to $FFFFFFFF and mask into a bogus index. Cheap safety net.
  if Table.CurrentCapacity = 0 then Exit;

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


{$ENDREGION}

{$REGION 'Objects'}
function isString(value : TValue) : boolean; inline;
begin
  if not isObject(Value) then Exit(False);
  result := GetObject(Value).ObjectKind = okString;
end;

function isFunction(value : TValue) : boolean; inline;
begin
  if not isObject(Value) then Exit(False);
  result := GetObject(Value).ObjectKind = okFunction;
end;

function isNative(value : TValue) : boolean; inline;
begin
  if not isObject(Value) then Exit(False);
  result := GetObject(Value).ObjectKind = okNative;
end;

function isClosure(value : TValue) : boolean; inline;
begin
  if not isObject(Value) then Exit(False);
  result := GetObject(Value).ObjectKind = okClosure;
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
  i : integer;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(func <> nil, 'newClosure: func is nil');
  {$ENDIF}
  // Single allocation: TObjClosure header + inline tail array of upvalue
  // pointers. `upvalues` is aimed at the byte immediately after the record
  // so `closure^.upvalues[i]` still works via POINTERMATH ({$POINTERMATH ON}).
  Result := nil;
  Allocate(Pointer(Result), 0, InlineClosureByteSize(func^.upvalueCount), MemTracker);
  Result^.Obj.ObjectKind := okClosure;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.func := func;
  Result^.upvalueCount := func^.upvalueCount;
  if func^.upvalueCount > 0 then
  begin
    Result^.upvalues := ppObjUpvalue(pByte(Result) + SizeOf(TObjClosure));
    for i := 0 to func^.upvalueCount - 1 do
      Result^.upvalues[i] := nil;
  end
  else
    Result^.upvalues := nil;
  AddToCreatedObjects(pObj(Result), MemTracker);

  // ---- Exit assertions ----
  {$IFOPT C+}
  Assert(Result <> nil, 'newClosure exit: Result is nil');
  Assert(Result^.Obj.ObjectKind = okClosure, 'newClosure exit: ObjectKind should be okClosure');
  Assert(Result^.func = func, 'newClosure exit: func does not match input');
  Assert((func^.upvalueCount = 0) or
    (NativeUInt(Result^.upvalues) = NativeUInt(Result) + SizeOf(TObjClosure)),
    'newClosure exit: upvalues does not point at tail of record');
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
  if arr^.Count < arr^.Capacity then Exit;  ///no growth path return

  //growth path
  if arr^.Capacity = 0 then
    newCap := ARRAY_START_CAPACITY
  else
    newCap := arr^.Capacity * GROWTH_FACTOR;

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
    else if (entry^.key = key) or ValuesEqual(entry^.key, key) then
    begin
      // Found the key. The bit-identity test does double duty:
      // 1. SameValueZero semantics — ValuesEqual says NaN <> NaN (correct for
      //    ==), which would make a NaN key unfindable and turn every
      //    d[nan] := x into a fresh unreachable entry. All runtime NaNs are
      //    canonicalized to CANON_NAN, so bit identity matches them here.
      //    (+0.0/-0.0 still unify via ValuesEqual's number path.)
      // 2. Fast path — identical bits skip the out-of-line ValuesEqual call.
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

function isNativeClass(value : TValue) : boolean; inline;
var obj: pObj;
begin
  if not isObject(Value) then Exit(False);
  obj := GetObject(Value);
  result := obj.ObjectKind = okNativeClass;
end;

var
  // One live wrapper per Delphi instance. Maintained by newNativeObject /
  // freeObject; consulted by ReleaseNativeInstance so a host Free can disarm
  // the wrapper scripts still hold. Created lazily; empty between VM runs
  // because FreeObjects sweeps every wrapper through freeObject.
  NativeWrapperMap : TDictionary<Pointer, pObjNativeObject> = nil;

function newNativeObject(instance : Pointer; classInfo : pNativeClassInfo; MemTracker : pMemTracker; AOwnsInstance : Boolean = True) : pObjNativeObject;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(instance <> nil, 'newNativeObject: instance is nil');
  Assert(classInfo <> nil, 'newNativeObject: classInfo is nil');
  {$ENDIF}
  if NativeWrapperMap = nil then
    NativeWrapperMap := TDictionary<Pointer, pObjNativeObject>.Create;

  // Dedup: the same Delphi instance always maps to the same wrapper, so
  // repeated property reads don't churn wrappers and ReleaseNativeInstance
  // can disarm every script reference in one place. Ownership never
  // upgrades on a dedup hit — if either side says Delphi owns it, it does.
  if NativeWrapperMap.TryGetValue(instance, Result) then
  begin
    Result^.ownsInstance := Result^.ownsInstance and AOwnsInstance;
    Exit;
  end;

  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjNativeObject), MemTracker);
  Result^.Obj.ObjectKind := okNativeObject;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.instance := instance;
  Result^.classInfo := classInfo;
  Result^.ownsInstance := AOwnsInstance;
  AddToCreatedObjects(pObj(Result), MemTracker);
  NativeWrapperMap.Add(instance, Result);

   {$IFOPT C+}
  Assert(Result <> nil, 'newNativeObject exit: Result is nil');
  {$ENDIF}
end;

function newNativeClass(classInfo : pNativeClassInfo; MemTracker : pMemTracker) : pObjNativeClass;
begin
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  Assert(classInfo <> nil, 'newNativeClass: classInfo is nil');
  {$ENDIF}
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjNativeClass), MemTracker);
  Result^.Obj.ObjectKind := okNativeClass;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.classInfo := classInfo;
  AddToCreatedObjects(pObj(Result), MemTracker);
end;

procedure ReleaseNativeInstance(instance : Pointer);
var
  wrapper : pObjNativeObject;
begin
  if (instance = nil) or (NativeWrapperMap = nil) then Exit;
  if NativeWrapperMap.TryGetValue(instance, wrapper) then
  begin
    // Disarm, don't free: the wrapper stays a valid (dead) Lox object until
    // the GC sweeps it; script access reports a clean runtime error.
    wrapper^.instance := nil;
    wrapper^.ownsInstance := false;
    NativeWrapperMap.Remove(instance);
  end;
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

type
  { Bridges a Lox closure to a Delphi method pointer (event handler or
    callback). TMethodImplementation generates a native thunk matching the
    exact tkMethod signature; the thunk forwards to InvokeHandler, which
    marshals the Delphi arguments into Lox values and re-enters the VM via
    InvokeCallback.

    Lifetime: adapters live until FreeVM (FreeLoxCallbackAdapters) and pin
    their closure via RegisterGCRoot. If the host leaves an event wired to an
    adapter after FreeVM, invoking it is use-after-free — unhook Delphi events
    that were assigned from script before tearing the VM down. }
  TLoxCallbackAdapter = class
  private
    FClosure    : TValue;           // Lox closure, pinned as a GC root
    FTypeHandle : PTypeInfo;        // tkMethod type this adapter satisfies
    FMethodType : TRttiMethodType;
    FImpl       : TMethodImplementation;
    procedure InvokeHandler(UserData: Pointer;
      const Args: TArray<System.Rtti.TValue>; out AResult: System.Rtti.TValue);
  public
    constructor Create(const AClosure: TValue; AMethodType: TRttiMethodType);
    destructor Destroy; override;
    function AsTMethod: TMethod;
  end;

var
  LoxCallbackAdapters : TObjectList<TLoxCallbackAdapter> = nil;

{$ENDREGION}

{$REGION 'Delphi RTTI Bridge'}
function DelphiValueToLox(const V: System.Rtti.TValue; MemTracker: pMemTracker): TValue;
var
  s: AnsiString;
  childObj: TObject;
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
      // Auto-register the class if not already known ([LoxClass] name honored)
      childClassInfo := ensureNativeClassRTTI(childObj.ClassType);
      // Non-owning: the Delphi side owns objects it hands out. newNativeObject
      // dedups, so repeated reads return the same wrapper.
      childNative := newNativeObject(Pointer(childObj), childClassInfo, MemTracker, False);
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

// Binds a Lox closure to a Delphi tkMethod target (event handler / callback),
// reusing an existing adapter for the same closure/type pair.
function LoxClosureToDelphiMethod(const V: TValue; TargetType: PTypeInfo): System.Rtti.TValue;
var
  rt      : TRttiType;
  adapter : TLoxCallbackAdapter;
  i       : Integer;
  m       : TMethod;
begin
  Result := System.Rtti.TValue.Empty;
  rt := RttiCtx.GetType(TargetType);
  if not (rt is TRttiMethodType) then
  begin
    RunTimeError(Format('Cannot bind a function to Delphi type ''%s''.',
      [String(TargetType^.Name)]));
    Exit;
  end;
  if LoxCallbackAdapters = nil then
    LoxCallbackAdapters := TObjectList<TLoxCallbackAdapter>.Create(True);
  adapter := nil;
  for i := 0 to LoxCallbackAdapters.Count - 1 do
    if (LoxCallbackAdapters[i].FClosure = V) and
       (LoxCallbackAdapters[i].FTypeHandle = TargetType) then
    begin
      adapter := LoxCallbackAdapters[i];
      Break;
    end;
  if adapter = nil then
  begin
    adapter := TLoxCallbackAdapter.Create(V, TRttiMethodType(rt));
    LoxCallbackAdapters.Add(adapter);
  end;
  m := adapter.AsTMethod;
  System.Rtti.TValue.Make(@m, TargetType, Result);
end;

function LoxValueToDelphi(const V: TValue; TargetType: PTypeInfo): System.Rtti.TValue;
var
  s: string;
  d: Double;
  typeData: PTypeData;
  hostObj: TObject;
  targetClass: TClass;
  nobj: pObjNativeObject;
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
  Result := System.Rtti.TValue.Empty;
  if isNumber(V) then
  begin
    d := GetNumber(V);
    if TargetType = nil then
      Exit(System.Rtti.TValue.From<Double>(d));
    case TargetType^.Kind of
      tkInteger:
      begin
        // Strict: a fractional or out-of-range number is an error, not a
        // silent Trunc. Scripts that mean to truncate should do it explicitly.
        if Frac(d) <> 0 then
        begin
          RunTimeError(Format('Expected an integer for ''%s'' but got %g.',
            [String(TargetType^.Name), d]));
          Exit;
        end;
        typeData := GetTypeData(TargetType);
        if typeData^.OrdType = otULong then
        begin
          // Unsigned 32-bit: MaxValue is stored as -1, check explicitly
          if (d < 0) or (d > 4294967295.0) then
          begin
            RunTimeError(Format('Value %g is out of range for ''%s''.',
              [d, String(TargetType^.Name)]));
            Exit;
          end;
        end
        else if (d < typeData^.MinValue) or (d > typeData^.MaxValue) then
        begin
          RunTimeError(Format('Value %g is out of range for ''%s''.',
            [d, String(TargetType^.Name)]));
          Exit;
        end;
        // Make (not From<Integer>) so Byte/Word/subrange targets keep their type
        System.Rtti.TValue.Make(NativeInt(Trunc(d)), TargetType, Result);
      end;
      tkInt64:
      begin
        if Frac(d) <> 0 then
        begin
          RunTimeError(Format('Expected an integer for ''%s'' but got %g.',
            [String(TargetType^.Name), d]));
          Exit;
        end;
        if (d < -9.2233720368547758E18) or (d >= 9.2233720368547758E18) then
        begin
          RunTimeError(Format('Value %g is out of range for ''%s''.',
            [d, String(TargetType^.Name)]));
          Exit;
        end;
        Result := System.Rtti.TValue.From<Int64>(Trunc(d));
      end;
      tkFloat:
        Result := System.Rtti.TValue.From<Double>(d);
      tkEnumeration:
      begin
        if Frac(d) <> 0 then
        begin
          RunTimeError(Format('Expected an integer ordinal for enum ''%s''.',
            [String(TargetType^.Name)]));
          Exit;
        end;
        typeData := GetTypeData(TargetType);
        enumOrd := Trunc(d);
        if (enumOrd < typeData^.MinValue) or (enumOrd > typeData^.MaxValue) then
        begin
          RunTimeError(Format('Ordinal %d is out of range for enum ''%s''.',
            [enumOrd, String(TargetType^.Name)]));
          Exit;
        end;
        System.Rtti.TValue.Make(enumOrd, TargetType, Result);
      end;
    else
      Result := System.Rtti.TValue.From<Double>(d);
    end;
  end
  else if isBoolean(V) then
    Result := System.Rtti.TValue.From<Boolean>(GetBoolean(V))
  else if isNill(V) then
  begin
    // Typed nil/default for reference-like targets so TRttiMethod.Invoke
    // accepts the argument; everything else keeps the legacy Empty.
    if (TargetType <> nil) and
       (TargetType^.Kind in [tkClass, tkClassRef, tkInterface, tkMethod, tkDynArray, tkPointer]) then
      System.Rtti.TValue.Make(nil, TargetType, Result)
    else
      Result := System.Rtti.TValue.Empty;
  end
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
        else if (TargetType <> nil) and (TargetType^.Kind in [tkChar, tkWChar]) then
        begin
          // Single-character string -> Char parameter
          if Length(s) <> 1 then
            RunTimeError(Format('Expected a single character for ''%s'' but got a %d-char string.',
              [String(TargetType^.Name), Length(s)]))
          else if (TargetType^.Kind = tkChar) and (Ord(s[1]) > 255) then
            RunTimeError(Format('Character out of AnsiChar range for ''%s''.',
              [String(TargetType^.Name)]))
          else
            System.Rtti.TValue.Make(NativeInt(Ord(s[1])), TargetType, Result);
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
      else if isClosure(V) then
      begin
        if (TargetType <> nil) and (TargetType^.Kind = tkMethod) then
          Result := LoxClosureToDelphiMethod(V, TargetType)
        else
          RunTimeError('A function can only be passed where a Delphi event/callback is expected.');
      end
      else if isNativeObject(V) then
      begin
        nobj := pObjNativeObject(GetObject(V));
        if nobj^.instance = nil then
        begin
          RunTimeError('Cannot pass a destroyed native object to Delphi.');
          Exit;
        end;
        hostObj := TObject(nobj^.instance);
        if TargetType = nil then
          Exit(System.Rtti.TValue.From<TObject>(hostObj));
        if TargetType^.Kind <> tkClass then
        begin
          RunTimeError(Format('Cannot pass a native object where ''%s'' is expected.',
            [String(TargetType^.Name)]));
          Exit;
        end;
        targetClass := GetTypeData(TargetType)^.ClassType;
        if not hostObj.InheritsFrom(targetClass) then
        begin
          RunTimeError(Format('Cannot pass a %s where a %s is expected.',
            [hostObj.ClassName, targetClass.ClassName]));
          Exit;
        end;
        // Ownership convention: once an instance is handed to Delphi, Delphi
        // manages its lifetime — the wrapper must never double-free it.
        nobj^.ownsInstance := False;
        // Typed as the parameter's class so Invoke's compatibility check passes
        System.Rtti.TValue.Make(@hostObj, TargetType, Result);
      end
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

{ TLoxCallbackAdapter }

constructor TLoxCallbackAdapter.Create(const AClosure: TValue; AMethodType: TRttiMethodType);
begin
  inherited Create;
  FClosure := AClosure;
  FMethodType := AMethodType;
  FTypeHandle := AMethodType.Handle;
  // Pin the closure: the adapter may outlive every script reference to it.
  RegisterGCRoot(FClosure);
  FImpl := AMethodType.CreateImplementation(nil, InvokeHandler);
end;

destructor TLoxCallbackAdapter.Destroy;
begin
  if VM <> nil then
    UnregisterGCRoot(FClosure);
  FImpl.Free;
  inherited;
end;

function TLoxCallbackAdapter.AsTMethod: TMethod;
begin
  Result.Code := FImpl.CodeAddress;
  Result.Data := Self;
end;

procedure TLoxCallbackAdapter.InvokeHandler(UserData: Pointer;
  const Args: TArray<System.Rtti.TValue>; out AResult: System.Rtti.TValue);
var
  loxArgs    : array of TValue;
  i, n       : Integer;
  ret        : TValue;
  savedDepth : NativeInt;
begin
  AResult := System.Rtti.TValue.Empty;
  if VM = nil then Exit;  // event fired after FreeVM — fail soft
  // Args[0] is the method's Self (TMethod.Data = this adapter) — skip it.
  n := Length(Args) - 1;
  SetLength(loxArgs, n);
  // Restore the exact pre-call depth on every exit path: ELoxHalt /
  // ELoxRuntimeError from the callback propagate past us by design, and an
  // errored Run may return with the stack unbalanced — a blind Dec would
  // corrupt the VM stack either way. Byte offset, not a raw pointer:
  // pushStack may realloc and rebase Values.
  savedDepth := NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values);
  try
    for i := 0 to n - 1 do
    begin
      loxArgs[i] := DelphiValueToLox(Args[i + 1], VM.MemTracker);
      // Root each converted arg on the VM stack: converting the next one may
      // allocate and trigger a GC that would otherwise collect this one.
      pushStack(VM.Stack, loxArgs[i]);
    end;
    InvokeCallback(FClosure, loxArgs, ret);
  finally
    VM.Stack.StackTop := pValue(PByte(VM.Stack.Values) + savedDepth);
  end;
  // On script error, RuntimeErrorStr is set and surfaces after the Delphi
  // call that fired this event returns (see RttiInvokeMethod).
  if (FMethodType.ReturnType <> nil) and (VM.RuntimeErrorStr = '') then
    AResult := LoxValueToDelphi(ret, FMethodType.ReturnType.Handle);
end;

// Called by FreeVM: destroys all closure->event adapters (unregistering
// their GC roots) before the VM record itself is disposed.
procedure FreeLoxCallbackAdapters;
begin
  FreeAndNil(LoxCallbackAdapters);
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

function PropAttrReadOnly(const attrs: TArray<TCustomAttribute>): Boolean;
var
  a: TCustomAttribute;
begin
  for a in attrs do
    if (a is LoxPropertyAttribute) and LoxPropertyAttribute(a).IsReadOnly then Exit(True);
  Result := False;
end;

// Lifecycle / dangerous TObject methods never callable from script in
// ungated (no-attribute) mode. Gated classes are allowlist-only anyway.
function IsDeniedMethodName(const AName: string): Boolean;
begin
  Result := SameText(AName, 'Free') or SameText(AName, 'Destroy') or
            SameText(AName, 'DisposeOf') or SameText(AName, 'FreeInstance') or
            SameText(AName, 'CleanupInstance') or SameText(AName, 'AfterConstruction') or
            SameText(AName, 'BeforeDestruction');
end;

// Case-insensitive compare (Delphi identifiers are case-insensitive, and the
// old rt.GetProperty lookup was too).
function ObjStringSameTextAnsi(s: pObjString; const a: AnsiString): Boolean;
begin
  Result := (s <> nil) and (s^.length = Length(a)) and
            ((s^.length = 0) or
             (System.AnsiStrings.AnsiStrLIComp(PAnsiChar(@s^.chars[0]), PAnsiChar(a), s^.length) = 0));
end;

function FindPropCache(classInfo: pNativeClassInfo; propName: pObjString): Integer;
var
  i: Integer;
begin
  for i := 0 to High(classInfo^.rttiProps) do
    if ObjStringSameTextAnsi(propName, classInfo^.rttiProps[i].name) then Exit(i);
  Result := -1;
end;

function FindMethodCache(classInfo: pNativeClassInfo; methodName: pObjString): Integer;
var
  i: Integer;
begin
  for i := 0 to High(classInfo^.rttiMethods) do
    if ObjStringSameTextAnsi(methodName, classInfo^.rttiMethods[i].name) then Exit(i);
  Result := -1;
end;

// Compatibility score of one Lox value against one Delphi parameter type.
// -1 = incompatible (overload rejected); higher = better match.
function ScoreLoxToDelphi(const V: TValue; T: PTypeInfo): Integer;
begin
  if T = nil then Exit(-1);
  if isNumber(V) then
  begin
    case T^.Kind of
      tkFloat: Exit(3);
      tkInteger, tkInt64:
        if Frac(GetNumber(V)) = 0 then Exit(3) else Exit(-1);
      tkEnumeration:
        if T = TypeInfo(Boolean) then Exit(-1)
        else if Frac(GetNumber(V)) = 0 then Exit(1) else Exit(-1);
    else
      Exit(-1);
    end;
  end;
  if isBoolean(V) then
  begin
    if (T^.Kind = tkEnumeration) and (T = TypeInfo(Boolean)) then Exit(3);
    Exit(-1);
  end;
  if isNill(V) then
  begin
    if T^.Kind in [tkClass, tkClassRef, tkInterface, tkMethod, tkDynArray, tkPointer] then Exit(1);
    Exit(0);  // marshals as Empty — weakly compatible (legacy behavior)
  end;
  if isString(V) then
  begin
    case T^.Kind of
      tkString, tkLString, tkWString, tkUString: Exit(3);
      tkChar, tkWChar:
        if pObjString(GetObject(V))^.length = 1 then Exit(2) else Exit(-1);
      tkEnumeration:
        if T = TypeInfo(Boolean) then Exit(-1) else Exit(2);
      tkSet: Exit(1);
    else
      Exit(-1);
    end;
  end;
  if isClosure(V) then
  begin
    if T^.Kind = tkMethod then Exit(3);
    Exit(-1);
  end;
  if isNativeObject(V) then
  begin
    if T^.Kind <> tkClass then Exit(-1);
    if pObjNativeObject(GetObject(V))^.instance = nil then Exit(-1);
    if TObject(pObjNativeObject(GetObject(V))^.instance).InheritsFrom(GetTypeData(T)^.ClassType) then Exit(3);
    Exit(-1);
  end;
  if isArray(V) then
  begin
    case T^.Kind of
      tkDynArray: Exit(3);
      tkSet: Exit(2);
    else
      Exit(-1);
    end;
  end;
  Result := -1;
end;

// Picks the best-scoring overload whose arity matches and every argument is
// compatible. Ties go to the first (most-derived) declaration.
function SelectOverload(const overloads: array of TNativeOverload;
  argCount: Integer; args: pValue): Integer;
var
  i, j, total, score, bestScore: Integer;
  ok: Boolean;
begin
  Result := -1;
  bestScore := -1;
  for i := 0 to High(overloads) do
  begin
    if Length(overloads[i].params) <> argCount then Continue;
    total := 0;
    ok := True;
    for j := 0 to argCount - 1 do
    begin
      if overloads[i].params[j].ParamType = nil then begin ok := False; Break; end;
      score := ScoreLoxToDelphi(args[j], overloads[i].params[j].ParamType.Handle);
      if score < 0 then begin ok := False; Break; end;
      Inc(total, score);
    end;
    if ok and (total > bestScore) then
    begin
      bestScore := total;
      Result := i;
    end;
  end;
end;

// Reflects a class ONCE at registration into classInfo's member tables.
// Dispatch (property get/set, invoke, subscript) then never touches
// GetProperties/GetMethods/GetAttributes again.
procedure BuildRttiMemberCache(info: pNativeClassInfo);
var
  rt         : TRttiType;
  gated      : Boolean;
  prop       : TRttiProperty;
  field      : TRttiField;
  m          : TRttiMethod;
  ip         : TRttiIndexedProperty;
  attrs      : TArray<TCustomAttribute>;
  entry      : TNativePropCache;
  ipParams   : TArray<TRttiParameter>;
  nProps, n  : Integer;
  ctorParent : TRttiType;

  function AlreadyCachedProp(const AName: string): Boolean;
  var k: Integer;
  begin
    for k := 0 to nProps - 1 do
      if SameText(String(info^.rttiProps[k].name), AName) then Exit(True);
    Result := False;
  end;

  procedure AddOverload(const AName: string; AMethod: TRttiMethod);
  var k, c: Integer;
  begin
    for k := 0 to High(info^.rttiMethods) do
      if SameText(String(info^.rttiMethods[k].name), AName) then
      begin
        c := Length(info^.rttiMethods[k].overloads);
        SetLength(info^.rttiMethods[k].overloads, c + 1);
        info^.rttiMethods[k].overloads[c].method := AMethod;
        info^.rttiMethods[k].overloads[c].params := AMethod.GetParameters;
        Exit;
      end;
    k := Length(info^.rttiMethods);
    SetLength(info^.rttiMethods, k + 1);
    info^.rttiMethods[k].name := AnsiString(AName);
    SetLength(info^.rttiMethods[k].overloads, 1);
    info^.rttiMethods[k].overloads[0].method := AMethod;
    info^.rttiMethods[k].overloads[0].params := AMethod.GetParameters;
  end;

begin
  info^.rttiProps := nil;
  info^.rttiMethods := nil;
  info^.rttiCtors := nil;
  info^.idxGet := nil;
  info^.idxSet := nil;
  info^.idxGetParamType := nil;
  info^.idxSetParamType := nil;
  info^.idxSetValueType := nil;
  if not info^.rttiEnabled then Exit;
  rt := RttiCtx.GetType(info^.rttiClass);
  if rt = nil then
  begin
    info^.rttiEnabled := False;
    Exit;
  end;

  // Gating: [LoxClass] on the class, or any [LoxProperty]/[LoxMethod] member,
  // switches the class to allowlist mode — only attributed members are
  // script-visible. Otherwise (legacy/auto-registered classes) every public
  // or published member is exposed, minus the lifecycle denylist. Private and
  // protected members are never exposed in ungated mode (extended RTTI covers
  // private fields; without this check they leaked to scripts).
  gated := ClassHasLoxClassAttr(rt);
  if not gated then
    for prop in rt.GetProperties do
      if HasAttribute(prop.GetAttributes, LoxPropertyAttribute) then
      begin
        gated := True;
        Break;
      end;
  if not gated then
    for field in rt.GetFields do
      if HasAttribute(field.GetAttributes, LoxPropertyAttribute) then
      begin
        gated := True;
        Break;
      end;
  if not gated then
    for m in rt.GetMethods do
      if HasAttribute(m.GetAttributes, LoxMethodAttribute) then
      begin
        gated := True;
        Break;
      end;

  // Properties (GetProperties is most-derived-first; first name wins)
  nProps := 0;
  for prop in rt.GetProperties do
  begin
    if prop.PropertyType = nil then Continue;
    attrs := prop.GetAttributes;
    if gated then
    begin
      if not HasAttribute(attrs, LoxPropertyAttribute) then Continue;
    end
    else if not (prop.Visibility in [mvPublic, mvPublished]) then
      Continue;
    if AlreadyCachedProp(prop.Name) then Continue;
    entry.name := AnsiString(prop.Name);
    entry.prop := prop;
    entry.field := nil;
    entry.typeInfo := prop.PropertyType.Handle;
    entry.readable := prop.IsReadable;
    entry.writable := prop.IsWritable and not (gated and PropAttrReadOnly(attrs));
    SetLength(info^.rttiProps, nProps + 1);
    info^.rttiProps[nProps] := entry;
    Inc(nProps);
  end;

  // Fields
  for field in rt.GetFields do
  begin
    if field.FieldType = nil then Continue;
    attrs := field.GetAttributes;
    if gated then
    begin
      if not HasAttribute(attrs, LoxPropertyAttribute) then Continue;
    end
    else if not (field.Visibility in [mvPublic, mvPublished]) then
      Continue;
    if AlreadyCachedProp(field.Name) then Continue;
    entry.name := AnsiString(field.Name);
    entry.prop := nil;
    entry.field := field;
    entry.typeInfo := field.FieldType.Handle;
    entry.readable := True;
    entry.writable := not (gated and PropAttrReadOnly(attrs));
    SetLength(info^.rttiProps, nProps + 1);
    info^.rttiProps[nProps] := entry;
    Inc(nProps);
  end;

  // Methods and constructors
  ctorParent := nil;
  for m in rt.GetMethods do
  begin
    if not m.HasExtendedInfo then Continue;
    if m.IsConstructor then
    begin
      // Mirror Delphi name hiding: keep only constructors declared by the
      // most-derived class that declares any (GetMethods is derived-first),
      // so TObject.Create doesn't offer a zero-arg bypass of the real ctor.
      if not (m.Visibility in [mvPublic, mvPublished]) then Continue;
      if ctorParent = nil then ctorParent := m.Parent;
      if m.Parent <> ctorParent then Continue;
      n := Length(info^.rttiCtors);
      SetLength(info^.rttiCtors, n + 1);
      info^.rttiCtors[n].method := m;
      info^.rttiCtors[n].params := m.GetParameters;
      Continue;
    end;
    if m.IsDestructor then Continue;
    if not (m.MethodKind in [mkProcedure, mkFunction]) then Continue;
    if gated then
    begin
      if not HasAttribute(m.GetAttributes, LoxMethodAttribute) then Continue;
    end
    else
    begin
      if not (m.Visibility in [mvPublic, mvPublished]) then Continue;
      if IsDeniedMethodName(m.Name) then Continue;
    end;
    AddOverload(m.Name, m);
  end;

  // Default indexed property (subscript access), first match wins
  for ip in rt.GetIndexedProperties do
  begin
    if not ip.IsDefault then Continue;
    if (info^.idxGet = nil) and ip.IsReadable and (ip.ReadMethod <> nil) then
    begin
      ipParams := ip.ReadMethod.GetParameters;
      if (Length(ipParams) = 1) and (ipParams[0].ParamType <> nil) then
      begin
        info^.idxGet := ip;
        info^.idxGetParamType := ipParams[0].ParamType.Handle;
      end;
    end;
    if (info^.idxSet = nil) and ip.IsWritable and (ip.WriteMethod <> nil) then
    begin
      ipParams := ip.WriteMethod.GetParameters;
      if (Length(ipParams) >= 1) and (ipParams[0].ParamType <> nil) then
      begin
        info^.idxSet := ip;
        info^.idxSetParamType := ipParams[0].ParamType.Handle;
        info^.idxSetValueType := ip.PropertyType.Handle;
      end;
    end;
  end;
end;

function RttiGetProperty(instance: Pointer; classInfo: pNativeClassInfo;
  propName: pObjString; MemTracker: pMemTracker; out loxVal: TValue): Boolean;
var
  idx : Integer;
  dv  : System.Rtti.TValue;
begin
  Result := False;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then Exit;
  idx := FindPropCache(classInfo, propName);
  if idx < 0 then Exit;  // caller reports 'Undefined property'
  if instance = nil then
  begin
    runtimeError('Cannot access property of a destroyed native object.');
    Exit;
  end;
  if not classInfo^.rttiProps[idx].readable then
  begin
    runtimeError(Format('Property ''%s'' is write-only.',
      [String(classInfo^.rttiProps[idx].name)]));
    Exit;
  end;
  try
    if classInfo^.rttiProps[idx].prop <> nil then
      dv := classInfo^.rttiProps[idx].prop.GetValue(TObject(instance))
    else
      dv := classInfo^.rttiProps[idx].field.GetValue(TObject(instance));
    loxVal := DelphiValueToLox(dv, MemTracker);
    Result := VM.RuntimeErrorStr = '';
  except
    // Control-flow exceptions unwind to CompileAndRun intact (see OP_CALL).
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError(Format('RTTI get ''%s'' failed: %s',
        [String(classInfo^.rttiProps[idx].name), E.Message]));
      Result := False;
    end;
  end;
end;

function RttiSetProperty(instance: Pointer; classInfo: pNativeClassInfo;
  propName: pObjString; const loxVal: TValue): Boolean;
var
  idx : Integer;
  dv  : System.Rtti.TValue;
begin
  Result := False;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then Exit;
  idx := FindPropCache(classInfo, propName);
  if idx < 0 then Exit;  // caller reports 'Undefined property'
  if instance = nil then
  begin
    runtimeError('Cannot set property on a destroyed native object.');
    Exit;
  end;
  if not classInfo^.rttiProps[idx].writable then
  begin
    runtimeError(Format('Property ''%s'' is read-only.',
      [String(classInfo^.rttiProps[idx].name)]));
    Exit;
  end;
  dv := LoxValueToDelphi(loxVal, classInfo^.rttiProps[idx].typeInfo);
  if VM.RuntimeErrorStr <> '' then Exit;
  try
    if classInfo^.rttiProps[idx].prop <> nil then
      classInfo^.rttiProps[idx].prop.SetValue(TObject(instance), dv)
    else
      classInfo^.rttiProps[idx].field.SetValue(TObject(instance), dv);
    Result := True;
  except
    // Control-flow exceptions unwind to CompileAndRun intact (see OP_CALL).
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError(Format('RTTI set ''%s'' failed: %s',
        [String(classInfo^.rttiProps[idx].name), E.Message]));
      Result := False;
    end;
  end;
end;

function RttiInvokeMethod(instance: Pointer; classInfo: pNativeClassInfo;
  methodName: pObjString; argCount: integer; args: pValue;
  MemTracker: pMemTracker; out loxResult: TValue): Boolean;
var
  idx, sel, i : Integer;
  delphiArgs  : array of System.Rtti.TValue;
  dv          : System.Rtti.TValue;
begin
  Result := False;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then Exit;
  idx := FindMethodCache(classInfo, methodName);
  if idx < 0 then
  begin
    // Lifecycle methods are excluded from the cache; keep the explicit
    // 'not callable' error rather than a misleading 'Undefined method'.
    if IsDeniedMethodName(ObjStringToWideStr(methodName)) then
      runtimeError(Format('Method ''%s'' is not callable from script.',
        [ObjStringToWideStr(methodName)]));
    Exit;
  end;
  if instance = nil then
  begin
    runtimeError('Cannot invoke method on a destroyed native object.');
    Exit;
  end;
  sel := SelectOverload(classInfo^.rttiMethods[idx].overloads, argCount, args);
  if sel < 0 then
  begin
    runtimeError(Format('No overload of ''%s'' matches the given %d argument(s).',
      [String(classInfo^.rttiMethods[idx].name), argCount]));
    Exit;
  end;
  SetLength(delphiArgs, argCount);
  for i := 0 to argCount - 1 do
  begin
    delphiArgs[i] := LoxValueToDelphi(args[i],
      classInfo^.rttiMethods[idx].overloads[sel].params[i].ParamType.Handle);
    if VM.RuntimeErrorStr <> '' then Exit;
  end;
  try
    dv := classInfo^.rttiMethods[idx].overloads[sel].method.Invoke(TObject(instance), delphiArgs);
    // The method may have fired a Lox event handler that failed — surface it
    // instead of returning a value computed after the script error.
    if VM.RuntimeErrorStr <> '' then Exit;
    if classInfo^.rttiMethods[idx].overloads[sel].method.ReturnType <> nil then
      loxResult := DelphiValueToLox(dv, MemTracker)
    else
      loxResult := CreateNilValue;
    Result := VM.RuntimeErrorStr = '';
  except
    // Control-flow exceptions unwind to CompileAndRun intact (see OP_CALL).
    // Critical here: an invoked Delphi method may run a Lox callback via
    // InvokeCallback, and that script may halt().
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError(Format('RTTI invoke ''%s'' failed: %s',
        [String(classInfo^.rttiMethods[idx].name), E.Message]));
      Result := False;
    end;
  end;
end;

// Called from OP_CALL when the callee is a native class object: runs the
// best-matching public Delphi constructor and returns a VM-owned wrapper.
function ConstructNativeClass(classInfo: pNativeClassInfo; argCount: Integer;
  args: pValue; out outVal: TValue): Boolean;
var
  sel, i     : Integer;
  delphiArgs : array of System.Rtti.TValue;
  dv         : System.Rtti.TValue;
  instObj    : TObject;
  wrapper    : pObjNativeObject;
begin
  Result := False;
  outVal := CreateNilValue;
  if (classInfo = nil) or (not classInfo^.rttiEnabled) then
  begin
    runtimeError('Class is not constructible from script.');
    Exit;
  end;
  if Length(classInfo^.rttiCtors) = 0 then
  begin
    runtimeError(Format('Class ''%s'' has no accessible constructor.',
      [String(classInfo^.name)]));
    Exit;
  end;
  sel := SelectOverload(classInfo^.rttiCtors, argCount, args);
  if sel < 0 then
  begin
    runtimeError(Format('No constructor of ''%s'' matches the given %d argument(s).',
      [String(classInfo^.name), argCount]));
    Exit;
  end;
  SetLength(delphiArgs, argCount);
  for i := 0 to argCount - 1 do
  begin
    delphiArgs[i] := LoxValueToDelphi(args[i],
      classInfo^.rttiCtors[sel].params[i].ParamType.Handle);
    if VM.RuntimeErrorStr <> '' then Exit;
  end;
  try
    dv := classInfo^.rttiCtors[sel].method.Invoke(classInfo^.rttiClass, delphiArgs);
    instObj := dv.AsObject;
  except
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError(Format('Constructor of ''%s'' raised %s: %s',
        [String(classInfo^.name), E.ClassName, E.Message]));
      Exit;
    end;
  end;
  if instObj = nil then
  begin
    runtimeError(Format('Constructor of ''%s'' returned nil.', [String(classInfo^.name)]));
    Exit;
  end;
  // Script-constructed instances are VM-owned: sweeping the wrapper runs the
  // class destructor (default: TObject.Free). Ownership transfers to Delphi
  // if the value is later passed into a Delphi parameter (LoxValueToDelphi).
  wrapper := newNativeObject(Pointer(instObj), classInfo, VM.MemTracker, True);
  outVal := CreateObject(pObj(wrapper));
  Result := True;
end;

{$ENDREGION}

{$REGION 'String Utilities'}
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
  result := InlineStringByteSize(p^.length);
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
      okNativeClass: begin
        Result := '<class ' + String(pObjNativeClass(GetObject(value))^.classInfo^.name) + '>';
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

  // Empty-operand fast paths: strings are immutable and interned, so we can
  // reuse the non-empty side directly (or either side if both are empty).
  // Avoids GetMem, hash, intern lookup, and any GC allocation.
  if below^.length = 0 then
  begin
    resultStr := top;
    popStack(stack);
    popStack(stack);
    PushStack(stack, StringToValue(resultStr));
    Assert(Stack.StackTop - Stack.Values = oldCount - 1, 'Concatenate exit: should pop 2, push 1 (net -1)');
    Assert(isString(peekStack(stack,0)), 'Concatenate exit: top of stack should be a string');
    Exit;
  end;

  if top^.length = 0 then
  begin
    resultStr := below;
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
    begin
      {$IFOPT C+} AssertInlineStringMoveBounds(below, below^.length); {$ENDIF}
      Move(below^.chars[0], buf[0], below^.length);
    end;
    if top^.length > 0 then
    begin
      {$IFOPT C+} AssertInlineStringMoveBounds(top, top^.length); {$ENDIF}
      Move(top^.chars[0], buf[below^.length], top^.length);
    end;

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

    // Not interned - allocate via GC and follow CreateString's safe pattern.
    // Load-bearing invariant: AllocateString may trigger GC internally, but
    // only BEFORE the memory is returned. No collection runs between the
    // AllocateString call and the pushStack that roots the new object, so
    // the window here is GC-safe. Do NOT introduce any allocating call
    // between AllocateString and the pushStack below.
    newSize := InlineStringByteSize(totalLen);
    resultStr := nil;
    AllocateString(resultStr, 0, newSize, MemTracker);
    {$IFOPT C+}
    Assert(resultStr <> nil, 'Concatenate: AllocateString returned nil');
    {$ENDIF}

    resultStr^.Obj.ObjectKind := okString;
    resultStr^.Obj.IsMarked := false;
    resultStr^.Obj.Next := nil;
    resultStr^.length := totalLen;
    resultStr^.hash := hash;

    // Copy from our raw buffer (immune to GC)
    if totalLen > 0 then
    begin
      {$IFOPT C+} AssertInlineStringMoveBounds(resultStr, totalLen); {$ENDIF}
      Move(buf^, resultStr^.chars[0], totalLen);
    end;

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


function StringsEqual(a, b: PObjString): Boolean;
begin
   {$IFOPT C+}
   AssertObjStringIsAssigned(a);
  AssertObjStringIsAssigned(b);
  {$ENDIF}

  // Interned strings share identity; pointer equality is the common case.
  if a = b then
    Exit(True);

  if (a^.Length <> b^.Length) or (a^.Hash <> b^.Hash) then
    Exit(False);

  Result := CompareMem(@a^.Chars, @b^.Chars, a^.Length);
end;



{$ENDREGION}

{$REGION 'Chunk Emission'}
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
    prevOpcodeOffset := lastOpcodeOffset;
    lastOpcodeOffset := Chunk.Count;
    writeChunk(Chunk, OP_CONSTANT,Line,MemTracker);
    WriteChunk(Chunk,idx,Line,memTracker);
  end
  else
  begin
    prevOpcodeOffset := lastOpcodeOffset;
    lastOpcodeOffset := Chunk.Count;
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

function ReadLongIndex(var code : pByte) : Integer;
var
  Bytes : TIntToByteResult;
begin
  {$IFOPT C+}
  AssertCodePointerIsAssigned(code);
  {$ENDIF}
  Bytes.byte0 := code^; Inc(code);
  Bytes.byte1 := code^; Inc(code);
  Bytes.byte2 := code^; Inc(code);
  Result := ByteToInt(Bytes);
end;

function ReadJumpOffset(var code : pByte) : Word;
begin
  {$IFOPT C+}
  AssertCodePointerIsAssigned(code);
  {$ENDIF}
  Result := Word(code^) shl 8; Inc(code);
  Result := Result or Word(code^); Inc(code);
end;

function ReadConstantLong(var code : pByte; constants : pValueArray) : TValue; inline;
var
  idx : integer;
begin
  {$IFOPT C+}
  AssertValueArrayIsAssigned(constants);
  AssertValuesIsAssigned(constants.Values);
  {$ENDIF}
  idx := ReadLongIndex(code);
  {$IFOPT C+}
  AssertIndexIsNotNegative(idx);
  AssertIndexInRange(idx, constants.Count);
  {$ENDIF}
  result := Constants.Values[idx];
end;

{$ENDREGION}

{$REGION 'Closures and Upvalues'}
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

{$ENDREGION}

{$REGION 'Dictionary Methods'}
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

{$ENDREGION}

{$REGION 'VM Dispatch'}
function CurrentFrame: pCallFrame; inline;
 begin
    Result := @VM.Frames[VM.FrameCount - 1];
 end;

{ ---- Cold-path helpers for RunLoop -------------------------------------
  Everything below exists so the dispatch loop itself contains no exception
  scopes, no nested-procedure captures, and no managed temporaries (string
  builds). Any one of those gives RunLoop an unwind map / frame captures and
  forces the Win64 compiler to give the hot locals (ip, frame, constants,
  stack) permanent stack homes. Do not fold these back into RunLoop, and do
  not build error strings inline there — concatenation at a call site
  creates a managed temp in the CALLER. }

procedure RunErrArity(expected, got: Integer);
begin
  runtimeError('Expected ' + IntToStr(expected) + ' arguments but got ' +
    IntToStr(got) + '.');
end;

procedure RunErrStackOverflow(funcName: pObjString);
begin
  runtimeError('Stack overflow (max ' + IntToStr(FRAMES_MAX) +
    ' frames). Last call: ' + String(ObjStringToAnsiString(funcName)));
end;

// Shared shape for "<prefix> 'name'." runtime errors (undefined variable /
// property / method, read-only property).
procedure RunErrNamed(const prefix: string; name: pObjString);
begin
  runtimeError(prefix + ' ''' + ObjStringToWideStr(name) + '''.');
end;

// require(): mirrors a module-owned global slot's new value into the
// module's export dict — the dict IS the module namespace, populated
// incrementally as the body runs (this is what makes import cycles work).
// Kept out of RunLoop so the hot arms stay lean; callers guard on
// GlobalMeta[slot].ModuleDict <> nil.
procedure RunModuleDictWrite(slot: Integer; const v: TValue);
begin
  DictSet(VM.GlobalMeta[slot].ModuleDict,
    StringToValue(VM.GlobalMeta[slot].Name), v, VM.MemTracker);
end;

procedure RunErrIndexBounds(idx, count: Integer);
begin
  runtimeError('Array index ' + IntToStr(idx) + ' out of bounds [0, ' +
    IntToStr(count - 1) + '].');
end;

procedure RunErrUnknownOpcode(op: Byte);
begin
  runtimeError('Unknown opcode: ' + IntToStr(op));
end;

// OP_PRINT body: holds the managed string local so RunLoop doesn't.
procedure EmitPrint(const value: TValue);
var
  printText: string;
begin
  if VM.PrintBuilder.Length > 0 then
    VM.PrintBuilder.Append(sLineBreak);
  // Convert once; the second ValueToStr call was pure waste when an
  // OnPrint handler was attached.
  printText := ValueToStr(value);
  VM.PrintBuilder.Append(printText);
  if Assigned(VM.OnPrint) then
    VM.OnPrint(printText);
end;

// OP_CALL native arm: arity check, invoke, exception mapping. Returns False
// when a runtime error has been flagged (message already set).
function CallNativeChecked(const callee: TValue; argCount: Integer; args: pValue;
  out ret: TValue): Boolean;
var
  nativePtr: pObjNative;
  {$IFDEF DEBUG_ASSERT_INVARIANTS}
  savedNativeStackDepth: NativeInt;
  {$ENDIF}
begin
  Result := False;
  nativePtr := pObjNative(GetObject(callee));
  // Arity check (-1 means variadic, skip check)
  if (nativePtr^.arity >= 0) and (argCount <> nativePtr^.arity) then
  begin
    if nativePtr^.arity = 1 then
      runtimeError(String(AnsiString(nativePtr^.name)) + '() takes exactly 1 argument.')
    else
      runtimeError(String(AnsiString(nativePtr^.name)) + '() takes exactly ' +
        IntToStr(nativePtr^.arity) + ' arguments.');
    Exit;
  end;
  {$IFDEF DEBUG_ASSERT_INVARIANTS}
  // Save depth (not raw pointer) — realloc-safe. A native may allocate
  // strings which pushStack for GC protection, causing a stack realloc
  // that rebases StackTop.
  savedNativeStackDepth := NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values);
  {$ENDIF}
  try
    ret := nativePtr^.func(argCount, args);
  except
    // Control-flow exceptions must unwind to CompileAndRun intact:
    // ELoxHalt carries halt()'s exit code (INTERPRET_HALTED) and
    // ELoxRuntimeError is a VM-internal abort (e.g. stack overflow).
    // Converting either to a generic native error here would break
    // their semantics.
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError(String(AnsiString(nativePtr^.name)) + '() raised ' +
        E.ClassName + ': ' + E.Message);
      Exit;
    end;
  end;
  {$IFDEF DEBUG_ASSERT_INVARIANTS}
  // Natives must not change logical stack depth — they receive a pointer to
  // args and return a single TValue. VM does all stack manipulation.
  Assert((NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values)) = savedNativeStackDepth,
    'OP_CALL native: native function modified stack depth');
  {$ENDIF}
  // A native may signal failure via RuntimeErrorStr without raising.
  Result := VM.RuntimeErrorStr = '';
end;

// OP_INVOKE dict arms: calls a non-closure callable found under a dict key —
// natives, record constructors, native classes (closures need frame surgery
// and stay inline in the dispatch arms). The callee sits in the receiver
// slot (StackTop[-1-argCount]); on success that slot holds the result and
// the args are popped. Returns False when a runtime error has been flagged.
function CallDictCalleeChecked(const callee: TValue; argCount: Integer): Boolean;
var
  ret : TValue;
  stack : pStack;
  j : Integer;
begin
  Result := False;
  stack := VM.Stack;
  if isNative(callee) then
  begin
    if not CallNativeChecked(callee, argCount, stack.StackTop - argCount, ret) then
      Exit;
    stack.StackTop[-1 - argCount] := ret;
    Dec(stack.StackTop, argCount);
  end
  else if isRecordType(callee) then
  begin
    // Mirrors OP_CALL's record-construction branch, with the receiver slot
    // standing in for the callee slot.
    if argCount <> pObjRecordType(GetObject(callee))^.fieldCount then
    begin
      RunErrArity(pObjRecordType(GetObject(callee))^.fieldCount, argCount);
      Exit;
    end;
    ret := CreateObject(pObj(newRecord(pObjRecordType(GetObject(callee)), VM.MemTracker)));
    pushStack(stack, ret);  // protect from GC while copying args
    for j := 0 to argCount - 1 do
      pObjRecord(GetObject(ret))^.fields[j] := (stack.StackTop - 1 - argCount + j)^;
    // Collapse protect-copy + args + receiver to the result in one step.
    stack.StackTop[-2 - argCount] := ret;
    Dec(stack.StackTop, argCount + 1);
  end
  else
  begin
    Assert(isNativeClass(callee), 'CallDictCalleeChecked: unexpected callee kind');
    if not ConstructNativeClass(pObjNativeClass(GetObject(callee))^.classInfo,
      argCount, stack.StackTop - argCount, ret) then
      Exit;
    stack.StackTop[-1 - argCount] := ret;
    Dec(stack.StackTop, argCount);
  end;
  Result := True;
end;

// OP_INDEX_GET on a native object via its cached default indexed property.
function NativeIndexGetChecked(nativeObj: pObjNativeObject; const indexVal: TValue;
  out ret: TValue): Boolean;
var
  idxArg, dv: System.Rtti.TValue;
begin
  Result := False;
  if (nativeObj^.classInfo = nil) or (not nativeObj^.classInfo^.rttiEnabled) or
     (nativeObj^.classInfo^.idxGet = nil) then
  begin
    runtimeError('Native object has no default indexed property.');
    Exit;
  end;
  if nativeObj^.instance = nil then
  begin
    runtimeError('Cannot index a destroyed native object.');
    Exit;
  end;
  try
    idxArg := LoxValueToDelphi(indexVal, nativeObj^.classInfo^.idxGetParamType);
    if VM.RuntimeErrorStr <> '' then Exit;
    dv := nativeObj^.classInfo^.idxGet.GetValue(TObject(nativeObj^.instance), [idxArg]);
    ret := DelphiValueToLox(dv, VM.MemTracker);
    Result := VM.RuntimeErrorStr = '';
  except
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError('Indexed property get failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

// OP_INDEX_SET on a native object via its cached default indexed property.
function NativeIndexSetChecked(nativeObj: pObjNativeObject;
  const indexVal, newVal: TValue): Boolean;
var
  idxArg, dv: System.Rtti.TValue;
begin
  Result := False;
  if (nativeObj^.classInfo = nil) or (not nativeObj^.classInfo^.rttiEnabled) or
     (nativeObj^.classInfo^.idxSet = nil) then
  begin
    runtimeError('Native object has no writable default indexed property.');
    Exit;
  end;
  if nativeObj^.instance = nil then
  begin
    runtimeError('Cannot index a destroyed native object.');
    Exit;
  end;
  try
    idxArg := LoxValueToDelphi(indexVal, nativeObj^.classInfo^.idxSetParamType);
    if VM.RuntimeErrorStr <> '' then Exit;
    dv := LoxValueToDelphi(newVal, nativeObj^.classInfo^.idxSetValueType);
    if VM.RuntimeErrorStr <> '' then Exit;
    nativeObj^.classInfo^.idxSet.SetValue(TObject(nativeObj^.instance), [idxArg], dv);
    Result := True;
  except
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      runtimeError('Indexed property set failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Build a diagnostic for a host exception that escaped the dispatch loop.
//
// Crash-reporter rule: this code must be more robust than the code it is
// reporting on. Every dereference of VM state is wrapped in its own
// try/except so any one of them being corrupt cannot replace the original
// failure with a secondary AV. Even `runtimeError` is wrapped because it
// does a string assignment and could OOM.
//
// This runs in Run's handler, OUTSIDE RunLoop, so the loop's cached ip and
// current opcode are gone (deliberately: a nested procedure capturing them
// would pin them to memory). State is reconstructed from the VM; frame^.ip
// is only synced at call boundaries, so the reported offset is the last
// call site, not the faulting instruction.
procedure HandleVMException(E: Exception);
var
  msg, funcName: string;
  offset: NativeInt;
  errFrame: pCallFrame;
begin
  errFrame := nil;
  try
    if (VM <> nil) and (VM.FrameCount > 0) then
      errFrame := @VM.Frames[VM.FrameCount - 1];
  except
    errFrame := nil;
  end;

  try
    if errFrame <> nil then
      funcName := String(ObjStringToAnsiString(errFrame^.closure^.func^.name))
    else
      funcName := '<no frame>';
  except
    funcName := '<unreadable>';
  end;

  try
    if errFrame <> nil then
      offset := NativeInt(errFrame^.ip) - NativeInt(errFrame^.closure^.func^.chunk^.Code)
    else
      offset := -1;
  except
    offset := -1;
  end;

  msg := 'Host exception in function ''' + funcName + '''' +
         ' near offset ' + IntToStr(offset) + ': ' +
         E.ClassName + ': ' + E.Message;

  try
    runtimeError(msg);
  except
    // Last resort: nothing more we can safely do. Caller still flags
    // INTERPRET_RUNTIME_ERROR so the failure remains visible.
  end;
end;

procedure RunLoop(baselineFrameCount: Integer; var outcome: TInterpretResult); forward;

// Thin wrapper owning the exception scope. The hot loop lives in RunLoop,
// which must stay free of try/except so its locals can be enregistered.
function Run(baselineFrameCount: Integer = 0) : TInterpretResult;
begin
  Result := Default(TInterpretResult);
  try
    RunLoop(baselineFrameCount, Result);
  except
    // Let control-flow exceptions unwind to CompileAndRun, which owns the
    // INTERPRET_HALTED translation and the transient-VM-state reset.
    on E: ELoxHalt do
      raise;
    on E: ELoxRuntimeError do
      raise;
    on E: Exception do
    begin
      HandleVMException(E);
      Result.code := INTERPRET_RUNTIME_ERROR;
    end;
    else
    begin
      // Non-class raise (e.g. `raise 123;`) or an OS-level escape that
      // didn't surface as an Exception subclass. Build the most
      // primitive diagnostic possible — no VM-state dereferences.
      try
        runtimeError('Non-class host exception in VM dispatch.');
      except
      end;
      Result.code := INTERPRET_RUNTIME_ERROR;
    end;
  end;
end;

procedure RunLoop(baselineFrameCount: Integer; var outcome: TInterpretResult);
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
  top : pValue;
  slot : Byte;
  offset : Word;
  value,ValueB,ValueC : TValue;
  argCount : byte;
  func : pObjFunction;
  closure : pObjClosure;
  nativeResult : TValue;
  isLocal : byte;
  index : byte;
  i, j : integer;

     // record support
  recFieldNames : TRecFieldNames;
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

        OP_CONSTANT:
      begin
          value := constants[ip^];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
          Inc(ip);
      end;
        OP_CALL :
      begin
          argCount := ip^; Inc(ip);
          // Store ip back to frame before entering new frame (preserves return address)
          frame^.ip := ip;
          value := stack.StackTop[-1-argCount];

          if isClosure(value) then
          begin
            closure := pObjClosure(GetObject(value));
            func := closure^.func;
            if argCount <> func^.arity then
            begin
              RunErrArity(func^.arity, argCount);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            if VM.FrameCount = FRAMES_MAX then
            begin
              RunErrStackOverflow(func^.name);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Advance the frame pointer up front so the field writes below
            // share one address computation (frame^ = VM.Frames[FrameCount]),
            // instead of re-scaling FrameCount for every store. Safe: the
            // FRAMES_MAX guard above proves the new slot is inside the array.
            Inc(frame);
            frame^.closure := closure;
            // Point slots at the callee slot (StackTop - argCount - 1). Typed
            // pointer arithmetic scales by SizeOf(TValue) under {$POINTERMATH ON}.
            frame^.slots := stack.StackTop - (argCount + 1);
            // frame^.ip is intentionally NOT initialized here: it's dead
            // storage until either (a) the callee makes its own OP_CALL,
            // which overwrites it via `frame^.ip := ip` at the top of this
            // arm, or (b) the callee returns, in which case OP_RETURN's
            // Dec(frame) rewinds past this frame and never reads it.
            {$IFDEF DEBUG_ASSERT_INVARIANTS}
            Assert(NativeUInt(frame^.slots) >= NativeUInt(stack.Values),
              'OP_CALL: frame slots below stack base');
            Assert(NativeUInt(frame^.slots) < NativeUInt(stack.StackTop),
              'OP_CALL: frame slots at or above StackTop');
            // Exact position: slots must point at callee (StackTop - argCount - 1)
            Assert(frame^.slots = stack.StackTop - (argCount + 1),
              'OP_CALL: frame slots not positioned at callee');
            {$ENDIF}
            Inc(VM.FrameCount);
            // ip / constants come directly from the just-installed closure
            // to skip the extra frame->closure->func->chunk indirection.
            var chunk := func^.chunk;
            ip := chunk^.Code;
            constants := chunk^.Constants^.Values;
          end
          else if isNative(value) then
          begin
            // Arity check, invoke, and exception mapping live in the helper —
            // its try/except must not be in this routine (see cold-path notes).
            if not CallNativeChecked(value, argCount, stack.StackTop - argCount, nativeResult) then
            begin
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Collapse args + callee to the result in one step: overwrite the
            // callee slot, shrink by argCount. No pushStack capacity branch —
            // the slots being written already exist.
            stack.StackTop[-1 - argCount] := nativeResult;
            Dec(stack.StackTop, argCount);
          end
          else if isRecordType(value) then
          begin
            // Construct a new record instance
            if argCount <> pObjRecordType(GetObject(value))^.fieldCount then
            begin
              RunErrArity(pObjRecordType(GetObject(value))^.fieldCount, argCount);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Create the record instance - push onto stack to protect from GC
            value := CreateObject(pObj(newRecord(pObjRecordType(GetObject(value)), VM.MemTracker)));
            pushStack(stack, value);
            // Copy arguments into the record's fields (args are on stack below the new record)
            for j := 0 to argCount - 1 do
              pObjRecord(GetObject(value))^.fields[j] :=
                (stack.StackTop - 1 - argCount + j)^;
            // Collapse protect-copy + args + callee to the result in one step:
            // overwrite the callee slot with the record, shrink past the
            // protect-copy and args. [callee, args..., rec] -> [rec].
            stack.StackTop[-2 - argCount] := value;
            Dec(stack.StackTop, argCount + 1);
          end
          else if isNativeClass(value) then
          begin
            // Construct a native Delphi object via its public constructor
            if not ConstructNativeClass(pObjNativeClass(GetObject(value))^.classInfo,
              argCount, stack.StackTop - argCount, nativeResult) then
            begin
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Collapse args + callee to the result in one step (see natives).
            stack.StackTop[-1 - argCount] := nativeResult;
            Dec(stack.StackTop, argCount);
          end
          else
          begin
            runtimeError('Can only call functions and classes.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          // No trailing rebase: the closure branch above already stepped
          // frame/ip/constants. Native and record calls don't push a frame
          // and don't alter the caller's ip (frame^.ip was written back at
          // the top of this arm), so the caller's cached values are still
          // valid on entry to the next dispatch.
      end;

        OP_ADD_RETURN: begin
          // Fused ADD + RETURN. No operands.
          // Adds top two stack values, then returns the result to the caller.
          if isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2]) then
            value := CreateNumber(GetNumber(stack.StackTop[-2]) + GetNumber(stack.StackTop[-1]))
          else if isString(stack.StackTop[-1]) and isString(stack.StackTop[-2]) then
          begin
            Concatenate(stack, vm.MemTracker);
            value := stack.StackTop[-1];
          end
          else
          begin
            runtimeError('Operands must be two numbers or two strings.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
            outcome.Code := INTERPRET_OK;
            outcome.value := value;
            Exit;
          end;

          stack.StackTop := frame^.slots;
          Assert(NativeUInt(stack.StackTop) >= NativeUInt(stack.Values), 'OP_ADD_RETURN: StackTop before Values');
          stack.StackTop^ := value;
          Inc(stack.StackTop);

          // Frames are contiguous and FrameCount just decremented by 1, so
          // the caller's frame is one slot below the callee - single ptr step.
          Dec(frame);
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
            outcome.Code := INTERPRET_OK;
            outcome.value := value;
            Exit;
          end;

          // Discard the returning function's stack window
          stack.StackTop := frame^.slots;
          Assert(NativeUInt(stack.StackTop) >= NativeUInt(stack.Values), 'OP_RETURN: StackTop before Values');
          // Push return value directly — safe because we just shrunk StackTop
          // to frame^.slots, which is well below CapacityEnd.
          stack.StackTop^ := value;
          Inc(stack.StackTop);

          Dec(frame);
          ip := frame^.ip;
          constants := frame^.closure^.func^.chunk^.Constants^.Values;
        end;





        OP_CONSTANT_LONG : begin
          i := ReadLongIndex(ip);
           {$IFOPT C+}
          AssertConstantIndex(i, 'OP_CONSTANT_LONG');
          AssertFrameValues('OP_CONSTANT_LONG');
          {$ENDIF}
          value := constants[i];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_NEGATE : begin
          // In-place negation: rewrite the top slot, no pop/push.
          if not isNumber(stack.StackTop[-1]) then
          begin
             outcome.code := INTERPRET_RUNTIME_ERROR;
             runtimeError('Operand must be a number.');
             exit;
          end;
          stack.StackTop[-1] := CreateNumber(-GetNumber(stack.StackTop[-1]));
        end;

        OP_NIL      : pushStack(stack, CreateNilValue);
        OP_TRUE     : pushStack(stack, CreateBoolean(true));
        OP_FALSE    : pushStack(stack, CreateBoolean(false));

        OP_EQUAL: begin
          // Inline ValuesEqual's bit-identity fast path: identical bits mean
          // equal for every value type except the canonical NaN (NaN <> NaN).
          // Interned strings and canonicalized NaNs make this sound — see
          // ValuesEqual. Skips an out-of-line call in the common case.
          value  := stack.StackTop[-2];
          ValueB := stack.StackTop[-1];
          if value = ValueB then
            stack.StackTop[-2] := CreateBoolean(value <> CANON_NAN)
          else
            stack.StackTop[-2] := CreateBoolean(valuesEqual(value, ValueB));
          Dec(stack.StackTop);
        end;

        OP_GREATER  : begin
                        // Inlined: peek-peek-typecheck, write boolean result, pop 2 push 1.
                        // Stack before: [..., A, B]   StackTop[-2]=A, StackTop[-1]=B
                        // Stack after:  [..., A>B]    boolean in old A slot
                        top := stack.StackTop;
                        if not (isNumber(top[-1]) and isNumber(top[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        top[-2] := CreateBoolean(
                          GetNumber(top[-2]) > GetNumber(top[-1]));
                        Dec(top);
                        stack.StackTop := top;
                      end;

        OP_LESS     : begin
                        top := stack.StackTop;
                        if not (isNumber(top[-1]) and isNumber(top[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        top[-2] := CreateBoolean(
                          GetNumber(top[-2]) < GetNumber(top[-1]));
                        Dec(top);
                        stack.StackTop := top;
                      end;

        OP_ADD      : begin
                        // Fast path: both numbers ? direct in-place add, no pop/push.
                        top := stack.StackTop;
                        if isNumber(top[-1]) and isNumber(top[-2]) then
                        begin
                          top[-2] := CreateNumber(
                            GetNumber(top[-2]) + GetNumber(top[-1]));
                          Dec(top);
                          stack.StackTop := top;
                        end
                        else if isString(top[-1]) and isString(top[-2]) then
                        begin
                          // Concatenate mutates stack.StackTop; `top` is now stale.
                          Concatenate(stack, vm.MemTracker);
                        end
                        else
                        begin
                          runtimeError('Operands must be two numbers or two strings.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                      end;

        OP_SUBTRACT : begin

                        top := stack.StackTop;
                        if not (isNumber(top[-1]) and isNumber(top[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        top[-2] := CreateNumber(
                          GetNumber(top[-2]) - GetNumber(top[-1]));
                        Dec(top);
                        stack.StackTop := top;
                      end;

        OP_MULTIPLY : begin
                        top := stack.StackTop;
                        if not (isNumber(top[-1]) and isNumber(top[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        top[-2] := CreateNumber(
                          GetNumber(top[-2]) * GetNumber(top[-1]));
                        Dec(top);
                        stack.StackTop := top;
                      end;

        OP_DIVIDE   : begin
                        top := stack.StackTop;
                        if not (isNumber(top[-1]) and isNumber(top[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        top[-2] := CreateNumber(
                          GetNumber(top[-2]) / GetNumber(top[-1]));
                        Dec(top);
                        stack.StackTop := top;
                      end;

        OP_MODULO   : begin
                        top := stack.StackTop;
                        if not (isNumber(top[-1]) and isNumber(top[-2])) then
                        begin
                          runtimeError('Operands must be numbers.');
                          outcome.code := INTERPRET_RUNTIME_ERROR;
                          exit;
                        end;
                        // Match prior semantics: A - Trunc(A/B)*B
                        top[-2] := CreateNumber(
                          GetNumber(top[-2]) -
                          Trunc(GetNumber(top[-2]) / GetNumber(top[-1])) *
                          GetNumber(top[-1]));
                        Dec(top);
                        stack.StackTop := top;
                      end;

        OP_NOT      : begin
          // In-place: rewrite top slot to boolean(isFalsey(top)).
          stack.StackTop[-1] := CreateBoolean(isFalsey(stack.StackTop[-1]));
        end;

        OP_PRINT: begin
          Dec(stack.StackTop);
          // The string conversion (managed local) lives in the helper —
          // a managed local here would give this routine an unwind scope.
          EmitPrint(stack.StackTop^);
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
          // Operand is a global slot index (Phase 2 cutover: was a
          // constant-pool index in the pre-slot design). See
          // global-slot-indexing-plan.md.
          i := ip^;
          Inc(ip);
          {$IFOPT C+}
          Assert(i < vm.GlobalCount, 'OP_GET_GLOBAL: slot out of range');
          {$ENDIF}
          value := vm.GlobalValues[i];
          if value = UNDEFINED_VAL then
          begin
            RunErrNamed('Undefined variable', vm.GlobalMeta[i].Name);
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
         // pushStack(stack, value); xxx

          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);

        end;

        OP_SET_GLOBAL: begin
          i := ip^;
          Inc(ip);
          {$IFOPT C+}
          Assert(i < vm.GlobalCount, 'OP_SET_GLOBAL: slot out of range');
          {$ENDIF}
          if vm.GlobalValues[i] = UNDEFINED_VAL then
          begin
            RunErrNamed('Undefined variable', vm.GlobalMeta[i].Name);
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          vm.GlobalValues[i] := stack.StackTop[-1];
          if vm.GlobalMeta[i].ModuleDict <> nil then
            RunModuleDictWrite(i, stack.StackTop[-1]);
          // Assignment is an expression — leave the value on stack.
        end;

        OP_GET_LOCAL: begin
            slot := ip^;
            Inc(ip);
            value := frame^.slots[slot];
            {$IFOPT C+}
            Assert(NativeUInt(@frame^.slots[slot]) < NativeUInt(stack.StackTop),
              'OP_GET_LOCAL: slot out of stack bounds');
            {$ENDIF}
            top := stack.StackTop;
            if top < stack.CapacityEnd then
            begin
              top^ := value;
              Inc(top);
              stack.StackTop := top;
            end
            else
              pushStackGrow(stack, value);

        end;

        // OP_GET_LOCAL_0..OP_GET_LOCAL_7: fast-slot get, slot encoded in opcode.
        // Split into 8 arms so the slot index is a compile-time constant
        // displacement (`frame^.slots[N]` lowers to `[rFrame + N*8]`) instead
        // of a runtime `instruction - OP_GET_LOCAL_0` plus indexed addressing.
        OP_GET_LOCAL_0: begin
           value := frame^.slots[0];

          {$IFOPT C+}
          Assert(NativeUInt(@frame^.slots[0]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_0: slot out of stack bounds');
          {$ENDIF}

          top := stack.StackTop;

          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_1: begin
          Assert(NativeUInt(@frame^.slots[1]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_1: slot out of stack bounds');
          value := frame^.slots[1];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_2: begin
          Assert(NativeUInt(@frame^.slots[2]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_2: slot out of stack bounds');
          value := frame^.slots[2];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_3: begin
          Assert(NativeUInt(@frame^.slots[3]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_3: slot out of stack bounds');
          value := frame^.slots[3];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_4: begin
          Assert(NativeUInt(@frame^.slots[4]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_4: slot out of stack bounds');
          value := frame^.slots[4];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_5: begin
          Assert(NativeUInt(@frame^.slots[5]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_5: slot out of stack bounds');
          value := frame^.slots[5];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_6: begin
          Assert(NativeUInt(@frame^.slots[6]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_6: slot out of stack bounds');
          value := frame^.slots[6];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_GET_LOCAL_7: begin
          Assert(NativeUInt(@frame^.slots[7]) < NativeUInt(stack.StackTop),
            'OP_GET_LOCAL_7: slot out of stack bounds');
          value := frame^.slots[7];
          top := stack.StackTop;
          if top < stack.CapacityEnd then
          begin
            top^ := value;
            Inc(top);
            stack.StackTop := top;
          end
          else
            pushStackGrow(stack, value);
        end;

        OP_SET_LOCAL: begin
          slot := ip^; Inc(ip);
          Assert(NativeUInt(@frame^.slots[slot]) < NativeUInt(stack.StackTop), 'OP_SET_LOCAL: slot out of stack bounds');
          frame^.slots[slot] := stack.StackTop[-1];
        end;

        // OP_SET_LOCAL_0..OP_SET_LOCAL_7: fast-slot set, slot encoded in opcode.
        // Leaves value on stack (assignment-expression semantics, identical to
        // OP_SET_LOCAL). Split for the same reason as the GET variants.
        OP_SET_LOCAL_0: begin
          Assert(NativeUInt(@frame^.slots[0]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_0: slot out of stack bounds');
          frame^.slots[0] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_1: begin
          Assert(NativeUInt(@frame^.slots[1]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_1: slot out of stack bounds');
          frame^.slots[1] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_2: begin
          Assert(NativeUInt(@frame^.slots[2]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_2: slot out of stack bounds');
          frame^.slots[2] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_3: begin
          Assert(NativeUInt(@frame^.slots[3]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_3: slot out of stack bounds');
          frame^.slots[3] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_4: begin
          Assert(NativeUInt(@frame^.slots[4]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_4: slot out of stack bounds');
          frame^.slots[4] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_5: begin
          Assert(NativeUInt(@frame^.slots[5]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_5: slot out of stack bounds');
          frame^.slots[5] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_6: begin
          Assert(NativeUInt(@frame^.slots[6]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_6: slot out of stack bounds');
          frame^.slots[6] := stack.StackTop[-1];
        end;

        OP_SET_LOCAL_7: begin
          Assert(NativeUInt(@frame^.slots[7]) < NativeUInt(stack.StackTop),
            'OP_SET_LOCAL_7: slot out of stack bounds');
          frame^.slots[7] := stack.StackTop[-1];
        end;

        OP_JUMP: begin
          offset := ReadJumpOffset(ip);
          Inc(ip, offset);
          {$IFOPT C+}
          AssertFrameCode('OP_JUMP');
          Assert(NativeUInt(ip) <= NativeUInt(frame^.closure^.func^.chunk^.Code) + NativeUInt(frame^.closure^.func^.chunk^.Count), 'OP_JUMP: ip past end of code');
          {$ENDIF}
        end;

        OP_JUMP_IF_FALSE: begin
          offset := ReadJumpOffset(ip);
          if isFalsey(stack.StackTop[-1]) then
            Inc(ip, offset);
        end;

        OP_JUMP_IF_FALSE_POP: begin
          // Fused: pop top, then jump if popped value was falsy.
          // Replaces OP_JUMP_IF_FALSE + OP_POP pair in if/while/for.
          offset := ReadJumpOffset(ip);
           {$IFOPT C+}
           AssertStackIsNotEmpty(Stack);
           {$ENDIF}
          Dec(stack.StackTop);
          if isFalsey(stack.StackTop^) then
            Inc(ip, offset);
        end;

        OP_LESS_JUMP_IF_FALSE: begin
          // Fused OP_LESS + OP_JUMP_IF_FALSE_POP: compare two numbers,
          // pop both, jump if NOT (a < b). Saves one dispatch cycle.
          offset := ReadJumpOffset(ip);
          if not (isNumber(stack.StackTop[-1]) and isNumber(stack.StackTop[-2])) then
          begin
            runtimeError('Operands must be numbers.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
          if stack.StackTop < stack.CapacityEnd then
          begin
            stack.StackTop^ := CreateNumber(
              GetNumber(frame^.slots[slot]) - GetNumber(constants[index]));
            Inc(stack.StackTop);
          end
          else
            pushStackGrow(stack, CreateNumber(
              GetNumber(frame^.slots[slot]) - GetNumber(constants[index])));
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
            Concatenate(stack, vm.MemTracker);
            frame^.slots[slot] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else
          begin
            runtimeError('Operands must be two numbers or two strings.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;


        OP_LOOP: begin
          offset := ReadJumpOffset(ip);
          Dec(ip, offset);
          {$IFOPT C+}
          AssertFrameCode('OP_LOOP');
          Assert(NativeUInt(ip) >= NativeUInt(frame^.closure^.func^.chunk^.Code), 'OP_LOOP: ip before start of code');
          {$ENDIF}
        end;

        OP_DEFINE_GLOBAL: begin
          i := ip^;
          Inc(ip);
          {$IFOPT C+}
          Assert(i < vm.GlobalCount, 'OP_DEFINE_GLOBAL: slot out of range');
          {$ENDIF}
          vm.GlobalValues[i] := stack.StackTop[-1];
          if vm.GlobalMeta[i].ModuleDict <> nil then
            RunModuleDictWrite(i, stack.StackTop[-1]);
          Dec(stack.StackTop);
        end;


        OP_CLOSURE: begin
          value := constants[ip^];
          Inc(ip);
          Assert(isFunction(value), 'OP_CLOSURE: constant is not a function');
         // func := pObjFunction(GetObject(value));
          closure := newClosure(pObjFunction(GetObject(value)), VM.MemTracker);
          pushStack(stack, CreateObject(pObj(closure)));
          for i := 0 to closure^.upvalueCount - 1 do
          begin
            isLocal := ip^; Inc(ip);
            index := ip^; Inc(ip);
            if isLocal = 1 then
              closure^.upvalues[i] := captureUpvalue(frame^.slots + index)
            else
            begin
              {$IFOPT C+}AssertUpvalueIndex(index, 'OP_CLOSURE');{$ENDIF}
              closure^.upvalues[i] := frame^.closure^.upvalues[index];
            end;
          end;
        end;

        OP_GET_UPVALUE: begin
          slot := ip^; Inc(ip);
          {$IFOPT C+}
          AssertUpvalueIndex(slot, 'OP_GET_UPVALUE');
          Assert(frame^.closure^.upvalues[slot] <> nil, 'OP_GET_UPVALUE: upvalue is nil');
          {$ENDIF}
          pushStack(stack, frame^.closure^.upvalues[slot]^.location^);
        end;

        OP_SET_UPVALUE: begin
          slot := ip^; Inc(ip);
          {$IFOPT C+}
          AssertUpvalueIndex(slot, 'OP_SET_UPVALUE');
          Assert(frame^.closure^.upvalues[slot] <> nil, 'OP_SET_UPVALUE: upvalue is nil');
          {$ENDIF}
          frame^.closure^.upvalues[slot]^.location^ := stack.StackTop[-1];
        end;

        OP_CLOSE_UPVALUE: begin
          closeUpvalues(stack.StackTop - 1);
          Dec(stack.StackTop);
        end;


        OP_DEFINE_GLOBAL_LONG: begin
          i := ReadLongIndex(ip);
          {$IFOPT C+}
          Assert(i < vm.GlobalCount, 'OP_DEFINE_GLOBAL_LONG: slot out of range');
          {$ENDIF}
          vm.GlobalValues[i] := stack.StackTop[-1];
          if vm.GlobalMeta[i].ModuleDict <> nil then
            RunModuleDictWrite(i, stack.StackTop[-1]);
          Dec(stack.StackTop);
        end;

        OP_GET_GLOBAL_LONG: begin
          i := ReadLongIndex(ip);
          {$IFOPT C+}
          Assert(i < vm.GlobalCount, 'OP_GET_GLOBAL_LONG: slot out of range');
          {$ENDIF}
          value := vm.GlobalValues[i];
          if value = UNDEFINED_VAL then
          begin
            RunErrNamed('Undefined variable', vm.GlobalMeta[i].Name);
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          pushStack(stack, value);
        end;

        OP_SET_GLOBAL_LONG: begin
          i := ReadLongIndex(ip);
          {$IFOPT C+}
          Assert(i < vm.GlobalCount, 'OP_SET_GLOBAL_LONG: slot out of range');
          {$ENDIF}
          if vm.GlobalValues[i] = UNDEFINED_VAL then
          begin
            RunErrNamed('Undefined variable', vm.GlobalMeta[i].Name);
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
          vm.GlobalValues[i] := stack.StackTop[-1];
          if vm.GlobalMeta[i].ModuleDict <> nil then
            RunModuleDictWrite(i, stack.StackTop[-1]);
          // Assignment is an expression — leave the value on stack.
        end;

        OP_CLOSURE_LONG: begin
          i := ReadLongIndex(ip);
          value := constants[i];
          Assert(isFunction(value), 'OP_CLOSURE_LONG: constant is not a function');
          //func := pObjFunction(GetObject(value));
          closure := newClosure(pObjFunction(GetObject(value)), VM.MemTracker);
          pushStack(stack, CreateObject(pObj(closure)));
          for i := 0 to closure^.upvalueCount - 1 do
          begin
            isLocal := ip^; Inc(ip);
            index := ip^; Inc(ip);
            if isLocal = 1 then
              closure^.upvalues[i] := captureUpvalue(frame^.slots + index)
            else
            begin
              {$IFOPT C+}AssertUpvalueIndex(index, 'OP_CLOSURE_LONG');{$ENDIF}
              closure^.upvalues[i] := frame^.closure^.upvalues[index];
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
            recNameIdx := ReadLongIndex(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_RECORD_LONG field name');
            AssertFrameValues('OP_RECORD_LONG field name');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_RECORD_LONG: field name constant is not a string');
            recFieldNames[i] := ValueToString(value);
          end;
          // Read the record type name (3 bytes, 24-bit LE)
          recNameIdx := ReadLongIndex(ip);
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
              RunErrNamed('Undefined property', fieldName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Replace receiver with the field value in place (no pop/push).
            stack.StackTop[-1] := rec^.fields[fieldIdx];
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
                RunErrNamed('Undefined property', rttiPropName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Replace receiver with the property value in place (no pop/push).
            stack.StackTop[-1] := rttiResult;
          end
          else if isDictionary(stack.StackTop[-1]) then
          begin
            // Dict dot-read: d.key is sugar for d["key"]. Module namespaces
            // returned by require() rely on this (U.forEach, U.VERSION).
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_GET_PROPERTY');
            AssertFrameValues('OP_GET_PROPERTY');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_GET_PROPERTY: name constant is not a string');
            if not DictGet(pObjDictionary(GetObject(stack.StackTop[-1])), value, ValueB) then
            begin
              RunErrNamed('Undefined property', ValueToString(value));
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Replace receiver with the entry value in place (no pop/push).
            stack.StackTop[-1] := ValueB;
          end
          else
          begin
            recNameIdx := ip^; Inc(ip); // consume operand
            runtimeError('Only records and native objects have properties.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
              RunErrNamed('Undefined property', fieldName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            rec^.fields[fieldIdx] := stack.StackTop[-1];
            // Write the assigned value over the receiver, shrink by one:
            // [recv, value] -> [value] (assignment-expression result).
            stack.StackTop[-2] := stack.StackTop[-1];
            Dec(stack.StackTop);
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
                RunErrNamed('Undefined or read-only property', rttiPropName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Write the assigned value over the receiver, shrink by one:
            // [recv, value] -> [value] (assignment-expression result).
            stack.StackTop[-2] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else if isDictionary(stack.StackTop[-2]) then
          begin
            // Dict dot-write: d.key = v is sugar for d["key"] = v.
            recNameIdx := ip^; Inc(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_SET_PROPERTY');
            AssertFrameValues('OP_SET_PROPERTY');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_SET_PROPERTY: name constant is not a string');
            DictSet(pObjDictionary(GetObject(stack.StackTop[-2])), value,
              stack.StackTop[-1], vm.MemTracker);
            // Write the assigned value over the receiver, shrink by one:
            // [recv, value] -> [value] (assignment-expression result).
            stack.StackTop[-2] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else
          begin
            recNameIdx := ip^; Inc(ip); // consume operand
            runtimeError('Only records and native objects have properties.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
            // Callable-key dispatch first: if the dict holds a callable under
            // this key (module exports via require() — closures, natives,
            // record constructors, native classes), call it. Falls back to
            // the builtin dict methods (Set/Get/Keys/...) otherwise.
            if DictGet(pObjDictionary(GetObject(value)), ValueB, nativeResult) and
               (isClosure(nativeResult) or isNative(nativeResult) or
                isRecordType(nativeResult) or isNativeClass(nativeResult)) then
            begin
              if isClosure(nativeResult) then
              begin
                closure := pObjClosure(GetObject(nativeResult));
                func := closure^.func;
                if invokeArgCount <> func^.arity then
                begin
                  RunErrArity(func^.arity, invokeArgCount);
                  outcome.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
                if VM.FrameCount = FRAMES_MAX then
                begin
                  RunErrStackOverflow(func^.name);
                  outcome.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
                // Overwrite the receiver with the callee, then enter the
                // frame exactly as OP_CALL's closure fast-path does.
                stack.StackTop[-1 - invokeArgCount] := nativeResult;
                frame^.ip := ip;
                Inc(frame);
                frame^.closure := closure;
                frame^.slots := stack.StackTop - (invokeArgCount + 1);
                Inc(VM.FrameCount);
                ip := func^.chunk^.Code;
                constants := func^.chunk^.Constants^.Values;
              end
              else
              begin
                if not CallDictCalleeChecked(nativeResult, invokeArgCount) then
                begin
                  outcome.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
              end;
            end
            else
            begin
              if not InvokeDictMethod(pObjDictionary(GetObject(value)), invokeMethodName,
                invokeArgCount, stack.StackTop - invokeArgCount, nativeResult) then
              begin
                outcome.code := INTERPRET_RUNTIME_ERROR;
                exit;
              end;
              // Collapse args + receiver to the result in one step (see OP_CALL).
              stack.StackTop[-1 - invokeArgCount] := nativeResult;
              Dec(stack.StackTop, invokeArgCount);
            end;
          end
          else if isNativeObject(value) then
          begin
            nativeObj := pObjNativeObject(GetObject(value));
            if findNativeMethod(nativeObj^.classInfo, invokeMethodName, nativeMethod) then
            begin
              // Call manual wrapper method
              nativeResult := nativeMethod(nativeObj^.instance, invokeArgCount,
                stack.StackTop - invokeArgCount);
              if VM.RuntimeErrorStr <> '' then
              begin
                outcome.code := INTERPRET_RUNTIME_ERROR;
                Exit;
              end;
            end
            else if RttiInvokeMethod(nativeObj^.instance, nativeObj^.classInfo,
              invokeMethodName, invokeArgCount,
              stack.StackTop - invokeArgCount,
              VM.MemTracker, nativeResult) then
            begin
              // RTTI method call succeeded
            end
            else
            begin
              if VM.RuntimeErrorStr = '' then
                RunErrNamed('Undefined method', invokeMethodName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Pop args + receiver, push result
            // Collapse args + receiver to the result in one step (see OP_CALL).
            stack.StackTop[-1 - invokeArgCount] := nativeResult;
            Dec(stack.StackTop, invokeArgCount);
          end
          else
          begin
            runtimeError('Only dictionaries and native objects have methods.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_INVOKE_LONG: begin
          invokeNameIdx := ReadLongIndex(ip);
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
            // Callable-key dispatch first — see OP_INVOKE for commentary.
            if DictGet(pObjDictionary(GetObject(value)), ValueB, nativeResult) and
               (isClosure(nativeResult) or isNative(nativeResult) or
                isRecordType(nativeResult) or isNativeClass(nativeResult)) then
            begin
              if isClosure(nativeResult) then
              begin
                closure := pObjClosure(GetObject(nativeResult));
                func := closure^.func;
                if invokeArgCount <> func^.arity then
                begin
                  RunErrArity(func^.arity, invokeArgCount);
                  outcome.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
                if VM.FrameCount = FRAMES_MAX then
                begin
                  RunErrStackOverflow(func^.name);
                  outcome.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
                stack.StackTop[-1 - invokeArgCount] := nativeResult;
                frame^.ip := ip;
                Inc(frame);
                frame^.closure := closure;
                frame^.slots := stack.StackTop - (invokeArgCount + 1);
                Inc(VM.FrameCount);
                ip := func^.chunk^.Code;
                constants := func^.chunk^.Constants^.Values;
              end
              else
              begin
                if not CallDictCalleeChecked(nativeResult, invokeArgCount) then
                begin
                  outcome.code := INTERPRET_RUNTIME_ERROR;
                  exit;
                end;
              end;
            end
            else
            begin
              if not InvokeDictMethod(pObjDictionary(GetObject(value)), invokeMethodName,
                invokeArgCount, stack.StackTop - invokeArgCount, nativeResult) then
              begin
                outcome.code := INTERPRET_RUNTIME_ERROR;
                exit;
              end;
              // Collapse args + receiver to the result in one step (see OP_CALL).
              stack.StackTop[-1 - invokeArgCount] := nativeResult;
              Dec(stack.StackTop, invokeArgCount);
            end;
          end
          else if isNativeObject(value) then
          begin
            nativeObj := pObjNativeObject(GetObject(value));
            if findNativeMethod(nativeObj^.classInfo, invokeMethodName, nativeMethod) then
            begin
              nativeResult := nativeMethod(nativeObj^.instance, invokeArgCount,
                stack.StackTop - invokeArgCount);
              if VM.RuntimeErrorStr <> '' then
              begin
                outcome.code := INTERPRET_RUNTIME_ERROR;
                Exit;
              end;
            end
            else if RttiInvokeMethod(nativeObj^.instance, nativeObj^.classInfo,
              invokeMethodName, invokeArgCount,
              stack.StackTop - invokeArgCount,
              VM.MemTracker, nativeResult) then
            begin
              // RTTI method call succeeded
            end
            else
            begin
              if VM.RuntimeErrorStr = '' then
                RunErrNamed('Undefined method', invokeMethodName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Collapse args + receiver to the result in one step (see OP_CALL).
            stack.StackTop[-1 - invokeArgCount] := nativeResult;
            Dec(stack.StackTop, invokeArgCount);
          end
          else
          begin
            runtimeError('Only dictionaries and native objects have methods.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_GET_PROPERTY_LONG: begin
          if isRecord(stack.StackTop[-1]) then
          begin
            rec := pObjRecord(GetObject(stack.StackTop[-1]));
            recNameIdx := ReadLongIndex(ip);
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
              RunErrNamed('Undefined property', fieldName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Replace receiver with the field value in place (no pop/push).
            stack.StackTop[-1] := rec^.fields[fieldIdx];
          end
          else if isNativeObject(stack.StackTop[-1]) then
          begin
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-1]));
            recNameIdx := ReadLongIndex(ip);
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
                RunErrNamed('Undefined property', rttiPropName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Replace receiver with the property value in place (no pop/push).
            stack.StackTop[-1] := rttiResult;
          end
          else if isDictionary(stack.StackTop[-1]) then
          begin
            // Dict dot-read — see OP_GET_PROPERTY for commentary.
            recNameIdx := ReadLongIndex(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_GET_PROPERTY_LONG');
            AssertFrameValues('OP_GET_PROPERTY_LONG');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_GET_PROPERTY_LONG: name constant is not a string');
            if not DictGet(pObjDictionary(GetObject(stack.StackTop[-1])), value, ValueB) then
            begin
              RunErrNamed('Undefined property', ValueToString(value));
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            stack.StackTop[-1] := ValueB;
          end
          else
          begin
            recNameIdx := ReadLongIndex(ip);
            runtimeError('Only records and native objects have properties.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
            exit;
          end;
        end;

        OP_SET_PROPERTY_LONG: begin
          if isRecord(stack.StackTop[-2]) then
          begin
            rec := pObjRecord(GetObject(stack.StackTop[-2]));
            recNameIdx := ReadLongIndex(ip);
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
              RunErrNamed('Undefined property', fieldName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            rec^.fields[fieldIdx] := stack.StackTop[-1];
            // Write the assigned value over the receiver, shrink by one:
            // [recv, value] -> [value] (assignment-expression result).
            stack.StackTop[-2] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else if isNativeObject(stack.StackTop[-2]) then
          begin
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-2]));
            recNameIdx := ReadLongIndex(ip);
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
                RunErrNamed('Undefined or read-only property', rttiPropName);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Write the assigned value over the receiver, shrink by one:
            // [recv, value] -> [value] (assignment-expression result).
            stack.StackTop[-2] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else if isDictionary(stack.StackTop[-2]) then
          begin
            // Dict dot-write — see OP_SET_PROPERTY for commentary.
            recNameIdx := ReadLongIndex(ip);
            {$IFOPT C+}
            AssertConstantIndex(recNameIdx, 'OP_SET_PROPERTY_LONG');
            AssertFrameValues('OP_SET_PROPERTY_LONG');
            {$ENDIF}
            value := constants[recNameIdx];
            Assert(isString(value), 'OP_SET_PROPERTY_LONG: name constant is not a string');
            DictSet(pObjDictionary(GetObject(stack.StackTop[-2])), value,
              stack.StackTop[-1], vm.MemTracker);
            stack.StackTop[-2] := stack.StackTop[-1];
            Dec(stack.StackTop);
          end
          else
          begin
            recNameIdx := ReadLongIndex(ip);
            runtimeError('Only records and native objects have properties.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            i := Trunc(GetNumber(value));
            if (i < 0) or (i >= pObjArray(GetObject(ValueB))^.Count) then
            begin
              RunErrIndexBounds(i, pObjArray(GetObject(ValueB))^.Count);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Two slots were just popped, so this write is in-capacity —
            // no pushStack branch needed.
            stack.StackTop^ := pObjArray(GetObject(ValueB))^.Elements[i];
            Inc(stack.StackTop);
          end
          else if isDictionary(ValueB) then
          begin
            if not DictGet(pObjDictionary(GetObject(ValueB)), value, ValueC) then
            begin
              runtimeError('Key not found in dictionary.');
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            // Two slots were just popped — in-capacity direct write.
            stack.StackTop^ := ValueC;
            Inc(stack.StackTop);
          end
          else if isNativeObject(ValueB) then
          begin
            // Marshal + invoke + exception mapping live in the helper — its
            // try/except and RTTI TValue locals must not be in this routine.
            if not NativeIndexGetChecked(pObjNativeObject(GetObject(ValueB)), value, nativeResult) then
            begin
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            pushStack(stack, nativeResult);
          end
          else
          begin
            runtimeError('Only arrays and dictionaries support subscript access.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            i := Trunc(GetNumber(ValueB));
            if (i < 0) or (i >= pObjArray(GetObject(stack.StackTop[-1]))^.Count) then
            begin
              RunErrIndexBounds(i, pObjArray(GetObject(stack.StackTop[-1]))^.Count);
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            pObjArray(GetObject(stack.StackTop[-1]))^.Elements[i] := value;
            // Overwrite the array slot with the assigned value in place.
            stack.StackTop[-1] := value;
          end
          else if isDictionary(stack.StackTop[-3]) then
          begin
            // Stack: [... dict, key, value]
            // Leave all on stack during DictSet to protect from GC
            value := stack.StackTop[-1];   // value
            ValueB := stack.StackTop[-2];  // key
            DictSet(pObjDictionary(GetObject(stack.StackTop[-3])), ValueB, value, vm.MemTracker);
            // Collapse [dict, key, value] -> [value] in one step.
            stack.StackTop[-3] := value;
            Dec(stack.StackTop, 2);
          end
          else if isNativeObject(stack.StackTop[-3]) then
          begin
            // Stack: [... nativeObj, index, value]
            Dec(stack.StackTop);     // value to assign

            value := stack.StackTop^;
            Dec(stack.StackTop);    // index

            ValueB := stack.StackTop^;
            nativeObj := pObjNativeObject(GetObject(stack.StackTop[-1]));
            // Marshal + invoke + exception mapping live in the helper — its
            // try/except and RTTI TValue locals must not be in this routine.
            if not NativeIndexSetChecked(nativeObj, ValueB, value) then
            begin
              outcome.code := INTERPRET_RUNTIME_ERROR;
              exit;
            end;
            Dec(stack.StackTop); // pop the native object
            pushStack(stack, value); // leave the assigned value
          end
          else
          begin
            runtimeError('Only arrays and dictionaries support subscript assignment.');
            outcome.code := INTERPRET_RUNTIME_ERROR;
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
          RunErrUnknownOpcode(instruction);
          outcome.code := INTERPRET_RUNTIME_ERROR;
          exit;
        end;
      end;

    end;
end;

{$ENDREGION}

{$REGION 'VM Callbacks'}
function InvokeCallback(closure: TValue; const args: array of TValue;
  out returnVal: TValue): TInterpretResult;
var
  closurePtr: pObjClosure;
  i, argCount: Integer;
  {$IFDEF DEBUG_ASSERT_INVARIANTS}
  savedStackDepth: NativeInt;
  savedFrameCount: Integer;
  {$ENDIF}
begin
  Result := Default(TInterpretResult);
  returnVal := CreateNilValue;

  {$IFDEF DEBUG_ASSERT_INVARIANTS}
  // Use depth (offset from Values) rather than raw pointer — realloc-safe.
  savedStackDepth := NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values);
  savedFrameCount := VM.FrameCount;
  {$ENDIF}

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

  {$IFDEF DEBUG_ASSERT_INVARIANTS}
  // Stack balance: after callback, stack depth must match saved depth.
  // Uses offset from Values (not raw pointer) so it's realloc-safe.
  if Result.code = INTERPRET_OK then
  begin
    Assert((NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values)) = savedStackDepth,
      'InvokeCallback: stack not balanced after return (leak or underflow)');
    // Frame balance: must return to original frame count
    Assert(VM.FrameCount = savedFrameCount,
      'InvokeCallback: FrameCount not balanced after return');
  end;
  {$ENDIF}
end;

{$ENDREGION}

{$REGION 'GC Root API'}
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

function IsGCRootRegistered(var slot: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to VM.ExtraRootCount - 1 do
    if VM.ExtraRoots[i] = @slot then
      Exit(True);
  Result := False;
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



{$ENDREGION}

{$REGION 'Public Entry Points'}
//entry point into vm
function CompileAndRun(source : pAnsiChar) : TInterpretResult;
var
  closure : pObjClosure;
begin
    {$IFOPT C+}
   AssertSourceCodeIsAssigned(source);
   AssertVMIsAssigned;
   {$ENDIF}

   // require(): the first user compile marks the builtin boundary — every
   // slot below it was registered by natives/builtins and is shared with
   // module compiles; everything at or above it is user code and is not.
   if VM.BuiltinGlobalCount < 0 then
     VM.BuiltinGlobalCount := VM.GlobalCount;

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
     {$IFDEF DEBUG_ASSERT_INVARIANTS}
     // After successful top-level run, verify no stack leaks.
     // Note: OP_RETURN at baselineFrameCount=0 does Dec(StackTop) to pop
     // the script function, leaving StackTop at Values-1. This is expected.
     // We check for LEAKS (StackTop above Values) which indicate real bugs.
     if Result.code = INTERPRET_OK then
     begin
       Assert(VM.FrameCount = 0,
         'CompileAndRun: FrameCount not 0 after successful run');
       Assert(NativeInt(VM.Stack.StackTop) <= NativeInt(VM.Stack.Values),
         'CompileAndRun: stack leak detected after successful run');
       Assert(VM.OpenUpvalues = nil,
         'CompileAndRun: open upvalues remain after successful run');
     end;
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

{$ENDREGION}

{$REGION 'Native Registration'}
// --- Global slot allocation (Phase 1 dormant; wired in Phase 2) --------------
// Slot table growth policy: capacity doubles from an initial 8. Matches the
// amortised-O(1) growth pattern used by the value stack and constant array.
procedure GrowGlobals; inline;
begin
  if VM.GlobalCapacity = 0 then
    VM.GlobalCapacity := 8
  else
    VM.GlobalCapacity := VM.GlobalCapacity * 2;
  SetLength(VM.GlobalValues, VM.GlobalCapacity);
  SetLength(VM.GlobalMeta,   VM.GlobalCapacity);
end;

// Returns the stable slot index for a global identifier. Idempotent: repeat
// calls for the same interned pObjString return the same slot.
// Callers must ensure `name` is GC-reachable (already on the value stack)
// because TableSet below may trigger a collection.
function resolveGlobalSlot(name : pObjString; isDeclaration : Boolean = false) : Integer;
var
  v : TValue;
begin
  if TableGet(VM.GlobalNameToSlot, name, v) then
  begin
    Result := Trunc(GetNumber(v));
    // require(): inside a module compile, a *declaration* whose name maps to
    // a seeded builtin slot must not overwrite the shared builtin — fall
    // through and give the module its own fresh slot (strict isolation).
    // References (isDeclaration=false) keep resolving to the builtin.
    if not ((VM.CompilingModuleDict <> nil) and isDeclaration and
            (Result < VM.BuiltinGlobalCount)) then
      Exit;
  end;

  if VM.GlobalCount = VM.GlobalCapacity then
    GrowGlobals;

  Result := VM.GlobalCount;
  Inc(VM.GlobalCount);
  VM.GlobalMeta[Result].Name := name;
  VM.GlobalMeta[Result].ModuleDict := VM.CompilingModuleDict;
  VM.GlobalValues[Result]    := UNDEFINED_VAL;
  // Slot index stored as a NaN-boxed double. A million slots is exactly
  // representable, so the float round-trip is lossless.
  TableSet(VM.GlobalNameToSlot, name, CreateNumber(Result), VM.MemTracker);
end;

// Native/builtin registration helper. Publishes `value` under `name` in the
// global slot table. Used by defineNative / InjectNativeObject.
procedure defineGlobalNative(name : pObjString; value : TValue);
var
  slot : Integer;
begin
  slot := resolveGlobalSlot(name);
  VM.GlobalValues[slot] := value;
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
  defineGlobalNative(nameStr, CreateObject(pObj(native)));
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
  // New() only zero-inits managed fields; the cached TRtti* references must
  // be nil'd explicitly or dispatch would follow garbage pointers.
  BuildRttiMemberCache(info);  // rttiEnabled=False -> clears all cache fields
  Inc(NativeClassCount);
  SetLength(NativeClassRegistry, NativeClassCount);
  NativeClassRegistry[NativeClassCount - 1] := info;
end;

// Default destructor for script-owned RTTI instances (see ConstructNativeClass).
procedure DefaultNativeDestructor(instance: Pointer);
begin
  TObject(instance).Free;
end;

procedure registerNativeClassRTTI(const AName : AnsiString; AClass : TClass; ADestructor : TNativeDestructor = nil);
var
  info : pNativeClassInfo;
begin
  if findNativeClass(AName) <> nil then Exit; // already registered
  New(info);
  info^.name := AName;
  SetLength(info^.methods, 0);
  if Assigned(ADestructor) then
    info^.destructor_ := ADestructor
  else
    // Only runs for wrappers with ownsInstance=true (script-constructed);
    // injected and marshaled instances are non-owning and never freed by GC.
    info^.destructor_ := DefaultNativeDestructor;
  info^.rttiEnabled := True;
  info^.rttiClass := AClass;
  // Reflect members ONCE here; dispatch reads the cache only.
  BuildRttiMemberCache(info);
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

function findNativeClassByClass(AClass : TClass) : pNativeClassInfo;
var
  i : integer;
begin
  for i := 0 to NativeClassCount - 1 do
    if NativeClassRegistry[i]^.rttiClass = AClass then
      Exit(NativeClassRegistry[i]);
  Result := nil;
end;

// The [LoxClass('Name')] attribute name, or '' when absent/unnamed.
function LoxClassAttrName(AClass : TClass) : AnsiString;
var
  rt : TRttiType;
  a  : TCustomAttribute;
begin
  Result := '';
  rt := RttiCtx.GetType(AClass);
  if rt = nil then Exit;
  for a in rt.GetAttributes do
    if a is LoxClassAttribute then
      Exit(AnsiString(LoxClassAttribute(a).Name));
end;

function ensureNativeClassRTTI(AClass : TClass) : pNativeClassInfo;
var
  loxName : AnsiString;
begin
  Result := findNativeClassByClass(AClass);
  if Result <> nil then Exit;
  // Prefer the [LoxClass] name; fall back to the Delphi ClassName when the
  // attribute is absent or its name is already taken by a different class.
  loxName := LoxClassAttrName(AClass);
  if (loxName = '') or (findNativeClass(loxName) <> nil) then
    loxName := AnsiString(AClass.ClassName);
  Assert(findNativeClass(loxName) = nil,
    'ensureNativeClassRTTI: name collision for ' + String(loxName));
  registerNativeClassRTTI(loxName, AClass);
  Result := findNativeClass(loxName);
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
  obj := newNativeObject(instance, classInfo, VM.MemTracker, False); // Delphi owns this object
  pushStack(VM.Stack, CreateObject(pObj(obj)));
  defineGlobalNative(nameStr, CreateObject(pObj(obj)));
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);
end;

procedure InjectObject(const name : AnsiString; instance : TObject);
var
  classInfo : pNativeClassInfo;
begin
  Assert(instance <> nil, 'InjectObject: instance is nil');
  classInfo := ensureNativeClassRTTI(instance.ClassType);
  InjectNativeObject(name, Pointer(instance), classInfo^.name);
end;

procedure ExposeNativeClass(AClass : TClass; const AGlobalName : AnsiString = '');
var
  info      : pNativeClassInfo;
  globalName: AnsiString;
  nameStr   : pObjString;
  classObj  : pObjNativeClass;
begin
   {$IFOPT C+}
  AssertVMIsAssigned;
  AssertMemTrackerIsNotNil(VM.MemTracker);
  Assert(AClass <> nil, 'ExposeNativeClass: AClass is nil');
  {$ENDIF}
  info := ensureNativeClassRTTI(AClass);
  if AGlobalName <> '' then
    globalName := AGlobalName
  else
    globalName := info^.name;

  // GC-safe order, same as InjectNativeObject.
  nameStr := CreateString(globalName, VM.MemTracker);
  pushStack(VM.Stack, StringToValue(nameStr));
  classObj := newNativeClass(info, VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(classObj)));
  defineGlobalNative(nameStr, CreateObject(pObj(classObj)));
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);
end;



{$ENDREGION}

{$REGION 'VM Lifecycle'}
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
  VM.GlobalNameToSlot := nil;
  VM.GlobalCount := 0;
  VM.GlobalCapacity := 0;
  VM.BuiltinGlobalCount := -1;
  VM.ModuleCompileSavedMap := nil;
  VM.CompilingModuleDict := nil;
  VM.OpenUpvalues := nil;
  VM.OnPrint := nil;
  VM.RuntimeErrorStr := '';
  VM.PrintBuilder := TStringBuilder.Create;

  RttiCtx := TRttiContext.Create;
  InitMemTracker(VM.MemTracker);
  InitTable(VM.Strings, VM.MemTracker);
  InitTable(VM.GlobalNameToSlot, VM.MemTracker);
  InitStack(VM.Stack,Vm.MemTracker);
  //ResetStack(vm.Stack);
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
  Assert(VM.GlobalNameToSlot <> nil, 'InitVM exit: GlobalNameToSlot table should not be nil');
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
  // Per-script teardown for native modules with state that outlives
  // the VM (e.g. host UI). Runs before we start dismantling anything
  // so a shutdown proc can still safely observe VM state.
  ShutdownAllNatives;
  {$IFDEF DEBUG_LOG_GC}
  try
  {$ENDIF}
  VM.PrintBuilder.Free;
  VM.PrintBuilder := nil;
  FreeStack(VM.Stack,Vm.MemTracker);
  FreeTable(VM.GlobalNameToSlot, VM.MemTracker);
  FreeTable(VM.Strings, VM.MemTracker);
  if (Vm.MemTracker.CreatedObjects <> nil) then
  begin
    FreeObjects(Vm.MemTracker.CreatedObjects);
  end;
  // Closure->event adapters unregister their GC roots in their destructor,
  // so they must go before dispose(VM). Any Delphi event still wired to one
  // of these is now dangling — hosts unhook script handlers before FreeVM.
  FreeLoxCallbackAdapters;
  // All wrappers were swept above. Free the instance map entirely (it is
  // created lazily by newNativeObject, so a later InitVM cycle just makes a
  // fresh one); merely clearing it leaks the dictionary at shutdown.
  FreeAndNil(NativeWrapperMap);
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

{$ENDREGION}

{$REGION 'Garbage Collection'}
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
      // Closure + inline upvalue tail live in one allocation - free them together.
      Allocate(Pointer(obj), InlineClosureByteSize(pObjClosure(obj)^.upvalueCount), 0, vm.MemTracker);
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
      // Keep the instance->wrapper map consistent: this wrapper is dying, so
      // its instance (if still tracked) must not resolve to freed memory.
      if (pObjNativeObject(obj)^.instance <> nil) and (NativeWrapperMap <> nil) then
        NativeWrapperMap.Remove(pObjNativeObject(obj)^.instance);
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
    okNativeClass : begin
      Allocate(Pointer(obj), SizeOf(TObjNativeClass), 0, vm.MemTracker);
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

  // Mark globals table: GlobalNameToSlot marks the name pObjString keys
  // (values are just slot numbers). GlobalValues holds live user values that
  // may reference heap objects; UNDEFINED_VAL is a tagged non-object so
  // MarkValue is a no-op for undefined slots.
  MarkTable(VM.GlobalNameToSlot);
  for i := 0 to VM.GlobalCount - 1 do
  begin
    MarkValue(VM.GlobalValues[i]);
    // require(): module slots outlive the transient per-module compile map
    // that once held their name keys, so names must be marked here. The
    // ModuleDict mark keeps a failed module's dict valid for its orphaned
    // slots even after its cache entry is retired.
    if VM.GlobalMeta[i].Name <> nil then
      MarkObject(pObj(VM.GlobalMeta[i].Name));
    if VM.GlobalMeta[i].ModuleDict <> nil then
      MarkObject(pObj(VM.GlobalMeta[i].ModuleDict));
  end;

  // require(): while a module compile is active, the main name->slot map is
  // parked here and must stay marked (its keys are interned strings).
  if VM.ModuleCompileSavedMap <> nil then
    MarkTable(VM.ModuleCompileSavedMap);
  if VM.CompilingModuleDict <> nil then
    MarkObject(pObj(VM.CompilingModuleDict));

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
        AssertValidObjPointer(pObj(pObjClosure(obj)^.upvalues[i]), 'BlackenObject closure upvalue');
        {$ENDIF}
        MarkObject(pObj(pObjClosure(obj)^.upvalues[i]));
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
    okNativeClass: ;  // classInfo is registry memory, not a GC object
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

{$ENDREGION}

{$REGION 'Scanner'}
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
          else if Match('>') then
            Result := MakeToken(TOKEN_ARROW)
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



{$ENDREGION}

{$REGION 'Compiler'}
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
{$IFOPT C+}
var
  oldCount : integer;
{$ENDIF}
begin
  // ---- Test MemTracker ---------------------------------------------------
   {$IFOPT C+}
  AssertMemTrackerIsNotNil(MemTracker);
  oldCount := Chunk.Count;
  {$ENDIF}

  writeChunk(Chunk,value,line,MemTracker);

  // ---- Exit assertions ----
   {$IFOPT C+}
  Assert(Chunk.Count = oldCount + 1, 'emitByte exit: chunk count should increase by 1');
  {$ENDIF}
end;

procedure emitOpcode(value : byte; Chunk : pChunk; Line : integer; MemTracker : pMemTracker); inline;
begin
  // Record where this opcode lives so peepholes can confirm an opcode
  // boundary later. Slide the previous boundary down one slot.
  prevOpcodeOffset := lastOpcodeOffset;
  lastOpcodeOffset := Chunk.Count;
  emitByte(value, Chunk, Line, MemTracker);
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
  prevOpcodeOffset := lastOpcodeOffset;
  lastOpcodeOffset := Chunk.Count;
  writeChunk(Chunk, OP_NIL, line, Memtracker);
  prevOpcodeOffset := lastOpcodeOffset;
  lastOpcodeOffset := Chunk.Count;
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
    // Table invariant: any token with precedence > PREC_NONE has an infix rule,
    // so reaching this point with a nil infix indicates a Rules[] table bug,
    // not a source-code error.
    {$IFOPT C+}
    Assert(Assigned(infixRule), 'parsePrecedence: token with non-none precedence missing infix rule');
    {$ENDIF}

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
var
  savedScanner       : TScanner;
  savedPrevious      : TToken;
  savedCurrent       : TToken;
  savedHadError      : boolean;
  savedPanicMode     : boolean;
  savedErrorStr      : string;
  isArrow            : boolean;

  function looksLikeArrowParams : boolean;
  begin
    // Empty list: ( ) =>
    if check(TOKEN_RIGHT_PAREN) then
    begin
      AdvanceParser;
      Exit(check(TOKEN_ARROW));
    end;
    // Non-empty list: ident (, ident)* ) =>
    if not check(TOKEN_IDENTIFIER) then Exit(False);
    AdvanceParser;
    while check(TOKEN_COMMA) do
    begin
      AdvanceParser;
      if not check(TOKEN_IDENTIFIER) then Exit(False);
      AdvanceParser;
    end;
    if not check(TOKEN_RIGHT_PAREN) then Exit(False);
    AdvanceParser;
    Result := check(TOKEN_ARROW);
  end;

begin
  // After the prefix dispatcher, parser.previous is '(' and parser.current
  // is the first token inside the parens. Tentatively scan ahead to decide
  // whether this is an arrow parameter list or an ordinary grouped expression.
  savedScanner   := Scanner;
  savedPrevious  := parser.Previous;
  savedCurrent   := parser.Current;
  savedHadError  := parser.HadError;
  savedPanicMode := parser.PanicMode;
  savedErrorStr  := parser.ErrorStr;

  isArrow := looksLikeArrowParams;

  Scanner         := savedScanner;
  parser.Previous := savedPrevious;
  parser.Current  := savedCurrent;
  parser.HadError := savedHadError;
  parser.PanicMode:= savedPanicMode;
  parser.ErrorStr := savedErrorStr;

  if isArrow then
  begin
    arrowFinish(False, parser.Previous);
    Exit;
  end;

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
    TOKEN_MINUS: emitOpcode(OP_NEGATE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_BANG: emitOpcode(OP_NOT,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
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
  line : Integer;
begin
  tokenType := parser.Previous.tokenType;
  rule := getRule(tokenType);
  parsePrecedence(TPrecedence(Ord(Rule.Precedence) + 1));

  // Cache after parsePrecedence: matches existing behaviour of attributing
  // the emitted opcode to the last token of the RHS (standard Lox compiler).
  line := Parser.Previous.Line;

  case tokenType of

      TOKEN_BANG_EQUAL: begin
        emitOpcode(OP_EQUAL,CurrentChunk,line,vm.MemTracker);
        emitOpcode(OP_NOT,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_EQUAL_EQUAL: begin
        emitOpcode(OP_EQUAL,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_GREATER:    begin
        emitOpcode(OP_GREATER,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_GREATER_EQUAL: begin
        emitOpcode(OP_LESS,CurrentChunk,line,vm.MemTracker);
        emitOpcode(OP_NOT,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_LESS: begin
        emitOpcode(OP_LESS,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_LESS_EQUAL: begin
          emitOpcode(OP_GREATER,CurrentChunk,line,vm.MemTracker);
          emitOpcode(OP_NOT,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_PLUS : begin
        emitOpcode(OP_ADD,CurrentChunk,line,vm.MemTracker);
        lastAddOffset := CurrentChunk.count - 1;
      end;

      TOKEN_MINUS : begin
        // Peephole: fuse [GET_LOCAL_N | GET_LOCAL slot] + OP_CONSTANT idx + OP_SUBTRACT
        // into OP_GET_LOCAL_CONST_SUBTRACT. Uses lastOpcodeOffset /
        // prevOpcodeOffset so we KNOW the bytes we're matching are opcode
        // boundaries, not operand bytes that happen to equal an opcode value.
        // NOTE: Only OP_CONSTANT (8-bit operand) is matched. OP_CONSTANT_LONG
        // is intentionally not fused; the optimisation will silently disable
        // once the constant table exceeds 255 entries.
        if (lastOpcodeOffset = CurrentChunk.count - 2) and
           (CurrentChunk.code[lastOpcodeOffset] = OP_CONSTANT) and
           (prevOpcodeOffset >= 0) and
           (prevOpcodeOffset = lastOpcodeOffset - 1) and
           (CurrentChunk.code[prevOpcodeOffset] >= OP_GET_LOCAL_0) and
           (CurrentChunk.code[prevOpcodeOffset] <= OP_GET_LOCAL_7) and
           (lastPatchTarget <> lastOpcodeOffset) and
           (lastPatchTarget <> prevOpcodeOffset) then
        begin
          // Fast-slot variant: slot encoded in the opcode
          slot := CurrentChunk.code[prevOpcodeOffset] - OP_GET_LOCAL_0;
          index := CurrentChunk.code[CurrentChunk.count - 1];
          // Erase: OP_GET_LOCAL_N (1 byte) + OP_CONSTANT + idx (2 bytes) = 3
          Dec(CurrentChunk.count, 3);
          // Erased the previous two opcodes; their boundaries are gone.
          lastOpcodeOffset := -1;
          prevOpcodeOffset := -1;
          emitOpcode(OP_GET_LOCAL_CONST_SUBTRACT, CurrentChunk, line, vm.MemTracker);
          emitByte(slot, CurrentChunk, line, vm.MemTracker);
          emitByte(index, CurrentChunk, line, vm.MemTracker);
        end
        else if (lastOpcodeOffset = CurrentChunk.count - 2) and
                (CurrentChunk.code[lastOpcodeOffset] = OP_CONSTANT) and
                (prevOpcodeOffset >= 0) and
                (prevOpcodeOffset = lastOpcodeOffset - 2) and
                (CurrentChunk.code[prevOpcodeOffset] = OP_GET_LOCAL) and
                (lastPatchTarget <> lastOpcodeOffset) and
                (lastPatchTarget <> prevOpcodeOffset) then
        begin
          // General GET_LOCAL variant: GET_LOCAL, slot, OP_CONSTANT, idx
          slot := CurrentChunk.code[prevOpcodeOffset + 1];
          index := CurrentChunk.code[CurrentChunk.count - 1];
          // Erase: OP_GET_LOCAL + slot (2 bytes) + OP_CONSTANT + idx (2 bytes) = 4
          Dec(CurrentChunk.count, 4);
          lastOpcodeOffset := -1;
          prevOpcodeOffset := -1;
          emitOpcode(OP_GET_LOCAL_CONST_SUBTRACT, CurrentChunk, line, vm.MemTracker);
          emitByte(slot, CurrentChunk, line, vm.MemTracker);
          emitByte(index, CurrentChunk, line, vm.MemTracker);
        end
        else
          emitOpcode(OP_SUBTRACT, CurrentChunk, line, vm.MemTracker);
      end;

      TOKEN_STAR : begin
        emitOpcode(OP_MULTIPLY,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_SLASH : begin
        emitOpcode(OP_DIVIDE,CurrentChunk,line,vm.MemTracker);
      end;

      TOKEN_PERCENT : begin
        emitOpcode(OP_MODULO,CurrentChunk,line,vm.MemTracker);
      end
      else
      begin
        Assert(False, 'binary: unhandled binary operator token');
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
    TOKEN_FALSE : emitOpcode(OP_FALSE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_TRUE  : emitOpcode(OP_TRUE,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
    TOKEN_NIL   : emitOpcode(OP_NIL,CurrentChunk,Parser.Previous.Line,vm.MemTracker);
  end;
end;


procedure printStatement();
begin
  Expression();
  consume(TOKEN_SEMICOLON, 'Expect '';'' after value.');
  emitOpcode(OP_PRINT, CurrentChunk, parser.previous.line, vm.MemTracker);
end;




function emitJump(instruction : byte) : integer; inline;
begin
  // Peephole: fuse OP_LESS + OP_JUMP_IF_FALSE_POP into OP_LESS_JUMP_IF_FALSE.
  // Guards (all required):
  //   - lastOpcodeOffset = count-1 confirms the previous byte is genuinely
  //     an opcode boundary, not an operand byte that happens to equal
  //     OP_LESS (13).
  //   - lastPatchTarget checks that the OP_LESS position is not a jump
  //     landing point (fusing would corrupt the target).
  if (instruction = OP_JUMP_IF_FALSE_POP) and
     (lastOpcodeOffset >= 0) and
     (lastOpcodeOffset = CurrentChunk.count - 1) and
     (CurrentChunk.code[lastOpcodeOffset] = OP_LESS) and
     (lastPatchTarget <> CurrentChunk.count) and
     (lastPatchTarget <> lastOpcodeOffset) then
  begin
    // Overwrite OP_LESS in place with the fused opcode. Its offset is still
    // a valid opcode boundary, so lastOpcodeOffset stays as-is.
    CurrentChunk.code[lastOpcodeOffset] := OP_LESS_JUMP_IF_FALSE;
    emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte($FF, CurrentChunk, parser.previous.line, vm.MemTracker);
    Result := CurrentChunk.count - 2;
    Exit;
  end;
  emitOpcode(instruction, CurrentChunk, parser.previous.line, vm.MemTracker);
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
  // The jump target lands HERE, so whatever opcode the trackers point at
  // is no longer adjacent to the next emitted instruction. Invalidate.
  lastOpcodeOffset := -1;
  prevOpcodeOffset := -1;
end;

procedure emitLoop(loopStart : integer); inline;
var
  offset : integer;
begin
  Assert(loopStart >= 0, 'emitLoop: loopStart is negative');
  emitOpcode(OP_LOOP, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_GET_LOCAL, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_GET_LOCAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(innerVarSlot), CurrentChunk, parser.previous.line, vm.MemTracker);
    emitOpcode(OP_SET_LOCAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(loopVarSlot), CurrentChunk, parser.previous.line, vm.MemTracker);
    emitOpcode(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
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
  emitOpcode(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
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
  emitOpcode(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
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
  lastOpcodeOffset := -1;
  prevOpcodeOffset := -1;
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
        emitOpcode(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
        pendingPops := 0;
      end
      else
      begin
        if pendingPops > 255 then chunkN := 255 else chunkN := pendingPops;
        emitOpcode(OP_POP_N, CurrentChunk, parser.previous.line, vm.MemTracker);
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
      emitOpcode(OP_CLOSE_UPVALUE, CurrentChunk, parser.previous.line, vm.MemTracker);
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
var
  local : pLocal;
begin
  Assert(Current <> nil, 'addLocal: Current compiler is nil');
  if Current^.localCount = UINT8_COUNT then
  begin
    Error('Too many local variables in function.');
    Exit;
  end;
  Assert(Current^.localCount < UINT8_COUNT, 'addLocal: locals index out of bounds');
  local := @Current^.locals[Current^.localCount];
  local^.name := name;
  local^.depth := -1; // sentinel: not yet initialized
  local^.isCaptured := false;
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
  //upvalues     : array[0..UINT8_COUNT - 1] of TUpvalue;
  // Preconditions: validated once up front so the body can trust the invariants
  // (compiler/func non-nil, upvalueCount in [0..UINT8_COUNT]) and skip per-access asserts.
  Assert(compiler <> nil, 'addUpvalue: compiler is nil');
  Assert(compiler^.func <> nil, 'addUpvalue: compiler func is nil');
  upvalueCount := compiler^.func^.upvalueCount;
  Assert((upvalueCount >= 0) and (upvalueCount <= UINT8_COUNT),
    'addUpvalue: upvalueCount out of range [0..UINT8_COUNT]');

  // Search must run BEFORE the capacity check: an already-captured (index, isLocal)
  // pair can be reused even when the array is full, because no growth is needed.
  // Loop bound upvalueCount <= UINT8_COUNT keeps i within array range.
  for i := 0 to upvalueCount - 1 do
    if (compiler^.upvalues[i].index = index) and (compiler^.upvalues[i].isLocal = isLocal) then
      Exit(i);

  // No reuse possible -- only now does fullness matter.
  // Upvalue indices are emitted as a single-byte operand, so UINT8_COUNT is the hard cap.
  // Use >= (not =) so a corrupted count past the cap still errors instead of writing OOB.
  if upvalueCount >= UINT8_COUNT then
  begin
    Error('Too many closure variables in function.');
    Exit(0);
  end;

  // Safe to write: upvalueCount < UINT8_COUNT here by the check above,
  // so upvalues[upvalueCount] is in range and func^.upvalueCount stays <= UINT8_COUNT.
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

// Global-scope equivalent of identifierConstant: interns the identifier and
// returns its stable VM slot index. Used by parseVariable and namedVariable
// when the name resolves to a global rather than a local/upvalue.
function identifierGlobalSlot(const name : TToken; isDeclaration : Boolean = false) : integer;
var
  strObj : pObjString;
begin
  strObj := CreateString(TokenToString(name), VM.MemTracker);
  // GC-protect during resolveGlobalSlot (its TableSet may allocate).
  pushStack(VM.Stack, StringToValue(strObj));
  Result := resolveGlobalSlot(strObj, isDeclaration);
  Dec(VM.Stack.StackTop);
end;

function parseVariable(const errorMsg : PAnsiChar) : integer;
begin
  consume(TOKEN_IDENTIFIER, errorMsg);

  declareVariable();
  if Current^.scopeDepth > 0 then
    Exit(0); // locals don't get stored in constant table

  Result := identifierGlobalSlot(parser.previous, true);
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
    emitOpcode(OP_DEFINE_GLOBAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(global), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitOpcode(OP_DEFINE_GLOBAL_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_NIL, CurrentChunk, parser.previous.line, vm.MemTracker);

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
    // Use emitOpcode (not emitByte) so prevOpcodeOffset/lastOpcodeOffset stay
    // accurate ? otherwise downstream peepholes (e.g. GET_LOCAL_CONST_SUBTRACT)
    // silently fail to match.
    if (arg >= 0) and (arg <= 7) then
    begin
      if canAssign and matchToken(TOKEN_EQUAL) then
      begin
        Expression();
        emitOpcode(Byte(OP_SET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
      end
      else if canAssign and MatchCompoundAssign(compoundOp) then
      begin
        // GET, expression, arithmetic op, SET
        emitOpcode(Byte(OP_GET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
        Expression();
        emitOpcode(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
        emitOpcode(Byte(OP_SET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
      end
      else
        emitOpcode(Byte(OP_GET_LOCAL_0 + arg), CurrentChunk, parser.previous.line, vm.MemTracker);
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
      arg := identifierGlobalSlot(name);
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
    emitOpcode(setOp, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(getOp, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitOpcode(setOp, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(getOp, CurrentChunk, parser.previous.line, vm.MemTracker);
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
  // Single-param arrow with no parens:  x => expr
  if check(TOKEN_ARROW) then
  begin
    arrowFinish(True, parser.previous);
    Exit;
  end;
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
    emitOpcode(OP_ADD_SET_LOCAL_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(slot, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else if (CurrentChunk.count >= 3) and
          (lastAddOffset = CurrentChunk.count - 3) and
          (CurrentChunk.code[CurrentChunk.count - 2] = OP_SET_LOCAL) then
  begin
    slot := CurrentChunk.code[CurrentChunk.count - 1];
    Dec(CurrentChunk.count, 3);
    emitOpcode(OP_ADD_SET_LOCAL_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(slot, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
    emitOpcode(OP_POP, CurrentChunk, parser.previous.line, vm.MemTracker);
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
  emitOpcode(OP_CALL, CurrentChunk, parser.previous.line, vm.MemTracker);
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
      emitOpcode(OP_SET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitOpcode(OP_SET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
  end
  else if canAssign and MatchCompoundAssign(compoundOp) then
  begin
    // obj.field += expr → DUP obj, GET_PROPERTY, expr, op, SET_PROPERTY
    emitOpcode(OP_DUP, CurrentChunk, parser.previous.line, vm.MemTracker);
    if nameIdx <= High(Byte) then
    begin
      emitOpcode(OP_GET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitOpcode(OP_GET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
      IntBytes := IntToBytes(nameIdx);
      emitByte(IntBytes.byte0, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte1, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(IntBytes.byte2, CurrentChunk, parser.previous.line, vm.MemTracker);
    end;
    Expression();
    emitOpcode(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    if nameIdx <= High(Byte) then
    begin
      emitOpcode(OP_SET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitOpcode(OP_SET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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
      emitOpcode(OP_INVOKE, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(argCount, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitOpcode(OP_INVOKE_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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
      emitOpcode(OP_GET_PROPERTY, CurrentChunk, parser.previous.line, vm.MemTracker);
      emitByte(Byte(nameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
    begin
      emitOpcode(OP_GET_PROPERTY_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_DICT_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(0, CurrentChunk, parser.previous.line, vm.MemTracker);
    Exit;
  end;

  // Empty array: []
  if check(TOKEN_RIGHT_BRACKET) then
  begin
    consume(TOKEN_RIGHT_BRACKET, 'Expect '']'' after array elements.');
    emitOpcode(OP_ARRAY_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_DICT_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_ARRAY_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_SET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else if canAssign and MatchCompoundAssign(compoundOp) then
  begin
    // a[key] += expr → DUP2 [obj,key], GET_SUBSCRIPT, expr, op, SET_SUBSCRIPT
    emitOpcode(OP_DUP2, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitOpcode(OP_GET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
    Expression();
    emitOpcode(compoundOp, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitOpcode(OP_SET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    // a[expr]
    emitOpcode(OP_GET_SUBSCRIPT, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_CLOSURE, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(closureIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitOpcode(OP_CLOSURE_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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

procedure arrowFinish(hasBareParam : Boolean; const bareParam : TToken);
// Compiles an arrow function expression starting from one of two states:
//   hasBareParam = True : caller has just consumed a single bare identifier
//                         (parser.previous = bareParam, parser.current = '=>').
//   hasBareParam = False: caller has just consumed '(' (parser.previous = '(',
//                         parser.current = first token after '(').
// Emits OP_CLOSURE for the resulting function so the closure value lands on
// the stack as the expression result, exactly like `lambda`.
var
  compiler     : pCompiler;
  func         : pObjFunction;
  i, j         : integer;
  closureIdx   : integer;
  closureBytes : TIntToByteResult;
begin
  compiler := nil;
  initCompiler(compiler, TYPE_FUNCTION);
  beginScope();

  if hasBareParam then
  begin
    Inc(Current^.func^.arity);
    addLocal(bareParam);
    markInitialized();
  end
  else
  begin
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
    consume(TOKEN_RIGHT_PAREN, 'Expect '')'' after arrow parameters.');
  end;

  consume(TOKEN_ARROW, 'Expect ''=>'' after arrow parameters.');

  if matchToken(TOKEN_LEFT_BRACE) then
    block()
  else
  begin
    // Expression body: evaluate the expression and return it.
    expression;
    emitOpcode(OP_RETURN, CurrentChunk, parser.previous.line, vm.MemTracker);
  end;

  // Trailing implicit nil-return — dead code if the body already returned,
  // but cheap insurance and keeps layout identical to functionBody.
  emitReturn(CurrentChunk, parser.previous.line, VM.MemTracker);
  func := Current^.func;
  Current := Current^.enclosing;

  // Push func onto stack to protect from GC until stored in constants.
  pushStack(VM.Stack, CreateObject(pObj(func)));

  closureIdx := AddValueConstant(CurrentChunk.Constants, CreateObject(pObj(func)), VM.MemTracker);
  if closureIdx <= High(Byte) then
  begin
    emitOpcode(OP_CLOSURE, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(closureIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    emitOpcode(OP_CLOSURE_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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
    emitOpcode(OP_RECORD, CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(fieldCount), CurrentChunk, parser.previous.line, vm.MemTracker);
    for i := 0 to fieldCount - 1 do
      emitByte(Byte(fieldNameIndices[i]), CurrentChunk, parser.previous.line, vm.MemTracker);
    emitByte(Byte(typeNameIdx), CurrentChunk, parser.previous.line, vm.MemTracker);
  end
  else
  begin
    // Long form: 3 bytes per index (24-bit little-endian)
    var intBytes : TIntToByteResult;
    emitOpcode(OP_RECORD_LONG, CurrentChunk, parser.previous.line, vm.MemTracker);
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
      emitOpcode(OP_ADD_RETURN, CurrentChunk, parser.previous.line, vm.MemTracker);
    end
    else
      emitOpcode(OP_RETURN, CurrentChunk, parser.previous.line, vm.MemTracker);
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

// require(): compile a module body with its own private name->slot map so
// its top-level definitions cannot collide with (or leak into) the main
// script's globals. The map is seeded with the builtin entries only, so
// natives (str, arrayLen, require, ...) resolve inside modules. Fresh slots
// allocated during the compile are tagged with moduleDict (see
// resolveGlobalSlot); the DEFINE/SET global opcodes then write those slots'
// values through to the dict, which is the module's live export namespace.
//
// Compiles never nest (a nested require runs at module-body *runtime*, after
// this compile has finished), so a single saved-map field suffices.
function CompileModuleSource(source : pAnsiChar; moduleDict : pObjDictionary) : pObjClosure;
var
  moduleMap : pTable;
  entry : pEntry;
  i : Integer;
  globalSnapshot : Integer;
begin
  Assert(moduleDict <> nil, 'CompileModuleSource: moduleDict is nil');
  Assert(VM.ModuleCompileSavedMap = nil, 'CompileModuleSource: module compile already active');
  Assert(VM.BuiltinGlobalCount >= 0, 'CompileModuleSource: builtin boundary not set (no CompileAndRun yet)');

  moduleMap := nil;
  InitTable(moduleMap, VM.MemTracker);
  // Swap first: MarkRoots marks GlobalNameToSlot (now the module map) and
  // ModuleCompileSavedMap (the main map), so both survive GCs triggered by
  // the TableSet calls below and by the compile itself.
  VM.ModuleCompileSavedMap := VM.GlobalNameToSlot;
  VM.GlobalNameToSlot := moduleMap;
  VM.CompilingModuleDict := moduleDict;
  globalSnapshot := VM.GlobalCount;
  try
    // Seed builtin/native names. Tombstones have key = nil and are skipped.
    for i := 0 to VM.ModuleCompileSavedMap.CurrentCapacity - 1 do
    begin
      entry := @VM.ModuleCompileSavedMap.Entries[i];
      if (entry^.key <> nil) and
         (Trunc(GetNumber(entry^.value)) < VM.BuiltinGlobalCount) then
        TableSet(VM.GlobalNameToSlot, entry^.key, entry^.value, VM.MemTracker);
    end;
    Result := compile(source);
    // Compile failure: roll back any globals allocated during this compile.
    // The abandoned ObjFunction (if any) is unreachable and will be GC'd, so
    // no live bytecode references the reclaimed slot indices. Without this,
    // a persistent compile error would monotonically grow VM.GlobalCount on
    // every require() retry and retain the discarded module dict via
    // GlobalMeta[].ModuleDict (which MarkRoots unconditionally marks).
    if Result = nil then
    begin
      for i := globalSnapshot to VM.GlobalCount - 1 do
      begin
        VM.GlobalMeta[i].Name       := nil;
        VM.GlobalMeta[i].ModuleDict := nil;
        VM.GlobalValues[i]          := UNDEFINED_VAL;
      end;
      VM.GlobalCount := globalSnapshot;
    end;
  finally
    VM.CompilingModuleDict := nil;
    // The module's slots are already burned into its bytecode; the map is
    // compile-time-only state and can go. Slot names stay reachable via the
    // GlobalMeta name-marking walk in MarkRoots.
    FreeTable(VM.GlobalNameToSlot, VM.MemTracker);
    VM.GlobalNameToSlot := VM.ModuleCompileSavedMap;
    VM.ModuleCompileSavedMap := nil;
  end;
end;

{$ENDREGION}

initialization
  VM := nil;
  Current := nil;

finalization

end.

