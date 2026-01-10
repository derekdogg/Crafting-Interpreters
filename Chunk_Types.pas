unit Chunk_Types;
{$POINTERMATH ON}
interface
uses
  Classes;


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

  OP_STRINGS : array[0..10] of string = (
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
    'OP_NOT');


type

  pChunk = ^TChunk;
  pValueArray = ^TValueArray;

  pCode = ^TCode;
  TCode = byte;

  pLine = ^TLine;
  TLine = integer;

  TChunk = record
    Count       : Integer;
    Capacity    : Integer;
    Code        : pCode;
    Constants   : pValueArray;
    Lines       : pLine;
    Initialised : boolean;
  end;

  pValue = ^TValue;
  TValueKind = (vkNumber, vkBoolean, vkNull);

  TValue = record
    ValueKind: TValueKind;
    case TValueKind of
      vkNumber:  (NumberValue: Double);
      vkBoolean: (BooleanValue: Boolean);
      vkNull:    (NullValue: Byte);
  end;


  TValueArray = record
    Count     : Integer;
    Capacity  : Integer;
    Values    : pValue;
  end;

  pStackRecord = ^StackRecord;
  StackRecord = record
    Count     : Integer;
    Capacity  : Integer;
    Values    : pValue;
    StackTop  : pValue;
  end;

  //Virtual Machine result
  TInterpretResult = record
    result : (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);
    value : TValue;
  end;

  pVirtualMachine = ^VirtualMachine;
  //Virtual Machine
  VirtualMachine = record
    Chunk : pChunk;
    ip    : pCode;
    Stack : pStackRecord;

  end;

  TBinaryOperation = (boAdd, boSubtract, boMultiply, boDivide);


   TScanner = record
     start    : pchar;
     current  : pchar;
     line     : integer;
   end;



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

   TToken = record
      tokenType : TTokenType;
      start : pchar;
      length : integer;
      line : integer;
   end;

   TParser = record
      Current   : TToken;
      Previous  : TToken;
      HadError  : boolean;
      PanicMode : boolean;
   end;


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

    TParseFn = procedure;

    TParseRule = record
      prefix : TParseFn;
      infix  : TParseFn;
      precedence : TPrecedence;
    end;





procedure initChunk(var chunk: pChunk);
procedure freeChunk(var chunk: pChunk);
procedure writeChunk(chunk: pChunk; value: byte; Line : Tline);
procedure AddConstant(chunk : pChunk; const value : TValue; Line : TLine);
procedure printChunk(chunk: pChunk;  strings: TStrings);
procedure initValueArray(var ValueArray : pValueArray);
procedure writeValueArray(ValueArray : pValueArray; Value : TValue);
procedure freeValueArray(var ValueArray : pValueArray);
procedure printValueArray(ValueArray: pValueArray; strings: TStrings);
procedure InitVM();
procedure InitStack(var Stack : pStackRecord);
procedure FreeStack(var Stack : pStackRecord);
procedure ResetStack(var stack : pStackRecord);
procedure pushStack(var stack : pStackRecord;const value : TValue);
function peekStack(stack : pStackRecord) : TValue; overload;
function peekStack(stack : pStackRecord; distanceFromTop : integer) : TValue;overload;
function  popStack(var stack : pStackRecord) : TValue;
procedure BinaryOp(Op: TBinaryOperation);
function InterpretResult(source : pChar) : TInterpretResult;
procedure FreeVM();
procedure InitScanner(source : pchar);
function  advance : char;
function  isAtEnd : boolean;
function compile(source : pChar; chunk : pChunk) : boolean;
procedure Number();
procedure grouping();
procedure unary();
procedure binary();
procedure literal();

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
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_EQUAL_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_GREATER }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_GREATER_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_LESS }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_LESS_EQUAL }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_IDENTIFIER }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_STRING }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

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
    (Prefix: literal;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_VAR }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_WHILE }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_ERROR }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE),

    { TOKEN_EOF }
    (Prefix: nil;      Infix: nil;     Precedence: PREC_NONE)
  );



var
  VM : pVirtualMachine;
  Scanner : TScanner;
  Parser  : TParser;
  CompilingChunk : pChunk;
  Output : TStrings;

implementation

uses
  sysutils, strUtils, typinfo;


function AS_BOOL(const Value: TValue): Boolean; inline;
begin
  Result := Value.BooleanValue;
end;

function AS_NUMBER(const Value: TValue): Double; inline;
begin
  Result := Value.NumberValue;
end;

function NUMBER_VAL(Value: Double): TValue; inline;
begin
  Result.ValueKind := vkNumber;
  Result.NumberValue := Value;
end;

function BOOL_VAL(Value: Boolean): TValue; inline;
begin
  Result.ValueKind := vkBoolean;
  Result.BooleanValue := Value;
end;

function NIL_VAL: TValue; inline;
begin
  Result.ValueKind := vkNull;
  Result.NullValue := 0;
end;

function IS_NUMBER(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkNumber;
end;

function IS_BOOL(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkBoolean;
end;

function IS_NIL(const Value: TValue): Boolean; inline;
begin
  Result := Value.ValueKind = vkNull;
end;

function IsFalsey(const Value: TValue): Boolean; inline;
begin
  Result :=
    (Value.ValueKind = vkNull) or
    ((Value.ValueKind = vkBoolean) and (not Value.BooleanValue));
end;



function GrowArray(var list: Pointer; var Capacity: Integer; Count: Integer; ElemSize: Integer) : boolean;
begin
  if Capacity = 0 then
  begin
    Capacity := START_CAPACITY;
    ReallocMem(list, Capacity * ElemSize);
    Exit(True);
  end;

  if Count < Capacity then Exit(False);

  Capacity := Capacity * GROWTH_FACTOR;

  Assert(Capacity <= MAX_SIZE, 'Max size for array growth has been reached.');

  ReallocMem(list, Capacity * ElemSize);

  Result := true;
end;


procedure initChunk(var chunk: pChunk);
begin
  assert(Chunk = nil,'Chunk is assigned');
  new(chunk);
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
    FreeMem(chunk.Code, Chunk.Capacity * SizeOf(Byte));
    chunk.Code := nil;

    FreeMem(Chunk.Lines,Chunk.Capacity * Sizeof(TLine));
  end;

  freeValueArray(chunk.Constants);


  Dispose(chunk);
  chunk := nil;
end;

procedure writeChunk(chunk: pChunk; value: byte; Line : Tline);
var
  currentCap : integer;
begin
  assert(assigned(chunk),'Chunk is not assigned');
  assert(chunk.Initialised = true, 'Chunk is not initialised');

  currentCap := Chunk.Capacity;
  if GrowArray(Pointer(chunk.Code),  Chunk.Capacity, Chunk.Count, sizeof(TCode)) then
  begin
    //we have to do it like this because we can't pass Chunk.Capacity to grow the line array (since it will be altered)
    GrowArray(Pointer(chunk.Lines), currentCap, Chunk.Count, sizeof(TLine));
  end;

  chunk.Code[chunk.Count] := value;
  chunk.Lines[chunk.count] := Line;
  Inc(chunk.Count);
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


procedure AddConstant(chunk : pChunk; const value : TValue; Line : TLine);
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




function InstructionSize(op: Byte): Integer;
begin
  case op of
    Ord(OP_CONSTANT): Result := 1 + SizeOf(Integer); // opcode + int32 index
    else
      Result := 1; // all other instructions are 1 byte
  end;
end;


procedure printChunk(chunk: pChunk; strings: TStrings);
const
  Chunk_Header = 'Chunk starts at address $%p';
  OPCODE_FIELD_WIDTH = 30;  // fixed width for opcode column
var
  i, idx: Integer;
  codePtr: pCode;
  valuePtr : pValue;
  codeName: string;
begin
  Assert(Assigned(chunk), 'Chunk is not assigned');
  Assert(Assigned(chunk.Constants),'Constants is not assigned');
  Assert(chunk.Initialised, 'Chunk is not initialised');
  Assert(Assigned(strings), 'Output strings is not assigned');
  strings.Clear;
  strings.Add(Format(Chunk_Header, [Pointer(chunk)]));

  codePtr := Chunk.Code;
  valuePtr := Chunk.Constants.Values;
  i := 0;
  while i < chunk.Count do
  begin

    inc(codePtr,i);

    if (codePtr^ >= Low(OP_STRINGS)) and (codePtr^ <= High(OP_STRINGS)) then
      codeName := OP_STRINGS[codePtr^]
    else
      codeName := 'UNKNOWN';

    // Print opcode
    codeName := codeName + StringOfChar(' ', OPCODE_FIELD_WIDTH - Length(codeName));

    if codePtr^ = Ord(OP_CONSTANT) then
    begin
      inc(CodePtr); //read next Byte (index into constants)
      inc(valuePtr,CodePtr^); //go to the correct index in value array
      strings.Add(Format('%.6d  %s  %4d  0x%.2X  -> %g',
                  [i, codeName, CodePtr^, CodePtr^, valuePtr^.numberValue]));
    end
    else
      strings.Add(Format('%.6d  %s  %4d  0x%.2X', [i, codeName, CodePtr^, CodePtr^]));

    i := i + InstructionSize(CodePtr^); // move to the next instruction
    end;


end;


procedure initValueArray(var ValueArray : pValueArray);
begin
  assert(ValueArray = nil,'values is not assigned');

  new(ValueArray);

  ValueArray.Count := 0;

  ValueArray.Capacity := 0;

  ValueArray.Values := nil;
end;


procedure writeValueArray(ValueArray : pValueArray; Value : TValue);
begin
  assert(assigned(ValueArray),'ValueArray is not assigned');

  GrowArray(pointer(ValueArray.Values), ValueArray.Capacity, ValueArray.Count,sizeof(TValue));

  ValueArray.Values[ValueArray.Count] := value;

  Inc(ValueArray.Count);
end;

procedure freeValueArray(var ValueArray : pValueArray);
begin
  assert(assigned(ValueArray),'ValueArray is not assigned');


  if (ValueArray.Capacity) > 0 then
  begin
    FreeMem(ValueArray.Values, ValueArray.Capacity * SizeOf(TValue));
    ValueArray.Values := nil;
  end;

  Dispose(ValueArray);
  ValueArray := nil;

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

procedure InitVM;
begin
  new(VM);
  VM.Chunk := nil;
  VM.Stack := nil;
  InitStack(VM.Stack);
  ResetStack(vm.Stack);  
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
  value : TValue;
  InterpretResult : TInterpretResult;
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

          if not is_Number(peekStack(vm.stack)) then
          begin
             InterpretResult.result := INTERPRET_RUNTIME_ERROR;
             runtimeError('Operand must be a number.');
             result := InterpretResult;
          end;

          value := popStack(vm.Stack);
          value.NumberValue := -Value.NumberValue;

          PushStack(vm.Stack,value);

        end;

        OP_NIL      : pushStack(vm.stack, NIL_VAL);
        OP_TRUE     : pushStack(vm.Stack, BOOL_VAL(true));
        OP_FALSE    : pushStack(vm.stack, BOOL_VAL(false));
        OP_ADD      : BinaryOp(boAdd);
        OP_SUBTRACT : BinaryOp(boSubtract);
        OP_MULTIPLY : BinaryOP(boMultiply);
        OP_DIVIDE   : BinaryOp(boDivide);
        OP_NOT      : pushStack(vm.Stack,BOOL_VAL(isFalsey(popStack(vm.Stack))));

        OP_RETURN: begin
           value := popStack(vm.Stack); //pop the last thing on stack and exit -- this unsafely assumes something is on the stack at this point..


          //TODO output.Add(floattostr(value.));

           InterpretResult.result := INTERPRET_OK;
           InterpretResult.value := value;
           Exit(InterpretResult);
        end;
      end;
    end;

end;

procedure InitStack(var Stack : pStackRecord);
begin
  new(Stack);
  Stack.Count := 0;
  Stack.Capacity := 0;
  GrowArray(pointer(Stack.Values),Stack.Capacity,Stack.Count,Sizeof(TValue));
  Stack.StackTop := Stack.Values;
end;

procedure FreeStack(var Stack : pStackRecord);
begin
  assert(Assigned(Stack),'Stack is not assigned - free stack');
  assert(Assigned(Stack.Values), 'Stack values is not assigned - free stack');
  if (stack.Capacity > 0) then
  begin
    FreeMem(Stack.Values,Stack.Capacity * Sizeof(TValue));
    Stack.Values := nil;
  end;

  Dispose(Stack);
  Stack := nil;
end;


procedure ResetStack(var stack : pStackRecord);
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
  Assert(Assigned(Stack.values), 'Stack values is not assigned');
  Stack.StackTop := Stack.Values;
end;

procedure pushStack(var stack : pStackRecord;const value : TValue);
begin
  Assert(Assigned(Stack), 'Stack is not assigned');
  Assert(Assigned(Stack.values), 'Stack values is not assigned');
  if GrowArray(pointer(Stack.Values),Stack.Capacity,Stack.Count,Sizeof(TValue)) then
  begin
    ResetStack(stack);
    Stack.StackTop := Stack.Values + Stack.Count;  //move stack top to next pointer available (at count)
  end;
  Stack.StackTop^ := Value;
  Inc(Stack.StackTop);
  inc(Stack.Count);
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
  if (not Is_Number(PeekStack(vm.stack,0))) or
     (not Is_Number(PeekStack(vm.stack,1))) then
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

  B := As_Number(PopStack(vm.Stack));
  A := As_Number(PopStack(vm.stack));

  PushStack(vm.stack,Number_Val(A + B));
  Result := true;
end;

function BinaryOpNumber_Subtract: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := As_Number(PopStack(vm.Stack));
  A := As_Number(PopStack(vm.stack));

  PushStack(vm.stack,Number_Val(A - B));
  Result := true;
end;

function BinaryOpNumber_Multiply: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := As_Number(PopStack(vm.Stack));
  A := As_Number(PopStack(vm.stack));

  PushStack(vm.stack,Number_Val(A * B));
  Result := true;
end;

function BinaryOpNumber_Divide: boolean;
var
  A, B: Double;
begin
  if not CheckBinaryNumbers then
    Exit(false);

  B := As_Number(PopStack(vm.Stack));
  A := As_Number(PopStack(vm.stack));

  PushStack(vm.stack,Number_Val(A / B));
  Result := true;
end;


procedure BinaryOp(Op: TBinaryOperation);
begin

  case Op of
    boAdd       : BinaryOpNumber_Add;
    boSubtract  : BinaryOpNumber_subtract;
    boMultiply  : BinaryOpNumber_Multiply;
    boDivide    : BinaryOpNumber_Divide;
  else
    raise Exception.Create('Unknown operator');
  end;
end;

//entry point into vm
function InterpretResult(source : pChar) : TInterpretResult;
var
  Chunk : pChunk;
  vmResult : TInterpretResult;
begin
   Assert(Assigned(output),'strings is not assigned');
   Assert(Assigned(VM),'VM is not assigned');

   chunk := nil;
   try
     InitChunk(Chunk);

     if not compile(source,chunk) then
     begin
       FreeChunk(chunk);
       vmResult.result :=  INTERPRET_COMPILE_ERROR;
       Exit(vmResult);
     end;

     vm.chunk := chunk;
     vm.ip := vm.chunk.Code;
     Result := Run;
   finally
    freeChunk(chunk);
   end;

end;

procedure FreeVM;
begin
  FreeStack(VM.Stack);
  dispose(VM); 
end;

procedure InitScanner(source : pchar);
begin
  scanner.start := source;
  scanner.current := source;
  scanner.line := 1;
end;


function advance : char;
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


function ErrorToken(msg : pChar) : TToken;
begin

  result.Tokentype := TOKEN_ERROR;
  result.start := msg;
  result.length := strlen(msg);
  result.line := scanner.line;
end;




function match(expected : char) : boolean;
begin
  if isAtEnd then exit(false);

  if scanner.current^ <> expected then exit(false);

  inc(scanner.current);

  result := true;
end;

function peek : char;
begin
  result := scanner.current^;
end;

function PeekNext: Char;
begin
  if IsAtEnd then
    Exit(#0);

  Result := (scanner.current + 1)^;
end;

procedure SkipWhitespace;
var
  c: Char;
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


function isDigit(c : char) : boolean;
begin
  Result := (c >= '0') and (c <= '9');
end;

(*
static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
} *)


function isAlpha(c : char) : boolean;
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


function CheckKeyword(start, length: Integer; const rest: PChar;
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
  c : char;
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


procedure DumpTokens;
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
    SetString(lexeme, token.Start, token.Length);

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



procedure ErrorAt(const Token: TToken; const Msg: PChar);
var
  s: string;
begin

  if (parser.panicMode) then Exit;

  // "[line %d] Error"
  s := Format('[line %d] Error', [Token.Line]);

  if Token.TokenType = TOKEN_EOF then
  begin
    s := s + ' at end';
  end
  else if Token.TokenType = TOKEN_ERROR then
  begin
    // Nothing.
  end
  else
  begin
    // " at '%.*s'"
    s := s + ' at ''' +
         Copy(string(Token.Start), 1, Token.Length) +
         '''';
  end;

  // ": %s\n"
  s := s + ': ' + string(Msg);

  Output.Add(s);

  Parser.HadError := True;
end;

procedure Error(const Msg: PChar);
begin
  parser.panicMode := true;
  ErrorAt(Parser.Previous, Msg);
end;


procedure errorAtCurrent(const msg : pchar);
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

  ErrorAtCurrent(PChar(Msg));
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
  if not(assigned(prefixRule)) then
  begin
    error('Expect expression.');
    exit;
  end;
  prefixRule();

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
  Value: TValue;
  numStr: string;
begin
  SetString(numStr, parser.previous.start, parser.previous.length);
  Value := NUMBER_VAL(StrToFloat(numStr));
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



procedure binary();
var
  tokenType : TTokenType;
  rule : TParseRule;
begin
  tokenType := parser.Previous.tokenType;
  rule := getRule(tokenType);
  parsePrecedence(TPrecedence(Ord(Rule.Precedence) + 1));

  case tokenType of
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

function compile(source : pChar; chunk : pChunk) : boolean;
var
  line : integer;
  token : TToken;
begin
   assert(assigned(chunk), 'Chunk is not assigned');
   assert(assigned(source), 'Source code is not assigned');
   assert(assigned(output), 'Output strings is not assigned');
   output.clear;
   initScanner(source);
   compilingChunk := chunk;
   //dumpTokens(output);


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
  Output := TStringList.Create; //not ideal - but ok for now, move here for simplicity

finalization
  FreeVM;
  Output.free;

end.
