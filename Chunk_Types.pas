unit Chunk_Types;

interface
uses
  classes;


const
  MAX_SIZE = 5000-1;
  START_CAPACITY = 8;
  GROWTH_FACTOR = 2;
  STACK_MAX  = 256;
type

  pValue = ^TValue;
  TValue = Double;

  PByteArray = ^TByteArray;

  TByteArray = array[0..0] of byte;

  OpCode = (
    OP_CONSTANT,
    OP_NEGATE,
    OP_RETURN
  );

  pChunk = ^Chunk;
  pValueRecord = ^ValueRecord;

  Chunk = record
    Count       : Integer;
    Capacity    : Integer;
    Code        : PByteArray;  //there is a fundamental flaw here in that the index into the constants can be greater > 256 max byte per slot into values idx...   We leave for simplicity for now. Can split into two later...
    Constants   : pValueRecord;
    Initialised : boolean;     // uint8_t* code
  end;

  pValues = ^TValues;

  TValues = array[0..0] of TValue;


  ValueRecord = record
    Count     : Integer;
    Capacity  : Integer;
    Values    : pValues;
  end;

  //Virtual Machine result
  TInterpretResult = (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);


  pVirtualMachine = ^VirtualMachine;
  //Virtual Machine
  VirtualMachine = record
    Chunk : pChunk;
    Stack : Array[0..STACK_MAX-1] of TValue;
    StackTop : pValue;
    ip    : integer;
  end;

var
  VM : pVirtualMachine;



procedure initChunk(var chunk: pChunk);
procedure freeChunk(var chunk: pChunk);
procedure writeChunk(chunk: pChunk; value: byte); overload;
procedure writeChunk(chunk: pChunk; value: OpCode); overload;
procedure AddConstant(chunk : pChunk; const value : TValue);
procedure printChunk(chunk: pChunk;  strings: TStrings);
procedure initValueRecord(var valueRecord : pValueRecord);
procedure writeValueRecord(valueRecord : pValueRecord; Value : TValue);
procedure freeValueRecord(var ValueRecord : pValueRecord);
procedure printValueRecord(valueRecord: pValueRecord; strings: TStrings);
procedure InitVM();
procedure ResetStack;
procedure PushValue(const value : TValue);
function PopValue : TValue;
function InterpretResult(chunk : pChunk; const output : TStrings) : TInterpretResult;
procedure FreeVM();



implementation
uses
 sysutils, typinfo;



procedure initChunk(var chunk: pChunk);
begin
  assert(Chunk = nil,'Chunk is assigned');
  new(chunk);
  chunk.Count := 0;
  chunk.Capacity := 0;
  chunk.Code := nil;
  InitValueRecord(chunk.Constants);
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
  end;

  freeValueRecord(chunk.Constants);


  Dispose(chunk);
  chunk := nil;
end;

procedure GrowArray(var list: Pointer; var Capacity: Integer; Count: Integer; ElemSize: Integer);
begin
  if Capacity = 0 then
  begin
    Capacity := START_CAPACITY;
    ReallocMem(list, Capacity * ElemSize);
    Exit;
  end;

  if Count < Capacity then Exit;

  Capacity := Capacity * GROWTH_FACTOR;
  Assert(Capacity <= MAX_SIZE);

  ReallocMem(list, Capacity * ElemSize);
end;


procedure writeChunk(chunk: pChunk; value: OpCode);
begin
  writeChunk(chunk,ord(value));
end;

procedure writeChunk(chunk: pChunk; value: byte);
begin
  assert(assigned(chunk),'Chunk is not assigned');

  assert(chunk.Initialised = true, 'Chunk is not initialised');

  GrowArray(pointer(chunk.Code), Chunk.Capacity, Chunk.Count, sizeof(byte));

  {$R-}
  chunk.Code[chunk.Count] := value;
  {$R+}

  Inc(chunk.Count);
end;


function AddValueConstant(valueRecord: pValueRecord; const value: TValue): Integer;
begin
  Assert(Assigned(valueRecord), 'ValueRecord is not assigned');

  // Add the value to the ValueRecord
  writeValueRecord(valueRecord, value);

  // Return the index of the newly added value
  Result := valueRecord.Count - 1;
end;

procedure AddConstant(chunk : pChunk; const value : TValue);
var
  idx : byte;
begin

  Assert(Assigned(chunk), 'Chunk is not assigned');
  Assert(Assigned(chunk.Constants), 'ValueRecord is not assigned');

  //add constant, 1st into value's array of the value record
  idx := AddValueConstant(chunk.Constants,12.12);
  //add constant op code into the chunk array
  writeChunk(Chunk,OP_CONSTANT);
  //followed by the index of the value inserted into the value array
  writeChunk(Chunk,idx);
end;




function InstructionSize(op: Byte): Integer;
begin
  case op of
    Ord(OP_CONSTANT): Result := 2; // opcode + 1-byte operand (index)
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
  b: byte;
  codeName: string;
begin
  Assert(Assigned(chunk), 'Chunk is not assigned');
  Assert(Assigned(chunk.Constants),'Constants is not assigned');
  Assert(chunk.Initialised, 'Chunk is not initialised');
  Assert(Assigned(strings), 'Output strings is not assigned');
  strings.Clear;
  strings.Add(Format(Chunk_Header, [Pointer(chunk)]));

  {$R-}
  try
    i := 0;
    while i < chunk.Count do
    begin
      b := chunk.Code^[i];

      if (b >= Ord(Low(OpCode))) and (b <= Ord(High(OpCode))) then
        codeName := GetEnumName(TypeInfo(OpCode), b)
      else
        codeName := 'UNKNOWN';

      // Print opcode
      codeName := codeName + StringOfChar(' ', OPCODE_FIELD_WIDTH - Length(codeName));

      if b = Ord(OP_CONSTANT) then
      begin
        idx := chunk.Code^[i+1]; // operand
        strings.Add(Format('%.6d  %s  %4d  0x%.2X  -> %g',
                    [i, codeName, b, b, Chunk.Constants.Values^[idx]]));
      end
      else
        strings.Add(Format('%.6d  %s  %4d  0x%.2X', [i, codeName, b, b]));

      i := i + InstructionSize(b); // move to the next instruction
    end;

  finally
    {$R+}
  end;
end;






procedure initValueRecord(var valueRecord : pValueRecord);
begin
  assert(valueRecord = nil,'values is not assigned');
  new(valueRecord);
  valueRecord.Count := 0;
  valueRecord.Capacity := 0;
  valueRecord.Values := nil;

end;


procedure writeValueRecord(valueRecord : pValueRecord; Value : TValue);
begin
  assert(assigned(valueRecord),'valueRecord is not assigned');

  GrowArray(pointer(valueRecord.Values), valueRecord.Capacity, valueRecord.Count,sizeof(TValue));

  {$R-}
  valueRecord.Values[valueRecord.Count] := value;
  {$R+}

  Inc(valueRecord.Count);
end;

procedure freeValueRecord(var ValueRecord : pValueRecord);
begin
  assert(assigned(ValueRecord),'ValueRecord is not assigned');


  if (ValueRecord.Capacity) > 0 then
  begin
    FreeMem(ValueRecord.Values, ValueRecord.Capacity * SizeOf(TValue));
    ValueRecord.Values := nil;
  end;

  Dispose(ValueRecord);
  ValueRecord := nil;

end;

procedure printValueRecord(valueRecord: pValueRecord; strings: TStrings);
const
  VR_Header = 'ValueRecord starts at address $%p';
  VALUE_FIELD_WIDTH = 50; // fixed width for the value column
var
  i: Integer;
  valStr: string;
  val: TValue;
begin
  Assert(Assigned(valueRecord), 'ValueRecord is not assigned');
  Assert(Assigned(strings), 'Output strings is not assigned');

  strings.Clear;
  strings.Add(Format(VR_Header, [Pointer(valueRecord)]));

  {$R-}
  try
    for i := 0 to valueRecord.Count - 1 do
    begin
      val := valueRecord.Values^[i];

      // format value as general format with up to 12 significant digits
      valStr := Format('%.12g', [val]);

      // pad to fixed width
      valStr := valStr + StringOfChar(' ', VALUE_FIELD_WIDTH - Length(valStr));

      // Index + fixed-width value
      strings.Add(Format('%.6d  %s', [i, valStr]));
    end;
  finally
    {$R+}
  end;
end;

procedure InitVM;
begin
  new(VM);
  VM.Chunk := nil;
  ResetStack;
  //initChunk(VM.Chunk); we don't do this since this will be assigned when we 'InterpretResult'
end;


function Run(const output : TStrings) : TInterpretResult;

  function ReadByte: Byte; inline;
  begin
    Assert(vm.ip < vm.chunk.Count, 'VM is beyond chunk count');
    {$R-}
    Result := vm.chunk.Code[vm.ip];
    {$R+}

    Inc(vm.ip);
  end;

  function ReadValue : TValue; inline;
  begin
    result := vm.Chunk.Constants.Values[ReadByte];
  end;

var
  instruction: Byte;
  idx : byte;
  value : TValue;
begin

    Assert(Assigned(VM),'VM is not assigned');
    Assert(Assigned(VM.Chunk),'VM Chunk is not assigned');
    Assert(Assigned(VM.Chunk.Code),'VM chunk code is not assigned');

    while True do
    begin
      instruction := ReadByte();
      case OpCode(instruction) of

        OP_CONSTANT : begin
          value := ReadValue;
          pushValue(value);
        end;

        OP_NEGATE : begin

          value := -popValue;

          pushValue(value);
        end;



        OP_RETURN: begin
           value := popValue; //pop the last thing on stack and exit -- this unsafely assumes something is on the stack at this point..
           output.Add(floattostr(value));
           Exit(INTERPRET_OK);
        end;
      end;
    end;

end;

procedure ResetStack;
begin
  Assert(Assigned(VM),'VM is not assigned');
  VM.StackTop := @VM.Stack[0];
end;

procedure PushValue(const value : TValue);
var
  Limit: pValue;

begin
  Limit := @VM.Stack[0];
  Inc(Limit, STACK_MAX); //note here we are beyond the actual range ot the stack (we are not going to dereference it though)

  Assert(NativeUInt(VM.StackTop) < NativeUInt(Limit), 'Stack overflow');

  vm.StackTop^ := Value;
  Inc(vm.StackTop);
end;

function PopValue : TValue;
begin
   Dec(Vm.StackTop);
   result := Vm.StackTop^;
end;


//entry point into vm
function InterpretResult(chunk : pChunk; const output : TStrings) : TInterpretResult;
begin
  Assert(Assigned(output),'strings is not assigned');
  Assert(Assigned(Chunk),'Chunk is not assigned');
  Assert(Assigned(VM),'VM is not assigned');

  output.clear;

  vm.chunk := chunk;
  vm.ip := 0;  //instead of using a pointer here (since we're using indexed array pointers) we just use an integer.
  (*We initialize ip by pointing it at the first byte of code in the chunk. We
   haven’t executed that instruction yet, so ip points to the instruction about
  to be executed.*)
  Result := Run(output);
end;

procedure FreeVM;
begin
  dispose(VM);
end;

initialization
  InitVM;

finalization
  FreeVM;

end.

