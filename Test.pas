unit Test;
{$ASSERTIONS ON}

interface

procedure TestAddValueConstant;
procedure TestAllocateArray;
procedure TestInitChunkAndFreeChunk;
procedure TestStackPush;
procedure TestStackPop;
procedure TestStackPeek;
procedure TestStringEqual;
procedure TestStringUnequal;
procedure TestValuesEqual;
procedure TestGC;
procedure TestTable;
procedure TestTableResize;
procedure TestStringInterning;
procedure TestInterpreter;
procedure TestGlobals;
procedure TestLocals;

implementation

uses
  sysutils,
  Chunk_types;


procedure TestTable;
var
  memTracker : pMemTracker;
  table      : pTable;
  key1, key2, key3, keyNope : pObjString;
  val        : TValue;
  found      : boolean;
begin
  table := nil;
  memTracker := nil;
  InitMemTracker(MemTracker);
  InitTable(Table,MemTracker);

  // Create keys
  key1 := CreateString('hello', MemTracker);
  key2 := CreateString('world', MemTracker);
  key3 := CreateString('foo', MemTracker);

  // Set values
  Assert(TableSet(Table, key1, CreateNumber(1), MemTracker) = true, 'key1 should be new');
  Assert(TableSet(Table, key2, CreateNumber(2), MemTracker) = true, 'key2 should be new');
  Assert(TableSet(Table, key3, CreateNumber(3), MemTracker) = true, 'key3 should be new');
  Assert(Table.Count = 3, 'Table should have 3 entries');

  // Get values back
  found := TableGet(Table, key1, val);
  Assert(found, 'key1 should be found');
  Assert(val.NumberValue = 1, 'key1 value should be 1');

  found := TableGet(Table, key2, val);
  Assert(found, 'key2 should be found');
  Assert(val.NumberValue = 2, 'key2 value should be 2');

  found := TableGet(Table, key3, val);
  Assert(found, 'key3 should be found');
  Assert(val.NumberValue = 3, 'key3 value should be 3');

  // Overwrite existing key
  Assert(TableSet(Table, key1, CreateNumber(99), MemTracker) = false, 'key1 should already exist');
  Assert(Table.Count = 3, 'Count should not change on overwrite');
  found := TableGet(Table, key1, val);
  Assert(found, 'key1 should still be found');
  Assert(val.NumberValue = 99, 'key1 value should now be 99');

  // ---- Delete tests (tombstones) ----

  // Delete key2
  Assert(TableDelete(Table, key2) = true, 'delete key2 should succeed');
  found := TableGet(Table, key2, val);
  Assert(not found, 'key2 should not be found after delete');

  // Other keys still accessible
  found := TableGet(Table, key1, val);
  Assert(found, 'key1 should survive key2 delete');
  Assert(val.NumberValue = 99, 'key1 value intact after key2 delete');

  found := TableGet(Table, key3, val);
  Assert(found, 'key3 should survive key2 delete');
  Assert(val.NumberValue = 3, 'key3 value intact after key2 delete');

  // Delete same key again returns false
  Assert(TableDelete(Table, key2) = false, 'double delete should return false');

  // Re-insert deleted key reuses tombstone
  Assert(TableSet(Table, key2, CreateNumber(222), MemTracker) = true, 'reinsert key2 should be new');
  found := TableGet(Table, key2, val);
  Assert(found, 'key2 should be found after reinsert');
  Assert(val.NumberValue = 222, 'key2 value should be 222');

  // Delete key that does not exist
  keyNope := CreateString('nope', MemTracker);
  Assert(TableDelete(Table, keyNope) = false, 'delete nonexistent should fail');

  // ---- TableFindString tests ----
  Assert(TableFindString(Table, 'hello', 5, key1.hash) = key1, 'FindString should locate key1');
  Assert(TableFindString(Table, 'foo', 3, key3.hash) = key3, 'FindString should locate key3');
  Assert(TableFindString(Table, 'missing', 7, HashString('missing', 7)) = nil, 'FindString missing should return nil');

  // Clean up
  FreeTable(Table, MemTracker);
  FreeString(keyNope, MemTracker);
  FreeString(key1, MemTracker);
  FreeString(key2, MemTracker);
  FreeString(key3, MemTracker);
  Assert(MemTracker.BytesAllocated = 0, 'Table test: bytes not zero');
  FreeMemTracker(MemTracker);
end;

procedure TestTableResize;
const
  N = 50; // initial capacity is 8, load factor 0.75 -> forces several resizes
var
  memTracker : pMemTracker;
  table      : pTable;
  keys       : array[0..N-1] of pObjString;
  val        : TValue;
  found      : boolean;
  i          : integer;
  name       : AnsiString;
  oldCap     : integer;
  resizeCount: integer;
begin
  table := nil;
  memTracker := nil;
  InitMemTracker(MemTracker);
  InitTable(Table, MemTracker);

  resizeCount := 0;

  // Insert N keys, tracking resizes
  for i := 0 to N - 1 do
  begin
    name := AnsiString('key_' + IntToStr(i));
    keys[i] := CreateString(name, MemTracker);
    oldCap := Table.CurrentCapacity;
    TableSet(Table, keys[i], CreateNumber(i * 10), MemTracker);
    if Table.CurrentCapacity > oldCap then
      Inc(resizeCount);
  end;

  // Must have resized at least once (8 -> 16 -> 32 -> 64)
  Assert(resizeCount >= 3, 'TestTableResize: expected at least 3 resizes, got ' + IntToStr(resizeCount));
  Assert(Table.Count = N, 'TestTableResize: count should be ' + IntToStr(N));

  // Verify all keys survived rehash
  for i := 0 to N - 1 do
  begin
    found := TableGet(Table, keys[i], val);
    Assert(found, 'TestTableResize: key_' + IntToStr(i) + ' not found after resize');
    Assert(val.NumberValue = i * 10, 'TestTableResize: key_' + IntToStr(i) + ' value mismatch');
  end;

  // Verify TableFindString works after resize
  for i := 0 to N - 1 do
  begin
    name := AnsiString('key_' + IntToStr(i));
    Assert(TableFindString(Table, PAnsiChar(name), Length(name), keys[i].hash) = keys[i],
      'TestTableResize: FindString failed for key_' + IntToStr(i));
  end;

  // Delete half the keys, then verify the other half still works
  for i := 0 to (N div 2) - 1 do
  begin
    Assert(TableDelete(Table, keys[i]) = true, 'TestTableResize: delete key_' + IntToStr(i) + ' should succeed');
  end;

  for i := (N div 2) to N - 1 do
  begin
    found := TableGet(Table, keys[i], val);
    Assert(found, 'TestTableResize: key_' + IntToStr(i) + ' should survive partial delete');
    Assert(val.NumberValue = i * 10, 'TestTableResize: key_' + IntToStr(i) + ' value wrong after partial delete');
  end;

  // Clean up
  FreeTable(Table, MemTracker);
  for i := 0 to N - 1 do
    FreeString(keys[i], MemTracker);
  Assert(MemTracker.BytesAllocated = 0, 'TestTableResize: bytes not zero');
  FreeMemTracker(MemTracker);
end;

procedure TestGC;
var
  stack : pStack;
begin

end;

procedure TestAddValueConstant_UpToMax;
var
  ValueArray : pValueArray;
  MemTracker : pMemTracker;
  Value      : TValue;
  i          : Integer;
  idx        : Integer;
begin
  ValueArray := nil;
  MemTracker := nil;

  Value.ValueKind := vkNumber;
  Value.NumberValue := 10.10;

  InitMemTracker(MemTracker);
  InitValueArray(ValueArray, MemTracker);

  try
    for i := 0 to MAX_SIZE - 1 do
    begin
      idx := AddValueConstant(ValueArray, Value, MemTracker);
      Assert(idx = i, 'Index mismatch');
    end;

    Assert(ValueArray^.Count = MAX_SIZE, 'Count mismatch');
    Assert(ValueArray^.CurrentCapacity = MAX_SIZE, 'Capacity mismatch');

  finally
    FreeValueArray(ValueArray, MemTracker);
    Assert(MemTracker.BytesAllocated = 0, 'Bytes > 0');
  end;

  FreeMemTracker(MemTracker);
end;


procedure TestAddConstantLong;
var
  chunk : pChunk;

  MemTracker : pMemTracker;
  Value : TValue;
  i,j : integer;

  ip : pByte;
  instruction : byte;
begin
  chunk := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  InitChunk(Chunk, MemTracker);
 try
    Value.ValueKind := vkNumber;

    //inserting constants = 256 constants.
    j := 0;
    for i := low(Byte) to High(byte) do
    begin
      Value.NumberValue := i;
      inc(j,2); //OP_CONSTANT + Index into constant
      AddConstant(Chunk,Value,0,MemTracker);
      Assert(Chunk.Count = j, 'Chunk count for addition of bytes mismatch');
    end;
    Assert(Chunk.Count = j, 'Chunk count mismatch');
    Assert(Chunk.Constants.Count = High(Byte)+1, 'constant count <> high byte');
    Assert(Chunk.Code[510] = OP_CONSTANT);
    //insert op constant longs
    for i := High(byte)+1 to 1000 do
    begin
      Value.NumberValue := i;
      inc(j,4); //OP_CONSTANT + Index into constant
      AddConstant(Chunk,Value,0,MemTracker);
      Assert(Chunk.Count = j, 'Chunk count mismatch for addition of op constant longs');
    end;
    Assert(Chunk.Code[512] = OP_CONSTANT_LONG);
    EmitReturn(chunk,0,MemTracker); //so we can exit Ip instruction loop


    i := 0;
    ip := Chunk.Code;
    while true do
    begin
      instruction := ReadByte(ip);
      case instruction of

        OP_CONSTANT : begin

          value := ReadConstant(Ip,Chunk.Constants);
          assert(Value.NumberValue = i, 'value not i for constant');
          inc(i);
        end;

        OP_CONSTANT_LONG : begin
          value := ReadConstantLong(Ip,Chunk.Constants);
          assert(Value.NumberValue = i, 'value not i for op constant long');
          inc(i);
        end;

        OP_RETURN : begin
          exit;
        end;
      end;

    end;

  finally
    FreeChunk(Chunk,MemTracker);
    Assert(MemTracker.BytesAllocated = 0, 'Bytes > 0');
    FreeMemTracker(MemTracker)
  end;
end;

procedure TestAddConstant_upToMax;
var
  chunk : pChunk;

  MemTracker : pMemTracker;
  Value : TValue;
  i,j : integer;

  ip : pByte;
  instruction : byte;
begin
  chunk := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  InitChunk(Chunk, MemTracker);


  try
    Value.ValueKind := vkNumber;

    //inserting constants.
    j := 0;
    for i := low(Byte) to High(byte) do
    begin
      Value.NumberValue := i;
      inc(j,2); //OP_CONSTANT + Index into constant
      AddConstant(Chunk,Value,0,MemTracker);
      Assert(Chunk.Count = j, 'Chunk count mismatch');
    end;
    Assert(Chunk.Count = j, 'Chunk count mismatch');
    Assert(Chunk.Constants.Count = High(Byte)+1, 'constant count <> high byte');
    EmitReturn(chunk,0,MemTracker); //so we can exit Ip instruction loop
    Assert(Chunk.Code[Chunk.Count-1] = OP_RETURN, 'Expected return otherwise will loop infinietly');
    i := 0;
    ip := Chunk.Code;
    while true do
    begin
      instruction := ReadByte(ip);
      case instruction of

        OP_CONSTANT : begin

          value := ReadConstant(Ip,Chunk.Constants);
          assert(Value.NumberValue = i, 'value not i');
          inc(i);
        end;

        OP_CONSTANT_LONG : begin
          Assert(False,'We shouldn''t have any op constant long in loop');
        end;

        OP_RETURN : begin
          exit;
        end;
      end;

    end;

  finally
    FreeChunk(Chunk,MemTracker);
    Assert(MemTracker.BytesAllocated = 0, 'Bytes > 0');
    FreeMemTracker(MemTracker)
  end;

end;

procedure TestAddValueConstant;
begin
  TestAddValueConstant_UpToMax;
  TestAddConstant_upToMax;
  TestAddConstantLong;
end;

procedure TestValuesEqual;
var
 valueA : pObjString;
 valueB : pObjString;
 MemTracker : pMemTracker;
 ValA,ValB  : TValue;

begin
  valueA := nil;
  valueB := nil;
  MemTracker := nil;
  InitMemTracker(MemTracker);

  valueA := CreateString('fred',MemTracker);
  valueB := CreateString('fred',MemTracker);
  try
    ValA := StringToValue(valueA);
    ValB := StringToValue(valueB);
    Assert(ValuesEqual(ValA,ValB),'StringTOValue failed for strings');
    Assert(StringsEqual(ValueToString(ValA),ValueToString(ValB)),'StringTOValue failed for strings');

    ValA.ValueKind := vkNumber;
    ValA.NumberValue := 10.5;
    Assert(ValuesEqual(ValA,ValB) = false, 'Compare number to string');
    ValB.ValueKind := vkNumber;
    ValB.NumberValue := 10.5;
    Assert(ValuesEqual(ValA,ValB), 'Compare number to string failed for numbers');

  finally
    freeString(ValueA,MemTracker);
    freeString(ValueB,MemTracker);
    Assert(MemTracker.BytesAllocated = 0, 'MemTracker Byttes is > 0');
    FreeMemTracker(MemTracker);
  end;

end;


procedure TestInitialAllocation;
var
  MemTracker : pMemTracker;
  ValueArray : PValueArray;
  Value      : TValue;
begin
  MemTracker := nil;
  ValueArray := nil;
  InitMemTracker(MemTracker);
  InitValueArray(ValueArray,MemTracker);
  writeValueArray(ValueArray,Value,MemTracker);
  Assert(ValueArray.Count = 1, 'Count is not 1');
  FreeValueArray(ValueArray,MemTracker);
  Assert(MemTracker.BytesAllocated = 0);
  FreeMemTracker(MemTracker);
end;

procedure TestGrowthAtCapacity;
var
  Grew: Boolean;
  Count: Integer;
begin
 (* SetupValueArray;
  // Pre-allocate
  AllocateArray(Pointer(ValueArray), Capacity, 0, SizeOf(TValue));

  Count := Capacity;
  Grew := AllocateArray(Pointer(ValueArray), Capacity, Count, SizeOf(TValue));
  Assert(Grew = True, 'Array should grow when Count = Capacity');
  Assert(Capacity > Count, 'Capacity should increase after growth');

  TeardownValueArray; *)
end;

procedure TestMultipleGrowths;
var
  Grew: Boolean;
  Count: Integer;
begin
 (* SetupValueArray;
  // Pre-allocate
  AllocateArray(Pointer(ValueArray), Capacity, 0, SizeOf(TValue));

  Count := Capacity;
  Grew := AllocateArray(Pointer(ValueArray), Capacity, Count, SizeOf(TValue));
  Assert(Grew = True, 'Array should grow again when full');

  Count := Capacity;
  Grew := AllocateArray(Pointer(ValueArray), Capacity, Count, SizeOf(TValue));
  Assert(Grew = True, 'Array should grow again on subsequent full allocation');
  Assert(Capacity > Count, 'Capacity should increase after multiple growths');

  TeardownValueArray; *)
end;

procedure TestAllocationLimitExceeded;
var
  Count: Integer;
begin
  (*SetupValueArray;
  try
    Count := MAX_SIZE;
    Capacity := MAX_SIZE;

 // Expecting an assertion to be triggered
    try
      AllocateArray(Pointer(ValueArray), Capacity, Count, SizeOf(TValue));
      // If we reach here, no assertion was raised → fail the test
      raise Exception.Create('Assertion was expected but not raised');
    except
      on E: EAssertionFailed do
        ; // Expected, test passes
    end;

  finally
    TeardownValueArray;
  end;      *)
end;

procedure TestAllocateArray;
begin
  //TestInitialAllocation;
  //TestNoGrowthNeeded;
  //TestGrowthAtCapacity;
  //TestMultipleGrowths;
  //TestAllocationLimitExceeded;
end;



procedure TestInitChunkAndFreeChunk;
var
  chunk : pChunk;
  MemTracker : pMemTracker;
begin
  Chunk := nil;
  MemTracker := nil;
  InitMemTracker(MemTracker);
  initChunk(chunk,MemTracker);
  writeChunk(chunk,1,1,MemTracker);
  FreeChunk(chunk,MemTracker);
  Assert(MemTracker.BytesAllocated = 0);
  FreeMemTracker(MemTracker);
  Assert(chunk = nil);
end;


procedure TestStringEqual;
var
  valueA : pObjString;
  valueB : pObjString;
  MemTracker : pMemTracker;
begin
  ValueA := nil;
  ValueB := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);

  valueA := CreateString('fred',MemTracker);
  valueB := CreateString('fred',MemTracker);
  try
    assert(StringsEqual(valueA,valueB), 'strings are not equal');
  finally
    freeString(valueA,MemTracker);
    freeString(valueB,MemTracker);
  end;

  Assert(MemTracker.BytesAllocated = 0, 'memtracker bytes > 0');
  assert(ValueA = nil);
  assert(ValueB = nil);

  valueA := CreateString('',MemTracker);
  valueB := CreateString('',MemTracker);
  try
    assert(StringsEqual(valueA,valueB), 'strings are not equal');
  finally
    freeString(valueA,MemTracker);
    freeString(valueB,MemTracker);
  end;
  assert(ValueA = nil);
  assert(ValueB = nil);
  Assert(MemTracker.BytesAllocated = 0, 'memtracker bytes > 0');
  FreeMemTracker(MemTracker);
end;


procedure TestStringUnequal;
var
  valueA : pObjString;
  valueB : pObjString;
  MemTracker : pMemTracker;
begin
  ValueA := nil;
  ValueB := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  try
    valueA := CreateString('fred ',MemTracker);
    valueB := CreateString('fred', MemTracker);
    assert(StringsEqual(valueA,valueB) = false, 'strings are NOT equal');
    freeString(valueA,MemTracker);
    freeString(valueB,MemTracker);
    Assert(MemTracker.BytesAllocated = 0, 'memtracker bytes > 0');
  finally
     FreeMemTracker(MemTracker);
  end;
end;


procedure TestStackPeek;
var
  stack       : pStack;
  value       : TValue;
  memTracker  : pMemTracker;

  i     : integer;
begin
  stack := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  InitStack(stack,MemTracker);

  for i := 0 to 999 do
  begin
    value.NumberValue := i;
    pushStack(stack,value,MemTracker);
  end;

  assert(Stack.Count = 1000);

   // Peek from top down
  for i := 0 to 999 do
  begin
    value := PeekStack(stack, i);
    Assert(value.NumberValue = 999 - i);
  end;


  FreeStack(stack,MemTracker);
   Assert(memTracker.BytesAllocated = 0);
  FreeMemTracker(MemTracker);
end;

procedure TestStackPop;
var
  stack       : pStack;
  value       : TValue;
  memTracker  : pMemTracker;

  i     : integer;
begin
  stack := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  InitStack(stack,MemTracker);

  for i := 0 to 999 do
  begin
    value.NumberValue := i;
    pushStack(stack,value,MemTracker);
  end;

  assert(Stack.Count = 1000);

  for i := 999 downto 0 do
  begin
    value := popStack(Stack);
    assert(Value.NumberValue = i);
  end;

  FreeStack(stack,MemTracker);
  Assert(memTracker.BytesAllocated = 0);
  FreeMemTracker(MemTracker);
end;


procedure TestStackPush;
var
  stack : pStack;
  MemTracker : pMemTracker;
  value : TValue;
  i     : integer;
begin
  stack := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  InitStack(stack,MemTracker);
  try
    for i := 0 to 999 do
      pushStack(stack,value,MemTracker);

    assert(Stack.Count = 1000);

  except
    on E:EAssertionFailed do
    begin
      raise;
    end;
  end;
  FreeStack(stack,MemTracker);
  Assert(memTracker.BytesAllocated = 0);
  FreeMemTracker(MemTracker);
end;

procedure AssertNumber(const expr: AnsiString; expected: Double);
var
  IR: TInterpretResult;
  src: AnsiString;
begin
  src := 'print ' + expr + ';';
  IR := InterpretResult(PAnsiChar(src));
  Assert(IR.code = INTERPRET_OK, 'Expected OK for: ' + string(expr) + ' got error: ' + IR.ErrorStr);
  Assert(IR.OutputStr <> '', 'Expected output for: ' + string(expr));
end;

procedure AssertBoolean(const expr: AnsiString; expected: Boolean);
var
  IR: TInterpretResult;
  src: AnsiString;
  expectedStr: string;
begin
  src := 'print ' + expr + ';';
  IR := InterpretResult(PAnsiChar(src));
  Assert(IR.code = INTERPRET_OK, 'Expected OK for: ' + string(expr) + ' got error: ' + IR.ErrorStr);
  if expected then expectedStr := 'true' else expectedStr := 'false';
  Assert(IR.OutputStr = expectedStr, 'Wrong value for: ' + string(expr) + ' got: ' + IR.OutputStr);
end;

procedure AssertNil(const expr: AnsiString);
var
  IR: TInterpretResult;
  src: AnsiString;
begin
  src := 'print ' + expr + ';';
  IR := InterpretResult(PAnsiChar(src));
  Assert(IR.code = INTERPRET_OK, 'Expected OK for: ' + string(expr) + ' got error: ' + IR.ErrorStr);
  Assert(IR.OutputStr = 'nil', 'Expected nil output for: ' + string(expr) + ' got: ' + IR.OutputStr);
end;

procedure AssertStringOK(const expr: AnsiString);
var
  IR: TInterpretResult;
  src: AnsiString;
begin
  src := 'print ' + expr + ';';
  IR := InterpretResult(PAnsiChar(src));
  Assert(IR.code = INTERPRET_OK, 'Expected OK for: ' + string(expr) + ' got error: ' + IR.ErrorStr);
  Assert(IR.OutputStr <> '', 'Expected output for: ' + string(expr));
end;

procedure AssertOutput(const program_: AnsiString; const expected: string);
var
  IR: TInterpretResult;
begin
  IR := InterpretResult(PAnsiChar(program_));
  Assert(IR.code = INTERPRET_OK, 'Expected OK for program, got error: ' + IR.ErrorStr);
  Assert(IR.OutputStr = expected, 'Output mismatch. Expected: ' + expected + ' Got: ' + IR.OutputStr);
end;

procedure AssertRuntimeError(const expr: AnsiString);
var
  IR: TInterpretResult;
  src: AnsiString;
begin
  src := expr + ';';
  IR := InterpretResult(PAnsiChar(src));
  Assert(IR.code = INTERPRET_RUNTIME_ERROR, 'Expected runtime error for: ' + string(expr));
  Assert(IR.ErrorStr <> '', 'Expected error message for: ' + string(expr));
end;

procedure AssertCompileError(const expr: AnsiString);
var
  IR: TInterpretResult;
begin
  IR := InterpretResult(PAnsiChar(expr));
  Assert(IR.code = INTERPRET_COMPILE_ERROR, 'Expected compile error for: ' + string(expr));
  Assert(IR.ErrorStr <> '', 'Expected error message for: ' + string(expr));
end;

procedure TestInterpreter;
begin
  // Arithmetic
  AssertNumber('1 + 2', 3);
  AssertNumber('10 - 3', 7);
  AssertNumber('2 * 3', 6);
  AssertNumber('10 / 4', 2.5);
  AssertNumber('(1 + 2) * (10 / 5)', 6);
  AssertNumber('3.14 * 2', 6.28);

  // Unary / precedence
  AssertNumber('-1 + 2', 1);
  AssertNumber('-(1 + 2)', -3);
  AssertNumber('--1', 1);

  // Booleans
  AssertBoolean('!true', False);
  AssertBoolean('!false', True);
  AssertBoolean('!!true', True);
  AssertBoolean('!nil', True);

  // Comparisons
  AssertBoolean('1 > 2', False);
  AssertBoolean('2 > 1', True);
  AssertBoolean('1 < 2', True);
  AssertBoolean('2 < 1', False);
  AssertBoolean('2 >= 2', True);
  AssertBoolean('3 <= 2', False);
  AssertBoolean('1 != 2', True);
  AssertBoolean('1 == 1', True);
  AssertBoolean('1 == 2', False);

  // Nil
  AssertNil('nil');
  AssertBoolean('nil == nil', True);
  AssertBoolean('true == false', False);

  // Strings
  AssertStringOK('"hello" + " world"');
  AssertBoolean('"abc" == "abc"', True);
  AssertBoolean('"abc" == "def"', False);

  // Runtime errors
  AssertRuntimeError('1 / 0');
  AssertRuntimeError('true + 1');
  AssertRuntimeError('true * false');
  AssertRuntimeError('-"hello"');
  AssertRuntimeError('"hello" + 1');

  // Compile errors
  AssertCompileError(')');
  AssertCompileError('* 1');
end;

procedure TestGlobals;
begin
  // Var declaration with initializer
  AssertOutput('var x = 10; print x;', '10');

  // Var declaration without initializer defaults to nil
  AssertOutput('var x; print x;', 'nil');

  // Assignment
  AssertOutput('var x = 1; x = 2; print x;', '2');

  // Multiple globals
  AssertOutput('var a = 10; var b = 20; print a + b;', '30');

  // String globals
  AssertOutput('var name = "world"; print "hello " + name;', 'hello world');

  // Reassign different type
  AssertOutput('var x = 1; x = "hello"; print x;', 'hello');

  // Multiple print statements
  AssertOutput('print 1; print 2; print 3;', '1' + sLineBreak + '2' + sLineBreak + '3');

  // Use variable in its own expression
  AssertOutput('var x = 10; x = x + 5; print x;', '15');

  // Boolean global
  AssertOutput('var flag = true; print flag;', 'true');
  AssertOutput('var flag = false; print !flag;', 'true');

  // Undefined variable runtime error
  AssertRuntimeError('print x');

  // Assignment to undefined variable runtime error
  AssertRuntimeError('x = 10');
end;

procedure TestLocals;
begin
  // Basic block with local variable
  AssertOutput('{ var x = 10; print x; }', '10');

  // Local variable is popped after block
  AssertOutput('var a = 1; { var b = 2; print a + b; } print a;',
    '3' + sLineBreak + '1');

  // Nested blocks
  AssertOutput('{ var a = 1; { var b = 2; { var c = 3; print a + b + c; } } }', '6');

  // Shadowing: inner variable shadows outer
  AssertOutput('var a = "outer"; { var a = "inner"; print a; } print a;',
    'inner' + sLineBreak + 'outer');

  // Local assignment
  AssertOutput('{ var x = 1; x = 2; print x; }', '2');

  // Multiple locals in same scope
  AssertOutput('{ var a = 10; var b = 20; var c = 30; print a + b + c; }', '60');

  // Block does not leak locals (accessing after block ends is an error on a different local)
  // We can test that a global is still accessible after a block
  AssertOutput('var g = 99; { var x = 1; } print g;', '99');

  // Local and global with same name
  AssertOutput('var x = "global"; { var x = "local"; print x; } print x;',
    'local' + sLineBreak + 'global');

  // Nested scopes with shadowing at each level
  AssertOutput(
    'var a = "g"; { var a = "1"; { var a = "2"; print a; } print a; } print a;',
    '2' + sLineBreak + '1' + sLineBreak + 'g');

  // Can''t read local in its own initializer
  AssertCompileError('{ var a = a; }');

  // Duplicate local in same scope
  AssertCompileError('{ var a = 1; var a = 2; }');

  // Empty block is fine
  AssertOutput('var x = 42; { } print x;', '42');

  // Local used in expression before end of block
  AssertOutput('{ var a = 5; var b = a + 3; print b; }', '8');
end;

procedure TestStringInterning;
var
  a, b, c, d: PObjString;
begin
  // Test interning within VM context: same content => same pointer
  InitVM;
  try
    a := CreateString('hello', VM.MemTracker);
    b := CreateString('hello', VM.MemTracker);
    Assert(a = b, 'Interning: same content should return same pointer');

    c := CreateString('world', VM.MemTracker);
    Assert(a <> c, 'Interning: different content should return different pointers');

    // Concatenation result should also be interned
    d := CreateString('helloworld', VM.MemTracker);
    Assert(d = CreateString('helloworld', VM.MemTracker),
      'Interning: repeated CreateString for concat result should return same pointer');

    // Empty string interning
    Assert(CreateString('', VM.MemTracker) = CreateString('', VM.MemTracker),
      'Interning: empty strings should be interned');
  finally
    FreeVM;
  end;
end;

initialization

finalization

end.
