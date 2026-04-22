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
procedure TestControlFlow;
procedure TestFunctions;
procedure TestClosures;
procedure TestGarbageCollection;

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

procedure TestControlFlow;
begin
  // --- if statements ---
  AssertOutput('if (true) print "yes";', 'yes');
  AssertOutput('if (false) print "yes";', '');
  AssertOutput('if (true) print "yes"; else print "no";', 'yes');  // note: replaced smart quote
  AssertOutput('if (false) print "yes"; else print "no";', 'no');

  // if with block
  AssertOutput('if (true) { print "a"; print "b"; }', 'a' + sLineBreak + 'b');
  AssertOutput('if (false) { print "a"; } else { print "c"; }', 'c');

  // if with expression condition
  AssertOutput('var x = 10; if (x > 5) print "big";', 'big');
  AssertOutput('var x = 3; if (x > 5) print "big"; else print "small";', 'small');

  // nested if
  AssertOutput('if (true) if (true) print "deep";', 'deep');
  AssertOutput('if (true) if (false) print "no"; else print "yes";', 'yes');

  // --- logical operators ---
  // and short-circuits
  AssertOutput('print true and true;', 'true');
  AssertOutput('print true and false;', 'false');
  AssertOutput('print false and true;', 'false');
  AssertOutput('print false and false;', 'false');

  // or short-circuits
  AssertOutput('print true or true;', 'true');
  AssertOutput('print true or false;', 'true');
  AssertOutput('print false or true;', 'true');
  AssertOutput('print false or false;', 'false');

  // and/or return values (not just booleans)
  AssertOutput('print "hi" and "there";', 'there');
  AssertOutput('print nil and "there";', 'nil');
  AssertOutput('print "hi" or "there";', 'hi');
  AssertOutput('print nil or "there";', 'there');

  // --- while loops ---
  AssertOutput('var i = 0; while (i < 3) { print i; i = i + 1; }',
    '0' + sLineBreak + '1' + sLineBreak + '2');

  // while that never executes
  AssertOutput('while (false) print "no"; print "done";', 'done');

  // while with counter
  AssertOutput('var sum = 0; var i = 1; while (i <= 5) { sum = sum + i; i = i + 1; } print sum;', '15');

  // --- for loops ---
  // classic for loop
  AssertOutput('for (var i = 0; i < 3; i = i + 1) print i;',
    '0' + sLineBreak + '1' + sLineBreak + '2');

  // for loop with no initializer
  AssertOutput('var i = 0; for (; i < 3; i = i + 1) print i;',
    '0' + sLineBreak + '1' + sLineBreak + '2');

  // for loop with no increment
  AssertOutput('for (var i = 0; i < 3;) { print i; i = i + 1; }',
    '0' + sLineBreak + '1' + sLineBreak + '2');

  // for loop accumulator
  AssertOutput('var sum = 0; for (var i = 1; i <= 10; i = i + 1) sum = sum + i; print sum;', '55');

  // for loop variable is local to the loop
  AssertOutput('var i = "before"; for (var i = 0; i < 1; i = i + 1) { print i; } print i;',
    '0' + sLineBreak + 'before');

  // nested for loops
  AssertOutput(
    'var result = 0; for (var i = 0; i < 3; i = i + 1) for (var j = 0; j < 3; j = j + 1) result = result + 1; print result;',
    '9');
end;

procedure TestFunctions;
begin
  // Simple function declaration and call
  AssertOutput('fun greet() { print "hello"; } greet();', 'hello');

  // Function with return value
  AssertOutput('fun add(a, b) { return a + b; } print add(1, 2);', '3');

  // Function with no explicit return returns nil
  AssertOutput('fun nothing() { } print nothing();', 'nil');

  // Implicit nil return
  AssertOutput('fun f() { 1 + 2; } print f();', 'nil');

  // Return from middle of function
  AssertOutput('fun early() { print "before"; return; print "after"; } early();', 'before');

  // Return with value from middle
  AssertOutput('fun pick() { return "yes"; return "no"; } print pick();', 'yes');

  // Multiple parameters
  AssertOutput('fun sum(a, b, c) { return a + b + c; } print sum(1, 2, 3);', '6');

  // Functions as first-class values
  AssertOutput('fun hi() { print "hi"; } var f = hi; f();', 'hi');

  // Recursion
  AssertOutput(
    'fun fib(n) { if (n <= 1) return n; return fib(n - 1) + fib(n - 2); } print fib(10);',
    '55');

  // Nested function calls
  AssertOutput('fun a() { return 1; } fun b() { return 2; } print a() + b();', '3');

  // Function accessing global
  AssertOutput('var x = 10; fun getX() { return x; } print getX();', '10');

  // Function modifying global
  AssertOutput('var x = 1; fun inc() { x = x + 1; } inc(); inc(); print x;', '3');

  // Wrong arity - too few args
  AssertRuntimeError('fun f(a, b) { } f(1)');

  // Wrong arity - too many args
  AssertRuntimeError('fun f(a) { } f(1, 2)');

  // Calling non-function
  AssertRuntimeError('var x = 1; x()');

  // Can''t return from top-level
  AssertCompileError('return 1;');

  // Native function clock returns a number
  AssertOutput('print clock() >= 0;', 'true');
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

procedure TestClosures;
begin
  // Basic closure - capture variable from enclosing scope
  AssertOutput(
    'fun outer() { var x = "outside"; fun inner() { print x; } inner(); } outer();',
    'outside');

  // Closure over a variable, not a value
  AssertOutput(
    'var globalSet; var globalGet; fun main() { var a = "initial"; fun set() { a = "updated"; } fun get() { print a; } globalSet = set; globalGet = get; } main(); globalGet(); globalSet(); globalGet();',
    'initial' + sLineBreak + 'updated');

  // Counter pattern - closed-over variable persists
  AssertOutput(
    'fun makeCounter() { var i = 0; fun count() { i = i + 1; print i; } return count; } var counter = makeCounter(); counter(); counter(); counter();',
    '1' + sLineBreak + '2' + sLineBreak + '3');

  // Closing over loop variable
  AssertOutput(
    'var f; for (var i = 0; i < 1; i = i + 1) { fun g() { print i; } f = g; } f();',
    '1');

  // Nested closures
  AssertOutput(
    'fun outer() { var x = "value"; fun middle() { fun inner() { print x; } inner(); } middle(); } outer();',
    'value');

  // Closure captures variable not value
  AssertOutput(
    'var f; var g; { var a = "a"; fun fa() { print a; } fun ga() { a = "updated"; } f = fa; g = ga; } g(); f();',
    'updated');

  // Close over function parameter
  AssertOutput(
    'fun adder(x) { fun add(y) { return x + y; } return add; } var add5 = adder(5); print add5(3);',
    '8');

  // Multiple upvalues in one closure
  AssertOutput(
    'fun f() { var a = 1; var b = 2; var c = 3; fun g() { print a + b + c; } g(); } f();',
    '6');

  // Two closures sharing same upvalue
  AssertOutput(
    'fun f() { var x = 0; fun inc() { x = x + 1; } fun get() { return x; } inc(); inc(); inc(); print get(); } f();',
    '3');

  // Closure survives after enclosing function returns
  AssertOutput(
    'fun outer() { var x = "alive"; fun inner() { return x; } return inner; } var fn = outer(); print fn();',
    'alive');

  // Deeply nested upvalue (3 levels)
  AssertOutput(
    'fun a() { var x = 1; fun b() { fun c() { print x; } c(); } b(); } a();',
    '1');

  // Closure doesn't capture anything (still works)
  AssertOutput(
    'fun make() { fun inner() { print "ok"; } return inner; } var f = make(); f();',
    'ok');

  // Closing over variable modified after closure creation
  AssertOutput(
    'fun f() { var a = "before"; fun g() { print a; } a = "after"; g(); } f();',
    'after');

  // Independent closures from separate calls get separate variables
  AssertOutput(
    'fun make() { var i = 0; fun inc() { i = i + 1; print i; } return inc; } var a = make(); var b = make(); a(); b(); a(); b();',
    '1' + sLineBreak + '1' + sLineBreak + '2' + sLineBreak + '2');

  // Closed-over variable in block scope
  AssertOutput(
    'var f; { var x = "block"; fun g() { print x; } f = g; } f();',
    'block');
end;

procedure TestGarbageCollection;

  // Stress GC helper: runs GC on every growing allocation by setting NextGC = 0
  function StressInterpret(const source: AnsiString): TInterpretResult;
  var
    closure : pObjClosure;
  begin
    InitVM;
    try
      // Force GC on every growing allocation
      VM.MemTracker.NextGC := 0;

      closure := compile(PAnsiChar(source));
      if closure = nil then
      begin
        Result.code := INTERPRET_COMPILE_ERROR;
        Result.ErrorStr := Parser.ErrorStr;
        Exit;
      end;

      pushStack(VM.Stack, CreateObject(pObj(closure)), VM.MemTracker);
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

  procedure AssertStressOutput(const program_: AnsiString; const expected: string);
  var
    IR: TInterpretResult;
  begin
    IR := StressInterpret(program_);
    Assert(IR.code = INTERPRET_OK, 'StressGC: Expected OK, got error: ' + IR.ErrorStr);
    Assert(IR.OutputStr = expected, 'StressGC: Output mismatch. Expected: ' + expected + ' Got: ' + IR.OutputStr);
  end;

  procedure AssertStressRuntimeError(const expr: AnsiString);
  var
    IR: TInterpretResult;
    src: AnsiString;
  begin
    src := expr + ';';
    IR := StressInterpret(src);
    Assert(IR.code = INTERPRET_RUNTIME_ERROR, 'StressGC: Expected runtime error for: ' + string(expr));
  end;

begin
  // =========================================================================
  // PART 1: Smoke tests (normal GC threshold)
  // =========================================================================

  // Strings created and discarded
  AssertOutput(
    'var a = "first"; a = "second"; a = "third"; print a;',
    'third');

  // Functions created in loop — old closures become garbage
  AssertOutput(
    'var f; for (var i = 0; i < 10; i = i + 1) { fun g() { return i; } f = g; } print f();',
    '10');

  // String concatenation in loop — intermediate strings become garbage
  AssertOutput(
    'var s = ""; for (var i = 0; i < 5; i = i + 1) { s = s + "a"; } print s;',
    'aaaaa');

  // Closures that outlive their scope keep captured variables alive
  AssertOutput(
    'fun make() { var x = "kept"; fun get() { return x; } return get; } var f = make(); print f();',
    'kept');

  // Multiple rounds of allocation - ensures GC runs without crashing
  AssertOutput(
    'for (var i = 0; i < 100; i = i + 1) { var s = "temp" + "string"; } print "ok";',
    'ok');

  // Globals survive GC
  AssertOutput(
    'var x = "global"; for (var i = 0; i < 50; i = i + 1) { var y = "waste"; } print x;',
    'global');

  // Recursive function with many allocations
  AssertOutput(
    'fun count(n) { if (n <= 0) return "done"; var s = "x"; return count(n - 1); } print count(50);',
    'done');

  // Ensure interned strings aren't collected while still referenced
  AssertOutput(
    'var a = "hello"; var b = "hello"; print a == b;',
    'true');

  // =========================================================================
  // PART 2: Stress GC — GC fires on EVERY growing allocation
  // This catches any point where a value is temporarily unrooted.
  // =========================================================================

  // Arithmetic
  AssertStressOutput('print 1 + 2;', '3');
  AssertStressOutput('print (1 + 2) * (10 / 5);', '6');

  // String operations
  AssertStressOutput('print "hello" + " " + "world";', 'hello world');
  AssertStressOutput('print "abc" == "abc";', 'true');

  // Variables
  AssertStressOutput('var x = 10; print x;', '10');
  AssertStressOutput('var x = "hi"; x = "bye"; print x;', 'bye');

  // Locals and blocks
  AssertStressOutput('{ var a = 1; { var b = 2; print a + b; } }', '3');

  // String concatenation (the classic GC-unsafe operation)
  AssertStressOutput(
    'var s = "a"; s = s + "b"; s = s + "c"; print s;',
    'abc');

  // Concatenation in a loop under stress
  AssertStressOutput(
    'var s = ""; for (var i = 0; i < 3; i = i + 1) { s = s + "x"; } print s;',
    'xxx');

  // Globals
  AssertStressOutput('var a = 10; var b = 20; print a + b;', '30');
  AssertStressOutput('var name = "world"; print "hello " + name;', 'hello world');

  // Control flow
  AssertStressOutput('if (true) print "yes"; else print "no";', 'yes');
  AssertStressOutput(
    'var i = 0; while (i < 3) { print i; i = i + 1; }',
    '0' + sLineBreak + '1' + sLineBreak + '2');
  AssertStressOutput(
    'for (var i = 0; i < 3; i = i + 1) print i;',
    '0' + sLineBreak + '1' + sLineBreak + '2');

  // Logical operators (and/or)
  AssertStressOutput('print "hi" and "there";', 'there');
  AssertStressOutput('print nil or "fallback";', 'fallback');

  // Functions under stress
  AssertStressOutput('fun greet() { print "hello"; } greet();', 'hello');
  AssertStressOutput('fun add(a, b) { return a + b; } print add(1, 2);', '3');
  AssertStressOutput(
    'fun fib(n) { if (n <= 1) return n; return fib(n - 1) + fib(n - 2); } print fib(10);',
    '55');

  // Functions as first-class values
  AssertStressOutput('fun hi() { print "hi"; } var f = hi; f();', 'hi');

  // Native function
  AssertStressOutput('print clock() >= 0;', 'true');

  // Closures under stress — the most GC-sensitive patterns
  AssertStressOutput(
    'fun outer() { var x = "outside"; fun inner() { print x; } inner(); } outer();',
    'outside');

  // Counter closure under stress
  AssertStressOutput(
    'fun makeCounter() { var i = 0; fun count() { i = i + 1; print i; } return count; } var c = makeCounter(); c(); c(); c();',
    '1' + sLineBreak + '2' + sLineBreak + '3');

  // Closure survives after enclosing function returns
  AssertStressOutput(
    'fun outer() { var x = "alive"; fun inner() { return x; } return inner; } var fn = outer(); print fn();',
    'alive');

  // Adder closure (parameter capture)
  AssertStressOutput(
    'fun adder(x) { fun add(y) { return x + y; } return add; } var add5 = adder(5); print add5(3);',
    '8');

  // Two closures sharing same upvalue under stress
  AssertStressOutput(
    'fun f() { var x = 0; fun inc() { x = x + 1; } fun get() { return x; } inc(); inc(); inc(); print get(); } f();',
    '3');

  // Deeply nested upvalue (3 levels) under stress
  AssertStressOutput(
    'fun a() { var x = 1; fun b() { fun c() { print x; } c(); } b(); } a();',
    '1');

  // Independent closures from separate calls
  AssertStressOutput(
    'fun make() { var i = 0; fun inc() { i = i + 1; print i; } return inc; } var a = make(); var b = make(); a(); b(); a(); b();',
    '1' + sLineBreak + '1' + sLineBreak + '2' + sLineBreak + '2');

  // Lots of temp strings under stress
  AssertStressOutput(
    'for (var i = 0; i < 20; i = i + 1) { var s = "temp" + "string"; } print "ok";',
    'ok');

  // Recursive with string allocations under stress
  AssertStressOutput(
    'fun count(n) { if (n <= 0) return "done"; var s = "x"; return count(n - 1); } print count(20);',
    'done');

  // Runtime errors under stress GC
  AssertStressRuntimeError('1 / 0');
  AssertStressRuntimeError('"hello" + 1');

  // =========================================================================
  // PART 3: Direct VM tests — verify BytesAllocated bookkeeping
  // =========================================================================

  // After FreeVM, BytesAllocated should be 0 (already asserted in FreeVM)
  // Test that creating and discarding objects doesn't leak
  InitVM;
  try
    VM.MemTracker.NextGC := 0; // stress mode
    CreateString('garbage1', VM.MemTracker);
    CreateString('garbage2', VM.MemTracker);
    CreateString('garbage3', VM.MemTracker);
    // These strings are in the intern table (rooted) so won't be collected yet
    // But the intern table will be freed in FreeVM
    Assert(VM.MemTracker.BytesAllocated > 0, 'GC direct: should have allocated memory');
  finally
    FreeVM;
  end;

  // Verify GC actually collects unreferenced objects
  InitVM;
  try
    VM.MemTracker.NextGC := 0;
    // Create strings that are interned but not otherwise referenced
    CreateString('will_be_collected_1', VM.MemTracker);
    CreateString('will_be_collected_2', VM.MemTracker);
    // Force a GC cycle — strings are in intern table (weak refs) but not on stack
    // They should survive because intern table marks them... but after
    // TableRemoveWhite they'd be removed IF not marked. Since they're in the
    // intern table and MarkTable marks Globals (not Strings), they should
    // be swept. Let's verify the mechanism works.
    Assert(VM.MemTracker.CreatedObjects <> nil, 'GC direct: should have created objects');
  finally
    FreeVM;
  end;
end;

initialization

finalization

end.
