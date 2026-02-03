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


implementation

uses
  sysutils,
  Chunk_types;


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
    Assert(ValueArray^.Capacity = MAX_SIZE, 'Capacity mismatch');

  finally
    FreeValueArray(ValueArray, MemTracker);
    Assert(MemTracker.BytesAllocated = 0, 'Bytes > 0');
  end;

  FreeMemTracker(MemTracker);
end;


procedure TestAddConstnat_upToMax;
var
  chunk : pChunk;
  MemTracker : pMemTracker;
  Value,constant : TValue;
  i,j : integer;

begin
  chunk := nil;
  MemTracker := nil;

  InitMemTracker(MemTracker);
  InitChunk(Chunk, MemTracker);
  Value.ValueKind := vkNumber;

  j := 0;
  for i := 0 to 9999 do
  begin
    Value.NumberValue := i;

    if i <= High(Byte) then
    begin
      inc(j,2); //OP_CONSTANT + Index into constant
      AddConstant(Chunk,Value,0,MemTracker);
      Assert(Chunk.Count = j, 'Chunk count <> i');
      constant := ReadConstant(chunk.Code,chunk.Constants);
      Assert(Constant.ValueKind = vkNumber, 'Constant returned is not a number');
      Assert(Constant.NumberValue = i, 'Constant value returned is not i');
    end
    else
    begin
      inc(j,4); //OP_CONSTANT_Long + Index into constant (3 bytes for the int)
      AddConstant(Chunk,Value,0,MemTracker);
      Assert(Chunk.Count = j, 'Chunk count <> i');
      Constant := ReadLongConstant(chunk.Code,chunk.Constants);
      Assert(Constant.ValueKind = vkNumber, 'Constant returned is not a number');
      Assert(Constant.NumberValue = i, 'Constant value returned is not i');

    end;


  end;

  FreeChunk(Chunk,MemTracker);
  Assert(MemTracker.BytesAllocated = 0, 'Bytes > 0');
  FreeMemTracker(MemTracker)

end;

procedure TestAddValueConstant;
begin
  TestAddValueConstant_UpToMax;
  TestAddConstnat_upToMax;
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
  stack       : pStackRecord;
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
  stack       : pStackRecord;
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
  stack : pStackRecord;
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

initialization

finalization

end.
