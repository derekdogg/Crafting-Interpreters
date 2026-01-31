unit Test;

interface

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
  New(MemTracker);

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
    dispose(MemTracker);
  end;

end;


procedure TestInitialAllocation;
var
  Grew: Boolean;
  MemTracker : pMemTracker;
  ValueArray : PValueArray;
  Capacity   : integer;
begin
  MemTracker := nil;
  ValueArray := nil;
  Capacity := 0;

  InitMemTracker(MemTracker);
  InitValueArray(ValueArray,MemTracker);

  Grew := AllocateArray(Pointer(ValueArray), Capacity, 10, SizeOf(TValue), MemTracker);
  Assert(Grew = True, 'Initial allocation should grow array');
  Assert(Capacity = START_CAPACITY, 'Capacity should be START_CAPACITY after first allocation');
  Assert(ValueArray <> nil, 'Pointer should be allocated');
  FreeValueArray(ValueArray,MemTracker);
  FreeMemTracker(MemTracker);
end;

procedure TestNoGrowthNeeded;
var
  Grew: Boolean;
  Count: Integer;
  ValueArray : PValueArray;
  MemTracker : pMemTracker;
  Capacity   : integer;
begin
 (* ValueArray := nil;
  MemTracker := nil;
  Capacity   := 0;

  InitMemTracker(MemTracker);
  InitValueArray(ValueArray,MemTracker);

  // Pre-allocate
  AllocateArray(Pointer(ValueArray), Capacity, 0, SizeOf(TValue));

  Count := Capacity - 1;
  Grew := AllocateArray(Pointer(ValueArray), Capacity, Count, SizeOf(TValue));
  Assert(Grew = False, 'No growth should occur if Count < Capacity');
  Assert(ValueArray <> nil, 'Pointer should remain allocated');

  FreeValueArray(ValueArray,MemTracker);
  FreeMemTracker(MemTracker); *)
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
 // writeChunk(chunk,1,1);
  FreeChunk(chunk,MemTracker);
  FreeMemTracker(MemTracker);
  Assert(chunk = nil);
end;


procedure TestStringEqual;
var
 valueA : pObjString;
 valueB : pObjString;
begin
 (* valueA := CreateString('fred');
  valueB := CreateString('fred');
  try
    assert(StringsEqual(valueA,valueB), 'strings are not equal');
  finally
    freeString(valueA);
    freeString(valueB);
  end;
  assert(ValueA = nil);
  assert(ValueB = nil);

  valueA := CreateString('');
  valueB := CreateString('');
  try
    assert(StringsEqual(valueA,valueB), 'strings are not equal');
  finally
    freeString(valueA);
    freeString(valueB);
  end;
  assert(ValueA = nil);
  assert(ValueB = nil);
  *)
end;


procedure TestStringUnequal;
var
 valueA : pObjString;
 valueB : pObjString;
begin
  (*valueA := CreateString('fred ');
  valueB := CreateString('fred');
  try
    assert(StringsEqual(valueA,valueB) = false, 'strings are NOT equal');
  finally
    freeString(valueA);
    freeString(valueB);
  end;
   *)
end;


procedure TestStackPeek;
var
  stack : pStackRecord;
  value : TValue;
  i     : integer;
begin
  (*InitStack(stack);

  try
    for i := 0 to 999 do
    begin
      value.NumberValue := i;
      pushStack(stack,value);
    end;

    assert(Stack.Count = 1000);

     // Peek from top down
    for i := 0 to 999 do
    begin
      value := PeekStack(stack, i);
      Assert(value.NumberValue = 999 - i);
    end;


  except
    on E:EAssertionFailed do
    begin
      raise;
    end;
  end;
  FreeStack(stack);
  *)
end;

procedure TestStackPop;
var
  stack : pStackRecord;
  value : TValue;
  i     : integer;
begin
 (* InitStack(stack);
  try
    for i := 0 to 999 do
    begin
      value.NumberValue := i;
      pushStack(stack,value);
    end;

    assert(Stack.Count = 1000);

    for i := 999 downto 0 do
    begin
      value := popStack(Stack);
      assert(Value.NumberValue = i);
    end;


  except
    on E:EAssertionFailed do
    begin
      raise;
    end;
  end;
  FreeStack(stack);  *)
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
