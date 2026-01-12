unit Test;

interface

procedure TestStackPush;
procedure TestStackPop;
procedure TestStackPeek;

implementation

uses
  sysutils,Chunk_types;


procedure TestStackPeek;
var
  stack : pStackRecord;
  value : TValue;
  i     : integer;
begin
  InitStack(stack);

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
end;

procedure TestStackPop;
var
  stack : pStackRecord;
  value : TValue;
  i     : integer;
begin
  InitStack(stack);
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
  FreeStack(stack);
end;



procedure TestStackPush;
var
  stack : pStackRecord;
  value : TValue;
  i     : integer;
begin
  InitStack(stack);
  try
    for i := 0 to 999 do
      pushStack(stack,value);

    assert(Stack.Count = 1000);

  except
    on E:EAssertionFailed do
    begin
      raise;
    end;
  end;
  FreeStack(stack);
end;

end.
