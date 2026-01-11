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

    value := popStack(Stack);
    assert(Value.NumberValue = 999);

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
      pushStack(stack,value);

    assert(Stack.Count = 1000);

    for i := 0 to 999 do
      popStack(Stack);

    assert(Stack.Count = 0);

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
