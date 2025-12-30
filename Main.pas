unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  Chunk_Types;

{$R *.dfm}


procedure TenPlusTen(strings : TStrings);   //10 + 10
var
  Chunk : pChunk;
  vmResult : TInterpretResult;
begin
   chunk := nil;
   InitChunk(Chunk);
   AddConstant(chunk,10);
   AddConstant(chunk,10);
   writeChunk(Chunk, OP_ADD);
   writeChunk(Chunk, OP_RETURN);
   vmResult := InterpretResult(chunk,strings);
   assert(vmResult.value = 20);
   freeChunk(Chunk);
end;


procedure SixTimesSeven(strings: TStrings);
var
  Chunk : pChunk;
  vmResult : TInterpretResult;
begin
  chunk := nil;
  InitChunk(Chunk);
  AddConstant(chunk, 6);
  AddConstant(chunk, 7);
  WriteChunk(chunk, OP_MULTIPLY);
  WriteChunk(chunk, OP_RETURN);
  vmResult := InterpretResult(chunk,strings);
  assert(vmResult.Value = 42);
  freeChunk(Chunk);
end;

procedure TwentyMinusSeven(strings: TStrings);
var
  chunk: pChunk;
  vmResult: TInterpretResult;
begin
  chunk := nil;
  InitChunk(chunk);
  AddConstant(chunk, 20);
  AddConstant(chunk, 7);
  WriteChunk(chunk, OP_SUBTRACT);
  WriteChunk(chunk, OP_RETURN);
  vmResult := InterpretResult(chunk, strings);
  assert(vmResult.Value = 13);
  FreeChunk(chunk);
end;

procedure FortyEightDividedBySix(strings: TStrings);
var
  Chunk : pChunk;

begin
   chunk := nil;
   InitChunk(Chunk);
   AddConstant(chunk, 48);
   AddConstant(chunk, 6);
   WriteChunk(chunk, OP_DIVIDE);
   WriteChunk(chunk, OP_RETURN);
   InterpretResult(chunk, strings);
   freeChunk(Chunk);
end;

procedure FivePlusThreeTimesTwo(strings: TStrings);  //(5 + 3) * 2
var
  Chunk : pChunk;
begin
  chunk := nil;
  InitChunk(Chunk);
  AddConstant(chunk, 5);
  AddConstant(chunk, 3);
  WriteChunk(chunk, OP_ADD);
  AddConstant(chunk, 2);
  WriteChunk(chunk, OP_MULTIPLY);
  WriteChunk(chunk, OP_RETURN);
  InterpretResult(chunk, strings);
  freeChunk(Chunk);
end;


procedure TenMinusTwenty(strings: TStrings);
var
  chunk: pChunk;
  vmResult: TInterpretResult;
begin
  chunk := nil;
  InitChunk(chunk);
  AddConstant(chunk, 10);
  AddConstant(chunk, 20);
  WriteChunk(chunk, OP_SUBTRACT);
  WriteChunk(chunk, OP_RETURN);
  vmResult := InterpretResult(chunk, strings);
  assert(vmResult.Value = -10);
  FreeChunk(chunk);
end;

procedure ThreePointFivePlusTwoPointTwo(strings: TStrings);
var
  chunk: pChunk;
  vmResult: TInterpretResult;
begin
  chunk := nil;
  InitChunk(chunk);
  AddConstant(chunk, 3.5);
  AddConstant(chunk, 2.2);
  WriteChunk(chunk, OP_ADD);
  WriteChunk(chunk, OP_RETURN);
  vmResult := InterpretResult(chunk, strings);
  assert(Abs(vmResult.Value - 5.7) < 1e-10); // floating-point comparison
  FreeChunk(chunk);
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  Chunk : pChunk;

  idx : integer;


  value : TValue;
begin

  chunk := nil;

  InitChunk(Chunk);

  TenPlusTen(memo1.Lines);
  SixTimesSeven(memo1.lines);
  FivePlusThreeTimesTwo(Memo1.Lines);
  FortyEightDividedBySix(Memo1.Lines);
  TwentyMinusSeven(memo1.lines);
  TenMinusTwenty(memo1.lines);
  ThreePointFivePlusTwoPointTwo(memo1.lines);
  freeChunk(Chunk);
end;



end.
