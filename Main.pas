unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

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
   AddConstant(chunk,10,123);
   AddConstant(chunk,10,123);
   writeChunk(Chunk, OP_ADD,123);
   writeChunk(Chunk, OP_RETURN,123);
//   vmResult := InterpretResult(chunk,strings);
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
  AddConstant(chunk, 6,123);
  AddConstant(chunk, 7,123);
  WriteChunk(chunk, OP_MULTIPLY,123);
  WriteChunk(chunk, OP_RETURN,123);
 // vmResult := InterpretResult(chunk,strings);
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
  AddConstant(chunk, 20,123);
  AddConstant(chunk, 7,123);
  WriteChunk(chunk, OP_SUBTRACT,123);
  WriteChunk(chunk, OP_RETURN,123);
 // vmResult := InterpretResult(chunk, strings);
  assert(vmResult.Value = 13);
  FreeChunk(chunk);
end;

procedure FortyEightDividedBySix(strings: TStrings);
var
  Chunk : pChunk;

begin
   chunk := nil;
   InitChunk(Chunk);
   AddConstant(chunk, 48,123);
   AddConstant(chunk, 6,123);
   WriteChunk(chunk, OP_DIVIDE,123);
   WriteChunk(chunk, OP_RETURN,123);
  // InterpretResult(chunk, strings);
   freeChunk(Chunk);
end;

procedure FivePlusThreeTimesTwo(strings: TStrings);  //(5 + 3) * 2
var
  Chunk : pChunk;
begin
  chunk := nil;
  InitChunk(Chunk);
  AddConstant(chunk, 5,123);
  AddConstant(chunk, 3,123);
  WriteChunk(chunk, OP_ADD,123);
  AddConstant(chunk, 2,123);
  WriteChunk(chunk, OP_MULTIPLY,123);
  WriteChunk(chunk, OP_RETURN,123);
  //InterpretResult(chunk, strings);
  freeChunk(Chunk);
end;

procedure ComplexExpressionTest(strings: TStrings);
var
  chunk: pChunk;
  vmResult: TInterpretResult;
begin
  chunk := nil;
  InitChunk(chunk);

  // Push constants
  AddConstant(chunk, 1,123);     // 1
  AddConstant(chunk, 2,123);     // 2
  AddConstant(chunk, 3,123);     // 3
  WriteChunk(chunk, OP_MULTIPLY,123); // 2 * 3

  WriteChunk(chunk, OP_ADD,123);      // 1 + (2*3)

  AddConstant(chunk, 4,123);          // 4
  AddConstant(chunk, -5,123);         // -5
  WriteChunk(chunk, OP_DIVIDE,123);   // 4 / -5

  WriteChunk(chunk, OP_SUBTRACT,123); // (1 + 2*3) - (4 / -5)

  WriteChunk(chunk, OP_RETURN,123);

  //vmResult := InterpretResult(chunk, strings);

  assert(Abs(vmResult.Value - 7.8) < 1e-10);

  FreeChunk(chunk);
end;

procedure TenMinusTwenty(strings: TStrings);
var
  chunk: pChunk;
  vmResult: TInterpretResult;
begin
  chunk := nil;
  InitChunk(chunk);
  AddConstant(chunk, 10,123);
  AddConstant(chunk, 20,123);
  WriteChunk(chunk, OP_SUBTRACT,123);
  WriteChunk(chunk, OP_RETURN,123);
  //vmResult := InterpretResult(chunk, strings);
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
  AddConstant(chunk, 3.5,123);
  AddConstant(chunk, 2.2,123);
  WriteChunk(chunk, OP_ADD,123);
  WriteChunk(chunk, OP_RETURN,123);
  //vmResult := InterpretResult(chunk, strings);
  assert(Abs(vmResult.Value - 5.7) < 1e-10); // floating-point comparison
  FreeChunk(chunk);
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  IR : TInterpretResult;
begin
  (*TenPlusTen(memo1.Lines);
  SixTimesSeven(memo1.lines);
  FivePlusThreeTimesTwo(Memo1.Lines);
  FortyEightDividedBySix(Memo1.Lines);
  TwentyMinusSeven(memo1.lines);
  TenMinusTwenty(memo1.lines);
  ThreePointFivePlusTwoPointTwo(memo1.lines);
  ComplexExpressionTest(memo1.lines);*)
  IR := interpretResult(PChar(Memo1.Lines.Text));
  if IR.result = INTERPRET_OK then
  begin
    memo2.Lines.assign(Output);
  end;

end;



procedure TForm4.Button2Click(Sender: TObject);
var
  c : Char;
begin
  //compile(PChar(Memo1.Lines.Text),memo2.lines);

end;



end.
