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
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  Chunk_Types, Test;

{$R *.dfm}



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



procedure TForm4.Button3Click(Sender: TObject);
begin
  TestStackPush;
  TestStackPop;
  TestStackPeek;
end;

end.
