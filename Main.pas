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
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);

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
  txt : ansiString;
begin

    txt := Memo1.Lines.Text;
    IR := interpretResult(PAnsiChar(txt));

    case Ir.code of
    INTERPRET_OK:
    begin
      if IR.OutputStr <> '' then
        Memo2.Lines.Add(IR.OutputStr);
      case IR.value.ValueKind of
        vkNumber: Memo2.lines.add(IR.value.NumberValue.ToString);
        vkBoolean:Memo2.Lines.add(BoolToStr(IR.value.BooleanValue,true));
        vkNull: ; // suppress null for statement-based programs
        vkObject :
        begin
          Memo2.lines.add(IR.ResultStr);
        end;
      end;
    end;

    INTERPRET_COMPILE_ERROR: begin
       Memo2.Lines.add(IR.ErrorStr);
    end;

    INTERPRET_RUNTIME_ERROR: begin
      Memo2.Lines.add(IR.ErrorStr);
    end;
    else
      begin
        Memo2.Lines.add('We fucked up');
      end;
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

procedure TForm4.Button4Click(Sender: TObject);
begin
  TestStringEqual;
  TestStringUnequal;
  TestValuesEqual;
end;

procedure TForm4.Button5Click(Sender: TObject);
begin
  TestInitChunkAndFreeChunk;
end;


procedure TForm4.Button6Click(Sender: TObject);
begin

  TestAddValueConstant;
end;

procedure TForm4.Button7Click(Sender: TObject);
begin
  testTable;
  TestTableResize;
  TestStringInterning;
  Memo2.Lines.Add('All table tests passed.');
end;

procedure TForm4.Button8Click(Sender: TObject);
begin
  TestInterpreter;
  TestGlobals;
  Memo2.Lines.Add('All interpreter tests passed.');
end;

end.
