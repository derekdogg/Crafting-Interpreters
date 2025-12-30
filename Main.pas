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

procedure TForm4.Button1Click(Sender: TObject);
var
  Chunk : pChunk;

  idx : integer;


  value : TValue;
begin

  chunk := nil;

  InitChunk(Chunk);

  AddConstant(chunk,123);

  writeChunk(Chunk, OP_NEGATE);

  writeChunk(Chunk, OP_RETURN);

  InterpretResult(chunk,Memo1.Lines);

  freeChunk(Chunk);
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  ValueRecord : pValueRecord;
begin
  ValueRecord := nil;
  initValueRecord(ValueRecord);
  Assert(Assigned(ValueRecord), 'Value Record is not assigned');
  writeValueRecord(ValueRecord,12.12);
  writeValueRecord(ValueRecord,12.12);
  writeValueRecord(ValueRecord,12.12);
  writeValueRecord(ValueRecord,12.12);
  writeValueRecord(ValueRecord,12.12);
  writeValueRecord(ValueRecord,12.12);
  writeValueRecord(ValueRecord,12.12);

  printValueRecord(ValueRecord,Memo1.Lines);


  freeValueRecord(ValueRecord);


end;

end.
