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
begin

  chunk := nil;

  InitChunk(Chunk);
  writeChunk(Chunk,OP_RETURN);
  Assert(Chunk.Capacity = 8, 'expected 8');
  writeChunk(Chunk,OP_FOO);
  writeChunk(Chunk,3);
  writeChunk(Chunk,4);
  writeChunk(Chunk,5);
  writeChunk(Chunk,6);
  writeChunk(Chunk,7);
  writeChunk(Chunk,8);

  writeChunk(Chunk,1);
  Assert(Chunk.Capacity = 16, 'expected 16');
  writeChunk(Chunk,2);
  writeChunk(Chunk,3);
  writeChunk(Chunk,4);
  writeChunk(Chunk,5);
  writeChunk(Chunk,6);
  writeChunk(Chunk,7);
  writeChunk(Chunk,8);

  Assert(Chunk.Capacity = 16, 'expected 16');
  writeChunk(Chunk,1);
  Assert(Chunk.Capacity = 32, 'expected 32');

  Memo1.Lines.clear;
  printChunk(Chunk,Memo1.Lines);

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
