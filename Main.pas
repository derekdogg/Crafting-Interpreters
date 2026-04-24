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
    procedure FormCreate(Sender: TObject);

  private
    procedure RunSampleFiles;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  Chunk_Types, IOUtils, Types;

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

procedure TForm4.RunSampleFiles;
var
  SamplesDir, ErrorsDir: string;
  Files: TStringDynArray;
  F, FileName, Content, FirstLine: string;
  IR: TInterpretResult;
  Passed, Failed: integer;
begin
  SamplesDir := TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\samples');
  if not TDirectory.Exists(SamplesDir) then
  begin
    Memo2.Lines.Add('samples/ directory not found');
    Exit;
  end;

  Passed := 0;
  Failed := 0;

  // Run normal samples (expect INTERPRET_OK)
  Files := TDirectory.GetFiles(SamplesDir, '*.lox');
  for F in Files do
  begin
    FileName := TPath.GetFileName(F);
    Content := TFile.ReadAllText(F);
    IR := InterpretResult(PAnsiChar(AnsiString(Content)));
    if IR.code = INTERPRET_OK then
      Inc(Passed)
    else
    begin
      Inc(Failed);
      Memo2.Lines.Add('FAIL: ' + FileName + ' - ' + IR.ErrorStr);
    end;
  end;

  // Run error samples (expect runtime or compile error)
  ErrorsDir := TPath.Combine(SamplesDir, 'errors');
  if TDirectory.Exists(ErrorsDir) then
  begin
    Files := TDirectory.GetFiles(ErrorsDir, '*.lox');
    for F in Files do
    begin
      FileName := TPath.GetFileName(F);
      Content := TFile.ReadAllText(F);
      IR := InterpretResult(PAnsiChar(AnsiString(Content)));
      if (IR.code = INTERPRET_RUNTIME_ERROR) or (IR.code = INTERPRET_COMPILE_ERROR) then
        Inc(Passed)
      else
      begin
        Inc(Failed);
        Memo2.Lines.Add('FAIL: errors/' + FileName + ' - expected error but got OK');
      end;
    end;
  end;

  if Failed = 0 then
    Memo2.Lines.Add('All ' + IntToStr(Passed) + ' sample tests passed.')
  else
    Memo2.Lines.Add(IntToStr(Failed) + ' of ' + IntToStr(Passed + Failed) + ' sample tests FAILED.');
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Memo2.Lines.Clear;
  try
    RunSampleFiles;
  except
    on E: Exception do
      Memo2.Lines.Add('TEST FAILURE: ' + E.Message);
  end;
end;

end.
