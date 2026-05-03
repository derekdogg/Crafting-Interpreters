unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Intf,
  FireDAC.Stan.Async, FireDAC.Phys.MSSQL, FireDAC.DApt, FireDAC.VCLUI.Wait;

type

  TForm4 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Memo2: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);

  private
    procedure RunSampleFiles;
    procedure RunOfficialTests;
    procedure RunStressTests;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  NativeObjectTestUnit, Chunk_Types, IOUtils, Types, StrUtils;

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
begin
  Memo2.Lines.Clear;
  try
    RunStressTests;
  except
    on E: Exception do
      Memo2.Lines.Add('STRESS TEST FAILURE: ' + E.Message);
  end;
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  Memo2.Lines.Clear;
  RunNativeObjectTest(Memo2.Lines);
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
  Memo2.Lines.Clear;
  try
    RunSampleFiles;
    RunOfficialTests;
  except
    on E: Exception do
      Memo2.Lines.Add('TEST FAILURE: ' + E.Message);
  end;
end;

procedure TForm4.RunStressTests;
var
  StressDir: string;
  Files: TStringDynArray;
  F, FileName, Content: string;
  IR: TInterpretResult;
  Passed, Failed: integer;
begin
  StressDir := TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\samples\stress');
  if not TDirectory.Exists(StressDir) then
  begin
    Memo2.Lines.Add('samples/stress/ directory not found');
    Exit;
  end;

  Passed := 0;
  Failed := 0;

  Files := TDirectory.GetFiles(StressDir, '*.lox');
  for F in Files do
  begin
    FileName := TPath.GetFileName(F);
    Memo2.Lines.Add('Running stress: ' + FileName);
    Application.ProcessMessages;
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

  if Failed = 0 then
    Memo2.Lines.Add('All ' + IntToStr(Passed) + ' stress tests passed.')
  else
    Memo2.Lines.Add(IntToStr(Failed) + ' of ' + IntToStr(Passed + Failed) + ' stress tests FAILED.');
end;

procedure TForm4.RunSampleFiles;
var
  SamplesDir, ErrorsDir: string;
  Files: TStringDynArray;
  F, FileName, Content: string;
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
    Memo2.Lines.Add('Running: samples/' + FileName);
    Application.ProcessMessages;
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
      Memo2.Lines.Add('Running: samples/errors/' + FileName);
      Application.ProcessMessages;
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

procedure TForm4.RunOfficialTests;
var
  TestDir, RelPath: string;
  AllFiles: TStringDynArray;
  F, Line, Content, Rest: string;
  Lines, ExpectedOutputs, ExpectedCompileErrors, ActualOutputs, Failures: TStringList;
  ExpectedRuntimeError: string;
  IR: TInterpretResult;
  Passed, Failed, Skipped, i, LineNum, p, BracketEnd: integer;
  HasOutput, HasRuntimeError, HasCompileErrors, TestOK: Boolean;
begin
  TestDir := TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\test');
  if not TDirectory.Exists(TestDir) then
  begin
    Memo2.Lines.Add('test/ directory not found');
    Exit;
  end;

  AllFiles := TDirectory.GetFiles(TestDir, '*.lox', TSearchOption.soAllDirectories);
  Passed := 0;
  Failed := 0;
  Skipped := 0;

  Lines := TStringList.Create;
  ExpectedOutputs := TStringList.Create;
  ExpectedCompileErrors := TStringList.Create;
  ActualOutputs := TStringList.Create;
  Failures := TStringList.Create;
  try
    for F in AllFiles do
    begin
      RelPath := F.Substring(Length(TestDir) + 1);
      Content := TFile.ReadAllText(F);

      Lines.Clear;
      ExpectedOutputs.Clear;
      ExpectedCompileErrors.Clear;
      Failures.Clear;
      Lines.Text := Content;
      ExpectedRuntimeError := '';

      // --- Parse expectations from comments ---
      for i := 0 to Lines.Count - 1 do
      begin
        Line := Lines[i];
        LineNum := i + 1;

        if Pos('// [java', Line) > 0 then
          Continue;

        p := Pos('// expect runtime error: ', Line);
        if p > 0 then
        begin
          ExpectedRuntimeError := Copy(Line, p + 25, MaxInt);
          Continue;
        end;

        p := Pos('// expect: ', Line);
        if p > 0 then
        begin
          ExpectedOutputs.Add(Copy(Line, p + 11, MaxInt));
          Continue;
        end;

        p := Pos('// [', Line);
        if p > 0 then
        begin
          Rest := Copy(Line, p + 4, MaxInt);
          if Copy(Rest, 1, 7) = 'c line ' then
          begin
            Rest := Copy(Rest, 8, MaxInt);
            BracketEnd := Pos(']', Rest);
            if BracketEnd > 0 then
              ExpectedCompileErrors.Add('[line ' + Copy(Rest, 1, BracketEnd - 1) + '] ' +
                Trim(Copy(Rest, BracketEnd + 1, MaxInt)));
          end
          else if Copy(Rest, 1, 5) = 'line ' then
          begin
            Rest := Copy(Rest, 6, MaxInt);
            BracketEnd := Pos(']', Rest);
            if BracketEnd > 0 then
              ExpectedCompileErrors.Add('[line ' + Copy(Rest, 1, BracketEnd - 1) + '] ' +
                Trim(Copy(Rest, BracketEnd + 1, MaxInt)));
          end;
          Continue;
        end;

        p := Pos('// Error', Line);
        if p > 0 then
        begin
          ExpectedCompileErrors.Add(Format('[line %d] %s', [LineNum, Trim(Copy(Line, p + 3, MaxInt))]));
          Continue;
        end;
      end;

      HasOutput := ExpectedOutputs.Count > 0;
      HasRuntimeError := ExpectedRuntimeError <> '';
      HasCompileErrors := ExpectedCompileErrors.Count > 0;

      if not HasOutput and not HasRuntimeError and not HasCompileErrors then
      begin
        Inc(Skipped);
        Continue;
      end;

      Memo2.Lines.Add('Running: ' + RelPath);
      Application.ProcessMessages;

      // --- Run the test ---
      try
        IR := InterpretResult(PAnsiChar(AnsiString(Content)));
        TestOK := True;

        if HasCompileErrors then
        begin
          if IR.code <> INTERPRET_COMPILE_ERROR then
          begin
            Failures.Add('Expected compile error but got ' +
              IfThen(IR.code = INTERPRET_OK, 'OK', 'runtime error'));
            TestOK := False;
          end
          else
          begin
            for i := 0 to ExpectedCompileErrors.Count - 1 do
            begin
              if Pos(ExpectedCompileErrors[i], IR.ErrorStr) = 0 then
              begin
                Failures.Add('Missing compile error: ' + ExpectedCompileErrors[i]);
                TestOK := False;
              end;
            end;
          end;
        end
        else if HasRuntimeError then
        begin
          if IR.code <> INTERPRET_RUNTIME_ERROR then
          begin
            Failures.Add('Expected runtime error but got ' +
              IfThen(IR.code = INTERPRET_OK, 'OK', 'compile error'));
            TestOK := False;
          end
          else if Trim(IR.ErrorStr) <> Trim(ExpectedRuntimeError) then
          begin
            Failures.Add('Expected error "' + ExpectedRuntimeError +
              '" but got "' + IR.ErrorStr + '"');
            TestOK := False;
          end;

          if HasOutput then
          begin
            ActualOutputs.Clear;
            if IR.OutputStr <> '' then
              ActualOutputs.Text := IR.OutputStr;
            if (ActualOutputs.Count > 0) and (ActualOutputs[ActualOutputs.Count - 1] = '') then
              ActualOutputs.Delete(ActualOutputs.Count - 1);
            if ActualOutputs.Count <> ExpectedOutputs.Count then
            begin
              Failures.Add(Format('Expected %d output lines but got %d',
                [ExpectedOutputs.Count, ActualOutputs.Count]));
              TestOK := False;
            end
            else
            begin
              for i := 0 to ExpectedOutputs.Count - 1 do
                if ActualOutputs[i] <> ExpectedOutputs[i] then
                begin
                  Failures.Add(Format('Line %d: expected "%s" got "%s"',
                    [i + 1, ExpectedOutputs[i], ActualOutputs[i]]));
                  TestOK := False;
                end;
            end;
          end;
        end
        else
        begin
          if IR.code <> INTERPRET_OK then
          begin
            Failures.Add('Expected OK but got error: ' + IR.ErrorStr);
            TestOK := False;
          end
          else
          begin
            ActualOutputs.Clear;
            if IR.OutputStr <> '' then
              ActualOutputs.Text := IR.OutputStr;
            if (ActualOutputs.Count > 0) and (ActualOutputs[ActualOutputs.Count - 1] = '') then
              ActualOutputs.Delete(ActualOutputs.Count - 1);
            if ActualOutputs.Count <> ExpectedOutputs.Count then
            begin
              Failures.Add(Format('Expected %d output lines but got %d',
                [ExpectedOutputs.Count, ActualOutputs.Count]));
              TestOK := False;
            end
            else
            begin
              for i := 0 to ExpectedOutputs.Count - 1 do
                if ActualOutputs[i] <> ExpectedOutputs[i] then
                begin
                  Failures.Add(Format('Line %d: expected "%s" got "%s"',
                    [i + 1, ExpectedOutputs[i], ActualOutputs[i]]));
                  TestOK := False;
                end;
            end;
          end;
        end;

      except
        on E: Exception do
        begin
          Failures.Add('CRASH: ' + E.ClassName + ': ' + E.Message);
          TestOK := False;
        end;
      end;

      if TestOK then
        Inc(Passed)
      else
      begin
        Inc(Failed);
        Memo2.Lines.Add('FAIL: ' + RelPath);
        for i := 0 to Failures.Count - 1 do
          Memo2.Lines.Add('  ' + Failures[i]);
      end;
    end;
  finally
    Lines.Free;
    ExpectedOutputs.Free;
    ExpectedCompileErrors.Free;
    ActualOutputs.Free;
    Failures.Free;
  end;

  Memo2.Lines.Add('');
  if Failed = 0 then
    Memo2.Lines.Add('Official tests: All ' + IntToStr(Passed) + ' passed.' +
      IfThen(Skipped > 0, ' (' + IntToStr(Skipped) + ' skipped)'))
  else
    Memo2.Lines.Add('Official tests: ' + IntToStr(Failed) + ' of ' +
      IntToStr(Passed + Failed) + ' FAILED.' +
      IfThen(Skipped > 0, ' (' + IntToStr(Skipped) + ' skipped)'));
end;

end.
