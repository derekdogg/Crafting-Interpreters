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
    procedure RunInjectionTest;
    procedure RunRttiTest;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  NativeObjectTestUnit, Chunk_Types, IOUtils, Types, StrUtils;

{$R *.dfm}

type
  TAddress = class
  private
    FStreet: string;
    FCity: string;
    FZip: string;
  public
    constructor Create(const AStreet, ACity, AZip: string);
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    property Zip: string read FZip write FZip;
  end;

  TCustomer = class
  private
    FName: string;
    FAge: Integer;
    FBalance: Double;
    FActive: Boolean;
    FAddress: TAddress;
  public
    constructor Create(const AName: string; AAge: Integer; ABalance: Double; AActive: Boolean);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Balance: Double read FBalance write FBalance;
    property Active: Boolean read FActive write FActive;
    property Address: TAddress read FAddress write FAddress;
    function Greet: string;
  end;

constructor TAddress.Create(const AStreet, ACity, AZip: string);
begin
  inherited Create;
  FStreet := AStreet;
  FCity := ACity;
  FZip := AZip;
end;

constructor TCustomer.Create(const AName: string; AAge: Integer; ABalance: Double; AActive: Boolean);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
  FBalance := ABalance;
  FActive := AActive;
  FAddress := nil;
end;

destructor TCustomer.Destroy;
begin
  FAddress.Free;
  inherited;
end;

function TCustomer.Greet: string;
begin
  Result := 'Hello, ' + FName + '!';
end;



procedure TForm4.Button1Click(Sender: TObject);
var
  IR : TInterpretResult;
  txt : ansiString;
begin

    txt := AnsiString(Memo1.Lines.Text);
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
  RunInjectionTest;
  RunRttiTest;
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

      // Skip tests that require Delphi-side injection (run separately)
      if Pos('inject', LowerCase(RelPath)) > 0 then
      begin
        Inc(Skipped);
        Continue;
      end;

      Memo2.Lines.Add('Running: ' + RelPath);
      Application.ProcessMessages;

      // --- Run the test ---
      TestOK := False;
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

procedure TForm4.RunInjectionTest;
var
  ScriptPath, Content: string;
  sl: TStringList;
  IR: TInterpretResult;
begin
  Memo2.Lines.Add('');
  Memo2.Lines.Add('=== Injection Test ===');

  ScriptPath := TPath.Combine(ExtractFilePath(ParamStr(0)),
    '..\..\test\record\inject_stringlist.lox');
  if not TFile.Exists(ScriptPath) then
  begin
    Memo2.Lines.Add('SKIP: inject_stringlist.lox not found');
    Exit;
  end;

  Content := TFile.ReadAllText(ScriptPath);

  // Create and populate a TStringList on the Delphi side
  sl := TStringList.Create;
  try
    sl.Add('Alice');
    sl.Add('Bob');
    sl.Add('Charlie');

    // Use the split pipeline: InitVM → Inject → CompileAndRun → FreeVM
    InitVM;
    try
      InjectNativeObject('items', Pointer(sl), 'StringList');
      IR := CompileAndRun(PAnsiChar(AnsiString(Content)));
    finally
      FreeVM;
    end;

    // Verify VM result
    if IR.code <> INTERPRET_OK then
    begin
      Memo2.Lines.Add('FAIL: ' + IR.ErrorStr);
      Exit;
    end;

    // Verify Delphi-side mutations survived
    if sl.Count <> 3 then
    begin
      Memo2.Lines.Add('FAIL: expected 3 items after script, got ' + IntToStr(sl.Count));
      Exit;
    end;
    if sl[0] <> 'Alice' then
    begin
      Memo2.Lines.Add('FAIL: expected items[0]="Alice", got "' + sl[0] + '"');
      Exit;
    end;
    if sl[1] <> 'Charlie' then
    begin
      Memo2.Lines.Add('FAIL: expected items[1]="Charlie", got "' + sl[1] + '"');
      Exit;
    end;
    if sl[2] <> 'Diana' then
    begin
      Memo2.Lines.Add('FAIL: expected items[2]="Diana", got "' + sl[2] + '"');
      Exit;
    end;

    Memo2.Lines.Add('PASS: Injected StringList survived round-trip');
    Memo2.Lines.Add('  Delphi sees: ' + sl.CommaText);
  finally
    sl.Free;
  end;
end;

procedure TForm4.RunRttiTest;
var
  cust: TCustomer;
  IR: TInterpretResult;
  Script: AnsiString;
begin
  Memo2.Lines.Add('');
  Memo2.Lines.Add('=== RTTI Injection Test ===');

  cust := TCustomer.Create('Alice', 30, 1500.50, True);
  try
    cust.Address := TAddress.Create('123 Main St', 'Springfield', '62704');

    Script :=
      'print cust.Name;' + #10 +
      'print cust.Age;' + #10 +
      'print cust.Balance;' + #10 +
      'print cust.Active;' + #10 +
      'cust.Name = "Bob";' + #10 +
      'cust.Age = 25;' + #10 +
      'print cust.Name;' + #10 +
      'print cust.Age;' + #10 +
      'print cust.Greet();' + #10 +
      '// Nested object access' + #10 +
      'var addr = cust.Address;' + #10 +
      'print addr.Street;' + #10 +
      'print addr.City;' + #10 +
      'print addr.Zip;' + #10 +
      '// Direct chained access' + #10 +
      'print cust.Address.City;' + #10 +
      '// Mutate via RTTI' + #10 +
      'addr.City = "Shelbyville";' + #10 +
      'print cust.Address.City;' + #10;

    InitVM;
    try
      InjectObject('cust', cust);
      IR := CompileAndRun(PAnsiChar(Script));
    finally
      FreeVM;
    end;

    if IR.code <> INTERPRET_OK then
    begin
      Memo2.Lines.Add('FAIL: ' + IR.ErrorStr);
      Exit;
    end;

    // Show script output
    if IR.OutputStr <> '' then
      Memo2.Lines.Add(IR.OutputStr);

    // Verify Delphi-side mutations
    if cust.Name <> 'Bob' then
    begin
      Memo2.Lines.Add('FAIL: expected Name="Bob", got "' + cust.Name + '"');
      Exit;
    end;
    if cust.Age <> 25 then
    begin
      Memo2.Lines.Add('FAIL: expected Age=25, got ' + IntToStr(cust.Age));
      Exit;
    end;
    if cust.Address.City <> 'Shelbyville' then
    begin
      Memo2.Lines.Add('FAIL: expected City="Shelbyville", got "' + cust.Address.City + '"');
      Exit;
    end;

    Memo2.Lines.Add('PASS: RTTI injection round-trip');
    Memo2.Lines.Add('  Delphi sees: Name=' + cust.Name + ', Age=' + IntToStr(cust.Age) +
      ', City=' + cust.Address.City);
  finally
    cust.Free;
  end;
end;

end.
