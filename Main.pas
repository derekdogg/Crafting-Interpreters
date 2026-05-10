unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ImgList,
  SynEdit, SynEditTypes, SynHighlighterLox,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Intf,
  FireDAC.Stan.Async, FireDAC.Phys.MSSQL, FireDAC.DApt, FireDAC.VCLUI.Wait;

type

  TForm4 = class(TForm)
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TSynEdit;
    Memo2: TMemo;
    Button1: TButton;
    TestTree: TTreeView;
    BtnPopulate: TButton;
    BtnRunSelected: TButton;
    BtnCheckAll: TButton;
    BtnUncheckAll: TButton;
    BtnRunAll: TButton;
    ProgressBar1: TProgressBar;
    LblStatus: TLabel;
    StateImages: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnPopulateClick(Sender: TObject);
    procedure BtnRunSelectedClick(Sender: TObject);
    procedure BtnCheckAllClick(Sender: TObject);
    procedure BtnUncheckAllClick(Sender: TObject);
    procedure BtnRunAllClick(Sender: TObject);
    procedure TestTreeClick(Sender: TObject);
  private
    procedure PopulateTestTree;
    procedure AddTestFiles(ParentNode: TTreeNode; const Dir, Pattern: string;
      Recurse: Boolean);
    procedure SetAllChecked(Node: TTreeNode; Checked: Boolean);
    procedure ToggleCheck(Node: TTreeNode);
    procedure UpdateParentCheck(Node: TTreeNode);
    procedure CollectCheckedFiles(Node: TTreeNode; Files: TStringList);
    function CountChecked(Node: TTreeNode): Integer;
    procedure RunTestFile(const FilePath: string; var Passed, Failed: Integer);
    procedure RunRttiTestFile(const FilePath: string; var Passed, Failed: Integer);
    procedure BuildStateImages;
  private
    FLoxSyn: TSynLoxSyn;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses
  NativeObjectTestUnit, Chunk_Types, IOUtils, Types, StrUtils, Math;

{$R *.dfm}

const
  // StateImages indices: 0=unchecked, 1=checked, 2=pass, 3=fail, 4=skipped
  STATE_UNCHECKED = 1;
  STATE_CHECKED   = 2;
  STATE_PASS      = 3;
  STATE_FAIL      = 4;
  STATE_SKIPPED   = 5;

// Parses '// expect: ' and '// expect runtime error: ' comments from script content.
// Validates IR output/error against expectations. Returns True if all match.
function CheckExpectedOutput(const Content: string; const IR: TInterpretResult;
  Memo: TMemo): Boolean;
var
  Lines, Expected, Actual: TStringList;
  Line, ExpectedRuntimeError: string;
  p, i: Integer;
  HasOutput, HasRuntimeError: Boolean;
begin
  Result := True;
  Lines := TStringList.Create;
  Expected := TStringList.Create;
  Actual := TStringList.Create;
  try
    Lines.Text := Content;
    ExpectedRuntimeError := '';

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      p := Pos('// expect runtime error: ', Line);
      if p > 0 then
      begin
        ExpectedRuntimeError := Copy(Line, p + 25, MaxInt);
        Continue;
      end;
      p := Pos('// expect: ', Line);
      if p > 0 then
        Expected.Add(Copy(Line, p + 11, MaxInt));
    end;

    HasOutput := Expected.Count > 0;
    HasRuntimeError := ExpectedRuntimeError <> '';

    if HasRuntimeError then
    begin
      if IR.code <> INTERPRET_RUNTIME_ERROR then
      begin
        Memo.Lines.Add('  Expected runtime error but got ' +
          IfThen(IR.code = INTERPRET_OK, 'OK', 'compile error'));
        Result := False;
      end
      else if Trim(IR.ErrorStr) <> Trim(ExpectedRuntimeError) then
      begin
        Memo.Lines.Add('  Expected error "' + ExpectedRuntimeError +
          '" but got "' + IR.ErrorStr + '"');
        Result := False;
      end;
    end
    else
    begin
      if IR.code <> INTERPRET_OK then
      begin
        Memo.Lines.Add('  Expected OK but got error: ' + IR.ErrorStr);
        Exit(False);
      end;
    end;

    // Check output lines (some runtime error tests also have expected output before the error)
    if HasOutput then
    begin
      if IR.OutputStr <> '' then
        Actual.Text := IR.OutputStr;
      if (Actual.Count > 0) and (Actual[Actual.Count - 1] = '') then
        Actual.Delete(Actual.Count - 1);

      if Actual.Count <> Expected.Count then
      begin
        Memo.Lines.Add(Format('  Expected %d output lines but got %d',
          [Expected.Count, Actual.Count]));
        Result := False;
      end
      else
      begin
        for i := 0 to Expected.Count - 1 do
          if Actual[i] <> Expected[i] then
          begin
            Memo.Lines.Add(Format('  Line %d: expected "%s" got "%s"',
              [i + 1, Expected[i], Actual[i]]));
            Result := False;
          end;
      end;
    end;
  finally
    Lines.Free;
    Expected.Free;
    Actual.Free;
  end;
end;

type
  // Enum type for RTTI marshaling tests
  TCustomerTier = (ctBronze, ctSilver, ctGold, ctPlatinum);

  // Set type for RTTI marshaling tests
  TCustomerFlag = (cfNewsletter, cfVIP, cfTaxExempt, cfWholesale);
  TCustomerFlags = set of TCustomerFlag;

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
    FTier: TCustomerTier;
    FFlags: TCustomerFlags;
    FTags: TArray<string>;
  public
    constructor Create(const AName: string; AAge: Integer; ABalance: Double; AActive: Boolean);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Balance: Double read FBalance write FBalance;
    property Active: Boolean read FActive write FActive;
    property Address: TAddress read FAddress write FAddress;
    property Tier: TCustomerTier read FTier write FTier;
    property Flags: TCustomerFlags read FFlags write FFlags;
    property Tags: TArray<string> read FTags write FTags;
    function Greet: string;
    procedure AddBalance(Amount: Double);
    procedure SetInfo(const NewName: string; NewAge: Integer);
    function Describe(const Prefix: string): string;
    procedure SetAddress(NewAddr: TAddress);
    function GetTags: TArray<string>;
    procedure SetTags(const NewTags: TArray<string>);
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
  FTier := ctBronze;
  FFlags := [];
  FTags := nil;
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

procedure TCustomer.AddBalance(Amount: Double);
begin
  FBalance := FBalance + Amount;
end;

procedure TCustomer.SetInfo(const NewName: string; NewAge: Integer);
begin
  FName := NewName;
  FAge := NewAge;
end;

function TCustomer.Describe(const Prefix: string): string;
begin
  Result := Prefix + FName + ', age ' + IntToStr(FAge);
end;

procedure TCustomer.SetAddress(NewAddr: TAddress);
begin
  if FAddress <> NewAddr then
    FAddress.Free;
  FAddress := NewAddr;
end;

function TCustomer.GetTags: TArray<string>;
begin
  Result := FTags;
end;

procedure TCustomer.SetTags(const NewTags: TArray<string>);
begin
  FTags := NewTags;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FLoxSyn := TSynLoxSyn.Create(Self);
  Memo1.Highlighter := FLoxSyn;

  { Editor behaviour }
  Memo1.Options := Memo1.Options + [
    eoAutoIndent,          // maintain indentation on new lines
    eoGroupUndo,           // undo whole words, not single chars
    eoSmartTabDelete,      // backspace removes to previous tab stop
    eoSmartTabs,           // tab aligns to previous non-blank line
    eoTabsToSpaces,        // insert spaces instead of tab characters
    eoTrimTrailingSpaces,  // remove trailing blanks on save/move
    eoDragDropEditing,     // drag-and-drop selected text
    eoShowScrollHint,      // show line number while scrolling
    eoEnhanceEndKey        // End key toggles between EOL and last char
  ];
  Memo1.TabWidth := 2;
  Memo1.WantTabs := True;

  { Dark theme — editor surface }
  Memo1.Color                := $001E1E1E;   // VS Code dark background
  Memo1.Font.Color           := $00D4D4D4;   // light grey default text
  Memo1.ActiveLineColor      := $002A2D2E;   // subtle current-line highlight
  Memo1.RightEdge            := 80;
  Memo1.RightEdgeColor       := $00404040;
  Memo1.SelectedColor.Background := $00264F78;  // blue selection (like VS Code)
  Memo1.SelectedColor.Foreground := $00FFFFFF;

  { Dark theme — gutter }
  Memo1.Gutter.Color         := $00252526;
  Memo1.Gutter.BorderColor   := $00404040;
  Memo1.Gutter.Font.Color    := $00858585;   // dim line numbers
  Memo1.Gutter.DigitCount    := 4;

  { Dark theme — syntax colors (override highlighter defaults) }
  FLoxSyn.CommentAttri.Foreground    := $006A9955;  // muted green
  FLoxSyn.CommentAttri.Style         := [fsItalic];
  FLoxSyn.KeyAttri.Foreground        := $00C586C0;  // purple-pink (like VS Code keywords)
  FLoxSyn.KeyAttri.Style             := [fsBold];
  FLoxSyn.StringAttri.Foreground     := $00CE9178;  // warm orange-brown
  FLoxSyn.NumberAttri.Foreground     := $00B5CEA8;  // soft green
  FLoxSyn.BuiltInAttri.Foreground    := $00DCDCAA;  // golden-yellow (function calls)
  FLoxSyn.IdentifierAttri.Foreground := $009CDCFE;  // light blue (variables)
  FLoxSyn.SymbolAttri.Foreground     := $00D4D4D4;  // match default text
end;

procedure TForm4.FormDestroy(Sender: TObject);
var
  N: TTreeNode;
begin
  N := TestTree.Items.GetFirstNode;
  while N <> nil do
  begin
    if N.Data <> nil then
    begin
      StrDispose(PChar(N.Data));
      N.Data := nil;
    end;
    N := N.GetNext;
  end;
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  IR : TInterpretResult;
  txt : AnsiString;
begin
  Memo2.Lines.Clear;
  txt := AnsiString(Memo1.Lines.Text);
  IR := interpretResult(PAnsiChar(txt));

  case IR.code of
    INTERPRET_OK:
    begin
      if IR.OutputStr <> '' then
        Memo2.Lines.Add(IR.OutputStr);
      case IR.value.ValueKind of
        vkNumber:  Memo2.Lines.Add(IR.value.NumberValue.ToString);
        vkBoolean: Memo2.Lines.Add(BoolToStr(IR.value.BooleanValue, True));
        vkNull:    ; // suppress
        vkObject:  Memo2.Lines.Add(IR.ResultStr);
      end;
    end;
    INTERPRET_COMPILE_ERROR: Memo2.Lines.Add(IR.ErrorStr);
    INTERPRET_RUNTIME_ERROR: Memo2.Lines.Add(IR.ErrorStr);
  end;
end;

// ---- State image construction (checkbox + result icons) ----

procedure TForm4.BuildStateImages;
var
  bmp: TBitmap;

  procedure DrawBox(AColor: TColor; Fill: Boolean; Symbol: Char);
  begin
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
    bmp.Canvas.Pen.Color := clGray;
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.Rectangle(1, 1, 15, 15);
    if Fill then
    begin
      bmp.Canvas.Font.Color := AColor;
      bmp.Canvas.Font.Size := 8;
      bmp.Canvas.Font.Style := [fsBold];
      bmp.Canvas.TextOut(3, 0, Symbol);
    end;
    StateImages.Add(bmp, nil);
  end;

  procedure DrawCircle(AColor: TColor);
  begin
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
    bmp.Canvas.Pen.Color := AColor;
    bmp.Canvas.Brush.Color := AColor;
    bmp.Canvas.Ellipse(3, 3, 13, 13);
    StateImages.Add(bmp, nil);
  end;

begin
  StateImages.Clear;
  StateImages.Width := 16;
  StateImages.Height := 16;
  bmp := TBitmap.Create;
  try
    bmp.Width := 16;
    bmp.Height := 16;
    bmp.PixelFormat := pf24bit;

    DrawBox(clGray, False, ' ');      // 0: unchecked
    DrawBox(clBlue, True, #$2713);    // 1: checked (checkmark)
    DrawCircle(clGreen);              // 2: pass
    DrawCircle(clRed);                // 3: fail
    DrawCircle(clSilver);             // 4: skipped
  finally
    bmp.Free;
  end;
end;

// ---- Tree population ----

procedure TForm4.AddTestFiles(ParentNode: TTreeNode; const Dir, Pattern: string;
  Recurse: Boolean);
var
  Files: TStringDynArray;
  F, DirName: string;
  Node, SubNode: TTreeNode;
begin
  if not TDirectory.Exists(Dir) then Exit;

  // Add subdirectories first
  if Recurse then
  begin
    var SubDirs: TStringDynArray;
    SubDirs := TDirectory.GetDirectories(Dir);
    for F in SubDirs do
    begin
      DirName := TPath.GetFileName(F);
      SubNode := TestTree.Items.AddChild(ParentNode, DirName);
      SubNode.StateIndex := STATE_CHECKED;
      SubNode.Data := nil;
      AddTestFiles(SubNode, F, Pattern, True);
      // Remove empty dirs
      if SubNode.Count = 0 then
        SubNode.Free;
    end;
  end;

  // Add files
  Files := TDirectory.GetFiles(Dir, Pattern, TSearchOption.soTopDirectoryOnly);
  for F in Files do
  begin
    Node := TestTree.Items.AddChild(ParentNode, TPath.GetFileName(F));
    Node.StateIndex := STATE_CHECKED;
    Node.Data := StrNew(PChar(F));
  end;
end;

procedure TForm4.PopulateTestTree;
var
  BaseDir: string;
  RootNode: TTreeNode;
  N: TTreeNode;
begin
  // Free previously allocated string data
  N := TestTree.Items.GetFirstNode;
  while N <> nil do
  begin
    if N.Data <> nil then
    begin
      StrDispose(PChar(N.Data));
      N.Data := nil;
    end;
    N := N.GetNext;
  end;

  TestTree.Items.Clear;
  BuildStateImages;

  BaseDir := TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..');

  // Samples (recursive — picks up errors, introspection, etc.)
  RootNode := TestTree.Items.Add(nil, 'Samples');
  RootNode.StateIndex := STATE_CHECKED;
  RootNode.Data := nil;
  AddTestFiles(RootNode, TPath.Combine(BaseDir, 'samples'), '*.lox', True);

  // Official Tests (with subdirectories)
  RootNode := TestTree.Items.Add(nil, 'Official Tests');
  RootNode.StateIndex := STATE_CHECKED;
  RootNode.Data := nil;
  AddTestFiles(RootNode, TPath.Combine(BaseDir, 'test'), '*.lox', True);

  TestTree.FullExpand;
  if TestTree.Items.Count > 0 then
    TestTree.Items[0].MakeVisible;
end;

// ---- Checkbox toggle logic ----

procedure TForm4.ToggleCheck(Node: TTreeNode);
begin
  if Node = nil then Exit;
  if Node.StateIndex = STATE_CHECKED then
    SetAllChecked(Node, False)
  else
    SetAllChecked(Node, True);
  UpdateParentCheck(Node.Parent);
end;

procedure TForm4.SetAllChecked(Node: TTreeNode; Checked: Boolean);
var
  Child: TTreeNode;
begin
  if Node = nil then Exit;
  if Checked then
    Node.StateIndex := STATE_CHECKED
  else
    Node.StateIndex := STATE_UNCHECKED;
  Child := Node.getFirstChild;
  while Child <> nil do
  begin
    SetAllChecked(Child, Checked);
    Child := Child.getNextSibling;
  end;
end;

procedure TForm4.UpdateParentCheck(Node: TTreeNode);
var
  Child: TTreeNode;
  AllChecked: Boolean;
begin
  if Node = nil then Exit;
  AllChecked := True;
  Child := Node.getFirstChild;
  while Child <> nil do
  begin
    if Child.StateIndex <> STATE_CHECKED then
      AllChecked := False;
    Child := Child.getNextSibling;
  end;
  if AllChecked then
    Node.StateIndex := STATE_CHECKED
  else
    Node.StateIndex := STATE_UNCHECKED;
  UpdateParentCheck(Node.Parent);
end;

procedure TForm4.TestTreeClick(Sender: TObject);
var
  P: TPoint;
  HT: THitTests;
begin
  P := TestTree.ScreenToClient(Mouse.CursorPos);
  HT := TestTree.GetHitTestInfoAt(P.X, P.Y);
  if htOnStateIcon in HT then
    ToggleCheck(TestTree.Selected);
end;

procedure TForm4.CollectCheckedFiles(Node: TTreeNode; Files: TStringList);
var
  Child: TTreeNode;
begin
  if Node = nil then Exit;
  if Node.Data <> nil then
  begin
    if Node.StateIndex = STATE_CHECKED then
      Files.Add(String(PChar(Node.Data)));
  end;
  Child := Node.getFirstChild;
  while Child <> nil do
  begin
    CollectCheckedFiles(Child, Files);
    Child := Child.getNextSibling;
  end;
end;

function TForm4.CountChecked(Node: TTreeNode): Integer;
var
  Child: TTreeNode;
begin
  Result := 0;
  if Node = nil then Exit;
  if (Node.Data <> nil) and (Node.StateIndex = STATE_CHECKED) then
    Inc(Result);
  Child := Node.getFirstChild;
  while Child <> nil do
  begin
    Inc(Result, CountChecked(Child));
    Child := Child.getNextSibling;
  end;
end;

// ---- Test execution ----

function FindNodeByFilePath(Tree: TTreeView; const Path: string): TTreeNode;
var
  N: TTreeNode;
begin
  Result := nil;
  N := Tree.Items.GetFirstNode;
  while N <> nil do
  begin
    if (N.Data <> nil) and (String(PChar(N.Data)) = Path) then
      Exit(N);
    N := N.GetNext;
  end;
end;

procedure TForm4.RunTestFile(const FilePath: string; var Passed, Failed: Integer);
var
  Content, Line, ExpectedRuntimeError, Rest: string;
  Lines, ExpectedOutputs, ExpectedCompileErrors, ActualOutputs, Failures: TStringList;
  IR: TInterpretResult;
  i, LineNum, p, BracketEnd: Integer;
  HasOutput, HasRuntimeError, HasCompileErrors, TestOK: Boolean;
begin
  Content := TFile.ReadAllText(FilePath);

  Lines := TStringList.Create;
  ExpectedOutputs := TStringList.Create;
  ExpectedCompileErrors := TStringList.Create;
  ActualOutputs := TStringList.Create;
  Failures := TStringList.Create;
  try
    Lines.Text := Content;
    ExpectedRuntimeError := '';

    // --- Parse expectations ---
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      LineNum := i + 1;

      if Pos('// [java', Line) > 0 then Continue;

      p := Pos('// expect runtime error: ', Line);
      if p > 0 then begin ExpectedRuntimeError := Copy(Line, p + 25, MaxInt); Continue; end;

      p := Pos('// expect: ', Line);
      if p > 0 then begin ExpectedOutputs.Add(Copy(Line, p + 11, MaxInt)); Continue; end;

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

      p := Pos('// Error at', Line);
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
      // No expectations — just needs to not crash
      try
        IR := InterpretResult(PAnsiChar(AnsiString(Content)));
        if IR.code = INTERPRET_OK then
          Inc(Passed)
        else
        begin
          Inc(Failed);
          Memo2.Lines.Add('  FAIL: ' + IR.ErrorStr);
        end;
      except
        on E: Exception do
        begin
          Inc(Failed);
          Memo2.Lines.Add('  CRASH: ' + E.Message);
        end;
      end;
      Exit;
    end;

    // --- Run the test ---
    TestOK := True;
    try
      IR := InterpretResult(PAnsiChar(AnsiString(Content)));

      if HasCompileErrors then
      begin
        if IR.code <> INTERPRET_COMPILE_ERROR then
        begin
          Failures.Add('Expected compile error but got ' +
            IfThen(IR.code = INTERPRET_OK, 'OK', 'runtime error'));
          TestOK := False;
        end
        else
          for i := 0 to ExpectedCompileErrors.Count - 1 do
            if Pos(ExpectedCompileErrors[i], IR.ErrorStr) = 0 then
            begin
              Failures.Add('Missing compile error: ' + ExpectedCompileErrors[i]);
              TestOK := False;
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
        else if Pos(Trim(ExpectedRuntimeError), Trim(IR.ErrorStr)) = 0 then
        begin
          Failures.Add('Expected error "' + ExpectedRuntimeError +
            '" but got "' + IR.ErrorStr + '"');
          TestOK := False;
        end;

        if HasOutput then
        begin
          ActualOutputs.Clear;
          if IR.OutputStr <> '' then ActualOutputs.Text := IR.OutputStr;
          if (ActualOutputs.Count > 0) and (ActualOutputs[ActualOutputs.Count - 1] = '') then
            ActualOutputs.Delete(ActualOutputs.Count - 1);
          if ActualOutputs.Count <> ExpectedOutputs.Count then
          begin
            Failures.Add(Format('Expected %d output lines but got %d',
              [ExpectedOutputs.Count, ActualOutputs.Count]));
            TestOK := False;
          end
          else
            for i := 0 to ExpectedOutputs.Count - 1 do
              if ActualOutputs[i] <> ExpectedOutputs[i] then
              begin
                Failures.Add(Format('Line %d: expected "%s" got "%s"',
                  [i + 1, ExpectedOutputs[i], ActualOutputs[i]]));
                TestOK := False;
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
          if IR.OutputStr <> '' then ActualOutputs.Text := IR.OutputStr;
          if (ActualOutputs.Count > 0) and (ActualOutputs[ActualOutputs.Count - 1] = '') then
            ActualOutputs.Delete(ActualOutputs.Count - 1);
          if ActualOutputs.Count <> ExpectedOutputs.Count then
          begin
            Failures.Add(Format('Expected %d output lines but got %d',
              [ExpectedOutputs.Count, ActualOutputs.Count]));
            TestOK := False;
          end
          else
            for i := 0 to ExpectedOutputs.Count - 1 do
              if ActualOutputs[i] <> ExpectedOutputs[i] then
              begin
                Failures.Add(Format('Line %d: expected "%s" got "%s"',
                  [i + 1, ExpectedOutputs[i], ActualOutputs[i]]));
                TestOK := False;
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
      for i := 0 to Failures.Count - 1 do
        Memo2.Lines.Add('  ' + Failures[i]);
    end;
  finally
    Lines.Free;
    ExpectedOutputs.Free;
    ExpectedCompileErrors.Free;
    ActualOutputs.Free;
    Failures.Free;
  end;
end;

procedure TForm4.RunRttiTestFile(const FilePath: string; var Passed, Failed: Integer);
var
  Content: string;
  cust: TCustomer;
  addr2: TAddress;
  IR: TInterpretResult;
begin
  Content := TFile.ReadAllText(FilePath);

  cust := TCustomer.Create('Alice', 30, 1500.50, True);
  try
    cust.Address := TAddress.Create('123 Main St', 'Springfield', '62704');
    cust.Tier := ctGold;
    cust.Flags := [cfNewsletter, cfVIP];
    cust.Tags := TArray<string>.Create('loyal', 'premium', 'early-adopter');
    addr2 := TAddress.Create('456 Oak Ave', 'Shelbyville', '62705');

    InitVM;
    try
      InjectObject('cust', cust);
      InjectObject('addr2', addr2);
      IR := CompileAndRun(PAnsiChar(AnsiString(Content)));
    finally
      FreeVM;
    end;

    if CheckExpectedOutput(Content, IR, Memo2) then
      Inc(Passed)
    else
    begin
      Inc(Failed);
      Memo2.Lines.Add('  output mismatch');
    end;
  finally
    if cust.Address <> addr2 then
      addr2.Free;
    cust.Free;
  end;
end;

// ---- Button handlers ----

procedure TForm4.BtnPopulateClick(Sender: TObject);
begin
  PopulateTestTree;
  LblStatus.Caption := 'Tests loaded.';
end;

procedure TForm4.BtnCheckAllClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TestTree.Items.GetFirstNode;
  while Node <> nil do
  begin
    SetAllChecked(Node, True);
    Node := Node.getNextSibling;
  end;
end;

procedure TForm4.BtnUncheckAllClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TestTree.Items.GetFirstNode;
  while Node <> nil do
  begin
    SetAllChecked(Node, False);
    Node := Node.getNextSibling;
  end;
end;

procedure TForm4.BtnRunAllClick(Sender: TObject);
begin
  BtnCheckAllClick(nil);
  BtnRunSelectedClick(nil);
end;

procedure TForm4.BtnRunSelectedClick(Sender: TObject);
var
  Files: TStringList;
  F, FileName: string;
  Passed, Failed, Total, Done: Integer;
  Node: TTreeNode;
  IsRtti, IsInject: Boolean;
  p1, f1: Integer;
  Content: string;
  sl: TStringList;
  IR: TInterpretResult;
begin
  Memo2.Lines.Clear;

  if TestTree.Items.Count = 0 then
    PopulateTestTree;

  Files := TStringList.Create;
  try
    Node := TestTree.Items.GetFirstNode;
    while Node <> nil do
    begin
      CollectCheckedFiles(Node, Files);
      Node := Node.getNextSibling;
    end;

    Total := Files.Count;
    if Total = 0 then
    begin
      LblStatus.Caption := 'No tests selected.';
      Exit;
    end;

    ProgressBar1.Min := 0;
    ProgressBar1.Max := Total;
    ProgressBar1.Position := 0;
    Passed := 0;
    Failed := 0;
    Done := 0;

    for F in Files do
    begin
      FileName := TPath.GetFileName(F);
      IsRtti := Pos('inject_rtti', LowerCase(FileName)) > 0;
      IsInject := (Pos('inject_', LowerCase(FileName)) > 0) and not IsRtti;

      Memo2.Lines.Add('Running: ' + FileName);

      p1 := 0;
      f1 := 0;

      try
        if IsInject then
        begin
          // inject_stringlist test
          if Pos('inject_stringlist', LowerCase(FileName)) > 0 then
          begin
            Content := TFile.ReadAllText(F);
            sl := TStringList.Create;
            try
              sl.Add('Alice'); sl.Add('Bob'); sl.Add('Charlie');
              InitVM;
              try
                InjectNativeObject('items', Pointer(sl), 'StringList');
                IR := CompileAndRun(PAnsiChar(AnsiString(Content)));
              finally
                FreeVM;
              end;
              if CheckExpectedOutput(Content, IR, Memo2) then
                p1 := 1
              else
                f1 := 1;
            finally
              sl.Free;
            end;
          end;
        end
        else if IsRtti then
          RunRttiTestFile(F, p1, f1)
        else
          RunTestFile(F, p1, f1);
      except
        on E: Exception do
        begin
          f1 := 1;
          Memo2.Lines.Add('  CRASH: ' + E.Message);
        end;
      end;

      Inc(Passed, p1);
      Inc(Failed, f1);

      Node := FindNodeByFilePath(TestTree, F);
      if Node <> nil then
      begin
        if f1 = 0 then Node.StateIndex := STATE_PASS
        else Node.StateIndex := STATE_FAIL;
      end;

      if f1 > 0 then
        Memo2.Lines.Add('FAIL: ' + FileName);

      Inc(Done);
      ProgressBar1.Position := Done;
      LblStatus.Caption := Format('Running %d / %d ...', [Done, Total]);
      Application.ProcessMessages;
    end;

    Memo2.Lines.Add('');
    if Failed = 0 then
      LblStatus.Caption := Format('All %d tests passed.', [Passed])
    else
      LblStatus.Caption := Format('%d FAILED, %d passed of %d total.',
        [Failed, Passed, Passed + Failed]);
    Memo2.Lines.Add(LblStatus.Caption);
  finally
    Files.Free;
  end;
end;

end.
