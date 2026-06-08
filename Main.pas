unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ImgList,
  SynEdit, SynEditTypes, SynHighlighterLox,
  Chunk_Types,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Intf,
  FireDAC.Stan.Async, FireDAC.Phys.MSSQL, FireDAC.DApt, FireDAC.VCLUI.Wait,
  System.ImageList,NativeObjects,
  System.Generics.Collections,
  Vcl.Themes, Vcl.Styles;

type

  TForm4 = class(TForm)
    PanelLeft: TPanel;
    Label2: TLabel;
    TestTree: TTreeView;
    BtnPopulate: TButton;
    BtnRunSelected: TButton;
    BtnCheckAll: TButton;
    BtnUncheckAll: TButton;
    BtnRunAll: TButton;
    ProgressBar1: TProgressBar;
    LblStatus: TLabel;
    StateImages: TImageList;
    PanelRight: TPanel;
    Memo1: TSynEdit;
    PanelToolbar: TPanel;
    Button1: TButton;
    BtnRunGame: TButton;
    Button2: TButton;
    Button3: TButton;
    SplitterOutput: TSplitter;
    Memo2: TMemo;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure BtnRunGameClick(Sender: TObject);
    procedure BtnPopulateClick(Sender: TObject);
    procedure BtnRunSelectedClick(Sender: TObject);
    procedure BtnCheckAllClick(Sender: TObject);
    procedure BtnUncheckAllClick(Sender: TObject);
    procedure BtnRunAllClick(Sender: TObject);
    procedure TestTreeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
    FScriptRunning: Boolean;
    FStyleCombo: TComboBox;
    procedure LoadStyles;
    procedure StyleComboChange(Sender: TObject);
  public
    FClosing: Boolean;
    procedure HandleLivePrint(const Text: string);
  end;

var
  Form4: TForm4;

implementation
uses
  NativeObjectTestUnit, IOUtils, Types, StrUtils, Math, fmGame, fmEventTest,
  LoxEventEngine;

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

procedure TForm4.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // F5 is a global shortcut (works regardless of focus) for Run.
  if Key = VK_F5 then
  begin
    Key := 0;
    Button1Click(nil);
    Exit;
  end;
  // All gameplay key handling lives on the TLoxGameCanvas component.
  // When the canvas has focus, KeyPreview routes events here first; we
  // simply leave Key untouched so the canvas sees and consumes them.
  // When other controls (editor, etc.) have focus, gameplay keys must
  // NOT be hijacked — they belong to that control.
end;

procedure TForm4.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // See FormKeyDown — no form-level handling for gameplay keys.
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

  // The game window owns the canvas, the input-event queue, and the
  // game-input plumbing. Form4 is created first by the .dpr so the
  // game form may not exist yet — construct it now if needed. Its
  // OnCreate handler sets up everything else.
  if frmGame = nil then
    frmGame := TfrmGame.Create(Application);

  LoadStyles;
end;

procedure TForm4.LoadStyles;
var
  StyleDir: string;
  Files: TStringDynArray;
  F, StyleName: string;
begin
  // Create style combo on the toolbar
  FStyleCombo := TComboBox.Create(Self);
  FStyleCombo.Parent := PanelToolbar;
  FStyleCombo.Style := csDropDownList;
  FStyleCombo.Left := PanelToolbar.Width - 180;
  FStyleCombo.Top := 4;
  FStyleCombo.Width := 170;
  FStyleCombo.Anchors := [akTop, akRight];
  FStyleCombo.OnChange := StyleComboChange;

  // Add default (no style) entry
  FStyleCombo.Items.Add('Windows (Default)');

  // Load .vsf files from styles/ folder relative to EXE
  StyleDir := TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\styles');
  if not TDirectory.Exists(StyleDir) then
    StyleDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'styles');

  if TDirectory.Exists(StyleDir) then
  begin
    Files := TDirectory.GetFiles(StyleDir, '*.vsf');
    for F in Files do
    begin
      try
        if TStyleManager.IsValidStyle(F) then
          TStyleManager.LoadFromFile(F);
      except
        // Skip invalid style files silently
      end;
    end;
  end;

  // Populate combo from registered style names
  for StyleName in TStyleManager.StyleNames do
    if not SameText(StyleName, 'Windows') then
      FStyleCombo.Items.Add(StyleName);

  FStyleCombo.ItemIndex := 0;
end;

procedure TForm4.StyleComboChange(Sender: TObject);
var
  SelectedStyle: string;
begin
  if FStyleCombo.ItemIndex <= 0 then
    TStyleManager.SetStyle(TStyleManager.SystemStyle)
  else
  begin
    SelectedStyle := FStyleCombo.Items[FStyleCombo.ItemIndex];
    TStyleManager.SetStyle(SelectedStyle);
  end;
end;

procedure TForm4.FormDestroy(Sender: TObject);
var
  N: TTreeNode;
begin
  // Canvas + event queue are owned by frmGame and freed in its
  // FormDestroy.
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
  FScriptRunning := True;
  Button1.Enabled := False;
  BtnRunGame.Enabled := False;
  BtnRunSelected.Enabled := False;
  BtnRunAll.Enabled := False;
  Memo1.ReadOnly := True;
  try
    // Run without game window — no canvas/sound/events, just the core VM.
    InitVM;
    try
      VM.OnPrint := HandleLivePrint;
      IR := CompileAndRun(PAnsiChar(txt));
    finally
      FreeVM;
    end;
  finally
    FScriptRunning := False;
    Button1.Enabled := True;
    BtnRunGame.Enabled := True;
    BtnRunSelected.Enabled := True;
    BtnRunAll.Enabled := True;
    Memo1.ReadOnly := False;
  end;

  case IR.code of
    INTERPRET_OK:
    begin
      if isNumber(IR.value) then
        Memo2.Lines.Add(FloatToStr(GetNumber(IR.value)))
      else if isBoolean(IR.value) then
        Memo2.Lines.Add(BoolToStr(GetBoolean(IR.value), True))
      else if isNill(IR.value) then
      begin
        // suppress
      end
      else if isObject(IR.value) then
        Memo2.Lines.Add(IR.ResultStr);
    end;
    INTERPRET_COMPILE_ERROR: Memo2.Lines.Add(IR.ErrorStr);
    INTERPRET_RUNTIME_ERROR: Memo2.Lines.Add(IR.ErrorStr);
    INTERPRET_HALTED: Memo2.Lines.Add('Script halted (exit code ' + IntToStr(IR.ExitCode) + '): ' + IR.ErrorStr);
  end;

  if FClosing then
    Close;
end;

procedure TForm4.BtnRunGameClick(Sender: TObject);
var
  IR : TInterpretResult;
  txt : AnsiString;
begin
  Memo2.Lines.Clear;
  txt := AnsiString(Memo1.Lines.Text);
  FScriptRunning := True;
  Button1.Enabled := False;
  BtnRunGame.Enabled := False;
  BtnRunSelected.Enabled := False;
  BtnRunAll.Enabled := False;
  Memo1.ReadOnly := True;
  try
    // The game form owns the canvas, the input-event queue, and the
    // whole VM lifecycle. It shows itself, sets up natives, runs the
    // script to completion, then tears the VM down again.
    IR := frmGame.RunScript(txt);
  finally
    FScriptRunning := False;
    Button1.Enabled := True;
    BtnRunGame.Enabled := True;
    BtnRunSelected.Enabled := True;
    BtnRunAll.Enabled := True;
    Memo1.ReadOnly := False;
  end;

  case IR.code of
    INTERPRET_OK:
    begin
      if isNumber(IR.value) then
        Memo2.Lines.Add(FloatToStr(GetNumber(IR.value)))
      else if isBoolean(IR.value) then
        Memo2.Lines.Add(BoolToStr(GetBoolean(IR.value), True))
      else if isNill(IR.value) then
      begin
        // suppress
      end
      else if isObject(IR.value) then
        Memo2.Lines.Add(IR.ResultStr);
    end;
    INTERPRET_COMPILE_ERROR: Memo2.Lines.Add(IR.ErrorStr);
    INTERPRET_RUNTIME_ERROR: Memo2.Lines.Add(IR.ErrorStr);
    INTERPRET_HALTED: Memo2.Lines.Add('Script halted (exit code ' + IntToStr(IR.ExitCode) + '): ' + IR.ErrorStr);
  end;

  if FClosing then
    Close;
end;

procedure TForm4.HandleLivePrint(const Text: string);
var
  Normalized: string;
  Lines: TArray<string>;
  i: Integer;
begin
  // Normalize all line-ending variants to LF, then split for display
  Normalized := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
  Normalized := StringReplace(Normalized, #13, #10, [rfReplaceAll]);
  Lines := Normalized.Split([#10]);
  for i := 0 to High(Lines) do
    Memo2.Lines.Add(Lines[i]);
  Memo2.Update;
  Application.ProcessMessages;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  frmGame.FEventQueue.Enqueue('click:TestButton');


end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  frmGame.FEventQueue.Enqueue('keydown:escape');
end;

procedure TForm4.Button4Click(Sender: TObject);
var
  IR: TInterpretResult;
  ScriptText: AnsiString;
begin
  Memo2.Lines.Clear;

  ScriptText := AnsiString(Memo1.Lines.Text);

  if Length(Trim(string(ScriptText))) = 0 then
  begin
    Memo2.Lines.Add('No script to run. Enter code in the editor first.');
    Exit;
  end;

  if frmEventTest = nil then
    frmEventTest := TfrmEventTest.Create(Self);
  FScriptRunning := True;
  Button4.Enabled := False;
  try
    IR := frmEventTest.RunScript(ScriptText);
  finally
    FScriptRunning := False;
    Button4.Enabled := True;
  end;
  case IR.code of
    INTERPRET_OK:
      Memo2.Lines.Add('== Script finished OK ==');
    INTERPRET_RUNTIME_ERROR:
      Memo2.Lines.Add('Runtime error: ' + IR.ErrorStr);
    INTERPRET_COMPILE_ERROR:
      Memo2.Lines.Add('Compile error: ' + IR.ErrorStr);
  end;
end;

procedure TForm4.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FScriptRunning then
  begin
    FClosing := True;
    CanClose := False;  // let the script abort first, then close
  end
  else
    CanClose := True;
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

  procedure DrawTick;
  begin
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
    bmp.Canvas.Pen.Color := clGreen;
    bmp.Canvas.Pen.Width := 2;
    // Draw a checkmark: short stroke down-right, then long stroke up-right
    bmp.Canvas.MoveTo(2, 8);
    bmp.Canvas.LineTo(6, 12);
    bmp.Canvas.LineTo(14, 3);
    bmp.Canvas.Pen.Width := 1;
    StateImages.Add(bmp, nil);
  end;

  procedure DrawCross;
  begin
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
    bmp.Canvas.Pen.Color := clRed;
    bmp.Canvas.Pen.Width := 2;
    // Draw an X
    bmp.Canvas.MoveTo(3, 3);
    bmp.Canvas.LineTo(13, 13);
    bmp.Canvas.MoveTo(13, 3);
    bmp.Canvas.LineTo(3, 13);
    bmp.Canvas.Pen.Width := 1;
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

    // Index 0: blank (StateIndex 0 = no visible state)
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
    StateImages.Add(bmp, nil);

    DrawBox(clGray, False, ' ');      // 1: unchecked  (STATE_UNCHECKED)
    DrawBox(clBlue, True, #$2713);    // 2: checked    (STATE_CHECKED)
    DrawTick;                          // 3: pass       (STATE_PASS)
    DrawCross;                         // 4: fail       (STATE_FAIL)
    DrawCircle(clSilver);             // 5: skipped
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
      if DirName = 'demos' then Continue;  // skip interactive demos
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
  Node: TTreeNode;
  FilePath: string;
  i: Integer;
  FileContent: TStringList;
begin
  P := TestTree.ScreenToClient(Mouse.CursorPos);
  HT := TestTree.GetHitTestInfoAt(P.X, P.Y);
  if htOnStateIcon in HT then
  begin
    Node := TestTree.GetNodeAt(P.X, P.Y);
    ToggleCheck(Node);
  end
  else if TestTree.SelectionCount > 1 then
  begin
    // Multi-select: concatenate all selected files into editor
    FileContent := TStringList.Create;
    try
      for i := 0 to TestTree.SelectionCount - 1 do
      begin
        Node := TestTree.Selections[i];
        if (Node <> nil) and (Node.Data <> nil) then
        begin
          FilePath := String(PChar(Node.Data));
          if FileExists(FilePath) then
          begin
            if FileContent.Count > 0 then
            begin
              FileContent.Add('');
              FileContent.Add('// ' + StringOfChar('=', 70));
              FileContent.Add('// FILE: ' + ExtractFileName(FilePath));
              FileContent.Add('// ' + StringOfChar('=', 70));
              FileContent.Add('');
            end
            else
              FileContent.Add('// FILE: ' + ExtractFileName(FilePath));
            FileContent.AddStrings(TFile.ReadAllLines(FilePath));
          end;
        end;
      end;
      Memo1.Lines.Assign(FileContent);
    finally
      FileContent.Free;
    end;
  end
  else
  begin
    Node := TestTree.Selected;
    if (Node <> nil) and (Node.Data <> nil) then
    begin
      FilePath := String(PChar(Node.Data));
      if FileExists(FilePath) then
        Memo1.Lines.LoadFromFile(FilePath);
    end;
  end;
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
    if cust.Address <> addr2 then  //cust now owns the address-- not ideal but simple enough for testing
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
        if Pos('\events\', LowerCase(F)) > 0 then
        begin
          // Event engine test — run with engine active
          Content := TFile.ReadAllText(F);
          InitVM;
          try
            var Engine := TLoxEventEngine.Create;
            try
              Engine.RegisterNatives;
              Engine.RegisterGCRoots;
              IR := CompileAndRun(PAnsiChar(AnsiString(Content)));
              Engine.UnregisterGCRoots;
            finally
              Engine.Free;
            end;
          finally
            FreeVM;
          end;
          if CheckExpectedOutput(Content, IR, Memo2) then
            p1 := 1
          else
            f1 := 1;
        end
        else if IsInject then
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
