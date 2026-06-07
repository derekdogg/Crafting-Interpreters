unit fmEventTest;

// ============================================================
// Lightweight test form for event-driven callback processing.
// Uses TLoxEventEngine for reusable event dispatch.
// ============================================================

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Chunk_Types, LoxEventEngine;

type
  TfrmEventTest = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1Click(Sender: TObject);
  private
    FEngine: TLoxEventEngine;
    procedure Log(const Msg: string);
  public
    function RunScript(const Source: AnsiString): TInterpretResult;
  end;

var
  frmEventTest: TfrmEventTest;

implementation

uses
  Main;

{$R *.dfm}

// --- Form implementation ---

procedure TfrmEventTest.FormCreate(Sender: TObject);
begin
  FEngine := TLoxEventEngine.Create;
  FEngine.OnLog := Log;
end;

procedure TfrmEventTest.FormDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

procedure TfrmEventTest.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FEngine.Running then
  begin
    FEngine.Running := False;
    CanClose := False;
    Hide;
  end
  else
    CanClose := True;
end;

procedure TfrmEventTest.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FEngine.QueueKeyDown(TLoxEventEngine.MapVKToName(Key));
end;

procedure TfrmEventTest.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FEngine.QueueKeyUp(TLoxEventEngine.MapVKToName(Key));
end;

procedure TfrmEventTest.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  btn: Integer;
begin
  case Button of
    mbLeft:   btn := 0;
    mbRight:  btn := 1;
    mbMiddle: btn := 2;
  else btn := -1;
  end;
  if btn >= 0 then
    FEngine.QueueMouseDown(btn, X, Y);
end;

procedure TfrmEventTest.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  btn: Integer;
begin
  case Button of
    mbLeft:   btn := 0;
    mbRight:  btn := 1;
    mbMiddle: btn := 2;
  else btn := -1;
  end;
  if btn >= 0 then
    FEngine.QueueMouseUp(btn, X, Y);
end;

procedure TfrmEventTest.Panel1Click(Sender: TObject);
begin
  // Ensure focus stays on the form for key events
end;

procedure TfrmEventTest.Log(const Msg: string);
begin
  Memo1.Lines.Add(Msg);
end;

function TfrmEventTest.RunScript(const Source: AnsiString): TInterpretResult;
begin
  Memo1.Lines.Clear;
  FEngine.Reset;
  FEngine.OnCheckAbort := function: Boolean
    begin
      Result := Form4.FClosing;
    end;

  Show;
  BringToFront;

  InitVM;
  try
    VM.OnPrint := Form4.HandleLivePrint;

    // Register event natives (includes processEvents)
    FEngine.RegisterNatives;

    // GC-root the callback slots
    FEngine.RegisterGCRoots;

    Result := CompileAndRun(PAnsiChar(Source));

    // Flush final output
    FEngine.FlushOutput;
  finally
    FEngine.UnregisterGCRoots;
    FreeVM;
    FEngine.Running := False;
  end;
end;

end.
