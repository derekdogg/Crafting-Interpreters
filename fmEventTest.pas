unit fmEventTest;

// ============================================================
// Lightweight test form for event-driven callback processing.
// Uses TLoxEventEngine for reusable event dispatch.
// Hosts a TLoxGameCanvas for rendering + canvas natives.
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
    PanelButtons: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Panel1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FEngine: TLoxEventEngine;
    procedure Log(const Msg: string);
    procedure CanvasKeyDown(Sender: TObject; const KeyName: string);
    procedure CanvasKeyUp(Sender: TObject; const KeyName: string);
    procedure CanvasMouseDown(Sender: TObject; Button, LX, LY: Integer);
    procedure CanvasMouseUp(Sender: TObject; Button, LX, LY: Integer);
    procedure CanvasMouseMove(Sender: TObject; Button, LX, LY: Integer);
  public
    function RunScript(const Source: AnsiString): TInterpretResult;
  end;

var
  frmEventTest: TfrmEventTest;

implementation

uses
  LoxCanvas, LoxSound, Main;

{$R *.dfm}

// --- Form implementation ---

procedure TfrmEventTest.FormCreate(Sender: TObject);
begin
  FEngine := TLoxEventEngine.Create;
  FEngine.OnLog := Log;
  // Game-style auto-repeat for the interactive event-test form: a tap shorter
  // than 250ms fires zero onKeyHeld events; holding past that streams them at
  // ~20Hz. Tests leave the defaults (every frame, no delay).
  FEngine.HeldDispatchInitialDelayMs := 250;
  FEngine.HeldDispatchIntervalMs := 50;
  Panel1.OnMouseMove := Panel1MouseMove;
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
    FEngine.QueueMouseDown(btn, X, Y, 'panel1');
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
    FEngine.QueueMouseUp(btn, X, Y, 'panel1');
end;

procedure TfrmEventTest.Panel1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  FEngine.QueueMouseMove(X, Y, 'panel1');
end;

procedure TfrmEventTest.Panel1Click(Sender: TObject);
begin
  // Ensure focus stays on the form for key events
end;

procedure TfrmEventTest.Button1Click(Sender: TObject);
begin
  FEngine.QueueClick('button1');
end;

procedure TfrmEventTest.Button2Click(Sender: TObject);
begin
  FEngine.QueueClick('button2');
end;

// --- Canvas event handlers (forwarded to the event engine) ---

procedure TfrmEventTest.CanvasKeyDown(Sender: TObject; const KeyName: string);
begin
  FEngine.QueueKeyDown(KeyName);
end;

procedure TfrmEventTest.CanvasKeyUp(Sender: TObject; const KeyName: string);
begin
  FEngine.QueueKeyUp(KeyName);
end;

procedure TfrmEventTest.CanvasMouseDown(Sender: TObject; Button, LX, LY: Integer);
begin
  FEngine.QueueMouseDown(Button, LX, LY);
end;

procedure TfrmEventTest.CanvasMouseUp(Sender: TObject; Button, LX, LY: Integer);
begin
  FEngine.QueueMouseUp(Button, LX, LY);
end;

procedure TfrmEventTest.CanvasMouseMove(Sender: TObject; Button, LX, LY: Integer);
begin
  FEngine.QueueMouseMove(LX, LY);
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

  // Create the canvas parented to Panel1
  InitCanvas(Panel1);
  // Note: do NOT wire GameCanvas.OnGameKeyDown/OnGameKeyUp here. The form has
  // KeyPreview=True and FormKeyDown/FormKeyUp already queue every keystroke;
  // wiring the canvas as well would double-queue each key event.
  GameCanvas.OnGameMouseDown := CanvasMouseDown;
  GameCanvas.OnGameMouseUp := CanvasMouseUp;
  GameCanvas.OnGameMouseMove := CanvasMouseMove;

  if GameCanvas.CanFocus then
    GameCanvas.SetFocus;

  InitVM;
  try
    VM.OnPrint := Form4.HandleLivePrint;

    // Register canvas + sound + event natives
    RegisterCanvasNatives;
    RegisterSoundNatives;
    FEngine.RegisterNatives;
    FEngine.RegisterGCRoots;

    Result := CompileAndRun(PAnsiChar(Source));

    // Flush final output
    FEngine.FlushOutput;
  finally
    FEngine.UnregisterGCRoots;
    FreeVM;
    StopAllSound;
    FreeCanvas;
    FEngine.Running := False;
  end;
end;

end.
