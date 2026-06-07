unit fmGame;

// ============================================================
// Game window. Hosts the TLoxGameCanvas for every running script.
// The canvas is created and parented here (alClient) in FormCreate and
// the form's client dimensions drive the LOGICAL_W / LOGICAL_H of the
// canvas. Resize the form before pressing Run to change resolution.
//
// This form also owns the per-session input-event plumbing that the
// running script consumes via the injected 'events' LoxQueue:
//   * FEventQueue          - the LoxQueue handed to the VM as 'events'
//   * FKeyDownCache/Up     - lazy 'keydown:<name>' / 'keyup:<name>' caches
//   * GameCanvas* handlers - stringify canvas input -> FEventQueue
// Form4 (the editor) reads these fields when starting/stopping a script
// but no longer creates or owns the canvas itself.
// ============================================================

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Generics.Collections,
  Chunk_Types,
  NativeObjects,
  LoxEventEngine;

type
  TfrmGame = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FScriptRunning: Boolean;
    FEngine: TLoxEventEngine;
    procedure GameCanvasKeyDown(Sender: TObject; const KeyName: string);
    procedure GameCanvasKeyUp(Sender: TObject; const KeyName: string);
    procedure GameCanvasMouseDown(Sender: TObject; Button, LX, LY: Integer);
    procedure GameCanvasMouseUp(Sender: TObject; Button, LX, LY: Integer);
    procedure GameCanvasMouseMove(Sender: TObject; Button, LX, LY: Integer);
    procedure EngineLog(const Msg: string);
  public
    // Input-event queue handed to the VM as the 'events' native object.
    FEventQueue: TLoxQueue;
    // Lazy caches mapping a key name ('left', 'a', ...) to its fully
    // formed event string ('keydown:left'). Built on first miss so we
    // pay the concat allocation once per (key, direction) pair instead
    // of once per keystroke.
    FKeyDownCache: TDictionary<string, string>;
    FKeyUpCache: TDictionary<string, string>;
    // Set by CloseQuery when the user tries to close the game window
    // mid-script. processMessagesNative observes it and raises a
    // runtime error so the Lox while-loop unwinds cleanly.
    FAbortScript: Boolean;
    function CloseQuery: Boolean; override;
    procedure NotifyScriptStarted;
    procedure NotifyScriptStopped;
    // Show the game window, set up the VM with the standard set of
    // natives (processMessages, canvas, sound, the 'events' LoxQueue),
    // compile and run Source, then tear the VM down again. The caller
    // (typically the editor's Run button) is responsible for any UI
    // around the call — disabling buttons, displaying IR output, etc.
    function RunScript(const Source: AnsiString): TInterpretResult;
  end;

var
  frmGame: TfrmGame;

implementation

uses
  LoxCanvas, LoxSound, Main;

{$R *.dfm}

function processMessagesNative(argCount: integer; args: pValue): TValue;
begin
  // Flush any pending print output to the output memo.
  if VM.PrintBuilder.Length > 0 then
  begin
    Form4.Memo2.Lines.Add(VM.PrintBuilder.ToString);
    VM.PrintBuilder.Clear;
  end;  // Wipe last frame's edge-trigger state BEFORE pumping. Any
  // up->down transitions delivered by the pump below will repopulate
  // FKeysPressed / FMouseBtnClicked so the script's keyPressed() /
  // mouseClicked() polls see only this frame's new presses.
  ClearFrameEdges;  Application.ProcessMessages;
  if Form4.FClosing or frmGame.FAbortScript then    //TODO - this feels a bit shonky tbh
  begin
    if frmGame.FAbortScript then
      RuntimeError('Script aborted: game window closed.')
    else
      RuntimeError('Script aborted: application closing.');
    Exit(CreateNilValue);
  end;
  Result := CreateNilValue;
end;

function TfrmGame.RunScript(const Source: AnsiString): TInterpretResult;
begin
  NotifyScriptStarted;
  FEngine.Reset;
  FEngine.OnCheckAbort := function: Boolean
    begin
      Result := Form4.FClosing or FAbortScript;
    end;

  // Ensure canvas is parented here (fmEventTest may have freed it)
  if GameCanvas = nil then
  begin
    InitCanvas(Self);
    GameCanvas.OnGameKeyDown := GameCanvasKeyDown;
    GameCanvas.OnGameKeyUp := GameCanvasKeyUp;
    GameCanvas.OnGameMouseDown := GameCanvasMouseDown;
    GameCanvas.OnGameMouseUp := GameCanvasMouseUp;
    GameCanvas.OnGameMouseMove := GameCanvasMouseMove;
  end;

  InitVM;
  try
    VM.OnPrint := Form4.HandleLivePrint;
    defineNative('processMessages', processMessagesNative, 0);
    RegisterCanvasNatives;
    RegisterSoundNatives;

    // Event engine natives (onKeyPressed, onKeyHeld, processEvents, etc.)
    FEngine.RegisterNatives;
    FEngine.RegisterGCRoots;

    registerNativeClassRTTI('LoxQueue', TLoxQueue);
    InjectNativeObject('events', Pointer(FEventQueue), 'LoxQueue');
    Result := CompileAndRun(PAnsiChar(Source));
  finally
    FEngine.UnregisterGCRoots;
    FreeVM;
    StopAllSound;
    NotifyScriptStopped;
  end;
end;

procedure TfrmGame.FormCreate(Sender: TObject);
begin
  FEventQueue := TLoxQueue.Create;
  FKeyDownCache := TDictionary<string, string>.Create;
  FKeyUpCache := TDictionary<string, string>.Create;
  FEngine := TLoxEventEngine.Create;
  FEngine.OnLog := EngineLog;

  // Build the game canvas parented to this form.
  InitCanvas(Self);

  // Hook canvas events for both the legacy queue AND the event engine
  GameCanvas.OnGameKeyDown := GameCanvasKeyDown;
  GameCanvas.OnGameKeyUp := GameCanvasKeyUp;
  GameCanvas.OnGameMouseDown := GameCanvasMouseDown;
  GameCanvas.OnGameMouseUp := GameCanvasMouseUp;
  GameCanvas.OnGameMouseMove := GameCanvasMouseMove;
end;

procedure TfrmGame.FormDestroy(Sender: TObject);
begin
  FreeCanvas;
  FEventQueue.Free;
  FKeyDownCache.Free;
  FKeyUpCache.Free;
  FEngine.Free;
end;

procedure TfrmGame.FormShow(Sender: TObject);
begin
  if (GameCanvas <> nil) and GameCanvas.CanFocus then
    GameCanvas.SetFocus;
end;

procedure TfrmGame.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Most of the work happens in the CloseQuery override below; this
  // published handler is kept so the DFM hookup stays valid.
  CanClose := True;
end;

function TfrmGame.CloseQuery: Boolean;
begin
  // Override (rather than rely on OnCloseQuery alone) so this always
  // fires for the game window, even if the DFM event mapping is missed.
  // While a script is running the canvas is its draw target — destroying
  // it now would crash the next draw. Signal the script loop to abort
  // on its next processMessages tick and hide ourselves immediately.
  if FScriptRunning then
  begin
    FAbortScript := True;
    Hide;
    Result := False;
  end
  else
    Result := inherited CloseQuery;
end;

procedure TfrmGame.NotifyScriptStarted;
begin
  FScriptRunning := True;
  FAbortScript := False;
  FEventQueue.clear;
  Show;
  BringToFront;
  if (GameCanvas <> nil) and GameCanvas.CanFocus then
    GameCanvas.SetFocus;
end;

procedure TfrmGame.NotifyScriptStopped;
begin
  FScriptRunning := False;
end;

procedure TfrmGame.GameCanvasKeyDown(Sender: TObject; const KeyName: string);
var
  Msg: string;
begin
  if not FKeyDownCache.TryGetValue(KeyName, Msg) then
  begin
    Msg := 'keydown:' + KeyName;
    FKeyDownCache.Add(KeyName, Msg);
  end;
  FEventQueue.Enqueue(Msg);
  FEngine.QueueKeyDown(KeyName);
end;

procedure TfrmGame.GameCanvasKeyUp(Sender: TObject; const KeyName: string);
var
  Msg: string;
begin
  if not FKeyUpCache.TryGetValue(KeyName, Msg) then
  begin
    Msg := 'keyup:' + KeyName;
    FKeyUpCache.Add(KeyName, Msg);
  end;
  FEventQueue.Enqueue(Msg);
  FEngine.QueueKeyUp(KeyName);
end;

procedure TfrmGame.GameCanvasMouseDown(Sender: TObject; Button, LX, LY: Integer);
begin
  FEventQueue.Enqueue('mousedown:' + IntToStr(Button) + ':' +
    IntToStr(LX) + ':' + IntToStr(LY));
  FEngine.QueueMouseDown(Button, LX, LY);
end;

procedure TfrmGame.GameCanvasMouseUp(Sender: TObject; Button, LX, LY: Integer);
begin
  FEventQueue.Enqueue('mouseup:' + IntToStr(Button) + ':' +
    IntToStr(LX) + ':' + IntToStr(LY));
  FEngine.QueueMouseUp(Button, LX, LY);
end;

procedure TfrmGame.GameCanvasMouseMove(Sender: TObject; Button, LX, LY: Integer);
var
  Msg: string;
begin
  // Button is unused for moves (caller passes -1). We still emit logical
  // coords so scripts can drive hover/drag UI without polling mouseX/Y.
  Msg := 'mousemove:' + IntToStr(LX) + ':' + IntToStr(LY);
  // Coalesce consecutive mouse-move events: WM_MOUSEMOVE can fire 100+
  // times per second, but only the latest position matters for typical
  // hover/drag logic. Overwriting the tail caps queue growth between
  // processMessages() calls.
  if not FEventQueue.ReplaceTailIfPrefix('mousemove:', Msg) then
    FEventQueue.Enqueue(Msg);
  FEngine.QueueMouseMove(LX, LY);
end;

procedure TfrmGame.EngineLog(const Msg: string);
begin
  Form4.Memo2.Lines.Add(Msg);
end;

end.
