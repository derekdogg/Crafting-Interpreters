unit ProgressNatives;

// ============================================================
// Host progress dialog for long-running script work (PoC).
//
// Natives:
//   setProgress(label)                  -- marquee (indeterminate) mode
//   setProgress(label, current, total)  -- determinate bar
//   hideProgress()                      -- hide the dialog
//
// The dialog is created lazily on first use and is owned by Application
// (freed at shutdown). It carries its own elapsed-time clock plus an
// anti-flicker debounce timer (mirroring FireDAC's tmrDelay); actual
// inherited Show/Hide waits FShowDelay/FHideDelay ms after a request so
// sub-200ms ops never flash and back-to-back statements collapse to no
// visible transition.
//
// FireDAC integration
// -------------------
// This unit REPLACES FireDAC's stock async-execute dialog: it registers
// TLoxGUIxAsyncExecuteImpl (an IFDGUIxAsyncExecuteDialog implementation
// backed by our progress form) with FireDAC's class factory under the
// standard 'Forms' provider. The impl is a thin FireDAC-shaped shell —
// FDGUIxSilent gating, storage-on-form for OnShow/OnHide/ShowDelay/
// HideDelay, setter guards that don't force early form creation, and a
// destructor that FDFreeAndNil-equivalents the singleton — so it slots
// in wherever the stock TFDGUIxFormsAsyncExecuteImpl would.
//
// When sqlExec/sqlQuery run with CmdExecMode=amCancelDialog, FireDAC's
// executor:
//   - calls our Show(AExecutor) before the statement and Hide after —
//     these arm/disarm the Cancel button AND raise/lower an internal
//     "FireDAC wants the form visible" request; the form is actually
//     Visible iff that request OR the "script wants it visible" request
//     (set by setProgress, cleared by hideProgress) is set, so a single
//     setProgress can span several sqlExec calls without flicker while
//     a bare sqlExec still self-hides;
//   - hands us the IFDStanAsyncExecutor, whose AbortJob is the cancel
//     mechanism (TDS attention signal for MSSQL);
//   - pumps messages during the statement, letting through ONLY mouse
//     messages aimed at whatever form IsFormMouseMessage claims and
//     keyboard when IsFormActive — i.e. our dialog is live, the rest of
//     the app is input-locked. Actual visibility transitions call
//     FDGUIxCancel (release capture/drag), pair FDGUIxBeginModal with
//     FDGUIxEndModal (balance Application.ModalStarted/ModalFinished
//     and DisableTaskWindows/EnableTaskWindows), and force a paint
//     pump — same recipe as the stock form's tmrDelayTimer.
//
// IMPORTANT: do not link FireDAC.VCLUI.Async anywhere in the project.
// It registers the stock dialog under the same 'Forms' provider and the
// winner would depend on unit-initialization order.
// ============================================================

interface

procedure RegisterProgressNatives;

implementation

uses
  Winapi.Windows, Classes, System.SysUtils, System.Diagnostics,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Consts, FireDAC.Stan.Factory,
  FireDAC.UI.Intf, FireDAC.UI, FireDAC.VCLUI.Controls,
  Suto, NativeRegistry;

type
  TLoxProgressForm = class(TForm)
  private
    FStepLabel: TLabel;
    FBar: TProgressBar;
    FElapsedLabel: TLabel;
    FCancelBtn: TButton;
    FElapsedTimer: TTimer;
    // Anti-flicker debounce: like FireDAC's tmrDelay, defers the actual
    // inherited Show/Hide by FShowDelay/FHideDelay ms so very short async
    // ops never flash the dialog and back-to-back statements don't
    // hide/show between them.
    FDelayTimer: TTimer;
    FStopwatch: TStopwatch;
    // FireDAC's abort handle for the statement currently executing.
    // Non-nil only between ArmCancel and DisarmCancel.
    FExecutor: IFDStanAsyncExecutor;
    // Two independent visibility requests. The form should be visible
    // iff either flag is set. FScriptShown is toggled by setProgress /
    // hideProgress; FInAsyncCall is toggled by ArmCancel / DisarmCancel.
    // This lets a script keep the dialog up across multiple sqlExec
    // calls (setProgress once, run several statements, hideProgress),
    // while a bare sqlExec still gets a self-hiding dialog.
    FScriptShown: Boolean;
    FInAsyncCall: Boolean;
    // Pending visibility state, applied by FDelayTimer. This is FireDAC's
    // FRequestShow: it changes synchronously when a flag flips, but the
    // real inherited Show/Hide waits FShowDelay/FHideDelay ms so brief
    // ops don't flash the dialog.
    FRequestVisible: Boolean;
    // Delay values consulted by RequestVisibility; owned by the form so
    // the impl's Get/SetShowDelay/HideDelay can delegate here, matching
    // the FireDAC storage-on-form pattern.
    FShowDelay, FHideDelay: Integer;
    // Opaque handle from FDGUIxBeginModal, matched by FDGUIxEndModal.
    // Non-nil only while the form is actually Visible AND FModalData
    // was returned by BeginModal.
    FModalData: Pointer;
    procedure ElapsedTimerTick(Sender: TObject);
    procedure DelayTimerTick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RequestVisibility;
    procedure ApplyVisibility;
    function GetPromptText: string;
    procedure SetPromptText(const AValue: string);
    function GetCancelCaptionText: string;
    procedure SetCancelCaptionText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure ShowMarquee(const AStep: string);
    procedure ShowDeterminate(const AStep: string; ACurrent, ATotal: Double);
    procedure HideProgress;
    // Called by the FireDAC dialog service around each async statement.
    procedure ArmCancel(const AExecutor: IFDStanAsyncExecutor);
    procedure DisarmCancel;
    // End-of-script teardown: unconditionally drop both visibility
    // requests AND bypass the delay timer, so the dialog is gone before
    // FreeVM returns to the host.
    procedure ForceHide;
    // Public surface for the IFDGUIxAsyncExecuteDialog impl. Matches
    // FireDAC's storage-on-form pattern (stock form exposes lblPrompt
    // and btnCancel2 as published members that the impl reads directly)
    // so callers of the impl never have to reach into private fields.
    property Prompt: string read GetPromptText write SetPromptText;
    property CancelCaption: string read GetCancelCaptionText
      write SetCancelCaptionText;
    // Storage for FShowDelay/FHideDelay — accessed by the impl via
    // GetShowDelay/SetShowDelay so callers see FireDAC-shaped behaviour.
    property ShowDelay: Integer read FShowDelay write FShowDelay;
    property HideDelay: Integer read FHideDelay write FHideDelay;
  end;

var
  ProgressForm: TLoxProgressForm = nil;

function EnsureForm: TLoxProgressForm;
begin
  if ProgressForm = nil then
    ProgressForm := TLoxProgressForm.Create(Application);
  Result := ProgressForm;
end;

constructor TLoxProgressForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'Working...';
  BorderStyle := bsDialog;
  BorderIcons := [];
  Position := poScreenCenter;
  FormStyle := fsStayOnTop;
  ClientWidth := 420;
  ClientHeight := 104;
  KeyPreview := True;
  OnKeyDown := HandleKeyDown;

  FStepLabel := TLabel.Create(Self);
  FStepLabel.Parent := Self;
  FStepLabel.AutoSize := False;
  FStepLabel.EllipsisPosition := epEndEllipsis;
  FStepLabel.SetBounds(16, 12, ClientWidth - 32, 20);

  FBar := TProgressBar.Create(Self);
  FBar.Parent := Self;
  FBar.SetBounds(16, 40, ClientWidth - 32, 20);

  FElapsedLabel := TLabel.Create(Self);
  FElapsedLabel.Parent := Self;
  FElapsedLabel.SetBounds(16, 72, ClientWidth - 32 - 106, 20);

  FCancelBtn := TButton.Create(Self);
  FCancelBtn.Parent := Self;
  FCancelBtn.SetBounds(ClientWidth - 106, 68, 90, 26);
  FCancelBtn.Caption := 'Cancel';
  FCancelBtn.Visible := False;
  FCancelBtn.OnClick := CancelClick;

  FElapsedTimer := TTimer.Create(Self);
  FElapsedTimer.Interval := 250;
  FElapsedTimer.OnTimer := ElapsedTimerTick;
  FElapsedTimer.Enabled := False;

  FDelayTimer := TTimer.Create(Self);
  FDelayTimer.OnTimer := DelayTimerTick;
  FDelayTimer.Enabled := False;

  // Match FireDAC's default (C_FD_DelayBeforeFWait = 200 ms). The impl
  // exposes these via IFDGUIxAsyncExecuteDialog.ShowDelay/HideDelay so
  // callers can override just like they would with the stock dialog.
  FShowDelay := C_FD_DelayBeforeFWait;
  FHideDelay := C_FD_DelayBeforeFWait;
end;

destructor TLoxProgressForm.Destroy;
begin
  // Belt-and-braces: if this form was ever wrapped in a modal push, undo
  // it before dying so DisableTaskWindows/ModalStarted stay balanced.
  FDelayTimer.Enabled := False;
  if FModalData <> nil then
  begin
    FDGUIxEndModal(FModalData);
    FModalData := nil;
  end;
  // Same trick FireDAC's TfrmFDGUIxFormsAsyncExecute.FormDestroy uses:
  // clear the singleton if it still points at us, so later access
  // through EnsureForm creates a fresh instance instead of dereferencing
  // freed memory.
  if ProgressForm = Self then
    ProgressForm := nil;
  inherited Destroy;
end;

procedure TLoxProgressForm.ElapsedTimerTick(Sender: TObject);
var
  secs: Int64;
begin
  secs := FStopwatch.ElapsedMilliseconds div 1000;
  FElapsedLabel.Caption := Format('Elapsed: %d:%2.2d', [secs div 60, secs mod 60]);
end;

procedure TLoxProgressForm.CancelClick(Sender: TObject);
begin
  // Same call the stock FireDAC dialog makes: the executor owns the
  // running statement; AbortJob sends the out-of-band cancel.
  if (FExecutor <> nil) and FExecutor.Operation.AbortSupported then
  begin
    FCancelBtn.Enabled := False;
    FCancelBtn.Caption := 'Cancelling...';
    FExecutor.AbortJob;
  end;
end;

procedure TLoxProgressForm.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and FCancelBtn.Visible and FCancelBtn.Enabled then
  begin
    CancelClick(nil);
    Key := 0;
  end;
end;

procedure TLoxProgressForm.RequestVisibility;
var
  want: Boolean;
begin
  // Called from every state-change entry (setProgress, hideProgress,
  // ArmCancel, DisarmCancel). Records the new desired visibility and
  // arms FDelayTimer; the actual inherited Show/Hide happens in
  // DelayTimerTick. Idempotent: if the request hasn't actually changed
  // we leave any pending timer alone (mirrors FireDAC's `if not
  // FRequestShow` guards in Show/Hide).
  want := FScriptShown or FInAsyncCall;
  if want = FRequestVisible then Exit;

  FRequestVisible := want;

  // Elapsed clock is tied to *requested* visibility, not actual: a
  // sub-delay op that never draws the dialog also never needs an
  // elapsed reading, so we harmlessly start/stop the stopwatch inside
  // the debounce window.
  if want then
  begin
    FStopwatch := TStopwatch.StartNew;
    FElapsedTimer.Enabled := True;
    FDelayTimer.Interval := FShowDelay;
  end
  else
  begin
    FElapsedTimer.Enabled := False;
    FStopwatch.Stop;
    FDelayTimer.Interval := FHideDelay;
  end;
  FDelayTimer.Enabled := True;
end;

procedure TLoxProgressForm.ApplyVisibility;
begin
  // Exact translation of FireDAC's tmrDelayTimer body: only act if the
  // requested state still differs from actual (a Show->Hide toggle
  // faster than FShowDelay collapses to nothing here), release any
  // captured mouse/drag before flipping, and pair BeginModal with
  // EndModal so Application.ModalStarted/ModalFinished + DisableTask-
  // Windows/EnableTaskWindows stay balanced.
  if (FRequestVisible = Visible) or (csDestroying in ComponentState) then
  begin
    FDelayTimer.Enabled := False;
    Exit;
  end;
  FDGUIxCancel;
  if FRequestVisible then
  begin
    FModalData := FDGUIxBeginModal(Self, False);
    inherited Show;
    Application.ProcessMessages;
  end
  else
  begin
    if FModalData <> nil then
      FDGUIxEndModal(FModalData);
    FModalData := nil;
    inherited Hide;
  end;
  FDelayTimer.Enabled := False;
end;

procedure TLoxProgressForm.DelayTimerTick(Sender: TObject);
begin
  ApplyVisibility;
end;

procedure TLoxProgressForm.ShowMarquee(const AStep: string);
begin
  FScriptShown := True;
  RequestVisibility;
  FStepLabel.Caption := AStep;
  if FBar.Style <> pbstMarquee then
    FBar.Style := pbstMarquee;
  ElapsedTimerTick(nil);
  if Visible then
    Update;  // paint now, before the VM disappears into a long native call
end;

procedure TLoxProgressForm.ShowDeterminate(const AStep: string;
  ACurrent, ATotal: Double);
begin
  FScriptShown := True;
  RequestVisibility;
  FStepLabel.Caption := AStep;
  if FBar.Style <> pbstNormal then
    FBar.Style := pbstNormal;
  if ATotal < 1 then ATotal := 1;
  if ACurrent < 0 then ACurrent := 0;
  if ACurrent > ATotal then ACurrent := ATotal;
  FBar.Max := 1000;
  FBar.Position := Round(ACurrent / ATotal * 1000);
  ElapsedTimerTick(nil);
  if Visible then
    Update;
end;

procedure TLoxProgressForm.HideProgress;
begin
  FScriptShown := False;
  RequestVisibility;
end;

procedure TLoxProgressForm.ArmCancel(const AExecutor: IFDStanAsyncExecutor);
begin
  FExecutor := AExecutor;
  FCancelBtn.Caption := 'Cancel';
  FCancelBtn.Enabled := True;
  FCancelBtn.Visible := (AExecutor <> nil) and AExecutor.Operation.AbortSupported;
  // A script that never called setProgress still gets a cancellable
  // dialog for the duration of the statement; DisarmCancel will hide
  // it again unless setProgress has also asked for it to stay up.
  FInAsyncCall := True;
  RequestVisibility;
  if Visible then
    Update;
end;

procedure TLoxProgressForm.DisarmCancel;
begin
  FExecutor := nil;
  FCancelBtn.Visible := False;
  FInAsyncCall := False;
  RequestVisibility;
end;

procedure TLoxProgressForm.ForceHide;
begin
  // Called from ShutdownAllNatives at the top of FreeVM. Bypass the
  // delay timer entirely — there won't be another message-pump tick
  // before FreeVM returns to the host, so any pending Show/Hide must
  // be applied synchronously right now.
  FDelayTimer.Enabled := False;
  FExecutor := nil;
  FCancelBtn.Visible := False;
  FScriptShown := False;
  FInAsyncCall := False;
  FRequestVisible := False;
  FElapsedTimer.Enabled := False;
  FStopwatch.Stop;
  if Visible then
  begin
    if FModalData <> nil then
      FDGUIxEndModal(FModalData);
    FModalData := nil;
    inherited Hide;
  end;
end;

function TLoxProgressForm.GetPromptText: string;
begin
  Result := FStepLabel.Caption;
end;

procedure TLoxProgressForm.SetPromptText(const AValue: string);
begin
  FStepLabel.Caption := AValue;
end;

function TLoxProgressForm.GetCancelCaptionText: string;
begin
  Result := FCancelBtn.Caption;
end;

procedure TLoxProgressForm.SetCancelCaptionText(const AValue: string);
begin
  FCancelBtn.Caption := AValue;
end;

// --- FireDAC async-execute dialog service backed by TLoxProgressForm ---

type
  TLoxGUIxAsyncExecuteImpl = class(TFDGUIxObject, IFDGUIxAsyncExecuteDialog)
  private
    function GetForm: TLoxProgressForm;
  protected
    // IFDGUIxAsyncExecuteDialog
    function GetOnShow: TNotifyEvent;
    procedure SetOnShow(const AValue: TNotifyEvent);
    function GetOnHide: TNotifyEvent;
    procedure SetOnHide(const AValue: TNotifyEvent);
    function GetCaption: String;
    procedure SetCaption(const AValue: String);
    function GetPrompt: String;
    procedure SetPrompt(const AValue: String);
    function GetCancelCaption: String;
    procedure SetCancelCaption(const AValue: String);
    function GetShowDelay: Integer;
    procedure SetShowDelay(AValue: Integer);
    function GetHideDelay: Integer;
    procedure SetHideDelay(AValue: Integer);
    procedure Show(const AExecutor: IFDStanAsyncExecutor);
    procedure Hide;
    {$IFDEF MSWINDOWS}
    function IsFormActive: Boolean;
    function IsFormMouseMessage(const AMsg: TMsg): Boolean;
    {$ENDIF}
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

procedure TLoxGUIxAsyncExecuteImpl.Initialize;
begin
  inherited Initialize;
  // Mirror FireDAC's TFDGUIxFormsAsyncExecuteImpl.Initialize: ensure the
  // singleton is nil at construction so a stale pointer from a prior
  // impl instance (unlikely — we're a factory singleton — but cheap) is
  // dropped before GetForm rehydrates it.
  ProgressForm := nil;
end;

destructor TLoxGUIxAsyncExecuteImpl.Destroy;
begin
  // Same lifecycle as FireDAC's impl (FDFreeAndNil). Application owns
  // the form so it would be freed at shutdown anyway, but freeing here
  // avoids relying on unit-finalization order for the dangling-pointer
  // safety net in TLoxProgressForm.Destroy.
  FreeAndNil(ProgressForm);
  inherited Destroy;
end;

function TLoxGUIxAsyncExecuteImpl.GetForm: TLoxProgressForm;
begin
  Result := EnsureForm;
end;

procedure TLoxGUIxAsyncExecuteImpl.Show(const AExecutor: IFDStanAsyncExecutor);
begin
  if not FDGUIxSilent() then
    GetForm.ArmCancel(AExecutor);
end;

procedure TLoxGUIxAsyncExecuteImpl.Hide;
begin
  if not FDGUIxSilent() then
    // DisarmCancel clears the FireDAC visibility request; the form's
    // reconciler decides whether it actually hides (script may still
    // be holding it up via setProgress).
    if ProgressForm <> nil then
      ProgressForm.DisarmCancel;
end;

{$IFDEF MSWINDOWS}
function TLoxGUIxAsyncExecuteImpl.IsFormActive: Boolean;
begin
  // Gates KEYBOARD messages in FireDAC's amCancelDialog pump: keys are
  // dispatched only while our dialog is the active form (Escape = cancel).
  Result := not FDGUIxSilent() and
    (ProgressForm <> nil) and ProgressForm.Visible and
    (Screen.ActiveForm = ProgressForm);
end;

function TLoxGUIxAsyncExecuteImpl.IsFormMouseMessage(const AMsg: TMsg): Boolean;
var
  oCtrl: TControl;
begin
  // Gates MOUSE messages in FireDAC's amCancelDialog pump: only messages
  // aimed at our dialog's controls are dispatched — the Cancel button is
  // clickable while the rest of the app stays input-locked.
  Result := False;
  if FDGUIxSilent() or (ProgressForm = nil) then Exit;
  oCtrl := FindControl(AMsg.hwnd);
  if oCtrl <> nil then
    Result := GetParentForm(oCtrl) = ProgressForm;
end;
{$ENDIF}

function TLoxGUIxAsyncExecuteImpl.GetOnShow: TNotifyEvent;
begin
  // Delegate to the form, matching FireDAC's storage-on-form pattern.
  // TForm's OnShow fires naturally from inside inherited Show, which is
  // where we want callers to observe visibility transitions.
  Result := GetForm.OnShow;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetOnShow(const AValue: TNotifyEvent);
begin
  // FireDAC guard: only create the form if the caller is assigning a
  // real handler; assigning nil to a not-yet-created form is a no-op.
  if Assigned(AValue) or Assigned(ProgressForm) then
    GetForm.OnShow := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetOnHide: TNotifyEvent;
begin
  Result := GetForm.OnHide;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetOnHide(const AValue: TNotifyEvent);
begin
  if Assigned(AValue) or Assigned(ProgressForm) then
    GetForm.OnHide := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetCaption: String;
begin
  Result := GetForm.Caption;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetCaption(const AValue: String);
begin
  if (AValue <> '') or Assigned(ProgressForm) then
    GetForm.Caption := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetPrompt: String;
begin
  Result := GetForm.Prompt;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetPrompt(const AValue: String);
begin
  // Symmetric with GetPrompt now. If the script has an active
  // setProgress the label was set from there; a caller writing to
  // Prompt after that will overwrite it — same behaviour a caller of
  // the stock FireDAC dialog would see.
  if (AValue <> '') or Assigned(ProgressForm) then
    GetForm.Prompt := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetCancelCaption: String;
begin
  Result := GetForm.CancelCaption;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetCancelCaption(const AValue: String);
begin
  if (AValue <> '') or Assigned(ProgressForm) then
    GetForm.CancelCaption := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetShowDelay: Integer;
begin
  Result := GetForm.ShowDelay;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetShowDelay(AValue: Integer);
begin
  if (AValue <> 0) or Assigned(ProgressForm) then
    GetForm.ShowDelay := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetHideDelay: Integer;
begin
  Result := GetForm.HideDelay;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetHideDelay(AValue: Integer);
begin
  if (AValue <> 0) or Assigned(ProgressForm) then
    GetForm.HideDelay := AValue;
end;

// --- Natives ---

function setProgressNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNilValue;
  if (argCount <> 1) and (argCount <> 3) then
  begin
    RuntimeError('setProgress() takes (label) or (label, current, total).');
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('setProgress() label must be a string.');
    Exit;
  end;
  if (argCount = 3) and (not isNumber(args[1]) or not isNumber(args[2])) then
  begin
    RuntimeError('setProgress() current and total must be numbers.');
    Exit;
  end;

  if argCount = 1 then
    EnsureForm.ShowMarquee(string(AsAnsiString(args[0])))
  else
    EnsureForm.ShowDeterminate(string(AsAnsiString(args[0])),
      GetNumber(args[1]), GetNumber(args[2]));
end;

function hideProgressNative(argCount: integer; args: pValue): TValue;
begin
  if ProgressForm <> nil then
    ProgressForm.HideProgress;
  Result := CreateNilValue;
end;

procedure ShutdownProgress;
begin
  // Registered via RegisterNativeShutdown; invoked by Suto.FreeVM at
  // the top of every script teardown. Force-hides the dialog so a
  // script that omitted hideProgress() doesn't leave it stranded.
  if ProgressForm <> nil then
    ProgressForm.ForceHide;
end;

procedure RegisterProgressNatives;
begin
  defineNative('setProgress', setProgressNative, -1);  // 1 or 3 args, self-validates
  defineNative('hideProgress', hideProgressNative, 0);
  RegisterNativeShutdown(ShutdownProgress);
end;

var
  LoxAsyncDlgFactory: TFDFactory = nil;

initialization
  RegisterNativeModule(RegisterProgressNatives);
  // Replace FireDAC's stock async-execute dialog app-wide: our impl is the
  // sole factory for IFDGUIxAsyncExecuteDialog under the 'Forms' provider
  // (FireDAC.VCLUI.Async, which registers the stock one, is not linked).
  LoxAsyncDlgFactory := TFDSingletonFactory.Create(TLoxGUIxAsyncExecuteImpl,
    IFDGUIxAsyncExecuteDialog, C_FD_GUIxFormsProvider);

finalization
  FDReleaseFactory(LoxAsyncDlgFactory);

end.
