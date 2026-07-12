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
// (freed at shutdown). It carries its own elapsed-time clock on a TTimer.
//
// FireDAC integration
// -------------------
// This unit also REPLACES FireDAC's stock async-execute dialog: it
// registers TLoxGUIxAsyncExecuteImpl (an IFDGUIxAsyncExecuteDialog
// implementation backed by our progress form) with FireDAC's class
// factory under the standard 'Forms' provider. When sqlExec/sqlQuery run
// with CmdExecMode=amCancelDialog, FireDAC's executor:
//   - calls our Show(AExecutor) before the statement and Hide after —
//     these arm/disarm the Cancel button AND raise/lower an internal
//     "FireDAC wants the form visible" flag; the form is actually
//     Visible iff that flag OR the "script wants it visible" flag (set
//     by setProgress, cleared by hideProgress) is set, so a single
//     setProgress can span several sqlExec calls without flicker but
//     a bare sqlExec still self-hides;
//   - hands us the IFDStanAsyncExecutor, whose AbortJob is the cancel
//     mechanism (TDS attention signal for MSSQL);
//   - pumps messages during the statement, letting through ONLY mouse
//     messages aimed at whatever form IsFormMouseMessage claims and
//     keyboard when IsFormActive — i.e. our dialog is live, the rest of
//     the app is input-locked. Actual show/hide is wrapped with
//     FDGUIxBeginModal/FDGUIxEndModal so Application.ModalStarted and
//     DisableTaskWindows are balanced just like the stock form.
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
    FTimer: TTimer;
    FStopwatch: TStopwatch;
    // FireDAC's abort handle for the statement currently executing.
    // Non-nil only between ArmCancel and DisarmCancel.
    FExecutor: IFDStanAsyncExecutor;
    // Two independent visibility requests. The form is visible iff
    // either flag is set. FScriptShown is toggled by setProgress /
    // hideProgress; FInAsyncCall is toggled by ArmCancel / DisarmCancel.
    // This lets a script keep the dialog up across multiple sqlExec
    // calls (setProgress once, run several statements, hideProgress),
    // while a bare sqlExec still gets a self-hiding dialog.
    FScriptShown: Boolean;
    FInAsyncCall: Boolean;
    // Opaque handle from FDGUIxBeginModal, matched by FDGUIxEndModal.
    // Non-nil only while the form is actually Visible.
    FModalData: Pointer;
    procedure TimerTick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SyncVisibility;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure ShowMarquee(const AStep: string);
    procedure ShowDeterminate(const AStep: string; ACurrent, ATotal: Double);
    procedure HideProgress;
    // Called by the FireDAC dialog service around each async statement.
    procedure ArmCancel(const AExecutor: IFDStanAsyncExecutor);
    procedure DisarmCancel;
    // End-of-script teardown: unconditionally drop both visibility
    // requests so the dialog doesn't survive into the next script run
    // if the script forgot to call hideProgress().
    procedure ForceHide;
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

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 250;
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := False;
end;

procedure TLoxProgressForm.TimerTick(Sender: TObject);
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

procedure TLoxProgressForm.SyncVisibility;
var
  want: Boolean;
begin
  // Reconcile actual Visible state against the two request flags,
  // mirroring the OG form's tmrDelayTimer logic (minus the delay):
  // pair every inherited Show with FDGUIxBeginModal and every inherited
  // Hide with FDGUIxEndModal so Application.ModalStarted/ModalFinished
  // and DisableTaskWindows/EnableTaskWindows stay balanced.
  want := FScriptShown or FInAsyncCall;
  if want = Visible then Exit;
  if want then
  begin
    FStopwatch := TStopwatch.StartNew;
    FTimer.Enabled := True;
    FModalData := FDGUIxBeginModal(Self, False);
    inherited Show;
  end
  else
  begin
    FTimer.Enabled := False;
    FStopwatch.Stop;
    if FModalData <> nil then
      FDGUIxEndModal(FModalData);
    inherited Hide;
  end;
end;

procedure TLoxProgressForm.ShowMarquee(const AStep: string);
begin
  FScriptShown := True;
  SyncVisibility;
  FStepLabel.Caption := AStep;
  if FBar.Style <> pbstMarquee then
    FBar.Style := pbstMarquee;
  TimerTick(nil);
  Update;  // paint now, before the VM disappears into a long native call
end;

procedure TLoxProgressForm.ShowDeterminate(const AStep: string;
  ACurrent, ATotal: Double);
begin
  FScriptShown := True;
  SyncVisibility;
  FStepLabel.Caption := AStep;
  if FBar.Style <> pbstNormal then
    FBar.Style := pbstNormal;
  if ATotal < 1 then ATotal := 1;
  if ACurrent < 0 then ACurrent := 0;
  if ACurrent > ATotal then ACurrent := ATotal;
  FBar.Max := 1000;
  FBar.Position := Round(ACurrent / ATotal * 1000);
  TimerTick(nil);
  Update;
end;

procedure TLoxProgressForm.HideProgress;
begin
  FScriptShown := False;
  SyncVisibility;
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
  SyncVisibility;
  Update;
end;

procedure TLoxProgressForm.DisarmCancel;
begin
  FExecutor := nil;
  FCancelBtn.Visible := False;
  FInAsyncCall := False;
  SyncVisibility;
end;

procedure TLoxProgressForm.ForceHide;
begin
  // Called from ShutdownAllNatives at the top of FreeVM. Reset both
  // flags so a script that never called hideProgress() doesn't leave
  // the dialog on-screen after control returns to the host.
  FExecutor := nil;
  FCancelBtn.Visible := False;
  FScriptShown := False;
  FInAsyncCall := False;
  SyncVisibility;
end;

// --- FireDAC async-execute dialog service backed by TLoxProgressForm ---

type
  TLoxGUIxAsyncExecuteImpl = class(TFDGUIxObject, IFDGUIxAsyncExecuteDialog)
  private
    FOnShow, FOnHide: TNotifyEvent;
    FShowDelay, FHideDelay: Integer;
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
    function IsFormActive: Boolean;
    function IsFormMouseMessage(const AMsg: TMsg): Boolean;
  end;

function TLoxGUIxAsyncExecuteImpl.GetForm: TLoxProgressForm;
begin
  Result := EnsureForm;
end;

procedure TLoxGUIxAsyncExecuteImpl.Show(const AExecutor: IFDStanAsyncExecutor);
begin
  GetForm.ArmCancel(AExecutor);
  if Assigned(FOnShow) then
    FOnShow(GetForm);
end;

procedure TLoxGUIxAsyncExecuteImpl.Hide;
begin
  // DisarmCancel clears the FireDAC visibility request; SyncVisibility
  // inside it decides whether the form actually hides (only if the
  // script isn't holding it open via setProgress).
  if ProgressForm <> nil then
    ProgressForm.DisarmCancel;
  if Assigned(FOnHide) then
    FOnHide(ProgressForm);
end;

function TLoxGUIxAsyncExecuteImpl.IsFormActive: Boolean;
begin
  // Gates KEYBOARD messages in FireDAC's amCancelDialog pump: keys are
  // dispatched only while our dialog is the active form (Escape = cancel).
  Result := (ProgressForm <> nil) and ProgressForm.Visible and
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
  if ProgressForm = nil then Exit;
  oCtrl := FindControl(AMsg.hwnd);
  if oCtrl <> nil then
    Result := GetParentForm(oCtrl) = ProgressForm;
end;

function TLoxGUIxAsyncExecuteImpl.GetOnShow: TNotifyEvent;
begin
  Result := FOnShow;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetOnShow(const AValue: TNotifyEvent);
begin
  FOnShow := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetOnHide: TNotifyEvent;
begin
  Result := FOnHide;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetOnHide(const AValue: TNotifyEvent);
begin
  FOnHide := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetCaption: String;
begin
  Result := GetForm.Caption;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetCaption(const AValue: String);
begin
  if AValue <> '' then
    GetForm.Caption := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetPrompt: String;
begin
  Result := GetForm.FStepLabel.Caption;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetPrompt(const AValue: String);
begin
  // The step label belongs to setProgress(); ignore FireDAC's prompt.
end;

function TLoxGUIxAsyncExecuteImpl.GetCancelCaption: String;
begin
  Result := GetForm.FCancelBtn.Caption;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetCancelCaption(const AValue: String);
begin
  if AValue <> '' then
    GetForm.FCancelBtn.Caption := AValue;
end;

function TLoxGUIxAsyncExecuteImpl.GetShowDelay: Integer;
begin
  Result := FShowDelay;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetShowDelay(AValue: Integer);
begin
  FShowDelay := AValue;  // stored but unused: our form has no show/hide dance
end;

function TLoxGUIxAsyncExecuteImpl.GetHideDelay: Integer;
begin
  Result := FHideDelay;
end;

procedure TLoxGUIxAsyncExecuteImpl.SetHideDelay(AValue: Integer);
begin
  FHideDelay := AValue;
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
