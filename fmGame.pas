unit fmGame;

// ============================================================
// Game window. Hosts the TLoxGameCanvas for every running script.
// The canvas is parented to this form (alClient) at startup and the
// form's client dimensions drive the LOGICAL_W / LOGICAL_H of the
// canvas. Resize the form before pressing Run to change resolution.
// ============================================================

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfrmGame = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FScriptRunning: Boolean;
  public
    destructor Destroy; override;
    function CloseQuery: Boolean; override;
    procedure NotifyScriptStarted;
    procedure NotifyScriptStopped;
  end;

var
  frmGame: TfrmGame;

implementation

uses
  LoxCanvas, Main;

{$R *.dfm}

procedure TfrmGame.FormShow(Sender: TObject);
begin
  if (GameCanvas <> nil) and GameCanvas.CanFocus then
    GameCanvas.SetFocus;
end;

destructor TfrmGame.Destroy;
begin
  // Application frees us BEFORE Form4 on shutdown, but the game canvas
  // (FGameSurface) is freed by Form4.OnDestroy -> FreeCanvas. Detach it
  // here so its dangling Parent doesn't AV inside FreeCanvas later.
  DetachCanvasFromHost;
  inherited;
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
  // it now would crash the next draw. Signal Form4 to abort the script
  // on its next processMessages tick and hide ourselves immediately.
  if FScriptRunning then
  begin
    if Form4 <> nil then
      Form4.FAbortScript := True;
    Hide;
    Result := False;
  end
  else
    Result := inherited CloseQuery;
end;

procedure TfrmGame.NotifyScriptStarted;
begin
  FScriptRunning := True;
  Show;
  BringToFront;
  if (GameCanvas <> nil) and GameCanvas.CanFocus then
    GameCanvas.SetFocus;
end;

procedure TfrmGame.NotifyScriptStopped;
begin
  FScriptRunning := False;
end;

end.
