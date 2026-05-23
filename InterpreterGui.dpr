program InterpreterGui;
uses
  Vcl.Forms,
  Main in 'Main.pas' {Form4},
  SynHighlighterLox in 'SynHighlighterLox.pas',
  NativeObjects in 'NativeObjects.pas',
  fmGame in 'fmGame.pas' {frmGame},
  LoxCanvas in 'LoxCanvas.pas',
  Chunk_Types in 'Chunk_Types.pas';

{$R *.res}

begin
  reportMemoryLeaksOnShutDown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  // frmGame is constructed lazily by Form4.FormCreate before InitCanvas.
  Application.Run;
end.
