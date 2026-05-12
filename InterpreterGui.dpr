program InterpreterGui;
uses

  Vcl.Forms,
  Main in 'Main.pas' {Form4},
  SynHighlighterLox in 'SynHighlighterLox.pas',
  NativeObjects in 'NativeObjects.pas';

{$R *.res}

begin
  reportMemoryLeaksOnShutDown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
