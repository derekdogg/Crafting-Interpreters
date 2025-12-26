program InterpreterGui;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form4};

{$R *.res}

begin
  reportMemoryLeaksOnShutDown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
