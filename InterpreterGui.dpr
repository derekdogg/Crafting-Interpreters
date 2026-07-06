program InterpreterGui;
uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Main in 'Main.pas' {Form4},
  SynHighlighterLox in 'SynHighlighterLox.pas',
  NativeObjects in 'NativeObjects.pas',
  fmGame in 'fmGame.pas' {frmGame},
  LoxCanvas in 'LoxCanvas.pas',
  Suto in 'Suto.pas',
  NativeRegistry in 'Natives\NativeRegistry.pas',
  ArrayNatives in 'Natives\ArrayNatives.pas',
  ConversionNatives in 'Natives\ConversionNatives.pas',
  DictNatives in 'Natives\DictNatives.pas',
  IntrospectionNatives in 'Natives\IntrospectionNatives.pas',
  MathNatives in 'Natives\MathNatives.pas',
  SqlNatives in 'Natives\SqlNatives.pas',
  StringNatives in 'Natives\StringNatives.pas',
  FileNatives in 'Natives\FileNatives.pas',
  SystemNatives in 'Natives\SystemNatives.pas',
  CallbackNatives in 'Natives\CallbackNatives.pas',
  EventNatives in 'Natives\EventNatives.pas',
  LoxEventEngine in 'LoxEventEngine.pas',
  fmEventTest in 'fmEventTest.pas' {frmEventTest};

{$R *.res}

begin
  reportMemoryLeaksOnShutDown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  // frmGame is constructed lazily by Form4.FormCreate before InitCanvas.
  Application.Run;
end.
