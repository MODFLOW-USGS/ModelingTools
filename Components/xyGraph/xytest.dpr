program xytest;

uses
  Forms,
  xyun in 'xyun.pas' {Form1},
  xygraph in 'xygraph.pas',
  XYCopy in 'XYCopy.pas' {CopyForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TCopyForm, CopyForm);
  Application.Run;
end.
