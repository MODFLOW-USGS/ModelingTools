program xytest3d;

uses
  Forms,
  xyun3d in 'xyun3d.pas' {Form1},
  xygraph3d in 'xygraph3d.pas',
  xygraph in 'xygraph.pas',
  XYCopy in 'XYCopy.pas' {CopyForm},
  xycommon in 'xycommon.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TCopyForm, CopyForm);
  Application.Run;
end.
