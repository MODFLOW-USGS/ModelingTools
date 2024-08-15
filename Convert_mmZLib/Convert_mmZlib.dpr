program Convert_mmZlib;

uses
  Vcl.Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
