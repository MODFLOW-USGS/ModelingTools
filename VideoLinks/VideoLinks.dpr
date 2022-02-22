program VideoLinks;

uses
  Vcl.Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  Xml.VerySimple in '..\ModelMuse\Xml.VerySimple.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
