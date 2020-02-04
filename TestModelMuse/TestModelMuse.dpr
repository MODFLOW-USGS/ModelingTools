program TestModelMuse;

uses
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  IniFileUtilities in 'IniFileUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
