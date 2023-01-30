program TestModelMuse;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  IniFileUtilities in 'IniFileUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
