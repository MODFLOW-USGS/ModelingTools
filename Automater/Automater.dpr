program Automater;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  frmAutomateUnit in 'frmAutomateUnit.pas' {frmAutomate},
  ReadModflowArrayUnit in 'ReadModflowArrayUnit.pas',
  RealListUnit in 'RealListUnit.pas',
  BMSearch in 'BMSearch.pas',
  IntListUnit in 'IntListUnit.pas',
  HantushUnit in 'HantushUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAutomate, frmAutomate);
  Application.Run;
end.
