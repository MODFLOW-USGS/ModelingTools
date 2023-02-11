program TaskHistory;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  MainUnit in 'MainUnit.pas' {frmTaskHistory},
  TaskHistoryUnit in 'TaskHistoryUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTaskHistory, frmTaskHistory);
  Application.Run;
end.
