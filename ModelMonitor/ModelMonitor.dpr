program ModelMonitor;

uses
  FastMM4 in '..\ModelMuse\FastMM4.pas',
  Forms,
  frmMonitorUnit in 'frmMonitorUnit.pas' {frmMonitor},
  ErrorMessages in 'ErrorMessages.pas',
  RealListUnit in '..\ModelMuse\RealListUnit.pas',
  SearchTrie in 'SearchTrie.pas',
  IniFileUtilities in '..\ModelMuse\IniFileUtilities.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

{$R *.res}
{#BACKUP ModelMonitor.cfg}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMonitor, frmMonitor);
  Application.Run;
end.
