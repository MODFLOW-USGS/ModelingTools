program FlowObservations;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FlowObs in 'FlowObs.pas',
  ReadFlowObsInput in 'ReadFlowObsInput.pas',
  ReadModflowArrayUnit in '..\ModelMuse\ReadModflowArrayUnit.pas',
  ReadStressPeriodsUnit in 'ReadStressPeriodsUnit.pas',
  ExtractFlowObservationsUnit in 'ExtractFlowObservationsUnit.pas',
  GetVersionUnit in 'GetVersionUnit.pas',
  CBC_ReaderUnit in 'CBC_ReaderUnit.pas',
  RealListUnit in '..\ModelMuse\RealListUnit.pas';

begin
  try
    ExtractFlowObservations;
    Writeln('Normal termination');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
