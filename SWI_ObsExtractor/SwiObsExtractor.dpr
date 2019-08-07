program SwiObsExtractor;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SwiObsInputReader in 'SwiObsInputReader.pas',
  SwiObsReaderUnit in 'SwiObsReaderUnit.pas',
  RealListUnit in 'RealListUnit.pas',
  SwiObsUtilities in 'SwiObsUtilities.pas',
  InterpolatedObsResourceUnit in '..\ModelMuse\InterpolatedObsResourceUnit.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

begin
  try
    ExtractSwiObservations;
    Writeln('');
    Writeln('normal termination of ', ExtractFileName(ParamStr(0)));
    Writeln('');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
