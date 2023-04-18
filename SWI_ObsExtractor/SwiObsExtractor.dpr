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

var
  InputFile: string;
  Extractor: TSwiObservationExtractor;

begin
  try
    if ParamCount > 0 then
    begin
      InputFile := ParamStr(1)
    end
    else
    begin
      WriteLn('What is the name of the input file?');
      Readln(InputFile);
    end;
    if FileExists(InputFile) then
    begin
      Extractor := TSwiObservationExtractor.Create(InputFile);
      try

      finally
        Extractor.Free;
      end;
    end
    else
    begin
      raise Exception.Create(Format('The input file "%s" does not exist.', [InputFile]));
    end;

    Writeln('');
    Writeln('normal termination of ', ExtractFileName(ParamStr(0)));
    Writeln('');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
