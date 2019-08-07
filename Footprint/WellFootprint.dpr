program WellFootprint;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FootprintUnit in 'FootprintUnit.pas',
  FootprintFileUnit in 'FootprintFileUnit.pas',
  FootPrintUtilities in 'FootPrintUtilities.pas',
  FastGEO in '..\ModelMuse\FastGEO.pas',
  Xml.VerySimple in '..\ModelMuse\Xml.VerySimple.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

procedure GenerateResults;
var
  InputFile: string;
  Generator: TFootPrintGenerator;
  LineIndex: Integer;
begin
  Writeln('');
  Writeln('WellFootprint Version ' + Version);
  Writeln('');
  for LineIndex := 0 to Disclaimer.Count - 1 do
  begin
    Writeln(Disclaimer[LineIndex]);
  end;
  Writeln('');
  if ParamCount >= 1 then
  begin
    InputFile := ParamStr(1);
  end
  else
  begin
    Writeln('Enter the WellFootprint input file name');
    ReadLn(InputFile);
  end;
  if FileExists(InputFile) then
  begin
    Generator := TFootPrintGenerator.Create;
    try
      Generator.ConsolWriteProcedure := WriteLine;
      Generator.LoadFromFile(InputFile);
      if Generator.GenerateResults then
      begin
        Writeln('Normal termination');
      end
      else
      begin
        Writeln('Failed to meet convergence criterion');
      end;
    finally
      Generator.Free;
    end;
  end
  else
  begin
    Writeln('"', InputFile, '" does not exist');
  end;
end;

begin
  try
    GenerateResults;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
