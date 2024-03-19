program GetValuesFromArrayFiles;

{$APPTYPE CONSOLE}

{$R *.res}

{#BACKUP GetValuesFromArrayFiles.lpr}
{#BACKUP GetValuesFromArrayFiles.lpi}


uses
  System.SysUtils,
  System.IOUtils,
  ExtractValuesFromFileUnit in 'ExtractValuesFromFileUnit.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

var
  ArrayFileHandler: TArrayFileHandler;
  InputFileName: string;
  LineIndex: Integer;

procedure WriteUsage;
begin
  Writeln('Usage: a command line parameters must be supplied.');
  Writeln('The parameter must be the name of the file defining the observations.');
  Writeln('The first line in that file is the name of the file from which values are to be extracted.');
  Writeln('The remaining lines list observation names followed by alternating index values (1-based) ');
  Writeln('in the array and weights associated with those values.');
  Writeln('The weights must be greater than or equal to zero');
  Writeln('and at least one weight should be greater than zero.');
  Writeln('For example:');
  Writeln;
  Writeln('MyFile.array');
  Writeln('Obs1 1 0.5 18 3.3, 7 0.2');
  Writeln('Obs2 16 0.4 25 2.1, 32 1.3');
end;

begin
  try
    if ParamCount <> 1 then
    begin
      WriteUsage;
      Exit;
    end
    else
    begin
      InputFileName := ExpandFileName(ParamStr(1));
      if not TFile.Exists(InputFileName) then
      begin
        Writeln(InputFileName + ' does not exist');
        Writeln;
        WriteUsage;
        Exit;
      end;
    end;
    for LineIndex := 0 to Disclaimer.Count - 1 do
    begin
      Writeln(Disclaimer[LineIndex])
    end;
    Writeln;

    ArrayFileHandler := TArrayFileHandler.Create;
    try
      ArrayFileHandler.ConvertValues;
    finally
      ArrayFileHandler.Free;
    end;
    WriteLn('normal termination');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
