program GetValuesFromArrayFiles;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ExtractValuesFromFileUnit in 'ExtractValuesFromFileUnit.pas';

var
  ArrayFileHandler: TArrayFileHandler;

begin
  try
    if ParamCount < 1 then
    begin
      Writeln('Usage: a command line parameters must be supplied.');
      Writeln('The first line in the file is the name of the file from which values are to be extracted.');
      Writeln('The remaining lines list observation names followed by alternating index values (1-based) ');
      Writeln('in the array and weights associated with those values.');
    end;
    ArrayFileHandler := TArrayFileHandler.Create;
    try
      ArrayFileHandler.ConvertValues;
    finally
      ArrayFileHandler.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
