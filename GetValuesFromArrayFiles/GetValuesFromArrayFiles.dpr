program GetValuesFromArrayFiles;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ExtractValuesFromFileUnit in 'ExtractValuesFromFileUnit.pas';

begin
  try
    if ParamCount < 2 then
    begin
      Writeln('Usage: two command line parameters must be supplied.');
      Writeln('1. Array file from which values are to be extracted.');
      Writeln('2. Input file with a list of array names followed by the index values (1-based) in the array to be averaged.');
    end;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
