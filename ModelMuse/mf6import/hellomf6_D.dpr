program HelloMf6;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Classes,
  Modflow6ConstantsUnit in 'Modflow6ConstantsUnit.pas',
  Mf6Variables in 'Mf6Variables.pas',
  Mf6Functions in 'Mf6Functions.pas',
  ImportModelUnit in 'ImportModelUnit.pas';


// Delphi does not support importing variables from DLLs or shared objects. :-(
// var
// BMI_LENVARTYPE : cint; cdecl;external 'libmf6.dll';
begin
  ImportModel;
  Writeln('Press any key to close');
  Readln;
end.
