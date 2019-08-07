program WellFootprintTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFootprintUnit in 'TestFootprintUnit.pas',
  FootprintUnit in '..\FootprintUnit.pas',
  DUnitTestRunner,
  TestFootprintFileUnit in 'TestFootprintFileUnit.pas',
  FootprintFileUnit in '..\FootprintFileUnit.pas',
  FootPrintUtilities in '..\FootPrintUtilities.pas',
  FastGEO in '..\..\ModelMuse\FastGEO.pas',
  Xml.VerySimple in '..\..\ModelMuse\Xml.VerySimple.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

