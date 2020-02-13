program GenerateSTable;

uses
  Forms,
  GenerateLookUpTableUnit in 'GenerateLookUpTableUnit.pas' {Form3},
  HantushUnit in 'HantushUnit.pas',
  RealListUnit in 'RealListUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
