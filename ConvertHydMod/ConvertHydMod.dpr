program ConvertHydMod;

uses
  Vcl.Forms,
  frmConvertHydModUnit in 'frmConvertHydModUnit.pas' {frmConvertHydMod},
  ReadModflowArrayUnit in '..\ModelMuse\ReadModflowArrayUnit.pas',
  SwiObsUtilities in '..\SWI_ObsExtractor\SwiObsUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmConvertHydMod, frmConvertHydMod);
  Application.Run;
end.
