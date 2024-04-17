program ConvertBinary;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmConvertBinaryUnit in 'frmConvertBinaryUnit.pas' {frmConvertBinary},
  ReadModflowArrayUnit in '..\ModelMuse\ReadModflowArrayUnit.pas',
  SwiObsUtilities in '..\SWI_ObsExtractor\SwiObsUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmConvertBinary, frmConvertBinary);
  Application.Run;
end.
