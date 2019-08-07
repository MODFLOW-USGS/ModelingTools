program MetaDataEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMetaDataUnit in 'frmMetaDataUnit.pas' {frmMetaData},
  MetaDataTreeViewItems in 'MetaDataTreeViewItems.pas',
  frameCustomMetaDataUnit in 'frameCustomMetaDataUnit.pas' {frameCustomMetaData: TFrame},
  frameNumericUnit in 'frameNumericUnit.pas' {frameNumeric: TFrame},
  MetaDataInterfacesUnit in 'MetaDataInterfacesUnit.pas',
  frameTextUnit in 'frameTextUnit.pas' {frameTextMetaData: TFrame},
  Xml.VerySimple in '..\ModelMuse\Xml.VerySimple.pas',
  frameDateUnit in 'frameDateUnit.pas' {frameDate: TFrame},
  frameChoiceUnit in 'frameChoiceUnit.pas' {frameChoice: TFrame},
  frameExtentUnit in 'frameExtentUnit.pas' {frameExtent: TFrame},
  frameChoicePlusUnit in 'frameChoicePlusUnit.pas' {frameChoicePlus: TFrame},
  frameChoicePlusDateUnit in 'frameChoicePlusDateUnit.pas' {frameChoicePlusDate: TFrame},
  frameDateEditsUnit in 'frameDateEditsUnit.pas' {frameDateEdits: TFrame},
  framePolygonUnit in 'framePolygonUnit.pas' {framePolygon: TFrame},
  frameIntegerMetaDataUnit in 'frameIntegerMetaDataUnit.pas' {frameIntegerMetaData: TFrame},
  frameTimeUnit in 'frameTimeUnit.pas' {frameTime: TFrame},
  frameTimeEditsUnit in 'frameTimeEditsUnit.pas' {frameTimeEdits: TFrame},
  frameChoicePlusTimeUnit in 'frameChoicePlusTimeUnit.pas' {frameChoicePlusTime: TFrame},
  Mobile.OpenURL in 'Mobile.OpenURL.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMetaData, frmMetaData);
  Application.RegisterFormFamily('TForm', [TfrmMetaData]);
  Application.Run;
end.
