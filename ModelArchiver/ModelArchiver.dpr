program ModelArchiver;

uses
  FMX.Forms,
  frmModelArchiverUnit in 'frmModelArchiverUnit.pas' {frmModelArchiver},
  Xml.VerySimple in '..\ModelMuse\Xml.VerySimple.pas',
  frmAddModelOrClassUnit in 'frmAddModelOrClassUnit.pas' {frmModelOrClass},
  ArchiveNodeInterface in 'ArchiveNodeInterface.pas',
  ArchiveSaveUnit in 'ArchiveSaveUnit.pas',
  frmModelFilesUnit in 'frmModelFilesUnit.pas' {frmModelFiles},
  frmExtensionsUnit in 'frmExtensionsUnit.pas' {frmExtensions},
  ExtensionTypeUnit in 'ExtensionTypeUnit.pas',
  frmPreviewUnit in 'frmPreviewUnit.pas' {frmPreview},
  frmArrangeModelsUnit in 'frmArrangeModelsUnit.pas' {frmArrangeModels},
  fOpen in 'fOpen.pas',
  frmReadmeUnit in 'frmReadmeUnit.pas' {frmReadme},
  frmHelpUnit in 'frmHelpUnit.pas' {frmHelp},
  frmEditMetadataUnit in 'frmEditMetadataUnit.pas' {frmEditMetadata},
  frameChoicePlusDateUnit in 'frameChoicePlusDateUnit.pas' {frameChoicePlusDate: TFrame},
  frameChoicePlusTimeUnit in 'frameChoicePlusTimeUnit.pas' {frameChoicePlusTime: TFrame},
  frameChoicePlusUnit in 'frameChoicePlusUnit.pas' {frameChoicePlus: TFrame},
  frameChoiceUnit in 'frameChoiceUnit.pas' {frameChoice: TFrame},
  frameCustomMetaDataUnit in 'frameCustomMetaDataUnit.pas' {frameCustomMetaData: TFrame},
  frameDateEditsUnit in 'frameDateEditsUnit.pas' {frameDateEdits: TFrame},
  frameDateUnit in 'frameDateUnit.pas' {frameDate: TFrame},
  frameExtentUnit in 'frameExtentUnit.pas' {frameExtent: TFrame},
  frameIntegerMetaDataUnit in 'frameIntegerMetaDataUnit.pas' {frameIntegerMetaData: TFrame},
  frameMetaDataEditorUnit in 'frameMetaDataEditorUnit.pas' {frameMetaDataEditor: TFrame},
  frameNumericUnit in 'frameNumericUnit.pas' {frameNumeric: TFrame},
  framePolygonUnit in 'framePolygonUnit.pas' {framePolygon: TFrame},
  frameTextUnit in 'frameTextUnit.pas' {frameTextMetaData: TFrame},
  frameTimeEditsUnit in 'frameTimeEditsUnit.pas' {frameTimeEdits: TFrame},
  frameTimeUnit in 'frameTimeUnit.pas' {frameTime: TFrame},
  MetaDataInterfacesUnit in 'MetaDataInterfacesUnit.pas',
  MetaDataTreeViewItems in 'MetaDataTreeViewItems.pas',
  Mobile.OpenURL in 'Mobile.OpenURL.pas',
  frmSearchUnit in 'frmSearchUnit.pas' {frmSearch},
  frmArchiveUpdateUnit in 'frmArchiveUpdateUnit.pas' {frmArchiveUpdate},
  frmUpdatePathsUnit in 'frmUpdatePathsUnit.pas' {frmUpdatePaths},
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas',
  frmAboutModelArchiverUnit in 'frmAboutModelArchiverUnit.pas' {frmAboutModelArchiver};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmModelArchiver, frmModelArchiver);
  Application.CreateForm(TfrmModelOrClass, frmModelOrClass);
  Application.CreateForm(TfrmModelFiles, frmModelFiles);
  Application.CreateForm(TfrmExtensions, frmExtensions);
  Application.CreateForm(TfrmArrangeModels, frmArrangeModels);
  Application.CreateForm(TfrmReadme, frmReadme);
  Application.CreateForm(TfrmEditMetadata, frmEditMetadata);
  Application.CreateForm(TfrmSearch, frmSearch);
  Application.CreateForm(TfrmArchiveUpdate, frmArchiveUpdate);
  Application.CreateForm(TfrmHelp, frmHelp);
  Application.CreateForm(TfrmUpdatePaths, frmUpdatePaths);
  Application.CreateForm(TfrmAboutModelArchiver, frmAboutModelArchiver);
  Application.Run;
end.
