program ListingAnalyst;

uses
  Forms,
  frmListAnalyzerUnit in 'frmListAnalyzerUnit.pas' {frmMain},
  FileIndexUnit in 'FileIndexUnit.pas',
  ModflowIdentifiersUnit in 'ModflowIdentifiersUnit.pas',
  ErrorMessages in 'ErrorMessages.pas',
  frameFileListHandlerUnit in 'frameFileListHandlerUnit.pas' {frameFileListHandler: TFrame},
  ExtractObservationsUnit in 'ExtractObservationsUnit.pas',
  BMSearch in 'BMSearch.pas',
  IntListUnit in '..\ModelMuse\IntListUnit.pas',
  CustomExtendedDialogForm in 'CustomExtendedDialogForm.pas',
  frmIndexFileUnit in 'frmIndexFileUnit.pas' {frmIndexFile},
  SearchTrie in 'SearchTrie.pas',
  frmSettingsUnit in 'frmSettingsUnit.pas' {frmSettings};

{$R *.res}
{#BACKUP ListingAnalyst.cfg}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmIndexFile, frmIndexFile);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.Run;
end.
