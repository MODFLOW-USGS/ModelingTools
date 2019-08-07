// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_INSERTJDBG ON
program ModelMate;





uses
  madExcept,
  //madExcept,
  Forms,
  frmStartUp in 'frmStartUp.pas' {FormStartUp},
  frmMainUnit in 'frmMainUnit.pas' {FormMain},
  frmUcodeSettingsUnit in 'frmUcodeSettingsUnit.pas' {frmUcodeSettings},
  frmParEstSettingsUnit in 'frmParEstSettingsUnit.pas' {frmParEstSettings},
  GlobalData in 'GlobalData.pas',
  CustomFileWriterUnit in 'CustomFileWriterUnit.pas',
  ModelMateUtilities in 'ModelMateUtilities.pas',
  frmHelpUcodeMethodsUnit in 'frmHelpUcodeMethodsUnit.pas' {frmHelpUcodeMethods},
  DataFileWriterUnit in 'DataFileWriterUnit.pas',
  DataItemUnit in 'DataItemUnit.pas',
  frmConfigureParTablesUnit in 'frmConfigureParTablesUnit.pas' {FormConfigureParTables},
  frmAddGroupUnit in 'frmAddGroupUnit.pas' {frmAddGroup},
  frmAddParOrDepUnit in 'frmAddParOrDepUnit.pas' {frmAddParOrDep},
  JupiterUnit in 'JupiterUnit.pas',
  UcodeUnit in 'UcodeUnit.pas',
  GlobalTypesUnit in 'GlobalTypesUnit.pas',
  ModelMateClassesUnit in 'ModelMateClassesUnit.pas',
  Utilities in 'Utilities.pas',
  frmProjectName in 'frmProjectName.pas' {frmProjName},
  GlobalBasicData in 'GlobalBasicData.pas',
  frmModelCommandsUnit in 'frmModelCommandsUnit.pas' {FormModelCommands},
  frmModelIO in 'frmModelIO.pas' {FormModelIO},
  frmObservations in 'frmObservations.pas' {FormObservations},
  DependentsUnit in 'DependentsUnit.pas',
  frmConfigureObsTablesUnit in 'frmConfigureObsTablesUnit.pas' {FormConfigureObsTables},
  frmProgramLocationsUnit in 'frmProgramLocationsUnit.pas' {FormProgramLocations},
  Modflow2005Unit in 'Modflow2005Unit.pas',
  frmModelSettings in 'frmModelSettings.pas' {FormModflow},
  frmParEstAdvancedUnit in 'frmParEstAdvancedUnit.pas' {FormParEstAdvanced},
  ModflowUnit in 'ModflowUnit.pas',
  frmOutput in 'frmOutput.pas' {FormOutput},
  frmAbout in 'frmAbout.pas' {FormAbout},
  sskutils in 'sskutils.pas',
  frmModelDirectories in 'frmModelDirectories.pas' {FormModelDir},
  frmSelectDirectory in 'frmSelectDirectory.pas' {FormSelectDirectory},
  Link_Jupiter in 'Link_Jupiter.pas',
  frmParallelJupiter in 'frmParallelJupiter.pas' {FormParallelJupiter},
  frmPredictions in 'frmPredictions.pas' {FormPredictions},
  frmConfigurePredTablesUnit in 'frmConfigurePredTablesUnit.pas' {FormConfigurePredTables},
  frmEditor in 'frmEditor.pas' {FormEditor},
  frmUcodeBasicSettingsUnit in 'frmUcodeBasicSettingsUnit.pas' {FormUcodeFileNames},
  frmPriorInfo in 'frmPriorInfo.pas' {FormPriorInfo},
  PriorInfoUnit in 'PriorInfoUnit.pas',
  frmConfigurePriTablesUnit in 'frmConfigurePriTablesUnit.pas' {FormConfigPriTables},
  frmParamSetsByIter in 'frmParamSetsByIter.pas' {FormParamSetsByIter},
  frmDerivedParameters in 'frmDerivedParameters.pas' {FormDerivedParameters},
  ParallelControlUnit in 'ParallelControlUnit.pas',
  ParallelRunnerUnit in 'ParallelRunnerUnit.pas',
  frmParallelControl in 'frmParallelControl.pas' {FormParallelControl},
  frmParallelRunners in 'frmParallelRunners.pas' {FormParallelRunners},
  frmRunnerFiles in 'frmRunnerFiles.pas' {FormRunnerFiles},
  frmPriorInfoControl in 'frmPriorInfoControl.pas' {FormPriorInfoControl},
  frmRenameGroup in 'frmRenameGroup.pas' {FormRenameGroup},
  IniFileUtilities in '..\ModelMuse\IniFileUtilities.pas',
  frmNamingConvention in 'frmNamingConvention.pas' {FormNamingConvention};

{FormModflow}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ModelMate';
  FormStartUp := TFormStartUp.Create(nil);
  // First call to Application.CreateForm establishes main form.
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TfrmProjName, frmProjName);
  Application.CreateForm(TFormModelCommands, FormModelCommands);
  Application.CreateForm(TFormModelIO, FormModelIO);
  Application.CreateForm(TFormObservations, FormObservations);
  Application.CreateForm(TFormConfigureObsTables, FormConfigureObsTables);
  Application.CreateForm(TFormProgramLocations, FormProgramLocations);
  Application.CreateForm(TFormModflow, FormModflow);
  Application.CreateForm(TFormParEstAdvanced, FormParEstAdvanced);
  Application.CreateForm(TFormOutput, FormOutput);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormModelDir, FormModelDir);
  Application.CreateForm(TFormSelectDirectory, FormSelectDirectory);
  Application.CreateForm(TFormParallelJupiter, FormParallelJupiter);
  Application.CreateForm(TFormPredictions, FormPredictions);
  Application.CreateForm(TFormConfigurePredTables, FormConfigurePredTables);
  Application.CreateForm(TFormUcodeFileNames, FormUcodeFileNames);
  Application.CreateForm(TFormPriorInfo, FormPriorInfo);
  Application.CreateForm(TFormConfigPriTables, FormConfigPriTables);
  Application.CreateForm(TFormParamSetsByIter, FormParamSetsByIter);
  Application.CreateForm(TFormDerivedParameters, FormDerivedParameters);
  Application.CreateForm(TFormConfigureParTables, FormConfigureParTables);
  Application.CreateForm(TFormParallelControl, FormParallelControl);
  Application.CreateForm(TFormParallelRunners, FormParallelRunners);
  Application.CreateForm(TFormRunnerFiles, FormRunnerFiles);
  Application.CreateForm(TFormPriorInfoControl, FormPriorInfoControl);
  Application.CreateForm(TfrmUcodeSettings, frmUcodeSettings);
  Application.CreateForm(TfrmParEstSettings, frmParEstSettings);
  Application.CreateForm(TFormRenameGroup, FormRenameGroup);
  Application.CreateForm(TFormNamingConvention, FormNamingConvention);
  Application.Run;
end.
