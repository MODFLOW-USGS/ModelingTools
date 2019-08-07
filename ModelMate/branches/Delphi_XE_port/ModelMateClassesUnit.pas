unit ModelMateClassesUnit;

interface

  uses Classes, Dialogs,
       {$WARN UNIT_PLATFORM OFF}
       FileCtrl,
       {$WARN UNIT_PLATFORM ON}
       SysUtils, Windows,
       UcodeUnit, GlobalBasicData, GlobalTypesUnit, JupiterUnit,
       DependentsUnit, Utilities, PriorInfoUnit, sskutils,
       JvBaseDlg, JvProgressDialog;

  type

    // Parameter attribute data.
    TParameterAttribute = class(TCollectionItem)
      private
        fParamAttType: TParamAttType; // an enumeration.
        fText: string;
      published
        property ParamAttType: TParamAttType read fParamAttType
                               write fParamAttType;
        property Text: string read fText write fText;
      public
        destructor Destroy; override;
        function Caption: string;
        function DefaultText: string;
        function Hint: string;
        function ItemType: TItemType;
        procedure Assign(Source: TPersistent); override;
        procedure Initialize(PAType: TParamAttType);
    end; // end of TParameterAttribute

    TParameterAttributes = class(TCollection)
      protected
        function GetItem(Index: Integer): TParameterAttribute;
        procedure SetItem(Index: Integer;
                          aPAttribute: TParameterAttribute);
      public
        property Items[Index: Integer]: TParameterAttribute
             read GetItem write SetItem; default;
        constructor Create;
        destructor Destroy; override;
//        procedure Empty;
        function Add: TParameterAttribute;
        procedure Assign(Source: TPersistent); override;
    end;

    // Parameter setup attribute data
    { A TParameterSetupAttribute is a TParameterAttribute with the addition of
      property ControlMethod, which is used to select the data grid
      (Parameter Groups or Parameters) used to control the parameter attribute }
    TParameterSetupAttribute = class(TParameterAttribute)
      private
        fControlMethod: TControlMethod;
      published
        property ControlMethod: TControlMethod read fControlMethod
                                write fControlMethod;
      public
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
    end; // end of TParameterSetupAttribute.

    TParameterSetupAttributes = class(TCollection)
      protected
        function GetItem(Index: Integer): TParameterSetupAttribute;
        procedure SetItem(Index: Integer;
                          aPSetupAttribute: TParameterSetupAttribute);
      public
        property Items[Index: Integer]: TParameterSetupAttribute read GetItem
             write SetItem; default;
        constructor Create;
        destructor Destroy; override;
        function Add: TParameterSetupAttribute;
//        procedure Empty;
    end;

    TParameterSetup = class(TComponent)
    { TParameterSetup is used to define and store the configuration of the
      Parameter Groups and Parameters data grids }
      private
        fNumAtt: integer;
        fAllParAttributes: TParameterSetupAttributes;
      published
        property NumAtt: integer read fNumAtt write fNumAtt;
        property ParAttributes: TParameterSetupAttributes read fAllParAttributes
             write fAllParAttributes;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure SetCombinedMethods;
        procedure Assign(Source: TPersistent); override;
        procedure Populate(Source: TPersistent);
        procedure SetControlMethod(paType: TParamAttType; ControlMethod: TControlMethod);
    end; // end of TParameterSetup.

    TParam = class(TCollectionItem)
      // One instance defines one Parameter.
      private
        // Fields.
        fParamName: string12;
        fAllAtts: TParameterAttributes; // a TCollection.
        fDerivEqn: string;
      public
        //  Methods.
        constructor Create(aParent: TCollection); override;
        constructor CreateAndName(aParent: TCollection; aName: string12;
                         aGroupName: string12);
        constructor CreateNameValue(aParent: TCollection; aName: string12;
                         aGroupName: string12; aValue: string);
        procedure Assign(Source: TPersistent); override;
        procedure Initialize(aName: string12; aGroupName: string12);
        procedure Populate(Source: TPersistent);
        destructor Destroy; override;
        function GetAttributeByType(paType: TParamAttType): string;
        procedure SetAttributeByType(paType: TParamAttType; Text: string);
        // TODO 3 : In TParam, need method GetAttributeByCaption.
      published
        property Name: string12 read fParamName write fParamName;
        property AllAtts: TParameterAttributes read fAllAtts write fAllAtts;
        property DerivEqn: string read fDerivEqn write fDerivEqn;
    end; // end of TParam.

    TParamSet = class(TCollection)
      private
        function GetItem(Index: integer): TParam;
        procedure SetItem(Index: integer; const Value: TParam);
      public
        property Items[I: integer]: TParam read GetItem write SetItem;
        constructor Create;
        destructor Destroy; override;
        function Add: TParam;
        procedure Assign(Source: TPersistent); override;
        procedure ChangeGroupNames(OldGroup, NewGroup: string);
        function CountDerivedParameters: integer;
        procedure SetGpDefault;
        function NumParsByGroup(GpName: string): integer;
    end;  // end of TParamset

    TProject = class(TComponent)
      { Properties and methods for one ModelMate project.
        The properties are only those that would be common to all types
        of projects (UCODE, Pest, OPR-PPR, etc.).  Properties are
        published so that they will be written to and read from a stream
        using the TStream.ReadComponent and .WriteComponent methods}
      private
        fModelMateVersion: string;
        fName: string255;
        fTitle: string255;
        fNameLikeProjectFile: boolean;
        fModelDirectory: string;       // Always stored as a relative path.
        fModelDirectoryPred: string;   // Always stored as a relative path.
        //fAppDirectory: string;         // Always stored as a relative path.
        fFileName: TFileName;
        fModelID: TModelID;
        fModflowNameFile: TFileName;   // Always stored as a relative path.
        fModflowNameFilePred: TFileName;   // Always stored as a relative path.
        fActiveApp: TActiveApp;
        fParameterSetup: TParameterSetup;
        fParGpSet: TParamSet;
        fParamSet: TParamSet;
        fObservationSetup: TObservationSetup;
        fObsGpSet: TDepSet;
        fObsSet: TDepSet;
        fPredictionSetup: TPredictionSetup;
        fPredGpSet: TDepSet;
        fPredSet: TDepSet;
        fPriorSetup: TPriorSetup;
        fPriGpSet: TPriSet;
        fPriSet: TPriSet;
        fMCLForward: string;  // Model command line, forward.
        fMCLFwdDeriv: string; // Model command line, forward & derivatives.
        fMCLPred: string;  // Model command line, forward.
        fMCLPredDeriv: string; // Model command line, forward & derivatives.
        fMIFiles: TModelIOPairs;
        fMOFiles: TModelIOPairs;
        fMIFilesPred: TModelIOPairs;
        fMOFilesPred: TModelIOPairs;
        fUcProject: TUcProject;
        fUseObsGps: boolean;
        fUsePredGps: boolean;
        fUseParGps: boolean;
        fUsePriGps: boolean;
        fUsePriorInfo: boolean;
        fLinkTemplateToParamsTable: boolean;
        procedure SetUcProject(const Value: TUcProject);
        procedure SetModelDirectory(const Value: string);
        procedure SetModflowNameFile(const Value: TFileName);
        procedure SetModflowNameFilePred(const Value: TFileName);
        procedure SetModelDirectoryPred(const Value: string);
      public
        property FileName: TFileName read fFileName write fFileName;
        constructor Create(AOwner: TComponent);  override;
        destructor Destroy; override;
        //
        function AbsAppDirectory(ModelUse: TModelUse): string;
        function AbsModelDirectory(ModelUse: TModelUse): string;
        function AbsModflowNameFile(ModelUse: TModelUse): string;
        procedure Assign(Source: TPersistent); override;
        procedure AssignVersion;
        function BuildModelCommandLinesBlock(CLBlock: TStringList): boolean;
        function BuildModelIOBlock(FileUse: TMIOFileUse; MIOBlock: TStringList;
                                   ModelUse: TModelUse): boolean;
        function BuildObservationDataBlock(BlkFmt: TBlockFormat; ODBlock: TStringList): boolean;
        procedure BuildObservationGpsBlock(BlkFmt: TBlockFormat; OGBlock: TStringList);
        function BuildParameterDataBlock(BlkFmt: TBlockFormat; PDBlock: TStringList): boolean;
        function BuildDerivedParametersBlock(const BlkFmt: TBlockFormat; DPBlock: TStringList): boolean;
//        function BuildParallelControlBlock(PCBlock: TStringList): boolean;
//        function BuildParallelRunnersBlock(BlkFmt: TBlockFormat; PRBlock: TStringList): boolean;
        procedure BuildParameterGpsBlock(BlkFmt: TBlockFormat; PGBlock: TStringList);
        function BuildPredictionDataBlock(BlkFmt: TBlockFormat; PrDBlock: TStringList): boolean;
        procedure BuildPredictionGpsBlock(BlkFmt: TBlockFormat; PrGBlock: TStringList);
        function BuildPriorInfoBlock(BlkFmt: TBlockFormat; PriDBlock: TStringList): boolean;
        function BuildPriorInfoGpsBlock(BlkFmt: TBlockFormat; PriGBlock: TStringList): boolean;
        procedure CorrectRelativePaths(OrigProjDir: string);
        procedure DefineObsGroups;
        procedure DefineObsSetup;
        procedure DefinePredGroups;
        procedure DefinePredSetup;
        procedure ExportOmitFile(FileName: string);
        function ExportUcodeFile(ModelUse: TModelUse; ProgressDialog: TJvProgressDialog): boolean;
        function GetTemplateFile(const MIOFile: TFileName; const ModelUse: TModelUse): string;
        function LocateModflowNameFile(ModelUse: TModelUse): boolean;
        function MakeUcodeBatchFile(ModelUseLocal: TModelUse;
                   var BatchFileLocation: string; var ErrMess: string): boolean;
        procedure NormalizePaths();
        function NumAdjustable(): integer;
        procedure SetupObsGroupUsage;
        procedure SetupParGroupUsage;
        procedure SetupPredGroupUsage;
        procedure SetupPriGroupUsage;
        procedure WriteToStream(SStream: TStringStream);
      published
        property ModelMateVersion: string read fModelMateVersion write fModelMateVersion;
        property ProjName: string255 read fName write fName;
        property Title: string255 read fTitle write fTitle;
        property NameLikeProjectFile: boolean read fNameLikeProjectFile
                                              write fNameLikeProjectFile;
        property ModelDirectory: string read fModelDirectory write SetModelDirectory;
        property ModelDirectoryPred: string read fModelDirectoryPred
                                            write SetModelDirectoryPred;
        property ModelID: TModelID read fModelID write fModelID;
        property ModflowNameFile: TFileName read fModflowNameFile
                                            write SetModflowNameFile;
        property ModflowNameFilePred: TFileName read fModflowNameFilePred
                                            write SetModflowNameFilePred;
        property ActiveApp: TActiveApp read fActiveApp write fActiveApp;
        property ParameterSetup: TParameterSetup read fParameterSetup
                                                 write fParameterSetup;
        property ParGpSet: TParamSet read fParGpSet write fParGpSet;
        property ParamSet: TParamSet read fParamSet write fParamSet;
        property ObservationSetup: TObservationSetup read fObservationSetup
                                                     write fObservationSetup;
        property ObsGpSet: TDepSet read fObsGpSet write FObsGpSet;
        property ObsSet: TDepSet read fObsSet write fObsSet;
        property PredictionSetup: TPredictionSetup read fPredictionSetup
                                                     write fPredictionSetup;
        property PredGpSet: TDepSet read fPredGpSet write fPredGpSet;
        property PredSet: TDepSet read fPredSet write fPredSet;
        property PriorSetup: TPriorSetup read fPriorSetup write fPriorSetup;
        property PriGpSet: TPriSet read fPriGpSet write fPriGpSet;
        property PriSet: TPriSet read fPriSet write fPriSet;
        property MCLForward: string read fMCLForward write fMCLForward;
        property MCLFwdDeriv: string read fMCLFwdDeriv write fMCLFwdDeriv;
        property MCLPred: string read fMCLPred write fMCLPred;
        property MCLPredDeriv: string read fMCLPredDeriv write fMCLPredDeriv;
        property MIFiles: TModelIOPairs read fMIFiles write fMIFiles;
        property MOFiles: TModelIOPairs read fMOFiles write fMOFiles;
        property MIFilesPred: TModelIOPairs read fMIFilesPred write fMIFilesPred;
        property MOFilesPred: TModelIOPairs read fMOFilesPred write fMOFilesPred;
        property UcProject: TUcProject read fUcProject write SetUcProject;
        property UseObsGps: boolean read fUseObsGps write fUseObsGps;
        property UsePredGps: boolean read fUsePredGps write fUsePredGps;
        property UseParGps: boolean read fUseParGps write fUseParGps;
        property UsePriGps: boolean read fUsePriGps write fUsePriGps;
        property UsePriorInfo: boolean read fUsePriorInfo write fUsePriorInfo;
        property LinkTemplateToParamsTable: boolean read fLinkTemplateToParamsTable
                                                  write fLinkTemplateToParamsTable;
    end; // end of TProject

  var
    AttributeTypes: array of TParamAttType;
    DefaultCtrlMethod: array of TControlMethod;

  procedure FreeModelMateData;
  procedure InitializeModelMateData;
  function ParAttPos(PAT: TParamAttType): integer;

implementation

//###################################################################

// Procedures

procedure FreeModelMateData;
begin
  SetLength(AttributeTypes, 0);
  SetLength(DefaultCtrlMethod, 0);
end;

procedure InitializeModelMateData;
begin
  //
  SetLength(AttributeTypes, NumParAttributes);
  SetLength(DefaultCtrlMethod, NumParAttributes);
  { This order determines order of attributes in data grids, and default
    assignment to Parameter Groups or Parameters table }
  AttributeTypes[0] := patGroupName;          DefaultCtrlMethod[0] := cmByItem;
  AttributeTypes[1] := patDerived;            DefaultCtrlMethod[1] := cmByDefault;
  AttributeTypes[2] := patStartValue;         DefaultCtrlMethod[2] := cmByItem;
  AttributeTypes[3] := patAdjustable;         DefaultCtrlMethod[3] := cmByGroup;
  AttributeTypes[4] := patTransform;          DefaultCtrlMethod[4] := cmByGroup;
  AttributeTypes[5] := patTolPar;             DefaultCtrlMethod[5] := cmByDefault;
  AttributeTypes[6] := patMaxChange;          DefaultCtrlMethod[6] := cmByGroup;
  AttributeTypes[7] := patPerturbAmt;         DefaultCtrlMethod[7] := cmByDefault;
  AttributeTypes[8] := patReasRange;          DefaultCtrlMethod[8] := cmByItem;
  AttributeTypes[9] := patLowerValue;         DefaultCtrlMethod[9] := cmByItem;
  AttributeTypes[10] := patUpperValue;        DefaultCtrlMethod[10] := cmByItem;
  AttributeTypes[11] := patSenMethod;         DefaultCtrlMethod[11] := cmByGroup;
  AttributeTypes[12] := patConstraints;       DefaultCtrlMethod[12] := cmByDefault;
  AttributeTypes[13] := patConstrain;         DefaultCtrlMethod[13] := cmByDefault;
  AttributeTypes[14] := patLowerBound;        DefaultCtrlMethod[14] := cmByDefault;
  AttributeTypes[15] := patUpperBound;        DefaultCtrlMethod[15] := cmByDefault;
  AttributeTypes[16] := patScalePval;         DefaultCtrlMethod[16] := cmByDefault;
  AttributeTypes[17] := patSosIncrement;      DefaultCtrlMethod[17] := cmByDefault;
  AttributeTypes[18] := patNonLinearInterval; DefaultCtrlMethod[18] := cmByDefault;
  AttributeTypes[19] := patParamName;         DefaultCtrlMethod[19] := cmByDefault;
end;

function ParAttPos(PAT: TParamAttType): integer;
// Assign parameter attribute index in arrays
var
  I: Integer;
begin
  result := -9;
  I := 0;
  while (I < NumParAttributes) and (result = -9) do
    begin
      if AttributeTypes[I] = PAT then
        result := I;
      I := I+1;
    end;
  { This order determines order of attributes in data grids, and default
    assignment to Parameter Groups or Parameters table }
end;

//###################################################################

{ TProject }

function TProject.AbsModelDirectory(ModelUse: TModelUse): string;
// Generate absolute path for ModelDirectory.
begin
  case ModelUse of
    muCalib: result := RelDirToAbsDir(ProjectDirectory, ModelDirectory);
    muPred: result := RelDirToAbsDir(ProjectDirectory, ModelDirectoryPred);
  end;
end;

function TProject.AbsModflowNameFile(ModelUse: TModelUse): string;
// Generate absolute path for ModflowNameFile.
begin
  case ModelUse of
    muCalib:
      begin
        if ModflowNameFile <> '' then
          result := PathToAbsPath(ProjectDirectory, ModflowNameFile);
      end;
    muPred:
      begin
        if ModflowNameFilePred <> '' then
          result := PathToAbsPath(ProjectDirectory, ModflowNameFilePred);
      end;
  end;
end;

function TProject.AbsAppDirectory(ModelUse: TModelUse): string;
// Generate absolute path for application directory, which is where
// main input file for active application resides.
var
  AppDir: string;
begin
  result := '';
  case self.ActiveApp of
    aaUcode:
      begin
        AppDir := UcProject.AbsMainInputFileName(ModelUse);
        if AppDir <> '' then
          result := ExtractFileDir(AppDir);
      end;
    aaPest: ;
    aaApp3: ;
    aaApp4: ;
  end;
end;

procedure TProject.Assign(Source: TPersistent);
var
  PrjSource: TProject;
begin
  if Source is TProject then
  begin
    PrjSource := TProject(Source); // cast Source as TProject
    // Assign ModelMateVersion only when a file is saved
    ProjName := PrjSource.ProjName;
    Title := PrjSource.Title;
    NameLikeProjectFile := PrjSource.fNameLikeProjectFile;
    fModelDirectory := PrjSource.ModelDirectory;
    fModelDirectoryPred := PrjSource.ModelDirectoryPred;
//    fAppDirectory := PrjSource.AppDirectory;
    FileName := PrjSource.Filename;
    ModelID := PrjSource.ModelID;
    fModflowNameFile := PrjSource.ModflowNameFile;
    fModflowNameFilePred := PrjSource.ModflowNameFilePred;
    ActiveApp := PrjSource.ActiveApp;
    ParameterSetup.Assign(PrjSource.ParameterSetup);
    ParGpSet.Assign(PrjSource.ParGpSet);
    ParamSet.Assign(PrjSource.ParamSet);
    ObservationSetup.Assign(PrjSource.ObservationSetup);
    ObsGpSet.Assign(PrjSource.ObsGpSet);
    ObsSet.Assign(PrjSource.ObsSet);
    PredictionSetup.Assign(PrjSource.PredictionSetup);
    PredGpSet.Assign(PrjSource.PredGpSet);
    PredSet.Assign(PrjSource.PredSet);
    PriorSetup.Assign(PrjSource.PriorSetup);
    PriGpSet.Assign(PrjSource.PriGpSet);
    PriSet.Assign(PrjSource.PriSet);
    MCLForward := PrjSource.MCLForward;
    MCLFwdDeriv := PrjSource.MCLFwdDeriv;
    MCLPred := PrjSource.MCLPred;
    MCLPredDeriv := PrjSource.MCLPredDeriv;
    MIFiles.Assign(PrjSource.MIFiles);
    MOFiles.Assign(PrjSource.MOFiles);
    MIFilesPred.Assign(PrjSource.MIFilesPred);
    MOFilesPred.Assign(PrjSource.MOFilesPred);
    UcProject.Assign(PrjSource.UcProject);
    UseObsGps := PrjSource.UseObsGps;
    UsePredGps := PrjSource.UsePredGps;
    UseParGps := PrjSource.UseParGps;
    UsePriGps := PrjSource.UsePriGps;
    UsePriorInfo := PrjSource.UsePriorInfo;
    LinkTemplateToParamsTable := PrjSource.LinkTemplateToParamsTable;
  end;
end;

procedure TProject.AssignVersion;
var
  Build: string;
  FullVersion: string;
  ShortVersion: string;
begin
  FullVersion := getMyFileVersion;
  ParseVersion(FullVersion, ShortVersion, Build);
  ModelMateVersion := ShortVersion;
end; // procedure TProject.Assign.

function TProject.BuildDerivedParametersBlock(const BlkFmt: TBlockFormat;
  DPBlock: TStringList): boolean;
  // Result is number of derived parameters written to StringList DPBlock.
  // If there are no derived parameters, 0 is returned.
  // If an error is encountered, -1 is returned.
var
  FoundError: Boolean;
  IAT, J, JRow, NC, NPar, NR: Integer;
  Messg, PName, S: string;
  bdDerivedParameters: TBlockData;     // Derived_Parameters.
begin
  DPBlock.Clear;
  // Determine numbers of rows (# parameters)
  // needed for the Derived_Parameters input block.
  FoundError := False;
  IAT := ParAttPos(patDerived);
  NPar := ParamSet.Count;
  NR := ParamSet.CountDerivedParameters;
  NC := 2; // Number of columns is always 2.
  // Populate KeyValMatrix with keywords and values for all derived parameters.
  if (NR > 0) then
    begin
      bdDerivedParameters := TBlockData.CreateAndAllocate(NR,NC);
      // Populate keywords.
      bdDerivedParameters.KeyValMatrix[0].Name := 'DerParName';
      bdDerivedParameters.KeyValMatrix[1].Name := 'DerParEqn';
      JRow := -1;
      for J := 0 to NPar - 1 do
        if StrToBoolean(ParamSet.Items[J].AllAtts.Items[IAT].Text) then
          begin
            JRow := JRow + 1;
            S := ParamSet.Items[J].Name;
            bdDerivedParameters.KeyValMatrix[0].SetVal(JRow,S);
            // Get equation for this derived parameter
            if not FoundError then
              begin
                S := ParamSet.Items[J].DerivEqn;
                if (S = '') then
                  begin
                    PName := ParamSet.Items[J].Name;
                    Messg := 'Error: ' +  'Equation for parameter "' + PName + '" is blank';
                    ShowMessage(Messg);
                    FoundError := True;
                  end;
                // Store equation.
                bdDerivedParameters.KeyValMatrix[1].SetVal(JRow,S);
              end;
          end;
      if not FoundError then
        begin
          J_BuildInputBlock('Derived_Parameters', BlkFmt, bdDerivedParameters, DPBlock);
        end
      else
        begin
          Messg := '# Derived_Parameters input block not generated because of error';
          DPBlock.Add(Messg);
        end;
      FreeAndNil(bdDerivedParameters);
    end
  else
    // NR = 0.
    begin
      Messg := '# Derived_Parameters input block not generated because it would be empty';
      DPBlock.Add(Messg);
    end;
  result := not FoundError;
end;

function TProject.BuildModelCommandLinesBlock(CLBlock: TStringList): boolean;
var
  Command, CmdLine, Messg: string;
  bdModelCommandLines: TBlockData; // Model_Command_Lines.
begin
  case self.ActiveApp of
    aaUcode:
      begin
        case UcProject.UcMode of
          umFwd: CmdLine := MCLForward;
          umSensAnal: CmdLine := MCLForward;
          umParEst: CmdLine := MCLForward;
          umTestLin: CmdLine := MCLForward;
          umPred: CmdLine := self.MCLPred;
          umAdvTestLin: CmdLine := MCLForward;
          umNonlinUncert: CmdLine := MCLForward;
          umInvObjFunc: CmdLine := MCLForward;
        end;
      end;
    aaPest: CmdLine := '';
  else
    CmdLine := '';
  end;
  CLBlock.Clear;
  // Need 3 columns (keywords) in bdModelCommandLines and 1 row.
  // To support MODFLOW-2000 Sensitivity Process, will need 2 rows.
  bdModelCommandLines := TBlockData.CreateAndAllocate(1,3);
  bdModelCommandLines.KeyValMatrix[0].Vtype := vtStr;
  bdModelCommandLines.KeyValMatrix[1].Vtype := vtStr;
  bdModelCommandLines.KeyValMatrix[2].Vtype := vtStr;
  if CmdLine = '' then
    begin
      Messg := 'Project lacks a Model Command Line';
      ShowMessage(Messg);
      Messg := '# Model_Command_Lines block not generated because command line is not defined.';
      CLBlock.Add(Messg);
      result := False;
    end
  else
    begin
      Command := CmdLine; // Allow command to include argument(s)
      bdModelCommandLines.KeyValMatrix[0].SetNameVal(0,'Command',Command);
      bdModelCommandLines.KeyValMatrix[1].SetNameVal(0,'Purpose','Forward');
      bdModelCommandLines.KeyValMatrix[2].SetNameVal(0,'CommandID','ForwardModel');
      J_BuildInputBlock('Model_Command_Lines', bfKeywords, bdModelCommandLines, CLBlock);
      result := True;
    end;
  FreeAndNil(bdModelCommandLines);
end; // procedure TProject.BuildModelCommandLinesBlock.

function TProject.BuildModelIOBlock(FileUse: TMIOFileUse; MIOBlock: TStringList;
                                    ModelUse: TModelUse): boolean;
// Build either a Model_Input_Files or a Model_Output_Files input block,
// depending on FileUse.
var
  I, NC, NR: Integer;
  BLabel, KeyItem, Keyword0, Keyword1, Keyword2, Msg1, Msg2: string;
  AbsAppDir, AbsAppFile, AbsModelDir, AbsModelFile, RelAppFile, RelModelFile: string;
  MFiles: TModelIOPairs;
  bdModelIOFiles: TBlockData;      // Model_Input_Files OR Model_Output_Files.
begin
  AbsAppDir := AbsAppDirectory(ModelUse);
  AbsModelDir := AbsModelDirectory(ModelUse);
  MIOBlock.Clear;
  MFiles := nil;
  NR := 0;
  NC := 0;
  // Set NR = number of model file/app file pairs.
  // Define variables based on FileUse.
  case FileUse of
    fuInput:  // Build a Model_Input_Files input block.
      begin
        case ModelUse of
          muCalib: MFiles := MIFiles; // Pointer assignment to define source of data.
          muPred: MFiles := MIFilesPred;
        end;
        BLabel := 'Model_Input_Files';
        KeyItem := 'ModInFile';
        NR := MFiles.Count;
        NC := 2;
        Msg1 := '# Model_Input_Files input block not generated because it would be empty';
        case ModelUse of
          muCalib: Msg2 := 'Model_Input_Files input block not generated because there are no model input files entered';
          muPred: Msg2 := 'Model_Input_Files input block not generated because there are no prediction-model input files entered';
        end;
        Keyword0 := 'ModInFile';
        Keyword1 := 'TemplateFile';
        Keyword2 := '';
      end;
    fuOutput: // Build a Model_Output_Files input block
      begin
        case ModelUse of
          muCalib: MFiles := MOFiles; // Pointer assignment to define source of data;
          muPred: MFiles := MOFilesPred; // Pointer assignment to define source of data;
        end;
        BLabel := 'Model_Output_Files';
        KeyItem := 'ModOutFile';
        NR := MFiles.Count;
        NC := 3;
        Msg1 := '# Model_Output_Files input block not generated because it would be empty';
        case ModelUse of
          muCalib: Msg2 := 'Model_Output_Files input block not generated because there are no model output files entered';
          muPred: Msg2 := 'Model_Output_Files input block not generated because there are no prediction-model output files entered';
        end;
        Keyword0 := 'ModOutFile';
        Keyword1 := 'InstructionFile';
        Keyword2 := 'Category';
      end;
  end;
  if (NR > 0) then
    begin
      // allocate TBlockData object
      bdModelIOFiles := TBlockData.CreateAndAllocate(NR,NC);
      // Assign keyword for each column
      bdModelIOFiles.KeyValMatrix[0].Name := Keyword0;
      bdModelIOFiles.KeyValMatrix[1].Name := Keyword1;
      if NC > 2 then bdModelIOFiles.KeyValMatrix[2].Name := Keyword2;
      for I := 0 to NR - 1 do
        // Store a model-file/app-file pair of file names
        begin
//          if AnsiSameText(AbsAppDir, AbsModelDir) then
            begin
              AbsModelFile := PathToAbsPath(ProjectDirectory,MFiles.Items[I].ModelFile);
              RelModelFile := MyExtractRelativePath(AbsModelDir,AbsModelFile);
              AbsAppFile := PathToAbsPath(ProjectDirectory,MFiles.Items[I].AppFile);
              RelAppFile := MyExtractRelativePath(AbsModelDir,AbsAppFile);
              bdModelIOFiles.KeyValMatrix[0].SetVal(I,RelModelFile);
              bdModelIOFiles.KeyValMatrix[1].SetVal(I,RelAppFile);
//            end
//          else
//            begin
//              bdModelIOFiles.KeyValMatrix[0].SetVal(I,MFiles.Items[I].AbsModelFile);
//              bdModelIOFiles.KeyValMatrix[1].SetVal(I,MFiles.Items[I].AbsAppFile);
            end;
          if NC > 2 then
            case ModelUse of
              muCalib: bdModelIOFiles.KeyValMatrix[2].SetVal(I,'Obs');
              muPred: bdModelIOFiles.KeyValMatrix[2].SetVal(I,'Pred');
            end;
        end;
      // Build input block
      J_BuildInputBlock(BLabel, bfKeywords, bdModelIOFiles, MIOBlock);
      FreeAndNil(bdModelIOFiles);
      result := True;
    end
  else
    // NR = 0.
    begin
      MIOBlock.Add(Msg1);
      ShowMessage(Msg2);
      result := False;
    end;
end; // procedure TProject.BuildModelIOBlock

function TProject.BuildObservationDataBlock(BlkFmt: TBlockFormat; ODBlock: TStringList): boolean;
var
  FoundError: Boolean;
  I, IC, J, NC, NR: Integer;
  Cap, Keyword, Messg, OName, S: string;
  datTemp: TDepAttType;
  cmTemp: TControlMethod;
  bdObservationData: TBlockData;   // Observation_Data.
begin
  ODBlock.Clear;
  // Determine numbers of rows (# observations) and columns (# attributes)
  // needed for the Observations_Data input block
  FoundError := False;
  NR := ObsSet.Count;
  NC := 1; // always include observation name
  for I := 0 to ObservationSetup.ObsAttributes.Count - 1 do
    begin
      datTemp := ObservationSetup.ObsAttributes.Items[I].DepAttType;
      cmTemp := ObservationSetup.ObsAttributes.Items[I].ControlMethod;
      if (datTemp in AttTypesObs) and (cmTemp = cmByItem) then
        begin
          Keyword := GT_DepAttKeyword(ObservationSetup.ObsAttributes.Items[I].DepAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all observations
  if (NR > 0) then
    begin
      bdObservationData := TBlockData.CreateAndAllocate(NR,NC);
      // Populate ObsName column
      Keyword := GT_DepAttKeyword(datObsName);
      bdObservationData.KeyValMatrix[0].Name := Keyword;
      for J := 0 to NR - 1 do
        begin
          S := ObsSet.Items[J].Name;
          bdObservationData.KeyValMatrix[0].SetNameVal(J,Keyword,S);
        end;
      // For each keyword with ControlMethod=ocmByObservation, store values for
      // all observations
      IC := 0;
      for I := 0 to ObservationSetup.ObsAttributes.Count - 1 do
        begin
          datTemp := ObservationSetup.ObsAttributes.Items[I].DepAttType;
          cmTemp := ObservationSetup.ObsAttributes.Items[I].ControlMethod;
          // Check that attribute should be included in Observation_Data block
          if (datTemp in AttTypesObs) and (cmTemp = cmByItem) then
            begin
              Keyword := GT_DepAttKeyword(ObservationSetup.ObsAttributes.Items[I].DepAttType);
              if Keyword <> 'not_a_keyword' then
                begin
                  // For each observation, store value for current keyword
                  IC := IC + 1;
                  bdObservationData.KeyValMatrix[IC].Name := Keyword;
                  for J := 0 to NR - 1 do
                    begin
                      if not FoundError then
                        begin
                          S := ObsSet.Items[J].AllAtts.Items[I].Text;
                          if (S = '') then
                            begin
                              Cap := ObsSet.Items[J].AllAtts.Items[I].Caption;
                              OName := ObsSet.Items[J].Name;
                              Messg := 'Error: ' + Cap + ' for observation "' + OName + '" is blank';
                              ShowMessage(Messg);
                              FoundError := True;
                            end;
                          bdObservationData.KeyValMatrix[IC].SetVal(J,S);
                        end;
                    end;
                end;
            end;
        end;

      if not FoundError then
        J_BuildInputBlock('Observation_Data', bfTable, bdObservationData, ODBlock)
      else
        begin
          Messg := '# Observation_Data input block not generated because of error';
          ODBlock.Add(Messg);
        end;
      FreeAndNil(bdObservationData);
    end
  else
    // NR = 0.
    begin
      Messg := '# Observation_Data input block not generated because it would be empty';
      ODBlock.Add(Messg);
      Messg := 'Observation_Data input block not generated because there are no observations';
      ShowMessage(Messg);
      FoundError := True;
    end;
  result := not FoundError;
end; // procedure TProject.BuildObservationsDataBlock

procedure TProject.BuildObservationGpsBlock(BlkFmt: TBlockFormat; OGBlock: TStringList);
var
  FoundError: Boolean;
  I, IC, J, N, NC, NMembers, NR: Integer;
  Cap, Keyword, Messg, GName, S: string;
  datTemp: TDepAttType;
  cmTemp: TControlMethod;
  bdObservationGroups: TBlockData; // Observation_Groups.
begin
  OGBlock.Clear;
  // Determine numbers of rows (# observation groups) and columns (# attributes)
  // needed for the Observation_Groups input block
  FoundError := False;
  NC := 1; // always include group name
  for I := 0 to ObservationSetup.ObsAttributes.Count - 1 do
    begin
      datTemp := ObservationSetup.ObsAttributes.Items[I].DepAttType;
      cmTemp := ObservationSetup.ObsAttributes.Items[I].ControlMethod;
      if (datTemp in AttTypesObs) and (cmTemp = cmByGroup) then
        begin
          Keyword := GT_DepAttKeyword(ObservationSetup.ObsAttributes.Items[I].DepAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all observation groups
//  NR := ObsGpSet.Count;
  NR := 0;
  for N := 0 to ObsGpSet.Count - 1 do
    begin
      GName := ObsGpSet.Items[N].Name;
      NMembers := ObsSet.NumDepByGroup(GName);
      if NMembers >0 then
        NR := NR + 1;
    end;
  if (NR > 0) then
    begin
      bdObservationGroups := TBlockData.CreateAndAllocate(NR,NC);
      // Populate GroupName column
      Keyword := GT_DepAttKeyword(datGroupName);
      bdObservationGroups.KeyValMatrix[0].Name := Keyword;
      J := 0;
      for N := 0 to ObsGpSet.Count - 1 do
        begin
          GName := ObsGpSet.Items[N].Name;
          NMembers := ObsSet.NumDepByGroup(GName);
          if NMembers > 0 then
            begin
              S := ObsGpSet.Items[N].Name;
              bdObservationGroups.KeyValMatrix[0].SetNameVal(J,Keyword,S);
              J := J + 1;
            end;
        end;
      // For each keyword with ControlMethod=cmByGroup, store values for
      // all groups
      IC := 0;
      for I := 0 to ObservationSetup.ObsAttributes.Count - 1 do
        begin
          datTemp := ObservationSetup.ObsAttributes.Items[I].DepAttType;
          cmTemp := ObservationSetup.ObsAttributes.Items[I].ControlMethod;
          // Check that attribute should be included in Observation_Groups block
          if (datTemp in AttTypesObs) and (cmTemp = cmByGroup) then
            begin
              Keyword := GT_DepAttKeyword(datTemp);
              if Keyword <> 'not_a_keyword' then
                begin
                  // For each group, store value for current keyword.
                  IC := IC + 1;
                  bdObservationGroups.KeyValMatrix[IC].Name := Keyword;
                  J := 0;
                  for N := 0 to ObsGpSet.Count - 1 do
                    begin
                      GName := ObsGpSet.Items[N].Name;
                      NMembers := ObsSet.NumDepByGroup(GName);
                      if NMembers > 0 then
                        begin
                          if not FoundError then
                            begin
                              S := ObsGpSet.Items[N].AllAtts.Items[I].Text;
                              if (S = '') then
                                begin
                                  Cap := ObsGpSet.Items[N].AllAtts.Items[I].Caption;
                                  Messg := 'Error: ' + Cap + ' for observation group "'
                                           + GName + '" is blank';
                                  ShowMessage(Messg);
                                  FoundError := True;
                                end;
                              bdObservationGroups.KeyValMatrix[IC].SetVal(J,S);
                              J := J + 1;
                            end;
                        end;
                    end;
                end;
            end;
        end;
      if not FoundError then
        J_BuildInputBlock('Observation_Groups', BlkFmt, bdObservationGroups, OGBlock)
      else
        begin
          Messg := '# Observation_Groups input block not generated because of error';
          OGBlock.Add(Messg);
        end;
      FreeAndNil(bdObservationGroups);
    end
  else
    // NR = 0.
    begin
      Messg := '# Observation_Groups input block not generated because it would be empty';
      OGBlock.Add(Messg);
      Messg := 'Observation_Groups input block not generated because there are no observations';
      ShowMessage(Messg);
    end;
end; // procedure TProject.BuildObservationGpsBlock.

function TProject.BuildParameterDataBlock(BlkFmt: TBlockFormat; PDBlock: TStringList): boolean;
var
  FoundError: Boolean;
  I, IC, PapDeriv, PapParVal, JRow, J, NC, NPar, NR: Integer;
  Cap, Keyword, Messg, PName, S: string;
  PatTemp: TParamAttType;
  bdParameterData: TBlockData;     // Parameter_Data.
  ParVal: double;
begin
  PapDeriv := ParAttPos(patDerived);
  PapParVal := ParAttPos(patStartValue);
  PDBlock.Clear;
  // Determine numbers of rows (# parameters) and columns (# attributes)
  // needed for the Parameter_Data input block.
  FoundError := False;
  NPar := ParamSet.Count;
  NR := NPar - ParamSet.CountDerivedParameters;
  NC := 1; // Always include parameter name.
  for I := 0 to ParameterSetup.ParAttributes.Count - 1 do
    begin
      if ParameterSetup.ParAttributes.Items[I].ControlMethod = cmByItem then
        begin
          Keyword := GT_ParAttKeyword(ParameterSetup.ParAttributes.Items[I].ParamAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all non-derived parameters.
  if (NR > 0) then
    begin
      bdParameterData := TBlockData.CreateAndAllocate(NR,NC);
      // Populate ParamName column.
      Keyword := GT_ParAttKeyword(patParamName);
      bdParameterData.KeyValMatrix[0].Name := Keyword;
      JRow := -1;
      for J := 0 to NPar - 1 do
        if not StrToBoolean(ParamSet.Items[J].AllAtts.Items[PapDeriv].Text) then
          begin
            JRow := JRow + 1;
            S := ParamSet.Items[J].Name;
            bdParameterData.KeyValMatrix[0].SetVal(JRow,S);
          end;
      // For each keyword with ControlMethod=cmByParameter, store values for
      // all parameters.
      IC := 0;
      for I := 0 to ParameterSetup.ParAttributes.Count - 1 do
        if ParameterSetup.ParAttributes.Items[I].ControlMethod = cmByItem then
          begin
            PatTemp := ParameterSetup.ParAttributes.Items[I].ParamAttType;
            Keyword := GT_ParAttKeyword(PatTemp);
            if Keyword <> 'not_a_keyword' then
              begin
                // For each parameter, store value for current keyword.
                IC := IC + 1;
                bdParameterData.KeyValMatrix[IC].Name := Keyword;
                JRow := -1;
                for J := 0 to NPar - 1 do
                  if not StrToBoolean(ParamSet.Items[J].AllAtts.Items[PapDeriv].Text) then
                    begin
                      JRow := JRow + 1;
                      if not FoundError then
                        begin
                          if PatTemp = patSenMethod then
                              S := '';
                          S := ParamSet.Items[J].AllAtts.Items[I].Text;
                          if (S = '') then
                            begin
                              Cap := ParamSet.Items[J].AllAtts.Items[I].Caption;
                              PName := ParamSet.Items[J].Name;
                              Messg := 'Error: ' + Cap + ' for parameter "' + PName + '" is blank';
                              ShowMessage(Messg);
                              FoundError := True;
                            end;
                          if PatTemp = patScalePval then
                            begin
                              if AnsiSameText(S,'Starting parameter value / 100') then
                                begin
                                  ParVal := StrToFloat(ParamSet.Items[J].AllAtts.Items[PapParVal].Text);
                                  S := FloatToStr(ParVal / 100.0);
                                end;
                            end;
                          bdParameterData.KeyValMatrix[IC].SetVal(JRow,S);
                        end;
                    end;
              end;
          end;

//          end;
      if not FoundError then
        J_BuildInputBlock('Parameter_Data', BlkFmt, bdParameterData, PDBlock)
      else
        begin
          Messg := '# Parameter_Data input block not generated because of error';
          PDBlock.Add(Messg);
        end;
      FreeAndNil(bdParameterData);
    end
  else
    // NR = 0.
    begin
      Messg := '# Parameter_Data input block not generated because it would be empty';
      PDBlock.Add(Messg);
      Messg := 'Parameter_Data input block not generated because there are no parameters';
      ShowMessage(Messg);
      FoundError := True;
    end;
  result := not FoundError;
end; // function TProject.BuildParameterDataBlock.

procedure TProject.BuildParameterGpsBlock(BlkFmt: TBlockFormat; PGBlock: TStringList);
var
  FoundError: Boolean;
  I, IC, J, N, NC, NMembers, NR: Integer;
  Cap, Keyword, Messg, GName, S: string;
  PatTemp: TParamAttType;
  bdParameterGroups: TBlockData;   // Parameter_Groups.
begin
  PGBlock.Clear;
  // Determine numbers of rows (# parameters) and columns (# attributes)
  // needed for the Parameter_Groups input block
  FoundError := False;
  NR := 0;
  for N := 0 to ParGpSet.Count - 1 do
    begin
      GName := ParGpSet.Items[N].Name;
      NMembers := ParamSet.NumParsByGroup(GName);
      if NMembers >0 then
        NR := NR + 1;
    end;
  NC := 1; // Always include parameter group name
  for I := 0 to ParameterSetup.ParAttributes.Count - 1 do
    begin
      if ParameterSetup.ParAttributes.Items[I].ControlMethod = cmByGroup then
        begin
          Keyword := GT_ParAttKeyword(ParameterSetup.ParAttributes.Items[I].ParamAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all parameter groups
  if (NR > 0) then
    begin
      bdParameterGroups := TBlockData.CreateAndAllocate(NR,NC);
      // Populate GroupName column
      Keyword := GT_ParAttKeyword(patGroupName);
      bdParameterGroups.KeyValMatrix[0].Name := Keyword;
      J := 0;
      for N := 0 to ParGpSet.Count - 1 do
        begin
          GName := ParGpSet.Items[N].Name;
          NMembers := ParamSet.NumParsByGroup(GName);
          if NMembers >0 then
            begin
              S := ParGpSet.Items[N].Name;
              bdParameterGroups.KeyValMatrix[0].SetNameVal(J,Keyword,S);
              J := J + 1;
            end;
        end;
      // For each keyword with ControlMethod=cmByGroup, store values for
      // all parameters.
      IC := 0;
      for I := 0 to ParameterSetup.ParAttributes.Count - 1 do
        if ParameterSetup.ParAttributes.Items[I].ControlMethod = cmByGroup then
          begin
            PatTemp := ParameterSetup.ParAttributes.Items[I].ParamAttType;
            Keyword := GT_ParAttKeyword(PatTemp);
            if Keyword <> 'not_a_keyword' then
              begin
                // For each group, store value for current keyword
                IC := IC + 1;
                bdParameterGroups.KeyValMatrix[IC].Name := Keyword;
                J := 0;
                for N := 0 to ParGpSet.Count - 1 do
                  begin
                    GName := ParGpSet.Items[N].Name;
                    NMembers := ParamSet.NumParsByGroup(GName);
                    if NMembers >0 then
                      begin
                        if not FoundError then
                          begin
                            if PatTemp = patSenMethod then S := '';
                            S := ParGpSet.Items[N].AllAtts.Items[I].Text;
                            if (S = '') then
                              begin
                                Cap := ParGpSet.Items[N].AllAtts.Items[I].Caption;
                                Messg := 'Error: ' + Cap + ' for group "' + GName + '" is blank';
                                ShowMessage(Messg);
                                FoundError := True;
                              end;
                            bdParameterGroups.KeyValMatrix[IC].SetVal(J,S);
                            J := J + 1;
                          end;
                      end;
                  end;
              end;
          end;
      if not FoundError then
        J_BuildInputBlock('Parameter_Groups', BlkFmt, bdParameterGroups, PGBlock)
      else
        begin
          Messg := '# Parameter_Groups input block not generated because of error';
          PGBlock.Add(Messg);
        end;
      FreeAndNil(bdParameterGroups);
    end
  else
    // NR = 0.
    begin
      Messg := '# Parameter_Groups input block not generated because it would be empty';
      PGBlock.Add(Messg);
      Messg := 'Parameter_Groups input block not generated because there are no groups';
      ShowMessage(Messg);
    end;
end;  // procedure TProject.BuildParameterGpsBlock

function TProject.BuildPredictionDataBlock(BlkFmt: TBlockFormat;
  PrDBlock: TStringList): boolean;
var
  FoundError: Boolean;
  I, IC, J, NC, NR: Integer;
  Cap, Keyword, Messg, PrName, S: string;
  datTemp: TDepAttType;
  cmTemp: TControlMethod;
  bdPredictionData: TBlockData;    // Prediction_Data.
begin
  PrDBlock.Clear;
  // Determine numbers of rows (# predictions) and columns (# attributes)
  // needed for the Predictions_Data input block
  FoundError := False;
  NR := PredSet.Count;
  NC := 1; // always include prediction name.
  for I := 0 to PredictionSetup.PredAttributes.Count - 1 do
    begin
      datTemp := PredictionSetup.PredAttributes.Items[I].DepAttType;
      cmTemp := PredictionSetup.PredAttributes.Items[I].ControlMethod;
      if (datTemp in AttTypesPred) and (cmTemp = cmByItem) then
        begin
          Keyword := GT_DepAttKeyword(PredictionSetup.PredAttributes.Items[I].DepAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all predictions
  if (NR > 0) then
    begin
      bdPredictionData := TBlockData.CreateAndAllocate(NR,NC);
      // Populate PredName column
      Keyword := GT_DepAttKeyword(datPredName);
      bdPredictionData.KeyValMatrix[0].Name := Keyword;
      for J := 0 to NR - 1 do
        begin
          S := PredSet.Items[J].Name;
          bdPredictionData.KeyValMatrix[0].SetNameVal(J,Keyword,S);
        end;
      // For each keyword with ControlMethod=dcmByDependent, store values for
      // all predictions.
      IC := 0;
      for I := 0 to PredictionSetup.PredAttributes.Count - 1 do
        begin
          datTemp := PredictionSetup.PredAttributes.Items[I].DepAttType;
          cmTemp := PredictionSetup.PredAttributes.Items[I].ControlMethod;
          // Check that attribute should be included in Prediction_Data block.
          if (datTemp in AttTypesPred) and (cmTemp = cmByItem) then
            begin
              Keyword := GT_DepAttKeyword(PredictionSetup.PredAttributes.Items[I].DepAttType);
              if Keyword <> 'not_a_keyword' then
                begin
                  // For each prediction, store value for current keyword.
                  IC := IC + 1;
                  bdPredictionData.KeyValMatrix[IC].Name := Keyword;
                  for J := 0 to NR - 1 do
                    begin
                      if not FoundError then
                        begin
                          S := PredSet.Items[J].AllAtts.Items[I].Text;
                          if (S = '') then
                            begin
                              Cap := PredSet.Items[J].AllAtts.Items[I].Caption;
                              PrName := PredSet.Items[J].Name;
                              Messg := 'Error: ' + Cap + ' for prediction "' + PrName + '" is blank';
                              ShowMessage(Messg);
                              FoundError := True;
                            end;
                          bdPredictionData.KeyValMatrix[IC].SetVal(J,S);
                        end;
                    end;
                end;
            end;
        end;

      if not FoundError then
        J_BuildInputBlock('Prediction_Data', bfTable, bdPredictionData, PrDBlock)
      else
        begin
          Messg := '# Prediction_Data input block not generated because of error';
          PrDBlock.Add(Messg);
        end;
      FreeAndNil(bdPredictionData);
    end
  else
    // NR = 0.
    begin
      Messg := '# Prediction_Data input block not generated because it would be empty';
      PrDBlock.Add(Messg);
      Messg := 'Prediction_Data input block not generated because there are no predictions';
      ShowMessage(Messg);
      FoundError := True;
    end;
  result := not FoundError;
end; // procedure TProject.BuildPredictionDataBlock.

procedure TProject.BuildPredictionGpsBlock(BlkFmt: TBlockFormat;
  PrGBlock: TStringList);
var
  FoundError: Boolean;
  I, IC, J, N, NC, NMembers, NR: Integer;
  Cap, Keyword, Messg, GName, S: string;
  datTemp: TDepAttType;
  cmTemp: TControlMethod;
  bdPredictionGroups: TBlockData;  // Prediction_Groups.
begin
  PrGBlock.Clear;
  // Determine numbers of rows (# prediction groups) and columns (# attributes)
  // needed for the Prediction_Groups input block.
  FoundError := False;
  NC := 1; // always include group name.
  for I := 0 to PredictionSetup.PredAttributes.Count - 1 do
    begin
      datTemp := PredictionSetup.PredAttributes.Items[I].DepAttType;
      cmTemp := PredictionSetup.PredAttributes.Items[I].ControlMethod;
      if (datTemp in AttTypesPred) and (cmTemp = cmByGroup) then
        begin
          Keyword := GT_DepAttKeyword(PredictionSetup.PredAttributes.Items[I].DepAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all prediction groups.
  NR := 0;
  for N := 0 to PredGpSet.Count - 1 do
    begin
      GName := PredGpSet.Items[N].Name;
      NMembers := PredSet.NumDepByGroup(GName);
      if NMembers >0 then
        NR := NR + 1;
    end;
  if (NR > 0) then
    begin
      bdPredictionGroups := TBlockData.CreateAndAllocate(NR,NC);
      // Populate GroupName column.
      Keyword := GT_DepAttKeyword(datGroupName);
      bdPredictionGroups.KeyValMatrix[0].Name := Keyword;
      J := 0;
      for N := 0 to PredGpSet.Count - 1 do
        begin
          GName := PredGpSet.Items[N].Name;
          NMembers := PredSet.NumDepByGroup(GName);
          if NMembers >0 then
            begin
              S := PredGpSet.Items[N].Name;
              bdPredictionGroups.KeyValMatrix[0].SetNameVal(J,Keyword,S);
              J := J + 1;
            end;
        end;
      // For each keyword with ControlMethod=cmByGroup, store values for
      // all groups.
      IC := 0;
      for I := 0 to PredictionSetup.PredAttributes.Count - 1 do
        begin
          datTemp := PredictionSetup.PredAttributes.Items[I].DepAttType;
          cmTemp := PredictionSetup.PredAttributes.Items[I].ControlMethod;
          // Check that attribute should be included in Prediction_Groups block
          if (datTemp in AttTypesPred) and (cmTemp = cmByGroup) then
            begin
              Keyword := GT_DepAttKeyword(datTemp);
              if Keyword <> 'not_a_keyword' then
                begin
                  // For each group, store value for current keyword.
                  IC := IC + 1;
                  bdPredictionGroups.KeyValMatrix[IC].Name := Keyword;
                  J := 0;
                  for N := 0 to PredGpSet.Count - 1 do
                    begin
                      GName := PredGpSet.Items[N].Name;
                      NMembers := PredSet.NumDepByGroup(GName);
                      if NMembers >0 then
                        begin
                          if not FoundError then
                            begin
                              S := PredGpSet.Items[N].AllAtts.Items[I].Text;
                              if (S = '') then
                                begin
                                  Cap := PredGpSet.Items[N].AllAtts.Items[I].Caption;
                                  Messg := 'Error: ' + Cap + ' for prediction group "'
                                           + GName + '" is blank';
                                  ShowMessage(Messg);
                                  FoundError := True;
                                end;
                              bdPredictionGroups.KeyValMatrix[IC].SetVal(J,S);
                              J := J + 1;
                            end;
                        end;
                    end;
                end;
            end;
        end;
      if not FoundError then
        J_BuildInputBlock('Prediction_Groups', BlkFmt, bdPredictionGroups, PrGBlock)
      else
        begin
          Messg := '# Prediction_Groups input block not generated because of error';
          PrGBlock.Add(Messg);
        end;
      FreeAndNil(bdPredictionGroups);
    end
  else
    // NR = 0.
    begin
      Messg := '# Prediction_Groups input block not generated because it would be empty';
      PrGBlock.Add(Messg);
      Messg := 'Prediction_Groups input block not generated because there are no predictions';
      ShowMessage(Messg);
    end;
end; // procedure TProject.BuildPredictionGpsBlock.

function TProject.BuildPriorInfoBlock(BlkFmt: TBlockFormat;
  PriDBlock: TStringList): boolean;
var
  FoundError: Boolean;
  I, IC, J, NC, NR: Integer;
  Cap, Keyword, Messg, PriName, S: string;
  piatTemp: TPriAttType;
  cmTemp: TControlMethod;
  bdPriorInfo: TBlockData;         // Linear_Prior_Information.
begin
  PriDBlock.Clear;
  // Determine numbers of rows (# prior items) and columns (# attributes)
  // needed for the Linear_Prior_Information input block
  FoundError := False;
  NR := PriSet.Count;
  NC := 1; // Always include prior name.
  for I := 0 to PriorSetup.PriAttributes.Count - 1 do
    begin
      piatTemp := PriorSetup.PriAttributes.Items[I].PriAttType;
      cmTemp := PriorSetup.PriAttributes.Items[I].ControlMethod;
      if (piatTemp in AttTypesPri) and (cmTemp = cmByItem) then
        begin
          Keyword := GT_PriAttKeyword(PriorSetup.PriAttributes.Items[I].PriAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all prior items
  if (NR > 0) then
    begin
      bdPriorInfo := TBlockData.CreateAndAllocate(NR,NC);
      // Populate PriorName column.
      Keyword := GT_PriAttKeyword(piatPriorName);
      bdPriorInfo.KeyValMatrix[0].Name := Keyword;
      for J := 0 to NR - 1 do
        begin
          S := PriSet.Items[J].Name;
          bdPriorInfo.KeyValMatrix[0].SetNameVal(J,Keyword,S);
        end;
      // For each keyword with ControlMethod=cmByItem, store values for
      // all observations.
      IC := 0;
      for I := 0 to PriorSetup.PriAttributes.Count - 1 do
        begin
          piatTemp := PriorSetup.PriAttributes.Items[I].PriAttType;
          cmTemp := PriorSetup.PriAttributes.Items[I].ControlMethod;
          // Check that attribute should be included in Linear_Prior_Informatio block
          if (piatTemp in AttTypesPri) and (cmTemp = cmByItem) then
            begin
              Keyword := GT_PriAttKeyword(PriorSetup.PriAttributes.Items[I].PriAttType);
              if Keyword <> 'not_a_keyword' then
                begin
                  // For each prior item, store value for current keyword.
                  IC := IC + 1;
                  bdPriorInfo.KeyValMatrix[IC].Name := Keyword;
                  for J := 0 to NR - 1 do
                    begin
                      if not FoundError then
                        begin
                          S := PriSet.Items[J].AllAtts.Items[I].Text;
                          if (S = '') then
                            begin
                              Cap := PriSet.Items[J].AllAtts.Items[I].Caption;
                              PriName := PriSet.Items[J].Name;
                              Messg := 'Error: ' + Cap + ' for prior item "' + PriName + '" is blank';
                              ShowMessage(Messg);
                              FoundError := True;
                            end;
                          bdPriorInfo.KeyValMatrix[IC].SetVal(J,S);
                        end;
                    end;
                end;
            end;
        end;

      if not FoundError then
        J_BuildInputBlock('Linear_Prior_Information', bfTable, bdPriorInfo, PriDBlock)
      else
        begin
          Messg := '# Linear_Prior_Information input block not generated because of error';
          PriDBlock.Add(Messg);
        end;
      FreeAndNil(bdPriorInfo);
    end
  else
    // NR = 0.
    begin
      Messg := '# Linear_Prior_Information input block not generated because it would be empty';
      PriDBlock.Add(Messg);
      Messg := 'Linear_Prior_Information input block not generated because there are no prior items';
      ShowMessage(Messg);
      FoundError := True;
    end;
  result := not FoundError;
end; // TProject.BuildPriorInfoDataBlock

function TProject.BuildPriorInfoGpsBlock(BlkFmt: TBlockFormat;
  PriGBlock: TStringList): boolean;
var
  FoundError: Boolean;
  I, IC, J, N, NC, NMembers, NR: Integer;
  Cap, Keyword, Messg, GName, S: string;
  piatTemp: TPriAttType;
  cmTemp: TControlMethod;
  bdPriorInfoGroups: TBlockData;   // Prior_Information_Groups.
begin
  PriGBlock.Clear;
  // Determine numbers of rows (# prior info groups) and columns (# attributes)
  // needed for the Prior_Information_Groups input block
  FoundError := False;
  result := True;
  NC := 1; // always include group name
  for I := 0 to PriorSetup.PriAttributes.Count - 1 do
    begin
      piatTemp := PriorSetup.PriAttributes.Items[I].PriAttType;
      cmTemp := PriorSetup.PriAttributes.Items[I].ControlMethod;
      if (piatTemp in AttTypesPri) and (cmTemp = cmByGroup) then
        begin
          Keyword := GT_PriAttKeyword(PriorSetup.PriAttributes.Items[I].PriAttType);
          if Keyword <> 'not_a_keyword' then
            NC := NC + 1;
        end;
    end;
  // Populate KeyValMatrix with keywords and values for all prior info groups.
  // Include only groups which have members.
  NR := 0;
  for N := 0 to PriGpSet.Count - 1 do
    begin
      GName := PriGpSet.Items[N].Name;
      NMembers := PriSet.NumPriByGroup(GName);
      if NMembers > 0 then
        NR := NR + 1;
    end;
  if (NR > 0) then
    begin
      bdPriorInfoGroups := TBlockData.CreateAndAllocate(NR,NC);
      // Populate GroupName column.
      Keyword := GT_PriAttKeyword(piatGroupName);
      bdPriorInfoGroups.KeyValMatrix[0].Name := Keyword;
      J := 0;
      for N := 0 to PriGpSet.Count - 1 do
        begin
          // Include only groups which have members.
          GName := PriGpSet.Items[N].Name;
          NMembers := PriSet.NumPriByGroup(GName);
          if NMembers > 0 then
            begin
              S := PriGpSet.Items[N].Name;
              bdPriorInfoGroups.KeyValMatrix[0].SetNameVal(J,Keyword,S);
              J := J + 1;
            end;
        end;
      // For each keyword with ControlMethod=cmByGroup, store values for
      // all groups.
      IC := 0;
      for I := 0 to PriorSetup.PriAttributes.Count - 1 do
        begin
          piatTemp := PriorSetup.PriAttributes.Items[I].PriAttType;
          cmTemp := PriorSetup.PriAttributes.Items[I].ControlMethod;
          // Check that attribute should be included in Observation_Groups block.
          if (piatTemp in AttTypesPri) and (cmTemp = cmByGroup) then
            begin
              Keyword := GT_PriAttKeyword(piatTemp);
              if Keyword <> 'not_a_keyword' then
                begin
                  // For each group, store value for current keyword.
                  IC := IC + 1;
                  bdPriorInfoGroups.KeyValMatrix[IC].Name := Keyword;
                  J := 0;
                  for N := 0 to PriGpSet.Count - 1 do
                    begin
                      // Include only groups which have members.
                      GName := PriGpSet.Items[N].Name;
                      NMembers := PriSet.NumPriByGroup(GName);
                      if NMembers > 0 then
                        begin
                          if not FoundError then
                            begin
                              S := PriGpSet.Items[N].AllAtts.Items[I].Text;
                              if (S = '') then
                                begin
                                  Cap := PriGpSet.Items[N].AllAtts.Items[I].Caption;
                                  Messg := 'Error: ' + Cap + ' for prior info group "'
                                           + GName + '" is blank';
                                  ShowMessage(Messg);
                                  FoundError := True;
                                end;
                              bdPriorInfoGroups.KeyValMatrix[IC].SetVal(J,S);
                              J := J + 1;
                            end;
                        end;
                    end;
                end;
            end;
        end;
      if not FoundError then
        J_BuildInputBlock('Prior_Information_Groups', BlkFmt, bdPriorInfoGroups, PriGBlock)
      else
        begin
          Messg := '# Prior_Information_Groups input block not generated because of error';
          PriGBlock.Add(Messg);
          result := False;
        end;
      FreeAndNil(bdPriorInfoGroups);
    end
  else
    // NR = 0.
    begin
      Messg := '# Prior_Information_Groups input block not generated because it would be empty';
      PriGBlock.Add(Messg);
      Messg := 'Prior_Information_Groups input block not generated because there are no prior info items';
      ShowMessage(Messg);
    end;
end; // TProject.BuildPriorInfoGpsBlock

procedure TProject.CorrectRelativePaths(OrigProjDir: string);
var
  OldRelPath: string;
  I: Integer;
begin
  if DirectoryExists(RelDirToAbsDir(OrigProjDir, ModelDirectory)) then
    begin
      OldRelPath := ModelDirectory;
      ModelDirectory := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if DirectoryExists(RelDirToAbsDir(OrigProjDir, ModelDirectoryPred)) then
    begin
      OldRelPath := ModelDirectoryPred;
      ModelDirectoryPred := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if FileExists(ModflowNameFile) then
    begin
      OldRelPath := ModflowNameFile;
      ModflowNameFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if ModflowNameFilePred <> '' then
    begin
      OldRelPath := ModflowNameFilePred;
      ModflowNameFilePred := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if MIFiles.Count > 0 then
    begin
      for I := 0 to MIFiles.Count - 1 do
        begin
          OldRelPath := MIFiles.Items[I].ModelFile;
          MIFiles.Items[I].ModelFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
          OldRelPath := MIFiles.Items[I].AppFile;
          MIFiles.Items[I].AppFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
        end;
    end;
  if MOFiles.Count > 0 then
    begin
      for I := 0 to MOFiles.Count - 1 do
        begin
          OldRelPath := MOFiles.Items[I].ModelFile;
          MOFiles.Items[I].ModelFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
          OldRelPath := MOFiles.Items[I].AppFile;
          MOFiles.Items[I].AppFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
        end;
    end;
  if MIFilesPred.Count > 0 then
    begin
      for I := 0 to MIFilesPred.Count - 1 do
        begin
          OldRelPath := MIFilesPred.Items[I].ModelFile;
          MIFilesPred.Items[I].ModelFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
          OldRelPath := MIFilesPred.Items[I].AppFile;
          MIFilesPred.Items[I].AppFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
        end;
    end;
  if MOFilesPred.Count > 0 then
    begin
      for I := 0 to MOFilesPred.Count - 1 do
        begin
          OldRelPath := MOFilesPred.Items[I].ModelFile;
          MOFilesPred.Items[I].ModelFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
          OldRelPath := MOFilesPred.Items[I].AppFile;
          MOFilesPred.Items[I].AppFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
        end;
    end;
    // Correct paths in UcProject
    UcProject.CorrectRelativePaths(OrigProjDir);
end;

constructor TProject.Create(AOwner: TComponent);
begin
  inherited;
  ModelMateVersion := '';
  ProjName := 'default_project';
  Title := 'Title for unnamed project';
  NameLikeProjectFile := True;
  fModelDirectory := '\';      // Avoid setter
  fModelDirectoryPred := '\';  // Avoid setter
  FileName := '';
  ModelID := midModflow2005;
  ModflowNameFile := '';
  ModflowNameFilePred := '';
  ActiveApp := aaUcode;
  fParameterSetup := TParameterSetup.Create(self);
  fParameterSetup.SetSubComponent(True);
  fParGpSet := TParamset.Create;
  fParGpSet.SetGpDefault;
  fParamSet := TParamSet.Create;
  fObservationSetup := TObservationSetup.Create(self);
  fObsGpSet := TDepSet.Create;
  fObsGpSet.Add;
  fObsGpSet.Items[0].CreateAndName(fObsGpSet,'DefaultObs', 'DefaultObs', dcObs);
  fObsSet := TDepSet.Create;
  fPredictionSetup := TPredictionSetup.Create(self);
  fPredGpSet := TDepSet.Create;
  fPredGpSet.Add;
  fPredGpSet.Items[0].CreateAndName(fPredGpSet,'DefaultPreds', 'DefaultPreds', dcPred);
  fPredSet := TDepSet.Create;
  fPriorSetup := TPriorSetup.Create(self);
  fPriGpSet := TPriSet.Create;
  fPriGpSet.Add;
  fPriGpSet.Items[0].CreateAndName(fPriGpSet,'DefaultPrior', 'DefaultPrior');
  fPriSet := TPriSet.Create;
  fMIFiles := TModelIOPairs.Create;
  fMOFiles := TModelIOPairs.Create;
  fMIFilesPred := TModelIOPairs.Create;
  fMOFilesPred := TModelIOPairs.Create;
  fUcProject := TUcProject.Create(self);
  UcProject.OutputPrefix := ProjName;
  fUseObsGps := True;
  fUsePredGps := True;
  fUseParGps := True;
  fUsePriorInfo := True;
  fLinkTemplateToParamsTable := True;
end; // constructor TProject.Create

procedure TProject.DefineObsGroups;
// Empty field fObsGpSet and repopulate based on contents of field fObsSet
var
  GpNew: string12;
  I, Index, J, K: Integer;
  Stored: boolean;
begin
  ObsGpSet.Clear;
  if ObsSet.Count > 0 then
    begin
      K := DepAttPos(datGroupName);
      GpNew := ObsSet.Items[0].AllAtts[K].Text;
      fObsGpSet.Add;
      Index := fObsGpSet.Count - 1;
      fObsGpSet.Items[Index].CreateAndName(fObsGpSet, GpNew, GpNew, dcObs);
      for I := 1 to ObsSet.Count - 1 do
        begin
          GpNew := ObsSet.Items[I].AllAtts[DepAttPos(datGroupName)].Text;
          // Check to see if GpNew has already been stored in ObsGpSet
          J := 0;
          Stored := False;
          while (J < ObsGpSet.Count) and not Stored do
            begin
              Stored := SameText(GpNew, ObsGpSet.Items[J].AllAtts[K].Text);
              J := J + 1;
            end;
          if not Stored then
            begin
              fObsGpSet.Add;
              Index := fObsGpSet.Count - 1;
              fObsGpSet.Items[Index].CreateAndName(fObsGpSet, GpNew, GpNew,
                                                   dcObs);
            end;
        end;
    end;
end;

procedure TProject.DefineObsSetup;
begin
  ObservationSetup.ObsAttributes.Items[DepAttPos(datStatistic)].ControlMethod := cmByGroup;
  ObservationSetup.ObsAttributes.Items[DepAttPos(datStatFlag)].ControlMethod := cmByGroup;
end;

procedure TProject.DefinePredGroups;
// Empty field fPredGpSet and repopulate based on contents of field fPredSet
var
  GpNew: string12;
  I, Index, J, K: Integer;
  Stored: boolean;
begin
  PredGpSet.Clear;
  if PredSet.Count > 0 then
    begin
      K := DepAttPos(datGroupName);
      GpNew := PredSet.Items[0].AllAtts[K].Text;
      fPredGpSet.Add;
      Index := fPredGpSet.Count - 1;
      fPredGpSet.Items[Index].CreateAndName(fPredGpSet, GpNew, GpNew, dcPred);
      for I := 1 to PredSet.Count - 1 do
        begin
          GpNew := PredSet.Items[I].AllAtts[DepAttPos(datGroupName)].Text;
          // Check to see if GpNew has already been stored in PredGpSet
          J := 0;
          Stored := False;
          while (J < PredGpSet.Count) and not Stored do
            begin
              Stored := SameText(GpNew, PredGpSet.Items[J].AllAtts[K].Text);
              J := J + 1;
            end;
          if not Stored then
            begin
              fPredGpSet.Add;
              Index := fPredGpSet.Count - 1;
              fPredGpSet.Items[Index].CreateAndName(fPredGpSet, GpNew, GpNew,
                                                    dcPred);
            end;
        end;
    end;
end;

procedure TProject.DefinePredSetup;
begin
  PredictionSetup.PredAttributes.Items[DepAttPos(datMeasStatistic)].ControlMethod := cmByGroup;
  PredictionSetup.PredAttributes.Items[DepAttPos(datMeasStatFlag)].ControlMethod := cmByGroup;
end;

destructor TProject.Destroy;
begin
  fParGpSet.Free;
  fParamSet.Free;
  fObsGpSet.Free;
  fObsSet.Free;
  fPredGpSet.Free;
  fPredSet.Free;
  //fPriorSetup.Free; // unneeded because fPriorSetup inherits from TComponent?
  fPriGpSet.Free;
  fPriSet.Free;
  fMIFiles.Free;
  fMOFiles.Free;
  fMIFilesPred.Free;
  fMOFilesPred.Free;
  fUcProject.Free;
  inherited;
end;

procedure TProject.ExportOmitFile(FileName: string);
// Export a UCODE fn.omit file.  FileName should include an extension.
var
  I: integer;
  slExport: TStringList;
begin
  if UcProject.OmitVals.Count > 0 then
    begin
      slExport := TStringList.Create;
      try
        slExport.Clear;
        slExport.Add('BEGIN OMIT_DATA');
        for I := 0 to UcProject.OmitVals.Count - 1 do
          begin
            slExport.Add('  ' + UcProject.OmitVals.Strings[I]);
          end;
        slExport.Add('END OMIT_DATA');
        slExport.SaveToFile(FileName);
      finally
        slExport.Free;
      end;
    end;
end;

function TProject.ExportUcodeFile(ModelUse: TModeluse; ProgressDialog: TJvProgressDialog): boolean;
// Export a UCODE input file.
var
  SL, slExport: TStringList;
  Errors: integer;
  AbsMIF, Line: string;
  NeedAdjPars: boolean;
begin
  Errors := 0;
  slExport := TStringList.Create;
  SL := TStringList.Create;
  try
  slExport.Clear;
  SL.Clear;
  ProgressDialog.Position := 15;
{ TODO 2 -cinterface : Change Build* procedures for remaining
  input blocks to boolean functions...accumulate warnings }
  // Options (optional, but always write).
  UcProject.BuildOptionsBlock(SL);
  ProgressDialog.Position := 17;
  slExport.AddStrings(SL);
  SL.Clear;
  slExport.Add(' ');

  // Merge_Files (optional).

  // UCODE_Control_Data (optional).
  UcProject.BuildUcodeControlDataBlock(SL);
  ProgressDialog.Position := 20;
  slExport.AddStrings(SL);
  SL.Clear;
  slExport.Add(' ');

  // Reg_GN_Controls (optional).
  if UcProject.UcMode = umParEst then
    begin
      UcProject.BuildRegGNControlsBlock(SL);
      slExport.AddStrings(SL);
      SL.Clear;
      slExport.Add(' ');
    end;
  ProgressDialog.Position := 22;
  // Reg_GN_NonLinInt (optional).

  // Model_Command_Lines (required).
  if not BuildModelCommandLinesBlock(SL) then Errors := Error + 1;
  slExport.AddStrings(SL);
  ProgressDialog.Position := 25;
  SL.Clear;
  slExport.Add(' ');

  // Parameter_Groups (optional).
  case UcProject.UcMode of
    umFwd: NeedAdjPars := False;
    umSensAnal: NeedAdjPars := True;
    umParEst: NeedAdjPars := True;
    umTestLin: NeedAdjPars := True;
    umPred: NeedAdjPars := False;
    umAdvTestLin: NeedAdjPars := True;
    umNonlinUncert: NeedAdjPars := True;
    umInvObjFunc: NeedAdjPars := False;
  else
    NeedAdjPars := False;
  end;
  if NeedAdjPars then
    begin
      if NumAdjustable = 0 then
        begin
          Line := 'Error: Adjustable parameter(s) required for selected' +
                  ' UCODE mode, but no parameters are adjustable';
          ShowMessage(Line);
          Errors := Errors + 1;
        end;
    end;
  BuildParameterGpsBlock(bfTable, SL);
  slExport.AddStrings(SL);
  ProgressDialog.Position := 30;
  SL.Clear;
  slExport.Add(' ');

  // Parameter_Data (required).
  if not BuildParameterDataBlock(bfTable, SL) then Errors := Errors + 1;
  slExport.AddStrings(SL);
  ProgressDialog.Position := 32;
  SL.Clear;
  slExport.Add(' ');

  // Parameter_Values (optional).

  // Derived_Parameters (optional).
  if ParamSet.CountDerivedParameters > 0 then
    begin
      if not BuildDerivedParametersBlock(bfTable, SL) then Errors := Errors + 1;
      slExport.AddStrings(SL);
      SL.Clear;
      slExport.Add(' ');
    end;
  ProgressDialog.Position := 35;

  // Include prediction input blocks only if Ucode mode is Prediction.
  if UcProject.UcMode = umPred then
    begin
      // Prediction_Groups (optional).
      if UsePredGps then
        begin
          BuildPredictionGpsBlock(bfTable, SL);
          slExport.AddStrings(SL);
          SL.Clear;
          slExport.Add(' ');
        end;
      // Prediction_Data (optional).
      if not BuildPredictionDataBlock(bfTable, SL) then Errors := Errors + 1;
      slExport.AddStrings(SL);
      ProgressDialog.Position := 40;
      SL.Clear;
      slExport.Add(' ');
      // Derived_Predictions (optional).
    end
  else
    begin
      // Observation_Groups (optional).
      if UseObsGps then
        begin
          BuildObservationGpsBlock(bfTable, SL);
          slExport.AddStrings(SL);
          ProgressDialog.Position := 40;
          SL.Clear;
          slExport.Add(' ');
        end;

      // Observation_Data (required).
      if not BuildObservationDataBlock(bfTable, SL) then Errors := Errors + 1;
      slExport.AddStrings(SL);
      ProgressDialog.Position := 50;
      SL.Clear;
      slExport.Add(' ');

      // Derived_Observations (optional).
    end;
  ProgressDialog.Position := 55;

  // Linear prior information.
  if (UsePriorInfo) and (PriSet.Count > 0) then
    begin
      if UsePriGps then
        begin
          // Prior_Information_Groups (optional).
          if not BuildPriorInfoGpsBlock(bfTable, SL) then Errors := Errors + 1;
          slExport.AddStrings(SL);
          SL.Clear;
          slExport.Add(' ');
        end;
      // Linear_Prior_Information (optional).
      if not BuildPriorInfoBlock(bfTable, SL) then Errors := Errors + 1;
      slExport.AddStrings(SL);
      SL.Clear;
      slExport.Add(' ');
    end;
  ProgressDialog.Position := 57;

  // Model_Input_Files (required).
  if not BuildModelIOBlock(fuInput, SL, ModelUse) then Errors := Errors + 1;
  slExport.AddStrings(SL);
  SL.Clear;
  slExport.Add(' ');
  ProgressDialog.Position := 60;

  // Model_Output_Files (required).
  if not BuildModelIOBlock(fuOutput, SL, ModelUse) then Errors := Errors + 1;
  slExport.AddStrings(SL);
  SL.Clear;
  slExport.Add(' ');
  ProgressDialog.Position := 62;

  if (UcProject.ParallelControl.Parallel) and (UcProject.ModeIsParallelizable) then
    begin
      // Parallel_Control (optional).
      if not UcProject.BuildParallelControlBlock(SL) then Errors := Errors + 1;
      slExport.AddStrings(SL);
      SL.Clear;
      slExport.Add(' ');
      // Parallel_Runners (optional).
      if not UcProject.BuildParallelRunnersBlock(bfTable, SL) then Errors := Errors + 1;
      slExport.AddStrings(SL);
      SL.Clear;
      slExport.Add(' ');
    end;
  ProgressDialog.Position := 65;

  AbsMIF := UcProject.AbsMainInputFileName(ModelUse);
  if AbsMIF <> '' then
    begin
     slExport.SaveToFile(AbsMIF);
     ProgressDialog.Position := 70;
    end
  else
    begin
      Line := 'UCODE file not exported because UCODE input file not defined';
      ProgressDialog.Hide;
      ShowMessage(Line);
    end;
  finally
    slExport.Free;
    SL.Free;
    if Errors = 0 then
      result := True
    else
      result := False;
  end;
  ProgressDialog.Position := 75;
end;

function TProject.GetTemplateFile(const MIOFile: TFileName;
  const ModelUse: TModelUse): string;
var
  I: integer;
  MIOPairs: TModelIOPairs;
begin
  result := '';
  case ModelUse of
    // pointer assignment.
    muCalib: MIOPairs := self.MIFiles;
    muPred: MIOPairs := self.MIFilesPred;
    else MIOPairs := self.MIFiles;
  end;
  for I := 0 to MIOPairs.Count - 1 do
    begin
      if (AnsiSameText(MIOPairs.Items[I].AbsModelFile,MIOFile))
          or (AnsiSameText(MIOPairs.Items[I].ModelFile,MIOFile)) then
        begin
          result := MIOPairs.Items[I].AppFile;
        end;
    end;
end;

function TProject.LocateModflowNameFile(ModelUse: TModelUse): boolean;
var
  AbsPath: string;
begin
  odModflowGlobal.InitialDir := AbsModelDirectory(ModelUse);
  case ModelUse of
    muCalib: odModflowGlobal.FileName := ModflowNameFile;
    muPred: odModflowGlobal.FileName := ModflowNameFilePred;
  end;
  if odModflowGlobal.Execute then
    begin
      case ModelUse of
        muCalib:
          begin
            ModflowNameFile := odModflowGlobal.FileName;
            AbsPath := AbsModflowNameFile(muCalib);
            ModelDirectory := ExtractFileDir(AbsPath);
          end;
        muPred:
          begin
            ModflowNameFilePred := odModflowGlobal.FileName;
            AbsPath := AbsModflowNameFile(muPred);
            ModelDirectoryPred := ExtractFileDir(AbsPath);
          end;
      end;
      result := True;
    end
  else
    begin
      result := False;
    end;
end;

function TProject.MakeUcodeBatchFile(ModelUseLocal: TModelUse;
  var BatchFileLocation: string; var ErrMess: string): boolean;
var
  AbsAppDir, AbsMIF, AbsModelDir, BatchBase, CmdLineOptions: string;
  OutPrefix, OutPrefixPred: string;
  OK: boolean;
begin
  OK := True;
  AbsAppDir := AbsAppDirectory(ModelUseLocal);
  if DirectoryExists(AbsAppDir) then
    begin
      AbsMIF := UcProject.AbsMainInputFileName(ModelUseLocal);
//      case ModelUseLocal of
//        muCalib: OutPrefix := UcProject.OutputPrefix;
//        muPred: OutPrefix := UcProject.OutputPrefixPred;
//      end;
      OutPrefix := UcProject.OutputPrefix;
      OutPrefixPred := UcProject.OutputPrefixPred;
      BatchBase := 'RunUcode_' + ProjName;
      AbsModelDir := AbsModelDirectory(ModelUseLocal);
      if AnsiSameText(AbsAppDir, AbsModelDir) then
        begin
          CmdLineOptions := AbsMIF + ' ' + OutPrefix + ' ' + OutPrefixPred;
          BatchFileLocation := WriteBatchFile(GlobalProgramLocations.Ucode2005Location, CmdLineOptions, BatchBase);
        end
      else
        begin
          BatchFileLocation := WriteCustomBatchFile(GlobalProgramLocations.Ucode2005Location, AbsMIF, OutPrefix, OutPrefixPred, BatchBase, AbsModelDir);
        end;
    end
  else
    begin
      OK := False;
      ErrMess := 'UCODE batch file was not created because directory "' +
                        AbsAppDir + '" does not exist.';
    end;
  result := OK;
end;

procedure TProject.NormalizePaths;
var
  PathTemp: string;
begin
  // Convert any absolute paths to relative paths.
  // Setters take care of conversion, which is why
  // the apparently pointless assignments are needed.
  PathTemp := self.ModelDirectory;
  if length(PathTemp) > 1 then
    begin
      if PathTemp[2] = ':' then
        self.ModelDirectory := PathTemp;
    end;
  //
  PathTemp := self.ModelDirectoryPred;
  if length(PathTemp) > 1 then
    begin
      if PathTemp[2] = ':' then
        self.ModelDirectoryPred := PathTemp;
    end;
  //
  PathTemp := self.ModflowNameFile;
  if length(PathTemp) > 1 then
    begin
      if PathTemp[2] = ':' then
        self.ModflowNameFile := PathTemp;
    end;
end;

function TProject.NumAdjustable: integer;
var
  I, IATadj, IATderived, IATgp, J, K: integer;
  DerivedTemp: string;
  GName: string;
begin
  IATadj := ParAttPos(patAdjustable);
  IATderived := ParAttPos(patDerived);
  IATgp := ParAttPos(patGroupName);
  K := 0;
  for I := 0 to ParamSet.Count - 1 do
    begin
      DerivedTemp := ParamSet.Items[I].AllAtts.Items[IATderived].Text;
      if (DerivedTemp = 'No') or (DerivedTemp = '') then
        begin
          // The default for the Adjustable attribute is No.
          if self.ParameterSetup.ParAttributes.Items[IATadj].ControlMethod = cmByGroup then
            begin
              GName := ParamSet.Items[I].AllAtts.Items[IATgp].Text;
              // Count parameter as adjustable if group is
              J := 0;
              while J < ParGpSet.Count do
                begin
                  if ParGpSet.Items[J].Name = GName then
                    begin
                      if ParGpSet.Items[J].AllAtts.Items[IATadj].Text = 'Yes' then
                        begin
                          K := K + 1;
                        end;
                      J := ParGpSet.Count;
                    end;
                  J := J + 1;
                end;
            end
          else
            begin
              if ParamSet.Items[I].AllAtts.Items[IATadj].Text = 'Yes' then
                K := K + 1;
            end;
        end;
    end;
  result := K;
end;

procedure TProject.SetModelDirectory(const Value: string);
begin
  fModelDirectory := RelativePath(Value);
end;

procedure TProject.SetModelDirectoryPred(const Value: string);
begin
  fModelDirectoryPred := RelativePath(Value);
end;

procedure TProject.SetModflowNameFile(const Value: TFileName);
begin
  fModflowNameFile := RelativePath(Value);
end;

procedure TProject.SetModflowNameFilePred(const Value: TFileName);
begin
  fModflowNameFilePred := RelativePath(Value);
end;

procedure TProject.SetUcProject(const Value: TUcProject);
begin
  fUcProject.Assign(Value);
  ActiveApp := aaUcode;
end;

procedure TProject.SetupObsGroupUsage;
var
  I: Integer;
begin
  UseObsGps := False;
  ObservationSetup.SetControlMethod(datGroupName, cmByDefault);
  for I := 0 to ObservationSetup.NumAtt - 1 do
  begin
    if ObservationSetup.ObsAttributes[I].ControlMethod = cmByGroup then
    begin
      UseObsGps := True;
      ObservationSetup.SetControlMethod(datGroupName, cmByItem);
      Break;
    end;
  end;
end;

procedure TProject.SetupParGroupUsage;
var
  I: Integer;
begin
  UseParGps := False;
  ParameterSetup.SetControlMethod(patGroupName, cmByDefault);
  for I := 0 to ParameterSetup.NumAtt - 1 do
  begin
    if ParameterSetup.ParAttributes[I].ControlMethod = cmByGroup then
    begin
      UseParGps := True;
      ParameterSetup.SetControlMethod(patGroupName, cmByItem);
      Break;
    end;
  end;
end;

procedure TProject.SetupPredGroupUsage;
var
  I: Integer;
begin
  UsePredGps := False;
  PredictionSetup.SetControlMethod(datGroupName, cmByDefault);
  for I := 0 to PredictionSetup.NumAtt - 1 do
  begin
    if PredictionSetup.PredAttributes[I].ControlMethod = cmByGroup then
    begin
      UsePredGps := True;
      PredictionSetup.SetControlMethod(datGroupName, cmByItem);
      Break;
    end;
  end;
end;

procedure TProject.SetupPriGroupUsage;
var
  I: Integer;
begin
  UsePriGps := False;
  PriorSetup.SetControlMethod(piatGroupName, cmByDefault);
  for I := 0 to PriorSetup.NumAtt - 1 do
  begin
    if PriorSetup.PriAttributes[I].ControlMethod = cmByGroup then
    begin
      UsePriGps := True;
      PriorSetup.SetControlMethod(piatGroupName, cmByItem);
      Break;
    end;
  end;
end;

procedure TProject.WriteToStream(SStream: TStringStream);
begin
  // Write TProject data to String Stream SStream.
end;

//###################################################################

{ TParameterAttribute }

procedure TParameterAttribute.Assign(Source: TPersistent);
var
  Item: TParameterAttribute;
begin
  if Source is TParameterAttribute then
    begin
      Item := Source as TParameterAttribute;
      ParamAttType := Item.ParamAttType;
      Text := Item.Text;
    end
  else
    inherited Assign(Source);
end;

function TParameterAttribute.Caption: string;
begin
  case ParamAttType of
    patAdjustable: result := 'Adjustable';
    patConstrain: result := 'Constrain';
    patConstraints: result := 'Constraints';
    patGroupName: result := 'Group Name';
    patDerived: result := 'Derived';
    patLowerBound: result := 'Lower Constraint';
    patUpperBound: result := 'Upper Constraint';
    patLowerValue: result := 'Lower Reasonable Value';
    patUpperValue: result := 'Upper Reasonable Value';
    patMaxChange: result := 'Maximum Change';
    patParamName: result := 'Parameter Name';
    patNonLinearInterval: result := 'NonLinearInterval';
    patPerturbAmt: result := 'Perturb Amount';
    patReasRange: result := 'Reasonable Range';
    patScalePval: result := 'ScalePval';
    patSenMethod: result := 'Sensitivity Method';
    patSosIncrement: result := 'SosIncrement';
    patTolPar: result := 'TolPar';
    patTransform: result := 'Transform';
    patStartValue: result := 'Start Value';
  end;
end;

function TParameterAttribute.DefaultText: string;
begin
  case ParamAttType of
    patAdjustable: result := 'No';
    patConstrain: result := 'No';
    patConstraints: result := 'No constraints';
    patLowerValue: result := '-10.0E38';
    patUpperValue: result := '10.0E38';
    patLowerBound: result := '-10.0E38';
    patUpperBound: result := '10.0E38';
    patGroupName: result := 'ParamDefault';
    patDerived: result := 'No';
    patMaxChange: result := '2.0';
    patNonLinearInterval: result := 'No';
    patPerturbAmt: result := '0.01';
    patReasRange: result := '-10**38 to 10**38';
    patScalePval: result := 'Starting parameter value / 100';
    //patSenMethod: result := '1: Forward differences';
    patSenMethod: result := '1';
    patSosIncrement: result := '5';
    patTolPar: result := '0.01';
    patTransform: result := 'No';
  else
    result := '';
  end;
end;

destructor TParameterAttribute.Destroy;
begin
  inherited;
end;

function TParameterAttribute.Hint: string;
begin
 case ParamAttType of
    patAdjustable: result := 'Is parameter adjustable?';
    patConstrain: result := 'Constrain parameter between limits?';
    patConstraints: result := 'Constraints on considered parameter values:' +
         ' Constrain, LowerConstraint, and UpperConstraint';
    patLowerBound: result := 'Lower constraint on parameter';
    patUpperBound: result := 'Upper constraint on parameter';
    patGroupName: result := 'Group to which parameter belongs';
    patDerived: result := 'Is parameter derived from other parameter(s)?';
    patMaxChange: result := 'Maximum fractional parameter change allowed' +
         ' between parameter-estimation iterations';
    patParamName: result := 'Parameter name';
    patNonLinearInterval: result := 'Calculate nonlinear confidence intervals?';
    patPerturbAmt: result :=
         'Perturbation amount, as fraction of parameter value';
    patReasRange: result := 'Limits of reasonable parameter values:' +
         ' LowerValue and UpperValue';
    patLowerValue: result := 'Lowermost reasonable value';
    patUpperValue: result := 'Uppermost reasonable value';
    patScalePval: result := 'Threshold parameter value below which scaled' +
         ' sensitivities no longer depend on parameter';
    patSenMethod: result :=
         'Options: -1: Calculated by process model (log-transformed as needed);'
         + ' 0: Calculated by process model (native only);' +
         ' 1: Forward-difference perturbation;' +
         ' 2: Central-difference perturbation';
    patSosIncrement: result := 'Number of values considered for Investigate' +
         ' Objective Function mode';
    patTolPar: result := 'Regression convergence criterion, as a fraction' +
         ' of parameter value';
    patTransform: result := 'Log-transform the parameter for regression?';
    patStartValue: result := 'Starting parameter value';
 else
   result := '';
 end;
end;

procedure TParameterAttribute.Initialize(PAType: TParamAttType);
begin
  ParamAttType := pAType;
  Text := DefaultText;
end;

function TParameterAttribute.ItemType: TItemType;
begin
  case ParamAttType of
    patAdjustable: result := itBool;
    patConstrain: result := itBool;
    patConstraints: result := itBool;
    patGroupName: result := itString;
    patDerived: result := itBool;
    patLowerBound: result := itDouble;
    patUpperBound: result := itDouble;
    patLowerValue: result := itDouble;
    patUpperValue: result := itDouble;
    patMaxChange: result := itDouble;
    patParamName: result := itString;
    patNonLinearInterval: result := itBool;
    patPerturbAmt: result := itDouble;
    patReasRange: result := itBool;
    patScalePval: result := itDouble;
    patSenMethod: result := itInteger;
    patSosIncrement: result := itInteger;
    patTolPar: result := itDouble;
    patTransform: result := itBool;
    patStartValue: result := itDouble;
  else
    result := itString;
  end;
end;

//###################################################################

{ TParameterSetupAttribute }

procedure TParameterSetupAttribute.Assign(Source: TPersistent);
var
  item: TParameterSetupAttribute;
begin
  if Source is TParameterSetupAttribute then
    begin
      item := TParameterSetupAttribute(Source);
      ControlMethod := item.ControlMethod;
    end;
  inherited Assign(Source);
end;

destructor TParameterSetupAttribute.Destroy;
begin
  inherited;
end;

//###################################################################

{ TParameterSetup }

procedure TParameterSetup.Assign(Source: TPersistent);
var
  Src: TParameterSetup;
begin
  if Source is TParameterSetup then
    begin
      Src := Source as TParameterSetup;
      if Src.NumAtt = NumParAttributes then
        begin
          NumAtt := Src.NumAtt;
          ParAttributes.Assign(Src.ParAttributes);
        end
      else
        // Handle case of Src.NumAtt < NumParAttributes w/ call to TParameterSetup.Populate.
        if Src.NumAtt < NumParAttributes then
          begin
            self.Populate(Source);
          end;
    end
  else
    inherited;
end;

constructor TParameterSetup.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  // Assign defaults for parameter attribute type and control method
  self.SetSubComponent(True);
  NumAtt := NumParAttributes;
  fAllParAttributes := TParameterSetupAttributes.Create;
  for I := 0 to NumAtt - 1 do
    begin
      ParAttributes.Add;
      ParAttributes.Items[I].ParamAttType := AttributeTypes[I];
      ParAttributes.Items[I].ControlMethod := DefaultCtrlMethod[I];
    end;
end;

destructor TParameterSetup.Destroy;
begin
  fAllParAttributes.Free;
  inherited;
end;

procedure TParameterSetup.Populate(Source: TPersistent);
var
  I, J: integer;
  Src: TParameterSetup;
  patTemp1, patTemp2: TParamAttType;
begin
  if Source is TParameterSetup then
    begin
      Src := Source as TParameterSetup;
      ParAttributes.Clear;
      self.NumAtt := NumParAttributes;
      for I := 0 to NumParAttributes - 1 do
        begin
          ParAttributes.Add;
          patTemp1 := AttributeTypes[I];
          // Assign default for this attribute type.
          ParAttributes.Items[I].ParamAttType := patTemp1;
          ParAttributes.Items[I].ControlMethod := DefaultCtrlMethod[I];
          // Find correct item in Src and use it to populate element in self.
          // If not found, leave ControlMethod as default.
          J := 0;
          while J < Src.ParAttributes.Count do
            begin
              patTemp2 := Src.ParAttributes.Items[J].ParamAttType;
              if patTemp2 = patTemp1 then
                begin
                  ParAttributes.Items[I].ControlMethod := Src.ParAttributes.Items[J].ControlMethod;
                  J := Src.ParAttributes.Count;
                end;
              J := J + 1;
            end;
        end;
    end
  else
    inherited;
end;

procedure TParameterSetup.SetCombinedMethods;
{ Assign ControlMethod for attributes that, for assignment to data grids,
  are combined and controlled by another attribute.  E.g. patLowerValue and
  patUpperValue are controlled by patReasRange. }
var
  I: integer;
  CMReas, CMCons: TControlMethod;
begin
  CMReas := cmByDefault;
  CMCons := cmByDefault;
  for I := 0 to NumAtt - 1 do
  begin
    case ParAttributes[I].ParamAttType of
      patReasRange: CMReas := ParAttributes[I].ControlMethod;
      patConstraints: CMCons := ParAttributes[I].ControlMethod;
    end;
  end;
  for I := 0 to NumAtt - 1 do
  begin
    case ParAttributes[I].ParamAttType of
      patLowerValue: ParAttributes[I].ControlMethod := CMReas;
      patUpperValue: ParAttributes[I].ControlMethod := CMReas;
      patConstrain: ParAttributes[I].ControlMethod := CMCons;
      patLowerBound: ParAttributes[I].ControlMethod := CMCons;
      patUpperBound: ParAttributes[I].ControlMethod := CMCons;
    end;
  end;
end;

procedure TParameterSetup.SetControlMethod(paType: TParamAttType;
  ControlMethod: TControlMethod);
var
  IAT: integer;
begin
  IAT := ParAttPos(paType);
  self.ParAttributes.Items[IAT].ControlMethod := ControlMethod;
end;

//###################################################################

{ TParam }

procedure TParam.Assign(Source: TPersistent);
var
  Item: TParam;
begin
  if Source is TParam then
    begin
      Item := Source as TParam;
      Name := Item.Name;
      if Item.AllAtts.Count = NumParAttributes then
        begin
          AllAtts.Assign(Item.AllAtts); // a TParameterAttributes (a TCollection)
          DerivEqn := Item.DerivEqn;
        end
      else
        if Item.AllAtts.Count < NumParAttributes then
          Populate(Source);
      if self.Name = '' then
        begin
          if Item.GetAttributeByType(patParamName) <> '' then
            begin
              self.SetAttributeByType(patParamName,self.Name);
            end;
        end
      else
        begin
          if self.GetAttributeByType(patParamName) = '' then
            begin
              self.SetAttributeByType(patParamName,self.Name);
            end;
        end;
    end
  else
    inherited;
end;

constructor TParam.Create(aParent: TCollection);
begin
  inherited;
  fAllAtts := TParameterAttributes.Create;
  fDerivEqn := '';
end;

constructor TParam.CreateAndName(aParent: TCollection; aName: string12;
                                 aGroupName: string12);
begin
  inherited Create(aParent);
  Initialize(aName,aGroupName);
end;

constructor TParam.CreateNameValue(aParent: TCollection; aName,
  aGroupName: string12; aValue: string);
begin
  inherited Create(aParent);
  Initialize(aName,aGroupName);
  AllAtts[ParAttPos(patStartValue)].Text := aValue;
end;

destructor TParam.Destroy;
begin
  fAllAtts.Free;
  inherited;
end;

function TParam.GetAttributeByType(paType: TParamAttType): string;
var
  I: integer;
begin
  result := '';
  for I := 0 to AllAtts.Count - 1 do
    begin
      if AllAtts.Items[I].ParamAttType = paType then
        begin
          result := AllAtts.Items[I].Text;
        end;
    end;
end;

procedure TParam.Initialize(aName, aGroupName: string12);
var
  I: Integer;
begin
  if not Assigned(fAllAtts) then fAllAtts := TParameterAttributes.Create;
  for I := 0 to NumParAttributes - 1 do
    begin
      if fAllAtts.Count < I+1 then fAllAtts.Add;
      AllAtts[I].Initialize(AttributeTypes[I]);
    end;
  Name := aName;
  AllAtts[ParAttPos(patParamName)].Text := aName;
  if aGroupName <> '' then AllAtts[ParAttPos(patGroupName)].Text := aGroupName;
  fDerivEqn := '';
end;

procedure TParam.Populate(Source: TPersistent);
var
  I, J: integer;
  Src: TParam;
  patTemp1, patTemp2: TParamAttType;
begin
  if Source is TParam then
    begin
      Src := Source as TParam;
      AllAtts.Clear;
      for I := 0 to NumParAttributes - 1 do
        begin
          AllAtts.Add;
          patTemp1 := AttributeTypes[I];
          // Assign defaults for this attribute type.
          AllAtts.Items[I].ParamAttType := patTemp1;
          AllAtts.Items[I].Text := AllAtts.Items[I].DefaultText;
          // Find correct item in Src and use it to populate element in self.
          // If not found, leave Text as default.
          J := 0;
          while J < Src.AllAtts.Count do
            begin
              patTemp2 := Src.AllAtts.Items[J].ParamAttType;
              if patTemp2 = patTemp1 then
                begin
                  AllAtts.Items[I].Text := Src.AllAtts.Items[J].Text;
                  J := Src.AllAtts.Count;
                end;
              J := J + 1;
            end;
        end;
    end
  else
    inherited;
end;

procedure TParam.SetAttributeByType(paType: TParamAttType; Text: string);
var
  I: integer;
begin
  for I := 0 to AllAtts.Count - 1 do
    begin
      if AllAtts.Items[I].ParamAttType = paType then
        begin
          AllAtts.Items[I].Text := Text;
          if paType = patParamName then
            begin
              self.Name := Text;
            end;
          if (paType = patGroupName) and (Name = '') then
            begin
              Name := Text;
            end;
        end;
    end;
end;

//###################################################################

{ TParamSet }

function TParamSet.Add: TParam;
begin
  Result := TParam(inherited Add); // Result is a TParam.
end;

procedure TParamSet.Assign(Source: TPersistent);
var
  Item: TParamSet;
  I: Integer;
begin
  if Source is TParamSet then
    begin
      Item := Source as TParamSet;
      if self.Count > 0 then Clear;
      for I := 0 to Item.Count - 1 do
        begin
          Add;
          Items[I].Assign(Item.Items[I]); // Copy TParam objects.
        end;
    end
  else
    inherited;
end;


procedure TParamSet.ChangeGroupNames(OldGroup, NewGroup: string);
var
  I, AttPos: integer;
begin
  AttPos := ParAttPos(patGroupName);
  if self.Count > 0 then
    begin
      for I := 0 to Count - 1 do
        begin
          if Items[I].AllAtts.Items[AttPos].Text = OldGroup then
            Items[I].AllAtts.Items[AttPos].Text := NewGroup;
        end;
    end;
end;

function TParamSet.CountDerivedParameters: integer;
var
  I, IAT, NPar, NR: integer;
begin
//
  IAT := ParAttPos(patDerived);
  NPar := Count;
  NR := 0;
  for I := 0 to NPar - 1 do
    begin
      if StrToBoolean(Items[I].AllAtts.Items[IAT].Text) then
        NR := NR + 1;
    end;
  result := NR;
end;

constructor TParamSet.Create;
begin
  inherited Create(TParam);
end;

destructor TParamSet.Destroy;
begin
  Clear;
  inherited;
end;

function TParamSet.GetItem(Index: integer): TParam;
begin
   result := TParam(inherited GetItem(Index));
end;

function TParamSet.NumParsByGroup(GpName: string): integer;
// Return number of parameters for which Group is GpName.
var
  APos, J, K: integer;
begin
  APos := ParAttPos(patGroupName);
  K := 0;
  for J := 0 to self.Count - 1 do
    begin
      if AnsiSameText(self.Items[J].AllAtts.Items[APos].Text, GpName) then
        begin
          K := K + 1;
        end;
    end;
  NumParsByGroup := K;
end;

procedure TParamSet.SetGpDefault;
var
  AttPos: integer;
begin
  if Count = 0 then
    begin
      Add;
      Items[0].Initialize('ParamDefault','ParamDefault');
      AttPos := ParAttPos(patAdjustable);
      Items[0].AllAtts.Items[AttPos].Text := 'Yes';
    end;
end;

procedure TParamSet.SetItem(Index: integer; const Value: TParam);
begin
  inherited SetItem(Index, Value);
end;

//###################################################################

{ TParameterSetupAttributes }

function TParameterSetupAttributes.Add: TParameterSetupAttribute;
begin
  Result := TParameterSetupAttribute(inherited Add);
end;

constructor TParameterSetupAttributes.Create;
begin
  inherited Create(TParameterSetupAttribute);
end;

destructor TParameterSetupAttributes.Destroy;
begin
  Clear;
  inherited;
end;

function TParameterSetupAttributes.GetItem(
  Index: Integer): TParameterSetupAttribute;
begin
  Result := TParameterSetupAttribute(inherited GetItem(Index));
end;

procedure TParameterSetupAttributes.SetItem(Index: Integer;
  aPSetupAttribute: TParameterSetupAttribute);
begin
  inherited SetItem(Index, aPSetupAttribute);
end;

//###################################################################

{ TParameterAttributes }

function TParameterAttributes.Add: TParameterAttribute;
begin
  Result := inherited Add as TParameterAttribute;
end;

procedure TParameterAttributes.Assign(Source: TPersistent);
var
  Item: TParameterAttributes;
  I: Integer;
begin
  if Source is TParameterAttributes then
    begin
      Item := Source as TParameterAttributes;
      if Count < Item.Count then
        begin
          for I := Count to Item.Count - 1 do
            Add;
        end;
      for I := 0 to Count - 1 do
        begin
          if I < Item.Count then 
            begin
              Items[I].ParamAttType := Item.Items[I].ParamAttType;
              Items[I].Text := Item.Items[I].Text;
            end;
        end;
    end
  else
    inherited Assign(Source);
end;

constructor TParameterAttributes.Create;
var
  I: Integer;
begin
  inherited Create(TParameterAttribute);
  for I := 0 to NumParAttributes - 1 do
    begin
      Add;
      self.Items[I].ParamAttType := AttributeTypes[I];
    end;
end;

destructor TParameterAttributes.Destroy;
begin
  Clear;
  inherited;
end;

function TParameterAttributes.GetItem(Index: Integer): TParameterAttribute;
begin
  Result := TParameterAttribute(inherited GetItem(Index));
end;

procedure TParameterAttributes.SetItem(Index: Integer;
  aPAttribute: TParameterAttribute);
begin
  Items[Index].Assign(aPAttribute);
end;

initialization
  InitializeModelMateData;

finalization
  FreeModelMateData;

end.


