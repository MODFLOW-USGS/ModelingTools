unit UcodeUnit;

interface

uses Classes, Dialogs, SysUtils, Windows,
     GlobalTypesUnit, GlobalBasicData, JupiterUnit, Utilities,
    ParallelControlUnit, ParallelRunnerUnit;

  type

    { Enumerations }
    TUcodeMode = (umFwd, umSensAnal, umParEst, umTestLin, umPred, umAdvTestLin,
                  umNonlinUncert, umInvObjFunc);

    TUcModelInfo = class(TModelInfo)
      // If fields are added, modify function NumFields.
      fKashyapPri: double;
      fLnDeterFisherInfoMxPri: double;
      fSensByFwdDiff: boolean;
    public
      function NumFields: integer; override;
    end;

    TUcProject = class(TComponent)
      { One record defines scalar values for one complete UCODE project
        and methods for opening and saving project files.  }
      private
        // Fields.
        fUcMode: TUcodeMode;
        fMainInputFileName: string;
        fMainInputFileNamePred: string;
        fOutputPrefix: string;
        fOutputPrefixPred: string;
        // Options.
        fVerbose: LongInt;
        fDerivativesInterface: TFileName;
        fPathToMergedFile: TFileName;
        fUseModelDerivatives: boolean;
        // Merge_Files.
        // TODO 5 : fPathToFile and fSkipLines need to be arrays
        // (but only needed for supporting MODFLOW-2000, not MODFLOW-2005;
        // until MODFLOW-2005 includes SEN Process).
        fPathToFile: string255;
        fSkipLines: LongInt;
        // UCODE_Control_Data.
        fModelName: string12;
        fModelLengthUnits: string12;
        fModelTimeUnits: string12;
        fModelMassUnits: string12;
        fSensitivities: boolean;
        fPredictionSensitivities: boolean;
        fOptimize: boolean;
        fLinearity: boolean;
        fPrediction: boolean;
        fAdvLin: boolean;
        fLinearityAdv: string12;
        fAdvLinOpt: TAdvLinOpt;
        fNonLinearIntervals: boolean;
        fSosMethod: TSosMethod;
        fSosFile: string255;
        fSosSurface: string12;
        fStdErrOne: boolean;
        fWriteDerivedParams: boolean;
        fWritePriorInfo: boolean;
        fEigenValues: boolean;
        fStartRes: string12;
        fIntermedRes: string12;
        fFinalRes: string12;
        fStartSens: string;
        fIntermedSens: string;
        fFinalSens: string;
        fDataExchange: boolean;
        fCreateInitFiles: boolean;
        // Reg_GN_Controls
        fTolpar: double;
        fTolSOSC: double;
        fMaxIter: LongInt;
        fMaxChange: double;
        fMaxChangeRealm: string12;
        fMqrtDirection: double;
        fMqrtFactor: double;
        fMqrtIncrement: double;
        fQuasiNewton: boolean;
        fQNiter: LongInt;
        fQNsosr: double;
        fOmitDefault: LongInt;
        fOmitVals: TStringList;
        fStatsOnNonconverge: boolean;
        fOmitInsensitive: boolean;
        fMinimumSensRatio: double;
        fReincludeSensRatio: double;
        fTolParWtOS: double;
        fTrustRegion: string12;
        fMaxStep: double;
        fUseMaxStep: boolean;
        fConsecMax: LongInt;
        fParallelControl: TParallelControl;
        fParallelRunners: TParallelRunners;
        fRunnerFiles: TRunnerFiles;
        procedure SetMainInputFileName(const Value: string);
        procedure SetMainInputFileNamePred(const Value: string);
      published
        property UcMode: TUcodeMode read fUcMode write fUcMode;
        property MainInputFileName: string read fMainInputFileName
                        write SetMainInputFileName;
        property MainInputFileNamePred: string read fMainInputFileNamePred
                        write SetMainInputFileNamePred;
        property OutputPrefix: string read fOutputPrefix write fOutputPrefix;
        property OutputPrefixPred: string read fOutputPrefixPred write fOutputPrefixPred;
        // Options
        property Verbose: LongInt read fVerbose write fVerbose;
        property DerivativesInterface: TFileName read fDerivativesInterface
                 write fDerivativesInterface;
        property PathToMergedFile: TFileName read fPathToMergedFile
                 write fPathToMergedFile;
        property UseModelDerivatives: boolean read fUseModelDerivatives
                 write fUseModelDerivatives;
        // Merge_Files
        property PathToFile: string255 read fPathToFile write fPathToFile;
        property SkipLines: LongInt read fSkipLines write fSkipLines;
        // UCODE_Control_Data
        property ModelName: string12 read fModelName write fModelName;
        property ModelLengthUnits: string12 read fModelLengthUnits
                 write fModelLengthUnits;
        property ModelTimeUnits: string12 read fModelTimeUnits
                 write fModelTimeUnits;
        property ModelMassUnits: string12 read fModelMassUnits
                 write fModelMassUnits;
        property Sensitivities: boolean read fSensitivities write fSensitivities;
        property PredictionSensitivities: boolean read fPredictionSensitivities write fPredictionSensitivities;
        property Optimize: boolean read fOptimize write fOptimize;
        property Linearity: boolean read fLinearity write fLinearity;
        property Prediction: boolean read fPrediction write fPrediction;
        property LinearityAdv: string12 read fLinearityAdv write fLinearityAdv;
        property NonLinearIntervals: boolean read fNonLinearIntervals write fNonLinearIntervals;
        property AdvLinOpt: TAdvLinOpt read fAdvLinOpt write fAdvLinOpt;
        property SosSurface: string12 read fSosSurface write fSosSurface;
        property SosMethod: TSosMethod read fSosMethod write fSosMethod;
        property SosFile: string255 read fSosFile write fSosFile;
        property StdErrOne: boolean read fStdErrOne write fStdErrOne;
        property WriteDerivedParams: boolean read fWriteDerivedParams write fWriteDerivedParams;
        property WritePriorInfo: boolean read fWritePriorInfo write fWritePriorInfo;
        property EigenValues: boolean read fEigenValues write fEigenValues;
        property StartRes: string12 read fStartRes write fStartRes;
        property IntermedRes: string12 read fIntermedRes write fIntermedRes;
        property FinalRes: string12 read fFinalRes write fFinalRes;
        property StartSens: string read fStartSens write fStartSens;
        property IntermedSens: string read fIntermedSens write fIntermedSens;
        property FinalSens: string read fFinalSens write fFinalSens;
        property DataExchange: boolean read fDataExchange write fDataExchange;
        property CreateInitFiles: boolean read fCreateInitFiles
                 write fCreateInitFiles;
        // Reg_GN_Controls
        property Tolpar: double read fTolpar write fTolpar;
        property TolSOSC: double read fTolSOSC write fTolSOSC;
        property MaxIter: LongInt read fMaxIter write fMaxIter;
        property MaxChange: double read fMaxChange write fMaxChange;
        property MaxChangeRealm: string12 read fMaxChangeRealm
                 write fMaxChangeRealm;
        property MqrtDirection: double read fMqrtDirection write fMqrtDirection;
        property MqrtFactor: double read fMqrtFactor write fMqrtFactor;
        property MqrtIncrement: double read fMqrtIncrement write fMqrtIncrement;
        property QuasiNewton: boolean read fQuasiNewton write fQuasiNewton;
        property QNiter: LongInt read fQNiter write fQNiter;
        property QNsosr: double read fQNsosr write fQNsosr;
        property OmitDefault: LongInt read fOmitDefault write fOmitDefault;
        property OmitVals: TStringList read fOmitVals write fOmitVals;
        property StatsOnNonconverge: boolean read fStatsOnNonconverge
                 write fStatsOnNonconverge;
        property OmitInsensitive: boolean read fOmitInsensitive
                 write fOmitInsensitive;
        property MinimumSensRatio: double read fMinimumSensRatio
                 write fMinimumSensRatio;
        property ReincludeSensRatio: double read fReincludeSensRatio
                 write fReincludeSensRatio;
        property TolParWtOS: double read fTolParWtOS write fTolParWtOS;
        property TrustRegion: string12 read fTrustRegion write fTrustRegion;
        property MaxStep: double read fMaxStep write fMaxStep;
        property UseMaxStep: boolean read fUseMaxStep write fUseMaxStep;
        property ConsecMax: LongInt read fConsecMax write fConsecMax;
        property ParallelControl: TParallelControl read fParallelControl
                                                   write fParallelControl;
        property ParallelRunners: TParallelRunners read fParallelRunners
                                                   write fParallelRunners;
        property RunnerFiles: TRunnerFiles read fRunnerFiles
                                           write fRunnerFiles;
        // more to be added (including arrays of parameters and dependents?)
      public
        //  Methods
        constructor Create(aOwner: TComponent); override;
        destructor Destroy; override;
        function AbsMainInputFileName(ModelUse: TModelUse): string;
        procedure Assign(Source: TPersistent); override;
        procedure AssignMode(Mode: TUcodeMode);
        procedure BuildOptionsBlock(OptBlock: TStringList);
        function BuildParallelControlBlock(PCBlock: TStringList): boolean;
        function BuildParallelRunnersBlock(BlkFmt: TBlockFormat;
                                           PRBlock: TStringList): boolean;
        procedure BuildUcodeControlDataBlock(UCDBlock: TStringList);
        procedure BuildRegGNControlsBlock(RGNCBlock: TStringList);
        procedure CorrectRelativePaths(OrigProjDir: string);
        procedure ExportOmitFile(const Directory: string);
        function FindMainInputFileName: string;
        function GetModelInfo(ModelInfo: TUcModelInfo): boolean;
        function ModeIsParallelizable: boolean;
        function OutputMainFile: TFileName;
        function PopulateRunnerDirectories(var ErrMsg: string;
                     var NFiles: integer; var NDir: integer): boolean;
        function StartLocalRunners: integer;
        procedure UpdateMode();
    end; // end of TUcProject

  var
    SensOptions: array of string;
    { Parameter attribute types defined for UCODE }
    UcodePATypes: set of TParamAttType;
    { Dependent attribute types defined for UCODE }
    sdUcode: TSaveDialog;
    spSenMethod: array of TStringPair;
    slSenMethod: TStringList;
    TempUcProject: TUcProject;
    UcModelInfo: TUcModelInfo;
    ParallelUcodeModes: set of TUcodeMode;
    // TODO 2 : Add procedures to read _dm, _pa, _ss.
//    procedure Read_dm;
//    function Read_pa(): boolean;
//    procedure Read_ss;
    procedure U_Free;
    function U_GetSensOptPos(SensOpt: string): integer;
    procedure U_Initialize(aOwner: TComponent);

implementation

//###################################################################

{ TUCProject }

function TUcProject.AbsMainInputFileName(ModelUse: TModelUse): string;
// Generate absolute path for MainInputFileName or MainInputFileNamePred,
// depending on model use.
begin
  result := '';
//TODO 2 : If MainInputFileName or MainInputFileNamePred is blank, this returns wrong result
  case ModelUse of
    muCalib:
     begin
       if MainInputFileName <> '' then
         result := PathToAbsPath(ProjectDirectory, MainInputFileName);
     end;
    muPred:
     begin
       if MainInputFileNamePred <> '' then
         result := PathToAbsPath(ProjectDirectory, MainInputFileNamePred);
     end;
  end;
end;

procedure TUCProject.Assign(Source: TPersistent);
var
  PrjSource: TUCProject;
begin
  if Source is TUCProject then
  begin
    PrjSource := TUCProject(Source);
    self.UcMode := PrjSource.UcMode;
    fMainInputFileName := PrjSource.MainInputFileName;
    fMainInputFileNamePred := PrjSource.MainInputFileNamePred;
    OutputPrefix := PrjSource.OutputPrefix;
    OutputPrefixPred := PrjSource.OutputPrefixPred;
    Verbose := PrjSource.Verbose;
    DerivativesInterface := PrjSource.DerivativesInterface;
    PathToMergedFile := PrjSource.PathToMergedFile;
    UseModelDerivatives := PrjSource.UseModelDerivatives;
    PathToFile := PrjSource.PathToFile;
    SkipLines := PrjSource.SkipLines;
    ModelName := PrjSource.ModelName;
    ModelLengthUnits := PrjSource.ModelLengthUnits;
    ModelTimeUnits := PrjSource.ModelTimeUnits;
    ModelMassUnits := PrjSource.ModelMassUnits;
    Sensitivities := PrjSource.Sensitivities;
    PredictionSensitivities := PrjSource.PredictionSensitivities;
    fOptimize := PrjSource.Optimize;
    fLinearity := PrjSource.Linearity;
    fPrediction := PrjSource.Prediction;
    fAdvLin := PrjSource.fAdvLin;
    LinearityAdv := PrjSource.LinearityAdv;
    fAdvLinOpt := PrjSource.AdvLinOpt;
    fNonLinearIntervals := PrjSource.fNonLinearIntervals;
    //SosSurface := PrjSource.SosSurface;
    SosMethod := PrjSource.SosMethod;
    SosFile := PrjSource.SosFile;
    StdErrOne := PrjSource.StdErrOne;
    WriteDerivedParams := PrjSource.WriteDerivedParams;
    WritePriorInfo := PrjSource.WritePriorInfo;
    EigenValues := PrjSource.EigenValues;
    StartRes := PrjSource.StartRes;
    IntermedRes := PrjSource.IntermedRes;
    FinalRes := PrjSource.FinalRes;
    StartSens := PrjSource.StartSens;
    IntermedSens := PrjSource.IntermedSens;
    FinalSens := PrjSource.FinalSens;
    DataExchange := PrjSource.DataExchange;
    CreateInitFiles := PrjSource.CreateInitFiles;
    Tolpar := PrjSource.Tolpar;
    TolSOSC := PrjSource.TolSOSC;
    MaxIter := PrjSource.MaxIter;
    MaxChange := PrjSource.MaxChange;
    MaxChangeRealm := PrjSource.MaxChangeRealm;
    MqrtDirection := PrjSource.MqrtDirection;
    MqrtFactor := PrjSource.MqrtFactor;
    MqrtIncrement := PrjSource.MqrtIncrement;
    QuasiNewton := PrjSource.QuasiNewton;
    QNiter := PrjSource.QNiter;
    self.QNsosr := PrjSource.QNsosr;
    OmitDefault := PrjSource.OmitDefault;
    OmitVals.Assign(PrjSource.OmitVals);
    StatsOnNonconverge := PrjSource.StatsOnNonconverge;
    OmitInsensitive := PrjSource.OmitInsensitive;
    MinimumSensRatio := PrjSource.MinimumSensRatio;
    ReincludeSensRatio := PrjSource.ReincludeSensRatio;
    TolParWtOS := PrjSource.TolParWtOS;
    TrustRegion := PrjSource.TrustRegion;
    MaxStep := PrjSource.MaxStep;
    UseMaxStep := PrjSource.UseMaxStep;
    ConsecMax := PrjSource.ConsecMax;
    ParallelControl.Assign(PrjSource.ParallelControl);
    ParallelRunners.Assign(PrjSource.ParallelRunners);
    RunnerFiles.Assign(PrjSource.RunnerFiles);
  end;
end;

procedure TUcProject.AssignMode(Mode: TUcodeMode);
begin
  // Start by assigning default (Forward mode) values
  fSensitivities := False;
  fOptimize := False;
  fLinearity := False;
  fPrediction := False;
  fAdvLin := False;
  fNonLinearIntervals := False;
  SosSurface := 'no';
  UcMode := Mode;
  case Mode of
    umFwd: ; // do nothing
    umSensAnal:
      begin
        fSensitivities := True;
      end;
    umParEst:
      begin
        fSensitivities := True;
        fOptimize := True;
      end;
    umTestLin:
      begin
        fLinearity := True;
      end;
    umPred:
      begin
        //fSensitivities := True; { TODO 2 -cPredictions : Prediction default should be sensitivities=no--Eileen is changing Ucode to do this? }
        fPrediction := True;
      end;
    umAdvTestLin:
      begin
        fAdvLin := True;
      end;
    umNonlinUncert:
      begin
        fNonLinearIntervals := True;
      end;
    umInvObjFunc:
      begin
        SosSurface := 'yes'; { TODO 4 -cSOS : Add support for SOSSurface = file }//.
      end;
  end;
end;

procedure TUcProject.BuildOptionsBlock(OptBlock: TStringList);
var
  bdOptions: TBlockData;                 // Options
begin
  bdOptions := TBlockData.CreateAndAllocate(1,3); // Need 3 keywords
  try
    bdOptions.KeyValMatrix[0].Vtype := vtInt;
    bdOptions.KeyValMatrix[0].SetNameVal(0, 'Verbose', Verbose);
    bdOptions.KeyValMatrix[1].Vtype := vtStr;
    bdOptions.KeyValMatrix[1].SetNameVal(0, 'Derivatives_Interface',
                                   DerivativesInterface);
    bdOptions.KeyValMatrix[2].Vtype := vtStr;
    bdOptions.KeyValMatrix[2].SetNameVal(0, 'PathToMergedFile',
                                   PathToMergedFile);
    J_BuildInputBlock('Options', bfKeywords, bdOptions, OptBlock);
  finally
    bdOptions.Free;
  end;
end;

function TUcProject.BuildParallelControlBlock(PCBlock: TStringList): boolean;
var
  Defaults: TBlockData;
  bdParallelControl: TBlockData;        // Parallel_Control
  OK: boolean;
  Messg: string;
begin
  OK := False;
  // No need for keyword "OperatingSystem" because in ModelMate, Windows is assumed.
  Defaults := TBlockData.CreateAndAllocate(1,5);
  bdParallelControl := TBlockData.CreateAndAllocate(1,5); // Max 5 keywords
  try
    with Defaults do
      begin
        // Assign defaults for the Parallel_Control input block
        KeyValMatrix[0].Vtype := vtBool;
        KeyValMatrix[0].SetNameVal(0,'Parallel',False);
        KeyValMatrix[1].Vtype := vtDbl;
        KeyValMatrix[1].SetNameVal(0,'Wait',0.001);
        KeyValMatrix[2].Vtype := vtInt;
        KeyValMatrix[2].SetNameVal(0,'VerboseRunner',3);
        KeyValMatrix[3].Vtype := vtBool;
        KeyValMatrix[3].SetNameVal(0,'AutostopRunners',True);
        KeyValMatrix[4].Vtype := vtDbl;
        KeyValMatrix[4].SetNameVal(0,'TimeoutFactor',3.0);
      end;
    bdParallelControl.Assign(Defaults);
    with bdParallelControl do
      begin
        KeyValMatrix[0].SetVal(0,ParallelControl.Parallel);
        KeyValMatrix[1].SetVal(0,ParallelControl.Wait);
        KeyValMatrix[2].SetVal(0,ParallelControl.VerboseRunner);
        KeyValMatrix[3].SetVal(0,ParallelControl.AutoStopRunners);
        KeyValMatrix[4].SetVal(0,ParallelControl.TimeOutFactor);
      end;
    J_BuildInputBlockExclDef('Parallel_Control',bdParallelControl,
                                     Defaults, PCBlock, False);
    OK := True;
  finally
    Defaults.Free;
    bdParallelControl.Free;
    result := OK;
    if not OK then
      begin
        Messg := 'Error encountered building Parallel_Control block';
        ShowMessage(Messg);
      end;
  end;
end; // function TUcProject.BuildParallelControlBlock

function TUcProject.BuildParallelRunnersBlock(BlkFmt: TBlockFormat;
  PRBlock: TStringList): boolean;
var
  bdParallelRunners: TBlockData; // Parallel_Runners.
  I, J, NR, NumToUse: integer;
  RunnerDir, DirSep, Messg: string;
  OK: boolean;
begin
  DirSep := '\';
  OK := False;
  PRBlock.Clear;
  NumToUse := self.ParallelControl.NumRunnersToUse;
  NR := 0;
  if ParallelControl.Parallel then
    begin
      for I := 0 to ParallelRunners.Count - 1 do
        begin
          if ParallelRunners.Items[I].Use then
            NR := NR + 1;
        end;
    end;
  if (NR > 0) and (NumToUse > 0) then
    try
      // Need 3 columns (keywords) and NR rows in bdParallelRunners.
      // Keyword "Rename" is unneeded because OS is always Windows, and default "ren" is correct.
      if NumToUse < NR then NR := NumToUse;
      bdParallelRunners := TBlockData.CreateAndAllocate(NR,3);
      bdParallelRunners.KeyValMatrix[0].Vtype := vtStr; // RunnerName
      bdParallelRunners.KeyValMatrix[1].Vtype := vtStr; // RunnerDir
      bdParallelRunners.KeyValMatrix[2].Vtype := vtDbl; // RunTime
      //
      J := 0;
      for I := 0 to ParallelRunners.Count - 1 do
        if (ParallelRunners.Items[I].Use) and (J < NumToUse) then
          begin
            bdParallelRunners.KeyValMatrix[0].SetNameVal(J,'RunnerName',ParallelRunners.Items[I].Name);
            RunnerDir := ParallelRunners.Items[I].Directory + DirSep;
            bdParallelRunners.KeyValMatrix[1].SetNameVal(J,'RunnerDir',RunnerDir);
            bdParallelRunners.KeyValMatrix[2].SetNameVal(J,'RunTime',ParallelRunners.Items[I].ExpectedRunTime);
            J := J + 1;
          end;
      J_BuildInputBlock('Parallel_Runners', BlkFmt, bdParallelRunners, PRBlock);
      OK := True;
    finally
      FreeAndNil(bdParallelRunners);
      if not OK then
        Messg := 'Error encountered building Parallel_Runners block';
    end
  else
    begin
      OK := False;
      Messg := 'No parallel runners have been defined';
    end;
  result := OK;
  if not OK then
    ShowMessage(Messg);
end; // function TUcProject.BuildParallelRunnersBlock


procedure TUcProject.BuildRegGNControlsBlock(RGNCBlock: TStringList);
var
  Defaults: TBlockData;
  bdRegGNControls: TBlockData;           // Reg_GN_Controls
begin
  Defaults := TBlockData.CreateAndAllocate(1,20);
  bdRegGNControls := TBlockData.CreateAndAllocate(1,20); // Max 20 keywords
  try
    with Defaults do
      begin
        // Assign defaults for the Reg_GN_Controls input block
        KeyValMatrix[0].Vtype := vtDbl;
        KeyValmatrix[0].SetNameVal(0,'TolPar',0.01);
        KeyValMatrix[1].Vtype := vtDbl;
        KeyValMatrix[1].SetNameVal(0,'TolSOSC',0.0);
        KeyValMatrix[2].Vtype := vtInt;
        KeyValMatrix[2].SetNameVal(0,'MaxIter',5);
        KeyValMatrix[3].Vtype := vtDbl;
        KeyValMatrix[3].SetNameVal(0,'MaxChange',2.0);
        KeyValMatrix[4].Vtype := vtStr;
        KeyValMatrix[4].SetNameVal(0,'MaxChangeRealm','Native');
        KeyValMatrix[5].Vtype := vtDbl;
        KeyValMatrix[5].SetNameVal(0,'MqrtDirection',85.4);
        KeyValMatrix[6].Vtype := vtDbl;
        KeyValMatrix[6].SetNameVal(0,'MqrtFactor',1.5);
        KeyValMatrix[7].Vtype := vtDbl;
        KeyValMatrix[7].SetNameVal(0,'MqrtIncrement',0.001);
        KeyValMatrix[8].Vtype := vtBool;
        KeyValMatrix[8].SetNameVal(0,'QuasiNewton',False);
        KeyValMatrix[9].Vtype := vtInt;
        KeyValMatrix[9].SetNameVal(0,'QNiter',5);
        KeyValMatrix[10].Vtype := vtDbl;
        KeyValMatrix[10].SetNameVal(0,'QNsosr',0.01);
        KeyValMatrix[11].Vtype := vtInt;
        KeyValMatrix[11].SetNameVal(0,'OmitDefault',0);
        KeyValMatrix[12].Vtype := vtBool;
        KeyValMatrix[12].SetNameVal(0,'Stats_On_Nonconverge',True);
        KeyValMatrix[13].Vtype := vtBool;
        KeyValMatrix[13].SetNameVal(0,'OmitInsensitive',False);
        KeyValMatrix[14].Vtype := vtDbl;
        KeyValMatrix[14].SetNameVal(0,'MinimumSensRatio',0.005);
        KeyValMatrix[15].Vtype := vtDbl;
        KeyValMatrix[15].SetNameVal(0,'ReincludeSensRatio',0.02);
        KeyValMatrix[16].Vtype := vtDbl;
        KeyValMatrix[16].SetNameVal(0,'TolParWtOS',10.0);
        KeyValMatrix[17].Vtype := vtStr;
        KeyValMatrix[17].SetNameVal(0,'TrustRegion','no');
        KeyValMatrix[18].Vtype := vtDbl;
        KeyValMatrix[18].SetNameVal(0,'MaxStep',0.0);
        KeyValMatrix[19].Vtype := vtInt;
        KeyValMatrix[19].SetNameVal(0,'ConsecMax',5);
      end;
    bdRegGNControls.Assign(Defaults);
    with bdRegGNControls do
      begin
        KeyValMatrix[0].SetVal(0,self.Tolpar);
        KeyValMatrix[1].SetVal(0,self.TolSOSC);
        KeyValMatrix[2].SetVal(0,self.MaxIter);
        KeyValMatrix[3].SetVal(0,self.MaxChange);
        KeyValMatrix[4].SetVal(0,self.MaxChangeRealm);
        KeyValMatrix[5].SetVal(0,self.MqrtDirection);
        KeyValMatrix[6].SetVal(0,self.MqrtFactor);
        KeyValMatrix[7].SetVal(0,self.MqrtIncrement);
        KeyValMatrix[8].SetVal(0,self.QuasiNewton);
        KeyValMatrix[9].SetVal(0,self.QNiter);
        KeyValMatrix[10].SetVal(0,self.QNsosr);
        KeyValMatrix[11].SetVal(0,self.OmitVals.Count);  // value for OmitDefault
        KeyValMatrix[12].SetVal(0,self.StatsOnNonconverge);
        KeyValMatrix[13].SetVal(0,self.OmitInsensitive);
        KeyValMatrix[14].SetVal(0,self.MinimumSensRatio);
        KeyValMatrix[15].SetVal(0,self.ReincludeSensRatio);
        KeyValMatrix[16].SetVal(0,self.TolParWtOS);
        KeyValMatrix[17].SetVal(0,self.TrustRegion);
        KeyValMatrix[18].SetVal(0,self.MaxStep);
        KeyValMatrix[19].SetVal(0,self.ConsecMax);
      end;
    J_BuildInputBlockExclDef('Reg_GN_Controls',bdRegGNControls,
                                     Defaults, RGNCBlock, False);
  finally
    Defaults.Free;
    bdRegGNControls.Free;
  end;
end; // function TUcProject.BuildRegGNControlsBlock

procedure TUcProject.BuildUcodeControlDataBlock(UCDBlock: TStringList);
var
  Defaults: TBlockData;
  bdUcodeControlData: TBlockData;        // UCODE_Control_Data
begin
  Defaults := TBlockData.CreateAndAllocate(1,24);
  bdUcodeControlData := TBlockData.CreateAndAllocate(1,24); // Max 24 keywords
  try
    with Defaults do
      begin
        // Assign defaults for the UCODE_Control_Data input block
        KeyValMatrix[0].Vtype := vtStr;
        KeyValMatrix[0].SetNameVal(0,'ModelName','');
        KeyValMatrix[1].Vtype := vtStr;
        KeyValMatrix[1].SetNameVal(0,'ModelLengthUnits','');
        KeyValMatrix[2].Vtype := vtStr;
        KeyValMatrix[2].SetNameVal(0,'ModelMassUnits','');
        KeyValMatrix[3].Vtype := vtStr;
        KeyValMatrix[3].SetNameVal(0,'ModelTimeUnits','');
        KeyValMatrix[4].Vtype := vtBool;
        KeyValMatrix[4].SetNameVal(0,'Sensitivities',False);
        KeyValMatrix[5].Vtype := vtBool;
        KeyValMatrix[5].SetNameVal(0,'Optimize',False);
        KeyValMatrix[6].Vtype := vtBool;
        KeyValMatrix[6].SetNameVal(0,'Linearity',False);
        KeyValMatrix[7].Vtype := vtBool;
        KeyValMatrix[7].SetNameVal(0,'Prediction',False);
        KeyValMatrix[8].Vtype := vtStr;
        KeyValMatrix[8].SetNameVal(0,'LinearityAdv','no');
        KeyValMatrix[9].Vtype := vtBool;
        KeyValMatrix[9].SetNameVal(0,'NonlinearIntervals',False);
        KeyValMatrix[10].Vtype := vtStr;
        KeyValMatrix[10].SetNameVal(0,'SOSSurface','no');
        KeyValMatrix[11].Vtype := vtStr;
        KeyValMatrix[11].SetNameVal(0,'SOSFile','');
        KeyValMatrix[12].Vtype := vtBool;
        KeyValMatrix[12].SetNameVal(0,'StdErrOne',False);
        KeyValMatrix[13].Vtype := vtBool;
        KeyValMatrix[13].SetNameVal(0,'WriteDerivedParams',True);
        KeyValMatrix[14].Vtype := vtBool;
        KeyValMatrix[14].SetNameVal(0,'WritePriorInfo',True);
        KeyValMatrix[15].Vtype := vtBool;
        KeyValMatrix[15].SetNameVal(0,'EigenValues',True);
        KeyValMatrix[16].Vtype := vtStr;
        KeyValMatrix[16].SetNameVal(0,'StartRes','yes');
        KeyValMatrix[17].Vtype := vtStr;
        KeyValMatrix[17].SetNameVal(0,'IntermedRes','no');
        KeyValMatrix[18].Vtype := vtStr;
        KeyValMatrix[18].SetNameVal(0,'FinalRes','yes');
        KeyValMatrix[19].Vtype := vtStr;
        KeyValMatrix[19].SetNameVal(0,'StartSens','dss');
        KeyValMatrix[20].Vtype := vtStr;
        KeyValMatrix[20].SetNameVal(0,'IntermedSens','none');
        KeyValMatrix[21].Vtype := vtStr;
        KeyValMatrix[21].SetNameVal(0,'FinalSens','dss');
        KeyValMatrix[22].Vtype := vtBool;
        KeyValMatrix[22].SetNameVal(0,'DataExchange',True);
        KeyValMatrix[23].Vtype := vtBool;
        KeyValMatrix[23].SetNameVal(0,'CreateInitFiles',False);
      end;
    bdUcodeControlData.Assign(Defaults);
    with bdUcodeControlData do
      begin
        KeyValMatrix[0].SetVal(0,self.ModelName);
        KeyValMatrix[1].SetVal(0,self.ModelLengthUnits);
        KeyValMatrix[2].SetVal(0,self.ModelMassUnits);
        KeyValMatrix[3].SetVal(0,self.ModelTimeUnits);
        // Sensitivities is a special case.
        case UcMode of
          umFwd: ;
          umSensAnal: KeyValMatrix[4].SetVal(0,True);
          umParEst: KeyValMatrix[4].SetVal(0,True);
          umTestLin: ;
          umPred: KeyValMatrix[4].SetVal(0,PredictionSensitivities);
          umAdvTestLin: ;
          umNonlinUncert: ;
          umInvObjFunc: ;
        end;
        KeyValMatrix[5].SetVal(0,self.Optimize);
        KeyValMatrix[6].SetVal(0,self.Linearity);
        KeyValMatrix[7].SetVal(0,self.Prediction);
        KeyValMatrix[8].SetVal(0,self.LinearityAdv);
        KeyValMatrix[9].SetVal(0,self.NonLinearIntervals);
        KeyValMatrix[10].SetVal(0,self.SosSurface);
        KeyValMatrix[11].SetVal(0,self.SosFile);
        KeyValMatrix[12].SetVal(0,self.StdErrOne);
        KeyValMatrix[13].SetVal(0,self.WriteDerivedParams);
        KeyValMatrix[14].SetVal(0,self.WritePriorInfo);
        KeyValMatrix[15].SetVal(0,self.EigenValues);
        KeyValMatrix[16].SetVal(0,self.StartRes);
        KeyValMatrix[17].SetVal(0,self.IntermedRes);
        KeyValMatrix[18].SetVal(0,self.FinalRes);
        KeyValMatrix[19].SetVal(0,self.StartSens);
        KeyValMatrix[20].SetVal(0,self.IntermedSens);
        KeyValMatrix[21].SetVal(0,self.FinalSens);
        KeyValMatrix[22].SetVal(0,self.DataExchange);
        // CreateInitFiles is a special case
        case UcMode of
          umFwd: ;
          umSensAnal: KeyValMatrix[23].SetVal(0,self.CreateInitFiles);
          umParEst: ;
          umTestLin: ;
          umPred: ;
          umAdvTestLin: ;
          umNonlinUncert: ;
          umInvObjFunc: ;
        end;
      end;
    J_BuildInputBlockExclDef('UCODE_Control_Data',bdUcodeControlData,
                                     Defaults, UCDBlock, False);
  finally
    Defaults.Free;
    bdUcodeControlData.Free;
  end;
end;

procedure TUcProject.CorrectRelativePaths(OrigProjDir: string);
var
  OldRelPath: string;
  I: Integer;
begin
  if MainInputFileName <> '' then
    begin
      OldRelPath := MainInputFileName;
      MainInputFileName := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if MainInputFileNamePred <> '' then
    begin
      OldRelPath := MainInputFileNamePred;
      MainInputFileNamePred := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if FileExists(DerivativesInterface) then
    begin
      OldRelPath := DerivativesInterface;
      DerivativesInterface := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if FileExists(PathToMergedFile) then
    begin
      OldRelPath := PathToMergedFile;
      PathToMergedFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if FileExists(PathToFile) then
    begin
      OldRelPath := PathToFile;
      PathToFile := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
    end;
  if ParallelRunners.Count > 0 then
    begin
      for I := 0 to ParallelRunners.Count - 1 do
        begin
          if DirectoryExists(ParallelRunners.Items[I].Directory) then
            begin
              OldRelPath := ParallelRunners.Items[I].Directory;
              ParallelRunners.Items[I].Directory := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
            end;
        end;
    end;
  if RunnerFiles.Count > 0 then
    begin
      for I := 0 to RunnerFiles.Count - 1 do
        begin
          if FileExists(RunnerFiles.Items[I].FileName) then
            begin
              OldRelPath := RunnerFiles.Items[I].FileName;
              RunnerFiles.Items[I].FileName := ChangeRelPath(OrigProjDir, ProjectDirectory, OldRelPath);
            end;
        end;
    end;
end;

constructor TUcProject.Create(aOwner: TComponent);
begin
  //inherited Create(aOwner);
  inherited;
  { SetSubComponent is needed to allow saving of TUcProject
    as component of TProject }
  SetSubComponent(True);
  // UCODE project scalar defaults are assigned here
  //IMode := 0;
  UcMode := umFwd;
  {  0=Forward; 1=Sensitivity Analysis; 2=Parameter Estimation;
     3=Test Model Linearity; 4=Prediction; 5=Advanced Test Model Linearity;
     6=Nonlinear Uncertainty; 7=Investigate Objective Function
  }
  MainInputFileName := 'Default_Ucode_main.in';
  MainInputFileNamePred := 'Default_Ucode_pred.in';
  OutputPrefix := 'Default_Ucode_main_out';
  OutputPrefixPred := 'Default_Ucode_pred_out';
  // Options
  Verbose := 3;
  DerivativesInterface := '';
  PathToMergedFile := '';
  UseModelDerivatives := False;
//  bdOptions := TBlockData.CreateAndAllocate(1,3); // Need 3 keywords
//  bdUcodeControlData := TBlockData.CreateAndAllocate(1,22); // Max 22 keywords
//  bdRegGNControls := TBlockData.CreateAndAllocate(1,20); // Max 20 keywords
  //
  // Merge_Files.
  PathToFile := '';
  SkipLines := 0;
  //
  // UCODE_Control_Data.
  ModelName := '';
  ModelLengthUnits := '';
  ModelTimeUnits := '';
  ModelMassUnits := '';
  fSensitivities := False;
  fPredictionSensitivities := False;
  fOptimize := False;
  fLinearity := False;
  fPrediction := False;
  fAdvLin := False;
  fAdvLinOpt := opConf;
  fNonLinearIntervals := False;
  SosSurface := 'no';
  SosMethod := smKeywords;
  SosFile := '';
  StdErrOne := False;
  WriteDerivedParams := True;
  WritePriorInfo := True;
  EigenValues := True;
  StartRes := 'yes';
  IntermedRes := 'no';
  FinalRes := 'yes';
  StartSens := 'dss';
  IntermedSens := 'none';
  FinalSens := 'dss';
  DataExchange := True;
  CreateInitFiles := False;
  //
  // Reg_GN_Controls
  Tolpar := 0.01;
  TolSOSC := 0.0;
  MaxIter := 5;
  MaxChange := 2.0;
  MaxChangeRealm := 'Native';
  MqrtDirection := 85.4;
  MqrtFactor := 1.5;
  MqrtIncrement := 0.001;
  QuasiNewton := False;
  QNiter := 5;
  QNsosr := 0.01;
  OmitDefault := 0;
  OmitVals := TStringList.Create;
  StatsOnNonconverge := True;
  OmitInsensitive := False;
  MinimumSensRatio := 0.005;
  ReincludeSensRatio := 0.02;
  TolParWtOS := 10.0;
  TrustRegion := 'no';
  MaxStep := 0.0;
  UseMaxStep := False;
  ConsecMax := 5;
  fParallelControl := TParallelControl.Create;
  fParallelRunners := TParallelRunners.Create;
  fRunnerFiles := TRunnerFiles.Create;
  //
end;

destructor TUCProject.Destroy;
begin
  fOmitVals.Free;
  fParallelControl.Free;
  fParallelRunners.Free;
  fRunnerFiles.Free;
  inherited;
end;

procedure TUcProject.ExportOmitFile(const Directory: string);
// Export a UCODE fn.omit file.
var
  I: integer;
  slExport: TStringList;
  OmitFile: TFileName;
begin
  if OmitVals.Count > 0 then
    begin
      slExport := TStringList.Create;
      try
        slExport.Clear;
        slExport.Add('BEGIN OMIT_DATA');
        for I := 0 to OmitVals.Count - 1 do
          begin
            slExport.Add('  ' + OmitVals.Strings[I]);
          end;
        slExport.Add('END OMIT_DATA');
        case self.UcMode of
          umFwd: OmitFile := Directory + '\' + OutputPrefix + '.omit';
          umSensAnal: OmitFile := Directory + '\' + OutputPrefix + '.omit';
          umParEst: OmitFile := Directory + '\' + OutputPrefix + '.omit';
          umTestLin: OmitFile := Directory + '\' + OutputPrefix + '.omit';
          umPred: OmitFile := Directory + '\' + OutputPrefixPred + '.omit';
          umAdvTestLin: OmitFile := Directory + '\' + OutputPrefix + '.omit';
          umNonlinUncert: OmitFile := Directory + '\' + OutputPrefix + '.omit';
          umInvObjFunc: OmitFile := Directory + '\' + OutputPrefix + '.omit';
        end;
        slExport.SaveToFile(OmitFile);
      finally
        slExport.Free;
      end;
    end;
end;

function TUcProject.FindMainInputFileName: string;
begin
  case self.fUcMode of
    umFwd: result := MainInputFileName;
    umSensAnal: result := MainInputFileName;
    umParEst: result := MainInputFileName;
    umTestLin: result := MainInputFileName;
    umPred: result := MainInputFileNamePred;
    umAdvTestLin: result := MainInputFileName;
    umNonlinUncert: result := MainInputFileName;
    umInvObjFunc: result := MainInputFileName;
  else
    result := MainInputFileName;
  end;
end;

function TUcProject.GetModelInfo(ModelInfo: TUcModelInfo): boolean;
// Read _dm file and return contents.
var
  slDM: TStringList;
  fnDm: TFileName;
  Dir, DirSep, Line, StrTemp, LabelStr, StrRem, StrX: string;
  Errors, I: integer;
begin
  Errors := 0;
  DirSep := '\';
  result := False;
//  StrTemp := self.AbsMainInputFileName(muCalib);
  Dir := ExtractFileDir(AbsMainInputFileName(muCalib));
  StrTemp := Dir + DirSep + OutputPrefix;
  fnDm := ChangeFileExt(StrTemp,'._dm');
  if FileExists(fnDm) then
    begin
      // Instantiate slDM and read _dm into slDM.
      slDM := TStringList.Create;
      try
        slDM.LoadFromFile(fnDm);
        for I := 0 to ModelInfo.NumFields - 1 do
          begin
            case I of
              0: LabelStr := 'MODEL NAME: ';
              1: LabelStr := 'MODEL LENGTH UNITS: ';
              2: LabelStr := 'MODEL MASS UNITS: ';
              3: LabelStr := 'MODEL TIME UNITS: ';
              4: LabelStr := 'NUMBER ESTIMATED PARAMETERS: ';
              5: LabelStr := 'ORIGINAL NUMBER ESTIMATED PARAMETERS: ';
              6: LabelStr := 'TOTAL NUMBER PARAMETERS: ';
              7: LabelStr := 'NUMBER OBSERVATIONS INCLUDED: ';
              8: LabelStr := 'NUMBER OBSERVATIONS PROVIDED: ';
              9: LabelStr := 'NUMBER PRIOR: ';
              10: LabelStr := 'REGRESSION CONVERGED: ';
              11: LabelStr := 'CALCULATED ERROR VARIANCE: ';
              12: LabelStr := 'STANDARD ERROR OF THE REGRESSION: ';
              13: LabelStr := 'MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS (MLOFD): ';
              14: LabelStr := 'MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS AND PRIOR (MLOFDP): ';
              15: LabelStr := 'AICc (MLOFD + AICc PENALTY): ';
              16: LabelStr := 'BIC (MLOFD + BIC PENALTY): ';
              17: LabelStr := 'HQ (MLOFD + HQ PENALTY): ';
              18: LabelStr := 'KASHYAP (MLOFD + KASHYAP PENALTY): ';
              19: LabelStr := 'LN DETERMINANT OF FISHER INFORMATION MATRIX: ';
              20: LabelStr := 'RN2 DEPENDENTS: ';
              21: LabelStr := 'RN2 DEPENDENTS AND PRIOR: ';
              22: LabelStr := 'NUMBER OF ITERATIONS: ';
              23: LabelStr := 'KASHYAP (MLOFDP + KASHYAP PENALTY wPri): ';
              24: LabelStr := 'LN DETERMINANT OF FISHER INFORMATION MATRIX wPri: ';
              25: LabelStr := 'SOME SENSITIVITIES BY FORWARD DIFFERENCE PERTURBATION: ';
            end;
            Line := slDM.Strings[I];
            // Get quoted string and remove it from Line.
            StrTemp := GetQuotedString(Line,StrRem);
            if not AnsiSameText(StrTemp,LabelStr) then
              Errors := Errors + 1;
            // Populate ModelInfo.
            case I of
              0: ModelInfo.fName := GetQuotedString(StrRem,StrX);
              1: ModelInfo.fLengthUnit := GetQuotedString(StrRem,StrX);
              2: ModelInfo.fMassUnit := GetQuotedString(StrRem,StrX);
              3: ModelInfo.fTimeUnit := GetQuotedString(StrRem,StrX);
              4: ModelInfo.fNumEstParams := StrToInt(StrRem);
              5: ModelInfo.fOrigNumEstParams := StrToInt(StrRem);
              6: ModelInfo.fTotalNumParams := StrToInt(StrRem);
              7: ModelInfo.fNumObsIncluded := StrToInt(StrRem);
              8: ModelInfo.fNumObsProvided := StrToInt(StrRem);
              9: ModelInfo.fNumPrior := StrToInt(StrRem);
              10: begin
                    StrTemp := GetQuotedString(StrRem,StrX);
                    ModelInfo.fRegConv := AnsiSameText(StrTemp,'YES');
                  end;
              11: ModelInfo.fCalcErrVar := StrToFloat(StrRem);
              12: ModelInfo.fStdErrReg := StrToFloat(StrRem);
              13: ModelInfo.fMLOFD := StrToFloat(StrRem);
              14: ModelInfo.fMLOFDP := StrToFloat(StrRem);
              15: ModelInfo.fAICc := StrToFloat(StrRem);
              16: ModelInfo.fBIC := StrToFloat(StrRem);
              17: ModelInfo.fHQ := StrToFloat(StrRem);
              18: ModelInfo.fKashyap := StrToFloat(StrRem);
              19: ModelInfo.fLnDeterFisherInfoMx := StrToFloat(StrRem);
              20: ModelInfo.fRN2Dep := StrToFloat(StrRem);
              21: ModelInfo.fRN2DepPri := StrToFloat(StrRem);
              22: ModelInfo.fNumIter := StrToInt(StrRem);
              23: ModelInfo.fKashyapPri := StrToFloat(StrRem);
              24: ModelInfo.fLnDeterFisherInfoMxPri := StrToFloat(StrRem);
              25: begin
                    StrTemp := GetQuotedString(StrRem,StrX);
                    ModelInfo.fSensByFwdDiff := AnsiSameText(StrTemp,'YES');
                  end;
            end;
          end;
      finally
        slDM.Free;
      end;
    end;
  if Errors = 0 then result := True;  
end;

function TUcProject.OutputMainFile: TFileName;
var
  FName: string;
begin
  case self.fUcMode of
    umFwd: FName := self.OutputPrefix + '.#uout';
    umSensAnal: FName := self.OutputPrefix + '.#uout';
    umParEst: FName := self.OutputPrefix + '.#uout';
    umTestLin: FName := self.OutputPrefix + '.#umodlin';
    umPred: FName := self.OutputPrefixPred + '.#upred';
    umAdvTestLin: FName := self.OutputPrefix + '.#uout';
    umNonlinUncert: FName := self.OutputPrefix + '.#unonlinint_conf';
    umInvObjFunc: FName := self.OutputPrefix + '.#usos';
  else
    FName := self.OutputPrefix + '.#uout';
  end;
  result := FName;
end;

function TUcProject.PopulateRunnerDirectories(var ErrMsg: string;
                     var NFiles: integer; var NDir: integer): boolean;
var
  IDir, IFile: integer;
  MFileName, RFileName: string;
  DirName, DirSep: string;
  MFileDate, RFileDate: TDateTime;
begin
  ErrMsg := '';
  DirSep := '\';
  result := True;
  NFiles := 0;
  NDir := 0;
  if RunnerFiles.Count > 0 then
    begin
      if ParallelRunners.NumUsable > 0 then
        begin
          for IDir := 0 to ParallelRunners.Count - 1 do
            begin
              DirName := ParallelRunners.Items[IDir].AbsDirectory;
              if DirectoryExists(DirName) then
                begin
                  for IFile := 0 to RunnerFiles.Count - 1 do
                    begin
                      MFileName := ExtractFileName(RunnerFiles.Items[IFile].FileName);
                      FileAge(RunnerFiles.Items[IFile].FileName,MFileDate);
                      RFileName := DirName + DirSep + MFileName;
                      MFileName := RunnerFiles.Items[IFile].FileName;
                      if FileExists(RFileName) then
                        begin
                          FileAge(RFileName,RFileDate);
                          if MFileDate > RFileDate then
                            begin
                              result := CopyFile(PChar(MFileName),PChar(RFileName),False);
                              if result then
                                begin
                                  NFiles := NFiles + 1;
                                end
                              else
                                begin
                                  ErrMsg := 'Failed to copy file "' + MFileName + '" to "' + RFileName + '"';
                                  Exit; // Exit the procedure.
                                end;
                            end;
                        end
                      else
                        begin
                          result := CopyFile(PChar(MFileName),PChar(RFileName),False);
                          if result then
                            begin
                              NFiles := NFiles + 1;
                            end
                          else
                            begin
                              ErrMsg := 'Failed to copy file "' + MFileName + '" to "' + RFileName + '"';
                              Exit; // Exit the procedure.
                            end;
                        end;
                    end;
                    NDir := NDir + 1;
                end
              else
                begin
                  if ParallelRunners.Items[IDir].Use then
                    begin
                      result := False;
                      ErrMsg := 'Runner directory "' + DirName + '" does not exist';
                      Exit; // Exit the procedure.
                    end;
                end;
            end;
        end;
    end;
end;

function TUcProject.ModeIsParallelizable: boolean;
begin
  result := ((UcMode in ParallelUcodeModes) or ((UcMode = umPred) and (self.PredictionSensitivities)))
end;

procedure TUcProject.SetMainInputFileName(const Value: string);
begin
  fMainInputFileName := RelativePath(Value);
end;

procedure TUcProject.SetMainInputFileNamePred(const Value: string);
begin
  fMainInputFileNamePred := RelativePath(Value);
end;

function TUcProject.StartLocalRunners: integer;
var
  I, KUse, NumRunners, NumToUse, NumUseTrue: integer;
  Cmd, DirSep, Exe, OrigDir, TempDir: string;
  FoundExe: boolean;
begin
  result := 0;
  if ModeIsParallelizable then
    begin
      DirSep := '\';
      NumRunners := ParallelRunners.Count;
      if NumRunners > 0 then
        begin
          OrigDir := GetCurrentDir;
          try
            NumUseTrue := ParallelRunners.NumUsable;
            NumToUse := self.ParallelControl.NumRunnersToUse;
            if (NumToUse > 0) and (NumUseTrue > 0)  then
              begin
                KUse := 0;
                for I := 0 to NumRunners - 1 do
                  begin
                    if (KUse < NumToUse) and (ParallelRunners.Items[I].Use) then
                      begin
                        try
                          TempDir := ParallelRunners.Items[I].AbsDirectory;
                          if DirectoryIsLocal(TempDir) then
                            begin
                              if DirectoryExists(TempDir) then
                                begin
                                  FoundExe := False;
                                  Exe := TempDir + DirSep + 'jrunner.exe';
                                  if FileExists(Exe) then
                                    begin
                                      FoundExe := True
                                    end
                                  else
                                    begin
                                      Exe := TempDir + DirSep + 'runner.exe';
                                      if FileExists(Exe) then
                                        begin
                                          FoundExe := True;
                                        end;
                                    end;
                                  if FoundExe then
                                    begin
                                      SetCurrentDir(TempDir);
                                      Cmd := Exe;
                                      if ExecuteCommand(Cmd, False) then
                                        result := result + 1;
                                    end;
                                end;
                            end;
                        finally
                          KUse := KUse + 1;
                        end;
                      end;
                  end;
              end;
          finally
            SetCurrentDir(OrigDir);
          end;
        end;
    end;
end;

procedure TUcProject.UpdateMode;
begin
  // TODO 2 : Need to cover all possibilities for mode.
  if Prediction then
    begin
      UcMode := umPred;
    end
  else if Sensitivities then
    begin
      if Optimize then
        UcMode := umParEst
      else
        UcMode := umSensAnal;
    end
  else
    begin
      UcMode := umFwd;
    end;
end;

//###################################################################

{ Public procedures }

//###################################################################

procedure U_Free;
var
  I: integer;
begin
  // Arrays
  SetLength(SensOptions,0);
  for I := length(spSenMethod) - 1 downto 0 do
      spSenMethod[I].Free;
  SetLength(spSenMethod,0);
  // Objects
  FreeAndNil(sdUcode);
  FreeAndNil(slSenMethod);
  TempUcProject.Free;
  UcModelInfo.Free;
end; // procedure U_Free

//###################################################################

procedure U_Initialize(aOwner:TComponent);
var
  I: integer;
begin
  // Arrays
  SetLength(SensOptions,7);
  SensOptions[0] := 'css';
  SensOptions[1] := 'dss';
  SensOptions[2] := 'onepercentss';
  SensOptions[3] := 'allss';
  SensOptions[4] := 'unscaled';
  SensOptions[5] := 'all';
  SensOptions[6] := 'none';

  SetLength(spSenMethod,4);
  for I := 0 to length(spSenMethod) - 1 do
      spSenMethod[I] := TStringPair.Create;
  spSenMethod[0].Value := '-1';
  spSenMethod[0].Explanation := '-1: Process model, Log-transformed as needed';
  spSenMethod[1].Value := '0';
  spSenMethod[1].Explanation := '0: Process Model, Native';
  spSenMethod[2].Value := '1';
  spSenMethod[2].Explanation := '1: Forward Differences';
  spSenMethod[3].Value := '2';
  spSenMethod[3].Explanation := '2: Central Differences';

  slSenMethod := TStringList.Create;
  for I := 0 to length(spSenMethod) - 1 do
    begin
      slSenMethod.Add(spSenMethod[I].Explanation);
    end;

  // Sets
  UcodePATypes := JupiterPATypes + [patConstrain, patConstraints,
         patLowerValue, patUpperValue, patMaxChange, patNonLinearInterval,
         patPerturbAmt, patReasRange, patScalePval, patSenMethod,
         patSosIncrement, patTolPar];
//  UcodeDATypes := JupiterOATypes + [datNonDetect, datWtOSConstant];
  // TODO 3: Check that set includes only modes for which parallel processing is useful.
  ParallelUcodeModes := [umSensAnal, umParEst, umTestLin, umAdvTestLin,
                         umNonlinUncert, umInvObjFunc];
  // Objects
  sdUcode := TSaveDialog.Create(aOwner);
  sdUcode.Filter := 'UCODE input files (*.in)|*.in';
  sdUcode.DefaultExt := 'in';
  TempUcProject := TUcProject.Create(nil);
  UcModelInfo := TUcModelInfo.Create;
end; // procedure U_Initialize

//###################################################################

function U_GetSensOptPos(SensOpt: string): integer;
var
  L: integer;
  I: Integer;
begin
  result := -1;
  L := length(SensOptions);
  for I := 0 to L - 1 do
    if SensOptions[I] = SensOpt then result := I;
end; // function U_GetSensOptPos

//###################################################################

{ TUcModelInfo }

function TUcModelInfo.NumFields: integer;
begin
  result := inherited NumFields + 3;
end;

initialization
  U_Initialize(nil);

finalization
  U_Free;
  
end.
