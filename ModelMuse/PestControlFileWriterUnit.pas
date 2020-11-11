unit PestControlFileWriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils, PestObsUnit, GoPhastTypes,
  PhastModelUnit, ObsInterfaceUnit;

type
  TPestControlFileWriter = class(TCustomFileWriter)
  private
    FNameOfFile: string;
    FUsedObservations: TObservationInterfaceList;
    procedure WriteFirstLine;
    procedure WriteSectionHeader(const SectionID: String);
    procedure WriteControlSection;
    procedure WriteSensitivityReuse;
    procedure WriteSingularValueDecomposition;
    procedure WriteLsqr;
    procedure WriteAutomaticUserIntervention;
    procedure WriteSVD_Assist;
    procedure WriteParameterGroups;
    procedure WriteParameters;
    procedure WriteObservationGroups;
    procedure WriteObservations;
    procedure WriteDerivatives;
    procedure WriteCommandLine;
    procedure WriteModelInputOutput;
    procedure WritePriorInformation;
    procedure WritePredictiveAnalysis;
    procedure WriteRegularisation;
    procedure WritePareto;
    // NPAR
    function NumberOfParameters: Integer;
    // NOBS
    function NumberOfObservations: integer;
    // NPARGP
    function NumberOfParameterGroups: Integer;
    // NPRIOR
    function NumberOfPriorInformation: Integer;
    // NOBSGP
    function NumberOfObservationGroups: Integer;
    function NumberOfTemplateFiles: Integer;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  PestPropertiesUnit, ModflowParameterUnit, OrderedCollectionUnit,
  PestParamGroupsUnit, PestObsGroupUnit, frmGoPhastUnit,
  PestObsExtractorInputWriterUnit, frmErrorsAndWarningsUnit,
  ModflowCHD_WriterUnit, ModflowHobUnit, ModflowDRN_WriterUnit,
  ModflowRiverWriterUnit, ModflowGHB_WriterUnit, ModflowStrWriterUnit,
  ModflowPackagesUnit, DataSetUnit;

resourcestring
  StrNoParametersHaveB = 'No parameters have been defined';
  StrNoPestParameters = 'No parameters have been defined for use with PEST.';
  StrNoParameterGroups = 'No parameter groups defined';
  StrNoPESTParameterGr = 'No PEST parameter groups have been defined. Define them in "Model|Manage Parameters".';
  StrNoObservationsDefi = 'No observations defined';
  StrNoObservationsHave = 'No observations have been defined for PEST.';
  StrNoObservationGroup = 'No observation groups defined';
  StrNoPESTObservation = 'No PEST observation groups have been defined. "Define them in "Model|Pest Properties".';
  StrParameterGroupName = 'Parameter group name not assigned';
  StrTheParameterSH = 'The parameter "%s" has not been assigned to a paramet' +
  'er group ';
  StrObservationGroupNo = 'Observation group not assigned';
  StrNoObservationGroupAssigned = 'No observation group has been assigned to %s.';

{ TPestControlFileWriter }

const
  Mf15ParamType: TParameterTypes = [ptRCH, ptETS, ptHFB, ptPEST, ptCHD,
  ptGHB, ptQ, ptRIV, ptDRN];

  Mf2005ParamType: TParameterTypes = [ptLPF_HK, ptLPF_HANI, ptLPF_VK,
    ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB, ptRCH, ptEVT, ptETS,
    ptCHD, ptGHB, ptQ,
    ptRIV, ptDRN, ptDRT, ptSFR, ptHFB,
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_SS, ptHUF_SY,
    ptHUF_SYTP, ptHUF_KDEP, ptHUF_LVDA, ptSTR, ptQMAX, ptPEST];

constructor TPestControlFileWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FUsedObservations := TObservationInterfaceList.Create;
end;

destructor TPestControlFileWriter.Destroy;
begin
  FUsedObservations.Free;
  inherited;
end;

class function TPestControlFileWriter.Extension: string;
begin
  result := '.pst';
end;

function TPestControlFileWriter.NumberOfObservationGroups: Integer;
begin
  result := Model.PestProperties.ObservationGroups.Count;
  if result = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoObservationGroup,
      StrNoPESTObservation);
  end;
end;

function TPestControlFileWriter.NumberOfObservations: integer;
var
  TempList: TObservationInterfaceList;
  ObsIndex: Integer;
  AnObs: TCustomObservationItem;
  IObs: IObservationItem;
begin
  TempList := TObservationInterfaceList.Create;
  try
    frmGoPhast.PhastModel.FillObsInterfaceItemList(TempList, True);
    FUsedObservations.Capacity := TempList.Count;
    for ObsIndex := 0 to TempList.Count - 1 do
    begin
      IObs := TempList[ObsIndex];
      if IObs is TCustomObservationItem then
      begin
        AnObs := TCustomObservationItem(IObs);
        if AnObs.Print then
        begin
          FUsedObservations.Add(IObs);
        end;
      end
      else
      begin
        FUsedObservations.Add(IObs);
      end;
    end;
    result := FUsedObservations.Count;
    if result = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoObservationsDefi,
        StrNoObservationsHave);
    end;
  finally
    TempList.Free;
  end;
end;

function TPestControlFileWriter.NumberOfParameterGroups: Integer;
begin
  result := Model.ParamGroups.Count;
  if result = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoParameterGroups,
      StrNoPESTParameterGr);
  end;
end;

function TPestControlFileWriter.NumberOfParameters: Integer;
var
  ParamIndex: Integer;
  AParam: TModflowParameter;
  UsedTypes: TParameterTypes;
begin
  result := 0;

  UsedTypes := [];
  if Model.ModelSelection = msModflow2015 then
  begin
    UsedTypes := Mf15ParamType;
  end
  else if Model.ModelSelection in Modflow2005Selection then
  begin
    UsedTypes := Mf2005ParamType
  end
  else
  begin
    Assert(False);
  end;

  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    AParam := Model.ModflowSteadyParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      Inc(result);
    end;
  end;

  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam := Model.ModflowTransientParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      Inc(result);
    end;
  end;

  for ParamIndex := 0 to Model.HufParameters.Count - 1 do
  begin
    AParam := Model.HufParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      Inc(result);
    end;
  end;

  if result = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoParametersHaveB,
      StrNoPestParameters)
  end;
end;

function TPestControlFileWriter.NumberOfPriorInformation: Integer;
begin
  // prior information will be added by the running ADDREG1 or a program in the
  // Groundwater Utility suite,
  result := 0;

end;

function TPestControlFileWriter.NumberOfTemplateFiles: Integer;
var
  DSIndex: Integer;
  ADataArray: TDataArray;
begin
  // PVAL file;
  result := 1;
  for DSIndex := 0 to Model.DataArrayManager.DataSetCount - 1 do
  begin
    ADataArray := Model.DataArrayManager[DSIndex];
    if ADataArray.PestParametersUsed then
    begin
      Inc(result);
    end;
  end;
end;

procedure TPestControlFileWriter.WriteAutomaticUserIntervention;
begin
// The Automatic User Intervention is not currently supported.
end;

procedure TPestControlFileWriter.WriteCommandLine;
begin
  WriteSectionHeader('model command line');
  WriteString('RunModel.bat');
  NewLine;
  NewLine;
end;

procedure TPestControlFileWriter.WriteControlSection;
var
  PestControlData: TPestControlData;
  NINSFLE: Integer;
begin
  PestControlData := Model.PestProperties.PestControlData;
  // First line 4.2.2.
  WriteSectionHeader('control data');

  {$REGION 'second line 4.2.3'}
  // second line 4.2.3
  case PestControlData.PestRestart of
    prNoRestart:
      begin
        WriteString('norestart ');
      end;
    prRestart:
      begin
        WriteString('restart ');
      end;
    else
      Assert(False);
  end;
  case PestControlData.PestMode of
    pmEstimation:
      begin
        WriteString('estimation ');
      end;
    pmPrediction:
      begin
        WriteString('prediction ');
      end;
    pmRegularisation:
      begin
        WriteString('regularisation ');
      end;
    pmPareto:
      begin
        WriteString('pareto ');
      end;
    else
      Assert(False);
  end;
  WriteString('# RSTFLE PESTMODE');
  NewLine;
  {$ENDREGION}

  {$REGION 'third line 4.2.4'}
  // third line 4.2.4
  // NPAR
  WriteInteger(NumberOfParameters);
  // NOBS
  WriteInteger(NumberOfObservations);
  // NPARGP
  WriteInteger(NumberOfParameterGroups);
  // NPRIOR
  WriteInteger(NumberOfPriorInformation);
  // NOBSGP
  WriteInteger(NumberOfObservationGroups);
  // MAXCOMPRDIM
  WriteInteger(PestControlData.MaxCompressionDimension);
  if PestControlData.MaxCompressionDimension > 1 then
  begin
    // DERZEROLIM
    WriteFloat(PestControlData.ZeroLimit);
  end;

  WriteString(' # NPAR NOBS NPARGP, NPRIOR NOBSGP, MAXCOMPRDIM');
  if PestControlData.MaxCompressionDimension > 1 then
  begin
    WriteString(', DERZEROLIM');
  end;
  NewLine;
  {$ENDREGION}

  {$REGION 'fourth line 4.2.5'}
  // NTPLFLE NINSFLE PRECIS DPOINT [NUMCOM JACFILE MESSFILE] [OBSREREF]
  // fourth line 4.2.5
  // NTPLFLE
  WriteInteger(NumberOfTemplateFiles);
  // NINSFLE
  NINSFLE := 0;
  case Model.MOdelSelection of
    msUndefined, msPhast, msFootPrint:
      Assert(False);
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp, msModflowCfp:
      begin
//        if Model.ModflowPackages.HobPackage.IsSelected then
//        begin
//          Inc(NINSFLE);
//        end;
//        if Model.ModflowPackages.ChobPackage.IsSelected then
//        begin
//          Inc(NINSFLE);
//        end;
//        if Model.ModflowPackages.DrobPackage.IsSelected then
//        begin
//          Inc(NINSFLE);
//        end;
//        if Model.ModflowPackages.GbobPackage.IsSelected then
//        begin
//          Inc(NINSFLE);
//        end;
//        if Model.ModflowPackages.RvobPackage.IsSelected then
//        begin
//          Inc(NINSFLE);
//        end;
//        if Model.ModflowPackages.StobPackage.IsSelected then
//        begin
//          Inc(NINSFLE);
//        end;
//        if FUsedObservations.Count > 0 then
//        begin
//          Inc(NINSFLE);
//        end;
        NINSFLE := 1;
      end;
    msSutra22, msSutra30:
      begin
        NINSFLE := 1;
      end;
    msModflow2015:
      begin
        NINSFLE := 1;
      end;
    else
      Assert(False);
  end;
  // PEST will always read all the simulated values from one file.
  WriteInteger(NINSFLE);
  // PRECIS
  // All data will be writen in double precision
  WriteString(' double');
  // DPOINT
  // The decimal point is never needed because free format is used exclusively.
  WriteString(' nopoint');
  // NUMCOM, JCFILE, and MESSFILE will be omited in all cases.
  // OBSREREF
  // observation rereferencing is not supported in ModelMuse.
  WriteString(' noobsreref');

  WriteString(' # NTPLFLE, NINSFLE, PRECIS, DPOINT, OBSREREF');
  NewLine;
  {$ENDREGION}

  {$REGION 'fifth line 4.2.6'}
  // Fifth line 4.2.6
  // RLAMBDA1
  WriteFloat(PestControlData.InitalLambda);
  // RLAMFAC
  WriteFloat(PestControlData.LambdaAdjustmentFactor);
  // PHIRATSUF
  WriteFloat(PestControlData.PhiRatioSufficient);
  // PHIREDLAM
  WriteFloat(PestControlData.PhiReductionLambda);
  // NUMLAM
  WriteInteger(PestControlData.NumberOfLambdas);
  // JACUPDATE
  WriteInteger(PestControlData.JacobianUpdate);
  // LAMFORGIVE
  case PestControlData.LambdaForgive of
    lfForgive:
      begin
        WriteString(' lamforgive');
      end;
    lfNoForgive:
      begin
        WriteString(' nolamforgive');
      end;
    else
      Assert(False);
  end;
  // DERFORGIVE
  case PestControlData.DerivedForgive of
    dfForgive:
      begin
        WriteString(' derforgive');
      end;
    dNoForgive:
      begin
        WriteString(' noderforgive');
      end;
    else
      Assert(False);
  end;

  WriteString(' # RLAMBDA1, RLAMFAC, PHIRATSUF, PHIREDLAM, NUMLAM, JACUPDATE, LAMFORGIVE, DERFORGIVE');
  NewLine;
  {$ENDREGION}

  {$REGION 'sixth line 4.2.7'}
  // sixth line 4.2.7
  //RELPARMAX
  WriteFloat(PestControlData.RelativeMaxParamChange);
  //FACPARMAX
  WriteFloat(PestControlData.FactorMaxParamChange);
  //FACORIG
  WriteFloat(PestControlData.FactorOriginal);
  // IBOUNDSTICK
  WriteInteger(Ord(PestControlData.BoundStick));
  // UPVECBEND
  WriteInteger(Ord(PestControlData.UpgradeParamVectorBending));
  // ABSPARMAX is not currently supported by ModelMuse.

  WriteString(' # RELPARMAX, FACPARMAX, FACORIG, IBOUNDSTICK, UPVECBEND');
  NewLine;
  {$ENDREGION}

  {$REGION 'seventh line 4.2.8'}
  // seventh line 4.2.8
  // PHIREDSWH
  WriteFloat(PestControlData.SwitchCriterion);
  // NOPTSWITCH
  WriteInteger(PestControlData.OptSwitchCount);
  // SPLITSWH
  WriteFloat(PestControlData.SplitSlopeCriterion);
  // DOAUI
  case PestControlData.AutomaticUserIntervation of
    auiInactive:
      begin
        WriteString(' noaui');
      end;
    auiActiveLeastSensitiveFirst:
      begin
        WriteString(' aui');
      end;
    auiMostSensitiveFirst:
      begin
        WriteString(' auid');
      end;
    else
      Assert(False);
  end;
  // DOSENREUSE
  case PestControlData.SensitivityReuse of
    srNoReuse:
      begin
        WriteString(' nosenreuse');
      end;
    srReuse:
      begin
        WriteString(' senreuse');
      end;
    else
      Assert(False);
  end;
  // BOUNDSCALE
  if PestControlData.Boundscaling = bsBoundsScaling then
  begin
    WriteString(' boundscale');
  end
  else
  begin
    WriteString(' noboundscale');
  end;

  WriteString(' # PHIREDSWH, NOPTSWITCH, SPLITSWH, DOAUI, DOSENREUSE, BOUNDSCALE');
  NewLine;
  {$ENDREGION}

  {$REGION 'eighth line 4.2.9'}
  // eighth line 4.2.9
  // NOPTMAX
  WriteInteger(PestControlData.MaxIterations);
  // PHIREDSTP
  WriteFloat(PestControlData.SlowConvergenceCriterion);
  // NPHISTP
  WriteInteger(PestControlData.SlowConvergenceCountCriterion);
  // NPHINORED
  WriteInteger(PestControlData.ConvergenceCountCriterion);
  // RELPARSTP
  WriteFloat(PestControlData.ParameterChangeConvergenceCriterion);
  // NRELPAR
  WriteInteger(PestControlData.ParameterChangeConvergenceCount);
  // PHISTOPTHRESH
  if PestControlData.PestMode in [pmPrediction, pmPareto] then
  begin
    WriteFloat(0);
  end
  else
  begin
    WriteFloat(PestControlData.ObjectiveCriterion);
  end;
  // LASTRUN
  WriteInteger(Ord(PestControlData.MakeFinalRun));
  // PHIABANDON
  WriteFloat(PestControlData.PhiAbandon);

  WriteString(' # NOPTMAX, PHIREDSTP, NPHISTP, NPHINORED, RELPARSTP, NRELPAR, PHISTOPTHRESH, LASTRUN, PHIABANDON');
  NewLine;
  {$ENDREGION}

  {$REGION 'nineth line 4.2.10'}
  // ICOV
  WriteInteger(Ord(PestControlData.WriteCovariance));
  // ICOR
  WriteInteger(Ord(PestControlData.WriteCorrelations));
  // IEIG
  WriteInteger(Ord(PestControlData.WriteEigenVectors));
  // IRES
  WriteInteger(Ord(PestControlData.SaveResolution));
  // JCOSAVE
  case PestControlData.SaveJacobian of
    sjDontSave:
      begin
        WriteString(' nojcosave');
      end;
    sjSave:
      begin
        WriteString(' jcosave');
      end;
    else
      Assert(False);
  end;
  // JCOSAVEITN
  case PestControlData.SaveJacobianIteration of
    sjiDontSave:
      begin
        WriteString(' nojcosaveitn');
      end;
    sjiSave:
      begin
        WriteString(' jcosaveitn');
      end;
    else
      Assert(False);
  end;
  // VERBOSEREC
  case PestControlData.VerboseRecord of
    vrNonVerbose:
      begin
        WriteString(' noverboserec');
      end;
    vrVerbose:
      begin
        WriteString(' verboserec');
      end;
    else
      Assert(False);
  end;
  // RESSAVEITN
  case PestControlData.SaveInterimResiduals of
    sirDontSave:
      begin
        WriteString(' noreisaveitn');
      end;
    sirSave:
      begin
        WriteString(' reisaveitn');
      end;
    else
      Assert(False);
  end;
  // PARSAVEITN
  case PestControlData.SaveParamIteration of
    spiDontSave:
      begin
        WriteString(' noparsaveitn');
      end;
    spiSave:
      begin
        WriteString(' parsaveitn');
      end;
    else
      Assert(False);
  end;
  // PARSAVERUN
  case PestControlData.SaveParamRun of
    sprDontSave:
      begin
        WriteString(' noparsaverun');
      end;
    sprSave:
      begin
        WriteString(' parsaverun');
      end;
    else
      Assert(False);
  end;

  WriteString(' # ICOV, ICOR, IEIG, IRES, JCOSAVE, JCOSAVEITN, VERBOSEREC, RESSAVEITN, PARSAVEITN, PARSAVERUN');
  NewLine;

  {$ENDREGION}
  NewLine;
end;

procedure TPestControlFileWriter.WriteDerivatives;
begin
// The Derivatives is not currently supported.
end;

procedure TPestControlFileWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoParametersHaveB);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoObservationGroup);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoObservationsDefi);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoParameterGroups);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrParameterGroupName);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrObservationGroupNo);


  if not Model.PestUsed then
  begin
    Exit;
  end;
//  if Model.ModelSelection <> msModflow2015 then
//  begin
//    Exit;
//  end;

  FNameOfFile := FileName(AFileName);
  OpenFile(FNameOfFile);
  try
    WriteFirstLine;
    WriteControlSection;
    // The Sensitivity Reuse Section is not currently supported.
    WriteSensitivityReuse;
    WriteSingularValueDecomposition;
    WriteLsqr;
    // The Automatic User Intervention Section is not currently supported.
    WriteAutomaticUserIntervention;
    // Writing the SVD Assist Section is not currently supported.
    WriteSVD_Assist;
    WriteParameterGroups;
    WriteParameters;
    WriteObservationGroups;
    WriteObservations;
    WriteDerivatives;
    WriteCommandLine;
    WriteModelInputOutput;
    WritePriorInformation;
    WritePredictiveAnalysis;
    WriteRegularisation;
    WritePareto;
  finally
    CloseFile;
  end;
end;

procedure TPestControlFileWriter.WriteFirstLine;
begin
  WriteString('pcf');
  NewLine;
end;

procedure TPestControlFileWriter.WriteLsqr;
var
  LsqrProperties: TLsqrProperties;
begin
  LsqrProperties := Model.PestProperties.LsqrProperties;
  WriteSectionHeader('lsqr');

  WriteInteger(Ord(LsqrProperties.Mode));
  WriteString(' # LSQRMODE');
  NewLine;

  WriteFloat(LsqrProperties.MatrixTolerance);
  WriteFloat(LsqrProperties.RightHandSideTolerance);
  WriteFloat(LsqrProperties.ConditionNumberLimit);
  if LsqrProperties.MaxIteration <> 0 then
  begin
    WriteInteger(LsqrProperties.MaxIteration);
  end
  else
  begin
    WriteInteger(NumberOfParameters * 4);
  end;
  WriteString(' # LSQR_ATOL LSQR_BTOL LSQR_CONLIM LSQR_ITNLIM');
  NewLine;

  WriteInteger(Ord(LsqrProperties.LsqrWrite));
  WriteString(' # LSQRWRITE');
  NewLine;
  NewLine;
end;

procedure TPestControlFileWriter.WriteModelInputOutput;
var
  TEMPFLE: string;
  INFLE: string;
  INSFLE: string;
  OUTFLE: string;
  DSIndex: Integer;
  ADataArray: TDataArray;
begin
  WriteSectionHeader('model input/output');
  TEMPFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPtf));
  INFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPvalExt));
  WriteString(TEMPFLE);
  WriteString(' ' + INFLE);
  NewLine;

  for DSIndex := 0 to Model.DataArrayManager.DataSetCount - 1 do
  begin
    ADataArray := Model.DataArrayManager[DSIndex];
    if ADataArray.PestParametersUsed then
    begin
      INFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
        '.' + ADataArray.Name + '.script' ));
      TEMPFLE := INFLE + '.tpl';
      WriteString(TEMPFLE);
      WriteString(' ' + INFLE);
      NewLine;
    end;
  end;

  if Model.ModelSelection in Modflow2005Selection then
  begin
    INSFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPestIns));
    OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrMf2005Values));
    WriteString(INSFLE);
    WriteString(' ' + OUTFLE);
    NewLine;
//    if Model.ModflowPackages.HobPackage.IsSelected then
//    begin
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrHobout));
//      INSFLE := OUTFLE + '.ins';
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
//    if Model.ModflowPackages.ChobPackage.IsSelected then
//    begin
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
//        TModflowCHD_Writer.ObservationOutputExtension));
//      INSFLE := OUTFLE + '.ins';
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
//    if Model.ModflowPackages.DrobPackage.IsSelected then
//    begin
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
//        TModflowDRN_Writer.ObservationOutputExtension));
//      INSFLE := OUTFLE + '.ins';
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
//    if Model.ModflowPackages.GbobPackage.IsSelected then
//    begin
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
//      TModflowGHB_Writer.ObservationOutputExtension));
//      INSFLE := OUTFLE + '.ins';
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
//    if Model.ModflowPackages.RvobPackage.IsSelected then
//    begin
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
//        TModflowRIV_Writer.ObservationOutputExtension));
//      INSFLE := OUTFLE + '.ins';
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
//    if Model.ModflowPackages.StobPackage.IsSelected then
//    begin
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
//        TStrWriter.ObservationOutputExtension));
//      INSFLE := OUTFLE + '.ins';
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
//    if FUsedObservations.Count > 0 then
//    begin
//      INSFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPestIns));
//      OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrMf6Values));
//      WriteString(INSFLE);
//      WriteString(' ' + OUTFLE);
//      NewLine;
//    end;
  end
  else
  begin
    INSFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPestIns));
    OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrMf6Values));
    WriteString(INSFLE);
    WriteString(' ' + OUTFLE);
    NewLine;
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WriteObservationGroups;
var
  ObservationGroups: TPestObservationGroups;
  ObsGrpIndex: Integer;
  ObsGroup: TPestObservationGroup;
  Mode: TPestMode;
  CorrelationFileName: string;
begin
  WriteSectionHeader('observation groups');
  ObservationGroups := Model.PestProperties.ObservationGroups;
  Mode := Model.PestProperties.PestControlData.PestMode;
  for ObsGrpIndex := 0 to ObservationGroups.Count - 1 do
  begin
    ObsGroup := ObservationGroups[ObsGrpIndex];
    WriteString(ObsGroup.ObsGroupName);
    if Mode = pmRegularisation then
    begin
      if ObsGroup.UseGroupTarget then
      begin
        WriteFloat(ObsGroup.GroupTarget);
      end;
    end;
    if ObsGroup. AbsoluteCorrelationFileName <> '' then
    begin
      CorrelationFileName := ' ' + ExtractRelativePath(FNameOfFile, ObsGroup.AbsoluteCorrelationFileName);
      WriteString(CorrelationFileName);
    end;
    NewLine;
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WriteObservations;
var
  ObsIndex: Integer;
  AnObs: IObservationItem;
  ObsItem: TCustomObservationItem;
begin
  WriteSectionHeader('observation data');
  for ObsIndex := 0 to FUsedObservations.Count - 1 do
  begin
    AnObs := FUsedObservations[ObsIndex];
    if AnObs is TCustomObservationItem then
    begin
      ObsItem := TCustomObservationItem(AnObs);
      if ObsItem.ExportedName <> '' then
      begin
        WriteString(ObsItem.ExportedName);
      end
      else
      begin
        WriteString(AnObs.Name);
      end;
    end
    else
    begin
      WriteString(AnObs.Name);
    end;
    WriteFloat(AnObs.ObservedValue);
    WriteFloat(AnObs.Weight);
    WriteString(' ' + AnObs.ObservationGroup);
    if AnObs.ObservationGroup = '' then
    begin
      frmErrorsAndWarnings.AddError(Model, StrObservationGroupNo,
        Format(StrNoObservationGroupAssigned, [AnObs.Name]));
    end;
    NewLine;
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WriteParameterGroups;
var
  GroupIndex: Integer;
  AGroup: TPestParamGroup;
begin
  WriteSectionHeader('parameter groups');
  for GroupIndex := 0 to Model.ParamGroups.Count - 1 do
  begin
    AGroup := Model.ParamGroups[GroupIndex];

   // PARGPNME
    WriteString(AGroup.ParamGroupName);

    // INCTYP
    case AGroup.IncrementType of
      icRelative:
        begin
          WriteString(' relative');
        end;
      icAbsolute:
        begin
          WriteString(' absolute');
        end;
      icRelativeToMax:
        begin
          WriteString(' rel_to_max');
        end;
      else Assert(False);
    end;

    // DERINC
    WriteFloat(AGroup.ParamIncrement);

    // DERINCLB
    WriteFloat(AGroup.MinParamIncrement);

    // FORCEN
    case AGroup.ForceCentral of
      fcAlways2:
        begin
          WriteString(' always_2');
        end;
      fcAlways3:
        begin
          WriteString(' always_3');
        end;
      fcAlways5:
        begin
          WriteString(' always_5');
        end;
      fcSwitch:
        begin
          WriteString(' switch');
        end;
      fcSwitch5:
        begin
          WriteString(' switch_5');
        end;
      else Assert(False);
    end;

    // DERINCMUL
    WriteFloat(AGroup.ParamIncrementMultiplier);

    // DERMTHD
    case AGroup.ForceCentral of
      fcAlways2, fcAlways3, fcSwitch:
        begin
          case AGroup.DM3 of
            dm3Parabolic:
              begin
                WriteString(' parabolic');
              end;
            dm3BestFit:
              begin
                WriteString(' best_fit');
              end;
            dm3OutsidePoints:
              begin
                WriteString(' outside_pts');
              end;
            else Assert(False);
          end;
        end;
      fcAlways5, fcSwitch5:
        begin
          case AGroup.DM5 of
            dm5MinimumVariance:
              begin
                WriteString(' minvar');
              end;
            dm5MaxPrecision:
              begin
                WriteString(' maxprec');
              end;
            else Assert(False);
          end;
        end;
      else
        Assert(False);
    end;

    if AGroup.UseSplitSlopeAnalysis then
    begin
      // SPLITTHRESH
      WriteFloat(AGroup.SplitThreshold);

      // SPLITRELDIFF
      WriteFloat(AGroup.RelSlopeDif);

      // SPLITACTION
      case AGroup.SplitAction of
        saSmaller:
          begin
            WriteString(' smaller');
          end;
        saZero:
          begin
            WriteString(' zero');
          end;
        saPrevious:
          begin
            WriteString(' previous');
          end;
        else Assert(False);
      end;
    end;
    NewLine;
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WriteParameters;
var
  UsedTypes: TParameterTypes;
  ParamIndex: Integer;
  AParam: TModflowParameter;
  procedure WriteParameter(AParam: TModflowParameter);
  begin
    //PARNME
    WriteString(AParam.ParameterName);

    //PARTRANS
    case AParam.Transform of
      ptNoTransform:
        begin
          WriteString(' none');
        end;
      ptLog:
        begin
          WriteString(' log');
        end;
      ptFixed:
        begin
          WriteString(' fixed');
        end;
      ptTied:
        begin
          WriteString(' tied');
        end;
      else Assert(False);
    end;

    //PARCHGLIM
    case AParam.ChangeLimitation of
      pclRelative:
        begin
          WriteString(' relative');
        end;
      pclFactor:
        begin
          WriteString(' factor');
        end;
      pclAbsolute:
        begin
          WriteString(' absolute(N)');
        end;
      else
        Assert(False);
    end;

    //PARVAL1
    WriteFloat(AParam.Value);

    //PARLBND
    WriteFloat(AParam.LowerBound);

    //PARUBND
    WriteFloat(AParam.UpperBound);

    //PARGP
    if AParam.ParameterGroup = '' then
    begin
      WriteString(' none');
      frmErrorsAndWarnings.AddError(Model, StrParameterGroupName,
        Format(StrTheParameterSH, [AParam.ParameterName]));
    end
    else
    begin
      WriteString(' ' + AParam.ParameterGroup);
    end;

    //SCALE
    WriteFloat(AParam.Scale);

    //OFFSET
    WriteFloat(AParam.Offset);

    //DERCOM
    // write only in NUMCOM is written in line 4 of the control data section
//    WriteInteger(1);

    NewLine;
  end;
  procedure WriteTiedParameter(AParam: TModflowParameter);
  begin
    if AParam.Transform = ptTied then
    begin
      // PARNME
      WriteString(AParam.ParameterName);

      // PARTIED
      WriteString(' ' + AParam.TiedParameterName);
      NewLine;
    end;
  end;
begin
  UsedTypes := [];
  if Model.ModelSelection = msModflow2015 then
  begin
    UsedTypes := Mf15ParamType;
  end
  else if Model.ModelSelection in Modflow2005Selection then
  begin
    UsedTypes := Mf2005ParamType;
  end
  else
  begin
    Assert(False);
  end;

  WriteSectionHeader('parameter data');

  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    AParam := Model.ModflowSteadyParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      WriteParameter(AParam);
    end;
  end;

  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam := Model.ModflowTransientParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      WriteParameter(AParam);
    end;
  end;

  for ParamIndex := 0 to Model.HufParameters.Count - 1 do
  begin
    AParam := Model.HufParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      WriteParameter(AParam);
    end;
  end;

  NewLine;
end;

procedure TPestControlFileWriter.WritePareto;
begin
// The Pareto section is not currently supported.
end;

procedure TPestControlFileWriter.WritePredictiveAnalysis;
begin
// The Predictive Analysis section is not currently supported.
end;

procedure TPestControlFileWriter.WritePriorInformation;
begin
  // prior information will be added by the running ADDREG1 or a program in the
  // Groundwater Utility suite,
end;

procedure TPestControlFileWriter.WriteRegularisation;
begin
// The Regularisation section is not currently supported.
end;

procedure TPestControlFileWriter.WriteSectionHeader(const SectionID: String);
begin
  WriteString('* ');
  WriteString(SectionID);
  NewLine;
end;

procedure TPestControlFileWriter.WriteSensitivityReuse;
begin
// The sensitivity reuse section is not currently supported.
end;

procedure TPestControlFileWriter.WriteSingularValueDecomposition;
var
  SvdProperties: TSingularValueDecompositionProperties;
begin
  SvdProperties := Model.PestProperties.SvdProperties;
  WriteSectionHeader('singular value decomposition');

  WriteInteger(Ord(SvdProperties.Mode));
  WriteString(' # SVDMODE');
  NewLine;

  WriteInteger(SvdProperties.MaxSingularValues);
  WriteFloat(SvdProperties.EigenThreshold);
  WriteString(' # MAXSING, EIGTHRESH');
  NewLine;

  WriteInteger(Ord(SvdProperties.EigenWrite));
  WriteString(' # EIGWRITE');
  NewLine;
  NewLine;
end;

procedure TPestControlFileWriter.WriteSVD_Assist;
begin
// Writing the SVD Assist section is not currently supported.
end;

end.
