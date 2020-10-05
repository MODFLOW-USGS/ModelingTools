unit PestControlFileWriterUnit;

interface

uses
  CustomModflowWriterUnit;

type
  TPestControlFileWriter = class(TCustomFileWriter)
  private
    FNameOfFile: string;
    procedure WriteFirstLine;
    procedure WriteSectionHeader(const SectionID: String);
    procedure WriteControlSection;
    procedure WriteSensitivityReuse;
    procedure WriteSingularValueDecomposition;
    procedure WriteLsqr;
    procedure WriteAutomaticUserIntervention;
    procedure WriteSVD_Assist;
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
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  PestPropertiesUnit;

{ TPestControlFileWriter }

class function TPestControlFileWriter.Extension: string;
begin
  result := '.pst';
end;

function TPestControlFileWriter.NumberOfObservationGroups: Integer;
begin

end;

function TPestControlFileWriter.NumberOfObservations: integer;
begin

end;

function TPestControlFileWriter.NumberOfParameterGroups: Integer;
begin

end;

function TPestControlFileWriter.NumberOfParameters: Integer;
begin

end;

function TPestControlFileWriter.NumberOfPriorInformation: Integer;
begin

end;

procedure TPestControlFileWriter.WriteAutomaticUserIntervention;
begin
// The Automatic User Intervention is not currently supported.
end;

procedure TPestControlFileWriter.WriteControlSection;
var
  PestControlData: TPestControlData;
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
  // fourth line 4.2.5
  // NTPLFLE
  // The pval file will always be the only file PEST writes.
  WriteInteger(1);
  // NINSFLE
  // PEST will always read all the simulated values from one file.
  WriteInteger(1);
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

procedure TPestControlFileWriter.WriteFile(const AFileName: string);
begin
  if not Model.PestUsed then
  begin
    Exit;
  end;

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
