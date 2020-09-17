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

procedure TPestControlFileWriter.WriteControlSection;
var
  PestControlData: TPestControlData;
begin
  // First line 4.2.2.
  WriteSectionHeader('control data');

  {$REGION 'second line 4.2.3'}
  // second line 4.2.3
  PestControlData := Model.PestProperties.PestControlData;
  case PestControlData.PestRestart of
    prRestart:
      begin
        WriteString('restart ');
      end;
    prNoRestart:
      begin
        WriteString('norestart ');
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
  // UPVECBEND
  WriteInteger(Ord(PestControlData.UpgradeParamVectorBending));
  // ABSPARMAX is not currently supported by ModelMuse.

  WriteString(' # RELPARMAX, FACPARMAX, FACORIG, UPVECBEND');
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
    auiActive:
      begin
        WriteString(' aui');
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
  if PestControlData. Boundscaling then
  begin
    WriteString(' boundscale');
  end;

  WriteString(' # PHIREDSWH, NOPTSWITCH, SPLITSWH, DOAUI, DOSENREUSE');
  if PestControlData.Boundscaling then
  begin
    WriteString(', BOUNDSCALE');
  end;
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
  WriteFloat(PestControlData.ObjectiveCriterion);
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

  WriteString(' # ICOV, ICOR, IEIG');
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
  finally
    CloseFile;
  end;
end;

procedure TPestControlFileWriter.WriteFirstLine;
begin
  WriteString('pcf');
  NewLine;
end;

procedure TPestControlFileWriter.WriteSectionHeader(const SectionID: String);
begin
  WriteString('* ');
  WriteString(SectionID);
  NewLine;
end;

end.
