unit PestPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes, GR32, FastGEO, PestObsGroupUnit,
  PointCollectionUnit;

type
  TPestRestart = (prNoRestart, prRestart);
  TPestMode = (pmEstimation, pmPrediction, pmRegularisation, pmPareto);
  TLambdaForgive = (lfNoForgive, lfForgive);
  TDerivedForgive = (dNoForgive, dfForgive);
  TUpgradeParamVectorBending = (upvbNoBending, upvbBending);
  TAutomaticUserIntervation = (auiInactive, auiActiveLeastSensitiveFirst,
    auiMostSensitiveFirst);
  TSensitivityReuse = (srNoReuse, srReuse);
  TBoundsScaling = (bsNoBoundsScaling, bsBoundsScaling);
  TMakeFinalRun = (mfrNoRun, mfrRun);
  TWriteMatrix = (wmDontWrite, wmWrite);
  TSaveResolution = (srDontSave, srSave);
  TSaveJacobian = (sjDontSave, sjSave);
  TSaveJacobianIteration = (sjiDontSave, sjiSave);
  TVerboseRecord = (vrNonVerbose, vrVerbose);
  TSaveInterimResiduals = (sirDontSave, sirSave);
  TSaveParamIteration = (spiDontSave, spiSave);
  TSaveParamRun = (sprDontSave, sprSave);

  TPestControlData = class(TGoPhastPersistent)
  private
    FPestMode: TPestMode;
    FPestRestart: TPestRestart;
    FMaxCompressionDimension: Integer;
    FStoredZeroLimit: TRealStorage;
    FStoredInitalLambda: TRealStorage;
    FStoredLambdaAdjustmentFactor: TRealStorage;
    FStoredPhiRatioSufficient: TRealStorage;
    FStoredPhiReductionLambda: TRealStorage;
    FNumberOfLambdas: Integer;
    FJacobianUpdate: Integer;
    FLambdaForgive: TLambdaForgive;
    FDerivedForgive: TDerivedForgive;
    FStoredRelativeMaxParamChange: TRealStorage;
    FStoredFactorMaxParamChange: TRealStorage;
    FStoredFactorOriginal: TRealStorage;
    FBoundStick: Integer;
    FUpgradeParamVectorBending: TUpgradeParamVectorBending;
    FStoredSwitchCriterion: TRealStorage;
    FOptSwitchCount: Integer;
    FStoredSplitSlopeCriterion: TRealStorage;
    FAutomaticUserIntervation: TAutomaticUserIntervation;
    FSensitivityReuse: TSensitivityReuse;
    FBoundscaling: TBoundsScaling;
    FMaxIterations: Integer;
    FStoredSlowConvergenceCriterion: TRealStorage;
    FSlowConvergenceCountCriterion: integer;
    FConvergenceCountCriterion: Integer;
    FStoredParameterChangeConvergenceCriterion: TRealStorage;
    FParameterChangeConvergenceCount: Integer;
    FStoredObjectiveCriterion: TRealStorage;
    FMakeFinalRun: TMakeFinalRun;
    FStoredPhiAbandon: TRealStorage;
    FWriteCovariance: TWriteMatrix;
    FWriteCorrelations: TWriteMatrix;
    FWriteEigenVectors: TWriteMatrix;
    FSaveResolution: TSaveResolution;
    FSaveJacobian: TSaveJacobian;
    FSaveJacobianIteration: TSaveJacobianIteration;
    FVerboseRecord: TVerboseRecord;
    FSaveInterimResiduals: TSaveInterimResiduals;
    FSaveParamIteration: TSaveParamIteration;
    FSaveParamRun: TSaveParamRun;
    procedure SetPestMode(const Value: TPestMode);
    procedure SetPestRestart(const Value: TPestRestart);
    procedure SetMaxCompressionDimension(const Value: Integer);
    function GetZeroLimit: double;
    procedure SetStoredZeroLimit(const Value: TRealStorage);
    procedure SetZeroLimit(const Value: double);
    function GetInitalLambda: double;
    procedure SetInitalLambda(const Value: double);
    procedure SetStoredInitalLambda(const Value: TRealStorage);
    function GetLambdaAdjustmentFactor: double;
    procedure SetLambdaAdjustmentFactor(const Value: double);
    procedure SetStoredLambdaAdjustmentFactor(const Value: TRealStorage);
    function GetPhiRatioSufficient: double;
    procedure SetPhiRatioSufficient(const Value: double);
    procedure SetStoredPhiRatioSufficient(const Value: TRealStorage);
    function GetPhiReductionLambda: double;
    procedure SetPhiReductionLambda(const Value: double);
    procedure SetStoredPhiReductionLambda(const Value: TRealStorage);
    procedure SetNumberOfLambdas(const Value: Integer);
    procedure SetJacobianUpdate(const Value: Integer);
    procedure SetLambdaForgive(const Value: TLambdaForgive);
    procedure SetDerivedForgive(const Value: TDerivedForgive);
    function GetRelativeMaxParamChange: double;
    procedure SetRelativeMaxParamChange(const Value: double);
    procedure SetStoredRelativeMaxParamChange(const Value: TRealStorage);
    function GetFactorMaxParamChange: double;
    procedure SetFactorMaxParamChange(const Value: double);
    procedure SetStoredFactorMaxParamChange(const Value: TRealStorage);
    function GetFactorOriginal: double;
    procedure SetFactorOriginal(const Value: double);
    procedure SetStoredFactorOriginal(const Value: TRealStorage);
    procedure SetBoundStick(const Value: Integer);
    procedure SetUpgradeParamVectorBending(
      const Value: TUpgradeParamVectorBending);
    function GetSwitchCriterion: double;
    procedure SetStoredSwitchCriterion(const Value: TRealStorage);
    procedure SetSwitchCriterion(const Value: double);
    procedure SetOptSwitchCount(const Value: Integer);
    function GetSplitSlopeCriterion: double;
    procedure SetSplitSlopeCriterion(const Value: double);
    procedure SetStoredSplitSlopeCriterion(const Value: TRealStorage);
    procedure SetAutomaticUserIntervation(
      const Value: TAutomaticUserIntervation);
    procedure SetSensitivityReuse(const Value: TSensitivityReuse);
    procedure SetBoundscaling(const Value: TBoundsScaling);
    procedure SetMaxIterations(const Value: Integer);
    function GetSlowConvergenceCriterion: double;
    procedure SetSlowConvergenceCriterion(const Value: double);
    procedure SetStoredSlowConvergenceCriterion(const Value: TRealStorage);
    procedure SetSlowConvergenceCountCriterion(const Value: integer);
    procedure SetConvergenceCountCriterion(const Value: Integer);
    procedure SetStoredParameterChangeConvergenceCriterion(const Value: TRealStorage);
    function GetParameterChangeConvergenceCriterion: double;
    procedure SetParameterChangeConvergenceCriterion(const Value: double);
    procedure SetParameterChangeConvergenceCount(const Value: Integer);
    function GetObjectiveCriterion: double;
    procedure SetObjectiveCriterion(const Value: double);
    procedure SetStoredObjectiveCriterion(const Value: TRealStorage);
    procedure SetMakeFinalRun(const Value: TMakeFinalRun);
    function GetPhiAbandon: double;
    procedure SetPhiAbandon(const Value: double);
    procedure SetStoredPhiAbandon(const Value: TRealStorage);
    procedure SetWriteCovariance(const Value: TWriteMatrix);
    procedure SetWriteCorrelations(const Value: TWriteMatrix);
    procedure SetWriteEigenVectors(const Value: TWriteMatrix);
    procedure SetSaveResolution(const Value: TSaveResolution);
    procedure SetSaveJacobian(const Value: TSaveJacobian);
    procedure SetSaveJacobianIteration(const Value: TSaveJacobianIteration);
    procedure SetVerboseRecord(const Value: TVerboseRecord);
    procedure SetSaveInterimResiduals(const Value: TSaveInterimResiduals);
    procedure SetSaveParamIteration(const Value: TSaveParamIteration);
    procedure SetSaveParamRun(const Value: TSaveParamRun);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    // DERZEROLIM
    // minimum value = 0
    property ZeroLimit: double read GetZeroLimit write SetZeroLimit;
    // RLAMBDA1 >= 0
    // must be positive
    property InitalLambda: double read GetInitalLambda
      write SetInitalLambda;
    // RLAMFAC
    // must be greater than 1 or less than -1.
    property LambdaAdjustmentFactor: double read GetLambdaAdjustmentFactor
      write SetLambdaAdjustmentFactor;
    // PHIRATSUF
    // should be > 0 and < 1.
    property PhiRatioSufficient: double read GetPhiRatioSufficient
      write SetPhiRatioSufficient;
    // PHIREDLAM  0 to 1
    property PhiReductionLambda: double read GetPhiReductionLambda
      write SetPhiReductionLambda;
    // RELPARMAX > 0
    property RelativeMaxParamChange: double read GetRelativeMaxParamChange
      write SetRelativeMaxParamChange;
    // FACPARMAX > 1
    property FactorMaxParamChange: double read GetFactorMaxParamChange
      write SetFactorMaxParamChange;
    // FACORIG
    // must be 0 to 1
    property FactorOriginal: double read GetFactorOriginal
      write SetFactorOriginal;
    // PHIREDSWH  0 to 1, must be > 0
    property SwitchCriterion: double read GetSwitchCriterion
      write SetSwitchCriterion;
    // SPLITSWH >= 0
    property SplitSlopeCriterion: double read GetSplitSlopeCriterion
      write SetSplitSlopeCriterion;
    // PHIREDSTP > 0
    property SlowConvergenceCriterion: double read GetSlowConvergenceCriterion
      write SetSlowConvergenceCriterion;
    // RELPARSTP > 0
    property ParameterChangeConvergenceCriterion: double
      read GetParameterChangeConvergenceCriterion
      write SetParameterChangeConvergenceCriterion;
    // PHISTOPTHRESH >= 0
    property ObjectiveCriterion: double read GetObjectiveCriterion
      write SetObjectiveCriterion;
    // PHIABANDON
    property PhiAbandon: double read GetPhiAbandon write SetPhiAbandon;
  published
    // RSTFLE
    property PestRestart: TPestRestart read FPestRestart write SetPestRestart;
    // PESTMODE
    property PestMode: TPestMode read FPestMode write SetPestMode;
    //MAXCOMPRDIM must be >= 0
    property MaxCompressionDimension: Integer read FMaxCompressionDimension
      write SetMaxCompressionDimension;
    // DERZEROLIM
    property StoredZeroLimit: TRealStorage read FStoredZeroLimit
      write SetStoredZeroLimit;
    // RLAMBDA1 must be >= 0
    property StoredInitalLambda: TRealStorage read FStoredInitalLambda
      write SetStoredInitalLambda;
    // RLAMFAC must not be zero but can be positive or negative
    property StoredLambdaAdjustmentFactor: TRealStorage
      read FStoredLambdaAdjustmentFactor write SetStoredLambdaAdjustmentFactor;
    // PHIRATSUF 0 to 1
    property StoredPhiRatioSufficient: TRealStorage
      read FStoredPhiRatioSufficient write SetStoredPhiRatioSufficient;
    // PHIREDLAM 0 to 1
    property StoredPhiReductionLambda: TRealStorage
      read FStoredPhiReductionLambda write SetStoredPhiReductionLambda;
    // NUMLAM 1 or more (negative allowed with BEOPEST)
    property NumberOfLambdas: Integer read FNumberOfLambdas
      write SetNumberOfLambdas;
    // JACUPDATE  >= 0
    property JacobianUpdate: Integer read FJacobianUpdate
      write SetJacobianUpdate;
    // LAMFORGIVE
    property LambdaForgive: TLambdaForgive read FLambdaForgive
      write SetLambdaForgive;
    // DERFORGIVE
    property DerivedForgive: TDerivedForgive read FDerivedForgive
      write SetDerivedForgive;
    // RELPARMAX > 0
    property StoredRelativeMaxParamChange: TRealStorage
      read FStoredRelativeMaxParamChange write SetStoredRelativeMaxParamChange;
    // FACPARMAX  > 1
    property StoredFactorMaxParamChange: TRealStorage
      read FStoredFactorMaxParamChange write SetStoredFactorMaxParamChange;
    // FACORIG  0 to 1
    property StoredFactorOriginal: TRealStorage
      read FStoredFactorOriginal write SetStoredFactorOriginal;
    // IBOUNDSTICK  >= 0
    property BoundStick: Integer read FBoundStick write SetBoundStick;
    // UPVECBEND
    property UpgradeParamVectorBending: TUpgradeParamVectorBending
      read FUpgradeParamVectorBending write SetUpgradeParamVectorBending;
    // PHIREDSWH 0 to 1
    property StoredSwitchCriterion: TRealStorage read FStoredSwitchCriterion
      write SetStoredSwitchCriterion;
    // NOPTSWITCH 1 or more
    property OptSwitchCount: Integer read FOptSwitchCount
      write SetOptSwitchCount;
    // SPLITSWH  >= 0
    property StoredSplitSlopeCriterion: TRealStorage
      read FStoredSplitSlopeCriterion write SetStoredSplitSlopeCriterion;
    // DOAUI
    property AutomaticUserIntervation: TAutomaticUserIntervation
      read FAutomaticUserIntervation write SetAutomaticUserIntervation;
    // DOSENREUSE
    property SensitivityReuse: TSensitivityReuse read FSensitivityReuse
      write SetSensitivityReuse;
    // BOUNDSCALE
    property Boundscaling: TBoundsScaling read FBoundscaling write SetBoundscaling
      stored True;
    // NOPTMAX >= -2
    property MaxIterations: Integer read FMaxIterations write SetMaxIterations;
    // PHIREDSTP > 0
    property StoredSlowConvergenceCriterion: TRealStorage
      read FStoredSlowConvergenceCriterion
      write SetStoredSlowConvergenceCriterion;
    // NPHISTP  > 0
    property SlowConvergenceCountCriterion: integer
      read FSlowConvergenceCountCriterion
      write SetSlowConvergenceCountCriterion;
    // NPHINORED  > 0
    property ConvergenceCountCriterion: Integer read FConvergenceCountCriterion
      write SetConvergenceCountCriterion;
    // RELPARSTP > 0
    property StoredParameterChangeConvergenceCriterion: TRealStorage
      read FStoredParameterChangeConvergenceCriterion
      write SetStoredParameterChangeConvergenceCriterion;
    // NRELPAR  > 0
    property ParameterChangeConvergenceCount: Integer
      read FParameterChangeConvergenceCount
      write SetParameterChangeConvergenceCount;
    // PHISTOPTHRESH  >= 0
    property StoredObjectiveCriterion: TRealStorage
      read FStoredObjectiveCriterion write SetStoredObjectiveCriterion;
    // LASTRUN
    property MakeFinalRun: TMakeFinalRun read FMakeFinalRun
      write SetMakeFinalRun;
    // PHIABANDON  positive number
    property StoredPhiAbandon: TRealStorage read FStoredPhiAbandon
      write SetStoredPhiAbandon;
    // ICOV
    property WriteCovariance: TWriteMatrix read FWriteCovariance
      write SetWriteCovariance;
    // ICOR
    property WriteCorrelations: TWriteMatrix read FWriteCorrelations
      write SetWriteCorrelations;
    // IEIG
    property WriteEigenVectors: TWriteMatrix read FWriteEigenVectors
      write SetWriteEigenVectors;
    // IRES
    property SaveResolution: TSaveResolution read FSaveResolution
      write SetSaveResolution;
    // JCOSAVE
    property SaveJacobian: TSaveJacobian read FSaveJacobian
      write SetSaveJacobian;
    // JCOSAVEITN
    property SaveJacobianIteration: TSaveJacobianIteration
      read FSaveJacobianIteration write SetSaveJacobianIteration;
    // VERBOSEREC
    property VerboseRecord: TVerboseRecord read FVerboseRecord
      write SetVerboseRecord;
    // RESSAVEITN
    property SaveInterimResiduals: TSaveInterimResiduals
      read FSaveInterimResiduals write SetSaveInterimResiduals;
    // PARSAVEITN
    property SaveParamIteration: TSaveParamIteration read FSaveParamIteration
      write SetSaveParamIteration;
    // PARSAVERUN
    property SaveParamRun: TSaveParamRun read FSaveParamRun
      write SetSaveParamRun;
{
  TSaveParamIteration = (spiDontSave, spiSave);
  TSaveParamRun = (sprDontSave, sprSave);
}
  end;

  TSvdMode = (smNone, smNormal, smDamped);
  TEigenWrite = (ewReduced, ewFull);

  TSingularValueDecompositionProperties = class(TGoPhastPersistent)
  private
    FMode: TSvdMode;
    FStoredEigenThreshold: TRealStorage;
    FEigenWrite: TEigenWrite;
    FMaxSingularValues: Integer;
    function GetEigenThreshold: Double;
    procedure SetEigenThreshold(const Value: Double);
    procedure SetEigenWrite(const Value: TEigenWrite);
    procedure SetMaxSingularValues(const Value: Integer);
    procedure SetMode(const Value: TSvdMode);
    procedure SetStoredEigenThreshold(const Value: TRealStorage);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    property EigenThreshold: Double read GetEigenThreshold
      write SetEigenThreshold;
  published
    // SVDMODE
    property Mode: TSvdMode read FMode write SetMode;
    // MAXSING
    property MaxSingularValues: Integer read FMaxSingularValues write SetMaxSingularValues;
    // EIGTHRESH
    property StoredEigenThreshold: TRealStorage read FStoredEigenThreshold write SetStoredEigenThreshold;
    // EIGWRITE
    property EigenWrite: TEigenWrite read FEigenWrite write SetEigenWrite;
  end;

  TLsqrMode = (lmDeactivate, lmActivate);
  TLsqrWrite = (lwDontWrite, lwWrite);

  TLsqrProperties = class(TGoPhastPersistent)
  private
    FMode: TLsqrMode;
    FStoredRightHandSideTolerance: TRealStorage;
    FLsqrWrite: TLsqrWrite;
    FStoredConditionNumberLimit: TRealStorage;
    FStoredMatrixTolerance: TRealStorage;
    FMaxIteration: Integer;
    function GetConditionNumberLimit: double;
    function GetMatrixTolerance: double;
    function GetRightHandSideTolerance: double;
    procedure SetConditionNumberLimit(const Value: double);
    procedure SetLsqrWrite(const Value: TLsqrWrite);
    procedure SetMatrixTolerance(const Value: double);
    procedure SetMaxIteration(const Value: Integer);
    procedure SetMode(const Value: TLsqrMode);
    procedure SetRightHandSideTolerance(const Value: double);
    procedure SetStoredConditionNumberLimit(const Value: TRealStorage);
    procedure SetStoredMatrixTolerance(const Value: TRealStorage);
    procedure SetStoredRightHandSideTolerance(const Value: TRealStorage);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    property MatrixTolerance: double read GetMatrixTolerance
      write SetMatrixTolerance;
    property RightHandSideTolerance: double read GetRightHandSideTolerance
      write SetRightHandSideTolerance;
    property ConditionNumberLimit: double read GetConditionNumberLimit
      write SetConditionNumberLimit;
  published
    // LSQRMODE
    property Mode: TLsqrMode read FMode write SetMode;
    // LSQR_ATOL
    property StoredMatrixTolerance: TRealStorage read FStoredMatrixTolerance write SetStoredMatrixTolerance;
    // LSQR_BTOL
    property StoredRightHandSideTolerance: TRealStorage read FStoredRightHandSideTolerance write SetStoredRightHandSideTolerance;
    // LSQR_CONLIM
    property StoredConditionNumberLimit: TRealStorage read FStoredConditionNumberLimit write SetStoredConditionNumberLimit;
    // LSQR_ITNLIM
    // if set as zero, use 4 times the number of adjustable parameters.
    property MaxIteration: Integer read FMaxIteration write SetMaxIteration;
    // LSQRWRITE
    property LsqrWrite: TLsqrWrite read FLsqrWrite write SetLsqrWrite;
  end;

  TMemSave = (msNoMemSave, msMemSave);
  TLinRegression = (lfNoLinReg, lfLinReg);
  TRegContinue = (rcNoContinue, rcContinue);

  // See PEST manual chapter 9.
  TPestRegularization = class(TGoPhastPersistent)
  private
    FStoredFracPhiM: TRealStorage;
    FStoredWeightFactorMaximum: TRealStorage;
    FMemSave: TMemSave;
    FLinearRegression: TLinRegression;
    FAutoPhiMAccept: Boolean;
    FStoredPhiMAccept: TRealStorage;
    FStoredRegularizationSingularValueThreshhold: TRealStorage;
    FStoredWeightFactorMinimum: TRealStorage;
    FRegContinue: TRegContinue;
    FStoredWFFac: TRealStorage;
    FOptimizationInterval: Integer;
    FStoredRegWeightRatio: TRealStorage;
    FStoredWFInit: TRealStorage;
    FRegularizationOption: Integer;
    FStoredWeightFactorTolerance: TRealStorage;
    FStoredPhiMLim: TRealStorage;
    procedure SetAutoPhiMAccept(const Value: Boolean);
    procedure SetLinearRegression(const Value: TLinRegression);
    procedure SetMemSave(const Value: TMemSave);
    procedure SetOptimizationInterval(const Value: Integer);
    procedure SetRegContinue(const Value: TRegContinue);
    procedure SetRegularizationOption(const Value: Integer);
    procedure SetStoredRegularizationSingularValueThreshhold(
      const Value: TRealStorage);
    procedure SetStoredFracPhiM(const Value: TRealStorage);
    procedure SetStoredPhiMAccept(const Value: TRealStorage);
    procedure SetStoredPhiMLim(const Value: TRealStorage);
    procedure SetStoredRegWeightRatio(const Value: TRealStorage);
    procedure SetStoredWeightFactorMaximum(const Value: TRealStorage);
    procedure SetStoredWeightFactorMinimum(const Value: TRealStorage);
    procedure SetStoredWFFac(const Value: TRealStorage);
    procedure SetStoredWFInit(const Value: TRealStorage);
    procedure SetStoredWeightFactorTolerance(const Value: TRealStorage);
    function GetFracPhiM: double;
    function GetPhiMAccept: double;
    function GetPhiMLim: double;
    function GetRegularizationSingularValueThreshhold: double;
    function GetRegWeightRatio: double;
    function GetWeightFactorMaximum: double;
    function GetWeightFactorMinimum: double;
    function GetWeightFactorTolerance: double;
    function GetWFFac: double;
    function GetWFInit: double;
    procedure SetFracPhiM(const Value: double);
    procedure SetPhiMAccept(const Value: double);
    procedure SetPhiMLim(const Value: double);
    procedure SetRegularizationSingularValueThreshhold(const Value: double);
    procedure SetRegWeightRatio(const Value: double);
    procedure SetWeightFactorMaximum(const Value: double);
    procedure SetWeightFactorMinimum(const Value: double);
    procedure SetWeightFactorTolerance(const Value: double);
    procedure SetWFFac(const Value: double);
    procedure SetWFInit(const Value: double);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    // PHIMLIM, Initial value 1E-10
    // target measurement objective function
    // greater than zero
    property PhiMLim: double read GetPhiMLim write SetPhiMLim;
    // PHIMACCEPT, Should be 5 to 10% greater than PHIMLIM
    // acceptable measurement objective function
    // greater than PHIMLIM
    property PhiMAccept: double read GetPhiMAccept write SetPhiMAccept;
    // FRACPHIM, Initial value 0.1.
    // FRACPHIM is optional
    // set target measurement objective function at this fraction
    // of current measurement objective function
    // zero or greater, but less than one
    property FracPhiM: double read GetFracPhiM write SetFracPhiM;
    // WFINIT Initial regularization weight factor.
    // If you have no idea of what the weight factor should be,
    // simply set it to 1.0.
    // greater than zero
    property WFInit: double read GetWFInit write SetWFInit;
    // WFMIN, minimum regularization weight factor.
    // Normally settings of 1E-10 is suitable
    // greater than zero
    property WeightFactorMinimum: double read GetWeightFactorMinimum write SetWeightFactorMinimum;
    // WFMAX
    // Normally settings of 1E10 is suitable
    // maximum regularization weight factor
    // greater than WFMAX
    property WeightFactorMaximum: double read GetWeightFactorMaximum write SetWeightFactorMaximum;
    // WFFAC Weight Factor adjustment factor. Initiall value 1.3
    // must be greater than 1.
    // regularization weight factor adjustment factor
    // greater than one
    property WFFac: double read GetWFFac write SetWFFac;
    // WFTOL Weight Factor tolerance Normally between 1E-3 and 1E-2 but can be
    // greater than 1E-2.
    // convergence criterion for regularization weight factor
    // greater than zero
    property WeightFactorTolerance: double read GetWeightFactorTolerance write SetWeightFactorTolerance;
    // REGWEIGHTRAT required if IREGADJ = 4
    // the ratio of highest to lowest regularization weight; spread is logarithmic with null space projection if set negative
    // absolute value of one or greater
    property RegWeightRatio: double read GetRegWeightRatio write SetRegWeightRatio;
    // REGSINGTHRESH required if IREGADJ = 5
    // singular value of JtQJ (as factor of highest singular value) at which use of higher regularization weights commences if IREGADJ is set to 5
    // less than one and greater than zero
    property RegularizationSingularValueThreshhold: double read GetRegularizationSingularValueThreshhold write SetRegularizationSingularValueThreshhold;
  published
    // PHIMLIM, Initial value 1E-10
    // target measurement objective function
    // greater than zero
    property StoredPhiMLim: TRealStorage read FStoredPhiMLim write SetStoredPhiMLim;
    // PHIMACCEPT, Should be 5 to 10% greater than PHIMLIM
    // acceptable measurement objective function
    // greater than PHIMLIM
    property StoredPhiMAccept: TRealStorage read FStoredPhiMAccept write SetStoredPhiMAccept;
    // If @name is try, PHIMACCEPT will be automatically set to be
    // 5% greater than PHIMLIM.
    property AutoPhiMAccept: Boolean read FAutoPhiMAccept write SetAutoPhiMAccept;
    // FRACPHIM, Initial value 0.1.
    // FRACPHIM is optional
    // set target measurement objective function at this fraction
    // of current measurement objective function
    // zero or greater, but less than one
    property StoredFracPhiM: TRealStorage read FStoredFracPhiM write SetStoredFracPhiM;
    // MEMSAVE “memsave” or “nomemsave”.  MEMSAVE is optional.
    // The optional MEMSAVE variable can be used to implement memory
    // conservation features that may assist PEST in very highly
    // parameterized cases.
    // activate conservation of memory at cost of execution speed and quantity of model output
    property MemSave: TMemSave read FMemSave write SetMemSave;
    // WFINIT Initial regularization weight factor.
    // If you have no idea of what the weight factor should be,
    // simply set it to 1.0.
    // greater than zero
    property StoredWFInit: TRealStorage read FStoredWFInit write SetStoredWFInit;
    // WFMIN, minimum regularization weight factor.
    // Normally settings of 1E-10 is suitable
    // greater than zero
    property StoredWeightFactorMinimum: TRealStorage
      read FStoredWeightFactorMinimum write SetStoredWeightFactorMinimum;
    // WFMAX
    // Normally settings of 1E10 is suitable
    // maximum regularization weight factor
    // greater than WFMAX
    property StoredWeightFactorMaximum: TRealStorage read FStoredWeightFactorMaximum write SetStoredWeightFactorMaximum;
    // WFFAC Weight Factor adjustment factor. Initiall value 1.3
    // must be greater than 1.
    // regularization weight factor adjustment factor
    // greater than one
    property StoredWFFac: TRealStorage read FStoredWFFac write SetStoredWFFac;
    // WFTOL Weight Factor tolerance Normally between 1E-3 and 1E-2 but can be
    // greater than 1E-2.
    // convergence criterion for regularization weight factor
    // greater than zero
    property StoredWeightFactorTolerance: TRealStorage read FStoredWeightFactorTolerance write SetStoredWeightFactorTolerance;
    // LINREG should be supplied as either “linreg” or “nonlinreg”.
    // InitialValue: nonlinreg
    // LINREG is optional.
    // If a value for LINREG is not supplied in a PEST control file, the
    // default value of “nonlinreg” is employed, unless all regularization
    // constraints are supplied as prior information, in which case the
    // default value of “linreg” is used.
    // informs PEST that all regularization constraints are linear
    property LinearRegression: TLinRegression read FLinearRegression write SetLinearRegression;
    // REGCONTINUE, Values = “continue” or “nocontinue”.
    // REGCONTINUE is optional. If absent nocontinue is assumed.
    // instructs PEST to continue minimising regularization objective function even if measurement objective function less than PHIMLIM
    property RegContinue: TRegContinue read FRegContinue write SetRegContinue;
    // IREGADJ. Optional
    // If absent, 0 is assumed.
    // Normally set to 1.
    // Values can be 1, 2, 3, 4, 5.
    // 0 no inter-regularization-group weights adjustment takes place.
    // 1,2,3 regularization is on a group by group basis.
    // 4, 5 regularization on an item by item basis.
    // 1 regularization is done so that sensitivities of
    // regularizaion groups are the same
    // 2 regularization is done so that weights of groups are the same.
    // 3 Like 1 but then multiply by a user specified group weight.
    // 4 weights calculated automatically
    // 5 items are diveded into two groups based on how informative the items
    //   and and each group is given a user supplied weight.
    property RegularizationOption: Integer read FRegularizationOption write SetRegularizationOption;
    // NOPTREGADJ required if IREGADJ = 4
    // NOPTREGADJ the optimization interval at which regularization weights
    // are re-calculated
    // Must be 1 or greater.
    // the optimization iteration interval for re-calculation of regularization weights if IREGADJ is 4 or 5
    property OptimizationInterval: Integer read FOptimizationInterval write SetOptimizationInterval;
    // REGWEIGHTRAT required if IREGADJ = 4
    // the ratio of highest to lowest regularization weight; spread is logarithmic with null space projection if set negative
    // absolute value of one or greater
    property StoredRegWeightRatio: TRealStorage read FStoredRegWeightRatio write SetStoredRegWeightRatio;
    // REGSINGTHRESH required if IREGADJ = 5
    // singular value of JtQJ (as factor of highest singular value) at which use of higher regularization weights commences if IREGADJ is set to 5
    // less than one and greater than zero
    property StoredRegularizationSingularValueThreshhold: TRealStorage
      read FStoredRegularizationSingularValueThreshhold write SetStoredRegularizationSingularValueThreshhold;
  end;

  //NPREDMAXMIN
  // -1 or 1
  TMinOrMax = (mmMinimize, mmMaximize);
  // PREDNOISE
  // 0 or 1
  TPredictiveNoise = (pnNoNoise, pnUseNoise);

  // PEST documentation Section 8.
  TPredictionProperties = class(TGoPhastPersistent)
  private
    FStoredAcceptedPhi: TRealStorage;
    FStoredUpdateLineSearchFactor: TRealStorage;
    FMinOrMax: TMinOrMax;
    FStoredAbsoluteImprovementCriterion: TRealStorage;
    FStoredRelativeImprovementCriterion: TRealStorage;
    FPredictiveNoise: TPredictiveNoise;
    FNumberOfPredictionsToCompare: Integer;
    FLineSearchRuns: Integer;
    FStoredTestLambdaPhi: TRealStorage;
    FStoredTargetPhi: TRealStorage;
    FStoredAbsolutePredictionSwitch: TRealStorage;
    FStoredInitialLineSearchFactor: TRealStorage;
    FMaxNoPredictionImprovmentRuns: Integer;
    FStoredRelativePredictionSwitch: TRealStorage;
    FStoredAbsoluteLamdaCriterion: TRealStorage;
    FStoredRelativeLamdaCriterion: TRealStorage;
    function GetAbsoluteImprovementCriterion: double;
    function GetAbsoluteLamdaCriterion: double;
    function GetAbsolutePredictionSwitch: double;
    function GetAcceptedPhi: double;
    function GetInitialLineSearchFactor: double;
    function GetRelativeImprovementCriterion: double;
    function GetRelativeLamdaCriterion: double;
    function GetRelativePredictionSwitch: double;
    function GetTargetPhi: double;
    function GetTestLambdaPhi: double;
    function GetUpdateLineSearchFactor: double;
    procedure SetAbsoluteImprovementCriterion(const Value: double);
    procedure SetAbsoluteLamdaCriterion(const Value: double);
    procedure SetAbsolutePredictionSwitch(const Value: double);
    procedure SetAcceptedPhi(const Value: double);
    procedure SetInitialLineSearchFactor(const Value: double);
    procedure SetLineSearchRuns(const Value: Integer);
    procedure SetMaxNoPredictionImprovmentRuns(const Value: Integer);
    procedure SetMinOrMax(const Value: TMinOrMax);
    procedure SetNumberOfPredictionsToCompare(const Value: Integer);
    procedure SetPredictiveNoise(const Value: TPredictiveNoise);
    procedure SetRelativeImprovementCriterion(const Value: double);
    procedure SetRelativeLamdaCriterion(const Value: double);
    procedure SetRelativePredictionSwitch(const Value: double);
    procedure SetStoredAbsoluteImprovementCriterion(const Value: TRealStorage);
    procedure SetStoredAbsoluteLamdaCriterion(const Value: TRealStorage);
    procedure SetStoredAbsolutePredictionSwitch(const Value: TRealStorage);
    procedure SetStoredAcceptedPhi(const Value: TRealStorage);
    procedure SetStoredInitialLineSearchFactor(const Value: TRealStorage);
    procedure SetStoredRelativeImprovementCriterion(const Value: TRealStorage);
    procedure SetStoredRelativeLamdaCriterion(const Value: TRealStorage);
    procedure SetStoredRelativePredictionSwitch(const Value: TRealStorage);
    procedure SetStoredTargetPhi(const Value: TRealStorage);
    procedure SetStoredTestLambdaPhi(const Value: TRealStorage);
    procedure SetStoredUpdateLineSearchFactor(const Value: TRealStorage);
    procedure SetTargetPhi(const Value: double);
    procedure SetTestLambdaPhi(const Value: double);
    procedure SetUpdateLineSearchFactor(const Value: double);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables;
    // PD0
    property TargetPhi: double read GetTargetPhi write SetTargetPhi;
    // PD1
    property AcceptedPhi: double read GetAcceptedPhi write SetAcceptedPhi;
    // PD2
    property TestLambdaPhi: double read GetTestLambdaPhi write SetTestLambdaPhi;
    // ABSPREDLAM
    property AbsoluteLamdaCriterion: double read GetAbsoluteLamdaCriterion
      write SetAbsoluteLamdaCriterion;
    // RELPREDLAM
    property RelativeLamdaCriterion: double read GetRelativeLamdaCriterion
      write SetRelativeLamdaCriterion;
    // INITSCHFAC
    // Usually 0.2 to 0.3
    property InitialLineSearchFactor: double read GetInitialLineSearchFactor
      write SetInitialLineSearchFactor;
    // MULSCHFAC
    // Usually 1.3 to 1.7
    property UpdateLineSearchFactor: double read GetUpdateLineSearchFactor
      write SetUpdateLineSearchFactor;
    // ABSPREDSWH
    // Usually 0
    property AbsolutePredictionSwitch: double read GetAbsolutePredictionSwitch
      write SetAbsolutePredictionSwitch;
    // RELPREDSWH
    // Usually 0.05
    property RelativePredictionSwitch: double read GetRelativePredictionSwitch
      write SetRelativePredictionSwitch;
    // ABSPREDSTP
    // Typically 0
    property AbsoluteImprovementCriterion: double
      read GetAbsoluteImprovementCriterion
      write SetAbsoluteImprovementCriterion;
    // RELPREDSTP
    // Typically 0.005
    property RelativeImprovementCriterion: double
      read GetRelativeImprovementCriterion
      write SetRelativeImprovementCriterion;
  published
    //NPREDMAXMIN
    // -1 or 1
    property MinOrMax: TMinOrMax read FMinOrMax write SetMinOrMax Stored True;
    // PREDNOISE
    // 0 or 1
    property PredictiveNoise: TPredictiveNoise read FPredictiveNoise
      write SetPredictiveNoise Stored True;
    // PD0
    property StoredTargetPhi: TRealStorage read FStoredTargetPhi
      write SetStoredTargetPhi;
    // PD1
    property StoredAcceptedPhi: TRealStorage read FStoredAcceptedPhi
      write SetStoredAcceptedPhi;
    // PD2
    property StoredTestLambdaPhi: TRealStorage read FStoredTestLambdaPhi
      write SetStoredTestLambdaPhi;
    // ABSPREDLAM
    property StoredAbsoluteLamdaCriterion: TRealStorage
      read FStoredAbsoluteLamdaCriterion write SetStoredAbsoluteLamdaCriterion;
    // RELPREDLAM
    property StoredRelativeLamdaCriterion: TRealStorage
      read FStoredRelativeLamdaCriterion write SetStoredRelativeLamdaCriterion;
    // INITSCHFAC
    // Usually 0.2 to 0.3
    property StoredInitialLineSearchFactor: TRealStorage
      read FStoredInitialLineSearchFactor
      write SetStoredInitialLineSearchFactor;
    // MULSCHFAC
    // Usually 1.3 to 1.7
    property StoredUpdateLineSearchFactor: TRealStorage
      read FStoredUpdateLineSearchFactor write SetStoredUpdateLineSearchFactor;
    // NSEARCH
    // >= 0
    property LineSearchRuns: Integer read FLineSearchRuns
      write SetLineSearchRuns;
    // ABSPREDSWH
    // Usually 0
    property StoredAbsolutePredictionSwitch: TRealStorage
      read FStoredAbsolutePredictionSwitch
      write SetStoredAbsolutePredictionSwitch;
    // RELPREDSWH
    // Usually 0.05
    property StoredRelativePredictionSwitch: TRealStorage
      read FStoredRelativePredictionSwitch
      write SetStoredRelativePredictionSwitch;
    // NPREDNORED
    // Typically 4
    property MaxNoPredictionImprovmentRuns: Integer
      read FMaxNoPredictionImprovmentRuns
      write SetMaxNoPredictionImprovmentRuns;
    // ABSPREDSTP
    // Typically 0
    property StoredAbsoluteImprovementCriterion: TRealStorage
      read FStoredAbsoluteImprovementCriterion
      write SetStoredAbsoluteImprovementCriterion;
    // RELPREDSTP
    // Typically 0.005
    property StoredRelativeImprovementCriterion: TRealStorage
      read FStoredRelativeImprovementCriterion
      write SetStoredRelativeImprovementCriterion;
    // NPREDSTP
    // Typically 4
    property NumberOfPredictionsToCompare: Integer
      read FNumberOfPredictionsToCompare write SetNumberOfPredictionsToCompare;
  end;

  TAltTerminationOption = (atoDontUse, atoUse);
  TAltDirection = (adAbove, adBelow);

  // PEST documentation Section 13
  TParetoProperties = class(TGoPhastPersistent)
  private
    FObservationsToReport: TStrings;
    FParetoIncrements: Integer;
    FParetoGroupName: string;
    FAltTerminationOption: TAltTerminationOption;
    FAltDirection: TAltDirection;
    FInitialIterationCount: Integer;
    FObservationName: string;
    FStoredInitialParetoWeight: TRealStorage;
    FAltIterations: Integer;
    FFinalIterationCount: Integer;
    FIntermediateIterationCount: Integer;
    FStoredAltThreshold: TRealStorage;
    FStoredFinalParetoWeight: TRealStorage;
    procedure SetAltDirection(const Value: TAltDirection);
    procedure SetAltIterations(const Value: Integer);
    procedure SetAltTerminationOption(const Value: TAltTerminationOption);
    procedure SetStoredAltThreshold(const Value: TRealStorage);
    procedure SetFinalIterationCount(const Value: Integer);
    procedure SetStoredFinalParetoWeight(const Value: TRealStorage);
    procedure SetInitialIterationCount(const Value: Integer);
    procedure SetStoredInitialParetoWeight(const Value: TRealStorage);
    procedure SetIntermediateIterationCount(const Value: Integer);
    procedure SetObservationName(const Value: string);
    procedure SetObservationsToReport(const Value: TStrings);
    procedure SetParetoGroupName(const Value: string);
    procedure SetParetoIncrements(const Value: Integer);
    function GetAltThreshold: double;
    function GetFinalParetoWeight: double;
    function GetInitialParetoWeight: double;
    procedure SetAltThreshold(const Value: double);
    procedure SetFinalParetoWeight(const Value: double);
    procedure SetInitialParetoWeight(const Value: double);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    // PARETO_WTFAC_START
    // Typical value = 0;
    property InitialParetoWeight: double read GetInitialParetoWeight
      write SetInitialParetoWeight;
    // PARETO_WTFAC_FIN
    // Typical value = 1;
    property FinalParetoWeight: double read GetFinalParetoWeight
      write SetFinalParetoWeight;
    // OBS_THRESH
    property AltThreshold: double read GetAltThreshold write SetAltThreshold;
  published
    // PARETO_OBSGROUP
    property ParetoGroupName: string read FParetoGroupName
      write SetParetoGroupName;
    // PARETO_WTFAC_START
    // Typical value = 0;
    property StoredInitialParetoWeight: TRealStorage
      read FStoredInitialParetoWeight write SetStoredInitialParetoWeight;
    // PARETO_WTFAC_FIN
    // Typical value = 1;
    property StoredFinalParetoWeight: TRealStorage read FStoredFinalParetoWeight
      write SetStoredFinalParetoWeight;
    // NUM_WTFAC_INC
    // use 10 as initial value?
    property ParetoIncrements: Integer read FParetoIncrements
      write SetParetoIncrements;
    // NUM_ITER_START
    property InitialIterationCount: Integer read FInitialIterationCount
      write SetInitialIterationCount;
    // NUM_ITER_GEN
    property IntermediateIterationCount: Integer
      read FIntermediateIterationCount write SetIntermediateIterationCount;
    // NUM_ITER_FIN
    property FinalIterationCount: Integer read FFinalIterationCount
      write SetFinalIterationCount;
    // ALT_TERM
    property AltTerminationOption: TAltTerminationOption
      read FAltTerminationOption write SetAltTerminationOption Stored True;
    // OBS_TERM
    property ObservationName: string read FObservationName
      write SetObservationName;
    // ABOVE_OR_BELOW
    property AltDirection: TAltDirection read FAltDirection
      write SetAltDirection Stored True;
    // OBS_THRESH
    property StoredAltThreshold: TRealStorage read FStoredAltThreshold
      write SetStoredAltThreshold;
    // NUM_ITER_THRESH
    property AltIterations: Integer read FAltIterations write SetAltIterations;
    // NOBS_REPORT, OBS_REPORT_1, OBS_REPORT_2, etc.
    property ObservationsToReport: TStrings read FObservationsToReport
      write SetObservationsToReport;
  end;

  TArrayPilotPointSelection = (appsNone, appsRectangular, appsTriangular);

  TPestProperties = class(TGoPhastPersistent)
  private
    FTemplateCharacter: Char;
    FExtendedTemplateCharacter: Char;
    FPestUsed: Boolean;
    FShowPilotPoints: Boolean;
    FStoredPilotPointSpacing: TRealStorage;
    FPestControlData: TPestControlData;
    FPilotPointRowCount: Integer;
    FPilotPointColumnCount: Integer;
    FLeftX: double;
    FTopY: double;
    FSvdProperties: TSingularValueDecompositionProperties;
    FLsqrProperties: TLsqrProperties;
    FObservatioGroups: TPestObservationGroups;
    FSpecifiedPilotPoints: TSimplePointCollection;
    FBetweenObservationsPilotPoints: TSimplePointCollection;
    FUseBetweenObservationsPilotPoints: Boolean;
    FArrayPilotPointSelection: TArrayPilotPointSelection;
    FTriangulaRowSpacing: Double;
    FStoredPilotPointBuffer: TRealStorage;
    FStoredMinimumSeparation: TRealStorage;
    FRegularization: TPestRegularization;
    FPriorInfoObservatioGroups: TPestObservationGroups;
    FUseInitialValuePriorInfo: Boolean;
    FUseSpatialContinuityPriorInfo: Boolean;
    FStoredSeachDistance: TRealStorage;
    FMaxPilotPointsInRange: Integer;
    FUseVertSpatialContinuityPriorInfo: Boolean;
    FArrayTemplateCharacter: Char;
    FPredictionProperties: TPredictionProperties;
    FParetoProperties: TParetoProperties;
    procedure SetTemplateCharacter(const Value: Char);
    procedure SetExtendedTemplateCharacter(const Value: Char);
    function GetPilotPointSpacing: double;
    procedure SetPilotPointSpacing(const Value: double);
    procedure SetPestUsed(const Value: Boolean);
    procedure SetShowPilotPoints(const Value: Boolean);
    procedure SetStoredPilotPointSpacing(const Value: TRealStorage);
    procedure SetPestControlData(const Value: TPestControlData);
    function GetPilotPoint(Index: Integer): TPoint2D;
    function GetPilotPointCount: Integer;
    procedure SetSvdProperties(
      const Value: TSingularValueDecompositionProperties);
    procedure SetLsqrProperties(const Value: TLsqrProperties);
    procedure SetObservatioGroups(const Value: TPestObservationGroups);
    procedure SetSpecifiedPilotPoints(const Value: TSimplePointCollection);
    procedure SetBetweenObservationsPilotPoints(
      const Value: TSimplePointCollection);
    procedure SetUseBetweenObservationsPilotPoints(const Value: Boolean);
    procedure SetArrayPilotPointSelection(
      const Value: TArrayPilotPointSelection);
    procedure SetStoredPilotPointBuffer(const Value: TRealStorage);
    function GetPilotPointBuffer: double;
    procedure SetPilotPointBuffer(const Value: double);
    procedure SetStoredMinimumSeparation(const Value: TRealStorage);
    function GetMinimumSeparation: Double;
    procedure SetMinimumSeparation(const Value: Double);
    procedure SetRegularization(const Value: TPestRegularization);
    procedure SetPriorInfoObservatioGroups(const Value: TPestObservationGroups);
    procedure SetUseInitialValuePriorInfo(const Value: Boolean);
    procedure SetUseSpatialContinuityPriorInfo(const Value: Boolean);
    procedure SetStoredSeachDistance(const Value: TRealStorage);
    function GetSeachDistance: double;
    procedure SetSeachDistance(const Value: double);
    procedure SetMaxPilotPointsInRange(const Value: Integer);
    procedure SetUseVertSpatialContinuityPriorInfo(const Value: Boolean);
    procedure SetArrayTemplateCharacter(const Value: Char);
    procedure SetPredictionProperties(const Value: TPredictionProperties);
    procedure SetParetoProperties(const Value: TParetoProperties);
  public
    Constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    property PilotPointSpacing: double read GetPilotPointSpacing
      write SetPilotPointSpacing;
    property PilotPointBuffer: double read GetPilotPointBuffer
      write SetPilotPointBuffer;
    procedure DrawPilotPoints(BitMap32: TBitmap32);
    function ShouldDrawPilotPoints: Boolean;
    property PilotPointCount: Integer read GetPilotPointCount;
    property PilotPoints[Index: Integer]: TPoint2D read GetPilotPoint;
    property MinimumSeparation: Double read GetMinimumSeparation
      write SetMinimumSeparation;
    property SeachDistance: double read GetSeachDistance write SetSeachDistance;
  Published
    property PestUsed: Boolean read FPestUsed write SetPestUsed Stored True;
    property TemplateCharacter: Char read FTemplateCharacter
      write SetTemplateCharacter;
    property ExtendedTemplateCharacter: Char read FExtendedTemplateCharacter
      write SetExtendedTemplateCharacter;
    property ArrayTemplateCharacter: Char read FArrayTemplateCharacter
      write SetArrayTemplateCharacter;
    property ShowPilotPoints: Boolean read FShowPilotPoints
      write SetShowPilotPoints Stored True;
    property StoredPilotPointSpacing: TRealStorage
      read FStoredPilotPointSpacing write SetStoredPilotPointSpacing;
    property PestControlData: TPestControlData read FPestControlData
      write SetPestControlData;
    property SvdProperties: TSingularValueDecompositionProperties
      read FSvdProperties write SetSvdProperties;
    property LsqrProperties: TLsqrProperties read FLsqrProperties
      write SetLsqrProperties;
    property ObservationGroups: TPestObservationGroups read FObservatioGroups
      write SetObservatioGroups;
    property PriorInfoObservationGroups: TPestObservationGroups read FPriorInfoObservatioGroups
      write SetPriorInfoObservatioGroups;
    property SpecifiedPilotPoints: TSimplePointCollection
      read FSpecifiedPilotPoints write SetSpecifiedPilotPoints;
    property BetweenObservationsPilotPoints: TSimplePointCollection
      read FBetweenObservationsPilotPoints write SetBetweenObservationsPilotPoints;
    property UseBetweenObservationsPilotPoints: Boolean
      read FUseBetweenObservationsPilotPoints
      write SetUseBetweenObservationsPilotPoints stored True;
    property ArrayPilotPointSelection: TArrayPilotPointSelection
      read FArrayPilotPointSelection write SetArrayPilotPointSelection;
    property StoredPilotPointBuffer: TRealStorage read FStoredPilotPointBuffer
      write SetStoredPilotPointBuffer;
    Property StoredMinimumSeparation: TRealStorage read FStoredMinimumSeparation
      write SetStoredMinimumSeparation;
    property Regularization: TPestRegularization read FRegularization
      write SetRegularization;
    property UseInitialValuePriorInfo: Boolean read FUseInitialValuePriorInfo
      write SetUseInitialValuePriorInfo;
    property UseHorizontalSpatialContinuityPriorInfo: Boolean
      read FUseSpatialContinuityPriorInfo
      write SetUseSpatialContinuityPriorInfo;
    Property StoredSeachDistance: TRealStorage read FStoredSeachDistance
      write SetStoredSeachDistance;
    property MaxPilotPointsInRange: Integer read FMaxPilotPointsInRange
      write SetMaxPilotPointsInRange;
    property UseVertSpatialContinuityPriorInfo: Boolean
      read FUseVertSpatialContinuityPriorInfo
      write SetUseVertSpatialContinuityPriorInfo;
    property PredictionProperties: TPredictionProperties
      read FPredictionProperties write SetPredictionProperties;
    property ParetoProperties: TParetoProperties read FParetoProperties
      write SetParetoProperties;
  end;

implementation

uses
  ZoomBox2, BigCanvasMethods, frmGoPhastUnit, PhastModelUnit, System.Math;

var  
  TriangleRowHeightFactor: double;

{ TPestProperties }

procedure TPestProperties.Assign(Source: TPersistent);
var
  PestSource: TPestProperties;
begin
  if Source is TPestProperties then
  begin
    PestSource := TPestProperties(Source);
    PestUsed := PestSource.PestUsed;
    ShowPilotPoints := PestSource.ShowPilotPoints;
    PilotPointSpacing := PestSource.PilotPointSpacing;
    PilotPointBuffer := PestSource.PilotPointBuffer;
    TemplateCharacter := PestSource.TemplateCharacter;
    ExtendedTemplateCharacter := PestSource.ExtendedTemplateCharacter;
    ArrayTemplateCharacter := PestSource.ArrayTemplateCharacter;
    PestControlData := PestSource.PestControlData;
    SvdProperties := PestSource.SvdProperties;
    LsqrProperties := PestSource.LsqrProperties;
    ObservationGroups := PestSource.ObservationGroups;
    PriorInfoObservationGroups := PestSource.PriorInfoObservationGroups;
    SpecifiedPilotPoints := PestSource.SpecifiedPilotPoints;
    BetweenObservationsPilotPoints := PestSource.BetweenObservationsPilotPoints;
    UseBetweenObservationsPilotPoints  := PestSource.UseBetweenObservationsPilotPoints;
    ArrayPilotPointSelection := PestSource.ArrayPilotPointSelection;
    MinimumSeparation := PestSource.MinimumSeparation;
    Regularization := PestSource.Regularization;
    UseInitialValuePriorInfo := PestSource.UseInitialValuePriorInfo;
    UseHorizontalSpatialContinuityPriorInfo := PestSource.UseHorizontalSpatialContinuityPriorInfo;
    SeachDistance := PestSource.SeachDistance;
    MaxPilotPointsInRange := PestSource.MaxPilotPointsInRange;
    UseVertSpatialContinuityPriorInfo := PestSource.UseVertSpatialContinuityPriorInfo;
    PredictionProperties := PestSource.PredictionProperties;
    ParetoProperties := PestSource.ParetoProperties;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestProperties.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(InvalidateModelEvent);
  FStoredPilotPointSpacing := TRealStorage.Create;
  FStoredPilotPointBuffer := TRealStorage.Create;
  FStoredMinimumSeparation := TRealStorage.Create;
  FStoredSeachDistance := TRealStorage.Create;
  FPestControlData := TPestControlData.Create(InvalidateModelEvent);
  FSvdProperties :=
    TSingularValueDecompositionProperties.Create(InvalidateModelEvent);
  FLsqrProperties := TLsqrProperties.Create(InvalidateModelEvent);
  FObservatioGroups := TPestObservationGroups.Create(Model);
  FPriorInfoObservatioGroups := TPestObservationGroups.Create(Model);

  FStoredPilotPointSpacing.OnChange := InvalidateModelEvent;
  FStoredPilotPointBuffer.OnChange := InvalidateModelEvent;
  FStoredMinimumSeparation.OnChange := InvalidateModelEvent;
  FStoredSeachDistance.OnChange := InvalidateModelEvent;

  FSpecifiedPilotPoints := TSimplePointCollection.Create;
  FBetweenObservationsPilotPoints := TSimplePointCollection.Create;
  FRegularization := TPestRegularization.Create(InvalidateModelEvent);
  FPredictionProperties := TPredictionProperties.Create(InvalidateModelEvent);
  FParetoProperties := TParetoProperties.Create(InvalidateModelEvent);
  InitializeVariables;
end;

destructor TPestProperties.Destroy;
begin
  FParetoProperties.Free;
  FPredictionProperties.Free;
  FRegularization.Free;
  FBetweenObservationsPilotPoints.Free;
  FSpecifiedPilotPoints.Free;
  FPriorInfoObservatioGroups.Free;
  FObservatioGroups.Free;
  FLsqrProperties.Free;
  FSvdProperties.Free;
  FPestControlData.Free;
  FStoredSeachDistance.Free;
  FStoredPilotPointBuffer.Free;
  FStoredPilotPointSpacing.Free;
  FStoredMinimumSeparation.Free;
  inherited;
end;

procedure TPestProperties.DrawPilotPoints(BitMap32: TBitmap32);
var
  ZoomBox: TQRbwZoomBox2;
  YInt: Integer;
  LineSegment: GoPhastTypes.TPointArray;
  XInt: Integer;
  PilotPointIndex: Integer;
  APilotPoint: TPoint2D;
  ArrayCount: Integer;
begin
  if ShouldDrawPilotPoints then
  begin
    ArrayCount := FPilotPointRowCount * FPilotPointColumnCount;
    SetLength(LineSegment, 2);
    ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    for PilotPointIndex := 0 to PilotPointCount - 1 do
    begin
      APilotPoint := PilotPoints[PilotPointIndex];
      YInt := ZoomBox.YCoord(APilotPoint.y);
      XInt := ZoomBox.XCoord(APilotPoint.x);

      if PilotPointIndex < ArrayCount then
      begin
        LineSegment[0].x := XInt;
        LineSegment[1].x := XInt;
        LineSegment[0].y := YInt-2;
        LineSegment[1].y := YInt+3;
        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
          True, False, 0, 2);

        LineSegment[0].x := XInt-2;
        LineSegment[1].x := XInt+3;
        LineSegment[0].y := YInt;
        LineSegment[1].y := YInt;
        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
          True, False, 0, 2);
      end
      else
      begin
        LineSegment[0].x := XInt-2;
        LineSegment[1].x := XInt+3;
        LineSegment[0].y := YInt-2;
        LineSegment[1].y := YInt+3;
        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
          True, False, 0, 2);

        LineSegment[0].x := XInt+2;
        LineSegment[1].x := XInt-3;
        LineSegment[0].y := YInt-2;
        LineSegment[1].y := YInt+3;
        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
          True, False, 0, 2);
      end;

    end;
  end;
end;

function TPestProperties.ShouldDrawPilotPoints: Boolean;
begin
  result := PestUsed and ShowPilotPoints and (PilotPointCount > 0);
end;

function TPestProperties.GetMinimumSeparation: Double;
begin
  result := StoredMinimumSeparation.Value;
end;

function TPestProperties.GetPilotPoint(Index: Integer): TPoint2D;
var
  RowIndex: Integer;
  ColIndex: Integer;
  ArrayCount: Integer;
  SpecifiedCount: Integer;
  ShortRowCount: Integer;
  LongRowCount: Integer;
  ShortIndex: Integer;
begin
  ArrayCount := 0;
  LongRowCount := 0;
//  ShortRowCount := 0;
  case ArrayPilotPointSelection of
    appsNone:
      begin
        ArrayCount := 0;
      end;
    appsRectangular:
      begin
        ArrayCount := FPilotPointColumnCount * FPilotPointRowCount;
      end;
    appsTriangular:
      begin
        ShortRowCount := FPilotPointRowCount div 2;
        LongRowCount := FPilotPointRowCount - ShortRowCount;
        ArrayCount := (ShortRowCount * (FPilotPointColumnCount - 1))
          + (LongRowCount * FPilotPointColumnCount);
      end;
  else 
    Assert(False);
  end;
  
  SpecifiedCount := ArrayCount + SpecifiedPilotPoints.Count;
  if Index < ArrayCount then
  begin
    if ArrayPilotPointSelection = appsRectangular then
    begin
      RowIndex := Index div FPilotPointColumnCount;
      ColIndex := Index  - RowIndex*FPilotPointColumnCount;
      result.Y := FTopY - RowIndex*PilotPointSpacing;
      result.X := FLeftX + ColIndex*PilotPointSpacing;
    end
    else
    begin
      Assert(ArrayPilotPointSelection = appsTriangular);
      if Index < LongRowCount * FPilotPointColumnCount then
      begin
        RowIndex := Index div FPilotPointColumnCount;
        ColIndex := Index  - RowIndex*FPilotPointColumnCount;
        result.Y := FTopY - RowIndex*2*FTriangulaRowSpacing;
        result.X := FLeftX + ColIndex*PilotPointSpacing;
      end
      else
      begin
        ShortIndex := Index - LongRowCount * FPilotPointColumnCount;
        RowIndex := ShortIndex div (FPilotPointColumnCount-1);
        ColIndex := ShortIndex - RowIndex*(FPilotPointColumnCount-1);
        result.Y := FTopY - (RowIndex*2 + 1) *FTriangulaRowSpacing;
        result.X := FLeftX + (ColIndex + 0.5)*PilotPointSpacing;
      end;
    end;
  end
  else if Index < SpecifiedCount  then
  begin
    result := (SpecifiedPilotPoints.Items[Index-ArrayCount] as TPointItem).Point2D;
  end
  else if UseBetweenObservationsPilotPoints then
  begin
    result := (BetweenObservationsPilotPoints.
      Items[Index-SpecifiedCount] as TPointItem).Point2D;
  end
  else
  begin
    Assert(False);
  end;

end;

function TPestProperties.GetPilotPointBuffer: double;
begin
  result := FStoredPilotPointBuffer.Value;
end;

function TPestProperties.GetPilotPointCount: Integer;
var
  DisLimits: TGridLimit;
  ShortRowCount: Integer;
begin
  if (PilotPointSpacing = 0) or (ArrayPilotPointSelection = appsNone) then
  begin
    result := 0;
    FPilotPointColumnCount := 0;
    FPilotPointRowCount := 0;
  end
  else
  begin
    DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
    FPilotPointColumnCount := Trunc((DisLimits.MaxX - DisLimits.MinX)/PilotPointSpacing) + 2;
    if ArrayPilotPointSelection = appsRectangular then
    begin
      FPilotPointRowCount := Trunc((DisLimits.MaxY - DisLimits.MinY)/PilotPointSpacing) + 2;
      result := FPilotPointRowCount * FPilotPointColumnCount;
      FTopY := (DisLimits.MaxY + DisLimits.MinY)/2 +
        (FPilotPointRowCount-1)/2*PilotPointSpacing;
    end
    else
    begin
      Assert(ArrayPilotPointSelection = appsTriangular);
      FTriangulaRowSpacing := PilotPointSpacing * TriangleRowHeightFactor;
      FPilotPointRowCount := Trunc((DisLimits.MaxY - DisLimits.MinY)/FTriangulaRowSpacing) + 2;
      ShortRowCount := FPilotPointRowCount div 2;
      result := (ShortRowCount * (FPilotPointColumnCount - 1))
        + ((FPilotPointRowCount - ShortRowCount) * FPilotPointColumnCount);
      FTopY := (DisLimits.MaxY + DisLimits.MinY)/2 +
        (FPilotPointRowCount-1)/2*FTriangulaRowSpacing;
    end;
    FLeftX := (DisLimits.MaxX + DisLimits.MinX)/2 -
      (FPilotPointColumnCount-1)/2*PilotPointSpacing;
  end;
  result := result + SpecifiedPilotPoints.Count;
  if UseBetweenObservationsPilotPoints then
  begin
    result := result + BetweenObservationsPilotPoints.Count;
  end;
end;

function TPestProperties.GetPilotPointSpacing: double;
begin
  result := FStoredPilotPointSpacing.Value;
end;

function TPestProperties.GetSeachDistance: double;
begin
  result := StoredSeachDistance.Value;
end;

procedure TPestProperties.InitializeVariables;
begin
  FPestUsed := False;
  FShowPilotPoints := False;
  PilotPointSpacing := 0;
  FTemplateCharacter := '@';
  FExtendedTemplateCharacter := '%';
  FArrayTemplateCharacter := '~';
  FUseBetweenObservationsPilotPoints := True;
  FArrayPilotPointSelection := appsNone;
  PilotPointBuffer := 0;
  FUseInitialValuePriorInfo := True;
  SeachDistance := 0;
  MaxPilotPointsInRange := 4;
  FUseSpatialContinuityPriorInfo := True;
  FUseVertSpatialContinuityPriorInfo := True;

  FPestControlData.InitializeVariables;
  FSvdProperties.InitializeVariables;
  FLsqrProperties.InitializeVariables;
  Regularization.InitializeVariables;
  PredictionProperties.InitializeVariables;
  ParetoProperties.InitializeVariables;

  FPriorInfoObservatioGroups.Clear;
  FObservatioGroups.Clear;
  SpecifiedPilotPoints.Clear;
  BetweenObservationsPilotPoints.Clear;
end;

procedure TPestProperties.SetArrayPilotPointSelection(
  const Value: TArrayPilotPointSelection);
begin
  if FArrayPilotPointSelection <> Value then
  begin
    FArrayPilotPointSelection := Value;
    InvalidateModel;
  end;
end;

procedure TPestProperties.SetArrayTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FArrayTemplateCharacter, Value);
end;

procedure TPestProperties.SetBetweenObservationsPilotPoints(
  const Value: TSimplePointCollection);
begin
  FBetweenObservationsPilotPoints.Assign(Value);
end;

procedure TPestProperties.SetExtendedTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FExtendedTemplateCharacter, Value);
end;

procedure TPestProperties.SetLsqrProperties(const Value: TLsqrProperties);
begin
  FLsqrProperties.Assign(Value);
end;

procedure TPestProperties.SetMaxPilotPointsInRange(const Value: Integer);
begin
  SetIntegerProperty(FMaxPilotPointsInRange, Value);
end;

procedure TPestProperties.SetMinimumSeparation(const Value: Double);
begin
  StoredMinimumSeparation.Value := Value;
end;

procedure TPestProperties.SetObservatioGroups(
  const Value: TPestObservationGroups);
begin
  FObservatioGroups.Assign(Value);
end;

procedure TPestProperties.SetParetoProperties(const Value: TParetoProperties);
begin
  FParetoProperties.Assign(Value);
end;

procedure TPestProperties.SetPestControlData(const Value: TPestControlData);
begin
  FPestControlData.Assign(Value);
end;

procedure TPestProperties.SetPestUsed(const Value: Boolean);
begin
  SetBooleanProperty(FPestUsed, Value);
end;

procedure TPestProperties.SetPilotPointBuffer(const Value: double);
begin
  FStoredPilotPointBuffer.Value := Value;
end;

procedure TPestProperties.SetPilotPointSpacing(const Value: double);
begin
  FStoredPilotPointSpacing.Value := Value;
end;

procedure TPestProperties.SetPredictionProperties(
  const Value: TPredictionProperties);
begin
  FPredictionProperties.Assign(Value);
end;

procedure TPestProperties.SetPriorInfoObservatioGroups(
  const Value: TPestObservationGroups);
begin
  FPriorInfoObservatioGroups.Assign(Value);
end;

procedure TPestProperties.SetRegularization(const Value: TPestRegularization);
begin
  FRegularization.Assign(Value);
end;

procedure TPestProperties.SetSeachDistance(const Value: double);
begin
  StoredSeachDistance.Value := Value;
end;

procedure TPestProperties.SetShowPilotPoints(const Value: Boolean);
begin
  SetBooleanProperty(FShowPilotPoints, Value);
end;

procedure TPestProperties.SetSpecifiedPilotPoints(
  const Value: TSimplePointCollection);
begin
  FSpecifiedPilotPoints.Assign(Value);
end;

procedure TPestProperties.SetStoredMinimumSeparation(const Value: TRealStorage);
begin
  FStoredMinimumSeparation.Assign(Value);
end;

procedure TPestProperties.SetStoredPilotPointBuffer(const Value: TRealStorage);
begin
  FStoredPilotPointBuffer.Assign(Value);
end;

procedure TPestProperties.SetStoredPilotPointSpacing(const Value: TRealStorage);
begin
  FStoredPilotPointSpacing.Assign(Value);
end;

procedure TPestProperties.SetStoredSeachDistance(const Value: TRealStorage);
begin
  FStoredSeachDistance.Assign(Value);
end;

procedure TPestProperties.SetSvdProperties(
  const Value: TSingularValueDecompositionProperties);
begin
  FSvdProperties.Assign(Value);
end;

procedure TPestProperties.SetTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FTemplateCharacter, Value);
end;

procedure TPestProperties.SetUseBetweenObservationsPilotPoints(
  const Value: Boolean);
begin
  SetBooleanProperty(FUseBetweenObservationsPilotPoints, Value)
end;

procedure TPestProperties.SetUseInitialValuePriorInfo(const Value: Boolean);
begin
  SetBooleanProperty(FUseInitialValuePriorInfo, Value);
end;

procedure TPestProperties.SetUseSpatialContinuityPriorInfo(
  const Value: Boolean);
begin
  SetBooleanProperty(FUseSpatialContinuityPriorInfo, Value);
end;

procedure TPestProperties.SetUseVertSpatialContinuityPriorInfo(
  const Value: Boolean);
begin
  SetBooleanProperty(FUseVertSpatialContinuityPriorInfo, Value);
end;

{ TPestControlData }

procedure TPestControlData.Assign(Source: TPersistent);
var
  PcdSource: TPestControlData;
begin
  if Source is TPestControlData then
  begin
    PcdSource := TPestControlData(Source);
    PestRestart := PcdSource.PestRestart;
    PestMode := PcdSource.PestMode;
    MaxCompressionDimension := PcdSource.MaxCompressionDimension;
    ZeroLimit := PcdSource.ZeroLimit;
    InitalLambda := PcdSource.InitalLambda;
    LambdaAdjustmentFactor := PcdSource.LambdaAdjustmentFactor;
    PhiRatioSufficient := PcdSource.PhiRatioSufficient;
    PhiReductionLambda := PcdSource.PhiReductionLambda;
    NumberOfLambdas := PcdSource.NumberOfLambdas;
    JacobianUpdate := PcdSource.JacobianUpdate;
    LambdaForgive := PcdSource.LambdaForgive;
    DerivedForgive := PcdSource.DerivedForgive;
    RelativeMaxParamChange := PcdSource.RelativeMaxParamChange;
    FactorMaxParamChange := PcdSource.FactorMaxParamChange;
    FactorOriginal := PcdSource.FactorOriginal;
    BoundStick := PcdSource.BoundStick;
    UpgradeParamVectorBending := PcdSource.UpgradeParamVectorBending;
    SwitchCriterion := PcdSource.SwitchCriterion;
    OptSwitchCount := PcdSource.OptSwitchCount;
    SplitSlopeCriterion := PcdSource.SplitSlopeCriterion;
    AutomaticUserIntervation := PcdSource.AutomaticUserIntervation;
    SensitivityReuse := PcdSource.SensitivityReuse;
    Boundscaling := PcdSource.Boundscaling;
    MaxIterations := PcdSource.MaxIterations;
    SlowConvergenceCriterion := PcdSource.SlowConvergenceCriterion;
    SlowConvergenceCountCriterion := PcdSource.SlowConvergenceCountCriterion;
    ConvergenceCountCriterion := PcdSource.ConvergenceCountCriterion;
    ParameterChangeConvergenceCriterion :=
      PcdSource.ParameterChangeConvergenceCriterion;
    ParameterChangeConvergenceCount :=
      PcdSource.ParameterChangeConvergenceCount;
    ObjectiveCriterion := PcdSource.ObjectiveCriterion;;
    PhiAbandon := PcdSource.PhiAbandon;
    WriteCovariance := PcdSource.WriteCovariance;
    WriteCorrelations := PcdSource.WriteCorrelations;
    WriteEigenVectors := PcdSource.WriteEigenVectors;
    SaveResolution := PcdSource.SaveResolution;
    SaveJacobian := PcdSource.SaveJacobian;
    SaveJacobianIteration := PcdSource.SaveJacobianIteration;
    VerboseRecord := PcdSource.VerboseRecord;
    SaveInterimResiduals := PcdSource.SaveInterimResiduals;
    SaveParamIteration := PcdSource.SaveParamIteration;
    SaveParamRun := PcdSource.SaveParamRun;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestControlData.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredZeroLimit := TRealStorage.Create;
  FStoredZeroLimit.OnChange := InvalidateModelEvent;
  FStoredInitalLambda := TRealStorage.Create;
  FStoredInitalLambda.OnChange := InvalidateModelEvent;
  FStoredLambdaAdjustmentFactor := TRealStorage.Create;
  FStoredLambdaAdjustmentFactor.OnChange := InvalidateModelEvent;
  FStoredPhiRatioSufficient := TRealStorage.Create;
  FStoredPhiRatioSufficient.OnChange := InvalidateModelEvent;
  FStoredPhiReductionLambda := TRealStorage.Create;
  FStoredPhiReductionLambda.OnChange := InvalidateModelEvent;
  FStoredRelativeMaxParamChange := TRealStorage.Create;
  FStoredRelativeMaxParamChange.OnChange := InvalidateModelEvent;
  FStoredFactorMaxParamChange := TRealStorage.Create;
  FStoredFactorMaxParamChange.OnChange := InvalidateModelEvent;
  FStoredFactorOriginal := TRealStorage.Create;
  FStoredFactorOriginal.OnChange := InvalidateModelEvent;
  FStoredSwitchCriterion := TRealStorage.Create;
  FStoredSwitchCriterion.OnChange := InvalidateModelEvent;
  FStoredSplitSlopeCriterion := TRealStorage.Create;
  FStoredSplitSlopeCriterion.OnChange := InvalidateModelEvent;
  FStoredSlowConvergenceCriterion := TRealStorage.Create;
  FStoredSlowConvergenceCriterion.OnChange := InvalidateModelEvent;
  FStoredParameterChangeConvergenceCriterion := TRealStorage.Create;
  FStoredParameterChangeConvergenceCriterion.OnChange := InvalidateModelEvent;
  FStoredObjectiveCriterion := TRealStorage.Create;
  FStoredObjectiveCriterion.OnChange := InvalidateModelEvent;
  FStoredPhiAbandon := TRealStorage.Create;
  FStoredPhiAbandon.OnChange := InvalidateModelEvent;

  InitializeVariables;
end;

destructor TPestControlData.Destroy;
begin
  FStoredPhiAbandon.Free;
  FStoredObjectiveCriterion.Free;
  FStoredParameterChangeConvergenceCriterion.Free;
  FStoredSlowConvergenceCriterion.Free;
  FStoredSplitSlopeCriterion.Free;
  FStoredSwitchCriterion.Free;
  FStoredFactorOriginal.Free;
  FStoredFactorMaxParamChange.Free;
  FStoredRelativeMaxParamChange.Free;
  FStoredPhiReductionLambda.Free;
  FStoredPhiRatioSufficient.Free;
  FStoredLambdaAdjustmentFactor.Free;
  FStoredInitalLambda.Free;
  FStoredZeroLimit.Free;
  inherited;
end;

function TPestControlData.GetSlowConvergenceCriterion: double;
begin
  result := StoredSlowConvergenceCriterion.Value;
end;

function TPestControlData.GetFactorMaxParamChange: double;
begin
  result := StoredFactorMaxParamChange.Value;
end;

function TPestControlData.GetFactorOriginal: double;
begin
  result := StoredFactorOriginal.Value;
end;

function TPestControlData.GetInitalLambda: double;
begin
  result := StoredInitalLambda.Value;
end;

function TPestControlData.GetLambdaAdjustmentFactor: double;
begin
  result := StoredLambdaAdjustmentFactor.Value;
end;

function TPestControlData.GetObjectiveCriterion: double;
begin
  result := StoredObjectiveCriterion.Value;
end;

function TPestControlData.GetParameterChangeConvergenceCriterion: double;
begin
  result := StoredParameterChangeConvergenceCriterion.Value;
end;

function TPestControlData.GetPhiAbandon: double;
begin
  result := StoredPhiAbandon.Value;
end;

function TPestControlData.GetPhiRatioSufficient: double;
begin
  result := StoredPhiRatioSufficient.Value;
end;

function TPestControlData.GetPhiReductionLambda: double;
begin
  result := StoredPhiReductionLambda.Value;
end;

function TPestControlData.GetRelativeMaxParamChange: double;
begin
  result := StoredRelativeMaxParamChange.Value;
end;

function TPestControlData.GetSplitSlopeCriterion: double;
begin
  result := StoredSplitSlopeCriterion.Value;
end;

function TPestControlData.GetSwitchCriterion: double;
begin
  result := StoredSwitchCriterion.Value;
end;

function TPestControlData.GetZeroLimit: double;
begin
  result := StoredZeroLimit.Value;
end;

procedure TPestControlData.InitializeVariables;
begin
  FPestRestart := prRestart;
  FPestMode := pmEstimation;
  FMaxCompressionDimension := 1;
  ZeroLimit := 0;
  InitalLambda := 10;
  LambdaAdjustmentFactor := -3;
  PhiRatioSufficient := 0.3;
  PhiReductionLambda := 0.01;
  FNumberOfLambdas := 10;
  FJacobianUpdate := 0;
  FLambdaForgive := lfForgive;
  FDerivedForgive := dNoForgive;
  RelativeMaxParamChange := 3;
  FactorMaxParamChange := 3;
  FactorOriginal := 0.001;
  FBoundStick := 0;
  FUpgradeParamVectorBending := upvbNoBending;
  SwitchCriterion := 0.1;
  FOptSwitchCount := 1;
  SplitSlopeCriterion := 0;
  FAutomaticUserIntervation := auiInactive;
  FSensitivityReuse := srNoReuse;
  FBoundscaling := bsBoundsScaling;
  FMaxIterations := 50;
  SlowConvergenceCriterion := 0.005;
  FSlowConvergenceCountCriterion := 4;
  FConvergenceCountCriterion := 4;
  ParameterChangeConvergenceCriterion := 0.005;
  FParameterChangeConvergenceCount := 4;
  ObjectiveCriterion := 0;
  FMakeFinalRun := mfrRun;
  PhiAbandon := 0;
  FWriteCovariance := wmWrite;
  FWriteCorrelations := wmWrite;
  FWriteEigenVectors := wmWrite;
  FSaveResolution := srDontSave;
  FSaveJacobian := sjSave;
  FSaveJacobianIteration := sjiDontSave;
  FVerboseRecord := vrVerbose;
  FSaveInterimResiduals := sirSave;
  FSaveParamIteration := spiSave;
  FSaveParamRun := sprDontSave;
end;

procedure TPestControlData.SetAutomaticUserIntervation(
  const Value: TAutomaticUserIntervation);
begin
  if FAutomaticUserIntervation <> Value then
  begin
    FAutomaticUserIntervation := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetBoundscaling(const Value: TBoundsScaling);
begin
  if FBoundscaling <> Value then
  begin
    FBoundscaling := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetBoundStick(const Value: Integer);
begin
  SetIntegerProperty(FBoundStick, Value);
end;

procedure TPestControlData.SetConvergenceCountCriterion(const Value: Integer);
begin
  SetIntegerProperty(FConvergenceCountCriterion, Value);
end;

procedure TPestControlData.SetSlowConvergenceCriterion(const Value: double);
begin
  StoredSlowConvergenceCriterion.Value := Value;
end;

procedure TPestControlData.SetDerivedForgive(const Value: TDerivedForgive);
begin
  if FDerivedForgive <> Value then
  begin
    FDerivedForgive := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetFactorMaxParamChange(const Value: double);
begin
  StoredFactorMaxParamChange.Value := Value;
end;

procedure TPestControlData.SetFactorOriginal(const Value: double);
begin
  StoredFactorOriginal.Value := Value;
end;

procedure TPestControlData.SetInitalLambda(const Value: double);
begin
  StoredInitalLambda.Value := Value;
end;

procedure TPestControlData.SetJacobianUpdate(const Value: Integer);
begin
  SetIntegerProperty(FJacobianUpdate, Value);
end;

procedure TPestControlData.SetLambdaAdjustmentFactor(const Value: double);
begin
  StoredLambdaAdjustmentFactor.Value := Value;
end;

procedure TPestControlData.SetLambdaForgive(const Value: TLambdaForgive);
begin
  if FLambdaForgive <> Value then
  begin
    FLambdaForgive := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetMakeFinalRun(const Value: TMakeFinalRun);
begin
  if FMakeFinalRun <> Value then
  begin
    FMakeFinalRun := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetMaxCompressionDimension(const Value: Integer);
begin
  SetIntegerProperty(FMaxCompressionDimension, Value);
end;

procedure TPestControlData.SetMaxIterations(const Value: Integer);
begin
  SetIntegerProperty(FMaxIterations, Value);
end;

procedure TPestControlData.SetNumberOfLambdas(const Value: Integer);
begin
  SetIntegerProperty(FNumberOfLambdas, Value);
end;

procedure TPestControlData.SetSlowConvergenceCountCriterion(const Value: integer);
begin
  SetIntegerProperty(FSlowConvergenceCountCriterion, Value);
end;

procedure TPestControlData.SetObjectiveCriterion(const Value: double);
begin
  StoredObjectiveCriterion.Value := Value;
end;

procedure TPestControlData.SetOptSwitchCount(const Value: Integer);
begin
  SetIntegerProperty(FOptSwitchCount, Value);
end;

procedure TPestControlData.SetStoredParameterChangeConvergenceCriterion(
  const Value: TRealStorage);
begin
  FStoredParameterChangeConvergenceCriterion.Assign(Value);
end;

procedure TPestControlData.SetParameterChangeConvergenceCount(
  const Value: Integer);
begin
  SetIntegerProperty(FParameterChangeConvergenceCount, Value);
end;

procedure TPestControlData.SetParameterChangeConvergenceCriterion(
  const Value: double);
begin
  StoredParameterChangeConvergenceCriterion.Value := Value;
end;

procedure TPestControlData.SetPestMode(const Value: TPestMode);
begin
  if FPestMode <> Value then
  begin
    FPestMode := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetPestRestart(const Value: TPestRestart);
begin
  if FPestRestart <> Value then
  begin
    FPestRestart := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetPhiAbandon(const Value: double);
begin
  StoredPhiAbandon.Value := Value;
end;

procedure TPestControlData.SetPhiRatioSufficient(const Value: double);
begin
  StoredPhiRatioSufficient.Value := Value;
end;

procedure TPestControlData.SetPhiReductionLambda(const Value: double);
begin
  StoredPhiReductionLambda.Value := Value;
end;

procedure TPestControlData.SetRelativeMaxParamChange(const Value: double);
begin
  StoredRelativeMaxParamChange.Value := Value;
end;

procedure TPestControlData.SetSaveInterimResiduals(
  const Value: TSaveInterimResiduals);
begin
  if FSaveInterimResiduals <> Value then
  begin
    FSaveInterimResiduals := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSaveJacobian(const Value: TSaveJacobian);
begin
  if FSaveJacobian <> Value then
  begin
    FSaveJacobian := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSaveJacobianIteration(
  const Value: TSaveJacobianIteration);
begin
  if FSaveJacobianIteration <> Value then
  begin
    FSaveJacobianIteration := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSaveParamIteration(
  const Value: TSaveParamIteration);
begin
  if FSaveParamIteration <> Value then
  begin
    FSaveParamIteration := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSaveParamRun(const Value: TSaveParamRun);
begin
  if FSaveParamRun <> Value then
  begin
    FSaveParamRun := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSaveResolution(const Value: TSaveResolution);
begin
  if FSaveResolution <> Value then
  begin
    FSaveResolution := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSensitivityReuse(const Value: TSensitivityReuse);
begin
  if FSensitivityReuse <> Value then
  begin
    FSensitivityReuse := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetSplitSlopeCriterion(const Value: double);
begin
  StoredSplitSlopeCriterion.Value := Value;
end;

procedure TPestControlData.SetStoredSlowConvergenceCriterion(
  const Value: TRealStorage);
begin
  FStoredSlowConvergenceCriterion.Assign(Value);
end;

procedure TPestControlData.SetStoredFactorMaxParamChange(
  const Value: TRealStorage);
begin
  FStoredFactorMaxParamChange.Assign(Value);
end;

procedure TPestControlData.SetStoredFactorOriginal(const Value: TRealStorage);
begin
  FStoredFactorOriginal.Assign(Value);
end;

procedure TPestControlData.SetStoredInitalLambda(const Value: TRealStorage);
begin
  FStoredInitalLambda.Assign(Value);
end;

procedure TPestControlData.SetStoredLambdaAdjustmentFactor(
  const Value: TRealStorage);
begin
  FStoredLambdaAdjustmentFactor.Assign(Value);
end;

procedure TPestControlData.SetStoredObjectiveCriterion(
  const Value: TRealStorage);
begin
  FStoredObjectiveCriterion.Assign(Value);
end;

procedure TPestControlData.SetStoredPhiAbandon(const Value: TRealStorage);
begin
  FStoredPhiAbandon.Assign(Value);
end;

procedure TPestControlData.SetStoredPhiRatioSufficient(
  const Value: TRealStorage);
begin
  FStoredPhiRatioSufficient.Assign(Value);
end;

procedure TPestControlData.SetStoredPhiReductionLambda(
  const Value: TRealStorage);
begin
  FStoredPhiReductionLambda.Assign(Value);
end;

procedure TPestControlData.SetStoredRelativeMaxParamChange(
  const Value: TRealStorage);
begin
  FStoredRelativeMaxParamChange.Assign(Value);
end;

procedure TPestControlData.SetStoredSplitSlopeCriterion(
  const Value: TRealStorage);
begin
  FStoredSplitSlopeCriterion.Assign(Value);
end;

procedure TPestControlData.SetStoredSwitchCriterion(const Value: TRealStorage);
begin
  FStoredSwitchCriterion.Assign(Value);
end;

procedure TPestControlData.SetStoredZeroLimit(const Value: TRealStorage);
begin
  FStoredZeroLimit.Assign(Value);
end;

procedure TPestControlData.SetSwitchCriterion(const Value: double);
begin
  StoredSwitchCriterion.Value := Value;
end;

procedure TPestControlData.SetUpgradeParamVectorBending(
  const Value: TUpgradeParamVectorBending);
begin
  if FUpgradeParamVectorBending <> Value then
  begin
    FUpgradeParamVectorBending := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetVerboseRecord(const Value: TVerboseRecord);
begin
  if FVerboseRecord <> Value then
  begin
    FVerboseRecord := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetWriteCorrelations(const Value: TWriteMatrix);
begin
  if FWriteCorrelations <> Value then
  begin
    FWriteCorrelations := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetWriteCovariance(const Value: TWriteMatrix);
begin
  if FWriteCovariance <> Value then
  begin
    FWriteCovariance := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetWriteEigenVectors(const Value: TWriteMatrix);
begin
  if FWriteEigenVectors <> Value then
  begin
    FWriteEigenVectors := Value;
    InvalidateModel;
  end;
end;

procedure TPestControlData.SetZeroLimit(const Value: double);
begin
  StoredZeroLimit.Value := Value;
end;

{ TSingularValueDecompositionProperties }

procedure TSingularValueDecompositionProperties.Assign(Source: TPersistent);
var
  SvdSource: TSingularValueDecompositionProperties;
begin
  if Source is TSingularValueDecompositionProperties then
  begin
    SvdSource := TSingularValueDecompositionProperties(Source);
    EigenThreshold := SvdSource.EigenThreshold;
    Mode := SvdSource.Mode;
    MaxSingularValues := SvdSource.MaxSingularValues;
    EigenWrite := SvdSource.EigenWrite;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSingularValueDecompositionProperties.Create(
  InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredEigenThreshold := TRealStorage.Create;
  FStoredEigenThreshold.OnChange := InvalidateModelEvent;
  InitializeVariables;
end;

destructor TSingularValueDecompositionProperties.Destroy;
begin
  FStoredEigenThreshold.Free;
  inherited;
end;

function TSingularValueDecompositionProperties.GetEigenThreshold: Double;
begin
  result := StoredEigenThreshold.Value;
end;

procedure TSingularValueDecompositionProperties.InitializeVariables;
begin
  FMode := smNormal;
  FMaxSingularValues := 1000;
  EigenThreshold := 5E-7;
  FEigenWrite := ewReduced;
end;

procedure TSingularValueDecompositionProperties.SetEigenThreshold(
  const Value: Double);
begin
  StoredEigenThreshold.Value := Value;
end;

procedure TSingularValueDecompositionProperties.SetEigenWrite(
  const Value: TEigenWrite);
begin
  if FEigenWrite <> Value then
  begin
    FEigenWrite := Value;
    InvalidateModel;
  end;
end;

procedure TSingularValueDecompositionProperties.SetMaxSingularValues(
  const Value: Integer);
begin
  SetIntegerProperty(FMaxSingularValues, Value);
end;

procedure TSingularValueDecompositionProperties.SetMode(const Value: TSvdMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    InvalidateModel;
  end;
end;

procedure TSingularValueDecompositionProperties.SetStoredEigenThreshold(
  const Value: TRealStorage);
begin
  FStoredEigenThreshold.Assign(Value);
end;

{ TLsqrProperties }

procedure TLsqrProperties.Assign(Source: TPersistent);
var
  LsqrSource: TLsqrProperties;
begin
  if Source is TLsqrProperties then
  begin
    LsqrSource := TLsqrProperties(Source);
    Mode := LsqrSource.Mode;
    MatrixTolerance := LsqrSource.MatrixTolerance;
    RightHandSideTolerance := LsqrSource.RightHandSideTolerance;
    ConditionNumberLimit := LsqrSource.ConditionNumberLimit;
    MaxIteration := LsqrSource.MaxIteration;
    LsqrWrite := LsqrSource.LsqrWrite;
  end
  else
  begin
    inherited;
  end;
end;

constructor TLsqrProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredMatrixTolerance := TRealStorage.Create;
  FStoredRightHandSideTolerance := TRealStorage.Create;
  FStoredConditionNumberLimit := TRealStorage.Create;
  FStoredMatrixTolerance.OnChange := InvalidateModelEvent;
  FStoredRightHandSideTolerance.OnChange := InvalidateModelEvent;
  FStoredConditionNumberLimit.OnChange := InvalidateModelEvent;
  InitializeVariables;
end;

destructor TLsqrProperties.Destroy;
begin
  FStoredConditionNumberLimit.Free;
  FStoredRightHandSideTolerance.Free;
  FStoredMatrixTolerance.Free;
  inherited;
end;

function TLsqrProperties.GetConditionNumberLimit: double;
begin
  result := StoredConditionNumberLimit.Value;
end;

function TLsqrProperties.GetMatrixTolerance: double;
begin
  result := StoredMatrixTolerance.Value;
end;

function TLsqrProperties.GetRightHandSideTolerance: double;
begin
  result := StoredRightHandSideTolerance.Value;
end;

procedure TLsqrProperties.InitializeVariables;
begin
  FMode := lmDeactivate;
  MatrixTolerance := 1E-4;
  RightHandSideTolerance := 1E-4;
  ConditionNumberLimit := 1000;
  MaxIteration := 0;
  LsqrWrite := lwWrite;
{
    // LSQRMODE
    property Mode: TLsqrMode read FMode write SetMode;
    // LSQR_ATOL
    property StoredMatrixTolerance: TRealStorage read FStoredMatrixTolerance write SetStoredMatrixTolerance;
    // LSQR_BTOL
    property StoredRightHandSideTolerance: TRealStorage read FStoredRightHandSideTolerance write SetStoredRightHandSideTolerance;
    // LSQR_CONLIM
    property StoredConditionNumberLimit: TRealStorage read FStoredConditionNumberLimit write SetStoredConditionNumberLimit;
    // LSQR_ITNLIM
    property MaxIteration: Integer read FMaxIteration write SetMaxIteration;
    // LSQRWRITE
    property LsqrWrite: TLsqrWrite read FLsqrWrite write SetLsqrWrite;
}
end;

procedure TLsqrProperties.SetConditionNumberLimit(const Value: double);
begin
  StoredConditionNumberLimit.Value := Value;
end;

procedure TLsqrProperties.SetLsqrWrite(const Value: TLsqrWrite);
begin
  if FLsqrWrite <> Value then
  begin
    FLsqrWrite := Value;
    InvalidateModel;
  end;
end;

procedure TLsqrProperties.SetMatrixTolerance(const Value: double);
begin
  StoredMatrixTolerance.Value := Value;
end;

procedure TLsqrProperties.SetMaxIteration(const Value: Integer);
begin
  SetIntegerProperty(FMaxIteration, Value);

end;

procedure TLsqrProperties.SetMode(const Value: TLsqrMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    InvalidateModel;
  end;
end;

procedure TLsqrProperties.SetRightHandSideTolerance(const Value: double);
begin
  StoredRightHandSideTolerance.Value := Value;
end;

procedure TLsqrProperties.SetStoredConditionNumberLimit(
  const Value: TRealStorage);
begin
  FStoredConditionNumberLimit.Assign(Value);
end;

procedure TLsqrProperties.SetStoredMatrixTolerance(const Value: TRealStorage);
begin
  FStoredMatrixTolerance.Assign(Value);
end;

procedure TLsqrProperties.SetStoredRightHandSideTolerance(
  const Value: TRealStorage);
begin
  FStoredRightHandSideTolerance.Assign(Value);
end;

{ TPestRegularization }

procedure TPestRegularization.Assign(Source: TPersistent);
var
  PReg: TPestRegularization;
begin
  if Source is TPestRegularization then
  begin
    PReg := TPestRegularization(Source);
    PhiMLim := PReg.PhiMLim;
    PhiMAccept := PReg.PhiMAccept;
    FracPhiM := PReg.FracPhiM;
    WFInit := PReg.WFInit;
    WeightFactorMinimum := PReg.WeightFactorMinimum;
    WeightFactorMaximum := PReg.WeightFactorMaximum;
    WFFac := PReg.WFFac;
    WeightFactorTolerance := PReg.WeightFactorTolerance;
    RegWeightRatio := PReg.RegWeightRatio;
    RegularizationSingularValueThreshhold := PReg.RegularizationSingularValueThreshhold;
    AutoPhiMAccept := PReg.AutoPhiMAccept;
    MemSave := PReg.MemSave;
    LinearRegression := PReg.LinearRegression;
    RegContinue := PReg.RegContinue;
    RegularizationOption := PReg.RegularizationOption;
    OptimizationInterval := PReg.OptimizationInterval;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestRegularization.Create(InvalidateModelEvent: TNotifyEvent);
begin
  FStoredFracPhiM := TRealStorage.Create;
  FStoredWeightFactorMaximum := TRealStorage.Create;
  FStoredPhiMAccept := TRealStorage.Create;
  FStoredRegularizationSingularValueThreshhold := TRealStorage.Create;
  FStoredWeightFactorMinimum := TRealStorage.Create;
  FStoredWFFac := TRealStorage.Create;
  FStoredRegWeightRatio := TRealStorage.Create;
  FStoredWFInit := TRealStorage.Create;
  FStoredWeightFactorTolerance := TRealStorage.Create;
  FStoredPhiMLim := TRealStorage.Create;
  
  FStoredFracPhiM.OnChange := InvalidateModelEvent;
  FStoredWeightFactorMaximum.OnChange := InvalidateModelEvent;
  FStoredPhiMAccept.OnChange := InvalidateModelEvent;
  FStoredRegularizationSingularValueThreshhold.OnChange := InvalidateModelEvent;
  FStoredWeightFactorMinimum.OnChange := InvalidateModelEvent;
  FStoredWFFac.OnChange := InvalidateModelEvent;
  FStoredRegWeightRatio.OnChange := InvalidateModelEvent;
  FStoredWFInit.OnChange := InvalidateModelEvent;
  FStoredWeightFactorTolerance.OnChange := InvalidateModelEvent;
  FStoredPhiMLim.OnChange := InvalidateModelEvent;
  
  InitializeVariables;
end;

destructor TPestRegularization.Destroy;
begin
  FStoredFracPhiM.Free;
  FStoredWeightFactorMaximum.Free;
  FStoredPhiMAccept.Free;
  FStoredRegularizationSingularValueThreshhold.Free;
  FStoredWeightFactorMinimum.Free;
  FStoredWFFac.Free;
  FStoredRegWeightRatio.Free;
  FStoredWFInit.Free;
  FStoredWeightFactorTolerance.Free;
  FStoredPhiMLim.Free;

  inherited;
end;

function TPestRegularization.GetFracPhiM: double;
begin
  result := StoredFracPhiM.Value;
end;

function TPestRegularization.GetPhiMAccept: double;
begin
  result := StoredPhiMAccept.Value;
end;

function TPestRegularization.GetPhiMLim: double;
begin
  result := StoredPhiMLim.Value;
end;

function TPestRegularization.GetRegularizationSingularValueThreshhold: double;
begin
  result := StoredRegularizationSingularValueThreshhold.Value
end;

function TPestRegularization.GetRegWeightRatio: double;
begin
  result := StoredRegWeightRatio.Value;
end;

function TPestRegularization.GetWeightFactorMaximum: double;
begin
  result := StoredWeightFactorMaximum.Value;
end;

function TPestRegularization.GetWeightFactorMinimum: double;
begin
  result := StoredWeightFactorMinimum.Value;
end;

function TPestRegularization.GetWeightFactorTolerance: double;
begin
  result := StoredWeightFactorTolerance.Value;
end;

function TPestRegularization.GetWFFac: double;
begin
  result := StoredWFFac.Value;
end;

function TPestRegularization.GetWFInit: double;
begin
  result := StoredWFInit.Value
end;

procedure TPestRegularization.InitializeVariables;
begin
  PhiMLim := 1E-10;
  PhiMAccept := 1.05E-10;
  AutoPhiMAccept := True;
  FracPhiM := 0.1;
  MemSave := msNoMemSave;
  WFInit := 1.0;
  WeightFactorMinimum := 1E-10;
  WeightFactorMaximum := 1E10;
  WFFac := 1.3;
  WeightFactorTolerance := 1e-2;
  LinearRegression := lfNoLinReg;
  RegContinue := rcNoContinue;
  RegularizationOption := 1;
  OptimizationInterval := 1;
  RegWeightRatio := 50;
  RegularizationSingularValueThreshhold := 0.5;
end;

procedure TPestRegularization.SetAutoPhiMAccept(const Value: Boolean);
begin
  SetBooleanProperty(FAutoPhiMAccept, Value)
end;

procedure TPestRegularization.SetFracPhiM(const Value: double);
begin
  StoredFracPhiM.Value := Value;
end;

procedure TPestRegularization.SetLinearRegression(const Value: TLinRegression);
begin
  if FLinearRegression <> Value then
  begin
    InvalidateModel;
    FLinearRegression := Value;
  end;
end;

procedure TPestRegularization.SetMemSave(const Value: TMemSave);
begin
  if FMemSave <> Value then
  begin
    InvalidateModel;
    FMemSave := Value;
  end;
end;

procedure TPestRegularization.SetOptimizationInterval(const Value: Integer);
begin
  SetIntegerProperty(FOptimizationInterval, Value)
end;

procedure TPestRegularization.SetPhiMAccept(const Value: double);
begin
  StoredPhiMAccept.Value := Value;
end;

procedure TPestRegularization.SetPhiMLim(const Value: double);
begin
  StoredPhiMLim.Value := Value;
end;

procedure TPestRegularization.SetRegContinue(const Value: TRegContinue);
begin
  if FRegContinue <> Value then
  begin
    InvalidateModel;
    FRegContinue := Value;
  end;
end;

procedure TPestRegularization.SetRegularizationOption(const Value: Integer);
begin
  SetIntegerProperty(FRegularizationOption, Value)
end;

procedure TPestRegularization.SetRegularizationSingularValueThreshhold(
  const Value: double);
begin
  StoredRegularizationSingularValueThreshhold.Value := Value;
end;

procedure TPestRegularization.SetRegWeightRatio(const Value: double);
begin
  StoredRegWeightRatio.Value := Value;
end;

procedure TPestRegularization.SetStoredRegularizationSingularValueThreshhold(
  const Value: TRealStorage);
begin
  FStoredRegularizationSingularValueThreshhold.Assign(Value);
end;

procedure TPestRegularization.SetStoredFracPhiM(const Value: TRealStorage);
begin
  FStoredFracPhiM.Assign(Value);
end;

procedure TPestRegularization.SetStoredPhiMAccept(const Value: TRealStorage);
begin
  FStoredPhiMAccept.Assign(Value);
end;

procedure TPestRegularization.SetStoredPhiMLim(const Value: TRealStorage);
begin
  FStoredPhiMLim.Assign(Value);
end;

procedure TPestRegularization.SetStoredRegWeightRatio(
  const Value: TRealStorage);
begin
  FStoredRegWeightRatio.Assign(Value);
end;

procedure TPestRegularization.SetStoredWeightFactorMaximum(
  const Value: TRealStorage);
begin
  FStoredWeightFactorMaximum.Assign(Value);
end;

procedure TPestRegularization.SetStoredWeightFactorMinimum(
  const Value: TRealStorage);
begin
  FStoredWeightFactorMinimum.Assign(Value);
end;

procedure TPestRegularization.SetStoredWFFac(const Value: TRealStorage);
begin
  FStoredWFFac.Assign(Value);
end;

procedure TPestRegularization.SetStoredWFInit(const Value: TRealStorage);
begin
  FStoredWFInit.Assign(Value);
end;

procedure TPestRegularization.SetWeightFactorMaximum(const Value: double);
begin
  StoredWeightFactorMaximum.Value := Value;
end;

procedure TPestRegularization.SetWeightFactorMinimum(const Value: double);
begin
  StoredWeightFactorMinimum.Value := Value;
end;

procedure TPestRegularization.SetWeightFactorTolerance(const Value: double);
begin
  StoredWeightFactorTolerance.Value := Value;
end;

procedure TPestRegularization.SetWFFac(const Value: double);
begin
  StoredWFFac.Value := Value;
end;

procedure TPestRegularization.SetWFInit(const Value: double);
begin
  StoredWFInit.Value := Value;
end;

procedure TPestRegularization.SetStoredWeightFactorTolerance(
  const Value: TRealStorage);
begin
  FStoredWeightFactorTolerance.Assign(Value);
end;

{ TPredictionProperties }

procedure TPredictionProperties.Assign(Source: TPersistent);
var
  PredSource: TPredictionProperties;
begin
  if Source is TPredictionProperties then
  begin
    PredSource := TPredictionProperties(Source);
    MinOrMax := PredSource.MinOrMax;
    PredictiveNoise := PredSource.PredictiveNoise;
    TargetPhi := PredSource.TargetPhi;
    AcceptedPhi := PredSource.AcceptedPhi;
    TestLambdaPhi := PredSource.TestLambdaPhi;
    AbsoluteLamdaCriterion := PredSource.AbsoluteLamdaCriterion;
    RelativeLamdaCriterion := PredSource.RelativeLamdaCriterion;
    InitialLineSearchFactor := PredSource.InitialLineSearchFactor;
    UpdateLineSearchFactor := PredSource.UpdateLineSearchFactor;
    LineSearchRuns := PredSource.LineSearchRuns;
    AbsolutePredictionSwitch := PredSource.AbsolutePredictionSwitch;
    RelativePredictionSwitch := PredSource.RelativePredictionSwitch;
    MaxNoPredictionImprovmentRuns := PredSource.MaxNoPredictionImprovmentRuns;
    AbsoluteImprovementCriterion := PredSource.AbsoluteImprovementCriterion;
    RelativeImprovementCriterion := PredSource.RelativeImprovementCriterion;
    NumberOfPredictionsToCompare := PredSource.NumberOfPredictionsToCompare;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPredictionProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;

  FStoredAcceptedPhi := TRealStorage.Create;
  FStoredUpdateLineSearchFactor := TRealStorage.Create;
  FStoredAbsoluteImprovementCriterion := TRealStorage.Create;
  FStoredRelativeImprovementCriterion := TRealStorage.Create;
  FStoredTestLambdaPhi := TRealStorage.Create;
  FStoredTargetPhi := TRealStorage.Create;
  FStoredAbsolutePredictionSwitch := TRealStorage.Create;
  FStoredInitialLineSearchFactor := TRealStorage.Create;
  FStoredRelativePredictionSwitch := TRealStorage.Create;
  FStoredAbsoluteLamdaCriterion := TRealStorage.Create;
  FStoredRelativeLamdaCriterion := TRealStorage.Create;

  FStoredAcceptedPhi.OnChange := InvalidateModelEvent;
  FStoredUpdateLineSearchFactor.OnChange := InvalidateModelEvent;
  FStoredAbsoluteImprovementCriterion.OnChange := InvalidateModelEvent;
  FStoredRelativeImprovementCriterion.OnChange := InvalidateModelEvent;
  FStoredTestLambdaPhi.OnChange := InvalidateModelEvent;
  FStoredTargetPhi.OnChange := InvalidateModelEvent;
  FStoredAbsolutePredictionSwitch.OnChange := InvalidateModelEvent;
  FStoredInitialLineSearchFactor.OnChange := InvalidateModelEvent;
  FStoredRelativePredictionSwitch.OnChange := InvalidateModelEvent;
  FStoredAbsoluteLamdaCriterion.OnChange := InvalidateModelEvent;
  FStoredRelativeLamdaCriterion.OnChange := InvalidateModelEvent;

  InitializeVariables;
end;

destructor TPredictionProperties.Destroy;
begin
  FStoredAcceptedPhi.Free;
  FStoredUpdateLineSearchFactor.Free;
  FStoredAbsoluteImprovementCriterion.Free;
  FStoredRelativeImprovementCriterion.Free;
  FStoredTestLambdaPhi.Free;
  FStoredTargetPhi.Free;
  FStoredAbsolutePredictionSwitch.Free;
  FStoredInitialLineSearchFactor.Free;
  FStoredRelativePredictionSwitch.Free;
  FStoredAbsoluteLamdaCriterion.Free;
  FStoredRelativeLamdaCriterion.Free;

  inherited;
end;

function TPredictionProperties.GetAbsoluteImprovementCriterion: double;
begin
  result := StoredAbsoluteImprovementCriterion.Value;
end;

function TPredictionProperties.GetAbsoluteLamdaCriterion: double;
begin
  result := StoredAbsoluteLamdaCriterion.Value;
end;

function TPredictionProperties.GetAbsolutePredictionSwitch: double;
begin
  result := StoredAbsolutePredictionSwitch.Value;
end;

function TPredictionProperties.GetAcceptedPhi: double;
begin
  result := StoredAcceptedPhi.Value;
end;

function TPredictionProperties.GetInitialLineSearchFactor: double;
begin
  result := StoredInitialLineSearchFactor.Value;
end;

function TPredictionProperties.GetRelativeImprovementCriterion: double;
begin
  result := StoredRelativeImprovementCriterion.Value;
end;

function TPredictionProperties.GetRelativeLamdaCriterion: double;
begin
  result := StoredRelativeLamdaCriterion.Value;
end;

function TPredictionProperties.GetRelativePredictionSwitch: double;
begin
  result := StoredRelativePredictionSwitch.Value;
end;

function TPredictionProperties.GetTargetPhi: double;
begin
  result := StoredTargetPhi.Value;
end;

function TPredictionProperties.GetTestLambdaPhi: double;
begin
  result := StoredTestLambdaPhi.Value;
end;

function TPredictionProperties.GetUpdateLineSearchFactor: double;
begin
  result := StoredUpdateLineSearchFactor.Value;
end;

procedure TPredictionProperties.InitializeVariables;
begin
  FMinOrMax := mmMaximize;
  FPredictiveNoise := pnNoNoise;
  TargetPhi := 0;
  AcceptedPhi := 0;
  TestLambdaPhi := 0;
  AbsoluteLamdaCriterion := 0;
  RelativeLamdaCriterion :=0.005;
  InitialLineSearchFactor := 0.25;
  UpdateLineSearchFactor := 1.5;
  LineSearchRuns := 0;
  AbsolutePredictionSwitch := 0;
  RelativePredictionSwitch := 0.5;
  MaxNoPredictionImprovmentRuns := 4;
  AbsoluteImprovementCriterion := 0;
  RelativeImprovementCriterion := 0.005;
  NumberOfPredictionsToCompare := 4;
end;

procedure TPredictionProperties.SetAbsoluteImprovementCriterion(
  const Value: double);
begin
  StoredAbsoluteImprovementCriterion.Value := Value;
end;

procedure TPredictionProperties.SetAbsoluteLamdaCriterion(const Value: double);
begin
  StoredAbsoluteLamdaCriterion.Value := Value;
end;

procedure TPredictionProperties.SetAbsolutePredictionSwitch(
  const Value: double);
begin
  StoredAbsolutePredictionSwitch.Value := Value;
end;

procedure TPredictionProperties.SetAcceptedPhi(const Value: double);
begin
  StoredAcceptedPhi.Value := Value;
end;

procedure TPredictionProperties.SetInitialLineSearchFactor(const Value: double);
begin
  StoredInitialLineSearchFactor.Value := Value;
end;

procedure TPredictionProperties.SetLineSearchRuns(const Value: Integer);
begin
  SetIntegerProperty(FLineSearchRuns, Value);
end;

procedure TPredictionProperties.SetMaxNoPredictionImprovmentRuns(
  const Value: Integer);
begin
  SetIntegerProperty(FMaxNoPredictionImprovmentRuns, Value);
end;

procedure TPredictionProperties.SetMinOrMax(const Value: TMinOrMax);
begin
  if FMinOrMax <> Value then
  begin
    FMinOrMax := Value;
    InvalidateModel;
  end;
end;

procedure TPredictionProperties.SetNumberOfPredictionsToCompare(
  const Value: Integer);
begin
  SetIntegerProperty(FNumberOfPredictionsToCompare, Value);
end;

procedure TPredictionProperties.SetPredictiveNoise(
  const Value: TPredictiveNoise);
begin
  if FPredictiveNoise <> Value then
  begin
    FPredictiveNoise := Value;
    InvalidateModel;
  end;
end;

procedure TPredictionProperties.SetRelativeImprovementCriterion(
  const Value: double);
begin
  StoredRelativeImprovementCriterion.Value := Value;
end;

procedure TPredictionProperties.SetRelativeLamdaCriterion(const Value: double);
begin
  StoredRelativeLamdaCriterion.Value := Value;
end;

procedure TPredictionProperties.SetRelativePredictionSwitch(
  const Value: double);
begin
  StoredRelativePredictionSwitch.Value := Value;
end;

procedure TPredictionProperties.SetStoredAbsoluteImprovementCriterion(
  const Value: TRealStorage);
begin
  FStoredAbsoluteImprovementCriterion.Assign(Value);
end;

procedure TPredictionProperties.SetStoredAbsoluteLamdaCriterion(
  const Value: TRealStorage);
begin
  FStoredAbsoluteLamdaCriterion.Assign(Value);
end;

procedure TPredictionProperties.SetStoredAbsolutePredictionSwitch(
  const Value: TRealStorage);
begin
  FStoredAbsolutePredictionSwitch.Assign(Value);
end;

procedure TPredictionProperties.SetStoredAcceptedPhi(const Value: TRealStorage);
begin
  FStoredAcceptedPhi.Assign(Value);
end;

procedure TPredictionProperties.SetStoredInitialLineSearchFactor(
  const Value: TRealStorage);
begin
  FStoredInitialLineSearchFactor.Assign(Value);
end;

procedure TPredictionProperties.SetStoredRelativeImprovementCriterion(
  const Value: TRealStorage);
begin
  FStoredRelativeImprovementCriterion.Assign(Value);
end;

procedure TPredictionProperties.SetStoredRelativeLamdaCriterion(
  const Value: TRealStorage);
begin
  FStoredRelativeLamdaCriterion.Assign(Value);
end;

procedure TPredictionProperties.SetStoredRelativePredictionSwitch(
  const Value: TRealStorage);
begin
  FStoredRelativePredictionSwitch.Assign(Value);
end;

procedure TPredictionProperties.SetStoredTargetPhi(const Value: TRealStorage);
begin
  FStoredTargetPhi.Assign(Value);
end;

procedure TPredictionProperties.SetStoredTestLambdaPhi(
  const Value: TRealStorage);
begin
  FStoredTestLambdaPhi.Assign(Value);
end;

procedure TPredictionProperties.SetStoredUpdateLineSearchFactor(
  const Value: TRealStorage);
begin
  FStoredUpdateLineSearchFactor.Assign(Value);
end;

procedure TPredictionProperties.SetTargetPhi(const Value: double);
begin
  StoredTargetPhi.Value := Value;
end;

procedure TPredictionProperties.SetTestLambdaPhi(const Value: double);
begin
  StoredTestLambdaPhi.Value := Value;
end;

procedure TPredictionProperties.SetUpdateLineSearchFactor(const Value: double);
begin
  StoredUpdateLineSearchFactor.Value := Value;
end;

{ TParetoProperties }

procedure TParetoProperties.Assign(Source: TPersistent);
var
  SourcePProp: TParetoProperties;
begin
  if Source is TParetoProperties then
  begin
    SourcePProp := TParetoProperties(Source);
    InitialParetoWeight := SourcePProp.InitialParetoWeight;
    FinalParetoWeight := SourcePProp.FinalParetoWeight;
    AltThreshold := SourcePProp.AltThreshold;
    ParetoGroupName := SourcePProp.ParetoGroupName;
    ParetoIncrements := SourcePProp.ParetoIncrements;
    IntermediateIterationCount := SourcePProp.IntermediateIterationCount;
    InitialIterationCount := SourcePProp.InitialIterationCount;
    FinalIterationCount := SourcePProp.FinalIterationCount;
    AltTerminationOption := SourcePProp.AltTerminationOption;
    ObservationName := SourcePProp.ObservationName;
    AltDirection := SourcePProp.AltDirection;
    AltIterations := SourcePProp.AltIterations;
    ObservationsToReport := SourcePProp.ObservationsToReport;
  end
  else
  begin
    inherited;
  end;
end;

constructor TParetoProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FObservationsToReport := TStringList.Create;
  FStoredInitialParetoWeight := TRealStorage.Create;
  FStoredAltThreshold := TRealStorage.Create;
  FStoredFinalParetoWeight := TRealStorage.Create;

  (FObservationsToReport as TStringList).OnChange := InvalidateModelEvent;
  (FObservationsToReport as TStringList).CaseSensitive := False;
  FStoredInitialParetoWeight.OnChange := InvalidateModelEvent;
  FStoredAltThreshold.OnChange := InvalidateModelEvent;
  FStoredFinalParetoWeight.OnChange := InvalidateModelEvent;

  InitializeVariables;
end;

destructor TParetoProperties.Destroy;
begin
  FObservationsToReport.Free;
  FStoredInitialParetoWeight.Free;
  FStoredAltThreshold.Free;
  FStoredFinalParetoWeight.Free;

  inherited;
end;

function TParetoProperties.GetAltThreshold: double;
begin
  result := StoredAltThreshold.Value;
end;

function TParetoProperties.GetFinalParetoWeight: double;
begin
  result := StoredFinalParetoWeight.Value;
end;

function TParetoProperties.GetInitialParetoWeight: double;
begin
  result := StoredInitialParetoWeight.Value;
end;

procedure TParetoProperties.InitializeVariables;
begin
  InitialParetoWeight := 0;
  FinalParetoWeight := 1;
  AltThreshold := 0;
  ParetoGroupName := '';
  ParetoIncrements := 10;
  InitialIterationCount := 0;
  IntermediateIterationCount := 1;
  FinalIterationCount := 0;
  AltTerminationOption := atoDontUse;
  ObservationName := '';
  AltDirection := adAbove;
  AltIterations := 1;
  ObservationsToReport.Clear;
end;

procedure TParetoProperties.SetAltDirection(const Value: TAltDirection);
begin
  if FAltDirection <> Value then
  begin
    FAltDirection := Value;
    InvalidateModel;
  end;
end;

procedure TParetoProperties.SetAltIterations(const Value: Integer);
begin
  SetIntegerProperty(FAltIterations, Value);
end;

procedure TParetoProperties.SetAltTerminationOption(
  const Value: TAltTerminationOption);
begin
  if FAltTerminationOption <> Value then
  begin
    FAltTerminationOption := Value;
    InvalidateModel;
  end;
end;

procedure TParetoProperties.SetAltThreshold(const Value: double);
begin
  StoredAltThreshold.Value := Value;
end;

procedure TParetoProperties.SetStoredAltThreshold(const Value: TRealStorage);
begin
  FStoredAltThreshold.Assign(Value);
end;

procedure TParetoProperties.SetFinalIterationCount(const Value: Integer);
begin
  SetIntegerProperty(FFinalIterationCount, Value);
end;

procedure TParetoProperties.SetFinalParetoWeight(const Value: double);
begin
  StoredFinalParetoWeight.Value := Value;
end;

procedure TParetoProperties.SetStoredFinalParetoWeight(const Value: TRealStorage);
begin
  FStoredFinalParetoWeight.Assign(Value);
end;

procedure TParetoProperties.SetInitialIterationCount(const Value: Integer);
begin
  SetIntegerProperty(FInitialIterationCount, Value);
end;

procedure TParetoProperties.SetInitialParetoWeight(const Value: double);
begin
  StoredInitialParetoWeight.Value := Value;
end;

procedure TParetoProperties.SetStoredInitialParetoWeight(const Value: TRealStorage);
begin
  FStoredInitialParetoWeight.Assign(Value);
end;

procedure TParetoProperties.SetIntermediateIterationCount(const Value: Integer);
begin
  SetIntegerProperty(FIntermediateIterationCount, Value);
end;

procedure TParetoProperties.SetObservationName(const Value: string);
begin
  SetStringProperty(FObservationName, Value);
end;

procedure TParetoProperties.SetObservationsToReport(const Value: TStrings);
begin
  FObservationsToReport.Assign(Value);
end;

procedure TParetoProperties.SetParetoGroupName(const Value: string);
begin
  SetStringProperty(FParetoGroupName, Value);
end;

procedure TParetoProperties.SetParetoIncrements(const Value: Integer);
begin
  SetIntegerProperty(FParetoIncrements, Value);
end;

initialization
  TriangleRowHeightFactor := Sin(ArcCos(0.5));

end.
