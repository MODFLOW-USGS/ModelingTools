unit PestPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes, GR32, FastGEO, PestObsGroupUnit;

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
  public
    Constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    property PilotPointSpacing: double read GetPilotPointSpacing
      write SetPilotPointSpacing;
    procedure DrawPilotPoints(BitMap32: TBitmap32);
    function ShouldDrawPilotPoints: Boolean;
    property PilotPointCount: Integer read GetPilotPointCount;
    property PilotPoints[Index: Integer]: TPoint2D read GetPilotPoint;
  Published
    property PestUsed: Boolean read FPestUsed write SetPestUsed Stored True;
    property TemplateCharacter: Char read FTemplateCharacter
      write SetTemplateCharacter;
    property ExtendedTemplateCharacter: Char read FExtendedTemplateCharacter
      write SetExtendedTemplateCharacter;
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
  end;

implementation

uses
  ZoomBox2, BigCanvasMethods, frmGoPhastUnit, PhastModelUnit;

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
    TemplateCharacter := PestSource.TemplateCharacter;
    ExtendedTemplateCharacter := PestSource.ExtendedTemplateCharacter;

    PestControlData := PestSource.PestControlData;
    SvdProperties := PestSource.SvdProperties;
    LsqrProperties := PestSource.LsqrProperties;
    ObservationGroups := PestSource.ObservationGroups;
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
  FPestControlData := TPestControlData.Create(InvalidateModelEvent);
  FSvdProperties :=
    TSingularValueDecompositionProperties.Create(InvalidateModelEvent);
  FLsqrProperties := TLsqrProperties.Create(InvalidateModelEvent);
  FObservatioGroups := TPestObservationGroups.Create(Model);
  InitializeVariables;
  FStoredPilotPointSpacing.OnChange := InvalidateModelEvent;
end;

destructor TPestProperties.Destroy;
begin
  FObservatioGroups.Free;
  FLsqrProperties.Free;
  FSvdProperties.Free;
  FPestControlData.Free;
  FStoredPilotPointSpacing.Free;
  inherited;
end;

procedure TPestProperties.DrawPilotPoints(BitMap32: TBitmap32);
var
  ZoomBox: TQRbwZoomBox2;
//  DisLimits: TGridLimit;
//  RowCount: Int64;
//  ColumnCount: Int64;
//  RowIndex: Integer;
//  Y: Double;
  YInt: Integer;
//  X: Double;
  LineSegment: GoPhastTypes.TPointArray;
//  ColIndex: Integer;
  XInt: Integer;
  PilotPointIndex: Integer;
  APilotPoint: TPoint2D;
begin
  if ShouldDrawPilotPoints then
  begin
    SetLength(LineSegment, 2);
    ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    for PilotPointIndex := 0 to PilotPointCount - 1 do
    begin
      APilotPoint := PilotPoints[PilotPointIndex];
      YInt := ZoomBox.YCoord(APilotPoint.y);
      XInt := ZoomBox.XCoord(APilotPoint.x);

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

    end;
//    DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
//    FPilotPointRowCount := Trunc((DisLimits.MaxY - DisLimits.MinY)/PilotPointSpacing) + 1;
//    FPilotPointColumnCount := Trunc((DisLimits.MaxX - DisLimits.MinX)/PilotPointSpacing) + 1;
//    FLeftX := (DisLimits.MaxX + DisLimits.MinX)/2 - FPilotPointColumnCount/2*PilotPointSpacing;
//    FTopY := (DisLimits.MaxY + DisLimits.MinY)/2 + FPilotPointRowCount/2*PilotPointSpacing;
//    for RowIndex := 0 to FPilotPointRowCount do
//    begin
//      Y := FTopY - RowIndex*PilotPointSpacing;
//      YInt := ZoomBox.YCoord(Y);
//      for ColIndex := 0 to FPilotPointColumnCount do
//      begin
//        X := FLeftX + ColIndex*PilotPointSpacing;
//        XInt := ZoomBox.XCoord(X);
//
//        LineSegment[0].x := XInt;
//        LineSegment[1].x := XInt;
//        LineSegment[0].y := YInt-2;
//        LineSegment[1].y := YInt+3;
//        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
//          True, False, 0, 2);
//
//        LineSegment[0].x := XInt-2;
//        LineSegment[1].x := XInt+3;
//        LineSegment[0].y := YInt;
//        LineSegment[1].y := YInt;
//        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
//          True, False, 0, 2);
//      end;
//    end;
  end;
end;

function TPestProperties.ShouldDrawPilotPoints: Boolean;
begin
  result := PestUsed and ShowPilotPoints and (PilotPointSpacing > 0);
end;

function TPestProperties.GetPilotPoint(Index: Integer): TPoint2D;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  RowIndex := Index div FPilotPointColumnCount;
  ColIndex := Index  - RowIndex*FPilotPointColumnCount;
  result.Y := FTopY - RowIndex*PilotPointSpacing;
  result.X := FLeftX + ColIndex*PilotPointSpacing;
end;

function TPestProperties.GetPilotPointCount: Integer;
var
  DisLimits: TGridLimit;
begin
  DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
  FPilotPointRowCount := Trunc((DisLimits.MaxY - DisLimits.MinY)/PilotPointSpacing) + 2;
  FPilotPointColumnCount := Trunc((DisLimits.MaxX - DisLimits.MinX)/PilotPointSpacing) + 2;
  result := FPilotPointRowCount * FPilotPointColumnCount;
  FLeftX := (DisLimits.MaxX + DisLimits.MinX)/2 -
    (FPilotPointColumnCount-1)/2*PilotPointSpacing;
  FTopY := (DisLimits.MaxY + DisLimits.MinY)/2 +
    (FPilotPointRowCount-1)/2*PilotPointSpacing;
end;

function TPestProperties.GetPilotPointSpacing: double;
begin
  result := FStoredPilotPointSpacing.Value;
end;

procedure TPestProperties.InitializeVariables;
begin
  {$IFDEF PEST}
  // When PEST is removed, the default should be FPestUsed := False;
  FPestUsed := True;
  {$ELSE}
  FPestUsed := False;
  {$ENDIF}
  FShowPilotPoints := False;
  PilotPointSpacing := 0;
  FTemplateCharacter := '@';
  FExtendedTemplateCharacter := '%';

  FPestControlData.InitializeVariables;
  FSvdProperties.InitializeVariables;
  FLsqrProperties.InitializeVariables;

  FObservatioGroups.Clear;
end;

procedure TPestProperties.SetExtendedTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FExtendedTemplateCharacter, Value);
end;

procedure TPestProperties.SetLsqrProperties(const Value: TLsqrProperties);
begin
  FLsqrProperties.Assign(Value);
end;

procedure TPestProperties.SetObservatioGroups(
  const Value: TPestObservationGroups);
begin
  FObservatioGroups.Assign(Value);
end;

procedure TPestProperties.SetPestControlData(const Value: TPestControlData);
begin
  FPestControlData.Assign(Value);
end;

procedure TPestProperties.SetPestUsed(const Value: Boolean);
begin
  SetBooleanProperty(FPestUsed, Value);
end;

procedure TPestProperties.SetPilotPointSpacing(const Value: double);
begin
  FStoredPilotPointSpacing.Value := Value;
end;

procedure TPestProperties.SetShowPilotPoints(const Value: Boolean);
begin
  FShowPilotPoints := Value;
end;

procedure TPestProperties.SetStoredPilotPointSpacing(const Value: TRealStorage);
begin
  FStoredPilotPointSpacing.Assign(Value);
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

end.
