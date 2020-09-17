unit PestPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes, GR32;

type
  TPestRestart = (prRestart, prNoRestart);
  TPestMode = (pmEstimation, pmPrediction, pmRegularisation, pmPareto);
  TLambdaForgive = (lfForgive, lfNoForgive);
  TDerivedForgive = (dfForgive, dNoForgive);
  TUpgradeParamVectorBending = (upvbNoBending, upvbBending);
  TAutomaticUserIntervation = (auiInactive, auiActive);
  TSensitivityReuse = (srNoReuse, srReuse);
  TMakeFinalRun = (mfrNoRun, mfrRun);
  TWriteMatrix = (wmDontWrite, wmWrite);

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
    FBoundscaling: Boolean;
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
    procedure SetBoundscaling(const Value: Boolean);
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
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    // DERZEROLIM
    // minimum value = 0
    property ZeroLimit: double read GetZeroLimit write SetZeroLimit;
    // RLAMBDA1
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
    // PHIREDLAM
    property PhiReductionLambda: double read GetPhiReductionLambda
      write SetPhiReductionLambda;
    // RELPARMAX
    property RelativeMaxParamChange: double read GetRelativeMaxParamChange
      write SetRelativeMaxParamChange;
    // FACPARMAX
    property FactorMaxParamChange: double read GetFactorMaxParamChange
      write SetFactorMaxParamChange;
    // FACORIG
    // must be > 0
    property FactorOriginal: double read GetFactorOriginal
      write SetFactorOriginal;
    // PHIREDSWH
    property SwitchCriterion: double read GetSwitchCriterion
      write SetSwitchCriterion;
    // SPLITSWH
    property SplitSlopeCriterion: double read GetSplitSlopeCriterion
      write SetSplitSlopeCriterion;
    // PHIREDSTP
    property SlowConvergenceCriterion: double read GetSlowConvergenceCriterion
      write SetSlowConvergenceCriterion;
    // RELPARSTP
    property ParameterChangeConvergenceCriterion: double
      read GetParameterChangeConvergenceCriterion
      write SetParameterChangeConvergenceCriterion;
    // PHISTOPTHRESH
    property ObjectiveCriterion: double read GetObjectiveCriterion
      write SetObjectiveCriterion;
    // PHIABANDON
    property PhiAbandon: double read GetPhiAbandon write SetPhiAbandon;
  published
    property PestRestart: TPestRestart read FPestRestart write SetPestRestart;
    property PestMode: TPestMode read FPestMode write SetPestMode;
    //MAXCOMPRDIM (Minimum value = 1.
    property MaxCompressionDimension: Integer read FMaxCompressionDimension
      write SetMaxCompressionDimension;
    // DERZEROLIM
    property StoredZeroLimit: TRealStorage read FStoredZeroLimit
      write SetStoredZeroLimit;
    // RLAMBDA1
    property StoredInitalLambda: TRealStorage read FStoredInitalLambda
      write SetStoredInitalLambda;
    // RLAMFAC
    property StoredLambdaAdjustmentFactor: TRealStorage
      read FStoredLambdaAdjustmentFactor write SetStoredLambdaAdjustmentFactor;
    // PHIRATSUF
    property StoredPhiRatioSufficient: TRealStorage
      read FStoredPhiRatioSufficient write SetStoredPhiRatioSufficient;
    // PHIREDLAM
    property StoredPhiReductionLambda: TRealStorage
      read FStoredPhiReductionLambda write SetStoredPhiReductionLambda;
    // NUMLAM
    property NumberOfLambdas: Integer read FNumberOfLambdas
      write SetNumberOfLambdas;
    // JACUPDATE
    property JacobianUpdate: Integer read FJacobianUpdate
      write SetJacobianUpdate;
    // LAMFORGIVE
    property LambdaForgive: TLambdaForgive read FLambdaForgive
      write SetLambdaForgive;
    // DERFORGIVE
    property DerivedForgive: TDerivedForgive read FDerivedForgive
      write SetDerivedForgive;
    // RELPARMAX
    property StoredRelativeMaxParamChange: TRealStorage
      read FStoredRelativeMaxParamChange write SetStoredRelativeMaxParamChange;
    // FACPARMAX
    property StoredFactorMaxParamChange: TRealStorage
      read FStoredFactorMaxParamChange write SetStoredFactorMaxParamChange;
    // FACORIG
    property StoredFactorOriginal: TRealStorage
      read FStoredFactorOriginal write SetStoredFactorOriginal;
    // IBOUNDSTICK
    property BoundStick: Integer read FBoundStick write SetBoundStick;
    // UPVECBEND
    property UpgradeParamVectorBending: TUpgradeParamVectorBending
      read FUpgradeParamVectorBending write SetUpgradeParamVectorBending;
    // PHIREDSWH
    property StoredSwitchCriterion: TRealStorage read FStoredSwitchCriterion
      write SetStoredSwitchCriterion;
    // NOPTSWITCH
    property OptSwitchCount: Integer read FOptSwitchCount
      write SetOptSwitchCount;
    // SPLITSWH
    property StoredSplitSlopeCriterion: TRealStorage
      read FStoredSplitSlopeCriterion write SetStoredSplitSlopeCriterion;
    // DOAUI
    property AutomaticUserIntervation: TAutomaticUserIntervation
      read FAutomaticUserIntervation write SetAutomaticUserIntervation;
    // DOSENREUSE
    property SensitivityReuse: TSensitivityReuse read FSensitivityReuse
      write SetSensitivityReuse;
    // BOUNDSCALE
    property Boundscaling: Boolean read FBoundscaling write SetBoundscaling
      stored True;
    // NOPTMAX
    property MaxIterations: Integer read FMaxIterations write SetMaxIterations;
    // PHIREDSTP
    property StoredSlowConvergenceCriterion: TRealStorage
      read FStoredSlowConvergenceCriterion
      write SetStoredSlowConvergenceCriterion;
    // NPHISTP
    property SlowConvergenceCountCriterion: integer
      read FSlowConvergenceCountCriterion
      write SetSlowConvergenceCountCriterion;
    // NPHINORED
    property ConvergenceCountCriterion: Integer read FConvergenceCountCriterion
      write SetConvergenceCountCriterion;
    // RELPARSTP
    property StoredParameterChangeConvergenceCriterion: TRealStorage
      read FStoredParameterChangeConvergenceCriterion
      write SetStoredParameterChangeConvergenceCriterion;
    // NRELPAR
    property ParameterChangeConvergenceCount: Integer
      read FParameterChangeConvergenceCount
      write SetParameterChangeConvergenceCount;
    // PHISTOPTHRESH
    property StoredObjectiveCriterion: TRealStorage
      read FStoredObjectiveCriterion write SetStoredObjectiveCriterion;
    // LASTRUN
    property MakeFinalRun: TMakeFinalRun read FMakeFinalRun
      write SetMakeFinalRun;
    // PHIABANDON
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
  end;


  TPestProperties = class(TGoPhastPersistent)
  private
    FTemplateCharacter: Char;
    FExtendedTemplateCharacter: Char;
    FPestUsed: Boolean;
    FShowPilotPoints: Boolean;
    FStoredPilotPointSpacing: TRealStorage;
    FPestControlData: TPestControlData;
    procedure SetTemplateCharacter(const Value: Char);
    procedure SetExtendedTemplateCharacter(const Value: Char);
    function GetPilotPointSpacing: double;
    procedure SetPilotPointSpacing(const Value: double);
    procedure SetPestUsed(const Value: Boolean);
    procedure SetShowPilotPoints(const Value: Boolean);
    procedure SetStoredPilotPointSpacing(const Value: TRealStorage);
    procedure SetPestControlData(const Value: TPestControlData);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    property PilotPointSpacing: double read GetPilotPointSpacing
      write SetPilotPointSpacing;
    procedure DrawPilotPoints(BitMap32: TBitmap32);
    function ShouldDrawPilotPoints: Boolean;
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
  end;

implementation

uses
  ZoomBox2, BigCanvasMethods, frmGoPhastUnit;

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
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredPilotPointSpacing := TRealStorage.Create;
  FPestControlData := TPestControlData.Create(InvalidateModelEvent);
  InitializeVariables;
  FStoredPilotPointSpacing.OnChange := InvalidateModelEvent;
end;

destructor TPestProperties.Destroy;
begin
  FPestControlData.Free;
  FStoredPilotPointSpacing.Free;
  inherited;
end;

procedure TPestProperties.DrawPilotPoints(BitMap32: TBitmap32);
var
  ZoomBox: TQRbwZoomBox2;
  DisLimits: TGridLimit;
  RowCount: Int64;
  ColumnCount: Int64;
  RowIndex: Integer;
  LeftX: double;
  TopY: double;
  Y: Double;
  YInt: Integer;
  X: Double;
  LineSegment: GoPhastTypes.TPointArray;
  ColIndex: Integer;
  XInt: Integer;
begin
  if ShouldDrawPilotPoints then
  begin
    SetLength(LineSegment, 2);
    ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
    RowCount := Trunc((DisLimits.MaxY - DisLimits.MinY)/PilotPointSpacing) + 1;
    ColumnCount := Trunc((DisLimits.MaxX - DisLimits.MinX)/PilotPointSpacing) + 1;
    LeftX := (DisLimits.MaxX + DisLimits.MinX)/2 - ColumnCount/2*PilotPointSpacing;
    TopY := (DisLimits.MaxY + DisLimits.MinY)/2 + RowCount/2*PilotPointSpacing;
    for RowIndex := 0 to RowCount do
    begin
      Y := TopY - RowIndex*PilotPointSpacing;
      YInt := ZoomBox.YCoord(Y);
      for ColIndex := 0 to ColumnCount do
      begin
        X := LeftX + ColIndex*PilotPointSpacing;
        XInt := ZoomBox.XCoord(X);

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
    end;
  end;
end;

function TPestProperties.ShouldDrawPilotPoints: Boolean;
begin
  result := PestUsed and ShowPilotPoints and (PilotPointSpacing > 0);
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

  FPestControlData.InitializeVariables
end;

procedure TPestProperties.SetExtendedTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FExtendedTemplateCharacter, Value);
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
  FBoundscaling := True;
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

procedure TPestControlData.SetBoundscaling(const Value: Boolean);
begin
  SetBooleanProperty(FBoundscaling, Value);
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

end.
