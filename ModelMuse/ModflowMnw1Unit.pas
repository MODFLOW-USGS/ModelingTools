unit ModflowMnw1Unit;

interface

uses
  Windows, ZLib, SysUtils, Classes, ModflowBoundaryUnit, OrderedCollectionUnit,
  ModflowCellUnit, RbwParser, GoPhastTypes, FormulaManagerUnit, SubscriptionUnit;

type
  // Rw  > 0, Rw  = 0, Rw  < 0
  TMnw1ConductanceMethod = (mcmRadius, mcmFixed, mcmConductance);

  // DD
  TMnw1WaterLevelLimitType = (mwlltAbsolute, mwlltRelative);

  // QCUT, Q-%CUT:
  TMnw1PumpingLimitType = (mpltNone, mpltAbsolute, mpltPercent);

  TMnw1CellRecord = record
    Cell: TCellLocation;
    // Qdes
    DesiredPumpingRate: double;
    DesiredPumpingRateAnnotation: string;
    // QWval
    WaterQuality: double;
    WaterQualityAnnotation: string;
    // Rw
    ConductanceMethod: TMnw1ConductanceMethod;
    // Rw
    WellRadius: double;
    WellRadiusAnnotation: string;
    // Rw
    Conductance: double;
    ConductanceAnnotation: string;
    // Skin
    SkinFactor: double;
    SkinFactorAnnotation: string;
    // DD
    WaterLevelLimitType: TMnw1WaterLevelLimitType;
    // Hlim
    LimitingWaterLevel: double;
    LimitingWaterLevelAnnotation: string;
    // Href
    ReferenceElevation: double;
    ReferenceElevationAnnotation: string;
    // Iwgrp
    WaterQualityGroup: integer;
    WaterQualityGroupAnnotation: string;
    // Cp:C
    NonLinearLossCoefficient: double;
    NonLinearLossCoefficientAnnotation: string;
    // QCUT, Q-%CUT:
    PumpingLimitType: TMnw1PumpingLimitType;
    // Qfrcmn
    MinimumPumpingRate: double;
    MinimumPumpingRateAnnotation: string;
    // Qfrcmx
    MaximumPumpingRate: double;
    MaximumPumpingRateAnnotation: string;
    StartingTime: double;
    EndingTime: double;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMnw1Array = array of TMnw1CellRecord;

  TMnw1Storage = class(TCustomBoundaryStorage)
  private
    FMnw1Array: TMnw1Array;
    function GetMnw1Array: TMnw1Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Mnw1Array: TMnw1Array read GetMnw1Array;
  end;

  TMnw1Item = class (TCustomModflowBoundaryItem)
  private
    // Qdes
    FDesiredPumpingRate: TFormulaObject;
    // QWval
    FWaterQuality: TFormulaObject;
    // Rw
    FWellRadius: TFormulaObject;
    // Rw
    FConductance: TFormulaObject;
    // Skin
    FSkinFactor: TFormulaObject;
    // Hlim
    FLimitingWaterLevel: TFormulaObject;
    // Href
    FReferenceElevation: TFormulaObject;
    // Iwgrp
    FWaterQualityGroup: TFormulaObject;
    // Cp:C
    FNonLinearLossCoefficient: TFormulaObject;
    // Qfrcmn
    FMinimumPumpingRate: TFormulaObject;
    // Qfrcmx
    FMaximumPumpingRate: TFormulaObject;

    FConductanceMethod: TMnw1ConductanceMethod;
    FPumpingLimitType: TMnw1PumpingLimitType;
    FWaterLevelLimitType: TMnw1WaterLevelLimitType;
    function GetConductance: string;
    function GetDesiredPumpingRate: string;
    function GetLimitingWaterLevel: string;
    function GetMaximumPumpingRate: string;
    function GetMinimumPumpingRate: string;
    function GetNonLinearLossCoefficient: string;
    function GetReferenceElevation: string;
    function GetSkinFactor: string;
    function GetWaterQuality: string;
    function GetWaterQualityGroup: string;
    function GetWellRadius: string;
    procedure SetConductance(const Value: string);
    procedure SetConductanceMethod(const Value: TMnw1ConductanceMethod);
    procedure SetDesiredPumpingRate(const Value: string);
    procedure SetLimitingWaterLevel(const Value: string);
    procedure SetMaximumPumpingRate(const Value: string);
    procedure SetMinimumPumpingRate(const Value: string);
    procedure SetNonLinearLossCoefficient(const Value: string);
    procedure SetPumpingLimitType(const Value: TMnw1PumpingLimitType);
    procedure SetReferenceElevation(const Value: string);
    procedure SetSkinFactor(const Value: string);
    procedure SetWaterLevelLimitType(const Value: TMnw1WaterLevelLimitType);
    procedure SetWaterQuality(const Value: string);
    procedure SetWaterQualityGroup(const Value: string);
    procedure SetWellRadius(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
  published
    // Qdes
    property DesiredPumpingRate: string read GetDesiredPumpingRate
      write SetDesiredPumpingRate;
    // QWval
    property WaterQuality: string read GetWaterQuality write SetWaterQuality;
    // Rw
    property ConductanceMethod: TMnw1ConductanceMethod read FConductanceMethod write SetConductanceMethod;
    // Rw
    property WellRadius: string read GetWellRadius write SetWellRadius;
    // Rw
    property Conductance: string read GetConductance write SetConductance;
    // Skin
    property SkinFactor: string read GetSkinFactor write SetSkinFactor;
    // DD
    property WaterLevelLimitType: TMnw1WaterLevelLimitType read FWaterLevelLimitType write SetWaterLevelLimitType;
    // Hlim
    property LimitingWaterLevel: string read GetLimitingWaterLevel
      write SetLimitingWaterLevel;
    // Href
    property ReferenceElevation: string read GetReferenceElevation
      write SetReferenceElevation;
    // Iwgrp
    property WaterQualityGroup: string read GetWaterQualityGroup
      write SetWaterQualityGroup;
    // Cp:C
    property NonLinearLossCoefficient: string read GetNonLinearLossCoefficient
      write SetNonLinearLossCoefficient;
    // QCUT, Q-%CUT:
    property PumpingLimitType: TMnw1PumpingLimitType read FPumpingLimitType write SetPumpingLimitType;
    // Qfrcmn
    property MinimumPumpingRate: string read GetMinimumPumpingRate
      write SetMinimumPumpingRate;
    // Qfrcmx
    property ReactivationPumpingRate: string read GetMaximumPumpingRate
      write SetMaximumPumpingRate;
  end;

  TMnw1TimeListLink = class(TTimeListsModelLink)
  private
    FDesiredPumpingRateData: TModflowTimeList;
    FWaterQualityData: TModflowTimeList;
    FWellRadiusData: TModflowTimeList;
    FConductanceData: TModflowTimeList;
    FSkinFactorData: TModflowTimeList;
    FLimitingWaterLevelData: TModflowTimeList;
    FReferenceElevationData: TModflowTimeList;
    FWaterQualityGroupData: TModflowTimeList;
    FNonLinearLossCoefficientData: TModflowTimeList;
    FMinimumPumpingRateData: TModflowTimeList;
    FReactivationPumpingRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TMnw1WellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateDesiredPumpingRate(Sender: TObject);
    procedure InvalidateWaterQuality(Sender: TObject);
    procedure InvalidateWellRadius(Sender: TObject);
    procedure InvalidateConductance(Sender: TObject);
    procedure InvalidateSkinFactor(Sender: TObject);
    procedure InvalidateLimitingWaterLevel(Sender: TObject);
    procedure InvalidateReferenceElevation(Sender: TObject);
    procedure InvalidateWaterQualityGroup(Sender: TObject);
    procedure InvalidateNonLinearLossCoefficient(Sender: TObject);
    procedure InvalidateMinimumPumpingRate(Sender: TObject);
    procedure InvalidateMaximumPumpingRate(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TWellStorage.WellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string); override;
    procedure AssignDirectlySpecifiedValues( AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); override;
  end;

  TMnw1Cell = class(TValueCell)
  private
    Values: TMnw1CellRecord;
    StressPeriod: integer;
    FSite: String;
    function GetConductance: double;
    function GetConductanceMethod: TMnw1ConductanceMethod;
    function GetDesiredPumpingRate: double;
    function GetLimitingWaterLevel: double;
    function GetMaximumPumpingRate: double;
    function GetMinimumPumpingRate: double;
    function GetNonLinearLossCoefficient: double;
    function GetPumpingLimitType: TMnw1PumpingLimitType;
    function GetReferenceElevation: double;
    function GetSkinFactor: double;
    function GetWaterLevelLimitType: TMnw1WaterLevelLimitType;
    function GetWaterQuality: double;
    function GetWaterQualityGroup: integer;
    function GetWellRadius: double;
    function GetConductanceAnnotation: string;
    function GetDesiredPumpingRateAnnotation: string;
    function GetLimitingWaterLevelAnnotation: string;
    function GetMaximumPumpingRateAnnotation: string;
    function GetMinimumPumpingRateAnnotation: string;
    function GetNonLinearLossCoefficientAnnotation: string;
    function GetReferenceElevationAnnotation: string;
    function GetSkinFactorAnnotation: string;
    function GetWaterQualityAnnotation: string;
    function GetWaterQualityGroupAnnotation: string;
    function GetWellRadiusAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    // Qdes
    property DesiredPumpingRate: double read GetDesiredPumpingRate;
    property DesiredPumpingRateAnnotation: string read GetDesiredPumpingRateAnnotation;
    // QWval
    property WaterQuality: double read GetWaterQuality;
    property WaterQualityAnnotation: string read GetWaterQualityAnnotation;
    // Rw
    property ConductanceMethod: TMnw1ConductanceMethod read GetConductanceMethod;
    // Rw
    property WellRadius: double read GetWellRadius;
    property WellRadiusAnnotation: string read GetWellRadiusAnnotation;
    // Rw
    property Conductance: double read GetConductance;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    // Skin
    property SkinFactor: double read GetSkinFactor;
    property SkinFactorAnnotation: string read GetSkinFactorAnnotation;
    // DD
    property WaterLevelLimitType: TMnw1WaterLevelLimitType read GetWaterLevelLimitType;
    // Hlim
    property LimitingWaterLevel: double read GetLimitingWaterLevel;
    property LimitingWaterLevelAnnotation: string read GetLimitingWaterLevelAnnotation;
    // Href
    property ReferenceElevation: double read GetReferenceElevation;
    property ReferenceElevationAnnotation: string read GetReferenceElevationAnnotation;
    // Iwgrp
    property WaterQualityGroup: integer read GetWaterQualityGroup;
    property WaterQualityGroupAnnotation: string read GetWaterQualityGroupAnnotation;
    // Cp:C
    property NonLinearLossCoefficient: double read GetNonLinearLossCoefficient;
    property NonLinearLossCoefficientAnnotation: string read GetNonLinearLossCoefficientAnnotation;
    // QCUT, Q-%CUT:
    property PumpingLimitType: TMnw1PumpingLimitType read GetPumpingLimitType;
    // Qfrcmn
    property MinimumPumpingRate: double read GetMinimumPumpingRate;
    property MinimumPumpingRateAnnotation: string read GetMinimumPumpingRateAnnotation;
    // Qfrcmx
    property MaximumPumpingRate: double read GetMaximumPumpingRate;
    property MaximumPumpingRateAnnotation: string read GetMaximumPumpingRateAnnotation;
    property Site: string read FSite;
  end;

  TMnw1Boundary = class(TModflowBoundary)
  private
    FSite: string;
    procedure SetSite(const Value: string);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TWell_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  published
    // SITE: MNWsite
    property Site: string read FSite write SetSite;
    procedure Assign(Source: TPersistent); override;
  end;

const
  DesiredPumpingRatePosition = 0;
  WaterQualityPosition = 1;
  WellRadiusPosition = 2;
  ConductancePosition = 3;
  SkinFactorPosition = 4;
  LimitingWaterLevelPosition = 5;
  ReferenceElevationPosition = 6;
  WaterQualityGroupPosition = 7;
  NonLinearLossCoefficientPosition = 8;
  MinimumPumpingRatePosition = 9;
  ReactivationPumpingRatePosition = 10;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, ScreenObjectUnit, ModflowTimeUnit,
  GIS_Functions;

resourcestring
  StrDesiredPumpingRate = 'Desired pumping rate';
  StrWaterQuality = 'Water quality';
  StrWellRadius = 'Well radius';
  StrSkinFactor = 'Skin factor';
  StrLimitingWaterLevel = 'Limiting water level';
  StrReferenceElevation = 'Reference Elevation';
  StrWaterQualityGroup = 'Water quality group';
  StrNonlinearLossSoef = 'Non-linear loss soefficient';
  StrMinimumPumpingRate = 'Minimum pumping rate';
  StrReactivationPumpingRate = 'Reactivation pumping rate;';

{ TMnw1Item }

procedure TMnw1Item.Assign(Source: TPersistent);
var
  MnwSource: TMnw1Item;
begin
  if Source is TMnw1Item then
  begin
    MnwSource := TMnw1Item(Source);
    DesiredPumpingRate := MnwSource.DesiredPumpingRate;
    WaterQuality := MnwSource.WaterQuality;
    ConductanceMethod := MnwSource.ConductanceMethod;
    WellRadius := MnwSource.WellRadius;
    Conductance := MnwSource.Conductance;
    SkinFactor := MnwSource.SkinFactor;
    WaterLevelLimitType := MnwSource.WaterLevelLimitType;
    LimitingWaterLevel := MnwSource.LimitingWaterLevel;
    ReferenceElevation := MnwSource.ReferenceElevation;
    WaterQualityGroup := MnwSource.WaterQualityGroup;
    NonLinearLossCoefficient := MnwSource.NonLinearLossCoefficient;
    PumpingLimitType := MnwSource.PumpingLimitType;
    MinimumPumpingRate := MnwSource.MinimumPumpingRate;
    ReactivationPumpingRate := MnwSource.ReactivationPumpingRate;
  end;
  inherited;
end;

procedure TMnw1Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMnw1WellCollection;
  DesiredPumpingRateObserver: TObserver;
  WaterQualityObserver: TObserver;
  WellRadiusObserver: TObserver;
  ConductanceRateObserver: TObserver;
  SkinFactorObserver: TObserver;
  LimitingWaterLevelRateObserver: TObserver;
  ReferenceElevationObserver: TObserver;
  WaterQualityGroupObserver: TObserver;
  NonLinearLossCoefficientObserver: TObserver;
  MinimumPumpingRateObserver: TObserver;
  MaximumPumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TMnw1WellCollection;

  DesiredPumpingRateObserver := FObserverList[DesiredPumpingRatePosition];
  DesiredPumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateDesiredPumpingRate;

  WaterQualityObserver := FObserverList[WaterQualityPosition];
  WaterQualityObserver.OnUpToDateSet := ParentCollection.InvalidateWaterQuality;

  WellRadiusObserver := FObserverList[WellRadiusPosition];
  WellRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateWellRadius;

  ConductanceRateObserver := FObserverList[ConductancePosition];
  ConductanceRateObserver.OnUpToDateSet := ParentCollection.InvalidateConductance;

  SkinFactorObserver := FObserverList[SkinFactorPosition];
  SkinFactorObserver.OnUpToDateSet := ParentCollection.InvalidateSkinFactor;

  LimitingWaterLevelRateObserver := FObserverList[LimitingWaterLevelPosition];
  LimitingWaterLevelRateObserver.OnUpToDateSet := ParentCollection.InvalidateLimitingWaterLevel;

  ReferenceElevationObserver := FObserverList[ReferenceElevationPosition];
  ReferenceElevationObserver.OnUpToDateSet := ParentCollection.InvalidateReferenceElevation;

  WaterQualityGroupObserver := FObserverList[WaterQualityGroupPosition];
  WaterQualityGroupObserver.OnUpToDateSet := ParentCollection.InvalidateWaterQualityGroup;

  NonLinearLossCoefficientObserver := FObserverList[NonLinearLossCoefficientPosition];
  NonLinearLossCoefficientObserver.OnUpToDateSet := ParentCollection.InvalidateNonLinearLossCoefficient;

  MinimumPumpingRateObserver := FObserverList[MinimumPumpingRatePosition];
  MinimumPumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateMinimumPumpingRate;

  MaximumPumpingRateObserver := FObserverList[ReactivationPumpingRatePosition];
  MaximumPumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateMaximumPumpingRate;
end;

function TMnw1Item.BoundaryFormulaCount: integer;
begin
  result := 11;
end;

procedure TMnw1Item.CreateFormulaObjects;
begin
  FDesiredPumpingRate := CreateFormulaObject(dso3D);
  FWaterQuality := CreateFormulaObject(dso3D);
  FWellRadius := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FSkinFactor := CreateFormulaObject(dso3D);
  FLimitingWaterLevel := CreateFormulaObject(dso3D);
  FReferenceElevation := CreateFormulaObject(dso3D);
  FWaterQualityGroup := CreateFormulaObject(dso3D);
  FNonLinearLossCoefficient := CreateFormulaObject(dso3D);
  FMinimumPumpingRate := CreateFormulaObject(dso3D);
  FMaximumPumpingRate := CreateFormulaObject(dso3D);

end;

destructor TMnw1Item.Destroy;
begin
  DesiredPumpingRate := '0';
  WaterQuality := '0';
  WellRadius := '0';
  Conductance := '0';
  SkinFactor := '0';
  LimitingWaterLevel := '0';
  ReferenceElevation := '0';
  WaterQualityGroup := '0';
  NonLinearLossCoefficient := '0';
  MinimumPumpingRate := '0';
  ReactivationPumpingRate := '0';
  inherited;
end;

function TMnw1Item.GetBoundaryFormula(Index: integer): string;
begin
  result := '';
  case Index of
    DesiredPumpingRatePosition: result := DesiredPumpingRate;
    WaterQualityPosition: result := WaterQuality;
    WellRadiusPosition: result := WellRadius;
    ConductancePosition: result := Conductance;
    SkinFactorPosition: result := SkinFactor;
    LimitingWaterLevelPosition: result := LimitingWaterLevel;
    ReferenceElevationPosition: result := ReferenceElevation;
    WaterQualityGroupPosition: result := WaterQualityGroup;
    NonLinearLossCoefficientPosition: result := NonLinearLossCoefficient;
    MinimumPumpingRatePosition: result := MinimumPumpingRate;
    ReactivationPumpingRatePosition: result := ReactivationPumpingRate;
    else Assert(False);
  end;
end;

function TMnw1Item.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

function TMnw1Item.GetDesiredPumpingRate: string;
begin
  Result := FDesiredPumpingRate.Formula;
  ResetItemObserver(DesiredPumpingRatePosition);
end;

function TMnw1Item.GetLimitingWaterLevel: string;
begin
  Result := FLimitingWaterLevel.Formula;
  ResetItemObserver(LimitingWaterLevelPosition);
end;

function TMnw1Item.GetMaximumPumpingRate: string;
begin
  Result := FMaximumPumpingRate.Formula;
  ResetItemObserver(ReactivationPumpingRatePosition);
end;

function TMnw1Item.GetMinimumPumpingRate: string;
begin
  Result := FMinimumPumpingRate.Formula;
  ResetItemObserver(MinimumPumpingRatePosition);
end;

function TMnw1Item.GetNonLinearLossCoefficient: string;
begin
  Result := FNonLinearLossCoefficient.Formula;
  ResetItemObserver(NonLinearLossCoefficientPosition);
end;

procedure TMnw1Item.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FDesiredPumpingRate then
  begin
    List.Add(FObserverList[DesiredPumpingRatePosition]);
  end;
  if Sender = FWaterQuality then
  begin
    List.Add(FObserverList[WaterQualityPosition]);
  end;
  if Sender = FWellRadius then
  begin
    List.Add(FObserverList[WellRadiusPosition]);
  end;
  if Sender = FConductance then
  begin
    List.Add(FObserverList[ConductancePosition]);
  end;
  if Sender = FSkinFactor then
  begin
    List.Add(FObserverList[SkinFactorPosition]);
  end;
  if Sender = FLimitingWaterLevel then
  begin
    List.Add(FObserverList[LimitingWaterLevelPosition]);
  end;
  if Sender = FReferenceElevation then
  begin
    List.Add(FObserverList[ReferenceElevationPosition]);
  end;
  if Sender = FWaterQualityGroup then
  begin
    List.Add(FObserverList[WaterQualityGroupPosition]);
  end;
  if Sender = FNonLinearLossCoefficient then
  begin
    List.Add(FObserverList[NonLinearLossCoefficientPosition]);
  end;
  if Sender = FMinimumPumpingRate then
  begin
    List.Add(FObserverList[MinimumPumpingRatePosition]);
  end;
  if Sender = FMaximumPumpingRate then
  begin
    List.Add(FObserverList[ReactivationPumpingRatePosition]);
  end;
end;

function TMnw1Item.GetReferenceElevation: string;
begin
  Result := FReferenceElevation.Formula;
  ResetItemObserver(ReferenceElevationPosition);
end;

function TMnw1Item.GetSkinFactor: string;
begin
  Result := FSkinFactor.Formula;
  ResetItemObserver(SkinFactorPosition);
end;

function TMnw1Item.GetWaterQuality: string;
begin
  Result := FWaterQuality.Formula;
  ResetItemObserver(WaterQualityPosition);
end;

function TMnw1Item.GetWaterQualityGroup: string;
begin
  Result := FWaterQualityGroup.Formula;
  ResetItemObserver(WaterQualityGroupPosition);
end;

function TMnw1Item.GetWellRadius: string;
begin
  Result := FWellRadius.Formula;
  ResetItemObserver(WellRadiusPosition);
end;

procedure TMnw1Item.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMnw1DesiredPumpingRate(Self);
    PhastModel.InvalidateMnw1WaterQuality(Self);
    PhastModel.InvalidateMnw1WellRadius(Self);
    PhastModel.InvalidateMnw1Conductance(Self);
    PhastModel.InvalidateMnw1SkinFactor(Self);
    PhastModel.InvalidateMnw1LimitingWaterLevel(Self);
    PhastModel.InvalidateMnw1ReferenceElevation(Self);
    PhastModel.InvalidateMnw1WaterQualityGroup(Self);
    PhastModel.InvalidateMnw1NonLinearLossCoefficient(Self);
    PhastModel.InvalidateMnw1MinimumPumpingRate(Self);
    PhastModel.InvalidateMnw1ReactivationPumpingRate(Self);
  end;
end;

function TMnw1Item.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMnw1Item;
begin
  result := (AnotherItem is TMnw1Item) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TMnw1Item(AnotherItem);
    result := (Item.DesiredPumpingRate = DesiredPumpingRate)
      and (Item.WaterQuality = WaterQuality)
      and (Item.ConductanceMethod = ConductanceMethod)
      and (Item.WellRadius = WellRadius)
      and (Item.Conductance = Conductance)
      and (Item.SkinFactor = SkinFactor)
      and (Item.WaterLevelLimitType = WaterLevelLimitType)
      and (Item.LimitingWaterLevel = LimitingWaterLevel)
      and (Item.ReferenceElevation = ReferenceElevation)
      and (Item.WaterQualityGroup = WaterQualityGroup)
      and (Item.NonLinearLossCoefficient = NonLinearLossCoefficient)
      and (Item.PumpingLimitType = PumpingLimitType)
      and (Item.MinimumPumpingRate = MinimumPumpingRate)
      and (Item.ReactivationPumpingRate = ReactivationPumpingRate);
  end;
end;

procedure TMnw1Item.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FDesiredPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWaterQuality,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWellRadius,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinFactor,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FLimitingWaterLevel,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReferenceElevation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWaterQualityGroup,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FNonLinearLossCoefficient,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMinimumPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMaximumPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWaterQuality,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWaterQuality,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TMnw1Item.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    DesiredPumpingRatePosition:
      DesiredPumpingRate := Value;
    WaterQualityPosition:
      WaterQuality := Value;
    WellRadiusPosition:
      WellRadius := Value;
    ConductancePosition:
      Conductance := Value;
    SkinFactorPosition:
      SkinFactor := Value;
    LimitingWaterLevelPosition:
      LimitingWaterLevel := Value;
    ReferenceElevationPosition:
      ReferenceElevation := Value;
    WaterQualityGroupPosition:
      WaterQualityGroup := Value;
    NonLinearLossCoefficientPosition:
      NonLinearLossCoefficient := Value;
    MinimumPumpingRatePosition:
      MinimumPumpingRate := Value;
    ReactivationPumpingRatePosition:
      ReactivationPumpingRate := Value;
    else Assert(False);
  end;

end;

procedure TMnw1Item.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConductancePosition, FConductance);
end;

procedure TMnw1Item.SetConductanceMethod(const Value: TMnw1ConductanceMethod);
begin
  FConductanceMethod := Value;
end;

procedure TMnw1Item.SetDesiredPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, DesiredPumpingRatePosition, FDesiredPumpingRate);
end;

procedure TMnw1Item.SetLimitingWaterLevel(const Value: string);
begin
  UpdateFormulaBlocks(Value, LimitingWaterLevelPosition, FLimitingWaterLevel);
end;

procedure TMnw1Item.SetMaximumPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, ReactivationPumpingRatePosition, FMaximumPumpingRate);
end;

procedure TMnw1Item.SetMinimumPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, MinimumPumpingRatePosition, FMinimumPumpingRate);
end;

procedure TMnw1Item.SetNonLinearLossCoefficient(const Value: string);
begin
  UpdateFormulaBlocks(Value, NonLinearLossCoefficientPosition, FNonLinearLossCoefficient);
end;

procedure TMnw1Item.SetPumpingLimitType(const Value: TMnw1PumpingLimitType);
begin
  FPumpingLimitType := Value;
end;

procedure TMnw1Item.SetReferenceElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, ReferenceElevationPosition, FReferenceElevation);
end;

procedure TMnw1Item.SetSkinFactor(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinFactorPosition, FSkinFactor);
end;

procedure TMnw1Item.SetWaterLevelLimitType(
  const Value: TMnw1WaterLevelLimitType);
begin
  FWaterLevelLimitType := Value;
end;

procedure TMnw1Item.SetWaterQuality(const Value: string);
begin
  UpdateFormulaBlocks(Value, WaterQualityPosition, FWaterQuality);
end;

procedure TMnw1Item.SetWaterQualityGroup(const Value: string);
begin
  UpdateFormulaBlocks(Value, WaterQualityGroupPosition, FWaterQualityGroup);
end;

procedure TMnw1Item.SetWellRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, WellRadiusPosition, FWellRadius);
end;

{ TMnw1CellRecord }

procedure TMnw1CellRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, DesiredPumpingRate);
  WriteCompInt(Comp, Strings.IndexOf(DesiredPumpingRateAnnotation));
  WriteCompReal(Comp, WaterQuality);
  WriteCompInt(Comp, Strings.IndexOf(WaterQualityAnnotation));
  WriteCompInt(Comp, Ord(ConductanceMethod));
  WriteCompReal(Comp, WellRadius);
  WriteCompInt(Comp, Strings.IndexOf(WellRadiusAnnotation));
  WriteCompReal(Comp, Conductance);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompReal(Comp, SkinFactor);
  WriteCompInt(Comp, Strings.IndexOf(SkinFactorAnnotation));
  WriteCompInt(Comp, Ord(WaterLevelLimitType));
  WriteCompReal(Comp, LimitingWaterLevel);
  WriteCompInt(Comp, Strings.IndexOf(LimitingWaterLevelAnnotation));
  WriteCompReal(Comp, ReferenceElevation);
  WriteCompInt(Comp, Strings.IndexOf(ReferenceElevationAnnotation));
  WriteCompInt(Comp, WaterQualityGroup);
  WriteCompInt(Comp, Strings.IndexOf(WaterQualityGroupAnnotation));
  WriteCompReal(Comp, NonLinearLossCoefficient);
  WriteCompInt(Comp, Strings.IndexOf(NonLinearLossCoefficientAnnotation));
  WriteCompInt(Comp, Ord(PumpingLimitType));
  WriteCompReal(Comp, MinimumPumpingRate);
  WriteCompInt(Comp, Strings.IndexOf(MinimumPumpingRateAnnotation));
  WriteCompReal(Comp, MaximumPumpingRate);
  WriteCompInt(Comp, Strings.IndexOf(MaximumPumpingRateAnnotation));
end;

procedure TMnw1CellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(DesiredPumpingRateAnnotation);
  Strings.Add(WaterQualityAnnotation);
  Strings.Add(WellRadiusAnnotation);
  Strings.Add(ConductanceAnnotation);
  Strings.Add(SkinFactorAnnotation);
  Strings.Add(LimitingWaterLevelAnnotation);
  Strings.Add(ReferenceElevationAnnotation);
  Strings.Add(WaterQualityGroupAnnotation);
  Strings.Add(NonLinearLossCoefficientAnnotation);
  Strings.Add(MinimumPumpingRateAnnotation);
  Strings.Add(MaximumPumpingRateAnnotation);
end;

procedure TMnw1CellRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  DesiredPumpingRate := ReadCompReal(Decomp);
  DesiredPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];

  WaterQuality := ReadCompReal(Decomp);
  WaterQualityAnnotation := Annotations[ReadCompInt(Decomp)];

  ConductanceMethod := TMnw1ConductanceMethod(ReadCompInt(Decomp));

  WellRadius := ReadCompReal(Decomp);
  WellRadiusAnnotation := Annotations[ReadCompInt(Decomp)];

  Conductance := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];

  SkinFactor := ReadCompReal(Decomp);
  SkinFactorAnnotation := Annotations[ReadCompInt(Decomp)];

  WaterLevelLimitType := TMnw1WaterLevelLimitType(ReadCompInt(Decomp));

  LimitingWaterLevel := ReadCompReal(Decomp);
  LimitingWaterLevelAnnotation := Annotations[ReadCompInt(Decomp)];

  ReferenceElevation := ReadCompReal(Decomp);
  ReferenceElevationAnnotation := Annotations[ReadCompInt(Decomp)];

  WaterQualityGroup := ReadCompInt(Decomp);
  WaterQualityGroupAnnotation := Annotations[ReadCompInt(Decomp)];

  NonLinearLossCoefficient := ReadCompReal(Decomp);
  NonLinearLossCoefficientAnnotation := Annotations[ReadCompInt(Decomp)];

  PumpingLimitType := TMnw1PumpingLimitType(ReadCompInt(Decomp));

  MinimumPumpingRate := ReadCompReal(Decomp);
  MinimumPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];

  MaximumPumpingRate := ReadCompReal(Decomp);
  MaximumPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];
end;


{ TMnw1Storage }

procedure TMnw1Storage.Clear;
begin
  SetLength(FMnw1Array, 0);
  FCleared := True;
end;

function TMnw1Storage.GetMnw1Array: TMnw1Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMnw1Array;
end;

procedure TMnw1Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMnw1Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FMnw1Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMnw1Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FMnw1Array);
    for Index := 0 to Count - 1 do
    begin
      FMnw1Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMnw1Array[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TMnw1TimeListLink }

procedure TMnw1TimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FDesiredPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FDesiredPumpingRateData.NonParamDescription := StrDesiredPumpingRate;
  FDesiredPumpingRateData.ParamDescription := ' ' + LowerCase(StrDesiredPumpingRate);
  AddTimeList(FDesiredPumpingRateData);

  FWaterQualityData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWaterQualityData.NonParamDescription := StrWaterQuality;
  FWaterQualityData.ParamDescription := ' ' + LowerCase(StrWaterQuality);
  AddTimeList(FWaterQualityData);

  FWellRadiusData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWellRadiusData.NonParamDescription := StrWellRadius;
  FWellRadiusData.ParamDescription := ' ' + LowerCase(StrWellRadius);
  AddTimeList(FWellRadiusData);

  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := ' ' + LowerCase(StrConductance);
  AddTimeList(FConductanceData);

  FSkinFactorData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSkinFactorData.NonParamDescription := StrSkinFactor;
  FSkinFactorData.ParamDescription := ' ' + LowerCase(StrSkinFactor);
  AddTimeList(FSkinFactorData);

  FLimitingWaterLevelData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLimitingWaterLevelData.NonParamDescription := StrLimitingWaterLevel;
  FLimitingWaterLevelData.ParamDescription := ' ' + LowerCase(StrSkinFactor);
  AddTimeList(FLimitingWaterLevelData);

  FReferenceElevationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReferenceElevationData.NonParamDescription := StrReferenceElevation;
  FReferenceElevationData.ParamDescription := ' ' + LowerCase(StrReferenceElevation);
  AddTimeList(FReferenceElevationData);

  FWaterQualityGroupData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWaterQualityGroupData.NonParamDescription := StrWaterQualityGroup;
  FWaterQualityGroupData.ParamDescription := ' ' + LowerCase(StrWaterQualityGroup);
  AddTimeList(FWaterQualityGroupData);

  FNonLinearLossCoefficientData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FNonLinearLossCoefficientData.NonParamDescription := StrNonlinearLossSoef;
  FNonLinearLossCoefficientData.ParamDescription := ' ' + LowerCase(StrNonlinearLossSoef);
  AddTimeList(FNonLinearLossCoefficientData);

  FMinimumPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMinimumPumpingRateData.NonParamDescription := StrMinimumPumpingRate;
  FMinimumPumpingRateData.ParamDescription := ' ' + LowerCase(StrMinimumPumpingRate);
  AddTimeList(FMinimumPumpingRateData);

  FReactivationPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReactivationPumpingRateData.NonParamDescription := StrReactivationPumpingRate;
  FReactivationPumpingRateData.ParamDescription := ' ' + LowerCase(StrReactivationPumpingRate);
  AddTimeList(FReactivationPumpingRateData);

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FDesiredPumpingRateData.OnInvalidate := LocalModel.InvalidateMnw1DesiredPumpingRate;
    FWaterQualityData.OnInvalidate := LocalModel.InvalidateMnw1WaterQuality;
    FWellRadiusData.OnInvalidate := LocalModel.InvalidateMnw1WellRadius;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMnw1Conductance;
    FSkinFactorData.OnInvalidate := LocalModel.InvalidateMnw1SkinFactor;
    FLimitingWaterLevelData.OnInvalidate := LocalModel.InvalidateMnw1LimitingWaterLevel;
    FReferenceElevationData.OnInvalidate := LocalModel.InvalidateMnw1ReferenceElevation;
    FWaterQualityGroupData.OnInvalidate := LocalModel.InvalidateMnw1WaterQualityGroup;
    FNonLinearLossCoefficientData.OnInvalidate := LocalModel.InvalidateMnw1NonLinearLossCoefficient;
    FMinimumPumpingRateData.OnInvalidate := LocalModel.InvalidateMnw1MinimumPumpingRate;
    FReactivationPumpingRateData.OnInvalidate := LocalModel.InvalidateMnw1ReactivationPumpingRate;
  end;
end;

destructor TMnw1TimeListLink.Destroy;
begin
  FDesiredPumpingRateData.Free;
  FWaterQualityData.Free;
  FWellRadiusData.Free;
  FConductanceData.Free;
  FSkinFactorData.Free;
  FLimitingWaterLevelData.Free;
  FReferenceElevationData.Free;
  FWaterQualityGroupData.Free;
  FNonLinearLossCoefficientData.Free;
  FMinimumPumpingRateData.Free;
  FReactivationPumpingRateData.Free;

  inherited;
end;

{ TMnw1WellCollection }

procedure TMnw1WellCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TMnw1Storage.Create(AModel));
end;

function TMnw1WellCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TMnw1Item;
begin
  Item := Items[ItemIndex] as TMnw1Item;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TMnw1WellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject;
  PestName: string);
var
  Mnw1Storage: TMnw1Storage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
        { TODO -cPEST : Add PEST support for PEST here }
        // record PEST parameter name if present.
        // record PEST DataArray name if present.
        // cache and restore PEST data.
  Assert(BoundaryFunctionIndex in
    [DesiredPumpingRatePosition..ReactivationPumpingRatePosition]);
  Assert(Expression <> nil);

  Mnw1Storage := BoundaryStorage as TMnw1Storage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with Mnw1Storage.Mnw1Array[Index] do
    begin
      case BoundaryFunctionIndex of
        DesiredPumpingRatePosition:
          begin
            DesiredPumpingRate := Expression.DoubleResult;
            DesiredPumpingRateAnnotation := ACell.Annotation;
          end;
        WaterQualityPosition:
          begin
            WaterQuality := Expression.DoubleResult;
            WaterQualityAnnotation := ACell.Annotation;
          end;
        WellRadiusPosition:
          begin
            WellRadius := Expression.DoubleResult;
            WellRadiusAnnotation := ACell.Annotation;
          end;
        ConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
          end;
        SkinFactorPosition:
          begin
            SkinFactor := Expression.DoubleResult;
            SkinFactorAnnotation := ACell.Annotation;
          end;
        LimitingWaterLevelPosition:
          begin
            LimitingWaterLevel := Expression.DoubleResult;
            LimitingWaterLevelAnnotation := ACell.Annotation;
          end;
        ReferenceElevationPosition:
          begin
            ReferenceElevation := Expression.DoubleResult;
            ReferenceElevationAnnotation := ACell.Annotation;
          end;
        WaterQualityGroupPosition:
          begin
            WaterQualityGroup := Expression.IntegerResult;
            WaterQualityGroupAnnotation := ACell.Annotation;
          end;
        NonLinearLossCoefficientPosition:
          begin
            NonLinearLossCoefficient := Expression.DoubleResult;
            NonLinearLossCoefficientAnnotation := ACell.Annotation;
          end;
        MinimumPumpingRatePosition:
          begin
            MinimumPumpingRate := Expression.DoubleResult;
            MinimumPumpingRateAnnotation := ACell.Annotation;
          end;
        ReactivationPumpingRatePosition:
          begin
            MaximumPumpingRate := Expression.DoubleResult;
            MaximumPumpingRateAnnotation := ACell.Annotation;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TMnw1WellCollection.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem;
  BoundaryStorage: TCustomBoundaryStorage);
var
  Mnw1Storage: TMnw1Storage;
  index: integer;
  Mnw1Item: TMnw1Item;
begin
  inherited;
  Mnw1Storage := BoundaryStorage as TMnw1Storage;
  Mnw1Item := AnItem as TMnw1Item;

  for index := 0 to Length(Mnw1Storage.FMnw1Array) - 1 do
  begin
    Mnw1Storage.FMnw1Array[index].ConductanceMethod := Mnw1Item.ConductanceMethod;
    Mnw1Storage.FMnw1Array[index].WaterLevelLimitType := Mnw1Item.WaterLevelLimitType;
    Mnw1Storage.FMnw1Array[index].PumpingLimitType := Mnw1Item.PumpingLimitType;
  end;
end;

procedure TMnw1WellCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  Mnw1Storage: TMnw1Storage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Mnw1Storage := BoundaryStorage as TMnw1Storage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with Mnw1Storage.Mnw1Array[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TMnw1WellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMnw1TimeListLink;
end;

procedure TMnw1WellCollection.InvalidateConductance(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FConductanceData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateDesiredPumpingRate(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FDesiredPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FDesiredPumpingRateData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateLimitingWaterLevel(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FLimitingWaterLevelData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FLimitingWaterLevelData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateMaximumPumpingRate(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FReactivationPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FReactivationPumpingRateData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateMinimumPumpingRate(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FMinimumPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FMinimumPumpingRateData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMnw1DesiredPumpingRate(Self);
    PhastModel.InvalidateMnw1WaterQuality(Self);
    PhastModel.InvalidateMnw1WellRadius(Self);
    PhastModel.InvalidateMnw1Conductance(Self);
    PhastModel.InvalidateMnw1SkinFactor(Self);
    PhastModel.InvalidateMnw1LimitingWaterLevel(Self);
    PhastModel.InvalidateMnw1ReferenceElevation(Self);
    PhastModel.InvalidateMnw1WaterQualityGroup(Self);
    PhastModel.InvalidateMnw1NonLinearLossCoefficient(Self);
    PhastModel.InvalidateMnw1MinimumPumpingRate(Self);
    PhastModel.InvalidateMnw1ReactivationPumpingRate(Self);
  end;
end;

procedure TMnw1WellCollection.InvalidateNonLinearLossCoefficient(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FNonLinearLossCoefficientData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FNonLinearLossCoefficientData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateReferenceElevation(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FReferenceElevationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FReferenceElevationData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateSkinFactor(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FSkinFactorData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FSkinFactorData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateWaterQuality(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FWaterQualityData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FWaterQualityData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateWaterQualityGroup(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FWaterQualityGroupData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FWaterQualityGroupData.Invalidate;
    end;
  end;
end;

procedure TMnw1WellCollection.InvalidateWellRadius(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw1TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMnw1TimeListLink;
    Link.FWellRadiusData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw1TimeListLink;
      Link.FWellRadiusData.Invalidate;
    end;
  end;
end;

class function TMnw1WellCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMnw1Item;
end;

procedure TMnw1WellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TMnw1Storage).FMnw1Array, BoundaryCount);
  inherited;
end;

{ TMnw1Cell }

procedure TMnw1Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
  WriteCompString(Comp, Site);
end;

function TMnw1Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TMnw1Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TMnw1Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TMnw1Cell.GetConductanceMethod: TMnw1ConductanceMethod;
begin
  result := Values.ConductanceMethod;
end;

function TMnw1Cell.GetDesiredPumpingRate: double;
begin
  result := Values.DesiredPumpingRate;
end;

function TMnw1Cell.GetDesiredPumpingRateAnnotation: string;
begin
  result := Values.DesiredPumpingRateAnnotation;
end;

function TMnw1Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    WaterQualityGroupPosition: result := WaterQualityGroupAnnotation;
    else Assert(False);
  end;
end;

function TMnw1Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    WaterQualityGroupPosition: result := WaterQualityGroup;
    else Assert(False);
  end;
end;

function TMnw1Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TMnw1Cell.GetLimitingWaterLevel: double;
begin
  result := Values.LimitingWaterLevel;
end;

function TMnw1Cell.GetLimitingWaterLevelAnnotation: string;
begin
  result := Values.LimitingWaterLevelAnnotation;
end;

function TMnw1Cell.GetMaximumPumpingRate: double;
begin
  result := Values.MaximumPumpingRate;
end;

function TMnw1Cell.GetMaximumPumpingRateAnnotation: string;
begin
  result := Values.MaximumPumpingRateAnnotation;
end;

function TMnw1Cell.GetMinimumPumpingRate: double;
begin
  result := Values.MinimumPumpingRate;
end;

function TMnw1Cell.GetMinimumPumpingRateAnnotation: string;
begin
  result := Values.MinimumPumpingRateAnnotation;
end;

function TMnw1Cell.GetNonLinearLossCoefficient: double;
begin
  result := Values.NonLinearLossCoefficient;
end;

function TMnw1Cell.GetNonLinearLossCoefficientAnnotation: string;
begin
  result := Values.NonLinearLossCoefficientAnnotation;
end;

function TMnw1Cell.GetPumpingLimitType: TMnw1PumpingLimitType;
begin
  result := Values.PumpingLimitType;
end;

function TMnw1Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    DesiredPumpingRatePosition: result := DesiredPumpingRateAnnotation;
    WaterQualityPosition: result := WaterQualityAnnotation;
    WellRadiusPosition: result := WellRadiusAnnotation;
    ConductancePosition: result := ConductanceAnnotation;
    SkinFactorPosition: result := SkinFactorAnnotation;
    LimitingWaterLevelPosition: result := LimitingWaterLevelAnnotation;
    ReferenceElevationPosition: result := ReferenceElevationAnnotation;
    WaterQualityGroupPosition: result := WaterQualityGroupAnnotation;
    NonLinearLossCoefficientPosition: result := NonLinearLossCoefficientAnnotation;
    MinimumPumpingRatePosition: result := MinimumPumpingRateAnnotation;
    ReactivationPumpingRatePosition: result := MaximumPumpingRateAnnotation;
    else Assert(False);
  end;
end;

function TMnw1Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    DesiredPumpingRatePosition: result := DesiredPumpingRate;
    WaterQualityPosition: result := WaterQuality;
    WellRadiusPosition: result := WellRadius;
    ConductancePosition: result := Conductance;
    SkinFactorPosition: result := SkinFactor;
    LimitingWaterLevelPosition: result := LimitingWaterLevel;
    ReferenceElevationPosition: result := ReferenceElevation;
    WaterQualityGroupPosition: result := WaterQualityGroup;
    NonLinearLossCoefficientPosition: result := NonLinearLossCoefficient;
    MinimumPumpingRatePosition: result := MinimumPumpingRate;
    ReactivationPumpingRatePosition: result := MaximumPumpingRate;
    else Assert(False);
  end;
end;

function TMnw1Cell.GetReferenceElevation: double;
begin
  result := Values.ReferenceElevation;
end;

function TMnw1Cell.GetReferenceElevationAnnotation: string;
begin
  result := Values.ReferenceElevationAnnotation;
end;

function TMnw1Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TMnw1Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TMnw1Cell.GetSkinFactor: double;
begin
  result := Values.SkinFactor;
end;

function TMnw1Cell.GetSkinFactorAnnotation: string;
begin
  result := Values.SkinFactorAnnotation;
end;

function TMnw1Cell.GetWaterLevelLimitType: TMnw1WaterLevelLimitType;
begin
  result := Values.WaterLevelLimitType;
end;

function TMnw1Cell.GetWaterQuality: double;
begin
  result := Values.WaterQuality;
end;

function TMnw1Cell.GetWaterQualityAnnotation: string;
begin
  result := Values.WaterQualityAnnotation;
end;

function TMnw1Cell.GetWaterQualityGroup: integer;
begin
  result := Values.WaterQualityGroup;
end;

function TMnw1Cell.GetWaterQualityGroupAnnotation: string;
begin
  result := Values.WaterQualityGroupAnnotation;
end;

function TMnw1Cell.GetWellRadius: double;
begin
  result := Values.WellRadius;
end;

function TMnw1Cell.GetWellRadiusAnnotation: string;
begin
  result := Values.WellRadiusAnnotation;
end;

function TMnw1Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Mnw1Cell: TMnw1Cell;
begin
  result := AnotherCell is TMnw1Cell;
  if result then
  begin
    Mnw1Cell := TMnw1Cell(AnotherCell);
    result :=
      (DesiredPumpingRate = Mnw1Cell.DesiredPumpingRate)
      and (WaterQuality = Mnw1Cell.WaterQuality)
      and (ConductanceMethod = Mnw1Cell.ConductanceMethod)
      and (WellRadius = Mnw1Cell.WellRadius)
      and (Conductance = Mnw1Cell.Conductance)
      and (SkinFactor = Mnw1Cell.SkinFactor)
      and (WaterLevelLimitType = Mnw1Cell.WaterLevelLimitType)
      and (LimitingWaterLevel = Mnw1Cell.LimitingWaterLevel)
      and (ReferenceElevation = Mnw1Cell.ReferenceElevation)
      and (WaterQualityGroup = Mnw1Cell.WaterQualityGroup)
      and (NonLinearLossCoefficient = Mnw1Cell.NonLinearLossCoefficient)
      and (PumpingLimitType = Mnw1Cell.PumpingLimitType)
      and (MinimumPumpingRate = Mnw1Cell.MinimumPumpingRate)
      and (MaximumPumpingRate = Mnw1Cell.MaximumPumpingRate)
      and (IFace = Mnw1Cell.IFace)
      and (Site = Mnw1Cell.Site);
  end;
end;

procedure TMnw1Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TMnw1Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
  FSite := ReadCompStringSimple(Decomp);
end;

procedure TMnw1Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TMnw1Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TMnw1Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TMnw1Boundary }

procedure TMnw1Boundary.Assign(Source: TPersistent);
begin
  if Source is TMnw1Boundary then
  begin
    Site := TMnw1Boundary(Source).Site;
  end;
  inherited;
end;

procedure TMnw1Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TMnw1Cell;
  BoundaryValues: TMnw1CellRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMnw1Storage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TMnw1Storage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TMnw1Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.Mnw1Array) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.Mnw1Array)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Mnw1Array) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.Mnw1Array[BoundaryIndex];
        Cell := TMnw1Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
        Cell.FSite := Site;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TMnw1Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  Result := TMnw1WellCollection;
end;

procedure TMnw1Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TMnw1Storage;
begin
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TMnw1Storage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

procedure TMnw1Boundary.InvalidateDisplay;
var
  LocalModel: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    LocalModel.InvalidateMnw1DesiredPumpingRate(Self);
    LocalModel.InvalidateMnw1WaterQuality(Self);
    LocalModel.InvalidateMnw1WellRadius(Self);
    LocalModel.InvalidateMnw1Conductance(Self);
    LocalModel.InvalidateMnw1SkinFactor(Self);
    LocalModel.InvalidateMnw1LimitingWaterLevel(Self);
    LocalModel.InvalidateMnw1ReferenceElevation(Self);
    LocalModel.InvalidateMnw1WaterQualityGroup(Self);
    LocalModel.InvalidateMnw1NonLinearLossCoefficient(Self);
    LocalModel.InvalidateMnw1MinimumPumpingRate(Self);
    LocalModel.InvalidateMnw1ReactivationPumpingRate(Self);
  end;

end;
procedure TMnw1Boundary.SetSite(const Value: string);
begin
  if FSite <> Value then
  begin
    FSite := Value;
    InvalidateModel;
  end;
end;

end.
