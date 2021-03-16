unit ModflowMnw2Unit;

interface

uses Classes, ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit,
  RbwParser, ModflowCellUnit, ZLib, GoPhastTypes, RealListUnit,
  SubscriptionUnit, SysUtils, Contnrs, DataSetUnit, PestObsUnit;

type
  TMnwLimitMethod = (mlmNoMinimum, mlmRate, mlmFraction);
  TMnwLossType = (mltNone, mltThiem, mltSkin, mltEquation, mtlSpecify);
  TMnwObsType = (motQin, motQout, motQnet, motQCumu, motHwell);

  TMnw2Record = record
    Cell: TCellLocation;
    WellRadius: double;
    SkinRadius: double;
    SkinK: double;
    B: double;
    C: double;
    P: double;
    CellToWellConductance: double;
    PartialPenetration: double;
    WellRadiusAnnotation: string;
    SkinRadiusAnnotation: string;
    SkinKAnnotation: string;
    BAnnotation: string;
    CAnnotation: string;
    PAnnotation: string;
    CellToWellConductanceAnnotation: string;
    PartialPenetrationAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMnw2Array = array of TMnw2Record;

  TMnw2Storage = class(TCustomBoundaryStorage)
  private
    FMnw2Array: TMnw2Array;
    function GetMnw2Array: TMnw2Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Mnw2Array: TMnw2Array read GetMnw2Array;
  end;

  TMnw2TimeItem = class(TCustomModflowBoundaryItem)
  private
    FLimitMethod: TMnwLimitMethod;
    FPumpingRate: TFormulaObject;
    FReactivationPumpingRate: TFormulaObject;
    FLimitingWaterLevel: TFormulaObject;
    FInactivationPumpingRate: TFormulaObject;
    FHeadCapacityMultiplier: TFormulaObject;
    FHlim: double;
    FQdes: double;
    FCapMult: double;
    FQfrcmn: double;
    FQfrcmx: double;
    function GetHeadCapacityMultiplier: string;
    function GetInactivationPumpingRate: string;
    function GetLimitingWaterLevel: string;
    function GetPumpingRate: string;
    function GetReactivationPumpingRate: string;
    procedure SetHeadCapacityMultiplier(const Value: string);
    procedure SetInactivationPumpingRate(const Value: string);
    procedure SetLimitingWaterLevel(const Value: string);
    procedure SetLimitMethod(const Value: TMnwLimitMethod);
    procedure SetPumpingRate(const Value: string);
    procedure SetReactivationPumpingRate(const Value: string);
    function GetHeadCapacityMultiplierValue: double;
    function GetInactivationPumpingRateValue: double;
    function GetLimitingWaterLevelValue: double;
    function GetPumpingRateValue: double;
    function GetReactivationPumpingRateValue: double;
    function GetBoundaryValue(Index: integer): double;
    procedure SetBoundaryValue(Index: integer; const Value: double);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Evaluate;
    property PumpingRateValue: double read GetPumpingRateValue;
    property HeadCapacityMultiplierValue: double
      read GetHeadCapacityMultiplierValue;
    property LimitingWaterLevelValue: double
      read GetLimitingWaterLevelValue;
    property InactivationPumpingRateValue: double
      read GetInactivationPumpingRateValue;
    property ReactivationPumpingRateValue: double
      read GetReactivationPumpingRateValue;
    property BoundaryValue[Index: integer]: double read GetBoundaryValue write SetBoundaryValue;
  published
    // QDes
    property PumpingRate: string read GetPumpingRate write SetPumpingRate;
    // CapMult
    property HeadCapacityMultiplier: string read GetHeadCapacityMultiplier
      write SetHeadCapacityMultiplier;
    // Hlim
    property LimitingWaterLevel: string read GetLimitingWaterLevel
      write SetLimitingWaterLevel;
    // Qfrcmn
    property InactivationPumpingRate: string read GetInactivationPumpingRate
      write SetInactivationPumpingRate;
    // Qfrcmx
    property ReactivationPumpingRate: string read GetReactivationPumpingRate
      write SetReactivationPumpingRate;
    // QCut
    property LimitMethod: TMnwLimitMethod read FLimitMethod
      write SetLimitMethod;
  end;

  TMnw2TimeCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure Evaluate;
    function IndexOfContainedStartTime(AStartTime: double): integer;
  end;

  TMnw2SpatialItem = class(TCustomModflowBoundaryItem)
  private
    FB: TFormulaObject;
    FC: TFormulaObject;
    FCellToWellConductance: TFormulaObject;
    FP: TFormulaObject;
    FPartialPenetration: TFormulaObject;
    FSkinK: TFormulaObject;
    FSkinRadius: TFormulaObject;
    FWellRadius: TFormulaObject;
    function GetB: string;
    function GetC: string;
    function GetCellToWellConductance: string;
    function GetP: string;
    function GetPartialPenetration: string;
    function GetSkinK: string;
    function GetSkinRadius: string;
    function GetWellRadius: string;
    procedure SetB(const Value: string);
    procedure SetC(const Value: string);
    procedure SetCellToWellConductance(const Value: string);
    procedure SetP(const Value: string);
    procedure SetPartialPenetration(const Value: string);
    procedure SetSkinK(const Value: string);
    procedure SetSkinRadius(const Value: string);
    procedure SetWellRadius(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property WellRadius: string read GetWellRadius write SetWellRadius;
    property SkinRadius: string read GetSkinRadius write SetSkinRadius;
    property SkinK: string read GetSkinK write SetSkinK;
    property B: string read GetB write SetB;
    property C: string read GetC write SetC;
    property P: string read GetP write SetP;
    property CellToWellConductance: string read GetCellToWellConductance
      write SetCellToWellConductance;
    property PartialPenetration: string read GetPartialPenetration
      write SetPartialPenetration;
  end;

  TMnw2TimeListLink = class(TTimeListsModelLink)
  private
    FWellRadiusData: TModflowTimeList;
    FSkinRadiusData: TModflowTimeList;
    FSkinKData: TModflowTimeList;
    FBData: TModflowTimeList;
    FCData: TModflowTimeList;
    FPData: TModflowTimeList;
    FCellToWellConductanceData: TModflowTimeList;
    FPartialPenetrationData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

//  TMnw2SpatialCollection = class(TCustomMF_ListBoundColl)
  TMnw2SpatialCollection = class(TCustomMF_ArrayBoundColl)
  private
//    FWellRadiusData: TModflowTimeList;
//    FSkinRadiusData: TModflowTimeList;
//    FSkinKData: TModflowTimeList;
//    FBData: TModflowTimeList;
//    FCData: TModflowTimeList;
//    FPData: TModflowTimeList;
//    FCellToWellConductanceData: TModflowTimeList;
//    FPartialPenetrationData: TModflowTimeList;
    procedure InvalidateWellRadiusData(Sender: TObject);
    procedure InvalidateSkinRadiusData(Sender: TObject);
    procedure InvalidateSkinKData(Sender: TObject);
    procedure InvalidateBData(Sender: TObject);
    procedure InvalidateCData(Sender: TObject);
    procedure InvalidatePData(Sender: TObject);
    procedure InvalidateCellToWellConductanceData(Sender: TObject);
    procedure InvalidatePartialPenetrationData(Sender: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetPhastModel: TBaseModel;
    procedure InvalidateWellRadius;
    procedure InvalidateSkinRadius;
    procedure InvalidateSkinK;
    procedure InvalidateB;
    procedure InvalidateC;
    procedure InvalidateP;
    procedure InvalidateCellToWellConductance;
    procedure InvalidatePartialPenetration;
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList; PestItemNames: TStringListObjectList); override;
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel; PestSeries: TStringList;
      PestMethods: TPestMethodList; PestItemNames: TStringListObjectList); override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    property PhastModel: TBaseModel read GetPhastModel;
  end;

  TLiftItem = class(TOrderedItem)
  private
    FLift: double;
    FQ: double;
    procedure SetLift(const Value: double);
    procedure SetQ(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Lift: double read FLift write SetLift;
    property Q: double read FQ write SetQ;
  end;

  TLiftCollection = class(TEnhancedOrderedCollection)
  public
    constructor Create(Model: TBaseModel);
    procedure Sort;
    procedure Assign(Source: TPersistent); override;
  end;

  TMnw2_Cell = class(TValueCell)
  private
    FValues: TMnw2Record;
    FStressPeriod: integer;
    function GetB: double;
    function GetBAnnotation: string;
    function GetC: double;
    function GetCAnnotation: string;
    function GetCellToWellConductance: double;
    function GetCellToWellConductanceAnnotation: string;
    function GetP: double;
    function GetPAnnotation: string;
    function GetPartialPenetration: double;
    function GetPartialPenetrationAnnotation: string;
    function GetSkinK: double;
    function GetSkinKAnnotation: string;
    function GetSkinRadius: double;
    function GetSkinRadiusAnnotation: string;
    function GetWellRadius: double;
    function GetWellRadiusAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetSection: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property WellRadius: double read GetWellRadius;
    property SkinRadius: double read GetSkinRadius;
    property SkinK: double read GetSkinK;
    property B: double read GetB;
    property C: double read GetC;
    property P: double read GetP;
    property CellToWellConductance: double read GetCellToWellConductance;
    property PartialPenetration: double read GetPartialPenetration;
    property WellRadiusAnnotation: string read GetWellRadiusAnnotation;
    property SkinRadiusAnnotation: string read GetSkinRadiusAnnotation;
    property SkinKAnnotation: string read GetSkinKAnnotation;
    property BAnnotation: string read GetBAnnotation;
    property CAnnotation: string read GetCAnnotation;
    property PAnnotation: string read GetPAnnotation;
    property CellToWellConductanceAnnotation: string
      read GetCellToWellConductanceAnnotation;
    property PartialPenetrationAnnotation: string
      read GetPartialPenetrationAnnotation;
  end;

  TTargetCell = Class(TGoPhastPersistent)
  private
    FLay: integer;
    FCol: integer;
    FRow: integer;
    procedure SetCol(const Value: integer);
    procedure SetLay(const Value: integer);
    procedure SetRow(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Col: integer read FCol write SetCol;
    property Row: integer read FRow write SetRow;
    property Lay: integer read FLay write SetLay;
  End;

  TTargetLocation = Class(TGoPhastPersistent)
  private
    FZ: real;
    FX: real;
    FY: real;
    procedure SetX(const Value: real);
    procedure SetY(const Value: real);
    procedure SetZ(const Value: real);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property X: real read FX write SetX;
    property Y: real read FY write SetY;
    property Z: real read FZ write SetZ;
  End;

  TTargetObject = class(TGoPhastPersistent)
  private
    FObjectName: string;
    FScreenObject: TObject;
    procedure SetObjectName(const Value: string);
    function GetObjectName: string;
    procedure SetScreenObject(const Value: TObject);
    function GetScreenObject: TObject;
    function ValidScreenObject(AScreenObject: TObject): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TObject read GetScreenObject
      write SetScreenObject;
  published
    property ObjectName: string read GetObjectName write SetObjectName;
  end;

  TTargetType = (ttNone, ttCell, ttLocation, ttObject);

  TTarget = class(TGoPhastPersistent)
  private
    FTargetType: TTargetType;
    FTargetObject: TTargetObject;
    FTargetCell: TTargetCell;
    FTargetLocation: TTargetLocation;
    procedure SetTargetCell(const Value: TTargetCell);
    procedure SetTargetLocation(const Value: TTargetLocation);
    procedure SetTargetObject(const Value: TTargetObject);
    procedure SetTargetType(const Value: TTargetType);
    function StoreTargetObject: Boolean;
    function StoreTargetCell: Boolean;
    function StoreTargetLocation: Boolean;
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TargetType: TTargetType read FTargetType write SetTargetType;
    property TargetObject: TTargetObject read FTargetObject
      write SetTargetObject stored StoreTargetObject;
    property TargetLocation: TTargetLocation read FTargetLocation
      write SetTargetLocation stored StoreTargetLocation;
    property TargetCell: TTargetCell read FTargetCell write SetTargetCell
      stored StoreTargetCell;
  end;

  TVerticalScreen = class(TCustomModflowBoundaryItem)
  private
    FSkinRadius: TFormulaObject;
    FB: TFormulaObject;
    FC: TFormulaObject;
    FZTop: double;
    FCellToWellConductance: TFormulaObject;
    FP: TFormulaObject;
    FWellRadius: TFormulaObject;
    FSkinK: TFormulaObject;
    FZBottom: double;
    procedure SetB(const Value: string);
    procedure SetC(const Value: string);
    procedure SetCellToWellConductance(const Value: string);
    procedure SetP(const Value: string);
    procedure SetSkinK(const Value: string);
    procedure SetSkinRadius(const Value: string);
    procedure SetWellRadius(const Value: string);
    procedure SetZBottom(const Value: double);
    procedure SetZTop(const Value: double);
    function GetB: string;
    function GetC: string;
    function GetCellToWellConductance: string;
    function GetP: string;
    function GetSkinK: string;
    function GetSkinRadius: string;
    function GetWellRadius: string;
  protected
    function BoundaryFormulaCount: integer; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function ScreenObject: TObject;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ZTop: double read FZTop write SetZTop;
    property ZBottom: double read FZBottom write SetZBottom;
    property WellRadius: string read GetWellRadius write SetWellRadius;
    property SkinRadius: string read GetSkinRadius write SetSkinRadius;
    property SkinK: string read GetSkinK write SetSkinK;
    property B: string read GetB write SetB;
    property C: string read GetC write SetC;
    property P: string read GetP write SetP;
    property CellToWellConductance: string read GetCellToWellConductance
      write SetCellToWellConductance;
  end;

  TVerticalScreenCollection = class(TCustomNonSpatialBoundColl)
  private
    function GetItem(index: integer): TVerticalScreen;
    procedure SetItem(index: integer; const Value: TVerticalScreen);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure Sort;
    property Items[index: integer]: TVerticalScreen read GetItem write SetItem; default;
  end;

  TMnw2ObsItem = class(TCustomTimeObservationItem)
  private
    FObsType: TMnwObsType;
    procedure SetObsType(const Value: TMnwObsType);
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(Value: Integer); override;
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
  public
    procedure Assign(Source: TPersistent); override;
    function ObservationType: string; override;
    function Units: string; override;
  published
    property ObsType: TMnwObsType read FObsType write SetObsType stored True;
    property GUID;
  end;

  TMnw2Observations = class(TCustomComparisonCollection)
  private
    function GetMnw2Item(Index: Integer): TMnw2ObsItem;
    procedure SetMnw2Item(Index: Integer; const Value: TMnw2ObsItem);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TMnw2ObsItem read GetMnw2Item
      write SetMnw2Item; default;
    function Add: TMnw2ObsItem;
  end;

  TMnw2Boundary = class(TModflowBoundary)
  private
    FTimeValues: TMnw2TimeCollection;
    FWellTolerance: double;
    FReferenceHead: double;
    FPumpElevation: double;
    FLiftAtMaxRate: double;
    FWellID: string;
    FLossType: TMnwLossType;
    FAdjustPumping: boolean;
    FConstrainPumping: boolean;
    FPartialPenetrationCorrection: boolean;
    FMaximumLift: double;
    FSpecifyPump: boolean;
    FLiftValues: TLiftCollection;
    FPumpCellTarget: TTarget;
    FSaveExternalFlows: boolean;
    FSaveInternalFlows: boolean;
    FVerticalScreens: TVerticalScreenCollection;
    FSaveMnwiInfo: boolean;
    FObservations: TMnw2Observations;
    procedure SetTimeValues(const Value: TMnw2TimeCollection);
    procedure SetAdjustPumping(const Value: boolean);
    procedure SetConstrainPumping(const Value: boolean);
    procedure SetLiftAtMaxRate(const Value: double);
    procedure SetLossType(const Value: TMnwLossType);
    procedure SetMaximumLift(const Value: double);
    procedure SetPartialPenetrationCorrection(const Value: boolean);
    procedure SetPumpElevation(const Value: double);
    procedure SetReferenceHead(const Value: double);
    procedure SetSpecifyPump(const Value: boolean);
    procedure SetWellID(const Value: string);
    procedure SetWellTolerance(const Value: double);
    procedure SetLiftValues(const Value: TLiftCollection);
    procedure SetPumpCellTarget(const Value: TTarget);
    function GetWellID: string;
    procedure SetSaveExternalFlows(const Value: boolean);
    procedure SetSaveInternalFlows(const Value: boolean);
    procedure SetVerticalScreens(const Value: TVerticalScreenCollection);
    procedure SetSaveMnwiInfo(const Value: boolean);
    procedure SetObservations(const Value: TMnw2Observations);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    procedure AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
      Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean); override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    function Used: boolean; override;
    function ConstantConstraints: boolean;
    function TargetCellLocation(AModel: TBaseModel): TCellLocation;
    procedure Clear; override;
    function DataTypeUsed(DataIndex: integer): boolean;
    procedure ReplaceGUID;
  published
    property TimeValues: TMnw2TimeCollection read FTimeValues
      write SetTimeValues;
    property LiftValues: TLiftCollection read FLiftValues write SetLiftValues;
    property WellID: string read GetWellID write SetWellID;
    property LossType: TMnwLossType read FLossType write SetLossType
      default mltThiem;
    // PUMPLOC
    property SpecifyPump: boolean read FSpecifyPump write SetSpecifyPump;
    // Zpump
    property PumpElevation: double read FPumpElevation write SetPumpElevation;
    // Qlimit
    property ConstrainPumping: boolean read FConstrainPumping
      write SetConstrainPumping;
    // PPFLAG
    property PartialPenetrationCorrection: boolean
      read FPartialPenetrationCorrection write SetPartialPenetrationCorrection;
    // PUMPCAP
    property AdjustPumping: boolean read FAdjustPumping write SetAdjustPumping;
    // Hlift
    property ReferenceHead: double read FReferenceHead write SetReferenceHead;
    // LIFTq0
    property MaximumLift: double read FMaximumLift write SetMaximumLift;
    // LIFTqmax
    property LiftAtMaxRate: double read FLiftAtMaxRate write SetLiftAtMaxRate;
    // HWtol
    property WellTolerance: double read FWellTolerance write SetWellTolerance;
    property PumpCellTarget: TTarget read FPumpCellTarget
      write SetPumpCellTarget;
    property SaveMnwiInfo: boolean read FSaveMnwiInfo write SetSaveMnwiInfo;
    property SaveExternalFlows: boolean read FSaveExternalFlows
      write SetSaveExternalFlows;
    property SaveInternalFlows: boolean read FSaveInternalFlows
      write SetSaveInternalFlows;
    property VerticalScreens: TVerticalScreenCollection read FVerticalScreens
      write SetVerticalScreens;
    property Observations: TMnw2Observations read FObservations
      write SetObservations
      {$IFNDEF PEST}
      stored False
      {$ENDIF}
      ;
  end;

const
  PumpingRatePosition = 0;
  HeadCapacityMultiplierPosition = 1;
  LimitingWaterLevelPosition = 2;
  InactivationPumpingRatePosition = 3;
  ReactivationPumpingRatePosition = 4;

const
  WellRadiusPosition = 0;
  SkinRadiusPosition = 1;
  SkinKPosition = 2;
  BPosition = 3;
  CPosition = 4;
  PPosition = 5;
  CellToWellConductancePosition = 6;
  PartialPenetrationPosition = 7;

implementation

uses
  frmGoPhastUnit, ScreenObjectUnit, PhastModelUnit,
  ModflowGridUnit, frmFormulaErrorsUnit, Math, SparseDataSets, SparseArrayUnit,
  frmErrorsAndWarningsUnit, AbstractGridUnit;

resourcestring
  StrOneOrMoreMNW2Wel = 'One or more MNW2 wells has a well radius that is '
    + 'less than or equal to zero.);';
  MnwSkinError = 'One or more MNW2 wells has a skin radius that is '
    + 'less than or equal to zero.);';
  MnwSkinToThinError = 'One or more MNW2 wells has a skin radius that is '
    + 'less than the well radius';
  MnwSkinKError = 'One or more MNW2 wells has a skin hydraulic conductivity that is '
    + 'less than or equal to zero.);';
  StrErrorInFormulaFor = 'Error in formula for %s in MNW2 data';
  StrIncorrectResultTyp = 'Incorrect result type in formula for %s in MNW2 d' +
  'ata';

{ TMnw2Item }

procedure TMnw2TimeItem.Assign(Source: TPersistent);
var
  Mnw: TMnw2TimeItem;
  Index: integer;
begin
  // if Assign is updated, update IsSame too.
  if Source is TMnw2TimeItem then
  begin
    Mnw := TMnw2TimeItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := Mnw.BoundaryFormula[Index];
    end;
    LimitMethod := Mnw.LimitMethod;
  end;
  inherited;
end;

procedure TMnw2TimeItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing.
end;

function TMnw2TimeItem.BoundaryFormulaCount: integer;
begin
  result := 5;
end;

procedure TMnw2TimeItem.CreateFormulaObjects;
begin
  FPumpingRate := CreateFormulaObject(dso3D);
  FReactivationPumpingRate := CreateFormulaObject(dso3D);
  FLimitingWaterLevel := CreateFormulaObject(dso3D);
  FInactivationPumpingRate := CreateFormulaObject(dso3D);
  FHeadCapacityMultiplier := CreateFormulaObject(dso3D);
end;

destructor TMnw2TimeItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

procedure TMnw2TimeItem.Evaluate;
var
  FormulaIndex: Integer;
  Formula: string;
  Compiler: TRbwParser;
  ObjectName: string;
  ErrorMessage: string;
  Expression: TExpression;
begin
// Set the following
//    FHlim: double;
//    FQdes: double;
//    FCapMult: double;
//    FQfrcmn: double;
//    FQfrcmx: double;

  Compiler := (Model as TPhastModel).rpThreeDFormulaCompiler;
  for FormulaIndex := 0 to BoundaryFormulaCount - 1 do
  begin
    Formula := BoundaryFormula[FormulaIndex];
    if Formula = '' then
    begin
      Formula := '0';
    end;
    try
      Compiler.Compile(Formula);
    except on E: ERbwParserError do
      begin
        ObjectName := (ScreenObject as TScreenObject).Name;
        case FormulaIndex of
          PumpingRatePosition:
            ErrorMessage := 'Qdes';
          HeadCapacityMultiplierPosition:
            ErrorMessage := 'CapMult';
          LimitingWaterLevelPosition:
            ErrorMessage := 'Hlim';
          InactivationPumpingRatePosition:
            ErrorMessage := 'Qfrcmn';
          ReactivationPumpingRatePosition:
            ErrorMessage := 'Qfrcmx';
          else Assert(False);
        end;
        ErrorMessage := Format(StrErrorInFormulaFor, [ErrorMessage]);

        frmFormulaErrors.AddFormulaError(ObjectName, '', Formula, ErrorMessage);
        Formula := '0';
        BoundaryFormula[FormulaIndex] := Formula;
        Compiler.Compile(Formula);
      end;
    end;
    Expression := Compiler.CurrentExpression;
    if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
    begin
      ObjectName := (ScreenObject as TScreenObject).Name;
      case FormulaIndex of
        PumpingRatePosition:
          ErrorMessage := 'Qdes';
        HeadCapacityMultiplierPosition:
          ErrorMessage := 'CapMult';
        LimitingWaterLevelPosition:
          ErrorMessage := 'Hlim';
        InactivationPumpingRatePosition:
          ErrorMessage := 'Qfrcmn';
        ReactivationPumpingRatePosition:
          ErrorMessage := 'Qfrcmx';
        else Assert(False);
      end;
      ErrorMessage := Format(StrIncorrectResultTyp, [ErrorMessage]);

      frmFormulaErrors.AddFormulaError(ObjectName, '', Formula, ErrorMessage);
      Formula := '0';
      BoundaryFormula[FormulaIndex] := Formula;
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
    end;
    Expression.Evaluate;
    BoundaryValue[FormulaIndex] := Expression.DoubleResult;
  end;
end;

function TMnw2TimeItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    PumpingRatePosition: result := PumpingRate;
    HeadCapacityMultiplierPosition: result := HeadCapacityMultiplier;
    LimitingWaterLevelPosition: result := LimitingWaterLevel;
    InactivationPumpingRatePosition: result := InactivationPumpingRate;
    ReactivationPumpingRatePosition: result := ReactivationPumpingRate;
    else Assert(False);
  end;
end;

function TMnw2TimeItem.GetBoundaryValue(Index: integer): double;
begin
  result := 0;
  case Index of
    PumpingRatePosition:
      result := FQdes;
    HeadCapacityMultiplierPosition:
      result := FCapMult;
    LimitingWaterLevelPosition:
      result := FHlim;
    InactivationPumpingRatePosition:
      result := FQfrcmn;
    ReactivationPumpingRatePosition:
      result := FQfrcmx;
    else Assert(False);
  end;
end;

function TMnw2TimeItem.GetHeadCapacityMultiplier: string;
begin
  Result := FHeadCapacityMultiplier.Formula;
  ResetItemObserver(HeadCapacityMultiplierPosition);
end;

function TMnw2TimeItem.GetHeadCapacityMultiplierValue: double;
begin
  result := FCapMult;
end;

function TMnw2TimeItem.GetInactivationPumpingRate: string;
begin
  Result := FInactivationPumpingRate.Formula;
  ResetItemObserver(InactivationPumpingRatePosition);
end;

function TMnw2TimeItem.GetInactivationPumpingRateValue: double;
begin
  result := FQfrcmn;
end;

function TMnw2TimeItem.GetLimitingWaterLevel: string;
begin
  Result := FLimitingWaterLevel.Formula;
  ResetItemObserver(LimitingWaterLevelPosition);
end;

function TMnw2TimeItem.GetLimitingWaterLevelValue: double;
begin
  result := FHlim
end;

procedure TMnw2TimeItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPumpingRate then
  begin
    List.Add(FObserverList[PumpingRatePosition]);
  end;
  if Sender = FReactivationPumpingRate then
  begin
    List.Add(FObserverList[ReactivationPumpingRatePosition]);
  end;
  if Sender = FLimitingWaterLevel then
  begin
    List.Add(FObserverList[LimitingWaterLevelPosition]);
  end;
  if Sender = FInactivationPumpingRate then
  begin
    List.Add(FObserverList[InactivationPumpingRatePosition]);
  end;
  if Sender = FHeadCapacityMultiplier then
  begin
    List.Add(FObserverList[HeadCapacityMultiplierPosition]);
  end;
end;

function TMnw2TimeItem.GetPumpingRate: string;
begin
  Result := FPumpingRate.Formula;
  ResetItemObserver(PumpingRatePosition);
end;

function TMnw2TimeItem.GetPumpingRateValue: double;
begin
  result := FQdes;
end;

function TMnw2TimeItem.GetReactivationPumpingRate: string;
begin
  Result := FReactivationPumpingRate.Formula;
  ResetItemObserver(ReactivationPumpingRatePosition);
end;

function TMnw2TimeItem.GetReactivationPumpingRateValue: double;
begin
  result := FQfrcmx;
end;

function TMnw2TimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMnw2TimeItem;
  Index: integer;
begin
  result := (AnotherItem is TMnw2TimeItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TMnw2TimeItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = Item.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
    result := LimitMethod = Item.LimitMethod;
  end;
end;

procedure TMnw2TimeItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReactivationPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FLimitingWaterLevel,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInactivationPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHeadCapacityMultiplier,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TMnw2TimeItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    PumpingRatePosition:
      PumpingRate := Value;
    HeadCapacityMultiplierPosition:
      HeadCapacityMultiplier := Value;
    LimitingWaterLevelPosition:
      LimitingWaterLevel := Value;
    InactivationPumpingRatePosition:
      InactivationPumpingRate := Value;
    ReactivationPumpingRatePosition:
      ReactivationPumpingRate := Value;
    else Assert(False);
  end;
end;

procedure TMnw2TimeItem.SetBoundaryValue(Index: integer; const Value: double);
begin
  case Index of
    PumpingRatePosition:
      FQdes := Value;
    HeadCapacityMultiplierPosition:
      FCapMult := Value;
    LimitingWaterLevelPosition:
      FHlim := Value;
    InactivationPumpingRatePosition:
      FQfrcmn := Value;
    ReactivationPumpingRatePosition:
      FQfrcmx := Value;
    else Assert(False);
  end;
end;

procedure TMnw2TimeItem.SetHeadCapacityMultiplier(const Value: string);
begin
  UpdateFormulaBlocks(Value, HeadCapacityMultiplierPosition, FHeadCapacityMultiplier);
end;

procedure TMnw2TimeItem.SetInactivationPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, InactivationPumpingRatePosition,
    FInactivationPumpingRate);
end;

procedure TMnw2TimeItem.SetLimitingWaterLevel(const Value: string);
begin
  UpdateFormulaBlocks(Value, LimitingWaterLevelPosition, FLimitingWaterLevel);
end;

procedure TMnw2TimeItem.SetLimitMethod(const Value: TMnwLimitMethod);
begin
  if FLimitMethod <> Value then
  begin
    FLimitMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2TimeItem.SetPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, PumpingRatePosition, FPumpingRate);
end;

procedure TMnw2TimeItem.SetReactivationPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, ReactivationPumpingRatePosition,
    FReactivationPumpingRate);
end;

{ TMnw2Boundary }

procedure TMnw2SpatialCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TMnw2Storage.Create(AModel));
end;

procedure TMnw2SpatialCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames: TStringListObjectList);
var
  WellRadiusArray: TDataArray;
  SkinRadiusArray: TDataArray;
  SkinKArray: TDataArray;
  BArray: TDataArray;
  CArray: TDataArray;
  CellToWellConductanceArray: TDataArray;
  PartialPenetrationArray: TDataArray;
  BoundaryStorage: TMnw2Storage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  PArray: TDataArray;
  LocalScreenObject: TScreenObject;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TCustomModel;
  Boundary: TMnw2Boundary;
  LossType: TMnwLossType;
  IsValue: Boolean;
  UsedCells: T3DSparseIntegerArray;
  VerticalScreenCount: Integer;
  Grid: TCustomModelGrid;
begin
  LocalModel := AModel as TCustomModel;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;
  Boundary := BoundaryGroup as TMnw2Boundary;
  LossType := Boundary.LossType;

  if LossType in [mltThiem, mltSkin, mltEquation] then
  begin
    WellRadiusArray := DataSets[0];
  end
  else
  begin
    WellRadiusArray := nil;
  end;

  if LossType = mltSkin then
  begin
    SkinRadiusArray := DataSets[1];
    SkinKArray := DataSets[2];
  end
  else
  begin
    SkinRadiusArray := nil;
    SkinKArray := nil;
  end;

  if LossType = mltEquation then
  begin
    BArray := DataSets[3];
    CArray := DataSets[4];
    PArray := DataSets[5];
  end
  else
  begin
    BArray := nil;
    CArray := nil;
    PArray := nil;
  end;

  if LossType = mtlSpecify then
  begin
    CellToWellConductanceArray := DataSets[6];
  end
  else
  begin
    CellToWellConductanceArray := nil;
  end;

  if Boundary.PartialPenetrationCorrection then
  begin
    PartialPenetrationArray := DataSets[7];
  end
  else
  begin
    PartialPenetrationArray := nil;
  end;

  BoundaryIndex := -1;
  BoundaryStorage := Boundaries[ItemIndex, AModel] as TMnw2Storage;

  Grid := LocalModel.Grid;
  UsedCells := T3DSparseIntegerArray.Create(GetQuantum(Grid.LayerCount),
    GetQuantum(Grid.RowCount), GetQuantum(Grid.ColumnCount));
  try
    LocalScreenObject := ScreenObject as TScreenObject;
    VerticalScreenCount := LocalScreenObject.ModflowBoundaries.
      ModflowMnw2Boundary.VerticalScreens.Count;
    for SegmentIndex := 0 to LocalScreenObject.Segments[LocalModel].Count - 1 do
    begin
      Segment := LocalScreenObject.Segments[LocalModel][SegmentIndex];
      ColIndex := Segment.Col;
      RowIndex := Segment.Row;
      LayerIndex := Segment.Layer;
      if not LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        Continue;
      end;

      IsValue := False;
      if LossType = mltNone then
      begin
        IsValue := True;
      end;
      if WellRadiusArray <> nil then
      begin
        IsValue := WellRadiusArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if SkinRadiusArray <> nil then
      begin
        IsValue := SkinRadiusArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if BArray <> nil then
      begin
        IsValue := BArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if CellToWellConductanceArray <> nil then
      begin
        IsValue := CellToWellConductanceArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if PartialPenetrationArray <> nil then
      begin
        IsValue := PartialPenetrationArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;

      if not IsValue then
      begin
        Continue;
      end;
      if (ColIndex = PriorCol)
        and (RowIndex = PriorRow)
        and (LayerIndex = PriorLayer) then
      begin
        Continue
      end;

      if UsedCells.IsValue[LayerIndex, RowIndex, ColIndex] then
      begin
        Continue
      end;
      UsedCells.Items[LayerIndex, RowIndex, ColIndex] := 1;

      Inc(BoundaryIndex);
      PriorCol := Segment.Col;
      PriorRow := Segment.Row;
      PriorLayer := Segment.Layer;

      Assert(BoundaryIndex < Length(BoundaryStorage.Mnw2Array));
      with BoundaryStorage.Mnw2Array[BoundaryIndex] do
      begin
        Cell.Layer := LayerIndex;
        Cell.Row := RowIndex;
        Cell.Column := ColIndex;
        if WellRadiusArray <> nil then
        begin
          WellRadius := WellRadiusArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          WellRadiusAnnotation := WellRadiusArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
          if (WellRadius <= 0) and (VerticalScreenCount = 0) then
          begin
            frmErrorsAndWarnings.AddError(LocalModel, StrOneOrMoreMNW2Wel,
              LocalScreenObject.Name, LocalScreenObject);
          end;
        end;

        if SkinRadiusArray <> nil then
        begin
          SkinRadius := SkinRadiusArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          SkinRadiusAnnotation := SkinRadiusArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
          if (SkinRadius <= 0) and (VerticalScreenCount = 0) then
          begin
            frmErrorsAndWarnings.AddError(LocalModel, MnwSkinError,
              LocalScreenObject.Name, LocalScreenObject);
          end;
          Assert(WellRadiusArray <> nil);
          if (SkinRadius < WellRadius) and (VerticalScreenCount = 0) then
          begin
            frmErrorsAndWarnings.AddError(LocalModel, MnwSkinToThinError,
              LocalScreenObject.Name, LocalScreenObject);
          end;
        end;
        if SkinKArray <> nil then
        begin
          SkinK := SkinKArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          SkinKAnnotation := SkinKArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
          if (SkinK <= 0) and (VerticalScreenCount = 0) then
          begin
            frmErrorsAndWarnings.AddError(LocalModel, MnwSkinKError,
              LocalScreenObject.Name, LocalScreenObject);
          end;
        end;

        if BArray <> nil then
        begin
          B := BArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          BAnnotation := BArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
        if CArray <> nil then
        begin
          C := CArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          CAnnotation := CArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
        if PArray <> nil then
        begin
          P := PArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          PAnnotation := PArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;

        if CellToWellConductanceArray <> nil then
        begin
          CellToWellConductance := CellToWellConductanceArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          CellToWellConductanceAnnotation := CellToWellConductanceArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;

        if PartialPenetrationArray <> nil then
        begin
          PartialPenetration := PartialPenetrationArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          PartialPenetrationAnnotation := PartialPenetrationArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
      end;
    end;
  finally
    UsedCells.Free;
  end;
  if WellRadiusArray <> nil then
  begin
    WellRadiusArray.CacheData;
  end;
  if SkinRadiusArray <> nil then
  begin
    SkinRadiusArray.CacheData;
  end;
  if SkinKArray <> nil then
  begin
    SkinKArray.CacheData;
  end;
  if BArray <> nil then
  begin
    BArray.CacheData;
  end;
  if CArray <> nil then
  begin
    CArray.CacheData;
  end;
  if PArray <> nil then
  begin
    PArray.CacheData;
  end;
  if CellToWellConductanceArray <> nil then
  begin
    CellToWellConductanceArray.CacheData;
  end;
  if PartialPenetrationArray <> nil then
  begin
    PartialPenetrationArray.CacheData;
  end;
  BoundaryStorage.CacheData;
end;

procedure TMnw2SpatialCollection.InvalidatePartialPenetration;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwPartialPenetration.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwPartialPenetration.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateCellToWellConductance;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwCellToWellConductance.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwCellToWellConductance.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateP;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwP.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwP.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateC;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwC.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwC.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList; PestItemNames: TStringListObjectList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TMnw2SpatialItem;
  Boundary: TMnw2Boundary;
  ScreenObject: TScreenObject;
  ItemUsed: boolean;
  LossType: TMnwLossType;
  ALink: TMnw2TimeListLink;
  WellRadiusData: TModflowTimeList;
  SkinRadiusData: TModflowTimeList;
  SkinKData: TModflowTimeList;
  BData: TModflowTimeList;
  CData: TModflowTimeList;
  PData: TModflowTimeList;
  CellToWellConductanceData: TModflowTimeList;
  PartialPenetrationData: TModflowTimeList;
begin
  SetLength(BoundaryValues, Count);

  Boundary := BoundaryGroup as TMnw2Boundary;
  LossType := Boundary.LossType;
  ScreenObject := Boundary.ScreenObject as TScreenObject;

  ItemUsed := LossType  in [mltThiem, mltSkin, mltEquation];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.WellRadius;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  ALink := TimeListLink.GetLink(AModel) as TMnw2TimeListLink;
  WellRadiusData := ALink.FWellRadiusData;
  WellRadiusData.Initialize(BoundaryValues, ScreenObject, lctIgnore, alAll);

  ItemUsed := LossType  = mltSkin;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.SkinRadius;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  SkinRadiusData := ALink.FSkinRadiusData;
  SkinRadiusData.Initialize(BoundaryValues, ScreenObject, lctIgnore, alAll);

  ItemUsed := LossType  = mltSkin;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.SkinK;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  SkinKData := ALink.FSkinKData;
  SkinKData.Initialize(BoundaryValues, ScreenObject, lctIgnore, alAll);

  ItemUsed := LossType  = mltEquation;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.B;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  BData := ALink.FBData;
  BData.Initialize(BoundaryValues, ScreenObject,
    lctIgnore, alAll);

  ItemUsed := LossType  = mltEquation;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.C;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  CData := ALink.FCData;
  CData.Initialize(BoundaryValues, ScreenObject,
    lctIgnore, alAll);

  ItemUsed := LossType  = mltEquation;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.P;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  PData := ALink.FPData;
  PData.Initialize(BoundaryValues, ScreenObject,
    lctIgnore, alAll);

  ItemUsed := LossType  = mtlSpecify;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.CellToWellConductance;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  CellToWellConductanceData := ALink.FCellToWellConductanceData;
  CellToWellConductanceData.Initialize(BoundaryValues, ScreenObject,
    lctIgnore, alAll);


  ItemUsed := Boundary.PartialPenetrationCorrection;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.PartialPenetration;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  PartialPenetrationData := ALink.FPartialPenetrationData;
  PartialPenetrationData.Initialize(BoundaryValues, ScreenObject,
    lctIgnore, alAll);



  Assert(WellRadiusData.Count = Count);
  Assert(SkinRadiusData.Count = Count);
  Assert(SkinKData.Count = Count);
  Assert(BData.Count = Count);
  Assert(CData.Count = Count);
  Assert(PData.Count = Count);
  Assert(CellToWellConductanceData.Count = Count);
  Assert(PartialPenetrationData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(WellRadiusData.Count, AModel);
  for TimeIndex := 0 to WellRadiusData.Count - 1 do
  begin
    AddBoundary(TMnw2Storage.Create(AModel));
  end;
  ListOfTimeLists.Add(WellRadiusData);
  ListOfTimeLists.Add(SkinRadiusData);
  ListOfTimeLists.Add(SkinKData);
  ListOfTimeLists.Add(BData);
  ListOfTimeLists.Add(CData);
  ListOfTimeLists.Add(PData);
  ListOfTimeLists.Add(CellToWellConductanceData);
  ListOfTimeLists.Add(PartialPenetrationData);
end;

procedure TMnw2SpatialCollection.InvalidateB;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwB.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwB.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinK;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwSkinK.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwSkinK.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinRadius;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwSkinRadius.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwSkinRadius.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateWellRadius;
var
  LocalPhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    if LocalPhastModel.Clearing then
    begin
      Exit;
    end;
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwWellRadius.Invalidate;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowPackages.Mnw2Package.MfMnwWellRadius.Invalidate;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateBData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FBData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FBData.Invalidate;
    end;
    InvalidateB;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateCData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FCData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FCData.Invalidate;
    end;
    InvalidateC;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateCellToWellConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FCellToWellConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FCellToWellConductanceData.Invalidate;
    end;
    InvalidateCellToWellConductance;
  end;
end;

procedure TMnw2SpatialCollection.InvalidatePartialPenetrationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FPartialPenetrationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FPartialPenetrationData.Invalidate;
    end;
    InvalidatePartialPenetration;
  end;
end;

procedure TMnw2SpatialCollection.InvalidatePData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FPData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FPData.Invalidate;
    end;
    InvalidateP;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinKData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FSkinKData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FSkinKData.Invalidate;
    end;
    InvalidateSkinK
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinRadiusData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FSkinRadiusData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FSkinRadiusData.Invalidate;
    end;
    InvalidateSkinRadius
  end;
end;

function TMnw2SpatialCollection.GetPhastModel: TBaseModel;
var
  OwnerScreenObject: TScreenObject;
begin
  result := nil;
  if ScreenObject <> nil then
  begin
    OwnerScreenObject := ScreenObject as TScreenObject;
    if OwnerScreenObject.Model <> nil then
    begin
      result := OwnerScreenObject.Model;
    end;
  end;
end;

function TMnw2SpatialCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMnw2TimeListLink;
end;

procedure TMnw2SpatialCollection.InvalidateWellRadiusData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMnw2TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMnw2TimeListLink;
    Link.FWellRadiusData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMnw2TimeListLink;
      Link.FWellRadiusData.Invalidate;
    end;
    InvalidateWellRadius;
  end;
end;

class function TMnw2SpatialCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMnw2SpatialItem;
end;

procedure TMnw2SpatialCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TMnw2Storage).FMnw2Array, BoundaryCount);
  inherited;
end;

{ TMnw2Boundary }

procedure TMnw2Boundary.AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
  Times: TRealList; StartTestTime, EndTestTime: double;
  var StartRangeExtended, EndRangeExtended: boolean);
var
  Index: Integer;
  TimeItem: TMnw2TimeItem;
begin
  for Index := 0 to TimeValues.Count - 1 do
  begin
    TimeItem := TimeValues[Index] as TMnw2TimeItem;
    Times.AddUnique(TimeItem.StartTime);
    Times.AddUnique(TimeItem.EndTime);
    if (TimeItem.StartTime < StartTestTime) then
    begin
      StartRangeExtended := True;
    end;
    if (TimeItem.EndTime > EndTestTime) then
    begin
      EndRangeExtended := True;
    end;
  end;
end;

procedure TMnw2Boundary.Assign(Source: TPersistent);
var
  SourceMnw2: TMnw2Boundary;
begin
  if Source is TMnw2Boundary then
  begin
    SourceMnw2 := TMnw2Boundary(Source);
    TimeValues := SourceMnw2.TimeValues;
    LiftValues := SourceMnw2.LiftValues;
    WellID := SourceMnw2.WellID;
    LossType := SourceMnw2.LossType;
    SpecifyPump := SourceMnw2.SpecifyPump;
    PumpElevation := SourceMnw2.PumpElevation;
    PumpElevation := SourceMnw2.PumpElevation;
    ConstrainPumping := SourceMnw2.ConstrainPumping;
    PartialPenetrationCorrection := SourceMnw2.PartialPenetrationCorrection;
    AdjustPumping := SourceMnw2.AdjustPumping;
    ReferenceHead := SourceMnw2.ReferenceHead;
    MaximumLift := SourceMnw2.MaximumLift;
    LiftAtMaxRate := SourceMnw2.LiftAtMaxRate;
    WellTolerance := SourceMnw2.WellTolerance;
    PumpCellTarget := SourceMnw2.PumpCellTarget;
    SaveMnwiInfo := SourceMnw2.SaveMnwiInfo;
    SaveExternalFlows := SourceMnw2.SaveExternalFlows;
    SaveInternalFlows := SourceMnw2.SaveInternalFlows;
    VerticalScreens := SourceMnw2.VerticalScreens;
    Observations := SourceMnw2.Observations;
  end;
  inherited;
end;

procedure TMnw2Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TMnw2_Cell;
  BoundaryValues: TMnw2Record;
  BoundaryIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMnw2Storage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TMnw2Storage;
  if 0 < ValueTimeList.Count then
  begin
    Cells := ValueTimeList[0];
  end
  else
  begin
    Cells := TValueCellList.Create(TMnw2_Cell);
    ValueTimeList.Add(Cells);
  end;
  // Check if the stress period is completely enclosed within the times
  // of the LocalBoundaryStorage;
  if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.Mnw2Array) then
  begin
    Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.Mnw2Array)
  end;
//  Cells.CheckRestore;
  for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Mnw2Array) - 1 do
  begin
    BoundaryValues := LocalBoundaryStorage.Mnw2Array[BoundaryIndex];
    Cell := TMnw2_Cell.Create;
    Assert(ScreenObject <> nil);
    Cell.IFace := (ScreenObject as TScreenObject).IFace;
    Cells.Add(Cell);
    Cell.FStressPeriod := 0;
    Cell.FValues := BoundaryValues;
    Cell.ScreenObject := ScreenObject;
    LocalModel.AdjustCellPosition(Cell);
  end;
  Cells.Cache;
  LocalBoundaryStorage.CacheData;
end;

class function TMnw2Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMnw2SpatialCollection;
end;

procedure TMnw2Boundary.Clear;
begin
  inherited;
  FLossType := mltThiem;
  TimeValues.Clear;
  LiftValues.Clear;
  VerticalScreens.Clear;
  Observations.Clear;
end;

function TMnw2Boundary.DataTypeUsed(DataIndex: integer): boolean;
begin
  result := False;
  case DataIndex of
    WellRadiusPosition:
      begin
        result := LossType in [mltThiem, mltSkin, mltEquation];
      end;
    SkinRadiusPosition, SkinKPosition:
      begin
        result := LossType = mltSkin;
      end;
    BPosition, CPosition, PPosition:
      begin
        result := LossType = mltEquation;
      end;
    CellToWellConductancePosition:
      begin
        result := LossType = mtlSpecify;
      end;
    PartialPenetrationPosition:
      begin
        result := PartialPenetrationCorrection;
      end;
    else Assert(False);
  end;
end;



function TMnw2Boundary.ConstantConstraints: boolean;
var
  Index: Integer;
  Item: TMnw2TimeItem;
  FirstItem: TMnw2TimeItem;
begin
  result := True;

  Assert(TimeValues.Count > 0);
  FirstItem := TimeValues.Items[0] as TMnw2TimeItem;
  for Index := 0 to TimeValues.Count - 1 do
  begin
    Item := TimeValues.Items[Index] as TMnw2TimeItem;
    result := (Item.FHlim = FirstItem.FHlim)
      and (Item.LimitMethod = FirstItem.LimitMethod);
    if not result then
    begin
      Exit;
    end;
    if FirstItem.LimitMethod <> mlmNoMinimum then
    begin
      result := (Item.FQfrcmn = FirstItem.FQfrcmn)
        and (Item.FQfrcmx = FirstItem.FQfrcmx);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

constructor TMnw2Boundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.Invalidate;
  end;
  inherited;
  FLossType := mltThiem;
  FTimeValues := TMnw2TimeCollection.Create(self, Model, ScreenObject);
  FLiftValues := TLiftCollection.Create(Model);
  FPumpCellTarget := TTarget.Create(OnInvalidateModelEvent);
  FVerticalScreens := TVerticalScreenCollection.Create(self, Model, ScreenObject);
  FObservations := TMnw2Observations.Create(OnInvalidateModelEvent, ScreenObject);
end;

destructor TMnw2Boundary.Destroy;
begin
  FObservations.Free;
  FVerticalScreens.Free;
  FPumpCellTarget.Free;
  FLiftValues.Free;
  FTimeValues.Free;
  inherited;
end;

procedure TMnw2Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TMnw2Storage;
//  ValueCount: Integer;
begin
  EvaluateArrayBoundaries(AModel);
//  EvaluateListBoundaries;
//  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TMnw2Storage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
//      Inc(ValueCount);
    end;
  end;
  TimeValues.Evaluate;
end;

function TMnw2Boundary.GetWellID: string;
begin
  if Length(FWellID) > 20 then
  begin
    SetLength(FWellID, 20);
  end;
  result := FWellID;
end;

procedure TMnw2Boundary.InvalidateDisplay;
begin
  inherited;
  // need to finish this
  Assert(False);
end;

procedure TMnw2Boundary.ReplaceGUID;
begin
  Observations.ReplaceGUID;
end;

procedure TMnw2Boundary.SetAdjustPumping(const Value: boolean);
begin
  if FAdjustPumping <> Value then
  begin
    FAdjustPumping := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetConstrainPumping(const Value: boolean);
begin
  if FConstrainPumping <> Value then
  begin
    FConstrainPumping := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetLiftAtMaxRate(const Value: double);
begin
  if FLiftAtMaxRate <> Value then
  begin
    FLiftAtMaxRate := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetLiftValues(const Value: TLiftCollection);
begin
  FLiftValues.Assign(Value);
end;

procedure TMnw2Boundary.SetLossType(const Value: TMnwLossType);
var
  SpatialCollection: TMnw2SpatialCollection;
begin
  if FLossType <> Value then
  begin
    FLossType := Value;
    InvalidateModel;
    SpatialCollection := Values as TMnw2SpatialCollection;
    SpatialCollection.InvalidateWellRadius;
    SpatialCollection.InvalidateSkinRadius;
    SpatialCollection.InvalidateSkinK;
    SpatialCollection.InvalidateB;
    SpatialCollection.InvalidateC;
    SpatialCollection.InvalidateP;
    SpatialCollection.InvalidateCellToWellConductance;
  end;
end;

procedure TMnw2Boundary.SetMaximumLift(const Value: double);
begin
  if FMaximumLift <> Value then
  begin
    InvalidateModel;
    FMaximumLift := Value;
  end;
end;

procedure TMnw2Boundary.SetObservations(const Value: TMnw2Observations);
begin
  FObservations.Assign(Value);
end;

procedure TMnw2Boundary.SetPartialPenetrationCorrection(const Value: boolean);
var
  SpatialCollection: TMnw2SpatialCollection;
begin
  if FPartialPenetrationCorrection <> Value then
  begin
    FPartialPenetrationCorrection := Value;
    InvalidateModel;
    SpatialCollection := Values as TMnw2SpatialCollection;
    SpatialCollection.InvalidatePartialPenetration;
  end;
end;

procedure TMnw2Boundary.SetPumpCellTarget(const Value: TTarget);
begin
  FPumpCellTarget.Assign(Value);
end;

procedure TMnw2Boundary.SetPumpElevation(const Value: double);
begin
  if FPumpElevation <> Value then
  begin
    FPumpElevation := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetReferenceHead(const Value: double);
begin
  if FReferenceHead <> Value then
  begin
    FReferenceHead := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSaveExternalFlows(const Value: boolean);
begin
  if FSaveExternalFlows <> Value then
  begin
    FSaveExternalFlows := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSaveInternalFlows(const Value: boolean);
begin
  if FSaveInternalFlows <> Value then
  begin
    FSaveInternalFlows := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSaveMnwiInfo(const Value: boolean);
begin
  if FSaveMnwiInfo <> Value then
  begin
    FSaveMnwiInfo := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSpecifyPump(const Value: boolean);
begin
  if FSpecifyPump <> Value then
  begin
    FSpecifyPump := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetTimeValues(const Value: TMnw2TimeCollection);
begin
  FTimeValues.Assign(Value);
end;

procedure TMnw2Boundary.SetVerticalScreens(
  const Value: TVerticalScreenCollection);
begin
  FVerticalScreens.Assign(Value);
end;

procedure TMnw2Boundary.SetWellID(const Value: string);
begin
  if FWellID <> Value then
  begin
    FWellID := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetWellTolerance(const Value: double);
begin
  if FWellTolerance <> Value then
  begin
    FWellTolerance := Value;
    InvalidateModel;
  end;
end;

function TMnw2Boundary.TargetCellLocation(AModel: TBaseModel): TCellLocation;
var
  ScreenObject: TScreenObject;
  X, Y, Z: double;
  Model: TCustomModel;
  Grid: TModflowGrid;
begin
  case PumpCellTarget.TargetType of
    ttNone:
      begin
        result.Layer := 0;
        result.Row := 1;
        result.Column := 1;
      end;
    ttObject:
      begin
        ScreenObject := PumpCellTarget.TargetObject.ScreenObject as TScreenObject;
        if ScreenObject = nil then
        begin
          result.Layer := 0;
          result.Row := 1;
          result.Column := 1;
        end
        else
        begin
          result := ScreenObject.SingleCellLocation(AModel);
        end;
      end;
    ttLocation:
      begin
        Model := AModel as TCustomModel;
        Grid := Model.ModflowGrid;
        X := PumpCellTarget.TargetLocation.X;
        Y := PumpCellTarget.TargetLocation.Y;
        Z := PumpCellTarget.TargetLocation.Z;
        if (X < Grid.ColumnPosition[Grid.ColumnCount]) and
          (Y > Grid.RowPosition[Grid.RowCount]) then
        begin
          result.Column := Grid.GetContainingColumn(X);
          result.Row := Grid.GetContainingRow(Y);
          if (result.Column >= 0) and (result.Row >= 0) then
          begin
            GetLayerFromZ(Z, Result, Grid, Model);
          end
          else
          begin
            result.Layer := 0;
            result.Row := 1;
            result.Column := 1;
          end;
        end
        else
        begin
          result.Layer := 0;
          result.Row := 1;
          result.Column := 1;
        end;
      end;
    ttCell:
      begin
        result.Layer := PumpCellTarget.TargetCell.Lay;
        result.Row := PumpCellTarget.TargetCell.Row;
        result.Column := PumpCellTarget.TargetCell.Col;
      end;
    else Assert(False);
  end;
end;

function TMnw2Boundary.Used: boolean;
begin
  result := (TimeValues.Count > 0);
end;

{ TMnw2SpatialItem }

procedure TMnw2SpatialItem.Assign(Source: TPersistent);
var
  SpatialSource: TMnw2SpatialItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TMnw2SpatialItem then
  begin
    SpatialSource := TMnw2SpatialItem(Source);
    WellRadius := SpatialSource.WellRadius;
    SkinRadius := SpatialSource.SkinRadius;
    SkinK := SpatialSource.SkinK;
    B := SpatialSource.B;
    C := SpatialSource.C;
    P := SpatialSource.P;
    CellToWellConductance := SpatialSource.CellToWellConductance;
    PartialPenetration := SpatialSource.PartialPenetration;
  end;
  inherited;

end;

procedure TMnw2SpatialItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMnw2SpatialCollection;
  WellRadiusObserver: TObserver;
  SkinRadiusObserver: TObserver;
  SkinKObserver: TObserver;
  BObserver: TObserver;
  CObserver: TObserver;
  PObserver: TObserver;
  CellToWellConductanceObserver: TObserver;
  PartialPenetrationObserver: TObserver;
begin
  ParentCollection := Collection as TMnw2SpatialCollection;

  WellRadiusObserver := FObserverList[WellRadiusPosition];
  WellRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateWellRadiusData;

  SkinRadiusObserver := FObserverList[SkinRadiusPosition];
  SkinRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateSkinRadiusData;

  SkinKObserver := FObserverList[SkinKPosition];
  SkinKObserver.OnUpToDateSet := ParentCollection.InvalidateSkinKData;

  BObserver := FObserverList[BPosition];
  BObserver.OnUpToDateSet := ParentCollection.InvalidateBData;

  CObserver := FObserverList[CPosition];
  CObserver.OnUpToDateSet := ParentCollection.InvalidateCData;

  PObserver := FObserverList[PPosition];
  PObserver.OnUpToDateSet := ParentCollection.InvalidatePData;

  CellToWellConductanceObserver := FObserverList[CellToWellConductancePosition];
  CellToWellConductanceObserver.OnUpToDateSet :=
    ParentCollection.InvalidateCellToWellConductanceData;

  PartialPenetrationObserver :=
    FObserverList[PartialPenetrationPosition];
  PartialPenetrationObserver.OnUpToDateSet :=
    ParentCollection.InvalidatePartialPenetrationData;
end;

function TMnw2SpatialItem.BoundaryFormulaCount: integer;
begin
  result := 8;
end;

procedure TMnw2SpatialItem.CreateFormulaObjects;
begin
  FB := CreateFormulaObject(dso3D);
  FC := CreateFormulaObject(dso3D);
  FCellToWellConductance := CreateFormulaObject(dso3D);
  FP := CreateFormulaObject(dso3D);
  FPartialPenetration := CreateFormulaObject(dso3D);
  FSkinK := CreateFormulaObject(dso3D);
  FSkinRadius := CreateFormulaObject(dso3D);
  FWellRadius := CreateFormulaObject(dso3D);

end;

function TMnw2SpatialItem.GetB: string;
begin
  Result := FB.Formula;
  ResetItemObserver(BPosition);
end;

function TMnw2SpatialItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    WellRadiusPosition:
      result := WellRadius;
    SkinRadiusPosition:
      result := SkinRadius;
    SkinKPosition:
      result := SkinK;
    BPosition:
      result := B;
    CPosition:
      result := C;
    PPosition:
      result := P;
    CellToWellConductancePosition:
      result := CellToWellConductance;
    PartialPenetrationPosition:
      result := PartialPenetration;
    else Assert(False);
  end;
end;

function TMnw2SpatialItem.GetC: string;
begin
  Result := FC.Formula;
  ResetItemObserver(CPosition);
end;

function TMnw2SpatialItem.GetCellToWellConductance: string;
begin
  Result := FCellToWellConductance.Formula;
  ResetItemObserver(CellToWellConductancePosition);
end;

function TMnw2SpatialItem.GetP: string;
begin
  Result := FP.Formula;
  ResetItemObserver(PPosition);
end;

function TMnw2SpatialItem.GetPartialPenetration: string;
begin
  Result := FPartialPenetration.Formula;
  ResetItemObserver(PartialPenetrationPosition);
end;

procedure TMnw2SpatialItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FB then
  begin
    List.Add(FObserverList[BPosition]);
  end;
  if Sender = FC then
  begin
    List.Add(FObserverList[CPosition]);
  end;
  if Sender = FCellToWellConductance then
  begin
    List.Add(FObserverList[CellToWellConductancePosition]);
  end;
  if Sender = FP then
  begin
    List.Add(FObserverList[PPosition]);
  end;
  if Sender = FPartialPenetration then
  begin
    List.Add(FObserverList[PartialPenetrationPosition]);
  end;
  if Sender = FSkinK then
  begin
    List.Add(FObserverList[SkinKPosition]);
  end;
  if Sender = FSkinRadius then
  begin
    List.Add(FObserverList[SkinRadiusPosition]);
  end;
  if Sender = FWellRadius then
  begin
    List.Add(FObserverList[WellRadiusPosition]);
  end;
end;

function TMnw2SpatialItem.GetSkinK: string;
begin
  Result := FSkinK.Formula;
  ResetItemObserver(SkinKPosition);
end;

function TMnw2SpatialItem.GetSkinRadius: string;
begin
  Result := FSkinRadius.Formula;
  ResetItemObserver(SkinRadiusPosition);
end;

function TMnw2SpatialItem.GetWellRadius: string;
begin
  Result := FWellRadius.Formula;
  ResetItemObserver(WellRadiusPosition);
end;

function TMnw2SpatialItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMnw2SpatialItem;
  Index: integer;
begin
  result := (AnotherItem is TMnw2SpatialItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TMnw2SpatialItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = Item.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;

end;

procedure TMnw2SpatialItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FB,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FC,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCellToWellConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FP,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FPartialPenetration,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinK,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinRadius,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FWellRadius,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

end;

procedure TMnw2SpatialItem.SetB(const Value: string);
begin
  UpdateFormulaBlocks(Value, BPosition, FB);
end;

procedure TMnw2SpatialItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    WellRadiusPosition:
      WellRadius := Value;
    SkinRadiusPosition:
      SkinRadius := Value;
    SkinKPosition:
      SkinK := Value;
    BPosition:
      B := Value;
    CPosition:
      C := Value;
    PPosition:
      P := Value;
    CellToWellConductancePosition:
      CellToWellConductance := Value;
    PartialPenetrationPosition:
      PartialPenetration := Value;
    else Assert(False);
  end;
end;

procedure TMnw2SpatialItem.SetC(const Value: string);
begin
  UpdateFormulaBlocks(Value, CPosition, FC);
end;

procedure TMnw2SpatialItem.SetCellToWellConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, CellToWellConductancePosition, FCellToWellConductance);
end;

procedure TMnw2SpatialItem.SetP(const Value: string);
begin
  UpdateFormulaBlocks(Value, PPosition, FP);
end;

procedure TMnw2SpatialItem.SetPartialPenetration(const Value: string);
begin
  UpdateFormulaBlocks(Value, PartialPenetrationPosition, FPartialPenetration);
end;

procedure TMnw2SpatialItem.SetSkinK(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinKPosition, FSkinK);
end;

procedure TMnw2SpatialItem.SetSkinRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinRadiusPosition, FSkinRadius);
end;

procedure TMnw2SpatialItem.SetWellRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, WellRadiusPosition, FWellRadius);
end;

{ TMnw2Record }

procedure TMnw2Record.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, WellRadius);
  WriteCompReal(Comp, SkinRadius);
  WriteCompReal(Comp, SkinK);
  WriteCompReal(Comp, B);
  WriteCompReal(Comp, C);
  WriteCompReal(Comp, P);
  WriteCompReal(Comp, CellToWellConductance);
  WriteCompReal(Comp, PartialPenetration);

  WriteCompInt(Comp, Strings.IndexOf(WellRadiusAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SkinRadiusAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SkinKAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(CAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(CellToWellConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PartialPenetrationAnnotation));
end;

procedure TMnw2Record.RecordStrings(Strings: TStringList);
begin
  Strings.Add(WellRadiusAnnotation);
  Strings.Add(SkinRadiusAnnotation);
  Strings.Add(SkinKAnnotation);
  Strings.Add(BAnnotation);
  Strings.Add(CAnnotation);
  Strings.Add(PAnnotation);
  Strings.Add(CellToWellConductanceAnnotation);
  Strings.Add(PartialPenetrationAnnotation);
end;

procedure TMnw2Record.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);

  WellRadius := ReadCompReal(Decomp);
  SkinRadius := ReadCompReal(Decomp);
  SkinK := ReadCompReal(Decomp);
  B := ReadCompReal(Decomp);
  C := ReadCompReal(Decomp);
  P := ReadCompReal(Decomp);
  CellToWellConductance := ReadCompReal(Decomp);
  PartialPenetration := ReadCompReal(Decomp);

  WellRadiusAnnotation := Annotations[ReadCompInt(Decomp)];
  SkinRadiusAnnotation := Annotations[ReadCompInt(Decomp)];
  SkinKAnnotation := Annotations[ReadCompInt(Decomp)];
  BAnnotation := Annotations[ReadCompInt(Decomp)];
  CAnnotation := Annotations[ReadCompInt(Decomp)];
  PAnnotation := Annotations[ReadCompInt(Decomp)];
  CellToWellConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  PartialPenetrationAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TMnw2Storage }

procedure TMnw2Storage.Clear;
begin
  SetLength(FMnw2Array, 0);
  FCleared := True;
end;

function TMnw2Storage.GetMnw2Array: TMnw2Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMnw2Array;
end;

procedure TMnw2Storage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMnw2Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FMnw2Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMnw2Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FMnw2Array);
    for Index := 0 to Count - 1 do
    begin
      FMnw2Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMnw2Array[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TMnw2TimeCollection }

procedure TMnw2TimeCollection.Evaluate;
var
  Index: Integer;
  Item: TMnw2TimeItem;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2TimeItem;
    Item.Evaluate;
  end;
end;

function TMnw2TimeCollection.IndexOfContainedStartTime(
  AStartTime: double): integer;
var
  Index: Integer;
  Item: TMnw2TimeItem;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2TimeItem;
    if (Item.StartTime <= AStartTime) and (Item.EndTime > AStartTime) then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

class function TMnw2TimeCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMnw2TimeItem;
end;

{ TMnw2_Cell }

procedure TMnw2_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TMnw2_Cell.GetB: double;
begin
  result := FValues.B;
end;

function TMnw2_Cell.GetBAnnotation: string;
begin
  result := FValues.BAnnotation;
end;

function TMnw2_Cell.GetC: double;
begin
  result := FValues.C;
end;

function TMnw2_Cell.GetCAnnotation: string;
begin
  result := FValues.CAnnotation;
end;

function TMnw2_Cell.GetCellToWellConductance: double;
begin
  result := FValues.CellToWellConductance;
end;

function TMnw2_Cell.GetCellToWellConductanceAnnotation: string;
begin
  result := FValues.CellToWellConductanceAnnotation;
end;

function TMnw2_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TMnw2_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TMnw2_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TMnw2_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TMnw2_Cell.GetP: double;
begin
  result := FValues.P;
end;

function TMnw2_Cell.GetPAnnotation: string;
begin
  result := FValues.PAnnotation;
end;

function TMnw2_Cell.GetPartialPenetration: double;
begin
  result := FValues.PartialPenetration;
end;

function TMnw2_Cell.GetPartialPenetrationAnnotation: string;
begin
  result := FValues.PartialPenetrationAnnotation;
end;

function TMnw2_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    WellRadiusPosition: result := WellRadiusAnnotation;
    SkinRadiusPosition: result := SkinRadiusAnnotation;
    SkinKPosition: result := SkinKAnnotation;
    BPosition: result := BAnnotation;
    CPosition: result := CAnnotation;
    PPosition: result := PAnnotation;
    CellToWellConductancePosition: result := CellToWellConductanceAnnotation;
    PartialPenetrationPosition: result := PartialPenetrationAnnotation;
    else Assert(False);
  end;
end;

function TMnw2_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    WellRadiusPosition: result := WellRadius;
    SkinRadiusPosition: result := SkinRadius;
    SkinKPosition: result := SkinK;
    BPosition: result := B;
    CPosition: result := C;
    PPosition: result := P;
    CellToWellConductancePosition: result := CellToWellConductance;
    PartialPenetrationPosition: result := PartialPenetration;
    else Assert(False);
  end;
end;

function TMnw2_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TMnw2_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

function TMnw2_Cell.GetSkinK: double;
begin
  result := FValues.SkinK;
end;

function TMnw2_Cell.GetSkinKAnnotation: string;
begin
  result := FValues.SkinKAnnotation;
end;

function TMnw2_Cell.GetSkinRadius: double;
begin
  result := FValues.SkinRadius;
end;

function TMnw2_Cell.GetSkinRadiusAnnotation: string;
begin
  result := FValues.SkinRadiusAnnotation;
end;

function TMnw2_Cell.GetWellRadius: double;
begin
  result := FValues.WellRadius;
end;

function TMnw2_Cell.GetWellRadiusAnnotation: string;
begin
  result := FValues.WellRadiusAnnotation;
end;

procedure TMnw2_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TMnw2_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TMnw2_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TMnw2_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TMnw2_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TLiftItem }

procedure TLiftItem.Assign(Source: TPersistent);
var
  SourceLift: TLiftItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TLiftItem then
  begin
    SourceLift := TLiftItem(Source);
    Lift := SourceLift.Lift;
    Q := SourceLift.Q;
  end;
  inherited;
end;

function TLiftItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherLift: TLiftItem;
begin
  result := (AnotherItem is TLiftItem);
  if result then
  begin
    OtherLift := TLiftItem(AnotherItem);
    result :=
      (Lift = OtherLift.Lift)
      and (Q = OtherLift.Q);
  end;
end;

procedure TLiftItem.SetLift(const Value: double);
begin
  if FLift <> Value then
  begin
    FLift := Value;
    InvalidateModel;
  end;
end;

procedure TLiftItem.SetQ(const Value: double);
begin
  if FQ <> Value then
  begin
    FQ := Value;
    InvalidateModel;
  end;
end;

{ TLiftCollection }

procedure TLiftCollection.Assign(Source: TPersistent);
begin
  inherited;
  Sort;
end;

constructor TLiftCollection.Create(Model: TBaseModel);
begin
  inherited Create(TLiftItem, Model);
end;

function CompareLiftItems(Item1, Item2: Pointer): Integer;
var
  LiftItem1: TLiftItem;
  LiftItem2: TLiftItem;
begin
  LiftItem1 := Item1;
  LiftItem2 := Item2;
  result := Sign(LiftItem2.Lift - LiftItem1.Lift);
end;

procedure TLiftCollection.Sort;
var
  List: TList;
  Index: Integer;
  Item: TLiftItem;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(CompareLiftItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

{ TTargetCell }

procedure TTargetCell.Assign(Source: TPersistent);
var
  TC: TTargetCell;
begin
  if Source is TTargetCell then
  begin
    TC := TTargetCell(Source);
    Col := TC.Col;
    Row := TC.Row;
    Lay := TC.Lay;
  end
  else
  begin
    inherited;
  end;
end;

procedure TTargetCell.SetCol(const Value: integer);
begin
  if FCol <> Value then
  begin
    FCol := Value;
    InvalidateModel;
  end;
end;

procedure TTargetCell.SetLay(const Value: integer);
begin
  if FLay <> Value then
  begin
    FLay := Value;
    InvalidateModel;
  end;
end;

procedure TTargetCell.SetRow(const Value: integer);
begin
  if FRow <> Value then
  begin
    FRow := Value;
    InvalidateModel;
  end;
end;

{ TTargetLocation }

procedure TTargetLocation.Assign(Source: TPersistent);
var
  TL: TTargetLocation;
begin
  if Source is TTargetLocation then
  begin
    TL := TTargetLocation(Source);
    X := TL.X;
    Y := TL.Y;
    Z := TL.Z;
  end
  else
  begin
    inherited;
  end;
end;

procedure TTargetLocation.SetX(const Value: real);
begin
  if FX <> Value then
  begin
    FX := Value;
    InvalidateModel;
  end;
end;

procedure TTargetLocation.SetY(const Value: real);
begin
  if FY <> Value then
  begin
    FY := Value;
    InvalidateModel;
  end;
end;

procedure TTargetLocation.SetZ(const Value: real);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    InvalidateModel;
  end;
end;

{ TTargetObject }

procedure TTargetObject.Assign(Source: TPersistent);
var
  TargetObject: TTargetObject;
begin
  if Source is TTargetObject then
  begin
    TargetObject := TTargetObject(Source);
    ScreenObject := TargetObject.ScreenObject;
    ObjectName := TargetObject.ObjectName;
  end
  else
  begin
    inherited;
  end;
end;

function TTargetObject.GetObjectName: string;
var
  ScreenObject: TScreenObject;
begin
  if FScreenObject = nil then
  begin
    result := FObjectName;
  end
  else
  begin
    if ValidScreenObject(FScreenObject) then
    begin
      ScreenObject := FScreenObject as TScreenObject;
      result := ScreenObject.Name;
    end
    else
    begin
      result := '';
    end;
  end;
end;

function TTargetObject.GetScreenObject: TObject;
begin
  if ValidScreenObject(FScreenObject) then
  begin
    result := FScreenObject;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TTargetObject.SetObjectName(const Value: string);
begin
  if FObjectName <> Value then
  begin
    FObjectName := Value;
    InvalidateModel;
  end;
end;

procedure TTargetObject.SetScreenObject(const Value: TObject);
begin
  Assert((Value = nil) or (Value is TScreenObject));
  FScreenObject := Value;
  if FScreenObject = nil then
  begin
    ObjectName := '';
  end
  else
  begin
    ObjectName := TScreenObject(Value).Name;
  end;
end;

function TTargetObject.ValidScreenObject(AScreenObject: TObject): boolean;
var
  ScreenObject: TScreenObject;
begin
  result := (AScreenObject <> nil);
  if result then
  begin
    ScreenObject := AScreenObject as TScreenObject;
    result :=  (ScreenObject.Count= 1)
      and (ScreenObject.ElevationCount = ecOne)
      and not ScreenObject.Deleted;
  end;
end;

{ TTarget }

procedure TTarget.Assign(Source: TPersistent);
var
  SourceTarget: TTarget;
begin
  if Source is TTarget then
  begin
    SourceTarget := TTarget(Source);
    TargetType := SourceTarget.TargetType;
    TargetObject := SourceTarget.TargetObject;
    TargetLocation := SourceTarget.TargetLocation;
    TargetCell := SourceTarget.TargetCell;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTarget.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FTargetObject:= TTargetObject.Create(InvalidateModelEvent);
  FTargetCell:= TTargetCell.Create(InvalidateModelEvent);
  FTargetLocation:= TTargetLocation.Create(InvalidateModelEvent);
  FTargetType := ttNone;
end;

destructor TTarget.Destroy;
begin
  FTargetLocation.Free;
  FTargetCell.Free;
  FTargetObject.Free;
  inherited;
end;

procedure TTarget.SetTargetCell(const Value: TTargetCell);
begin
  FTargetCell.Assign(Value);
end;

procedure TTarget.SetTargetLocation(const Value: TTargetLocation);
begin
  FTargetLocation.Assign(Value);
end;

procedure TTarget.SetTargetObject(const Value: TTargetObject);
begin
  FTargetObject.Assign(Value);
end;

procedure TTarget.SetTargetType(const Value: TTargetType);
begin
  FTargetType := Value;
end;

function TTarget.StoreTargetCell: Boolean;
begin
  result := TargetType = ttCell;
end;

function TTarget.StoreTargetLocation: Boolean;
begin
  result := TargetType = ttLocation;
end;

function TTarget.StoreTargetObject: Boolean;
begin
  result := TargetType = ttObject;
end;

{ TVerticalScreen }

procedure TVerticalScreen.Assign(Source: TPersistent);
var
  SourceScreen: TVerticalScreen;
begin
  // if Assign is updated, update IsSame too.
  if Source is TVerticalScreen then
  begin
    SourceScreen := TVerticalScreen(Source);
    ZTop := SourceScreen.ZTop;
    ZBottom := SourceScreen.ZBottom;
    WellRadius := SourceScreen.WellRadius;
    SkinRadius := SourceScreen.SkinRadius;
    SkinK := SourceScreen.SkinK;
    B := SourceScreen.B;
    C := SourceScreen.C;
    P := SourceScreen.P;
    CellToWellConductance := SourceScreen.CellToWellConductance;
  end;
  inherited;
end;

procedure TVerticalScreen.AssignObserverEvents(Collection: TCollection);
begin

end;

function TVerticalScreen.BoundaryFormulaCount: integer;
begin
  result := 7;
end;

procedure TVerticalScreen.CreateFormulaObjects;
begin
  FB := CreateFormulaObject(dso3D);
  FC := CreateFormulaObject(dso3D);
  FCellToWellConductance := CreateFormulaObject(dso3D);
  FP := CreateFormulaObject(dso3D);
  FSkinK := CreateFormulaObject(dso3D);
  FSkinRadius := CreateFormulaObject(dso3D);
  FWellRadius := CreateFormulaObject(dso3D);
end;

function TVerticalScreen.GetB: string;
begin
  Result := FB.Formula;
  ResetItemObserver(BPosition);
end;

function TVerticalScreen.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    WellRadiusPosition:
      result := WellRadius;
    SkinRadiusPosition:
      result := SkinRadius;
    SkinKPosition:
      result := SkinK;
    BPosition:
      result := B;
    CPosition:
      result := C;
    PPosition:
      result := P;
    CellToWellConductancePosition:
      result := CellToWellConductance;
    else Assert(False);
  end;
end;

function TVerticalScreen.GetC: string;
begin
  Result := FC.Formula;
  ResetItemObserver(CPosition);
end;

function TVerticalScreen.GetCellToWellConductance: string;
begin
  Result := FCellToWellConductance.Formula;
  ResetItemObserver(CellToWellConductancePosition);
end;

function TVerticalScreen.GetP: string;
begin
  Result := FP.Formula;
  ResetItemObserver(PPosition);
end;

procedure TVerticalScreen.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FB then
  begin
    List.Add(FObserverList[BPosition]);
  end;
  if Sender = FC then
  begin
    List.Add(FObserverList[CPosition]);
  end;
  if Sender = FCellToWellConductance then
  begin
    List.Add(FObserverList[CellToWellConductancePosition]);
  end;
  if Sender = FP then
  begin
    List.Add(FObserverList[PPosition]);
  end;
  if Sender = FSkinK then
  begin
    List.Add(FObserverList[SkinKPosition]);
  end;
  if Sender = FSkinRadius then
  begin
    List.Add(FObserverList[SkinRadiusPosition]);
  end;
  if Sender = FWellRadius then
  begin
    List.Add(FObserverList[WellRadiusPosition]);
  end;
end;

function TVerticalScreen.GetSkinK: string;
begin
  Result := FSkinK.Formula;
  ResetItemObserver(SkinKPosition);
end;

function TVerticalScreen.GetSkinRadius: string;
begin
  Result := FSkinRadius.Formula;
  ResetItemObserver(SkinRadiusPosition);
end;

function TVerticalScreen.GetWellRadius: string;
begin
  Result := FWellRadius.Formula;
  ResetItemObserver(WellRadiusPosition);
end;

function TVerticalScreen.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherScreen: TVerticalScreen;
begin
  result := (AnotherItem is TVerticalScreen);
  if result then
  begin
    OtherScreen := TVerticalScreen(AnotherItem);
    result :=
      (ZTop = OtherScreen.ZTop)
      and (ZBottom = OtherScreen.ZBottom)
      and (WellRadius = OtherScreen.WellRadius)
      and (SkinRadius = OtherScreen.SkinRadius)
      and (SkinK = OtherScreen.SkinK)
      and (B = OtherScreen.B)
      and (C = OtherScreen.C)
      and (P = OtherScreen.P)
      and (CellToWellConductance = OtherScreen.CellToWellConductance)
  end;
end;

procedure TVerticalScreen.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FB,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FC,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCellToWellConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FP,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinK,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinRadius,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FWellRadius,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

function TVerticalScreen.ScreenObject: TObject;
begin
  result := (Collection as TVerticalScreenCollection).ScreenObject;
end;

procedure TVerticalScreen.SetB(const Value: string);
begin
  UpdateFormulaBlocks(Value, BPosition, FB);
end;

procedure TVerticalScreen.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    WellRadiusPosition:
      WellRadius := Value;
    SkinRadiusPosition:
      SkinRadius := Value;
    SkinKPosition:
      SkinK := Value;
    BPosition:
      B := Value;
    CPosition:
      C := Value;
    PPosition:
      P := Value;
    CellToWellConductancePosition:
      CellToWellConductance := Value;
    else Assert(False);
  end;
end;

procedure TVerticalScreen.SetC(const Value: string);
begin
  UpdateFormulaBlocks(Value, CPosition, FC);
end;

procedure TVerticalScreen.SetCellToWellConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, CellToWellConductancePosition, FCellToWellConductance);
end;

procedure TVerticalScreen.SetP(const Value: string);
begin
  UpdateFormulaBlocks(Value, PPosition, FP);
end;

procedure TVerticalScreen.SetSkinK(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinKPosition, FSkinK);
end;

procedure TVerticalScreen.SetSkinRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinRadiusPosition, FSkinRadius);
end;

procedure TVerticalScreen.SetWellRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, WellRadiusPosition, FWellRadius);
end;

procedure TVerticalScreen.SetZBottom(const Value: double);
begin
  if FZBottom <> Value then
  begin
    FZBottom := Value;
    InvalidateModel;
  end;
end;

procedure TVerticalScreen.SetZTop(const Value: double);
begin
  if FZTop <> Value then
  begin
    FZTop := Value;
    InvalidateModel;
  end;
end;

{ TVerticalScreenCollection }

function CompareVerticalScreenItems(Item1, Item2: Pointer): Integer;
var
  VerticalScreen1: TVerticalScreen;
  VerticalScreen2: TVerticalScreen;
begin
  VerticalScreen1 := Item1;
  VerticalScreen2 := Item2;
  result := Sign(VerticalScreen2.ZTop - VerticalScreen1.ZTop);
  if result = 0 then
  begin
    result := Sign(VerticalScreen2.ZBottom - VerticalScreen1.ZBottom);
  end;
end;

function TVerticalScreenCollection.GetItem(index: integer): TVerticalScreen;
begin
  result := inherited Items[index] as  TVerticalScreen;
end;

class function TVerticalScreenCollection.ItemClass: TBoundaryItemClass;
begin
  result := TVerticalScreen;
end;

procedure TVerticalScreenCollection.SetItem(index: integer;
  const Value: TVerticalScreen);
begin
  inherited Items[index] := Value;
end;

procedure TVerticalScreenCollection.Sort;
var
  List: TList;
  Index: Integer;
  Item: TLiftItem;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(CompareVerticalScreenItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

{ TMnw2TimeListLink }

procedure TMnw2TimeListLink.CreateTimeLists;
begin
  inherited;
  FWellRadiusData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSkinRadiusData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSkinKData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FCData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FCellToWellConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPartialPenetrationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
end;

destructor TMnw2TimeListLink.Destroy;
begin
  FWellRadiusData.Free;
  FSkinRadiusData.Free;
  FSkinKData.Free;
  FBData.Free;
  FCData.Free;
  FPData.Free;
  FCellToWellConductanceData.Free;
  FPartialPenetrationData.Free;
  inherited;
end;

{ TMnw2ObsItem }

procedure TMnw2ObsItem.Assign(Source: TPersistent);
var
  ObsSource: TMnw2ObsItem;
begin
  if Source is TMnw2ObsItem then
  begin
    ObsSource := TMnw2ObsItem(Source);
    ObsType := ObsSource.ObsType;
  end;
  inherited;

end;

function TMnw2ObsItem.GetObsTypeIndex: Integer;
begin
  result := Ord(ObsType);
end;

function TMnw2ObsItem.GetObsTypeString: string;
begin
  Result := ObservationType;
end;

function TMnw2ObsItem.ObservationType: string;
begin
  case ObsType of
    motQin:
      begin
        result := 'MNW2_Qin';
      end;
    motQout:
      begin
        result := 'MNW2_Qout';
      end;
    motQnet:
      begin
        result := 'MNW2_Qnet';
      end;
    motQCumu:
      begin
        result := 'MNW2_QCumu';
      end;
    motHwell:
      begin
        result := 'MNW2_Hwell';
      end;
     else
       Assert(False);
  end;
end;

procedure TMnw2ObsItem.SetObsType(const Value: TMnwObsType);
begin
  if FObsType <> Value then
  begin
    BeginUpdate;
    try
      FObsType := Value;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TMnw2ObsItem.SetObsTypeIndex(Value: Integer);
begin
  Assert(Value >= 0);
  Assert(Value <= Ord(motHwell));
  ObsType := TMnwObsType(Value);
end;

procedure TMnw2ObsItem.SetObsTypeString(const Value: string);
begin
  if Value = 'MNW2_Qin' then
  begin
    ObsType := motQin;
  end
  else if Value = 'MNW2_Qout' then
  begin
    ObsType := motQout;
  end
  else if Value = 'MNW2_Qnet' then
  begin
    ObsType := motQnet;
  end
  else if Value = 'MNW2_QCumu' then
  begin
    ObsType := motQCumu;
  end
  else if Value = 'MNW2_Hwell' then
  begin
    ObsType := motHwell;
  end
  else
  begin
    Assert(False);
  end;
end;

function TMnw2ObsItem.Units: string;
begin
  case ObsType of
    motQin, motQout, motQnet:
      begin
        result := 'L3/T';
      end;
    motQCumu:
      begin
        result := 'L3';
      end;
    motHwell:
      begin
        result := 'L';
      end;
    else
       Assert(False);
  end;
end;

{ TMnw2Observations }

function TMnw2Observations.Add: TMnw2ObsItem;
begin
  result := inherited Add as TMnw2ObsItem; 
end;

constructor TMnw2Observations.Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
begin
  inherited Create(TMnw2ObsItem, InvalidateModelEvent, ScreenObject);
end;

function TMnw2Observations.GetMnw2Item(Index: Integer): TMnw2ObsItem;
begin
  result := inherited Items[Index] as TMnw2ObsItem;
end;

procedure TMnw2Observations.SetMnw2Item(Index: Integer;
  const Value: TMnw2ObsItem);
begin
  inherited Items[Index] := Value;
end;

end.
