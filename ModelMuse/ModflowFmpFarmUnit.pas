unit ModflowFmpFarmUnit;

interface

uses
  Classes, ModflowBoundaryUnit,
  OrderedCollectionUnit, GoPhastTypes, ModflowDrtUnit, ModflowCellUnit,
  Generics.Collections, ModflowFmpBaseClasses, RealListUnit,
  ModflowFmpAllotmentUnit, System.SysUtils, OrderedCollectionInterfaceUnit,
  ModflowParameterInterfaceUnit;

type
  // FMP data sets 19 or 32.
  TFarmCostsItem = class(TCustomZeroFarmItem)
  private
  const
    GWcost1_Position = 0;
    GWcost2_Position = 1;
    GWcost3_Position = 2;
    GWcost4_Position = 3;
    SWcost1_Position = 4;
    SWcost2_Position = 5;
    SWcost3_Position = 6;
    SWcost4_Position = 7;
    function GetGWcost1: string;
    function GetGWcost2: string;
    function GetGWcost3: string;
    function GetGWcost4: string;
    function GetSWcost1: string;
    function GetSWcost2: string;
    function GetSWcost3: string;
    function GetSWcost4: string;
    procedure SetGWcost1(const Value: string);
    procedure SetGWcost2(const Value: string);
    procedure SetGWcost3(const Value: string);
    procedure SetGWcost4(const Value: string);
    procedure SetSWcost1(const Value: string);
    procedure SetSWcost2(const Value: string);
    procedure SetSWcost3(const Value: string);
    procedure SetSWcost4(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property GWcost1: string read GetGWcost1 write SetGWcost1;
    property GWcost2: string read GetGWcost2 write SetGWcost2;
    property GWcost3: string read GetGWcost3 write SetGWcost3;
    property GWcost4: string read GetGWcost4 write SetGWcost4;
    property SWcost1: string read GetSWcost1 write SetSWcost1;
    property SWcost2: string read GetSWcost2 write SetSWcost2;
    property SWcost3: string read GetSWcost3 write SetSWcost3;
    property SWcost4: string read GetSWcost4 write SetSWcost4;
  end;

  // FMP data sets 19 or 32.
  TFarmCostsCollection = class(TCustomFarmCollection)
  private
    function GetItem(Index: Integer): TFarmCostsItem;
    procedure SetItem(Index: Integer; const Value: TFarmCostsItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TFarmCostsItem read GetItem write SetItem; default;
    function Add: TFarmCostsItem;
  end;

  TDiversionPosition = (dpStart, dpMiddle, dpEnd);

  TSfrDiversionObject = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FObjectName: string;
    FScreenObject: TObject;
    FDiversionVertex: integer;
    FDiversionPosition: TDiversionPosition;
    procedure SetObjectName(const Value: string);
    function GetObjectName: string;
    procedure SetScreenObject(const Value: TObject);
    function GetScreenObject: TObject;
    procedure SetDiversionPosition(const Value: TDiversionPosition);
    procedure SetDiversionVertex(const Value: integer);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel);
    class function ValidScreenObject(AScreenObject: TObject): boolean;
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TObject read GetScreenObject
      write SetScreenObject;
    function IsSame(OtherReturnObject: TSfrDiversionObject): boolean;
    procedure Loaded;
  published
    property ObjectName: string read GetObjectName write SetObjectName;
    property DiversionPosition: TDiversionPosition read FDiversionPosition
      write SetDiversionPosition;
    // If @link(DiversionPosition) = dpMiddle, @name is used to determine the
    // reach from which water is diverted. It represents the vertex number
    // (starting at 1) of the object that is in the desired reach.
    property DiversionVertex: integer read FDiversionVertex
      write SetDiversionVertex;
  end;

  TSegmentReach = record
    Segment: integer;
    Reach: integer;
  end;

  TSfrDiversion = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FDiversionObject: TSfrDiversionObject;
    FDiversionCell: TReturnCell;
    FDiversionLocation: TReturnLocation;
    FDiversinChoice: TReturnChoice;
    procedure SetDiversionCell(const Value: TReturnCell);
    procedure SetDiversionChoice(const Value: TReturnChoice);
    procedure SetDiversionLocation(const Value: TReturnLocation);
    procedure SetDiversionObject(const Value: TSfrDiversionObject);
    function StoreDiversionCell: boolean;
    function StoreDiversionLocation: boolean;
    function StoreDiversionObject: boolean;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    // @name returns the cell where the farm gets or returns water.
    // The cell numbers in the @link(TCellLocation) will be 1 based.
    // The @link(TCellLocation.Layer TCellLocation.Layer) is always 0.
    function ReturnCellLocation(AModel: TBaseModel): TCellLocation;
    // @name returns the segment and reach where the farm gets or returns water.
    // The cell numbers in the @link(TSegmentReach) will be 1 based.
    function SegmentReach: TSegmentReach;
    function IsSame(OtherDrainReturn: TSfrDiversion): boolean;
    procedure Loaded;
  published
    property DiversionChoice: TReturnChoice read FDiversinChoice
      write SetDiversionChoice;
    property DiversionCell: TReturnCell read FDiversionCell
      write SetDiversionCell stored StoreDiversionCell;
    property DiversionLocation: TReturnLocation read FDiversionLocation
      write SetDiversionLocation stored StoreDiversionLocation;
    property DiversionObject: TSfrDiversionObject read FDiversionObject
      write SetDiversionObject stored StoreDiversionObject;
  end;


  //Data Sets 20 and 34
  TSemiRoutedDeliveriesAndRunoffItem = class(TCustomZeroFarmItem)
  private
    const
      FracPosition = 0;
      LowerLimitPosition = 1;
      UpperLimitPostion = 2;
    var
    FLinkedStream: TSfrDiversion;
    procedure SetLinkedStream(const Value: TSfrDiversion);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Loaded;
  published
    property LinkedStream: TSfrDiversion read FLinkedStream write SetLinkedStream;
    property Frac: string index FracPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property LowerLimit: string index LowerLimitPosition
      read GetBoundaryFormula write SetBoundaryFormula;
    property UpperLimit: string index UpperLimitPostion
      read GetBoundaryFormula write SetBoundaryFormula;
  end;

  //Data Sets 20 and 34
  TSemiRoutedDeliveriesAndReturnFlowCollection = class(TCustomFarmCollection)
  private
    FUpperLimitPestParamMethod: TPestParamMethod;
    FLowerLimitPestParamMethod: TPestParamMethod;
    FLowerLimitPestSeriesParameter: IModflowParameter;
    FUpperLimitPestSeriesParameter: IModflowParameter;
    FLowerLimitPestSeriesParameterName: string;
    FUpperLimitPestSeriesParameterName: string;
    function GetItem(Index: Integer): TSemiRoutedDeliveriesAndRunoffItem;
    procedure SetItem(Index: Integer;
      const Value: TSemiRoutedDeliveriesAndRunoffItem);
    function GetLowerLimitPestSeriesParameter: string;
    function GetUpperLimitPestSeriesParameter: string;
    procedure SetLowerLimitPestParamMethod(const Value: TPestParamMethod);
    procedure SetPestLowerLimitSeriesParameter(const Value: string);
    procedure SetPestUpperLimitSeriesParameter(const Value: string);
    procedure SetUpperLimitPestParamMethod(const Value: TPestParamMethod);
  protected
    // @name returns TSemiRoutedDeliveriesAndRunoffItem
    class function ItemClass: TBoundaryItemClass; override;
  public
    function Add: TSemiRoutedDeliveriesAndRunoffItem;
    property Items[Index: Integer]: TSemiRoutedDeliveriesAndRunoffItem
      read GetItem write SetItem; default;
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
  published
    property LowerLimitPestSeriesParameter: string
      read GetLowerLimitPestSeriesParameter
      write SetPestLowerLimitSeriesParameter;
    property LowerLimitPestParamMethod: TPestParamMethod
      read FLowerLimitPestParamMethod write SetLowerLimitPestParamMethod;
    property UpperLimitPestSeriesParameter: string
      read GetUpperLimitPestSeriesParameter
      write SetPestUpperLimitSeriesParameter;
    property UpperLimitPestParamMethod: TPestParamMethod
      read FUpperLimitPestParamMethod write SetUpperLimitPestParamMethod;
  end;

  TSrCollList = TList<TSemiRoutedDeliveriesAndReturnFlowCollection>;

  TMultiSrdItem = class(TOrderedItem)
  private
    FSemiRouted: TSemiRoutedDeliveriesAndReturnFlowCollection;
    FName: string;
    procedure SetSemiRouted(
      const Value: TSemiRoutedDeliveriesAndReturnFlowCollection);
    procedure SetName(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
     constructor Create(Collection: TCollection); override;
     destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property Name: string read FName write SetName;
    property SemiRouted: TSemiRoutedDeliveriesAndReturnFlowCollection
      read FSemiRouted write SetSemiRouted;
  end;

  TMultiSrdCollection = class(TOrderedCollection)
  private
    FModel: TBaseModel;
    function GetItem(Index: Integer): TMultiSrdItem;
    procedure SetItem(Index: Integer; const Value: TMultiSrdItem);
  public
    procedure Loaded;
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TMultiSrdItem read GetItem write SetItem; default;
  end;

  TNonRoutedDeliveryType = (nrdtFarmDemand, nrdtDischarged, nrdtStored,
    nrdtVirtualFarm);

  TNonRoutedDeliveryTypeOwhm2 = (nrdt2FarmDemand, nrdt2Discharged, nrdt2Stored,
    nrdt2Infiltrate);

  //Data Set 33
  TNonRoutedDeliveryParameterItem = class(TCustomZeroFarmItem)
  private
  const
    VolumePostion = 0;
    RankPosition = 1;
    VirtualFarmPosition = 2;
  var
    FNonRoutedDeliveryType: TNonRoutedDeliveryType;
    FNonRoutedDeliveryTypeOwhm2: TNonRoutedDeliveryTypeOwhm2;
    procedure SetNonRoutedDeliveryType(const Value: TNonRoutedDeliveryType);
    procedure SetNonRoutedDeliveryTypeOwhm2(
      const Value: TNonRoutedDeliveryTypeOwhm2);
    function GetRank: string;
    function GetVolume: string;
    procedure SetRank(const Value: string);
    procedure SetVolume(const Value: string);
    function GetVirtualFarm: string;
    procedure SetVirtualFarm(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property Volume: string read GetVolume write SetVolume;
    property Rank: string read GetRank write SetRank;
    property VirtualFarm: string read GetVirtualFarm write SetVirtualFarm;
    property NonRoutedDeliveryType: TNonRoutedDeliveryType
      read FNonRoutedDeliveryType write SetNonRoutedDeliveryType;
    property NonRoutedDeliveryTypeOwhm2: TNonRoutedDeliveryTypeOwhm2
      read FNonRoutedDeliveryTypeOwhm2 write SetNonRoutedDeliveryTypeOwhm2;
  end;

  //Data Set 33
  TNonRoutedDeliveryParameterCollection = class(TCustomFarmCollection)
  private
    function GetItem(Index: Integer): TNonRoutedDeliveryParameterItem;
    procedure SetItem(Index: Integer;
      const Value: TNonRoutedDeliveryParameterItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    function Add: TNonRoutedDeliveryParameterItem;
    property Items[Index: Integer]: TNonRoutedDeliveryParameterItem read GetItem
      write SetItem; default;
  end;

  TDeliveryParamItem = class(TOrderedItem)
  private
    FDeliveryParam: TNonRoutedDeliveryParameterCollection;
    procedure SetDeliveryParam(
      const Value: TNonRoutedDeliveryParameterCollection);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property DeliveryParam: TNonRoutedDeliveryParameterCollection
      read FDeliveryParam write SetDeliveryParam;
  end;

  TDeliveryParamCollection = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TDeliveryParamItem;
    procedure SetItem(Index: Integer; const Value: TDeliveryParamItem);
  public
//    function Add: TDeliveryParamItem;
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TDeliveryParamItem read GetItem
      write SetItem; default;
  end;

  TWaterRightsItem = class(TCustomZeroFarmItem)
  private
  const
    WaterRightsPosition = 0;
    function GetWaterRights: string;
    procedure SetWaterRights(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property WaterRights: string read GetWaterRights write SetWaterRights;
  end;

  TWaterRightsCollection = class(TCustomFarmCollection)
  private
    function GetItem(Index: Integer): TWaterRightsItem;
    procedure SetItem(Index: Integer; const Value: TWaterRightsItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    function Add: TWaterRightsItem;
    property Items[Index: Integer]: TWaterRightsItem read GetItem
      write SetItem;  default;
  end;

  // @name orginally represented the crop efficiency for one crop for one farm
  // for one time period. FMP Data sets 7 or 24.
  // It is now used for other purposes too.
  TCropEfficiencyItem = class(TCustomZeroFarmItem)
  private
  const
    EfficiencyPosition = 0;
    function GetEfficiency: string;
    procedure SetEfficiency(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property Efficiency: string read GetEfficiency write SetEfficiency;
  end;

  // @name orginally represented the crop efficiencies for one crop for one
  // farm over multiple time periods. FMP Data sets 7 or 24.
  // It is now used for other purposes too.
  TCropEfficiencyCollection = class(TCustomFarmCollection)
  private
    FCropName: string;
    procedure SetCropName(const Value: string);
    function GetItem(Index: Integer): TCropEfficiencyItem;
    procedure SetItem(Index: Integer; const Value: TCropEfficiencyItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    property Items[Index: Integer]: TCropEfficiencyItem read GetItem
      write SetItem;  default;
    function First: TCropEfficiencyItem;
  published
    property CropName: string read FCropName write SetCropName;
  end;

  // @name represents the crop efficiencies for one crop for one farm over
  // multiple time periods. FMP Data sets 7 or 24.
  // It is now used for other purposes too.
  TFarmEfficienciesItem = class(TOrderedItem)
  private
    FCropEfficiency: TCropEfficiencyCollection;
    procedure SetCropEfficiency(const Value: TCropEfficiencyCollection);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Loaded;
  published
    property CropEfficiency: TCropEfficiencyCollection read FCropEfficiency
      write SetCropEfficiency;
  end;

  // name defines a farm.
  // @name represents the crop or irrigation efficiencies or efficiency improvements
  // for all crops or irrigation types for one farm over
  // multiple time periods. FMP Data sets 7 or 24.
  // It is now used for other purposes too.
  TFarmEfficiencyCollection = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TFarmEfficienciesItem;
    procedure SetItem(Index: Integer; const Value: TFarmEfficienciesItem);
    function GetFirst: TFarmEfficienciesItem;
  public
    function Add: TFarmEfficienciesItem;
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TFarmEfficienciesItem read GetItem
      write SetItem; default;
    property First: TFarmEfficienciesItem read GetFirst;
    procedure Loaded;
  end;

  TDeficiencyScenarioItem = class(TBoolFarmItem)
  protected
    class function DefaultFormula: string; override;
  end;

  TDeficiencyScenarioCollection = class(TBoolFarmCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TWaterSourceItem = class(TCustomDefaultFormulaItem)
  const
    GWPosition = 0;
    SWPosition = 1;
    NRDPosition = 2;
  protected
    class function DefaultFormula: string; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property Groundwater: string index GWPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property SurfaceWater: string index SWPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property NonRoutedDelivery: string index NRDPosition read GetBoundaryFormula
      write SetBoundaryFormula;
  end;

  TWaterSourceCollection = class(TCustomFarmCollection)
  private
    function GetItem(Index: Integer): TWaterSourceItem;
    procedure SetItem(Index: Integer; const Value: TWaterSourceItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    property Items[Index: Integer]: TWaterSourceItem read GetItem
      write SetItem;  default;
    function First: TWaterSourceItem;
  end;

  TBareRunoffFractionCollection = class(TOwhmCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TNoReturnOption = (nroReturnToStream, nroDeepPercolation);

  TNoReturnItem = class(TCustomModflowBoundaryItem)
  private
    FNoReturnOption: TNoReturnOption;
    procedure SetNoReturnOption(const Value: TNoReturnOption);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property NoReturnOption: TNoReturnOption read FNoReturnOption write SetNoReturnOption;
  end;

  TNoReturnCollection = class(TCustomFarmCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TSaltSupplyConcentrationItem = class(TCustomDefaultFormulaItem)
  private
    const
    NonRoutedPosition = 0;
    SurfaceWaterPosition  = 1;
    GroundWaterPosiiton = 2;
    ExternalPosition = 3;
  protected
    function BoundaryFormulaCount: integer; override;
  published
    property NonRoutedConcentration: string index NonRoutedPosition
      read GetBoundaryFormula write SetBoundaryFormula;
    property SurfaceWaterConcentration: string index SurfaceWaterPosition
      read GetBoundaryFormula write SetBoundaryFormula;
    property GroundwaterConcentration: string index GroundWaterPosiiton
      read GetBoundaryFormula write SetBoundaryFormula;
    property ExternalConcentration: string index ExternalPosition
      read GetBoundaryFormula write SetBoundaryFormula;
  end;

  TSaltSupplyConcentrationCollection = class(TCustomFarmCollection)
  private
    FGWConcPestParamMethod: TPestParamMethod;
    FSWConcPestParamMethod: TPestParamMethod;
    FExtConcPestParamMethod: TPestParamMethod;
    FExtConcPestSeriesParameterName: string;
    FExtConcPestSeriesParameter: IModflowParameter;
    FGWConcPestSeriesParameterName: string;
    FGWConcPestSeriesParameter: IModflowParameter;
    FSWConcPestSeriesParameterName: string;
    FSWConcPestSeriesParameter: IModflowParameter;
    function GetExtConcPestSeriesParameter: string;
    function GetGWConcPestSeriesParameter: string;
    function GetSWConcPestSeriesParameter: string;
    procedure SetExtConcPestParamMethod(const Value: TPestParamMethod);
    procedure SetExtConcPestSeriesParameter(const Value: string);
    procedure SetGWConcPestParamMethod(const Value: TPestParamMethod);
    procedure SetGWConcPestSeriesParameter(const Value: string);
    procedure SetSWConcPestParamMethod(const Value: TPestParamMethod);
    procedure SetSWConcPestSeriesParameter(const Value: string);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
  published
    property SWConcPestSeriesParameter: string read GetSWConcPestSeriesParameter
      write SetSWConcPestSeriesParameter;
    property SWConcPestParamMethod: TPestParamMethod read FSWConcPestParamMethod
      write SetSWConcPestParamMethod;
    property GWConcPestSeriesParameter: string read GetGWConcPestSeriesParameter
      write SetGWConcPestSeriesParameter;
    property GWConcPestParamMethod: TPestParamMethod read FGWConcPestParamMethod
      write SetGWConcPestParamMethod;
    property ExtConcPestSeriesParameter: string read GetExtConcPestSeriesParameter
      write SetExtConcPestSeriesParameter;
    property ExtConcPestParamMethod: TPestParamMethod read FExtConcPestParamMethod
      write SetExtConcPestParamMethod;
  end;

  TFarm = class(TOrderedItem)
  private
    FFarmId: Integer;
    FWaterRights: TWaterRightsCollection;
    FSemiRoutedDeliveries: TSemiRoutedDeliveriesAndReturnFlowCollection;
    FDeliveryParamCollection: TDeliveryParamCollection;
    FSemiRoutedReturnFlow: TSemiRoutedDeliveriesAndReturnFlowCollection;
    FFarmCostsCollection: TFarmCostsCollection;
    FFarmEfficiencyCollection: TFarmEfficiencyCollection;
    FGwAllotment: TAllotmentCollection;
    FFarmName: string;
    FFarmIrrigationEfficiencyCollection: TFarmEfficiencyCollection;
    FFarmIrrigationEfficiencyImprovementCollection: TFarmEfficiencyCollection;
    FAddedDemandRunoffSplitCollection: TFarmEfficiencyCollection;
    FIrrigationUniformity: TFarmEfficiencyCollection;
    FDeficiencyScenario: TDeficiencyScenarioCollection;
    FWaterSource: TWaterSourceCollection;
    FBareRunoffFraction: TBareRunoffFractionCollection;
    FAddedCropDemandRate: TFarmEfficiencyCollection;
    FAddedCropDemandFlux: TFarmEfficiencyCollection;
    FNoReturnFlow: TNoReturnCollection;
    FMultiSrd: TMultiSrdCollection;
    FMultiSrReturns: TMultiSrdCollection;
    FSWAllotment: TAllotmentCollection;
    FFarmGUID: string;
    FSaltSupplyConcentrationCollection: TSaltSupplyConcentrationCollection;
    FPumpSpreadChoice: TOwhmV2PumpSpreadChoice;
    procedure SetDeliveryParamCollection(const Value: TDeliveryParamCollection);
    procedure SetFarmCostsCollection(const Value: TFarmCostsCollection);
    procedure SetFarmId(const Value: Integer);
    procedure SetSemiRoutedDeliveries(
      const Value: TSemiRoutedDeliveriesAndReturnFlowCollection);
    procedure SetSemiRoutedReturnFlow(
      const Value: TSemiRoutedDeliveriesAndReturnFlowCollection);
    procedure SetWaterRights(const Value: TWaterRightsCollection);
    procedure SetFarmEfficiencyCollection(
      const Value: TFarmEfficiencyCollection);
    procedure SetGwAllotment(const Value: TAllotmentCollection);
    procedure AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
      Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean); virtual;
    procedure SetFarmName(const Value: string);
    procedure SetFarmIrrigationEfficiencyCollection(
      const Value: TFarmEfficiencyCollection);
    function GetCurrentFarmEfficiencyCollection: TFarmEfficiencyCollection;
    procedure SetCurrentFarmEfficiencyCollection(
      const Value: TFarmEfficiencyCollection);
    procedure SetFarmIrrigationEfficiencyImprovementCollection(
      const Value: TFarmEfficiencyCollection);
    procedure SetAddedDemandRunoffSplitCollection(
      const Value: TFarmEfficiencyCollection);
    procedure SetIrrigationUniformity(const Value: TFarmEfficiencyCollection);
    procedure SetDeficiencyScenario(const Value: TDeficiencyScenarioCollection);
    procedure SetWaterSource(const Value: TWaterSourceCollection);
    procedure SetBareRunoffFraction(const Value: TBareRunoffFractionCollection);
    procedure SetAddedCropDemandFlux(const Value: TFarmEfficiencyCollection);
    procedure SetAddedCropDemandRate(const Value: TFarmEfficiencyCollection);
    procedure SetNoReturnFlow(const Value: TNoReturnCollection);
    procedure SetMultiSrd(const Value: TMultiSrdCollection);
    procedure SetMultiSrReturns(const Value: TMultiSrdCollection);
    procedure SetSWAllotment(const Value: TAllotmentCollection);
    procedure SetFarmGUID(const Value: string);
    procedure SetSaltSupplyConcentrationCollection(
      const Value: TSaltSupplyConcentrationCollection);
    procedure SetPumpSpreadChoice(const Value: TOwhmV2PumpSpreadChoice);
  public
    function Used: boolean;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Loaded;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean); virtual;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    property CurrentFarmEfficiencyCollection: TFarmEfficiencyCollection
      read GetCurrentFarmEfficiencyCollection
      write SetCurrentFarmEfficiencyCollection;
  published
    property FarmGUID: string read FFarmGUID write SetFarmGUID;
    // FID, FMP Data set 6.
    property FarmId: Integer read FFarmId write SetFarmId;
    // FMP Data sets 7 or 27.
    property FarmEfficiencyCollection: TFarmEfficiencyCollection
      read FFarmEfficiencyCollection write SetFarmEfficiencyCollection;
    // FMP data sets 19 or 35.
    property FarmCostsCollection: TFarmCostsCollection read FFarmCostsCollection
      write SetFarmCostsCollection;
    //Data Sets 20a and 37a OWHM version 1
    // SEMI_ROUTED_DELIVERY OWHM version 2
    property SemiRoutedDeliveries: TSemiRoutedDeliveriesAndReturnFlowCollection
      read FSemiRoutedDeliveries write SetSemiRoutedDeliveries;
    //Data Sets 20b and 37b
    property SemiRoutedReturnFlow: TSemiRoutedDeliveriesAndReturnFlowCollection
      read FSemiRoutedReturnFlow write SetSemiRoutedReturnFlow;
    //Data Sets 36
    property DeliveryParamCollection: TDeliveryParamCollection
      read FDeliveryParamCollection write SetDeliveryParamCollection;
    //Data Sets 39
    property WaterRights: TWaterRightsCollection read FWaterRights
      write SetWaterRights;
    // Data Set 25 in OWHM version 1
    // Groundwater in ALLOTMENTS in OWHM version 2
    property GwAllotment: TAllotmentCollection read FGwAllotment
      write SetGwAllotment;
    property FarmName: string read FFarmName write SetFarmName;

    // irrigation efficiency in OWHM verison 2
    property FarmIrrigationEfficiencyCollection: TFarmEfficiencyCollection
      read FFarmIrrigationEfficiencyCollection
      write SetFarmIrrigationEfficiencyCollection;
    // irrigation efficiency improvement in OWHM verison 2
    property FarmIrrigationEfficiencyImprovementCollection: TFarmEfficiencyCollection
      read FFarmIrrigationEfficiencyImprovementCollection
      write SetFarmIrrigationEfficiencyImprovementCollection;
    // ADDED_DEMAND_RUNOFF_SPLIT in OWHM verison 2
    property AddedDemandRunoffSplitCollection: TFarmEfficiencyCollection
      read FAddedDemandRunoffSplitCollection
      write SetAddedDemandRunoffSplitCollection;
      // WBS_IRRIGATION_UNIFORMITY in OWHM verison 2
      property IrrigationUniformity: TFarmEfficiencyCollection
        read FIrrigationUniformity write SetIrrigationUniformity;
      property DeficiencyScenario: TDeficiencyScenarioCollection
        read FDeficiencyScenario write SetDeficiencyScenario;
      property WaterSource: TWaterSourceCollection read FWaterSource
        write SetWaterSource;
      property BareRunoffFraction: TBareRunoffFractionCollection
        read FBareRunoffFraction write SetBareRunoffFraction;
      property AddedCropDemandFlux: TFarmEfficiencyCollection
        read FAddedCropDemandFlux write SetAddedCropDemandFlux;
      property AddedCropDemandRate: TFarmEfficiencyCollection
        read FAddedCropDemandRate write SetAddedCropDemandRate;
      property NoReturnFlow: TNoReturnCollection read FNoReturnFlow
        write SetNoReturnFlow;
      // SEMI_ROUTED_DELIVERY
      property MultiSrDeliveries: TMultiSrdCollection read FMultiSrd write SetMultiSrd;
      // SEMI_ROUTED_RETURN
      property MultiSrReturns: TMultiSrdCollection read FMultiSrReturns
        write SetMultiSrReturns;
      // SURFACE_WATER in ALLOCATIONS
    property SWAllotment: TAllotmentCollection read FSWAllotment
      write SetSWAllotment;
      property SaltSupplyConcentrationCollection: TSaltSupplyConcentrationCollection
        read FSaltSupplyConcentrationCollection
        write SetSaltSupplyConcentrationCollection;
    property PumpSpreadChoice: TOwhmV2PumpSpreadChoice read FPumpSpreadChoice
      write SetPumpSpreadChoice;
  end;

  TFarmCollection = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TFarm;
    procedure SetItem(Index: Integer; const Value: TFarm);
  public
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TFarm read GetItem write SetItem; default;
    function Add: TFarm;
    function Last: TFarm;
    procedure Loaded;
    procedure Sort;
  end;

  TFarmList = TList<TFarm>;
  TFarmObjectList = TObjectList<TFarm>;

implementation

uses
  ScreenObjectUnit, FastGEO, PhastModelUnit, ModflowGridUnit,
  frmGoPhastUnit, CellLocationUnit;

{ TFarmCostsItem }

function TFarmCostsItem.BoundaryFormulaCount: integer;
begin
  result := 8;
end;

function TFarmCostsItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    GWcost1_Position:
      result := GWcost1;
    GWcost2_Position:
      result := GWcost2;
    GWcost3_Position:
      result := GWcost3;
    GWcost4_Position:
      result := GWcost4;
    SWcost1_Position:
      result := SWcost1;
    SWcost2_Position:
      result := SWcost2;
    SWcost3_Position:
      result := SWcost3;
    SWcost4_Position:
      result := SWcost4;
    else Assert(False);
  end;
end;

function TFarmCostsItem.GetGWcost1: string;
begin
  Result := FFormulaObjects[GWcost1_Position].Formula;
  ResetItemObserver(GWcost1_Position);
end;

function TFarmCostsItem.GetGWcost2: string;
begin
  Result := FFormulaObjects[GWcost2_Position].Formula;
  ResetItemObserver(GWcost2_Position);
end;

function TFarmCostsItem.GetGWcost3: string;
begin
  Result := FFormulaObjects[GWcost3_Position].Formula;
  ResetItemObserver(GWcost3_Position);
end;

function TFarmCostsItem.GetGWcost4: string;
begin
  Result := FFormulaObjects[GWcost4_Position].Formula;
  ResetItemObserver(GWcost4_Position);
end;

function TFarmCostsItem.GetSWcost1: string;
begin
  Result := FFormulaObjects[SWcost1_Position].Formula;
  ResetItemObserver(SWcost1_Position);
end;

function TFarmCostsItem.GetSWcost2: string;
begin
  Result := FFormulaObjects[SWcost2_Position].Formula;
  ResetItemObserver(SWcost2_Position);
end;

function TFarmCostsItem.GetSWcost3: string;
begin
  Result := FFormulaObjects[SWcost3_Position].Formula;
  ResetItemObserver(SWcost3_Position);
end;

function TFarmCostsItem.GetSWcost4: string;
begin
  Result := FFormulaObjects[SWcost4_Position].Formula;
  ResetItemObserver(SWcost4_Position);
end;

procedure TFarmCostsItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  inherited;
  case Index of
    GWcost1_Position:
      GWcost1 := Value;
    GWcost2_Position:
      GWcost2 := Value;
    GWcost3_Position:
      GWcost3 := Value;
    GWcost4_Position:
      GWcost4 := Value;
    SWcost1_Position:
      SWcost1 := Value;
    SWcost2_Position:
      SWcost2 := Value;
    SWcost3_Position:
      SWcost3 := Value;
    SWcost4_Position:
      SWcost4 := Value;
    else Assert(False);
  end;
end;

procedure TFarmCostsItem.SetGWcost1(const Value: string);
begin
  if FFormulaObjects[GWcost1_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, GWcost1_Position, FFormulaObjects[GWcost1_Position]);
  end;
end;

procedure TFarmCostsItem.SetGWcost2(const Value: string);
begin
  if FFormulaObjects[GWcost2_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, GWcost2_Position, FFormulaObjects[GWcost2_Position]);
  end;
end;

procedure TFarmCostsItem.SetGWcost3(const Value: string);
begin
  if FFormulaObjects[GWcost3_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, GWcost3_Position, FFormulaObjects[GWcost3_Position]);
  end;
end;

procedure TFarmCostsItem.SetGWcost4(const Value: string);
begin
  if FFormulaObjects[GWcost4_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, GWcost4_Position, FFormulaObjects[GWcost4_Position]);
  end;
end;

procedure TFarmCostsItem.SetSWcost1(const Value: string);
begin
  if FFormulaObjects[SWcost1_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SWcost1_Position, FFormulaObjects[SWcost1_Position]);
  end;
end;

procedure TFarmCostsItem.SetSWcost2(const Value: string);
begin
  if FFormulaObjects[SWcost2_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SWcost2_Position, FFormulaObjects[SWcost2_Position]);
  end;
end;

procedure TFarmCostsItem.SetSWcost3(const Value: string);
begin
  if FFormulaObjects[SWcost3_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SWcost3_Position, FFormulaObjects[SWcost3_Position]);
  end;
end;

procedure TFarmCostsItem.SetSWcost4(const Value: string);
begin
  if FFormulaObjects[SWcost4_Position].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SWcost4_Position, FFormulaObjects[SWcost4_Position]);
  end;
end;

{ TFarmCostsCollection }

function TFarmCostsCollection.Add: TFarmCostsItem;
begin
  result := inherited Add as TFarmCostsItem;
end;

function TFarmCostsCollection.GetItem(Index: Integer): TFarmCostsItem;
begin
  result := inherited Items[index] as TFarmCostsItem;
end;

class function TFarmCostsCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFarmCostsItem;
end;

procedure TFarmCostsCollection.SetItem(Index: Integer;
  const Value: TFarmCostsItem);
begin
  inherited Items[index] := Value;
end;

{ TSemiRoutedDeliveriesAndRunoffItem }

procedure TSemiRoutedDeliveriesAndRunoffItem.Assign(Source: TPersistent);
begin
  if Source is TSemiRoutedDeliveriesAndRunoffItem then
  begin
    LinkedStream := TSemiRoutedDeliveriesAndRunoffItem(Source).LinkedStream;
  end;
  inherited;
end;

function TSemiRoutedDeliveriesAndRunoffItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

constructor TSemiRoutedDeliveriesAndRunoffItem.Create(Collection: TCollection);
begin
  inherited;
  FLinkedStream := TSfrDiversion.Create(Model as TCustomModel);
end;

destructor TSemiRoutedDeliveriesAndRunoffItem.Destroy;
begin
  FLinkedStream.Free;
  inherited;
end;

function TSemiRoutedDeliveriesAndRunoffItem.GetBoundaryFormula(
  Index: integer): string;
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

function TSemiRoutedDeliveriesAndRunoffItem.IsSame(
  AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TSemiRoutedDeliveriesAndRunoffItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
    result := LinkedStream.IsSame(
      TSemiRoutedDeliveriesAndRunoffItem(AnotherItem).LinkedStream);
  end;
end;

procedure TSemiRoutedDeliveriesAndRunoffItem.Loaded;
begin
  LinkedStream.Loaded;
end;

procedure TSemiRoutedDeliveriesAndRunoffItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

procedure TSemiRoutedDeliveriesAndRunoffItem.SetLinkedStream(
  const Value: TSfrDiversion);
begin
  FLinkedStream.Assign(Value);
end;

{ TSemiRoutedDeliveriesAndReturnFlowCollection }

function TSemiRoutedDeliveriesAndReturnFlowCollection.Add:
  TSemiRoutedDeliveriesAndRunoffItem;
begin
  result := inherited Add as TSemiRoutedDeliveriesAndRunoffItem;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.Assign(
  Source: TPersistent);
var
  OtherFarmCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
begin
  if Source is TSemiRoutedDeliveriesAndReturnFlowCollection then
  begin
    OtherFarmCollection := TSemiRoutedDeliveriesAndReturnFlowCollection(Source);
    LowerLimitPestSeriesParameter := OtherFarmCollection.LowerLimitPestSeriesParameter;
    LowerLimitPestParamMethod := OtherFarmCollection.LowerLimitPestParamMethod;
    UpperLimitPestSeriesParameter := OtherFarmCollection.UpperLimitPestSeriesParameter;
    UpperLimitPestParamMethod := OtherFarmCollection.UpperLimitPestParamMethod;
  end;
  inherited;
end;

function TSemiRoutedDeliveriesAndReturnFlowCollection.GetItem(
  Index: Integer): TSemiRoutedDeliveriesAndRunoffItem;
begin
  result := inherited Items[index] as TSemiRoutedDeliveriesAndRunoffItem
end;

function TSemiRoutedDeliveriesAndReturnFlowCollection.GetLowerLimitPestSeriesParameter: string;
begin
  if FLowerLimitPestSeriesParameter <> nil then
  begin
    FLowerLimitPestSeriesParameterName := FLowerLimitPestSeriesParameter.ParameterName;
  end;
  result := FLowerLimitPestSeriesParameterName;
end;

function TSemiRoutedDeliveriesAndReturnFlowCollection.GetUpperLimitPestSeriesParameter: string;
begin
  if FUpperLimitPestSeriesParameter <> nil then
  begin
    FUpperLimitPestSeriesParameterName := FUpperLimitPestSeriesParameter.ParameterName;
  end;
  result := FUpperLimitPestSeriesParameterName;
end;

function TSemiRoutedDeliveriesAndReturnFlowCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  OtherFarmCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
begin
  result := (AnOrderedCollection is TSemiRoutedDeliveriesAndReturnFlowCollection)
    and inherited;
  if result then
  begin
    OtherFarmCollection := TSemiRoutedDeliveriesAndReturnFlowCollection(AnOrderedCollection);
    result := (LowerLimitPestSeriesParameter = OtherFarmCollection.LowerLimitPestSeriesParameter)
      and (LowerLimitPestParamMethod = OtherFarmCollection.LowerLimitPestParamMethod)
      and (UpperLimitPestSeriesParameter = OtherFarmCollection.UpperLimitPestSeriesParameter)
      and (UpperLimitPestParamMethod = OtherFarmCollection.UpperLimitPestParamMethod)
  end;
end;

class function TSemiRoutedDeliveriesAndReturnFlowCollection.ItemClass:
  TBoundaryItemClass;
begin
  result := TSemiRoutedDeliveriesAndRunoffItem;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
  inherited;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.SetItem(Index: Integer;
  const Value: TSemiRoutedDeliveriesAndRunoffItem);
begin
  inherited Items[index] := Value;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.SetLowerLimitPestParamMethod(
  const Value: TPestParamMethod);
begin
  if FLowerLimitPestParamMethod <> Value then
  begin
    FLowerLimitPestParamMethod := Value;
    InvalidateModel;
  end;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.SetPestLowerLimitSeriesParameter(
  const Value: string);
begin
  if FLowerLimitPestSeriesParameterName <> Value then
  begin
    FLowerLimitPestSeriesParameterName := Value;
    InvalidateModel;
  end;
  if (Model <> nil) then
  begin
    if Value <> '' then
    begin
      FLowerLimitPestSeriesParameter := Model.GetPestParameterByNameI(Value);
      if FLowerLimitPestSeriesParameter = nil then
      begin
        FLowerLimitPestSeriesParameterName := '';
        InvalidateModel;
      end;
    end
    else
    begin
      FLowerLimitPestSeriesParameter := nil;
    end;
  end;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.SetPestUpperLimitSeriesParameter(
  const Value: string);
begin
  if FUpperLimitPestSeriesParameterName <> Value then
  begin
    FUpperLimitPestSeriesParameterName := Value;
    InvalidateModel;
  end;
  if (Model <> nil) then
  begin
    if Value <> '' then
    begin
      FUpperLimitPestSeriesParameter := Model.GetPestParameterByNameI(Value);
      if FUpperLimitPestSeriesParameter = nil then
      begin
        FUpperLimitPestSeriesParameterName := '';
        InvalidateModel;
      end;
    end
    else
    begin
      FUpperLimitPestSeriesParameter := nil;
    end;
  end;
end;

procedure TSemiRoutedDeliveriesAndReturnFlowCollection.SetUpperLimitPestParamMethod(
  const Value: TPestParamMethod);
begin
  if FUpperLimitPestParamMethod <> Value then
  begin
    FUpperLimitPestParamMethod := Value;
    InvalidateModel;
  end;
end;

{ TNonRoutedDeliveryParameterItem }

procedure TNonRoutedDeliveryParameterItem.Assign(Source: TPersistent);
var
  OtherItem: TNonRoutedDeliveryParameterItem;
begin
  if Source is TNonRoutedDeliveryParameterItem then
  begin
    OtherItem := TNonRoutedDeliveryParameterItem(Source);
    NonRoutedDeliveryType := OtherItem.NonRoutedDeliveryType;
    NonRoutedDeliveryTypeOwhm2 := OtherItem.NonRoutedDeliveryTypeOwhm2;
  end;
  inherited;

end;

function TNonRoutedDeliveryParameterItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

function TNonRoutedDeliveryParameterItem.GetBoundaryFormula(
  Index: integer): string;
begin
  case Index of
    VolumePostion:
      result := Volume;
    RankPosition:
      result := Rank;
    VirtualFarmPosition:
      result := VirtualFarm;
    else Assert(False);
  end;
end;

function TNonRoutedDeliveryParameterItem.GetRank: string;
begin
  Result := FFormulaObjects[RankPosition].Formula;
  ResetItemObserver(RankPosition);
end;

function TNonRoutedDeliveryParameterItem.GetVirtualFarm: string;
begin
  Result := FFormulaObjects[VirtualFarmPosition].Formula;
  ResetItemObserver(VirtualFarmPosition);
end;

function TNonRoutedDeliveryParameterItem.GetVolume: string;
begin
  Result := FFormulaObjects[VolumePostion].Formula;
  ResetItemObserver(VolumePostion);
end;

function TNonRoutedDeliveryParameterItem.IsSame(
  AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TNonRoutedDeliveryParameterItem;
begin
  Result := (AnotherItem is TNonRoutedDeliveryParameterItem) and inherited;
  if result then
  begin
    OtherItem := TNonRoutedDeliveryParameterItem(AnotherItem);
    result :=( NonRoutedDeliveryType = OtherItem.NonRoutedDeliveryType)
      and (NonRoutedDeliveryTypeOwhm2 = OtherItem.NonRoutedDeliveryTypeOwhm2);
  end;
end;

procedure TNonRoutedDeliveryParameterItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    VolumePostion:
      Volume := Value;
    RankPosition:
      Rank := Value;
    VirtualFarmPosition:
      VirtualFarm := Value;
    else Assert(False);
  end;
end;

procedure TNonRoutedDeliveryParameterItem.SetNonRoutedDeliveryType(
  const Value: TNonRoutedDeliveryType);
begin
  if FNonRoutedDeliveryType <> Value then
  begin
    FNonRoutedDeliveryType := Value;
    InvalidateModel;
  end;
end;

procedure TNonRoutedDeliveryParameterItem.SetNonRoutedDeliveryTypeOwhm2(
  const Value: TNonRoutedDeliveryTypeOwhm2);
begin
  if FNonRoutedDeliveryTypeOwhm2 <> Value then
  begin
    FNonRoutedDeliveryTypeOwhm2 := Value;
    InvalidateModel;
  end;
end;

procedure TNonRoutedDeliveryParameterItem.SetRank(const Value: string);
begin
  if FFormulaObjects[RankPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, RankPosition, FFormulaObjects[RankPosition]);
  end;
end;

procedure TNonRoutedDeliveryParameterItem.SetVirtualFarm(const Value: string);
begin
  if Value = '' then
  begin
    Exit;
  end;
  if FFormulaObjects[VirtualFarmPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, VirtualFarmPosition, FFormulaObjects[VirtualFarmPosition]);
  end;
end;

procedure TNonRoutedDeliveryParameterItem.SetVolume(const Value: string);
begin
  if FFormulaObjects[VolumePostion].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, VolumePostion, FFormulaObjects[VolumePostion]);
  end;
end;

{ TNonRoutedDeliveryParameterCollection }

function TNonRoutedDeliveryParameterCollection.Add: TNonRoutedDeliveryParameterItem;
begin
  result := inherited Add as TNonRoutedDeliveryParameterItem;
end;

function TNonRoutedDeliveryParameterCollection.GetItem(
  Index: Integer): TNonRoutedDeliveryParameterItem;
begin
  result := inherited Items[Index] as TNonRoutedDeliveryParameterItem;
end;

class function TNonRoutedDeliveryParameterCollection.ItemClass: TBoundaryItemClass;
begin
  result := TNonRoutedDeliveryParameterItem;
end;

procedure TNonRoutedDeliveryParameterCollection.SetItem(Index: Integer;
  const Value: TNonRoutedDeliveryParameterItem);
begin
  inherited Items[Index] := Value;
end;

{ TDeliveryParamItem }

procedure TDeliveryParamItem.Assign(Source: TPersistent);
begin
  if Source is TDeliveryParamItem  then
  begin
    DeliveryParam := TDeliveryParamItem(Source).DeliveryParam;
  end;
  inherited;
end;

constructor TDeliveryParamItem.Create(Collection: TCollection);
begin
  inherited;
  FDeliveryParam := TNonRoutedDeliveryParameterCollection.Create(Model as TCustomModel);
end;

destructor TDeliveryParamItem.Destroy;
begin
  FDeliveryParam.Free;
  inherited;
end;

function TDeliveryParamItem.IsSame(
  AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDeliveryParamItem) {and inherited IsSame(AnotherItem)};
  if result then
  begin
    result := DeliveryParam.IsSame(TDeliveryParamItem(AnotherItem).DeliveryParam)
  end;
end;

procedure TDeliveryParamItem.SetDeliveryParam(
  const Value: TNonRoutedDeliveryParameterCollection);
begin
  FDeliveryParam.Assign(Value);
end;

{ TDeliveryParamCollection }

constructor TDeliveryParamCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TDeliveryParamItem, Model);
end;

function TDeliveryParamCollection.GetItem(Index: Integer): TDeliveryParamItem;
begin
  result := inherited Items[Index] as TDeliveryParamItem;
end;

procedure TDeliveryParamCollection.SetItem(Index: Integer;
  const Value: TDeliveryParamItem);
begin
  inherited Items[Index] := Value;
end;

{ TWaterRightsCollection }

function TWaterRightsCollection.Add: TWaterRightsItem;
begin
  result := inherited Add as TWaterRightsItem;
end;

function TWaterRightsCollection.GetItem(Index: Integer): TWaterRightsItem;
begin
  result := inherited Items[Index] as TWaterRightsItem;
end;

class function TWaterRightsCollection.ItemClass: TBoundaryItemClass;
begin
  result := TWaterRightsItem;
end;

procedure TWaterRightsCollection.SetItem(Index: Integer;
  const Value: TWaterRightsItem);
begin
  inherited Items[Index] := Value;
end;

{ TCropEfficiencyItem }

function TCropEfficiencyItem.BoundaryFormulaCount: integer;
begin
  Result := 1;
end;

function TCropEfficiencyItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    EfficiencyPosition:
      result := Efficiency;
    else
      Assert(False);
  end;
end;

function TCropEfficiencyItem.GetEfficiency: string;
begin
  Result := FFormulaObjects[EfficiencyPosition].Formula;
  ResetItemObserver(EfficiencyPosition);
end;

procedure TCropEfficiencyItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    EfficiencyPosition:
      Efficiency := Value;
    else
      Assert(False);
  end;
end;

procedure TCropEfficiencyItem.SetEfficiency(const Value: string);
begin
  if FFormulaObjects[EfficiencyPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, EfficiencyPosition, FFormulaObjects[EfficiencyPosition]);
  end;
end;

{ TCropEfficiencyCollection }

procedure TCropEfficiencyCollection.Assign(Source: TPersistent);
begin
  if Source is TCropEfficiencyCollection then
  begin
    CropName := TCropEfficiencyCollection(Source).CropName
  end;
  inherited;
end;

function TCropEfficiencyCollection.First: TCropEfficiencyItem;
begin
  result := inherited First as TCropEfficiencyItem;
end;

function TCropEfficiencyCollection.GetItem(Index: Integer): TCropEfficiencyItem;
begin
  result := inherited Items[index] as TCropEfficiencyItem;
end;

function TCropEfficiencyCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := (AnOrderedCollection is TCropEfficiencyCollection)
    and inherited IsSame(AnOrderedCollection);
  if result then
  begin
    result := CropName = TCropEfficiencyCollection(AnOrderedCollection).CropName
  end;
end;

class function TCropEfficiencyCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TCropEfficiencyItem;
end;

procedure TCropEfficiencyCollection.SetCropName(const Value: string);
begin
  if FCropName <> Value then
  begin
    FCropName := Value;
    InvalidateModel;
  end;
end;

procedure TCropEfficiencyCollection.SetItem(Index: Integer;
  const Value: TCropEfficiencyItem);
begin
  inherited Items[index] := Value;
end;

{ TFarmEfficienciesItem }

procedure TFarmEfficienciesItem.Assign(Source: TPersistent);
begin
  if Source is TFarmEfficienciesItem then
  begin
    CropEfficiency := TFarmEfficienciesItem(Source).CropEfficiency;
  end;
  inherited;
end;

constructor TFarmEfficienciesItem.Create(Collection: TCollection);
begin
  inherited;
  FCropEfficiency := TCropEfficiencyCollection.Create(Model as TCustomModel);
end;

destructor TFarmEfficienciesItem.Destroy;
begin
  FCropEfficiency.Free;
  inherited;
end;

function TFarmEfficienciesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TFarmEfficienciesItem) and
    CropEfficiency.IsSame(TFarmEfficienciesItem(AnotherItem).CropEfficiency)
end;

procedure TFarmEfficienciesItem.Loaded;
begin
  CropEfficiency.Loaded;
end;

procedure TFarmEfficienciesItem.SetCropEfficiency(
  const Value: TCropEfficiencyCollection);
begin
  FCropEfficiency.Assign(Value);
end;

{ TFarm }

procedure TFarm.AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
  Times: TRealList; StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean);
var
  BoundaryIndex: Integer;
  Boundary: TCustomModflowBoundaryItem;
  SP_Epsilon: Double;
  CosestIndex: Integer;
  ExistingTime: Double;
begin
  SP_Epsilon := (Model as TCustomModel).SP_Epsilon;
  for BoundaryIndex := 0 to BoundCol.Count - 1 do
  begin
    Boundary := BoundCol[BoundaryIndex] as TCustomModflowBoundaryItem;
    CosestIndex := Times.IndexOfClosest(Boundary.StartTime);
    if CosestIndex >= 0 then
    begin
      ExistingTime := Times[CosestIndex];
      if Abs(ExistingTime-Boundary.StartTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.StartTime);
      end;
    end;
    CosestIndex := Times.IndexOfClosest(Boundary.EndTime);
    if CosestIndex >= 0 then
    begin
      ExistingTime := Times[CosestIndex];
      if Abs(ExistingTime-Boundary.EndTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.EndTime);
      end;
    end;
//    Times.AddUnique(Boundary.StartTime);
//    Times.AddUnique(Boundary.EndTime);
    if (Boundary.StartTime < StartTestTime-SP_Epsilon) then
    begin
      StartRangeExtended := True;
    end;
    if (Boundary.EndTime > EndTestTime+SP_Epsilon) then
    begin
      EndRangeExtended := True;
    end;
//    if (Boundary.StartTime < StartTestTime) then
//    begin
//      StartRangeExtended := True;
//    end;
//    if (Boundary.EndTime > EndTestTime) then
//    begin
//      EndRangeExtended := True;
//    end;
  end;
end;

procedure TFarm.Assign(Source: TPersistent);
var
  SourceFarm: TFarm;
begin
  if Source is TFarm then
  begin
    SourceFarm := TFarm(Source);
    FarmId := SourceFarm.FarmId;
    FarmEfficiencyCollection := SourceFarm.FarmEfficiencyCollection;
    FarmCostsCollection := SourceFarm.FarmCostsCollection;
    SemiRoutedDeliveries := SourceFarm.SemiRoutedDeliveries;
    SemiRoutedReturnFlow := SourceFarm.SemiRoutedReturnFlow;
    DeliveryParamCollection := SourceFarm.DeliveryParamCollection;
    WaterRights := SourceFarm.WaterRights;
    GwAllotment := SourceFarm.GwAllotment;
    FarmName := SourceFarm.FarmName;

    FarmIrrigationEfficiencyCollection := SourceFarm.FarmIrrigationEfficiencyCollection;
    FarmIrrigationEfficiencyImprovementCollection := SourceFarm.FarmIrrigationEfficiencyImprovementCollection;
    AddedDemandRunoffSplitCollection := SourceFarm.AddedDemandRunoffSplitCollection;
    IrrigationUniformity := SourceFarm.IrrigationUniformity;
    DeficiencyScenario := SourceFarm.DeficiencyScenario;
    WaterSource := SourceFarm.WaterSource;
    BareRunoffFraction := SourceFarm.BareRunoffFraction;
    AddedCropDemandFlux := SourceFarm.AddedCropDemandFlux;
    AddedCropDemandRate := SourceFarm.AddedCropDemandRate;
    NoReturnFlow := SourceFarm.NoReturnFlow;
    MultiSrDeliveries := SourceFarm.MultiSrDeliveries;
    MultiSrReturns := SourceFarm.MultiSrReturns;
    SWAllotment := SourceFarm.SWAllotment;
    SaltSupplyConcentrationCollection := SourceFarm.SaltSupplyConcentrationCollection;
    PumpSpreadChoice := SourceFarm.PumpSpreadChoice;

    FarmGUID := SourceFarm.FarmGUID;

  end
  else
  begin
    inherited;
  end;
end;

function TFarmEfficiencyCollection.Add: TFarmEfficienciesItem;
begin
  Result := inherited Add as TFarmEfficienciesItem;
end;

constructor TFarmEfficiencyCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TFarmEfficienciesItem, Model);
//  FWaterRights := TWaterRightsCollection.Create(Model);
//  FSemiRoutedDeliveries := TSemiRoutedDeliveriesAndRunoffCollection.Create(Model);
//  FDeliveryParamCollection := TDeliveryParamCollection.Create(Model);
//  FSemiRoutedRunoff := TSemiRoutedDeliveriesAndRunoffCollection.Create(Model);
//  FFarmCostsCollection := TFarmCostsCollection.Create(Model);
end;

constructor TFarm.Create(Collection: TCollection);
var
  LocalModel: TCustomModel;
  InvalidateEvent: TNotifyEvent;
  NewGUID: TGUID;
begin
  inherited Create(Collection);
  FPumpSpreadChoice := pscTopNode;
  if CreateGUID(NewGUID) = 0 then
  begin
    FarmGUID := GUIDToString(NewGUID);
  end;
  LocalModel := Model as TCustomModel;
  if LocalModel = nil then
  begin
    InvalidateEvent := nil;
  end
  else
  begin
    InvalidateEvent := LocalModel.DoInvalidate
  end;
  FFarmEfficiencyCollection := TFarmEfficiencyCollection.Create(Model);
  FWaterRights := TWaterRightsCollection.Create(LocalModel);
  FSemiRoutedDeliveries := TSemiRoutedDeliveriesAndReturnFlowCollection.Create(LocalModel);
  FDeliveryParamCollection := TDeliveryParamCollection.Create(Model);
  FSemiRoutedReturnFlow := TSemiRoutedDeliveriesAndReturnFlowCollection.Create(LocalModel);
  FFarmCostsCollection := TFarmCostsCollection.Create(LocalModel);
  FGwAllotment := TAllotmentCollection.Create(LocalModel);

  FFarmIrrigationEfficiencyCollection := TFarmEfficiencyCollection.Create(Model);
  FFarmIrrigationEfficiencyImprovementCollection := TFarmEfficiencyCollection.Create(Model);
  FAddedDemandRunoffSplitCollection := TFarmEfficiencyCollection.Create(Model);
  FIrrigationUniformity := TFarmEfficiencyCollection.Create(Model);
  FDeficiencyScenario := TDeficiencyScenarioCollection.Create(LocalModel);
  FWaterSource := TWaterSourceCollection.Create(LocalModel);
  FBareRunoffFraction := TBareRunoffFractionCollection.Create(LocalModel);
  FAddedCropDemandFlux := TFarmEfficiencyCollection.Create(Model);
  FAddedCropDemandRate := TFarmEfficiencyCollection.Create(Model);
  FNoReturnFlow := TNoReturnCollection.Create(LocalModel);
  FMultiSrd := TMultiSrdCollection.Create(Model);
  FMultiSrReturns := TMultiSrdCollection.Create(Model);
  FSWAllotment := TAllotmentCollection.Create(LocalModel);
  FSaltSupplyConcentrationCollection := TSaltSupplyConcentrationCollection.Create(LocalModel);
end;

destructor TFarm.Destroy;
begin
  FSaltSupplyConcentrationCollection.Free;
  FSWAllotment.Free;
  FMultiSrReturns.Free;
  FMultiSrd.Free;
  FNoReturnFlow.Free;
  FAddedCropDemandFlux.Free;
  FAddedCropDemandRate.Free;
  FBareRunoffFraction.Free;
  FWaterSource.Free;
  FDeficiencyScenario.Free;
  FIrrigationUniformity.Free;
  FAddedDemandRunoffSplitCollection.Free;
  FFarmIrrigationEfficiencyImprovementCollection.Free;
  FFarmIrrigationEfficiencyCollection.Free;

  FGwAllotment.Free;
  FFarmCostsCollection.Free;
  FSemiRoutedReturnFlow.Free;
  FDeliveryParamCollection.Free;
  FSemiRoutedDeliveries.Free;
  FWaterRights.Free;
  FFarmEfficiencyCollection.Free;
  inherited;
end;

function TFarm.GetCurrentFarmEfficiencyCollection: TFarmEfficiencyCollection;
begin
  if frmGophast.ModelSelection = msModflowFmp then
  begin
    result := FarmEfficiencyCollection;
  end
  else
  begin
    result := FarmIrrigationEfficiencyCollection;
  end;
end;

procedure TFarm.Loaded;
begin
  SemiRoutedDeliveries.Loaded;
  SemiRoutedReturnFlow.Loaded;

  FarmEfficiencyCollection.Loaded;
  FarmIrrigationEfficiencyCollection.Loaded;
  FarmIrrigationEfficiencyImprovementCollection.Loaded;
  IrrigationUniformity.Loaded;
  AddedCropDemandFlux.Loaded;
  AddedCropDemandRate.Loaded;

  FarmCostsCollection.Loaded;
  WaterRights.Loaded;
  GwAllotment.Loaded;
  AddedDemandRunoffSplitCollection.Loaded;
  IrrigationUniformity.Loaded;
  DeficiencyScenario.Loaded;
  WaterSource.Loaded;
  BareRunoffFraction.Loaded;
  NoReturnFlow.Loaded;
  MultiSrDeliveries.Loaded;
  MultiSrReturns.Loaded;
  SWAllotment.Loaded;
  SaltSupplyConcentrationCollection.Loaded;

end;

function TFarm.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceFarm: TFarm;
begin
  Result := (AnotherItem is TFarm);
//    and inherited IsSame(AnotherItem);
  if result then
  begin
    SourceFarm := TFarm(AnotherItem);
    result :=
      (FarmId = SourceFarm.FarmId)
      and (PumpSpreadChoice = SourceFarm.PumpSpreadChoice)
      and FarmEfficiencyCollection.IsSame(SourceFarm.FarmEfficiencyCollection)
      and FarmCostsCollection.IsSame(SourceFarm.FarmCostsCollection)
      and SemiRoutedDeliveries.IsSame(SourceFarm.SemiRoutedDeliveries)
      and SemiRoutedReturnFlow.IsSame(SourceFarm.SemiRoutedReturnFlow)
      and DeliveryParamCollection.IsSame(SourceFarm.DeliveryParamCollection)
      and WaterRights.IsSame(SourceFarm.WaterRights)
      and GwAllotment.IsSame(SourceFarm.GwAllotment)
      and (FarmName = SourceFarm.FarmName)

      and FarmIrrigationEfficiencyCollection.IsSame(
        SourceFarm.FarmIrrigationEfficiencyCollection)
      and FarmIrrigationEfficiencyImprovementCollection.IsSame(
        SourceFarm.FarmIrrigationEfficiencyImprovementCollection)
      and AddedDemandRunoffSplitCollection.IsSame(
        SourceFarm.AddedDemandRunoffSplitCollection)
      and IrrigationUniformity.IsSame(
        SourceFarm.IrrigationUniformity)
      and DeficiencyScenario.IsSame(SourceFarm.DeficiencyScenario)
      and WaterSource.IsSame(SourceFarm.WaterSource)
      and BareRunoffFraction.IsSame(SourceFarm.BareRunoffFraction)
      and AddedCropDemandFlux.IsSame(SourceFarm.AddedCropDemandFlux)
      and AddedCropDemandRate.IsSame(SourceFarm.AddedCropDemandRate)
      and NoReturnFlow.IsSame(SourceFarm.NoReturnFlow)
      and MultiSrDeliveries.IsSame(SourceFarm.MultiSrDeliveries)
      and MultiSrReturns.IsSame(SourceFarm.MultiSrReturns)
      and SWAllotment.IsSame(SourceFarm.SWAllotment)
      and SaltSupplyConcentrationCollection.IsSame(
        SourceFarm.SaltSupplyConcentrationCollection)

  end;

end;

procedure TFarm.SetAddedCropDemandFlux(const Value: TFarmEfficiencyCollection);
begin
  FAddedCropDemandFlux.Assign(Value);
end;

procedure TFarm.SetAddedCropDemandRate(const Value: TFarmEfficiencyCollection);
begin
  FAddedCropDemandRate.Assign(Value);
end;

procedure TFarm.SetAddedDemandRunoffSplitCollection(
  const Value: TFarmEfficiencyCollection);
begin
  FAddedDemandRunoffSplitCollection.Assign(Value);
end;

procedure TFarm.SetBareRunoffFraction(
  const Value: TBareRunoffFractionCollection);
begin
  FBareRunoffFraction.Assign(Value);
end;

procedure TFarm.SetCurrentFarmEfficiencyCollection(
  const Value: TFarmEfficiencyCollection);
begin
  if frmGophast.ModelSelection = msModflowFmp then
  begin
    FarmEfficiencyCollection := Value;
  end
  else
  begin
    FarmIrrigationEfficiencyCollection := Value;
  end;
end;

procedure TFarm.SetDeficiencyScenario(const Value: TDeficiencyScenarioCollection);
begin
  FDeficiencyScenario.Assign(Value);
end;

procedure TFarm.SetDeliveryParamCollection(
  const Value: TDeliveryParamCollection);
begin
  FDeliveryParamCollection.Assign(Value);
end;

procedure TFarm.SetFarmCostsCollection(
  const Value: TFarmCostsCollection);
begin
  FFarmCostsCollection.Assign(Value);
end;

procedure TFarm.SetFarmEfficiencyCollection(
  const Value: TFarmEfficiencyCollection);
begin
  FFarmEfficiencyCollection.Assign(Value);
end;

procedure TFarm.SetFarmGUID(const Value: string);
begin
  FFarmGUID := Value;
end;

procedure TFarm.SetFarmId(const Value: Integer);
begin
  SetIntegerProperty(FFarmId, Value);
//  if FFarmId <> Value then
//  begin
//    FFarmId := Value;
//    InvalidateModel;
//  end;
end;

procedure TFarm.SetFarmIrrigationEfficiencyCollection(
  const Value: TFarmEfficiencyCollection);
begin
  FFarmIrrigationEfficiencyCollection.Assign(Value)
end;

procedure TFarm.SetFarmIrrigationEfficiencyImprovementCollection(
  const Value: TFarmEfficiencyCollection);
begin
  FFarmIrrigationEfficiencyImprovementCollection.Assign(Value);
end;

procedure TFarm.SetFarmName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FFarmName, Value);
end;

procedure TFarm.SetGwAllotment(const Value: TAllotmentCollection);
begin
  FGwAllotment.Assign(Value);
end;

procedure TFarm.SetIrrigationUniformity(const Value: TFarmEfficiencyCollection);
begin
  FIrrigationUniformity.Assign(Value);
end;

procedure TFarm.SetMultiSrd(const Value: TMultiSrdCollection);
begin
  FMultiSrd.Assign(Value);
end;

procedure TFarm.SetMultiSrReturns(const Value: TMultiSrdCollection);
begin
  FMultiSrReturns.Assign(Value);
end;

procedure TFarm.SetNoReturnFlow(const Value: TNoReturnCollection);
begin
  FNoReturnFlow.Assign(Value);
end;

procedure TFarm.SetPumpSpreadChoice(const Value: TOwhmV2PumpSpreadChoice);
begin
  if FPumpSpreadChoice <> Value then
  begin
    FPumpSpreadChoice := Value;
    InvalidateModel;
  end;
end;

procedure TFarm.SetSaltSupplyConcentrationCollection(
  const Value: TSaltSupplyConcentrationCollection);
begin
  FSaltSupplyConcentrationCollection.Assign(Value);
end;

procedure TFarm.SetSemiRoutedDeliveries(
  const Value: TSemiRoutedDeliveriesAndReturnFlowCollection);
begin
  FSemiRoutedDeliveries.Assign(Value);
end;

procedure TFarm.SetSemiRoutedReturnFlow(
  const Value: TSemiRoutedDeliveriesAndReturnFlowCollection);
begin
  FSemiRoutedReturnFlow.Assign(Value);
end;

procedure TFarm.SetSWAllotment(const Value: TAllotmentCollection);
begin
  FSWAllotment.Assign(Value);
end;

procedure TFarm.SetWaterRights(
  const Value: TWaterRightsCollection);
begin
  FWaterRights.Assign(Value);
end;

procedure TFarm.SetWaterSource(const Value: TWaterSourceCollection);
begin
  FWaterSource.Assign(Value);
end;

procedure TFarm.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean);
var
  DelivIndex: Integer;
  Deliv: TNonRoutedDeliveryParameterCollection;
  EffIndex: Integer;
  EfficiencyCol: TCropEfficiencyCollection;
begin
  AddBoundaryTimes(FWaterRights, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(FSemiRoutedDeliveries, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(FSemiRoutedReturnFlow, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(FFarmCostsCollection, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(FGwAllotment, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);

  for EffIndex := 0 to FarmEfficiencyCollection.Count - 1 do
  begin
    EfficiencyCol := FarmEfficiencyCollection[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  for DelivIndex := 0 to FDeliveryParamCollection.Count - 1 do
  begin
    Deliv := FDeliveryParamCollection[DelivIndex].FDeliveryParam;
    AddBoundaryTimes(Deliv, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  for EffIndex := 0 to FarmIrrigationEfficiencyCollection.Count - 1 do
  begin
    EfficiencyCol := FarmIrrigationEfficiencyCollection[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  for EffIndex := 0 to FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
  begin
    EfficiencyCol := FarmIrrigationEfficiencyImprovementCollection[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  for EffIndex := 0 to AddedDemandRunoffSplitCollection.Count - 1 do
  begin
    EfficiencyCol := AddedDemandRunoffSplitCollection[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  for EffIndex := 0 to IrrigationUniformity.Count - 1 do
  begin
    EfficiencyCol := IrrigationUniformity[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  AddBoundaryTimes(DeficiencyScenario, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(WaterSource, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(BareRunoffFraction, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);

  for EffIndex := 0 to AddedCropDemandFlux.Count - 1 do
  begin
    EfficiencyCol := AddedCropDemandFlux[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;

  for EffIndex := 0 to AddedCropDemandRate.Count - 1 do
  begin
    EfficiencyCol := AddedCropDemandRate[EffIndex].CropEfficiency;
    AddBoundaryTimes(EfficiencyCol, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;
end;

function TFarm.Used: boolean;
begin
  result := (FarmEfficiencyCollection.Count > 0)
    or (FarmIrrigationEfficiencyCollection.Count > 0)
end;

{ TWaterRightsItem }

function TWaterRightsItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

function TWaterRightsItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    WaterRightsPosition:
      result := WaterRights;
    else
      Assert(False);
  end;
end;

function TWaterRightsItem.GetWaterRights: string;
begin
  Result := FFormulaObjects[WaterRightsPosition].Formula;
  ResetItemObserver(WaterRightsPosition);
end;

procedure TWaterRightsItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    WaterRightsPosition:
      WaterRights := Value;
    else
      Assert(False);
  end;
end;

procedure TWaterRightsItem.SetWaterRights(const Value: string);
begin
  if FFormulaObjects[WaterRightsPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, WaterRightsPosition, FFormulaObjects[WaterRightsPosition]);
  end;
end;

{ TSfrDiversionObject }

procedure TSfrDiversionObject.Assign(Source: TPersistent);
var
  DivObj: TSfrDiversionObject;
begin
  if Source is TSfrDiversionObject then
  begin
    DivObj := TSfrDiversionObject(Source);
    ScreenObject := DivObj.ScreenObject;
    ObjectName := DivObj.ObjectName;
    DiversionPosition := DivObj.DiversionPosition;
    DiversionVertex := DivObj.DiversionVertex;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSfrDiversionObject.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
  end
  else
  begin
    inherited Create(Model.DoInvalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FDiversionPosition := dpStart;
  FModel := Model;
end;

function TSfrDiversionObject.GetObjectName: string;
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

function TSfrDiversionObject.GetScreenObject: TObject;
begin
  if (FScreenObject = nil) and (ObjectName  <> '') then
  begin
    if Model <> nil then
    begin
      FScreenObject := (Model as TCustomModel).GetScreenObjectByName(ObjectName);
    end;
  end;
  if ValidScreenObject(FScreenObject) then
  begin
    result := FScreenObject as TObject;
  end
  else
  begin
    result := nil;
  end;
end;

function TSfrDiversionObject.IsSame(OtherReturnObject: TSfrDiversionObject): boolean;
begin
  result := (ObjectName = OtherReturnObject.ObjectName)
    and (DiversionPosition = OtherReturnObject.DiversionPosition)
    and (DiversionVertex = OtherReturnObject.DiversionVertex);
end;

procedure TSfrDiversionObject.Loaded;
begin
  GetScreenObject;
end;

procedure TSfrDiversionObject.SetDiversionPosition(
  const Value: TDiversionPosition);
begin
  if FDiversionPosition <> Value then
  begin
    FDiversionPosition := Value;
    InvalidateModel;
  end;
end;

procedure TSfrDiversionObject.SetDiversionVertex(const Value: integer);
begin
  if FDiversionVertex <> Value then
  begin
    FDiversionVertex := Value;
    InvalidateModel;
  end;
end;

procedure TSfrDiversionObject.SetObjectName(const Value: string);
begin
  if FObjectName <> Value then
  begin
    FObjectName := Value;
    InvalidateModel;
  end;
end;

procedure TSfrDiversionObject.SetScreenObject(const Value: TObject);
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

class function TSfrDiversionObject.ValidScreenObject(AScreenObject: TObject): boolean;
var
  ScreenObject: TScreenObject;
begin
  result := (AScreenObject <> nil);
  if result then
  begin
    ScreenObject := AScreenObject as TScreenObject;
    result := not ScreenObject.Deleted;
    if result then
    begin
      result := ((ScreenObject.ModflowSfrBoundary <> nil)
      and (ScreenObject.ModflowSfrBoundary.Used))
      or ((ScreenObject.ModflowSwrReaches <> nil)
      and (ScreenObject.ModflowSwrReaches.Used))
    end;
  end
end;

{ TSfrDiversion }

procedure TSfrDiversion.Assign(Source: TPersistent);
var
  Diversion: TSfrDiversion;
begin
  if Source is TSfrDiversion then
  begin
    Diversion := TSfrDiversion(Source);
    DiversionChoice := Diversion.DiversionChoice;
    DiversionCell := Diversion.DiversionCell;
    DiversionLocation := Diversion.DiversionLocation;
    DiversionObject := Diversion.DiversionObject;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSfrDiversion.Create(Model: TBaseModel);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(OnInvalidateModelEvent);
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FDiversionCell := TReturnCell.Create(OnInvalidateModelEvent);
  FDiversionLocation := TReturnLocation.Create(OnInvalidateModelEvent);
  FDiversionObject := TSfrDiversionObject.Create(Model);
end;

destructor TSfrDiversion.Destroy;
begin
  FDiversionCell.Free;
  FDiversionLocation.Free;
  FDiversionObject.Free;
  inherited;
end;

function TSfrDiversion.IsSame(OtherDrainReturn: TSfrDiversion): boolean;
begin
  result := DiversionChoice = OtherDrainReturn.DiversionChoice;
  if Result then
  begin
    case DiversionChoice of
      rtNone: ;
      rtObject: result := DiversionObject.IsSame(OtherDrainReturn.DiversionObject);
      rtLocation: result := DiversionLocation.IsSame(OtherDrainReturn.DiversionLocation);
      rtCell: result := DiversionCell.IsSame(OtherDrainReturn.DiversionCell);
      else Assert(False);
    end;
  end;
end;

procedure TSfrDiversion.Loaded;
begin
  if DiversionChoice = rtObject then
  begin
    DiversionObject.Loaded;
  end;
end;

function TSfrDiversion.ReturnCellLocation(AModel: TBaseModel): TCellLocation;
var
  ScreenObject: TScreenObject;
//  X, Y {, Z}: double;
  Model: TCustomModel;
  Grid: TModflowGrid;
  APoint: TPoint2D;
  function LocationToCell: TCellLocation;
  begin
    Model := AModel as TCustomModel;
    Grid := Model.ModflowGrid;
    APoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
    if (APoint.X < Grid.ColumnPosition[Grid.ColumnCount]) and
      (APoint.Y > Grid.RowPosition[Grid.RowCount]) then
    begin
      result.Column := Grid.GetContainingColumn(APoint.X)+1;
      result.Row := Grid.GetContainingRow(APoint.Y)+1;
      result.Layer := 0;
    end
    else
    begin
      result.Layer := 0;
      result.Row := 0;
      result.Column := 0;
    end;
  end;

begin
  case DiversionChoice of
    rtNone:
      begin
        result.Layer := 0;
        result.Row := 0;
        result.Column := 0;
      end;
    rtObject:
      begin
        ScreenObject := DiversionObject.ScreenObject as TScreenObject;
        if ScreenObject = nil then
        begin
          result.Layer := 0;
          result.Row := 0;
          result.Column := 0;
        end
        else
        begin
          case DiversionObject.DiversionPosition of
            dpStart:
              begin
                APoint := ScreenObject.Points[0];
              end;
            dpMiddle:
              begin
                if DiversionObject.DiversionVertex <= ScreenObject.Count then
                begin
                  APoint := ScreenObject.Points[DiversionObject.DiversionVertex-1];
                end
                else
                begin
                  APoint := ScreenObject.Points[ScreenObject.Count-1];
                end;
              end;
            dpEnd:
              begin
                APoint := ScreenObject.Points[ScreenObject.Count-1];
              end;
            else
              Assert(False);
          end;
          result := LocationToCell;
        end;
      end;
    rtLocation:
      begin
        APoint.X := DiversionLocation.X;
        APoint.Y := DiversionLocation.Y;
        result := LocationToCell;
      end;
    rtCell:
      begin
        result.Layer := DiversionCell.Lay;
        result.Row := DiversionCell.Row;
        result.Column := DiversionCell.Col;
      end;
    else Assert(False);
  end;
end;

procedure TSfrDiversion.SetDiversionObject(const Value: TSfrDiversionObject);
begin
  FDiversionObject.Assign(Value);
end;

function TSfrDiversion.SegmentReach: TSegmentReach;
var
  ScreenObject: TScreenObject;
  PointNumber: Integer;
  SegmentIndex: NativeInt;
  ASegment: TCellElementSegment;
  ReachNumber: Integer;
  PriorSegment: TCellElementSegment;
begin
  Result.Segment := 0;
  Result.Reach := 0;
  if DiversionChoice = rtObject then
  begin
    ScreenObject := DiversionObject.ScreenObject as TScreenObject;
    if ScreenObject <> nil then
    begin
      Result.Segment := ScreenObject.SfrSegmentNumber;
      PointNumber := -1;
      case DiversionObject.DiversionPosition of
        dpStart:
          begin
            PointNumber := 0;
          end;
        dpMiddle:
          begin
            if DiversionObject.DiversionVertex <= ScreenObject.Count then
            begin
              PointNumber := DiversionObject.DiversionVertex-1;
            end
            else
            begin
              PointNumber := ScreenObject.Count-1;
            end;
          end;
        dpEnd:
          begin
            PointNumber := ScreenObject.Count-1;
          end;
        else
          Assert(False);
      end;
      ReachNumber := 0;
      PriorSegment := nil;
      for SegmentIndex := 0 to ScreenObject.Segments[Model].Count - 1 do
      begin
        ASegment := ScreenObject.Segments[Model][SegmentIndex];
        if (PriorSegment = nil) or (PriorSegment.Col <> ASegment.Col)
          or (PriorSegment.Row <> ASegment.Row)
          or (PriorSegment.Layer <> ASegment.Layer) then
        begin
          Inc(ReachNumber);
        end;
        if ASegment.VertexIndex = PointNumber then
        begin
          Result.Reach := ReachNumber;
          break;
        end;
        PriorSegment := ASegment;
      end;
      if (Result.Segment > 0) and (Result.Reach = 0)
        and (DiversionObject.DiversionPosition in [dpEnd, dpMiddle]) then
      begin
        Result.Reach := ReachNumber;
      end;
    end;
  end;
end;

procedure TSfrDiversion.SetDiversionCell(const Value: TReturnCell);
begin
  FDiversionCell.Assign(Value);
end;

procedure TSfrDiversion.SetDiversionChoice(const Value: TReturnChoice);
begin
  if FDiversinChoice <> Value then
  begin
    FDiversinChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSfrDiversion.SetDiversionLocation(const Value: TReturnLocation);
begin
  FDiversionLocation.Assign(Value);
end;

function TSfrDiversion.StoreDiversionObject: boolean;
begin
  result := DiversionChoice = rtObject;
end;

function TSfrDiversion.StoreDiversionCell: boolean;
begin
  result := DiversionChoice = rtCell;
end;

function TSfrDiversion.StoreDiversionLocation: boolean;
begin
  result := DiversionChoice = rtLocation;
end;

function TFarmEfficiencyCollection.GetFirst: TFarmEfficienciesItem;
begin
  result := inherited First as TFarmEfficienciesItem;
end;

function TFarmEfficiencyCollection.GetItem(
  Index: Integer): TFarmEfficienciesItem;
begin
  result := inherited Items[Index] as TFarmEfficienciesItem;
end;

procedure TFarmEfficiencyCollection.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;


procedure TFarmEfficiencyCollection.SetItem(Index: Integer;
  const Value: TFarmEfficienciesItem);
begin
  inherited Items[Index] := Value;
end;

{ TFarmCollection }

function TFarmCollection.Add: TFarm;
begin
  result := inherited Add as TFarm;
end;

constructor TFarmCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TFarm, Model);
end;

function TFarmCollection.GetItem(Index: Integer): TFarm;
begin
  result := inherited Items[index] as TFarm;
end;

function TFarmCollection.Last: TFarm;
begin
  result := inherited Last as TFarm;
end;

procedure TFarmCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;

end;

procedure TFarmCollection.SetItem(Index: Integer; const Value: TFarm);
begin
  inherited Items[index] := Value;
end;

function CompareFarms(Item1, Item2: Pointer): Integer;
var
  Farm1: TFarm;
  Farm2: TFarm;
begin
  Farm1 := Item1;
  Farm2 := Item2;
  result := Farm1.FarmId - Farm2.FarmId;
end;

procedure TFarmCollection.Sort;
var
  List: TList;
  Index: Integer;
  AFarm: TFarm;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[index]);
    end;
    List.Sort(CompareFarms);
    for Index := 0 to List.Count - 1 do
    begin
      AFarm := List[index];
      AFarm.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

{ TDefaultScenarioItem }

class function TDeficiencyScenarioItem.DefaultFormula: string;
begin
  result := 'True';
end;

{ TDefaultScenarioCollection }

class function TDeficiencyScenarioCollection.ItemClass: TBoundaryItemClass;
begin
  result := TDeficiencyScenarioItem;
end;

{ TWaterSourceItem }

function TWaterSourceItem.BoundaryFormulaCount: integer;
begin
  result :=3;
end;

class function TWaterSourceItem.DefaultFormula: string;
begin
  result := 'True';
end;

function TWaterSourceItem.GetBoundaryFormula(Index: integer): string;
begin
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

procedure TWaterSourceItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

{ TWaterSourceCollection }

function TWaterSourceCollection.First: TWaterSourceItem;
begin
  result := inherited First as TWaterSourceItem;
end;

function TWaterSourceCollection.GetItem(Index: Integer): TWaterSourceItem;
begin
  result := inherited Items[Index] as TWaterSourceItem;
end;

function TWaterSourceCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := (AnOrderedCollection is TWaterSourceCollection)
    and inherited IsSame(AnOrderedCollection);
end;

class function TWaterSourceCollection.ItemClass: TBoundaryItemClass;
begin
  result := TWaterSourceItem;
end;

procedure TWaterSourceCollection.SetItem(Index: Integer;
  const Value: TWaterSourceItem);
begin
  inherited Items[Index] := Value;
end;

{ TBareRunoffFractionCollection }

class function TBareRunoffFractionCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCustomOneFarmItem;
end;

{ TNoReturnItem }

procedure TNoReturnItem.Assign(Source: TPersistent);
begin
  if Source is TNoReturnItem then
  begin
    NoReturnOption := TNoReturnItem(Source).NoReturnOption;
  end;
  inherited;
end;

procedure TNoReturnItem.AssignObserverEvents(Collection: TCollection);
begin
  inherited;

end;

function TNoReturnItem.BoundaryFormulaCount: integer;
begin
  result := 0;
end;

procedure TNoReturnItem.CreateFormulaObjects;
begin
  inherited;

end;

function TNoReturnItem.GetBoundaryFormula(Index: integer): string;
begin
  result := '';
end;

procedure TNoReturnItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  inherited;

end;

function TNoReturnItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TNoReturnItem)
    and (NoReturnOption = TNoReturnItem(AnotherItem).NoReturnOption);
end;

procedure TNoReturnItem.RemoveFormulaObjects;
begin
  inherited;

end;

procedure TNoReturnItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;

end;

procedure TNoReturnItem.SetNoReturnOption(const Value: TNoReturnOption);
begin
  if FNoReturnOption <> Value then
  begin
    FNoReturnOption := Value;
    InvalidateModel;
  end;
end;

{ TNoReturnCollection }

class function TNoReturnCollection.ItemClass: TBoundaryItemClass;
begin
  result := TNoReturnItem
end;

{ TMultiSrdItem }

procedure TMultiSrdItem.Assign(Source: TPersistent);
var
  OtherItem: TMultiSrdItem;
begin
  if Source is TMultiSrdItem then
  begin
    OtherItem := TMultiSrdItem(Source);
    SemiRouted := OtherItem.SemiRouted;
    Name := OtherItem.Name;
    InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

constructor TMultiSrdItem.Create(Collection: TCollection);
var
  SrdCollection: TMultiSrdCollection;
begin
  inherited;
  SrdCollection := Collection as TMultiSrdCollection;
  FSemiRouted := TSemiRoutedDeliveriesAndReturnFlowCollection.
    Create(SrdCollection.FModel as TCustomModel);
end;

destructor TMultiSrdItem.Destroy;
begin
  FSemiRouted.Free;
  inherited;
end;

function TMultiSrdItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TMultiSrdItem;
begin
  result := (AnotherItem is TMultiSrdItem);
  if result then
  begin
    OtherItem := TMultiSrdItem(AnotherItem);
    result := SemiRouted.IsSame(OtherItem.SemiRouted)
      and (Name = OtherItem.Name);
  end;
end;

procedure TMultiSrdItem.Loaded;
begin
  SemiRouted.Loaded;
end;

procedure TMultiSrdItem.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
end;

procedure TMultiSrdItem.SetSemiRouted(
  const Value: TSemiRoutedDeliveriesAndReturnFlowCollection);
begin
  FSemiRouted.Assign(Value);
end;

{ TMultiSrdCollection }

constructor TMultiSrdCollection.Create(Model: IModelForTOrderedCollection);
begin
  FModel := Model as TCustomModel;
  inherited Create(TMultiSrdItem, Model);
end;

function TMultiSrdCollection.GetItem(Index: Integer): TMultiSrdItem;
begin
  result := inherited Items[index] as TMultiSrdItem
end;

procedure TMultiSrdCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TMultiSrdCollection.SetItem(Index: Integer;
  const Value: TMultiSrdItem);
begin
  inherited Items[index] := Value;
end;

{ TSaltSupplyConcentrationItem }

function TSaltSupplyConcentrationItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

{ TSaltSupplyConcentrationCollection }

procedure TSaltSupplyConcentrationCollection.Assign(Source: TPersistent);
var
  OtherFarmCollection: TSaltSupplyConcentrationCollection;
begin
  if Source is TSaltSupplyConcentrationCollection then
  begin
    OtherFarmCollection := TSaltSupplyConcentrationCollection(Source);
    ExtConcPestSeriesParameter := OtherFarmCollection.ExtConcPestSeriesParameter;
    ExtConcPestParamMethod := OtherFarmCollection.ExtConcPestParamMethod;
    GWConcPestSeriesParameter := OtherFarmCollection.GWConcPestSeriesParameter;
    GWConcPestParamMethod := OtherFarmCollection.GWConcPestParamMethod;
    SWConcPestSeriesParameter := OtherFarmCollection.SWConcPestSeriesParameter;
    SWConcPestParamMethod := OtherFarmCollection.SWConcPestParamMethod;
  end;
  inherited;
end;

function TSaltSupplyConcentrationCollection.GetExtConcPestSeriesParameter: string;
begin
  if FExtConcPestSeriesParameter <> nil then
  begin
    FExtConcPestSeriesParameterName := FExtConcPestSeriesParameter.ParameterName;
  end;
  result := FExtConcPestSeriesParameterName;
end;

function TSaltSupplyConcentrationCollection.GetGWConcPestSeriesParameter: string;
begin
  if FGWConcPestSeriesParameter <> nil then
  begin
    FGWConcPestSeriesParameterName := FGWConcPestSeriesParameter.ParameterName;
  end;
  result := FGWConcPestSeriesParameterName;
end;

function TSaltSupplyConcentrationCollection.GetSWConcPestSeriesParameter: string;
begin
  if FSWConcPestSeriesParameter <> nil then
  begin
    FSWConcPestSeriesParameterName := FSWConcPestSeriesParameter.ParameterName;
  end;
  result := FSWConcPestSeriesParameterName;
end;

function TSaltSupplyConcentrationCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  OtherFarmCollection: TSaltSupplyConcentrationCollection;
begin
  result := (AnOrderedCollection is TSaltSupplyConcentrationCollection)
    and inherited;
  if result then
  begin
    OtherFarmCollection := TSaltSupplyConcentrationCollection(AnOrderedCollection);
    result := (ExtConcPestSeriesParameter = OtherFarmCollection.ExtConcPestSeriesParameter)
      and (ExtConcPestParamMethod = OtherFarmCollection.ExtConcPestParamMethod)
      and (GWConcPestSeriesParameter = OtherFarmCollection.GWConcPestSeriesParameter)
      and (GWConcPestParamMethod = OtherFarmCollection.GWConcPestParamMethod)
      and (SWConcPestSeriesParameter = OtherFarmCollection.SWConcPestSeriesParameter)
      and (SWConcPestParamMethod = OtherFarmCollection.SWConcPestParamMethod)
  end;
end;

class function TSaltSupplyConcentrationCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSaltSupplyConcentrationItem;
end;

procedure TSaltSupplyConcentrationCollection.Loaded;
begin
  if (Model <> nil) then
  begin
    if (FExtConcPestSeriesParameterName <> '') then
    begin
      FExtConcPestSeriesParameter := Model.GetPestParameterByNameI(FExtConcPestSeriesParameterName)
    end;
    if (FGWConcPestSeriesParameterName <> '') then
    begin
      FGWConcPestSeriesParameter := Model.GetPestParameterByNameI(FGWConcPestSeriesParameterName)
    end;
    if (FSWConcPestSeriesParameterName <> '') then
    begin
      FSWConcPestSeriesParameter := Model.GetPestParameterByNameI(FSWConcPestSeriesParameterName)
    end;
  end;
  inherited;
end;

procedure TSaltSupplyConcentrationCollection.SetExtConcPestParamMethod(
  const Value: TPestParamMethod);
begin
  FExtConcPestParamMethod := Value;
end;

procedure TSaltSupplyConcentrationCollection.SetExtConcPestSeriesParameter(
  const Value: string);
begin
  if FExtConcPestSeriesParameterName <> Value then
  begin
    FExtConcPestSeriesParameterName := Value;
    InvalidateModel;
  end;
  if (Model <> nil) then
  begin
    if Value <> '' then
    begin
      FExtConcPestSeriesParameter := Model.GetPestParameterByNameI(Value);
      if FExtConcPestSeriesParameter = nil then
      begin
        FExtConcPestSeriesParameterName := '';
        InvalidateModel;
      end;
    end
    else
    begin
      FExtConcPestSeriesParameter := nil;
    end;
  end;
end;

procedure TSaltSupplyConcentrationCollection.SetGWConcPestParamMethod(
  const Value: TPestParamMethod);
begin
  FGWConcPestParamMethod := Value;
end;

procedure TSaltSupplyConcentrationCollection.SetGWConcPestSeriesParameter(
  const Value: string);
begin
  if FGWConcPestSeriesParameterName <> Value then
  begin
    FGWConcPestSeriesParameterName := Value;
    InvalidateModel;
  end;
  if (Model <> nil) then
  begin
    if Value <> '' then
    begin
      FGWConcPestSeriesParameter := Model.GetPestParameterByNameI(Value);
      if FGWConcPestSeriesParameter = nil then
      begin
        FGWConcPestSeriesParameterName := '';
        InvalidateModel;
      end;
    end
    else
    begin
      FGWConcPestSeriesParameter := nil;
    end;
  end;
end;

procedure TSaltSupplyConcentrationCollection.SetSWConcPestParamMethod(
  const Value: TPestParamMethod);
begin
  FSWConcPestParamMethod := Value;
end;

procedure TSaltSupplyConcentrationCollection.SetSWConcPestSeriesParameter(
  const Value: string);
begin
  if FSWConcPestSeriesParameterName <> Value then
  begin
    FSWConcPestSeriesParameterName := Value;
    InvalidateModel;
  end;
  if (Model <> nil) then
  begin
    if Value <> '' then
    begin
      FSWConcPestSeriesParameter := Model.GetPestParameterByNameI(Value);
      if FSWConcPestSeriesParameter = nil then
      begin
        FSWConcPestSeriesParameterName := '';
        InvalidateModel;
      end;
    end
    else
    begin
      FSWConcPestSeriesParameter := nil;
    end;
  end;
end;

end.
