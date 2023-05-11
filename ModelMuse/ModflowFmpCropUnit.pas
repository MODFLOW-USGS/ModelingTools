unit ModflowFmpCropUnit;

interface

uses
  ModflowBoundaryUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  Classes,
  OrderedCollectionUnit, SysUtils, GoPhastTypes, ModflowFmpFarmUnit,
  ModflowFmpBaseClasses, RealListUnit, OrderedCollectionInterfaceUnit;

type
  // FMP Data Sets 11 and 26
  // LAND_USE ROOT_DEPTH
  TRootingDepthItem = class(TOwhmItem)
  private
    const
    RootingDepthPosition = 0;
  published
    property RootingDepth: string index RootingDepthPosition
      read GetBoundaryFormula write SetBoundaryFormula;
  end;

  TFmpRootDepthCollection = class(TOwhmCollection)
  private
    function GetItems(Index: Integer): TRootingDepthItem;
    procedure SetItems(Index: Integer; const Value: TRootingDepthItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TRootingDepthItem read GetItems
      write SetItems; default;
  end;

  TCropIrrigationItem = class(TCustomZeroFarmItem)
  private
    const
    IrrigationPosition = 0;
    EvapIrrigateFractionPosition = 1;
    SurfWaterLossFracIrrPosition = 2;
    function GetIrrigation: string;
    procedure SetIrrigation(const Value: string);
    function GetEvapIrrigateFraction: string;
    procedure SetEvapIrrigateFraction(const Value: string);
    function GetSurfaceWaterLossFractionIrrigation: string;
    procedure SetSurfaceWaterLossFractionIrrigation(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property Irrigation: string read GetIrrigation write SetIrrigation;
    property EvapIrrigateFraction: string read GetEvapIrrigateFraction
      write SetEvapIrrigateFraction;
    property SurfaceWaterLossFractionIrrigation: string
      read GetSurfaceWaterLossFractionIrrigation
      write SetSurfaceWaterLossFractionIrrigation;
  end;

  // @name represents the choice of irrigation type for one crop.
  TIrrigationCollection = class(TCustomFarmCollection)
  private
    function GetItems(Index: Integer): TCropIrrigationItem;
    procedure SetItems(Index: Integer; const Value: TCropIrrigationItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TCropIrrigationItem read GetItems
      write SetItems; default;
  end;

  // FMP Data Sets 12 and 28
  TEvapFractionsRecord = record
    StartingTime: double;
    EndingTime: Double;
    // FTR: Transpiratory fraction of consumptive use
    TranspirationFraction: double;
    TranspirationFractionAnnotation: string;
    // FEP: Evaporative fraction of consumptive use related to precipitation
    PrecipFraction: double;
    PrecipFractionAnnotation: string;
    // FEI: Evaporative fraction of consumptive use related to irrigation
    IrrigFraction: double;
    IrrigFractionAnnotation: string;
  end;

  TEvapFractionsItem = class(TCustomZeroFarmItem)
  private
    const
    TranspirationFractionPosition = 0;
    PrecipFractionPosition = 1;
    IrrigFractionPosition = 2;
    function GetTranspirationFraction: string;
    procedure SetTranspirationFraction(const Value: string);
    function GetIrrigFraction: string;
    function GetPrecipFraction: string;
    procedure SetIrrigFraction(const Value: string);
    procedure SetPrecipFraction(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    // FTR
    property TranspirationFraction: string read GetTranspirationFraction
      write SetTranspirationFraction;
    // FEP
    property PrecipFraction: string read GetPrecipFraction
      write SetPrecipFraction;
    // FEI
    property IrrigFraction: string read GetIrrigFraction write SetIrrigFraction;
  end;

  TEvapFractionsCollection = class(TCustomFarmCollection)
  private
    FTimeValues: array of TEvapFractionsRecord;
    function GetEvapFractionTimeValues(Index: integer): TEvapFractionsRecord;
    procedure SetEvapFractionTimeValues(Index: integer;
      const Value: TEvapFractionsRecord);
    function GetItems(Index: Integer): TEvapFractionsItem;
    procedure SetItems(Index: Integer; const Value: TEvapFractionsItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property EvapFractionTimeValues[Index: integer]: TEvapFractionsRecord
      read GetEvapFractionTimeValues write SetEvapFractionTimeValues;
    function GetEvapFractionTimeValuesFromTime(StartTime: double): TEvapFractionsRecord;
    property Items[Index: Integer]: TEvapFractionsItem read GetItems
      write SetItems; default;
  end;

  // FMP Data Sets 13 and 29
  TLossesRecord = record
    StartingTime: double;
    EndingTime: Double;
    // FIESWP: Fraction of in-efficient losses to surface-water related to precipitation
    PrecipitationLosses: double;
    PrecipitationLossesAnnotation: string;
    // FIESWI: Fraction of in-efficient losses to surface-water related to irrigation
    IrrigationLosses: double;
    IrrigationLossesAnnotation: string;
  end;

  TLossesItem = class(TCustomZeroFarmItem)
  private
    const
    PrecipitationLossesPosition = 0;
    IrrigationLossesPosition = 1;
    function GetPrecipitationLosses: string;
    procedure SetPrecipitationLosses(const Value: string);
    function GetIrrigationLosses: string;
    procedure SetIrrigationLosses(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property PrecipitationLosses: string read GetPrecipitationLosses
      write SetPrecipitationLosses;
    property IrrigationLosses: string read GetIrrigationLosses
      write SetIrrigationLosses;
  end;

  TLossesCollection = class(TCustomFarmCollection)
  private
    FTimeValues: array of TLossesRecord;
    function GetLossesTimeValues(Index: integer): TLossesRecord;
    procedure SetLossesTimeValues(Index: integer;
      const Value: TLossesRecord);
    function GetItems(Index: Integer): TLossesItem;
    procedure SetItems(Index: Integer; const Value: TLossesItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property LossesTimeValues[Index: integer]: TLossesRecord
      read GetLossesTimeValues write SetLossesTimeValues;
    function GetLossesTimeValuesFromTime(StartTime: double): TLossesRecord;
    property Items[Index: Integer]: TLossesItem read GetItems
      write SetItems; default;
  end;

  // FMP Data Sets 18 and 31
  TCropFunctionRecord = record
    StartingTime: double;
    EndingTime: Double;
    // WPF-Slope: Slope of crop-specific water-production function
    Slope: double;
    SlopeAnnotation: string;
    // WPF-Int: Intercept of crop-specific water-production function
    Intercept: double;
    InterceptAnnotation: string;
    // Crop-Price: Market-price per crop
    Price: double;
    PriceAnnotation: string;
  end;

  TCropFunctionItem = class(TCustomZeroFarmItem)
  private
    const
    SlopePosition = 0;
    InterceptPosition = 1;
    PricePosition = 2;
    function GetSlope: string;
    procedure SetSlope(const Value: string);
    function GetIntercept: string;
    function GetPrice: string;
    procedure SetIntercept(const Value: string);
    procedure SetPrice(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
  published
    property Slope: string read GetSlope write SetSlope;
    property Intercept: string read GetIntercept write SetIntercept;
    property Price: string read GetPrice write SetPrice;
  end;

  TCropFunctionCollection = class(TCustomFarmCollection)
  private
    FTimeValues: array of TCropFunctionRecord;
    function GetCropFunctionTimeValues(Index: integer): TCropFunctionRecord;
    procedure SetCropFunctionTimeValues(Index: integer;
      const Value: TCropFunctionRecord);
    function GetItems(Index: Integer): TCropFunctionItem;
    procedure SetItems(Index: Integer; const Value: TCropFunctionItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property CropFunctionTimeValues[Index: integer]: TCropFunctionRecord
      read GetCropFunctionTimeValues write SetCropFunctionTimeValues;
    function GetCropFunctionTimeValuesFromTime(StartTime: double): TCropFunctionRecord;
    property Items[Index: Integer]: TCropFunctionItem read GetItems
      write SetItems; default;
  end;

  // FMP Data Set 27a
  TCropWaterUseRecord = record
    StartingTime: double;
    EndingTime: Double;
    // CU: Crop consumptive use flux if ICUFL = 1, 2; crop coefficient if ICUFL = –1
    CropValue: double;
    CropValueAnnotation: string;
    // inverse of NONIRR: Non-irrigation flag
    Irrigated: boolean;
    IrrigatedAnnotation: string;
  end;

  TCropWaterUseItem = class(TCustomFarmItem)
  private
    const
    CropValuePosition = 0;
    WaterUseIrrigatedPosition = 1;
    function GetCropValue: string;
    procedure SetCropValue(const Value: string);
    function GetIrrigated: string;
    procedure SetIrrigated(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // CU Crop consumptive use flux, data set 30a
    property CropValue: string read GetCropValue write SetCropValue;
    // NONIRR, data set 30a
    property Irrigated: string read GetIrrigated write SetIrrigated;
  end;

  TCropWaterUseCollection = class(TCustomFarmCollection)
  private
    FTimeValues: array of TCropWaterUseRecord;
    function GetCropCostTimeValues(Index: integer): TCropWaterUseRecord;
    procedure SetCropCostTimeValues(Index: integer;
      const Value: TCropWaterUseRecord);
    function GetItems(Index: Integer): TCropWaterUseItem;
    procedure SetItems(Index: Integer; const Value: TCropWaterUseItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property CropFunctionTimeValues[Index: integer]: TCropWaterUseRecord
      read GetCropCostTimeValues write SetCropCostTimeValues;
    function GetCropFunctionTimeValuesFromTime(StartTime: double): TCropWaterUseRecord;
    property Items[Index: Integer]: TCropWaterUseItem read GetItems
      write SetItems; default;
  end;

  TCropRecord = record
    CropID: integer;
    BaseTemperature: double;
    BeginningRootDepth: double;
    Coefficient0: double;
    Coefficient1: double;
    Coefficient2: double;
    Coefficient3: double;
    Irrigated: boolean;
    MaximumCutoffTemperature: double;
    MaximumRootDepth: double;
    MinimumCutoffTemperature: double;
    PSI1: double;
    PSI2: double;
    PSI3: double;
    PSI4: double;
    RootGrowthCoefficient: double;
    FAllow: boolean;
  end;

  TCropArray = array of TCropRecord;

  TRootPressureItem = class(TCustomFarmItem)
  private
    const
    PSI1Position = 0;
    PSI2Position = 1;
    PSI3Position = 2;
    PSI4Position = 3;
  protected
    function BoundaryFormulaCount: integer; override;
  published
    property PSI1: string index PSI1Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property PSI2: string index PSI2Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property PSI3: string index PSI3Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property PSI4: string index PSI4Position read GetBoundaryFormula
      write SetBoundaryFormula;
  end;

  TRootPressureCollection = class(TCustomFarmCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TInteractionCode = 0..5;

  TGroundwaterRootInteraction = class(TGoPhastPersistent)
  private
    FInteractionCode: TInteractionCode;
    function GetAnoxia: Boolean;
    function GetGroundwaterUptake: Boolean;
    function GetSoilStress: Boolean;
    procedure SetAnoxia(const Value: Boolean);
    procedure SetGroundwaterUptake(const Value: Boolean);
    procedure SetInteractionCode(const Value: TInteractionCode);
    procedure SetSoilStress(const Value: Boolean);
    function GetHasTranspiration: Boolean;
    procedure SetHasTranspiration(const Value: Boolean);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    property GroundwaterUptake: Boolean read GetGroundwaterUptake
      write SetGroundwaterUptake;
    property Anoxia: Boolean read GetAnoxia write SetAnoxia;
    property SoilStress: Boolean read GetSoilStress write SetSoilStress;
    property HasTranspiration: Boolean read GetHasTranspiration
      write SetHasTranspiration;
    function IsSame(OtherItem: TGroundwaterRootInteraction): Boolean;
    class function ConvertToInteractionCode(HasTranspriationB, GWUptakeB, AnoxiaB,
      SoilStressB: Boolean): TInteractionCode;
    class procedure ConvertFromInteractionCode(Code: TInteractionCode; var
      HasTranspirationB, HasGroundwater, HasAnoxia, HasStress: Boolean);
  published
    property InteractionCode: TInteractionCode read FInteractionCode
      write SetInteractionCode;
  end;

  TAddedDemandFarmItem = class(TCustomStringValueItem)
  private
    FFarmGuid: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FarmGuid: string read FFarmGuid write FFarmGuid;
  end;

  TAddedDemandFarmCollection = class(TCustomStringCollection)
  private
    function GetItem(Index: Integer): TAddedDemandFarmItem;
    procedure SetItem(Index: Integer; const Value: TAddedDemandFarmItem);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TAddedDemandFarmItem read GetItem
      write SetItem; default;
    function Add: TAddedDemandFarmItem;
  end;

  TAddedDemandItem = class(TNoFormulaItem)
  private
    FAddedDemandValues: TAddedDemandFarmCollection;
    procedure SetAddedDemandValues(const Value: TAddedDemandFarmCollection);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Assign(Source: TPersistent); override;
    function GetItemByFarmGUID(FarmGUID: string): TAddedDemandFarmItem;
  published
    property AddedDemandValues: TAddedDemandFarmCollection
      read FAddedDemandValues write SetAddedDemandValues;
  end;

  TAddedDemandCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    constructor Create(Model: IModelForTOrderedCollection); reintroduce;
  end;

  TLeachChoice = (lcValue, lcRhoades, lcNone, lcCustomFormula);

  TLeachItem = class(TOwhmItem)
  private
    FLeachChoice: TLeachChoice;
    FCustomFormula: string;
    procedure SetLeachChoice(const Value: TLeachChoice);
    procedure SetCustomFormula(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property LeachChoice: TLeachChoice read FLeachChoice write SetLeachChoice;
    property CustomFormula: string read FCustomFormula write SetCustomFormula;
  end;

  TLeachCollection = class(TOwhmCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;

  // define crops and FMP Data sets 14 and 15.
  TCropItem = class(TCustomFarmItem)
  private
    var
    FCropName: string;
    FCropFunctionCollection: TCropFunctionCollection;
    FFmpRootDepthCollection: TFmpRootDepthCollection;
    FEvapFractionsCollection: TEvapFractionsCollection;
    FLossesCollection: TLossesCollection;
    FCropWaterUseCollection: TCropWaterUseCollection;
    FLandUseAreaFractionDataArrayName: string;
    FCropCoefficientDisplayName: string;
    FCropCoefficientDataArrayName: string;
    FRootDepthDataArrayName: string;
    FIrrigationDataArrayName: string;
    FConsumptiveUseDataArrayName: string;
    FSWLossFractionIrrigationDataArrayName: string;
    FAddedDemandDataArrayName: string;
    FEvaporationIrrigationDataArrayName: string;
    FSWLossFractionPrecipDataArrayName: string;
    FAddedDemandDisplayName: string;
    FEvaporationIrrigationDisplayName: string;
    FIrrigationDisplayName: string;
    FRootDepthDisplayName: string;
    FSWLossFractionPrecipDisplayName: string;
    FSWLossFractionIrrigationDisplayName: string;
    FConsumptiveUseDisplayName: string;
    FLandUseAreaFractionDisplayName: string;
    FGroundwaterRootInteractionDataArrayName: string;
    FGroundwaterRootInteractionDisplayName: string;
    FTranspirationFractionDataArrayName: string;
    FTranspirationFractionDisplayName: string;
    FCropHasSalinityDemandDataArrayName: string;
    FCropHasSalinityDemandDisplayName: string;
    FIrrigationCollection: TIrrigationCollection;
    FLandUseFractionCollection: TOwhmCollection;
    FCropCoefficientCollection: TOwhmCollection;
    FConsumptiveUseCollection: TOwhmCollection;
    FRootPressureCollection: TRootPressureCollection;
    FGroundwaterRootInteraction: TGroundwaterRootInteraction;
    FTranspirationFractionCollection: TOwhmCollection;
    FSWLossFractionPrecipCollection: TOwhmCollection;
    FPondDepthCollection: TOwhmCollection;
    FAddedDemandCollection: TAddedDemandCollection;
    FConvertToBareSoilCollection: TBoolFarmCollection;
    FUseEvapFractionCorrectionCollection: TBoolFarmCollection;
    FSalinityToleranceCollection: TOwhmCollection;
    FMaxLeachingRequirementCollection: TOwhmCollection;
    FLeachingRequirementCollection: TLeachCollection;
    FSalinityAppliedWater: TLeachCollection;
    procedure SetRootPressureCollection(const Value: TRootPressureCollection);
    procedure SetGroundwaterRootInteraction(
      const Value: TGroundwaterRootInteraction);
    procedure SetTranspirationFractionCollection(const Value: TOwhmCollection);
    procedure SetSWLossFractionPrecipCollection(const Value: TOwhmCollection);
    procedure SetPondDepthCollection(const Value: TOwhmCollection);
    procedure SetAddedDemandCollection(const Value: TAddedDemandCollection);
    procedure SetConvertToBareSoilCollection(const Value: TBoolFarmCollection);
    procedure SetUseEvapFractionCorrectionCollection(const Value: TBoolFarmCollection);
    procedure SetSalinityToleranceCollection(const Value: TOwhmCollection);
    procedure SetMaxLeachingRequirementCollection(const Value: TOwhmCollection);
    procedure SetLeachingRequirementCollection(const Value: TLeachCollection);
    procedure SetSalinityAppliedWater(const Value: TLeachCollection);
    const
    PSI1Position = 0;
    PSI2Position = 1;
    PSI3Position = 2;
    PSI4Position = 3;
    BaseTemperaturePosition = 4;
    MinimumCutoffTemperaturePosition = 5;
    MaximumCutoffTemperaturePosition = 6;
    Coefficient0Position = 7;
    Coefficient1Position = 8;
    Coefficient2Position = 9;
    Coefficient3Position = 10;
    BeginningRootDepthPosition = 11;
    MaximumRootDepthPosition = 12;
    RootGrowthCoefficientPosition = 13;
    IrrigatedPosition = 14;
    FAllowPosition = 15;
    procedure SetConsumptiveUseCollection(const Value: TOwhmCollection);
    procedure SetCropCoefficientCollection(const Value: TOwhmCollection);
    procedure SetLandUseFractionCollection(const Value: TOwhmCollection);
    procedure SetLandUseAreaFractionDataArrayName(const NewName: string);
    procedure SetCropCoefficientDataArrayName(const NewName: string);
    procedure SetAddedDemandDataArrayName(const NewName: string);
    procedure SetConsumptiveUseDataArrayName(const NewName: string);
    procedure SetEvaporationIrrigationDataArrayName(const NewName: string);
    procedure SetIrrigationDataArrayName(const NewName: string);
    procedure SetRootDepthDataArrayName(const NewName: string);
    procedure SetSWLossFractionIrrigationDataArrayName(const NewName: string);
    procedure SetSWLossFractionPrecipDataArrayName(const NewName: string);
    procedure SetGroundwaterRootInteractionDataArrayName(const NewName: string);
    procedure SetTranspirationFractionDataArrayName(const NewName: string);
    procedure SetCropHasSalinityDemandDataArrayName(const NewName: string);
    procedure SetCropName(Value: string);
    procedure SetCropFunctionCollection(const Value: TCropFunctionCollection);
    procedure SetCropWaterUseCollection(const Value: TCropWaterUseCollection);
    procedure SetEvapFractionsCollection(const Value: TEvapFractionsCollection);
    procedure SetFmpRootDepthCollection(const Value: TFmpRootDepthCollection);
    procedure SetLossesCollection(const Value: TLossesCollection);
    procedure UpdateAllDataArrays;
    procedure SetIrrigationCollection(const Value: TIrrigationCollection);
    procedure UpdateFarmProperties;
  protected
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
    procedure InitializeFormulas;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetIndex(Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean);
  published
    property CropName: string read FCropName write SetCropName;
    property PSI1: string index PSI1Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property PSI2: string index PSI2Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property PSI3: string index PSI3Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property PSI4: string index PSI4Position read GetBoundaryFormula
      write SetBoundaryFormula;
    // BaseT
    property BaseTemperature: string index BaseTemperaturePosition
      read GetBoundaryFormula write SetBoundaryFormula;
    // MinCutT
    property MinimumCutoffTemperature: string
      index MinimumCutoffTemperaturePosition read GetBoundaryFormula
      write SetBoundaryFormula;
    // MaxCutT
    property MaximumCutoffTemperature: string
      index MaximumCutoffTemperaturePosition read GetBoundaryFormula
      write SetBoundaryFormula;
    // C0
    property Coefficient0: string index Coefficient0Position
      read GetBoundaryFormula write SetBoundaryFormula;
    // C1
    property Coefficient1: string index Coefficient1Position
      read GetBoundaryFormula write SetBoundaryFormula;
    // C2
    property Coefficient2: string index Coefficient2Position
      read GetBoundaryFormula write SetBoundaryFormula;
    // C3
    property Coefficient3: string index Coefficient3Position
      read GetBoundaryFormula write SetBoundaryFormula;
    // BegRootD
    property BeginningRootDepth: string index BeginningRootDepthPosition
      read GetBoundaryFormula write SetBoundaryFormula;
    // MaxRootD
    property MaximumRootDepth: string index MaximumRootDepthPosition
      read GetBoundaryFormula write SetBoundaryFormula;
    // RootGC
    property RootGrowthCoefficient: string index RootGrowthCoefficientPosition
      read GetBoundaryFormula write SetBoundaryFormula;
    // inverse of NONIRR
    property Irrigated: string index IrrigatedPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    // IFALLOW
    property Fallow: string index FAllowPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    // // FMP Data Sets 11 and 26
    // LAND_USE ROOT_DEPTH
    property FmpRootDepthCollection: TFmpRootDepthCollection
      read FFmpRootDepthCollection write SetFmpRootDepthCollection;
    // FMP Data Sets 12 and 28
    property EvapFractionsCollection: TEvapFractionsCollection
      read FEvapFractionsCollection write SetEvapFractionsCollection;
    // FMP Data Sets 13 and 29
    property LossesCollection: TLossesCollection
      read FLossesCollection write SetLossesCollection;
    // FMP Data Sets 18 and 31
    property CropFunctionCollection: TCropFunctionCollection
      read FCropFunctionCollection write SetCropFunctionCollection;
    // FMP Data Set 27a
    property CropWaterUseCollection: TCropWaterUseCollection
      read FCropWaterUseCollection write SetCropWaterUseCollection;
  // FMP4
    property LandUseAreaFractionDataArrayName: string
      read FLandUseAreaFractionDataArrayName write SetLandUseAreaFractionDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property CropCoefficientDataArrayName: string
      read FCropCoefficientDataArrayName write SetCropCoefficientDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property ConsumptiveUseDataArrayName: string
      read FConsumptiveUseDataArrayName write SetConsumptiveUseDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property IrrigationDataArrayName: string
      read FIrrigationDataArrayName write SetIrrigationDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property RootDepthDataArrayName: string
      read FRootDepthDataArrayName write SetRootDepthDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property GroundwaterRootInteractionDataArrayName: string
      read FGroundwaterRootInteractionDataArrayName write SetGroundwaterRootInteractionDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property TranspirationFractionDataArrayName: string
      read FTranspirationFractionDataArrayName write SetTranspirationFractionDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property EvaporationIrrigationDataArrayName: string
      read FEvaporationIrrigationDataArrayName write SetEvaporationIrrigationDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property SWLossFractionPrecipDataArrayName: string
      read FSWLossFractionPrecipDataArrayName write SetSWLossFractionPrecipDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property SWLossFractionIrrigationDataArrayName: string
      read FSWLossFractionIrrigationDataArrayName
      write SetSWLossFractionIrrigationDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property AddedDemandDataArrayName: string
      read FAddedDemandDataArrayName write SetAddedDemandDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property CropHasSalinityDemandDataArrayName: string
      read FCropHasSalinityDemandDataArrayName
      write SetCropHasSalinityDemandDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    // EVAPORATION_IRRIGATION_FRACTION and SURFACEWATER_LOSS_FRACTION_IRRIGATION
    property IrrigationCollection: TIrrigationCollection
      read FIrrigationCollection write SetIrrigationCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    { TODO : SW_LossFractionIrrigationCollection can be deleted upon publication }
    property SW_LossFractionIrrigationCollection: TIrrigationCollection
      read FIrrigationCollection write SetIrrigationCollection
      stored False;

    property LandUseFractionCollection: TOwhmCollection
      read FLandUseFractionCollection write SetLandUseFractionCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property CropCoefficientCollection: TOwhmCollection
      read FCropCoefficientCollection write SetCropCoefficientCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property ConsumptiveUseCollection: TOwhmCollection
      read FConsumptiveUseCollection write SetConsumptiveUseCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property RootPressureCollection: TRootPressureCollection
      read FRootPressureCollection write SetRootPressureCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property GroundwaterRootInteraction: TGroundwaterRootInteraction
      read FGroundwaterRootInteraction write SetGroundwaterRootInteraction
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property TranspirationFractionCollection: TOwhmCollection
      read FTranspirationFractionCollection
      write SetTranspirationFractionCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property SWLossFractionPrecipCollection: TOwhmCollection
      read FSWLossFractionPrecipCollection
      write SetSWLossFractionPrecipCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property PondDepthCollection: TOwhmCollection read FPondDepthCollection
      write SetPondDepthCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property AddedDemandCollection: TAddedDemandCollection
      read FAddedDemandCollection write SetAddedDemandCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    // ZERO_CONSUMPTIVE_USE_BECOMES_BARE_SOIL
    property ConvertToBareSoilCollection: TBoolFarmCollection
      read FConvertToBareSoilCollection write SetConvertToBareSoilCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    //EVAPORATION_IRRIGATION_FRACTION_SUM_ONE_CORRECTION
    property UseEvapFractionCorrectionCollection: TBoolFarmCollection
      read FUseEvapFractionCorrectionCollection
      write SetUseEvapFractionCorrectionCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    // CROP_SALINITY_TOLERANCE
    property SalinityToleranceCollection: TOwhmCollection
      read FSalinityToleranceCollection write SetSalinityToleranceCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    // CROP_MAX_LEACHING_REQUIREMENT
    property MaxLeachingRequirementCollection: TOwhmCollection
      read FMaxLeachingRequirementCollection
      write SetMaxLeachingRequirementCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    // CROP_LEACHING_REQUIREMENT
    property LeachingRequirementCollection: TLeachCollection
      read FLeachingRequirementCollection
      write SetLeachingRequirementCollection
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    // CROP_SALINITY_APPLIED_WATER
    property SalinityAppliedWater: TLeachCollection read FSalinityAppliedWater
      write SetSalinityAppliedWater
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
  end;

  TCropCollection = class(TCustomFarmCollection)
  private
    FCropArray: TCropArray;
    FFarmList: TFarmList;
    function GetItems(Index: Integer): TCropItem;
    procedure SetItems(Index: Integer; const Value: TCropItem);
    function GetFarmList: TFarmList;
  protected
    class function ItemClass: TBoundaryItemClass; override;
    property FarmList: TFarmList read GetFarmList;
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    property CropArray: TCropArray read FCropArray;
    property Items[Index: Integer]: TCropItem read GetItems write SetItems; default;
    procedure EvaluateCrops;
    procedure UpdateAllDataArrays;
    procedure UpdateFarmProperties;
  end;

implementation

uses
  frmGoPhastUnit, frmErrorsAndWarningsUnit,
  RbwParser, frmFormulaErrorsUnit, PhastModelUnit, ModflowPackageSelectionUnit,
  GlobalVariablesUnit, LockedGlobalVariableChangers, UpdateDataArrayUnit;

const
  KLandUseFractionPrefix  = 'Land_Use_Area_Fraction_';
  KCropCoefficientPrefix = 'Crop_Coefficient_';
  KConsumptiveUsePrefix = 'Consumptive_Use_';
  KIrrigationPrefix = 'Irrigation_';
  KRootDepthPrefix = 'Root_Depth_';
  KGroundwaterRootInteractionPrefix = 'Groundwater_Root_Interaction_';
  KTranspirationFractionPrefix = 'Transpiration_Fraction_';
  KEvaporationIrrigationPrefix = 'Evaporation_Irrigation_Fraction_';
  KSWLossFractionPrecipPrefix = 'Frac_Unconsumed_Precip_to_SW_';
  KSWLossFractionIrrigationPrefix = 'Frac_Unconsumed_Irrigation_to_SW_';
  KAddedDemandPrefix = 'Added_Demand_';
  KCropHasSalinityDemandPrefix = 'Crop_Has_Salinity_Demand_';

resourcestring
  StrLandUseFractionPrefix  = KLandUseFractionPrefix;
  StrCropCoefficientPrefix = KCropCoefficientPrefix;
  StrConsumptiveUsePrefix = KConsumptiveUsePrefix;
  StrIrrigationPrefix = KIrrigationPrefix;
  StrRootDepthPrefix = KRootDepthPrefix;
  StrGroundwaterRootInteractionPrefix = KGroundwaterRootInteractionPrefix;
  StrTranspirationFractionPrefix = KTranspirationFractionPrefix;
  StrEvaporationIrrigationPrefix = KEvaporationIrrigationPrefix;
  StrSWLossFractionIrrigationPrefix = KSWLossFractionIrrigationPrefix;
  StrSWLossFractionPrecipPrefix = KSWLossFractionPrecipPrefix;
  StrAddedDemandPrefix = KAddedDemandPrefix;
  StrCropHasSalinityDemandPrefix = KCropHasSalinityDemandPrefix;
  StrIncompleteIrrigatio = 'Incomplete Irrigation data';
  StrLandUseAreaFracti = 'Land Use Area Fraction';
  StrCropCoefficient = 'Crop Coefficient';
  StrConsumptiveUse = 'Consumptive Use';
  StrIrrigation = 'Irrigation';
  StrRootingDepth = 'Rooting Depth';
  StrTranspirationFracti = 'Transpiration Fraction';
  StrSurfaceWaterLossF = 'Surface Water Loss Fraction Precipitation';
  StrPondDepth = 'Pond Depth';
  StrAddedDemand = 'Added Demand';
  StrZeroConsumptiveUse = 'Zero Consumptive Use Becomes Bare Soil';
  StrEvaporationIrrigatiCorrection = 'Evaporation Irrigation Fraction Sum On' +
  'e Correction';
  StrSalinityTolerance = 'Salinity Tolerance (dS/m)';
  StrMaximumLeachingReq = 'Maximum Leaching Requirement';
  StrLeachingChoice = 'Leaching Choice';
  StrFormula = 'Formula';

resourcestring
  IDError = 'Time: %g.';
  StrAssignedUsingS = 'Assigned using %s';
  StrRootingDepthInThe = 'Rooting Depth in the Farm Process';
  StrIncompleteRootingD = 'Incomplete Rooting Depth data';
  StrIrrigationInThe = 'Irrigation in the Farm Process';

  StrIncompleteEvaporativeFractions = 'Incomplete Evaporative Fractions data';
  StrTranspirationFraction = 'Transpiration Fraction in the Farm Process';
  StrPrecipitationFraction = 'Precipitation Fraction in the Farm Process';
  StrIrrigationFraction = 'Irrigation Fraction in the Farm Process';

  StrIncompleteLosses = 'Incomplete Losses data';
  StrPrecipitationLosses = 'Precipitation Losses in the Farm Process';
  StrIrrigationLosses = 'Irrigation Losses in the Farm Process';

  StrIncompleteCropFunction = 'Incomplete Crop Function data';
  StrCropFunctionSlope = 'Crop Function Slope in the Farm Process';
  StrCropFunctionCropValue = 'Crop Value in the Farm Process';

  StrCropFunctionIntercept = 'Crop Function Intercept in the Farm Process';
  StrCropFunctionPrice = 'Crop Function Price in the Farm Process';
  StrCropFunctionIrrigation = 'Crop Irrigation in the Farm Process';

  StrIncompleteCropWaterUse = 'Incomplete Crop Water Use data';
  StrCropVariable = 'Crop Variable';


{ TFmpRootDepthCollection }

function TFmpRootDepthCollection.GetItems(Index: Integer): TRootingDepthItem;
begin
  result := inherited Items[Index] as TRootingDepthItem;
end;

class function TFmpRootDepthCollection.ItemClass: TBoundaryItemClass;
begin
  result := TRootingDepthItem;
end;

procedure TFmpRootDepthCollection.SetItems(Index: Integer;
  const Value: TRootingDepthItem);
begin
  inherited Items[Index] := Value;
end;

{ TEvapFractionsItem }

function TEvapFractionsItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

function TEvapFractionsItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := TranspirationFraction;
    1:
      result := PrecipFraction;
    2:
      result := IrrigFraction;
    else Assert(False);
  end;
end;

function TEvapFractionsItem.GetIrrigFraction: string;
begin
  Result := FFormulaObjects[IrrigFractionPosition].Formula;
  ResetItemObserver(IrrigFractionPosition);
end;

function TEvapFractionsItem.GetPrecipFraction: string;
begin
  Result := FFormulaObjects[PrecipFractionPosition].Formula;
  ResetItemObserver(PrecipFractionPosition);
end;

function TEvapFractionsItem.GetTranspirationFraction: string;
begin
  Result := FFormulaObjects[TranspirationFractionPosition].Formula;
  ResetItemObserver(TranspirationFractionPosition);
end;

procedure TEvapFractionsItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    0:
      TranspirationFraction := Value;
    1:
      PrecipFraction := Value;
    2:
      IrrigFraction := Value;
    else Assert(False);
  end;
end;

procedure TEvapFractionsItem.SetIrrigFraction(const Value: string);
begin
  if FFormulaObjects[IrrigFractionPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, IrrigFractionPosition, FFormulaObjects[IrrigFractionPosition]);
  end;
end;

procedure TEvapFractionsItem.SetPrecipFraction(const Value: string);
begin
  if FFormulaObjects[PrecipFractionPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PrecipFractionPosition, FFormulaObjects[PrecipFractionPosition]);
  end;
end;

procedure TEvapFractionsItem.SetTranspirationFraction(const Value: string);
begin
  if FFormulaObjects[TranspirationFractionPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, TranspirationFractionPosition, FFormulaObjects[TranspirationFractionPosition]);
  end;
end;

{ TEvapFractionsCollection }

procedure TEvapFractionsCollection.EvaluateBoundaries;
var
  CurrentRecord: TEvapFractionsRecord;
  CurrentItem: TEvapFractionsItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
begin
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TEvapFractionsItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

    Expression := nil;
    Formula := CurrentItem.TranspirationFraction;
    CurrentRecord.TranspirationFractionAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrTranspirationFraction,
          Formula, E.Message);

        CurrentItem.TranspirationFraction := '0.';
        Formula := CurrentItem.TranspirationFraction;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.TranspirationFraction := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.PrecipFraction;
    CurrentRecord.PrecipFractionAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrPrecipitationFraction,
          Formula, E.Message);

        CurrentItem.PrecipFraction := '0.';
        Formula := CurrentItem.PrecipFraction;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.PrecipFraction := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.IrrigFraction;
    CurrentRecord.IrrigFractionAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrIrrigationFraction,
          Formula, E.Message);

        CurrentItem.IrrigFraction := '0.';
        Formula := CurrentItem.IrrigFraction;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.IrrigFraction := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TEvapFractionsCollection.GetEvapFractionTimeValues(
  Index: integer): TEvapFractionsRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TEvapFractionsCollection.GetEvapFractionTimeValuesFromTime(
  StartTime: double): TEvapFractionsRecord;
var
  Index: integer;
  ErrorMessage: string;
begin
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if (FTimeValues[Index].StartingTime <= StartTime) then
    begin
      result := FTimeValues[Index];
      if (FTimeValues[Index].EndingTime > StartTime) then
      begin
        Exit;
      end;
    end;
  end;
  ErrorMessage := Format(IDError, [StartTime]);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteEvaporativeFractions, ErrorMessage);
end;

function TEvapFractionsCollection.GetItems(Index: Integer): TEvapFractionsItem;
begin
  Result := inherited Items[Index] as TEvapFractionsItem;
end;

class function TEvapFractionsCollection.ItemClass: TBoundaryItemClass;
begin
  result := TEvapFractionsItem;
end;

procedure TEvapFractionsCollection.SetEvapFractionTimeValues(Index: integer;
  const Value: TEvapFractionsRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

procedure TEvapFractionsCollection.SetItems(Index: Integer;
  const Value: TEvapFractionsItem);
begin
  inherited Items[Index] := Value;
end;

{ TLossesItem }

function TLossesItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

function TLossesItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := PrecipitationLosses;
    1:
      result := IrrigationLosses;
    else Assert(False);
  end;
end;

function TLossesItem.GetIrrigationLosses: string;
begin
  Result := FFormulaObjects[IrrigationLossesPosition].Formula;
  ResetItemObserver(IrrigationLossesPosition);
end;

function TLossesItem.GetPrecipitationLosses: string;
begin
  Result := FFormulaObjects[PrecipitationLossesPosition].Formula;
  ResetItemObserver(PrecipitationLossesPosition);
end;

procedure TLossesItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    0:
      PrecipitationLosses := Value;
    1:
      IrrigationLosses := Value;
    else Assert(False);
  end;
end;

procedure TLossesItem.SetIrrigationLosses(const Value: string);
begin
  if FFormulaObjects[IrrigationLossesPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, IrrigationLossesPosition, FFormulaObjects[IrrigationLossesPosition]);
  end;
end;

procedure TLossesItem.SetPrecipitationLosses(const Value: string);
begin
  if FFormulaObjects[PrecipitationLossesPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PrecipitationLossesPosition, FFormulaObjects[PrecipitationLossesPosition]);
  end;
end;

{ TLossesCollection }

procedure TLossesCollection.EvaluateBoundaries;
var
  CurrentRecord: TLossesRecord;
  CurrentItem: TLossesItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
begin
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TLossesItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

    Expression := nil;
    Formula := CurrentItem.PrecipitationLosses;
    CurrentRecord.PrecipitationLossesAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrPrecipitationLosses,
          Formula, E.Message);

        CurrentItem.PrecipitationLosses := '0.';
        Formula := CurrentItem.PrecipitationLosses;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.PrecipitationLosses := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.IrrigationLosses;
    CurrentRecord.IrrigationLossesAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrIrrigationLosses,
          Formula, E.Message);

        CurrentItem.IrrigationLosses := '0.';
        Formula := CurrentItem.IrrigationLosses;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.IrrigationLosses := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TLossesCollection.GetLossesTimeValuesFromTime(
  StartTime: double): TLossesRecord;
var
  Index: integer;
  ErrorMessage: string;
begin
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if (FTimeValues[Index].StartingTime <= StartTime) then
    begin
      result := FTimeValues[Index];
      if (FTimeValues[Index].EndingTime > StartTime) then
      begin
        Exit;
      end;
    end;
  end;
  ErrorMessage := Format(IDError, [StartTime]);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteLosses, ErrorMessage);
end;

function TLossesCollection.GetItems(Index: Integer): TLossesItem;
begin
  Result := inherited Items[Index] as TLossesItem;
end;

function TLossesCollection.GetLossesTimeValues(Index: integer): TLossesRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

class function TLossesCollection.ItemClass: TBoundaryItemClass;
begin
  result := TLossesItem;
end;

procedure TLossesCollection.SetItems(Index: Integer; const Value: TLossesItem);
begin
  inherited Items[Index] := Value;
end;

procedure TLossesCollection.SetLossesTimeValues(Index: integer;
  const Value: TLossesRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

{ TCropFunctionItem }

function TCropFunctionItem.BoundaryFormulaCount: integer;
begin
  Result := 3;
end;

function TCropFunctionItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := Slope;
    1:
      result := Intercept;
    2:
      result := Price;
    else Assert(False);
  end;
end;

function TCropFunctionItem.GetIntercept: string;
begin
  Result := FFormulaObjects[InterceptPosition].Formula;
  ResetItemObserver(InterceptPosition);
end;

function TCropFunctionItem.GetPrice: string;
begin
  Result := FFormulaObjects[PricePosition].Formula;
  ResetItemObserver(PricePosition);
end;

procedure TCropFunctionItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  inherited;

end;

function TCropFunctionItem.GetSlope: string;
begin
  Result := FFormulaObjects[SlopePosition].Formula;
  ResetItemObserver(SlopePosition);
end;

procedure TCropFunctionItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    0:
      Slope := Value;
    1:
      Intercept := Value;
    2:
      Price := Value;
    else Assert(False);
  end;
end;

procedure TCropFunctionItem.SetIntercept(const Value: string);
begin
  if FFormulaObjects[InterceptPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, InterceptPosition, FFormulaObjects[InterceptPosition]);
  end;
end;

procedure TCropFunctionItem.SetPrice(const Value: string);
begin
  if FFormulaObjects[PricePosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PricePosition, FFormulaObjects[PricePosition]);
  end;
end;

procedure TCropFunctionItem.SetSlope(const Value: string);
begin
  if FFormulaObjects[SlopePosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SlopePosition, FFormulaObjects[SlopePosition]);
  end;
end;

{ TCropFunctionCollection }

procedure TCropFunctionCollection.EvaluateBoundaries;
var
  CurrentRecord: TCropFunctionRecord;
  CurrentItem: TCropFunctionItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
begin
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TCropFunctionItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

    Expression := nil;
    Formula := CurrentItem.Slope;
    CurrentRecord.SlopeAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrCropFunctionSlope,
          Formula, E.Message);

        CurrentItem.Slope := '0.';
        Formula := CurrentItem.Slope;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Slope := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.Intercept;
    CurrentRecord.InterceptAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrCropFunctionIntercept,
          Formula, E.Message);

        CurrentItem.Intercept := '0.';
        Formula := CurrentItem.Intercept;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Intercept := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.Price;
    CurrentRecord.PriceAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrCropFunctionPrice,
          Formula, E.Message);

        CurrentItem.Price := '0.';
        Formula := CurrentItem.Price;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Price := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TCropFunctionCollection.GetCropFunctionTimeValues(
  Index: integer): TCropFunctionRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TCropFunctionCollection.GetCropFunctionTimeValuesFromTime(
  StartTime: double): TCropFunctionRecord;
var
  Index: integer;
  ErrorMessage: string;
begin
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if (FTimeValues[Index].StartingTime <= StartTime) then
    begin
      result := FTimeValues[Index];
      if (FTimeValues[Index].EndingTime > StartTime) then
      begin
        Exit;
      end;
    end;
  end;
  ErrorMessage := Format(IDError, [StartTime]);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteCropFunction, ErrorMessage);
end;

function TCropFunctionCollection.GetItems(Index: Integer): TCropFunctionItem;
begin
  result := inherited Items[Index] as TCropFunctionItem;
end;

class function TCropFunctionCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TCropFunctionItem;
end;

procedure TCropFunctionCollection.SetCropFunctionTimeValues(Index: integer;
  const Value: TCropFunctionRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

procedure TCropFunctionCollection.SetItems(Index: Integer;
  const Value: TCropFunctionItem);
begin
  inherited Items[Index] := Value;
end;

{ TCropCostItem }

function TCropWaterUseItem.BoundaryFormulaCount: integer;
begin
  Result := 2;
end;

constructor TCropWaterUseItem.Create(Collection: TCollection);
begin
  inherited;
  CropValue := '0';
  Irrigated := 'True';
end;

destructor TCropWaterUseItem.Destroy;
begin
  CropValue := '0';
  Irrigated := 'True';
  inherited;
end;

function TCropWaterUseItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := CropValue;
    1:
      result := Irrigated;
    else Assert(False);
  end;
end;

function TCropWaterUseItem.GetCropValue: string;
begin
  Result := FFormulaObjects[CropValuePosition].Formula;
  ResetItemObserver(CropValuePosition);
end;

function TCropWaterUseItem.GetIrrigated: string;
begin
  Result := FFormulaObjects[WaterUseIrrigatedPosition].Formula;
  ResetItemObserver(WaterUseIrrigatedPosition);
end;

procedure TCropWaterUseItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    0:
      CropValue := Value;
    1:
      Irrigated := Value;
    else Assert(False);
  end;
end;

procedure TCropWaterUseItem.SetCropValue(const Value: string);
begin
  if FFormulaObjects[CropValuePosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, CropValuePosition, FFormulaObjects[CropValuePosition]);
  end;
end;

procedure TCropWaterUseItem.SetIrrigated(const Value: string);
begin
  if FFormulaObjects[WaterUseIrrigatedPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, WaterUseIrrigatedPosition, FFormulaObjects[WaterUseIrrigatedPosition]);
  end;
end;

{ TCropCostCollection }

procedure TCropWaterUseCollection.EvaluateBoundaries;
var
  CurrentRecord: TCropWaterUseRecord;
  CurrentItem: TCropWaterUseItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
begin
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TCropWaterUseItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

    Expression := nil;
    Formula := CurrentItem.CropValue;
    CurrentRecord.CropValueAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrCropFunctionCropValue,
          Formula, E.Message);

        CurrentItem.CropValue := '0.';
        Formula := CurrentItem.CropValue;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.CropValue := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.Irrigated;
    CurrentRecord.IrrigatedAnnotation := Format(StrAssignedUsingS,
      [Formula]);
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrCropFunctionIrrigation,
          Formula, E.Message);

        CurrentItem.Irrigated := 'True';
        Formula := CurrentItem.Irrigated;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Irrigated := Expression.BooleanResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TCropWaterUseCollection.GetCropCostTimeValues(
  Index: integer): TCropWaterUseRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TCropWaterUseCollection.GetCropFunctionTimeValuesFromTime(
  StartTime: double): TCropWaterUseRecord;
var
  Index: integer;
  ErrorMessage: string;
begin
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if (FTimeValues[Index].StartingTime <= StartTime) then
    begin
      result := FTimeValues[Index];
      if (FTimeValues[Index].EndingTime > StartTime) then
      begin
        Exit;
      end;
    end;
  end;
  ErrorMessage := Format(IDError, [StartTime]);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteCropWaterUse, ErrorMessage);
end;

function TCropWaterUseCollection.GetItems(Index: Integer): TCropWaterUseItem;
begin
  result := inherited Items[Index] as TCropWaterUseItem;
end;

class function TCropWaterUseCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TCropWaterUseItem;
end;

procedure TCropWaterUseCollection.SetCropCostTimeValues(Index: integer;
  const Value: TCropWaterUseRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

procedure TCropWaterUseCollection.SetItems(Index: Integer;
  const Value: TCropWaterUseItem);
begin
  inherited Items[Index] := Value;
end;

{ TCropItem }

procedure TCropItem.Assign(Source: TPersistent);
var
  Index: Integer;
  SourceItem: TCropItem;
begin
  if Source is TCropItem then
  begin
    SourceItem := TCropItem(Source);
    CropName := SourceItem.CropName;
    FmpRootDepthCollection := SourceItem.FmpRootDepthCollection;
    EvapFractionsCollection := SourceItem.EvapFractionsCollection;
    LossesCollection := SourceItem.LossesCollection;
    CropFunctionCollection := SourceItem.CropFunctionCollection;
    PondDepthCollection := SourceItem.PondDepthCollection;
    AddedDemandCollection := SourceItem.AddedDemandCollection;
    ConvertToBareSoilCollection := SourceItem.ConvertToBareSoilCollection;
    UseEvapFractionCorrectionCollection := SourceItem.UseEvapFractionCorrectionCollection;

    IrrigationCollection := SourceItem.IrrigationCollection;
    LandUseFractionCollection := SourceItem.LandUseFractionCollection;
    CropCoefficientCollection := SourceItem.CropCoefficientCollection;
    ConsumptiveUseCollection := SourceItem.ConsumptiveUseCollection;
    RootPressureCollection := SourceItem.RootPressureCollection;
    GroundwaterRootInteraction := SourceItem.GroundwaterRootInteraction;
    TranspirationFractionCollection := SourceItem.TranspirationFractionCollection;
    SWLossFractionPrecipCollection := SourceItem.SWLossFractionPrecipCollection;
    SWLossFractionPrecipCollection := SourceItem.SWLossFractionPrecipCollection;

    // This is done differently in TChemSpeciesItem
    // There the field is first assign then set to an empty string
    // and then the property is assigned.
    LandUseAreaFractionDataArrayName := SourceItem.LandUseAreaFractionDataArrayName;
    CropCoefficientDataArrayName := SourceItem.CropCoefficientDataArrayName;
    ConsumptiveUseDataArrayName := SourceItem.ConsumptiveUseDataArrayName;
    IrrigationDataArrayName := SourceItem.IrrigationDataArrayName;
    RootDepthDataArrayName := SourceItem.RootDepthDataArrayName;
    GroundwaterRootInteractionDataArrayName := SourceItem.GroundwaterRootInteractionDataArrayName;
    TranspirationFractionDataArrayName := SourceItem.TranspirationFractionDataArrayName;
    EvaporationIrrigationDataArrayName := SourceItem.EvaporationIrrigationDataArrayName;
    SWLossFractionIrrigationDataArrayName := SourceItem.SWLossFractionIrrigationDataArrayName;
    SWLossFractionPrecipDataArrayName := SourceItem.SWLossFractionPrecipDataArrayName;
    AddedDemandDataArrayName := SourceItem.AddedDemandDataArrayName;
    CropHasSalinityDemandDataArrayName := SourceItem.CropHasSalinityDemandDataArrayName;
    SalinityToleranceCollection := SourceItem.SalinityToleranceCollection;
    MaxLeachingRequirementCollection := SourceItem.MaxLeachingRequirementCollection;
    LeachingRequirementCollection := SourceItem.LeachingRequirementCollection;
    SalinityAppliedWater := SourceItem.SalinityAppliedWater;
  end;
  inherited;
end;

function TCropItem.BoundaryFormulaCount: integer;
begin
  result := 16;
end;

constructor TCropItem.Create(Collection: TCollection);
var
  InvalidatEvent: TNotifyEvent;
begin
  inherited;
  CreateFormulaObjects;
  InitializeFormulas;

  if Model = nil then
  begin
    InvalidatEvent := nil;
  end
  else
  begin
   InvalidatEvent := (Model as TCustomModel).DoInvalidate;
  end;

  FCropFunctionCollection := TCropFunctionCollection.Create(Model as TCustomModel);
  FFmpRootDepthCollection := TFmpRootDepthCollection.Create(Model as TCustomModel);
  FFmpRootDepthCollection.OwhmNames.Add(StrRootingDepth);
  FEvapFractionsCollection := TEvapFractionsCollection.Create(Model as TCustomModel);
  FLossesCollection := TLossesCollection.Create(Model as TCustomModel);
  FCropWaterUseCollection := TCropWaterUseCollection.Create(Model as TCustomModel);

  FIrrigationCollection :=
    TIrrigationCollection.Create(Model as TCustomModel);
  FLandUseFractionCollection := TOwhmCollection.Create(Model as TCustomModel);
  FLandUseFractionCollection.OwhmNames.Add(StrLandUseAreaFracti);
  FCropCoefficientCollection := TOwhmCollection.Create(Model as TCustomModel);
  FCropCoefficientCollection.OwhmNames.Add(StrCropCoefficient);
  FConsumptiveUseCollection := TOwhmCollection.Create(Model as TCustomModel);
  FConsumptiveUseCollection.OwhmNames.Add(StrConsumptiveUse);
  FRootPressureCollection := TRootPressureCollection.Create(Model as TCustomModel);
  FGroundwaterRootInteraction :=
    TGroundwaterRootInteraction.Create(InvalidatEvent);
  FTranspirationFractionCollection := TOwhmCollection.Create(Model as TCustomModel);
  FTranspirationFractionCollection.OwhmNames.Add(StrTranspirationFracti);
  FSWLossFractionPrecipCollection := TOwhmCollection.Create(Model as TCustomModel);
  FSWLossFractionPrecipCollection.OwhmNames.Add(StrSurfaceWaterLossF);
  FPondDepthCollection := TOwhmCollection.Create(Model as TCustomModel);
  FPondDepthCollection.OwhmNames.Add(StrPondDepth);
  FAddedDemandCollection := TAddedDemandCollection.Create(Model as TCustomModel);
  FConvertToBareSoilCollection := TBoolFarmCollection.Create(Model as TCustomModel);
  FConvertToBareSoilCollection.OwhmNames.Add(StrZeroConsumptiveUse);
  FUseEvapFractionCorrectionCollection := TBoolFarmCollection.Create(Model as TCustomModel);
  FUseEvapFractionCorrectionCollection.OwhmNames.Add(StrEvaporationIrrigatiCorrection);
  FSalinityToleranceCollection := TOwhmCollection.Create(Model as TCustomModel);
  FSalinityToleranceCollection.OwhmNames.Add(StrSalinityTolerance);

  FMaxLeachingRequirementCollection := TOwhmCollection.Create(Model as TCustomModel);
  FMaxLeachingRequirementCollection.OwhmNames.Add(StrMaximumLeachingReq);

  FLeachingRequirementCollection := TLeachCollection.Create(Model as TCustomModel);
  FLeachingRequirementCollection.OwhmNames.Add(StrLeachingChoice);
  FLeachingRequirementCollection.OwhmNames.Add(StrFormula);

  FSalinityAppliedWater := TLeachCollection.Create(Model as TCustomModel);
  FSalinityAppliedWater.OwhmNames.Add(StrLeachingChoice);
  FSalinityAppliedWater.OwhmNames.Add(StrFormula);
end;

destructor TCropItem.Destroy;
var
  LocalModel: TPhastModel;
  Position: integer;
  FarmList: TFarmList;
  FarmIndex: Integer;
  AFarm: TFarm;
  CropEffIndex: Integer;
  AFarmEff: TFarmEfficienciesItem;
  Unlocker: TDefineGlobalIntegerObject;
begin
  if (Model <> nil) and (CropName <> '')  then
  begin
    LocalModel := Model as TPhastModel;

    if ([csLoading, csDestroying] * (Model as TComponent).ComponentState) = [] then
    begin
      Unlocker := TDefineGlobalIntegerObject.Create(Model as TCustomModel,
        FCropName, FCropName, StrCropVariable);
      try
        Unlocker.Locked := False;
      finally
        Unlocker.Free;
      end;
    end;

    Position := LocalModel.GlobalVariables.IndexOfVariable(CropName);
    if Position >= 0 then
    begin
      if (not (csDestroying in LocalModel.ComponentState))
        and (not LocalModel.Clearing) then
      begin
        FarmList := (Collection as TCropCollection).FarmList;
        for FarmIndex := 0 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[FarmIndex];
          for CropEffIndex := 0 to AFarm.FarmEfficiencyCollection.Count - 1 do
          begin
            AFarmEff := AFarm.FarmEfficiencyCollection[CropEffIndex];
            if AFarmEff.CropEfficiency.CropName = FCropName then
            begin
              AFarmEff.Free;
              break;
            end;
          end;
        end;

        for FarmIndex := 0 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[FarmIndex];
          for CropEffIndex := 0 to AFarm.AddedCropDemandFlux.Count - 1 do
          begin
            AFarmEff := AFarm.AddedCropDemandFlux[CropEffIndex];
            if AFarmEff.CropEfficiency.CropName = FCropName then
            begin
              AFarmEff.Free;
              break;
            end;
          end;
        end;

        for FarmIndex := 0 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[FarmIndex];
          for CropEffIndex := 0 to AFarm.AddedCropDemandRate.Count - 1 do
          begin
            AFarmEff := AFarm.AddedCropDemandRate[CropEffIndex];
            if AFarmEff.CropEfficiency.CropName = FCropName then
            begin
              AFarmEff.Free;
              break;
            end;
          end;
        end;
      end;
    end;
  end;

  FSalinityAppliedWater.Free;
  FLeachingRequirementCollection.Free;
  FMaxLeachingRequirementCollection.Free;
  FSalinityToleranceCollection.Free;
  FUseEvapFractionCorrectionCollection.Free;
  FConvertToBareSoilCollection.Free;
  FAddedDemandCollection.Free;
  FPondDepthCollection.Free;
  FSWLossFractionPrecipCollection.Free;
  FTranspirationFractionCollection.Free;
  FGroundwaterRootInteraction.Free;
  FRootPressureCollection.Free;
  FConsumptiveUseCollection.Free;
  FCropCoefficientCollection.Free;
  FLandUseFractionCollection.Free;
  FIrrigationCollection.Free;

  FCropFunctionCollection.Free;
  FFmpRootDepthCollection.Free;
  FEvapFractionsCollection.Free;
  FLossesCollection.Free;
  FCropWaterUseCollection.Free;

  InitializeFormulas;
  inherited;
end;

function TCropItem.GetBoundaryFormula(Index: integer): string;
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

procedure TCropItem.InitializeFormulas;
begin
  PSI1 := '0';
  PSI2 := '0';
  PSI3 := '0';
  PSI4 := '0';
  BaseTemperature := '0';
  MinimumCutoffTemperature := '0';
  MaximumCutoffTemperature := '0';
  Coefficient0 := '0';
  Coefficient1 := '0';
  Coefficient2 := '0';
  Coefficient3 := '0';
  BeginningRootDepth := '0';
  MaximumRootDepth := '0';
  RootGrowthCoefficient := '0';
  Irrigated := 'True';
  Fallow := 'False';
end;

function TCropItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCropItem;
  PropIndex: Integer;
begin
  OtherItem := nil;
  result := (AnotherItem is TCropItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TCropItem(AnotherItem);
    result := (CropName = OtherItem.CropName)
      and FmpRootDepthCollection.IsSame(OtherItem.FmpRootDepthCollection)
      and EvapFractionsCollection.IsSame(OtherItem.EvapFractionsCollection)
      and LossesCollection.IsSame(OtherItem.LossesCollection)
      and CropFunctionCollection.IsSame(OtherItem.CropFunctionCollection)
      and CropWaterUseCollection.IsSame(OtherItem.CropWaterUseCollection)

      and IrrigationCollection.IsSame(OtherItem.IrrigationCollection)
      and LandUseFractionCollection.IsSame(OtherItem.LandUseFractionCollection)
      and CropCoefficientCollection.IsSame(OtherItem.CropCoefficientCollection)
      and ConsumptiveUseCollection.IsSame(OtherItem.ConsumptiveUseCollection)
      and RootPressureCollection.IsSame(OtherItem.RootPressureCollection)
      and GroundwaterRootInteraction.IsSame(OtherItem.GroundwaterRootInteraction)
      and TranspirationFractionCollection.IsSame(OtherItem.TranspirationFractionCollection)
      and SWLossFractionPrecipCollection.IsSame(OtherItem.SWLossFractionPrecipCollection)
      and PondDepthCollection.IsSame(OtherItem.PondDepthCollection)
      and AddedDemandCollection.IsSame(OtherItem.AddedDemandCollection)
      and ConvertToBareSoilCollection.IsSame(OtherItem.ConvertToBareSoilCollection)
      and UseEvapFractionCorrectionCollection.IsSame(OtherItem.UseEvapFractionCorrectionCollection)
      and SalinityToleranceCollection.IsSame(OtherItem.SalinityToleranceCollection)
      and MaxLeachingRequirementCollection.IsSame(OtherItem.MaxLeachingRequirementCollection)
      and LeachingRequirementCollection.IsSame(OtherItem.LeachingRequirementCollection)
      and SalinityAppliedWater.IsSame(OtherItem.SalinityAppliedWater)

      and (LandUseAreaFractionDataArrayName = OtherItem.LandUseAreaFractionDataArrayName)
      and (CropCoefficientDataArrayName = OtherItem.CropCoefficientDataArrayName)
      and (ConsumptiveUseDataArrayName = OtherItem.ConsumptiveUseDataArrayName)
      and (IrrigationDataArrayName = OtherItem.IrrigationDataArrayName)
      and (RootDepthDataArrayName = OtherItem.RootDepthDataArrayName)
      and (GroundwaterRootInteractionDataArrayName = OtherItem.GroundwaterRootInteractionDataArrayName)
      and (TranspirationFractionDataArrayName = OtherItem.TranspirationFractionDataArrayName)
      and (EvaporationIrrigationDataArrayName = OtherItem.EvaporationIrrigationDataArrayName)
      and (SWLossFractionIrrigationDataArrayName = OtherItem.SWLossFractionIrrigationDataArrayName)
      and (SWLossFractionPrecipDataArrayName = OtherItem.SWLossFractionPrecipDataArrayName)
      and (AddedDemandDataArrayName = OtherItem.AddedDemandDataArrayName)
      and (CropHasSalinityDemandDataArrayName = OtherItem.CropHasSalinityDemandDataArrayName)
  end;
end;

procedure TCropItem.SetAddedDemandCollection(const Value: TAddedDemandCollection);
begin
  FAddedDemandCollection.Assign(Value);
end;

procedure TCropItem.SetAddedDemandDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleAddedDemandUsed;
    UpdateDat.OldDataArrayName := FAddedDemandDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FAddedDemandDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: ADDED_DEMAND';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FAddedDemandDataArrayName, NewName);
end;

procedure TCropItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

procedure TCropItem.SetConsumptiveUseCollection(const Value: TOwhmCollection);
begin
  FConsumptiveUseCollection.Assign(Value);
end;

procedure TCropItem.SetConsumptiveUseDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleConsumptiveUseUsed;
    UpdateDat.OldDataArrayName := FConsumptiveUseDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FConsumptiveUseDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: CONSUMPTIVE_USE';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);
  end;

  SetCaseSensitiveStringProperty(FConsumptiveUseDataArrayName, NewName);
end;

procedure TCropItem.SetConvertToBareSoilCollection(const Value: TBoolFarmCollection);
begin
  FConvertToBareSoilCollection.Assign(Value);
end;

procedure TCropItem.SetCropCoefficientCollection(const Value: TOwhmCollection);
begin
  FCropCoefficientCollection.Assign(Value);
end;

procedure TCropItem.SetCropCoefficientDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleCropCoefficientUsed;
    UpdateDat.OldDataArrayName := FCropCoefficientDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FCropCoefficientDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: CROP_COEFFICIENT';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FCropCoefficientDataArrayName, NewName);
end;

procedure TCropItem.SetCropFunctionCollection(
  const Value: TCropFunctionCollection);
begin
  FCropFunctionCollection.Assign(Value);
end;

procedure TCropItem.SetCropHasSalinityDemandDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmSalinityFlush: TFarmProcess4SalinityFlush;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmSalinityFlush := LocalModel.ModflowPackages.FarmSalinityFlush;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleCropHasSalinityDemandUsed;
    UpdateDat.OldDataArrayName := FCropHasSalinityDemandDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FCropHasSalinityDemandDisplayName;
    UpdateDat.NewFormula := 'False';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: CROP_HAS_SALINITY_DEMAND';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtBoolean;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FCropHasSalinityDemandDataArrayName, NewName);
end;

procedure TCropItem.SetCropName(Value: string);
var
  ChangeGlobals: TDefineGlobalIntegerObject;
  FarmList: TFarmList;
  FarmIndex: Integer;
  AFarm: TFarm;
  CropEffIndex: Integer;
  AFarmEff: TFarmEfficienciesItem;
  OldRoot: string;
  NewRoot: string;
begin
  if (FCropName <> Value) and (Model <> nil)
    and not (csReading in (Model as TComponent).ComponentState) then
  begin
    Value := GenerateNewName(Value, nil, '_');
  end;
  ChangeGlobals := TDefineGlobalIntegerObject.Create(Model as TCustomModel, FCropName, Value,
    StrCropVariable);
  try
    if FCropName <> Value then
    begin
      if (Model <> nil) and (Value <> '') then
      begin
        ChangeGlobals.Rename;

        FarmList := (Collection as TCropCollection).FarmList;
        for FarmIndex := 0 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[FarmIndex];
          if FCropName = '' then
          begin
            AFarmEff := AFarm.FarmEfficiencyCollection.Add;
            AFarmEff.CropEfficiency.CropName := Value;
            AFarmEff.Index := index;
          end
          else
          begin
            for CropEffIndex := 0 to AFarm.FarmEfficiencyCollection.Count - 1 do
            begin
              AFarmEff := AFarm.FarmEfficiencyCollection[CropEffIndex];
              if AFarmEff.CropEfficiency.CropName = FCropName then
              begin
                AFarmEff.CropEfficiency.CropName := Value;
                break;
              end;
            end;
          end;
        end;

        for FarmIndex := 0 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[FarmIndex];
          if FCropName = '' then
          begin
            AFarmEff := AFarm.AddedCropDemandFlux.Add;
            AFarmEff.CropEfficiency.CropName := Value;
            AFarmEff.Index := index;
          end
          else
          begin
            for CropEffIndex := 0 to AFarm.AddedCropDemandFlux.Count - 1 do
            begin
              AFarmEff := AFarm.AddedCropDemandFlux[CropEffIndex];
              if AFarmEff.CropEfficiency.CropName = FCropName then
              begin
                AFarmEff.CropEfficiency.CropName := Value;
                break;
              end;
            end;
          end;
        end;

        for FarmIndex := 0 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[FarmIndex];
          if FCropName = '' then
          begin
            AFarmEff := AFarm.AddedCropDemandRate.Add;
            AFarmEff.CropEfficiency.CropName := Value;
            AFarmEff.Index := index;
          end
          else
          begin
            for CropEffIndex := 0 to AFarm.AddedCropDemandRate.Count - 1 do
            begin
              AFarmEff := AFarm.AddedCropDemandRate[CropEffIndex];
              if AFarmEff.CropEfficiency.CropName = FCropName then
              begin
                AFarmEff.CropEfficiency.CropName := Value;
                break;
              end;
            end;
          end;
        end;
      end;

      if SameText(FCropName, Value) then
      begin
        OldRoot := GenerateNewRoot(FCropName);
        NewRoot := GenerateNewRoot(Value);

        FLandUseAreaFractionDisplayName := StringReplace(FLandUseAreaFractionDisplayName,
          OldRoot,NewRoot, []);
        LandUseAreaFractionDataArrayName := StringReplace(LandUseAreaFractionDataArrayName,
          OldRoot,NewRoot, []);

        FCropCoefficientDisplayName := StringReplace(FCropCoefficientDisplayName,
          OldRoot,NewRoot, []);
        CropCoefficientDataArrayName := StringReplace(CropCoefficientDataArrayName,
          OldRoot,NewRoot, []);

        FConsumptiveUseDisplayName := StringReplace(FConsumptiveUseDisplayName,
          OldRoot,NewRoot, []);
        ConsumptiveUseDataArrayName := StringReplace(ConsumptiveUseDataArrayName,
          OldRoot,NewRoot, []);

        FIrrigationDisplayName := StringReplace(FIrrigationDisplayName,
          OldRoot,NewRoot, []);
        IrrigationDataArrayName := StringReplace(IrrigationDataArrayName,
          OldRoot,NewRoot, []);

        FRootDepthDisplayName := StringReplace(FRootDepthDisplayName,
          OldRoot,NewRoot, []);
        RootDepthDataArrayName := StringReplace(RootDepthDataArrayName,
          OldRoot,NewRoot, []);

        FGroundwaterRootInteractionDisplayName := StringReplace(FGroundwaterRootInteractionDisplayName,
          OldRoot,NewRoot, []);
        GroundwaterRootInteractionDataArrayName := StringReplace(GroundwaterRootInteractionDataArrayName,
          OldRoot,NewRoot, []);

        FTranspirationFractionDisplayName := StringReplace(FTranspirationFractionDisplayName,
          OldRoot,NewRoot, []);
        TranspirationFractionDataArrayName := StringReplace(TranspirationFractionDataArrayName,
          OldRoot,NewRoot, []);

        FEvaporationIrrigationDisplayName := StringReplace(FEvaporationIrrigationDisplayName,
          OldRoot,NewRoot, []);
        EvaporationIrrigationDataArrayName := StringReplace(EvaporationIrrigationDataArrayName,
          OldRoot,NewRoot, []);

        FSWLossFractionPrecipDisplayName := StringReplace(FSWLossFractionPrecipDisplayName,
          OldRoot,NewRoot, []);
        SWLossFractionPrecipDataArrayName := StringReplace(SWLossFractionPrecipDataArrayName,
          OldRoot,NewRoot, []);

        FSWLossFractionIrrigationDisplayName := StringReplace(FSWLossFractionIrrigationDisplayName,
          OldRoot,NewRoot, []);
        SWLossFractionIrrigationDataArrayName := StringReplace(SWLossFractionIrrigationDataArrayName,
          OldRoot,NewRoot, []);

        FAddedDemandDisplayName := StringReplace(FAddedDemandDisplayName,
          OldRoot,NewRoot, []);
        AddedDemandDataArrayName := StringReplace(AddedDemandDataArrayName,
          OldRoot,NewRoot, []);

        FCropHasSalinityDemandDisplayName := StringReplace(FCropHasSalinityDemandDisplayName,
          OldRoot,NewRoot, []);
        CropHasSalinityDemandDataArrayName := StringReplace(CropHasSalinityDemandDataArrayName,
          OldRoot,NewRoot, []);
      end
      else
      begin
        FLandUseAreaFractionDisplayName := GenerateNewRoot(StrLandUseFractionPrefix + Value);
        LandUseAreaFractionDataArrayName := GenerateNewRoot(KLandUseFractionPrefix + Value);

        FCropCoefficientDisplayName := GenerateNewRoot(StrCropCoefficientPrefix + Value);
        CropCoefficientDataArrayName := GenerateNewRoot(KCropCoefficientPrefix + Value);

        FConsumptiveUseDisplayName := GenerateNewRoot(StrConsumptiveUsePrefix + Value);
        ConsumptiveUseDataArrayName := GenerateNewRoot(KConsumptiveUsePrefix + Value);

        FIrrigationDisplayName := GenerateNewRoot(StrIrrigationPrefix + Value);
        IrrigationDataArrayName := GenerateNewRoot(KIrrigationPrefix + Value);

        FRootDepthDisplayName := GenerateNewRoot(StrRootDepthPrefix + Value);
        RootDepthDataArrayName := GenerateNewRoot(KRootDepthPrefix + Value);

        FGroundwaterRootInteractionDisplayName := GenerateNewRoot(StrGroundwaterRootInteractionPrefix + Value);
        GroundwaterRootInteractionDataArrayName := GenerateNewRoot(KGroundwaterRootInteractionPrefix + Value);

        FTranspirationFractionDisplayName := GenerateNewRoot(StrTranspirationFractionPrefix + Value);
        TranspirationFractionDataArrayName := GenerateNewRoot(KTranspirationFractionPrefix + Value);

        FEvaporationIrrigationDisplayName := GenerateNewRoot(StrEvaporationIrrigationPrefix + Value);
        EvaporationIrrigationDataArrayName := GenerateNewRoot(KEvaporationIrrigationPrefix + Value);

        FSWLossFractionPrecipDisplayName := GenerateNewRoot(StrSWLossFractionPrecipPrefix + Value);
        SWLossFractionPrecipDataArrayName := GenerateNewRoot(KSWLossFractionPrecipPrefix + Value);

        FSWLossFractionIrrigationDisplayName := GenerateNewRoot(StrSWLossFractionIrrigationPrefix + Value);
        SWLossFractionIrrigationDataArrayName := GenerateNewRoot(KSWLossFractionIrrigationPrefix + Value);

        FAddedDemandDisplayName := GenerateNewRoot(StrAddedDemandPrefix + Value);
        AddedDemandDataArrayName := GenerateNewRoot(KAddedDemandPrefix + Value);

        FCropHasSalinityDemandDisplayName := GenerateNewRoot(StrCropHasSalinityDemandPrefix + Value);
        CropHasSalinityDemandDataArrayName := GenerateNewRoot(KCropHasSalinityDemandPrefix + Value);
      end;


      FCropName := Value;
      InvalidateModel;
    end;
    if (Model <> nil) and (FCropName <> '') then
    begin
      ChangeGlobals.SetValue(Index+1);
    end;
  finally
    ChangeGlobals.Free;
  end;

end;

procedure TCropItem.SetSalinityAppliedWater(const Value: TLeachCollection);
begin
  FSalinityAppliedWater.Assign(Value);
end;

procedure TCropItem.SetSalinityToleranceCollection(
  const Value: TOwhmCollection);
begin
  FSalinityToleranceCollection.Assign(Value);
end;

procedure TCropItem.SetCropWaterUseCollection(
  const Value: TCropWaterUseCollection);
begin
  FCropWaterUseCollection.Assign(Value);
end;

procedure TCropItem.SetEvapFractionsCollection(
  const Value: TEvapFractionsCollection);
begin
  FEvapFractionsCollection.Assign(Value);
end;

procedure TCropItem.SetEvaporationIrrigationDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleEvaporationIrrigationUsed;
    UpdateDat.OldDataArrayName := FEvaporationIrrigationDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FEvaporationIrrigationDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: EVAPORATION_IRRIGATION_FRACTION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FEvaporationIrrigationDataArrayName, NewName);
end;

procedure TCropItem.SetFmpRootDepthCollection(
  const Value: TFmpRootDepthCollection);
begin
  FFmpRootDepthCollection.Assign(Value);
end;

procedure TCropItem.SetGroundwaterRootInteraction(
  const Value: TGroundwaterRootInteraction);
begin
  FGroundwaterRootInteraction.Assign(Value);
end;

procedure TCropItem.SetGroundwaterRootInteractionDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleGroundwaterRootInteractionUsed;
    UpdateDat.OldDataArrayName := FGroundwaterRootInteractionDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FGroundwaterRootInteractionDisplayName;
    UpdateDat.NewFormula := '5';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: GROUNDWATER_ROOT_INTERACTION' + sLineBreak
    + '0 = No Transpiration' + sLineBreak
    + '1 = No Groundwater Interaction' + sLineBreak
    + '2 = Has Anoxia/Soil Stress Loss, NO Root-Groundwater Uptake' + sLineBreak
    + '3 = Has Root-Groundwater Uptake, NO Anoxia/Soil Stress Loss' + sLineBreak
    + '4 = Has Root-Groundwater Uptake and Soil Stress Loss, NO Anoxia Loss' + sLineBreak
    + '5 = Full Interaction' + sLineBreak
    + 'It is recommended to only use option 1, 3, or 5. If keyword is not specified, the default value is 5.';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtInteger;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FGroundwaterRootInteractionDataArrayName, NewName);
end;

procedure TCropItem.SetIndex(Value: Integer);
var
  ChangeGlobals: TDefineGlobalIntegerObject;
begin
  if {(Index <> Value) and} (Model <> nil) and (FCropName <> '') then
  begin
    ChangeGlobals := TDefineGlobalIntegerObject.Create(Model as TCustomModel, FCropName, FCropName,
      StrCropVariable);
    try
      ChangeGlobals.SetValue(Value+1);
    finally
      ChangeGlobals.Free;
    end;
  end;
  inherited;

end;

procedure TCropItem.SetIrrigationCollection(const Value: TIrrigationCollection);
begin
  FIrrigationCollection.Assign(Value);
end;

procedure TCropItem.SetIrrigationDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleIrrigationUsed;
    UpdateDat.OldDataArrayName := FIrrigationDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FIrrigationDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: IRRIGATION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtInteger;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FIrrigationDataArrayName, NewName);
end;

procedure TCropItem.SetLandUseAreaFractionDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleLandUseFractionsUsed;
    UpdateDat.OldDataArrayName := FLandUseAreaFractionDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FLandUseAreaFractionDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: LAND_USE_AREA_FRACTION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FLandUseAreaFractionDataArrayName, NewName);
end;

procedure TCropItem.SetLandUseFractionCollection(const Value: TOwhmCollection);
begin
  FLandUseFractionCollection.Assign(Value);
end;

procedure TCropItem.SetLeachingRequirementCollection(
  const Value: TLeachCollection);
begin
  FLeachingRequirementCollection.Assign(Value);
end;

procedure TCropItem.SetLossesCollection(const Value: TLossesCollection);
begin
  FLossesCollection.Assign(Value);
end;

procedure TCropItem.SetMaxLeachingRequirementCollection(
  const Value: TOwhmCollection);
begin
  FMaxLeachingRequirementCollection.Assign(Value)
end;

procedure TCropItem.SetPondDepthCollection(const Value: TOwhmCollection);
begin
  FPondDepthCollection.Assign(Value);
end;

procedure TCropItem.SetRootDepthDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleRootDepthUsed;
    UpdateDat.OldDataArrayName := FRootDepthDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FRootDepthDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: ROOT_DEPTH';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FRootDepthDataArrayName, NewName);
end;

procedure TCropItem.SetRootPressureCollection(
  const Value: TRootPressureCollection);
begin
  FRootPressureCollection.Assign(Value);
end;

procedure TCropItem.SetSWLossFractionIrrigationDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleSWLossFractionIrrigationUsed;
    UpdateDat.OldDataArrayName := FSWLossFractionIrrigationDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FSWLossFractionIrrigationDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: SURFACEWATER_LOSS_FRACTION_IRRIGATION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FSWLossFractionIrrigationDataArrayName, NewName);
end;

procedure TCropItem.SetSWLossFractionPrecipCollection(
  const Value: TOwhmCollection);
begin
  FSWLossFractionPrecipCollection.Assign(Value);
end;

procedure TCropItem.SetSWLossFractionPrecipDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleSWLossFractionPrecipUsed;
    UpdateDat.OldDataArrayName := FSWLossFractionPrecipDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FSWLossFractionPrecipDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: SURFACEWATER_LOSS_FRACTION_PRECIPITATION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FSWLossFractionPrecipDataArrayName, NewName);
end;

procedure TCropItem.SetTranspirationFractionCollection(const Value: TOwhmCollection);
begin
  FTranspirationFractionCollection.Assign(Value);
end;

procedure TCropItem.SetTranspirationFractionDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  UpdateDat: TUpdataDataArrayRecord;
  FarmLandUse: TFarmProcess4LandUse;
begin
  LocalModel := (Collection as TCropCollection).Model as TPhastModel;

  if LocalModel <> nil then
  begin
    FarmLandUse := LocalModel.ModflowPackages.FarmLandUse;

    UpdateDat.Model := LocalModel;
    UpdateDat.OnDataSetUsed := LocalModel.MultipleTranspirationFractionUsed;
    UpdateDat.OldDataArrayName := FTranspirationFractionDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FTranspirationFractionDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: ROOT_DEPTH';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateDat.DataType := rdtDouble;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FTranspirationFractionDataArrayName, NewName);
end;

procedure TCropItem.SetUseEvapFractionCorrectionCollection(
  const Value: TBoolFarmCollection);
begin
  FUseEvapFractionCorrectionCollection.Assign(Value);
end;

procedure TCropItem.UpdateAllDataArrays;
begin
  if (Collection as TCropCollection).Model <> nil then
  begin
    // Reassigning the name will cause the data set to be created if it is
    // needed.
    LandUseAreaFractionDataArrayName := LandUseAreaFractionDataArrayName;
    CropCoefficientDataArrayName := CropCoefficientDataArrayName;
    ConsumptiveUseDataArrayName := ConsumptiveUseDataArrayName;
    IrrigationDataArrayName := IrrigationDataArrayName;
    RootDepthDataArrayName := RootDepthDataArrayName;
    GroundwaterRootInteractionDataArrayName := GroundwaterRootInteractionDataArrayName;
    TranspirationFractionDataArrayName := TranspirationFractionDataArrayName;
    EvaporationIrrigationDataArrayName := EvaporationIrrigationDataArrayName;
    SWLossFractionPrecipDataArrayName := SWLossFractionPrecipDataArrayName;
    SWLossFractionIrrigationDataArrayName := SWLossFractionIrrigationDataArrayName;
    AddedDemandDataArrayName := AddedDemandDataArrayName;
    CropHasSalinityDemandDataArrayName := CropHasSalinityDemandDataArrayName;
  end;
end;

procedure TCropItem.UpdateFarmProperties;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  FoundItem: Boolean;
  CropEffIndex: Integer;
  AFarmEff: TFarmEfficienciesItem;
  Farms: TFarmCollection;
begin
  if Model <> nil then
  begin
    Farms := (Model as TCustomModel).Farms;
    for FarmIndex := 0 to Farms.Count - 1 do
    begin
      AFarm := Farms[FarmIndex];
      FoundItem := False;
      begin
        for CropEffIndex := 0 to AFarm.FarmEfficiencyCollection.Count - 1 do
        begin
          AFarmEff := AFarm.FarmEfficiencyCollection[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = FCropName then
          begin
            FoundItem := True;
            break;
          end;
        end;
      end;
      if not FoundItem then
      begin
        AFarmEff := AFarm.FarmEfficiencyCollection.Add;
        AFarmEff.CropEfficiency.CropName := FCropName;
      end;

      FoundItem := False;
      begin
        for CropEffIndex := 0 to AFarm.AddedCropDemandFlux.Count - 1 do
        begin
          AFarmEff := AFarm.AddedCropDemandFlux[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = FCropName then
          begin
            FoundItem := True;
            break;
          end;
        end;
      end;
      if not FoundItem then
      begin
        AFarmEff := AFarm.AddedCropDemandFlux.Add;
        AFarmEff.CropEfficiency.CropName := FCropName;
      end;

      FoundItem := False;
      begin
        for CropEffIndex := 0 to AFarm.AddedCropDemandRate.Count - 1 do
        begin
          AFarmEff := AFarm.AddedCropDemandRate[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = FCropName then
          begin
            FoundItem := True;
            break;
          end;
        end;
      end;
      if not FoundItem then
      begin
        AFarmEff := AFarm.AddedCropDemandRate.Add;
        AFarmEff.CropEfficiency.CropName := FCropName;
      end;
    end;
  end;
end;

procedure TCropItem.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean);
begin
  StartRangeExtended := False;
  EndRangeExtended := False;
  FmpRootDepthCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  EvapFractionsCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  LossesCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  CropFunctionCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  CropWaterUseCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);

  IrrigationCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  LandUseFractionCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  CropCoefficientCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  ConsumptiveUseCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  RootPressureCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  TranspirationFractionCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  SWLossFractionPrecipCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  PondDepthCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  AddedDemandCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  ConvertToBareSoilCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  UseEvapFractionCorrectionCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  SalinityToleranceCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  MaxLeachingRequirementCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  LeachingRequirementCollection.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
  SalinityAppliedWater.UpdateTimes(Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
end;

{ TCropCollection }


procedure TCropCollection.Assign(Source: TPersistent);
begin
  FreeAndNil(FFarmList);
  inherited;
  if Model <> nil then
  begin
    (Model as TCustomModel).ModflowPackages.FarmLandUse.InvalidateAll;
  end;
end;

destructor TCropCollection.Destroy;
begin
  FFarmList.Free;
  inherited;
end;

procedure TCropCollection.EvaluateCrops;
var
  CurrentRecord: TCropRecord;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
  CurrentItem: TCropItem;
  FarmProcess: TFarmProcess;
  FarmProcess4: TFarmProcess4;
  FarmLandUse: TFarmProcess4LandUse;
begin
  PhastModel := Model as TPhastModel;
  FarmProcess := PhastModel.ModflowPackages.FarmProcess;
  FarmProcess4 := PhastModel.ModflowPackages.FarmProcess4;
  FarmLandUse := PhastModel.ModflowPackages.FarmLandUse;

  SetLength(FCropArray, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index];
    CurrentRecord.CropID := Index+1;

    if (FarmProcess.CropConsumptiveConcept = cccConcept1) then
    begin
      Expression := nil;
      Formula := CurrentItem.PSI1;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in PSI(1) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.PSI1 := '0.';
          Formula := CurrentItem.PSI1;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.PSI1 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.PSI2;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in PSI(2) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.PSI2 := '0.';
          Formula := CurrentItem.PSI2;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.PSI2 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.PSI3;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in PSI(3) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.PSI3 := '0.';
          Formula := CurrentItem.PSI3;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.PSI3 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.PSI4;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in PSI(4) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.PSI4 := '0.';
          Formula := CurrentItem.PSI4;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.PSI4 := Expression.DoubleResult;
    end;

    if (FarmProcess.RootingDepth = rdCalculated)
      or (FarmProcess.ConsumptiveUse = cuCalculated)
      or (FarmProcess.Precipitation = pSpatiallyDistributed) then
    begin
      Expression := nil;
      Formula := CurrentItem.BaseTemperature;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Base Temperature for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.BaseTemperature := '0.';
          Formula := CurrentItem.BaseTemperature;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.BaseTemperature := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.MinimumCutoffTemperature;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Minimum Cutoff Temperature for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.MinimumCutoffTemperature := '0.';
          Formula := CurrentItem.MinimumCutoffTemperature;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.MinimumCutoffTemperature := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.MaximumCutoffTemperature;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Maximum Cutoff Temperature for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.MaximumCutoffTemperature := '0.';
          Formula := CurrentItem.MaximumCutoffTemperature;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.MaximumCutoffTemperature := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.Coefficient0;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Coefficient(0) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.Coefficient0 := '0.';
          Formula := CurrentItem.Coefficient0;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Coefficient0 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.Coefficient1;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Coefficient(1) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.Coefficient1 := '0.';
          Formula := CurrentItem.Coefficient1;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Coefficient1 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.Coefficient2;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Coefficient(2) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.Coefficient2 := '0.';
          Formula := CurrentItem.Coefficient2;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Coefficient2 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.Coefficient3;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Coefficient(3) for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.Coefficient3 := '0.';
          Formula := CurrentItem.Coefficient3;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Coefficient3 := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.BeginningRootDepth;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Beginning Root Depth for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.BeginningRootDepth := '0.';
          Formula := CurrentItem.BeginningRootDepth;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.BeginningRootDepth := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.MaximumRootDepth;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Maximum Root Depth for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.MaximumRootDepth := '0.';
          Formula := CurrentItem.MaximumRootDepth;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.MaximumRootDepth := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.RootGrowthCoefficient;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Maximum Root Depth for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.RootGrowthCoefficient := '0.';
          Formula := CurrentItem.RootGrowthCoefficient;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.RootGrowthCoefficient := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.Irrigated;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Irrigated for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.Irrigated := 'False';
          Formula := CurrentItem.Irrigated;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Irrigated := Expression.BooleanResult;
    end;

    if FarmProcess.DeficiencyPolicy = dpWaterStacking then
    begin
      Expression := nil;
      Formula := CurrentItem.Fallow;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in Fallow for "%s" in the Farm Process', [CurrentItem.CropName]),
            Formula, E.Message);

          CurrentItem.Fallow := 'False';
          Formula := CurrentItem.Fallow;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Fallow := Expression.BooleanResult;
    end;
    FCropArray[Index] := CurrentRecord;

//    if FarmProcess.RootingDepth = rdSpecified then
//    begin
//      CurrentItem.FmpRootDepthCollection.EvaluateBoundaries;
//    end;

    CurrentItem.EvapFractionsCollection.EvaluateBoundaries;

    if FarmProcess.FractionOfInefficiencyLosses = filSpecified then
    begin
      CurrentItem.LossesCollection.EvaluateBoundaries;
    end;

    if FarmProcess.DeficiencyPolicy in
      [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool] then
    begin
      CurrentItem.CropFunctionCollection.EvaluateBoundaries;
    end;

    if FarmProcess.ConsumptiveUse in
      [cuPotentialET, cuPotentialAndReferenceET, cuCropCoefficient] then
    begin
      CurrentItem.CropWaterUseCollection.EvaluateBoundaries;
    end;

  end;
end;

function TCropCollection.GetFarmList: TFarmList;
var
  LocalModel: TPhastModel;
  FarmIndex: integer;
begin
  if FFarmList = nil then
  begin
    FFarmList := TFarmList.Create;
    Assert(Model <> nil);
    LocalModel := Model as TPhastModel;
    for FarmIndex := 0 to LocalModel.Farms.Count - 1 do
    begin
      FFarmList.Add(LocalModel.Farms[FarmIndex]);
    end;
  end;
  result := FFarmList
end;

function TCropCollection.GetItems(Index: Integer): TCropItem;
begin
  result := inherited Items[Index] as TCropItem;
end;

class function TCropCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCropItem;
end;

procedure TCropCollection.SetItems(Index: Integer; const Value: TCropItem);
begin
  inherited Items[Index] := Value;
end;

function TCropCollection.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := False;
end;

procedure TCropCollection.UpdateAllDataArrays;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].UpdateAllDataArrays;
  end;
end;

procedure TCropCollection.UpdateFarmProperties;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].UpdateFarmProperties;
  end;
end;

{ TIrrigationItem }

function TCropIrrigationItem.BoundaryFormulaCount: integer;
begin
  Result := 3;
end;

function TCropIrrigationItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    IrrigationPosition:
      result := Irrigation;
    EvapIrrigateFractionPosition:
      result := EvapIrrigateFraction;
    SurfWaterLossFracIrrPosition:
      result := SurfaceWaterLossFractionIrrigation;
    else Assert(False);
  end;
end;

function TCropIrrigationItem.GetEvapIrrigateFraction: string;
begin
  Result := FFormulaObjects[EvapIrrigateFractionPosition].Formula;
  ResetItemObserver(EvapIrrigateFractionPosition);
end;

function TCropIrrigationItem.GetIrrigation: string;
begin
  Result := FFormulaObjects[IrrigationPosition].Formula;
  ResetItemObserver(IrrigationPosition);
end;

function TCropIrrigationItem.GetSurfaceWaterLossFractionIrrigation: string;
begin
  Result := FFormulaObjects[SurfWaterLossFracIrrPosition].Formula;
  ResetItemObserver(SurfWaterLossFracIrrPosition);
end;

procedure TCropIrrigationItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    IrrigationPosition:
      Irrigation := Value;
    EvapIrrigateFractionPosition:
      EvapIrrigateFraction := Value;
    SurfWaterLossFracIrrPosition:
      SurfaceWaterLossFractionIrrigation := Value;
    else Assert(False);
  end;
end;

procedure TCropIrrigationItem.SetEvapIrrigateFraction(const Value: string);
begin
  if FFormulaObjects[EvapIrrigateFractionPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, EvapIrrigateFractionPosition,
      FFormulaObjects[EvapIrrigateFractionPosition]);
  end;
end;

procedure TCropIrrigationItem.SetIrrigation(const Value: string);
begin
  if FFormulaObjects[IrrigationPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, IrrigationPosition,
      FFormulaObjects[IrrigationPosition]);
  end;
end;

procedure TCropIrrigationItem.SetSurfaceWaterLossFractionIrrigation(
  const Value: string);
begin
  if FFormulaObjects[SurfWaterLossFracIrrPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SurfWaterLossFracIrrPosition,
      FFormulaObjects[SurfWaterLossFracIrrPosition]);
  end;
end;

{ TFmp4IrrigationCollection }

function TIrrigationCollection.GetItems(Index: Integer): TCropIrrigationItem;
begin
  result := inherited Items[Index] as TCropIrrigationItem;
end;

class function TIrrigationCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCropIrrigationItem;
end;

procedure TIrrigationCollection.SetItems(Index: Integer;
  const Value: TCropIrrigationItem);
begin
  inherited Items[Index] := Value;
end;

{ TRootPressureItem }

function TRootPressureItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

{ TRootPressureCollection }

class function TRootPressureCollection.ItemClass: TBoundaryItemClass;
begin
  result := TRootPressureItem;
end;

{ TGroundwaterRootInteraction }

procedure TGroundwaterRootInteraction.Assign(Source: TPersistent);
begin
  if Source is TGroundwaterRootInteraction then
  begin
    InteractionCode := TGroundwaterRootInteraction(Source).InteractionCode
  end
  else
  begin
    inherited;
  end;
end;

class procedure TGroundwaterRootInteraction.ConvertFromInteractionCode(
  Code: TInteractionCode; var HasTranspirationB, HasGroundwater, HasAnoxia,
  HasStress: Boolean);
begin
  case Code of
    0:
      begin
        HasTranspirationB := False;
        HasGroundwater := False;
        HasAnoxia := False;
        HasStress := False;
      end;
    1:
      begin
        HasTranspirationB := True;
        HasGroundwater := False;
        HasAnoxia := False;
        HasStress := False;
      end;
    2:
      begin
        HasTranspirationB := True;
        HasGroundwater := False;
        HasAnoxia := True;
        HasStress := True;
      end;
    3:
      begin
        HasTranspirationB := True;
        HasGroundwater := True;
        HasAnoxia := False;
        HasStress := False;
      end;
    4:
      begin
        HasTranspirationB := True;
        HasGroundwater := True;
        HasAnoxia := False;
        HasStress := True;
      end;
    5:
      begin
        HasTranspirationB := True;
        HasGroundwater := True;
        HasAnoxia := True;
        HasStress := True;

      end;
  end;
end;

class function TGroundwaterRootInteraction.ConvertToInteractionCode(
  HasTranspriationB, GWUptakeB, AnoxiaB, SoilStressB: Boolean): TInteractionCode;
begin
  if HasTranspriationB then
  begin
    if GWUptakeB then
    begin
      if AnoxiaB then
      begin
        result := 5;
      end
      else
      begin
        if SoilStressB then
        begin
          result := 4;
        end
        else
        begin
          result := 3;
        end;
      end;
    end
    else
    begin
      if AnoxiaB or SoilStressB then
      begin
        result := 2;
      end
      else
      begin
        result := 1;
      end;
     end;
  end
  else
  begin
    result := 0;
  end;
end;

constructor TGroundwaterRootInteraction.Create(
  InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FInteractionCode := 5;
end;

function TGroundwaterRootInteraction.GetAnoxia: Boolean;
begin
  Result := FInteractionCode in [2, 5]
end;

function TGroundwaterRootInteraction.GetGroundwaterUptake: Boolean;
begin
  Result := FInteractionCode in [3..5]
end;

function TGroundwaterRootInteraction.GetHasTranspiration: Boolean;
begin
  Result := FInteractionCode <> 0
end;

function TGroundwaterRootInteraction.GetSoilStress: Boolean;
begin
  Result := FInteractionCode in [2, 4, 5]
end;

function TGroundwaterRootInteraction.IsSame(
  OtherItem: TGroundwaterRootInteraction): Boolean;
begin
  result := InteractionCode = OtherItem.InteractionCode;
end;

procedure TGroundwaterRootInteraction.SetAnoxia(const Value: Boolean);
begin
  InteractionCode := ConvertToInteractionCode(HasTranspiration, GroundwaterUptake, Value, SoilStress)
end;

procedure TGroundwaterRootInteraction.SetGroundwaterUptake(
  const Value: Boolean);
begin
  InteractionCode := ConvertToInteractionCode(HasTranspiration, Value, Anoxia, SoilStress)
end;

procedure TGroundwaterRootInteraction.SetHasTranspiration(const Value: Boolean);
begin
  InteractionCode := ConvertToInteractionCode(Value, GroundwaterUptake, Anoxia, SoilStress)
end;

procedure TGroundwaterRootInteraction.SetInteractionCode(const Value: TInteractionCode);
begin
  if FInteractionCode <> Value then
  begin
    FInteractionCode := Value;
    InvalidateModel;
  end;
end;

procedure TGroundwaterRootInteraction.SetSoilStress(const Value: Boolean);
begin
  InteractionCode := ConvertToInteractionCode(HasTranspiration, GroundwaterUptake, Anoxia, Value)
end;

{ TAddedDemandCollection }

constructor TAddedDemandCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(nil, Model, nil);
end;

class function TAddedDemandCollection.ItemClass: TBoundaryItemClass;
begin
  result := TAddedDemandItem;
end;

{ TAddedDemandFarmCollection }

function TAddedDemandFarmCollection.Add: TAddedDemandFarmItem;
begin
  result := inherited Add as TAddedDemandFarmItem
end;

constructor TAddedDemandFarmCollection.Create(Model: TBaseModel);
begin
  inherited Create(TAddedDemandFarmItem, Model, nil, nil);
end;

{ TAddedDemandItem }

procedure TAddedDemandItem.Assign(Source: TPersistent);
begin
  if Source is TAddedDemandItem then
  begin
    AddedDemandValues := TAddedDemandItem(Source).AddedDemandValues;
  end;
  inherited;

end;

constructor TAddedDemandItem.Create(Collection: TCollection);
begin
  inherited;
  FAddedDemandValues := TAddedDemandFarmCollection.Create(Model as TCustomModel);
end;

destructor TAddedDemandItem.Destroy;
begin
  FAddedDemandValues.Free;
  inherited;
end;

function TAddedDemandItem.GetItemByFarmGUID(
  FarmGUID: string): TAddedDemandFarmItem;
var
  FormulaIndex: Integer;
  AnItem: TAddedDemandFarmItem;
begin
  result := nil;
  for FormulaIndex := 0 to FAddedDemandValues.Count - 1 do
  begin
    AnItem := FAddedDemandValues[FormulaIndex];
    if AnItem.FarmGuid = FarmGUID then
    begin
      result := AnItem;
      Exit;
    end;
  end;
end;

function TAddedDemandItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TAddedDemandItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    result := AddedDemandValues.IsSame(TAddedDemandItem(AnotherItem).AddedDemandValues);
  end;
end;

procedure TAddedDemandItem.SetAddedDemandValues(
  const Value: TAddedDemandFarmCollection);
begin
  FAddedDemandValues.Assign(Value);
end;

function TAddedDemandFarmCollection.GetItem(
  Index: Integer): TAddedDemandFarmItem;
begin
  Assert(Index >= 0);
  while Index >= Count do
  begin
    Add;
  end;
  result := inherited Items[Index] as  TAddedDemandFarmItem
end;

procedure TAddedDemandFarmCollection.SetItem(Index: Integer;
  const Value: TAddedDemandFarmItem);
begin
  Assert(Index >= 0);
  while Index >= Count do
  begin
    Add;
  end;
  inherited Items[Index] := Value;
end;

{ TAddedDemandFarmItem }

procedure TAddedDemandFarmItem.Assign(Source: TPersistent);
begin
  if Source is TAddedDemandFarmItem then
  begin
    FarmGuid := TAddedDemandFarmItem(Source).FarmGuid;
  end;
  inherited;
end;

{ TLeachItem }

procedure TLeachItem.Assign(Source: TPersistent);
var
  LeachSource: TLeachItem;
begin
  if Source is TLeachItem then
  begin
    LeachSource := TLeachItem(Source);
    LeachChoice := LeachSource.LeachChoice;
    CustomFormula := LeachSource.CustomFormula;
  end;
  inherited;
end;

function TLeachItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherLeachItem: TLeachItem;
begin
  result := (AnotherItem is TLeachItem) and inherited;
  if result then
  begin
    OtherLeachItem := TLeachItem(AnotherItem);
    result := (LeachChoice = OtherLeachItem.LeachChoice)
      and (CustomFormula = OtherLeachItem.CustomFormula)
  end;
end;

procedure TLeachItem.SetCustomFormula(const Value: string);
begin
  SetCaseSensitiveStringProperty(FCustomFormula, Value);
end;

procedure TLeachItem.SetLeachChoice(const Value: TLeachChoice);
begin
  if FLeachChoice <> Value then
  begin
    FLeachChoice := Value;
    InvalidateModel;
  end;
end;

{ TLeachCollection }

class function TLeachCollection.ItemClass: TBoundaryItemClass;
begin
  result := TLeachItem;
end;

end.
