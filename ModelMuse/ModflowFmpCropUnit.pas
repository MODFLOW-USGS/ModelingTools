unit ModflowFmpCropUnit;

interface

uses
  ModflowBoundaryUnit, FormulaManagerUnit, Classes,
  OrderedCollectionUnit, SysUtils, GoPhastTypes, ModflowFmpFarmUnit,
  ModflowFmpBaseClasses, RealListUnit;

type
  // FMP Data Sets 11 and 26
  TRootingDepthRecord = record
    StartingTime: double;
    EndingTime: Double;
    // ROOT
    RootingDepth: double;
    RootingDepthAnnotation: string;
  end;

  TRootingDepthItem = class(TCustomZeroFarmItem)
  private
    const
    RootingDepthPosition = 0;
    function GetRootingDepth: string;
    procedure SetRootingDepth(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property RootingDepth: string read GetRootingDepth write SetRootingDepth;
  end;

  TFmpRootDepthCollection = class(TCustomFarmCollection)
  private
    FTimeValues: array of TRootingDepthRecord;
    function GetRootingDepthTimeValues(Index: integer): TRootingDepthRecord;
    procedure SetRootingDepthTimeValues(Index: integer;
      const Value: TRootingDepthRecord);
    function GetItems(Index: Integer): TRootingDepthItem;
    procedure SetItems(Index: Integer; const Value: TRootingDepthItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property RootingDepthTimeValues[Index: integer]: TRootingDepthRecord
      read GetRootingDepthTimeValues write SetRootingDepthTimeValues;
    function GetRootingDepthTimeValuesFromTime(StartTime: double): TRootingDepthRecord;
    property Items[Index: Integer]: TRootingDepthItem read GetItems
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

  // define crops and FMP Data sets 14 and 15.
  TCropItem = class(TCustomBoundaryItem)
  private
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
    var
    FCropName: string;
    FBaseTemperature: TFormulaObject;
    FBeginningRootDepth: TFormulaObject;
    FCoefficient0: TFormulaObject;
    FCoefficient1: TFormulaObject;
    FCoefficient2: TFormulaObject;
    FCoefficient3: TFormulaObject;
    FIrrigated: TFormulaObject;
    FMaximumCutoffTemperature: TFormulaObject;
    FMaximumRootDepth: TFormulaObject;
    FMinimumCutoffTemperature: TFormulaObject;
    FPSI1: TFormulaObject;
    FPSI2: TFormulaObject;
    FPSI3: TFormulaObject;
    FPSI4: TFormulaObject;
    FRootGrowthCoefficient: TFormulaObject;
    FFAllow: TFormulaObject;
    FCropFunctionCollection: TCropFunctionCollection;
    FFmpRootDepthCollection: TFmpRootDepthCollection;
    FEvapFractionsCollection: TEvapFractionsCollection;
    FLossesCollection: TLossesCollection;
    FCropWaterUseCollection: TCropWaterUseCollection;
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
    procedure SetCropCoefficientDataArrayName(const NewName: string);
    procedure SetAddedDemandDataArrayName(const NewName: string);
    procedure SetConsumptiveUseDataArrayName(const NewName: string);
    procedure SetEvaporationIrrigationDataArrayName(const NewName: string);
    procedure SetIrrigationDataArrayName(const NewName: string);
    procedure SetRootDepthDataArrayName(const NewName: string);
    procedure SetSWLossFractionIrrigationDataArrayName(const NewName: string);
    procedure SetSWLossFractionPrecipDataArrayName(const NewName: string);
    function GetBaseTemperature: string;
    function GetBeginningRootDepth: string;
    function GetCoefficient0: string;
    function GetCoefficient1: string;
    function GetCoefficient2: string;
    function GetCoefficient3: string;
    function GetIrrigated: string;
    function GetMaximumCutoffTemperature: string;
    function GetMaximumRootDepth: string;
    function GetMinimumCutoffTemperature: string;
    function GetPSI1: string;
    function GetPSI2: string;
    function GetPSI3: string;
    function GetPSI4: string;
    function GetRootGrowthCoefficient: string;
    procedure SetBaseTemperature(const Value: string);
    procedure SetBeginningRootDepth(const Value: string);
    procedure SetCoefficient0(const Value: string);
    procedure SetCoefficient1(const Value: string);
    procedure SetCoefficient2(const Value: string);
    procedure SetCoefficient3(const Value: string);
    procedure SetCropName(Value: string);
    procedure SetIrrigated(const Value: string);
    procedure SetMaximumCutoffTemperature(const Value: string);
    procedure SetMaximumRootDepth(const Value: string);
    procedure SetMinimumCutoffTemperature(const Value: string);
    procedure SetPSI1(const Value: string);
    procedure SetPSI2(const Value: string);
    procedure SetPSI3(const Value: string);
    procedure SetPSI4(const Value: string);
    procedure SetRootGrowthCoefficient(const Value: string);
    procedure SetCropFunctionCollection(const Value: TCropFunctionCollection);
    procedure SetCropWaterUseCollection(const Value: TCropWaterUseCollection);
    procedure SetEvapFractionsCollection(const Value: TEvapFractionsCollection);
    procedure SetFmpRootDepthCollection(const Value: TFmpRootDepthCollection);
    procedure SetLossesCollection(const Value: TLossesCollection);
    function GetFallow: string;
    procedure SetFallow(const Value: string);
    procedure UpdateAllDataArrays;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
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
    property PSI1: string read GetPSI1 write SetPSI1;
    property PSI2: string read GetPSI2 write SetPSI2;
    property PSI3: string read GetPSI3 write SetPSI3;
    property PSI4: string read GetPSI4 write SetPSI4;
    // BaseT
    property BaseTemperature: string read GetBaseTemperature
      write SetBaseTemperature;
    // MinCutT
    property MinimumCutoffTemperature: string read GetMinimumCutoffTemperature
      write SetMinimumCutoffTemperature;
    // MaxCutT
    property MaximumCutoffTemperature: string read GetMaximumCutoffTemperature
      write SetMaximumCutoffTemperature;
    // C0
    property Coefficient0: string read GetCoefficient0 write SetCoefficient0;
    // C1
    property Coefficient1: string read GetCoefficient1 write SetCoefficient1;
    // C2
    property Coefficient2: string read GetCoefficient2 write SetCoefficient2;
    // C3
    property Coefficient3: string read GetCoefficient3 write SetCoefficient3;
    // BegRootD
    property BeginningRootDepth: string read GetBeginningRootDepth
      write SetBeginningRootDepth;
    // MaxRootD
    property MaximumRootDepth: string read GetMaximumRootDepth
      write SetMaximumRootDepth;
    // RootGC
    property RootGrowthCoefficient: string read GetRootGrowthCoefficient
      write SetRootGrowthCoefficient;
    // inverse of NONIRR
    property Irrigated: string read GetIrrigated write SetIrrigated;
    // IFALLOW
    property Fallow: string read GetFallow write SetFallow;
    // // FMP Data Sets 11 and 26
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
    property EvaporationIrrigationDataArrayName: string
      read FEvaporationIrrigationDataArrayName write SetEvaporationIrrigationDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
    property SWLossFractionIrrigationDataArrayName: string
      read FSWLossFractionIrrigationDataArrayName write SetSWLossFractionIrrigationDataArrayName
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
    property AddedDemandDataArrayName: string
      read FAddedDemandDataArrayName write SetAddedDemandDataArrayName
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
    ;
  end;

//  StrConsumptiveUsePrefix = KConsumptiveUsePrefix;
//  StrIrrigationPrefix = KIrrigationPrefix;
//  StrRootDepthPrefix = KRootDepthPrefix;
//  StrEvaporationIrrigationPrefix = KEvaporationIrrigationPrefix;
//  StrSWLossFractionIrrigationPrefix = KSWLossFractionIrrigationPrefix;
//  StrSWLossFractionPrecipPrefix = KSWLossFractionPrecipPrefix;
//  StrAddedDemandPrefix = KAddedDemandPrefix;


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
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    property CropArray: TCropArray read FCropArray;
    property Items[Index: Integer]: TCropItem read GetItems write SetItems; default;
    procedure EvaluateCrops;
    procedure UpdateAllDataArrays;
  end;

implementation

uses
  ScreenObjectUnit, frmGoPhastUnit, frmErrorsAndWarningsUnit,
  RbwParser, frmFormulaErrorsUnit, PhastModelUnit, ModflowPackageSelectionUnit,
  GlobalVariablesUnit, LockedGlobalVariableChangers, UpdateDataArrayUnit;

const
  KCropCoefficientPrefix = 'Crop_Coefficient_';
  KConsumptiveUsePrefix = 'Consumptive_Use_';
  KIrrigationPrefix = 'Irrigatiion_';
  KRootDepthPrefix = 'Root_Depth_';
  KEvaporationIrrigationPrefix = 'Evaporation_Irrigation_';
  KSWLossFractionPrecipPrefix = 'SW_Loss_Fraction_Precip_';
  KSWLossFractionIrrigationPrefix = 'SW_Loss_Fraction_Irrigation_';
  KAddedDemandPrefix = 'Added_Demand_';

resourcestring
  StrCropCoefficientPrefix = KCropCoefficientPrefix;
  StrConsumptiveUsePrefix = KConsumptiveUsePrefix;
  StrIrrigationPrefix = KIrrigationPrefix;
  StrRootDepthPrefix = KRootDepthPrefix;
  StrEvaporationIrrigationPrefix = KEvaporationIrrigationPrefix;
  StrSWLossFractionIrrigationPrefix = KSWLossFractionIrrigationPrefix;
  StrSWLossFractionPrecipPrefix = KSWLossFractionPrecipPrefix;
  StrAddedDemandPrefix = KAddedDemandPrefix;

//const
//  RootingDepthPosition = 0;

//  TranspirationFractionPosition = 0;
//  PrecipFractionPosition = 1;
//  IrrigFractionPosition = 2;

//  PrecipitationLossesPosition = 0;
//  IrrigationLossesPosition = 1;

//  SlopePosition = 0;
//  InterceptPosition = 1;
//  PricePosition = 2;

//  CropValuePosition = 0;
//  WaterUseIrrigatedPosition = 1;

//  PSI1Position = 0;
//  PSI2Position = 1;
//  PSI3Position = 2;
//  PSI4Position = 3;
//  BaseTemperaturePosition = 4;
//  MinimumCutoffTemperaturePosition = 5;
//  MaximumCutoffTemperaturePosition = 6;
//  Coefficient0Position = 7;
//  Coefficient1Position = 8;
//  Coefficient2Position = 9;
//  Coefficient3Position = 10;
//  BeginningRootDepthPosition = 11;
//  MaximumRootDepthPosition = 12;
//  RootGrowthCoefficientPosition = 13;
//  IrrigatedPosition = 14;
//  FAllowPosition = 15;

resourcestring
  IDError = 'Time: %g.';
  StrAssignedUsingS = 'Assigned using %s';
  StrRootingDepthInThe = 'Rooting Depth in the Farm Process';
  StrIncompleteRootingD = 'Incomplete Rooting Depth data';

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
{ TRootingDepthItem }

function TRootingDepthItem.BoundaryFormulaCount: integer;
begin
  Result := 1;
end;

function TRootingDepthItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := RootingDepth;
    else Assert(False);
  end;
end;

function TRootingDepthItem.GetRootingDepth: string;
begin
  Result := FFormulaObjects[RootingDepthPosition].Formula;
  ResetItemObserver(RootingDepthPosition);
end;

procedure TRootingDepthItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    0:
      RootingDepth := Value;
    else Assert(False);
  end;
end;

procedure TRootingDepthItem.SetRootingDepth(const Value: string);
//var
//  PhastModel: TPhastModel;
//  ScreenObj: TObject;
begin
  if FFormulaObjects[RootingDepthPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, RootingDepthPosition, FFormulaObjects[RootingDepthPosition]);
//    PhastModel := Model as TPhastModel;
//    if (PhastModel <> nil)
//      and not (csDestroying in PhastModel.ComponentState)
//      and not PhastModel.Clearing then
//    begin
//      ScreenObj := ScreenObject;
//      if (ScreenObj <> nil)
//        and (ScreenObj as TScreenObject).CanInvalidateModel then
//      begin
//        Assert(False);
////        PhastModel.InvalidateMfSfrDepthExponent(self);
//      end;
//    end;
  end;
end;

{ TFmpRootDepthCollection }

procedure TFmpRootDepthCollection.EvaluateBoundaries;
var
  CurrentRecord: TRootingDepthRecord;
  CurrentItem: TRootingDepthItem;
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
    CurrentItem := Items[Index] as TRootingDepthItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;
    Expression := nil;
    Formula := CurrentItem.RootingDepth;
    CurrentRecord.RootingDepthAnnotation := Format(StrAssignedUsingS,
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
          StrRootingDepthInThe,
          Formula, E.Message);

        CurrentItem.RootingDepth := '0.';
        Formula := CurrentItem.RootingDepth;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.RootingDepth := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TFmpRootDepthCollection.GetItems(Index: Integer): TRootingDepthItem;
begin
  result := inherited Items[Index] as TRootingDepthItem;
end;

function TFmpRootDepthCollection.GetRootingDepthTimeValues(
  Index: integer): TRootingDepthRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TFmpRootDepthCollection.GetRootingDepthTimeValuesFromTime(
  StartTime: double): TRootingDepthRecord;
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
//  ErrorMessage := 'Object = ' + ScreenObjectName
//    + '; Time = ' + FloatToStr(StartTime);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteRootingD, ErrorMessage);
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

procedure TFmpRootDepthCollection.SetRootingDepthTimeValues(Index: integer;
  const Value: TRootingDepthRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
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
//  TranspirationFractionPosition = 0;
//  PrecipFractionPosition = 1;
//  IrrigFractionPosition = 2;
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
//  TranspirationFractionPosition = 0;
//  PrecipFractionPosition = 1;
//  IrrigFractionPosition = 2;
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
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := SourceItem.BoundaryFormula[Index];
    end;
    CropName := SourceItem.CropName;
    FmpRootDepthCollection := SourceItem.FmpRootDepthCollection;
    EvapFractionsCollection := SourceItem.EvapFractionsCollection;
    LossesCollection := SourceItem.LossesCollection;
    CropFunctionCollection := SourceItem.CropFunctionCollection;
    CropWaterUseCollection := SourceItem.CropWaterUseCollection;

    // This is done differently in TChemSpeciesItem
    // There the field is first assign then set to an empty string
    // and then the property is assigned.
    CropCoefficientDataArrayName := SourceItem.CropCoefficientDataArrayName;
    ConsumptiveUseDataArrayName := SourceItem.ConsumptiveUseDataArrayName;
    IrrigationDataArrayName := SourceItem.IrrigationDataArrayName;
    RootDepthDataArrayName := SourceItem.RootDepthDataArrayName;
    EvaporationIrrigationDataArrayName := SourceItem.EvaporationIrrigationDataArrayName;
    SWLossFractionIrrigationDataArrayName := SourceItem.SWLossFractionIrrigationDataArrayName;
    SWLossFractionPrecipDataArrayName := SourceItem.SWLossFractionPrecipDataArrayName;
    AddedDemandDataArrayName := SourceItem.AddedDemandDataArrayName;
  end;
  inherited;
end;

procedure TCropItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing
end;

function TCropItem.BoundaryFormulaCount: integer;
begin
  result := 16;
end;

constructor TCropItem.Create(Collection: TCollection);
begin
  inherited;
  CreateFormulaObjects;
  InitializeFormulas;

  FCropFunctionCollection := TCropFunctionCollection.Create(Model);
  FFmpRootDepthCollection := TFmpRootDepthCollection.Create(Model);
  FEvapFractionsCollection := TEvapFractionsCollection.Create(Model);
  FLossesCollection := TLossesCollection.Create(Model);
  FCropWaterUseCollection := TCropWaterUseCollection.Create(Model);

end;

procedure TCropItem.CreateFormulaObjects;
begin
  inherited;
  FPSI1 := CreateFormulaObject(dso3D);
  FPSI2 := CreateFormulaObject(dso3D);
  FPSI3 := CreateFormulaObject(dso3D);
  FPSI4 := CreateFormulaObject(dso3D);
  FBaseTemperature := CreateFormulaObject(dso3D);
  FMinimumCutoffTemperature := CreateFormulaObject(dso3D);
  FMaximumCutoffTemperature := CreateFormulaObject(dso3D);
  FCoefficient0 := CreateFormulaObject(dso3D);
  FCoefficient1 := CreateFormulaObject(dso3D);
  FCoefficient2 := CreateFormulaObject(dso3D);
  FCoefficient3 := CreateFormulaObject(dso3D);
  FBeginningRootDepth := CreateFormulaObject(dso3D);
  FMaximumRootDepth := CreateFormulaObject(dso3D);
  FRootGrowthCoefficient := CreateFormulaObject(dso3D);
  FIrrigated := CreateFormulaObject(dso3D);
  FFAllow := CreateFormulaObject(dso3D);
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

    if ([csLoading, csDestroying] * Model.ComponentState) = [] then
    begin
      Unlocker := TDefineGlobalIntegerObject.Create(Model,
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
//      LocalModel.GlobalVariables.Delete(Position);

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
      end;
    end;
  end;

  FCropFunctionCollection.Free;
  FFmpRootDepthCollection.Free;
  FEvapFractionsCollection.Free;
  FLossesCollection.Free;
  FCropWaterUseCollection.Free;

  InitializeFormulas;
  inherited;
end;

function TCropItem.GetBaseTemperature: string;
begin
  Result := FBaseTemperature.Formula;
  ResetItemObserver(BaseTemperaturePosition);
end;

function TCropItem.GetBeginningRootDepth: string;
begin
  Result := FBeginningRootDepth.Formula;
  ResetItemObserver(BeginningRootDepthPosition);
end;

function TCropItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    PSI1Position:
      Result := PSI1;
    PSI2Position:
      Result := PSI2;
    PSI3Position:
      Result := PSI3;
    PSI4Position:
      Result := PSI4;
    BaseTemperaturePosition:
      Result := BaseTemperature;
    MinimumCutoffTemperaturePosition:
      Result := MinimumCutoffTemperature;
    MaximumCutoffTemperaturePosition:
      Result := MaximumCutoffTemperature;
    Coefficient0Position:
      Result := Coefficient0;
    Coefficient1Position:
      Result := Coefficient1;
    Coefficient2Position:
      Result := Coefficient2;
    Coefficient3Position:
      Result := Coefficient3;
    BeginningRootDepthPosition:
      Result := BeginningRootDepth;
    MaximumRootDepthPosition:
      Result := MaximumRootDepth;
    RootGrowthCoefficientPosition:
      Result := RootGrowthCoefficient;
    IrrigatedPosition:
      Result := Irrigated;
    FAllowPosition:
      Result := FAllow;
    else
      Assert(False);
  end;
end;

function TCropItem.GetCoefficient0: string;
begin
  Result := FCoefficient0.Formula;
  ResetItemObserver(Coefficient0Position);
end;

function TCropItem.GetCoefficient1: string;
begin
  Result := FCoefficient1.Formula;
  ResetItemObserver(Coefficient1Position);
end;

function TCropItem.GetCoefficient2: string;
begin
  Result := FCoefficient2.Formula;
  ResetItemObserver(Coefficient2Position);
end;

function TCropItem.GetCoefficient3: string;
begin
  Result := FCoefficient3.Formula;
  ResetItemObserver(Coefficient3Position);
end;

function TCropItem.GetFallow: string;
begin
  Result := FFallow.Formula;
  ResetItemObserver(FAllowPosition);
end;

function TCropItem.GetIrrigated: string;
begin
  Result := FIrrigated.Formula;
  ResetItemObserver(IrrigatedPosition);
end;

function TCropItem.GetMaximumCutoffTemperature: string;
begin
  Result := FMaximumCutoffTemperature.Formula;
  ResetItemObserver(MaximumCutoffTemperaturePosition);
end;

function TCropItem.GetMaximumRootDepth: string;
begin
  Result := FMaximumRootDepth.Formula;
  ResetItemObserver(MaximumRootDepthPosition);
end;

function TCropItem.GetMinimumCutoffTemperature: string;
begin
  Result := FMinimumCutoffTemperature.Formula;
  ResetItemObserver(MinimumCutoffTemperaturePosition);
end;

procedure TCropItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPSI1 then
  begin
    List.Add(FObserverList[PSI1Position]);
  end
  else if Sender = FPSI2 then
  begin
    List.Add(FObserverList[PSI2Position]);
  end
  else if Sender = FPSI3 then
  begin
    List.Add(FObserverList[PSI3Position]);
  end
  else if Sender = FPSI4 then
  begin
    List.Add(FObserverList[PSI4Position]);
  end
  else if Sender = FBaseTemperature then
  begin
    List.Add(FObserverList[BaseTemperaturePosition]);
  end
  else if Sender = FMinimumCutoffTemperature then
  begin
    List.Add(FObserverList[MinimumCutoffTemperaturePosition]);
  end
  else if Sender = FMaximumCutoffTemperature then
  begin
    List.Add(FObserverList[MaximumCutoffTemperaturePosition]);
  end
  else if Sender = FCoefficient0 then
  begin
    List.Add(FObserverList[Coefficient0Position]);
  end
  else if Sender = FCoefficient1 then
  begin
    List.Add(FObserverList[Coefficient1Position]);
  end
  else if Sender = FCoefficient2 then
  begin
    List.Add(FObserverList[Coefficient2Position]);
  end
  else if Sender = FCoefficient3 then
  begin
    List.Add(FObserverList[Coefficient3Position]);
  end
  else if Sender = FBeginningRootDepth then
  begin
    List.Add(FObserverList[BeginningRootDepthPosition]);
  end
  else if Sender = FMaximumRootDepth then
  begin
    List.Add(FObserverList[MaximumRootDepthPosition]);
  end
  else if Sender = FRootGrowthCoefficient then
  begin
    List.Add(FObserverList[RootGrowthCoefficientPosition]);
  end
  else if Sender = FIrrigated then
  begin
    List.Add(FObserverList[IrrigatedPosition]);
  end
  else if Sender = FFAllow then
  begin
    List.Add(FObserverList[FAllowPosition]);
  end

//  BeginningRootDepthPosition = 11;
//  MaximumRootDepthPosition = 12;
//  RootGrowthCoefficientPosition = 13;
//  IrrigatedPosition = 14;

end;

function TCropItem.GetPSI1: string;
begin
  Result := FPSI1.Formula;
  ResetItemObserver(PSI1Position);
end;

function TCropItem.GetPSI2: string;
begin
  Result := FPSI2.Formula;
  ResetItemObserver(PSI2Position);
end;

function TCropItem.GetPSI3: string;
begin
  Result := FPSI3.Formula;
  ResetItemObserver(PSI3Position);
end;

function TCropItem.GetPSI4: string;
begin
  Result := FPSI4.Formula;
  ResetItemObserver(PSI4Position);
end;

function TCropItem.GetRootGrowthCoefficient: string;
begin
  Result := FRootGrowthCoefficient.Formula;
  ResetItemObserver(RootGrowthCoefficientPosition);
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
    for PropIndex := 0 to BoundaryFormulaCount - 1 do
    begin
      Result := BoundaryFormula[PropIndex] = OtherItem.BoundaryFormula[PropIndex];
      if not result then
      begin
        break;
      end;
    end;
  end;
  if result then
  begin
//    OtherItem := TCropItem(AnotherItem);
    result := (CropName = OtherItem.CropName)
      and FmpRootDepthCollection.IsSame(OtherItem.FmpRootDepthCollection)
      and EvapFractionsCollection.IsSame(OtherItem.EvapFractionsCollection)
      and LossesCollection.IsSame(OtherItem.LossesCollection)
      and CropFunctionCollection.IsSame(OtherItem.CropFunctionCollection)
      and CropWaterUseCollection.IsSame(OtherItem.CropWaterUseCollection)

      and (CropCoefficientDataArrayName = OtherItem.CropCoefficientDataArrayName)
      and (ConsumptiveUseDataArrayName = OtherItem.ConsumptiveUseDataArrayName)
      and (IrrigationDataArrayName = OtherItem.IrrigationDataArrayName)
      and (RootDepthDataArrayName = OtherItem.RootDepthDataArrayName)
      and (EvaporationIrrigationDataArrayName = OtherItem.EvaporationIrrigationDataArrayName)
      and (SWLossFractionIrrigationDataArrayName = OtherItem.SWLossFractionIrrigationDataArrayName)
      and (SWLossFractionPrecipDataArrayName = OtherItem.SWLossFractionPrecipDataArrayName)
      and (AddedDemandDataArrayName = OtherItem.AddedDemandDataArrayName)
  end;
end;

procedure TCropItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPSI1,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FPSI1,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FPSI3,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FPSI4,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FBaseTemperature,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FMinimumCutoffTemperature,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FMaximumCutoffTemperature,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCoefficient0,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCoefficient1,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCoefficient2,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCoefficient3,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FBeginningRootDepth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FMaximumRootDepth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FRootGrowthCoefficient,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FIrrigated,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FFallow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
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
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FAddedDemandDataArrayName, NewName);
end;

procedure TCropItem.SetBaseTemperature(const Value: string);
begin
  if FBaseTemperature.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, BaseTemperaturePosition, FBaseTemperature);
  end;
end;

procedure TCropItem.SetBeginningRootDepth(const Value: string);
begin
  if FBeginningRootDepth.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, BeginningRootDepthPosition, FBeginningRootDepth);
  end;
end;

procedure TCropItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    PSI1Position:
      PSI1 := Value;
    PSI2Position:
      PSI2 := Value;
    PSI3Position:
      PSI3 := Value;
    PSI4Position:
      PSI4 := Value;
    BaseTemperaturePosition:
      BaseTemperature := Value;
    MinimumCutoffTemperaturePosition:
      MinimumCutoffTemperature := Value;
    MaximumCutoffTemperaturePosition:
      MaximumCutoffTemperature := Value;
    Coefficient0Position:
      Coefficient0 := Value;
    Coefficient1Position:
      Coefficient1 := Value;
    Coefficient2Position:
      Coefficient2 := Value;
    Coefficient3Position:
      Coefficient3 := Value;
    BeginningRootDepthPosition:
      BeginningRootDepth := Value;
    MaximumRootDepthPosition:
      MaximumRootDepth := Value;
    RootGrowthCoefficientPosition:
      RootGrowthCoefficient := Value;
    IrrigatedPosition:
      Irrigated := Value;
    FAllowPosition:
      FAllow := Value;
    else
      Assert(False);
  end;
end;

procedure TCropItem.SetCoefficient0(const Value: string);
begin
  if FCoefficient0.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Coefficient0Position, FCoefficient0);
  end;
end;

procedure TCropItem.SetCoefficient1(const Value: string);
begin
  if FCoefficient1.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Coefficient1Position, FCoefficient1);
  end;
end;

procedure TCropItem.SetCoefficient2(const Value: string);
begin
  if FCoefficient2.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Coefficient2Position, FCoefficient2);
  end;
end;

procedure TCropItem.SetCoefficient3(const Value: string);
begin
  if FCoefficient3.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Coefficient3Position, FCoefficient3);
  end;
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
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FConsumptiveUseDataArrayName, NewName);
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
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FCropCoefficientDataArrayName, NewName);
end;

procedure TCropItem.SetCropFunctionCollection(
  const Value: TCropFunctionCollection);
begin
  FCropFunctionCollection.Assign(Value);
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
    and not (csReading in Model.ComponentState) then
  begin
    Value := GenerateNewName(Value, nil, '_');
  end;
  ChangeGlobals := TDefineGlobalIntegerObject.Create(Model, FCropName, Value,
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
      end;

      if SameText(FCropName, Value) then
      begin
        OldRoot := GenerateNewRoot(FCropName);
        NewRoot := GenerateNewRoot(Value);

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
      end
      else
      begin

        FCropCoefficientDisplayName := GenerateNewRoot(StrCropCoefficientPrefix + Value);
        CropCoefficientDataArrayName := GenerateNewRoot(KCropCoefficientPrefix + Value);

        FConsumptiveUseDisplayName := GenerateNewRoot(StrConsumptiveUsePrefix + Value);
        ConsumptiveUseDataArrayName := GenerateNewRoot(KConsumptiveUsePrefix + Value);

        FIrrigationDisplayName := GenerateNewRoot(StrIrrigationPrefix + Value);
        IrrigationDataArrayName := GenerateNewRoot(KIrrigationPrefix + Value);

        FRootDepthDisplayName := GenerateNewRoot(StrRootDepthPrefix + Value);
        RootDepthDataArrayName := GenerateNewRoot(KRootDepthPrefix + Value);

        FEvaporationIrrigationDisplayName := GenerateNewRoot(StrEvaporationIrrigationPrefix + Value);
        EvaporationIrrigationDataArrayName := GenerateNewRoot(KEvaporationIrrigationPrefix + Value);

        FSWLossFractionPrecipDisplayName := GenerateNewRoot(StrSWLossFractionPrecipPrefix + Value);
        SWLossFractionPrecipDataArrayName := GenerateNewRoot(KSWLossFractionPrecipPrefix + Value);

        FSWLossFractionIrrigationDisplayName := GenerateNewRoot(StrSWLossFractionIrrigationPrefix + Value);
        SWLossFractionIrrigationDataArrayName := GenerateNewRoot(KSWLossFractionIrrigationPrefix + Value);

        FAddedDemandDisplayName := GenerateNewRoot(StrAddedDemandPrefix + Value);
        AddedDemandDataArrayName := GenerateNewRoot(KAddedDemandPrefix + Value);
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
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FEvaporationIrrigationDataArrayName, NewName);
end;

procedure TCropItem.SetFallow(const Value: string);
begin
  if FFallow.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, FAllowPosition, FFallow);
  end;
end;

procedure TCropItem.SetFmpRootDepthCollection(
  const Value: TFmpRootDepthCollection);
begin
  FFmpRootDepthCollection.Assign(Value);
end;

procedure TCropItem.SetIndex(Value: Integer);
var
  ChangeGlobals: TDefineGlobalIntegerObject;
begin
  if {(Index <> Value) and} (Model <> nil) and (FCropName <> '') then
  begin
    ChangeGlobals := TDefineGlobalIntegerObject.Create(Model, FCropName, FCropName,
      StrCropVariable);
    try
      ChangeGlobals.SetValue(Value+1);
    finally
      ChangeGlobals.Free;
    end;
  end;
  inherited;

end;

procedure TCropItem.SetIrrigated(const Value: string);
begin
  if FIrrigated.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, IrrigatedPosition, FIrrigated);
  end;
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
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FIrrigationDataArrayName, NewName);
end;

procedure TCropItem.SetLossesCollection(const Value: TLossesCollection);
begin
  FLossesCollection.Assign(Value);
end;

procedure TCropItem.SetMaximumCutoffTemperature(const Value: string);
begin
  if FMaximumCutoffTemperature.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, MaximumCutoffTemperaturePosition, FMaximumCutoffTemperature);
  end;
end;

procedure TCropItem.SetMaximumRootDepth(const Value: string);
begin
  if FMaximumRootDepth.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, MaximumRootDepthPosition, FMaximumRootDepth);
  end;
end;

procedure TCropItem.SetMinimumCutoffTemperature(const Value: string);
begin
  if FMinimumCutoffTemperature.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, MinimumCutoffTemperaturePosition, FMinimumCutoffTemperature);
  end;
end;

procedure TCropItem.SetPSI1(const Value: string);
begin
  if FPSI1.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PSI1Position, FPSI1);
  end;
end;

procedure TCropItem.SetPSI2(const Value: string);
begin
  if FPSI2.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PSI2Position, FPSI2);
  end;
end;

procedure TCropItem.SetPSI3(const Value: string);
begin
  if FPSI3.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PSI3Position, FPSI3);
  end;
end;

procedure TCropItem.SetPSI4(const Value: string);
begin
  if FPSI4.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PSI4Position, FPSI4);
  end;
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
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FRootDepthDataArrayName, NewName);
end;

procedure TCropItem.SetRootGrowthCoefficient(const Value: string);
begin
  if FRootGrowthCoefficient.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, RootGrowthCoefficientPosition, FRootGrowthCoefficient);
  end;
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
    UpdateDat.OnDataSetUsed := LocalModel.MultipleSWLossFractionPrecipUsed;
    UpdateDat.OldDataArrayName := FSWLossFractionPrecipDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FSWLossFractionPrecipDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: SURFACEWATER_LOSS_FRACTION_IRRIGATION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FSWLossFractionPrecipDataArrayName, NewName);
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
    UpdateDat.OnDataSetUsed := LocalModel.MultipleSWLossFractionIrrigationUsed;
    UpdateDat.OldDataArrayName := FSWLossFractionIrrigationDataArrayName;
    UpdateDat.NewName := NewName;
    UpdateDat.NewDisplayName := FSWLossFractionIrrigationDisplayName;
    UpdateDat.NewFormula := '0';
    UpdateDat.AssociatedDataSets := 'MODFLOW-OWHM FMP: SURFACEWATER_LOSS_FRACTION_PRECIPITATION';
  {$IFDEF OWHMV2}
    UpdateDat.ShouldCreate := UpdateDat.OnDataSetUsed(nil);
  {$ELSE}
    UpdateDat.ShouldCreate := False;
  {$ENDIF}
    UpdateDat.Classification := StrFmp2Classifiation;
    UpdateDat.Orientation := dsoTop;
    UpdateOrCreateDataArray(UpdateDat);

  end;

  SetCaseSensitiveStringProperty(FSWLossFractionIrrigationDataArrayName, NewName);
end;

procedure TCropItem.UpdateAllDataArrays;
begin
  if (Collection as TCropCollection).Model <> nil then
  begin
    // Reassigning the name will cause the data set to be created if it is
    // needed.
    CropCoefficientDataArrayName := CropCoefficientDataArrayName;
    ConsumptiveUseDataArrayName := ConsumptiveUseDataArrayName;
    IrrigationDataArrayName := IrrigationDataArrayName;
    RootDepthDataArrayName := RootDepthDataArrayName;
    EvaporationIrrigationDataArrayName := EvaporationIrrigationDataArrayName;
    SWLossFractionPrecipDataArrayName := SWLossFractionPrecipDataArrayName;
    SWLossFractionIrrigationDataArrayName := SWLossFractionIrrigationDataArrayName;
    AddedDemandDataArrayName := AddedDemandDataArrayName;
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
end;

{ TCropCollection }


procedure TCropCollection.Assign(Source: TPersistent);
begin
  FreeAndNil(FFarmList);
  inherited;
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
begin
  PhastModel := Model as TPhastModel;
  FarmProcess := PhastModel.ModflowPackages.FarmProcess;
  SetLength(FCropArray, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index];
    CurrentRecord.CropID := Index+1;

    if FarmProcess.CropConsumptiveConcept = cccConcept1 then
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

    if FarmProcess.RootingDepth = rdSpecified then
    begin
      CurrentItem.FmpRootDepthCollection.EvaluateBoundaries;
    end;

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

procedure TCropCollection.UpdateAllDataArrays;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].UpdateAllDataArrays;
  end;
end;

end.
