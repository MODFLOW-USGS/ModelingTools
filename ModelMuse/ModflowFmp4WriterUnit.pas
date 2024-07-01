{@name writes the FMP input file for MODFLOW-OWHM version 2.}
unit ModflowFmp4WriterUnit;

interface

uses
  System.Classes, System.Contnrs, Vcl.Forms, System.SysUtils,
  CustomModflowWriterUnit, PhastModelUnit, ModflowFmpWellUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, ScreenObjectUnit, ModflowBoundaryUnit, RbwParser,
  DataSetUnit, OrderedCollectionUnit, GoPhastTypes, ModflowBoundaryDisplayUnit,
  ModflowFmpSoilUnit, ModflowFmpBaseClasses, ModflowFmpCropUnit;

type
  TWriteLocation = (wlMain, wlOpenClose, wlOFE, wlCID, wlFID, wlRoot, wlCropUse, wlETR,
    wlEtFrac, wlSwLosses, wlPFLX, wlCropFunc, wlWaterCost, wlDeliveries,
    wlSemiRouteDelivery, wlSemiRouteReturn, wlCall, wlEfficiency,
    wlEfficiencyImprovement, wlBareRunoffFraction,
    wlBarePrecipitationConsumptionFraction, wlCapillaryFringe, wlSoilID,
    wlSurfaceK, wlBareEvap, wlDirectRecharge, wlPrecipPotConsumption,
    wlNrdInfilLoc, wlLandUseAreaFraction, wlCropCoefficient, wlConsumptiveUse,
    wlIrrigation, wlRootDepth, wlGwRootInteraction, wlTranspirationFraction,
    wlEvaporationIrrigationFraction, wlFractionOfPrecipToSurfaceWater,
    wlFractionOfIrrigToSurfaceWater, wlAddedDemand, wlCropHasSalinityDemand,
    wlAddedDemandRunoffSplit, wlIrrigationUniformity, wlDeficiencyScenario,
    wlWaterSource, wlAddedCropDemandFlux, wlAddedCropDemandRate,
    wlPrecipicationTable, wlSoilCoefficient, wlNoReturnFlow,
    wlSemiRouteDeliveryLowerLimit, wlSemiRouteDeliveryUpperLimit, wlSwAllotment,
    wlGwAllotment, wlRootPressure, wlPondDepth, wlNoCropMeansBareSoil,
    wlNoEvapErrCorrection, wlSaltSupplyConcentration, wlCropSalinityTolerance,
    wlCropMaxLeachRequirement, wlCropLeachRequirement, wlSalinityAppliedWater,
    wlSupplyWells, wlCropNames, wlSpecifyPrintCrops);

  TWriteTransientData = procedure (WriteLocation: TWriteLocation) of object;

  TCheckDataSet = procedure (IntegerArray: TDataArray;
    const ErrorMessage: string) of object;

  TTransientDataUsed = function (Sender: TObject): Boolean of object;

  TGetCropCollection = function (Crop: TCropItem): TOwhmCollection of object;


  TEvaluateProcedure = procedure of object;

  TUpdateRequirements = record
    EvaluateProcedure: TEvaluateProcedure;
    TransientDataUsed: TTransientDataUsed;
    WriteLocation: TWriteLocation;
    TimeLists: TModflowBoundListOfTimeLists;
  end;

  TRequiredValues = record
    WriteLocation: TWriteLocation;
    Comment: string;
    DataTypeIndex: Integer;
    DataType: TRbwDataType;
    DefaultValue: Double;
    ErrorID: string;
    ID: string;
    StaticDataName: string;
    WriteTransientData: Boolean;
    CheckProcedure: TCheckDataSet;
    CheckError: string;
    Option: string;
    FarmProperty: TFarmProperty;
    LandUseStaticFileNames: TStringList;
    MaxDataTypeIndex: Integer;
  end;

  TPestValues = record
    Formula: string;
    PestName: string;
    PestSeriesName: string;
    PestSeriesMethod: TPestParamMethod;
    FormulaErrorMessage: string;
    ErrorObjectName: string;
    ParameterUsed: Boolean;
  end;

  TModflowFmp4Writer = class(TCustomListWriter)
  private
    FFarmProcess4: TFarmProcess4;
    FClimatePackage: TFarmProcess4Climate;
    FLandUse: TFarmProcess4LandUse;
    FSurfaceWater4: TFarmProcess4SurfaceWater;
    FSalinityFlush: TFarmProcess4SalinityFlush;
    FSoil4: TFarmProcess4Soil;
    FAllotments: TFarmProcess4Allotments;
    FFarmWells4: TFarmProcess4Wells;

    FACtiveSurfaceCells: array of array of boolean;
    FWriteLocation: TWriteLocation;

    FFarmIDs: TList;
    FRefEts: TList;
    FPrecip: TList;
    FCropIDs: TList;
    FEfficiencies: TList;
    FEfficiencyImprovements: TList;
    FBareRunoffFractions: TList;
    FBarePrecipitationConsumptionFractions: TList;
    FEvapBare: TList;
    FDirectRecharge: TList;
    FPrecipPotConsumption: TList;
    FNrdInfilLocation: TList;
    FLandUseAreaFraction: TList;
    FCropCoefficient: TList;
    FConsumptiveUse: TList;
    FIrrigation: TList;
    FRootDepth: TList;
    FTranspirationFraction: TList;
    FEvaporationIrrigationFraction: TList;
    FFractionOfPrecipToSurfaceWater: TList;
    FFractionOfIrrigToSurfaceWater: TList;
    FAddedDemand: TList;
    FCropHasSalinityDemand: TList;
    FAddedDemandRunoffSplit: TList;

    FFarmWellID: Integer;
    FBaseName: string;

    FFID_FileStream: TFileStream;
    FOpenCloseFileStream: TFileStream;
    FPFLX_FileStream: TFileStream;
    FETR_FileStream: TFileStream;
    FCID_FileStream: TFileStream;
    FEFFICIENCY_FileStream: TFileStream;
    FEFFICIENCY_IMPROVEMENT_FileStream: TFileStream;
    FBARE_RUNOFF_FRACTION_FileStream: TFileStream;
    FBarePrecipitationConsumptionFractionFileStream: TFileStream;
    FCapillaryFringeFileStream: TFileStream;
    FSoilIdStream: TFileStream;
    FSurfaceKFileStream: TFileStream;
    FEvapBareFileStream: TFileStream;
    FDirectRechargeFileStream: TFileStream;
    FPrecipPotConsumptionFileStream: TFileStream;
    FNrdInfilLocationFileStream: TFileStream;
    FCropcoefficientFileStream: TFileStream;
    FLandUseAreaFractionFileStream: TFileStream;
    FConsumptiveUseFileStream: TFileStream;
    FIrrigationFileStream: TFileStream;
    FRootDepthFileStream: TFileStream;
    FGwRootInteractionStream: TFileStream;
    FTranspirationFractionFileStream: TFileStream;
    FEvaporationIrrigationFractionFileStream: TFileStream;
    FFractionOfPrecipToSurfaceWaterFileStream: TFileStream;
    FFractionOfIrrigToSurfaceWaterFileStream: TFileStream;
    FAddedDemandFileStream: TFileStream;
    FCropHasSalinityDemandFileStream: TFileStream;
    FAddedDemandRunoffSplitFileStream: TFileStream;
    FNWBS: Integer;
    FIrrigationUniformityFileStream: TFileStream;
    FDeficiencyScenarioFileStream: TFileStream;
    FWaterSourceFileStream: TFileStream;
    FAddedCropDemandFluxFileStream: TFileStream;
    FAddedCropDemandRateFileStream: TFileStream;
    FFmpSoils: TSoilCollection;
    FEffectivPrecipitationTableFileStream: TFileStream;
    FEffectivCoefficientTableFileStream: TFileStream;
    FNonRoutedDeliveryFileStream: TFileStream;
    FNrdTypes: Integer;
    FNoReturnFlowFileStream: TFileStream;
    FSemiRoutedDeliveryFileStream: TFileStream;
    FSemiRoutedDeliveryLowerLimitFileStream: TFileStream;
    FSemiRoutedDeliveryUpperLimitFileStream: TFileStream;
    FSemiRoutedReturnFileStream: TFileStream;
    FSurfaceWaterAllotmentFileStream: TFileStream;
    FGroundWaterAllotmentFileStream: TFileStream;
    FRootPressureFileStream: TFileStream;
    FPondDepthFileStream: TFileStream;
    FConvertToBareFileStream: TFileStream;
    FSumOneCorrectionFileStream: TFileStream;
    FSaltSupplyConcentrationFileStream: TFileStream;
    FCropSalinityToleranceFileStream: TFileStream;
    FCropMaxLeachingRequirementFileStream: TFileStream;
    FCropLeachingRequirementFileStream: TFileStream;
    FCropSalinityAppliedWaterFileStream: TFileStream;
    FSupplyWellFileStream: TFileStream;
    FCropNamesFileStream: TFileStream;
    FCropPrintFileStream: TFileStream;
    procedure WriteGobalDimension;
    procedure WriteOutput;
    procedure WriteOptions;
    procedure WriteWaterBalanceSubregion;
    procedure WriteSoil;
    procedure WriteClimate;
    procedure WriteSurfaceWater;
    procedure WriteSupplyWell;
    procedure WriteAllotments;
    procedure WriteLandUse;
    procedure WriteSalinityFlush;
    procedure WriteSurfaceWaterIrrigation;
    procedure WriteFileInternal;
    procedure EvaluateAll;

    // WBS
    // WBS_NAME
    procedure WriteFarmNames;

    // LOCATION
    procedure EvaluateFarmID;
    procedure WriteFarmLocation;

    // EFFICIENCY
    procedure EvaluateEfficiency;
    procedure WriteEfficiency;

    // EFFICIENCY_IMPROVEMENT
    procedure EvaluateEfficiencyImprovement;
    procedure WriteEfficiencyImprovement;

    // DEFFICIENCY_SCENARIO
    procedure WriteDeficiencyScenario;

    // PRORATE_DEFICIENCY
    procedure WriteProrateDeficiency;

    // WATERSOURCE
    procedure WriteWaterSource;

    // BARE_RUNOFF_FRACTION
    procedure EvaluateBareRunoffFraction;
    procedure WriteBareRunoffFraction;

    // BARE_PRECIPITATON_CONSUMPTION_FRACTION
    procedure EvaluateBarePrecipitationConsumptionFraction;
    procedure WriteBarePrecipitationConsumptionFraction;

    // ADDED_DEMAND_RUNOFF_SPLIT
    procedure EvaluateAddedDemandRunoffSplit;
    procedure WriteAddedDemandRunoffSplit;


    // ADDED_CROP_DEMAND FLUX
    procedure WriteAddedCropDemandFlux;

    // ADDED_CROP_DEMAND Rate
    procedure WriteAddedCropDemandRate;

    // soil
    procedure WriteCapillaryFringe;
    procedure WriteSoilID;
    procedure WriteSoilCoefficient;
    procedure WriteSurfaceK;
    procedure WriteEffectivePrecipitationTable;

    // climate
    procedure EvaluateReferenceET;
    procedure WriteRefET;

    procedure EvaluatePrecip;
    procedure WritePrecipitation;

    procedure EvaluateBareEvap;
    procedure WriteBareEvap;

    procedure EvaluateDirectRecharge;
    procedure WriteDirectRecharge;

    procedure EvaluatePrecipPotConsumption;
    procedure WritePrecipPotConsumption;

    // Surface water
    // NON_ROUTED_DELIVERY
    procedure WriteNonRoutedDelivery;

    // NRD_INFILTRATION_LOCATION
    procedure EvaluateNrdInfilLocation;
    procedure WriteNrdInfilLocation;
    procedure PrintSurfaceWaterOutputOptions;

    procedure WriteSemiRoutedDelivery;
    procedure WriteSemiRoutedDeliveryLowerLimit;
    procedure WriteSemiRoutedDeliveryUpperLimit;
    procedure WriteSemiRoutedDeliveryClosureTolerance;

    procedure WriteNoReturnFlow;
    procedure WriteSemiRoutedReturn;
    procedure WriteRoutedReturn;
    procedure WriteRebuildFullyRoutedReturn;

    // ALLOTMENT
    // SURFACE_WATER
    procedure WriteSurfaceWaterAllotment;
    // GROUNDWATER
    procedure WriteGroundWaterAllotment;

    // Land use
    procedure WriteLandUseOption;
    // LOCATION
    procedure EvaluateCropID;
    procedure WriteLandUseLocation;

    // LAND_USE_AREA_FRACTION
    procedure EvaluateLandUseAreaFraction;
    function GetLandUseFractionCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteLandUseAreaFraction;

    procedure WriteLandUsePrintOptions;

    // SPECIFY_PRINT_ALL_CROPS
    procedure WriteSpecifyPrintAllCrops;
    // CROP_NAME
    procedure WriteCropName;

    // CROP_COEFFICIENT
    procedure EvaluateCropCoefficient;
    function GetCropCoefficentCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteCropCoefficient;

    // CONSUMPTIVE_USE
    procedure EvaluateConsumptiveUse;
    function GetConsumptiveUseCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteConsumptiveUse;  // finish list

    // IRRIGATION
    procedure EvaluateIrrigation;
    procedure WriteIrrigation;

    // ROOT_DEPTH
    procedure EvaluateRootDepth;
    function GetRootDepthCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteRootDepth; // finish list

//    ROOT_PRESSURE
    procedure WriteRootPressure;

    // GROUNDWATER_ROOT_INTERACTION
    procedure WriteGroundwaterRootInteraction;

    // TRANSPIRATION_FRACTION
    procedure EvaluateTranspirationFraction;
    function GetTranspirationFractionCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteTranspirationFraction;

    // EVAPORATION_IRRIGATION_FRACTION
    procedure EvaluateEvaporationIrrigationFraction;
    procedure WriteEvaporationIrrigationFraction;

    // SURFACEWATER_LOSS_FRACTION_PRECIPITATION
    procedure EvaluateFractionOfPrecipToSurfaceWater;
    function GetSWLossFracPrecipCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteFractionOfPrecipToSurfaceWater;

    // SURFACEWATER_LOSS_FRACTION_IRRIGATION
    procedure EvaluateFractionOfIrrigToSurfaceWater;
    procedure WriteFractionOfIrrigToSurfaceWater;

    // POND_DEPTH
    function GetPondDepthCollection(Crop: TCropItem): TOwhmCollection;
    procedure WritePondDepth;

    // ADDED_DEMAND
    procedure EvaluateAddedDemand;
    procedure WriteAddedDemand;

    // ZERO_CONSUMPTIVE_USE_BECOMES_BARE_SOIL
    function GetZeroConsumptiveUseBecomesBareSoilCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteZeroConsumptiveUseBecomesBareSoil;

    // EVAPORATION_IRRIGATION_FRACTION_SUM_ONE_CORRECTION
    function GetEvapIrrigateFracSumOneCorrectionCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteEvapIrrigateFracSumOneCorrection; // finish

    //  CROPS_THAT_SPECIFY_SURFACE_ELEVATION
    procedure WriteCropsThatSpecifySurfaceElevation; // should this be done?
    //  CROP_SURFACE_ELEVATION
    procedure WriteCropSurfaceElevation; // should this be done?
    //  CROP_SURFACE_ELEVATION_OFFSET
    procedure WriteSurfaceElevationOffset; // should this be done?


    // Salinity flush
    procedure WriteSalinityFlushPrintOptions;
    procedure WriteExpressionLineLength;
    procedure WriteExpressionVariableNearZero;
    procedure WriteSaltSupplyConcentration;
    procedure WriteIrrigationUniformity;

    // CROP_SALINITY_TOLERANCE
    function GetSalinityToleranceCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteCropSalinityTolerance;
    // CROP_MAX_LEACHING_REQUIREMENT
    function GetMaxLeachingRequirementCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteCropMaxLeachingRequirement;

    // CROP_LEACHING_REQUIREMENT
    function GetLeachingRequirementCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteCropLeachingRequirement;

    //CROP_SALINITY_APPLIED_WATER
    function GetSalinityAppliedWaterCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteSalinityAppliedWater;

    // SUPPLY_WELL
    procedure WriteMnwPumpSpread;
    procedure WriteSupplyWells;

    procedure EvaluateCropHasSalinityDemand;
    function GetCropHasSalinityDemandCollection(Crop: TCropItem): TOwhmCollection;
    procedure WriteCropHasSalinityDemand;

    procedure FreeFileStreams;
    // wbs location
//    procedure WriteDataSet26(TimeIndex: Integer);
    procedure CheckDataSetZeroOrPositive(IntegerArray: TDataArray;
      const ErrorMessage: string);
    procedure CheckDataSetZeroOrGETen(IntegerArray: TDataArray;
      const ErrorMessage: string);
    procedure CheckDataSetBetweenZeroAndOne(RealArray: TDataArray;
      const ErrorMessage: string);
    procedure CheckDataSetBetweenZeroAndFive(IntegerArray: TDataArray;
      const ErrorMessage: string);
    procedure EvaluateActiveCells;
    procedure RemoveErrorAndWarningMessages;
    procedure WriteTransientFmpArrayData(RequiredValues: TRequiredValues);
    function GetTransientList(WriteLocation: TWriteLocation): TList;
    function GetFileStreamName(WriteLocation: TWriteLocation): string;
    function GetFmpBoundary(ScreenObject: TScreenObject;
      WriteLocation: TWriteLocation): TModflowBoundary;
    procedure EvaluateTransientArrayData(WriteLocation: TWriteLocation);
//    procedure EvaluateMultTransientArrayData(WriteLocation: TWriteLocation);
    procedure WriteFmpArrayData(AFileName: string; RequiredValues: TRequiredValues);
    procedure WriteLandUseArrayData(AFileName: string; RequiredValues: TRequiredValues);
    procedure UpdateDisplay(UpdateRequirements: TUpdateRequirements);
    function TransientCropUsed(Sender: TObject): Boolean;
    function TransientRefEtUsed(Sender: TObject): Boolean;
    procedure GetScaleFactorsAndExternalFile(RequiredValues: TRequiredValues;
      var UnitConversionScaleFactor: string; var ExternalFileName: string;
      var ExternalScaleFileName: string);
    procedure WriteScaleFactorsID_andOption(RequiredValues: TRequiredValues;
      UnitConversionScaleFactor: string; ExternalScaleFileName: string);
    procedure WriteOwhmList(RequiredValues: TRequiredValues; AFileName: string;
      GetCollection: TGetCropCollection; const ErrorMessage: string);
    procedure WriteLeachList(RequiredValues: TRequiredValues; AFileName: string;
      GetCollection: TGetCropCollection; const ErrorMessage: string);
    procedure WriteSurfaceElevation;
    procedure WriteArrayTemplate(RequiredValues: TRequiredValues);
    function GetFormulaValue(var PestValues: TPestValues): Double;
    procedure AdjustFormulaForPest(var PestValues: TPestValues);
  protected
    procedure Evaluate; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    function ITMPUsed: Boolean; override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteString(const Value: AnsiString); overload; override;
    procedure WriteConstantU2DINT(const Comment: string;
      const Value: integer; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); override;
    procedure WriteConstantU2DREL(const Comment: string;
      const Value: double; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); override;
    procedure UpdateFarmIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateRefEtDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdatePrecipDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateCropIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEfficiencyDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEfficiencyImprovementDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateBareRunoffFractionDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateBarePrecipitationConsumptionFractionDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEvapBareDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateDirectRechargeDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdatePrecipPotConsumptionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateNrdInfilLocationDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateLandUseAreaFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultLandUseAreaFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateCropCoefficientDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultCropCoefficientDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateConsumptiveUseDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultConsumptiveUseDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateIrrigationDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultIrrigationDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateRootDepthDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultRootDepthDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateTranspirationFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultTranspirationFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEvaporationIrrigationFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultEvaporationIrrigationFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateFractionOfPrecipToSurfaceWaterDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultFractionOfPrecipToSurfaceWaterDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateFractionOfIrrigToSurfaceWaterDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultFractionOfIrrigToSurfaceWaterDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateAddedDemandDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultAddedDemandDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateHasSalinityDemandDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateMultHasSalinityDemandDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateAddedDemandRunoffSplitDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  frmErrorsAndWarningsUnit, ModflowFmpWriterUnit,
  ModflowUnitNumbers, frmProgressUnit,
  ModflowFmpFarmUnit, System.StrUtils,
  ModflowFmpIrrigationUnit,
  ModflowFmpAllotmentUnit, DataSetNamesUnit, ModflowMNW2_WriterUnit, FastGEO,
  ModflowParameterInterfaceUnit, frmFormulaErrorsUnit;

resourcestring
  StrUndefinedError = 'Undefined value in one or more stress periods';
  StrInvalidEfficiencyV = 'Invalid Efficiency value';
  StrInvalidEfficiencyI = 'Invalid Efficiency Improvement value';
  StrInvalidBareRunoff = 'Invalid Bare Runoff Fraction value';
  StrInvalidPrecipConsumpRunoff = 'Invalid Bare Precipitation Consumption Fraction value';
  StrInvalidCapillaryFringe = 'Invalid Capillary Fringe value';
  StrInvalidPotentialEv = 'Invalid Potential Evaporation Bare value';
  StrStressPeriodD = 'Stress Period %d';
  StrNonRoutedDeliverie = 'Non-Routed Deliveries enabled but not used';
  StrInTheMODFLOWPacka = 'In the MODFLOW Packages and Programs dialog box, i' +
  'n the Surface Water section of the Farm processs, non routed deliveries a' +
  're enabled but none have been defined in the Farms dialog box.';
  StrNonroutedDelivery = 'Non-routed delivery infiltration locations must be' +
  ' greater than or equal to 10.';
  StrLANDUSEPRINTROWC = 'LAND_USE PRINT ROW_COLUMN Issue';
  PrintRowColWarning1 = 'The "PRINT ROW_COLUMN" option in the LAND_USE block' +
  ' of the Farm Process has been selected but every cell has been selected i' +
  'n the %s data set. This is equivalent to "PRINT ALL" and "PRINT ALL" is a' +
  'lso selected so PRINT ROW_COLUMN is being skipped.';
  PrintRowColWarning2 = 'The "PRINT ROW_COLUMN" option in the LAND_USE block' +
  ' of the Farm Process has been selected but every cell has been selected i' +
  'n the %s data set. This is equivalent to "PRINT ALL" so "PRINT ALL" is be' +
  'ing used instead.';
  PrintRowColWarning3 = 'The PRINT ROW_COLUMN option in the LAND_USE block o' +
  'f the Farm Process has been selected but no cell has been selected in the' +
  ' %s data set so "PRINT ROW_COLUMN" is being skipped.';
  StrInvalidLandUseAre = 'Invalid land use area fraction formula in ';
  StrInvalidCropCoeffic = 'Invalid crop coefficient formula in ';
  StrInvalidConsumptive = 'Invalid consumptive use formula in ';
  StrInvalidRootDepthF = 'Invalid root depth formula in ';
  StrInvalidTranspiratio = 'Invalid transpiration fraction formula in ';
  StrInvalidSurfaceWate = 'Invalid surface water loss fraction precipitation' +
  ' formula in ';
  StrInvalidPondDepthF = 'Invalid pond depth formula in ';
  StrInvalidAddedDemand = 'Invalid added demand formula in ';
  StrInvalidZeroConsump = 'Invalid Zero consumptive use becomes bare soil fo' +
  'rmula in ';
  StrInvalidEvaporation = 'Invalid Evaporation Irrigation Fraction Sum One C' +
  'orrection formula in ';
  StrInvalidGroundwater = 'Invalid Groundwater Root Interaction value';
  StrInvalidDeficiencyS = 'Invalid Deficiency Scenario value';
  StrInvalidFractionOf = 'Invalid Fraction of Unconsumed Irrig. to Surface W' +
  'ater value';
  StrInvalidFractionOf2 = 'Invalid Fraction of Unconsumed Precip. to Surface ' +
  'Water value';
  StrInvalidAddedDemand2 = 'Invalid Added Demand Runoff Split value';
  StrInvalidTranspiratio2 = 'Invalid Transpiration Fraction value';
  StrInvalidGroundWaterAllotment = 'Invalid ground water allotment value';
  StrInvalidIrrigationU = 'Invalid Irrigation Uniformity';
  StrInvalidLandUseArea = 'Invalid Land Use Area Fraction value';
  StrInvalidPrecipitatio = 'Invalid Precipitation Potential Consumption valu' +
  'e';
  StrInvalidSurfaceWaterallotment = 'Invalid surface water allotment value';
  StrInvalidWaterSource = 'Invalid Water Source value';
  StrInvalidSaltSupply = 'Invalid salt supply concentration formula';
  StrInvalidSemirouted = 'Invalid semi-routed delivery value';
  StrInvalidSemirouted2 = 'Invalid semi-routed delivery lower limit value';
  StrInvalidSemiroutedUpper = 'Invalid semi-routed delivery upper limit valu' +
  'e';
  StrInvalidSemiroutedRet = 'Invalid semi-routed return value';
  StrInvalidAddedCropD = 'Invalid Added_Crop_Demand Flux value';
  StrInvalidAddedCropDRate = 'Invalid Added_Crop_Demand rate value';
  StrInvalidAddedDemandV = 'Invalid Added Demand value';
  StrInvalidConsumptiveUse = 'Invalid Consumptive Use value';
  StrInvalidCropCoefficV = 'Invalid Crop Coefficient value';
  StrInvalidNonroutedD = 'Invalid non-routed delivery value';
  StrInvalidNoReturnFl = 'Invalid no return flow value';
  StrInvalidIrrigationV = 'Invalid Irrigation value';
  StrInvalidRootDepthV = 'Invalid Root Depth value';
  StrInvalidRootPressur = 'Invalid Root Pressure value';
  StrInvalidSoilCoeffic = 'Invalid soil coefficient value';
  StrInvalidSoilIDValu = 'Invalid Soil ID value';
  StrInvalidSurfaceVert = 'Invalid Surface Vertical K value';
  StrStressPeriod = ' Stress period ';
  StrNoSoilsDefined = 'No soils defined';
  StrNoSoilsAreDefined = 'No soils are defined, so the FMP4 SOIL block is sk' +
  'ipped.';
  StrAtLeastOneSoilMu = 'At least one soil must be defined if land uses are ' +
  'used.';
  StrSoilCoefficientNot = 'Soil Coefficient not defined.';
  StrWhenTheRootPressu = 'When the Root pressure option is used in the Land ' +
  'Use section of the Farm Process, the Farm process requires that the soil ' +
  'coefficient be defined in the Soil section. You do that by first enabling' +
  ' the Soil Coefficient in the "Model|MODFLOW Packages and Programs" dialog ' +
  'box and then specifying a value in the "Model|Farm Process|Farm Soils" ' +
  'dialog box.';
  StrSoilTypeUndefined = 'Soil type undefined.';

{ TModflowFmp4Writer }

function TModflowFmp4Writer.CellType: TValueCellType;
begin
  result := TFmpWell_Cell;
end;

procedure TModflowFmp4Writer.CheckDataSetBetweenZeroAndFive(
  IntegerArray: TDataArray; const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Assert(IntegerArray <> nil);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      if FACtiveSurfaceCells[RowIndex, ColIndex] then
      begin
        if IntegerArray.IsValue[0, RowIndex, ColIndex] then
        begin
          if (IntegerArray.IntegerData[0, RowIndex, ColIndex] < 0)
            or (IntegerArray.IntegerData[0, RowIndex, ColIndex] > 5) then
          begin
            frmErrorsAndWarnings.AddError(Model, ErrorMessage,
              Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.CheckDataSetBetweenZeroAndOne(
  RealArray: TDataArray; const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Assert(RealArray <> nil);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      if FACtiveSurfaceCells[RowIndex, ColIndex] then
      begin
        if RealArray.IsValue[0, RowIndex, ColIndex] then
        begin
          if RealArray.DataType = rdtInteger then
          begin
            if (RealArray.IntegerData[0, RowIndex, ColIndex] < 0)
              or (RealArray.IntegerData[0, RowIndex, ColIndex] > 1) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end
          else
          begin
            if (RealArray.RealData[0, RowIndex, ColIndex] < 0)
              or (RealArray.RealData[0, RowIndex, ColIndex] > 1) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.CheckDataSetZeroOrGETen(IntegerArray: TDataArray;
  const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
  Value: Integer;
begin
  Assert(IntegerArray <> nil);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      if FACtiveSurfaceCells[RowIndex, ColIndex] then
      begin
        if IntegerArray.IsValue[0, RowIndex, ColIndex] then
        begin
          if IntegerArray.DataType = rdtInteger then
          begin
            Value := IntegerArray.IntegerData[0, RowIndex, ColIndex];
            if not ((Value = 0) or (Value >= 10)) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end
          else
          begin
            Assert(False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.CheckDataSetZeroOrPositive(IntegerArray: TDataArray;
  const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Assert(IntegerArray <> nil);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      if FACtiveSurfaceCells[RowIndex, ColIndex] then
      begin
        if IntegerArray.IsValue[0, RowIndex, ColIndex] then
        begin
          if IntegerArray.DataType = rdtInteger then
          begin
            if (IntegerArray.IntegerData[0, RowIndex, ColIndex] < 0) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end
          else
          begin
            if (IntegerArray.RealData[0, RowIndex, ColIndex] < 0) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TModflowFmp4Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FFarmIDs := TObjectList.Create;
  FRefEts := TObjectList.Create;
  FPrecip := TObjectList.Create;
  FCropIDs := TObjectList.Create;
  FEfficiencies := TObjectList.Create;
  FEfficiencyImprovements := TObjectList.Create;
  FBareRunoffFractions := TObjectList.Create;
  FBarePrecipitationConsumptionFractions := TObjectList.Create;
  FEvapBare := TObjectList.Create;
  FDirectRecharge := TObjectList.Create;
  FPrecipPotConsumption := TObjectList.Create;
  FNrdInfilLocation := TObjectList.Create;
  FLandUseAreaFraction := TObjectList.Create;
  FCropCoefficient := TObjectList.Create;
  FConsumptiveUse := TObjectList.Create;
  FIrrigation := TObjectList.Create;
  FRootDepth := TObjectList.Create;
  FTranspirationFraction := TObjectList.Create;
  FEvaporationIrrigationFraction := TObjectList.Create;
  FFractionOfPrecipToSurfaceWater := TObjectList.Create;
  FFractionOfIrrigToSurfaceWater := TObjectList.Create;
  FAddedDemand := TObjectList.Create;
  FCropHasSalinityDemand := TObjectList.Create;
  FAddedDemandRunoffSplit := TObjectList.Create;

  FFarmProcess4 := Package as TFarmProcess4;
  FClimatePackage := Model.ModflowPackages.FarmClimate4;
  FLandUse := Model.ModflowPackages.FarmLandUse;
  FSoil4 := Model.ModflowPackages.FarmSoil4;
  FSurfaceWater4 := Model.ModflowPackages.FarmSurfaceWater4;
  FAllotments := Model.ModflowPackages.FarmAllotments;
  FSalinityFlush := Model.ModflowPackages.FarmSalinityFlush;
  FFarmWells4 := Model.ModflowPackages.FarmWells4;

  FFmpSoils := Model.FmpSoils;
end;

procedure TModflowFmp4Writer.WriteDeficiencyScenario;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  TimeIndex: Integer;
  StartTime: Double;
  FarmID: Integer;
  FarmIndex: Integer;
  AFarm: TFarm;
  InnerFarmIndex: Integer;
  DSItem: TOwhmItem;
  procedure WriteDeficiencyScenarioItem(AFarm: TFarm; DSItem: TOwhmItem);
  var
    Formula: string;
  begin
    if DSItem <> nil then
    begin
      Formula := DSItem.OwhmValue;
      WriteBooleanValueFromGlobalFormula(Formula, AFarm,
        'Invalid deficiency scenario formula in ' + AFarm.FarmName);
    end
    else
    begin
      WriteInteger(1);
    end;
  end;
begin
  if FFarmProcess4.DeficiencyScenario.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlDeficiencyScenario;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: DEFICIENCY_SCENARIO';
  RequiredValues.ErrorID := 'FMP WBS: DEFICIENCY_SCENARIO';
  RequiredValues.ID := 'DEFICIENCY_SCENARIO';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.DeficiencyScenario.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidDeficiencyS;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.DeficiencyScenario;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlDeficiencyScenario);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    try
      FWriteLocation := RequiredValues.WriteLocation;
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              WriteInteger(1);
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            DSItem := AFarm.DeficiencyScenario.ItemByStartTime(StartTime) as TOwhmItem;
            WriteDeficiencyScenarioItem(AFarm, DSItem);

            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            WriteInteger(1);
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.DeficiencyScenario.Count > 0 then
          begin
            DSItem := AFarm.DeficiencyScenario.First;
          end
          else
          begin
            DSItem := nil;
          end;
          WriteDeficiencyScenarioItem(AFarm, DSItem);

          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

destructor TModflowFmp4Writer.Destroy;
begin
  FreeFileStreams;

  FAddedDemandRunoffSplit.Free;
  FCropHasSalinityDemand.Free;
  FAddedDemand.Free;
  FFractionOfIrrigToSurfaceWater.Free;
  FFractionOfPrecipToSurfaceWater.Free;
  FEvaporationIrrigationFraction.Free;
  FTranspirationFraction.Free;
  FRootDepth.Free;
  FIrrigation.Free;
  FConsumptiveUse.Free;
  FCropCoefficient.Free;
  FLandUseAreaFraction.Free;
  FNrdInfilLocation.Free;
  FPrecipPotConsumption.Free;
  FDirectRecharge.Free;
  FEvapBare.Free;
  FBarePrecipitationConsumptionFractions.Free;
  FBareRunoffFractions.Free;
  FEfficiencyImprovements.Free;
  FEfficiencies.Free;
  FCropIDs.Free;
  FPrecip.Free;
  FRefEts.Free;
  FFarmIDs.Free;
  inherited;
end;

procedure TModflowFmp4Writer.Evaluate;
begin
  if FFarmWells4.IsSelected then
  begin
    inherited;
  end;
end;

procedure TModflowFmp4Writer.EvaluateActiveCells;
var
  RowIndex: Integer;
  LayerIndex: Integer;
  ActiveDataSet: TDataArray;
  ColumnIndex: Integer;
begin
  SetLength(FACtiveSurfaceCells, Model.Grid.RowCount, Model.Grid.ColumnCount);
  ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColumnIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      FACtiveSurfaceCells[RowIndex,ColumnIndex] := False;
      for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
      begin
        FACtiveSurfaceCells[RowIndex,ColumnIndex] :=
          ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColumnIndex];
        if FACtiveSurfaceCells[RowIndex,ColumnIndex] then
        begin
          break;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.EvaluateAddedDemand;
begin
  if FLandUse.TransientAddedDemandarrayUsed(nil)
    or FLandUse.TransientAddedDemandMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Added Demand value');
    EvaluateTransientArrayData(wlAddedDemand);
  end;
end;

procedure TModflowFmp4Writer.EvaluateAddedDemandRunoffSplit;
begin
  if FFarmProcess4.TransientArrayAddedDemandRunoffSplitDisplayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Added Demand Runoff Split value');
    EvaluateTransientArrayData(wlAddedDemandRunoffSplit);
  end;
end;

procedure TModflowFmp4Writer.EvaluateAll;
begin
  RemoveErrorAndWarningMessages;

  if FLandUse.IsSelected then
  begin
    if FFmpSoils.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoSoilsDefined, StrAtLeastOneSoilMu)
    end;
  end;

  Evaluate;
  EvaluateActiveCells;
  EvaluateFarmID;
  EvaluateCropID;
  EvaluateEfficiencyImprovement;
  EvaluateNrdInfilLocation;
  EvaluateIrrigation;
  EvaluateCropHasSalinityDemand;
end;

procedure TModflowFmp4Writer.EvaluateBareEvap;
begin
  if FClimatePackage.TransientBareEvapUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPotentialEv);
    EvaluateTransientArrayData(wlBareEvap);
  end;
end;

procedure TModflowFmp4Writer.EvaluateBarePrecipitationConsumptionFraction;
begin
  if FFarmProcess4.TransientArrayBarePrecipitationConsumptionFractionDisplayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPrecipConsumpRunoff);
    EvaluateTransientArrayData(wlBarePrecipitationConsumptionFraction);
  end;
end;

procedure TModflowFmp4Writer.EvaluateBareRunoffFraction;
begin
  if FFarmProcess4.TransientArrayBareRunoffFractionDisplayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidBareRunoff);
    EvaluateTransientArrayData(wlBareRunoffFraction);
  end;
end;

procedure TModflowFmp4Writer.EvaluateConsumptiveUse;
begin
  if FLandUse.TransientConsumptiveUsearrayUsed(nil)
    or FLandUse.TransientConsumptiveUseMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Consumptive Use value');
    EvaluateTransientArrayData(wlConsumptiveUse);
  end;
end;

procedure TModflowFmp4Writer.EvaluateCropCoefficient;
begin
  if FLandUse.TransientCropCoefficientarrayUsed(nil)
    or FLandUse.TransientCropCoefficientMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Crop Coefficient value');
    EvaluateTransientArrayData(wlCropCoefficient);
  end;
end;

procedure TModflowFmp4Writer.EvaluateCropHasSalinityDemand;
begin
  if FSalinityFlush.TransientCropHasSalinityDemandarrayUsed(nil)
    or FSalinityFlush.TransientCropHasSalinityDemandMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Crop has Salinity Demand value');
    EvaluateTransientArrayData(wlCropHasSalinityDemand);
  end;
end;

procedure TModflowFmp4Writer.EvaluateCropID;
begin
  if FLandUse.CropLocation = rstTransient then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidCropIDInF);
    EvaluateTransientArrayData(wlCID);
  end;
end;

procedure TModflowFmp4Writer.EvaluateDirectRecharge;
begin
  if FClimatePackage.TransientDirectRechargeUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Direct Recharge value');
    EvaluateTransientArrayData(wlDirectRecharge);
  end;
end;

procedure TModflowFmp4Writer.EvaluateEfficiency;
begin
  if (FFarmProcess4.EfficiencyOptions.ArrayList = alArray)
    and (FFarmProcess4.EfficiencyOptions.FarmOption = foTransient) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidEfficiencyV);
    EvaluateTransientArrayData(wlEfficiency);
  end;

end;

procedure TModflowFmp4Writer.EvaluateEfficiencyImprovement;
begin
  if FFarmProcess4.TransientArrayEfficiencyImprovementUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidEfficiencyI);
    EvaluateTransientArrayData(wlEfficiencyImprovement);
  end;
end;

procedure TModflowFmp4Writer.EvaluateEvaporationIrrigationFraction;
begin
  if FLandUse.TransientEvaporationIrrigationFractionarrayUsed(nil)
    or FLandUse.TransientEvaporationIrrigationFractionMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Evaporation Irrigation Fraction value');
    EvaluateTransientArrayData(wlEvaporationIrrigationFraction);
  end;
end;

procedure TModflowFmp4Writer.EvaluateFarmID;
begin
  if FFarmProcess4.TransientFarmIdUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmID);
    EvaluateTransientArrayData(wlFID);
  end;
end;

procedure TModflowFmp4Writer.EvaluateFractionOfIrrigToSurfaceWater;
begin
  if FLandUse.TransientFractionOfIrrigToSurfaceWaterarrayUsed(nil)
    or FLandUse.TransientFractionOfIrrigToSurfaceWaterMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Fraction of Unconsumed Irrig to Surface Water value');
    EvaluateTransientArrayData(wlFractionOfIrrigToSurfaceWater);
  end;
end;

procedure TModflowFmp4Writer.EvaluateFractionOfPrecipToSurfaceWater;
begin
  if FLandUse.TransientFractionOfPrecipToSurfaceWaterarrayUsed(nil)
    or FLandUse.TransientFractionOfPrecipToSurfaceWaterMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Fraction of Unconsumed Precip to Surface Water value');
    EvaluateTransientArrayData(wlFractionOfPrecipToSurfaceWater);
  end;
end;

procedure TModflowFmp4Writer.EvaluateIrrigation;
begin
  if FLandUse.TransientIrrigationarrayUsed(nil)
    or FLandUse.TransientIrrigationMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Irrigation value');
    EvaluateTransientArrayData(wlIrrigation);
  end;
end;

procedure TModflowFmp4Writer.EvaluateLandUseAreaFraction;
begin
  if FLandUse.TransientLandUseAreaFractionArrayUsed(nil)
    or FLandUse.TransientLandUseAreaFractionMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Land Use Area Fraction');
    EvaluateTransientArrayData(wlLandUseAreaFraction);
  end;
end;

//procedure TModflowFmp4Writer.EvaluateMultTransientArrayData(
//  WriteLocation: TWriteLocation);
//begin
//
//end;

procedure TModflowFmp4Writer.EvaluateNrdInfilLocation;
begin
  if FSurfaceWater4.TransientNrdInfilLocationUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Non-Routed Delivery Infiltration Location in Farm Process');
    EvaluateTransientArrayData(wlNrdInfilLoc);
  end;
end;

procedure TModflowFmp4Writer.EvaluatePrecip;
begin
  if FClimatePackage.TransientPrecipUsed(Self) then
  begin
    EvaluateTransientArrayData(wlPFLX);
  end;
end;

procedure TModflowFmp4Writer.EvaluatePrecipPotConsumption;
begin
  if FClimatePackage.TransientPrecipPotConsumptionUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Precipitation Potential Consumption value');
    EvaluateTransientArrayData(wlPrecipPotConsumption);
  end;
end;

procedure TModflowFmp4Writer.EvaluateReferenceET;
begin
  if FClimatePackage.TransientEvapUsed(Self) then
  begin
    EvaluateTransientArrayData(wlETR);
  end;
end;

procedure TModflowFmp4Writer.EvaluateRootDepth;
begin
  if FLandUse.TransientRootDeptharrayUsed(nil)
    or FLandUse.TransientRootDepthMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Root Depth value');
    EvaluateTransientArrayData(wlRootDepth);
  end;
end;

class function TModflowFmp4Writer.Extension: string;
begin
  Result := '.fmp';
end;

procedure TModflowFmp4Writer.FreeFileStreams;
begin
  FreeAndNil(FFID_FileStream);
  FreeAndNil(FOpenCloseFileStream);
  FreeAndNil(FPFLX_FileStream);
  FreeAndNil(FETR_FileStream);
  FreeAndNil(FCID_FileStream);
  FreeAndNil(FEFFICIENCY_FileStream);
  FreeAndNil(FEFFICIENCY_IMPROVEMENT_FileStream);
  FreeAndNil(FBARE_RUNOFF_FRACTION_FileStream);
  FreeAndNil(FBarePrecipitationConsumptionFractionFileStream);
  FreeAndNil(FCapillaryFringeFileStream);
  FreeAndNil(FSoilIdStream);
  FreeAndNil(FSurfaceKFileStream);
  FreeAndNil(FEvapBareFileStream);
  FreeAndNil(FDirectRechargeFileStream);
  FreeAndNil(FPrecipPotConsumptionFileStream);
  FreeAndNil(FNrdInfilLocationFileStream);
  FreeAndNil(FLandUseAreaFractionFileStream);
  FreeAndNil(FCropcoefficientFileStream);
  FreeAndNil(FConsumptiveUseFileStream);
  FreeAndNil(FIrrigationFileStream);
  FreeAndNil(FRootDepthFileStream);
  FreeAndNil(FGwRootInteractionStream);
  FreeAndNil(FTranspirationFractionFileStream);
  FreeAndNil(FEvaporationIrrigationFractionFileStream);
  FreeAndNil(FFractionOfPrecipToSurfaceWaterFileStream);
  FreeAndNil(FFractionOfIrrigToSurfaceWaterFileStream);
  FreeAndNil(FAddedDemandFileStream);
  FreeAndNil(FCropHasSalinityDemandFileStream);
  FreeAndNil(FAddedDemandRunoffSplitFileStream);
  FreeAndNil(FIrrigationUniformityFileStream);
  FreeAndNil(FDeficiencyScenarioFileStream);
  FreeAndNil(FWaterSourceFileStream);
  FreeAndNil(FAddedCropDemandFluxFileStream);
  FreeAndNil(FAddedCropDemandRateFileStream);
  FreeAndNil(FEffectivPrecipitationTableFileStream);
  FreeAndNil(FEffectivCoefficientTableFileStream);
  FreeAndNil(FNonRoutedDeliveryFileStream);
  FreeAndNil(FNoReturnFlowFileStream);
  FreeAndNil(FSemiRoutedDeliveryFileStream);
  FreeAndNil(FSemiRoutedDeliveryLowerLimitFileStream);
  FreeAndNil(FSemiRoutedDeliveryUpperLimitFileStream);
  FreeAndNil(FSemiRoutedReturnFileStream);
  FreeAndNil(FSurfaceWaterAllotmentFileStream);
  FreeAndNil(FGroundWaterAllotmentFileStream);
  FreeAndNil(FRootPressureFileStream);
  FreeAndNil(FPondDepthFileStream);
  FreeAndNil(FConvertToBareFileStream);
  FreeAndNil(FSumOneCorrectionFileStream);
  FreeAndNil(FSaltSupplyConcentrationFileStream);
  FreeAndNil(FCropSalinityToleranceFileStream);
  FreeAndNil(FCropMaxLeachingRequirementFileStream);
  FreeAndNil(FCropLeachingRequirementFileStream);
  FreeAndNil(FCropSalinityAppliedWaterFileStream);
  FreeAndNil(FSupplyWellFileStream);
  FreeAndNil(FCropNamesFileStream);
  FreeAndNil(FCropPrintFileStream);

end;

//function TModflowFmp4Writer.GetAddedDemandCollection(
//  Crop: TCropItem): TOwhmCollection;
//begin
//  result := Crop.AddedDemandCollection;
//end;

function TModflowFmp4Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowFmpWellBoundary;
end;

function TModflowFmp4Writer.GetConsumptiveUseCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.ConsumptiveUseCollection;
end;

function TModflowFmp4Writer.GetCropCoefficentCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.CropCoefficientCollection;
end;

function TModflowFmp4Writer.GetCropHasSalinityDemandCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.CropHasSalinityDemand;
end;

function TModflowFmp4Writer.GetEvapIrrigateFracSumOneCorrectionCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.UseEvapFractionCorrectionCollection;
end;

function TModflowFmp4Writer.GetFileStreamName(WriteLocation: TWriteLocation): string;
  procedure InitializeTemplate;
  begin
    if WritingTemplate then
    begin
      FNameOfFile := result;
      WritePestTemplateLine(result);
      FWriteLocation := WriteLocation;
      try
        WriteTemplateHeader;
      finally
        FWriteLocation := wlMain;
      end;
    end;
  end;
  procedure UpdateTemplateFileName(var FileStream: TFileStream);
  begin
    if WritingTemplate then
    begin
      FreeAndNil(FileStream);
      result := result + '.tpl';
    end;
  end;
begin
  case WriteLocation of
    wlMain:
      begin
      end;
    wlOpenClose:
      begin
      end;
    wlOFE:
      begin
      end;
    wlCID:
      begin
        RESULT := ChangeFileExt(FBaseName, '.cid');
        if FCID_FileStream = nil then
        begin
          FCID_FileStream := TFileStream.Create(RESULT,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlFID:
      begin
        RESULT := ChangeFileExt(FBaseName, '.fid');
        if FFID_FileStream = nil then
        begin
          FFID_FileStream := TFileStream.Create(RESULT,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlRoot:
      begin
      end;
    wlCropUse:
      begin
      end;
    wlETR:
      begin
        result := ChangeFileExt(FBaseName, '.etr');
        UpdateTemplateFileName(FETR_FileStream);
        if FETR_FileStream = nil then
        begin
          FETR_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlEtFrac:
      begin
      end;
    wlSwLosses:
      begin
      end;
    wlPFLX:
      begin
        result := ChangeFileExt(FBaseName, '.pflx');
        UpdateTemplateFileName(FPFLX_FileStream);
        if FPFLX_FileStream = nil then
        begin
          FPFLX_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropFunc:
      begin
      end;
    wlWaterCost:
      begin
      end;
    wlDeliveries:
      begin
        result := ChangeFileExt(FBaseName, '.non_routed_delivery');
        UpdateTemplateFileName(FNonRoutedDeliveryFileStream);
        if FNonRoutedDeliveryFileStream = nil then
        begin
          FNonRoutedDeliveryFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSemiRouteDelivery:
      begin
        result := ChangeFileExt(FBaseName, '.semi_routed_delivery');
        UpdateTemplateFileName(FSemiRoutedDeliveryFileStream);
        if FSemiRoutedDeliveryFileStream = nil then
        begin
          FSemiRoutedDeliveryFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSemiRouteReturn:
      begin
        result := ChangeFileExt(FBaseName, '.semi_routed_return');
        UpdateTemplateFileName(FSemiRoutedReturnFileStream);
        if FSemiRoutedReturnFileStream = nil then
        begin
          FSemiRoutedReturnFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCall:
      begin
      end;
    wlEfficiency:
      begin
        result := ChangeFileExt(FBaseName, '.efficiency');
        UpdateTemplateFileName(FEFFICIENCY_FileStream);
        if FEFFICIENCY_FileStream = nil then
        begin
          FEFFICIENCY_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlEfficiencyImprovement:
      begin
        result := ChangeFileExt(FBaseName, '.efficiency_improvement');
        UpdateTemplateFileName(FEFFICIENCY_IMPROVEMENT_FileStream);
        if FEFFICIENCY_IMPROVEMENT_FileStream = nil then
        begin
          FEFFICIENCY_IMPROVEMENT_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlBareRunoffFraction:
      begin
        result := ChangeFileExt(FBaseName, '.bare_runoff_fraction');
        UpdateTemplateFileName(FBARE_RUNOFF_FRACTION_FileStream);
        if FBARE_RUNOFF_FRACTION_FileStream = nil then
        begin
          FBARE_RUNOFF_FRACTION_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlBarePrecipitationConsumptionFraction:
      begin
        result := ChangeFileExt(FBaseName, '.bare_precipitation_consumption_fraction');
        UpdateTemplateFileName(FBarePrecipitationConsumptionFractionFileStream);
        if FBarePrecipitationConsumptionFractionFileStream = nil then
        begin
          FBarePrecipitationConsumptionFractionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCapillaryFringe:
      begin
        result := ChangeFileExt(FBaseName, '.capillary_fringe');
        UpdateTemplateFileName(FCapillaryFringeFileStream);
        if FCapillaryFringeFileStream = nil then
        begin
          FCapillaryFringeFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSoilID:
      begin
        result := ChangeFileExt(FBaseName, '.soil_id');
        if FSoilIdStream = nil then
        begin
          FSoilIdStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlSurfaceK:
      begin
        result := ChangeFileExt(FBaseName, '.surface_vertical_hydraulic_conductivity');
        UpdateTemplateFileName(FSurfaceKFileStream);
        if FSurfaceKFileStream = nil then
        begin
          FSurfaceKFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlBareEvap:
      begin
        result := ChangeFileExt(FBaseName, '.potential_evaporation_bare');
        UpdateTemplateFileName(FEvapBareFileStream);
        if FEvapBareFileStream = nil then
        begin
          FEvapBareFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlDirectRecharge:
      begin
        result := ChangeFileExt(FBaseName, '.direct_recharge');
        UpdateTemplateFileName(FDirectRechargeFileStream);
        if FDirectRechargeFileStream = nil then
        begin
          FDirectRechargeFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlPrecipPotConsumption:
      begin
        result := ChangeFileExt(FBaseName, '.precipitation_potential_consumption');
        UpdateTemplateFileName(FPrecipPotConsumptionFileStream);
        if FPrecipPotConsumptionFileStream = nil then
        begin
          FPrecipPotConsumptionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlNrdInfilLoc:
      begin
        result := ChangeFileExt(FBaseName, '.nrd_infiltration_location');
        if FNrdInfilLocationFileStream = nil then
        begin
          FNrdInfilLocationFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlLandUseAreaFraction:
      begin
        result := ChangeFileExt(FBaseName, '.land_use_area_fraction');
        UpdateTemplateFileName(FLandUseAreaFractionFileStream);
        if FLandUseAreaFractionFileStream = nil then
        begin
          FLandUseAreaFractionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropCoefficient:
      begin
        result := ChangeFileExt(FBaseName, '.crop_coefficient');
        UpdateTemplateFileName(FCropcoefficientFileStream);
        if FCropcoefficientFileStream = nil then
        begin
          FCropcoefficientFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlConsumptiveUse:
      begin
        result := ChangeFileExt(FBaseName, '.consumptive_use');
        UpdateTemplateFileName(FConsumptiveUseFileStream);
        if FConsumptiveUseFileStream = nil then
        begin
          FConsumptiveUseFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlIrrigation:
      begin
        result := ChangeFileExt(FBaseName, '.irrigation');
        if FIrrigationFileStream = nil then
        begin
          FIrrigationFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlRootDepth:
      begin
        result := ChangeFileExt(FBaseName, '.root_depth');
        UpdateTemplateFileName(FRootDepthFileStream);
        if FRootDepthFileStream = nil then
        begin
          FRootDepthFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlGwRootInteraction:
      begin
        result := ChangeFileExt(FBaseName, '.groundwater_root_interaction');
        if FGwRootInteractionStream = nil then
        begin
          FGwRootInteractionStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlTranspirationFraction:
      begin
        result := ChangeFileExt(FBaseName, '.transpiration_fraction');
        UpdateTemplateFileName(FTranspirationFractionFileStream);
        if FTranspirationFractionFileStream = nil then
        begin
          FTranspirationFractionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlEvaporationIrrigationFraction:
      begin
        result := ChangeFileExt(FBaseName, '.evaporation_irrigation_fraction');
        UpdateTemplateFileName(FEvaporationIrrigationFractionFileStream);
        if FEvaporationIrrigationFractionFileStream = nil then
        begin
          FEvaporationIrrigationFractionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlFractionOfPrecipToSurfaceWater:
      begin
        result := ChangeFileExt(FBaseName, '.surfacewater_loss_fraction_precipitation');
        UpdateTemplateFileName(FFractionOfPrecipToSurfaceWaterFileStream);
        if FFractionOfPrecipToSurfaceWaterFileStream = nil then
        begin
          FFractionOfPrecipToSurfaceWaterFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlFractionOfIrrigToSurfaceWater:
      begin
        result := ChangeFileExt(FBaseName, '.surfacewater_loss_fraction_irrigation');
        UpdateTemplateFileName(FFractionOfIrrigToSurfaceWaterFileStream);
        if FFractionOfIrrigToSurfaceWaterFileStream = nil then
        begin
          FFractionOfIrrigToSurfaceWaterFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlAddedDemand:
      begin
        result := ChangeFileExt(FBaseName, '.added_demand');
        UpdateTemplateFileName(FAddedDemandFileStream);
        if FAddedDemandFileStream = nil then
        begin
          FAddedDemandFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropHasSalinityDemand:
      begin
        result := ChangeFileExt(FBaseName, '.crop_has_salinity_demand');
        if FCropHasSalinityDemandFileStream = nil then
        begin
          FCropHasSalinityDemandFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlAddedDemandRunoffSplit:
      begin
        result := ChangeFileExt(FBaseName, '.added_demand_runoff_split');
        UpdateTemplateFileName(FAddedDemandRunoffSplitFileStream);
        if FAddedDemandRunoffSplitFileStream = nil then
        begin
          FAddedDemandRunoffSplitFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlIrrigationUniformity:
      begin
        result := ChangeFileExt(FBaseName, '.wbs_irrigation_uniformity');
        UpdateTemplateFileName(FIrrigationUniformityFileStream);
        if FIrrigationUniformityFileStream = nil then
        begin
          FIrrigationUniformityFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlDeficiencyScenario:
      begin
        result := ChangeFileExt(FBaseName, '.deficiency_scenario');
        if FDeficiencyScenarioFileStream = nil then
        begin
          FDeficiencyScenarioFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlWaterSource:
      begin
        result := ChangeFileExt(FBaseName, '.watersource');
        if FWaterSourceFileStream = nil then
        begin
          FWaterSourceFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlAddedCropDemandFlux:
      begin
        result := ChangeFileExt(FBaseName, '.added_crop_demand_flux');
        UpdateTemplateFileName(FAddedCropDemandFluxFileStream);
        if FAddedCropDemandFluxFileStream = nil then
        begin
          FAddedCropDemandFluxFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlAddedCropDemandRate:
      begin
        result := ChangeFileExt(FBaseName, '.added_crop_demand_rate');
        UpdateTemplateFileName(FAddedCropDemandRateFileStream);
        if FAddedCropDemandRateFileStream = nil then
        begin
          FAddedCropDemandRateFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlPrecipicationTable:
      begin
        result := ChangeFileExt(FBaseName, '.effective_precipitation_table');
        UpdateTemplateFileName(FEffectivPrecipitationTableFileStream);
        if FEffectivPrecipitationTableFileStream = nil then
        begin
          FEffectivPrecipitationTableFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSoilCoefficient:
      begin
        result := ChangeFileExt(FBaseName, '.coefficient');
        UpdateTemplateFileName(FEffectivCoefficientTableFileStream);
        if FEffectivCoefficientTableFileStream = nil then
        begin
          FEffectivCoefficientTableFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlNoReturnFlow:
      begin
        result := ChangeFileExt(FBaseName, '.no_return_flow');
        if FNoReturnFlowFileStream = nil then
        begin
          FNoReturnFlowFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlSemiRouteDeliveryLowerLimit:
      begin
        result := ChangeFileExt(FBaseName, '.semi_routed_delivery_lower_limit');
        UpdateTemplateFileName(FSemiRoutedDeliveryLowerLimitFileStream);
        if FSemiRoutedDeliveryLowerLimitFileStream = nil then
        begin
          FSemiRoutedDeliveryLowerLimitFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSemiRouteDeliveryUpperLimit:
      begin
        result := ChangeFileExt(FBaseName, '.semi_routed_delivery_upper_limit');
        UpdateTemplateFileName(FSemiRoutedDeliveryUpperLimitFileStream);
        if FSemiRoutedDeliveryUpperLimitFileStream = nil then
        begin
          FSemiRoutedDeliveryUpperLimitFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSwAllotment:
      begin
        result := ChangeFileExt(FBaseName, '.surface_water_allotment');
        UpdateTemplateFileName(FSurfaceWaterAllotmentFileStream);
        if FSurfaceWaterAllotmentFileStream = nil then
        begin
          FSurfaceWaterAllotmentFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlGwAllotment:
      begin
        result := ChangeFileExt(FBaseName, '.groundwater_allotment');
        UpdateTemplateFileName(FGroundWaterAllotmentFileStream);
        if FGroundWaterAllotmentFileStream = nil then
        begin
          FGroundWaterAllotmentFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlRootPressure:
      begin
        result := ChangeFileExt(FBaseName, '.root_pressure');
        UpdateTemplateFileName(FRootPressureFileStream);
        if FRootPressureFileStream = nil then
        begin
          FRootPressureFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlPondDepth:
      begin
        result := ChangeFileExt(FBaseName, '.pond_depth');
        UpdateTemplateFileName(FPondDepthFileStream);
        if FPondDepthFileStream = nil then
        begin
          FPondDepthFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlNoCropMeansBareSoil:
      begin
        result := ChangeFileExt(FBaseName, '.zero_consumptive_use_becomes_bare_soil');
        if FConvertToBareFileStream = nil then
        begin
          FConvertToBareFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlNoEvapErrCorrection:
      begin
        result := ChangeFileExt(FBaseName, '.evaporation_irrigation_fraction_sum_one_correction');
        if FSumOneCorrectionFileStream = nil then
        begin
          FSumOneCorrectionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlSaltSupplyConcentration:
      begin
        result := ChangeFileExt(FBaseName, '.wbs_salt_supply_concentration');
        UpdateTemplateFileName(FSaltSupplyConcentrationFileStream);
        if FSaltSupplyConcentrationFileStream = nil then
        begin
          FSaltSupplyConcentrationFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropSalinityTolerance:
      begin
        result := ChangeFileExt(FBaseName, '.crop_salinity_tolerance');
        UpdateTemplateFileName(FCropSalinityToleranceFileStream);
        if FCropSalinityToleranceFileStream = nil then
        begin
          FCropSalinityToleranceFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropMaxLeachRequirement:
      begin
        result := ChangeFileExt(FBaseName, '.crop_max_leaching_requirement');
        UpdateTemplateFileName(FCropMaxLeachingRequirementFileStream);
        if FCropMaxLeachingRequirementFileStream = nil then
        begin
          FCropMaxLeachingRequirementFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropLeachRequirement:
      begin
        result := ChangeFileExt(FBaseName, '.crop_leaching_requirement');
        UpdateTemplateFileName(FCropLeachingRequirementFileStream);
        if FCropLeachingRequirementFileStream = nil then
        begin
          FCropLeachingRequirementFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSalinityAppliedWater:
      begin
        result := ChangeFileExt(FBaseName, '.crop_salinity_applied_water');
        UpdateTemplateFileName(FCropSalinityAppliedWaterFileStream);
        if FCropSalinityAppliedWaterFileStream = nil then
        begin
          FCropSalinityAppliedWaterFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlSupplyWells:
      begin
        result := ChangeFileExt(FBaseName, '.supply_well');
        UpdateTemplateFileName(FSupplyWellFileStream);
        if FSupplyWellFileStream = nil then
        begin
          FSupplyWellFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
          InitializeTemplate;
        end;
      end;
    wlCropNames:
      begin
        result := ChangeFileExt(FBaseName, '.crop_names');
        if FCropNamesFileStream = nil then
        begin
          FCropNamesFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlSpecifyPrintCrops:
      begin
        result := ChangeFileExt(FBaseName, '.specify_print_all_crops');
        if FCropPrintFileStream = nil then
        begin
          FCropPrintFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    else Assert(False);
  end;
  if (result <> '') and not WritingTemplate then
  begin
    Model.ModelInputFiles.Add(result);
  end;
end;

function TModflowFmp4Writer.GetFmpBoundary(ScreenObject: TScreenObject;
  WriteLocation: TWriteLocation): TModflowBoundary;
begin
  result := nil;
  case WriteLocation of
    wlMain: ;
    wlOpenClose: ;
    wlOFE: ;
    wlCID: result := ScreenObject.ModflowFmpCropID;
    wlFID: result := ScreenObject.ModflowFmpFarmID;
    wlRoot: ;
    wlCropUse: ;
    wlETR: result := ScreenObject.ModflowFmpRefEvap;
    wlEtFrac: ;
    wlSwLosses: ;
    wlPFLX: result := ScreenObject.ModflowFmpPrecip;
    wlCropFunc: ;
    wlWaterCost: ;
    wlDeliveries: ;
    wlSemiRouteDelivery: ;
    wlSemiRouteReturn: ;
    wlCall: ;
    wlEfficiency: result := ScreenObject.Fmp4EfficiencyBoundary;
    wlEfficiencyImprovement: result := ScreenObject.Fmp4EfficiencyImprovementBoundary;
    wlBareRunoffFraction: result := ScreenObject.Fmp4BareRunoffFractionBoundary;
    wlBarePrecipitationConsumptionFraction: result :=
      ScreenObject.Fmp4BarePrecipitationConsumptionFractionBoundary;
    wlCapillaryFringe: ;
    wlSoilID: ;
    wlSurfaceK: ;
    wlBareEvap: result := ScreenObject.ModflowFmpBareEvap;
    wlDirectRecharge: result := ScreenObject.ModflowFmpDirectRecharge;
    wlPrecipPotConsumption: result := ScreenObject.ModflowFmpPrecipPotConsumption;
    wlNrdInfilLoc: result := ScreenObject.ModflowFmp4NrdInfilLocationBoundary;
    wlLandUseAreaFraction:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4LandUseAreaFraction;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultLandUseAreaFraction;
        end;
      end;
    wlCropCoefficient:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4CropCoefficient;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultCropCoefficient;
        end;
      end;
    wlConsumptiveUse:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4ConsumptiveUse;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultConsumptiveUse;
        end;
      end;
    wlIrrigation:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4Irrigation;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultIrrigation;
        end;
      end;
    wlRootDepth:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4RootDepth;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultRootDepth;
        end;
      end;
    wlGwRootInteraction: ;
    wlTranspirationFraction:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4TranspirationFraction;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultTranspirationFraction;
        end;
      end;
    wlEvaporationIrrigationFraction:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4EvaporationIrrigationFraction;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultEvaporationIrrigationFraction;
        end;
      end;
    wlFractionOfPrecipToSurfaceWater:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4FractionOfPrecipToSurfaceWater;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultFractionOfPrecipToSurfaceWater;
        end;
      end;
    wlFractionOfIrrigToSurfaceWater:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4FractionOfIrrigToSurfaceWater;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultFractionOfIrrigToSurfaceWater;
        end;
      end;
    wlAddedDemand:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4AddedDemand;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultAddedDemand;
        end;
      end;
    wlCropHasSalinityDemand:
      begin
        if FLandUse.LandUseOption = luoSingle then
        begin
          result := ScreenObject.ModflowFmp4CropHasSalinityDemand;
        end
        else
        begin
          result := ScreenObject.ModflowFmp4MultCropHasSalinityDemand;
        end;
      end;
    wlAddedDemandRunoffSplit:
      begin
        result := ScreenObject.Fmp4AddedDemandRunoffSplitBoundary;
      end;
    wlIrrigationUniformity: ;
    wlDeficiencyScenario: ;
    wlWaterSource: ;
    wlAddedCropDemandFlux: ;
    wlAddedCropDemandRate: ;
    wlPrecipicationTable: ;
    wlSoilCoefficient: ;
    wlNoReturnFlow: ;
    wlSemiRouteDeliveryLowerLimit: ;
    wlSemiRouteDeliveryUpperLimit: ;
    wlSwAllotment: ;
    wlGwAllotment: ;
    wlRootPressure: ;
    wlPondDepth: ;
    wlNoCropMeansBareSoil: ;
    wlNoEvapErrCorrection: ;
    wlSaltSupplyConcentration: ;
    wlCropSalinityTolerance: ;
    wlCropMaxLeachRequirement: ;
    wlCropLeachRequirement: ;
    wlSalinityAppliedWater: ;
    wlSupplyWells: ;
    wlCropNames: ;
    wlSpecifyPrintCrops: ;
    else Assert(False);
  end;
end;

function TModflowFmp4Writer.GetTransientList(
  WriteLocation: TWriteLocation): TList;
begin
  result := nil;
  case WriteLocation of
    wlMain: ;
    wlOpenClose: ;
    wlOFE: ;
    wlCID: result := FCropIDs;
    wlFID: result := FFarmIDs;
    wlRoot: ;
    wlCropUse: ;
    wlETR: result := FRefEts;
    wlEtFrac: ;
    wlSwLosses: ;
    wlPFLX: result := FPrecip;
    wlCropFunc: ;
    wlWaterCost: ;
    wlDeliveries: ;
    wlSemiRouteDelivery: ;
    wlSemiRouteReturn: ;
    wlCall: ;
    wlEfficiency: result := FEfficiencies;
    wlEfficiencyImprovement: result := FEfficiencyImprovements;
    wlBareRunoffFraction: result := FBareRunoffFractions;
    wlBarePrecipitationConsumptionFraction: result := FBarePrecipitationConsumptionFractions;
    wlCapillaryFringe: ;
    wlSoilID: ;
    wlSurfaceK: ;
    wlBareEvap: result := FEvapBare;
    wlDirectRecharge: result := FDirectRecharge;
    wlPrecipPotConsumption: result := FPrecipPotConsumption;
    wlNrdInfilLoc: result := FNrdInfilLocation;
    wlLandUseAreaFraction: result := FLandUseAreaFraction;
    wlCropCoefficient: result := FCropCoefficient;
    wlConsumptiveUse: result := FConsumptiveUse;
    wlIrrigation: result := FIrrigation;
    wlRootDepth: result := FRootDepth;
    wlGwRootInteraction: ;
    wlTranspirationFraction: result := FTranspirationFraction;
    wlEvaporationIrrigationFraction: result := FEvaporationIrrigationFraction;
    wlFractionOfPrecipToSurfaceWater: result := FFractionOfPrecipToSurfaceWater;
    wlFractionOfIrrigToSurfaceWater: result := FFractionOfIrrigToSurfaceWater;
    wlAddedDemand: result := FAddedDemand;
    wlCropHasSalinityDemand: result := FCropHasSalinityDemand;
    wlAddedDemandRunoffSplit: result := FAddedDemandRunoffSplit;
    wlIrrigationUniformity: ;
    wlDeficiencyScenario: ;
    wlWaterSource: ;
    wlAddedCropDemandFlux: ;
    wlAddedCropDemandRate: ;
    wlPrecipicationTable: ;
    wlSoilCoefficient: ;
    wlNoReturnFlow: ;
    wlSemiRouteDeliveryLowerLimit: ;
    wlSemiRouteDeliveryUpperLimit: ;
    wlSwAllotment: ;
    wlGwAllotment: ;
    wlRootPressure: ;
    wlPondDepth: ;
    wlNoCropMeansBareSoil: ;
    wlNoEvapErrCorrection: ;
    wlSaltSupplyConcentration: ;
    wlCropSalinityTolerance: ;
    wlCropMaxLeachRequirement: ;
    wlCropLeachRequirement: ;
    wlSalinityAppliedWater: ;
    wlSupplyWells: ;
    wlCropNames: ;
    wlSpecifyPrintCrops: ;
    else Assert(False)
  end;
end;

function TModflowFmp4Writer.GetTranspirationFractionCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.TranspirationFractionCollection;
end;

function TModflowFmp4Writer.GetZeroConsumptiveUseBecomesBareSoilCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.ConvertToBareSoilCollection;
end;

function TModflowFmp4Writer.ITMPUsed: Boolean;
begin
  result := False;
end;

function TModflowFmp4Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.FarmProcess4;
end;

function TModflowFmp4Writer.ParameterType: TParameterType;
begin
  result := ptQMAX;
//  Assert(False);
end;

procedure TModflowFmp4Writer.PrintSurfaceWaterOutputOptions;
var
  PrintOption: TSurfaceWaterPrint;
  OutputFile: string;
begin
  for PrintOption in FSurfaceWater4.SurfaceWaterPrints do
  begin
    case PrintOption of
      swpPrint_Sfr_Delivery:
        begin
          WriteString('  PRINT SFR_DELIVERY ');
          OutputFile := ChangeFileExt(FInputFileName, '.SFR_Delivery');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      swpPrint_Sfr_Delivery_By_Wbs:
        begin
          WriteString('  PRINT SFR_DELIVERY_BY_WBS ');
          OutputFile := ChangeFileExt(FInputFileName, '.SFR_DeliveryByWBS');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      swpPrint_Sfr_Return:
        begin
          WriteString('  PRINT SFR_RETURN ');
          OutputFile := ChangeFileExt(FInputFileName, '.SFR_Return');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      swpPrint_Sfr_Srr_Only:
        begin
          WriteString('  PRINT SFR_SRR_ONLY ');
          OutputFile := ChangeFileExt(FInputFileName, '.SFR_SRR_Only');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      swpPrint_Nrd:
        begin
          WriteString('  PRINT NRD ');
          OutputFile := ChangeFileExt(FInputFileName, '.Non_Routed_Deliveries');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      swpPrint_Nrd_By_Wbs:
        begin
          WriteString('  PRINT NRD_BY_WBS ');
          OutputFile := ChangeFileExt(FInputFileName, '.Non_Routed_Deliveries_By_WBS');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
    end;
  end;
end;

procedure TModflowFmp4Writer.RemoveErrorAndWarningMessages;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidEfficiencyV);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidEfficiencyI);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidBareRunoff);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPrecipConsumpRunoff);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidCapillaryFringe);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPotentialEv);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNonroutedDelivery);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidGroundwater);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidDeficiencyS);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFractionOf);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFractionOf2);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidAddedDemand2);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidTranspiratio2);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidGroundWaterAllotment);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidIrrigationU);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidLandUseArea);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPrecipitatio);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSurfaceWaterallotment);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidWaterSource);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSaltSupply);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSaltSupply);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSemirouted);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSemirouted2);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSemiroutedUpper);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSemiroutedRet);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidAddedCropD);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidAddedCropDRate);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidAddedDemandV);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidConsumptiveUse);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidCropCoefficV);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidNonroutedD);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidNoReturnFl);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidIrrigationV);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidRootDepthV);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidRootPressur);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSoilCoeffic);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSoilIDValu);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSurfaceVert);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrThePriorityForNon);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUndefinedError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmProcess);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoSoilsDefined);



  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNonRoutedDeliverie);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheMODFLOWPacka);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLANDUSEPRINTROWC);

end;

procedure TModflowFmp4Writer.UpdateAddedDemandDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateAddedDemand;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientAddedDemandarrayUsed;
  UpdateRequirements.WriteLocation := wlAddedDemand;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateAddedDemandRunoffSplitDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateAddedDemandRunoffSplit;
  UpdateRequirements.TransientDataUsed := FFarmProcess4.
    TransientArrayAddedDemandRunoffSplitDisplayUsed;
  UpdateRequirements.WriteLocation := wlAddedDemandRunoffSplit;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteArrayTemplate(RequiredValues: TRequiredValues);
var
  TemplateFileName: string;
begin
  if FPestParamUsed then
  begin
    WritingTemplate := True;
    try
      TemplateFileName := GetFileStreamName(RequiredValues.WriteLocation);
      WriteFmpArrayData(TemplateFileName, RequiredValues);
    finally
      FPestParamUsed := False;
      WritingTemplate := False;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteMnwPumpSpread;
var
  FarmID: Integer;
  FarmIndex: Integer;
  AFarm: TFarm;
  InnerFarmIndex: Integer;
begin
  WriteString('  MNW_PUMP_SPREAD');
  case FFarmWells4.MnwPumpSpread of
    pscConductance:
      begin
        WriteString(' BY_COND');
        NewLine;
      end;
    pscByNodeCount:
      begin
        WriteString(' BY_COUNT');
        NewLine;
      end;
    pscTopNode:
      begin
        WriteString(' BY_TOP');
        NewLine;
      end;
    pscByWbs:
      begin
        WriteString(' ByWBS');
        NewLine;
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(0);
            Inc(FarmID);
            NewLine;
          end;
          WriteInteger(Ord(AFarm.PumpSpreadChoice));
          Inc(FarmID);
          NewLine;
        end;
      end;
  end;
end;

procedure TModflowFmp4Writer.WriteSurfaceElevation;
var
  RequiredValues: TRequiredValues;
begin
  RequiredValues.WriteLocation := wlMain;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP GLOBAL_DIMENSION: SURFACE_ELEVATION';
  RequiredValues.ErrorID := 'FMP GLOBAL_DIMENSION: SURFACE_ELEVATION';
  RequiredValues.ID := 'SURFACE_ELEVATION';
  RequiredValues.StaticDataName := StrUzfLandSurface;
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckError :=  '';
  RequiredValues.CheckProcedure := nil;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

  FPestParamUsed := False;
  WriteFmpArrayData('', RequiredValues);
  WriteArrayTemplate(RequiredValues);

end;

procedure TModflowFmp4Writer.WriteOwhmList(RequiredValues: TRequiredValues;
  AFileName: string; GetCollection: TGetCropCollection; const ErrorMessage: string);
var
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  OwhmCollection: TOwhmCollection;
  procedure WriteOwhmItem(Crop: TCropItem; OwhmItem: TOwhmItem);
  var
    Value: double;
    PestValues: TPestValues;
    Formula: string;
  begin
    if OwhmItem <> nil then
    begin
      Formula := OwhmItem.OwhmValue;
      if OwhmItem is TBoolFarmItem then
      begin
        WriteBooleanValueFromGlobalFormula(Formula, Crop,
          ErrorMessage + Crop.CropName);
      end
      else
      begin
        PestValues.Formula := Formula;

        PestValues.PestName := '';
        PestValues.PestSeriesName := OwhmCollection.PestSeriesParameter;
        PestValues.PestSeriesMethod := OwhmCollection.PestParamMethod;
        PestValues.FormulaErrorMessage := ErrorMessage + Crop.CropName;
        PestValues.ErrorObjectName := Crop.CropName;

        AdjustFormulaForPest(PestValues);

        if WritingTemplate and PestValues.ParameterUsed then
        begin
          Value := GetFormulaValue(PestValues);

          WritePestTemplateFormula(Value, PestValues.PestName,
            PestValues.PestSeriesName, PestValues.PestSeriesMethod,
            nil);
        end
        else
        begin
          WriteFloatValueFromGlobalFormula(PestValues.Formula, Crop,
            PestValues.FormulaErrorMessage);
        end;

//        WriteFloatValueFromGlobalFormula(Formula, Crop,
//          ErrorMessage + Crop.CropName);
      end;
    end
    else
    begin
      if OwhmCollection is TBoolFarmCollection then
      begin
        WriteInteger(Round(RequiredValues.DefaultValue));
      end
      else
      begin
        WriteFloat(RequiredValues.DefaultValue);
      end;
    end;
  end;
  procedure WriteTransientData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
    StartTime: Double;
    ACrop: TCropItem;
//    OwhmCollection: TOwhmCollection;
    OwhmItem: TOwhmItem;
  begin
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      WriteCommentLine(Format(StrStressPeriodD, [TimeIndex + 1]));
      StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;
      for CropIndex := 0 to Model.FmpCrops.Count - 1 do
      begin
        ACrop := Model.FmpCrops[CropIndex];
        WriteInteger(CropIndex + 1);
        OwhmCollection := GetCollection(ACrop);
        OwhmItem := OwhmCollection.ItemByStartTime(StartTime) as TOwhmItem;
        WriteOwhmItem(ACrop, OwhmItem);
        NewLine;
      end;
    end;
  end;
  procedure WriteStaticData;
  var
    CropIndex: Integer;
    ACrop: TCropItem;
    OwhmItem: TOwhmItem;
  begin
    for CropIndex := 0 to Model.FmpCrops.Count - 1 do
    begin
      ACrop := Model.FmpCrops[CropIndex];
      WriteInteger(CropIndex + 1);
      OwhmCollection := GetCollection(ACrop);
      if OwhmCollection.Count > 0 then
      begin
        OwhmItem := OwhmCollection.First as TOwhmItem;
      end
      else
      begin
        OwhmItem := nil;
      end;
      WriteOwhmItem(ACrop, OwhmItem);
      NewLine;
    end;
  end;
  procedure WriteListData;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        WriteTransientData;
      end
      else
      begin
        WriteStaticData;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  FPestParamUsed := False;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues, UnitConversionScaleFactor,
    ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteProrateDeficiency;
begin
  if (FFarmProcess4.DeficiencyScenario.FarmOption <> foNotUsed) then
  begin
    WriteString('  PRORATE_DEFICIENCY ');
    case FFarmProcess4.ProrateDeficiency of
      pdoByDemand:
        begin
          WriteString('ByDEMAND');
        end;
      pdoAverage:
        begin
          WriteString('ByAVERAGE');
        end;
    else
      begin
        Assert(False);
      end;
    end;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteExpressionLineLength;
var
  ACrop: TCropItem;
  ItemIndex: Integer;
  CropIndex: Integer;
  AnItem: TLeachItem;
  LineLength: Integer;
  CustomFormula: string;
begin
  LineLength := 20;
  for CropIndex := 0 to Model.FmpCrops.Count - 1 do
  begin
    ACrop := Model.FmpCrops[CropIndex];
    for ItemIndex := 0 to ACrop.LeachingRequirementCollection.Count - 1 do
    begin
      AnItem := ACrop.LeachingRequirementCollection[ItemIndex]as TLeachItem;
      if AnItem.LeachChoice = lcCustomFormula then
      begin
        CustomFormula := AnItem.CustomFormula;
        if Length(CustomFormula) > LineLength then
        begin
          LineLength := Length(CustomFormula);
        end;
      end;
    end;
    for ItemIndex := 0 to ACrop.SalinityAppliedWater.Count - 1 do
    begin
      AnItem := ACrop.SalinityAppliedWater[ItemIndex]as TLeachItem;
      if AnItem.LeachChoice = lcCustomFormula then
      begin
        CustomFormula := AnItem.CustomFormula;
        if Length(CustomFormula) > LineLength then
        begin
          LineLength := Length(CustomFormula);
        end;
      end;
    end;
  end;
  WriteString('  EXPRESSION_LINE_LENGTH');
  WriteInteger(LineLength);
  NewLine;
end;

procedure TModflowFmp4Writer.WriteExpressionVariableNearZero;
begin
  WriteString('  EXPRESSION_VARIABLE_NEARZERO');
  WriteFloat(FSalinityFlush.ExpressionMin);
  NewLine;
end;

procedure TModflowFmp4Writer.WriteSalinityFlushPrintOptions;
var
  OutputFile: string;
  PrintOption: TSalinityFlushPrint;
begin
  if Model.ModflowOutputControl.PrintInputArrays or Model.ModflowOutputControl.PrintInputCellLists then
  begin
    WriteString('  PRINT INPUT ');
    OutputFile := ChangeFileExt(FInputFileName, '.Salinity_Flush_Input');
    WriteString(ExtractFileName(OutputFile));
    Model.AddModelOutputFile(OutputFile);
    NewLine;
  end;
  for PrintOption in FSalinityFlush.SalinityFlushPrints do
  begin
    case PrintOption of
      sfpPrintByFarm:
        begin
          WriteString('  PRINT BYWBS ');
          OutputFile := ChangeFileExt(FInputFileName, '.Salinity_Flush_Irrigation_By_WBS');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      sfpPrintByFarmByCrop:
        begin
          WriteString('  PRINT BYWBS_BYCROP ');
          OutputFile := ChangeFileExt(FInputFileName, '.Salinity_Flush_Irrigation_By_WBS_By_Crop');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      sfpPrintAll:
        begin
          WriteString('  PRINT ALL ');
          OutputFile := ChangeFileExt(FInputFileName, '.Salinity_Flush_Irrigation_All');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSaltSupplyConcentration;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  AFarm: TFarm;
  FarmID: Integer;
  StartTime: Double;
  SourceConcentration: TSaltSupplyConcentrationItem;
  procedure WriteItem(AFarm: TFarm; SourceConcentration: TSaltSupplyConcentrationItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if SourceConcentration <> nil then
    begin
      PestValues.Formula := SourceConcentration.NonRoutedConcentration;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.SaltSupplyConcentrationCollection.PestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.SaltSupplyConcentrationCollection.PestParamMethod;
      PestValues.FormulaErrorMessage :=
        Format('Invalid non-routed salt supply concentration formula for farm %0:s',
        [AFarm.FarmName]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;





//      Formula := SourceConcentration.NonRoutedConcentration;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        Format('Invalid non-routed salt supply concentration formula for farm %0:s',
//        [AFarm.FarmName]));


      PestValues.Formula := SourceConcentration.SurfaceWaterConcentration;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.SaltSupplyConcentrationCollection.SWConcPestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.SaltSupplyConcentrationCollection.SWConcPestParamMethod;
      PestValues.FormulaErrorMessage :=
        Format('Invalid surface water salt supply concentration formula for farm %0:s',
        [AFarm.FarmName]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;


//      Formula := SourceConcentration.SurfaceWaterConcentration;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        Format('Invalid surface water salt supply concentration formula for farm %0:s',
//        [AFarm.FarmName]));



      PestValues.Formula := SourceConcentration.GroundwaterConcentration;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.SaltSupplyConcentrationCollection.GWConcPestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.SaltSupplyConcentrationCollection.GWConcPestParamMethod;
      PestValues.FormulaErrorMessage :=
        Format('Invalid groundwater salt supply concentration formula for farm %0:s',
        [AFarm.FarmName]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;


//      Formula := SourceConcentration.GroundwaterConcentration;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        Format('Invalid groundwater salt supply concentration formula for farm %0:s',
//        [AFarm.FarmName]));

      PestValues.Formula := SourceConcentration.ExternalConcentration;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.SaltSupplyConcentrationCollection.ExtConcPestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.SaltSupplyConcentrationCollection.ExtConcPestParamMethod;
      PestValues.FormulaErrorMessage := Format('Invalid external salt supply concentration formula for farm %0:s',
        [AFarm.FarmName]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;



//      Formula := SourceConcentration.ExternalConcentration;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        Format('Invalid external salt supply concentration formula for farm %0:s',
//        [AFarm.FarmName]));
    end
    else
    begin
      WriteFloat(0);
      WriteFloat(0);
      WriteFloat(0);
      WriteFloat(0);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
    Index: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for Index := 1 to 4 do
              begin
                WriteFloat(0);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            SourceConcentration :=AFarm.SaltSupplyConcentrationCollection.
              ItemByStartTime(StartTime) as TSaltSupplyConcentrationItem;
              WriteItem(AFarm, SourceConcentration);
            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for Index := 1 to 4 do
            begin
              WriteFloat(0);
            end;
            Inc(FarmID);
            NewLine;
          end;
          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.SaltSupplyConcentrationCollection.Count > 0 then
          begin
            SourceConcentration := AFarm.SaltSupplyConcentrationCollection.First
              as TSaltSupplyConcentrationItem;
          end
          else
          begin
            SourceConcentration := nil;
          end;
          WriteItem(AFarm,  SourceConcentration);
          Inc(FarmID);
          NewLine;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FSalinityFlush.FarmSaltConcentrationsChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSaltSupplyConcentration;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: WBS_SUPPLY_SALT_CONCENTRATION';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: WBS_SUPPLY_SALT_CONCENTRATION';
  RequiredValues.ID := 'WBS_SUPPLY_SALT_CONCENTRATION';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FSalinityFlush.FarmSaltConcentrationsChoice.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSaltSupply;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.FarmSaltConcentrationsChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSaltSupplyConcentration);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.UpdateBarePrecipitationConsumptionFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateBarePrecipitationConsumptionFraction;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).TransientArrayBarePrecipitationConsumptionFractionDisplayUsed;
  UpdateRequirements.WriteLocation := wlBarePrecipitationConsumptionFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateBareRunoffFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
//var
//  DataSets: TList;
//  BareRunoffFraction: TModflowBoundaryDisplayTimeList;
//  TimeIndex: integer;
//  TimeListIndex: integer;
//  List: TValueCellList;
//  TimeList: TModflowBoundaryDisplayTimeList;
//  DataArray: TDataArray;
//  EfficiencyIndex: integer;
//  DataSetIndex: integer;
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateBareRunoffFraction;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).TransientArrayBareRunoffFractionDisplayUsed;
  UpdateRequirements.WriteLocation := wlBareRunoffFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

function TModflowFmp4Writer.TransientCropUsed(Sender: TObject): Boolean;
begin
  result := FLandUse.CropLocation = rstTransient;
end;

procedure TModflowFmp4Writer.UpdateConsumptiveUseDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateConsumptiveUse;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientConsumptiveUsearrayUsed;
  UpdateRequirements.WriteLocation := wlConsumptiveUse;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateCropCoefficientDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropCoefficient;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientCropCoefficientarrayUsed;
  UpdateRequirements.WriteLocation := wlCropCoefficient;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteLandUseOption;
begin
  case FLandUse.LandUseOption of
    luoSingle:
      begin
        WriteString('  SINGLE_LAND_USE_PER_CELL');
      end;
    luoMultiple:
      begin
        WriteString('  MULTIPLE_LAND_USE_PER_CELL');
      end;
  end;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteLandUsePrintOptions;
var
  PrintOption: TLandUsePrint;
  OutputFile: string;
  CellsToPrint: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  OutputFileFormat: string;
begin
  if Model.ModflowOutputControl.PrintInputArrays
    or Model.ModflowOutputControl.PrintInputCellLists then
  begin
    WriteString('  PRINT INPUT ');
    OutputFile := ChangeFileExt(FInputFileName, '.Crop_Input');
    WriteString(ExtractFileName(OutputFile));
    Model.AddModelOutputFile(OutputFile);
    NewLine;
  end;
  for PrintOption in FLandUse.LandUsePrints do
  begin
    case PrintOption of
      lupPrintByFarm:
        begin
          WriteString('  PRINT BYWBS ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_ByWBS');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintByFarmByCrop:
        begin
          WriteString('  PRINT BYWBS_BYCROP ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_ByWBS_ByCrop');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintByCrop:
        begin
          WriteString('  PRINT BYCROP ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_ByCrop');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintBare:
        begin
          WriteString('  PRINT BARE ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_Bare');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintByCell:
        begin
          WriteString('  PRINT ALL ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_All');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintByCellVerbose:
        begin
          WriteString('  PRINT ALL_VERBOSE ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_All_VERBOSE');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintET_ByFarmByCrop:
        begin
          WriteString('  PRINT ET_BYWBS_BYCROP ');
          OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_ET_ByWBS_ByCrop');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      lupPrintRowCol:
        begin
          CellsToPrint := Model.DataArrayManager.GetDataSetByName(KLandUseCellsToPrint);
          CellsToPrint.Initialize;
          if CellsToPrint.IsUniform = iuTrue then
          begin
            if CellsToPrint.UniformBooleanValue then
            begin
              if (lupPrintByCell in FLandUse.LandUsePrints)  then
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrLANDUSEPRINTROWC,
                  Format(PrintRowColWarning1, [KLandUseCellsToPrint]), nil);
              end
              else
              begin
                WriteString('  PRINT ALL ');
                OutputFile := ChangeFileExt(FInputFileName, '.CropOutput_All');
                WriteString(ExtractFileName(OutputFile));
                Model.AddModelOutputFile(OutputFile);
                NewLine;
                frmErrorsAndWarnings.AddWarning(Model, StrLANDUSEPRINTROWC,
                  Format(PrintRowColWarning2, [KLandUseCellsToPrint]), nil);
              end;
            end
            else
            begin
                frmErrorsAndWarnings.AddWarning(Model, StrLANDUSEPRINTROWC,
                  Format(PrintRowColWarning3, [KLandUseCellsToPrint]), nil);
            end;
          end
          else
          begin
            OutputFileFormat := ChangeFileExt(FInputFileName, '.CropOutput_Row_%0:d_Column_%1:d');
            for RowIndex := 0 to CellsToPrint.RowCount - 1 do
            begin
              for ColIndex := 0 to CellsToPrint.ColumnCount - 1 do
              begin
                if CellsToPrint.BooleanData[0, RowIndex, ColIndex] then
                begin
                  WriteString('  PRINT ROW_COLUMN ');
                  WriteInteger(RowIndex+1);
                  WriteInteger(ColIndex+1);
                  OutputFile := Format(OutputFileFormat, [RowIndex+1, ColIndex+1]);
                  Model.AddModelOutputFile(OutputFile);
                  WriteString(' ' + ExtractFileName(OutputFile));
                  NewLine;
                end;
              end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteLeachList(RequiredValues: TRequiredValues;
  AFileName: string; GetCollection: TGetCropCollection;
  const ErrorMessage: string);
var
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  procedure WriteOwhmItem(Crop: TCropItem; OwhmItem: TLeachItem);
  var
    Formula: string;
  begin
    if OwhmItem <> nil then
    begin
      case OwhmItem.LeachChoice of
        lcValue:
          begin
            Formula := OwhmItem.OwhmValue;
            WriteFloatValueFromGlobalFormula(Formula, Crop,
              ErrorMessage + Crop.CropName);
          end;
        lcRhoades:
          begin
            WriteString(' RHOADES')
          end;
        lcNone:
          begin
            WriteString(' None')
          end;
        lcCustomFormula:
          begin
            WriteString(' ' + OwhmItem.CustomFormula)
          end;
        else
          begin
            Assert(False)
          end;
      end;
    end
    else
    begin
      WriteString(' None')
    end;
  end;
  procedure WriteTransientData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
    StartTime: Double;
    ACrop: TCropItem;
    OwhmCollection: TLeachCollection;
    OwhmItem: TLeachItem;
  begin
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      WriteCommentLine(Format(StrStressPeriodD, [TimeIndex + 1]));
      StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;
      for CropIndex := 0 to Model.FmpCrops.Count - 1 do
      begin
        ACrop := Model.FmpCrops[CropIndex];
        WriteInteger(CropIndex + 1);
        OwhmCollection := GetCollection(ACrop) as TLeachCollection;
        OwhmItem := OwhmCollection.ItemByStartTime(StartTime) as TLeachItem;
        WriteOwhmItem(ACrop, OwhmItem);
        NewLine;
      end;
    end;
  end;
  procedure WriteStaticData;
  var
    CropIndex: Integer;
    ACrop: TCropItem;
    OwhmCollection: TLeachCollection;
    OwhmItem: TLeachItem;
  begin
    for CropIndex := 0 to Model.FmpCrops.Count - 1 do
    begin
      ACrop := Model.FmpCrops[CropIndex];
      WriteInteger(CropIndex + 1);
      OwhmCollection := GetCollection(ACrop) as TLeachCollection;
      if OwhmCollection.Count > 0 then
      begin
        OwhmItem := OwhmCollection.First as TLeachItem;
      end
      else
      begin
        OwhmItem := nil;
      end;
      WriteOwhmItem(ACrop, OwhmItem);
      NewLine;
    end;
  end;
begin
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues, UnitConversionScaleFactor,
    ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    try
      FWriteLocation := RequiredValues.WriteLocation;
      if RequiredValues.WriteTransientData then
      begin
        WriteTransientData;
      end
      else
      begin
        WriteStaticData;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteScaleFactorsID_andOption(
  RequiredValues: TRequiredValues; UnitConversionScaleFactor: string;
  ExternalScaleFileName: string);
begin
  if UnitConversionScaleFactor <> '' then
  begin
    WriteString('  INTERNAL SF ');
    WriteString(UnitConversionScaleFactor);
    NewLine;
  end;
  if ExternalScaleFileName <> '' then
  begin
    WriteString('  SFAC OPEN/CLOSE ');
    WriteString(ExternalScaleFileName);
    NewLine;
  end;
  WriteString('  ');
  WriteString(RequiredValues.ID);
  WriteString(' ');
  WriteString(RequiredValues.Option);
  WriteString(' ');
end;

procedure TModflowFmp4Writer.WriteSemiRoutedDelivery;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: double;
  AFarm: TFarm;
  ADelivery: TSemiRoutedDeliveriesAndRunoffItem;
  ISRD: Integer;
  SrdCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
  SegmentReach: TSegmentReach;
  procedure WriteDelivery(ADelivery: TSemiRoutedDeliveriesAndRunoffItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if ADelivery = nil then
    begin
      WriteInteger(0);
      WriteInteger(0);
      WriteInteger(0);
    end
    else
    begin
      SegmentReach := ADelivery.LinkedStream.SegmentReach;
      WriteInteger(SegmentReach.Segment);
      WriteInteger(SegmentReach.Reach);


      if ADelivery.Frac <> '' then
      begin
        PestValues.Formula := ADelivery.Frac;

        PestValues.PestName := '';
        PestValues.PestSeriesName := SrdCollection.PestSeriesParameter;
        PestValues.PestSeriesMethod := SrdCollection.PestParamMethod;
        PestValues.FormulaErrorMessage := 'Invalid semi-routed delivery fraction';
        PestValues.ErrorObjectName := AFarm.FarmName;

        AdjustFormulaForPest(PestValues);

        if WritingTemplate and PestValues.ParameterUsed then
        begin
          Value := GetFormulaValue(PestValues);

          WritePestTemplateFormula(Value, PestValues.PestName,
            PestValues.PestSeriesName, PestValues.PestSeriesMethod,
            nil);
        end
        else
        begin
          WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
            PestValues.FormulaErrorMessage);
        end;
      end
      else
      begin
        WriteInteger(-1);
      end;
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
  SrdIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          ISRD := 1;
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

//          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            if AFarm.MultiSrDeliveries.Count > 0 then
            begin
              for SrdIndex := 0 to AFarm.MultiSrDeliveries.Count - 1 do
              begin
                SrdCollection := AFarm.MultiSrDeliveries[SrdIndex].SemiRouted;

                WriteInteger(ISRD);
                ADelivery := SrdCollection.ItemByStartTime(
                  StartTime) as TSemiRoutedDeliveriesAndRunoffItem;
                WriteInteger(AFarm.FarmId);
                WriteDelivery(ADelivery);
                NewLine;
                Inc(ISRD);
              end;
            end
            else
            begin
              WriteInteger(ISRD);
              WriteInteger(AFarm.FarmId);
              WriteInteger(0);
              WriteInteger(0);
              WriteInteger(0);
              NewLine;
              Inc(ISRD);
            end;
          end;
        end;
      end
      else
      begin
        ISRD := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          if AFarm.MultiSrDeliveries.Count > 0 then
          begin
            for SrdIndex := 0 to AFarm.MultiSrDeliveries.Count - 1 do
            begin
              SrdCollection := AFarm.MultiSrDeliveries[SrdIndex].SemiRouted;

              WriteInteger(ISRD);

              if SrdCollection.Count > 0 then
              begin
                ADelivery := SrdCollection.First as TSemiRoutedDeliveriesAndRunoffItem;
              end
              else
              begin
                ADelivery := nil;
              end;
              WriteInteger(AFarm.FarmId);
              WriteDelivery(ADelivery);
              NewLine;
              Inc(ISRD);
            end;
          end
          else
          begin
            WriteInteger(ISRD);
            WriteInteger(AFarm.FarmId);
            WriteInteger(0);
            WriteInteger(0);
            WriteInteger(0);
            NewLine;
            Inc(ISRD);
          end;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if (FSurfaceWater4.Semi_Routed_Delivery.FarmOption = foNotUsed) then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSemiRouteDelivery;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: SEMI_ROUTED_DELIVERY';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: SEMI_ROUTED_DELIVERY';
  RequiredValues.ID := 'SEMI_ROUTED_DELIVERY';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSurfaceWater4.Semi_Routed_Delivery.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSemirouted;
  RequiredValues.Option := '';

  RequiredValues.FarmProperty := FSurfaceWater4.Semi_Routed_Delivery;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSemiRouteDelivery);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSemiRoutedDeliveryClosureTolerance;
begin
  WriteString('  SEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE');
  WriteFloat(FSurfaceWater4.Semi_Routed_Delivery_Closure_Tolerance);
  NewLine;
end;

procedure TModflowFmp4Writer.WriteSemiRoutedDeliveryLowerLimit;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: double;
  AFarm: TFarm;
  ADelivery: TSemiRoutedDeliveriesAndRunoffItem;
  ISRD: Integer;
  SrdCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
  procedure WriteDeliveryItem(ADelivery: TSemiRoutedDeliveriesAndRunoffItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if ADelivery = nil then
    begin
      WriteFloat(RequiredValues.DefaultValue);
    end
    else
    begin

      PestValues.Formula := ADelivery.LowerLimit;

      PestValues.PestName := '';
      PestValues.PestSeriesName := SrdCollection.LowerLimitPestSeriesParameter;
      PestValues.PestSeriesMethod := SrdCollection.LowerLimitPestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid semi-routed delivery lower limit';
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;

//      WriteFloatValueFromGlobalFormula(ADelivery.LowerLimit,
//        AFarm, 'Invalid semi-routed delivery lower limit');
    end;
    NewLine;
    Inc(ISRD);
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    SrdIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          ISRD := 1;
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            if AFarm.MultiSrDeliveries.Count > 0 then
            begin
              for SrdIndex := 0 to AFarm.MultiSrDeliveries.Count - 1 do
              begin
                SrdCollection := AFarm.MultiSrDeliveries[SrdIndex].SemiRouted;

                WriteInteger(ISRD);
                ADelivery := SrdCollection.ItemByStartTime(
                  StartTime) as TSemiRoutedDeliveriesAndRunoffItem;
                WriteDeliveryItem(ADelivery);
              end;
            end
            else
            begin
              WriteInteger(ISRD);
              WriteFloat(RequiredValues.DefaultValue);
              NewLine;
              Inc(ISRD);
            end;
          end;
        end;
      end
      else
      begin
        ISRD := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
            for SrdIndex := 0 to AFarm.MultiSrDeliveries.Count - 1 do
            begin
              SrdCollection := AFarm.MultiSrDeliveries[SrdIndex].SemiRouted;

              WriteInteger(ISRD);

              if SrdCollection.Count > 0 then
              begin
                ADelivery := SrdCollection.First as TSemiRoutedDeliveriesAndRunoffItem;
              end
              else
              begin
                ADelivery := nil;
              end;
              WriteDeliveryItem(ADelivery);
            end;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if (FSurfaceWater4.Semi_Routed_Delivery.FarmOption = foNotUsed)
   or (FSurfaceWater4.SemiRoutedDeliveryLowerLimit.FarmOption = foNotUsed) then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSemiRouteDeliveryLowerLimit;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: SEMI_ROUTED_DELIVERY_LOWER_LIMIT';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: SEMI_ROUTED_DELIVERY_LOWER_LIMIT';
  RequiredValues.ID := 'SEMI_ROUTED_DELIVERY_LOWER_LIMIT';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSurfaceWater4.Semi_Routed_Delivery.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSemirouted2;
  RequiredValues.Option := '';

  RequiredValues.FarmProperty := FSurfaceWater4.SemiRoutedDeliveryLowerLimit;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSemiRouteDeliveryLowerLimit);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSemiRoutedDeliveryUpperLimit;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: double;
  AFarm: TFarm;
  ADelivery: TSemiRoutedDeliveriesAndRunoffItem;
  ISRD: Integer;
  SrdCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
  procedure WriteDelivery(ADelivery: TSemiRoutedDeliveriesAndRunoffItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if ADelivery = nil then
    begin
      WriteFloat(RequiredValues.DefaultValue);
    end
    else
    begin
      PestValues.Formula := ADelivery.UpperLimit;

      PestValues.PestName := '';
      PestValues.PestSeriesName := SrdCollection.UpperLimitPestSeriesParameter;
      PestValues.PestSeriesMethod := SrdCollection.UpperLimitPestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid semi-routed delivery upper limit';
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
    end;
    NewLine;
    Inc(ISRD);
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    SrdIndex: Integer;
    begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          ISRD := 1;
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            if AFarm.MultiSrDeliveries.Count > 0 then
            begin
              for SrdIndex := 0 to AFarm.MultiSrDeliveries.Count - 1 do
              begin
                SrdCollection := AFarm.MultiSrDeliveries[SrdIndex].SemiRouted;

                WriteInteger(ISRD);
                ADelivery := SrdCollection.ItemByStartTime(
                  StartTime) as TSemiRoutedDeliveriesAndRunoffItem;
                WriteDelivery(ADelivery);
              end;
            end
            else
            begin
              WriteInteger(ISRD);
              WriteFloat(RequiredValues.DefaultValue);
              NewLine;
              Inc(ISRD);
            end;
          end;
        end;
      end
      else
      begin
        ISRD := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
            for SrdIndex := 0 to AFarm.MultiSrDeliveries.Count - 1 do
            begin
              SrdCollection := AFarm.MultiSrDeliveries[SrdIndex].SemiRouted;

              WriteInteger(ISRD);

              if SrdCollection.Count > 0 then
              begin
                ADelivery := SrdCollection.First as TSemiRoutedDeliveriesAndRunoffItem;
              end
              else
              begin
                ADelivery := nil;
              end;
              WriteDelivery(ADelivery);
            end;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if (FSurfaceWater4.Semi_Routed_Delivery.FarmOption = foNotUsed)
   or (FSurfaceWater4.SemiRoutedDeliveryUpperLimit.FarmOption = foNotUsed) then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSemiRouteDeliveryUpperLimit;
  RequiredValues.DefaultValue := 1E100;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: SEMI_ROUTED_DELIVERY_UPPER_LIMIT';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: SEMI_ROUTED_DELIVERY_UPPER_LIMIT';
  RequiredValues.ID := 'SEMI_ROUTED_DELIVERY_UPPER_LIMIT';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSurfaceWater4.Semi_Routed_Delivery.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSemiroutedUpper;
  RequiredValues.Option := '';

  RequiredValues.FarmProperty := FSurfaceWater4.SemiRoutedDeliveryUpperLimit;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSemiRouteDeliveryUpperLimit);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSemiRoutedReturn;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: double;
  AFarm: TFarm;
  ADelivery: TSemiRoutedDeliveriesAndRunoffItem;
  ISRD: Integer;
  SrdCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
  SegmentReach: TSegmentReach;
  procedure WriteDelivery(ADelivery: TSemiRoutedDeliveriesAndRunoffItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if ADelivery = nil then
    begin
      WriteInteger(AFarm.FarmId);
      WriteInteger(0);
      WriteInteger(0);
      WriteInteger(0);
    end
    else
    begin
      WriteInteger(AFarm.FarmId);

      SegmentReach := ADelivery.LinkedStream.SegmentReach;
      WriteInteger(SegmentReach.Segment);
      WriteInteger(SegmentReach.Reach);

      PestValues.Formula := ADelivery.Frac;

      PestValues.PestName := '';
      PestValues.PestSeriesName := SrdCollection.PestSeriesParameter;
      PestValues.PestSeriesMethod := SrdCollection.PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid semi-routed return fraction';
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
    end;
    NewLine;
    Inc(ISRD);
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    SrdIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          ISRD := 1;
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            if AFarm.MultiSrReturns.Count > 0 then
            begin
              for SrdIndex := 0 to AFarm.MultiSrReturns.Count - 1 do
              begin
                SrdCollection := AFarm.MultiSrReturns[SrdIndex].SemiRouted;

                WriteInteger(ISRD);
                ADelivery := SrdCollection.ItemByStartTime(
                  StartTime) as TSemiRoutedDeliveriesAndRunoffItem;
                WriteDelivery(ADelivery);
              end;
            end
            else
            begin
              WriteInteger(ISRD);
              WriteInteger(AFarm.FarmId);
              WriteInteger(0);
              WriteInteger(0);
              WriteInteger(0);
              NewLine;
              Inc(ISRD);
            end;
          end;
        end;
      end
      else
      begin
        ISRD := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          if AFarm.MultiSrReturns.Count > 0 then
          begin
            for SrdIndex := 0 to AFarm.MultiSrReturns.Count - 1 do
            begin
              SrdCollection := AFarm.MultiSrReturns[SrdIndex].SemiRouted;

              WriteInteger(ISRD);

              if SrdCollection.Count > 0 then
              begin
                ADelivery := SrdCollection.First as TSemiRoutedDeliveriesAndRunoffItem;
              end
              else
              begin
                ADelivery := nil;
              end;
              WriteDelivery(ADelivery);
            end;
          end
            else
            begin
              WriteInteger(ISRD);
              WriteInteger(AFarm.FarmId);
              WriteInteger(0);
              WriteInteger(0);
              WriteInteger(0);
              NewLine;
              Inc(ISRD);
            end;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
end;
begin
  if (FSurfaceWater4.SemiRoutedReturn.FarmOption = foNotUsed) then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSemiRouteReturn;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: SEMI_ROUTED_RETURN';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: SEMI_ROUTED_RETURN';
  RequiredValues.ID := 'SEMI_ROUTED_RETURN';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSurfaceWater4.SemiRoutedReturn.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSemiroutedRet;
  RequiredValues.Option := '';

  RequiredValues.FarmProperty := FSurfaceWater4.SemiRoutedReturn;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSemiRouteReturn);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

function TModflowFmp4Writer.GetSalinityAppliedWaterCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.SalinityAppliedWater;
end;

function TModflowFmp4Writer.GetSalinityToleranceCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.SalinityToleranceCollection;
end;

procedure TModflowFmp4Writer.GetScaleFactorsAndExternalFile(
  RequiredValues: TRequiredValues; var UnitConversionScaleFactor: string;
  var ExternalFileName: string; var ExternalScaleFileName: string);
begin
  if RequiredValues.FarmProperty <> nil then
  begin
    UnitConversionScaleFactor := RequiredValues.FarmProperty.UnitConversionScaleFactor;
    ExternalFileName := RequiredValues.FarmProperty.ExternalFileName;
    ExternalScaleFileName := RequiredValues.FarmProperty.ExternalScaleFileName;
  end
  else
  begin
    UnitConversionScaleFactor := '';
    ExternalFileName := '';
    ExternalScaleFileName := '';
  end;
end;

function TModflowFmp4Writer.GetSWLossFracPrecipCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.SWLossFractionPrecipCollection;
end;

procedure TModflowFmp4Writer.UpdateCropIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropID;
  UpdateRequirements.TransientDataUsed := TransientCropUsed;
  UpdateRequirements.WriteLocation := wlCID;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateDirectRechargeDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateDirectRecharge;
  UpdateRequirements.TransientDataUsed := FClimatePackage.
    TransientDirectRechargeUsed;
  UpdateRequirements.WriteLocation := wlDirectRecharge;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateDisplay(
  UpdateRequirements: TUpdateRequirements);
var
  DataSets: TList;
  ATimeList: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  Index: integer;
  DataSetIndex: integer;
  BoundaryLists: TList;
begin
  EvaluateActiveCells;
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not UpdateRequirements.TransientDataUsed(self) then
    begin
      UpdateNotUsedDisplay(UpdateRequirements.TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      UpdateRequirements.EvaluateProcedure;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      ATimeList := UpdateRequirements.TimeLists[0];
      for TimeIndex := 0 to ATimeList.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to UpdateRequirements.TimeLists.Count - 1 do
        begin
          TimeList := UpdateRequirements.TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        BoundaryLists := GetTransientList(UpdateRequirements.WriteLocation);
        for Index := 0 to BoundaryLists.Count - 1 do
        begin
          List := BoundaryLists[Index];
          UpdateCellDisplay(List, DataSets, [], nil, []);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(UpdateRequirements.TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmp4Writer.UpdateEfficiencyDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateEfficiency;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).
    FarmTransientArrayEfficiencyUsed;
  UpdateRequirements.WriteLocation := wlEfficiency;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateEfficiencyImprovementDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateEfficiencyImprovement;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).
    TransientArrayEfficiencyImprovementUsed;
  UpdateRequirements.WriteLocation := wlEfficiencyImprovement;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateEvapBareDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateBareEvap;
  UpdateRequirements.TransientDataUsed := FClimatePackage.
    TransientBareEvapUsed;
  UpdateRequirements.WriteLocation := wlBareEvap;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateEvaporationIrrigationFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateEvaporationIrrigationFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientEvaporationIrrigationFractionarrayUsed;
  UpdateRequirements.WriteLocation := wlEvaporationIrrigationFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateFarmIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateFarmID;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).
    TransientFarmIdUsed;
  UpdateRequirements.WriteLocation := wlFID;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateFractionOfIrrigToSurfaceWaterDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateFractionOfIrrigToSurfaceWater;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientFractionOfIrrigToSurfaceWaterarrayUsed;
  UpdateRequirements.WriteLocation := wlFractionOfIrrigToSurfaceWater;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateFractionOfPrecipToSurfaceWaterDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateFractionOfPrecipToSurfaceWater;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientFractionOfPrecipToSurfaceWaterarrayUsed;
  UpdateRequirements.WriteLocation := wlFractionOfPrecipToSurfaceWater;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateHasSalinityDemandDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropHasSalinityDemand;
  UpdateRequirements.TransientDataUsed := FSalinityFlush.
    TransientCropHasSalinityDemandarrayUsed;
  UpdateRequirements.WriteLocation := wlCropHasSalinityDemand;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateIrrigationDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateIrrigation;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientIrrigationarrayUsed;
  UpdateRequirements.WriteLocation := wlIrrigation;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateLandUseAreaFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateLandUseAreaFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientLandUseAreaFractionarrayUsed;
  UpdateRequirements.WriteLocation := wlLandUseAreaFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultAddedDemandDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateAddedDemand;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientAddedDemandMultArrayUsed;
  UpdateRequirements.WriteLocation := wlAddedDemand;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultConsumptiveUseDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateConsumptiveUse;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientConsumptiveUseMultArrayUsed;
  UpdateRequirements.WriteLocation := wlConsumptiveUse;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultCropCoefficientDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropCoefficient;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientCropCoefficientMultArrayUsed;
  UpdateRequirements.WriteLocation := wlCropCoefficient;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultEvaporationIrrigationFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateEvaporationIrrigationFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientEvaporationIrrigationFractionMultArrayUsed;
  UpdateRequirements.WriteLocation := wlEvaporationIrrigationFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultFractionOfIrrigToSurfaceWaterDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateFractionOfIrrigToSurfaceWater;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientFractionOfIrrigToSurfaceWaterMultArrayUsed;
  UpdateRequirements.WriteLocation := wlFractionOfIrrigToSurfaceWater;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultFractionOfPrecipToSurfaceWaterDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateFractionOfPrecipToSurfaceWater;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientFractionOfPrecipToSurfaceWaterMultArrayUsed;
  UpdateRequirements.WriteLocation := wlFractionOfPrecipToSurfaceWater;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultHasSalinityDemandDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropHasSalinityDemand;
  UpdateRequirements.TransientDataUsed := FSalinityFlush.
    TransientCropHasSalinityDemandMultArrayUsed;
  UpdateRequirements.WriteLocation := wlCropHasSalinityDemand;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultIrrigationDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateIrrigation;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientIrrigationMultArrayUsed;
  UpdateRequirements.WriteLocation := wlIrrigation;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultLandUseAreaFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateLandUseAreaFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientLandUseAreaFractionMultArrayUsed;
  UpdateRequirements.WriteLocation := wlLandUseAreaFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultRootDepthDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateRootDepth;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientRootDepthMultArrayUsed;
  UpdateRequirements.WriteLocation := wlRootDepth;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateMultTranspirationFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateTranspirationFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientTranspirationFractionMultArrayUsed;
  UpdateRequirements.WriteLocation := wlTranspirationFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateNrdInfilLocationDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateNrdInfilLocation;
  UpdateRequirements.TransientDataUsed := FSurfaceWater4.
    TransientNrdInfilLocationUsed;
  UpdateRequirements.WriteLocation := wlNrdInfilLoc;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdatePrecipDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluatePrecip;
  UpdateRequirements.TransientDataUsed := FClimatePackage.TransientPrecipUsed;
  UpdateRequirements.WriteLocation := wlPFLX;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdatePrecipPotConsumptionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluatePrecipPotConsumption;
  UpdateRequirements.TransientDataUsed := FClimatePackage.
    TransientPrecipPotConsumptionUsed;
  UpdateRequirements.WriteLocation := wlPrecipPotConsumption;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteFmpArrayData(AFileName: string; RequiredValues: TRequiredValues);
var
  DataArray: TDataArray;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  UnitConversionScaleFactor: string;
  IntValue: Integer;
  BoolValue: Boolean;
  RealValue: Double;
  UniformValue: Boolean;
begin
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    if not WritingTemplate or (RequiredValues.WriteLocation = wlMain) then
    begin
      WriteScaleFactorsID_andOption(RequiredValues,
        UnitConversionScaleFactor, ExternalScaleFileName);
      WriteString('TRANSIENT ARRAY DATAFILE ');
    end;
    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      if not WritingTemplate or (RequiredValues.WriteLocation = wlMain) then
      begin
        WriteString(ExtractFileName(AFileName));
        NewLine;
      end;
      WriteTransientFmpArrayData(RequiredValues);
    end;
  end
  else
  begin
    if not WritingTemplate or (RequiredValues.WriteLocation = wlMain) then
    begin
      WriteScaleFactorsID_andOption(RequiredValues,
        UnitConversionScaleFactor, ExternalScaleFileName);
    end;

    if ExternalFileName <> '' then
    begin
      WriteString('STATIC ARRAY DATAFILE ');
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(RequiredValues.StaticDataName);
      Assert(DataArray <> nil);
    end;

    UniformValue := False;
    if (DataArray <> nil) then
    begin
      DataArray.Initialize;
      if DataArray.IsUniform = iuTrue then
      begin
        UniformValue := True;
      end
      else
      begin
        IntValue := 0;
        BoolValue := False;
        RealValue := 0;
        UniformValue := CheckArrayUniform(DataArray, IntValue, BoolValue, RealValue);
      end;
    end;
    if (DataArray <> nil) and UniformValue
       and not DataArray.PestParametersUsed then
    begin
      WriteString('STATIC CONSTANT ');
      if DataArray.DataType = rdtDouble then
      begin
        WriteFloat(DataArray.RealData[0,0,0]);
      end
      else
      begin
        WriteInteger(DataArray.IntegerData[0,0,0]);
      end;
      NewLine;
    end
    else
    begin
      if RequiredValues.WriteLocation <> wlMain then
      begin
        if not WritingTemplate or (RequiredValues.WriteLocation = wlMain) then
        begin
          WriteString('STATIC ARRAY DATAFILE ');
        end;
        if ExternalFileName <> '' then
        begin
          WriteString(ExternalFileName);
          NewLine;
        end
        else
        begin
          if not WritingTemplate or (RequiredValues.WriteLocation = wlMain) then
          begin
            WriteString(ExtractFileName(AFileName));
            NewLine;
          end;
          FWriteLocation := RequiredValues.WriteLocation;
          try
            WriteArray(DataArray, 0, RequiredValues.ErrorID, '', RequiredValues.ID, False, False);
            if Assigned(RequiredValues.CheckProcedure) then
            begin
              RequiredValues.CheckProcedure(DataArray, RequiredValues.CheckError);
            end;
          finally
            FWriteLocation := wlMain;
          end;

        end;
      end
      else
      begin
        WriteString('STATIC ARRAY INTERNAL');
        NewLine;
        WriteArray(DataArray, 0, RequiredValues.ErrorID, '', RequiredValues.ID, False, False);
        if Assigned(RequiredValues.CheckProcedure) then
        begin
          RequiredValues.CheckProcedure(DataArray, RequiredValues.CheckError);
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteFractionOfIrrigToSurfaceWater;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  ACrop: TCropItem;
  IrregationItem: TCropIrrigationItem;
  IrrigationTypes: TIrrigationCollection;
  IrrigationType: TIrrigationItem;
  EvapFracItem: TEvapFractionItem;
  CropIndex: Integer;
  procedure WriteIrrigationItem(ACrop: TCropItem; IrregationItem: TCropIrrigationItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if IrregationItem <> nil then
    begin
      PestValues.Formula := IrregationItem.SurfaceWaterLossFractionIrrigation;

      PestValues.PestName := '';
      PestValues.PestSeriesName := ACrop.IrrigationCollection.SurfaceWaterLossFractionIrrigationPestSeriesParameter;
      PestValues.PestSeriesMethod := ACrop.IrrigationCollection.SurfaceWaterLossFractionIrrigationPestParamMethod;
      PestValues.FormulaErrorMessage :=  'Invalid Surface Water Loss Fraction Irrigation in crop ' + ACrop.CropName;
      PestValues.ErrorObjectName := ACrop.CropName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, ACrop,
          PestValues.FormulaErrorMessage);
      end;

//      Formula := IrregationItem.SurfaceWaterLossFractionIrrigation;
//      WriteFloatValueFromGlobalFormula(Formula, ACrop,
//        'Invalid Surface Water Loss Fraction Irrigation in crop ' + ACrop.CropName);
    end
    else
    begin
      WriteFloat(0.0);
    end;
    NewLine;
  end;
  procedure WriteEvapFractItem(IrrigationType: TIrrigationItem; EvapFracItem: TEvapFractionItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if EvapFracItem <> nil then
    begin

      PestValues.Formula := EvapFracItem.SurfaceWaterLossFractionIrrigation;

      PestValues.PestName := '';
      PestValues.PestSeriesName := IrrigationType.EvapFraction.SurfaceWaterLossFractionPestSeriesParameter;
      PestValues.PestSeriesMethod := IrrigationType.EvapFraction.SurfaceWaterLossFractionPestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid Surface Water Loss Fraction Irrigation in irrigation type ' + IrrigationType.Name;
      PestValues.ErrorObjectName := IrrigationType.Name;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, IrrigationType,
          PestValues.FormulaErrorMessage);
      end;
    end
    else
    begin
      WriteFloat(0.0);
    end;
    NewLine;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
    IrrIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if FLandUse.FractionOfPrecipToSurfaceWaterIrrigationOption = ioByCrop then
      begin
        if RequiredValues.WriteTransientData then
        begin
          for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
          begin
            WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

            StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

            for CropIndex := 0 to Model.FmpCrops.Count - 1 do
            begin
              ACrop := Model.FmpCrops[CropIndex];
              WriteInteger(CropIndex+1);
              IrregationItem := ACrop.IrrigationCollection.
                ItemByStartTime(StartTime) as TCropIrrigationItem;
              WriteIrrigationItem(ACrop, IrregationItem);
            end;
          end;
        end
        else
        begin
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            ACrop := Model.FmpCrops[CropIndex];
            WriteInteger(CropIndex + 1);
            if ACrop.IrrigationCollection.Count > 0 then
            begin
              IrregationItem := ACrop.IrrigationCollection.First as TCropIrrigationItem;
            end
            else
            begin
              IrregationItem := nil;
            end;
            WriteIrrigationItem(ACrop, IrregationItem);
          end;
        end;
      end
      else
      begin
        IrrigationTypes := Model.IrrigationTypes;
        if RequiredValues.WriteTransientData then
        begin
          for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
          begin
            WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

            StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

            for IrrIndex := 0 to IrrigationTypes.Count - 1 do
            begin
              IrrigationType := IrrigationTypes[IrrIndex];
              WriteInteger(IrrIndex+1);
              EvapFracItem := IrrigationType.EvapFraction.
                ItemByStartTime(StartTime) as TEvapFractionItem;
              WriteEvapFractItem(IrrigationType, EvapFracItem);
            end;
          end;
        end
        else
        begin
          for IrrIndex := 0 to IrrigationTypes.Count - 1 do
          begin
            IrrigationType := IrrigationTypes[IrrIndex];
            WriteInteger(IrrIndex + 1);
            if IrrigationType.EvapFraction.Count > 0 then
            begin
              EvapFracItem := IrrigationType.EvapFraction.First as TEvapFractionItem;
            end
            else
            begin
              EvapFracItem := nil;
            end;
            WriteEvapFractItem(IrrigationType, EvapFracItem);
          end;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FLandUse.FractionOfIrrigationToSurfaceWater.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateFractionOfIrrigToSurfaceWater;

  RequiredValues.WriteLocation := wlFractionOfIrrigToSurfaceWater;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: SURFACEWATER_LOSS_FRACTION_IRRIGATION';
  RequiredValues.ErrorID := 'FMP LAND_USE: SURFACEWATER_LOSS_FRACTION_IRRIGATION';
  RequiredValues.ID := 'SURFACEWATER_LOSS_FRACTION_IRRIGATION';
  RequiredValues.StaticDataName := KFractionOfIrrigToSurfaceWater;
  RequiredValues.WriteTransientData :=
    (FLandUse.FractionOfIrrigationToSurfaceWater.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidFractionOf;
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
//  RequiredValues.Option := '';
  if FLandUse.FractionOfPrecipToSurfaceWaterIrrigationOption = ioByIrrigate then
  begin
    RequiredValues.Option := 'BY_IRRIGATE ';
  end
  else
  begin
    RequiredValues.Option := 'BY_CROP ';
  end;
  RequiredValues.FarmProperty := FLandUse.FractionOfIrrigationToSurfaceWater;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlFractionOfIrrigToSurfaceWater);
  end;

  if (FLandUse.FractionOfIrrigationToSurfaceWater.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].SWLossFractionIrrigationDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);
//    WriteString(RequiredValues.Option);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteFractionOfPrecipToSurfaceWater;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.FractionOfPrecipToSurfaceWater.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateFractionOfPrecipToSurfaceWater;

  RequiredValues.WriteLocation := wlFractionOfPrecipToSurfaceWater;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: SURFACEWATER_LOSS_FRACTION_PRECIPITATION';
  RequiredValues.ErrorID := 'FMP LAND_USE: SURFACEWATER_LOSS_FRACTION_PRECIPITATION';
  RequiredValues.ID := 'SURFACEWATER_LOSS_FRACTION_PRECIPITATION';
  RequiredValues.StaticDataName := KFractionOfPrecipToSurfaceWater;
  RequiredValues.WriteTransientData :=
    (FLandUse.FractionOfPrecipToSurfaceWater.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidFractionOf2;
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.FractionOfPrecipToSurfaceWater;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlFractionOfPrecipToSurfaceWater);
  end;

  if (FLandUse.FractionOfPrecipToSurfaceWater.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].SWLossFractionPrecipDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetSWLossFracPrecipCollection,
      StrInvalidSurfaceWate);
  end;
end;

procedure TModflowFmp4Writer.WriteRoutedReturn;
begin
  if FSurfaceWater4.RoutedReturn.FarmOption = foNotUsed then
  begin
    Exit;
  end;
  case FSurfaceWater4.ReturnOption of
    roNonDiversion: 
      begin
        WriteString('  ROUTED_RETURN_ANY_NON_DIVERSION_REACH');
      end;
    foAny:
      begin
        WriteString('  ROUTED_RETURN_ANY_REACH');
      end;
  end;
  NewLine;
end;

procedure TModflowFmp4Writer.EvaluateTransientArrayData(WriteLocation: TWriteLocation);
var
  TransList: TList;
  EmptyParamList: TStringList;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TModflowBoundary;
begin
  TransList := GetTransientList(WriteLocation);
  frmErrorsAndWarnings.BeginUpdate;
  EmptyParamList := TStringList.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := GetFmpBoundary(ScreenObject, WriteLocation);
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(TransList, EmptyParamList, Model, self);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmp4Writer.EvaluateTranspirationFraction;
begin
  if FLandUse.TransientTranspirationFractionarrayUsed(nil)
    or FLandUse.TransientTranspirationFractionMultArrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Transpiration Fraction value');
    EvaluateTransientArrayData(wlTranspirationFraction);
  end;
end;

function TModflowFmp4Writer.TransientRefEtUsed(Sender: TObject): Boolean;
begin
  result := FClimatePackage.ReferenceET.FarmOption = foTransient;
end;

procedure TModflowFmp4Writer.UpdateRefEtDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateReferenceET;
  UpdateRequirements.TransientDataUsed := TransientRefEtUsed;
  UpdateRequirements.WriteLocation := wlETR;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateRootDepthDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateRootDepth;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientRootDeptharrayUsed;
  UpdateRequirements.WriteLocation := wlRootDepth;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateTranspirationFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateTranspirationFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientTranspirationFractionarrayUsed;
  UpdateRequirements.WriteLocation := wlTranspirationFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteAddedCropDemandFlux;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  OfeItem: TCropEfficiencyItem;
  FmpCrops: TCropCollection;
  AFarm: TFarm;
  FarmID: Integer;
  OFE: TFarmEfficienciesItem;
  procedure WriteItem(AFarm: TFarm; CropIndex: Integer; OfeItem: TCropEfficiencyItem);
  var
    ACrop: TCropItem;
    Value: double;
    PestValues: TPestValues;
  begin
    if OfeItem <> nil then
    begin
      ACrop := FmpCrops[CropIndex];

      PestValues.Formula := OfeItem.Efficiency;

      PestValues.PestName := '';
      PestValues.PestSeriesName := OFE.CropEfficiency.PestSeriesParameter;
      PestValues.PestSeriesMethod := OFE.CropEfficiency.PestParamMethod;
      PestValues.FormulaErrorMessage := Format('Invalid Added Crop Demand Flux in Farm %0:s for crop %1:s.',
        [AFarm.FarmName, ACrop.CropName]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;

//      Formula := OfeItem.Efficiency;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        Format('Invalid Added Crop Demand Flux in Farm %0:s for crop %1:s.',
//        [AFarm.FarmName, ACrop.CropName]));
    end
    else
    begin
      WriteFloat(RequiredValues.DefaultValue);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
    CropIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for CropIndex := 0 to FmpCrops.Count - 1 do
              begin
                WriteFloat(RequiredValues.DefaultValue);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(AFarm.AddedCropDemandFlux.Count
              = FmpCrops.Count);
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);
            for CropIndex := 0 to AFarm.AddedCropDemandFlux.Count - 1 do
            begin
              OFE := AFarm.AddedCropDemandFlux[CropIndex];
              OfeItem := OFE.CropEfficiency.
                ItemByStartTime(StartTime) as TCropEfficiencyItem;
              WriteItem(AFarm, CropIndex, OfeItem);
            end;
            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for CropIndex := 0 to FmpCrops.Count - 1 do
            begin
              WriteFloat(RequiredValues.DefaultValue);
            end;
            Inc(FarmID);
            NewLine;
          end;
          Assert(AFarm.AddedCropDemandFlux.Count
            = FmpCrops.Count);
          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);
          for CropIndex := 0 to AFarm.AddedCropDemandFlux.Count - 1 do
          begin
            OFE := AFarm.AddedCropDemandFlux[CropIndex];
            if OFE.CropEfficiency.Count > 0 then
            begin
              OfeItem := OFE.CropEfficiency.First;
            end
            else
            begin
              OfeItem := nil;
            end;
            WriteItem(AFarm, CropIndex, OfeItem);
          end;
          Inc(FarmID);
          NewLine;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FFarmProcess4.Added_Crop_Demand_Flux.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlAddedCropDemandFlux;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: : ADDED_CROP_DEMAND';
  RequiredValues.ErrorID := 'FMP LAND_USE: ADDED_CROP_DEMAND';
  RequiredValues.ID := 'ADDED_CROP_DEMAND';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.Added_Crop_Demand_Flux.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidAddedCropD;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := 'FLUX ';
  RequiredValues.FarmProperty := FFarmProcess4.Added_Crop_Demand_Flux;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlAddedCropDemandFlux);
  end;

  FmpCrops := Model.FmpCrops;
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
//    WriteString(RequiredValues.Option);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteAddedCropDemandRate;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  OfeItem: TCropEfficiencyItem;
  FmpCrops: TCropCollection;
  AFarm: TFarm;
  FarmID: Integer;
  OFE: TFarmEfficienciesItem;
  procedure WriteItem(AFarm: TFarm; CropIndex: Integer; OfeItem: TCropEfficiencyItem);
  var
//    Formula: string;
    ACrop: TCropItem;
    Value: double;
    PestValues: TPestValues;
  begin
    if OfeItem <> nil then
    begin
      ACrop := FmpCrops[CropIndex];

      PestValues.Formula := OfeItem.Efficiency;

      PestValues.PestName := '';
      PestValues.PestSeriesName := OFE.CropEfficiency.PestSeriesParameter;
      PestValues.PestSeriesMethod := OFE.CropEfficiency.PestParamMethod;
      PestValues.FormulaErrorMessage :=
        Format('Invalid Added Crop Demand Rate in Farm %0:s for crop %1:s.',
        [AFarm.FarmName, ACrop.CropName]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;

//      Formula := OfeItem.Efficiency;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        Format('Invalid Added Crop Demand Rate in Farm %0:s for crop %1:s.',
//        [AFarm.FarmName, ACrop.CropName]));
    end
    else
    begin
      WriteFloat(RequiredValues.DefaultValue);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for CropIndex := 0 to FmpCrops.Count - 1 do
              begin
                WriteFloat(RequiredValues.DefaultValue);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(AFarm.AddedCropDemandRate.Count
              = FmpCrops.Count);
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);
            for CropIndex := 0 to AFarm.AddedCropDemandRate.Count - 1 do
            begin
              OFE := AFarm.AddedCropDemandRate[CropIndex];
              OfeItem := OFE.CropEfficiency.
                ItemByStartTime(StartTime) as TCropEfficiencyItem;
              WriteItem(AFarm, CropIndex, OfeItem);
            end;
            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for CropIndex := 0 to FmpCrops.Count - 1 do
            begin
              WriteFloat(RequiredValues.DefaultValue);
            end;
            Inc(FarmID);
            NewLine;
          end;
          Assert(AFarm.AddedCropDemandRate.Count
            = FmpCrops.Count);
          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);
          for CropIndex := 0 to AFarm.AddedCropDemandRate.Count - 1 do
          begin
            OFE := AFarm.AddedCropDemandRate[CropIndex];
            if OFE.CropEfficiency.Count > 0 then
            begin
              OfeItem := OFE.CropEfficiency.First;
            end
            else
            begin
              OfeItem := nil;
            end;
            WriteItem(AFarm, CropIndex, OfeItem);
          end;
          Inc(FarmID);
          NewLine;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FFarmProcess4.Added_Crop_Demand_Rate.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlAddedCropDemandRate;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: : ADDED_CROP_DEMAND';
  RequiredValues.ErrorID := 'FMP LAND_USE: ADDED_CROP_DEMAND';
  RequiredValues.ID := 'ADDED_CROP_DEMAND';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.Added_Crop_Demand_Rate.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidAddedCropDRate;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := 'RATE ';
  RequiredValues.FarmProperty := FFarmProcess4.Added_Crop_Demand_Rate;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlAddedCropDemandRate);
  end;

  FmpCrops := Model.FmpCrops;
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
//    WriteString(RequiredValues.Option);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteAddedDemand;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  ACrop: TCropItem;
  AddedDemandItem: TAddedDemandItem;
  CropIndex: Integer;
  procedure WriteAddedDemandItem(ACrop: TCropItem; AddedDemandItem: TAddedDemandItem);
  var
    FarmIndex: Integer;
    AFarm: TFarm;
    FarmItem: TAddedDemandFarmItem;
    FarmID: Integer;
    InnerFarmIndex: Integer;
    Formula: string;
  begin
    FarmID := 1;
    for FarmIndex := 0 to Model.Farms.Count - 1 do
    begin
      AFarm := Model.Farms[FarmIndex];
      for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
      begin
        WriteInteger(0);
        Inc(FarmID);
      end;
      if AddedDemandItem = nil then
      begin
          WriteInteger(0);
      end
      else
      begin
        FarmItem := AddedDemandItem.GetItemByFarmGUID(AFarm.FarmGUID);
        if FarmItem  = nil then
        begin
          WriteInteger(0);
        end
        else
        begin
          Formula := FarmItem.Value;
          if Formula = '' then
          begin
            Formula := '0';
          end;
          WriteFloatValueFromGlobalFormula(Formula, ACrop,
            Format('Invalid Added Demand formula for %0:s for Farm %1:s.',
            [ACrop.CropName, AFarm.FarmName]));
        end;
      end;
      Inc(FarmID);
    end;
    NewLine;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            ACrop := Model.FmpCrops[CropIndex];
            WriteInteger(CropIndex+1);
            AddedDemandItem := ACrop.AddedDemandCollection.
              ItemByStartTime(StartTime) as TAddedDemandItem;
            WriteAddedDemandItem(ACrop, AddedDemandItem);
          end;
        end;
      end
      else
      begin
        for CropIndex := 0 to Model.FmpCrops.Count - 1 do
        begin
          ACrop := Model.FmpCrops[CropIndex];
          WriteInteger(CropIndex + 1);
          if ACrop.AddedDemandCollection.Count > 0 then
          begin
            AddedDemandItem := ACrop.AddedDemandCollection.First as TAddedDemandItem;
          end
          else
          begin
            AddedDemandItem := nil;
          end;
          WriteAddedDemandItem(ACrop, AddedDemandItem);
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FLandUse.AddedDemand.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateAddedDemand;

  RequiredValues.WriteLocation := wlAddedDemand;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: ADDED_DEMAND';
  RequiredValues.ErrorID := 'FMP LAND_USE: ADDED_DEMAND';
  RequiredValues.ID := 'ADDED_DEMAND';
  RequiredValues.StaticDataName := KAddedDemand;
  RequiredValues.WriteTransientData :=
    (FLandUse.AddedDemand.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidAddedDemandV;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  case FLandUse.AddedDemandOption of
    doLength:
      begin
        RequiredValues.Option := 'LENGTH';
      end;
    doRate:
      begin
        RequiredValues.Option := 'RATE';
      end;
  end;
  RequiredValues.FarmProperty := FLandUse.AddedDemand;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlAddedDemand);
  end;

  if (FLandUse.AddedDemand.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].AddedDemandDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteAddedDemandRunoffSplit;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  IrrigationTypes: TIrrigationCollection;
  StartTime: Double;
  FarmID: Integer;
  AFarm: TFarm;
  OFE: TFarmEfficienciesItem;
  OfeItem: TCropEfficiencyItem;
  procedure WriteItem(AFarm: TFarm; IrrIndex: Integer; OfeItem: TCropEfficiencyItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if OfeItem <> nil then
    begin
      PestValues.Formula := OfeItem.Efficiency;

      PestValues.PestName := '';
      PestValues.PestSeriesName := OFE.CropEfficiency.PestSeriesParameter;
      PestValues.PestSeriesMethod := OFE.CropEfficiency.PestParamMethod;
      PestValues.FormulaErrorMessage := Format('Invalid Added Demand Runoff Split for %0:s in irrigation type %1:s.',
        [AFarm.FarmName, IrrigationTypes[IrrIndex].Name]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
    end
    else
    begin
      WriteFloat(0.1);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
    IrrIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for IrrIndex := 0 to IrrigationTypes.Count - 1 do
              begin
                WriteFloat(0.1);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(AFarm.AddedDemandRunoffSplitCollection.Count
              = IrrigationTypes.Count);
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);
            for IrrIndex := 0 to AFarm.AddedDemandRunoffSplitCollection.Count - 1 do
            begin
              OFE := AFarm.AddedDemandRunoffSplitCollection[IrrIndex];
              OfeItem := OFE.CropEfficiency.
                ItemByStartTime(StartTime) as TCropEfficiencyItem;
              WriteItem(AFarm, IrrIndex, OfeItem);
            end;
            Inc(FarmID);
            NewLine;
          end;


        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for IrrIndex := 0 to IrrigationTypes.Count - 1 do
            begin
              WriteFloat(0.1);
            end;
            Inc(FarmID);
            NewLine;
          end;
          Assert(AFarm.AddedDemandRunoffSplitCollection.Count
            = IrrigationTypes.Count);
          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);
          for IrrIndex := 0 to AFarm.AddedDemandRunoffSplitCollection.Count - 1 do
          begin
            OFE := AFarm.AddedDemandRunoffSplitCollection[IrrIndex];
            if OFE.CropEfficiency.Count > 0 then
            begin
              OfeItem := OFE.CropEfficiency.First;
            end
            else
            begin
              OfeItem := nil;
            end;
            WriteItem(AFarm, IrrIndex, OfeItem);
          end;
          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FFarmProcess4.Added_Demand_Runoff_Split.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateAddedDemandRunoffSplit;


  RequiredValues.WriteLocation := wlAddedDemandRunoffSplit;
  RequiredValues.DefaultValue := 0.1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: ADDED_DEMAND_RUNOFF_SPLIT';
  RequiredValues.ErrorID := 'FMP WBS: ADDED_DEMAND_RUNOFF_SPLIT';
  RequiredValues.ID := 'ADDED_DEMAND_RUNOFF_SPLIT';
  RequiredValues.StaticDataName := KAddedDemandRunoffSplit;
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.Added_Demand_Runoff_Split.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidAddedDemand2;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.Added_Demand_Runoff_Split;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlAddedDemandRunoffSplit);
  end;

  if (FFarmProcess4.Added_Demand_Runoff_Split.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    IrrigationTypes := Model.IrrigationTypes;
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteAllotments;
begin
  if FAllotments.IsSelected then
  begin
    WriteString('BEGIN ALLOTMENTS');
    NewLine;

    // SURFACE_WATER
    WriteSurfaceWaterAllotment;
    // GROUNDWATER
    WriteGroundWaterAllotment;

    WriteString('END ALLOTMENTS');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteBareEvap;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Potential_Evaporation_Bare.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateBareEvap;

  RequiredValues.WriteLocation := wlBareEvap;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP CLIMATE: POTENTIAL_EVAPORATION_BARE';
  RequiredValues.ErrorID := 'FMP CLIMATE: POTENTIAL_EVAPORATION_BARE';
  RequiredValues.ID := 'POTENTIAL_EVAPORATION_BARE';
  RequiredValues.StaticDataName := KPotential_Evap_Bare;
  RequiredValues.WriteTransientData :=
    (FClimatePackage.Potential_Evaporation_Bare.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidPotentialEv;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FClimatePackage.Potential_Evaporation_Bare;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlBareEvap);
  end;

  if (FClimatePackage.Potential_Evaporation_Bare.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteBarePrecipitationConsumptionFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.Bare_Precipitation_Consumption_Fraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateBarePrecipitationConsumptionFraction;

  RequiredValues.WriteLocation := wlBarePrecipitationConsumptionFraction;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: BARE_PRECIPITATION_CONSUMPTION_FRACTION';
  RequiredValues.ErrorID := 'FMP WBS: BARE_PRECIPITATION_CONSUMPTION_FRACTION';
  RequiredValues.ID := 'BARE_PRECIPITATION_CONSUMPTION_FRACTION';
  RequiredValues.StaticDataName := KBarePrecipitationConsumptionFraction;
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.Bare_Precipitation_Consumption_Fraction.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidPrecipConsumpRunoff;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.Bare_Precipitation_Consumption_Fraction;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlBarePrecipitationConsumptionFraction);
  end;

  WriteFmpArrayData(AFileName, RequiredValues);
  WriteArrayTemplate(RequiredValues);
end;

procedure TModflowFmp4Writer.WriteBareRunoffFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  FarmID: Integer;
  AFarm: TFarm;
  OwhmItem: TOwhmItem;
  procedure WriteItem(AFarm: TFarm; Item: TOwhmItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if Item <> nil then
    begin
      PestValues.Formula := Item.OwhmValue;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.BareRunoffFraction.PestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.BareRunoffFraction.PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid bare runoff fraction formula in ' + AFarm.FarmName;
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
//
//
//      Formula := Item.OwhmValue;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        'Invalid bare runoff fraction formula in ' + AFarm.FarmName);
    end
    else
    begin
      WriteInteger(1);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              WriteInteger(1);
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            OwhmItem := AFarm.BareRunoffFraction.ItemByStartTime(StartTime) as TOwhmItem;
            WriteItem(AFarm, OwhmItem);

            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            WriteInteger(1);
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.BareRunoffFraction.Count > 0 then
          begin
            OwhmItem := AFarm.BareRunoffFraction.First;
          end
          else
          begin
            OwhmItem := nil;
          end;
          WriteItem(AFarm, OwhmItem);

          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FFarmProcess4.Bare_Runoff_Fraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateBareRunoffFraction;

  RequiredValues.WriteLocation := wlBareRunoffFraction;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: BARE_RUNOFF_FRACTION';
  RequiredValues.ErrorID := 'FMP WBS: BARE_RUNOFF_FRACTION';
  RequiredValues.ID := 'BARE_RUNOFF_FRACTION';
  RequiredValues.StaticDataName := KBareRunoffFraction;
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.Bare_Runoff_Fraction.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidBareRunoff;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.Bare_Runoff_Fraction;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlBareRunoffFraction);
  end;

  if (FFarmProcess4.Bare_Runoff_Fraction.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);
    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;
    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
    end;

  end;
end;

procedure TModflowFmp4Writer.WriteCapillaryFringe;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
//  Formula: string;
  SoilID: Integer;
  ASoil: TSoilItem;
  procedure WriteListData;
  var
    SoilIndex: Integer;
    Value: double;
    PestValues: TPestValues;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      for SoilIndex := 0 to FFmpSoils.Count - 1 do
      begin
        ASoil := FFmpSoils[SoilIndex];
        SoilID := SoilIndex + 1;
        WriteInteger(SoilID);

        PestValues.Formula := ASoil.CapillaryFringe;

        PestValues.PestName := '';
        PestValues.PestSeriesName := '';
        PestValues.PestSeriesMethod := ppmMultiply;
        PestValues.FormulaErrorMessage := 'Invalid formula for capillary fringe in soil ' + ASoil.SoilName;
        PestValues.ErrorObjectName := ASoil.SoilName;

        AdjustFormulaForPest(PestValues);

        if WritingTemplate and PestValues.ParameterUsed then
        begin
          Value := GetFormulaValue(PestValues);

          WritePestTemplateFormula(Value, PestValues.PestName,
            PestValues.PestSeriesName, PestValues.PestSeriesMethod,
            nil);
        end
        else
        begin
          WriteFloatValueFromGlobalFormula(PestValues.Formula, ASoil,
            PestValues.FormulaErrorMessage);
        end;
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FSoil4.CapFringe.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlCapillaryFringe;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SOIL: CAPILLARY_FRINGE';
  RequiredValues.ErrorID := 'FMP SOIL: CAPILLARY_FRINGE';
  RequiredValues.ID := 'CAPILLARY_FRINGE';
  RequiredValues.StaticDataName := KCapillary_Fringe;
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidCapillaryFringe;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSoil4.CapFringe;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlCapillaryFringe);
  end;

  if (FSoil4.CapFringe.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);
    WriteString('STATIC LIST DATAFILE ');
    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Well_Cell: TFmpWell_Cell;
  LocalLayer: integer;
//  AuxValue: integer;
//  AuxName: string;
//  MNW2NAM: STRING;
  WellName: string;
  APoint: TPoint2D;
//  DataArray: TDataArray;
begin
  Well_Cell := Cell as TFmpWell_Cell;
  if Well_Cell.Mnw2 then
  begin
    LocalLayer := 0;
    WellName := Copy(Well_Cell.MnwName, 1, 20);
    TModflowMNW2_Writer.AdjustWellID(WellName);
  end
  else
  begin
    LocalLayer := Model.
      DataSetLayerToModflowLayer(Well_Cell.Layer);
    WellName := IntToStr(FFarmWellID);
  end;
  WriteString(WellName);
  WriteInteger(Well_Cell.FarmID);


  if Well_Cell.Mnw2 then
  begin
    WriteString(' MNW') ;
  end
  else
  begin
    WriteInteger(LocalLayer);
  end;

  if FFarmWells4.WellXY = xyCells then
  begin
    WriteInteger(Well_Cell.Row+1);
    WriteInteger(Well_Cell.Column+1);
  end
  else
  begin
    Assert(Well_Cell.ScreenObject <> nil);
    if Well_Cell.ScreenObject.Count = 1 then
    begin
      APoint := Well_Cell.ScreenObject.Points[0];
    end
    else
    begin
      APoint := Model.Grid.TwoDElementCenter(Well_Cell.Column, Well_Cell.Row);
    end;
    WriteFloat(APoint.x);
    WriteFloat(APoint.Y);
  end;

  if (Well_Cell.MaxPumpingRatePestName <> '')
    or (Well_Cell.MaxPumpingRatePestSeriesName <> '') then
  begin
    FPestParamUsed := True;
  end;

  WriteValueOrFormula(Well_Cell, FmpWellMaxPumpingRatePosition);

//  WriteFloat(Well_Cell.MaxPumpingRate);

  WriteInteger(StressPeriod+1);
  WriteInteger(StressPeriod+1);

  Inc(FFarmWellID);

//  Skip QMAXRESET because MNW1 is not supported.

//  AuxName := '';
//  case FFarmProcess.CropIrrigationRequirement of
//    cirOnlyWhenNeeded:
//      begin
//        if Well_Cell.PumpOnlyIfCropRequiresWater then
//        begin
//          AuxValue := 1;
//        end
//        else
//        begin
//          AuxValue := 0;
//        end;
//        WriteInteger(AuxValue);
//        AuxName := ' AUX-NOCIRNOQ';
//      end;
//    cirContinuously: ;// do nothing
//    else Assert(False);
//  end;

  // FMP does not accept auxilliary variables.
//  WriteIface(Well_Cell.IFace);

//  WriteString(' # ' + DataSetIdentifier
//    + ' Layer, Row, Column, Farm-Well-ID, Farm ID, ' + VariableIdentifiers);
//  if Well_Cell.Mnw2 then
//  begin
//    WriteString(', MNW2NAM ');
//  end;

//  WriteString(VariableIdentifiers + ',' + AuxName + ',' + ' IFACE');

//  // The annotation identifies the object used to define the well.
//  // This can be helpful in identifying when used with PEST.
//  WriteString(Well_Cell.PumpingRateAnnotation);
  NewLine;
end;

procedure TModflowFmp4Writer.WriteClimate;
begin
  if FClimatePackage.IsSelected then
  begin
    WriteString('BEGIN CLIMATE');
    NewLine;

    WritePrecipitation;
    WriteRefET;
    WriteBareEvap;

    WriteString('  REFERENCE_ET_TO_BARE');
    WriteFloat(FClimatePackage.RefEtToBare);
    NewLine;

    WriteDirectRecharge;
    WritePrecipPotConsumption;

    WriteString('END CLIMATE');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteConstantU2DINT(const Comment: string;
  const Value: integer; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
var
  OldLocation: TWriteLocation;
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose:
      begin
        OldLocation := FWriteLocation;
        FWriteLocation := wlMain;
        try
          inherited;
        finally
          FWriteLocation := OldLocation
        end;
      end;
    wlCID, wlFID, wlSoilID, wlNrdInfilLoc, wlCropHasSalinityDemand:
      begin
        inherited;
      end
    else
      Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteConstantU2DREL(const Comment: string;
  const Value: double; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
begin
  inherited;

end;

procedure TModflowFmp4Writer.WriteConsumptiveUse;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.ConsumptiveUse.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateConsumptiveUse;

  RequiredValues.WriteLocation := wlConsumptiveUse;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: CONSUMPTIVE_USE';
  RequiredValues.ErrorID := 'FMP LAND_USE: CONSUMPTIVE_USE';
  RequiredValues.ID := 'CONSUMPTIVE_USE';
  RequiredValues.StaticDataName := KConsumptiveUse;
  RequiredValues.WriteTransientData :=
    (FLandUse.ConsumptiveUse.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidConsumptiveUse;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.ConsumptiveUse;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlConsumptiveUse);
  end;

  if (FLandUse.ConsumptiveUse.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].ConsumptiveUseDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetConsumptiveUseCollection,
      StrInvalidConsumptive);
  end;
end;

procedure TModflowFmp4Writer.WriteCropCoefficient;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.CropCoeff.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateCropCoefficient;

  RequiredValues.WriteLocation := wlCropCoefficient;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: CROP_COEFFICIENT';
  RequiredValues.ErrorID := 'FMP LAND_USE: CROP_COEFFICIENT';
  RequiredValues.ID := 'CROP_COEFFICIENT';
  RequiredValues.StaticDataName := KCropCoefficient;
  RequiredValues.WriteTransientData :=
    (FLandUse.CropCoeff.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidCropCoefficV;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.CropCoeff;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlCropCoefficient);
  end;

  if (FLandUse.CropCoeff.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].CropCoefficientDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetCropCoefficentCollection,
      StrInvalidCropCoeffic);
  end;
end;

procedure TModflowFmp4Writer.WriteCropHasSalinityDemand;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FSalinityFlush.CropSalinityDemandChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlCropHasSalinityDemand;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtBoolean;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_HAS_SALINITY_DEMAND';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_HAS_SALINITY_DEMAND';
  RequiredValues.ID := 'CROP_HAS_SALINITY_DEMAND';
  RequiredValues.StaticDataName := KCropHasSalinityDemand;
  RequiredValues.WriteTransientData :=
    (FSalinityFlush.CropSalinityDemandChoice.FarmOption = foTransient);
  RequiredValues.CheckError :=  'Invalid Crop has Irrigation Demand value';
  RequiredValues.CheckProcedure := nil;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.CropSalinityDemandChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlCropHasSalinityDemand);
  end;

  if (FSalinityFlush.CropSalinityDemandChoice.ArrayList = alArray) then
  begin
    if FSalinityFlush.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].CropHasSalinityDemandDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetCropHasSalinityDemandCollection,
      'Invalid Crop has salinity deman formula in ');
  end;
end;

procedure TModflowFmp4Writer.WriteCropLeachingRequirement;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSalinityFlush.CropLeachRequirementChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlCropLeachRequirement;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_LEACHING_REQUIREMENT';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_LEACHING_REQUIREMENT';
  RequiredValues.ID := 'CROP_LEACHING_REQUIREMENT';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSalinityFlush.CropLeachRequirementChoice.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.CropLeachRequirementChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlCropLeachRequirement);
  end;

  WriteLeachList(RequiredValues, AFileName, GetLeachingRequirementCollection,
    'Invalid Crop Leaching Requirement formula in ');

end;

procedure TModflowFmp4Writer.WriteCropMaxLeachingRequirement;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSalinityFlush.CropMaxLeachChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlCropMaxLeachRequirement;
  RequiredValues.DefaultValue := 0.99;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_MAX_LEACHING_REQUIREMENT';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_MAX_LEACHING_REQUIREMENT';
  RequiredValues.ID := 'CROP_MAX_LEACHING_REQUIREMENT';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSalinityFlush.CropMaxLeachChoice.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.CropMaxLeachChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlCropMaxLeachRequirement);
  end;

  WriteOwhmList(RequiredValues, AFileName, GetMaxLeachingRequirementCollection,
    'Invalid Crop Max Leaching Requirement formula in ');

end;

procedure TModflowFmp4Writer.WriteCropName;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
//  DataArrayNames: TStringList;
  CropIndex: Integer;
  ACrop: TCropItem;
begin
//  if FSalinityFlush.CropSalinityToleranceChoice.FarmOption = foNotUsed then
//  begin
//    Exit;
//  end;


  RequiredValues.WriteLocation := wlCropNames;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtstring;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: CROP_NAME';
  RequiredValues.ErrorID := 'FMP LAND_USE: CROP_NAME';
  RequiredValues.ID := 'CROP_NAME';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

//  if RequiredValues.FarmProperty.ExternalFileName = '' then
//  begin
    AFileName := GetFileStreamName(wlCropNames);
//  end;

  WriteScaleFactorsID_andOption(RequiredValues, '', '');
    WriteString('STATIC LIST DATAFILE ');
    WriteString(ExtractFileName(AFileName));
    NewLine;
    try
      FWriteLocation := RequiredValues.WriteLocation;

      for CropIndex := 0 to Model.FmpCrops.Count - 1 do
      begin
        ACrop := Model.FmpCrops[CropIndex];
        WriteInteger(CropIndex+1);
        WriteString(' ' + ACrop.CropName);
        NewLine;
      end;

    finally
      FWriteLocation := wlMain;
    end;

end;

procedure TModflowFmp4Writer.WriteCropSalinityTolerance;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSalinityFlush.CropSalinityToleranceChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlCropSalinityTolerance;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_SALINITY_TOLERANCE';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_SALINITY_TOLERANCE';
  RequiredValues.ID := 'CROP_SALINITY_TOLERANCE';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSalinityFlush.CropSalinityToleranceChoice.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.CropSalinityToleranceChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlCropSalinityTolerance);
  end;

  WriteOwhmList(RequiredValues, AFileName, GetSalinityToleranceCollection,
    'Invalid Crop Salinity Tolerance formula in ');

end;

procedure TModflowFmp4Writer.WriteCropsThatSpecifySurfaceElevation;
begin

end;

procedure TModflowFmp4Writer.WriteCropSurfaceElevation;
begin

end;

procedure TModflowFmp4Writer.WriteDirectRecharge;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Direct_Recharge.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateDirectRecharge;

  RequiredValues.WriteLocation := wlDirectRecharge;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP CLIMATE: DIRECT_RECHARGE';
  RequiredValues.ErrorID := 'FMP CLIMATE: DIRECT_RECHARGE';
  RequiredValues.ID := 'DIRECT_RECHARGE';
  RequiredValues.StaticDataName := KDirectRecharge;
  RequiredValues.WriteTransientData :=
    (FClimatePackage.Direct_Recharge.FarmOption = foTransient);
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError :=  'Invalid Direct Recharge value';
  case FClimatePackage.DirectRechargeOption of
    droFlux: RequiredValues.Option := 'FLUX';
    droRate: RequiredValues.Option := 'RATE';
  end;
  RequiredValues.FarmProperty := FClimatePackage.Direct_Recharge;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlDirectRecharge);
  end;

  if (FClimatePackage.Direct_Recharge.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteEffectivePrecipitationTable;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  Formula: string;
  SoilIndex: Integer;
  SoilID: Integer;
  LookUpTable: TLookUpTable;
  Item: TLookupItem;
  ItemIndex: Integer;
  ASoil: TSoilItem;
begin
  if FSoil4.EffPrecipTable.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlPrecipicationTable;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SOIL: EFFECTIVE_PRECIPITATION_TABLE';
  RequiredValues.ErrorID := 'FMP SOIL: EFFECTIVE_PRECIPITATION_TABLE';
  RequiredValues.ID := 'EFFECTIVE_PRECIPITATION_TABLE';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := 'Invalid precipitation value';
  case FSoil4.EffPrecipTableOption of
    ppcLength:
      begin
        RequiredValues.Option := 'BY_LENGTH';
      end;
    ppcFraction:
      begin
        RequiredValues.Option := 'BY_FRACTION';
      end;
    else
      Assert(False);
  end;
  RequiredValues.FarmProperty := FSoil4.EffPrecipTable;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlPrecipicationTable);
  end;

  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);
    WriteString('STATIC LIST DATAFILE ');
    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      try
        FWriteLocation := RequiredValues.WriteLocation;
        for SoilIndex := 0 to FFmpSoils.Count - 1 do
        begin
          ASoil := FFmpSoils[SoilIndex];
          LookUpTable := ASoil.LookUpTable;
          SoilID := SoilIndex + 1;
          WriteInteger(SoilID);
          if LookUpTable.Method = smConstant then
          begin
            WriteString(' CONSTANT');
            if LookUpTable.Count > 0 then
            begin
              Item := LookUpTable.First as TLookupItem;
              Formula := Item.ReturnValue;
              WriteFloatValueFromGlobalFormula(Formula, ASoil,
                'Invalid formula Effective precipitation return value in soil ' + ASoil.SoilName);
            end
            else
            begin
              WriteFloat(0);
            end;
            NewLine;
          end
          else
          begin
            case LookUpTable.Method of
              smInterpolate:
                begin
                  WriteString(' INTERPOLATE');
                end;
              smStep:
                begin
                  WriteString(' STEP_FUNCTION');
                end;
              smNearest:
                begin
                  WriteString(' NEAREST');
                end;
              else
                Assert(False);
            end;
            WriteInteger(LookUpTable.Count);
            WriteString(' INTERNAL');
            NewLine;
            for ItemIndex := 0 to LookUpTable.Count - 1 do
            begin
              Item := LookUpTable[ItemIndex];
              Formula := Item.LookupValue;
              WriteFloatValueFromGlobalFormula(Formula, ASoil,
                'Invalid formula Effective precipitation look-up value in soil ' + ASoil.SoilName);

              Formula := Item.ReturnValue;
              WriteFloatValueFromGlobalFormula(Formula, ASoil,
                'Invalid formula Effective precipitation return value in soil ' + ASoil.SoilName);
              NewLine;
            end;
          end;

          NewLine;
        end;
      finally
        FWriteLocation := wlMain;
      end;
    end;
  end;
end;

function TModflowFmp4Writer.GetFormulaValue(var PestValues: TPestValues): Double;
var
  Compiler: TRbwParser;
  ErrorFormula: string;
  Expression: TExpression;
  InternalFormula: string;
begin
  Compiler := Model.ParentModel.rpTopFormulaCompiler;
  InternalFormula := PestValues.Formula;
  ErrorFormula := PestValues.Formula;
  try
    Compiler.Compile(PestValues.Formula);
  except
    on E: ERbwParserError do
    begin
      frmFormulaErrors.AddFormulaError(PestValues.ErrorObjectName, '',
        ErrorFormula, E.Message);
      PestValues.Formula := '0';
      Compiler.Compile(PestValues.Formula);
    end;
  end;
  Expression := Compiler.CurrentExpression;
  if Expression.ResultType in [rdtDouble, rdtInteger] then
  begin
    result := Expression.DoubleResult;
  end
  else
  begin
    frmFormulaErrors.AddFormulaError(PestValues.ErrorObjectName, '',
      ErrorFormula, PestValues.FormulaErrorMessage);
    result := 0;
  end;
end;

procedure TModflowFmp4Writer.AdjustFormulaForPest(var PestValues: TPestValues);
var
  PestParam: IModflowParameter;
  PestSeriesParameter: IModflowParameter;
begin
  PestValues.ParameterUsed := False;

  PestParam := Model.GetPestParameterByNameI(PestValues.Formula);
  if PestParam <> nil then
  begin
    FPestParamUsed := True;
    PestValues.ParameterUsed := True;
    PestValues.Formula := FortranFloatToStr(PestParam.Value);
    PestValues.PestName := PestParam.ParameterName;
  end;

  PestSeriesParameter := nil;
  if PestValues.PestSeriesName <> '' then
  begin
    PestSeriesParameter := Model.GetPestParameterByNameI(PestValues.PestSeriesName);
    if PestSeriesParameter <> nil then
    begin
      FPestParamUsed := True;
      PestValues.ParameterUsed := True;
      PestValues.PestSeriesName := PestSeriesParameter.ParameterName;
      case PestValues.PestSeriesMethod of
        ppmMultiply:
          begin
            PestValues.Formula := Format('(%0:s) * %1:s',
              [PestValues.Formula, FortranFloatToStr(PestSeriesParameter.Value)]);
          end;
        ppmAdd:
          begin
            PestValues.Formula := Format('(%0:s) + %1:s',
              [PestValues.Formula, FortranFloatToStr(PestSeriesParameter.Value)]);
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      PestValues.PestSeriesName := '';
    end;
  end;
end;


procedure TModflowFmp4Writer.WriteEfficiency;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  AFarm: TFarm;
  IrrigationTypes: TIrrigationCollection;
  OFE: TFarmEfficienciesItem;
  FarmID: Integer;
  OfeItem: TCropEfficiencyItem;
  StartTime: Double;
  procedure WriteOnFarmEfficiency(
    AFarm: TFarm; IrrIndex: Integer; OfeItem: TCropEfficiencyItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if OfeItem <> nil then
    begin
      PestValues.Formula := OfeItem.Efficiency;

      PestValues.PestName := '';
      PestValues.PestSeriesName := OFE.CropEfficiency.PestSeriesParameter;
      PestValues.PestSeriesMethod := OFE.CropEfficiency.PestParamMethod;
      PestValues.FormulaErrorMessage := Format('Invalid efficiency for %0:s in irrigation type %1:s.',
        [AFarm.FarmName, IrrigationTypes[IrrIndex].Name]);
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
    end
    else
    begin
      WriteFloat(1);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
    IrrIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for IrrIndex := 0 to IrrigationTypes.Count - 1 do
              begin
                WriteFloat(1);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(AFarm.FarmIrrigationEfficiencyCollection.Count
              = IrrigationTypes.Count);
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);
            for IrrIndex := 0 to AFarm.FarmIrrigationEfficiencyCollection.Count - 1 do
            begin
              OFE := AFarm.FarmIrrigationEfficiencyCollection[IrrIndex];
              OfeItem := OFE.CropEfficiency.
                ItemByStartTime(StartTime) as TCropEfficiencyItem;
              WriteOnFarmEfficiency(AFarm, IrrIndex, OfeItem);
            end;
            Inc(FarmID);
            NewLine;
          end;


        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for IrrIndex := 0 to IrrigationTypes.Count - 1 do
            begin
              WriteFloat(1);
            end;
            Inc(FarmID);
            NewLine;
          end;
          Assert(AFarm.FarmIrrigationEfficiencyCollection.Count
            = IrrigationTypes.Count);
          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);
          for IrrIndex := 0 to AFarm.FarmIrrigationEfficiencyCollection.Count - 1 do
          begin
            OFE := AFarm.FarmIrrigationEfficiencyCollection[IrrIndex];
            if OFE.CropEfficiency.Count > 0 then
            begin
              OfeItem := OFE.CropEfficiency.First;
            end
            else
            begin
              OfeItem := nil;
            end;
            WriteOnFarmEfficiency(AFarm, IrrIndex, OfeItem);
          end;
          Inc(FarmID);
          NewLine;
        end;

      end;


    finally
      FWriteLocation := wlMain;
    end;
  end;
begin
  if FFarmProcess4.EfficiencyOptions.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateEfficiency;

  RequiredValues.WriteLocation := wlEfficiency;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: Efficiency';
  RequiredValues.ErrorID := 'FMP WBS: Efficiency';
  RequiredValues.ID := 'EFFICIENCY';
  RequiredValues.StaticDataName := KEfficiency;
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.EfficiencyOptions.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidEfficiencyV;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.EfficiencyOptions;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlEfficiency);
  end;

  if (FFarmProcess4.EfficiencyOptions.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    IrrigationTypes := Model.IrrigationTypes;
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;

      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
    end;
  end;

end;

procedure TModflowFmp4Writer.WriteEfficiencyImprovement;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  IrrigationTypes: TIrrigationCollection;
  TimeIndex: Integer;
  StartTime: Double;
  FarmID: Integer;
  FarmIndex: Integer;
  AFarm: TFarm;
  InnerFarmIndex: Integer;
  IrrIndex: Integer;
  OFE: TFarmEfficienciesItem;
  OfeItem: TCropEfficiencyItem;
  procedure WriteFarmEfficiencyItem(AFarm: TFarm; IrrIndex: Integer; OfeItem: TCropEfficiencyItem);
  var
    Formula: string;
  begin
    if OfeItem <> nil then
    begin
      Formula := OfeItem.Efficiency;
      WriteBooleanValueFromGlobalFormula(Formula, AFarm,
        Format('Invalid efficiency improvement for %0:s in irrigation type %1:s.',
        [AFarm.FarmName, IrrigationTypes[IrrIndex].Name]));
    end
    else
    begin
      WriteInteger(0);
    end;
  end;
begin
  if FFarmProcess4.EfficiencyImprovement.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlEfficiencyImprovement;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: Efficiency Improvement';
  RequiredValues.ErrorID := 'FMP WBS: Efficiency Improvement';
  RequiredValues.ID := 'EFFICIENCY_IMPROVEMENT';
  RequiredValues.StaticDataName := KEfficiencyImprovement;
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.EfficiencyImprovement.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidEfficiencyI;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.EfficiencyImprovement;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlEfficiencyImprovement);
  end;

  if (FFarmProcess4.EfficiencyImprovement.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin
    IrrigationTypes := Model.IrrigationTypes;
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      try
        FWriteLocation := RequiredValues.WriteLocation;
        if RequiredValues.WriteTransientData then
        begin
          for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
          begin
            WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
            StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

            FarmID := 1;
            for FarmIndex := 0 to Model.Farms.Count - 1 do
            begin
              AFarm := Model.Farms[FarmIndex];
              for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
              begin
                WriteInteger(FarmID);
                for IrrIndex := 0 to IrrigationTypes.Count - 1 do
                begin
                  WriteInteger(0);
                end;
                Inc(FarmID);
                NewLine;
              end;
              Assert(AFarm.FarmIrrigationEfficiencyImprovementCollection.Count
                = IrrigationTypes.Count);
              Assert(FarmID = AFarm.FarmId);
              WriteInteger(AFarm.FarmId);
              for IrrIndex := 0 to AFarm.FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
              begin
                OFE := AFarm.FarmIrrigationEfficiencyImprovementCollection[IrrIndex];
                OfeItem := OFE.CropEfficiency.
                  ItemByStartTime(StartTime) as TCropEfficiencyItem;
                WriteFarmEfficiencyItem(AFarm, IrrIndex, OfeItem);
              end;
              Inc(FarmID);
              NewLine;
            end;


          end;
        end
        else
        begin
          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for IrrIndex := 0 to IrrigationTypes.Count - 1 do
              begin
                WriteInteger(0);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(AFarm.FarmIrrigationEfficiencyImprovementCollection.Count
              = IrrigationTypes.Count);
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);
            for IrrIndex := 0 to AFarm.FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
            begin
              OFE := AFarm.FarmIrrigationEfficiencyImprovementCollection[IrrIndex];
              if OFE.CropEfficiency.Count > 0 then
              begin
                OfeItem := OFE.CropEfficiency.First;
              end
              else
              begin
                OfeItem := nil;
              end;
              WriteFarmEfficiencyItem(AFarm, IrrIndex, OfeItem);
            end;
            Inc(FarmID);
            NewLine;
          end;

        end;
      finally
        FWriteLocation := wlMain;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteEvapIrrigateFracSumOneCorrection;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FLandUse.ET_IrrigFracCorrection.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlNoEvapErrCorrection;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: EVAPORATION_IRRIGATION_FRACTION_SUM_ONE_CORRECTION';
  RequiredValues.ErrorID := 'FMP LAND_USE: EVAPORATION_IRRIGATION_FRACTION_SUM_ONE_CORRECTION';
  RequiredValues.ID := 'EVAPORATION_IRRIGATION_FRACTION_SUM_ONE_CORRECTION';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FLandUse.ET_IrrigFracCorrection.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.ET_IrrigFracCorrection;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlNoEvapErrCorrection);
  end;

  WriteOwhmList(RequiredValues, AFileName, GetEvapIrrigateFracSumOneCorrectionCollection,
    StrInvalidEvaporation);

end;

procedure TModflowFmp4Writer.WriteEvaporationIrrigationFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  ACrop: TCropItem;
  IrregationItem: TCropIrrigationItem;
//  Formula: string;
  IrrigationTypes: TIrrigationCollection;
  EvapFracItem: TEvapFractionItem;
  IrrigationType: TIrrigationItem;
  CropIndex: Integer;
  procedure WriteEvapIrrigationItem(ACrop: TCropItem; IrregationItem: TCropIrrigationItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if IrregationItem <> nil then
    begin
      PestValues.Formula := IrregationItem.EvapIrrigateFraction;

      PestValues.PestName := '';
      PestValues.PestSeriesName := ACrop.IrrigationCollection.EvapIrrigateFractionPestSeriesParameter;
      PestValues.PestSeriesMethod := ACrop.IrrigationCollection.EvapIrrigateFractionPestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid Evaporation Irrigation Fraction in ' + ACrop.CropName;
      PestValues.ErrorObjectName := ACrop.CropName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, ACrop,
          PestValues.FormulaErrorMessage);
      end;
    end
    else
    begin
      WriteFloat(0.0);
    end;
    NewLine;
  end;
  procedure WriteEvapFractItem(EvapFracItem: TEvapFractionItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if EvapFracItem <> nil then
    begin
      PestValues.Formula := EvapFracItem.EvapIrrigateFraction;

      PestValues.PestName := '';
      PestValues.PestSeriesName := IrrigationType.EvapFraction.PestSeriesParameter;
      PestValues.PestSeriesMethod := IrrigationType.EvapFraction.PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid Evaporation Irrigation Fraction in ' + IrrigationType.Name;
      PestValues.ErrorObjectName := IrrigationType.Name;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, IrrigationType,
          PestValues.FormulaErrorMessage);
      end;
//
//
//      Formula := EvapFracItem.EvapIrrigateFraction;
//      WriteFloatValueFromGlobalFormula(Formula,
//        IrrigationType, IrrigationType.Name);
    end
    else
    begin
      WriteFloat(0.0);
    end;
    NewLine;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
    IrrIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if FLandUse.EvapIrrigationOption = ioByCrop then
      begin
        if RequiredValues.WriteTransientData then
        begin
          for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
          begin
            WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

            StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

            for CropIndex := 0 to Model.FmpCrops.Count - 1 do
            begin
              ACrop := Model.FmpCrops[CropIndex];
              WriteInteger(CropIndex+1);
              IrregationItem := ACrop.IrrigationCollection.
                ItemByStartTime(StartTime) as TCropIrrigationItem;
              WriteEvapIrrigationItem(ACrop, IrregationItem);
            end;
          end;
        end
        else
        begin
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            ACrop := Model.FmpCrops[CropIndex];
            WriteInteger(CropIndex + 1);
            if ACrop.IrrigationCollection.Count > 0 then
            begin
              IrregationItem := ACrop.IrrigationCollection.First as TCropIrrigationItem;
            end
            else
            begin
              IrregationItem := nil;
            end;
            WriteEvapIrrigationItem(ACrop, IrregationItem);
          end;
        end;
      end
      else
      begin
        IrrigationTypes := Model.IrrigationTypes;
        if RequiredValues.WriteTransientData then
        begin
          for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
          begin
            WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

            StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

            for IrrIndex := 0 to IrrigationTypes.Count - 1 do
            begin
              IrrigationType := IrrigationTypes[IrrIndex];
              WriteInteger(IrrIndex+1);
              EvapFracItem := IrrigationType.EvapFraction.
                ItemByStartTime(StartTime) as TEvapFractionItem;
              WriteEvapFractItem(EvapFracItem)
//              if EvapFracItem <> nil then
//              begin
//                Formula := EvapFracItem.EvapIrrigateFraction;
//                WriteFloatValueFromGlobalFormula(Formula,
//                  IrrigationType, IrrigationType.Name);
//              end
//              else
//              begin
//                WriteFloat(0.0);
//              end;
//              NewLine;
            end;
          end;
        end
        else
        begin
          for IrrIndex := 0 to IrrigationTypes.Count - 1 do
          begin
            IrrigationType := IrrigationTypes[IrrIndex];
            WriteInteger(IrrIndex + 1);
            if IrrigationType.EvapFraction.Count > 0 then
            begin
              EvapFracItem := IrrigationType.EvapFraction.First as TEvapFractionItem;
//              Formula := EvapFracItem.EvapIrrigateFraction;
//              WriteFloatValueFromGlobalFormula(Formula,
//                IrrigationType, IrrigationType.Name);
            end
            else
            begin
              EvapFracItem := nil
//              WriteFloat(0.0);
            end;
            WriteEvapFractItem(EvapFracItem)
//            NewLine;
          end;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FLandUse.EvapIrrigationFraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateEvaporationIrrigationFraction;

  RequiredValues.WriteLocation := wlEvaporationIrrigationFraction;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: EVAPORATION_IRRIGATION_FRACTION';
  RequiredValues.ErrorID := 'FMP LAND_USE: EVAPORATION_IRRIGATION_FRACTION';
  RequiredValues.ID := 'EVAPORATION_IRRIGATION_FRACTION';
  RequiredValues.StaticDataName := KEvaporationIrrigationFraction;
  RequiredValues.WriteTransientData :=
    (FLandUse.EvapIrrigationFraction.FarmOption = foTransient);
  RequiredValues.CheckError :=  'Invalid Evaporation Irrigation Fraction value';
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
//  RequiredValues.Option := '';
  if FLandUse.EvapIrrigationOption = ioByIrrigate then
  begin
    RequiredValues.Option := 'BY_IRRIGATE ';
  end
  else
  begin
    RequiredValues.Option := 'BY_CROP ';
  end;
  RequiredValues.FarmProperty := FLandUse.EvapIrrigationFraction;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlEvaporationIrrigationFraction);
  end;

  if (FLandUse.EvapIrrigationFraction.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].EvaporationIrrigationDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
//      FWriteLocation := RequiredValues.WriteLocation;
//      try
//        if FLandUse.EvapIrrigationOption = ioByCrop then
//        begin
//          if RequiredValues.WriteTransientData then
//          begin
//            for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
//            begin
//              WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
//
//              StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;
//
//              for CropIndex := 0 to Model.FmpCrops.Count - 1 do
//              begin
//                ACrop := Model.FmpCrops[CropIndex];
//                WriteInteger(CropIndex+1);
//                IrregationItem := ACrop.IrrigationCollection.
//                  ItemByStartTime(StartTime) as TCropIrrigationItem;
//                WriteEvapIrrigationItem(ACrop, IrregationItem);
//              end;
//            end;
//          end
//          else
//          begin
//            for CropIndex := 0 to Model.FmpCrops.Count - 1 do
//            begin
//              ACrop := Model.FmpCrops[CropIndex];
//              WriteInteger(CropIndex + 1);
//              if ACrop.IrrigationCollection.Count > 0 then
//              begin
//                IrregationItem := ACrop.IrrigationCollection.First as TCropIrrigationItem;
//              end
//              else
//              begin
//                IrregationItem := nil;
//              end;
//              WriteEvapIrrigationItem(ACrop, IrregationItem);
//            end;
//          end;
//        end
//        else
//        begin
//          IrrigationTypes := Model.IrrigationTypes;
//          if RequiredValues.WriteTransientData then
//          begin
//            for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
//            begin
//              WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
//
//              StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;
//
//              for IrrIndex := 0 to IrrigationTypes.Count - 1 do
//              begin
//                IrrigationType := IrrigationTypes[IrrIndex];
//                WriteInteger(IrrIndex+1);
//                EvapFracItem := IrrigationType.EvapFraction.
//                  ItemByStartTime(StartTime) as TEvapFractionItem;
//
//                if EvapFracItem <> nil then
//                begin
//                  Formula := EvapFracItem.EvapIrrigateFraction;
//                  WriteFloatValueFromGlobalFormula(Formula,
//                    IrrigationType, IrrigationType.Name);
//                end
//                else
//                begin
//                  WriteFloat(0.0);
//                end;
//                NewLine;
//              end;
//            end;
//          end
//          else
//          begin
//            for IrrIndex := 0 to IrrigationTypes.Count - 1 do
//            begin
//              IrrigationType := IrrigationTypes[IrrIndex];
//              WriteInteger(IrrIndex + 1);
//              if IrrigationType.EvapFraction.Count > 0 then
//              begin
//                EvapFracItem := IrrigationType.EvapFraction.First as TEvapFractionItem;
//                Formula := EvapFracItem.EvapIrrigateFraction;
//                WriteFloatValueFromGlobalFormula(Formula,
//                  IrrigationType, IrrigationType.Name);
//              end
//              else
//              begin
//                WriteFloat(0.0);
//              end;
//              NewLine;
//            end;
//          end;
//        end;
//      finally
//        FWriteLocation := wlMain;
//      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteLandUseLocation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  RequiredValues.WriteLocation := wlCID;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: Location';
  RequiredValues.ErrorID := 'FMP LAND_USE: Location';
  RequiredValues.ID := 'LOCATION';
  RequiredValues.StaticDataName := KLand_Use_ID;
  RequiredValues.WriteTransientData := FLandUse.CropLocation = rstTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidCropIDInF;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

  AFileName := GetFileStreamName(wlCID);

  if FLandUse.LandUseOption = luoSingle then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
  end;
end;

procedure TModflowFmp4Writer.WriteNonRoutedDelivery;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: double;
  FarmID: Integer;
  AFarm: TFarm;
  ADeliveryCollection: TNonRoutedDeliveryParameterCollection;
  ADeliveryItem: TNonRoutedDeliveryParameterItem;
  procedure WriteDeliveryItem(ADeliveryItem: TNonRoutedDeliveryParameterItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if ADeliveryItem <> nil then
    begin
      PestValues.Formula := ADeliveryItem.Volume;

      PestValues.PestName := '';
      PestValues.PestSeriesName := ADeliveryCollection.PestSeriesParameter;
      PestValues.PestSeriesMethod := ADeliveryCollection.PestParamMethod;
      PestValues.FormulaErrorMessage := 'NRDV';
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;


      WriteIntegerValueFromGlobalFormula(ADeliveryItem.Rank ,
        AFarm, 'NRDR',
        procedure (Value: integer)
        begin
          if Value <= 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrThePriorityForNon,
              AFarm.FarmName);
          end;
        end);
      case ADeliveryItem.NonRoutedDeliveryTypeOwhm2 of
        nrdt2FarmDemand:
          begin
            WriteInteger(0);
          end;
        nrdt2Discharged:
          begin
            WriteInteger(1);
          end;
        nrdt2Stored:
          begin
            WriteInteger(2);
          end;
        nrdt2Infiltrate:
          begin
            WriteIntegerValueFromGlobalFormula(ADeliveryItem.VirtualFarm ,
              AFarm, 'NRDU',
              procedure (Value: integer)
              begin
                if Value < 10 then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrNonroutedDelivery,
                    AFarm.FarmName);
                end;
              end);
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end
    else
    begin
      WriteFloat(0);
      WriteInteger(0);
      WriteInteger(0);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
    DeliveryIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for DeliveryIndex := 0 to FNrdTypes - 1 do
              begin
                WriteFloatCondensed(0);
                WriteFreeInteger(DeliveryIndex+1);
                WriteFreeInteger(0);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            for DeliveryIndex := 0 to AFarm.DeliveryParamCollection.Count - 1 do
            begin
              ADeliveryCollection := AFarm.DeliveryParamCollection
                [DeliveryIndex].DeliveryParam;
              ADeliveryItem := ADeliveryCollection.ItemByStartTime(StartTime)
                 as TNonRoutedDeliveryParameterItem;
              WriteDeliveryItem(ADeliveryItem);
            end;
            for DeliveryIndex := AFarm.DeliveryParamCollection.Count to FNrdTypes - 1 do
            begin
              WriteFloatCondensed(0);
              WriteFreeInteger(DeliveryIndex+1);
              WriteFreeInteger(0);
            end;
            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for DeliveryIndex := 0 to FNrdTypes - 1 do
            begin
              WriteFloatCondensed(0);
              WriteFreeInteger(DeliveryIndex+1);
              WriteFreeInteger(0);
            end;
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          for DeliveryIndex := 0 to AFarm.DeliveryParamCollection.Count - 1 do
          begin
            ADeliveryCollection := AFarm.DeliveryParamCollection
              [DeliveryIndex].DeliveryParam;
            if ADeliveryCollection.Count > 0 then
            begin
              ADeliveryItem := ADeliveryCollection.First
                 as TNonRoutedDeliveryParameterItem;
              WriteDeliveryItem(ADeliveryItem);
            end
            else
            begin
              WriteDeliveryItem(nil);
            end;
          end;
          for DeliveryIndex := AFarm.DeliveryParamCollection.Count to FNrdTypes - 1 do
          begin
            WriteFloatCondensed(0);
            WriteFreeInteger(DeliveryIndex+1);
            WriteFreeInteger(0);
          end;

          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if (FSurfaceWater4.Non_Routed_Delivery.FarmOption = foNotUsed) then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  if (FNrdTypes = 0) then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrNonRoutedDeliverie,
      StrInTheMODFLOWPacka)
  end;

  RequiredValues.WriteLocation := wlDeliveries;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: NON_ROUTED_DELIVERY';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: NON_ROUTED_DELIVERY';
  RequiredValues.ID := 'NON_ROUTED_DELIVERY';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSurfaceWater4.Non_Routed_Delivery.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidNonroutedD;
  case FSurfaceWater4.NRDOption of
    nrdoRate:
      begin
        RequiredValues.Option := 'RATE ';
      end;
    nrdoVolume:
      begin
        RequiredValues.Option := 'VOLUME ';
      end;
  end;

  RequiredValues.FarmProperty := FSurfaceWater4.Non_Routed_Delivery;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlDeliveries);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteNoReturnFlow;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  TimeIndex: Integer;
  StartTime: double;
  FarmID: Integer;
  FarmIndex: Integer;
  AFarm: TFarm;
  InnerFarmIndex: Integer;
  NoReturnItem: TNoReturnItem;
begin
  if (FSurfaceWater4.NoReturnFlow.FarmOption = foNotUsed) then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlNoReturnFlow;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: NO_RETURN_FLOW';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: NO_RETURN_FLOW';
  RequiredValues.ID := 'NO_RETURN_FLOW';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSurfaceWater4.NoReturnFlow.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidNoReturnFl;
  RequiredValues.Option := '';

  RequiredValues.FarmProperty := FSurfaceWater4.NoReturnFlow;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlNoReturnFlow);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    try
      FWriteLocation := RequiredValues.WriteLocation;

      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              WriteFreeInteger(0);
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            NoReturnItem := AFarm.NoReturnFlow.ItemByStartTime(StartTime)
               as TNoReturnItem;
            if NoReturnItem = nil then               
            begin   
              WriteInteger(0);         
            end              
            else            
            begin            
              WriteInteger(Ord(NoReturnItem.NoReturnOption));
            end;                        
            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            WriteInteger(0);
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.NoReturnFlow.Count > 0 then
          begin
            NoReturnItem := AFarm.NoReturnFlow.First
               as TNoReturnItem;
            WriteInteger(Ord(NoReturnItem.NoReturnOption));
          end
          else
          begin
            WriteInteger(0);
          end;

          Inc(FarmID);
          NewLine;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteNrdInfilLocation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  // As of MODFLOW-OWHM version 4.3, this option has not been implemented
  // so FSurfaceWater4.Nrd_Infiltration_Location.FarmOption = foNotUsed
  // is always true and FMP SURFACE_WATER: NRD_INFILTRATION_LOCATION
  // is never written to the input file.
  // If it is implemented in the future, the help for this feature will
  // need to be updated in both the MODFLOW Packages and Programs dialog box
  // and in the Object Properties dialog box.
  if FSurfaceWater4.Nrd_Infiltration_Location.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlNrdInfilLoc;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SURFACE_WATER: NRD_INFILTRATION_LOCATION';
  RequiredValues.ErrorID := 'FMP SURFACE_WATER: NRD_INFILTRATION_LOCATION';
  RequiredValues.ID := 'NRD_INFILTRATION_LOCATION';
  RequiredValues.StaticDataName := KNRD_Infiltration_Location;
  RequiredValues.WriteTransientData :=
    (FSurfaceWater4.Nrd_Infiltration_Location.FarmOption = foTransient);
  RequiredValues.CheckError :=  'Invalid Non-Routed Delivery Infiltration Location value';
  RequiredValues.CheckProcedure := CheckDataSetZeroOrGETen;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSurfaceWater4.Nrd_Infiltration_Location;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlNrdInfilLoc);
  end;

  if (FSurfaceWater4.Nrd_Infiltration_Location.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteTransientFmpArrayData(RequiredValues: TRequiredValues);
var
  ValueCellList: TValueCellList;
  Dummy: TDataArray;
  TransList: TList;
  StressPeriodIndex: Integer;
  CheckAssigned: Boolean;
  DataTypeIndex: Integer;
begin
  TransList := GetTransientList(RequiredValues.WriteLocation);

  FWriteLocation := RequiredValues.WriteLocation;
  try
    for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      if TransList.Count <= StressPeriodIndex then
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrUndefinedError, RequiredValues.ErrorID + StrStressPeriod +
          IntToStr(StressPeriodIndex+1));
        Exit;
      end;
      ValueCellList := TransList[StressPeriodIndex];
      CheckAssigned := Assigned(RequiredValues.CheckProcedure);
      WriteCommentLine(Format(StrStressPeriodD, [StressPeriodIndex+1]));
      for DataTypeIndex := RequiredValues.DataTypeIndex to
        RequiredValues.MaxDataTypeIndex do
      begin
        Dummy := nil;
        try
          WriteTransient2DArray(RequiredValues.Comment, DataTypeIndex,
            RequiredValues.DataType, RequiredValues.DefaultValue,
            ValueCellList, umAssign, False, Dummy, RequiredValues.ID,
            (not CheckAssigned), False, (FLandUse.LandUseOption = luoMultiple));
          if CheckAssigned then
          begin
            RequiredValues.CheckProcedure(Dummy, RequiredValues.CheckError);
          end;
        finally
          Dummy.Free;
        end;
      end;
    end;
  finally
    FWriteLocation := wlMain;
  end;
end;

procedure TModflowFmp4Writer.WriteTranspirationFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.TranspirationFraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateTranspirationFraction;

  RequiredValues.WriteLocation := wlTranspirationFraction;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: TRANSPIRATION_FRACTION';
  RequiredValues.ErrorID := 'FMP LAND_USE: TRANSPIRATION_FRACTION';
  RequiredValues.ID := 'TRANSPIRATION_FRACTION';
  RequiredValues.StaticDataName := KTranspirationFraction;
  RequiredValues.WriteTransientData :=
    (FLandUse.TranspirationFraction.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidTranspiratio2;
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.TranspirationFraction;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlTranspirationFraction);
  end;

  if (FLandUse.TranspirationFraction.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].TranspirationFractionDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetTranspirationFractionCollection,
      StrInvalidTranspiratio);
  end;
end;

procedure TModflowFmp4Writer.WriteFarmLocation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.Farms.FarmOption = foNotUsed then
  begin
    Assert(False);
  end;

  RequiredValues.WriteLocation := wlFID;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: Location';
  RequiredValues.ErrorID := 'FMP WBS: Location';
  RequiredValues.ID := 'LOCATION';
  RequiredValues.StaticDataName := KFarmID;
  RequiredValues.WriteTransientData := FFarmProcess4.Farms.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidFarmID;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.Farms;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlFID);
  end;

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmProcess);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSoilTypeUndefined);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSoilCoefficientNot);

  if (not FFarmProcess4.IsSelected)
    or (Model.ModelSelection <> msModflowOwhm2) then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrFMP) then
  begin
    Exit;
  end;
  if Model is TChildModel then
  begin
    if not TChildModel(Model).ParentModel.ModflowPackages.FarmProcess4.IsSelected then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidFarmProcess, StrIfTheFarmProcess);
    end;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  FBaseName := ChangeFileExt(FNameOfFile, '');

  WriteToNameFile(StrFMP, Model.UnitNumbers.UnitNumber(StrFMP),
    FNameOfFile, foInput, Model);

  EvaluateAll;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  WriteFileInternal;

  if  Model.PestUsed and FPestParamUsed then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FreeFileStreams;
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteFileInternal;
begin
  ClearTimeLists(Model);
  FFarmWellID := 1;
  OpenFile(FNameOfFile);
  try
    FWriteLocation := wlMain;
    frmProgressMM.AddMessage('Writing FMP3 Package input.');
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

    WriteDataSet0;
    WriteGobalDimension;
    WriteOutput;
    WriteOptions;
    WriteWaterBalanceSubregion;
    WriteSoil;
    WriteClimate;
    WriteSurfaceWater;
    WriteSupplyWell;
    WriteAllotments;
    WriteLandUse;
    WriteSalinityFlush;
    WriteSurfaceWaterIrrigation;
  finally
    CloseFile;
  end;
end;

procedure TModflowFmp4Writer.WriteGobalDimension;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  NSFR_DELIV: Integer;
  NSFR_RETURN: Integer;
//  RowIndex: Integer;
begin
  WriteString('BEGIN GLOBAL DIMENSION');
  NewLine;

  Model.Farms.Sort;
  if Model.Farms.Count > 0 then
  begin
    FNWBS := Model.Farms.Last.FarmId;
  end
  else
  begin
    FNWBS := 0;
  end;
  WriteString('  NWBS');
  WriteInteger(FNWBS);
  NewLine;

  WriteString('  NCROP');
  if FLandUse.IsSelected then
  begin
    WriteInteger(Model.FmpCrops.Count);
  end
  else
  begin
    WriteInteger(0);
  end;
  NewLine;

  WriteString('  NSOIL');
  if FSoil4.IsSelected then
  begin
    WriteInteger(Model.FmpSoils.Count);
  end
  else
  begin
    WriteInteger(0);
  end;
  NewLine;

  WriteString('  NIRRIGATE');
  WriteInteger(Model.IrrigationTypes.Count);
  NewLine;

  if FSurfaceWater4.IsSelected then  
  begin  
    FNrdTypes := 0;
    if (FSurfaceWater4.Non_Routed_Delivery.FarmOption <> foNotUsed) then
    begin
      for FarmIndex := 0 to Model.Farms.Count - 1 do
      begin
        AFarm := Model.Farms[FarmIndex];
        if AFarm.DeliveryParamCollection.Count > FNrdTypes then
        begin
          FNrdTypes := AFarm.DeliveryParamCollection.Count;
        end;
      end;
    end;
    WriteString('  NRD_TYPES');
    WriteInteger(FNrdTypes);
    NewLine;

    NSFR_DELIV := 0;
    if FSurfaceWater4.Semi_Routed_Delivery.FarmOption <> foNotUsed then
    begin
      for FarmIndex := 0 to Model.Farms.Count - 1 do
      begin
        AFarm := Model.Farms[FarmIndex];
        if AFarm.MultiSrDeliveries.Count > 0 then
        begin
          Inc(NSFR_DELIV, AFarm.MultiSrDeliveries.Count);
        end
        else
        begin
          Inc(NSFR_DELIV)
        end;
      end;
    end;
    WriteString('  NSFR_DELIV');
    WriteInteger(NSFR_DELIV);
    NewLine;

    NSFR_RETURN := 0;
    if FSurfaceWater4.SemiRoutedReturn.FarmOption <> foNotUsed then
    begin
      for FarmIndex := 0 to Model.Farms.Count - 1 do
      begin
        AFarm := Model.Farms[FarmIndex];
        if AFarm.MultiSrReturns.Count > 0 then
        begin
          Inc(NSFR_RETURN, AFarm.MultiSrReturns.Count);
        end
        else
        begin
          Inc(NSFR_RETURN);
        end;
      end;
    end;
    WriteString('  NSFR_RETURN');
    WriteInteger(NSFR_RETURN);
    NewLine;
  end;

  WriteSurfaceElevation;

  WriteString('END');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteGroundWaterAllotment;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  FarmID: Integer;
  AFarm: TFarm;
  AllotmentItem: TAllotmentItem;
  procedure WriteAllotmentItem(AFarm: TFarm; AllotmentItem: TAllotmentItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if AllotmentItem <> nil then
    begin
      PestValues.Formula := AllotmentItem.Allotment;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.GwAllotment.PestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.GwAllotment.PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid ground water allotment formula in ' + AFarm.FarmName;
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
//
//
//      Formula := AllotmentItem.Allotment;
//      WriteFloatValueFromGlobalFormula(Formula, AFarm,
//        'Invalid ground water allotment formula in ' + AFarm.FarmName);
    end
    else
    begin
      WriteInteger(1);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              WriteInteger(0);
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            AllotmentItem := AFarm.GwAllotment.ItemByStartTime(StartTime) as TAllotmentItem;
            WriteAllotmentItem(AFarm, AllotmentItem);

            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            WriteInteger(0);
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.GwAllotment.Count > 0 then
          begin
            AllotmentItem := AFarm.GwAllotment.First as TAllotmentItem;
          end
          else
          begin
            AllotmentItem := nil;
          end;
          WriteAllotmentItem(AFarm, AllotmentItem);

          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FAllotments.GroundWater.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlGwAllotment;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP ALLOTMENTS: GROUNDWATER';
  RequiredValues.ErrorID := 'FMP ALLOTMENTS: GROUNDWATER';
  RequiredValues.ID := 'GROUNDWATER';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FAllotments.GroundWater.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidGroundWaterAllotment;
  case FAllotments.GroundWaterAllotmentMethod of
    amHeight:
      begin
        RequiredValues.Option := 'HEIGHT ';
      end;
    amVolume:
      begin
        RequiredValues.Option := 'VOLUME ';
      end;
    amRate:
      begin
        RequiredValues.Option := 'RATE ';
      end;
  end;
  RequiredValues.FarmProperty := FAllotments.GroundWater;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlGwAllotment);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteGroundwaterRootInteraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  ACrop: TCropItem;
begin
  if FLandUse.GroundwaterRootInteraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlGwRootInteraction;
  RequiredValues.DefaultValue := 5;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: GROUNDWATER_ROOT_INTERACTION';
  RequiredValues.ErrorID := 'FMP LAND_USE: GROUNDWATER_ROOT_INTERACTION';
  RequiredValues.ID := 'GROUNDWATER_ROOT_INTERACTION';
  RequiredValues.StaticDataName := KGWRootInteraction;
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckError :=  StrInvalidGroundwater;
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndFive;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.GroundwaterRootInteraction;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlGwRootInteraction);
  end;

  if (FLandUse.GroundwaterRootInteraction.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
    end
    else
    begin
      DataArrayNames := TStringList.Create;
      try
        for CropIndex := 0 to Model.FmpCrops.Count - 1 do
        begin
          DataArrayNames.Add(
            Model.FmpCrops[CropIndex].GroundwaterRootInteractionDataArrayName);
        end;
        RequiredValues.LandUseStaticFileNames := DataArrayNames;
        WriteLandUseArrayData(AFileName, RequiredValues);
      finally
        DataArrayNames.Free;
      end;
    end;
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues, UnitConversionScaleFactor,
      ExternalScaleFileName);
    WriteString('STATIC LIST DATAFILE ');
    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      try
        FWriteLocation := RequiredValues.WriteLocation;
        for CropIndex := 0 to Model.FmpCrops.Count - 1 do
        begin
          ACrop := Model.FmpCrops[CropIndex];
          WriteInteger(CropIndex + 1);
          WriteInteger(ACrop.GroundwaterRootInteraction.InteractionCode);
          NewLine;
        end;
      finally
        FWriteLocation := wlMain;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteIrrigation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  TimeIndex: Integer;
  StartTime: Double;
  ACrop: TCropItem;
  IrregationItem: TCropIrrigationItem;
  procedure WriteIrrigationItem(ACrop: TCropItem; IrregationItem: TCropIrrigationItem);
  var
    Formula: string;
  begin
    if IrregationItem <> nil then
    begin
      Formula := IrregationItem.Irrigation;
      WriteIntegerValueFromGlobalFormula(Formula, ACrop,
        'Invalid Irrigation formula in ' + ACrop.CropName);
    end
    else
    begin
      WriteInteger(0);
    end;
    NewLine;
  end;
begin
  if FLandUse.Irrigation.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlIrrigation;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: IRRIGATION';
  RequiredValues.ErrorID := 'FMP LAND_USE: IRRIGATION';
  RequiredValues.ID := 'IRRIGATION';
  RequiredValues.StaticDataName := KIrrigation;
  RequiredValues.WriteTransientData :=
    (FLandUse.Irrigation.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidIrrigationV;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.Irrigation;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlIrrigation);
  end;

  if (FLandUse.Irrigation.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].IrrigationDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.WriteTransientData then
    begin
      WriteString('TRANSIENT LIST DATAFILE ');
    end
    else
    begin
      WriteString('STATIC LIST DATAFILE ');
    end;

    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      try

        FWriteLocation := RequiredValues.WriteLocation;
        if RequiredValues.WriteTransientData then
        begin
          for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
          begin
            WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

            StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

            for CropIndex := 0 to Model.FmpCrops.Count - 1 do
            begin
              ACrop := Model.FmpCrops[CropIndex];
              WriteInteger(CropIndex+1);
              IrregationItem := ACrop.IrrigationCollection.
                ItemByStartTime(StartTime) as TCropIrrigationItem;
              WriteIrrigationItem(ACrop, IrregationItem);
            end;
          end;
        end
        else
        begin
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            ACrop := Model.FmpCrops[CropIndex];
            WriteInteger(CropIndex + 1);
            if ACrop.IrrigationCollection.Count > 0 then
            begin
              IrregationItem := ACrop.IrrigationCollection.First as TCropIrrigationItem;
            end
            else
            begin
              IrregationItem := nil;
            end;
            WriteIrrigationItem(ACrop, IrregationItem);
          end;
        end;
      finally
        FWriteLocation := wlMain;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteIrrigationUniformity;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  FarmIndex: Integer;
  AFarm: TFarm;
  IrrigationTypes: TIrrigationCollection;
  OFE: TFarmEfficienciesItem;
  FarmID: Integer;
  InnerFarmIndex: Integer;
  IrrIndex: Integer;
  OfeItem: TCropEfficiencyItem;
  TimeIndex: Integer;
  StartTime: Double;
  procedure WriteItem(AFarm: TFarm; IrrIndex: Integer; OfeItem: TCropEfficiencyItem);
  var
    Formula: string;
  begin
    if OfeItem <> nil then
    begin
      Formula := OfeItem.Efficiency;
      WriteFloatValueFromGlobalFormula(Formula, AFarm,
        Format('Invalid irrigation uniformity for farm %0:s in irrigation type %1:s.',
        [AFarm.FarmName, IrrigationTypes[IrrIndex].Name]));
    end
    else
    begin
      WriteFloat(1);
    end;
  end;
begin
  if FSalinityFlush.FarmIrrigationUniformityChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlIrrigationUniformity;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: WBS_IRRIGATION_UNIFORMITY';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: WBS_IRRIGATION_UNIFORMITY';
  RequiredValues.ID := 'WBS_IRRIGATION_UNIFORMITY';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FSalinityFlush.FarmIrrigationUniformityChoice.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidIrrigationU;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.FarmIrrigationUniformityChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlIrrigationUniformity);
  end;

  IrrigationTypes := Model.IrrigationTypes;
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    try
      FWriteLocation := RequiredValues.WriteLocation;
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));

          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              for IrrIndex := 0 to IrrigationTypes.Count - 1 do
              begin
                WriteFloat(1);
              end;
              Inc(FarmID);
              NewLine;
            end;
            Assert(AFarm.IrrigationUniformity.Count
              = IrrigationTypes.Count);
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);
            for IrrIndex := 0 to AFarm.IrrigationUniformity.Count - 1 do
            begin
              OFE := AFarm.IrrigationUniformity[IrrIndex];
              OfeItem := OFE.CropEfficiency.
                ItemByStartTime(StartTime) as TCropEfficiencyItem;
              WriteItem(AFarm, IrrIndex, OfeItem);
            end;
            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            for IrrIndex := 0 to IrrigationTypes.Count - 1 do
            begin
              WriteFloat(1);
            end;
            Inc(FarmID);
            NewLine;
          end;
          Assert(AFarm.IrrigationUniformity.Count
            = IrrigationTypes.Count);
          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);
          for IrrIndex := 0 to AFarm.IrrigationUniformity.Count - 1 do
          begin
            OFE := AFarm.IrrigationUniformity[IrrIndex];
            if OFE.CropEfficiency.Count > 0 then
            begin
              OfeItem := OFE.CropEfficiency.First;
            end
            else
            begin
              OfeItem := nil;
            end;
            WriteItem(AFarm, IrrIndex, OfeItem);
          end;
          Inc(FarmID);
          NewLine;
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteLandUse;
begin
  if FLandUse.IsSelected then
  begin
    WriteString('BEGIN LAND_USE');
    NewLine;

    WriteLandUseOption;

    WriteLandUseLocation;
    WriteLandUseAreaFraction;
    WriteLandUsePrintOptions;

    // SPECIFY_PRINT_ALL_CROPS
    WriteSpecifyPrintAllCrops;
    // CROP_NAME
    WriteCropName;

    WriteCropCoefficient;
    WriteConsumptiveUse;
    WriteIrrigation;
    WriteRootDepth;
    WriteRootPressure;

    WriteGroundwaterRootInteraction;
    WriteTranspirationFraction;
    WriteEvaporationIrrigationFraction;
    WriteFractionOfPrecipToSurfaceWater;
    WriteFractionOfIrrigToSurfaceWater;
    WritePondDepth;
    WriteAddedDemand;
    WriteZeroConsumptiveUseBecomesBareSoil;

    WriteString('  MIN_BARE_FRACTION');
    WriteFloat(FLandUse.MinimumBareFraction);
    NewLine;

    WriteEvapIrrigateFracSumOneCorrection;

    WriteString('  RELAXATION_FACTOR_HEAD_CHANGE');
    WriteFloat(FLandUse.RelaxFracHeadChange);
    NewLine;

    WriteCropsThatSpecifySurfaceElevation;
    WriteCropSurfaceElevation;
    WriteSurfaceElevationOffset;

    WriteString('END LAND_USE');
    NewLine;
    NewLine;
  end;
end;

function TModflowFmp4Writer.GetLandUseFractionCollection(Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.LandUseFractionCollection;
end;

function TModflowFmp4Writer.GetLeachingRequirementCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.LeachingRequirementCollection;
end;

function TModflowFmp4Writer.GetMaxLeachingRequirementCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.MaxLeachingRequirementCollection;
end;

function TModflowFmp4Writer.GetPondDepthCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.PondDepthCollection;
end;

function TModflowFmp4Writer.GetRootDepthCollection(
  Crop: TCropItem): TOwhmCollection;
begin
  result := Crop.FmpRootDepthCollection;
end;

procedure TModflowFmp4Writer.WriteLandUseAreaFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.LandUseFraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateLandUseAreaFraction;

  RequiredValues.WriteLocation := wlLandUseAreaFraction;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: LAND_USE_AREA_FRACTION';
  RequiredValues.ErrorID := 'FMP LAND_USE: LAND_USE_AREA_FRACTION';
  RequiredValues.ID := 'LAND_USE_AREA_FRACTION';
  RequiredValues.StaticDataName := KLandUseAreaFraction;
  RequiredValues.WriteTransientData :=
    (FLandUse.LandUseFraction.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidLandUseArea;
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.LandUseFraction;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlLandUseAreaFraction);
  end;

  if (FLandUse.LandUseFraction.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].LandUseAreaFractionDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetLandUseFractionCollection,
      StrInvalidLandUseAre);
  end;
end;

procedure TModflowFmp4Writer.WriteLandUseArrayData(AFileName: string;
  RequiredValues: TRequiredValues);
var
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  DataSetList: TList;
  IsConstant: Boolean;
  index: Integer;
  DataArray: TDataArray;
  ConstantValue: double;
begin
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    Assert(False)
  end
  else
  begin
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

//    if RequiredValues.Option <> '' then
//    begin
//      WriteString(RequiredValues.Option + ' ');
//    end;

    if ExternalFileName <> '' then
    begin
      WriteString('STATIC ARRAY DATAFILE ');
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      Assert(RequiredValues.LandUseStaticFileNames <> nil);
      DataSetList := TList.Create;
      try
        IsConstant := True;
        ConstantValue := 0.0;
        for index := 0 to RequiredValues.LandUseStaticFileNames.Count - 1 do
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(
            RequiredValues.LandUseStaticFileNames[index]);
          DataSetList.Add(DataArray);
          Assert(DataArray <> nil);
          DataArray.Initialize;
          if DataArray.IsUniform <> iuTrue then
          begin
            IsConstant := False;
          end;
          if IsConstant then
          begin
            if (index = 0) then
            begin
              ConstantValue := DataArray.RealData[0,0,0];
            end
            else
            begin
              IsConstant := ConstantValue = DataArray.RealData[0,0,0];
            end;
          end;
        end;

        if IsConstant then
        begin
          WriteString('STATIC CONSTANT ');
          WriteFloat(ConstantValue);
          NewLine;
        end
        else
        begin
          WriteString('STATIC ARRAY DATAFILE ');
          WriteString(ExtractFileName(AFileName));
          NewLine;
          FWriteLocation := RequiredValues.WriteLocation;
          try
            for Index := 0 to DataSetList.Count -1 do
            begin
              DataArray := DataSetList[Index];
              WriteArray(DataArray, 0, RequiredValues.ErrorID, '',
                RequiredValues.ID, False, False, True);
              if Assigned(RequiredValues.CheckProcedure) then
              begin
                RequiredValues.CheckProcedure(DataArray, RequiredValues.CheckError);
              end;
            end;
          finally
            FWriteLocation := wlMain;
          end;
        end;
      finally
        DataSetList.Free;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteOptions;
begin
  if not FFarmProcess4.Print or FFarmProcess4.WELLFIELD
    or FFarmProcess4.Recompute or FFarmProcess4.UseMnwCriteria then
  begin
    WriteString('BEGIN OPTIONS');
    NewLine;

    if not FFarmProcess4.Print then
    begin
      WriteString('  NOPRINT');
      NewLine;
    end;

    if  FFarmProcess4.WELLFIELD then
    begin
      WriteString('  WELLFIELD');
      NewLine;
    end;

    if  FFarmProcess4.Recompute then
    begin
      WriteString('  RECOMP_Q_BD');
      NewLine;
    end;

//    if  FFarmProcess4.UseMnwCriteria then
//    begin
//      WriteString('  MNWCLOSE');
//      WriteFloat(FFarmProcess4.MnwQClose);
//      WriteFloat(FFarmProcess4.MnwHPercent);
//      WriteFloat(FFarmProcess4.MnwRPercent);
//      NewLine;
//    end;

    WriteString('END OPTIONS');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteOutput;
var
  WaterUse: TFarmPrint;
  OutputFile: string;
begin
  WriteString('BEGIN OUTPUT');
  NewLine;

  for WaterUse in FFarmProcess4.FarmPrints do
  begin
    case WaterUse of
      fpWbs_Water_Use:
        begin
          WriteString('  WBS_WATER_USE ');
          OutputFile := ChangeFileExt(FInputFileName, '.WBS_WATER_USE');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpFarm_Demand_Supply_Summary:
        begin
          WriteString('  FARM_DEMAND_SUPPLY_SUMMARY ');
          OutputFile := ChangeFileExt(FInputFileName, '.FARM_DEMAND_SUPPLY_SUMMARY');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpFarm_Budget:
        begin
          WriteString('  FARM_BUDGET ');
          OutputFile := ChangeFileExt(FInputFileName, '.FARM_BUDGET');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpFarm_Budget_Compact:
        begin
          WriteString('  FARM_BUDGET_COMPACT ');
          OutputFile := ChangeFileExt(FInputFileName, '.FARM_BUDGET_COMPACT');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpFarm_Net_Recharge_Array:
        begin
          WriteString('  FARM_NET_RECHARGE_ARRAY ');
          OutputFile := ChangeFileExt(FInputFileName, '.FARM_NET_RECHARGE_ARRAY');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpFarm_Net_Recharge_List:
        begin
          WriteString('  FARM_NET_RECHARGE_LIST ');
          OutputFile := ChangeFileExt(FInputFileName, '.FARM_NET_RECHARGE_LIST');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpEvapotranspiration_SummarySum:
        begin
          WriteString('  EVAPOTRANSPIRATION_SUMMARY SUM ');
          OutputFile := ChangeFileExt(FInputFileName, '.EVAPOTRANSPIRATION_SUMMARY_SUM');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpEvapotranspiration_SummarySeparate:
        begin
          WriteString('  EVAPOTRANSPIRATION_SUMMARY SEPARATE ');
          OutputFile := ChangeFileExt(FInputFileName, '.EVAPOTRANSPIRATION_SUMMARY_SEPARATE');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpEt_List:
        begin
          WriteString('  ET_LIST ');
          OutputFile := ChangeFileExt(FInputFileName, '.ET_LIST');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpFarm_Well_Summary:
        begin
          WriteString('  FARM_WELL_SUMMARY ');
          OutputFile := ChangeFileExt(FInputFileName, '.FARM_WELL_SUMMARY');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpLandscape_Runoff:
        begin
          WriteString('  LANDSCAPE_RUNOFF COMPACT ');
          OutputFile := ChangeFileExt(FInputFileName, '.LANDSCAPE_RUNOFF');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
      fpDeep_Percolation:
        begin
          WriteString('  DEEP_PERCOLATION COMPACT ');
          OutputFile := ChangeFileExt(FInputFileName, '.DEEP_PERCOLATION');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
        end;
    end;
  end;

  case FFarmProcess4.Routing_Information of
    foNotUsed: ; // do nothing
    foStatic:
      begin
          WriteString('  ROUTING_INFORMATION STATIC ');
          OutputFile := ChangeFileExt(FInputFileName, '.ROUTING_INFORMATION');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
      end;
    foTransient:
      begin
          WriteString('  ROUTING_INFORMATION TRANSIENT ');
          OutputFile := ChangeFileExt(FInputFileName, '.ROUTING_INFORMATION');
          WriteString(ExtractFileName(OutputFile));
          Model.AddModelOutputFile(OutputFile);
          NewLine;
      end;
  end;

//  {ET_LIST,  FARM_WELL_SUMMARY,   LANDSCAPE_RUNOFF [COMPACT]}
//  fpEt_List, fpFarm_Well_Summary, fpLandscape_Runoff,
//  {DEEP_PERCOLATION    [COMPACT]}
//  fpDeep_Percolation);

//  TFarmPrint = (

  WriteString('END OUTPUT');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);

end;

procedure TModflowFmp4Writer.WritePondDepth;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FLandUse.PondDepth.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlPondDepth;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: POND_DEPTH';
  RequiredValues.ErrorID := 'FMP LAND_USE: POND_DEPTH';
  RequiredValues.ID := 'POND_DEPTH';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FLandUse.PondDepth.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.PondDepth;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlPondDepth);
  end;

  WriteOwhmList(RequiredValues, AFileName, GetPondDepthCollection,
    StrInvalidPondDepthF);

end;

procedure TModflowFmp4Writer.WritePrecipitation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Precipitation.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluatePrecip;

  RequiredValues.WriteLocation := wlPFLX;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'Data Set 33: PFLX';
  RequiredValues.ErrorID := 'FMP CLIMATE: PRECIPITATION';
  RequiredValues.ID := 'PRECIPITATION';
  RequiredValues.StaticDataName := KPrecipitation;
  RequiredValues.WriteTransientData := FClimatePackage.TransientPrecipUsed(Self);
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FClimatePackage.Precipitation;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlPFLX);
  end;

  WriteFmpArrayData(AFileName, RequiredValues);
  WriteArrayTemplate(RequiredValues);
end;

procedure TModflowFmp4Writer.WritePrecipPotConsumption;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Precipitation_Potential_Consumption.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluatePrecipPotConsumption;

  RequiredValues.WriteLocation := wlPrecipPotConsumption;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP CLIMATE: PRECIPITATION_POTENTIAL_CONSUMPTION';
  RequiredValues.ErrorID := 'FMP CLIMATE: PRECIPITATION_POTENTIAL_CONSUMPTION';
  RequiredValues.ID := 'PRECIPITATION_POTENTIAL_CONSUMPTION';
  RequiredValues.StaticDataName := KPrecipPotConsumption;
  RequiredValues.WriteTransientData :=
    (FClimatePackage.Precipitation_Potential_Consumption.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidPrecipitatio;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlPrecipPotConsumption);
  end;

  if (FClimatePackage.Precipitation_Potential_Consumption.ArrayList = alArray) then
  begin
    case FClimatePackage.PrecipPotConsum of
      ppcLength:
      begin
        RequiredValues.CheckProcedure := nil;
        RequiredValues.Option := 'BY_LENGTH';
      end;
      ppcFraction:
        begin
          RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
          RequiredValues.Option := 'BY_FRACTION';
        end;
    end;
    RequiredValues.FarmProperty := FClimatePackage.Precipitation_Potential_Consumption;

    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteRebuildFullyRoutedReturn;
begin
  if FSurfaceWater4.Rebuild_Fully_Routed_Return then
  begin
    WriteString('  REBUILD_FULLY_ROUTED_RETURN');
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteRefET;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.ReferenceET.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateReferenceET;

  RequiredValues.WriteLocation := wlETR;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'Data Set 30b: ETR';
  RequiredValues.ErrorID := 'FMP CLIMATE: REFERENCE_ET';
  RequiredValues.ID := 'REFERENCE_ET';
  RequiredValues.StaticDataName := KRefET;
  RequiredValues.WriteTransientData := FClimatePackage.TransientEvapUsed(Self);
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FClimatePackage.ReferenceET;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlETR);
  end;

  WriteFmpArrayData(AFileName, RequiredValues);
  WriteArrayTemplate(RequiredValues);
end;

procedure TModflowFmp4Writer.WriteRootDepth;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.RootDepth.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  EvaluateRootDepth;

  RequiredValues.WriteLocation := wlRootDepth;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: ROOT_DEPTH';
  RequiredValues.ErrorID := 'FMP LAND_USE: ROOT_DEPTH';
  RequiredValues.ID := 'ROOT_DEPTH';
  RequiredValues.StaticDataName := KRootDepth;
  RequiredValues.WriteTransientData :=
    (FLandUse.RootDepth.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidRootDepthV;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.RootDepth;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlRootDepth);
  end;

  if (FLandUse.RootDepth.ArrayList = alArray) then
  begin
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
      WriteArrayTemplate(RequiredValues);
    end
    else
    begin
      if RequiredValues.WriteTransientData then
      begin
        RequiredValues.MaxDataTypeIndex := Model.FmpCrops.Count -1;
        WriteFmpArrayData(AFileName, RequiredValues);
        WriteArrayTemplate(RequiredValues);
      end
      else
      begin
        DataArrayNames := TStringList.Create;
        try
          for CropIndex := 0 to Model.FmpCrops.Count - 1 do
          begin
            DataArrayNames.Add(
              Model.FmpCrops[CropIndex].RootDepthDataArrayName);
          end;
          RequiredValues.LandUseStaticFileNames := DataArrayNames;
          WriteLandUseArrayData(AFileName, RequiredValues);
        finally
          DataArrayNames.Free;
        end;
      end;
    end;
  end
  else
  begin
    WriteOwhmList(RequiredValues, AFileName, GetRootDepthCollection,
      StrInvalidRootDepthF);
  end;
end;

procedure TModflowFmp4Writer.WriteRootPressure;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
var
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  procedure WriteRootPressureItem(Crop: TCropItem;
    RootPressureItem: TRootPressureItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if RootPressureItem <> nil then
    begin
      PestValues.Formula := RootPressureItem.Psi1;

      PestValues.PestName := '';
      PestValues.PestSeriesName := Crop.RootPressureCollection.PestSeriesParameter;
      PestValues.PestSeriesMethod := Crop.RootPressureCollection.PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid Anoxia Pressure formula in ' + Crop.CropName;
      PestValues.ErrorObjectName := Crop.CropName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, Crop,
          PestValues.FormulaErrorMessage);
      end;



      PestValues.Formula := RootPressureItem.Psi2;

      PestValues.PestName := '';
      PestValues.PestSeriesName := Crop.RootPressureCollection.P2PestSeriesParameter;
      PestValues.PestSeriesMethod := Crop.RootPressureCollection.P2PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid High Optimal Pressure formula in ' + Crop.CropName;
      PestValues.ErrorObjectName := Crop.CropName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, Crop,
          PestValues.FormulaErrorMessage);
      end;



      PestValues.Formula := RootPressureItem.Psi3;

      PestValues.PestName := '';
      PestValues.PestSeriesName := Crop.RootPressureCollection.P3PestSeriesParameter;
      PestValues.PestSeriesMethod := Crop.RootPressureCollection.P3PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid Low Optimal Pressure formula in ' + Crop.CropName;
      PestValues.ErrorObjectName := Crop.CropName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, Crop,
          PestValues.FormulaErrorMessage);
      end;



      PestValues.Formula := RootPressureItem.Psi4;

      PestValues.PestName := '';
      PestValues.PestSeriesName := Crop.RootPressureCollection.P4PestSeriesParameter;
      PestValues.PestSeriesMethod := Crop.RootPressureCollection.P4PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid Wilting Pressure formula in ' + Crop.CropName;
      PestValues.ErrorObjectName := Crop.CropName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, Crop,
          PestValues.FormulaErrorMessage);
      end;
    end
    else
    begin
      WriteInteger(0);
      WriteInteger(0);
      WriteInteger(0);
      WriteInteger(0);
    end;
  end;
  procedure WriteTransientData;
  var
    TimeIndex: Integer;
    CropIndex: Integer;
    StartTime: Double;
    ACrop: TCropItem;
    RootPressureItem: TRootPressureItem;
  begin
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      WriteCommentLine(Format(StrStressPeriodD, [TimeIndex + 1]));
      StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;
      for CropIndex := 0 to Model.FmpCrops.Count - 1 do
      begin
        ACrop := Model.FmpCrops[CropIndex];
        WriteInteger(CropIndex + 1);
        RootPressureItem := ACrop.RootPressureCollection.ItemByStartTime(StartTime) as TRootPressureItem;
        WriteRootPressureItem(ACrop, RootPressureItem);
        NewLine;
      end;
    end;
  end;
  procedure WriteStaticData;
  var
    CropIndex: Integer;
    ACrop: TCropItem;
    RootPressureItem: TRootPressureItem;
  begin
    for CropIndex := 0 to Model.FmpCrops.Count - 1 do
    begin
      ACrop := Model.FmpCrops[CropIndex];
      WriteInteger(CropIndex + 1);
      if ACrop.RootPressureCollection.Count > 0 then
      begin
        RootPressureItem := ACrop.RootPressureCollection.First as TRootPressureItem;
      end
      else
      begin
        RootPressureItem := nil;
      end;
      WriteRootPressureItem(ACrop, RootPressureItem);
      NewLine;
    end;
  end;
  procedure WriteListData;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        WriteTransientData;
      end
      else
      begin
        WriteStaticData;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
begin
  if FLandUse.RootPressure.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlRootPressure;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: ROOT_PRESSURE';
  RequiredValues.ErrorID := 'FMP LAND_USE: ROOT_PRESSURE';
  RequiredValues.ID := 'ROOT_PRESSURE';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FLandUse.RootPressure.FarmOption = foTransient);
  RequiredValues.CheckError :=  StrInvalidRootPressur;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.RootPressure;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlRootPressure);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues, UnitConversionScaleFactor,
    ExternalScaleFileName);
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSalinityAppliedWater;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSalinityFlush.CropExtraWaterChoice.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlSalinityAppliedWater;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_SALINITY_APPLIED_WATER';
  RequiredValues.ErrorID := 'FMP SALINITY_FLUSH_IRRIGATION: CROP_SALINITY_APPLIED_WATER';
  RequiredValues.ID := 'CROP_SALINITY_APPLIED_WATER';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FSalinityFlush.CropExtraWaterChoice.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSalinityFlush.CropExtraWaterChoice;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSalinityAppliedWater);
  end;

  WriteLeachList(RequiredValues, AFileName, GetSalinityAppliedWaterCollection,
    'Invalid Salinity of Applied Water formula in ');

end;

procedure TModflowFmp4Writer.WriteSalinityFlush;
begin
  if FSalinityFlush.IsSelected then
  begin
    WriteString('BEGIN SALINITY_FLUSH_IRRIGATION');
    NewLine;

    WriteSalinityFlushPrintOptions;

    // EXPRESSION_LINE_LENGTH
    WriteExpressionLineLength;

    WriteExpressionVariableNearZero;

    // WBS_SUPPLY_SALT_CONCENTRATION
    WriteSaltSupplyConcentration;

    WriteIrrigationUniformity;
    WriteCropHasSalinityDemand;
    WriteCropSalinityTolerance;
    WriteCropMaxLeachingRequirement;
//   CROP_LEACHING_REQUIREMENT
    WriteCropLeachingRequirement;

//   CROP_SALINITY_APPLIED_WATER
    WriteSalinityAppliedWater;


    WriteString('END SALINITY_FLUSH_IRRIGATION');
    NewLine;
    NewLine;
  end;

end;

procedure TModflowFmp4Writer.WriteSoil;
begin
  if not FSoil4.IsSelected then
  begin
    Exit;
  end;
  if Model.FmpSoils.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoSoilsDefined,
      StrNoSoilsAreDefined);
    Exit;
  end;

  WriteString('BEGIN SOIL');
  NewLine;

  WriteCapillaryFringe;
  WriteSoilID;
  WriteSoilCoefficient;
  WriteSurfaceK;
  WriteEffectivePrecipitationTable;

  WriteString('END SOIL');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteSoilCoefficient;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  SoilID: Integer;
  ASoil: TSoilItem;
  procedure WriteListData;
  var
    SoilIndex: Integer;
    procedure WriteItem(const Formula, ErrorMessage: string);
    var
      Value: double;
      PestValues: TPestValues;
    begin
      PestValues.Formula := Formula;

      PestValues.PestName := '';
      PestValues.PestSeriesName := '';
      PestValues.PestSeriesMethod := ppmMultiply;
      PestValues.FormulaErrorMessage := ErrorMessage;
      PestValues.ErrorObjectName := ASoil.SoilName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, ASoil,
          PestValues.FormulaErrorMessage);
      end;
    end;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      for SoilIndex := 0 to FFmpSoils.Count - 1 do
      begin
        ASoil := FFmpSoils[SoilIndex];
        SoilID := SoilIndex + 1;
        WriteInteger(SoilID);
        case ASoil.SoilType of
          stSand:
            begin
              WriteString(' SAND')
            end;
          stSandyLoam:
            begin
              WriteString(' SANDYLOAM')
            end;
          stSilt:
            begin
              WriteString(' SILT')
            end;
          stSiltyClay:
            begin
              WriteString(' SILTYCLAY')
            end;
          stOther:
            begin
              WriteItem(ASoil.ACoeff, 'Invalid formula for soil coefficient A in soil ' + ASoil.SoilName);
              WriteItem(ASoil.BCoeff, 'Invalid formula for soil coefficient B in soil ' + ASoil.SoilName);
              WriteItem(ASoil.CCoeff, 'Invalid formula for soil coefficient C in soil ' + ASoil.SoilName);
              WriteItem(ASoil.DCoeff, 'Invalid formula for soil coefficient D in soil ' + ASoil.SoilName);
              WriteItem(ASoil.ECoeff, 'Invalid formula for soil coefficient E in soil ' + ASoil.SoilName);
            end;
          stUndefined:
            begin
              frmErrorsAndWarnings.AddError(Model, StrSoilTypeUndefined,
                Format('The soil type has not been defined for %s', [ASoil.SoilName]))
            end;
          else
            Assert(False)
        end;
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FLandUse.IsSelected and (FLandUse.RootPressure.FarmOption <> foNotUsed) then
  begin
    if FSoil4.Coefficient.FarmOption = foNotUsed then
    begin
      frmErrorsAndWarnings.AddError(Model, StrSoilCoefficientNot, StrWhenTheRootPressu)
    end;
  end;

  if FSoil4.Coefficient.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlSoilCoefficient;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SOIL: COEFFICIENT';
  RequiredValues.ErrorID := 'FMP SOIL: COEFFICIENT';
  RequiredValues.ID := 'COEFFICIENT';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSoilCoeffic;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSoil4.Coefficient;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSoilCoefficient);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);
  WriteString('STATIC LIST DATAFILE ');
  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSoilID;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if not FSoil4.IsSelected then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlSoilID;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SOIL: SOIL_ID ';
  RequiredValues.ErrorID := 'FMP SOIL: SOIL_ID ';
  RequiredValues.ID := 'SOIL_ID ';
  RequiredValues.StaticDataName := KSoilID;
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSoilIDValu;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

  AFileName := GetFileStreamName(wlSoilID);

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteSpecifyPrintAllCrops;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
//  DataArrayNames: TStringList;
  CropIndex: Integer;
  ACrop: TCropItem;
begin
  if FLandUse.SpecifyCropsToPrint = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlSpecifyPrintCrops;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtBoolean;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: SPECIFY_PRINT_ALL_CROPS';
  RequiredValues.ErrorID := 'FMP LAND_USE: SPECIFY_PRINT_ALL_CROPS';
  RequiredValues.ID := 'SPECIFY_PRINT_ALL_CROPS';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

//  if RequiredValues.FarmProperty.ExternalFileName = '' then
//  begin
    AFileName := GetFileStreamName(wlSpecifyPrintCrops);
//  end;

  WriteScaleFactorsID_andOption(RequiredValues, '', '');
  WriteString('STATIC LIST DATAFILE ');
  WriteString(ExtractFileName(AFileName));
  NewLine;
  try
    FWriteLocation := RequiredValues.WriteLocation;

    for CropIndex := 0 to Model.FmpCrops.Count - 1 do
    begin
      ACrop := Model.FmpCrops[CropIndex];
      WriteInteger(CropIndex+1);
      if ACrop.Print then
      begin
        WriteInteger(1);
      end
      else
      begin
        WriteInteger(0);
      end;
      NewLine;
    end;

  finally
    FWriteLocation := wlMain;
  end;

end;

procedure TModflowFmp4Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
begin
  inherited;

end;

procedure TModflowFmp4Writer.WriteString(const Value: AnsiString);
begin
  if Length(Value) > 0 then
  begin
    case FWriteLocation of
      wlMain: inherited;
      wlOpenClose:
        begin
          Assert(FOpenCloseFileStream <> nil);
          FOpenCloseFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlOFE:
        begin
          Assert(False);
//          Assert(FOFE_FileStream <> nil);
//          FOFE_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCID:
        begin
          Assert(FCID_FileStream <> nil);
          FCID_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlFID:
        begin
          Assert(FFID_FileStream <> nil);
          FFID_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlRoot:
        begin
          Assert(False);
//          Assert(FRoot_FileStream <> nil);
//          FRoot_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropUse:
        begin
          Assert(False);
//          Assert(FCropUse_FileStream <> nil);
//          FCropUse_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlETR:
        begin
          Assert(FETR_FileStream <> nil);
          FETR_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEtFrac:
        begin
          Assert(False);
//          Assert(FET_Frac_FileStream <> nil);
//          FET_Frac_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSwLosses:
        begin
          Assert(False);
//          Assert(FSW_Losses_FileStream <> nil);
//          FSW_Losses_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPFLX:
        begin
          Assert(FPFLX_FileStream <> nil);
          FPFLX_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropFunc:
        begin
          Assert(False);
//          Assert(FCropFunc_FileStream <> nil);
//          FCropFunc_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlWaterCost:
        begin
          Assert(False);
//          Assert(FWaterCost_FileStream <> nil);
//          FWaterCost_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDeliveries:
        begin
          Assert(FNonRoutedDeliveryFileStream <> nil);
          FNonRoutedDeliveryFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteDelivery:
        begin
          Assert(FSemiRoutedDeliveryFileStream <> nil);
          FSemiRoutedDeliveryFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteReturn:
        begin
          Assert(FSemiRoutedReturnFileStream <> nil);
          FSemiRoutedReturnFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCall:
        begin
          Assert(False);
//          Assert(FCall_FileStream <> nil);
//          FCall_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEfficiency:
        begin
          Assert(FEFFICIENCY_FileStream <> nil);
          FEFFICIENCY_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEfficiencyImprovement:
        begin
          Assert(FEFFICIENCY_IMPROVEMENT_FileStream <> nil);
          FEFFICIENCY_IMPROVEMENT_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlBareRunoffFraction:
        begin
          Assert(FBARE_RUNOFF_FRACTION_FileStream <> nil);
          FBARE_RUNOFF_FRACTION_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlBarePrecipitationConsumptionFraction:
        begin
          Assert(FBarePrecipitationConsumptionFractionFileStream <> nil);
          FBarePrecipitationConsumptionFractionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCapillaryFringe:
        begin
          Assert(FCapillaryFringeFileStream <> nil);
          FCapillaryFringeFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSoilID:
        begin
          Assert(FSoilIdStream <> nil);
          FSoilIdStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSurfaceK:
        begin
          Assert(FSurfaceKFileStream <> nil);
          FSurfaceKFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlBareEvap:
        begin
          Assert(FEvapBareFileStream <> nil);
          FEvapBareFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDirectRecharge:
        begin
          Assert(FDirectRechargeFileStream <> nil);
          FDirectRechargeFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPrecipPotConsumption:
        begin
          Assert(FPrecipPotConsumptionFileStream <> nil);
          FPrecipPotConsumptionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlNrdInfilLoc:
        begin
          Assert(FNrdInfilLocationFileStream <> nil);
          FNrdInfilLocationFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlLandUseAreaFraction:
        begin
          Assert(FLandUseAreaFractionFileStream <> nil);
          FLandUseAreaFractionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropCoefficient:
        begin
          Assert(FCropcoefficientFileStream <> nil);
          FCropcoefficientFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlConsumptiveUse:
        begin
          Assert(FConsumptiveUseFileStream <> nil);
          FConsumptiveUseFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlIrrigation:
        begin
          Assert(FIrrigationFileStream <> nil);
          FIrrigationFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlRootDepth:
        begin
          Assert(FRootDepthFileStream <> nil);
          FRootDepthFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlGwRootInteraction:
        begin
          Assert(FGwRootInteractionStream <> nil);
          FGwRootInteractionStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlTranspirationFraction:
        begin
          Assert(FTranspirationFractionFileStream <> nil);
          FTranspirationFractionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEvaporationIrrigationFraction:
        begin
          Assert(FEvaporationIrrigationFractionFileStream <> nil);
          FEvaporationIrrigationFractionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlFractionOfPrecipToSurfaceWater:
        begin
          Assert(FFractionOfPrecipToSurfaceWaterFileStream <> nil);
          FFractionOfPrecipToSurfaceWaterFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlFractionOfIrrigToSurfaceWater:
        begin
          Assert(FFractionOfIrrigToSurfaceWaterFileStream <> nil);
          FFractionOfIrrigToSurfaceWaterFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlAddedDemand:
        begin
          Assert(FAddedDemandFileStream <> nil);
          FAddedDemandFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropHasSalinityDemand:
        begin
          Assert(FCropHasSalinityDemandFileStream <> nil);
          FCropHasSalinityDemandFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlAddedDemandRunoffSplit:
        begin
          Assert(FAddedDemandRunoffSplitFileStream <> nil);
          FAddedDemandRunoffSplitFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlIrrigationUniformity:
        begin
          Assert(FIrrigationUniformityFileStream <> nil);
          FIrrigationUniformityFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDeficiencyScenario:
        begin
          Assert(FDeficiencyScenarioFileStream <> nil);
          FDeficiencyScenarioFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlWaterSource:
        begin
          Assert(FWaterSourceFileStream <> nil);
          FWaterSourceFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlAddedCropDemandFlux:
        begin
          Assert(FAddedCropDemandFluxFileStream <> nil);
          FAddedCropDemandFluxFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlAddedCropDemandRate:
        begin
          Assert(FAddedCropDemandRateFileStream <> nil);
          FAddedCropDemandRateFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPrecipicationTable:
        begin
          Assert(FEffectivPrecipitationTableFileStream <> nil);
          FEffectivPrecipitationTableFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSoilCoefficient:
        begin
          Assert(FEffectivCoefficientTableFileStream <> nil);
          FEffectivCoefficientTableFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlNoReturnFlow:
        begin
          Assert(FNoReturnFlowFileStream <> nil);
          FNoReturnFlowFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteDeliveryLowerLimit:
        begin
          Assert(FSemiRoutedDeliveryLowerLimitFileStream <> nil);
          FSemiRoutedDeliveryLowerLimitFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteDeliveryUpperLimit:
        begin
          Assert(FSemiRoutedDeliveryUpperLimitFileStream <> nil);
          FSemiRoutedDeliveryUpperLimitFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSwAllotment:
        begin
          Assert(FSurfaceWaterAllotmentFileStream <> nil);
          FSurfaceWaterAllotmentFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlGwAllotment:
        begin
          Assert(FGroundWaterAllotmentFileStream <> nil);
          FGroundWaterAllotmentFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlRootPressure:
        begin
          Assert(FRootPressureFileStream <> nil);
          FRootPressureFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPondDepth:
        begin
          Assert(FPondDepthFileStream <> nil);
          FPondDepthFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlNoCropMeansBareSoil:
        begin
          Assert(FConvertToBareFileStream <> nil);
          FConvertToBareFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlNoEvapErrCorrection:
        begin
          Assert(FSumOneCorrectionFileStream <> nil);
          FSumOneCorrectionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSaltSupplyConcentration:
        begin
          Assert(FSaltSupplyConcentrationFileStream <> nil);
          FSaltSupplyConcentrationFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropSalinityTolerance:
        begin
          Assert(FCropSalinityToleranceFileStream <> nil);
          FCropSalinityToleranceFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropMaxLeachRequirement:
        begin
          Assert(FCropMaxLeachingRequirementFileStream <> nil);
          FCropMaxLeachingRequirementFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropLeachRequirement:
        begin
          Assert(FCropLeachingRequirementFileStream <> nil);
          FCropLeachingRequirementFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSalinityAppliedWater:
        begin
          Assert(FCropSalinityAppliedWaterFileStream <> nil);
          FCropSalinityAppliedWaterFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSupplyWells:
        begin
          Assert(FSupplyWellFileStream <> nil);
          FSupplyWellFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropNames:
        begin
          Assert(FCropNamesFileStream <> nil);
          FCropNamesFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSpecifyPrintCrops:
        begin
          Assert(FCropPrintFileStream <> nil);
          FCropPrintFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSupplyWell;
var
  PrintOption: TFarmWellPrint;
  OutputFile: string;
begin
  if FFarmWells4.IsSelected then
  begin
    WriteString('BEGIN SUPPLY_WELL');
    NewLine;

    if Model.ModflowOutputControl.PrintInputCellLists then
    begin
      WriteString('  PRINT INPUT ');
      OutputFile := ChangeFileExt(FInputFileName, '.Well_Data_Input');
      WriteString(ExtractFileName(OutputFile));
      Model.AddModelOutputFile(OutputFile);
      NewLine;
    end;

    for PrintOption in FFarmWells4.FarmWellPrints do
    begin
      case PrintOption of
        fwpPrint_ByWell:
          begin
            WriteString('  PRINT BYWELL ');
            OutputFile := ChangeFileExt(FInputFileName, '.Well_Data_By_Well');
            WriteString(ExtractFileName(OutputFile));
            Model.AddModelOutputFile(OutputFile);
            NewLine;
          end;
        fwpPrint_ByWbs:
          begin
            WriteString('  PRINT ByWBS ');
            OutputFile := ChangeFileExt(FInputFileName, '.Well_Data_By_WBS');
            WriteString(ExtractFileName(OutputFile));
            Model.AddModelOutputFile(OutputFile);
            NewLine;
          end;
        fwpPrint_ByMNW:
          begin
            WriteString('  PRINT ByMNW ');
            OutputFile := ChangeFileExt(FInputFileName, '.Well_Data_By_MNW');
            WriteString(ExtractFileName(OutputFile));
            Model.AddModelOutputFile(OutputFile);
            NewLine;
          end;
        fwpPrint_List:
          begin
            WriteString('  PRINT LIST ');
            NewLine;
          end;
        fwpPrint_Smoothing:
          begin
            WriteString('  PRINT SMOOTHING ');
            OutputFile := ChangeFileExt(FInputFileName, '.Well_Smoothing_Data');
            WriteString(ExtractFileName(OutputFile));
            Model.AddModelOutputFile(OutputFile);
            NewLine;
          end;
        fwpPrint_ByWbs_ByLayer:
          begin
            WriteString('  PRINT BYWBS_BYLAYER ');
            OutputFile := ChangeFileExt(FInputFileName, '.Well_Data_By_WBS_By_Layer');
            WriteString(ExtractFileName(OutputFile));
            Model.AddModelOutputFile(OutputFile);
            NewLine;
          end;
      end;
    end;

//    case FFarmWells4.Smoothing of
//      sNone: ;
//      sByFraction:
//        begin
//          WriteString('  SMOOTH ByFRACTION');
//          NewLine;
//        end;
//      sByThickness:
//        begin
//          WriteString('  SMOOTH ByTHICK');
//          NewLine;
//        end;
//      else
//        begin
//          Assert(False);
//        end;
//    end;
//
//    case FFarmWells4.ProrateDemand of
//      sNone: ;
//      sByFraction:
//        begin
//          WriteString('  SMOOTH ByFRACTION');
//          NewLine;
//        end;
//      sByThickness:
//        begin
//          WriteString('  SMOOTH ByTHICK');
//          NewLine;
//        end;
//      else
//        begin
//          Assert(False);
//        end;
//    end;

    WriteMnwPumpSpread;


    if FFarmWells4.WellXY = xyCoordinates then
    begin
      WriteString('  INPUT_OPTION XY');
      NewLine;
    end;

//    case FFarmWells4.WellLayerChoice of
//      plcLayer:
//        begin
//          // do nothing
//        end;
//      plcElevation:
//        begin
//          WriteString('  INPUT_OPTION ELEVATION');
//          NewLine;
//        end;
//      plcDepth:
//        begin
//          WriteString('  INPUT_OPTION DEPTH');
//          NewLine;
//        end;
//    end;

    WriteSupplyWells;

    WriteString('END SUPPLY_WELL');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteSupplyWells;
var
  AFileName: string;
begin
  WriteString('  TIME FRAME');
  NewLine;
  AFileName := GetFileStreamName(wlSupplyWells);
  WriteString('  OPEN/CLOSE ');
  WriteString(ExtractFileName(AFileName));
  NewLine;

  FWriteLocation := wlSupplyWells;
  try
    WriteStressPeriods('', '','','','')
  finally
    FWriteLocation := wlMain;
  end;

end;

procedure TModflowFmp4Writer.WriteSurfaceElevationOffset;
begin

end;

procedure TModflowFmp4Writer.WriteSurfaceK;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  ASoil: TSoilItem;
  SoilID: Integer;
  procedure WriteListData;
  var
    SoilIndex: Integer;
    Value: double;
    PestValues: TPestValues;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      for SoilIndex := 0 to FFmpSoils.Count - 1 do
      begin
        ASoil := FFmpSoils[SoilIndex];
        SoilID := SoilIndex + 1;
        WriteInteger(SoilID);
//        Formula := ASoil.SurfVK;

        PestValues.Formula := ASoil.SurfVK;

        PestValues.PestName := '';
        PestValues.PestSeriesName := '';
        PestValues.PestSeriesMethod := ppmMultiply;
        PestValues.FormulaErrorMessage := 'Invalid formula for surface vertical hydraulic conductivity in soil ' + ASoil.SoilName;
        PestValues.ErrorObjectName := ASoil.SoilName;

        AdjustFormulaForPest(PestValues);

        if WritingTemplate and PestValues.ParameterUsed then
        begin
          Value := GetFormulaValue(PestValues);

          WritePestTemplateFormula(Value, PestValues.PestName,
            PestValues.PestSeriesName, PestValues.PestSeriesMethod,
            nil);
        end
        else
        begin
          WriteFloatValueFromGlobalFormula(PestValues.Formula, ASoil,
            PestValues.FormulaErrorMessage);
        end;


//        WriteFloatValueFromGlobalFormula(Formula, ASoil,
//          'Invalid formula for surface vertical hydraulic conductivity in soil ' + ASoil.SoilName);
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FSoil4.SurfVertK.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSurfaceK;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP SOIL: SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
  RequiredValues.ErrorID := 'FMP SOIL: SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
  RequiredValues.ID := 'SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
  RequiredValues.StaticDataName := KSurfaceK;
  RequiredValues.WriteTransientData := False;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidSurfaceVert;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FSoil4.SurfVertK;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSurfaceK);
  end;

  if (FSoil4.SurfVertK.ArrayList = alArray) then
  begin
    WriteFmpArrayData(AFileName, RequiredValues);
    WriteArrayTemplate(RequiredValues);
  end
  else
  begin
    GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
      ExternalFileName, ExternalScaleFileName);
    WriteScaleFactorsID_andOption(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);
    WriteString('STATIC LIST DATAFILE ');
    if ExternalFileName <> '' then
    begin
      WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
      NewLine;
      Exit;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteListData;
      if FPestParamUsed then
      begin
        WritingTemplate := True;
        try
          GetFileStreamName(RequiredValues.WriteLocation);
          WriteListData;
        finally
          FPestParamUsed := False;
          WritingTemplate := False;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSurfaceWater;
begin
  if FSurfaceWater4.IsSelected then
  begin
    WriteString('BEGIN SURFACE_WATER');
    NewLine;

    WriteNonRoutedDelivery;
    WriteNrdInfilLocation;
    PrintSurfaceWaterOutputOptions;

    WriteSemiRoutedDelivery; 
    WriteSemiRoutedDeliveryLowerLimit; 
    WriteSemiRoutedDeliveryUpperLimit; 
    WriteSemiRoutedDeliveryClosureTolerance; 
    
    WriteNoReturnFlow;
    WriteSemiRoutedReturn;
    WriteRoutedReturn;
    WriteRebuildFullyRoutedReturn;

    WriteString('END SURFACE_WATER');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteSurfaceWaterAllotment;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  StartTime: Double;
  FarmID: Integer;
  AFarm: TFarm;
  AllotmentItem: TAllotmentItem;
  procedure WriteAllotmentItem(AFarm: TFarm; AllotmentItem: TAllotmentItem);
  var
    Value: double;
    PestValues: TPestValues;
  begin
    if AllotmentItem <> nil then
    begin
      PestValues.Formula := AllotmentItem.Allotment;

      PestValues.PestName := '';
      PestValues.PestSeriesName := AFarm.SWAllotment.PestSeriesParameter;
      PestValues.PestSeriesMethod := AFarm.SWAllotment.PestParamMethod;
      PestValues.FormulaErrorMessage := 'Invalid surface water allotment formula in ' + AFarm.FarmName;
      PestValues.ErrorObjectName := AFarm.FarmName;

      AdjustFormulaForPest(PestValues);

      if WritingTemplate and PestValues.ParameterUsed then
      begin
        Value := GetFormulaValue(PestValues);

        WritePestTemplateFormula(Value, PestValues.PestName,
          PestValues.PestSeriesName, PestValues.PestSeriesMethod,
          nil);
      end
      else
      begin
        WriteFloatValueFromGlobalFormula(PestValues.Formula, AFarm,
          PestValues.FormulaErrorMessage);
      end;
    end
    else
    begin
      WriteInteger(1);
    end;
  end;
  procedure WriteListData;
  var
    TimeIndex: Integer;
    FarmIndex: Integer;
    InnerFarmIndex: Integer;
  begin
    FWriteLocation := RequiredValues.WriteLocation;
    try
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              WriteInteger(0);
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            AllotmentItem := AFarm.SWAllotment.ItemByStartTime(StartTime) as TAllotmentItem;
            WriteAllotmentItem(AFarm, AllotmentItem);

            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            WriteInteger(0);
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.SWAllotment.Count > 0 then
          begin
            AllotmentItem := AFarm.SWAllotment.First as TAllotmentItem;
          end
          else
          begin
            AllotmentItem := nil;
          end;
          WriteAllotmentItem(AFarm, AllotmentItem);

          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end
  end;
begin
  if FAllotments.SurfaceWater.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  RequiredValues.WriteLocation := wlSwAllotment;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP ALLOTMENTS: SURFACE_WATER';
  RequiredValues.ErrorID := 'FMP ALLOTMENTS: SURFACE_WATER';
  RequiredValues.ID := 'SURFACE_WATER';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FAllotments.SurfaceWater.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidSurfaceWaterallotment;
  case FAllotments.SurfaceWaterAllotmentMethod of
    amHeight:
      begin
        RequiredValues.Option := 'HEIGHT ';
      end;
    amVolume:
      begin
        RequiredValues.Option := 'VOLUME ';
      end;
    amRate:
      begin
        RequiredValues.Option := 'RATE ';
      end;
  end;
  RequiredValues.FarmProperty := FAllotments.SurfaceWater;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlSwAllotment);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteListData;
    if FPestParamUsed then
    begin
      WritingTemplate := True;
      try
        GetFileStreamName(RequiredValues.WriteLocation);
        WriteListData;
      finally
        FPestParamUsed := False;
        WritingTemplate := False;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSurfaceWaterIrrigation;
begin

end;

procedure TModflowFmp4Writer.WriteFarmNames;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  FarmNames: array of string;
begin
  WriteString('  WBS_NAME INTERNAL');
  NewLine;
  if FNWBS <> Model.Farms.Count then
  begin
    SetLength(FarmNames, FNWBS + 1);
    for FarmIndex := 1 to Model.Farms.Count do
    begin
      FarmNames[FarmIndex] := ' Unused';
    end;
    for FarmIndex := 0 to Model.Farms.Count - 1 do
    begin
      AFarm := Model.Farms[FarmIndex];
      if AFarm.FarmName = '' then
      begin
        AFarm.FarmName := Format('Farm%d', [FarmIndex+1]);
      end;
      FarmNames[AFarm.FarmId] :=' ' + ReplaceStr(AFarm.FarmName, ' ', '_')
    end;
    for FarmIndex := 1 to Length(FarmNames) - 1 do
    begin
      WriteInteger(FarmIndex);
      WriteString(FarmNames[FarmIndex]);
      NewLine;
    end;
  end
  else
  begin
    for FarmIndex := 0 to Model.Farms.Count - 1 do
    begin
      AFarm := Model.Farms[FarmIndex];
      WriteInteger(AFarm.FarmId);
      if AFarm.FarmName = '' then
      begin
        AFarm.FarmName := Format('Farm%d', [FarmIndex+1]);
      end;
      WriteString(' ' + ReplaceStr(AFarm.FarmName, ' ', '_'));
      NewLine;
    end;
  end;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteWaterBalanceSubregion;
begin
  WriteString('BEGIN WATER_BALANCE_SUBREGION');
  NewLine;

  WriteFarmNames;
  WriteFarmLocation;
  WriteEfficiency;
  WriteEfficiencyImprovement;
  WriteDeficiencyScenario;
  WriteProrateDeficiency;
  WriteWaterSource;
  WriteBareRunoffFraction;
  WriteBarePrecipitationConsumptionFraction;
  WriteAddedDemandRunoffSplit;
  WriteAddedCropDemandFlux;
  WriteAddedCropDemandRate;

  WriteString('END WATER_BALANCE_SUBREGION');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteWaterSource;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  TimeIndex: Integer;
  StartTime: Double;
  FarmID: Integer;
  FarmIndex: Integer;
  AFarm: TFarm;
  InnerFarmIndex: Integer;
  WaterSourceItem: TWaterSourceItem;
  procedure WriteWaterSourceItem(AFarm: TFarm; WaterSourceItem: TWaterSourceItem);
  var
    Formula: string;
  begin
    if WaterSourceItem <> nil then
    begin
      Formula := WaterSourceItem.GroundWater;
      WriteBooleanValueFromGlobalFormula(Formula, AFarm,
        'Groundwater Water Source in ' + AFarm.FarmName);
      Formula := WaterSourceItem.SurfaceWater;
      WriteBooleanValueFromGlobalFormula(Formula, AFarm,
        'Surface-Water Water Source in ' + AFarm.FarmName);
      Formula := WaterSourceItem.NonRoutedDelivery;
      WriteBooleanValueFromGlobalFormula(Formula, AFarm,
        'Non-Routed Delivery Water Source in ' + AFarm.FarmName);
    end
    else
    begin
      WriteInteger(0);
      WriteInteger(0);
      WriteInteger(0);
    end;
  end;
begin
  if FFarmProcess4.WaterSource.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  RequiredValues.WriteLocation := wlWaterSource;
  RequiredValues.DefaultValue := 1;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: WATERSOURCE';
  RequiredValues.ErrorID := 'FMP WBS: WATERSOURCE';
  RequiredValues.ID := 'WATERSOURCE';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData :=
    (FFarmProcess4.WaterSource.FarmOption = foTransient);
  RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
  RequiredValues.CheckError := StrInvalidWaterSource;
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FFarmProcess4.WaterSource;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlWaterSource);
  end;

  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);
  WriteScaleFactorsID_andOption(RequiredValues,
    UnitConversionScaleFactor, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT LIST DATAFILE ');
  end
  else
  begin
    WriteString('STATIC LIST DATAFILE ');
  end;

  if ExternalFileName <> '' then
  begin
    WriteString(ExtractRelativePath(FInputFileName, ExternalFileName));
    NewLine;
    Exit;
  end
  else
  begin
    WriteString(ExtractFileName(AFileName));
    NewLine;
    try
      FWriteLocation := RequiredValues.WriteLocation;
      if RequiredValues.WriteTransientData then
      begin
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          WriteCommentLine(Format(StrStressPeriodD, [TimeIndex+1]));
          StartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

          FarmID := 1;
          for FarmIndex := 0 to Model.Farms.Count - 1 do
          begin
            AFarm := Model.Farms[FarmIndex];
            for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
            begin
              WriteInteger(FarmID);
              WriteInteger(0);
              WriteInteger(0);
              WriteInteger(0);
              Inc(FarmID);
              NewLine;
            end;
            Assert(FarmID = AFarm.FarmId);
            WriteInteger(AFarm.FarmId);

            WaterSourceItem := AFarm.WaterSource.ItemByStartTime(StartTime) as TWaterSourceItem;
            WriteWaterSourceItem(AFarm, WaterSourceItem);

            Inc(FarmID);
            NewLine;
          end;
        end;
      end
      else
      begin
        FarmID := 1;
        for FarmIndex := 0 to Model.Farms.Count - 1 do
        begin
          AFarm := Model.Farms[FarmIndex];
          for InnerFarmIndex := FarmID to AFarm.FarmID - 1 do
          begin
            WriteInteger(FarmID);
            WriteInteger(0);
            WriteInteger(0);
            WriteInteger(0);
            Inc(FarmID);
            NewLine;
          end;

          Assert(FarmID = AFarm.FarmId);
          WriteInteger(AFarm.FarmId);

          if AFarm.WaterSource.Count > 0 then
          begin
            WaterSourceItem := AFarm.WaterSource.First;
          end
          else
          begin
            WaterSourceItem := nil;
          end;
          WriteWaterSourceItem(AFarm, WaterSourceItem);

          Inc(FarmID);
          NewLine;
        end;

      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteZeroConsumptiveUseBecomesBareSoil;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FLandUse.NoCropUseMeansBareSoil.FarmOption = foNotUsed then
  begin
    Exit;
  end;


  RequiredValues.WriteLocation := wlNoCropMeansBareSoil;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.MaxDataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: ZERO_CONSUMPTIVE_USE_BECOMES_BARE_SOIL';
  RequiredValues.ErrorID := 'FMP LAND_USE: ZERO_CONSUMPTIVE_USE_BECOMES_BARE_SOIL';
  RequiredValues.ID := 'ZERO_CONSUMPTIVE_USE_BECOMES_BARE_SOIL';
  RequiredValues.StaticDataName := '';
  RequiredValues.WriteTransientData := FLandUse.NoCropUseMeansBareSoil.FarmOption = foTransient;
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FLandUse.NoCropUseMeansBareSoil;

  if RequiredValues.FarmProperty.ExternalFileName = '' then
  begin
    AFileName := GetFileStreamName(wlNoCropMeansBareSoil);
  end;

  WriteOwhmList(RequiredValues, AFileName, GetZeroConsumptiveUseBecomesBareSoilCollection,
    StrInvalidZeroConsump);

end;

end.
