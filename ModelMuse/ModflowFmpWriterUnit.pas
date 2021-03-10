unit ModflowFmpWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, ModflowCellUnit,
  ModflowBoundaryUnit, OrderedCollectionUnit, ScreenObjectUnit, GoPhastTypes,
  ModflowFmpWellUnit, PhastModelUnit, ModflowFmpFarmUnit, Contnrs,
  ModflowFmpCropUnit, RbwParser, Classes, ModflowBoundaryDisplayUnit,
  DataSetUnit;

type
  TWellFieldOption = (wfNotUsed, wfUsed);
  TWriteLocation = (wlMain, wlOpenClose, wlOFE, wlCID, wlFID, wlRoot, wlCropUse, wlETR,
    wlEtFrac, wlSwLosses, wlPFLX, wlCropFunc, wlWaterCost, wlDeliveries,
    wlSemiRouteDeliv, wlSemiRouteReturn, wlCall);

  TTestRealValueOkProcedure = reference to procedure (Value: double);
  TTestIntValueOkProcedure = reference to procedure (Value: Integer);

  TModflowFmpWriter = class(TCustomListWriter)
  private
    FFarmProcess: TFarmProcess;
    FCropIDs: TList;
    FFarmIDs: TList;
    FRefEts: TList;
    FPrecip: TList;
    FNameOfFile: string;
    NPFWL: Integer;
    MXL: Integer;
    FFarmWellID: Integer;
    FFarms: TFarmList;
    IRTFL: Integer;
    IEFFL: Integer;
    IFTEFL: Integer;
    IIESWFL: integer;
    ICCFL: Integer;
    ICUFL: Integer;
    IPFL: integer;
    IDEFFL: Integer;
    NCROPS: Integer;
    IBEN: Integer;
    ICOST: Integer;
    MXNRDT: integer;
    INRDFL: integer;
    IALLOTSW: integer;
    ISRDFL: integer;
    ISRRFL: integer;
    IALLOTGW: Integer;
    FStressPeriodStartTime: Double;
    FWriteLocation: TWriteLocation;
    FOpenCloseFileStream: TFileStream;
    FOFE_FileStream: TFileStream;
    FOpenCloseFileName: string;
    FCID_FileStream: TFileStream;
    FFID_FileStream: TFileStream;
    FRoot_FileStream: TFileStream;
    FCropUse_FileStream: TFileStream;
    FETR_FileStream: TFileStream;
    FET_Frac_FileStream: TFileStream;
    FSW_Losses_FileStream: TFileStream;
    FPFLX_FileStream: TFileStream;
    FCropFunc_FileStream: TFileStream;
    FWaterCost_FileStream: TFileStream;
    FDeliveries_FileStream: TFileStream;
    FSemiDeliveries_FileStream: TFileStream;
    FSemiReturn_FileStream: TFileStream;
    FCall_FileStream: TFileStream;
    FStressPeriod: Integer;
    FACtiveSurfaceCells: array of array of boolean;
    FPestParamUsed: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet2a;
    procedure WriteDataSet2cParamDimen;
    procedure WriteDataSet2cWhenToRead;
    procedure WriteDataSet2cWaterPolicy;
    procedure WriteDataSet2cCropConsumptiveUse;
    procedure WriteDataSet2cSurfaceWater;
    procedure WriteDataSet2cPrintFlags;
    procedure WriteDataSet2cAuxVar;
    procedure WriteDataSet2cOptions;
    procedure WriteDataSet2cMnwOptions;
    procedure WriteNwtOptions;
    procedure WriteDataSets3And4;
    procedure WriteDataSet5;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    procedure WriteDataSet9;
    procedure WriteDataSet11;
    procedure WriteDataSet12;
    procedure WriteDataSet13;
    procedure WriteDataSet14;
    procedure WriteDataSet15;
    procedure WriteDataSet16;
    procedure WriteDataSet17;
    procedure WriteDataSet18;
    procedure WriteDataSet19;
    procedure WriteDataSet20;
    procedure WriteDataSets22to39;
    procedure WriteDataSet25;
    procedure WriteDataSet26(TimeIndex: Integer);
    procedure WriteDataSet27;
    procedure WriteDataSet28(TimeIndex: Integer);
    procedure WriteDataSet29;
    procedure WriteDataSet30a;
    procedure WriteDataSet30b(TimeIndex: Integer);
    procedure WriteDataSet31;
    procedure WriteDataSet32;
    procedure WriteDataSet33(TimeIndex: Integer);
    procedure WriteDataSet34;
    procedure WriteDataSet35;
    procedure WriteDataSet36;
    procedure WriteDataSet37a;
    procedure WriteDataSet37b;
    procedure WriteDataSet38;
    procedure WriteDataSet39;
    function NumberOfFarms: Integer;
    function GetEfficiencyFlag: integer;
    function GetRootingDepthFlag: integer;
    // IALLOTGW;
    function GetGroundwaterAllotmentFlag: Integer;
    procedure FillFarmList;
    function GetCropFractionFlag: integer;
    function GetInnefficienyLossesFlag: Integer;
    function GetCropConsumptiveUseFlag: Integer;
    function GetConsumptiveUseFlag: Integer;
    function GetPrecipitationFlag: integer;
    function EvaluateValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string;
      const OKTypes: TRbwDataTypes): TExpression;
    procedure WriteFloatValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string;
      TestProc: TTestRealValueOkProcedure = nil);
    procedure WriteIntegerValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string;
      TestValue: TTestIntValueOkProcedure = nil);
    procedure WriteBooleanValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string);
    function GetCropBenefitsFlag: integer;
    function GetWaterCostCoefficientsFlag: integer;
    function GetMaxNonRoutedDelivery: integer;
    procedure EvaluateCropID;
    procedure EvaluateFarmID;
    procedure EvaluateReferenceET;
    procedure EvaluatePrecip;
    procedure EvaluateGwAllotment;
    function GetSemiRoutedSurfaceWaterDeliveryFlag: integer;
    function GetSemiRoutedSurfaceWaterRunoffReturnflowFlag: integer;
    function GetWellFieldOption: TWellFieldOption;
    procedure SetFlags;
    procedure RemoveErrorAndWarningMessages;
    procedure EvaluateActiveCells;
    procedure CheckIntegerDataSet(IntegerArray: TDataArray;
      const ErrorMessage: string);
    function GetObjectString(ErrorObject: TObject): string;
    procedure WriteFileInternal;
    procedure FreeFileStreams;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteCustomStressPeriod(TimeIndex: Integer); override;
    procedure EvaluateAll;
    procedure GetFlowUnitNumber(var UnitNumber: Integer); override;
    procedure FarmBudgetUnitNumber(var UnitNumber: Integer);
    procedure RemoveNoDefinedError(var NoDefinedErrorRoot: string); override;
    procedure ShowNoBoundaryError(const NoDefinedErrorRoot: string); override;
    procedure DoBeforeWriteCells; override;
    procedure DoBeforeWriteParamCells; override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateRefEtDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdatePrecipDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateCropIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateFarmIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure WriteString(const Value: AnsiString); overload; override;
    Procedure WriteU2DRELHeader(const Comment: string;
      ArrayType: TModflowArrayType; const MF6_ArrayName: string); override;
    procedure WriteConstantU2DREL(const ArrayName: string;
      const Value: double; ArrayType: TModflowArrayType;
        const MF6_ArrayName: string); override;
    Procedure WriteU2DINTHeader(const Comment: string;
      ArrayType: TModflowArrayType; const MF2015_ArrayName: string); override;
    procedure WriteConstantU2DINT(const Comment: string;
      const Value: integer; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); override;
    procedure Evaluate; override;
  end;

implementation

uses
  ModflowUnitNumbers, Forms, frmProgressUnit,
  frmFormulaErrorsUnit, ModflowFmpSoilUnit, SysUtils, ModflowTimeUnit,
  RealListUnit, ModflowFmpClimateUnit, ModflowFmpAllotmentUnit,
  frmErrorsAndWarningsUnit, ModflowFmpCropSpatialUnit,
  Generics.Collections, ModflowFmpEvapUnit, ModflowFmpPrecipitationUnit,
  ModflowOutputControlUnit, ModflowFmpFarmIdUnit, frmGoPhastUnit,
  ModflowMNW2_WriterUnit, PestParamRoots;

resourcestring
  StrWritingDataSet2a = '  Writing Data Set 2a.';
  StrWritingDataSet2c = '  Writing Data Set 2c.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
  StrWritingDataSets = '  Writing Data Sets 22 to 39.';
  StrSoilS = 'Soil: %s';
  StrCapillaryFringe = 'Capillary Fringe';
  StrRootingDepth = 'Rooting Depth';
  StrCropS = 'Crop: %s';
  StrTranspiratoryFracti = 'Transpiratory fraction of consumptive use';
  StrEvaporativeFraction = 'Evaporative fraction of consumptive use related ' +
  'to precipitation ';
  StrEvaporativeFractionIrr = 'Evaporative fraction of consumptive use related ' +
  'to irrigation ';
  StrACoeff = 'A-Coeff';
  StrBCoeff = 'B-Coeff';
  StrCCoeff = 'C-Coeff';
  StrDCoeff = 'D-Coeff';
  StrECoeff = 'E-Coeff';
  StrFractionOfInefficPrecip = 'Fraction of in-efficient losses to surface-w' +
  'ater related to precipitation';
  StrFractionOfInefficIrrig = 'Fraction of in-efficient losses to surface-wa' +
  'ter related to irrigation ';
  StrPSI1 = 'PSI(1)';
  StrPSI2 = 'PSI(2)';
  StrPSI3 = 'PSI(3)';
  StrPSI4 = 'PSI(4)';
  StrBaseTemperature = 'Base temperature';
  StrMinimumCutoffTempe = 'Minimum Cutoff Temperature';
  StrMaximumCutoffTempe = 'Maximum Cutoff Temperature';
  StrCoefficient0 = 'Coefficient 0';
  StrCoefficient1 = 'Coefficient 1';
  StrCoefficient2 = 'Coefficient 2';
  StrCoefficient3 = 'Coefficient 3';
  StrBeginningRootingDe = 'Beginning Rooting Depth';
  StrMaximumRootingDept = 'Maximum Rooting Depth';
  StrRootGrowthCoeffici = 'Root growth coefficient';
  StrIrrigationFlag = 'Irrigation flag';
  StrMaximumTemperature = 'Maximum temperature';
  StrMinimumTemperature = 'Minimum temperature';
  StrPrecipitation = 'Precipitation';
  StrReferenceEvapotrans = 'Reference Evapotranspiration flux ';
//  StrTimeSeriesStepMaxT = ' # Data Set 16: TimeSeriesStep MaxT MinT Precip ETref';
  StrGWcost1 = 'GWcost1';
  StrGWcost2 = 'GWcost2';
  StrGWcost3 = 'GWcost3';
  StrGWcost4 = 'GWcost4';
  StrSWcost1 = 'SWcost1';
  StrSWcost2 = 'SWcost2';
  StrSWcost3 = 'SWcost3';
  StrSWcost4 = 'SWcost4';
  StrWritingDataSet30a = '  Writing Data Set 30a.';
  StrWritingDataSet30b = '  Writing Data Set 30b.';
  StrWPFSlope = 'WPF-Slope';
  StrWIntercept = 'WIntercept';
  StrPrice = 'Price';
  StrWritingDataSet37a = '  Writing Data Set 37a.';
  StrWritingDataSet37b = '  Writing Data Set 37b.';
  StrRootingDepthNotDe = 'Rooting depth not defined in the following crops.';
  StrConsumptiveUseFact = 'Consumptive use factors undefined in the followin' +
  'g crops';
  StrInefficiencyLosses = 'Inefficiency losses to surface water not defined ' +
  'for the following crops';
  StrCropEfficiencesNot = 'Crop Efficiencies not fully defined in the followi' +
  'ng objects.';
  StrVirtualFarmNumber = 'Virtual Farm Number not defined in the following o' +
  'bjects.';
  StrTheClimateDataFor = 'The climate data for the Farm Process has not been' +
  ' specified.';
  StrNoFarmsHaveBeenD = 'No farms have been defined.';
  StrNoFarmWellsHaveB = 'No farm wells have been defined in the %s.';
  StrTheFarmPackageHasB = 'The %s has been activated but no farm wells ' +
  'for it have been defined.';
  StrNoCropsHaveBeenD = 'No crops have been defined in the Farm Process';
  StrInvalidFarmSurface = 'Invalid Farm surface water flow option';
  StrIRDFLInTheFarmPr = 'IRDFL in the Farm Process must be equal to 0 if the' +
  ' SFR package is not selected.';
  StrClimateStartingTim = 'Climate starting time: %d';
  StrErrorInFarmD = 'Error in Farm %d.';
  StrFarmD = 'Farm %d';
  StrFarmCostsHaveNot = 'Farm Costs have not been defined';
  StrInFarmDFarmC = 'In farm # %d, farm costs have not been defined';
  StrFarmAllotmentsNot = 'Farm allotments not defined.';
  StrInFarmDTheFar = 'In farm #%d, the farm allotment has not been defined.';
  StrTheCropspecificWa = 'The crop-specific water-production function is und' +
  'efined in the following crops';
  StrIfTheFarmProcess = 'If the Farm Process is active in a child model, it ' +
  'must also be active in the parent model.';
  StrInvalidFarmProcess = 'Invalid Farm Process activation';
  StrInvalidFarmID = 'Invalid Farm ID in Farm Process';
  StrRow0dColumn = 'Row: %0:d; Column: %1:d';
  StrInvalidSoilIDInF = 'Invalid Soil ID in Farm Process';
  StrInvalidCropIDInF = 'Invalid Crop ID in Farm Process.';
  StrTheFormulaShouldR = 'The formula should result in a Boolean';
  StrTheFormulaShouldInt = 'The formula should result in an integer';
  StrTheFormulaShouldReal = 'The formula should result in a real number';
  StrFMPFarmsNotDefine = 'FMP Farms not defined in one or more stress period' +
  's';
  StrUndefinedFMPRefere = 'Undefined FMP Reference Evapotranspiration in one' +
  ' or nore stress periods';
  StrFMPCropIDsNotDef = 'FMP Crop IDs not defined in one or more stress peri' +
  'ods';
  StrFMPPrecipitationNo = 'FMP Precipitation not defined in one or more stre' +
  'ss periods';
  StrTheFarmProcessIs = 'The Farm Process is not supported by MT3DMS.';
  StrMT3DMSVersion53D = 'MT3DMS version 5.3 and MT3D-USGS do not suppport the Farm Proce' +
  'ss.';
  StrTheEvaporationFrac = 'The evaporation fraction, has not been defined fo' +
  'r the following crops at the following times.';
  StrCrop0sTime = 'Crop = %0:s; Time = %1:g';
  StrThePriorityForNon = 'The priority rank (NRDR) for non routed delivery is less than ' +
  'or equal to zero for the following farms';
  StrTheSpecifiedRootin = 'The specified rooting depth must be greater than ' +
  'zero. That isn''t true for the following crops.';
//  StrIRRFLInTheFarmPr = 'IRRFL in the Farm Process must be equal to 0 if the' +
//  ' SFR package is not selected.';

{ TModflowFmpWriter }

function TModflowFmpWriter.CellType: TValueCellType;
begin
  result := TFmpWell_Cell;
end;

constructor TModflowFmpWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FFarmProcess := Model.ModflowPackages.FarmProcess;
  FFarms := TFarmList.Create;
  FCropIDs := TObjectList.Create;
  FFarmIDs := TObjectList.Create;
  FRefEts := TObjectList.Create;
  FPrecip := TObjectList.Create;
end;

destructor TModflowFmpWriter.Destroy;
begin
  FPrecip.Free;
  FRefEts.Free;
  FFarmIDs.Free;
  FCropIDs.Free;
  FFarms.Free;
  FreeFileStreams;
  inherited;
end;

class function TModflowFmpWriter.Extension: string;
begin
  Result := '.fmp';
end;

function TModflowFmpWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowFmpWellBoundary;
end;

function TModflowFmpWriter.GetEfficiencyFlag: integer;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  ACropEfficiency: TFarmEfficienciesItem;
begin
  result := 1;
  for FarmIndex := 0 to FFarms.Count - 1 do
  begin
    AFarm := FFarms[FarmIndex];
    if AFarm.FarmEfficiencyCollection.Count > 0 then
    begin
      // @ACropEfficiency represents the crop efficiencies for one crop
      // for one farm over
      // multiple time periods. FMP Data sets 7 or 24.
      // The times will be the same for all the different crops at one farm.
      ACropEfficiency := AFarm.FarmEfficiencyCollection[0];
      if ACropEfficiency.CropEfficiency.Count > 1 then
      begin
        result := 2;
        Exit;
      end;
    end;
  end;
end;

procedure TModflowFmpWriter.GetFlowUnitNumber(var UnitNumber: Integer);
begin
  if FFarmProcess.SaveWellFlowRates = swfrAscii then
  begin
    UnitNumber := 1
  end
  else
  begin
    inherited GetFlowUnitNumber(UnitNumber);
  end;
end;

function TModflowFmpWriter.GetGroundwaterAllotmentFlag: Integer;
var
  LocalModel: TCustomModel;
  FarmIndex: integer;
  AFarm: TFarm;
begin
  if FFarmProcess.GroundwaterAllotmentsUsed then
  begin
    if Model is TChildModel then
    begin
      result := -1;
    end
    else
    begin
      Result := 1;
      LocalModel := Model;
      for FarmIndex := 0 to LocalModel.Farms.Count - 1 do
      begin
        AFarm := LocalModel.Farms[FarmIndex];
        if AFarm.GwAllotment.Count > 1 then
        begin
          Result := 2;
          exit;
        end;
      end;
    end;
  end
  else
  begin
    Result := 0;
  end;
end;

function TModflowFmpWriter.GetInnefficienyLossesFlag: Integer;
var
  Crops: TCropCollection;
  Losses: TLossesCollection;
  CropIndex: Integer;
begin
  result := -1;
  case FFarmProcess.FractionOfInefficiencyLosses of
    filCalculated:
      begin
        result := 0;
      end;
    filSpecified:
      begin
        result := 1;
        Crops := Model.FmpCrops;
        for CropIndex := 0 to Crops.Count - 1 do
        begin
          Losses := Crops[CropIndex].LossesCollection;
          if Losses.Count > 1 then
          begin
            result := 2;
            Exit;
          end;
        end;
      end
    else
      Assert(False);
  end;
end;

function TModflowFmpWriter.GetMaxNonRoutedDelivery: integer;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  DeliveryParam: TDeliveryParamCollection;
  TestCount: Integer;
begin
  result := 0;
  for FarmIndex := 0 to FFarms.Count - 1 do
  begin
    AFarm := FFarms[FarmIndex];
    DeliveryParam := AFarm.DeliveryParamCollection;
    TestCount := DeliveryParam.Count;
    if TestCount > result  then
    begin
      result := TestCount;
    end;
  end;
end;

function TModflowFmpWriter.GetPrecipitationFlag: integer;
begin
  Result := 0;
  case FFarmProcess.Precipitation of
    pTimeSeries: result := 3;
    pSpatiallyDistributed: result := 2;
    else Assert(False);
  end;
end;

function TModflowFmpWriter.GetRootingDepthFlag: integer;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  RootDepths: TFmpRootDepthCollection;
begin
  result := 0;
  case FFarmProcess.RootingDepth of
    rdSpecified:
      begin
        result := 1;
        Crops := Model.FmpCrops;
        for CropIndex := 0 to Crops.Count - 1 do
        begin
          RootDepths := Crops[CropIndex].FmpRootDepthCollection;
          if RootDepths.Count > 1 then
          begin
            result := 2;
            Exit;
          end;
        end;
      end;
    rdCalculated:
      begin
        result := 3;
      end;
    else
      Assert(False);
  end;
end;

function TModflowFmpWriter.NumberOfFarms: Integer;
begin
  result := FFarms.Count;
end;

function TModflowFmpWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.FarmProcess;
end;

function TModflowFmpWriter.ParameterType: TParameterType;
begin
  result := ptQMAX;
end;

procedure TModflowFmpWriter.RemoveNoDefinedError(
  var NoDefinedErrorRoot: string);
begin
  NoDefinedErrorRoot := Format(StrNoFarmWellsHaveB, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, NoDefinedErrorRoot);
end;

procedure TModflowFmpWriter.SetFlags;
var
  LocalModel: TCustomModel;
begin
  // Parameter Dimensions
  LocalModel := Model as TCustomModel;
  NCROPS := LocalModel.FmpCrops.Count;

  // When-to-read flags
  IRTFL := GetRootingDepthFlag;
  ICUFL := GetConsumptiveUseFlag;
  IPFL := GetPrecipitationFlag;
  IFTEFL := GetCropFractionFlag;
  IIESWFL := GetInnefficienyLossesFlag;
  IEFFL := GetEfficiencyFlag;

  // Water Policy Flags
  IDEFFL := Ord(FFarmProcess.DeficiencyPolicy)-2;
  IBEN := GetCropBenefitsFlag;
  ICOST := GetWaterCostCoefficientsFlag;
  IALLOTGW := GetGroundwaterAllotmentFlag;

  // Crop Consumptive Use Flag
  ICCFL := GetCropConsumptiveUseFlag;

  // Surface-Water Flags
  MXNRDT := GetMaxNonRoutedDelivery;
  if MXNRDT > 0 then
  begin
    INRDFL := 1;
  end
  else
  begin
    INRDFL := 0;
  end;
  IALLOTSW := Ord(FFarmProcess.SurfaceWaterAllotment);
  ISRDFL := GetSemiRoutedSurfaceWaterDeliveryFlag;
  ISRRFL := GetSemiRoutedSurfaceWaterRunoffReturnflowFlag;

end;

procedure TModflowFmpWriter.ShowNoBoundaryError(
  const NoDefinedErrorRoot: string);
begin
  frmErrorsAndWarnings.AddError(Model, NoDefinedErrorRoot,
    Format( StrTheFarmPackageHasB, [Package.PackageIdentifier]));
end;

procedure TModflowFmpWriter.UpdateCropIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataSets: TList;
  CropID: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  CropIDIndex: integer;
  DataSetIndex: integer;
begin
  EvaluateActiveCells;
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not (Package as TFarmProcess).CropIdUsed(self) then
    begin
      UpdateNotUsedDisplay(TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      EvaluateCropID;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      CropID := TimeLists[0];
      for TimeIndex := 0 to CropID.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for CropIDIndex := 0 to FCropIDs.Count - 1 do
        begin
          List := FCropIDs[CropIDIndex];
          UpdateCellDisplay(List, DataSets, [], nil, [0]);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          CheckIntegerDataSet(DataArray, StrInvalidCropIDInF);
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.UpdateFarmIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataSets: TList;
  FarmID: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  FarmIDIndex: integer;
  DataSetIndex: integer;
begin
  EvaluateActiveCells;
  { TODO -cFMP : This needs to be finished }
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not (Package as TFarmProcess).FarmIdUsed(self) then
    begin
      UpdateNotUsedDisplay(TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      EvaluateFarmID;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      FarmID := TimeLists[0];
      for TimeIndex := 0 to FarmID.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for FarmIDIndex := 0 to FFarmIDs.Count - 1 do
        begin
          List := FFarmIDs[FarmIDIndex];
          UpdateCellDisplay(List, DataSets, [], nil, [0]);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          CheckIntegerDataSet(DataArray, StrInvalidFarmID);
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.UpdatePrecipDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataSets: TList;
  PrecipRate: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  PrecipIndex: integer;
  DataSetIndex: integer;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not (Package as TFarmProcess).PrecipUsed(self) then
    begin
      UpdateNotUsedDisplay(TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      EvaluatePrecip;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      PrecipRate := TimeLists[0];
      for TimeIndex := 0 to PrecipRate.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for PrecipIndex := 0 to FPrecip.Count - 1 do
        begin
          List := FPrecip[PrecipIndex];
          UpdateCellDisplay(List, DataSets, [], nil, [0]);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.UpdateRefEtDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataSets: TList;
  EvapRate: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  EtIndex: integer;
  DataSetIndex: integer;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not (Package as TFarmProcess).EvapUsed(self) then
    begin
      UpdateNotUsedDisplay(TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      EvaluateReferenceET;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      EvapRate := TimeLists[0];
      for TimeIndex := 0 to EvapRate.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for EtIndex := 0 to FRefEts.Count - 1 do
        begin
          List := FRefEts[EtIndex];
          UpdateCellDisplay(List, DataSets, [], nil, [0]);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Well_Cell: TFmpWell_Cell;
  LocalLayer: integer;
  AuxValue: integer;
  AuxName: string;
  MNW2NAM: STRING;
  DataArray: TDataArray;
begin
  Well_Cell := Cell as TFmpWell_Cell;
  if Well_Cell.Mnw1 or Well_Cell.Mnw2 then
  begin
    LocalLayer := 0;
  end
  else
  begin
    LocalLayer := Model.
      DataSetLayerToModflowLayer(Well_Cell.Layer);
  end;
  WriteInteger(LocalLayer);
  WriteInteger(Well_Cell.Row+1);
  WriteInteger(Well_Cell.Column+1);
  if Well_Cell.Mnw1 or Well_Cell.Mnw2 then
  begin
    WriteInteger(-FFarmWellID);
  end
  else
  begin
    WriteInteger(FFarmWellID);
  end;
  WriteInteger(Well_Cell.FarmID);

  if (Well_Cell.MaxPumpingRatePestName <> '')
    or (Well_Cell.MaxPumpingRatePestSeriesName <> '') then
  begin
    FPestParamUsed := True;
  end;

  if Model.PestUsed and WritingTemplate
    and ((Well_Cell.MaxPumpingRatePestName <> '') or (Well_Cell.MaxPumpingRatePestSeriesName <> '')) then
  begin
    WritePestTemplateFormula(Well_Cell.MaxPumpingRate, Well_Cell.MaxPumpingRatePestName,
      Well_Cell.MaxPumpingRatePestSeriesName, Well_Cell.MaxPumpingRatePestSeriesMethod,
      Well_Cell);
  end
  else
  begin
    WriteFloat(Well_Cell.MaxPumpingRate);
    if Well_Cell.MaxPumpingRatePestName <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Well_Cell.MaxPumpingRatePestName);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
    if Well_Cell.MaxPumpingRatePestSeriesName <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Well_Cell.MaxPumpingRatePestSeriesName);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
  end;

//  WriteFloat(Well_Cell.MaxPumpingRate);


  if Well_Cell.Mnw2 then
  begin
    MNW2NAM := Copy(Well_Cell.MnwName, 1, 20);
    TModflowMNW2_Writer.AdjustWellID(MNW2NAM);
    WriteString(MNW2NAM);
  end;
  Inc(FFarmWellID);

//  Skip QMAXRESET because MNW1 is not supported.

  AuxName := '';
  case FFarmProcess.CropIrrigationRequirement of
    cirOnlyWhenNeeded:
      begin
        if Well_Cell.PumpOnlyIfCropRequiresWater then
        begin
          AuxValue := 1;
        end
        else
        begin
          AuxValue := 0;
        end;
        WriteInteger(AuxValue);
        AuxName := ' AUX-NOCIRNOQ';
      end;
    cirContinuously: ;// do nothing
    else Assert(False);
  end;

  // FMP does not accept auxilliary variables.
//  WriteIface(Well_Cell.IFace);

  WriteString(' # ' + DataSetIdentifier
    + ' Layer, Row, Column, Farm-Well-ID, Farm ID, ' + VariableIdentifiers);
  if Well_Cell.Mnw2 then
  begin
    WriteString(', MNW2NAM ');
  end;

//  WriteString(VariableIdentifiers + ',' + AuxName + ',' + ' IFACE');

//  // The annotation identifies the object used to define the well.
//  // This can be helpful in identifying when used with PEST.
//  WriteString(Well_Cell.PumpingRateAnnotation);
  NewLine;
end;

procedure TModflowFmpWriter.DoBeforeWriteParamCells;
begin
  inherited;
  WriteNwtOptions;
end;

procedure TModflowFmpWriter.WriteConstantU2DINT(const Comment: string;
  const Value: integer; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
var
  OldLocation: TWriteLocation;
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose, wlCID, wlFID:
      begin
        OldLocation := FWriteLocation;
        FWriteLocation := wlMain;
        try
          inherited;
        finally
          FWriteLocation := OldLocation
        end;
      end
    else
      Assert(False);
  end;
end;

procedure TModflowFmpWriter.RemoveErrorAndWarningMessages;
var
  Dummy: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrRootingDepthNotDe);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrConsumptiveUseFact);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInefficiencyLosses);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCropEfficiencesNot);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrVirtualFarmNumber);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheClimateDataFor);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoFarmsHaveBeenD);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmSurface);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFarmCostsHaveNot);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFarmAllotmentsNot);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMultinodeWellsCan);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheCropspecificWa);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSoilIDInF);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFMPFarmsNotDefine);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUndefinedFMPRefere);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFMPCropIDsNotDef);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFMPPrecipitationNo);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFarmProcessIs);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheEvaporationFrac);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrThePriorityForNon);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheSpecifiedRootin);



  RemoveNoDefinedError(Dummy);
end;

procedure TModflowFmpWriter.WriteConstantU2DREL(const ArrayName: string;
  const Value: double; ArrayType: TModflowArrayType; const MF6_ArrayName: string);
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose, wlETR, wlPFLX:
      begin
        FWriteLocation := wlMain;
        try
          inherited;
        finally
          FWriteLocation := wlOpenClose
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TModflowFmpWriter.WriteCustomStressPeriod(TimeIndex: Integer);
begin
  inherited;
  FStressPeriod := TimeIndex;
  FStressPeriodStartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;

  frmProgressMM.AddMessage(StrWritingDataSet25);
  WriteDataSet25;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet26);
  WriteDataSet26(TimeIndex);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet27);
  WriteDataSet27;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet28);
  WriteDataSet28(TimeIndex);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet29);
  WriteDataSet29;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet30a);
  WriteDataSet30a;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet30b);
  WriteDataSet30b(TimeIndex);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet31);
  WriteDataSet31;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet32);
  WriteDataSet32;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet33);
  WriteDataSet33(TimeIndex);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet34);
  WriteDataSet34;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet35);
  WriteDataSet35;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet36);
  WriteDataSet36;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet37a);
  WriteDataSet37a;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet37b);
  WriteDataSet37b;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet38);
  WriteDataSet38;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSet39);
  WriteDataSet39;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet1;
begin
  // MXLP is omitted. How would it ever be used?
  CountParametersAndParameterCells(NPFWL, MXL);
  if NPFWL > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPFWL);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPFWL MXL');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet11;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  RootDepths: TFmpRootDepthCollection;
  CropID: Integer;
  RootItem: TRootingDepthItem;
begin
  if (IRTFL = 1) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.ROOT');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data set 11: Crop-ID ROOT');
      NewLine;

      FWriteLocation := wlOpenClose;

      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        RootDepths := Crops[CropIndex].FmpRootDepthCollection;
        if (RootDepths.Count <> 1) then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrRootingDepthNotDe, Crops[CropIndex].CropName);
          Exit;
        end;
        Assert(RootDepths.Count = 1);
        CropID := CropIndex + 1;
        RootItem := RootDepths[0];
        WriteInteger(CropID);
        WriteFloatValueFromGlobalFormula(RootItem.RootingDepth,
          Crops[CropIndex], StrRootingDepth,
          procedure (Value: double)
          begin
            if Value <= 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, StrTheSpecifiedRootin,
                Crops[CropIndex].CropName);
            end;
          end);
        WriteString(' # Data set 11: Crop-ID ROOT');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet12;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  EvapFractions: TEvapFractionsCollection;
  CropID: Integer;
  EvapFractionItem: TEvapFractionsItem;
begin
  if (IFTEFL = 1) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.ET_Frac');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data set 12: Crop-ID FTR FEP FEI');
      NewLine;

      FWriteLocation := wlOpenClose;

      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteFreeInteger(CropID);
        EvapFractions := Crops[CropIndex].EvapFractionsCollection;
        if (EvapFractions.Count <> 1) then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrConsumptiveUseFact, Crops[CropIndex].CropName);
          Exit;
        end;
        Assert(EvapFractions.Count = 1);
        EvapFractionItem := EvapFractions[0];
        WriteFloatValueFromGlobalFormula(EvapFractionItem.TranspirationFraction,
          Crops[CropIndex], StrTranspiratoryFracti);
        WriteFloatValueFromGlobalFormula(EvapFractionItem.PrecipFraction,
          Crops[CropIndex], StrEvaporativeFraction);
        WriteFloatValueFromGlobalFormula(EvapFractionItem.IrrigFraction,
          Crops[CropIndex], StrEvaporativeFractionIrr);
        WriteString(' # Data set 12: Crop-ID FTR FEP FEI');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet13;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  Losses: TLossesCollection;
  CropID: Integer;
  LossesItem: TLossesItem;
begin
  if (IIESWFL = 1) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.SW_Losses');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data set 13: Crop-ID FIESWP FIESWI');
      NewLine;

      FWriteLocation := wlOpenClose;

      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteFreeInteger(CropID);
        Losses := Crops[CropIndex].LossesCollection;
        if (Losses.Count <> 1) then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrInefficiencyLosses, Crops[CropIndex].CropName);
          Exit;
        end;
        Assert(Losses.Count = 1);
        LossesItem := Losses[0];
        WriteFloatValueFromGlobalFormula(LossesItem.PrecipitationLosses,
          Crops[CropIndex], StrFractionOfInefficPrecip);
        WriteFloatValueFromGlobalFormula(LossesItem.IrrigationLosses,
          Crops[CropIndex], StrFractionOfInefficIrrig);
        WriteString(' # Data set 13: Crop-ID FIESWP FIESWI');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet14;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  ACrop: TCropItem;
  CropID: Integer;
begin
  if ICCFL in [1,3] then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.PSI');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data set 14: Crop-ID PSI(1) PSI(2) PSI(3) PSI(4)');
      NewLine;

      FWriteLocation := wlOpenClose;

      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        ACrop := Crops[CropIndex];
        CropID := CropIndex + 1;
        WriteInteger(CropID);
        WriteFloatValueFromGlobalFormula(ACrop.PSI1,
          Crops[CropIndex], StrPSI1);
        WriteFloatValueFromGlobalFormula(ACrop.PSI2,
          Crops[CropIndex], StrPSI2);
        WriteFloatValueFromGlobalFormula(ACrop.PSI3,
          Crops[CropIndex], StrPSI3);
        WriteFloatValueFromGlobalFormula(ACrop.PSI4,
          Crops[CropIndex], StrPSI4);
        WriteString(' # Data set 14: Crop-ID PSI(1) PSI(2) PSI(3) PSI(4)');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet15;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  ACrop: TCropItem;
  CropID: Integer;
//  Expression: TExpression;
//  Value: Boolean;
begin
  if (IRTFL = 3) or (ICUFL = 3) or (IPFL = 3) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.ET_Func');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data Set 15: Crop-ID BaseT MinCutT MaxCutT C0 C1 C2 C3 BegRootD MaxRootD RootGC {NONIRR}');
      NewLine;

      FWriteLocation := wlOpenClose;


      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        ACrop := Crops[CropIndex];
        CropID := CropIndex + 1;
        WriteInteger(CropID);
        WriteFloatValueFromGlobalFormula(ACrop.BaseTemperature,
          ACrop, StrBaseTemperature);
        WriteFloatValueFromGlobalFormula(ACrop.MinimumCutoffTemperature,
          ACrop, StrMinimumCutoffTempe);
        WriteFloatValueFromGlobalFormula(ACrop.MaximumCutoffTemperature,
          ACrop, StrMaximumCutoffTempe);
        WriteFloatValueFromGlobalFormula(ACrop.Coefficient0,
          ACrop, StrCoefficient0);
        WriteFloatValueFromGlobalFormula(ACrop.Coefficient1,
          ACrop, StrCoefficient1);
        WriteFloatValueFromGlobalFormula(ACrop.Coefficient2,
          ACrop, StrCoefficient2);
        WriteFloatValueFromGlobalFormula(ACrop.Coefficient3,
          ACrop, StrCoefficient3);
        WriteFloatValueFromGlobalFormula(ACrop.BeginningRootDepth,
          ACrop, StrBeginningRootingDe);
        WriteFloatValueFromGlobalFormula(ACrop.MaximumRootDepth,
          ACrop, StrMaximumRootingDept);
        WriteFloatValueFromGlobalFormula(ACrop.RootGrowthCoefficient,
          ACrop, StrRootGrowthCoeffici);
        WriteBooleanValueFromGlobalFormula('not (' + ACrop.Irrigated + ')',
          ACrop, StrIrrigationFlag);
//        Expression := EvaluateValueFromGlobalFormula(ACrop.Irrigated, ACrop,
//          StrIrrigationFlag, [rdtBoolean]);
//        Value := Expression.BooleanResult;
//        if Value then
//        begin
//          WriteInteger(0);
//        end
//        else
//        begin
//          WriteInteger(1);
//        end;
        WriteString(' # Data Set 15: Crop-ID BaseT MinCutT MaxCutT C0 C1 C2 C3 BegRootD MaxRootD RootGC {NONIRR}');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;

  end;
end;

procedure TModflowFmpWriter.WriteDataSet16;
var
  FmpClimate: TClimateCollection;
  LocalModel: TCustomModel;
  StressPeriods: TModflowStressPeriods;
  StepIndex: Integer;
  StartingTime: Double;
  TimeSeriesStep : integer;
  ClimateRecord: TClimateRecord;
  LenSim: integer;
  STime: double;
//  AStressPeriod: TModflowStressPeriod;
//  TimeStepIndex: Integer;
//  StressPeriodIndex: Integer;
//  StartTime: Double;
//  TimeStepLength: Double;
begin
  if (IRTFL = 3) or (ICUFL = 3) or (IPFL = 3) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.TimeSeries');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data Set 16: TimeSeriesStep MaxT MinT Precip ETref');
      NewLine;

      FWriteLocation := wlOpenClose;


      LocalModel := Model;
      StressPeriods := LocalModel.ModflowFullStressPeriods;
      StartingTime := StressPeriods.First.StartTime;
      LenSim := Trunc(StressPeriods.Last.EndTime - StartingTime);
      FmpClimate := LocalModel.FmpClimate;

      if FmpClimate.Count = 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrTheClimateDataFor,
          StrTheClimateDataFor);
        Exit;
      end;

      FmpClimate.EvaluateClimate;

//      for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
//      begin
//        AStressPeriod := StressPeriods[StressPeriodIndex];
//        StartTime := AStressPeriod.StartTime;
//        TimeStepLength := AStressPeriod.LengthOfFirstTimeStep;
//        for TimeStepIndex := 0 to AStressPeriod.NumberOfSteps - 1 do
//        begin
//          ClimateRecord := FmpClimate.GetClimateTimeValuesFromTime(StartTime);
//          TimeSeriesStep := StepIndex + 1;
//          WriteInteger(TimeSeriesStep);
//          WriteFloat(ClimateRecord.MaxT);
//          WriteFloat(ClimateRecord.MinT);
//          WriteFloat(ClimateRecord.Precip);
//          WriteFloat(ClimateRecord.ETref);
//          WriteString(' # Data Set 16: TimeSeriesStep MaxT MinT Precip ETref');
//          NewLine;
//
//          StartTime := StartTime + TimeStepLength;
//          TimeStepLength := TimeStepLength * AStressPeriod.TimeStepMultiplier;
//        end;
//      end;

      for StepIndex := 0 to LenSim - 1 do
      begin
        STime := StepIndex + StartingTime;
        ClimateRecord := FmpClimate.GetClimateTimeValuesFromTime(STime);
        TimeSeriesStep := StepIndex + 1;
        WriteInteger(TimeSeriesStep);
        WriteFloat(ClimateRecord.MaxT);
        WriteFloat(ClimateRecord.MinT);
        WriteFloat(ClimateRecord.Precip);
        WriteFloat(ClimateRecord.ETref);
        WriteString(' # Data Set 16: TimeSeriesStep MaxT MinT Precip ETref');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet17;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  ACrop: TCropItem;
  CropID: Integer;
begin
  if IDEFFL = -2 then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.IFALLOW');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data Set 17: Crop-ID IFALLOW');
      NewLine;

      FWriteLocation := wlOpenClose;


      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        ACrop := Crops[CropIndex];
        CropID := CropIndex + 1;
        WriteInteger(CropID);

        WriteBooleanValueFromGlobalFormula(ACrop.Fallow, ACrop, 'IFALLOW');
        WriteString(' # Data Set 17: Crop-ID IFALLOW');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet18;
var
  CropIndex: Integer;
  Crops: TCropCollection;
  CropFunctions: TCropFunctionCollection;
  CropFuncItem: TCropFunctionItem;
  CropID: Integer;
begin
  if (IDEFFL > 0) and (IBEN = 1) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.CropFunc');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data set 18: Crop-ID WPF-Slope WPF-Int Crop-Price');
      NewLine;

      FWriteLocation := wlOpenClose;


      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteInteger(CropID);
        CropFunctions := Crops[CropIndex].CropFunctionCollection;
        if (CropFunctions.Count <> 1) then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrTheCropspecificWa, Crops[CropIndex].CropName);
          Exit;
        end;
        Assert(CropFunctions.Count = 1);
        CropFuncItem := CropFunctions[0];
        WriteFloatValueFromGlobalFormula(CropFuncItem.Slope ,
          Crops[CropIndex], StrWPFSlope);
        WriteFloatValueFromGlobalFormula(CropFuncItem.Intercept ,
          Crops[CropIndex], StrWIntercept);
        WriteFloatValueFromGlobalFormula(CropFuncItem.Price ,
          Crops[CropIndex], StrPrice);
        WriteString(' # Data set 18: Crop-ID WPF-Slope WPF-Int Crop-Price');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet19;
var
  CostsItem: TFarmCostsItem;
  FarmIndex: Integer;
  AFarm: TFarm;
begin
  if (IDEFFL > 0) and (ICOST = 1) then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.WaterCost');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data Set 19: Farm-ID GWcost1 GWcost2 GWcost3 GWcost4 SWcost1 SWcost2 SWcost3 SWcost4');
      NewLine;

      FWriteLocation := wlOpenClose;


      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteInteger(AFarm.FarmId);

        if AFarm.FarmCostsCollection.Count > 0 then
        begin
          CostsItem := AFarm.FarmCostsCollection[0];
          WriteFloatValueFromGlobalFormula(CostsItem.GWcost1 ,
            AFarm, StrGWcost1);
          WriteFloatValueFromGlobalFormula(CostsItem.GWcost2 ,
            AFarm, StrGWcost2);
          WriteFloatValueFromGlobalFormula(CostsItem.GWcost3 ,
            AFarm, StrGWcost3);
          WriteFloatValueFromGlobalFormula(CostsItem.GWcost4 ,
            AFarm, StrGWcost4);
          WriteFloatValueFromGlobalFormula(CostsItem.SWcost1 ,
            AFarm, StrSWcost1);
          WriteFloatValueFromGlobalFormula(CostsItem.SWcost2 ,
            AFarm, StrSWcost2);
          WriteFloatValueFromGlobalFormula(CostsItem.SWcost3 ,
            AFarm, StrSWcost3);
          WriteFloatValueFromGlobalFormula(CostsItem.SWcost4 ,
            AFarm, StrSWcost4);
          WriteString(' # Data Set 19: Farm-ID GWcost1 GWcost2 GWcost3 GWcost4 SWcost1 SWcost2 SWcost3 SWcost4');
          NewLine;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrFarmCostsHaveNot,
            Format(StrInFarmDFarmC, [AFarm.FarmId]));
        end;
      end;
    finally
      FWriteLocation := wlMain;
      FOpenCloseFileStream.Free;
    end;
  end;

end;

procedure TModflowFmpWriter.WriteDataSet20;
var
  FarmIndex: Integer;
  Farms: TFarmCollection;
  AFarm: TFarm;
  Allotment: string;
begin
  if IALLOTGW = 1 then
  begin
    Farms := Model.Farms;
    for FarmIndex := 0 to Farms.Count - 1 do
    begin
      AFarm := Farms[FarmIndex];
      if AFarm.GwAllotment.Count > 0 then
      begin
        Allotment := AFarm.GwAllotment[0].Allotment;
        WriteInteger(AFarm.FarmId);
        WriteFloatValueFromGlobalFormula(Allotment, AFarm, 'ALLOTGW');
        WriteString(' # Data Set 20');
        NewLine;
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model, StrFarmAllotmentsNot,
          Format(StrInFarmDTheFar, [AFarm.FarmId]));
      end;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet25;
var
  Farms: TFarmCollection;
  FarmIndex: Integer;
  AFarm: TFarm;
  ALLOTGW: Double;
//  Allotment: string;
begin
  if IALLOTGW = 2 then
  begin
    Farms := Model.Farms;
    for FarmIndex := 0 to Farms.Count - 1 do
    begin
      AFarm := Farms[FarmIndex];
      ALLOTGW := AFarm.GwAllotment.GetAllotmentTimeValuesFromTime(
        FStressPeriodStartTime).Allotment;
      WriteInteger(AFarm.FarmId);
      WriteFloat(ALLOTGW);
      WriteString(' # Data Set 25');
      NewLine;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet26(TimeIndex: Integer);
var
  FarmIDList: TValueCellList;
  Comment: string;
  DataTypeIndex: Integer;
  DataType: TRbwDataType;
  DefaultValue: Double;
//  Dummy: TDataArray;
  AFileName: string;
  FarmIdArray: TDataArray;
begin
  if FFarmIDs.Count <= TimeIndex then
  begin
    frmErrorsAndWarnings.AddError(Model, StrFMPFarmsNotDefine, IntToStr(TimeIndex+1));
    Exit;
  end;
  FarmIDList := FFarmIDs[TimeIndex];
  DefaultValue := 0;
  DataType := rdtInteger;
  DataTypeIndex := 0;
  Comment := 'Data Set 26: FID';

  if FFID_FileStream = nil then
  begin
    AFileName := ChangeFileExt(FNameOfFile, '.FID');
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpFID),
            AFileName, foInput, Model);
    end;
    FFID_FileStream := TFileStream.Create(AFileName,
      fmCreate or fmShareDenyWrite);
  end;

  FarmIdArray := nil;
  FWriteLocation := wlFID;
  try
    WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
      FarmIDList, umAssign, False, FarmIdArray, 'FID', False);
    CheckIntegerDataSet(FarmIdArray, StrInvalidFarmID);
  finally
    FWriteLocation := wlMain;
    FarmIdArray.Free;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet27;
var
  Crops: TCropCollection;
  FarmIndex: Integer;
  AFarm: TFarm;
  Efficiencies: TFarmEfficiencyCollection;
  CropIndex: Integer;
  ACropEffCollection: TCropEfficiencyCollection;
  UsedCropEffItem: TCropEfficiencyItem;
  AFileName: string;
begin
  if IEFFL = 2 then
  begin
    if FOFE_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.OFE');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpOFE),
                AFileName, foInput, Model);
      end;
      FOFE_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpOFE));
    WriteString(' # Data Set 27 Farm-ID OFE(FID,CID)');
    NewLine;

    FWriteLocation := wlOFE;

    try
      Crops := Model.FmpCrops;
      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteFreeInteger(AFarm.FarmId);
        Efficiencies := AFarm.FarmEfficiencyCollection;
        if Efficiencies.Count <> NCROPS then
        begin
          if (Efficiencies.Count <> 1) then
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrCropEfficiencesNot, Format(StrFarmD, [AFarm.FarmId]));
            Exit;
          end;
        end;
        Assert(Efficiencies.Count = NCROPS);
        for CropIndex := 0 to Efficiencies.Count - 1 do
        begin
          ACropEffCollection := Efficiencies[CropIndex].CropEfficiency;
          UsedCropEffItem :=ACropEffCollection.
            ItemByStartTime(FStressPeriodStartTime) as TCropEfficiencyItem;

          if UsedCropEffItem = nil then
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrCropEfficiencesNot, Format(StrFarmD, [AFarm.FarmId]));
            Exit;
          end;

          Assert(UsedCropEffItem <> nil);
          WriteFloatValueFromGlobalFormula(UsedCropEffItem.Efficiency,
            AFarm, Crops[CropIndex].CropName);
        end;
        NewLine;
      end;
    finally
      FWriteLocation := wlMain
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet28(TimeIndex: Integer);
var
  CropIDList: TValueCellList;
  Comment: string;
  DataTypeIndex: Integer;
  DataType: TRbwDataType;
  DefaultValue: Double;
//  Dummy: TDataArray;
  AFileName: string;
  CropIdArray: TDataArray;
begin
  if FCropIDs.Count <= TimeIndex then
  begin
    frmErrorsAndWarnings.AddError(Model, StrFMPCropIDsNotDef, IntToStr(TimeIndex+1));
    Exit;
  end;
  CropIDList := FCropIDs[TimeIndex];
  DefaultValue := 0;
  DataType := rdtInteger;
  DataTypeIndex := 0;
  Comment := 'Data Set 28: CID';

  if FCID_FileStream = nil then
  begin
    AFileName := ChangeFileExt(FNameOfFile, '.CID');
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpCID),
            AFileName, foInput, Model);
    end;
    FCID_FileStream := TFileStream.Create(AFileName,
      fmCreate or fmShareDenyWrite);
  end;

  CropIdArray := nil;
  FWriteLocation := wlCID;
  try
    WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
      CropIDList, umAssign, False, CropIdArray, 'CID', False);
    CheckIntegerDataSet(CropIdArray, StrInvalidCropIDInF);
  finally
    FWriteLocation := wlMain;
    CropIdArray.Free;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet29;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  RootDepths: TFmpRootDepthCollection;
  ARootDepthItem: TRootingDepthItem;
  CropID: Integer;
  AFileName: string;
begin
  if (IRTFL = 2) then
  begin
    if FRoot_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.ROOT');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpRoot),
                AFileName, foInput, Model);
      end;
      FRoot_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpRoot));
    WriteString(' # Data set 29: Crop-ID ROOT');
    NewLine;


    FWriteLocation := wlRoot;
    try
      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteInteger(CropID);

        RootDepths := Crops[CropIndex].FmpRootDepthCollection;
        ARootDepthItem := RootDepths.ItemByStartTime(FStressPeriodStartTime)
          as TRootingDepthItem;
        if ARootDepthItem = nil then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrRootingDepthNotDe, Crops[CropIndex].CropName);
          Exit;
        end;
        Assert(ARootDepthItem <> nil);

        WriteFloatValueFromGlobalFormula(ARootDepthItem.RootingDepth,
          Crops[CropIndex], StrRootingDepth,
          procedure (Value: double)
          begin
            if Value <= 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, StrTheSpecifiedRootin,
                Crops[CropIndex].CropName);
            end;
          end);
        WriteString(' # Data set 26: Crop-ID ROOT');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet30a;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  CropID: Integer;
  WaterUseCollection: TCropWaterUseCollection;
  AWaterUseItem: TCropWaterUseItem;
  AFileName: string;
  ACrop: TCropItem;
begin
  if ICUFL < 3  then
  begin
    if FCropUse_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.CropUse');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpCropUse),
                AFileName, foInput, Model);
      end;
      FCropUse_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpCropUse));
    WriteString(' # Data set 30a: Crop-ID CU {NONIRR}');
    NewLine;


    FWriteLocation := wlCropUse;


//    WriteString('INTERNAL');
//    WriteString(' # Data set 27a: Crop-ID CU {NONIRR}');
//    NewLine;
    Try
      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        ACrop := Crops[CropIndex];
        WriteInteger(CropID);
        WaterUseCollection := ACrop.CropWaterUseCollection;
        AWaterUseItem := WaterUseCollection.ItemByStartTime(FStressPeriodStartTime)
          as TCropWaterUseItem;
        if AWaterUseItem = nil then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrConsumptiveUseFact, ACrop.CropName);
          Exit;
        end;
        Assert(AWaterUseItem <> nil);

        WriteFloatValueFromGlobalFormula(AWaterUseItem.CropValue,
          ACrop, 'Crop-ID CU {NONIRR}');
        WriteBooleanValueFromGlobalFormula('not (' + AWaterUseItem.Irrigated + ')',
          ACrop, 'Crop-ID CU {NONIRR}');
        WriteString(' # Data set 30a: Crop-ID CU NONIRR');
        if CropIndex = 0 then
        begin
          WriteString(' Stress Period: ');
          WriteInteger(FStressPeriod+1);
        end;
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet30b(TimeIndex: Integer);
var
  RefEtList: TValueCellList;
  Comment: string;
  DataTypeIndex: Integer;
  DataType: TRbwDataType;
  DefaultValue: Double;
  Dummy: TDataArray;
  AFileName: string;
begin
  if (ICUFL = 1) or (ICUFL = -1) then
  begin
    if FRefEts.Count <= TimeIndex then
    begin
      frmErrorsAndWarnings.AddError(Model, StrUndefinedFMPRefere, IntToStr(TimeIndex+1));
      Exit;
    end;
    RefEtList := FRefEts[TimeIndex];
    DefaultValue := 0;
    DataType := rdtDouble;
    DataTypeIndex := 0;
    Comment := 'Data Set 30b: ETR';

    if FETR_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.ETR');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpETR),
                AFileName, foInput, Model);
      end;
      FETR_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    FWriteLocation := wlETR;
    try
      WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
        RefEtList, umAssign, False, Dummy, 'ETR');
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet31;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  CropID: Integer;
  EvapFractions: TEvapFractionsCollection;
  EvapFractionItem: TEvapFractionsItem;
  AFileName: string;
begin
  if IFTEFL = 2 then
  begin

    if FET_Frac_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.ET_Frac');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpETFrac),
                AFileName, foInput, Model);
      end;
      FET_Frac_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpETFrac));
    WriteString(' # Data set 31: Crop-ID FTR FEP FEI');
    NewLine;


    FWriteLocation := wlEtFrac;

    try
      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteInteger(CropID);
        EvapFractions := Crops[CropIndex].EvapFractionsCollection;
        EvapFractionItem := EvapFractions.ItemByStartTime(FStressPeriodStartTime)
          as TEvapFractionsItem;
        if EvapFractionItem <> nil then
        begin
          WriteFloatValueFromGlobalFormula(EvapFractionItem.TranspirationFraction,
            Crops[CropIndex], StrTranspiratoryFracti);
          WriteFloatValueFromGlobalFormula(EvapFractionItem.PrecipFraction,
            Crops[CropIndex], StrEvaporativeFraction);
          WriteFloatValueFromGlobalFormula(EvapFractionItem.IrrigFraction,
            Crops[CropIndex], StrEvaporativeFractionIrr);
          WriteString(' # Data set 32: Crop-ID FTR FEP FEI');
          NewLine;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrTheEvaporationFrac,
            Format(StrCrop0sTime, [Crops[CropIndex].CropName,
            FStressPeriodStartTime]));
        end;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet32;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  Losses: TLossesCollection;
  CropID: Integer;
  LossesItem: TLossesItem;
  AFileName: string;
begin
  if (IIESWFL = 2) then
  begin
    if FSW_Losses_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.SW_Losses');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpSwLosses),
                AFileName, foInput, Model);
      end;
      FSW_Losses_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpSwLosses));
    WriteString(' # Data set 32: Crop-ID FIESWP FIESWI');
    NewLine;


    FWriteLocation := wlSwLosses;

    try
      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteInteger(CropID);
        Losses := Crops[CropIndex].LossesCollection;
        LossesItem := Losses.ItemByStartTime(FStressPeriodStartTime) as TLossesItem;
        WriteFloatValueFromGlobalFormula(LossesItem.PrecipitationLosses,
          Crops[CropIndex], StrFractionOfInefficPrecip);
        WriteFloatValueFromGlobalFormula(LossesItem.IrrigationLosses,
          Crops[CropIndex], StrFractionOfInefficIrrig);
        WriteString(' # Data set 32: Crop-ID FIESWP FIESWI');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet2a;
begin
  WriteString('FLAG_BLOCKS # DataSet 2a');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cAuxVar;
//var
//  AuxUsed: Boolean;
var
  ChildModels: TChildModelCollection;
  ChildIndex: Integer;
begin
  if FFarmProcess.ResetMnwQMax
    and (Model.ModflowPackages.Mnw1Package.IsSelected
    or Model.ModflowPackages.Mnw2Package.IsSelected) then
  begin
    WriteString('AUX QMAXRESET ');
  end;
  case FFarmProcess.CropIrrigationRequirement of
    cirContinuously: ;
    cirOnlyWhenNeeded:
      begin
        WriteString('AUX NOCIRNOQ ');
      end;
    else Assert(False);
  end;
  if Model is TChildModel then
  begin
    ChildModels := frmGoPhast.PhastModel.ChildModels;
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      if ChildModels[ChildIndex].ChildModel = Model then
      begin
        WriteString('AUX LGRGRID');
//        WriteInteger(ChildIndex+1);
        break;
      end;
    end;
  end;
  // FMP does not accept auxilliary variables.
//  WriteString('AUX IFACE');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cCropConsumptiveUse;
begin
  WriteInteger(ICCFL);
  WriteString(' # Data Set 2c Crop Consumptive-Use Flag: ICCFL');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cMnwOptions;
var
  QCLOSE: Double;
  HPCT: Double;
  RPCT: Double;
begin
  if FFarmProcess.MnwClose then
  begin
    QCLOSE := FFarmProcess.MnwClosureCriterion;
    HPCT := FFarmProcess.HeadChangeReduction;
    RPCT := FFarmProcess.ResidualChangeReduction;
    WriteFloat(QClose);
    WriteFloat(HPCT);
    WriteFloat(RPCT);
    WriteString('# Data Set 2c: QCLOSE HPCT RPCT');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet2cOptions;
var
  OptionsUsed: Boolean;
  WellFieldOption: TWellFieldOption;
begin
  OptionsUsed := False;
  // CBC is not specified because the groundwater transport process is not included.
  if not Model.ModflowOutputControl.PrintInputCellLists then
  begin
    WriteString('NOPRINT ');
    OptionsUsed := True;
  end;

  WellFieldOption := GetWellFieldOption;
  if WellFieldOption = wfUsed then
  begin
    WriteString('WELLFIELD ');
    OptionsUsed := True;
  end;

  if FFarmProcess.RecomputeOption = roComputed then
  begin
    WriteString('RECOMP_Q_BD ');
    OptionsUsed := True;
  end;

  if FFarmProcess.MnwClose then
  begin
    WriteString('MNWCLOSE ');
    OptionsUsed := True;
  end;

  if not OptionsUsed then
  begin
    WriteString('NOOPT');
  end;
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cParamDimen;
const
  Dummy = -1000;
var
  MXACTW: Integer;
  NFARMS: Integer;
  NSOILS: Integer;
  LocalModel: TCustomModel;
begin
  CountCells(MXACTW);
  NFARMS := NumberOfFarms;
  LocalModel := Model;
  NSOILS := LocalModel.FmpSoils.Count;

  WriteInteger(MXACTW);
  // MXACTFWP is always omitted in ModelMuse.
  WriteInteger(NFARMS);
  WriteInteger(NCROPS);
  WriteInteger(NSOILS);
  WriteString(' # Data Set 2c Parameter Dimensions: MXACTW NFARMS NCROPS NSOILS');
  NewLine
end;

procedure TModflowFmpWriter.WriteDataSet2cPrintFlags;
const
  Dummy = -1000;
var
  IFWLCB, IFNRCB, ISDPFL, IFBPFL, IRTPFL, IOPFL, IPAPFL, IETPFL: Integer;
  FileDir: string;
  CommentString: string;
  IRTPFL_Required: Boolean;
//  AFileName: string;
begin
  FileDir := IncludeTrailingPathDelimiter(ExtractFileDir(FNameOfFile));

  GetFlowUnitNumber(IFWLCB);
  if IFWLCB = 1 then
  begin
    Model.AddModelOutputFile(FileDir + 'FWELLS.OUT');
  end;
  {$REGION 'IFNRCB'}
  FarmBudgetUnitNumber(IFNRCB);
  case IFNRCB of
    1:
      begin
        Model.AddModelOutputFile(FileDir + 'FNRCH_ARRAY.OUT');
      end;
    2:
      begin
        Model.AddModelInputFile(FileDir + 'FNRCH_LIST.OUT');
      end;
    3:
      begin
        Model.AddModelOutputFile(FileDir + 'FNRCH_LIST_BIN.OUT');
      end;
  end;
  {$ENDREGION}

  {$REGION 'ISDPFL'}
  ISDPFL := Dummy;
  case FFarmProcess.SupplyAndDemand of
    sadDefault:
      begin
        case Model.ModflowOutputControl.SaveCellFlows of
          csfNone: ISDPFL := 0;
          csfBinary: ISDPFL := Model.UnitNumbers.UnitNumber(StrFmpSupplyDemand);
          csfListing: ISDPFL := -1;
        end;
      end;
    sadListingEveryIteration: ISDPFL := -3;
    sadListingEachTimeStep: ISDPFL := -2;
//    sadListingEachTimeStepWhenBudgetSaved: ISDPFL := -1;
//    sadNone: ISDPFL := 0;
    sadAscii: ISDPFL := 1;
//    sadBinary:
//      begin
//        ISDPFL := Model.UnitNumbers.UnitNumber(StrFmpSupplyDemand);
//      end;
    else Assert(False);
  end;
  if ISDPFL = 1 then
  begin
    Model.AddModelOutputFile(FileDir + 'FDS.OUT');
  end
  else if ISDPFL > 1 then
  begin
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATABINARY, ISDPFL, ChangeFileExt(FNameOfFile,
            '.FDS_BIN'), foOutput, Model);
    end;
  end;
  {$ENDREGION}

  {$REGION 'IFBPFL'}
  IFBPFL := Dummy;
  case FFarmProcess.FarmBudgetPrintFlags of
    fbpNone: IFBPFL := 0;
    fbpAscii:
      begin
        case FFarmProcess.FarmBudgetPrintHowMuch of
          fbpCompact: IFBPFL := 1;
          fbpDetailed: IFBPFL := 2;
          else Assert(False);
        end;
      end;
    fbpBinary:
      begin
        case FFarmProcess.FarmBudgetPrintHowMuch of
          fbpCompact: IFBPFL := Model.UnitNumbers.UnitNumber(StrFmpFarmBudgetCompact);
          fbpDetailed: IFBPFL := Model.UnitNumbers.UnitNumber(StrFmpFarmBudgetDetailed);
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;
  case IFBPFL of
    1:
      begin
        Model.AddModelOutputFile(FileDir + 'FB_COMPACT.OUT');
      end;
    2:
      begin
        Model.AddModelOutputFile(FileDir + 'FB_DETAILS.OUT');
      end;
    else
      begin
        if IFBPFL > 2 then
        begin
          if Odd(IFBPFL) then
          begin
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, IFBPFL,
                            ChangeFileExt(FNameOfFile, '.FB_COMPACT_BIN_OUT'), foOutput, Model);
            end;
          end
          else
          begin
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, IFBPFL,
                ChangeFileExt(FNameOfFile, '.FB_DETAILS_BIN_OUT'),
                foOutput, Model);
            end;
          end;
        end;
      end;
  end;
  {$ENDREGION}

  {$REGION 'IETPFL'}
  IETPFL := Ord(FFarmProcess.EtPrintType);
  if FFarmProcess.EtPrintLocation = eplListing then
  begin
    IETPFL := -IETPFL;
  end;
  {$ENDREGION}

  WriteInteger(IFWLCB);
  WriteInteger(IFNRCB);
  WriteInteger(ISDPFL);
  WriteInteger(IFBPFL);
  WriteInteger(IETPFL);

  CommentString := ' # Data Set 2c Mandatory and Optional Print Flags: IFWLCB IFNRCB ISDPFL IFBPFL IETPFL';

  IRTPFL_Required := Model.ModflowPackages.SfrPackage.IsSelected;
  if (not IRTPFL_Required) and (Model is TChildModel) then
  begin
    IRTPFL_Required := TChildModel(Model).ParentModel.
      ModflowPackages.SfrPackage.IsSelected;
  end;
  if IRTPFL_Required then
  begin
    {$REGION 'IRTPFL'}
    IRTPFL := Dummy;
    case FFarmProcess.PrintRouting of
      prNoRoutingPrinted: IRTPFL := 0;
      prListingFile:
        begin
          case FFarmProcess.PrintRoutingFrequency of
            prfAllPeriods: IRTPFL := -1;
            prfFirstPeriod: IRTPFL := -2;
            else Assert(False);
          end;
        end;
      prAscii:
        begin
          case FFarmProcess.PrintRoutingFrequency of
            prfAllPeriods: IRTPFL := 1;
            prfFirstPeriod: IRTPFL := 2;
            else Assert(False);
          end;
        end;
      else Assert(False);
    end;
    if IRTPFL in [1,2] then
    begin
      Model.AddModelOutputFile(FileDir + 'ROUT.OUT');
    end;
    {$ENDREGION}
    WriteInteger(IRTPFL);
    CommentString := CommentString + ' IRTPFL';
  end;
  if IDEFFL > 0 then
  begin
    {$REGION 'IOPFL'}
    IOPFL := Ord(FFarmProcess.AcerageOptimizationPrintChoice);
    if FFarmProcess.AcerageOptimizationPrintLocation = aoplListing then
    begin
      IOPFL := -IOPFL;
    end;
    if IOPFL > 0 then
    begin
      Model.AddModelOutputFile(FileDir + 'ACR_OPT.OUT');
    end;
    {$ENDREGION}
    WriteInteger(IOPFL);
    CommentString := CommentString + ' IOPFL';
  end;
  if  IALLOTSW > 1 then
  begin
   {$REGION 'IPAPFL'}
   IPAPFL := Dummy;
    case FFarmProcess.DiversionBudgetLocation of
      dblListing: IPAPFL := -1;
      dblExternal: IPAPFL := 1;
    end;
    if IPAPFL = 1 then
    begin
      Model.AddModelOutputFile(FileDir + 'PRIOR.OUT');
    end;
    {$ENDREGION}
    WriteInteger(IPAPFL);
    CommentString := CommentString + ' IPAPFL';
  end;
  WriteString(CommentString);
  NewLine;
end;

function TModflowFmpWriter.GetSemiRoutedSurfaceWaterDeliveryFlag: integer;
var
  index: Integer;
  AFarm: TFarm;
begin
  result := 0;

  if not Model.ModflowPackages.SfrPackage.IsSelected
    and not Model.ModflowPackages.SwrPackage.IsSelected then
  begin
    Exit;
  end;

  for index := 0 to FFarms.Count - 1 do
  begin
    AFarm := FFarms[index];
    if AFarm.SemiRoutedDeliveries.Count > 0 then
    begin
      result := 2;
      Exit;
    end;
  end;
end;

function TModflowFmpWriter.GetSemiRoutedSurfaceWaterRunoffReturnflowFlag: integer;
var
  index: Integer;
  AFarm: TFarm;
begin
  result := 0;
  
  if not Model.ModflowPackages.SfrPackage.IsSelected
    and not Model.ModflowPackages.SwrPackage.IsSelected then
  begin
    Exit;
  end;
  
  for index := 0 to FFarms.Count - 1 do
  begin
    AFarm := FFarms[index];
    if AFarm.SemiRoutedReturnFlow.Count > 0 then
    begin
      result := 2;
      Exit;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet2cSurfaceWater;
var
  IRDFL, IRRFL : integer;
  PCLOSE: double;
  DataSetLabel: string;
  SfrSelected: boolean;
begin

  PCLOSE := FFarmProcess.SurfaceWaterClosure;

  SfrSelected := Model.ModflowPackages.SfrPackage.IsSelected;
  if (not SfrSelected) and (Model is TChildModel) then
  begin
    SfrSelected := TChildModel(Model).ParentModel.ModflowPackages.SfrPackage.IsSelected;
  end;

  IRDFL := 0;
  case FFarmProcess.RoutedDelivery of
    rdNone: IRDFL := 0;
    rdDiversion: IRDFL := 1;
    rdAny: IRDFL := -1;
    else Assert(False);
  end;

  if (IRDFL <> 0) and not SfrSelected then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidFarmSurface,
      StrIRDFLInTheFarmPr);
  end;

  if not SfrSelected {or (IRDFL = 0)}  then
  begin
    IRRFL := 0;
  end
  else
  begin
    IRRFL := 0;
    case FFarmProcess.RoutedReturn of
//      rrNone: IRRFL := 0;
      rrNonDiversion: IRRFL := 1;
      rrAny: IRRFL := -1;
      else Assert(False);
    end;
  end;

//  if (IRRFL <> 0) and not SfrSelected then
//  begin
//    frmErrorsAndWarnings.AddError(Model, StrInvalidFarmSurface,
//      StrIRRFLInTheFarmPr);
//  end;

  DataSetLabel := ' # Data Set 2c Surface-Water Flags: INRDFL ';
  WriteInteger(INRDFL);
  if INRDFL = 1 then
  begin
    WriteInteger(MXNRDT);
    DataSetLabel := DataSetLabel + 'MXNRDT ';
  end;
  WriteInteger(ISRDFL);
  WriteInteger(IRDFL);
  WriteInteger(ISRRFL);
  WriteInteger(IRRFL);
  WriteInteger(IALLOTSW);
  DataSetLabel := DataSetLabel + 'ISRDFL IRDFL ISRRFL IRRFL IALLOTSW ';
  if IALLOTSW > 1 then
  begin
    WriteFloat(PCLOSE);
    DataSetLabel := DataSetLabel + 'PCLOSE ';
  end;
  WriteString(DataSetLabel);
  NewLine;
end;

function TModflowFmpWriter.GetCropFractionFlag: integer;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  EvapFractions: TEvapFractionsCollection;
begin
  result := 1;
  Crops := Model.FmpCrops;
  for CropIndex := 0 to Crops.Count - 1 do
  begin
    EvapFractions := Crops[CropIndex].EvapFractionsCollection;
    if EvapFractions.Count > 1 then
    begin
      result := 2;
      Exit;
    end;
  end;
end;

function TModflowFmpWriter.GetCropBenefitsFlag: integer;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  CropFunctions: TCropFunctionCollection;
begin
  result := 0;
  if IDEFFL > 0 then
  begin
    result := 1;
    Crops := Model.FmpCrops;
    for CropIndex := 0 to Crops.Count - 1 do
    begin
      CropFunctions := Crops[CropIndex].CropFunctionCollection;
      if CropFunctions.Count > 1 then
      begin
        result := 2;
        Exit;
      end;
    end;

  end;
end;

function TModflowFmpWriter.GetWaterCostCoefficientsFlag: integer;
var
  FarmIndex: Integer;
  AFarm: TFarm;
begin
//  result := -1;
  if FFarmProcess.DeficiencyPolicy in
    [dpWaterStacking, dpDeficitIrrigation, dpNoPolicy] then
  begin
    result := 0;
  end
  else
  begin
    result := 1;
    for FarmIndex := 0 to FFarms.Count - 1 do
    begin
      AFarm := FFarms[FarmIndex];
      if AFarm.FarmCostsCollection.Count > 1 then
      begin
        result := 2;
        Exit;
      end;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet2cWaterPolicy;
var
  IEBFL, IROTFL: integer;
  CommentString: string;
begin
  IROTFL := -1;
  {$REGION 'IEBFL'}
  IEBFL := -1;
  case FFarmProcess.EfficiencyGroundwaterFunction of
    egfDeliveriesVary:
      begin
        case FFarmProcess.EfficiencyReset of
          erStressPeriod:
            begin
              IEBFL := 0;
            end;
          erTimeStep:
            begin
              IEBFL := 1;
            end;
          else
            Assert(False);
        end;
      end;
    egfEfficienciesVary:
      begin
        case FFarmProcess.EfficiencyReset of
          erStressPeriod:
            begin
              IEBFL := 2;
            end;
          erTimeStep:
            begin
              IEBFL := 3;
            end;
          else
            Assert(False);
        end;
      end;
    else
      Assert(False);
  end;
  {$ENDREGION}

  WriteInteger(IEBFL);
  WriteInteger(IROTFL);
  WriteInteger(IDEFFL);
  CommentString := ' # Data set 2c Water Policy Flags: IEBFL IROTFL IDEFFL';
  if  IDEFFL > 0 then
  begin
    WriteInteger(IBEN);
    WriteInteger(ICOST);
    CommentString := CommentString + ' IBEN ICOST';
  end;
  if IALLOTGW < 0 then
  begin
    WriteString(' P');
  end
  else
  begin
    WriteInteger(IALLOTGW);
  end;
  CommentString := CommentString + ' IALLOTGW';
  WriteString(CommentString);
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cWhenToRead;
const
  IFRMFL = 2;
begin
  WriteInteger(IFRMFL);
  WriteInteger(IRTFL);
  WriteInteger(ICUFL);
  WriteInteger(IPFL);
  WriteInteger(IFTEFL);
  WriteInteger(IIESWFL);
  WriteInteger(IEFFL);
  WriteString(' # Data Set 2c When-to-Read-Flags: IFRMFL, IRTFL ICUFL IPFL IFTEFL IIESWFL IEFFL');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet33(TimeIndex: Integer);
var
  PrecipList: TValueCellList;
  Comment: string;
  DataTypeIndex: Integer;
  DataType: TRbwDataType;
  DefaultValue: Double;
  Dummy: TDataArray;
  AFileName: string;
begin
  if (IPFL = 2) then
  begin
    if FPrecip.Count <= TimeIndex then
    begin
      frmErrorsAndWarnings.AddError(Model, StrFMPPrecipitationNo, IntToStr(TimeIndex+1));
      Exit;
    end;
    PrecipList := FPrecip[TimeIndex];
    DefaultValue := 0;
    DataType := rdtDouble;
    DataTypeIndex := 0;
    Comment := 'Data Set 33: PFLX';

    if FPFLX_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.PFLX');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpPFLX),
                AFileName, foInput, Model);
      end;
      FPFLX_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    FWriteLocation := wlPFLX;
    try
      WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
        PrecipList, FFarmProcess.AssignmentMethod, False, Dummy, 'PFLX');
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet34;
var
  CropIndex: Integer;
  Crops: TCropCollection;
  CropFunctions: TCropFunctionCollection;
  CropFuncItem: TCropFunctionItem;
  CropID: Integer;
  AFileName: string;
begin
  if (IDEFFL > 0) and (IBEN = 2) then
  begin
    if FCropFunc_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.CropFunc');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpCropFunc),
                AFileName, foInput, Model);
      end;
      FCropFunc_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpCropFunc));
    WriteString(' # Data set 34: Crop-ID WPF-Slope WPF-Int Crop-Price');
    NewLine;


    FWriteLocation := wlCropFunc;
    try
      Crops := Model.FmpCrops;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        CropID := CropIndex + 1;
        WriteInteger(CropID);
        CropFunctions := Crops[CropIndex].CropFunctionCollection;
        CropFuncItem := CropFunctions.ItemByStartTime(FStressPeriodStartTime) as TCropFunctionItem;
        WriteFloatValueFromGlobalFormula(CropFuncItem.Slope ,
          Crops[CropIndex], StrWPFSlope);
        WriteFloatValueFromGlobalFormula(CropFuncItem.Intercept ,
          Crops[CropIndex], StrWIntercept);
        WriteFloatValueFromGlobalFormula(CropFuncItem.Price ,
          Crops[CropIndex], StrPrice);
        WriteString(' # Data set 34: Crop-ID WPF-Slope WPF-Int Crop-Price');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet35;
var
  CostsItem: TFarmCostsItem;
  FarmIndex: Integer;
  AFarm: TFarm;
  AFileName: string;
begin
  if (IDEFFL > 0) and (ICOST = 2) then
  begin
    if FWaterCost_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.WaterCost');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpWaterCost),
                AFileName, foInput, Model);
      end;
      FWaterCost_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpWaterCost));
    WriteString(' # Data Set 35: Farm-ID GWcost1 GWcost2 GWcost3 GWcost4 SWcost1 SWcost2 SWcost3 SWcost4');
    NewLine;

    FWriteLocation := wlWaterCost;
    try
      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteInteger(AFarm.FarmId);
        CostsItem := AFarm.FarmCostsCollection.ItemByStartTime(FStressPeriodStartTime) as TFarmCostsItem;
        WriteFloatValueFromGlobalFormula(CostsItem.GWcost1 ,
          AFarm, StrGWcost1);
        WriteFloatValueFromGlobalFormula(CostsItem.GWcost2 ,
          AFarm, StrGWcost2);
        WriteFloatValueFromGlobalFormula(CostsItem.GWcost3 ,
          AFarm, StrGWcost3);
        WriteFloatValueFromGlobalFormula(CostsItem.GWcost4 ,
          AFarm, StrGWcost4);
        WriteFloatValueFromGlobalFormula(CostsItem.SWcost1 ,
          AFarm, StrSWcost1);
        WriteFloatValueFromGlobalFormula(CostsItem.SWcost2 ,
          AFarm, StrSWcost2);
        WriteFloatValueFromGlobalFormula(CostsItem.SWcost3 ,
          AFarm, StrSWcost3);
        WriteFloatValueFromGlobalFormula(CostsItem.SWcost4 ,
          AFarm, StrSWcost4);
        WriteString(' # Data Set 35: Farm-ID GWcost1 GWcost2 GWcost3 GWcost4 SWcost1 SWcost2 SWcost3 SWcost4');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet36;
var
  DeliveryIndex: Integer;
  FarmIndex: Integer;
  AFarm: TFarm;
  DeliveryParam: TDeliveryParamCollection;
  DPam: TNonRoutedDeliveryParameterCollection;
  NonRoutedDeliv: TNonRoutedDeliveryParameterItem;
  NRDU: Integer;
  Expression: TExpression;
  AFileName: string;
begin
  if INRDFL = 1 then
  begin
    if FDeliveries_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.NonRouteDeliv');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpDeliveries),
                AFileName, foInput, Model);
      end;
      FDeliveries_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpDeliveries));
    WriteString(' # Data Set 36: Farm-ID (NRDV NRDR NRDU)...');
    NewLine;

    FWriteLocation := wlDeliveries;


    try
      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteFreeInteger(AFarm.FarmId);
        DeliveryParam := AFarm.DeliveryParamCollection;
        for DeliveryIndex := 0 to DeliveryParam.Count - 1 do
        begin
          DPam := DeliveryParam[DeliveryIndex].DeliveryParam;
          NonRoutedDeliv := DPam.ItemByStartTime(FStressPeriodStartTime)
            as TNonRoutedDeliveryParameterItem;
          WriteFloatValueFromGlobalFormula(NonRoutedDeliv.Volume ,
            AFarm, 'NRDV');
          WriteIntegerValueFromGlobalFormula(NonRoutedDeliv.Rank ,
            AFarm, 'NRDR',
            procedure (Value: integer)
            begin
              if Value <= 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, StrThePriorityForNon,
                  AFarm.FarmName);
              end;
            end);
          if NonRoutedDeliv.NonRoutedDeliveryType = nrdtVirtualFarm then
          begin
            Expression := EvaluateValueFromGlobalFormula(NonRoutedDeliv.VirtualFarm,
              AFarm, 'NRDU', [rdtInteger]);
            NRDU := Expression.IntegerResult;
            if NRDU > 0 then
            begin
              NRDU := -NRDU;
            end;
            if NRDU = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrVirtualFarmNumber, Format(StrFarmD, [AFarm.FarmId]));
              Exit;
            end;
            Assert(NRDU <> 0);
          end
          else
          begin
            NRDU := Ord(NonRoutedDeliv.NonRoutedDeliveryType);
          end;
          WriteInteger(NRDU);
        end;
        for DeliveryIndex := DeliveryParam.Count to MXNRDT - 1 do
        begin
          WriteFloatCondensed(0);
          WriteFreeInteger(DeliveryIndex+1);
          WriteFreeInteger(0);
        end;
        WriteString(' # Data Set 36: Farm-ID (NRDV NRDR NRDU)...');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet37a;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  ADelivery: TSemiRoutedDeliveriesAndRunoffItem;
  ReturnCell: TCellLocation;
  SegmentReach: TSegmentReach;
  AFileName: string;
begin
  if ISRDFL = 2 then
  begin
    if FSemiDeliveries_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.SemiRouteDeliv');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpSemiRouteDeliv),
                AFileName, foInput, Model);
      end;
      FSemiDeliveries_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpSemiRouteDeliv));
    WriteString(' # Data set 37a Farm-ID Row Column Segment Reach');
    NewLine;

    FWriteLocation := wlSemiRouteDeliv;

    try
      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteInteger(AFarm.FarmId);
        ADelivery := AFarm.SemiRoutedDeliveries.ItemByStartTime(
          FStressPeriodStartTime) as TSemiRoutedDeliveriesAndRunoffItem;
        if ADelivery = nil then
        begin
          WriteInteger(0);
          WriteInteger(0);
          WriteInteger(0);
          WriteInteger(0);
        end
        else
        begin
          ReturnCell := ADelivery.LinkedStream.ReturnCellLocation(Model);
          WriteInteger(ReturnCell.Row);
          WriteInteger(ReturnCell.Column);
          SegmentReach := ADelivery.LinkedStream.SegmentReach;
          WriteInteger(SegmentReach.Segment);
          WriteInteger(SegmentReach.Reach);
        end;
        WriteString(' # Data set 37a Farm-ID Row Column Segment Reach');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet37b;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  ADelivery: TSemiRoutedDeliveriesAndRunoffItem;
  ReturnCell: TCellLocation;
  SegmentReach: TSegmentReach;
  AFileName: string;
begin
  if ISRRFL = 2 then
  begin
    if FSemiReturn_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.SemiRouteReturn');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpSemiRouteReturn),
                AFileName, foInput, Model);
      end;
      FSemiReturn_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpSemiRouteReturn));
    WriteString(' # Data set 37b Farm-ID Row Column Segment Reach');
    NewLine;

    FWriteLocation := wlSemiRouteReturn;

    try
      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteInteger(AFarm.FarmId);
        ADelivery := AFarm.SemiRoutedReturnFlow.ItemByStartTime(
          FStressPeriodStartTime) as TSemiRoutedDeliveriesAndRunoffItem;
        if ADelivery = nil then
        begin
          WriteInteger(0);
          WriteInteger(0);
          WriteInteger(0);
          WriteInteger(0);
        end
        else
        begin
          ReturnCell := ADelivery.LinkedStream.ReturnCellLocation(Model);
          WriteInteger(ReturnCell.Row);
          WriteInteger(ReturnCell.Column);
          SegmentReach := ADelivery.LinkedStream.SegmentReach;
          WriteInteger(SegmentReach.Segment);
          WriteInteger(SegmentReach.Reach);
        end;
        WriteString(' # Data set 37b Farm-ID Row Column Segment Reach');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet38;
var
  FmpAllotment: TAllotmentCollection;
  AllotmentRecord: TAllotmentRecord;
begin
  if (IALLOTSW = 1) then
  begin
    FmpAllotment := Model.FmpAllotment;

    AllotmentRecord := FmpAllotment.GetAllotmentTimeValuesFromTime(FStressPeriodStartTime);

    WriteFloat(AllotmentRecord.Allotment);
    WriteString(' # Data Set 38: ALLOT');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet39;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  WaterRights: TWaterRightsCollection;
  WaterRightsItem: TWaterRightsItem;
  AFileName: string;
begin
  if IALLOTSW = 2 then
  begin
    if FCall_FileStream = nil then
    begin
      AFileName := ChangeFileExt(FNameOfFile, '.CALL');
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpCall),
                AFileName, foInput, Model);
      end;
      FCall_FileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    end;

    WriteString('EXTERNAL ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpCall));
    WriteString(' # Data Set 39: Farm-ID CALL');
    NewLine;

    FWriteLocation := wlCall;

    try
      for FarmIndex := 0 to FFarms.Count - 1 do
      begin
        AFarm := FFarms[FarmIndex];
        WriteInteger(AFarm.FarmId);
        WaterRights := AFarm.WaterRights;
        WaterRightsItem := WaterRights.ItemByStartTime(FStressPeriodStartTime)
          as TWaterRightsItem;
        WriteFloatValueFromGlobalFormula(WaterRightsItem.WaterRights ,
          AFarm, 'Call');
        WriteString(' # Data Set 39: Farm-ID CALL');
        NewLine;
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;

end;

procedure TModflowFmpWriter.WriteDataSets22to39;
const
  D7PName =      ' # Data Set 24: PARNAM';
  D7PNameIname = ' # Data Set 24: PARNAM Iname';
  DS5 = ' # Data Set 22: ITMP NP';
  DataSetIdentifier = 'Data Set 23:';
  VariableIdentifiers = 'QMAX';
begin
  // WriteBeforeCells and WriteCustomStressPeriod are called
  // by WriteStressPeriods.
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowFmpWriter.WriteDataSets3And4;
const
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'QMAXfact';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, umAssign, nil, nil);
end;

procedure TModflowFmpWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmProcess);
  if (not Package.IsSelected) or (Model.ModelSelection <> msModflowFmp) then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrFMP) then
  begin
    Exit;
  end;
  if Model is TChildModel then
  begin
    if not TChildModel(Model).ParentModel.ModflowPackages.FarmProcess.IsSelected then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidFarmProcess, StrIfTheFarmProcess);
    end;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(StrFMP, Model.UnitNumbers.UnitNumber(StrFMP),
    FNameOfFile, foInput, Model);

  EvaluateAll;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

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

function TModflowFmpWriter.GetWellFieldOption: TWellFieldOption;
var
  DPamItem: TNonRoutedDeliveryParameterItem;
  DPam: TNonRoutedDeliveryParameterCollection;
  DeliveryParam: TDeliveryParamCollection;
  TimeIndex: Integer;
  DeliveryIndex: Integer;
  FarmIndex: Integer;
begin
  result := wfNotUsed;
  for FarmIndex := 0 to FFarms.Count - 1 do
  begin
    DeliveryParam := FFarms[FarmIndex].DeliveryParamCollection;
    for DeliveryIndex := 0 to DeliveryParam.Count - 1 do
    begin
      DPam := DeliveryParam[DeliveryIndex].DeliveryParam;
      for TimeIndex := 0 to DPam.Count - 1 do
      begin
        DPamItem := DPam[TimeIndex];
        if DPamItem.NonRoutedDeliveryType = nrdtVirtualFarm then
        begin
          result := wfUsed;
          break;
        end;
      end;
      if result = wfUsed then
      begin
        break;
      end;
    end;
    if result = wfUsed then
    begin
      break;
    end;
  end;
end;

procedure TModflowFmpWriter.EvaluateReferenceET;
var
  EmptyParamList: TStringList;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Boundary: TFmpRefEvapBoundary;
begin
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
      Boundary := ScreenObject.ModflowFmpRefEvap;
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(FRefEts, EmptyParamList, Model);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end
end;

procedure TModflowFmpWriter.EvaluatePrecip;
var
  EmptyParamList: TStringList;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Boundary: TFmpPrecipBoundary;
//  Boundary: TFmpRefEvapBoundary;
begin
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
      Boundary := ScreenObject.ModflowFmpPrecip;
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(FPrecip, EmptyParamList, Model);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end
end;

procedure TModflowFmpWriter.EvaluateCropID;
var
  EmptyParamList: TStringList;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Boundary: TFmpCropIDBoundary;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidCropIDInF);
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
      Boundary := ScreenObject.ModflowFmpCropID;
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(FCropIDs, EmptyParamList, Model);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.EvaluateFarmID;
var
  EmptyParamList: TStringList;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Boundary: TFmpFarmIDBoundary;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmID);
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
      Boundary := ScreenObject.ModflowFmpFarmID;
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(FFarmIDs, EmptyParamList, Model);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.EvaluateGwAllotment;
var
  Farms: TFarmCollection;
  FarmIndex: Integer;
  AFarm: TFarm;
begin
  if IALLOTGW = 2 then
  begin
    Farms := Model.Farms;
    for FarmIndex := 0 to Farms.Count - 1 do
    begin
      AFarm := Farms[FarmIndex];
      AFarm.GwAllotment.EvaluateAllotment;
    end;
  end;
end;

function TModflowFmpWriter.GetConsumptiveUseFlag: Integer;
begin
  result := 0;
  case FFarmProcess.ConsumptiveUse of
    cuCalculated: result := 3;
    cuPotentialET: result := 2;
    cuPotentialAndReferenceET: result := 1;
    cuCropCoefficient: result := -1;
    else
      Assert(False);
  end;
end;

function TModflowFmpWriter.GetCropConsumptiveUseFlag: Integer;
begin
  result := 0;
  case FFarmProcess.CropConsumptiveLinkage of
    cclNotLinked:
      begin
        case FFarmProcess.CropConsumptiveConcept of
          cccConcept1:
            begin
              result := 1;
            end;
          cccConcept2:
            begin
              result := 2;
            end;
        end;
      end;
    cclLinked:
      begin
        case FFarmProcess.CropConsumptiveConcept of
          cccConcept1:
            begin
              result := 3;
            end;
          cccConcept2:
            begin
              result := 4;
            end;
        end;
      end;
  end;
end;

procedure TModflowFmpWriter.FarmBudgetUnitNumber(var UnitNumber: Integer);
begin
  case FFarmProcess.SaveNetRecharge of
    snrAsciiByCell: UnitNumber := 1;
    snrAsciiByFarm: UnitNumber := 2;
    snrBinary: UnitNumber := 3;
    else inherited GetFlowUnitNumber(UnitNumber);
  end;
end;

procedure TModflowFmpWriter.FillFarmList;
var
  LocalModel: TCustomModel;
  FarmIndex: Integer;
begin
  LocalModel := Model;
  for FarmIndex := 0 to LocalModel.Farms.Count - 1 do
  begin
    FFarms.Add(LocalModel.Farms[FarmIndex]);
  end;
  if FFarms.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoFarmsHaveBeenD, StrNoFarmsHaveBeenD);
  end;
end;

procedure TModflowFmpWriter.WriteDataSet8;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(KSoilID);
  Assert(DataArray <> nil);
  CheckIntegerDataSet(DataArray, StrInvalidSoilIDInF);
  FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
  FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.SID');
  Model.AddModelInputFile(FOpenCloseFileName);
  FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
    fmCreate or fmShareDenyWrite);
  try
    FWriteLocation := wlOpenClose;
    FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);
    WriteArray(DataArray, 0, 'Data set 8: SID', StrNoValueAssigned, 'SID');
  finally
    FreeAndNil(FOpenCloseFileStream);
    FWriteLocation := wlMain;
  end;
end;

procedure TModflowFmpWriter.Evaluate;
var
  TimeIndex: Integer;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    inherited;
    if Model.ModflowPackages.Mt3dBasic.IsSelected then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheFarmProcessIs,
        StrMT3DMSVersion53D);
    end;

    if (Values.Count = 0) and (ParamValues.Count = 0) then
    begin
      for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
      begin
        Values.Add(TValueCellList.Create(TFmpWell_Cell));
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmpWriter.FreeFileStreams;
begin
 FreeAndNil(FOFE_FileStream);
 FreeAndNil(FFID_FileStream);
 FreeAndNil(FCID_FileStream);
 FreeAndNil(FRoot_FileStream);
 FreeAndNil(FCropUse_FileStream);
 FreeAndNil(FETR_FileStream);
 FreeAndNil(FET_Frac_FileStream);
 FreeAndNil(FSW_Losses_FileStream);
 FreeAndNil(FPFLX_FileStream);
 FreeAndNil(FCropFunc_FileStream);
 FreeAndNil(FWaterCost_FileStream);
 FreeAndNil(FDeliveries_FileStream);
 FreeAndNil(FSemiDeliveries_FileStream);
 FreeAndNil(FSemiReturn_FileStream);
 FreeAndNil(FCall_FileStream);
end;

procedure TModflowFmpWriter.WriteFileInternal;
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
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2a);
    WriteDataSet2a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Data set 2b is never used with ModelMuse. Data set 2c is used instead.
    frmProgressMM.AddMessage(StrWritingDataSet2c);
    WriteDataSet2cParamDimen;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cWhenToRead;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cWaterPolicy;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cCropConsumptiveUse;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cSurfaceWater;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cPrintFlags;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cAuxVar;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cMnwOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3And4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet5);
    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Data set 6 is never used. Data set 26 is used instead.
    //    frmProgressMM.AddMessage(StrWritingDataSet6);
    //    WriteDataSet6;
    //    Application.ProcessMessages;
    //    if not frmProgressMM.ShouldContinue then
    //    begin
    //      Exit;
    //    end;

    frmProgressMM.AddMessage(StrWritingDataSet7);
    WriteDataSet7;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet8);
    WriteDataSet8;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet9);
    WriteDataSet9;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Data set 10 is never used. Data set 28 is always used instead.

    frmProgressMM.AddMessage(StrWritingDataSet11);
    WriteDataSet11;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet12);
    WriteDataSet12;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet13);
    WriteDataSet13;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet14);
    WriteDataSet14;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet15);
    WriteDataSet15;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet16);
    WriteDataSet16;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet17);
    WriteDataSet17;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet18);
    WriteDataSet18;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet19);
    WriteDataSet19;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet20);
    WriteDataSet20;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Data sets 21a and 21b are never used.
    // Data sets 37a and 37b are used instead.

    frmProgressMM.AddMessage(StrWritingDataSets);
    WriteDataSets22to39;
  finally
    CloseFile;
  end;
end;

procedure TModflowFmpWriter.CheckIntegerDataSet(IntegerArray: TDataArray;
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
        if {not} IntegerArray.IsValue[0, RowIndex, ColIndex] then
//        begin
//          frmErrorsAndWarnings.AddError(Model, ErrorMessage,
//            Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
//        end
//        else
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


procedure TModflowFmpWriter.EvaluateActiveCells;
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

procedure TModflowFmpWriter.EvaluateAll;
var
  LocalModel: TCustomModel;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    // The following is not included in RemoveErrorAndWarningMessages
    // because the error message would be deleted in Evaluate.
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoCropsHaveBeenD);

    LocalModel := Model as TCustomModel;
    if LocalModel.FmpSoils.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoCropsHaveBeenD, StrNoCropsHaveBeenD);
    end;
    FillFarmList;

    SetFlags;

    EvaluateActiveCells;
    Evaluate;
    EvaluateCropID;
    EvaluateFarmID;
    EvaluateReferenceET;
    EvaluatePrecip;
    EvaluateGwAllotment;
    Model.FmpAllotment.EvaluateAllotment;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

end;

function TModflowFmpWriter.GetObjectString(ErrorObject: TObject): string;
var
  ACrop: TCropItem;
  AClimate: TClimateItem;
  Farm: TFarm;
  ASoil: TSoilItem;
  AScreenObject: TScreenObject;
begin
  if ErrorObject = nil then
  begin
    result := '';
  end
  else if ErrorObject is TSoilItem then
  begin
    ASoil := TSoilItem(ErrorObject);
    result := Format(StrSoilS, [ASoil.SoilName]);
  end
  else if ErrorObject is TCropItem then
  begin
    ACrop := TCropItem(ErrorObject);
    result := Format(StrCropS, [ACrop.CropName]);
  end
  else if ErrorObject is TClimateItem then
  begin
    AClimate := TClimateItem(ErrorObject);
    result := Format(StrClimateStartingTim, [AClimate.StartTime]);
  end
  else if ErrorObject is TFarm then
  begin
    Farm := TFarm(ErrorObject);
    result := Format(StrErrorInFarmD, [Farm.FarmID]);
  end
  else if ErrorObject is TScreenObject then
  begin
    AScreenObject := TScreenObject(ErrorObject);
    result := AScreenObject.Name;
  end
  else
  begin
    Assert(False);
  end;
end;

function TModflowFmpWriter.EvaluateValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string;
  const OKTypes: TRbwDataTypes): TExpression;
var
  Compiler: TRbwParser;
  ErrorFormula: string;
  ObjectString: string;
begin
  Compiler := Model.ParentModel.rpThreeDFormulaCompiler;

  ErrorFormula := Formula;

  try
    Compiler.Compile(Formula)
  except on E: ERbwParserError do
    begin
      ObjectString := GetObjectString(ErrorObject);
      frmFormulaErrors.AddFormulaError(ObjectString,
        DataSetErrorString, ErrorFormula, E.Message);
      Formula := '0';
      Compiler.Compile(Formula);
      // send error message
    end;
  end;
  result := Compiler.CurrentExpression;
  if result = nil then
  begin
    Formula := '0';
    Compiler.Compile(Formula);
    result := Compiler.CurrentExpression;
  end;
  if not (result.ResultType in OKTypes) then
  begin
    ObjectString := GetObjectString(ErrorObject);
    frmFormulaErrors.AddFormulaError(ObjectString,
      DataSetErrorString, ErrorFormula, StrInvalidResultType);
    if rdtInteger in OKTypes then
    begin
      Formula := '0';
    end
    else if rdtBoolean in OKTypes then
    begin
      Formula := 'False';
    end
    else if rdtString in OKTypes then
    begin
      Formula := '""';
    end
    else
    begin
      Assert(False);

    Compiler.Compile(Formula);
    // send error message
    result := Compiler.CurrentExpression;
    end;
  end;

  result.Evaluate;
end;

procedure TModflowFmpWriter.WriteFloatValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string;
  TestProc: TTestRealValueOkProcedure = nil);
var
  Value: double;
  Expression: TExpression;
begin
  Expression := EvaluateValueFromGlobalFormula(Formula, ErrorObject,
    DataSetErrorString, [rdtDouble, rdtInteger]);
  if Expression.ResultType in [rdtDouble, rdtInteger] then
  begin
    Value := Expression.DoubleResult;
    WriteFloatCondensed(Value);
    if Assigned(TestProc) then
    begin
      TestProc(Value);
    end;
  end
  else
  begin
    WriteFloatCondensed(0);
    frmFormulaErrors.AddFormulaError(GetObjectString(ErrorObject), DataSetErrorString,
      Formula, StrTheFormulaShouldReal);
  end;
end;

procedure TModflowFmpWriter.WriteIntegerValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string;
  TestValue: TTestIntValueOkProcedure = nil);
var
  Value: integer;
  Expression: TExpression;
begin
  Expression := EvaluateValueFromGlobalFormula(Formula, ErrorObject,
    DataSetErrorString, [rdtInteger]);
  if Expression.ResultType = rdtInteger then
  begin
    Value := Expression.IntegerResult;
    WriteFreeInteger(Value);
    if Assigned(TestValue) then
    begin
      TestValue(Value);
    end;
  end
  else
  begin
    WriteFreeInteger(0);
    frmFormulaErrors.AddFormulaError(GetObjectString(ErrorObject), DataSetErrorString,
      Formula, StrTheFormulaShouldInt);
  end;
end;

procedure TModflowFmpWriter.WriteNwtOptions;
var
  PSIRAMPF: double;
  SATTHK: Extended;
begin
  if Model.ModflowPackages.NwtPackage.IsSelected then
  begin
    PSIRAMPF := FFarmProcess.PsiRampf;
    SATTHK := FFarmProcess.SatThick;

    WriteString('SPECIFY ');
    WriteFloat(PSIRAMPF);
    WriteFloat(SATTHK);
    WriteString(' # NWT options: PSIRAMPF SATTHK');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.DoBeforeWriteCells;
begin
  inherited;
  WriteNwtOptions;
end;

procedure TModflowFmpWriter.WriteBooleanValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string);
var
  Expression: TExpression;
  Value: Boolean;
begin
  Expression := EvaluateValueFromGlobalFormula(Formula, ErrorObject,
    DataSetErrorString, [rdtBoolean]);

  if Expression.ResultType = rdtBoolean then
  begin
    Value := Expression.BooleanResult;
    if Value then
    begin
      WriteInteger(1);
    end
    else
    begin
      WriteInteger(0);
    end;
  end
  else
  begin
    WriteInteger(0);
    frmFormulaErrors.AddFormulaError(GetObjectString(ErrorObject), DataSetErrorString,
      Formula, StrTheFormulaShouldR);
  end;
end;

procedure TModflowFmpWriter.WriteDataSet9;
var
  SoilIndex: Integer;
  LocalModel: TCustomModel;
  ASoil: TSoilItem;
  SoilID: Integer;
  Formula: string;
begin
  FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
  FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.SoilProp');
  Model.AddModelInputFile(FOpenCloseFileName);
  FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
    fmCreate or fmShareDenyWrite);
  try
    FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

    WriteString('OPEN/CLOSE ');
    WriteString(FOpenCloseFileName);
    WriteString(' # Data Set 9 Soil Properties');
    NewLine;

    FWriteLocation := wlOpenClose;


    LocalModel := Model;
    for SoilIndex := 0 to LocalModel.FmpSoils.Count - 1 do
    begin
      ASoil := LocalModel.FmpSoils[SoilIndex];
      SoilID := SoilIndex + 1;
      WriteInteger(SoilID);

      Formula := ASoil.CapillaryFringe;
      WriteFloatValueFromGlobalFormula(Formula, ASoil, StrCapillaryFringe);

      if FFarmProcess.CropConsumptiveConcept = cccConcept1 then
      begin
        case ASoil.SoilType of
          stSandyLoam:
            WriteString('SANDYLOAM');
          stSilt:
            WriteString('SILT');
          stSiltyClay:
            WriteString('SILTYCLAY');
          stOther:
            begin
              WriteFloatValueFromGlobalFormula(ASoil.ACoeff, ASoil, StrACoeff);
              WriteFloatValueFromGlobalFormula(ASoil.BCoeff, ASoil, StrBCoeff);
              WriteFloatValueFromGlobalFormula(ASoil.CCoeff, ASoil, StrCCoeff);
              WriteFloatValueFromGlobalFormula(ASoil.DCoeff, ASoil, StrDCoeff);
              WriteFloatValueFromGlobalFormula(ASoil.ECoeff, ASoil, StrECoeff);
            end;
          else
            Assert(False);
        end;
      end;
      WriteString(' # Data Set 9: Soil-ID CapFringe');
      if FFarmProcess.CropConsumptiveConcept = cccConcept1 then
      begin
        if ASoil.SoilType = stOther then
        begin
          WriteString(' A-Coeff B-Coeff C-Coeff D-Coeff E-Coeff');
        end
        else
        begin
          WriteString(' Soil-Type');
        end;
      end;
      NewLine;
    end;
  finally
    FWriteLocation := wlMain;
    FreeAndNil(FOpenCloseFileStream);
  end;
end;

//procedure TModflowFmpWriter.WriteDataSet6;
//var
//  DataArray: TDataArray;
//begin
//  DataArray := Model.DataArrayManager.GetDataSetByName(KFarmID);
//  Assert(DataArray <> nil);
//  FOpenCloseFileName := ChangeFileExt(FNameOfFile, '.FID');
//  Model.AddModelInputFile(FOpenCloseFileName);
//  FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
//    fmCreate or fmShareDenyWrite);
//  try
//    FWriteLocation := wlOpenClose;
//    FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);
//    WriteArray(DataArray, 0, 'Data set 6: FID', StrNoValueAssigned);
//  finally
//    FreeAndNil(FOpenCloseFileStream);
//    FWriteLocation := wlMain;
//  end;
//end;

procedure TModflowFmpWriter.WriteDataSet7;
var
  FarmIndex: Integer;
  Efficiencies: TFarmEfficiencyCollection;
  CropIndex: Integer;
  ACropEffCollection: TCropEfficiencyCollection;
  AFarm: TFarm;
  EffItem: TCropEfficiencyItem;
//  Formula: string;
  Crops: TCropCollection;
begin
  if IEFFL = 1 then
  begin
    FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
    FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.OFE');
    Model.AddModelInputFile(FOpenCloseFileName);
    FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
      fmCreate or fmShareDenyWrite);
    try
      FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);

      WriteString('OPEN/CLOSE ');
      WriteString(FOpenCloseFileName);
      WriteString(' # Data Set 7 Farm-ID OFE(FID,CID)');
      NewLine;

      FWriteLocation := wlOpenClose;
      try
        Crops := Model.FmpCrops;
        for FarmIndex := 0 to FFarms.Count - 1 do
        begin
          AFarm := FFarms[FarmIndex];
          WriteFreeInteger(AFarm.FarmId);
          Efficiencies := AFarm.FarmEfficiencyCollection;
          if Efficiencies.Count <> NCROPS then
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrCropEfficiencesNot, Format(StrFarmD, [AFarm.FarmId]));
            Exit;
          end;
          Assert(Efficiencies.Count = NCROPS);
          for CropIndex := 0 to Efficiencies.Count - 1 do
          begin
            ACropEffCollection := Efficiencies[CropIndex].CropEfficiency;
            if ACropEffCollection.Count <> 1 then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrCropEfficiencesNot, Format(StrFarmD, [AFarm.FarmId]));
              Exit;
            end;
            Assert(ACropEffCollection.Count = 1);
            EffItem := ACropEffCollection[0];

            WriteFloatValueFromGlobalFormula(EffItem.Efficiency, AFarm,
              Crops[CropIndex].CropName);
          end;
          NewLine;
        end;
      finally
        FWriteLocation := wlMain;
      end;
    finally
      FreeAndNil(FOpenCloseFileStream);
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet5;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfLandSurface);
  Assert(DataArray <> nil);
  FOpenCloseFileName := ChangeFileExt(FNameOfFile, '');
  FOpenCloseFileName := ChangeFileExt(FOpenCloseFileName, '.GSURF');
  Model.AddModelInputFile(FOpenCloseFileName);
  FOpenCloseFileStream := TFileStream.Create(FOpenCloseFileName,
    fmCreate or fmShareDenyWrite);
  try
    FWriteLocation := wlOpenClose;
    FOpenCloseFileName := ExtractFileName(FOpenCloseFileName);
    WriteArray(DataArray, 0, 'Data set 5: GSURF', StrNoValueAssigned, 'GSURF');
  finally
    FreeAndNil(FOpenCloseFileStream);
    FWriteLocation := wlMain;
  end;

  WritePestZones(DataArray, FInputFileName, StrGSUR);

end;

procedure TModflowFmpWriter.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TFmpWell_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TFmpWell_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'FMP3');
  end;
  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Condfact is set equal to zero, the
  // dummy boundaries have no effect.
  for CellIndex := CellList.Count to NLST - 1 do
  begin
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(0);
    WriteInteger(0);
    WriteFloat(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Farm-Well-ID Farm-ID QMAXfact] [xyz] (Dummy boundary)');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
begin
  inherited;
end;

procedure TModflowFmpWriter.WriteString(const Value: AnsiString);
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
          Assert(FOFE_FileStream <> nil);
          FOFE_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
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
          Assert(FRoot_FileStream <> nil);
          FRoot_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropUse:
        begin
          Assert(FCropUse_FileStream <> nil);
          FCropUse_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlETR:
        begin
          Assert(FETR_FileStream <> nil);
          FETR_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEtFrac:
        begin
          Assert(FET_Frac_FileStream <> nil);
          FET_Frac_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSwLosses:
        begin
          Assert(FSW_Losses_FileStream <> nil);
          FSW_Losses_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPFLX:
        begin
          Assert(FPFLX_FileStream <> nil);
          FPFLX_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropFunc:
        begin
          Assert(FCropFunc_FileStream <> nil);
          FCropFunc_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlWaterCost:
        begin
          Assert(FWaterCost_FileStream <> nil);
          FWaterCost_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDeliveries:
        begin
          Assert(FDeliveries_FileStream <> nil);
          FDeliveries_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteDeliv:
        begin
          Assert(FSemiDeliveries_FileStream <> nil);
          FSemiDeliveries_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteReturn:
        begin
          Assert(FSemiReturn_FileStream <> nil);
          FSemiReturn_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCall:
        begin
          Assert(FCall_FileStream <> nil);
          FCall_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowFmpWriter.WriteU2DINTHeader(const Comment: string;
  ArrayType: TModflowArrayType; const MF2015_ArrayName: string);
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose:
      begin
        FWriteLocation := wlMain;
        try
          WriteString( 'OPEN/CLOSE ');
          WriteString(FOpenCloseFileName);
          WriteString( ' 1 (FREE)   ');
          WriteInteger(IPRN_Integer);
          WriteString( ' # ' + Comment);
          NewLine;
        finally
          FWriteLocation := wlOpenClose;
        end;
      end;
    wlCID:
      begin
        FWriteLocation := wlMain;
        try
          WriteString( 'EXTERNAL ');
          WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpCID));
          WriteString( ' 1 (FREE)   ');
          WriteInteger(IPRN_Integer);
          WriteString( ' # ' + Comment);
          NewLine;
        finally
          FWriteLocation := wlCID;
        end;
      end;
    wlFID:
      begin
        FWriteLocation := wlMain;
        try
          WriteString( 'EXTERNAL ');
          WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpFID));
          WriteString( ' 1 (FREE)   ');
          WriteInteger(IPRN_Integer);
          WriteString( ' # ' + Comment);
          NewLine;
        finally
          FWriteLocation := wlFID;
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TModflowFmpWriter.WriteU2DRELHeader(const Comment: string;
  ArrayType: TModflowArrayType; const MF6_ArrayName: string);
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose:
      begin
        FWriteLocation := wlMain;
        try
          WriteString( 'OPEN/CLOSE ');
          WriteString(FOpenCloseFileName);
          WriteString( ' 1.0 (FREE)   ');
          WriteInteger(IPRN_Real);
          WriteString( ' # ' + Comment);
          NewLine;
        finally
          FWriteLocation := wlOpenClose;
        end;
      end;
    wlETR:
      begin
        FWriteLocation := wlMain;
        try
          WriteString( 'EXTERNAL ');
          WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpETR));
          WriteString( ' 1 (FREE)   ');
          WriteInteger(IPRN_Integer);
          WriteString( ' # ' + Comment);
          NewLine;
        finally
          FWriteLocation := wlETR;
        end;
      end;
    wlPFLX:
      begin
        FWriteLocation := wlMain;
        try
          WriteString( 'EXTERNAL ');
          WriteInteger(Model.UnitNumbers.UnitNumber(StrFmpPFLX));
          WriteString( ' 1 (FREE)   ');
          WriteInteger(IPRN_Integer);
          WriteString( ' # ' + Comment);
          NewLine;
        finally
          FWriteLocation := wlPFLX;
        end;
      end;
    else
      Assert(False);
  end;
end;

end.
