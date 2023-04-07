unit frmCropPropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, JvExControls,
  JvPageList, ExtCtrls, JvExExtCtrls, JvNetscapeSplitter, ComCtrls,
  JvExComCtrls, JvPageListTreeView, StdCtrls, Buttons, frameGridUnit,
  PhastModelUnit, ModflowFmpCropUnit, RbwDataGrid4, GoPhastTypes, RbwParser,
  ModflowPackageSelectionUnit, UndoItems, frameFormulaGridUnit,
  ModflowFmpFarmUnit, ModflowFmpBaseClasses, Vcl.Grids;

type
  TUndoCrops = class(TCustomUndo)
  private
    FOldCrops: TCropCollection;
    FNewCrops: TCropCollection;
    FFarmList: TFarmObjectList;
//    FFarmScreenObjects: TScreenObjectList;
  protected
    function Description: string; override;
  public
    constructor Create(var NewCrops: TCropCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

  TfrmCropProperties = class(TfrmCustomGoPhast)
    jvpltvMain: TJvPageListTreeView;
    splitterMain: TJvNetscapeSplitter;
    jplMain: TJvPageList;
    jvspCropName: TJvStandardPage;
    jvspEvapFractions: TJvStandardPage;
    jvspLosses: TJvStandardPage;
    jvspCropFunction: TJvStandardPage;
    jvspCropWaterUse: TJvStandardPage;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    frameCropName: TFrameFormulaGrid;
    frameCropFunction: TFrameFormulaGrid;
    frameCropWaterUse: TFrameFormulaGrid;
    frameEvapFractions: TFrameFormulaGrid;
    frameLosses: TFrameFormulaGrid;
    rbwprsrGlobal: TRbwParser;
    jvspIrrigation: TJvStandardPage;
    frameIrrigation: TframeFormulaGrid;
    jvspLandUseFraction: TJvStandardPage;
    frameLandUseFraction: TframeFormulaGrid;
    jvspRootPressure: TJvStandardPage;
    frameRootPressure: TframeFormulaGrid;
    jvspGwRootInteraction: TJvStandardPage;
    rdgGwRootInteraction: TRbwDataGrid4;
    procedure FormDestroy(Sender: TObject); override;
    procedure FormCreate(Sender: TObject); override;
    procedure jvpltvMainChange(Sender: TObject; Node: TTreeNode);
    procedure frameCropNameseNumberChange(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameCropNameGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure GridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameCropWaterUseGridEndUpdate(Sender: TObject);
    procedure frameCropFunctionGridEndUpdate(Sender: TObject);
    procedure frameEvapFractionsGridEndUpdate(Sender: TObject);
    procedure frameLossesGridEndUpdate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chklstGwRootInteractionClickCheck(Sender: TObject);
    procedure chklstGwRootInteractionExit(Sender: TObject);
    procedure frameCropNamesbAddClick(Sender: TObject);
    procedure frameCropNamesbInsertClick(Sender: TObject);
    procedure frameCropNamesbDeleteClick(Sender: TObject);
    procedure jvpltvMainCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure jplMainChange(Sender: TObject);
    procedure frameCropWaterUseGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameCropNameGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameIrrigationGridEndUpdate(Sender: TObject);
    procedure frameLandUseFractionGridEndUpdate(Sender: TObject);
    procedure frameRootPressureGridEndUpdate(Sender: TObject);
    procedure jvspGwRootInteractionShow(Sender: TObject);
    procedure rdgGwRootInteractionExit(Sender: TObject);
    procedure rdgGwRootInteractionStateChange(Sender: TObject; ACol, ARow: Integer;
        const Value: TCheckBoxState);
  private
    FNameStart: integer;
    FPressureStart: integer;
    FCropPropStart: Integer;
    FFallowStart: integer;
    FLastCol: integer;
    FCrops: TCropCollection;
    FCropsNode: TJvPageIndexNode;
    FFarmProcess: TFarmProcess;
    FEvapFract: TEvapFractionsCollection;
    FLosses: TLossesCollection;
    FCropFunctions: TCropFunctionCollection;
    FWaterUseCollection: TCropWaterUseCollection;
    FGettingData: Boolean;
    FIrrigation: TIrrigationCollection;
    FFarmProcess4: TFarmProcess4;
    FFarmLandUse: TFarmProcess4LandUse;
    FLandUseFraction: TOwhmCollection;
    FRootPressure: TRootPressureCollection;
    FGroundwaterRootInteraction: TGroundwaterRootInteraction;
    FSettingGwInteraction: Boolean;
    procedure SetUpCropNameTable(Model: TCustomModel);
    procedure SetCropNameTableColumns(Model: TCustomModel);
    procedure GetCrops(CropCollection: TCropCollection);
    procedure SetUpEvapFractionsTable(Model: TCustomModel);
    procedure GetEvapFractions(EvapFract: TEvapFractionsCollection);
    procedure SetUpLossesTable(Model: TCustomModel);
    procedure GetLosses(Losses: TLossesCollection);
    procedure SetUpCropFunctionTable(Model: TCustomModel);
    procedure GetCropFunction(CropFunctions: TCropFunctionCollection);
    procedure SetUpCropWaterUseTable(Model: TCustomModel);
    procedure GetCropWaterUseFunction(WaterUseCollection: TCropWaterUseCollection);
    procedure SetUpIrrigationTable(Model: TCustomModel);
    procedure GetIrrigation(Irrigation: TIrrigationCollection);
    procedure SetUpLandUseFractionTable(Model: TCustomModel);
    procedure GetLandUseFraction(LandUseFraction: TOwhmCollection);
    procedure SetUpRootPressureTable(Model: TCustomModel);
    procedure GetRootPressure(RootPressure: TRootPressureCollection);
    procedure SetUpGroundwaterRootInteractionTable(Model: TCustomModel);
    procedure GetGroundwaterRootInteraction(
      GroundwaterRootInteraction: TGroundwaterRootInteraction);
    procedure GetData;
    procedure SetGridColumnProperties(Grid: TRbwDataGrid4);
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    procedure SetUseButton(Grid: TRbwDataGrid4; StartCol: Integer);
    procedure CreateChildNodes(ACrop: TCropItem; CropNode: TJvPageIndexNode);
    procedure SetStartAndEndTimeLists(StartTimes, EndTimes: TStringList;
      Grid: TRbwDataGrid4);
    procedure GetGlobalVariables;
    procedure SetData;
    procedure AssignGWRootInteractionCheckboxes(Code: TInteractionCode);
    function CheckBoxesToGwRootInteractionCode: TInteractionCode;
    { Private declarations }

  public
    { Public declarations }
  end;

var
  frmCropProperties: TfrmCropProperties;

implementation

uses
  frmGoPhastUnit, frmConvertChoiceUnit, frmFormulaUnit, ModflowTimeUnit;

resourcestring
  StrRootingDepthRoot = 'Rooting depth (ROOT)';
  StrTranspiratoryFracti = 'Transpiratory fraction (FTR)';
  StrEvaporativeFraction = 'Evaporative fraction related to precipitation (FEP)';
  StrIrrigFraction = 'Evaporative fraction related to irrigation (FEI)';
  StrCropConsumptiveUse = 'Crop consumptive use flux (CU)';
  StrConsumptiveUseFlux = 'Consumptive Use Flux';
  StrCropCoefficient = 'Crop Coefficient (CU)';
  StrErrorInFormulaS = 'Error in formula: %s';
  StrPrecipitationrelate = 'Precipitation-related loss fraction (FIESWP)';
  StrIrrigationrelatedL = 'Irrigation-related loss fraction (FIESWI)';
  StrSlopeWPFSlope = 'Slope (WPF-Slope)';
  StrInterceptWPFInt = 'Intercept (WPF-Int)';
  StrPriceCropPrice = 'Price (Crop-Price)';
  StrIrrigatedInverseO15 = 'Irrigated (inverse of NONIRR) Data Set 15';
  StrIrrigatedInverseO30 = 'Irrigated (inverse of NONIRR) Data Set 30a';
  StrCropIDCID = 'Crop ID (CID)';
  StrCropName = 'Crop Name';
  StrPSI1 = 'PSI(1) Anoxia';
  StrPSI2 = 'PSI(2) Optimal';
  StrPSI3 = 'PSI(3) Optimal';
  StrPSI4 = 'PSI(4) Wilting';
  StrBaseTemperatureBa = 'Base Temperature (BaseT)';
  StrMinimumCutoffTempe = 'Minimum Cutoff Temperature (MinCutT)';
  StrMaximumCutoffTempe = 'Maximum Cutoff Temperature (MaxCutT)';
  StrCoefficient0C0 = 'Coefficient 0 (C0)';
  StrCoefficient1C1 = 'Coefficient 1 (C1)';
  StrCoefficient2C2 = 'Coefficient 2 (C2)';
  StrCoefficient3C3 = 'Coefficient 3 (C3)';
  StrBeginningRootDepth = 'Beginning Root Depth (BegRootD)';
  StrMaximumRootDepth = 'Maximum Root Depth (MaxRootD)';
  StrRootGrowthCoeffici = 'Root Growth Coefficient (RootGC)';
  StrFallowIFALLOW = 'Can be fallow (IFALLOW)';
  StrRootingDepth = 'Rooting Depth';
  StrConsumptiveUse = 'Consumptive Use Factors';
  StrInefficiencylosses = 'Inefficiency-Losses to Surface Water';
  StrCropPriceFunction = 'Crop Price Function';
  StrChangeFarmCrops = 'change farm crops';
  StrCrops = 'Crops';
  StrIrrigationTypeIRR = 'Irrigation type (IRRIGATION)';
  StrIrrigation = 'Irrigation';
  StrEvaporationIrrigati = 'Evaporation Irrigation Fraction';
  StrSurfaceWaterLossF = 'Surface Water Loss Fraction Irrigation';
  StrLandUseAreaFracti = 'Land Use Area Fraction';
  StrCropCoefficient2 = 'Crop Coefficient';
  StrConsumptiveUse2 = 'Consumptive Use';
  StrTranspirationFracti = 'Transpiration Fraction';
  StrGroundwaterRootInt = 'Groundwater Root Interaction';
  StrRootPressure = 'Root Pressure';
  StrSurfaceWaterLossFPrecip = 'Surface Water Loss Fraction Precipitation';
  StrPondDepth = 'Pond Depth';

type
  TNameCol = (ncID, ncName);
  TPressureCol = (pc1, pc2, pc3, pc4);
  TCropProp = (cpBaseTemperature, cpMinimumCutoffTemperature,
    cpMaximumCutoffTemperature, cpCoefficient0, cpCoefficient1,
    cpCoefficient2, cpCoefficient3, cpBeginningRootDepth, cpMaximumRootDepth,
    cpRootGrowthCoefficient, cpIrrigated);
  TFallowCol = (fcFallow);

  TRootDepthColumns = (rdcStart, rdcEnd, rdcRootingDepth);
  TEvapFracColumns = (efcStart, efcEnd, efcTransp, efcPrecip, efcIrrig);
  TLossesColumns = (lcStart, lcEnd, lcPrecip, lcIrrig);
  TCropFunctionColumns = (cfcStart, cfcEnd, cfcSlope, cfcIntercept, cfcPrice);
  TCropWaterUse = (cwuStart, cwuEnd, cwuCropValue, cwuIrrigated);
  TIrrigationColumns = (icStart, icEnd, IcIrrigation, icEvapIrrigateFraction,
    icSWLossFracIrrigate);
  TOwhmColumns = (ocStart, ocEnd, ocFormula);
  TPsiColumns = (pcStart, pcEnd, pcPsi1, pcPsi2, pcPsi3, pcPsi4);
  TGwRootInteractionIndicies = (griHasTranspiration, griGroundwater,
    griAnoxia, griStress);

{$R *.dfm}

{ TfrmCropProperties }

procedure TfrmCropProperties.SetUpCropFunctionTable(Model: TCustomModel);
begin
  frameCropFunction.Grid.ColCount := 5;
  frameCropFunction.Grid.FixedCols := 0;
  frameCropFunction.Grid.Columns[Ord(cfcStart)].Format := rcf4Real;
  frameCropFunction.Grid.Columns[Ord(cfcEnd)].Format := rcf4Real;
  frameCropFunction.Grid.Cells[Ord(cfcStart), 0] := StrStartingTime;
  frameCropFunction.Grid.Cells[Ord(cfcEnd), 0] := StrEndingTime;
  frameCropFunction.Grid.Cells[Ord(cfcSlope), 0] := StrSlopeWPFSlope;
  frameCropFunction.Grid.Cells[Ord(cfcIntercept), 0] := StrInterceptWPFInt;
  frameCropFunction.Grid.Cells[Ord(cfcPrice), 0] := StrPriceCropPrice;
  SetGridColumnProperties(frameCropFunction.Grid);
  SetUseButton(frameCropFunction.Grid, Ord(cfcSlope));
  frameCropFunction.FirstFormulaColumn := 2;
  frameCropFunction.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpCropNameTable(Model: TCustomModel);
var
  index: Integer;
begin
  SetCropNameTableColumns(Model);
  frameCropName.Grid.ColCount := FLastCol + 1;
  for index := 0 to frameCropName.Grid.ColCount - 1 do
  begin
    frameCropName.Grid.Columns[index].WordWrapCaptions := True;
  end;
  frameCropName.Grid.FixedCols := 1;
  frameCropName.Grid.Cells[Ord(ncID), 0] := StrCropIDCID;
  frameCropName.Grid.Cells[Ord(ncName), 0] := StrCropName;
  if FFallowStart >= 0 then
  begin
    frameCropName.Grid.Cells[FFallowStart + Ord(fcFallow), 0]
      := StrFallowIFALLOW;
  end;
  if FPressureStart >= 0 then
  begin
    frameCropName.Grid.Cells[FPressureStart + Ord(pc1), 0] := StrPSI1;
    frameCropName.Grid.Cells[FPressureStart + Ord(pc2), 0] := StrPSI2;
    frameCropName.Grid.Cells[FPressureStart + Ord(pc3), 0] := StrPSI3;
    frameCropName.Grid.Cells[FPressureStart + Ord(pc4), 0] := StrPSI4;
  end;
  if FCropPropStart >= 0 then
  begin
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpBaseTemperature), 0]
      := StrBaseTemperatureBa;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpMinimumCutoffTemperature), 0]
      := StrMinimumCutoffTempe;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpMaximumCutoffTemperature), 0]
      := StrMaximumCutoffTempe;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient0), 0]
      := StrCoefficient0C0;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient1), 0]
      := StrCoefficient1C1;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient2), 0]
      := StrCoefficient2C2;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient3), 0]
      := StrCoefficient3C3;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpBeginningRootDepth), 0]
      := StrBeginningRootDepth;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpMaximumRootDepth), 0]
      := StrMaximumRootDepth;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpRootGrowthCoefficient), 0]
      := StrRootGrowthCoeffici;
    frameCropName.Grid.Cells[FCropPropStart + Ord(cpIrrigated), 0]
      := StrIrrigatedInverseO15;
  end;

  SetGridColumnProperties(frameCropName.Grid);
  SetUseButton(frameCropName.Grid, Ord(ncName) + 1);
  frameCropName.FirstFormulaColumn := 2;
  frameCropName.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpCropWaterUseTable(Model: TCustomModel);
begin
//  TCropWaterUse = (cwuStart, cwuEnd, cwuCropValue, cwuIrrigated);
  frameCropWaterUse.Grid.ColCount := 4;
  frameCropWaterUse.Grid.FixedCols := 0;
  frameCropWaterUse.Grid.Columns[Ord(cwuStart)].Format := rcf4Real;
  frameCropWaterUse.Grid.Columns[Ord(cwuEnd)].Format := rcf4Real;
  frameCropWaterUse.Grid.Cells[Ord(cwuStart), 0] := StrStartingTime;
  frameCropWaterUse.Grid.Cells[Ord(cwuEnd), 0] := StrEndingTime;
  case Model.ModflowPackages.FarmProcess.ConsumptiveUse of
    cuCalculated: ; // do nothing
    cuPotentialET, cuPotentialAndReferenceET:
      frameCropWaterUse.Grid.Cells[Ord(cwuCropValue), 0] := StrCropConsumptiveUse;
    cuCropCoefficient:
      frameCropWaterUse.Grid.Cells[Ord(cwuCropValue), 0] := StrCropCoefficient;
    else
      Assert(False);
  end;
  frameCropWaterUse.Grid.Cells[Ord(cwuIrrigated), 0] := StrIrrigatedInverseO30;
  SetGridColumnProperties(frameCropWaterUse.Grid);
  SetUseButton(frameCropWaterUse.Grid, Ord(cwuCropValue));
  frameCropWaterUse.FirstFormulaColumn := 2;
  frameCropWaterUse.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpEvapFractionsTable(Model: TCustomModel);
begin
  frameEvapFractions.Grid.ColCount := 5;
  frameEvapFractions.Grid.FixedCols := 0;
  frameEvapFractions.Grid.Columns[Ord(efcStart)].Format := rcf4Real;
  frameEvapFractions.Grid.Columns[Ord(efcEnd)].Format := rcf4Real;
  frameEvapFractions.Grid.Cells[Ord(efcStart), 0] := StrStartingTime;
  frameEvapFractions.Grid.Cells[Ord(efcEnd), 0] := StrEndingTime;
  frameEvapFractions.Grid.Cells[Ord(efcTransp), 0] := StrTranspiratoryFracti;
  frameEvapFractions.Grid.Cells[Ord(efcPrecip), 0] := StrEvaporativeFraction;
  frameEvapFractions.Grid.Cells[Ord(efcIrrig), 0] := StrIrrigFraction;
  SetGridColumnProperties(frameEvapFractions.Grid);
  SetUseButton(frameEvapFractions.Grid, Ord(efcTransp));
  frameEvapFractions.FirstFormulaColumn := 2;
  frameEvapFractions.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpGroundwaterRootInteractionTable(
  Model: TCustomModel);
begin
  rdgGwRootInteraction.Cells[0, Ord(griHasTranspiration)] := 'Has Transpiration';
  rdgGwRootInteraction.Cells[0, Ord(griGroundwater)] := 'Has groundwater uptake';
  rdgGwRootInteraction.Cells[0, Ord(griAnoxia)] := 'Anoxia: Consumptive use reduced from any anoxia';
  rdgGwRootInteraction.Cells[0, Ord(griStress)] := 'Water Stress: Consumptive use reduced from any water stress';
  rdgGwRootInteraction.RowCount := 5;
  rdgGwRootInteraction.Row := 4;
  rdgGwRootInteraction.RowCount := 4;
end;

procedure TfrmCropProperties.SetUpIrrigationTable(Model: TCustomModel);
begin
  frameIrrigation.Grid.ColCount := 5;
  frameIrrigation.Grid.FixedCols := 0;
  frameIrrigation.Grid.Columns[Ord(icStart)].Format := rcf4Real;
  frameIrrigation.Grid.Columns[Ord(icEnd)].Format := rcf4Real;
  frameIrrigation.Grid.Cells[Ord(icStart), 0] := StrStartingTime;
  frameIrrigation.Grid.Cells[Ord(icEnd), 0] := StrEndingTime;
  frameIrrigation.Grid.Cells[Ord(icIrrigation), 0] := StrIrrigationTypeIRR;
  frameIrrigation.Grid.Cells[Ord(icEvapIrrigateFraction), 0] := StrEvaporationIrrigati;
  frameIrrigation.Grid.Cells[Ord(icSWLossFracIrrigate), 0] := StrSurfaceWaterLossF;

  SetGridColumnProperties(frameIrrigation.Grid);
  SetUseButton(frameIrrigation.Grid, Ord(icIrrigation));
  frameIrrigation.FirstFormulaColumn := 2;
  frameIrrigation.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpLandUseFractionTable(Model: TCustomModel);
begin

  frameLandUseFraction.Grid.ColCount := 3;
  frameLandUseFraction.Grid.FixedCols := 0;
  frameLandUseFraction.Grid.Columns[Ord(ocStart)].Format := rcf4Real;
  frameLandUseFraction.Grid.Columns[Ord(ocEnd)].Format := rcf4Real;
  frameLandUseFraction.Grid.Cells[Ord(ocStart), 0] := StrStartingTime;
  frameLandUseFraction.Grid.Cells[Ord(ocEnd), 0] := StrEndingTime;
  frameLandUseFraction.Grid.Cells[Ord(ocFormula), 0] := 'Land Use Fraction';

  SetGridColumnProperties(frameLandUseFraction.Grid);
  SetUseButton(frameLandUseFraction.Grid, Ord(ocFormula));
  frameLandUseFraction.FirstFormulaColumn := 2;
  frameLandUseFraction.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpLossesTable(Model: TCustomModel);
begin
  frameLosses.Grid.ColCount := 4;
  frameLosses.Grid.FixedCols := 0;
  frameLosses.Grid.Columns[Ord(lcStart)].Format := rcf4Real;
  frameLosses.Grid.Columns[Ord(lcEnd)].Format := rcf4Real;
  frameLosses.Grid.Cells[Ord(lcStart), 0] := StrStartingTime;
  frameLosses.Grid.Cells[Ord(lcEnd), 0] := StrEndingTime;
  frameLosses.Grid.Cells[Ord(lcPrecip), 0] := StrPrecipitationrelate;
  frameLosses.Grid.Cells[Ord(lcIrrig), 0] := StrIrrigationrelatedL;
  SetGridColumnProperties(frameLosses.Grid);
  SetUseButton(frameLosses.Grid, Ord(lcPrecip));
  frameLosses.FirstFormulaColumn := 2;
  frameLosses.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.SetUpRootPressureTable(Model: TCustomModel);
begin
  frameRootPressure.Grid.ColCount := 6;
  frameRootPressure.Grid.FixedCols := 0;
  frameRootPressure.Grid.Columns[Ord(pcStart)].Format := rcf4Real;
  frameRootPressure.Grid.Columns[Ord(pcEnd)].Format := rcf4Real;
  frameRootPressure.Grid.Cells[Ord(pcStart), 0] := StrStartingTime;
  frameRootPressure.Grid.Cells[Ord(pcEnd), 0] := StrEndingTime;
  frameRootPressure.Grid.Cells[Ord(pcPsi1), 0] := 'Anoxia Pressure';
  frameRootPressure.Grid.Cells[Ord(pcPsi2), 0] := 'High Optimal Pressure';
  frameRootPressure.Grid.Cells[Ord(pcPsi3), 0] := 'Low Optimal Pressure';
  frameRootPressure.Grid.Cells[Ord(pcPsi4), 0] := 'Wilting Pressure';

  SetGridColumnProperties(frameRootPressure.Grid);
  SetUseButton(frameRootPressure.Grid, Ord(pcPsi1));
  frameRootPressure.FirstFormulaColumn := 2;
  frameRootPressure.LayoutMultiRowEditControls;
end;

procedure TfrmCropProperties.btnOKClick(Sender: TObject);
var
  index: Integer;
begin
  // This is to ensure that the order of the crops does not change.
  Assert(FCrops <> nil);
  for index := 0 to FCrops.Count - 1 do
  begin
    FCrops[index].StartTime := index;
  end;
  SetData;
  inherited;
end;

procedure TfrmCropProperties.chklstGwRootInteractionExit(Sender: TObject);
var
  Code: TInteractionCode;
begin
  inherited;
  Code := CheckBoxesToGwRootInteractionCode;

  FGroundwaterRootInteraction.InteractionCode := Code;


end;

procedure TfrmCropProperties.CreateBoundaryFormula(
  const DataGrid: TRbwDataGrid4; const ACol, ARow: integer; Formula: string;
  const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
var
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
begin
  // CreateBoundaryFormula creates an Expression for a boundary condition
  // based on the text in DataGrid at ACol, ARow. Orientation, and EvaluatedAt
  // are used to chose the TRbwParser.
  TempCompiler := rbwprsrGlobal;
  try
    TempCompiler.Compile(Formula);

  except on E: ERbwParserError do
    begin
      Beep;
      raise ERbwParserError.Create(Format(StrErrorInFormulaS,
        [E.Message]));
      Exit;
    end
  end;
  CompiledFormula := TempCompiler.CurrentExpression;

  ResultType := rdtDouble;
  if (DataGrid = frameCropWaterUse.Grid)then
  begin
    if ACol = Ord(cwuIrrigated) then
    begin
      ResultType := rdtBoolean;
    end
  end
  else if (DataGrid = frameIrrigation.Grid)then
  begin
    if ACol = Ord(IcIrrigation) then
    begin
      ResultType := rdtInteger;
    end
  end
  else if (DataGrid = frameCropName.Grid)then
  begin
    if (FFallowStart > 0) and (ACol = FFallowStart + Ord(fcFallow)) then
    begin
      ResultType := rdtBoolean;
    end;
    if (FCropPropStart > 0) and (ACol = FCropPropStart + Ord(cpIrrigated)) then
    begin
      ResultType := rdtBoolean;
    end;
  end;

  if (ResultType = CompiledFormula.ResultType) or
    ((ResultType = rdtDouble) and (CompiledFormula.ResultType = rdtInteger))
      then
  begin
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end
  else
  begin
    Formula := AdjustFormula(Formula, CompiledFormula.ResultType, ResultType);
    TempCompiler.Compile(Formula);
    CompiledFormula := TempCompiler.CurrentExpression;
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end;
  if Assigned(DataGrid.OnSetEditText) then
  begin
    DataGrid.OnSetEditText(DataGrid, ACol, ARow, DataGrid.Cells[ACol, ARow]);
  end;
end;

procedure TfrmCropProperties.SetUseButton(Grid: TRbwDataGrid4; StartCol: Integer);
var
  ColIndex: Integer;
begin
  for ColIndex := StartCol to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].ButtonUsed := True;
    Grid.Columns[ColIndex].ButtonCaption := StrFormulaButtonCaption;
    Grid.Columns[ColIndex].ButtonWidth := 35;
  end;
end;

procedure TfrmCropProperties.CreateChildNodes(ACrop: TCropItem; CropNode: TJvPageIndexNode);
var
  ANode: TJvPageIndexNode;
begin
  if frmGoPhast.ModelSelection = msModflowFmp then
  begin
    if FFarmProcess.RootingDepth = rdSpecified then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrRootingDepth) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.FmpRootDepthCollection;
    end;
  end
  else
  begin
    if FFarmLandUse.RootDepth.ListUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, 'Root Depth') as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.FmpRootDepthCollection;
    end;
  end;

  ANode := jvpltvMain.Items.AddChild(CropNode, StrConsumptiveUse) as TJvPageIndexNode;
  ANode.PageIndex := jvspEvapFractions.PageIndex;
  ANode.Data := ACrop.EvapFractionsCollection;

  if FFarmProcess.FractionOfInefficiencyLosses = filSpecified then
  begin
    ANode := jvpltvMain.Items.AddChild(CropNode, StrInefficiencylosses) as TJvPageIndexNode;
    ANode.PageIndex := jvspLosses.PageIndex;
    ANode.Data := ACrop.LossesCollection;
  end;

  if FFarmProcess.DeficiencyPolicy in
    [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool] then
  begin
    ANode := jvpltvMain.Items.AddChild(CropNode, StrCropPriceFunction) as TJvPageIndexNode;
    ANode.PageIndex := jvspCropFunction.PageIndex;
    ANode.Data := ACrop.CropFunctionCollection;
  end;

  if FFarmProcess.ConsumptiveUse in
    [cuPotentialET, cuPotentialAndReferenceET, cuCropCoefficient] then
  begin
    case frmGoPhast.PhastModel.ModflowPackages.FarmProcess.ConsumptiveUse of
      cuPotentialET, cuPotentialAndReferenceET:
        ANode := jvpltvMain.Items.AddChild(CropNode, StrConsumptiveUseFlux) as TJvPageIndexNode;
      cuCropCoefficient:
        ANode := jvpltvMain.Items.AddChild(CropNode, StrCropCoefficient) as TJvPageIndexNode;
    else
      Assert(False);
    end;
    ANode.PageIndex := jvspCropWaterUse.PageIndex;
    ANode.Data := ACrop.CropWaterUseCollection;
  end;

{$IFDEF OWHMV2}
  if (frmGoPhast.ModelSelection = msModflowOwhm2)
      and FFarmProcess4.IsSelected and FFarmLandUse.IsSelected then
  begin
    if (FFarmLandUse.IrrigationListUsed
        or FFarmLandUse.EvapIrrigateFractionListByCropUsed
        or FFarmLandUse.SwLossFracIrrigListByCropUsed) then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrSurfaceWaterLossF) as TJvPageIndexNode;
      ANode.PageIndex := jvspIrrigation.PageIndex;
      ANode.Data := ACrop.IrrigationCollection;
    end;

    if FFarmLandUse.LandUseFraction.ListUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrLandUseAreaFracti) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.LandUseFractionCollection;
    end;

    if FFarmLandUse.CropCoeff.ListUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrCropCoefficient2) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.CropCoefficientCollection;
    end;

    if FFarmLandUse.ConsumptiveUse.ListUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrConsumptiveUse2) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.ConsumptiveUseCollection;
    end;

    if FFarmLandUse.RootPressure.FarmOption <> foNotUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrRootPressure) as TJvPageIndexNode;
      ANode.PageIndex := jvspRootPressure.PageIndex;
      ANode.Data := ACrop.RootPressureCollection;
    end;

    if FFarmLandUse.GroundwaterRootInteraction.FarmOption <> foNotUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrGroundwaterRootInt) as TJvPageIndexNode;
      ANode.PageIndex := jvspGwRootInteraction.PageIndex;
      ANode.Data := ACrop.GroundwaterRootInteraction;
    end;

    if FFarmLandUse.TranspirationFraction.ListUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrTranspirationFracti) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.TranspirationFractionCollection;
    end;

    if FFarmLandUse.FractionOfPrecipToSurfaceWater.ListUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrSurfaceWaterLossFPrecip) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.SWLossFractionPrecipCollection;
    end;

    if FFarmLandUse.PondDepth.FarmOption <> foNotUsed then
    begin
      ANode := jvpltvMain.Items.AddChild(CropNode, StrPondDepth) as TJvPageIndexNode;
      ANode.PageIndex := jvspLandUseFraction.PageIndex;
      ANode.Data := ACrop.PondDepthCollection;
    end;

    if FFarmLandUse.AddedDemand.ListUsed then
    begin
//      ANode := jvpltvMain.Items.AddChild(CropNode, 'Added Demand') as TJvPageIndexNode;
//      ANode.PageIndex := jvspLandUseFraction.PageIndex;
//      ANode.Data := ACrop.AddedDemandCollection;
//      if FFarmLandUse.AddedDemandOption = doLength then
//      begin
//        ACrop.AddedDemandCollection.OwhmNames[0] := 'Added Demand [L]'
//      end
//      else
//      begin
//        Assert(FFarmLandUse.AddedDemandOption = doRate);
//        ACrop.AddedDemandCollection.OwhmNames[0] := 'Added Demand [L^3/T]'
//      end;
    end;
  end;
{$ENDIF}

end;

procedure TfrmCropProperties.FormCreate(Sender: TObject);
begin
  inherited;
  jplMain.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmCropProperties.FormDestroy(Sender: TObject);
begin
  inherited;
  FCrops.Free;
end;

procedure TfrmCropProperties.GridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  Grid: TRbwDataGrid4;
  ItemIndex: Integer;
begin
  inherited;
  if (ACol = 0) and (ARow >= 1) then
  begin
    Grid := Sender as TRbwDataGrid4;
    if (Grid.Cells[ACol+1, ARow] = '') then
    begin
      ItemIndex := Grid.ItemIndex[ACol, ARow];
      if ItemIndex >= 0 then
      begin
        Grid.ItemIndex[ACol+1, ARow] := ItemIndex;
      end;
    end;
  end;
end;

procedure TfrmCropProperties.frameCropFunctionGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TCropFunctionItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  frameCropFunction.GridEndUpdate(Sender);
  if FCropFunctions <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to frameCropFunction.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameCropFunction.Grid.Cells[
        Ord(cfcStart), RowIndex], StartTime)
        and TryStrToFloat(frameCropFunction.Grid.Cells[
        Ord(cfcEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FCropFunctions.Count then
        begin
          FCropFunctions.Add;
        end;
        AnItem := FCropFunctions[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.Slope := frameCropFunction.Grid.Cells[Ord(cfcSlope), RowIndex];
        AnItem.Intercept := frameCropFunction.Grid.Cells[Ord(cfcIntercept), RowIndex];
        AnItem.Price := frameCropFunction.Grid.Cells[Ord(cfcPrice), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FCropFunctions.Count > ItemCount do
    begin
      FCropFunctions.Last.Free;
    end;
  end;
end;

procedure TfrmCropProperties.GridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  Orientation: TDataSetOrientation;
  DataGrid: TRbwDataGrid4;
  EvaluatedAt: TEvaluatedAt;
  NewValue: string;
begin
  inherited;
  DataGrid := Sender as TRbwDataGrid4;
  // Lakes and reservoirs can only be specified from the top.
  Orientation := dsoTop;
  // All the MODFLOW boundary conditions are evaluated at blocks.
  EvaluatedAt := eaBlocks;

  NewValue := DataGrid.Cells[ACol, ARow];
  if (NewValue = '') then
  begin
    NewValue := '0';
  end;

//  with TfrmFormula.Create(self) do
  with frmFormula do
  begin
    try
      Initialize;
      // GIS functions are not included and
      // Data sets are not included
      // because the variables will be evaluated for screen objects and
      // not at specific locations.

      PopupParent := self;

      // Show the functions and global variables.
      IncludeTimeSeries := False;
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        CreateBoundaryFormula(DataGrid, ACol, ARow, Formula, Orientation,
          EvaluatedAt);
        if Assigned(DataGrid.OnEndUpdate) then
        begin
          DataGrid.OnEndUpdate(nil);
        end;
      end;
    finally
      Initialize;
//      Free;
    end;
  end;
end;

procedure TfrmCropProperties.frameCropNameGridBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  AFormula: string;
  Compiler: TRbwParser;
begin
  inherited;
  if (ARow >= 1) and (ACol = FCropPropStart + Ord(cpIrrigated)) then
  begin
    frameCropName.Grid.Canvas.Brush.Color := clWindow;
    AFormula := frameCropName.Grid.Cells[ACol, ARow];
    Compiler := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
    try
      Compiler.Compile(AFormula)
    except on E: ERbwParserError do
      begin
        frameCropName.Grid.Canvas.Brush.Color := clRed;
        Exit;
        // send error message
      end;
    end;
    if Compiler.CurrentExpression.ResultType <> rdtBoolean then
    begin
      frameCropName.Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmCropProperties.frameCropNameGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  RowIndex: Integer;
//  ItemCount: Integer;
  ItemIndex: Integer;
  ACrop: TCropItem;
  ANode: TJvPageIndexNode;
  ExtraNode: TJvPageIndexNode;
  NewName: string;
begin
  inherited;
  if (csReading in ComponentState) or (FCrops = nil) then
  begin
    Exit;
  end;
  if ACol = Ord(ncName) then
  begin
    if ARow > frameCropName.seNumber.AsInteger then
    begin
      frameCropName.seNumber.AsInteger := ARow
    end;
  end;

  ItemIndex := -1;

  ANode := FCropsNode;
  for RowIndex := 1 to frameCropName.seNumber.AsInteger do
  begin
//    if frameCropName.Grid.Cells[Ord(ncName), RowIndex] <> '' then
    begin
      Inc(ItemIndex);

      while FCrops.Count <= ItemIndex do
      begin
        FCrops.Add;
      end;
      ACrop := FCrops[ItemIndex];

      NewName := frameCropName.Grid.Cells[Ord(ncName), RowIndex];
      if NewName <> '' then
      begin
        ACrop.CropName := GenerateNewRoot(NewName);
      end;

      ANode := ANode.getNextSibling as TJvPageIndexNode;
      if ANode = nil then
      begin
        ANode := jvpltvMain.Items.Add(nil, ACrop.CropName) as TJvPageIndexNode;
        CreateChildNodes(ACrop, ANode);
        ANode.Expanded := True;
      end
      else
      begin
        ANode.Text := ACrop.CropName
      end;

      if FPressureStart > 0 then
      begin
        ACrop.PSI1 := frameCropName.Grid.Cells[FPressureStart + Ord(pc1), RowIndex];
        ACrop.PSI2 := frameCropName.Grid.Cells[FPressureStart + Ord(pc2), RowIndex];
        ACrop.PSI3 := frameCropName.Grid.Cells[FPressureStart + Ord(pc3), RowIndex];
        ACrop.PSI4 := frameCropName.Grid.Cells[FPressureStart + Ord(pc4), RowIndex];
      end;

      if FCropPropStart > 0 then
      begin
         ACrop.BaseTemperature := frameCropName.Grid.Cells[FCropPropStart + Ord(cpBaseTemperature), RowIndex];
         ACrop.MinimumCutoffTemperature := frameCropName.Grid.Cells[FCropPropStart + Ord(cpMinimumCutoffTemperature), RowIndex];
         ACrop.MaximumCutoffTemperature := frameCropName.Grid.Cells[FCropPropStart + Ord(cpMaximumCutoffTemperature), RowIndex];
         ACrop.Coefficient0 := frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient0), RowIndex];
         ACrop.Coefficient1 := frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient1), RowIndex];
         ACrop.Coefficient2 := frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient2), RowIndex];
         ACrop.Coefficient3 := frameCropName.Grid.Cells[FCropPropStart + Ord(cpCoefficient3), RowIndex];
         ACrop.BeginningRootDepth := frameCropName.Grid.Cells[FCropPropStart + Ord(cpBeginningRootDepth), RowIndex];
         ACrop.MaximumRootDepth := frameCropName.Grid.Cells[FCropPropStart + Ord(cpMaximumRootDepth), RowIndex];
         ACrop.RootGrowthCoefficient := frameCropName.Grid.Cells[FCropPropStart + Ord(cpRootGrowthCoefficient), RowIndex];
         ACrop.Irrigated := frameCropName.Grid.Cells[FCropPropStart + Ord(cpIrrigated), RowIndex];
      end;

      if FFallowStart > 0 then
      begin
         ACrop.Fallow := frameCropName.Grid.Cells[FFallowStart + Ord(fcFallow), RowIndex];
      end;
    end;
  end;
  ExtraNode := ANode.getNextSibling as TJvPageIndexNode;
  while ExtraNode <> nil do
  begin
    ExtraNode.Free;
    ExtraNode := ANode.getNextSibling as TJvPageIndexNode;
  end;
end;

procedure TfrmCropProperties.frameCropNamesbAddClick(Sender: TObject);
begin
  inherited;
  FCrops.Add;
  frameCropName.sbAddClick(Sender);
end;

procedure TfrmCropProperties.frameCropNamesbDeleteClick(Sender: TObject);
var
  ItemIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  ItemIndex := frameCropName.Grid.SelectedRow-1;
  if (ItemIndex >= 0) and (ItemIndex < FCrops.Count) then
  begin
    FCrops.Delete(ItemIndex)
  end;
  frameCropName.sbDeleteClick(Sender);
  for ColIndex := 0 to frameCropName.Grid.ColCount - 1 do
  begin
    frameCropNameGridSetEditText(frameCropName.Grid, ColIndex, frameCropName.Grid.SelectedRow,
      frameCropName.Grid.Cells[ColIndex, frameCropName.Grid.SelectedRow]);
  end;

end;

procedure TfrmCropProperties.frameCropNamesbInsertClick(Sender: TObject);
var
  ItemIndex: Integer;
begin
  inherited;
  ItemIndex := frameCropName.Grid.SelectedRow-1;
  if (ItemIndex >= 0) and (ItemIndex <= FCrops.Count) then
  begin
    FCrops.Insert(ItemIndex)
  end;
  frameCropName.sbInsertClick(Sender);

end;

procedure TfrmCropProperties.frameCropNameseNumberChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  if FCrops <> nil then
  begin
    while FCrops.Count > frameCropName.seNumber.AsInteger do
    begin
      FCrops.Last.Free;
    end;
    while FCrops.Count < frameCropName.seNumber.AsInteger do
    begin
      FCrops.Add;
    end;
  end;
  frameCropName.seNumberChange(Sender);
  if frameCropName.seNumber.AsInteger = 0 then
  begin
    frameCropName.Grid.Cells[Ord(ncID), 1] := '';
  end
  else
  begin
    for RowIndex := 1 to frameCropName.seNumber.AsInteger do
    begin
      frameCropName.Grid.Cells[Ord(ncID), RowIndex] := IntToStr(RowIndex);
    end;
  end;
end;

procedure TfrmCropProperties.frameCropWaterUseGridBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  AFormula: string;
  Compiler: TRbwParser;
begin
  inherited;
  if (ARow >= 1) and (ACol = Ord(cwuIrrigated)) then
  begin
    frameCropWaterUse.Grid.Canvas.Brush.Color := clWindow;
    AFormula := frameCropWaterUse.Grid.Cells[ACol, ARow];
    Compiler := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
    try
      Compiler.Compile(AFormula)
    except on E: ERbwParserError do
      begin
        frameCropWaterUse.Grid.Canvas.Brush.Color := clRed;
        Exit;
        // send error message
      end;
    end;
    if Compiler.CurrentExpression.ResultType <> rdtBoolean then
    begin
      frameCropWaterUse.Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmCropProperties.frameCropWaterUseGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TCropWaterUseItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  frameCropWaterUse.GridEndUpdate(Sender);
  if FWaterUseCollection <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to frameCropWaterUse.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameCropWaterUse.Grid.Cells[
        Ord(cwuStart), RowIndex], StartTime)
        and TryStrToFloat(frameCropWaterUse.Grid.Cells[
        Ord(cwuEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FWaterUseCollection.Count then
        begin
          FWaterUseCollection.Add;
        end;
        AnItem := FWaterUseCollection[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.CropValue := frameCropWaterUse.Grid.Cells[Ord(cwuCropValue), RowIndex];
        AnItem.Irrigated := frameCropWaterUse.Grid.Cells[Ord(cwuIrrigated), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FWaterUseCollection.Count > ItemCount do
    begin
      FWaterUseCollection.Last.Free;
    end;
  end;
end;

procedure TfrmCropProperties.frameEvapFractionsGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TEvapFractionsItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  frameEvapFractions.GridEndUpdate(Sender);
  if FEvapFract <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to frameEvapFractions.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameEvapFractions.Grid.Cells[
        Ord(efcStart), RowIndex], StartTime)
        and TryStrToFloat(frameEvapFractions.Grid.Cells[
        Ord(efcEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FEvapFract.Count then
        begin
          FEvapFract.Add;
        end;
        AnItem := FEvapFract[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.TranspirationFraction := frameEvapFractions.Grid.Cells[Ord(efcTransp), RowIndex];
        AnItem.PrecipFraction := frameEvapFractions.Grid.Cells[Ord(efcPrecip), RowIndex];
        AnItem.IrrigFraction := frameEvapFractions.Grid.Cells[Ord(efcIrrig), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FEvapFract.Count > ItemCount do
    begin
      FEvapFract.Last.Free;
    end;
  end;
end;

procedure TfrmCropProperties.frameIrrigationGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TCropIrrigationItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;

  frameIrrigation.GridEndUpdate(Sender);
  if FIrrigation <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to frameIrrigation.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameIrrigation.Grid.Cells[
        Ord(icStart), RowIndex], StartTime)
        and TryStrToFloat(frameIrrigation.Grid.Cells[
        Ord(icEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FIrrigation.Count then
        begin
          FIrrigation.Add;
        end;
        AnItem := FIrrigation[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.Irrigation :=
          frameIrrigation.Grid.Cells[Ord(IcIrrigation), RowIndex];
        AnItem.EvapIrrigateFraction :=
          frameIrrigation.Grid.Cells[Ord(icEvapIrrigateFraction), RowIndex];
        AnItem.SurfaceWaterLossFractionIrrigation :=
          frameIrrigation.Grid.Cells[Ord(icSWLossFracIrrigate), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FIrrigation.Count > ItemCount do
    begin
      FIrrigation.Last.Free;
    end;
  end
end;

procedure TfrmCropProperties.frameLandUseFractionGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TOwhmItem;
  Frame: TframeFormulaGrid;
  OwhmCollection: TOwhmCollection;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  Frame := frameLandUseFraction;
  OwhmCollection := FLandUseFraction;

  Frame.GridEndUpdate(Sender);
  if OwhmCollection <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to Frame.seNumber.AsInteger do
    begin
      if TryStrToFloat(Frame.Grid.Cells[
        Ord(ocStart), RowIndex], StartTime)
        and TryStrToFloat(Frame.Grid.Cells[
        Ord(ocEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= OwhmCollection.Count then
        begin
          OwhmCollection.Add;
        end;
        AnItem := OwhmCollection[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.OwhmValue :=
          Frame.Grid.Cells[Ord(ocFormula), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while OwhmCollection.Count > ItemCount do
    begin
      OwhmCollection.Last.Free;
    end;
  end
end;

procedure TfrmCropProperties.frameLossesGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TLossesItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  frameLosses.GridEndUpdate(Sender);
  if FLosses <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to frameLosses.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameLosses.Grid.Cells[
        Ord(lcStart), RowIndex], StartTime)
        and TryStrToFloat(frameLosses.Grid.Cells[
        Ord(lcEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FLosses.Count then
        begin
          FLosses.Add;
        end;
        AnItem := FLosses[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.PrecipitationLosses := frameLosses.Grid.Cells[Ord(lcPrecip), RowIndex];
        AnItem.IrrigationLosses := frameLosses.Grid.Cells[Ord(lcIrrig), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FLosses.Count > ItemCount do
    begin
      FLosses.Last.Free;
    end;
  end;
end;

procedure TfrmCropProperties.frameRootPressureGridEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TRootPressureItem;
//  AnItem: TRootingDepthItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  frameRootPressure.GridEndUpdate(Sender);
  if FRootPressure <> nil then
  begin
    ItemCount := 0;
    for RowIndex := 1 to frameRootPressure.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameRootPressure.Grid.Cells[
        Ord(pcStart), RowIndex], StartTime)
        and TryStrToFloat(frameRootPressure.Grid.Cells[
        Ord(pcEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FRootPressure.Count then
        begin
          FRootPressure.Add;
        end;
        AnItem := FRootPressure[ItemCount] as TRootPressureItem;
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.Psi1 := frameRootPressure.Grid.Cells[Ord(pcPsi1), RowIndex];
        AnItem.Psi2:= frameRootPressure.Grid.Cells[Ord(pcPsi2), RowIndex];
        AnItem.Psi3:= frameRootPressure.Grid.Cells[Ord(pcPsi3), RowIndex];
        AnItem.Psi4:= frameRootPressure.Grid.Cells[Ord(pcPsi4), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FRootPressure.Count > ItemCount do
    begin
      FRootPressure.Last.Free;
    end;
  end;
end;

procedure TfrmCropProperties.GetCropFunction(
  CropFunctions: TCropFunctionCollection);
var
  ItemIndex: Integer;
  AnItem: TCropFunctionItem;
begin
  FCropFunctions := CropFunctions;
  frameCropFunction.ClearGrid;
  frameCropFunction.seNumber.AsInteger := CropFunctions.Count;
  frameCropFunction.seNumber.OnChange(frameCropFunction.seNumber);
  if frameCropFunction.seNumber.AsInteger = 0 then
  begin
    frameCropFunction.Grid.Row := 1;
    frameCropFunction.ClearSelectedRow;
  end;
  frameCropFunction.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to CropFunctions.Count - 1 do
    begin
      AnItem := CropFunctions[ItemIndex];
      frameCropFunction.Grid.Cells[Ord(cfcStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameCropFunction.Grid.Cells[Ord(cfcEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameCropFunction.Grid.Cells[Ord(cfcSlope), ItemIndex+1] := AnItem.Slope;
      frameCropFunction.Grid.Cells[Ord(cfcIntercept), ItemIndex+1] := AnItem.Intercept;
      frameCropFunction.Grid.Cells[Ord(cfcPrice), ItemIndex+1] := AnItem.Price;
    end;
  finally
    frameCropFunction.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.GetCrops(CropCollection: TCropCollection);
var
  CropIndex: Integer;
  ACrop: TCropItem;
  Grid: TRbwDataGrid4;
begin
  frameCropName.ClearGrid;
  frameCropName.seNumber.AsInteger := CropCollection.Count;
  Grid := frameCropName.Grid;
  Grid.BeginUpdate;
  try
    for CropIndex := 0 to CropCollection.Count - 1 do
    begin
      ACrop := CropCollection[CropIndex];
      Grid.Cells[Ord(ncID), CropIndex+1] := IntToStr(CropIndex+1);
      Grid.Cells[Ord(ncName), CropIndex+1] := ACrop.CropName;
  //    ColIndex := Ord(ncName);
      if FPressureStart > 0 then
      begin
        Grid.Cells[FPressureStart + Ord(pc1), CropIndex+1] := ACrop.PSI1;
        Grid.Cells[FPressureStart + Ord(pc2), CropIndex+1] := ACrop.PSI2;
        Grid.Cells[FPressureStart + Ord(pc3), CropIndex+1] := ACrop.PSI3;
        Grid.Cells[FPressureStart + Ord(pc4), CropIndex+1] := ACrop.PSI4;
      end;
      if FCropPropStart > 0 then
      begin
        Grid.Cells[FCropPropStart + Ord(cpBaseTemperature), CropIndex+1]
          := ACrop.BaseTemperature;
        Grid.Cells[FCropPropStart + Ord(cpMinimumCutoffTemperature), CropIndex+1]
          := ACrop.MinimumCutoffTemperature;
        Grid.Cells[FCropPropStart + Ord(cpMaximumCutoffTemperature), CropIndex+1]
          := ACrop.MaximumCutoffTemperature;
        Grid.Cells[FCropPropStart + Ord(cpCoefficient0), CropIndex+1]
          := ACrop.Coefficient0;
        Grid.Cells[FCropPropStart + Ord(cpCoefficient1), CropIndex+1]
          := ACrop.Coefficient1;
        Grid.Cells[FCropPropStart + Ord(cpCoefficient2), CropIndex+1]
          := ACrop.Coefficient2;
        Grid.Cells[FCropPropStart + Ord(cpCoefficient3), CropIndex+1]
          := ACrop.Coefficient3;
        Grid.Cells[FCropPropStart + Ord(cpBeginningRootDepth), CropIndex+1]
          := ACrop.BeginningRootDepth;
        Grid.Cells[FCropPropStart + Ord(cpMaximumRootDepth), CropIndex+1]
          := ACrop.MaximumRootDepth;
        Grid.Cells[FCropPropStart + Ord(cpRootGrowthCoefficient), CropIndex+1]
          := ACrop.RootGrowthCoefficient;
        Grid.Cells[FCropPropStart + Ord(cpIrrigated), CropIndex+1]
          := ACrop.Irrigated;
      end;
      if FFallowStart > 0 then
      begin
        Grid.Cells[FFallowStart + Ord(fcFallow), CropIndex+1]
          := ACrop.Fallow;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.GetCropWaterUseFunction(
  WaterUseCollection: TCropWaterUseCollection);
var
  ItemIndex: Integer;
  AnItem: TCropWaterUseItem;
begin
  FWaterUseCollection := WaterUseCollection;
  frameCropWaterUse.ClearGrid;
  frameCropWaterUse.seNumber.AsInteger := WaterUseCollection.Count;
  frameCropWaterUse.seNumber.OnChange(frameCropWaterUse.seNumber);
  if frameCropWaterUse.seNumber.AsInteger = 0 then
  begin
    frameCropWaterUse.Grid.Row := 1;
    frameCropWaterUse.ClearSelectedRow;
  end;
  frameCropWaterUse.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to WaterUseCollection.Count - 1 do
    begin
      AnItem := WaterUseCollection[ItemIndex];
      frameCropWaterUse.Grid.Cells[Ord(cwuStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameCropWaterUse.Grid.Cells[Ord(cwuEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameCropWaterUse.Grid.Cells[Ord(cwuCropValue), ItemIndex+1] := AnItem.CropValue;
      frameCropWaterUse.Grid.Cells[Ord(cwuIrrigated), ItemIndex+1] := AnItem.Irrigated;
    end;
  finally
    frameCropWaterUse.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.GetData;
var
  CropIndex: Integer;
  ACrop: TCropItem;
  CropNode: TJvPageIndexNode;
  StartTimes: TStringList;
  EndTimes: TStringList;
  TimeIndex: Integer;
  StressPeriods: TModflowStressPeriods;
  AStressPeriod: TModflowStressPeriod;
begin
  GetGlobalVariables;
  SetUpCropNameTable(frmGoPhast.PhastModel);
  SetUpEvapFractionsTable(frmGoPhast.PhastModel);
  SetUpLossesTable(frmGoPhast.PhastModel);
  SetUpCropFunctionTable(frmGoPhast.PhastModel);
  SetUpCropWaterUseTable(frmGoPhast.PhastModel);
  SetUpIrrigationTable(frmGoPhast.PhastModel);
  SetUpLandUseFractionTable(frmGoPhast.PhastModel);
  SetUpRootPressureTable(frmGoPhast.PhastModel);
  SetUpGroundwaterRootInteractionTable(frmGoPhast.PhastModel);

  StartTimes := TStringList.Create;
  EndTimes := TStringList.Create;
  try
    StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
    for TimeIndex := 0 to StressPeriods.Count - 1 do
    begin
      AStressPeriod := StressPeriods[TimeIndex];
      StartTimes.Add(FloatToStr(AStressPeriod.StartTime));
      EndTimes.Add(FloatToStr(AStressPeriod.EndTime));
    end;
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameEvapFractions.Grid);
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameLosses.Grid);
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameCropFunction.Grid);
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameCropWaterUse.Grid);
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameIrrigation.Grid);
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameLandUseFraction.Grid);
    SetStartAndEndTimeLists(StartTimes, EndTimes, frameRootPressure.Grid);
  finally
    EndTimes.Free;
    StartTimes.Free;
  end;


  FCrops := TCropCollection.Create(nil);
  FCrops.Assign(frmGoPhast.PhastModel.FmpCrops);

  FCropsNode := jvpltvMain.Items.Add(nil, StrCrops) as TJvPageIndexNode;
  FCropsNode.PageIndex := jvspCropName.PageIndex;
  FCropsNode.Data := FCrops;

  FFarmProcess := frmGoPhast.PhastModel.ModflowPackages.FarmProcess;
  FFarmProcess4 := frmGoPhast.PhastModel.ModflowPackages.FarmProcess4;
  FFarmLandUse := frmGoPhast.PhastModel.ModflowPackages.FarmLandUse;

  for CropIndex := 0 to FCrops.Count - 1 do
  begin
    ACrop := FCrops[CropIndex];
    CropNode := jvpltvMain.Items.Add(nil, ACrop.CropName) as TJvPageIndexNode;
    CreateChildNodes(ACrop, CropNode);
  end;
end;

procedure TfrmCropProperties.SetGridColumnProperties(Grid: TRbwDataGrid4);
var
  ColIndex: Integer;
begin
  Grid.BeginUpdate;
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].WordWrapCaptions := True;
    Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
    Grid.Columns[ColIndex].AutoAdjustColWidths := True;
  end;
  Grid.EndUpdate;
  Grid.BeginUpdate;
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
  Grid.EndUpdate;
end;

procedure TfrmCropProperties.SetStartAndEndTimeLists(StartTimes,
  EndTimes: TStringList; Grid: TRbwDataGrid4);
begin
  Grid.Columns[0].PickList := StartTimes;
  Grid.Columns[0].ComboUsed := True;
  Grid.Columns[1].PickList := EndTimes;
  Grid.Columns[1].ComboUsed := True;
end;

procedure TfrmCropProperties.GetEvapFractions(
  EvapFract: TEvapFractionsCollection);
var
  ItemIndex: Integer;
  AnItem: TEvapFractionsItem;
begin
  FEvapFract := EvapFract;
  frameEvapFractions.ClearGrid;
  frameEvapFractions.seNumber.AsInteger := EvapFract.Count;
  frameEvapFractions.seNumber.OnChange(frameEvapFractions.seNumber);
  if frameEvapFractions.seNumber.AsInteger = 0 then
  begin
    frameEvapFractions.Grid.Row := 1;
    frameEvapFractions.ClearSelectedRow;
  end;
  frameEvapFractions.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to EvapFract.Count - 1 do
    begin
      AnItem := EvapFract[ItemIndex];
      frameEvapFractions.Grid.Cells[Ord(efcStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameEvapFractions.Grid.Cells[Ord(efcEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameEvapFractions.Grid.Cells[Ord(efcTransp), ItemIndex+1] := AnItem.TranspirationFraction;
      frameEvapFractions.Grid.Cells[Ord(efcPrecip), ItemIndex+1] := AnItem.PrecipFraction;
      frameEvapFractions.Grid.Cells[Ord(efcIrrig), ItemIndex+1] := AnItem.IrrigFraction;
    end;
  finally
    frameEvapFractions.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.GetLandUseFraction(
  LandUseFraction: TOwhmCollection);
const
  TimeColumnCount = 2;
var
  ItemIndex: Integer;
  AnItem: TOwhmItem;
  frame: TframeFormulaGrid;
begin
  Assert(LandUseFraction <> nil);
  FLandUseFraction := LandUseFraction;
  frame := frameLandUseFraction;

  frame.ClearGrid;
  frame.seNumber.AsInteger := LandUseFraction.Count;
  frame.seNumber.OnChange(frame.seNumber);
  if frame.seNumber.AsInteger = 0 then
  begin
    frame.Grid.Row := 1;
    frame.ClearSelectedRow;
  end;
  frame.Grid.BeginUpdate;
  try
    if Ord(ocFormula) - TimeColumnCount < LandUseFraction.OwhmNames.Count then
    begin
      frame.Grid.Cells[Ord(ocFormula), 0] :=
        LandUseFraction.OwhmNames[Ord(ocFormula) - TimeColumnCount];
    end;
    for ItemIndex := 0 to LandUseFraction.Count - 1 do
    begin
      AnItem := LandUseFraction[ItemIndex];
      frame.Grid.Cells[Ord(ocStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frame.Grid.Cells[Ord(ocEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frame.Grid.Cells[Ord(ocFormula), ItemIndex+1] := AnItem.OwhmValue;
    end;
  finally
    frame.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.GetLosses(Losses: TLossesCollection);
var
  ItemIndex: Integer;
  AnItem: TLossesItem;
begin
  FLosses := Losses;
  frameLosses.ClearGrid;
  frameLosses.seNumber.AsInteger := Losses.Count;
  frameLosses.seNumber.OnChange(frameLosses.seNumber);
  if frameLosses.seNumber.AsInteger = 0 then
  begin
    frameLosses.Grid.Row := 1;
    frameLosses.ClearSelectedRow;
  end;
  frameLosses.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to Losses.Count - 1 do
    begin
      AnItem := Losses[ItemIndex];
      frameLosses.Grid.Cells[Ord(lcStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameLosses.Grid.Cells[Ord(lcEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameLosses.Grid.Cells[Ord(lcPrecip), ItemIndex+1] := AnItem.PrecipitationLosses;
      frameLosses.Grid.Cells[Ord(lcIrrig), ItemIndex+1] := AnItem.IrrigationLosses;
    end;
  finally
    frameLosses.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.GetRootPressure(RootPressure: TRootPressureCollection);
var
  ItemIndex: Integer;
  AnItem: TRootPressureItem;
begin
  Assert(RootPressure <> nil);
  FRootPressure := RootPressure;
  frameRootPressure.ClearGrid;
  frameRootPressure.seNumber.AsInteger := RootPressure.Count;
  frameRootPressure.seNumber.OnChange(frameRootPressure.seNumber);
  if frameRootPressure.seNumber.AsInteger = 0 then
  begin
    frameRootPressure.Grid.Row := 1;
    frameRootPressure.ClearSelectedRow;
  end;
  frameRootPressure.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to RootPressure.Count - 1 do
    begin
      AnItem := RootPressure[ItemIndex] as TRootPressureItem;
      frameRootPressure.Grid.Cells[Ord(pcStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameRootPressure.Grid.Cells[Ord(pcEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameRootPressure.Grid.Cells[Ord(pcPsi1), ItemIndex+1] := AnItem.Psi1;
      frameRootPressure.Grid.Cells[Ord(pcPsi2), ItemIndex+1] := AnItem.Psi2;
      frameRootPressure.Grid.Cells[Ord(pcPsi3), ItemIndex+1] := AnItem.Psi3;
      frameRootPressure.Grid.Cells[Ord(pcPsi4), ItemIndex+1] := AnItem.Psi4;
    end;
  finally
    frameRootPressure.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.jplMainChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := jplMain.ActivePage.HelpKeyword;
end;

procedure TfrmCropProperties.jvpltvMainChange(Sender: TObject; Node: TTreeNode);
var
  AnObject: TObject;
begin
  inherited;
  if Node.Data <> nil then
  begin
    FGettingData := True;
    try
      AnObject := Node.Data;
      Assert(AnObject <> nil);
      if AnObject is TCropCollection then
      begin
        GetCrops(TCropCollection(AnObject));
      end
//      else if AnObject is TFmpRootDepthCollection then
//      begin
//        GetRootingDepth(TFmpRootDepthCollection(AnObject));
//      end
      else if AnObject is TEvapFractionsCollection then
      begin
        GetEvapFractions(TEvapFractionsCollection(AnObject));
      end
      else if AnObject is TLossesCollection then
      begin
        GetLosses(TLossesCollection(AnObject));
      end
      else if AnObject is TCropFunctionCollection then
      begin
        GetCropFunction(TCropFunctionCollection(AnObject));
      end
      else if AnObject is TCropWaterUseCollection then
      begin
        GetCropWaterUseFunction(TCropWaterUseCollection(AnObject));
      end
      else if AnObject is TIrrigationCollection then
      begin
        GetIrrigation(TIrrigationCollection(AnObject));
      end
      else if AnObject is TOwhmCollection then
      begin
        GetLandUseFraction(TOwhmCollection(AnObject));
      end
      else if AnObject is TRootPressureCollection then
      begin
        GetRootPressure(TRootPressureCollection(AnObject));
      end
      else if AnObject is TGroundwaterRootInteraction then
      begin
        GetGroundwaterRootInteraction(TGroundwaterRootInteraction(AnObject));
      end
      else
        Assert(False);
    finally
      FGettingData := False;
    end;
  end;
end;

procedure TfrmCropProperties.jvpltvMainCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

procedure TfrmCropProperties.SetCropNameTableColumns(Model: TCustomModel);
var
  FarmProcess: TFarmProcess;
begin
  FarmProcess := Model.ModflowPackages.FarmProcess;
  FNameStart := 0;
  FLastCol := Ord(High(TNameCol));

  FFallowStart := -1;
  if FarmProcess.DeficiencyPolicy = dpWaterStacking then
  begin
    FFallowStart := Succ(FLastCol);
    FLastCol := FFallowStart + Ord(High(TFallowCol));
  end;

  FPressureStart := -1;
  if FarmProcess.CropConsumptiveConcept = cccConcept1 then
  begin
    FPressureStart := Succ(FLastCol);
    FLastCol := FPressureStart + Ord(High(TPressureCol));
  end;

  FCropPropStart := -1;
  if (FarmProcess.RootingDepth = rdCalculated)
    or (FarmProcess.ConsumptiveUse = cuCalculated)
    or (FarmProcess.Precipitation = pTimeSeries) then
  begin
    FCropPropStart := Succ(FLastCol);
    FLastCol := FCropPropStart + Ord(High(TCropProp));
  end;
end;

procedure TfrmCropProperties.GetGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    CompilerList.Add(rbwprsrGlobal);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmCropProperties.GetGroundwaterRootInteraction(
  GroundwaterRootInteraction: TGroundwaterRootInteraction);
var
  Code: TInteractionCode;
begin
  FSettingGwInteraction := True;
  try
//    rdgGwRootInteraction.HideEditor;
    FGroundwaterRootInteraction := GroundwaterRootInteraction;

    Code := FGroundwaterRootInteraction.InteractionCode;
    AssignGWRootInteractionCheckboxes(Code);
  finally
    FSettingGwInteraction := False;
  end;
end;

procedure TfrmCropProperties.GetIrrigation(
  Irrigation: TIrrigationCollection);
var
  ItemIndex: Integer;
  AnItem: TCropIrrigationItem;
begin
  Assert(Irrigation <> nil);
  FIrrigation := Irrigation;
  frameIrrigation.ClearGrid;
  frameIrrigation.seNumber.AsInteger := Irrigation.Count;
  frameIrrigation.seNumber.OnChange(frameIrrigation.seNumber);
  if frameIrrigation.seNumber.AsInteger = 0 then
  begin
    frameIrrigation.Grid.Row := 1;
    frameIrrigation.ClearSelectedRow;
  end;
  frameIrrigation.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to Irrigation.Count - 1 do
    begin
      AnItem := Irrigation[ItemIndex];
      frameIrrigation.Grid.Cells[Ord(icStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameIrrigation.Grid.Cells[Ord(icEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameIrrigation.Grid.Cells[Ord(IcIrrigation), ItemIndex+1] := AnItem.Irrigation;
      frameIrrigation.Grid.Cells[Ord(icEvapIrrigateFraction), ItemIndex+1] := AnItem.EvapIrrigateFraction;
      frameIrrigation.Grid.Cells[Ord(icSWLossFracIrrigate), ItemIndex+1] := AnItem.SurfaceWaterLossFractionIrrigation;
    end;
  finally
    frameIrrigation.Grid.EndUpdate;
  end;
end;

procedure TfrmCropProperties.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoCrops.Create(FCrops));
end;

procedure TfrmCropProperties.AssignGWRootInteractionCheckboxes(Code: TInteractionCode);
var
  HasTranspiration: Boolean;
  GW: Boolean;
  Anoxia: Boolean;
  Stress: Boolean;
begin
  TGroundwaterRootInteraction.ConvertFromInteractionCode(Code,
    HasTranspiration, GW, Anoxia, Stress);
  rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)] := HasTranspiration;
  rdgGwRootInteraction.Checked[0,Ord(griGroundwater)] := GW;
  rdgGwRootInteraction.Checked[0,Ord(griAnoxia)] := Anoxia;
  rdgGwRootInteraction.Checked[0,Ord(griStress)] := Stress;
end;

function TfrmCropProperties.CheckBoxesToGwRootInteractionCode: TInteractionCode;
var
  HasTranspriationB: Boolean;
  GWUptakeB: Boolean;
  AnoxiaB: Boolean;
  SoilStressB: Boolean;
begin
  HasTranspriationB := rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)];
  GWUptakeB := rdgGwRootInteraction.Checked[0,Ord(griGroundwater)];
  AnoxiaB := rdgGwRootInteraction.Checked[0,Ord(griAnoxia)];
  SoilStressB := rdgGwRootInteraction.Checked[0,Ord(griStress)];
  result := TGroundwaterRootInteraction.ConvertToInteractionCode(
    HasTranspriationB, GWUptakeB, AnoxiaB, SoilStressB);
end;

procedure TfrmCropProperties.chklstGwRootInteractionClickCheck(Sender: TObject);
begin
  inherited;
  if FSettingGwInteraction then
  begin
    Exit;
  end;
  FSettingGwInteraction := True;
  try
//    casae

    if rdgGwRootInteraction.Checked[0,Ord(griGroundwater)]
      or rdgGwRootInteraction.Checked[0,Ord(griAnoxia)]
      or rdgGwRootInteraction.Checked[0,Ord(griStress)]
      then
    begin
      rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)] := True;
    end;
    if not rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)] then
    begin

    end;

    if rdgGwRootInteraction.Checked[0,Ord(griAnoxia)] then
    begin
      rdgGwRootInteraction.Checked[0,Ord(griStress)] := True;
    end;
    AssignGWRootInteractionCheckboxes(CheckBoxesToGwRootInteractionCode);
  finally
    FSettingGwInteraction := False;
  end;
end;

procedure TfrmCropProperties.jvspGwRootInteractionShow(Sender: TObject);
begin
  rdgGwRootInteraction.RowCount := 5;
  rdgGwRootInteraction.Row := 4;
  rdgGwRootInteraction.RowCount := 4;
  inherited;
end;

procedure TfrmCropProperties.rdgGwRootInteractionExit(Sender: TObject);
begin
  inherited;
  FGroundwaterRootInteraction.InteractionCode :=
    CheckBoxesToGwRootInteractionCode
end;

procedure TfrmCropProperties.rdgGwRootInteractionStateChange(Sender: TObject;
    ACol, ARow: Integer; const Value: TCheckBoxState);
var
  index: TGwRootInteractionIndicies;
  Checked: Boolean;
begin
  inherited;
  if FSettingGwInteraction or (ARow < 0) then
  begin
    Exit;
  end;
  FSettingGwInteraction := True;
  try

    Checked := rdgGwRootInteraction.Checked[0,ARow];

    index := TGwRootInteractionIndicies(ARow);
    case index of
      griHasTranspiration:
        begin
          if not Checked then
          begin
            rdgGwRootInteraction.Checked[0,Ord(griGroundwater)] := False;
            rdgGwRootInteraction.Checked[0,Ord(griAnoxia)] := False;
            rdgGwRootInteraction.Checked[0,Ord(griStress)] := False;
          end;
        end;
      griGroundwater:
        begin
          if Checked then
          begin
            rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)] := True;
          end;
        end;
      griAnoxia:
        begin
          if Checked then
          begin
            rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)] := True;
            rdgGwRootInteraction.Checked[0,Ord(griStress)] := True;
          end
          else
          begin
            if not rdgGwRootInteraction.Checked[0,Ord(griGroundwater)] then
            begin
              rdgGwRootInteraction.Checked[0,Ord(griStress)] := False;
            end;
          end;
        end;
      griStress:
        begin
          if Checked then
          begin
            rdgGwRootInteraction.Checked[0,Ord(griHasTranspiration)] := True;
          end
          else
          begin
            rdgGwRootInteraction.Checked[0,Ord(griAnoxia)] := False;
          end;
        end;
    end;

    AssignGWRootInteractionCheckboxes(CheckBoxesToGwRootInteractionCode);
  finally
    FSettingGwInteraction := False;
  end;

end;

//procedure TfrmCropProperties.chklstGwRootInteractionKeyUp(Sender: TObject; var
//    Key: Word; Shift: TShiftState);
//begin
//end;

{ TUndoCrops }

constructor TUndoCrops.Create(var NewCrops: TCropCollection);
var
  FarmIndex: Integer;
  AFarm: TFarm;
  NewFarm: TFarm;
begin
  FNewCrops := NewCrops;
  NewCrops := nil;
  FOldCrops := TCropCollection.Create(nil);
  FOldCrops.Assign(frmGoPhast.PhastModel.FmpCrops);
  FFarmList := TFarmObjectList.Create;

  for FarmIndex := 0 to frmGoPhast.PhastModel.Farms.Count - 1 do
  begin
    AFarm := frmGoPhast.PhastModel.Farms[FarmIndex];
    if (AFarm <> nil) and AFarm.Used then
    begin
      NewFarm := TFarm.Create(nil);
      NewFarm.Assign(AFarm);
      FFarmList.Add(NewFarm);
    end;
  end;
end;

function TUndoCrops.Description: string;
begin
  result := StrChangeFarmCrops;
end;

destructor TUndoCrops.Destroy;
begin
//  FFarmScreenObjects.Free;
  FFarmList.Free;
  FNewCrops.Free;
  FOldCrops.Free;

  inherited;
end;

procedure TUndoCrops.DoCommand;
begin
  frmGoPhast.PhastModel.FmpCrops := FNewCrops;
  frmGoPhast.PhastModel.FmpCrops.UpdateAllDataArrays;
end;

procedure TUndoCrops.Undo;
var
  index: Integer;
  AFarm: TFarm;
begin
  frmGoPhast.PhastModel.FmpCrops := FOldCrops;
  frmGoPhast.PhastModel.FmpCrops.UpdateAllDataArrays;
  Assert(frmGoPhast.PhastModel.Farms.Count = FFarmList.Count);
  for index := 0 to FFarmList.Count - 1 do
  begin
    AFarm := FFarmList[index];
    frmGoPhast.PhastModel.Farms[index].Assign(AFarm);
  end;
end;

end.
