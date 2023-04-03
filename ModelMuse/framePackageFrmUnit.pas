unit framePackageFrmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, Grids,
  RbwDataGrid4, RbwController, StdCtrls, ModflowPackageSelectionUnit, ExtCtrls,
  ComCtrls, frameRadioGridUnit, JvPageList,
  JvExControls, JvExComCtrls, JvPageListTreeView, JvExExtCtrls,
  JvNetscapeSplitter, ArgusDataEntry;

type
  TframePkgFarm = class(TframePackage)
    rgAssignmentMethod: TRadioGroup;
    frameCropConsumptiveUse: TframeRadioGrid;
    jvplFarm: TJvPageList;
    tvpglstFarm: TJvPageListTreeView;
    jvspOptions: TJvStandardPage;
    jvspParameters: TJvStandardPage;
    jvspWhenToRead: TJvStandardPage;
    jvspWaterPolicy: TJvStandardPage;
    jvspCropConsumptiveUse: TJvStandardPage;
    jvspSurfaceWater: TJvStandardPage;
    jvspMandatoryPrintFlags1: TJvStandardPage;
    frameFarmBudgetPrintFlag: TframeRadioGrid;
    frameAcreageOptimizationPrintSettings: TframeRadioGrid;
    splttrFarm: TJvNetscapeSplitter;
    lblRootingDepth: TLabel;
    lblConsumptiveUse: TLabel;
    lblPrecipitation: TLabel;
    lblInefficiencyLosses: TLabel;
    comboRootingDepth: TComboBox;
    comboConsumptiveUse: TComboBox;
    comboInefficiencyLosses: TComboBox;
    comboPrecipitation: TComboBox;
    lblDeficiency: TLabel;
    comboDeficiency: TComboBox;
    frameEfficiencyBehavior: TframeRadioGrid;
    lblRoutedDelivery: TLabel;
    comboRoutedDelivery: TComboBox;
    lblRoutedReturnFlow: TLabel;
    comboRoutedReturnFlow: TComboBox;
    lblAllotment: TLabel;
    comboAllotment: TComboBox;
    lblDiversionCriterion: TLabel;
    rdeDiversionCriterion: TRbwDataEntry;
    jvspOptionalPrintFlags: TJvStandardPage;
    lblSaveWellFlowRates: TLabel;
    comboSaveWellFlowRates: TComboBox;
    lblSaveRecharge: TLabel;
    comboSaveRecharge: TComboBox;
    lblSupplyAndDemand: TLabel;
    comboSupplyAndDemand: TComboBox;
    lblDiversionBudgetLocation: TLabel;
    comboDiversionBudgetLocation: TComboBox;
    lblCropIrrigationRequirement: TLabel;
    comboCropIrrigationRequirement: TComboBox;
    lblRecomputeFlows: TLabel;
    comboRecomputeFlows: TComboBox;
    frameRoutingInformationPrintFlag: TframeRadioGrid;
    cbGroundwaterAllotments: TCheckBox;
    frameET_PrintFlag: TframeRadioGrid;
    jvspMandatoryPrintFlags2: TJvStandardPage;
    cbResetQMax: TCheckBox;
    jvspMnwNwtOptions: TJvStandardPage;
    lblMnwExplanation: TLabel;
    grpMNWOptions: TGroupBox;
    cbMnwClose: TCheckBox;
    rdeRPCT: TRbwDataEntry;
    lblRPCT: TLabel;
    rdeHPCT: TRbwDataEntry;
    lblHPCT: TLabel;
    rdeQClose: TRbwDataEntry;
    lblQClose: TLabel;
    grpNwtOptions: TGroupBox;
    rdePSIRAMPF: TRbwDataEntry;
    lblPSIRAMPF: TLabel;
    rdeSATTHK: TRbwDataEntry;
    lblSATTHK: TLabel;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboAllotmentChange(Sender: TObject);
    procedure comboDeficiencyChange(Sender: TObject);
    procedure tvpglstFarmCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure frameET_PrintFlagrdgGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure cbMnwCloseClick(Sender: TObject);
  private
    procedure InitializeGrids;
    procedure MoveInheritedControls;
    procedure CreatePageLinks;
    { Private declarations }
    procedure EnablePclose;
    procedure EnableIopfl;
    procedure EnableIPAPFL;
    procedure EnableMnwControls;
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePkgFarm: TframePkgFarm;

implementation

uses
  GoPhastTypes;

resourcestring
  StrConstant = 'Delivery varies with groundwater level';
  StrVaries = 'Efficiency varies with groundwater level';
  StrEachStressPeriod = 'Farm efficiency reset at each stress period';
  StrEachTimeStep = 'Farm efficiency reset at each time step';
  Str0 = '0';
  Str1 = '1';
  Str2 = '2';
  Str3 = '3';
  StrConcept1 = 'Concept 1';
  StrConcept2 = 'Concept 2';
  StrNotLinked = 'Not linked to UZF';
  StrLinked = 'Linked to UZF';
  Str4 = '4';
  StrNone0 = 'None (0)';
  StrTextFile = 'Text file';
  StrBinaryFile = 'Binary file';
  StrCompactOdd = 'Compact (odd)';
  StrDetailedEven = 'Detailed (even)';
  Str2Odd = '>2 (odd)';
  Str2Even = '>2 (even)';
  StrListing = 'Listing';
  StrTextFileACROPTO = 'Text file “ACR_OPT.OUT”';
  StrNone = 'None';
  StrCellFractions = 'Cell fractions every time step';
  StrResourceConstraints = 'Resource constraints if optimized';
  StrCellFractionsAndR = 'Cell fractions and resource constraints if optimized';
  StrMatrix = 'Tableaux matrix if optimized';
  StrMinus1 = '-1';
  StrMinus2 = '-2';
  StrMinus3 = '-3';
  StrMinus4 = '-4';
  StrOptions = 'Options';
  StrParameters = 'Parameters';
  StrWhenToReadFlags = 'When to Read Flags';
  StrWaterPolicyFlags = 'Water Policy Flags';
  StrCropConsumptiveUse = 'Crop Consumptive-Use Flags';
  StrSurfaceWaterFlags = 'Surface-Water Flags';
  StrMandatoryPrintFlag1 = 'Mandatory Print Flags 1';
  StrMandatoryPrintFlag2 = 'Mandatory Print Flags 2';
  StrOptionalPrintFlags = 'Optional Print Flags';
  StrTextFileROUTOUT = 'Text file ROUT.OUT';
  StrEveryStressPeriod = 'Every Stress Period';
  StrFirstStressPeriod = 'First Stress Period';
  Str1FBCOMPACTOUT = '1 FB_COMPACT.OUT';
  Str2FBDETAILSOUT = '2 FB_DETAILS.OUT';
  StrETArray = 'ET Array';
  StrEvapAndTranspArra = 'Evap and Transp Arrays';
  StrList = 'List by Farm';
  StrListAndArrays = 'List and arrays';
  StrTextFiles = 'Text files';
  StrMNWOptions = 'MNW and NWT Options';
//  StrAuxiliaryVariables = 'Auxiliary Variables and Options';

{$R *.dfm}

{ TframePkgFarm }

procedure TframePkgFarm.EnableIopfl;
begin
  frameAcreageOptimizationPrintSettings.Enabled := rcSelectionController.Enabled and
    (TDeficiencyPolicy(comboDeficiency.ItemIndex) in
    [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool]);
  if frameAcreageOptimizationPrintSettings.Enabled then
  begin
    frameAcreageOptimizationPrintSettings.rdgGrid.Color := clWindow;
  end
  else
  begin
    frameAcreageOptimizationPrintSettings.rdgGrid.Color := clBtnFace;
  end;
end;

procedure TframePkgFarm.EnableIPAPFL;
begin
  comboDiversionBudgetLocation.Enabled := rcSelectionController.Enabled
    and (TSurfaceWaterAllotment(comboAllotment.ItemIndex) in
    [swaEqual, swaPriorWithCalls, swaPriorWithoutCalls]);
end;

procedure TframePkgFarm.EnablePclose;
begin
  rdeDiversionCriterion.Enabled := rcSelectionController.Enabled
    and (TSurfaceWaterAllotment(comboAllotment.ItemIndex) in
    [swaEqual, swaPriorWithCalls, swaPriorWithoutCalls]);
end;

procedure TframePkgFarm.frameET_PrintFlagrdgGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ACol = 5) and (ARow = 2) then
  begin
    CanSelect := False;
  end;
end;

procedure TframePkgFarm.GetData(Package: TModflowPackageSelection);
var
  FarmProcess: TFarmProcess;
begin
  inherited;
  MoveInheritedControls;
  CreatePageLinks;

  FarmProcess := Package as TFarmProcess;

  // When to Read
  comboRootingDepth.ItemIndex := Ord(FarmProcess.RootingDepth);
  comboConsumptiveUse.ItemIndex := Ord(FarmProcess.ConsumptiveUse);
  comboPrecipitation.ItemIndex := Ord(FarmProcess.Precipitation);
  comboInefficiencyLosses.ItemIndex := Ord(FarmProcess.FractionOfInefficiencyLosses);

  // Water Policy
  frameEfficiencyBehavior.ColItemIndex := Ord(FarmProcess.EfficiencyGroundwaterFunction)+1;
  frameEfficiencyBehavior.RowItemIndex := Ord(FarmProcess.EfficiencyReset) + 1;
  comboDeficiency.ItemIndex := Ord(FarmProcess.DeficiencyPolicy);
  cbGroundwaterAllotments.Checked := FarmProcess.GroundwaterAllotmentsUsed;

  // Crop Consumptive-Use
  frameCropConsumptiveUse.ColItemIndex := Ord(FarmProcess.CropConsumptiveConcept)+1;
  frameCropConsumptiveUse.RowItemIndex := Ord(FarmProcess.CropConsumptiveLinkage)+1;

  // Surface-Water Flags
  comboRoutedDelivery.ItemIndex := Ord(FarmProcess.RoutedDelivery);
  comboRoutedReturnFlow.ItemIndex := Ord(FarmProcess.RoutedReturn);
  comboAllotment.ItemIndex := Ord(FarmProcess.SurfaceWaterAllotment);
  rdeDiversionCriterion.RealValue := FarmProcess.SurfaceWaterClosure;


  // Mandatory Print flags
  comboSaveWellFlowRates.ItemIndex := Ord(FarmProcess.SaveWellFlowRates);
  comboSaveRecharge.ItemIndex := Ord(FarmProcess.SaveNetRecharge);
  comboSupplyAndDemand.ItemIndex := Ord(FarmProcess.SupplyAndDemand);
  frameFarmBudgetPrintFlag.ColItemIndex := Ord(FarmProcess.FarmBudgetPrintFlags)+1;
  frameFarmBudgetPrintFlag.RowItemIndex := Ord(FarmProcess.FarmBudgetPrintHowMuch)+1;
  frameET_PrintFlag.ColItemIndex := Ord(FarmProcess.EtPrintType)+1;
  frameET_PrintFlag.RowItemIndex := Ord(FarmProcess.EtPrintLocation)+1;

  // Optional Print Flags
  frameRoutingInformationPrintFlag.ColItemIndex := Ord(FarmProcess.PrintRouting)+1;
  frameRoutingInformationPrintFlag.RowItemIndex := Ord(FarmProcess.PrintRoutingFrequency)+1;

  comboDiversionBudgetLocation.ItemIndex := Ord(FarmProcess.DiversionBudgetLocation);
  frameAcreageOptimizationPrintSettings.ColItemIndex := Ord(FarmProcess.AcerageOptimizationPrintLocation) +1;
  frameAcreageOptimizationPrintSettings.RowItemIndex := Ord(FarmProcess.AcerageOptimizationPrintChoice) + 1;

  // Auxilliary and Options

  comboCropIrrigationRequirement.ItemIndex := Ord(FarmProcess.CropIrrigationRequirement);
  comboRecomputeFlows.ItemIndex := Ord(FarmProcess.RecomputeOption);
  cbResetQMax.Checked := FarmProcess.ResetMnwQMax;

  rgAssignmentMethod.ItemIndex := Ord(FarmProcess.AssignmentMethod);

  // MNW and NWT options
  cbMnwClose.Checked := FarmProcess.MnwClose;
  rdeQClose.RealValue := FarmProcess.MnwClosureCriterion;
  rdeHPCT.RealValue := FarmProcess.HeadChangeReduction;
  rdeRPCT.RealValue := FarmProcess.ResidualChangeReduction;

  rdePSIRAMPF.RealValue := FarmProcess.PsiRampf;
  rdeSATTHK.RealValue := FarmProcess.SatThick;

  EnableIopfl;
  EnablePclose;
  EnableIPAPFL;
  EnableMnwControls;
end;

procedure TframePkgFarm.InitializeGrids;
begin

  frameEfficiencyBehavior.rdgGrid.BeginUpdate;
  try
    frameEfficiencyBehavior.rdgGrid.Cells[1,0] := StrConstant;
    frameEfficiencyBehavior.rdgGrid.Cells[2,0] := StrVaries;
    frameEfficiencyBehavior.rdgGrid.Cells[0,1] := StrEachStressPeriod;
    frameEfficiencyBehavior.rdgGrid.Cells[0,2] := StrEachTimeStep;
    frameEfficiencyBehavior.rdgGrid.Cells[1,1] := Str0;
    frameEfficiencyBehavior.rdgGrid.Cells[1,2] := Str1;
    frameEfficiencyBehavior.rdgGrid.Cells[2,1] := Str2;
    frameEfficiencyBehavior.rdgGrid.Cells[2,2] := Str3;
    frameEfficiencyBehavior.rdgGrid.FixedCols := 1;
  finally
    frameEfficiencyBehavior.rdgGrid.EndUpdate;
  end;

  // Crop Consumptive-Use
  frameCropConsumptiveUse.rdgGrid.BeginUpdate;
  try
    frameCropConsumptiveUse.rdgGrid.Cells[1,0] := StrConcept1;
    frameCropConsumptiveUse.rdgGrid.Cells[2,0] := StrConcept2;
    frameCropConsumptiveUse.rdgGrid.Cells[0,1] := StrNotLinked;
    frameCropConsumptiveUse.rdgGrid.Cells[0,2] := StrLinked;
    frameCropConsumptiveUse.rdgGrid.Cells[1,1] := Str1;
    frameCropConsumptiveUse.rdgGrid.Cells[1,2] := Str3;
    frameCropConsumptiveUse.rdgGrid.Cells[2,1] := Str2;
    frameCropConsumptiveUse.rdgGrid.Cells[2,2] := Str4;
    frameCropConsumptiveUse.rdgGrid.FixedCols := 1;
  finally
    frameCropConsumptiveUse.rdgGrid.EndUpdate;
  end;

  frameFarmBudgetPrintFlag.rdgGrid.BeginUpdate;
  try
    frameFarmBudgetPrintFlag.rdgGrid.Cells[1,0] := StrNone0;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[2,0] := StrTextFile;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[3,0] := StrBinaryFile;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[0,1] := StrCompactOdd;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[0,2] := StrDetailedEven;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[1,1] := Str0;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[1,2] := Str0;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[2,1] := Str1FBCOMPACTOUT;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[2,2] := Str2FBDETAILSOUT;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[3,1] := Str2Odd;
    frameFarmBudgetPrintFlag.rdgGrid.Cells[3,2] := Str2Even;
    frameFarmBudgetPrintFlag.rdgGrid.FixedCols := 1;
  finally
    frameFarmBudgetPrintFlag.rdgGrid.EndUpdate;
  end;

  frameAcreageOptimizationPrintSettings.rdgGrid.BeginUpdate;
  try
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[1,0] := StrListing;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[2,0] := StrTextFileACROPTO;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[0,1] := StrNone;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[0,2] := StrCellFractions;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[0,3] := StrResourceConstraints;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[0,4] := StrCellFractionsAndR;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[0,5] := StrMatrix;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[1,1] := Str0;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[2,1] := Str0;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[1,2] := StrMinus1;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[2,2] := Str1;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[1,3] := StrMinus2;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[2,3] := Str2;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[1,4] := StrMinus3;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[2,4] := Str3;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[1,5] := StrMinus4;
    frameAcreageOptimizationPrintSettings.rdgGrid.Cells[2,5] := Str4;
    frameAcreageOptimizationPrintSettings.rdgGrid.FixedCols := 1;
  finally
    frameAcreageOptimizationPrintSettings.rdgGrid.EndUpdate
  end;

  frameRoutingInformationPrintFlag.rdgGrid.BeginUpdate;
  try
    frameRoutingInformationPrintFlag.rdgGrid.Cells[1,0] := StrNone;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[2,0] := StrListing;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[3,0] := StrTextFileROUTOUT;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[0,1] := StrEveryStressPeriod;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[0,2] := StrFirstStressPeriod;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[1,1] := Str0;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[1,2] := Str0;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[2,1] := StrMinus1;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[2,2] := StrMinus2;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[3,1] := Str1;
    frameRoutingInformationPrintFlag.rdgGrid.Cells[3,2] := Str2;
  finally
    frameRoutingInformationPrintFlag.rdgGrid.EndUpdate
  end;

  frameET_PrintFlag.rdgGrid.BeginUpdate;
  try
    frameET_PrintFlag.rdgGrid.FixedCols := 1;
    frameET_PrintFlag.rdgGrid.Cells[1,0] := StrNone;
    frameET_PrintFlag.rdgGrid.Cells[2,0] := StrETArray;
    frameET_PrintFlag.rdgGrid.Cells[3,0] := StrEvapAndTranspArra;
    frameET_PrintFlag.rdgGrid.Cells[4,0] := StrList;
    frameET_PrintFlag.rdgGrid.Cells[5,0] := StrListAndArrays;
    frameET_PrintFlag.rdgGrid.Cells[0,1] := StrTextFiles;
    frameET_PrintFlag.rdgGrid.Cells[0,2] := StrListing;
    frameET_PrintFlag.rdgGrid.Cells[1,1] := Str0;
    frameET_PrintFlag.rdgGrid.Cells[1,2] := Str0;
    frameET_PrintFlag.rdgGrid.Cells[2,1] := Str1;
    frameET_PrintFlag.rdgGrid.Cells[2,2] := StrMinus1;
    frameET_PrintFlag.rdgGrid.Cells[3,1] := Str2;
    frameET_PrintFlag.rdgGrid.Cells[3,2] := StrMinus2;
    frameET_PrintFlag.rdgGrid.Cells[4,1] := Str3;
    frameET_PrintFlag.rdgGrid.Cells[4,2] := StrMinus3;
    frameET_PrintFlag.rdgGrid.Cells[5,1] := Str4;
  finally
    frameET_PrintFlag.rdgGrid.EndUpdate;
  end;
end;

procedure TframePkgFarm.Loaded;
begin
  inherited;
  InitializeGrids;
end;

procedure TframePkgFarm.SetData(Package: TModflowPackageSelection);
var
  FarmProcess: TFarmProcess;
  AValue: double;
begin
  inherited;
  FarmProcess := Package as TFarmProcess;

  // When to Read
  FarmProcess.RootingDepth := TRootingDepth(comboRootingDepth.ItemIndex);
  FarmProcess.ConsumptiveUse := TConsumptiveUse(comboConsumptiveUse.ItemIndex);
  FarmProcess.Precipitation := TPrecipitation(comboPrecipitation.ItemIndex);
  FarmProcess.FractionOfInefficiencyLosses :=
    TFractionOfInefficiencyLosses(comboInefficiencyLosses.ItemIndex);

  // Water Policy

  FarmProcess.EfficiencyGroundwaterFunction := TEfficiencyGroundwaterFunction(frameEfficiencyBehavior.ColItemIndex-1);
  FarmProcess.EfficiencyReset := TEfficiencyReset(frameEfficiencyBehavior.RowItemIndex-1);
  FarmProcess.DeficiencyPolicy := TDeficiencyPolicy(comboDeficiency.ItemIndex);
  FarmProcess.GroundwaterAllotmentsUsed := cbGroundwaterAllotments.Checked;

  // Crop Consumptive-Use
  FarmProcess.CropConsumptiveConcept := TCropConsumptiveConcept(frameCropConsumptiveUse.ColItemIndex-1);
  FarmProcess.CropConsumptiveLinkage := TCropConsumptiveLinkage(frameCropConsumptiveUse.RowItemIndex-1);

  // Surface-Water Flags
  FarmProcess.RoutedDelivery := TRoutedDelivery(comboRoutedDelivery.ItemIndex);
  FarmProcess.RoutedReturn := TRoutedReturn(comboRoutedReturnFlow.ItemIndex);
  FarmProcess.SurfaceWaterAllotment := TSurfaceWaterAllotment(comboAllotment.ItemIndex);
  FarmProcess.SurfaceWaterClosure := rdeDiversionCriterion.RealValue;

  // Mandatory Print flags
  FarmProcess.SaveWellFlowRates := TSaveWellFlowRates(comboSaveWellFlowRates.ItemIndex);
  FarmProcess.SaveNetRecharge := TSaveNetRecharge(comboSaveRecharge.ItemIndex);
  FarmProcess.SupplyAndDemand := TSupplyAndDemand(comboSupplyAndDemand.ItemIndex);
  FarmProcess.FarmBudgetPrintFlags := TFarmBudgetPrintFlags(frameFarmBudgetPrintFlag.ColItemIndex-1);
  FarmProcess.FarmBudgetPrintHowMuch := TFarmBudgetPrintHowMuch(frameFarmBudgetPrintFlag.RowItemIndex-1);
  FarmProcess.EtPrintType := TEtPrintType(frameET_PrintFlag.ColItemIndex-1);
  FarmProcess.EtPrintLocation := TEtPrintLocation(frameET_PrintFlag.RowItemIndex-1);

  // Optional Print Flags
  FarmProcess.PrintRouting := TPrintRouting(frameRoutingInformationPrintFlag.ColItemIndex-1);
  FarmProcess.PrintRoutingFrequency := TPrintRoutingFrequency(frameRoutingInformationPrintFlag.RowItemIndex-1);


  FarmProcess.DiversionBudgetLocation := TDiversionBudgetLocation(comboDiversionBudgetLocation.ItemIndex);
  FarmProcess.AcerageOptimizationPrintLocation := TAcerageOptimizationPrintLocation(frameAcreageOptimizationPrintSettings.ColItemIndex-1);
  FarmProcess.AcerageOptimizationPrintChoice := TAcerageOptimizationPrintChoice(frameAcreageOptimizationPrintSettings.RowItemIndex-1);

  // Auxilliary and Options

  FarmProcess.CropIrrigationRequirement := TCropIrrigationRequirement(comboCropIrrigationRequirement.ItemIndex);
  FarmProcess.RecomputeOption := TRecomputeOption(comboRecomputeFlows.ItemIndex);
  FarmProcess.ResetMnwQMax := cbResetQMax.Checked;

  FarmProcess.AssignmentMethod := TUpdateMethod(rgAssignmentMethod.ItemIndex);

  // MNW and NWT options
  FarmProcess.MnwClose := cbMnwClose.Checked;
  if TryStrToFloat(rdeQClose.Text, AValue) then
  begin
    FarmProcess.MnwClosureCriterion := AValue;
  end;
  if TryStrToFloat(rdeHPCT.Text, AValue) then
  begin
    FarmProcess.HeadChangeReduction := AValue;
  end;
  if TryStrToFloat(rdeRPCT.Text, AValue) then
  begin
    FarmProcess.ResidualChangeReduction := AValue;
  end;

  if TryStrToFloat(rdePSIRAMPF.Text, AValue) then
  begin
    FarmProcess.PsiRampf := AValue;
  end;
  if TryStrToFloat(rdeSATTHK.Text, AValue) then
  begin
    FarmProcess.SatThick := AValue;
  end;
end;

procedure TframePkgFarm.tvpglstFarmCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

procedure TframePkgFarm.EnableMnwControls;
begin
  rdeQClose.Enabled := cbMnwClose.Checked and rcSelectionController.Enabled;
  rdeHPCT.Enabled := rdeQClose.Enabled;
  rdeRPCT.Enabled := rdeQClose.Enabled;
end;

procedure TframePkgFarm.cbMnwCloseClick(Sender: TObject);
begin
  inherited;
  EnableMnwControls;
end;

procedure TframePkgFarm.comboAllotmentChange(Sender: TObject);
begin
  inherited;
  EnablePclose;
  EnableIPAPFL;
end;

procedure TframePkgFarm.comboDeficiencyChange(Sender: TObject);
begin
  inherited;
  EnableIopfl;
end;

procedure TframePkgFarm.CreatePageLinks;
var
  Node: TJvPageIndexNode;
begin
  tvpglstFarm.Items.Clear;
  Node := tvpglstFarm.Items.Add(nil, StrOptions) as TJvPageIndexNode;
  Node.PageIndex := jvspOptions.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrMNWOptions) as TJvPageIndexNode;
  Node.PageIndex := jvspMnwNwtOptions.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrParameters) as TJvPageIndexNode;
  Node.PageIndex := jvspParameters.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrWhenToReadFlags) as TJvPageIndexNode;
  Node.PageIndex := jvspWhenToRead.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrWaterPolicyFlags) as TJvPageIndexNode;
  Node.PageIndex := jvspWaterPolicy.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrCropConsumptiveUse) as TJvPageIndexNode;
  Node.PageIndex := jvspCropConsumptiveUse.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrSurfaceWaterFlags) as TJvPageIndexNode;
  Node.PageIndex := jvspSurfaceWater.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrMandatoryPrintFlag1) as TJvPageIndexNode;
  Node.PageIndex := jvspMandatoryPrintFlags1.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrMandatoryPrintFlag2) as TJvPageIndexNode;
  Node.PageIndex := jvspMandatoryPrintFlags2.PageIndex;
  Node := tvpglstFarm.Items.Add(nil, StrOptionalPrintFlags) as TJvPageIndexNode;
  Node.PageIndex := jvspOptionalPrintFlags.PageIndex;
//  Node := tvpglstFarm.Items.Add(nil, StrAuxiliaryVariables) as TJvPageIndexNode;
//  Node.PageIndex := jvspAuxiliaryVariablesAndOptions.PageIndex;
end;

procedure TframePkgFarm.MoveInheritedControls;
begin
  jvplFarm.ActivePageIndex := 0;
  lblPackage.Parent := jvspOptions;
  lblPackage.Top := 8;
  lblComments.Parent := jvspOptions;
  lblComments.Top := lblPackage.Top + lblPackage.Height + 8;

  cbResetQMax.Top := jvspOptions.ClientHeight
    - cbResetQMax.Height - 8;

  comboRecomputeFlows.Top := cbResetQMax.Top
    - comboRecomputeFlows.Height - 8;
  lblRecomputeFlows.Top := comboRecomputeFlows.Top
    - lblRecomputeFlows.Height - 4;

  comboCropIrrigationRequirement.Top := lblRecomputeFlows.Top
    - comboCropIrrigationRequirement.Height - 8;
  lblCropIrrigationRequirement.Top := comboCropIrrigationRequirement.Top
    - lblCropIrrigationRequirement.Height - 4;

  rgAssignmentMethod.Top := lblCropIrrigationRequirement.Top
    - rgAssignmentMethod.Height - 8;
  rgAssignmentMethod.Width := jvspOptions.ClientWidth -
    rgAssignmentMethod.Left - 8;

  memoComments.Parent := jvspOptions;
  memoComments.Top := lblComments.Top + lblComments.Height + 8;
  memoComments.Width := jvspOptions.ClientWidth - memoComments.Left - 8;
  memoComments.Height := rgAssignmentMethod.Top - memoComments.Top - 8;
end;

procedure TframePkgFarm.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnablePclose;
  EnableIopfl;
  EnableIPAPFL;
  EnableMnwControls;
end;

end.
