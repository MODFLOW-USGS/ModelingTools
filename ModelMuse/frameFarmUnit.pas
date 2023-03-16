unit frameFarmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectUnit, StdCtrls,
  Mask, JvExMask, JvSpin, ExtCtrls, ComCtrls, frameGridUnit,
  frameFormulaGridUnit, JvgPage, frameDeliveryGridUnit, frameFarmDiversionUnit,
  UndoItemsScreenObjects, ModflowFmpFarmUnit, RbwDataGrid4, ModflowFmpCropUnit,
  ClassificationUnit, RbwParser, ModflowFmpIrrigationUnit;

type
  TframeFarm = class(TframeScreenObject)
    tabCrops: TTabSheet;
    tabDiversionLocation: TTabSheet;
    tabReturnFlowLocation: TTabSheet;
    tabNonRoutedDelivery: TTabSheet;
    pnlCaption: TPanel;
    seFarmId: TJvSpinEdit;
    lblFarmId: TLabel;
    tabWaterRights: TTabSheet;
    frameFormulaGridCrops: TframeFormulaGrid;
    frameFormulaGridDiversion: TframeFarmDiversion;
    frameFormulaGridReturnFlow: TframeFarmDiversion;
    frameFormulaGridWaterRights: TframeFormulaGrid;
    tabCosts: TTabSheet;
    pcMain: TJvgPageControl;
    frameFormulaGridCosts: TframeFormulaGrid;
    frameDelivery: TframeDeliveryGrid;
    pnlTop: TPanel;
    tabGW_Allocation: TTabSheet;
    frameGW_Allocation: TframeFormulaGrid;
    rbwprsrFarmParser: TRbwParser;
    edFarmName: TLabeledEdit;
    tabEfficiencyImprovement: TTabSheet;
    frameFormulaGridEfficiencyImprovement: TframeFormulaGrid;
    procedure frameFormulaGridCropsedFormulaChange(Sender: TObject);
    procedure frameFormulaGridCropsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridCropsseNumberChange(Sender: TObject);
    procedure frameFormulaGridCropssbAddClick(Sender: TObject);
    procedure frameFormulaGridCropssbInsertClick(Sender: TObject);
    procedure frameFormulaGridCropssbDeleteClick(Sender: TObject);
    procedure frameFormulaGridCostsedFormulaChange(Sender: TObject);
    procedure frameFormulaGridCostsseNumberChange(Sender: TObject);
    procedure frameFormulaGridCostsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridCostssbAddClick(Sender: TObject);
    procedure frameFormulaGridCostssbInsertClick(Sender: TObject);
    procedure frameFormulaGridCostssbDeleteClick(Sender: TObject);
    procedure frameFormulaGridWaterRightsedFormulaChange(Sender: TObject);
    procedure frameFormulaGridWaterRightsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridWaterRightsseNumberChange(Sender: TObject);
    procedure frameFormulaGridWaterRightssbAddClick(Sender: TObject);
    procedure frameFormulaGridWaterRightssbInsertClick(Sender: TObject);
    procedure frameFormulaGridWaterRightssbDeleteClick(Sender: TObject);
    procedure seFarmIdChange(Sender: TObject);
    procedure frameDeliveryGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameFormulaGridDiversionGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridReturnFlowGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameGW_AllocationGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameGW_AllocationseNumberChange(Sender: TObject);
    procedure frameGW_AllocationedFormulaChange(Sender: TObject);
    procedure frameGW_AllocationsbAddClick(Sender: TObject);
    procedure frameGW_AllocationsbInsertClick(Sender: TObject);
    procedure frameGW_AllocationsbDeleteClick(Sender: TObject);
    procedure frameFormulaGridCropsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameFormulaGridCostsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameDeliveryGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameFormulaGridWaterRightsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameGW_AllocationGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameFormulaGridEfficiencyImprovementedFormulaChange(
      Sender: TObject);
    procedure frameFormulaGridEfficiencyImprovementGridSetEditText(
      Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure frameFormulaGridEfficiencyImprovementsbAddClick(Sender: TObject);
    procedure frameFormulaGridEfficiencyImprovementsbDeleteClick(
      Sender: TObject);
    procedure frameFormulaGridEfficiencyImprovementsbInsertClick(
      Sender: TObject);
    procedure frameFormulaGridEfficiencyImprovementseNumberChange(
      Sender: TObject);
    procedure frameFormulaGridEfficiencyImprovementGridButtonClick(
      Sender: TObject; ACol, ARow: Integer);
  private
    FChangedCrops: boolean;
    FChangedCosts: boolean;
    FChangedWaterRights: boolean;
    FOnChange: TNotifyEvent;
    FChangedID: Boolean;
    FChanging: Boolean;
    FChangedAllotment: Boolean;
    FEfficiencyImprovementChanged: Boolean;
    procedure GetCropEffForFirstFarm(FirstFarm: TFarm);
    procedure GetCropEffImproveForFirstFarm(FirstFarm: TFarm);
    procedure GetCostsForFirstFarm(FirstFarm: TFarm);
    procedure GetWaterRightsForFirstFarm(FirstFarm: TFarm);
    procedure GetGwAllotmentForFirstFarm(FirstFarm: TFarm);
    procedure GetMaxTimeAndCountForCrops(var MaxIndex, MaxTimeCount: Integer;
      AFarm: TFarm);
    procedure SetCropEfficiencies(Farm: TFarm; Crops: TCropCollection;
      IrrigationTypes: TIrrigationCollection);
    procedure SetCropEfficiencyImprove(Farm: TFarm;
      IrrigationTypes: TIrrigationCollection);
    procedure SetFarmCosts(Farm: TFarm);
    procedure SetWaterRights(Farm: TFarm);
    procedure SetGwAllotment(Farm: TFarm);
    procedure Change(Sender: TObject);
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    procedure EditFormula(Grid: TRbwDataGrid4; ACol, ARow: Integer);
    { Private declarations }
  public
    procedure InitializeControls;
    procedure GetData(FarmList: TFarmList);
    procedure SetData(FarmList: TFarmList);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameFarm: TframeFarm;

implementation

uses
  GoPhastTypes, frmGoPhastUnit,
  ModflowTimeUnit, Generics.Collections, ScreenObjectUnit, DataSetUnit,
  PhastModelUnit, ModflowPackagesUnit, ModflowPackageSelectionUnit,
  ModflowFmpAllotmentUnit, frmFormulaUnit, frmConvertChoiceUnit;

resourcestring
  StrGWBaseMaintenance = 'GW base maintenance costs / volume (GWCost1)';
  StrGWPumpingCostsV = 'GW pumping costs / (volume * lift) (GWCost2)';
  StrGWVerticalLiftCos = 'GW vertical lift costs / (volume * lift) (GWCost3)';
  StrGWDeliveryCosts = 'GW delivery costs / (volume * distance) (GWCost4)';
  StrFixedPriceOfSemi = 'Fixed price of (semi-) routed SW / volume (SWCost1)';
  StrVerticalLiftCosts = 'Vertical lift costs of (semi-) routed SW / (volume ' +
  '* lift) (SWCost2)';
  StrDeliveryCostsOfS = 'Delivery costs of (semi-) routed SW / (volume * dist' +
  'ance) (SWCost3)';
  StrFixedPriceOfNonr = 'Fixed price of non-routed SW / volume (SWCost4)';
  StrWaterRightsCallC = 'Water Rights Call (CALL)';
  StrCropEfficiency = '%s on-farm efficiency (OFE)';

type
  TCropColumns = (ccStartTime, ccEndTime, ccCrop);
  TWaterCostColumns = (wccStartTime, wccEndTime, wccGWCost1, wccGWCost2,
    wccGWCost3, wccGWCost4, wccSWCost1, wccSWCost2, wccSWCost3, wccSWCost4);
  TWaterRightsCallColumns = (wrccStartTime, wrccEndTime, wrccCall);
  TGwAllocationColumns = (gacStartTime, gacEndTime, gacAllotment);

{$R *.dfm}

{ TframeFarm }

resourcestring
  StrErrorInFormulaS = 'Error in formula: %s';


procedure TframeFarm.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TframeFarm.EditFormula(Grid: TRbwDataGrid4; ACol, ARow: Integer);
var
  AFormula: string;
//  frmFormula: TfrmFormula;
  CompiledFormula: TExpression;
  ParentControl: TWinControl;
  ValidTypes: TRbwDataTypes;
  RequiredType: TRbwDataType;
begin
  if Grid = frameFormulaGridEfficiencyImprovement.Grid then
  begin
    ValidTypes := [rdtBoolean];
    RequiredType := rdtBoolean;
  end
  else
  begin
    ValidTypes := [rdtDouble, rdtInteger];
    RequiredType := rdtDouble;
  end;
  AFormula := Grid.Cells[ACol, ARow];
  if AFormula = '' then
  begin
    AFormula := '0';
  end;

  begin
    try
      frmFormula.Initialize;
      // GIS functions are not included and
      // Data sets are not included
      // because the variables will be evaluated for screen objects and
      // not at specific locations.

      ParentControl := Parent;
      while ParentControl <> nil do
      begin
        if ParentControl is TCustomForm then
        begin
          break;
        end;
        ParentControl := ParentControl.Parent;
      end;

      if (ParentControl <> nil) and (ParentControl is TCustomForm) then
      begin
        frmFormula.PopupParent := TCustomForm(ParentControl);
      end;

      // Show the functions and global variables.
      frmFormula.IncludeTimeSeries := False;
      frmFormula.UpdateTreeList;

      // put the formula in the TfrmFormula.
      frmFormula.Formula := AFormula;
      // The user edits the formula.
      frmFormula.ShowModal;
      if frmFormula.ResultSet then
      begin
        try
          AFormula := frmFormula.Formula;
          rbwprsrFarmParser.Compile(AFormula);

        except on E: ERbwParserError do
          begin
            Beep;
            raise ERbwParserError.Create(Format(StrErrorInFormulaS,
              [E.Message]));
            Exit;
          end
        end;
        CompiledFormula := rbwprsrFarmParser.CurrentExpression;

        if CompiledFormula.ResultType in ValidTypes then
        begin
          Grid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
        end
        else
        begin
          AFormula := AdjustFormula(AFormula, CompiledFormula.ResultType, RequiredType);
          rbwprsrFarmParser.Compile(AFormula);
          CompiledFormula := rbwprsrFarmParser.CurrentExpression;
          Grid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
        end;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(Grid, ACol, ARow, Grid.Cells[ACol, ARow]);
        end;
      end;
    finally
      frmFormula.Initialize;
//      frmFormula.Free;
    end;
  end
end;

procedure TframeFarm.Change(Sender: TObject);
begin
  DoChange;
end;

procedure TframeFarm.GetData(FarmList: TFarmList);
var
  ItemIndex: Integer;
  AFarm: TFarm;
  FirstFarm: TFarm;
  FarmProcess: TFarmProcess;
  Packages: TModflowPackages;
  SfrPackage: TSfrPackageSelection;
begin
  Changing := True;
  FrameLoaded := False;
  try
    if FarmList.count = 0 then
    begin
      seFarmId.AsInteger := 0;
      ClearGrid(frameFormulaGridCrops.Grid);
      ClearGrid(frameFormulaGridCosts.Grid);
      ClearGrid(frameFormulaGridWaterRights.Grid);
      ClearGrid(frameGW_Allocation.Grid);
      ClearGrid(frameFormulaGridEfficiencyImprovement.Grid);
      Enabled := False;
      Exit;
    end;
    Enabled := True;
    Packages := frmGoPhast.PhastModel.ModflowPackages;
    FarmProcess := Packages.FarmProcess;
    tabCosts.TabVisible :=
      (FarmProcess.DeficiencyPolicy in
      [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool])
      and (FarmProcess.DeficiencyPolicy in
      [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool]);


    SfrPackage := Packages.SfrPackage;
    tabDiversionLocation.TabVisible := SfrPackage.IsSelected;
    tabReturnFlowLocation.TabVisible := SfrPackage.IsSelected;

    tabWaterRights.TabVisible :=
      FarmProcess.SurfaceWaterAllotment = swaPriorWithCalls;

    try
      frameFormulaGridCrops.Grid.BeginUpdate;
      frameFormulaGridCosts.Grid.BeginUpdate;
      frameFormulaGridWaterRights.Grid.BeginUpdate;
      frameGW_Allocation.Grid.BeginUpdate;
      frameFormulaGridEfficiencyImprovement.Grid.BeginUpdate;
      try
        ClearGrid(frameFormulaGridCrops.Grid);
        ClearGrid(frameFormulaGridCosts.Grid);
        ClearGrid(frameFormulaGridWaterRights.Grid);
        ClearGrid(frameGW_Allocation.Grid);
        ClearGrid(frameFormulaGridEfficiencyImprovement.Grid);

        FirstFarm := FarmList[0];
        GetCropEffForFirstFarm(FirstFarm);
        GetCropEffImproveForFirstFarm(FirstFarm);
        GetCostsForFirstFarm(FirstFarm);
        GetWaterRightsForFirstFarm(FirstFarm);
        GetGwAllotmentForFirstFarm(FirstFarm);
        if FarmList.Count = 1 then
        begin
          seFarmId.AsInteger := FirstFarm.FarmId;
          seFarmId.Enabled := True;
          edFarmName.Text := FirstFarm.FarmName;
          edFarmName.Enabled := True;
        end
        else
        begin
          seFarmId.AsInteger := 0;
          seFarmId.Enabled := False;
          edFarmName.Text := '';
          edFarmName.Enabled := False;
        end;

        for ItemIndex := 1 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[ItemIndex];
          if not AFarm.CurrentFarmEfficiencyCollection.IsSame(
            FirstFarm.CurrentFarmEfficiencyCollection) then
          begin
            ClearGrid(frameFormulaGridCrops.Grid);
            frameFormulaGridCrops.seNumber.AsInteger := 0;
            break;
          end;
        end;

        for ItemIndex := 1 to FarmList.Count - 1 do
        begin
          if not AFarm.FarmIrrigationEfficiencyImprovementCollection.IsSame(
            FirstFarm.FarmIrrigationEfficiencyImprovementCollection) then
          begin
            ClearGrid(frameFormulaGridEfficiencyImprovement.Grid);
            frameFormulaGridEfficiencyImprovement.seNumber.AsInteger := 0;
            break;
          end;
        end;

        for ItemIndex := 1 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[ItemIndex];
          if not AFarm.FarmCostsCollection.IsSame(
            FirstFarm.FarmCostsCollection) then
          begin
            ClearGrid(frameFormulaGridCosts.Grid);
            frameFormulaGridCosts.seNumber.AsInteger := 0;
            break;
          end;
        end;

        for ItemIndex := 1 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[ItemIndex];
          if not AFarm.WaterRights.IsSame(
            FirstFarm.WaterRights) then
          begin
            ClearGrid(frameFormulaGridWaterRights.Grid);
            frameFormulaGridWaterRights.seNumber.AsInteger := 0;
            break;
          end;
        end;

        for ItemIndex := 1 to FarmList.Count - 1 do
        begin
          AFarm := FarmList[ItemIndex];
          if not AFarm.GwAllotment.IsSame(
            FirstFarm.GwAllotment) then
          begin
            ClearGrid(frameGW_Allocation.Grid);
            frameGW_Allocation.seNumber.AsInteger := 0;
            break;
          end;
        end;

      finally
        frameFormulaGridCrops.Grid.EndUpdate;
        frameFormulaGridEfficiencyImprovement.Grid.EndUpdate;
        frameFormulaGridCosts.Grid.EndUpdate;
        frameFormulaGridWaterRights.Grid.EndUpdate;
        frameGW_Allocation.Grid.EndUpdate;
      end;

      frameFormulaGridDiversion.GetData(FarmList, dtDiversion);
      frameFormulaGridReturnFlow.GetData(FarmList, dtReturnFlow);

      frameDelivery.GetData(FarmList);

    finally
      FChangedCrops := False;
      FChangedCosts := False;
      FChangedWaterRights := False;
      FChangedID := False;
      FChangedAllotment := false;
      FEfficiencyImprovementChanged := False;
    end;
  finally
    Changing := False;
    FrameLoaded := True;
  end;
end;

procedure TframeFarm.GetGwAllotmentForFirstFarm(FirstFarm: TFarm);
var
  AFarm: TFarm;
  Grid: TRbwDataGrid4;
  TimeIndex: Integer;
  ATimeItem: TAllotmentItem;
begin
  AFarm := FirstFarm;
  frameGW_Allocation.seNumber.AsInteger := AFarm.GwAllotment.Count;
  frameGW_Allocation.seNumber.OnChange(frameGW_Allocation.seNumber);
  Grid := frameGW_Allocation.Grid;
  for TimeIndex := 0 to AFarm.GwAllotment.Count - 1 do
  begin
    ATimeItem := AFarm.GwAllotment[TimeIndex];
    Grid.Cells[Ord(wrccStartTime), TimeIndex+1] := FloatToStr(ATimeItem.StartTime);
    Grid.Cells[Ord(wrccEndTime), TimeIndex+1] := FloatToStr(ATimeItem.EndTime);
    Grid.Cells[Ord(wrccCall), TimeIndex+1] := ATimeItem.Allotment;
  end;
end;

procedure TframeFarm.frameDeliveryGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  EditFormula(Sender as TRbwDataGrid4, ACol, ARow);
end;

procedure TframeFarm.frameDeliveryGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameDelivery.GridSetEditText(Sender, ACol, ARow, Value);
  UpdateNextTimeCell(frameDelivery.Grid, ACol, ARow);

end;

procedure TframeFarm.frameFormulaGridCostsedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.edFormulaChange(Sender);
  FChangedCosts := True;
end;

procedure TframeFarm.frameFormulaGridCostsGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  EditFormula(Sender as TRbwDataGrid4, ACol, ARow);
end;

procedure TframeFarm.frameFormulaGridCostsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FChangedCosts := True;
  UpdateNextTimeCell(frameFormulaGridCosts.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCostssbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.sbAddClick(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCostssbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.sbDeleteClick(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCostssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.sbInsertClick(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCostsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.seNumberChange(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCropsedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.edFormulaChange(Sender);
  FChangedCrops := True;
end;

procedure TframeFarm.frameFormulaGridCropsGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  EditFormula(Sender as TRbwDataGrid4, ACol, ARow);
end;

procedure TframeFarm.frameFormulaGridCropsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FChangedCrops := True;
  UpdateNextTimeCell(frameFormulaGridCrops.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCropssbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.sbAddClick(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCropssbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.sbDeleteClick(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCropssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.sbInsertClick(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridCropsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.seNumberChange(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridDiversionGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFormulaGridDiversion.GridSetEditText(Sender, ACol, ARow, Value);
  UpdateNextTimeCell(frameFormulaGridDiversion.Grid, ACol, ARow);

end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridEfficiencyImprovement.edFormulaChange(Sender);
  FEfficiencyImprovementChanged := True;
end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  EditFormula(Sender as TRbwDataGrid4, ACol, ARow);
end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FEfficiencyImprovementChanged := True;
  UpdateNextTimeCell(frameFormulaGridEfficiencyImprovement.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementsbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridEfficiencyImprovement.sbAddClick(Sender);
  FEfficiencyImprovementChanged := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementsbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridEfficiencyImprovement.sbDeleteClick(Sender);
  FEfficiencyImprovementChanged := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementsbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridEfficiencyImprovement.sbInsertClick(Sender);
  FEfficiencyImprovementChanged := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridEfficiencyImprovementseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridEfficiencyImprovement.seNumberChange(Sender);
  FEfficiencyImprovementChanged := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridReturnFlowGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFormulaGridReturnFlow.GridSetEditText(Sender, ACol, ARow, Value);
  UpdateNextTimeCell(frameFormulaGridReturnFlow.Grid, ACol, ARow);
end;

procedure TframeFarm.frameFormulaGridWaterRightsedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.edFormulaChange(Sender);
  FChangedWaterRights := True;
end;

procedure TframeFarm.frameFormulaGridWaterRightsGridButtonClick(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  EditFormula(Sender as TRbwDataGrid4, ACol, ARow);
end;

procedure TframeFarm.frameFormulaGridWaterRightsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FChangedWaterRights := True;
  UpdateNextTimeCell(frameFormulaGridWaterRights.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeFarm.frameFormulaGridWaterRightssbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.sbAddClick(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridWaterRightssbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.sbDeleteClick(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridWaterRightssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.sbInsertClick(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeFarm.frameFormulaGridWaterRightsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.seNumberChange(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeFarm.frameGW_AllocationedFormulaChange(Sender: TObject);
begin
  inherited;
  frameGW_Allocation.edFormulaChange(Sender);
  FChangedAllotment := True;
end;

procedure TframeFarm.frameGW_AllocationGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  EditFormula(Sender as TRbwDataGrid4, ACol, ARow);
end;

procedure TframeFarm.frameGW_AllocationGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  FChangedAllotment := True;
  UpdateNextTimeCell(frameGW_Allocation.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeFarm.frameGW_AllocationsbAddClick(Sender: TObject);
begin
  inherited;
  frameGW_Allocation.sbAddClick(Sender);
  FChangedAllotment := True;
  DoChange;
end;

procedure TframeFarm.frameGW_AllocationsbDeleteClick(Sender: TObject);
begin
  inherited;
  frameGW_Allocation.sbDeleteClick(Sender);
  FChangedAllotment := True;
  DoChange;
end;

procedure TframeFarm.frameGW_AllocationsbInsertClick(Sender: TObject);
begin
  inherited;
  frameGW_Allocation.sbInsertClick(Sender);
  FChangedAllotment := True;
  DoChange;
end;

procedure TframeFarm.frameGW_AllocationseNumberChange(Sender: TObject);
begin
  inherited;
  frameGW_Allocation.seNumberChange(Sender);
  FChangedAllotment := True;
  DoChange;
end;

procedure TframeFarm.GetCostsForFirstFarm(
  FirstFarm: TFarm);
var
  AFarm: TFarm;
  TimeIndex: Integer;
  Grid: TRbwDataGrid4;
  TimeItem: TFarmCostsItem;
begin
  AFarm := FirstFarm;
  frameFormulaGridCosts.seNumber.AsInteger := AFarm.FarmCostsCollection.Count;
  frameFormulaGridCosts.seNumber.OnChange(frameFormulaGridCosts.seNumber);
  Grid := frameFormulaGridCosts.Grid;
  for TimeIndex := 0 to AFarm.FarmCostsCollection.Count - 1 do
  begin
    TimeItem := AFarm.FarmCostsCollection[TimeIndex];
    Grid.Cells[Ord(wccStartTime), TimeIndex+1] := FloatToStr(TimeItem.StartTime);
    Grid.Cells[Ord(wccEndTime), TimeIndex+1] := FloatToStr(TimeItem.EndTime);
    Grid.Cells[Ord(wccGWCost1), TimeIndex+1] := TimeItem.GWCost1;
    Grid.Cells[Ord(wccGWCost2), TimeIndex+1] := TimeItem.GWCost2;
    Grid.Cells[Ord(wccGWCost3), TimeIndex+1] := TimeItem.GWCost3;
    Grid.Cells[Ord(wccGWCost4), TimeIndex+1] := TimeItem.GWCost4;
    Grid.Cells[Ord(wccSWCost1), TimeIndex+1] := TimeItem.SWCost1;
    Grid.Cells[Ord(wccSWCost2), TimeIndex+1] := TimeItem.SWCost2;
    Grid.Cells[Ord(wccSWCost3), TimeIndex+1] := TimeItem.SWCost3;
    Grid.Cells[Ord(wccSWCost4), TimeIndex+1] := TimeItem.SWCost4;
  end;
end;

procedure TframeFarm.GetCropEffForFirstFarm(
  FirstFarm: TFarm);
var
  AFarm: TFarm;
  CropIndex: Integer;
  FarmEff: TFarmEfficienciesItem;
  MaxIndex: Integer;
  TimeIndex: Integer;
  TimeItem: TCropEfficiencyItem;
  Grid: TRbwDataGrid4;
  MaxTimeCount: Integer;
begin
  AFarm := FirstFarm;
  GetMaxTimeAndCountForCrops(MaxIndex, MaxTimeCount, AFarm);
  frameFormulaGridCrops.seNumber.AsInteger := MaxTimeCount;
  frameFormulaGridCrops.seNumber.OnChange(frameFormulaGridCrops.seNumber);

  if MaxIndex >= 0 then
  begin
    FarmEff := AFarm.CurrentFarmEfficiencyCollection[MaxIndex];
    Grid := frameFormulaGridCrops.Grid;
    for TimeIndex := 0 to FarmEff.CropEfficiency.Count - 1 do
    begin
      TimeItem := FarmEff.CropEfficiency[TimeIndex];
      Grid.Cells[Ord(ccStartTime), TimeIndex+1] := FloatToStr(TimeItem.StartTime);
      Grid.Cells[Ord(ccEndTime), TimeIndex+1] := FloatToStr(TimeItem.EndTime);
    end;

    for CropIndex := 0 to AFarm.CurrentFarmEfficiencyCollection.Count - 1 do
    begin
      FarmEff := AFarm.CurrentFarmEfficiencyCollection[CropIndex];
      for TimeIndex := 0 to FarmEff.CropEfficiency.Count - 1 do
      begin
        TimeItem := FarmEff.CropEfficiency[TimeIndex];
        Grid.Cells[Ord(ccCrop) + CropIndex, TimeIndex+1] := TimeItem.Efficiency;
      end;
    end;
  end;
end;

procedure TframeFarm.GetCropEffImproveForFirstFarm(FirstFarm: TFarm);
var
  AFarm: TFarm;
  CropIndex: Integer;
  FarmEff: TFarmEfficienciesItem;
  MaxIndex: Integer;
  TimeIndex: Integer;
  TimeItem: TCropEfficiencyItem;
  Grid: TRbwDataGrid4;
  MaxTimeCount: Integer;
begin
  AFarm := FirstFarm;
  GetMaxTimeAndCountForCrops(MaxIndex, MaxTimeCount, AFarm);
  frameFormulaGridEfficiencyImprovement.seNumber.AsInteger := MaxTimeCount;
  frameFormulaGridEfficiencyImprovement.seNumber.OnChange(frameFormulaGridEfficiencyImprovement.seNumber);

  if MaxIndex >= 0 then
  begin
    FarmEff := AFarm.FarmIrrigationEfficiencyImprovementCollection[MaxIndex];
    Grid := frameFormulaGridEfficiencyImprovement.Grid;
    for TimeIndex := 0 to FarmEff.CropEfficiency.Count - 1 do
    begin
      TimeItem := FarmEff.CropEfficiency[TimeIndex];
      Grid.Cells[Ord(ccStartTime), TimeIndex+1] := FloatToStr(TimeItem.StartTime);
      Grid.Cells[Ord(ccEndTime), TimeIndex+1] := FloatToStr(TimeItem.EndTime);
    end;

    for CropIndex := 0 to AFarm.FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
    begin
      FarmEff := AFarm.FarmIrrigationEfficiencyImprovementCollection[CropIndex];
      for TimeIndex := 0 to FarmEff.CropEfficiency.Count - 1 do
      begin
        TimeItem := FarmEff.CropEfficiency[TimeIndex];
        Grid.Cells[Ord(ccCrop) + CropIndex, TimeIndex+1] := TimeItem.Efficiency;
      end;
    end;
  end;
end;

procedure TframeFarm.GetWaterRightsForFirstFarm(
  FirstFarm: TFarm);
var
  AFarm: TFarm;
  TimeIndex: Integer;
  ATimeItem: TWaterRightsItem;
  Grid: TRbwDataGrid4;
begin
  AFarm := FirstFarm;
  frameFormulaGridWaterRights.seNumber.AsInteger := AFarm.WaterRights.Count;
  frameFormulaGridWaterRights.seNumber.OnChange(frameFormulaGridWaterRights.seNumber);
  Grid := frameFormulaGridWaterRights.Grid;
  for TimeIndex := 0 to AFarm.WaterRights.Count - 1 do
  begin
    ATimeItem := AFarm.WaterRights[TimeIndex];
    Grid.Cells[Ord(wrccStartTime), TimeIndex+1] := FloatToStr(ATimeItem.StartTime);
    Grid.Cells[Ord(wrccEndTime), TimeIndex+1] := FloatToStr(ATimeItem.EndTime);
    Grid.Cells[Ord(wrccCall), TimeIndex+1] := ATimeItem.WaterRights;
  end;
end;

procedure TframeFarm.InitializeControls;
var
  Grid: TRbwDataGrid4;
  Crops: TCropCollection;
  CropIndex: integer;
  ACrop: TCropItem;
  StressPeriods: TModflowStressPeriods;
  StartTimes: TStringList;
  EndTimes: TStringList;
  ColIndex: Integer;
  IrrigationType: TIrrigationItem;
  IrrigationTypes: TIrrigationCollection;
begin
  seFarmId.AsInteger := 0;
  edFarmName.Text := '';

  frameFormulaGridDiversion.OnChange := Change;
  frameFormulaGridReturnFlow.OnChange := Change;
  frameDelivery.OnChange := Change;

  tabDiversionLocation.TabVisible := frmGoPhast.PhastModel.SfrIsSelected;
  tabReturnFlowLocation.TabVisible := tabDiversionLocation.TabVisible;

  tabGW_Allocation.TabVisible := frmGoPhast.PhastModel.ModflowPackages.FarmProcess.GroundwaterAllotmentsUsed;

  pcMain.ActivePageIndex := 0;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;

  StartTimes := TStringList.Create;
  EndTimes := TStringList.Create;
  try
    // set up Crops grid.
    StressPeriods.FillStringsWithStartTimes(StartTimes);
    StressPeriods.FillStringsWithEndTimes(EndTimes);
    frameFormulaGridCrops.FirstFormulaColumn := Ord(ccCrop);
    Grid := frameFormulaGridCrops.Grid;
    ClearGrid(Grid);
    if frmGoPhast.ModelSelection = msModflowFmp then
    begin
      Crops := frmGoPhast.PhastModel.FmpCrops;
      Grid.ColCount := Crops.Count + 2;
      Grid.BeginUpdate;
      try
        Grid.Cells[Ord(ccStartTime), 0] := StrStartingTime;
        Grid.Cells[Ord(ccEndTime), 0] := StrEndingTime;
        Grid.Columns[Ord(ccStartTime)].PickList := StartTimes;
        Grid.Columns[Ord(ccEndTime)].PickList := EndTimes;
        for CropIndex := 0 to Crops.Count - 1 do
        begin
          ACrop := Crops[CropIndex];
          Grid.Cells[Ord(ccCrop) + CropIndex, 0] :=
            Format(StrCropEfficiency, [ACrop.CropName]);
          Grid.Columns[Ord(ccCrop) + CropIndex].UseButton := True;
          Grid.Columns[Ord(ccCrop) + CropIndex].ButtonCaption := StrF;
          Grid.Columns[Ord(ccCrop) + CropIndex].ButtonWidth := 35;
          Grid.Columns[Ord(ccCrop) + CropIndex].WordWrapCaptions := True;
          Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := True;
          Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustRowHeights := True;
        end;
      finally
        Grid.EndUpdate;
      end;
      Grid.BeginUpdate;
      try
        for CropIndex := 0 to Crops.Count - 1 do
        begin
          Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := False;
        end;
      finally
        Grid.EndUpdate;
      end;
    end
    else
    begin
    {$IFDEF OWHMV2}
      Assert(frmGoPhast.ModelSelection = msModflowOwhm2);
    {$ELSE}
      Assert(False);
    {$ENDIF}
      IrrigationTypes := frmGoPhast.PhastModel.IrrigationTypes;
      Grid.ColCount := IrrigationTypes.Count + 2;
      Grid.BeginUpdate;
      try
        Grid.Cells[Ord(ccStartTime), 0] := StrStartingTime;
        Grid.Cells[Ord(ccEndTime), 0] := StrEndingTime;
        Grid.Columns[Ord(ccStartTime)].PickList := StartTimes;
        Grid.Columns[Ord(ccEndTime)].PickList := EndTimes;
        for CropIndex := 0 to IrrigationTypes.Count - 1 do
        begin
          IrrigationType := IrrigationTypes[CropIndex];
          Grid.Cells[Ord(ccCrop) + CropIndex, 0] :=
            Format(StrCropEfficiency, [IrrigationType.Name]);
          Grid.Columns[Ord(ccCrop) + CropIndex].UseButton := True;
          Grid.Columns[Ord(ccCrop) + CropIndex].ButtonCaption := StrF;
          Grid.Columns[Ord(ccCrop) + CropIndex].ButtonWidth := 35;
          Grid.Columns[Ord(ccCrop) + CropIndex].WordWrapCaptions := True;
          Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := True;
          Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustRowHeights := True;
        end;
      finally
        Grid.EndUpdate;
      end;
      Grid.BeginUpdate;
      try
        for CropIndex := 0 to IrrigationTypes.Count - 1 do
        begin
          Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := False;
        end;
      finally
        Grid.EndUpdate;
      end;
    end;
    frameFormulaGridCrops.LayoutMultiRowEditControls;

    Grid := frameFormulaGridEfficiencyImprovement.Grid;
    IrrigationTypes := frmGoPhast.PhastModel.IrrigationTypes;
    Grid.ColCount := IrrigationTypes.Count + 2;
    Grid.BeginUpdate;
    try
      Grid.Cells[Ord(ccStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(ccEndTime), 0] := StrEndingTime;
      Grid.Columns[Ord(ccStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(ccEndTime)].PickList := EndTimes;
      for CropIndex := 0 to IrrigationTypes.Count - 1 do
      begin
        IrrigationType := IrrigationTypes[CropIndex];
        Grid.Cells[Ord(ccCrop) + CropIndex, 0] :=
          Format('%s efficiency improvement', [IrrigationType.Name]);
        Grid.Columns[Ord(ccCrop) + CropIndex].UseButton := True;
        Grid.Columns[Ord(ccCrop) + CropIndex].ButtonCaption := StrF;
        Grid.Columns[Ord(ccCrop) + CropIndex].ButtonWidth := 35;
        Grid.Columns[Ord(ccCrop) + CropIndex].WordWrapCaptions := True;
        Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := True;
        Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustRowHeights := True;
      end;
    finally
      Grid.EndUpdate;
    end;
    Grid.BeginUpdate;
    try
      for CropIndex := 0 to IrrigationTypes.Count - 1 do
      begin
        Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := False;
      end;
    finally
      Grid.EndUpdate;
    end;
    frameFormulaGridEfficiencyImprovement.LayoutMultiRowEditControls;

    Grid := frameFormulaGridCosts.Grid;
    ClearGrid(Grid);
    Grid.BeginUpdate;
    try
      frameFormulaGridCosts.FirstFormulaColumn := Ord(wccGWCost1);
      Grid.Cells[Ord(wccStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(wccEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(wccGWCost1), 0] := StrGWBaseMaintenance;
      Grid.Cells[Ord(wccGWCost2), 0] := StrGWPumpingCostsV;
      Grid.Cells[Ord(wccGWCost3), 0] := StrGWVerticalLiftCos;
      Grid.Cells[Ord(wccGWCost4), 0] := StrGWDeliveryCosts;
      Grid.Cells[Ord(wccSWCost1), 0] := StrFixedPriceOfSemi;
      Grid.Cells[Ord(wccSWCost2), 0] := StrVerticalLiftCosts;
      Grid.Cells[Ord(wccSWCost3), 0] := StrDeliveryCostsOfS;
      Grid.Cells[Ord(wccSWCost4), 0] := StrFixedPriceOfNonr;

      Grid.Columns[Ord(wccStartTime)].ComboUsed := True;
      Grid.Columns[Ord(wccEndTime)].ComboUsed := True;
      Grid.Columns[Ord(ccStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(ccEndTime)].PickList := EndTimes;

      for ColIndex := Ord(wccGWCost1) to Grid.ColCount - 1 do
      begin
        Grid.Columns[ColIndex].ButtonUsed := True;
        Grid.Columns[ColIndex].ButtonCaption := StrF;
        Grid.Columns[ColIndex].ButtonWidth := 35;
      end;

      for ColIndex := 0 to Grid.ColCount - 1 do
      begin
        Grid.Columns[ColIndex].AutoAdjustColWidths := True;
        Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
        Grid.Columns[ColIndex].WordWrapCaptions := True;
      end;

    finally
      Grid.EndUpdate;
    end;
    frameFormulaGridCosts.LayoutMultiRowEditControls;

    frameFormulaGridDiversion.InitializeControls;
    frameFormulaGridReturnFlow.InitializeControls;

//    frameDelivery
    frameDelivery.InitializeControls;

    Grid := frameFormulaGridWaterRights.Grid;
    ClearGrid(Grid);
    Grid.BeginUpdate;
    try
      frameFormulaGridWaterRights.FirstFormulaColumn := Ord(wrccCall);
      Grid.Cells[Ord(wrccStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(wrccEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(wrccCall), 0] := StrWaterRightsCallC;
      Grid.Columns[Ord(wrccStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(wrccEndTime)].PickList := EndTimes;
      Grid.Columns[Ord(wrccStartTime)].ComboUsed := True;
      Grid.Columns[Ord(wrccEndTime)].ComboUsed := True;

    //wrccStartTime, wrccEndTime, wrccCall
    finally
      Grid.EndUpdate;
    end;
    frameFormulaGridWaterRights.LayoutMultiRowEditControls;

    Grid := frameGW_Allocation.Grid;
    ClearGrid(Grid);
    Grid.BeginUpdate;
    try
      frameFormulaGridWaterRights.FirstFormulaColumn := Ord(gacAllotment);
      Grid.Cells[Ord(gacStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(gacEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(gacAllotment), 0] := 'Groundwater allotment';
      Grid.Columns[Ord(gacStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(gacEndTime)].PickList := EndTimes;
      Grid.Columns[Ord(gacStartTime)].ComboUsed := True;
      Grid.Columns[Ord(gacEndTime)].ComboUsed := True;
    finally
      Grid.EndUpdate;
    end;
    frameGW_Allocation.LayoutMultiRowEditControls;

  finally
    EndTimes.Free;
    StartTimes.Free;
  end;
end;

procedure TframeFarm.seFarmIdChange(Sender: TObject);
begin
  inherited;
  FChangedID := True;
  DoChange;
end;


procedure TframeFarm.SetData(FarmList: TFarmList);
var
  index: Integer;
  Farm: TFarm;
  Crops: TCropCollection;
  IntValue: Integer;
  IrrigationTypes: TIrrigationCollection;
begin
  if {FarmCreated or} FChangedID then
  begin
    IntValue := seFarmId.AsInteger;
    for index := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[index];
      Farm.FarmId := IntValue;
      Farm.FarmName := edFarmName.Text;
    end;
  end;
  if {FarmCreated or} FChangedCrops then
  begin
    Crops := frmGoPhast.PhastModel.FmpCrops;
    IrrigationTypes := frmGoPhast.PhastModel.IrrigationTypes;
    for index := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[index];
//        Farm := Item.ScreenObject.ModflowFmpFarm;
      if Farm <> nil then
      begin
        SetCropEfficiencies(Farm, Crops, IrrigationTypes);
      end;
    end;
  end;
  if FEfficiencyImprovementChanged then
  begin
    IrrigationTypes := frmGoPhast.PhastModel.IrrigationTypes;
    for index := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[index];
//        Farm := Item.ScreenObject.ModflowFmpFarm;
      if Farm <> nil then
      begin
        SetCropEfficiencyImprove(Farm, IrrigationTypes);
      end;
    end;
  end;

  if {armCreated or} FChangedCosts then
  begin
    for index := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[index];
      if Farm <> nil then
      begin
        SetFarmCosts(Farm);
      end;
    end;
  end;
  if {FarmCreated or} FChangedWaterRights then
  begin
    for index := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[index];
      if Farm <> nil then
      begin
        SetWaterRights(Farm);
      end;
    end;
  end;
  if FChangedAllotment then
  begin
    for index := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[index];
      if Farm <> nil then
      begin
        SetGwAllotment(Farm);
      end;
    end;
  end;
  if {FarmCreated or} frameFormulaGridDiversion.DataChanged then
  begin
    frameFormulaGridDiversion.SetData(FarmList, dtDiversion);
  end;
  if {FarmCreated or} frameFormulaGridReturnFlow.DataChanged then
  begin
    frameFormulaGridReturnFlow.SetData(FarmList, dtReturnFlow);
  end;
  if {FarmCreated or} frameDelivery.DataChanged then
  begin
    frameDelivery.SetData(FarmList);
  end;
end;

procedure TframeFarm.SetWaterRights(Farm: TFarm);
var
  Grid: TRbwDataGrid4;
  WaterRightsItem: TWaterRightsItem;
  WaterRights: TWaterRightsCollection;
  StartTime: Double;
  Count: Integer;
  RowIndex: Integer;
  EndTime: Double;
begin
  WaterRights := Farm.WaterRights;
  Grid := frameFormulaGridWaterRights.Grid;
  Count := 0;
  for RowIndex := 1 to frameFormulaGridWaterRights.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(wrccStartTime), RowIndex], StartTime) and TryStrToFloat(Grid.Cells[Ord(wrccEndTime), RowIndex], EndTime) then
    begin
      if Count < WaterRights.Count then
      begin
        WaterRightsItem := WaterRights[Count];
      end
      else
      begin
        WaterRightsItem := WaterRights.Add;
      end;
      Inc(Count);
      WaterRightsItem.StartTime := StartTime;
      WaterRightsItem.EndTime := EndTime;
      WaterRightsItem.WaterRights := Grid.Cells[Ord(wrccCall), RowIndex];
    end;
  end;
  while WaterRights.Count > Count do
  begin
    WaterRights.Last.Free;
  end;
end;

procedure TframeFarm.SetFarmCosts(Farm: TFarm);
var
  EndTime: Double;
  Grid: TRbwDataGrid4;
  FarmCosts: TFarmCostsCollection;
  CostItem: TFarmCostsItem;
  RowIndex: Integer;
  Count: Integer;
  StartTime: Double;
begin
  FarmCosts := Farm.FarmCostsCollection;
  Grid := frameFormulaGridCosts.Grid;
  Count := 0;
  for RowIndex := 1 to frameFormulaGridCosts.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(wccStartTime), RowIndex], StartTime)
      and TryStrToFloat(Grid.Cells[Ord(wccEndTime), RowIndex], EndTime) then
    begin
      if Count < FarmCosts.Count then
      begin
        CostItem := FarmCosts[Count];
      end
      else
      begin
        CostItem := FarmCosts.Add;
      end;
      Inc(Count);
      CostItem.StartTime := StartTime;
      CostItem.EndTime := EndTime;
      CostItem.GWcost1 := Grid.Cells[Ord(wccGWCost1), RowIndex];
      CostItem.GWcost2 := Grid.Cells[Ord(wccGWCost2), RowIndex];
      CostItem.GWcost3 := Grid.Cells[Ord(wccGWCost3), RowIndex];
      CostItem.GWcost4 := Grid.Cells[Ord(wccGWCost4), RowIndex];
      CostItem.SWcost1 := Grid.Cells[Ord(wccSWCost1), RowIndex];
      CostItem.SWcost2 := Grid.Cells[Ord(wccSWCost2), RowIndex];
      CostItem.SWcost3 := Grid.Cells[Ord(wccSWCost3), RowIndex];
      CostItem.SWcost4 := Grid.Cells[Ord(wccSWCost4), RowIndex];
    end;
  end;
  while FarmCosts.Count > Count do
  begin
    FarmCosts.Last.Free;
  end;
end;

procedure TframeFarm.SetGwAllotment(Farm: TFarm);
var
  GwAllotment: TAllotmentCollection;
  Grid: TRbwDataGrid4;
  Count: Integer;
  RowIndex: Integer;
  StartTime: double;
  EndTime: double;
  AllotmentItem: TAllotmentItem;
begin
  GwAllotment := Farm.GwAllotment;
  Grid := frameGW_Allocation.Grid;
  Count := 0;
  for RowIndex := 1 to frameGW_Allocation.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(gacStartTime), RowIndex], StartTime)
      and TryStrToFloat(Grid.Cells[Ord(gacEndTime), RowIndex], EndTime) then
    begin
      if Count < GwAllotment.Count then
      begin
        AllotmentItem := GwAllotment[Count];
      end
      else
      begin
        AllotmentItem := GwAllotment.Add;
      end;
      Inc(Count);
      AllotmentItem.StartTime := StartTime;
      AllotmentItem.EndTime := EndTime;
      AllotmentItem.Allotment := Grid.Cells[Ord(gacAllotment), RowIndex];
    end;
  end;
  while GwAllotment.Count > Count do
  begin
    GwAllotment.Last.Free;
  end;
end;

procedure TframeFarm.SetCropEfficiencies(Farm: TFarm; Crops: TCropCollection;
  IrrigationTypes: TIrrigationCollection);
var
  EndTime: Double;
  EfficienciesItem: TFarmEfficienciesItem;
  ColIndex: Integer;
  StartTime: Double;
  Grid: TRbwDataGrid4;
  EfficiencyItem: TCropEfficiencyItem;
  Rows: Generics.Collections.TList<Integer>;
  EfficiencyCollection: TFarmEfficiencyCollection;
  CropEfficiency: TCropEfficiencyCollection;
  StartTimes: Generics.Collections.TList<Double>;
  CropIndex: Integer;
  EndTimes: Generics.Collections.TList<Double>;
  RowIndex: Integer;
  ARow: Integer;
begin
  EfficiencyCollection := Farm.CurrentFarmEfficiencyCollection;
  if frmGoPhast.ModelSelection = msModflowFmp then
  begin
    for CropIndex := EfficiencyCollection.Count to Crops.Count - 1 do
    begin
      EfficienciesItem := EfficiencyCollection.Add;
      EfficienciesItem.CropEfficiency.CropName := Crops[CropIndex].CropName;
    end;
    while EfficiencyCollection.Count > Crops.Count do
    begin
      EfficiencyCollection.Last.Free;
    end;
  end
  else
  begin
    for CropIndex := EfficiencyCollection.Count to IrrigationTypes.Count - 1 do
    begin
      EfficienciesItem := EfficiencyCollection.Add;
      EfficienciesItem.CropEfficiency.CropName := IrrigationTypes[CropIndex].Name;
    end;
    while EfficiencyCollection.Count > IrrigationTypes.Count do
    begin
      EfficiencyCollection.Last.Free;
    end;
  end;
  StartTimes := TList<Double>.Create;
  EndTimes := TList<Double>.Create;
  Rows := TList<Integer>.Create;
  try
    Grid := frameFormulaGridCrops.Grid;
    for RowIndex := 1 to frameFormulaGridCrops.seNumber.AsInteger do
    begin
      if TryStrToFloat(Grid.Cells[Ord(ccStartTime), RowIndex], StartTime)
        and TryStrToFloat(Grid.Cells[Ord(ccEndTime), RowIndex], EndTime) then
      begin
        Rows.Add(RowIndex);
        StartTimes.Add(StartTime);
        EndTimes.Add(EndTime);
      end;
    end;
    for CropIndex := 0 to EfficiencyCollection.Count - 1 do
    begin
      EfficienciesItem := EfficiencyCollection[CropIndex];
      CropEfficiency := EfficienciesItem.CropEfficiency;
      while CropEfficiency.Count > Rows.Count do
      begin
        CropEfficiency.Last.Free;
      end;
      while CropEfficiency.Count < Rows.Count do
      begin
        CropEfficiency.Add;
      end;
      ColIndex := CropIndex + Ord(ccCrop);
      for RowIndex := 0 to Rows.Count - 1 do
      begin
        ARow := Rows[RowIndex];
        EfficiencyItem := CropEfficiency[RowIndex];
        EfficiencyItem.StartTime := StartTimes[RowIndex];
        EfficiencyItem.EndTime := EndTimes[RowIndex];
        EfficiencyItem.Efficiency := Grid.Cells[ColIndex, ARow];
      end;
    end;
  finally
    StartTimes.Free;
    EndTimes.Free;
    Rows.Free;
  end;
end;

procedure TframeFarm.SetCropEfficiencyImprove(Farm: TFarm;
  IrrigationTypes: TIrrigationCollection);
var
  EndTime: Double;
  EfficienciesItem: TFarmEfficienciesItem;
  ColIndex: Integer;
  StartTime: Double;
  Grid: TRbwDataGrid4;
  EfficiencyItem: TCropEfficiencyItem;
  Rows: Generics.Collections.TList<Integer>;
  EfficiencyCollection: TFarmEfficiencyCollection;
  CropEfficiency: TCropEfficiencyCollection;
  StartTimes: Generics.Collections.TList<Double>;
  CropIndex: Integer;
  EndTimes: Generics.Collections.TList<Double>;
  RowIndex: Integer;
  ARow: Integer;
begin
  EfficiencyCollection := Farm.FarmIrrigationEfficiencyImprovementCollection;
  for CropIndex := EfficiencyCollection.Count to IrrigationTypes.Count - 1 do
  begin
    EfficienciesItem := EfficiencyCollection.Add;
    EfficienciesItem.CropEfficiency.CropName := IrrigationTypes[CropIndex].Name;
  end;
  while EfficiencyCollection.Count > IrrigationTypes.Count do
  begin
    EfficiencyCollection.Last.Free;
  end;

  StartTimes := TList<Double>.Create;
  EndTimes := TList<Double>.Create;
  Rows := TList<Integer>.Create;
  try
    Grid := frameFormulaGridEfficiencyImprovement.Grid;
    for RowIndex := 1 to frameFormulaGridEfficiencyImprovement.seNumber.AsInteger do
    begin
      if TryStrToFloat(Grid.Cells[Ord(ccStartTime), RowIndex], StartTime)
        and TryStrToFloat(Grid.Cells[Ord(ccEndTime), RowIndex], EndTime) then
      begin
        Rows.Add(RowIndex);
        StartTimes.Add(StartTime);
        EndTimes.Add(EndTime);
      end;
    end;
    for CropIndex := 0 to EfficiencyCollection.Count - 1 do
    begin
      EfficienciesItem := EfficiencyCollection[CropIndex];
      CropEfficiency := EfficienciesItem.CropEfficiency;
      while CropEfficiency.Count > Rows.Count do
      begin
        CropEfficiency.Last.Free;
      end;
      while CropEfficiency.Count < Rows.Count do
      begin
        CropEfficiency.Add;
      end;
      ColIndex := CropIndex + Ord(ccCrop);
      for RowIndex := 0 to Rows.Count - 1 do
      begin
        ARow := Rows[RowIndex];
        EfficiencyItem := CropEfficiency[RowIndex];
        EfficiencyItem.StartTime := StartTimes[RowIndex];
        EfficiencyItem.EndTime := EndTimes[RowIndex];
        EfficiencyItem.Efficiency := Grid.Cells[ColIndex, ARow];
      end;
    end;
  finally
    StartTimes.Free;
    EndTimes.Free;
    Rows.Free;
  end;
end;

procedure TframeFarm.GetMaxTimeAndCountForCrops(
  var MaxIndex: Integer; var MaxTimeCount: Integer; AFarm: TFarm);
var
  FarmEff: TFarmEfficienciesItem;
  CropIndex: Integer;
begin
  MaxTimeCount := 0;
  Assert(frameFormulaGridCrops.Grid.ColCount = AFarm.CurrentFarmEfficiencyCollection.Count + 2);
  Assert(AFarm.CurrentFarmEfficiencyCollection.Count > 0);
  MaxIndex := -1;
  for CropIndex := 0 to AFarm.CurrentFarmEfficiencyCollection.Count - 1 do
  begin
    FarmEff := AFarm.CurrentFarmEfficiencyCollection[CropIndex];
    if MaxTimeCount <= FarmEff.CropEfficiency.Count then
    begin
      MaxTimeCount := FarmEff.CropEfficiency.Count;
      MaxIndex := CropIndex;
    end;
  end;
end;

end.
