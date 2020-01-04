unit frameScreenObjectMnw1Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectNoParamUnit, Grids,
  RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls, UndoItemsScreenObjects, JvExStdCtrls, JvCombobox,
  JvListComb;

type
  TMnw1Columns = (mcStartTime, mcEndTime, mcDesiredPumpingRate, mcWaterQuality,
    mcConductanceMethod, mcWellRadius, mcConductance, mcSkinFactor,
    mcWaterLevelLimitType, mcLimitingWaterLevel, mcReferenceElevation,
    mcWaterQualityGroup, mcNonLinearLossCoefficient, mcPumpingLimitType,
    mcMinimumActiveRate, mcReactivationPumpingRate);

  TframeScreenObjectMnw1 = class(TframeScreenObjectNoParam)
    edSiteLabel: TEdit;
    lblSite: TLabel;
    comboConductance: TJvImageComboBox;
    comboWaterLevelLimit: TJvImageComboBox;
    comboPumpingLevelLimit: TJvImageComboBox;
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure edSiteLabelChange(Sender: TObject);
    procedure rdgModflowBoundaryEndUpdate(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdeFormulaChange(Sender: TObject);
    procedure comboConductanceChange(Sender: TObject);
    procedure comboWaterLevelLimitChange(Sender: TObject);
    procedure comboPumpingLevelLimitChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    procedure InitializeGrid;
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetChanging(const Value: Boolean);
    procedure ApplyComboTextToColumn(ColIndex: TMnw1Columns; NewText: string);
    { Private declarations }
  protected
    procedure LayoutMultiRowEditControls; override;
  public
    property Changing: Boolean read FChanging write SetChanging;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectMnw1: TframeScreenObjectMnw1;

implementation

uses
  GoPhastTypes, frmGoPhastUnit, ScreenObjectUnit, Generics.Collections,
  ModflowMnw1Unit, Math, frmCustomGoPhastUnit,
  ModflowPackageSelectionUnit;

resourcestring
  StrDesiredPumpingRate = 'Desired pumping rate per cell (Qdes)';
  StrWaterQualityQWval = 'Water quality (QWval)';
  StrConductanceMethod = 'Conductance method (Rw)';
  StrWellRadiusRw = 'Well radius (Rw)';
  StrConductanceRw = 'Conductance (Rw)';
  StrSkinFactorSkin = 'Skin factor (Skin)';
  StrSkinFactorCoefficientB = 'Coefficient B (Skin)';
  StrWaterLevelLimitTy = 'Water level limit type (DD)';
  StrLimitingWaterLevel = 'Limiting water level (Hlim)';
  StrReferenceElevation = 'Reference elevation (Href)';
  StrWaterQualityGroup = 'Water quality group (Iwgrp)';
  StrNonlinearLossCoeff = 'Nonlinear loss coefficient (Cp:C)';
  StrPumpingLimitType = 'Pumping limit type (QCUT, Q-%CUT)';
  StrMinimumActivePumpingRate = 'Minimum active pumping rate (Qfrcmn)';
  StrReactivationPumpingRate = 'Reactivation pumping rate (Qfrcmx)';
  StrWellRadius0 = 'Well radius (>0)';
  StrEqualsHeadInCell = 'Equals head in cell (0)';
  StrConductance0 = 'Conductance (<0)';
  StrAbsoluteDDAbsent = 'Absolute (DD absent)';
  StrRelativeDDPresent = 'Relative (DD present)';
  StrNoLimits = 'No limits';
  StrAbsoluteRatesQCUT = 'Absolute rates (QCUT)';
  StrRelativeRatesQC = 'Relative rates (Q-%CUT:)';

{$R *.dfm}

{ TframeScreenObjectMnw1 }

procedure TframeScreenObjectMnw1.Changed;
begin
  if Assigned(OnChange) then
  begin
    if Changing then
      Exit;
    Changing := True;
    try
      OnChange(self);
    finally
      Changing := False;
    end;
  end;
end;

procedure TframeScreenObjectMnw1.comboConductanceChange(Sender: TObject);
begin
  ApplyComboTextToColumn(mcConductanceMethod, comboConductance.Text);
end;

procedure TframeScreenObjectMnw1.comboPumpingLevelLimitChange(Sender: TObject);
begin
  inherited;
  ApplyComboTextToColumn(mcPumpingLimitType, comboPumpingLevelLimit.Text);
end;

procedure TframeScreenObjectMnw1.comboWaterLevelLimitChange(Sender: TObject);
begin
  inherited;
  ApplyComboTextToColumn(mcWaterLevelLimitType, comboWaterLevelLimit.Text);
end;

procedure TframeScreenObjectMnw1.rdgModflowBoundaryEndUpdate(Sender: TObject);
begin
  inherited;
  Changed;
end;

procedure TframeScreenObjectMnw1.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  Mnw1Col: TMnw1Columns;
  ItemIndex: Integer;
begin
  inherited;
  if (ARow >= rdgModflowBoundary.FixedRows)
    and (ARow < rdgModflowBoundary.RowCount)
    and (ACol >= rdgModflowBoundary.FixedCols)
    and (ACol < rdgModflowBoundary.ColCount)
    then
  begin
    Mnw1Col := TMnw1Columns(ACol);
    case Mnw1Col of
      mcStartTime: ;
      mcEndTime: ;
      mcDesiredPumpingRate: ;
      mcWaterQuality: ;
      mcConductanceMethod: ;
      mcWellRadius, mcSkinFactor:
        begin
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(mcConductanceMethod),ARow];
          CanSelect := (ItemIndex >= 0)
            and (TMnw1ConductanceMethod(ItemIndex) = mcmRadius);
        end;
      mcConductance:
        begin
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(mcConductanceMethod),ARow];
          CanSelect := (ItemIndex >= 0)
            and (TMnw1ConductanceMethod(ItemIndex) = mcmConductance);
        end;
      mcWaterLevelLimitType: ;
      mcLimitingWaterLevel: ;
      mcReferenceElevation: ;
      mcWaterQualityGroup: ;
      mcNonLinearLossCoefficient:
        begin
          CanSelect := mlt1NonLinear in frmGoPhast.PhastModel.Mnw1LossTypes;
        end;
      mcPumpingLimitType: ;
      mcMinimumActiveRate, mcReactivationPumpingRate:
        begin
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(mcPumpingLimitType),ARow];
          CanSelect := (ItemIndex >= 0)
            and (TMnw1PumpingLimitType(ItemIndex) in [mpltAbsolute, mpltPercent]);
        end;
      else Assert(False);
    end;
  end;
end;

procedure TframeScreenObjectMnw1.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if ACol in [Ord(mcConductanceMethod),  Ord(mcPumpingLimitType)]  then
  begin
    rdgModflowBoundary.Invalidate;
  end;
end;

procedure TframeScreenObjectMnw1.edSiteLabelChange(Sender: TObject);
begin
  inherited;
  Changed;
end;

procedure TframeScreenObjectMnw1.GetData(ScreenObjectList: TScreenObjectEditCollection);
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Mnw1List: TList<TMnw1Boundary>;
  FirstMnw1: TMnw1Boundary;
  Mnw1Collection: TMnw1WellCollection;
  ItemIndex: Integer;
  Mnw1Item: TMnw1Item;
  BoundaryIndex: Integer;
  Mnw1Boundary: TMnw1Boundary;
begin
  Changing := True;
  try
    ClearGrid(rdgModflowBoundary);
    InitializeGrid;
    edSiteLabel.Text := '';
    rdgModflowBoundary.LeftCol := 0;

    Mnw1List := TList<TMnw1Boundary>.Create;
    try
      for ObjectIndex := 0 to ScreenObjectList.Count - 1 do
      begin
        AScreenObject := ScreenObjectList[ObjectIndex].ScreenObject;
        if (AScreenObject.ModflowMnw1Boundary <> nil)
          and AScreenObject.ModflowMnw1Boundary.Used then
        begin
          Mnw1List.Add(AScreenObject.ModflowMnw1Boundary);
        end;
      end;

      if Mnw1List.Count = 0 then
      begin
        Exit;
      end;

      FirstMnw1 := Mnw1List[0];

      edSiteLabel.Text := FirstMnw1.Site;

      Mnw1Collection := FirstMnw1.Values as TMnw1WellCollection;
      seNumberOfTimes.AsInteger := Mnw1Collection.Count;
      seNumberOfTimes.OnChange(nil);

      for ItemIndex := 0 to Mnw1Collection.Count - 1 do
      begin
        Mnw1Item := Mnw1Collection[ItemIndex] as TMnw1Item;

        rdgModflowBoundary.RealValue[Ord(mcStartTime),ItemIndex+1] :=
          Mnw1Item.StartTime;
        rdgModflowBoundary.RealValue[Ord(mcEndTime),ItemIndex+1] :=
          Mnw1Item.EndTime;
        rdgModflowBoundary.Cells[Ord(mcDesiredPumpingRate),ItemIndex+1] :=
          Mnw1Item.DesiredPumpingRate;
        rdgModflowBoundary.Cells[Ord(mcWaterQuality),ItemIndex+1] :=
          Mnw1Item.WaterQuality;
        rdgModflowBoundary.ItemIndex[Ord(mcConductanceMethod),ItemIndex+1] :=
          Ord(Mnw1Item.ConductanceMethod);
        rdgModflowBoundary.Cells[Ord(mcWellRadius),ItemIndex+1] :=
          Mnw1Item.WellRadius;
        rdgModflowBoundary.Cells[Ord(mcConductance),ItemIndex+1] :=
          Mnw1Item.Conductance;
        rdgModflowBoundary.Cells[Ord(mcSkinFactor),ItemIndex+1] :=
          Mnw1Item.SkinFactor;
        rdgModflowBoundary.ItemIndex[Ord(mcWaterLevelLimitType),ItemIndex+1] :=
          Ord(Mnw1Item.WaterLevelLimitType);
        rdgModflowBoundary.Cells[Ord(mcLimitingWaterLevel),ItemIndex+1] :=
          Mnw1Item.LimitingWaterLevel;
        rdgModflowBoundary.Cells[Ord(mcReferenceElevation),ItemIndex+1] :=
          Mnw1Item.ReferenceElevation;
        rdgModflowBoundary.Cells[Ord(mcWaterQualityGroup),ItemIndex+1] :=
          Mnw1Item.WaterQualityGroup;
        rdgModflowBoundary.Cells[Ord(mcNonLinearLossCoefficient),ItemIndex+1] :=
          Mnw1Item.NonLinearLossCoefficient;
        rdgModflowBoundary.ItemIndex[Ord(mcPumpingLimitType),ItemIndex+1] :=
          Ord(Mnw1Item.PumpingLimitType);
        rdgModflowBoundary.Cells[Ord(mcMinimumActiveRate),ItemIndex+1] :=
          Mnw1Item.MinimumPumpingRate;
        rdgModflowBoundary.Cells[Ord(mcReactivationPumpingRate),ItemIndex+1] :=
          Mnw1Item.ReactivationPumpingRate;
      end;

      for BoundaryIndex := 1 to Mnw1List.Count - 1 do
      begin
        Mnw1Boundary := Mnw1List[BoundaryIndex];
        if Mnw1Boundary.Site <> edSiteLabel.Text then
        begin
          edSiteLabel.Text := '';
          break;
        end;
      end;

      for BoundaryIndex := 1 to Mnw1List.Count - 1 do
      begin
        Mnw1Boundary := Mnw1List[BoundaryIndex];
        if Mnw1Boundary.Values.Count <> seNumberOfTimes.AsInteger  then
        begin
          ClearGrid(rdgModflowBoundary);
          seNumberOfTimes.AsInteger := 1;
          seNumberOfTimes.OnChange(nil);
          Exit;
        end;
      end;

      for BoundaryIndex := 1 to Mnw1List.Count - 1 do
      begin
        Mnw1Boundary := Mnw1List[BoundaryIndex];
        if not Mnw1Boundary.Values.IsSame(FirstMnw1.Values) then
        begin
          ClearGrid(rdgModflowBoundary);
          Exit;
        end;
      end;

    finally
      Mnw1List.Free;
    end;
  finally
    Changing := False;
  end;
end;

procedure TframeScreenObjectMnw1.InitializeGrid;
var
  PickList: TStrings;
  AColumn: TRbwColumn4;
  ColIndex: Integer;
  Col: TMnw1Columns;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    rdgModflowBoundary.RowCount := 2;

    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
      (rdgModflowBoundary, Ord(mcStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
      (rdgModflowBoundary, Ord(mcEndTime));

    for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.WordWrapCaptions := True;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
    end;

    for Col in [mcDesiredPumpingRate, mcWaterQuality,
      mcWellRadius, mcConductance, mcSkinFactor,
      mcLimitingWaterLevel, mcReferenceElevation,
      mcWaterQualityGroup, mcNonLinearLossCoefficient,
      mcMinimumActiveRate, mcReactivationPumpingRate] do
    begin
      AColumn := rdgModflowBoundary.Columns[Ord(Col)];
      AColumn.ButtonCaption := StrFormulaButtonCaption;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonUsed := True;
    end;

    AColumn := rdgModflowBoundary.Columns[Ord(mcStartTime)];
    AColumn.ComboUsed := True;
    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;

    AColumn := rdgModflowBoundary.Columns[Ord(mcEndTime)];
    AColumn.ComboUsed := True;
    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;

    AColumn := rdgModflowBoundary.Columns[Ord(mcConductanceMethod)];
    AColumn.ComboUsed := True;
    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;
    PickList := AColumn.PickList;
    PickList.Clear;
    PickList.Add(StrWellRadius0);
    PickList.Add(StrEqualsHeadInCell);
    PickList.Add(StrConductance0);
    comboConductance.Items.Assign(PickList);

    AColumn := rdgModflowBoundary.Columns[Ord(mcWaterLevelLimitType)];
    AColumn.ComboUsed := True;
    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;
    PickList := AColumn.PickList;
    PickList.Clear;
    PickList.Add(StrAbsoluteDDAbsent);
    PickList.Add(StrRelativeDDPresent);
    comboWaterLevelLimit.Items.Assign(PickList);

    AColumn := rdgModflowBoundary.Columns[Ord(mcPumpingLimitType)];
    AColumn.ComboUsed := True;
    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;
    PickList := AColumn.PickList;
    PickList.Clear;
    PickList.Add(StrNoLimits);
    PickList.Add(StrAbsoluteRatesQCUT);
    PickList.Add(StrRelativeRatesQC);
    comboPumpingLevelLimit.Items.Assign(PickList);

    rdgModflowBoundary.Cells[Ord(mcStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(mcEndTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(mcDesiredPumpingRate), 0] := StrDesiredPumpingRate;
    rdgModflowBoundary.Cells[Ord(mcWaterQuality), 0] := StrWaterQualityQWval;
    rdgModflowBoundary.Cells[Ord(mcConductanceMethod), 0] := StrConductanceMethod;
    rdgModflowBoundary.Cells[Ord(mcWellRadius), 0] := StrWellRadiusRw;
    rdgModflowBoundary.Cells[Ord(mcConductance), 0] := StrConductanceRw;
    if frmGoPhast.PhastModel.ModflowPackages.Mnw1Package.LossType = mlt1Skin then
    begin
      rdgModflowBoundary.Cells[Ord(mcSkinFactor), 0] := StrSkinFactorSkin;
    end
    else
    begin
      rdgModflowBoundary.Cells[Ord(mcSkinFactor), 0] := StrSkinFactorCoefficientB;
    end;
    rdgModflowBoundary.Cells[Ord(mcWaterLevelLimitType), 0] := StrWaterLevelLimitTy;
    rdgModflowBoundary.Cells[Ord(mcLimitingWaterLevel), 0] := StrLimitingWaterLevel;
    rdgModflowBoundary.Cells[Ord(mcReferenceElevation), 0] := StrReferenceElevation;
    rdgModflowBoundary.Cells[Ord(mcWaterQualityGroup), 0] := StrWaterQualityGroup;
    rdgModflowBoundary.Cells[Ord(mcNonLinearLossCoefficient), 0] := StrNonlinearLossCoeff;
    rdgModflowBoundary.Cells[Ord(mcPumpingLimitType), 0] := StrPumpingLimitType;
    rdgModflowBoundary.Cells[Ord(mcMinimumActiveRate), 0] := StrMinimumActivePumpingRate;
    rdgModflowBoundary.Cells[Ord(mcReactivationPumpingRate), 0] := StrReactivationPumpingRate;
  finally
    rdgModflowBoundary.EndUpdate;
  end;

  rdgModflowBoundary.BeginUpdate;
  try
    for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.AutoAdjustColWidths := False;
      AColumn.AutoAdjustRowHeights := False;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
end;

procedure TframeScreenObjectMnw1.LayoutMultiRowEditControls;
var
  ACol: Integer;
begin
//  inherited;
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  ACol := Max(Ord(mcDesiredPumpingRate), rdgModflowBoundary.LeftCol);
  if ACol in [Ord(mcConductanceMethod), Ord(mcWaterLevelLimitType),
    Ord(mcPumpingLimitType)] then
  begin
    Inc(ACol)
  end;
  LayoutControls(rdgModflowBoundary, rdeFormula, lblFormula, ACol);

  LayoutControls(rdgModflowBoundary, comboConductance, nil,
    Ord(mcConductanceMethod));
  LayoutControls(rdgModflowBoundary, comboWaterLevelLimit, nil,
    Ord(mcWaterLevelLimitType));
  LayoutControls(rdgModflowBoundary, comboPumpingLevelLimit, nil,
    Ord(mcPumpingLimitType));
end;

procedure TframeScreenObjectMnw1.rdeFormulaChange(Sender: TObject);
var
  RowIndex: Integer;
  TempOptions: TGridOptions;
  ColIndex: TMnw1Columns;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex in [mcDesiredPumpingRate, mcWaterQuality,
        mcWellRadius, mcConductance, mcSkinFactor,
        mcLimitingWaterLevel, mcReferenceElevation,
        mcWaterQualityGroup, mcNonLinearLossCoefficient,
        mcMinimumActiveRate, mcReactivationPumpingRate] do
      begin
        if rdgModflowBoundary.IsSelectedCell(Ord(ColIndex), RowIndex) then
        begin
          rdgModflowBoundary.Cells[Ord(ColIndex), RowIndex] := rdeFormula.Text;
          if Assigned(rdgModflowBoundary.OnSetEditText) then
          begin
            rdgModflowBoundary.OnSetEditText(
              rdgModflowBoundary,Ord(ColIndex),RowIndex, rdeFormula.Text);
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectMnw1.SetChanging(const Value: Boolean);
begin
  FChanging := Value;
end;

procedure TframeScreenObjectMnw1.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TMnw1Boundary;
  BoundaryUsed: Boolean;
  Mnw1Collection: TMnw1WellCollection;
  RowIndex: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TMnw1Item;
  AFormula: string;
  ItemIndex: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Boundary := Item.ScreenObject.ModflowMnw1Boundary;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;

    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.Clear;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Boundary = nil then
      begin
        Item.ScreenObject.CreateMnw1Boundary;
        Boundary := Item.ScreenObject.ModflowMnw1Boundary;
      end;

      Boundary.Site := edSiteLabel.Text;

      Mnw1Collection := Boundary.Values as TMnw1WellCollection;
      while Mnw1Collection.Count > seNumberOfTimes.AsInteger do
      begin
        Mnw1Collection.Last.Free;
      end;
      while Mnw1Collection.Count < seNumberOfTimes.AsInteger do
      begin
        Mnw1Collection.Add;
      end;
      for RowIndex := seNumberOfTimes.AsInteger downto 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(mcStartTime), RowIndex], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[Ord(mcEndTime), RowIndex], EndTime) then
        begin
          AnItem := Mnw1Collection.Items[RowIndex-1] as TMnw1Item;
          AnItem.StartTime := StartTime;
          AnItem.EndTime := EndTime;

          AFormula := rdgModflowBoundary.Cells[Ord(mcDesiredPumpingRate), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.DesiredPumpingRate := AFormula;
          end
          else if AnItem.DesiredPumpingRate = '' then
          begin
            AnItem.DesiredPumpingRate := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcWaterQuality), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.WaterQuality := AFormula;
          end
          else if AnItem.WaterQuality = '' then
          begin
            AnItem.WaterQuality := '0';
          end;

          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(mcConductanceMethod), RowIndex];
          if ItemIndex >= 0 then
          begin
            AnItem.ConductanceMethod := TMnw1ConductanceMethod(ItemIndex);
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcWellRadius), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.WellRadius := AFormula;
          end
          else if AnItem.WellRadius = '' then
          begin
            AnItem.WellRadius := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcConductance), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.Conductance := AFormula;
          end
          else if AnItem.Conductance = '' then
          begin
            AnItem.Conductance := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcSkinFactor), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.SkinFactor := AFormula;
          end
          else if AnItem.SkinFactor = '' then
          begin
            AnItem.SkinFactor := '0';
          end;

          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(mcWaterLevelLimitType), RowIndex];
          if ItemIndex >= 0 then
          begin
            AnItem.WaterLevelLimitType := TMnw1WaterLevelLimitType(ItemIndex);
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcLimitingWaterLevel), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.LimitingWaterLevel := AFormula;
          end
          else if AnItem.LimitingWaterLevel = '' then
          begin
            AnItem.LimitingWaterLevel := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcReferenceElevation), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.ReferenceElevation := AFormula;
          end
          else if AnItem.ReferenceElevation = '' then
          begin
            AnItem.ReferenceElevation := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcWaterQualityGroup), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.WaterQualityGroup := AFormula;
          end
          else if AnItem.WaterQualityGroup = '' then
          begin
            AnItem.WaterQualityGroup := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcNonLinearLossCoefficient), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.NonLinearLossCoefficient := AFormula;
          end
          else if AnItem.NonLinearLossCoefficient = '' then
          begin
            AnItem.NonLinearLossCoefficient := '0';
          end;

          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(mcPumpingLimitType), RowIndex];
          if ItemIndex >= 0 then
          begin
            AnItem.PumpingLimitType := TMnw1PumpingLimitType(ItemIndex);
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcMinimumActiveRate), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.MinimumPumpingRate := AFormula;
          end
          else if AnItem.MinimumPumpingRate = '' then
          begin
            AnItem.MinimumPumpingRate := '0';
          end;

          AFormula := rdgModflowBoundary.Cells[Ord(mcReactivationPumpingRate), RowIndex];
          if (AFormula <> '')  then
          begin
            AnItem.ReactivationPumpingRate := AFormula;
          end
          else if AnItem.ReactivationPumpingRate = '' then
          begin
            AnItem.ReactivationPumpingRate := '0';
          end;
        end
        else
        begin
          Mnw1Collection.Items[RowIndex-1].Free;
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMnw1.ApplyComboTextToColumn(ColIndex: TMnw1Columns; NewText: string);
var
  TempOptions: TGridOptions;
  RowIndex: Integer;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount - 1 do
    begin
      if rdgModflowBoundary.IsSelectedCell(Ord(ColIndex), RowIndex) then
      begin
        rdgModflowBoundary.Cells[Ord(ColIndex), RowIndex] := NewText;
        if Assigned(rdgModflowBoundary.OnSetEditText) then
        begin
          rdgModflowBoundary.OnSetEditText(rdgModflowBoundary, Ord(ColIndex), RowIndex, NewText);
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate
  end;
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectMnw1.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

end.
