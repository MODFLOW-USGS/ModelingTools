unit frameSutraGeneralizedFlowBoundaryUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomSutraFeatureUnit,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.Grids, RbwDataGrid4, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, JvExStdCtrls, JvCombobox, JvListComb,
  UndoItemsScreenObjects, SutraGeneralBoundaryUnit;

type
  TGenFlowCol = (gfcTime, gfcUsed, gfcPress1, gfcFlow1, gfcPress2, gfcFlow2,
    gfcLimit1, gfcLimit2, gfcInflowU, gfcOutflowType, gfcOutflowU);

  TframeSutraGeneralizedFlowBoundary = class(TframeCustomSutraTimeVaryingFeature)
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    comboLimit: TJvImageComboBox;
    comboExit: TJvImageComboBox;
    lblGeneralizedFlowPresent: TLabel;
    comboGeneralizedFlowPresent: TComboBox;
    lblLakeGeneralizedFlowType: TLabel;
    comboLakeGeneralizedFlowType: TComboBox;
    cbBCTime: TCheckBox;
    procedure rdgSutraFeatureSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgSutraFeatureBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure rdgSutraFeatureColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgSutraFeatureHorizontalScroll(Sender: TObject);
    procedure comboLimitChange(Sender: TObject);
    procedure comboExitChange(Sender: TObject);
    procedure rdgSutraFeatureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgSutraFeatureSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure cbBCTimeClick(Sender: TObject);
  private
    FBoundariesTheSame: Boolean;
    procedure InitializeColumns;
    procedure GetScheduleName(BoundaryList: TSutraGeneralFlowBoundaryList);
    procedure GetLakeInteractions(BoundaryList: TSutraGeneralFlowBoundaryList);
    procedure GetBoundaryValues(BoundaryList: TSutraGeneralFlowBoundaryList);
    procedure SetBoundaryValues(BoundValues: TSutraGeneralFlowCollection);
    procedure DisplayBoundaries(BoundColl: TSutraGeneralFlowCollection);
    procedure LayoutMultiEditControls;
    { Private declarations }
  protected
    function UsedColumn: Integer; override;
  public
    procedure GetData(ScreenObjects: TScreenObjectEditCollection); override;
    procedure SetData(ScreenObjects: TScreenObjectEditCollection; SetAll,
      ClearAll: boolean); override;
    { Public declarations }
  end;

var
  frameSutraGeneralizedFlowBoundary: TframeSutraGeneralizedFlowBoundary;

implementation

uses
  SutraBoundaryUnit, SutraBoundariesUnit, SutraTimeScheduleUnit,
  AdjustSutraBoundaryValuesUnit, ScreenObjectUnit, System.Generics.Collections,
  frmGoPhastUnit, frmErrorsAndWarningsUnit, GoPhastTypes, frmCustomGoPhastUnit,
  SutraOptionsUnit;

resourcestring
  StrSUTRAGeneralFlowB = 'SUTRA General Flow Boundary';
  StrTime = 'Time';
  StrUsedIPBG = 'Used (IPBG)';
  StrPressure1PBG1 = 'Pressure 1 (PBG1)';
  StrFlow1QPBG1 = 'Flow 1 (QPBG1)';
  StrPressure2PBG2 = 'Pressure 2 (PBG2)';
  StrFlow2QPBG2 = 'Flow 2 (QPBG2)';
  StrLimit1CPQL1 = 'Limit 1 (CPQL1)';
  StrLimit2CPQL2 = 'Limit 2 (CPQL2)';
  StrInflowUUPBGI = 'Inflow U (UPBGI)';
  StrOutflowUSpecificat = 'Outflow U Specification Method (CUPBGO)';
  StrOutflowUUPBGO = 'Outflow U (UPBGO)';

{$R *.dfm}

{ TframeSutraGeneralizedFlowBoundary }

procedure TframeSutraGeneralizedFlowBoundary.btnDeleteClick(Sender: TObject);
begin
  inherited;
//
end;

procedure TframeSutraGeneralizedFlowBoundary.btnInsertClick(Sender: TObject);
begin
  inherited;
//
end;

procedure TframeSutraGeneralizedFlowBoundary.cbBCTimeClick(Sender: TObject);
begin
  inherited;
  cbBCTime.AllowGrayed := False;
end;

procedure TframeSutraGeneralizedFlowBoundary.comboExitChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgSutraFeature, Ord(gfcOutflowType), comboExit.Text);
end;

procedure TframeSutraGeneralizedFlowBoundary.comboLimitChange(Sender: TObject);
var
  Col: TGenFlowCol;
begin
  inherited;
  for Col in [gfcLimit1, gfcLimit2] do
  begin
    ChangeSelectedCellsInColumn(rdgSutraFeature, Ord(Col), comboLimit.Text);
  end;
//  TGenFlowCol = (gfcTime, gfcUsed, gfcPress1, gfcFlow1, gfcPress2, gfcFlow2,
//    gfcLimit1, gfcLimit2, gfcInflowU, gfcOutflowType, gfcOutflowU);
end;

procedure TframeSutraGeneralizedFlowBoundary.DisplayBoundaries(
  BoundColl: TSutraGeneralFlowCollection);
var
  ItemIndex: Integer;
  Item: TSutraGeneralFlowItem;
begin
//  FDisplayingData := True;
  rdgSutraFeature.BeginUpdate;
  try
    seNumberOfTimes.AsInteger := BoundColl.Count + PestRowOffset;
    rdgSutraFeature.RowCount := BoundColl.Count+1 + PestRowOffset;

    {$IFDEF PEST}
    rdgSutraFeature.Cells[Ord(gfcTime), PestModifierRow] := StrPestModifier;
    rdgSutraFeature.Cells[Ord(gfcTime), PestMethodRow] := StrModificationMethod;
    {$ENDIF}

    for ItemIndex := 0 to BoundColl.Count - 1 do
    begin
      Item := BoundColl[ItemIndex] as TSutraGeneralFlowItem;
      rdgSutraFeature.Cells[Ord(gfcTime),ItemIndex+1+PestRowOffset] := FloatToStr(Item.StartTime);
      rdgSutraFeature.Cells[Ord(gfcUsed),ItemIndex+1+PestRowOffset] := Item.UsedFormula;
      rdgSutraFeature.Cells[Ord(gfcPress1),ItemIndex+1+PestRowOffset] := Item.LowerPressureFormula;
      rdgSutraFeature.Cells[Ord(gfcFlow1),ItemIndex+1+PestRowOffset] := Item.LowerFlowRateFormula;
      rdgSutraFeature.Cells[Ord(gfcPress2),ItemIndex+1+PestRowOffset] := Item.HigherPressureFormula;
      rdgSutraFeature.Cells[Ord(gfcFlow2),ItemIndex+1+PestRowOffset] := Item.HigherFlowRateFormula;
      rdgSutraFeature.ItemIndex[Ord(gfcLimit1),ItemIndex+1+PestRowOffset] := Ord(Item.LowerLimitType);
      rdgSutraFeature.ItemIndex[Ord(gfcLimit2),ItemIndex+1+PestRowOffset] := Ord(Item.UpperLimitType);
      rdgSutraFeature.Cells[Ord(gfcInflowU),ItemIndex+1+PestRowOffset] := Item.UInFormula;
      rdgSutraFeature.ItemIndex[Ord(gfcOutflowType),ItemIndex+1+PestRowOffset] := Ord(Item.ExitSpecMethod);
      rdgSutraFeature.Cells[Ord(gfcOutflowU),ItemIndex+1+PestRowOffset] := Item.UoutFormula;
    end;
  finally
    rdgSutraFeature.EndUpdate;
//    FDisplayingData := False;
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.GetBoundaryValues(
  BoundaryList: TSutraGeneralFlowBoundaryList);
var
  FirstBoundary: TSutraGeneralFlowBoundary;
  Same: Boolean;
  BoundColl: TSutraGeneralFlowCollection;
  Index: Integer;
  ABoundary: TSutraGeneralFlowBoundary;
  ASchedule: TSutraTimeSchedule;
  Columns: TGenericIntegerList;
  FormulaIndexes: TGenericIntegerList;
  BoundaryIndex: Integer;
begin
  FirstBoundary := BoundaryList[0];
  cbBCTime.Checked := FirstBoundary.UseBCTime;
  BoundColl := FirstBoundary.Values as TSutraGeneralFlowCollection;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    Same := BoundColl.isSame(ABoundary.Values);
    if not Same then
    begin
      Break;
    end;
  end;
  FBoundariesTheSame := Same;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    if cbBCTime.Checked <> ABoundary.UseBCTime then
    begin
      cbBCTime.State := cbGrayed;
    end;
  end;

  if Same then
  begin
    if comboSchedule.ItemIndex >= 1 then
    begin
      ASchedule := comboSchedule.Items.Objects[comboSchedule.ItemIndex]
        as TSutraTimeSchedule;

      AdjustBoundaryValues(ASchedule, BoundColl);
    end;
//    CheckSchedule(BoundaryList);
    DisplayBoundaries(BoundColl);
  end
  else
  begin
    ClearBoundaries;
  end;

  {$IFDEF PEST}
  Columns := TGenericIntegerList.Create;
  FormulaIndexes := TGenericIntegerList.Create;
  try
    Columns.Add(Ord(gfcPress1));
    FormulaIndexes.Add(LowerPressurePosition);
    Columns.Add(Ord(gfcFlow1));
    FormulaIndexes.Add(LowerFlowRatePosition);
    Columns.Add(Ord(gfcPress2));
    FormulaIndexes.Add(HigherPressurePosition);
    Columns.Add(Ord(gfcFlow2));
    FormulaIndexes.Add(HigherFlowRatePosition);
    Columns.Add(Ord(gfcInflowU));
    FormulaIndexes.Add(UInPosition);
    Columns.Add(Ord(gfcOutflowU));
    FormulaIndexes.Add(UOutPosition);

    for Index := 0 to Columns.Count - 1 do
    begin
      PestMethod[Columns[Index]] :=
        TSutraGeneralFlowBoundary.DefaultBoundaryMethod(FormulaIndexes[Index]);

      if BoundaryList.Count > 0 then
      begin
        FirstBoundary := BoundaryList[0];

        Same := True;
        for BoundaryIndex := 1 to BoundaryList.Count - 1 do
        begin
          ABoundary := BoundaryList[BoundaryIndex];
          Same := FirstBoundary.PestBoundaryFormula[FormulaIndexes[Index]]
            = ABoundary.PestBoundaryFormula[FormulaIndexes[Index]];
          if not Same then
          begin
            Break;
          end;
        end;
        if Same then
        begin
          PestModifier[Columns[Index]] :=
            FirstBoundary.PestBoundaryFormula[FormulaIndexes[Index]];
        end
        else
        begin
          PestModifierAssigned[Columns[Index]] := False;
        end;

        Same := True;
        for BoundaryIndex := 1 to BoundaryList.Count - 1 do
        begin
          ABoundary := BoundaryList[BoundaryIndex];
          Same := FirstBoundary.PestBoundaryMethod[FormulaIndexes[Index]]
            = ABoundary.PestBoundaryMethod[FormulaIndexes[Index]];
          if not Same then
          begin
            Break;
          end;
        end;
        if Same then
        begin
          PestMethod[Columns[Index]] :=
            FirstBoundary.PestBoundaryMethod[FormulaIndexes[Index]];
        end
        else
        begin
          PestMethodAssigned[Columns[Index]] := False;
        end;
      end;

    end;

  finally
    FormulaIndexes.Free;
    Columns.Free;
  end;
  {$ENDIF}
end;

procedure TframeSutraGeneralizedFlowBoundary.GetData(
  ScreenObjects: TScreenObjectEditCollection);
var
  BoundaryList: TSutraGeneralFlowBoundaryList;
  index: Integer;
  SutraBoundaries: TSutraBoundaries;
  ABoundary: TSutraGeneralFlowBoundary;
begin
  rdgSutraFeature.Columns[Ord(gfcUsed)].Format := rcf4String;
  rdgSutraFeature.Columns[Ord(gfcUsed)].ButtonUsed := True;
  rdgSutraFeature.Columns[Ord(gfcUsed)].ButtonCaption := 'F()';
  rdgSutraFeature.Columns[Ord(gfcUsed)].ButtonWidth := 35;

  comboGeneralizedFlowPresent.ItemIndex := Ord(lbiUseDefaults);
  comboLakeGeneralizedFlowType.ItemIndex := Ord(gfitUseDefaults);

  rdgSutraFeature.BeginUpdate;
  try
    inherited;
    ClearData;
    InitializeColumns;

    BoundaryList := TSutraGeneralFlowBoundaryList.Create;
    try
      for index := 0 to ScreenObjects.Count - 1 do
      begin
        SutraBoundaries := ScreenObjects[index].ScreenObject.SutraBoundaries;
        ABoundary := SutraBoundaries.GeneralFlowBoundary;
        if (ABoundary <> nil) and ABoundary.Used then
        begin
          BoundaryList.Add(ABoundary);
        end;
      end;

      if BoundaryList.Count = 0 then
      begin
        FCheckState := cbUnchecked;
      end
      else if ScreenObjects.Count = BoundaryList.Count then
      begin
        FCheckState := cbChecked;
      end
      else
      begin
        FCheckState := cbGrayed;
      end;
      if Assigned(OnActivate) then
      begin
        OnActivate(self, FCheckState);
      end;

      if BoundaryList.Count = 0 then
      begin
        Exit;
      end;

      cbBCTime.AllowGrayed := BoundaryList.Count > 1;

      GetScheduleName(BoundaryList);
      GetLakeInteractions(BoundaryList);
      GetBoundaryValues(BoundaryList);

    finally
      BoundaryList.Free;
    end;
  finally
    rdgSutraFeature.EndUpdate;
  end;
  LayoutMultiEditControls;
end;

procedure TframeSutraGeneralizedFlowBoundary.GetLakeInteractions(
  BoundaryList: TSutraGeneralFlowBoundaryList);
var
//  ScheduleName: AnsiString;
  Same: Boolean;
  FirstBoundary: TSutraGeneralFlowBoundary;
//  ABoundColl: TSutraGeneralFlowCollection;
//  BoundColl: TSutraGeneralFlowCollection;
  Index: Integer;
  ABoundary: TSutraGeneralFlowBoundary;
  LakeInteraction: TLakeBoundaryInteraction;
  LakeInteractionType: TGeneralizedFlowInteractionType;
begin
  FirstBoundary := BoundaryList[0];
//  BoundColl := FirstBoundary.Values as TSutraGeneralFlowCollection;
  LakeInteraction := FirstBoundary.LakeInteraction;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    Same := LakeInteraction = ABoundary.LakeInteraction;
    if not Same then
    begin
      Break;
    end;
  end;
  if Same then
  begin
    comboGeneralizedFlowPresent.ItemIndex := Ord(LakeInteraction);
  end
  else
  begin
    comboGeneralizedFlowPresent.ItemIndex := -1
  end;

  LakeInteractionType := FirstBoundary.LakeInteractionType;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    Same := LakeInteractionType = ABoundary.LakeInteractionType;
    if not Same then
    begin
      Break;
    end;
  end;
  if Same then
  begin
    comboLakeGeneralizedFlowType.ItemIndex := Ord(LakeInteractionType);
  end
  else
  begin
    comboLakeGeneralizedFlowType.ItemIndex := 1
  end;

end;

procedure TframeSutraGeneralizedFlowBoundary.GetScheduleName(
  BoundaryList: TSutraGeneralFlowBoundaryList);
var
  ScheduleName: AnsiString;
  Same: Boolean;
  FirstBoundary: TSutraGeneralFlowBoundary;
  ABoundColl: TSutraGeneralFlowCollection;
  BoundColl: TSutraGeneralFlowCollection;
  Index: Integer;
  ABoundary: TSutraGeneralFlowBoundary;
begin
  FirstBoundary := BoundaryList[0];
  BoundColl := FirstBoundary.Values as TSutraGeneralFlowCollection;
  ScheduleName := BoundColl.ScheduleName;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    ABoundColl := ABoundary.Values as TSutraGeneralFlowCollection;
    Same := ScheduleName = ABoundColl.ScheduleName;
    if not Same then
    begin
      Break;
    end;
  end;
  SetScheduleIndex(ScheduleName, Same);
end;

procedure TframeSutraGeneralizedFlowBoundary.InitializeColumns;
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
  Limits: TStringList;
  ExitSpec: TStringList;
  ColFormat: TGenFlowCol;
  ItemIndex: Integer;
begin
  Limits := TStringList.Create;
  ExitSpec := TStringList.Create;
  try
    for ItemIndex := 0 to comboLimit.Items.Count - 1 do
    begin
      Limits.Add(comboLimit.Items[ItemIndex].Text)
    end;
    for ItemIndex := 0 to comboExit.Items.Count - 1 do
    begin
      ExitSpec.Add(comboExit.Items[ItemIndex].Text)
    end;
    for ColIndex := 0 to rdgSutraFeature.ColCount - 1 do
    begin
      ColFormat := TGenFlowCol(ColIndex);
      AColumn := rdgSutraFeature.Columns[ColIndex];
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.WordWrapCaptions := True;
      if ColFormat in [gfcLimit1, gfcLimit2] then
      begin
        AColumn.ComboUsed := True;
        AColumn.PickList := Limits;
        AColumn.Format := rcf4String;
        AColumn.LimitToList := True;
      end
      else if ColFormat = gfcOutflowType then
      begin
        AColumn.ComboUsed := True;
        AColumn.PickList := ExitSpec;
        AColumn.Format := rcf4String;
        AColumn.LimitToList := True;
      end
      else if ColFormat = gfcTime then
      begin
        AColumn.Format := rcf4Real;
      end
      else
      begin
        AColumn.Format := rcf4String;
        AColumn.ButtonUsed := True;
        AColumn.ButtonCaption := 'F()';
        AColumn.ButtonWidth := 40;
      end;
    end;
  finally
    Limits.Free;
    ExitSpec.Free;
  end;

  rdgSutraFeature.Cells[Ord(gfcTime), 0] := StrTime;
  rdgSutraFeature.Cells[Ord(gfcUsed), 0] := StrUsedIPBG;
  rdgSutraFeature.Cells[Ord(gfcPress1), 0] := StrPressure1PBG1;
  rdgSutraFeature.Cells[Ord(gfcFlow1), 0] := StrFlow1QPBG1;
  rdgSutraFeature.Cells[Ord(gfcPress2), 0] := StrPressure2PBG2;
  rdgSutraFeature.Cells[Ord(gfcFlow2), 0] := StrFlow2QPBG2;
  rdgSutraFeature.Cells[Ord(gfcLimit1), 0] := StrLimit1CPQL1;
  rdgSutraFeature.Cells[Ord(gfcLimit2), 0] := StrLimit2CPQL2;
  rdgSutraFeature.Cells[Ord(gfcInflowU), 0] := StrInflowUUPBGI;
  rdgSutraFeature.Cells[Ord(gfcOutflowType), 0] := StrOutflowUSpecificat;
  rdgSutraFeature.Cells[Ord(gfcOutflowU), 0] := StrOutflowUUPBGO;
end;

procedure TframeSutraGeneralizedFlowBoundary.LayoutMultiEditControls;
var
  FirstVisibleFormulaCol: TGenFlowCol;
  ColIndex: TGenFlowCol;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;

  FirstVisibleFormulaCol := gfcUsed;
  for ColIndex := FirstVisibleFormulaCol to High(TGenFlowCol) do
  begin
    if ColIndex in [gfcLimit1, gfcLimit2, gfcOutflowType] then
    begin
      Continue;
    end;
    if rdgSutraFeature.ColVisible[Ord(ColIndex)] then
    begin
      FirstVisibleFormulaCol := ColIndex;
      break;
    end;
  end;
  LayoutControls(rdgSutraFeature, rdeFormula, lblFormula, Ord(FirstVisibleFormulaCol));

  if rdgSutraFeature.ColVisible[Ord(gfcLimit1)] then
  begin
    comboLimit.Visible := True;
    LayoutControls(rdgSutraFeature, comboLimit, nil, Ord(gfcLimit1));
  end
  else if rdgSutraFeature.ColVisible[Ord(gfcLimit2)] then
  begin
    comboLimit.Visible := True;
    LayoutControls(rdgSutraFeature, comboLimit, nil, Ord(gfcLimit2));
  end
  else
  begin
    comboLimit.Visible := False;
  end;
  if rdgSutraFeature.ColVisible[Ord(gfcOutflowType)] then
  begin
    comboExit.Visible := True;
    LayoutControls(rdgSutraFeature, comboExit, nil, Ord(gfcOutflowType));
  end
  else
  begin
    comboExit.Visible := False;
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.rdeFormulaChange(Sender: TObject);
var
  Col: TGenFlowCol;
begin
  inherited;
  for Col in [gfcUsed, gfcPress1, gfcFlow1, gfcPress2, gfcFlow2, gfcInflowU, gfcOutflowU] do
  begin
    ChangeSelectedCellsInColumn(rdgSutraFeature, Ord(Col), rdeFormula.Text);
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.rdgSutraFeatureBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  CanSelect: Boolean;
begin
  inherited;
  CanSelect := True;
  rdgSutraFeatureSelectCell(Sender, ACol, ARow, CanSelect);
  if not CanSelect then
  begin
    rdgSutraFeature.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.rdgSutraFeatureColSize(
  Sender: TObject; ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TframeSutraGeneralizedFlowBoundary.rdgSutraFeatureHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiEditControls
end;

procedure TframeSutraGeneralizedFlowBoundary.rdgSutraFeatureMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(rdgSutraFeature, rdeFormula, [Ord(gfcUsed), Ord(gfcPress1),
    Ord(gfcFlow1), Ord(gfcPress2), Ord(gfcFlow2), Ord(gfcInflowU),
    Ord(gfcOutflowU)]);
  EnableMultiEditControl(rdgSutraFeature, comboLimit, [Ord(gfcLimit1),
    Ord(gfcLimit2)]);
  EnableMultiEditControl(rdgSutraFeature, comboExit, Ord(gfcOutflowType));
end;

procedure TframeSutraGeneralizedFlowBoundary.rdgSutraFeatureSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if not rdgSutraFeature.Drawing then
  begin
    LayoutMultiEditControls;
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.rdgSutraFeatureSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if Value <> '' then
  begin
    seNumberOfTimes.AsInteger := rdgSutraFeature.RowCount -1 - PestRowOffset;
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.seNumberOfTimesChange(
  Sender: TObject);
begin
  inherited;
  UpdateCheckState;
end;

procedure TframeSutraGeneralizedFlowBoundary.SetBoundaryValues(
  BoundValues: TSutraGeneralFlowCollection);
var
  ColIndex: Integer;
//  AssocItem: TCustomSutraAssociatedBoundaryItem;
  ItemIndex: Integer;
  RowIndex: Integer;
  BoundItem: TSutraGeneralFlowItem;
  ATime: Extended;
  OK: Boolean;
  StartIndex: Integer;
  UsedRow: Integer;
//  Initialtime: Double;
//  BoundaryTypeString: string;
begin
  if seNumberOfTimes.AsInteger > 0 then
  begin
//    Initialtime := frmGoPhast.PhastModel.SutraTimeOptions.InitialTime;
    ItemIndex := 0;
    for RowIndex := 1 to seNumberOfTimes.AsInteger do
    begin
      UsedRow := RowIndex+PestRowOffset;
      if TryStrToFloat(rdgSutraFeature.Cells[0, UsedRow], ATime) then
      begin
        OK := False;
        StartIndex := Ord(gfcUsed);
        for ColIndex := StartIndex to rdgSutraFeature.ColCount - 1 do
        begin
          OK := rdgSutraFeature.Cells[ColIndex, UsedRow] <> '';
          if not OK then
          begin
            Break;
          end;
        end;
        if OK then
        begin
          if ItemIndex < BoundValues.Count then
          begin
            BoundItem := BoundValues.Items[ItemIndex] as TSutraGeneralFlowItem;
          end
          else
          begin
            BoundItem := BoundValues.Add as TSutraGeneralFlowItem;
          end;
          BoundItem.StartTime := ATime;
          BoundItem.UsedFormula := rdgSutraFeature.Cells[Ord(gfcUsed), UsedRow];
          BoundItem.LowerPressureFormula := rdgSutraFeature.Cells[Ord(gfcPress1), UsedRow];
          BoundItem.LowerPressureFormula := rdgSutraFeature.Cells[Ord(gfcPress1),UsedRow];
          BoundItem.LowerFlowRateFormula := rdgSutraFeature.Cells[Ord(gfcFlow1),UsedRow];
          BoundItem.HigherPressureFormula := rdgSutraFeature.Cells[Ord(gfcPress2),UsedRow];
          BoundItem.HigherFlowRateFormula := rdgSutraFeature.Cells[Ord(gfcFlow2),UsedRow];
          BoundItem.LowerLimitType := TSutraLimitType(rdgSutraFeature.ItemIndex[Ord(gfcLimit1),UsedRow]);
          BoundItem.UpperLimitType := TSutraLimitType(rdgSutraFeature.ItemIndex[Ord(gfcLimit2),UsedRow]);
          BoundItem.UInFormula := rdgSutraFeature.Cells[Ord(gfcInflowU),UsedRow];
          BoundItem.ExitSpecMethod := TSutraExitSpecificationMethod(rdgSutraFeature.ItemIndex[Ord(gfcOutflowType),UsedRow]);
          BoundItem.UoutFormula := rdgSutraFeature.Cells[Ord(gfcOutflowU),UsedRow];
          Inc(ItemIndex);
        end;
      end;
      while BoundValues.Count > ItemIndex do
      begin
        BoundValues.Delete(BoundValues.Count - 1);
      end;
    end;
  end;
end;

procedure TframeSutraGeneralizedFlowBoundary.SetData(
  ScreenObjects: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  BoundaryList: TSutraGeneralFlowBoundaryList;
  index: integer;
  SutraBoundaries: TSutraBoundaries;
  ABoundary: TSutraGeneralFlowBoundary;
  LocalScreenObjects: TList<TScreenObject>;
  BoundValues: TSutraGeneralFlowCollection;
  Columns: TGenericIntegerList;
  FormulaIndexes: TGenericIntegerList;
  BoundaryIndex: Integer;
begin
  inherited;
  LocalScreenObjects := TList<TScreenObject>.Create;
  BoundaryList := TSutraGeneralFlowBoundaryList.Create;
  try
    for index := 0 to ScreenObjects.Count - 1 do
    begin
//      ABoundary := nil;
      SutraBoundaries := ScreenObjects[index].ScreenObject.SutraBoundaries;
      ABoundary := SutraBoundaries.GeneralFlowBoundary;
      if ClearAll then
      begin
        ABoundary.Values.Clear;
      end
      else if SetAll or ABoundary.Used then
      begin
        BoundaryList.Add(ABoundary);
        LocalScreenObjects.Add(ScreenObjects[index].ScreenObject);
      end;
    end;

    for index := 0 to BoundaryList.Count - 1 do
    begin
      ABoundary := BoundaryList[index];
      BoundValues := ABoundary.Values as TSutraGeneralFlowCollection;

      if comboSchedule.ItemIndex > 0 then
      begin
        BoundValues.ScheduleName := AnsiString(comboSchedule.Text);
      end
      else
      begin
        BoundValues.ScheduleName := '';
      end;

      if comboGeneralizedFlowPresent.ItemIndex >= 0 then
      begin
        ABoundary.LakeInteraction :=
          TLakeBoundaryInteraction(comboGeneralizedFlowPresent.ItemIndex);
      end;

      if comboLakeGeneralizedFlowType.ItemIndex >= 0 then
      begin
        ABoundary.LakeInteractionType :=
          TGeneralizedFlowInteractionType(comboLakeGeneralizedFlowType.ItemIndex);
      end;

      SetBoundaryValues(BoundValues);

      if cbBCTime.State <> cbGrayed then
      begin
        ABoundary.UseBCTime := cbBCTime.Checked;
      end;
    end;

    {$IFDEF PEST}
    Columns := TGenericIntegerList.Create;
    FormulaIndexes := TGenericIntegerList.Create;
    try
      Columns.Add(Ord(gfcPress1));
      FormulaIndexes.Add(LowerPressurePosition);
      Columns.Add(Ord(gfcFlow1));
      FormulaIndexes.Add(LowerFlowRatePosition);
      Columns.Add(Ord(gfcPress2));
      FormulaIndexes.Add(HigherPressurePosition);
      Columns.Add(Ord(gfcFlow2));
      FormulaIndexes.Add(HigherFlowRatePosition);
      Columns.Add(Ord(gfcInflowU));
      FormulaIndexes.Add(UInPosition);
      Columns.Add(Ord(gfcOutflowU));
      FormulaIndexes.Add(UOutPosition);

      for Index := 0 to Columns.Count - 1 do
      begin
        if BoundaryList.Count > 0 then
        begin
          for BoundaryIndex := 0 to BoundaryList.Count - 1 do
          begin
            ABoundary := BoundaryList[BoundaryIndex];
            if PestModifierAssigned[Columns[Index]] then
            begin
              ABoundary.PestBoundaryFormula[FormulaIndexes[Index]] :=
                PestModifier[Columns[Index]];
            end;
            if PestMethodAssigned[Columns[Index]] then
            begin
              ABoundary.PestBoundaryMethod[FormulaIndexes[Index]] :=
                PestMethod[Columns[Index]];
            end;
          end;
        end;
      end;

    finally
      FormulaIndexes.Free;
      Columns.Free;
    end;
    {$ENDIF}
  finally
    BoundaryList.Free;
    LocalScreenObjects.Free;
  end;
end;

function TframeSutraGeneralizedFlowBoundary.UsedColumn: Integer;
begin
  result := Ord(gfcUsed);
end;

end.
