unit frameSutraGeneralizeTransBoundaryUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomSutraFeatureUnit,
  Vcl.StdCtrls, Vcl.Grids, RbwDataGrid4, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ArgusDataEntry, UndoItemsScreenObjects,
  SutraGenTransBoundUnit, ScreenObjectUnit;

type
  TGenTransCol = (gtcTime, gtcUsed, gtcU1, gtcQU1, gtcU2, gtcQU2);

  TframeSutraGeneralizeTransBoundary = class(TframeCustomSutraTimeVaryingFeature)
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    lblGeneralizedTransportPresent: TLabel;
    comboGeneralizedTransportPresent: TComboBox;
    lbl1: TLabel;
    comboLakeGeneralizedTransportType: TComboBox;
    procedure rdgSutraFeatureSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgSutraFeatureBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure rdgSutraFeatureColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgSutraFeatureHorizontalScroll(Sender: TObject);
    procedure rdgSutraFeatureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgSutraFeatureSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FBoundariesTheSame: Boolean;
    procedure InitializeColumns;
    procedure LayoutMultiEditControls;
    procedure GetScheduleName(BoundaryList: TSutraGeneralTransBoundaryList);
    procedure GetLakeInteractions(BoundaryList: TSutraGeneralTransBoundaryList);
    procedure GetBoundaryValues(BoundaryList: TSutraGeneralTransBoundaryList);
    procedure SetBoundaryValues(BoundValues: TSutraGeneralTransportCollection);
    procedure DisplayBoundaries(BoundColl: TSutraGeneralTransportCollection);
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
  frameSutraGeneralizeTransBoundary: TframeSutraGeneralizeTransBoundary;

implementation

uses
  frmCustomGoPhastUnit, SutraTimeScheduleUnit, AdjustSutraBoundaryValuesUnit,
  SutraBoundariesUnit, System.Generics.Collections, SutraOptionsUnit;

resourcestring
  StrTime = 'Time';
  StrUsedIUBG = 'Used (IUBG)';
  Str1stConcOrTempU = '1''st Conc or Temp (UBG1)';
  Str1stSoluteOrEnerg = '1''st Solute or Energy Flow (QUBG1)';
  Str2ndConcOrTempU = '2''nd Conc or Temp (UBG2)';
  Str2ndSoluteOrEnerg = '2''nd Solute or Energy Flow (QUBG2)';

{$R *.dfm}

{ TframeSutraGeneralizeTransBoundary }

procedure TframeSutraGeneralizeTransBoundary.btnDeleteClick(Sender: TObject);
begin
  inherited;
//
end;

procedure TframeSutraGeneralizeTransBoundary.btnInsertClick(Sender: TObject);
begin
  inherited;
//
end;


procedure TframeSutraGeneralizeTransBoundary.DisplayBoundaries(
  BoundColl: TSutraGeneralTransportCollection);
var
  ItemIndex: Integer;
  Item: TSutraGenTransportItem;
begin
//  FDisplayingData := True;
  rdgSutraFeature.BeginUpdate;
  try
    seNumberOfTimes.AsInteger := BoundColl.Count;
    rdgSutraFeature.RowCount := BoundColl.Count+1;
    for ItemIndex := 0 to BoundColl.Count - 1 do
    begin
      Item := BoundColl[ItemIndex] as TSutraGenTransportItem;
      rdgSutraFeature.Cells[Ord(gtcTime),ItemIndex+1] := FloatToStr(Item.StartTime);
      rdgSutraFeature.Cells[Ord(gtcUsed),ItemIndex+1] := Item.UsedFormula;
      rdgSutraFeature.Cells[Ord(gtcU1),ItemIndex+1] := Item.LowerUFormula;
      rdgSutraFeature.Cells[Ord(gtcQU1),ItemIndex+1] := Item.LowerFlowUFormula;
      rdgSutraFeature.Cells[Ord(gtcU2),ItemIndex+1] := Item.HigherUFormula;
      rdgSutraFeature.Cells[Ord(gtcQU2),ItemIndex+1] := Item.HigherFlowUFormula;
    end;
  finally
    rdgSutraFeature.EndUpdate;
//    FDisplayingData := False;
  end;
end;

procedure TframeSutraGeneralizeTransBoundary.GetBoundaryValues(
  BoundaryList: TSutraGeneralTransBoundaryList);
var
  FirstBoundary: TSutraGeneralTransportBoundary;
  Same: Boolean;
  BoundColl: TSutraGeneralTransportCollection;
  Index: Integer;
  ABoundary: TSutraGeneralTransportBoundary;
  ASchedule: TSutraTimeSchedule;
begin
  FirstBoundary := BoundaryList[0];
  BoundColl := FirstBoundary.Values as TSutraGeneralTransportCollection;
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
end;

procedure TframeSutraGeneralizeTransBoundary.GetData(
  ScreenObjects: TScreenObjectEditCollection);
var
  BoundaryList: TSutraGeneralTransBoundaryList;
  index: Integer;
  SutraBoundaries: TSutraBoundaries;
  ABoundary: TSutraGeneralTransportBoundary;
begin
  rdgSutraFeature.Columns[Ord(gtcUsed)].Format := rcf4String;
  rdgSutraFeature.Columns[Ord(gtcUsed)].ButtonUsed := True;
  rdgSutraFeature.Columns[Ord(gtcUsed)].ButtonCaption := 'F()';
  rdgSutraFeature.Columns[Ord(gtcUsed)].ButtonWidth := 35;
  comboGeneralizedTransportPresent.ItemIndex := Ord(lbiNoChange);
  comboLakeGeneralizedTransportType.ItemIndex := Ord(gtitSoluteSource);

  rdgSutraFeature.BeginUpdate;
  try
    inherited;
    ClearData;
    InitializeColumns;

    BoundaryList := TSutraGeneralTransBoundaryList.Create;
    try
      for index := 0 to ScreenObjects.Count - 1 do
      begin
        SutraBoundaries := ScreenObjects[index].ScreenObject.SutraBoundaries;
        ABoundary := SutraBoundaries.GenTransportBoundary;
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

procedure TframeSutraGeneralizeTransBoundary.GetLakeInteractions(
  BoundaryList: TSutraGeneralTransBoundaryList);
var
  Same: Boolean;
  FirstBoundary: TSutraGeneralTransportBoundary;
  Index: Integer;
  ABoundary: TSutraGeneralTransportBoundary;
  LakeInteraction: TLakeBoundaryInteraction;
  LakeInteractionType: TGeneralizedTransportInteractionType;
begin
  FirstBoundary := BoundaryList[0];
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
    comboGeneralizedTransportPresent.ItemIndex := Ord(LakeInteraction);
  end
  else
  begin
    comboGeneralizedTransportPresent.ItemIndex := 1
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
    comboLakeGeneralizedTransportType.ItemIndex := Ord(LakeInteractionType);
  end
  else
  begin
    comboLakeGeneralizedTransportType.ItemIndex := 1
  end;

end;

procedure TframeSutraGeneralizeTransBoundary.GetScheduleName(
  BoundaryList: TSutraGeneralTransBoundaryList);
var
  ScheduleName: AnsiString;
  Same: Boolean;
  ABoundColl: TSutraGeneralTransportCollection;
  BoundColl: TSutraGeneralTransportCollection;
  Index: Integer;
  FirstBoundary: TSutraGeneralTransportBoundary;
  ABoundary: TSutraGeneralTransportBoundary;
begin
  FirstBoundary := BoundaryList[0];
  BoundColl := FirstBoundary.Values as TSutraGeneralTransportCollection;
  ScheduleName := BoundColl.ScheduleName;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    ABoundColl := ABoundary.Values as TSutraGeneralTransportCollection;
    Same := ScheduleName = ABoundColl.ScheduleName;
    if not Same then
    begin
      Break;
    end;
  end;
  SetScheduleIndex(ScheduleName, Same);
end;

procedure TframeSutraGeneralizeTransBoundary.InitializeColumns;
var
  ColIndex: Integer;
  ColFormat: TGenTransCol;
  AColumn: TRbwColumn4;
begin
  for ColIndex := 0 to rdgSutraFeature.ColCount - 1 do
  begin
    ColFormat := TGenTransCol(ColIndex);
    AColumn := rdgSutraFeature.Columns[ColIndex];
    AColumn.AutoAdjustColWidths := True;
    AColumn.AutoAdjustRowHeights := True;
    AColumn.WordWrapCaptions := True;
    if ColFormat = gtcTime then
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

  rdgSutraFeature.Cells[Ord(gtcTime), 0] := StrTime;
  rdgSutraFeature.Cells[Ord(gtcUsed), 0] := StrUsedIUBG;
  rdgSutraFeature.Cells[Ord(gtcU1), 0] := Str1stConcOrTempU;
  rdgSutraFeature.Cells[Ord(gtcQU1), 0] := Str1stSoluteOrEnerg;
  rdgSutraFeature.Cells[Ord(gtcU2), 0] := Str2ndConcOrTempU;
  rdgSutraFeature.Cells[Ord(gtcQU2), 0] := Str2ndSoluteOrEnerg;
end;

procedure TframeSutraGeneralizeTransBoundary.LayoutMultiEditControls;
var
  FirstVisibleFormulaCol: TGenTransCol;
  ColIndex: TGenTransCol;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;

  FirstVisibleFormulaCol := gtcUsed;
  for ColIndex := FirstVisibleFormulaCol to High(TGenTransCol) do
  begin
    if rdgSutraFeature.ColVisible[Ord(ColIndex)] then
    begin
      FirstVisibleFormulaCol := ColIndex;
      break;
    end;
  end;
  LayoutControls(rdgSutraFeature, rdeFormula, lblFormula, Ord(FirstVisibleFormulaCol));

end;

procedure TframeSutraGeneralizeTransBoundary.rdeFormulaChange(Sender: TObject);
var
  Col: TGenTransCol;
begin
  inherited;
  for Col in [gtcUsed, gtcU1, gtcQU1, gtcU2, gtcQU2] do
  begin
    ChangeSelectedCellsInColumn(rdgSutraFeature, Ord(Col), rdeFormula.Text);
  end;
end;

procedure TframeSutraGeneralizeTransBoundary.rdgSutraFeatureBeforeDrawCell(
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

procedure TframeSutraGeneralizeTransBoundary.rdgSutraFeatureColSize(
  Sender: TObject; ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TframeSutraGeneralizeTransBoundary.rdgSutraFeatureHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TframeSutraGeneralizeTransBoundary.rdgSutraFeatureMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(rdgSutraFeature, rdeFormula, [Ord(gtcU1),
    Ord(gtcQU1), Ord(gtcU2), Ord(gtcQU2)]);
end;

procedure TframeSutraGeneralizeTransBoundary.rdgSutraFeatureSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if not rdgSutraFeature.Drawing then
  begin
    LayoutMultiEditControls;
  end;

end;

procedure TframeSutraGeneralizeTransBoundary.rdgSutraFeatureSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if Value <> '' then
  begin
    seNumberOfTimes.AsInteger := rdgSutraFeature.RowCount -1;
  end;
end;

procedure TframeSutraGeneralizeTransBoundary.seNumberOfTimesChange(
  Sender: TObject);
begin
  inherited;
  UpdateCheckState;
end;

procedure TframeSutraGeneralizeTransBoundary.SetBoundaryValues(
  BoundValues: TSutraGeneralTransportCollection);
var
  ColIndex: Integer;
//  AssocItem: TCustomSutraAssociatedBoundaryItem;
  ItemIndex: Integer;
  RowIndex: Integer;
  BoundItem: TSutraGenTransportItem;
  ATime: Extended;
  OK: Boolean;
  StartIndex: Integer;
//  Initialtime: Double;
//  BoundaryTypeString: string;
begin
  if seNumberOfTimes.AsInteger > 0 then
  begin
//    Initialtime := frmGoPhast.PhastModel.SutraTimeOptions.InitialTime;
    ItemIndex := 0;
    for RowIndex := 1 to seNumberOfTimes.AsInteger do
    begin
      if TryStrToFloat(rdgSutraFeature.Cells[0, RowIndex], ATime) then
      begin
        OK := False;
        StartIndex := 1;
        for ColIndex := StartIndex to rdgSutraFeature.ColCount - 1 do
        begin
          OK := rdgSutraFeature.Cells[ColIndex, RowIndex] <> '';
          if not OK then
          begin
            Break;
          end;
        end;
        if OK then
        begin
          if ItemIndex < BoundValues.Count then
          begin
            BoundItem := BoundValues.Items[ItemIndex] as TSutraGenTransportItem;
          end
          else
          begin
            BoundItem := BoundValues.Add as TSutraGenTransportItem;
          end;
//          if ATime <= Initialtime then
//          begin
//            BoundaryTypeString := StrSUTRAGeneralFlowB;
//            frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
//              StrInvalidBoundaryTim,
//              Format(StrInSTheFirstSpe, [BoundaryTypeString])
//              );
//            frmErrorsAndWarnings.Show;
//          end;
          BoundItem.StartTime := ATime;
          BoundItem.UsedFormula := rdgSutraFeature.Cells[Ord(gtcUsed), RowIndex];
          BoundItem.LowerUFormula := rdgSutraFeature.Cells[Ord(gtcU1), RowIndex];
          BoundItem.LowerFlowUFormula := rdgSutraFeature.Cells[Ord(gtcQU1),ItemIndex+1];
          BoundItem.HigherUFormula := rdgSutraFeature.Cells[Ord(gtcU2),ItemIndex+1];
          BoundItem.HigherFlowUFormula := rdgSutraFeature.Cells[Ord(gtcQU2),ItemIndex+1];
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

procedure TframeSutraGeneralizeTransBoundary.SetData(
  ScreenObjects: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  BoundaryList: TSutraGeneralTransBoundaryList;
  index: integer;
  SutraBoundaries: TSutraBoundaries;
  ABoundary: TSutraGeneralTransportBoundary;
  LocalScreenObjects: TList<TScreenObject>;
  BoundValues: TSutraGeneralTransportCollection;
begin
  inherited;
  LocalScreenObjects := TList<TScreenObject>.Create;
  BoundaryList := TSutraGeneralTransBoundaryList.Create;
  try
    for index := 0 to ScreenObjects.Count - 1 do
    begin
//      ABoundary := nil;
      SutraBoundaries := ScreenObjects[index].ScreenObject.SutraBoundaries;
      ABoundary := SutraBoundaries.GenTransportBoundary;
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
      BoundValues := ABoundary.Values as TSutraGeneralTransportCollection;

      if comboSchedule.ItemIndex > 0 then
      begin
        BoundValues.ScheduleName := AnsiString(comboSchedule.Text);
      end
      else
      begin
        BoundValues.ScheduleName := '';
      end;

      if comboSchedule.ItemIndex > 0 then
      begin
        BoundValues.ScheduleName := AnsiString(comboSchedule.Text);
      end
      else
      begin
        BoundValues.ScheduleName := '';
      end;

      if comboGeneralizedTransportPresent.ItemIndex >= 0 then
      begin
        ABoundary.LakeInteraction :=
          TLakeBoundaryInteraction(comboGeneralizedTransportPresent.ItemIndex);
      end;

      if comboLakeGeneralizedTransportType.ItemIndex >= 0 then
      begin
        ABoundary.LakeInteractionType :=
          TGeneralizedTransportInteractionType(comboLakeGeneralizedTransportType.ItemIndex);
      end;

      SetBoundaryValues(BoundValues);
    end;

  finally
    BoundaryList.Free;
    LocalScreenObjects.Free;
  end;
end;

function TframeSutraGeneralizeTransBoundary.UsedColumn: Integer;
begin
  Result := Ord(gtcUsed);
end;

end.
