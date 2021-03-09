unit frameScreenObjectStrUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectCondParamUnit,
  Grids, RbwDataGrid4, ArgusDataEntry, JvExControls, JvxCheckListBox,
  StdCtrls, Buttons, Mask, JvExMask, JvSpin, ExtCtrls,
  UndoItemsScreenObjects, ModflowStrUnit, Generics.Collections;

type
  TStrTimeColumns = (stcStartTime, stcEndTime, stcDownstreamSegment,
    stcDiversionSegment, stcFlow, stcStage, stcCond,
    stcSbot, stcStop, stcWidth, stcSlope, stcRough);

  TframeScreenObjectStr = class(TframeScreenObjectCondParam)
    pnlNumber: TPanel;
    seSegmentNumber: TJvSpinEdit;
    lblSegmentNumber: TLabel;
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seSegmentNumberChange(Sender: TObject);
    procedure clbParametersClick(Sender: TObject);
    procedure comboFormulaInterpChange(Sender: TObject);
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FCalculateStage: Boolean;
    FFirstParamColumn: Integer;
    FGettingData: boolean;
    FOnEdited: TNotifyEvent;
    procedure InitializeControls;
    procedure AssignFirstItem(LocalList: TList<TStrBoundary>);
    function UpdateTimeTable(Boundary: TStrBoundary): boolean;
    procedure UpdateTimeGridCell(Value: string; Column, Row: integer);
    procedure Edited;
    procedure UpdateConductanceCaption;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    { Public declarations }
  end;

var
  frameScreenObjectStr: TframeScreenObjectStr;

implementation

uses
  frmGoPhastUnit, GoPhastTypes, ModflowTransientListParameterUnit,
  OrderedCollectionUnit, frmScreenObjectPropertiesUnit,
  ScreenObjectUnit, ModflowTimeUnit, ModflowBoundaryUnit,
  frameScreenObjectNoParamUnit, frmCustomGoPhastUnit;

resourcestring
  StrOutflowSegmentItr = 'Outflow segment (Itrib)';
  StrDiversionSegmentI = 'Diversion segment (Iupseg)';
  StrFlow = 'Flow';
  StrHead = 'Head';
  StrConductanceCond = 'Conductance (Cond)';
  StrConductanceCondMult = 'Conductance multiplier (Condfact)';
  StrStreambedBottomSb = 'Streambed bottom (Sbot)';
  StrStreambedTopStop = 'Streambed top (Stop)';
  StrStreamWidthWidth = 'Stream width (Width)';
  StrStreamSlopeSlope = 'Stream slope (Slope)';
  StrManningsRoughness = 'Manning’s roughness coefficient (Rough)';
  StrNearest = 'Closest';
  StrF = 'F()';

{$R *.dfm}

{ TframeScreeenObjectStr }

procedure TframeScreenObjectStr.AssignFirstItem(
  LocalList: TList<TStrBoundary>);
const
  StreamConductancePosition = 0;
  StreamBedTopPosition = 1;
  StreamBedBottomPosition = 2;
  StreamFlowPosition = 3;
  StreamStagePosition = 4;
  StreamWidthPosition = 5;
  StreamSlopePosition = 6;
  StreamRoughnessPosition = 7;
  ColumnOffset = 4;
var
  Boundary: TStrBoundary;
  ParamItem: TStrParamItem;
  Item: TStrItem;
  Values: TStrCollection;
  ItemIndex: Integer;
//  Index: Integer;
begin
  Boundary := LocalList[0];
  seSegmentNumber.Enabled := True;
  seSegmentNumber.AsInteger := Boundary.SegmentNumber;
  comboFormulaInterp.ItemIndex := Ord(Boundary.FormulaInterpretation);
  if Boundary.Parameters.Count > 0 then
  begin
    Assert(Boundary.Parameters.Count = 1);
    ParamItem := Boundary.Parameters[0] as TStrParamItem;
    Values := ParamItem.Param as TStrCollection;

//    AParam := ParamItem.Param.ParamName;
    clbParameters.CheckedIndex := clbParameters.Items.IndexOf(ParamItem.Param.ParamName);
  end
  else
  begin
    Values := Boundary.Values as TStrCollection;
    if clbParameters.Items.Count > 0 then
    begin
      clbParameters.CheckedIndex := 0;
    end;
  end;

  seNumberOfTimes.AsInteger := Values.Count;
  for ItemIndex := 0 to Values.Count - 1 do
  begin
    Item := Values[ItemIndex] as TStrItem;
    rdgModflowBoundary.Cells[Ord(stcStartTime), ItemIndex+1+PestRowOffset] :=
      FloatToStr(Item.StartTime);
    rdgModflowBoundary.Cells[Ord(stcEndTime), ItemIndex+1+PestRowOffset] :=
      FloatToStr(Item.EndTime);
    rdgModflowBoundary.Cells[Ord(stcDownstreamSegment), ItemIndex+1+PestRowOffset] :=
      IntToStr(Item.OutflowSegment);
    rdgModflowBoundary.Cells[Ord(stcDiversionSegment), ItemIndex+1+PestRowOffset] :=
      IntToStr(Item.DiversionSegment);
    rdgModflowBoundary.Cells[Ord(stcFlow), ItemIndex+1+PestRowOffset] := Item.Flow;
    rdgModflowBoundary.Cells[Ord(stcStage), ItemIndex+1+PestRowOffset] := Item.Stage;
    rdgModflowBoundary.Cells[Ord(stcCond), ItemIndex+1+PestRowOffset] := Item.Conductance;
    rdgModflowBoundary.Cells[Ord(stcSbot), ItemIndex+1+PestRowOffset] := Item.BedBottom;
    rdgModflowBoundary.Cells[Ord(stcStop), ItemIndex+1+PestRowOffset] := Item.BedTop;
    if FCalculateStage then
    begin
      rdgModflowBoundary.Cells[Ord(stcWidth), ItemIndex+1+PestRowOffset] := Item.Width;
      rdgModflowBoundary.Cells[Ord(stcSlope), ItemIndex+1+PestRowOffset] := Item.Slope;
      rdgModflowBoundary.Cells[Ord(stcRough), ItemIndex+1+PestRowOffset] := Item.Roughness;
    end;
  end;

  {$IFDEF PEST}

  PestMethod[Ord(stcFlow)] := Boundary.PestBoundaryMethod[StreamFlowPosition];
  PestMethod[Ord(stcStage)] := Boundary.PestBoundaryMethod[StreamStagePosition];
  PestMethod[Ord(stcCond)] := Boundary.PestBoundaryMethod[StreamConductancePosition];
  PestMethod[Ord(stcSbot)] := Boundary.PestBoundaryMethod[StreamBedBottomPosition];
  PestMethod[Ord(stcStop)] := Boundary.PestBoundaryMethod[StreamBedTopPosition];
  PestMethod[Ord(stcWidth)] := Boundary.PestBoundaryMethod[StreamWidthPosition];
  PestMethod[Ord(stcSlope)] := Boundary.PestBoundaryMethod[StreamSlopePosition];
  PestMethod[Ord(stcRough)] := Boundary.PestBoundaryMethod[StreamRoughnessPosition];

  PestModifier[Ord(stcFlow)] := Boundary.PestBoundaryFormula[StreamFlowPosition];
  PestModifier[Ord(stcStage)] := Boundary.PestBoundaryFormula[StreamStagePosition];
  PestModifier[Ord(stcCond)] := Boundary.PestBoundaryFormula[StreamConductancePosition];
  PestModifier[Ord(stcSbot)] := Boundary.PestBoundaryFormula[StreamBedBottomPosition];
  PestModifier[Ord(stcStop)] := Boundary.PestBoundaryFormula[StreamBedTopPosition];
  PestModifier[Ord(stcWidth)] := Boundary.PestBoundaryFormula[StreamWidthPosition];
  PestModifier[Ord(stcSlope)] := Boundary.PestBoundaryFormula[StreamSlopePosition];
  PestModifier[Ord(stcRough)] := Boundary.PestBoundaryFormula[StreamRoughnessPosition];

  {$ENDIF}

end;

procedure TframeScreenObjectStr.clbParametersClick(Sender: TObject);
begin
  inherited;
  UpdateConductanceCaption;
  Edited;
end;

procedure TframeScreenObjectStr.comboFormulaInterpChange(Sender: TObject);
begin
  inherited;
  UpdateConductanceCaption;
  Edited;
end;

procedure TframeScreenObjectStr.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if FCalculateStage and (ARow > 0) and (ACol = Ord(stcStage)) then
  begin
    CanSelect := false;
  end;
end;

procedure TframeScreenObjectStr.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectStr.Edited;
begin
  if Assigned(FOnEdited) and not FGettingData then
  begin
    FOnEdited(self);
  end;
end;

procedure TframeScreenObjectStr.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  LocalList: TList<TStrBoundary>;
  Index: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
begin
  ConductanceColumn := Ord(stcCond);
  FGettingData := True;
  try
    InitializeControls;

    LocalList := TList<TStrBoundary>.Create;
    try
      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        if (AScreenObject.ModflowStrBoundary <> nil)
          and (AScreenObject.ModflowStrBoundary.Used) then
        begin
          LocalList.Add(AScreenObject.ModflowStrBoundary)
        end;
      end;
      if LocalList.Count > 0 then
      begin
        AssignFirstItem(LocalList);
        for Index := 1 to LocalList.Count - 1 do
        begin
          if not UpdateTimeTable(LocalList[Index]) then
          begin
            Break;
          end;
        end;
      end;
    finally
      LocalList.Free;
    end;
    UpdateConductanceCaption;
    rdgModflowBoundary.HideEditor;
  finally
    FGettingData := False;
  end;
end;

type TGridCrack = class(TStringGrid);

procedure TframeScreenObjectStr.InitializeControls;
const
  StreamConductancePosition = 0;
  StreamBedTopPosition = 1;
  StreamBedBottomPosition = 2;
  StreamFlowPosition = 3;
  StreamStagePosition = 4;
  StreamWidthPosition = 5;
  StreamSlopePosition = 6;
  StreamRoughnessPosition = 7;
  ColumnOffset = 4;
var
  Parameters: TModflowTransientListParameters;
  ParamIndex: Integer;
  AParameter: TModflowTransientListParameter;
  RowIndex: Integer;
  ColIndex: Integer;
  StartTimes: TStrings;
  EndTimes: TStrings;
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
//  MethodIndex: Integer;
begin
  comboFormulaInterp.ItemIndex := 0;
  Parameters := frmGoPhast.PhastModel.ModflowTransientParameters;
  clbParameters.Clear;
  if Parameters.CountParam(ptSTR) > 0 then
  begin
    if clbParameters.Visible  then
    begin
      pnlTop.ClientHeight := pnlCaption.Height + pnlNumber.Height
        + clbParameters.Height;
    end;
    clbParameters.Visible := True;
    clbParameters.Items.Add(StrNoParameter);
    for ParamIndex := 0 to Parameters.Count - 1 do
    begin
      AParameter := Parameters[ParamIndex];
      if AParameter.ParameterType = ptSTR then
      begin
        clbParameters.Items.AddObject(AParameter.ParameterName, AParameter);
      end;
    end;
    clbParameters.CheckedIndex := 0;
    clbParameters.EnabledItem[0] := False;
  end
  else
  begin
    clbParameters.Visible := False;
    pnlTop.ClientHeight := pnlCaption.Height + pnlNumber.Height;
  end;


  rdgModflowBoundary.Column := 0;
  TGridCrack(rdgModflowBoundary).HideEditor;
  rdgModflowBoundary.Invalidate;
  rdgModflowBoundary.BeginUpdate;
  try
    FCalculateStage := frmGoPhast.PhastModel.ModflowPackages.StrPackage.CalculateStage;
    if FCalculateStage then
    begin
      FFirstParamColumn := Ord(High(TStrTimeColumns))+1;
    end
    else
    begin
      FFirstParamColumn := Ord(stcStop)+1;
    end;
    rdgModflowBoundary.ColCount := FFirstParamColumn;
    rdgModflowBoundary.Cells[Ord(stcStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(stcEndTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(stcDownstreamSegment), 0] := StrOutflowSegmentItr;
    rdgModflowBoundary.Cells[Ord(stcDiversionSegment), 0] := StrDiversionSegmentI;
    rdgModflowBoundary.Cells[Ord(stcFlow), 0] := StrFlow;
    rdgModflowBoundary.Cells[Ord(stcStage), 0] := StrHead;
    rdgModflowBoundary.Cells[Ord(stcCond), 0] := ConductanceCaption(StrConductanceCond);
    rdgModflowBoundary.Cells[Ord(stcSbot), 0] := StrStreambedBottomSb;
    rdgModflowBoundary.Cells[Ord(stcStop), 0] := StrStreambedTopStop;
    if FCalculateStage then
    begin
      rdgModflowBoundary.Cells[Ord(stcWidth), 0] := StrStreamWidthWidth;
      rdgModflowBoundary.Cells[Ord(stcSlope), 0] := StrStreamSlopeSlope;
      rdgModflowBoundary.Cells[Ord(stcRough), 0] := StrManningsRoughness;
    end;

    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := rdgModflowBoundary.FixedCols to
        rdgModflowBoundary.ColCount - 1 do
      begin
        rdgModflowBoundary.Cells[ColIndex, RowIndex] := '';
      end;
    end;
    for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
    begin
      rdgModflowBoundary.ColWidths[ColIndex] := rdgModflowBoundary.DefaultColWidth;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustRowHeights := True;
      rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
    end;

    rdgModflowBoundary.Columns[ord(stcStartTime)].Format := rcf4Real;
    rdgModflowBoundary.Columns[ord(stcEndTime)].Format := rcf4Real;
    rdgModflowBoundary.Columns[ord(stcDownstreamSegment)].Format := rcf4Integer;
    rdgModflowBoundary.Columns[ord(stcDiversionSegment)].Format := rcf4Integer;
    rdgModflowBoundary.Columns[ord(stcFlow)].Format := rcf4String;
    rdgModflowBoundary.Columns[ord(stcStage)].Format := rcf4String;
    rdgModflowBoundary.Columns[ord(stcCond)].Format := rcf4String;
    rdgModflowBoundary.Columns[ord(stcSbot)].Format := rcf4String;
    rdgModflowBoundary.Columns[ord(stcStop)].Format := rcf4String;

    rdgModflowBoundary.Columns[ord(stcDownstreamSegment)].Min := 0;
    rdgModflowBoundary.Columns[ord(stcDownstreamSegment)].CheckMin := True;
    rdgModflowBoundary.Columns[ord(stcDiversionSegment)].Min := 0;
    rdgModflowBoundary.Columns[ord(stcDownstreamSegment)].CheckMin := True;

    if FCalculateStage then
    begin
      rdgModflowBoundary.Columns[ord(stcWidth)].Format := rcf4String;
      rdgModflowBoundary.Columns[ord(stcSlope)].Format := rcf4String;
      rdgModflowBoundary.Columns[ord(stcRough)].Format := rcf4String;
    end;

    StartTimes := rdgModflowBoundary.Columns[Ord(stcStartTime)].PickList;
    StartTimes.Clear;
    EndTimes := rdgModflowBoundary.Columns[Ord(stcEndTime)].PickList;
    EndTimes.Clear;
    for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
    begin
      StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[TimeIndex];
      StartTimes.Add(FloatToStr(StressPeriod.StartTime));
      EndTimes.Add(FloatToStr(StressPeriod.EndTime));
    end;

    for ColIndex := Ord(stcDownstreamSegment) to Ord(stcDiversionSegment) do
    begin
      rdgModflowBoundary.Columns[ColIndex].UseButton := True;
      rdgModflowBoundary.Columns[ColIndex].ButtonCaption := StrNearest;
      rdgModflowBoundary.Columns[ColIndex].ButtonWidth := 70;
      rdgModflowBoundary.ColWidths[ColIndex] := 100;
    end;
    for ColIndex := Ord(stcFlow) to rdgModflowBoundary.ColCount-1 do
    begin
      rdgModflowBoundary.Columns[ColIndex].UseButton := True;
      rdgModflowBoundary.Columns[ColIndex].ButtonCaption := StrF;
      rdgModflowBoundary.Columns[ColIndex].ButtonWidth := 40;
    end;

    {$IFDEF PEST}
    rdgModflowBoundary.Cells[0,PestMethodRow] := StrModificationMethod;
    rdgModflowBoundary.Cells[0,PestModifierRow] := StrPestModifier;

    PestMethod[Ord(stcFlow)] := TStrBoundary.DefaultBoundaryMethod(StreamFlowPosition);
    PestMethod[Ord(stcStage)] := TStrBoundary.DefaultBoundaryMethod(StreamStagePosition);
    PestMethod[Ord(stcCond)] := TStrBoundary.DefaultBoundaryMethod(StreamConductancePosition);
    PestMethod[Ord(stcSbot)] := TStrBoundary.DefaultBoundaryMethod(StreamBedBottomPosition);
    PestMethod[Ord(stcStop)] := TStrBoundary.DefaultBoundaryMethod(StreamBedTopPosition);
    PestMethod[Ord(stcWidth)] := TStrBoundary.DefaultBoundaryMethod(StreamWidthPosition);
    PestMethod[Ord(stcSlope)] := TStrBoundary.DefaultBoundaryMethod(StreamSlopePosition);
    PestMethod[Ord(stcRough)] := TStrBoundary.DefaultBoundaryMethod(StreamRoughnessPosition);
    {$ENDIF}
  finally
    rdgModflowBoundary.EndUpdate
  end;

  for ColIndex := Ord(stcFlow) to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := false;
  end;

end;

procedure TframeScreenObjectStr.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectStr.seSegmentNumberChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectStr.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
const
  StreamConductancePosition = 0;
  StreamBedTopPosition = 1;
  StreamBedBottomPosition = 2;
  StreamFlowPosition = 3;
  StreamStagePosition = 4;
  StreamWidthPosition = 5;
  StreamSlopePosition = 6;
  StreamRoughnessPosition = 7;
  ColumnOffset = 4;
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TStrBoundary;
  BoundaryUsed: Boolean;
  ParamItem: TStrParamItem;
  Values: TStrCollection;
  RowIndex: Integer;
  StartTime: double;
  EndTime: double;
  ItemIndex: Integer;
  StrItem: TStrItem;
  SegNum: Integer;
  Formula: string;
  MethodIndex: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
//    ScreenObject := Item.ScreenObject;
    Boundary := Item.ScreenObject.ModflowStrBoundary;
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
        Item.ScreenObject.CreateStrBoundary;
        Boundary := Item.ScreenObject.ModflowStrBoundary;
      end;
      if comboFormulaInterp.ItemIndex >= 0 then
      begin
        Boundary.FormulaInterpretation :=
          TFormulaInterpretation(comboFormulaInterp.ItemIndex);
      end;
      if seSegmentNumber.Enabled then
      begin
        Boundary.SegmentNumber := seSegmentNumber.AsInteger;
      end;
      if clbParameters.CheckedIndex > 0 then
      begin
        if Boundary.Parameters.Count = 0 then
        begin
          ParamItem := Boundary.Parameters.Add as TStrParamItem;
          if Boundary.Values.Count > 0 then
          begin
            ParamItem.Param.Assign(Boundary.Values);
            Boundary.Values.Clear;
          end;
        end
        else
        begin
          ParamItem := Boundary.Parameters[0] as TStrParamItem;
        end;
        ParamItem.Param.Param := clbParameters.Items.Objects[
          clbParameters.CheckedIndex] as TModflowTransientListParameter;
        Values := ParamItem.Param as TStrCollection;
      end
      else
      begin
        Values := Boundary.Values as TStrCollection;
        if Boundary.Parameters.Count > 0 then
        begin
          ParamItem := Boundary.Parameters[0] as TStrParamItem;
          Values.Assign(ParamItem.Param);
          Boundary.Parameters.Clear;
        end;
      end;
      ItemIndex := 0;
      for RowIndex := 1 to seNumberOfTimes.AsInteger do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[
          Ord(stcStartTime), RowIndex+PestRowOffset], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[
          Ord(stcEndTime), RowIndex+PestRowOffset], EndTime) then
        begin
          if ItemIndex >= Values.Count then
          begin
            StrItem := Values.Add as TStrItem;
          end
          else
          begin
            StrItem := Values[ItemIndex] as TStrItem;
          end;
          Inc(ItemIndex);
          StrItem.StartTime := StartTime;
          StrItem.EndTime := EndTime;

          if TryStrToInt(rdgModflowBoundary.Cells[
            Ord(stcDownstreamSegment), RowIndex+PestRowOffset], SegNum) then
          begin
            StrItem.OutflowSegment := SegNum
          end;

          if TryStrToInt(rdgModflowBoundary.Cells[
            Ord(stcDiversionSegment), RowIndex+PestRowOffset], SegNum) then
          begin
            StrItem.DiversionSegment := SegNum
          end;

          Formula := rdgModflowBoundary.Cells[Ord(stcFlow), RowIndex+PestRowOffset];
          if Formula <> '' then
          begin
            StrItem.Flow := Formula;
          end;

          Formula := rdgModflowBoundary.Cells[Ord(stcStage), RowIndex+PestRowOffset];
          if Formula <> '' then
          begin
            StrItem.Stage := Formula;
          end;

          Formula := rdgModflowBoundary.Cells[Ord(stcCond), RowIndex+PestRowOffset];
          if Formula <> '' then
          begin
            StrItem.Conductance := Formula;
          end;

          Formula := rdgModflowBoundary.Cells[Ord(stcSbot), RowIndex+PestRowOffset];
          if Formula <> '' then
          begin
            StrItem.BedBottom := Formula;
          end;

          Formula := rdgModflowBoundary.Cells[Ord(stcStop), RowIndex+PestRowOffset];
          if Formula <> '' then
          begin
            StrItem.BedTop := Formula;
          end;

          if FCalculateStage then
          begin
            Formula := rdgModflowBoundary.Cells[Ord(stcWidth), RowIndex+PestRowOffset];
            if Formula <> '' then
            begin
              StrItem.Width := Formula;
            end;

            Formula := rdgModflowBoundary.Cells[Ord(stcSlope), RowIndex+PestRowOffset];
            if Formula <> '' then
            begin
              StrItem.Slope := Formula;
            end;

            Formula := rdgModflowBoundary.Cells[Ord(stcRough), RowIndex+PestRowOffset];
            if Formula <> '' then
            begin
              StrItem.Roughness := Formula;
            end;
          end;
        end;
      end;
      Values.Count := ItemIndex;
    end;
    {$IFDEF PEST}
    if rdgModflowBoundary.Cells[Ord(stcFlow),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamFlowPosition] := PestMethod[Ord(stcFlow)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcStage),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamStagePosition] := PestMethod[Ord(stcStage)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcCond),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamConductancePosition] := PestMethod[Ord(stcCond)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcSbot),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamBedBottomPosition] := PestMethod[Ord(stcSbot)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcStop),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamBedTopPosition] := PestMethod[Ord(stcStop)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcWidth),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamWidthPosition] := PestMethod[Ord(stcWidth)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcSlope),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamSlopePosition] := PestMethod[Ord(stcSlope)];
    end;

    if rdgModflowBoundary.Cells[Ord(stcRough),PestMethodRow] <> '' then
    begin
      Boundary.PestBoundaryMethod[StreamRoughnessPosition] := PestMethod[Ord(stcRough)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcFlow),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamFlowPosition] := PestModifier[Ord(stcFlow)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcStage),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamStagePosition] := PestModifier[Ord(stcStage)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcCond),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamConductancePosition] := PestModifier[Ord(stcCond)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcSbot),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamBedBottomPosition] := PestModifier[Ord(stcSbot)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcStop),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamBedTopPosition] := PestModifier[Ord(stcStop)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcWidth),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamWidthPosition] := PestModifier[Ord(stcWidth)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcSlope),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamSlopePosition] := PestModifier[Ord(stcSlope)];
    end;

    if (rdgModflowBoundary.Cells[Ord(stcRough),PestModifierRow] <> '') then
    begin
      Boundary.PestBoundaryFormula[StreamRoughnessPosition] := PestModifier[Ord(stcRough)];
    end;
    {$ENDIF}
  end;
end;

procedure TframeScreenObjectStr.UpdateConductanceCaption;
begin
  if clbParameters.CheckedIndex > 0 then
  begin
    rdgModflowBoundary.Cells[Ord(stcCond), 0] :=
      ConductanceCaption(StrConductanceCondMult);
  end
  else
  begin
    rdgModflowBoundary.Cells[Ord(stcCond), 0] :=
      ConductanceCaption(StrConductanceCond);
  end;
end;

procedure TframeScreenObjectStr.UpdateTimeGridCell(Value: string; Column,
  Row: integer);
begin
  if rdgModflowBoundary.Cells[Column,Row] <> Value then
  begin
    rdgModflowBoundary.Cells[Column,Row] := '';
  end;
end;

function TframeScreenObjectStr.UpdateTimeTable(Boundary: TStrBoundary): boolean;
const
  StreamConductancePosition = 0;
  StreamBedTopPosition = 1;
  StreamBedBottomPosition = 2;
  StreamFlowPosition = 3;
  StreamStagePosition = 4;
  StreamWidthPosition = 5;
  StreamSlopePosition = 6;
  StreamRoughnessPosition = 7;
  ColumnOffset = 4;
var
  ParamItem: TStrParamItem;
  Item: TStrItem;
//  AParam: TModflowTransientListParameter;
  Values: TStrCollection;
  ItemIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewItemIndex: Integer;
  Index: Integer;
begin
  seSegmentNumber.Enabled := False;
  if comboFormulaInterp.ItemIndex <> Ord(Boundary.FormulaInterpretation) then
  begin
    comboFormulaInterp.ItemIndex := -1;
  end;
  if Boundary.Parameters.Count > 0 then
  begin
    Assert(Boundary.Parameters.Count = 1);
    ParamItem := Boundary.Parameters[0] as TStrParamItem;
    Values := ParamItem.Param as TStrCollection;

//    AParam := ParamItem.Param.Param;
    NewItemIndex := clbParameters.Items.IndexOf(ParamItem.Param.ParamName);
  end
  else
  begin
    Values := Boundary.Values as TStrCollection;
    if clbParameters.Items.Count > 0 then
    begin
      NewItemIndex := 0;
    end
    else
    begin
      NewItemIndex := -1;
    end;
  end;

  if clbParameters.CheckedIndex <> NewItemIndex then
  begin
    clbParameters.CheckedIndex := -1;
  end;

  {$IFDEF PEST}
  if rdgModflowBoundary.Cells[Ord(stcFlow),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcFlow)] <> Boundary.PestBoundaryMethod[StreamFlowPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcFlow),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcStage),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcStage)] <> Boundary.PestBoundaryMethod[StreamStagePosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcStage),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcCond),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcCond)] <> Boundary.PestBoundaryMethod[StreamConductancePosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcCond),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcSbot),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcSbot)] <> Boundary.PestBoundaryMethod[StreamBedBottomPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcSbot),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcStop),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcStop)] <> Boundary.PestBoundaryMethod[StreamBedTopPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcStop),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcWidth),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcWidth)] <> Boundary.PestBoundaryMethod[StreamWidthPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcWidth),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcSlope),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcSlope)] <> Boundary.PestBoundaryMethod[StreamSlopePosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcSlope),PestMethodRow] := ''
    end;
  end;

  if rdgModflowBoundary.Cells[Ord(stcRough),PestMethodRow] <> '' then
  begin
    if PestMethod[Ord(stcRough)] <> Boundary.PestBoundaryMethod[StreamRoughnessPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcRough),PestMethodRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcFlow),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcFlow)] <> Boundary.PestBoundaryFormula[StreamFlowPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcFlow),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcStage),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcStage)] <> Boundary.PestBoundaryFormula[StreamStagePosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcStage),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcCond),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcCond)] <> Boundary.PestBoundaryFormula[StreamConductancePosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcCond),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcSbot),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcSbot)] <> Boundary.PestBoundaryFormula[StreamBedBottomPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcSbot),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcStop),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcStop)] <> Boundary.PestBoundaryFormula[StreamBedTopPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcStop),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcWidth),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcWidth)] <> Boundary.PestBoundaryFormula[StreamWidthPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcWidth),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcSlope),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcSlope)] <> Boundary.PestBoundaryFormula[StreamSlopePosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcSlope),PestModifierRow] := ''
    end;
  end;

  if (rdgModflowBoundary.Cells[Ord(stcRough),PestModifierRow] <> strNone) then
  begin
    if PestModifier[Ord(stcRough)] <> Boundary.PestBoundaryFormula[StreamRoughnessPosition] then
    begin
      rdgModflowBoundary.Cells[Ord(stcRough),PestModifierRow] := ''
    end;
  end;
  {$ENDIF}

  if Values.Count = seNumberOfTimes.AsInteger then
  begin
    result := True;
    for ItemIndex := 0 to Values.Count - 1 do
    begin
      Item := Values[ItemIndex] as TStrItem;
      UpdateTimeGridCell(FloatToStr(Item.StartTime),
        Ord(stcStartTime), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(FloatToStr(Item.EndTime),
        Ord(stcEndTime), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(IntToStr(Item.OutflowSegment),
        Ord(stcDownstreamSegment), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(IntToStr(Item.DiversionSegment),
        Ord(stcDiversionSegment), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(Item.Flow, Ord(stcFlow), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(Item.Stage, Ord(stcStage), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(Item.Conductance, Ord(stcCond), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(Item.BedBottom, Ord(stcSbot), ItemIndex+1+PestRowOffset);
      UpdateTimeGridCell(Item.BedTop, Ord(stcStop), ItemIndex+1+PestRowOffset);
      if FCalculateStage then
      begin
        UpdateTimeGridCell(Item.Width, Ord(stcWidth), ItemIndex+1+PestRowOffset);
        UpdateTimeGridCell(Item.Slope, Ord(stcSlope), ItemIndex+1+PestRowOffset);
        UpdateTimeGridCell(Item.Roughness, Ord(stcRough), ItemIndex+1+PestRowOffset);
      end;
    end;
  end
  else
  begin
    result := False;
    for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
      begin
        rdgModflowBoundary.Cells[ColIndex, RowIndex] := '';
      end;
    end;
  end;
end;

end.
