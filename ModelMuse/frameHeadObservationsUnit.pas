{@abstract(@name defines @link(TframeHeadObservations).
@link(TframeHeadObservations)
is used in @link(TfrmScreenObjectProperties) to specify head observations.)

@author(Richard B. Winston <rbwinst@usgs.gov>))
}
unit frameHeadObservationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameCustomCellObservationUnit, ExtCtrls, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, ArgusDataEntry, ComCtrls, JvExComCtrls,
  JvComCtrls, JvExStdCtrls, JvCombobox, JvListComb, UndoItemsScreenObjects;

type
  TframeHeadObservations = class(TframeCustomCellObservation)
    rgMultiObsMethod: TRadioGroup;
    comboMultiStatFlag: TJvImageComboBox;
    procedure comboMultiStatFlagChange(Sender: TObject);
    procedure comboTreatmentChange(Sender: TObject); override;
    procedure seTimesChange(Sender: TObject); override;
    procedure rdeMultiValueEditChange(Sender: TObject);
    procedure rgMultiObsMethodClick(Sender: TObject);
    procedure rdgObservationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean); override;
    procedure edObsNameExit(Sender: TObject);
  private
    { Private declarations }
  protected
     procedure LayoutMultiCellEditControls; override;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean; MissingHeadObsNames: TStringList);
    procedure InitializeControls; override;
    { Public declarations }
  end;

resourcestring
  StrObservedHead = 'Observed  Head';
  StrStatistic = 'Statistic';
  StrStatFlag = 'StatFlag';

implementation

uses
  frmCustomGoPhastUnit, ModflowHobUnit, GoPhastTypes, ModflowBoundaryUnit;


{$R *.dfm}

type
  THeadObsCol = (hocTime, hocHead, hocStatistic, hocStatFlag, hocComment);

{ TframeHeadObservations1 }

procedure TframeHeadObservations.comboMultiStatFlagChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(comboMultiStatFlag.Text, rdgObservations,
    Ord(hocStatFlag), Ord(hocStatFlag));
end;

procedure TframeHeadObservations.comboTreatmentChange(Sender: TObject);
var
  Purpose: TObservationPurpose;
  Index: Integer;
begin
  inherited;
  if comboTreatment.ItemIndex >= 0 then
  begin
    Purpose := TObservationPurpose(comboTreatment.ItemIndex)
  end
  else
  begin
    Purpose := ofObserved;
  end;
  case Purpose of
    ofObserved, ofInacative:
      begin
        rdgObservations.Columns[Ord(hocStatFlag)].PickList := ObservationStatFlagLabels;
  {$IF CompilerVersion > 28}
        comboMultiStatFlag.Items.ClearAndResetID;
  {$ENDIF}
        comboMultiStatFlag.Items.Assign(ObservationStatFlagLabels);
      end;
    ofPredicted:
      begin
        rdgObservations.Columns[Ord(hocStatFlag)].PickList := PredictionStatFlagLabels;
  {$IF CompilerVersion > 28}
        comboMultiStatFlag.Items.ClearAndResetID;
  {$ENDIF}
        comboMultiStatFlag.Items.Assign(PredictionStatFlagLabels);
        for Index := 1 to rdgObservations.RowCount - 1 do
        begin
          if (rdgObservations.Cells[Ord(hocStatFlag), Index] <> '')
            and (rdgObservations.ItemIndex[Ord(hocStatFlag), Index] < 0) then
          begin
            rdgObservations.ItemIndex[Ord(hocStatFlag), Index] := 0;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

procedure TframeHeadObservations.edObsNameExit(Sender: TObject);
begin
  inherited;
  edObsName.Text := string(AnsiString(edObsName.Text));
end;

procedure TframeHeadObservations.GetData(List: TScreenObjectEditCollection);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Observations: THobBoundary;
  FoundFirst: Boolean;
  ItemIndex: Integer;
  HobItem: THobItem;
  LayerItem: TMultiHeadItem;
begin
  InitializeControls;
  rdgObservations.BeginUpdate;
  rdgLayers.BeginUpdate;
  try
    edObsName.Text := '';
    edObsName.Enabled := True;
    edObsName.Color := clWindow;
    ClearGrid(rdgObservations);
    ClearGrid(rdgLayers);
    seTimes.AsInteger := 0;
    seLayers.AsInteger := 0;
    FoundFirst := False;
    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      Observations := Item.ScreenObject.ModflowHeadObservations;
      if (Observations <> nil) and Observations.Used then
      begin
        if not FoundFirst then
        begin
          rgMultiObsMethod.ItemIndex := Ord(Observations.MultiObsMethod);
          edObsName.Text := Observations.ObservationName;
          seTimes.AsInteger := Observations.Values.Count;
          rgMultiObsMethod.Enabled := Observations.Values.Count > 1;
          for ItemIndex := 0 to Observations.Values.Count - 1 do
          begin
            HobItem := Observations.Values.HobItems[ItemIndex];
            rdgObservations.Cells[Ord(hocTime),ItemIndex +1] := FloatToStr(HobItem.Time);
            rdgObservations.Cells[Ord(hocHead),ItemIndex +1] := FloatToStr(HobItem.Head);
            rdgObservations.Cells[Ord(hocStatistic),ItemIndex +1] := FloatToStr(HobItem.Statistic);
            rdgObservations.Cells[Ord(hocStatFlag),ItemIndex +1] :=
              rdgObservations.Columns[Ord(hocStatFlag)].PickList[Ord(HobItem.StatFlag)];
            rdgObservations.Cells[Ord(hocComment),ItemIndex +1] := HobItem.Comment;
          end;
          seLayers.AsInteger := Observations.LayerFractions.Count;
          for ItemIndex := 0 to Observations.LayerFractions.Count - 1 do
          begin
            LayerItem := Observations.LayerFractions.MultiHeadItems[ItemIndex];
            rdgLayers.Cells[Ord(hlLayer),ItemIndex +1] := IntToStr(LayerItem.Layer);
            rdgLayers.Cells[Ord(hlFraction),ItemIndex +1] := FloatToStr(LayerItem.Proportion);
          end;
          comboTreatment.ItemIndex := Ord(Observations.Purpose);
          comboTreatmentChange(nil);
        end
        else
        begin
          edObsName.Text := '';
          edObsName.Enabled := False;
          edObsName.Color := Color;
          if rgMultiObsMethod.ItemIndex <> Ord(Observations.MultiObsMethod) then
          begin
            rgMultiObsMethod.ItemIndex := -1;
          end;
          if Observations.Values.Count > 1 then
          begin
            rgMultiObsMethod.Enabled := True;
          end;
          if seTimes.AsInteger <> Observations.Values.Count then
          begin
            ClearGrid(rdgObservations);
          end
          else
          begin
            for ItemIndex := 0 to Observations.Values.Count - 1 do
            begin
              HobItem := Observations.Values.HobItems[ItemIndex];
              if rdgObservations.Cells[Ord(hocTime),ItemIndex +1]
                <> FloatToStr(HobItem.Time) then
              begin
                rdgObservations.Cells[Ord(hocTime),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgObservations.Cells[Ord(hocHead),ItemIndex +1]
                <> FloatToStr(HobItem.Head) then
              begin
                rdgObservations.Cells[Ord(hocHead),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgObservations.Cells[Ord(hocStatistic),ItemIndex +1]
                <> FloatToStr(HobItem.Statistic) then
              begin
                rdgObservations.Cells[Ord(hocStatistic),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgObservations.Cells[Ord(hocStatFlag),ItemIndex +1]
                <> rdgObservations.Columns[Ord(hocStatFlag)].
                PickList[Ord(HobItem.StatFlag)] then
              begin
                rdgObservations.Cells[Ord(hocStatFlag),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgObservations.Cells[Ord(hocComment),ItemIndex +1]
                <> HobItem.Comment then
              begin
                rdgObservations.Cells[Ord(hocComment),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
            end;
          end;
          if seLayers.AsInteger <> Observations.LayerFractions.Count then
          begin
            ClearGrid(rdgLayers);
          end
          else
          begin
            for ItemIndex := 0 to Observations.LayerFractions.Count - 1 do
            begin
              LayerItem := Observations.LayerFractions.MultiHeadItems[ItemIndex];
              if rdgLayers.Cells[Ord(hlLayer),ItemIndex +1]
                <> IntToStr(LayerItem.Layer) then
              begin
                rdgLayers.Cells[Ord(hlLayer),ItemIndex +1] := '';
//                ClearGrid(rdgLayers);
//                break;
              end;
              if rdgLayers.Cells[Ord(hlFraction),ItemIndex +1]
                <> FloatToStr(LayerItem.Proportion) then
              begin
                rdgLayers.Cells[Ord(hlFraction),ItemIndex +1] := '';
//                ClearGrid(rdgLayers);
//                break;
              end;
            end;
          end;
          if comboTreatment.ItemIndex <> Ord(Observations.Purpose) then
          begin
            comboTreatment.ItemIndex := -1;
            comboTreatmentChange(nil);
          end;
        end;
        FoundFirst := True;
      end;
    end;
  finally
    rdgObservations.EndUpdate;
    rdgLayers.EndUpdate;
  end;
  FChanged := False;
  FTimesCountChanged := False;
  FLayerCountChanged := False;
end;

procedure TframeHeadObservations.InitializeControls;
var
  Column: TRbwColumn4;
  Index: Integer;
begin
  inherited;
  rdgObservations.Columns[Ord(hocStatFlag)].PickList := ObservationStatFlagLabels;
  {$IF CompilerVersion > 28}
  comboMultiStatFlag.Items.ClearAndResetID;
  {$ENDIF}
  comboMultiStatFlag.Items.Assign(ObservationStatFlagLabels);
  Column := rdgObservations.Columns[Ord(hocStatFlag)];
  Assert(comboMultiStatFlag.Items.Count = Column.PickList.Count);
  for Index := 0 to Column.PickList.Count - 1 do
  begin
    comboMultiStatFlag.Items[Index].Text := Column.PickList[Index];
  end;

  rdgObservations.Cells[Ord(hocTime),0] := StrTime;
  rdgObservations.Cells[Ord(hocHead),0] := StrObservedHead;
  rdgObservations.Cells[Ord(hocStatistic),0] := StrStatistic;
  rdgObservations.Cells[Ord(hocStatFlag),0] := StrStatFlag;
  rdgObservations.Cells[Ord(hocComment),0] := StrComment;

  rgMultiObsMethod.ItemIndex := 1;
end;

procedure TframeHeadObservations.LayoutMultiCellEditControls;
var
  Index: Integer;
  AColVisible: Boolean;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  AColVisible := False;
  for Index := Ord(hocTime) to Ord(hocStatistic) do
  begin
    if rdgObservations.ColVisible[Index] then
    begin
      LayoutControls(rdgObservations, rdeMultiValueEdit, nil, Index);
      AColVisible := True;
      break;
    end;
  end;
  if not AColVisible then
  begin
    LayoutControls(rdgObservations, rdeMultiValueEdit, nil, 0);
  end;
  LayoutControls(rdgObservations, comboMultiStatFlag, nil, Ord(hocStatFlag));
end;

procedure TframeHeadObservations.rdeMultiValueEditChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(rdeMultiValueEdit.Text, rdgObservations,
    Ord(hocTime), Ord(hocStatistic));
end;

procedure TframeHeadObservations.rdgObservationsSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  EnableMultiEditControl(rdgObservations, rdeMultiValueEdit,
    [Ord(hocTime), Ord(hocHead), Ord(hocStatistic)]);
  EnableMultiEditControl(rdgObservations, comboMultiStatFlag,
    Ord(hocStatFlag));
end;

procedure TframeHeadObservations.rgMultiObsMethodClick(Sender: TObject);
begin
  inherited;
  FChanged := True;
end;

procedure TframeHeadObservations.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean; MissingHeadObsNames: TStringList);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Observations: THobBoundary;
  ObservationUsed: boolean;
  ValueCount: integer;
  RowIndex: Integer;
  Head, Time, Statistic: double;
  ObsHead: THobItem;
  Layer: integer;
  Fraction: double;
  ObsLayer: TMultiHeadItem;
  NewComment: string;
begin
  MissingHeadObsNames.Clear;
  if not FChanged then
  begin
    Exit;
  end;
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Observations := Item.ScreenObject.ModflowHeadObservations;
    ObservationUsed := (Observations <> nil) and Observations.Used;

    if ClearAll then
    begin
      if ObservationUsed then
      begin
        Observations.Clear;
      end;
    end
    else if SetAll or ObservationUsed then
    begin
      if Observations = nil then
      begin
        Item.ScreenObject.CreateHeadObservations;
        Observations := Item.ScreenObject.ModflowHeadObservations;
      end;

      if edObsName.Text <> '' then
      begin
        Observations.ObservationName := edObsName.Text;
      end;
      if Observations.ObservationName = '' then
      begin
        MissingHeadObsNames.AddObject(Item.ScreenObject.Name, Item.ScreenObject);
      end;
      if rgMultiObsMethod.ItemIndex >= 0 then
      begin
        Observations.MultiObsMethod :=
          TMultiObsMethod(rgMultiObsMethod.ItemIndex);
      end;
      if comboTreatment.ItemIndex >= 0 then
      begin
        Observations.Purpose :=
          TObservationPurpose(comboTreatment.ItemIndex);
      end;
      ValueCount := 0;
      for RowIndex := 1 to seTimes.AsInteger do
      begin
        if (TryStrToFloat(rdgObservations.Cells[Ord(hocTime), RowIndex], Time)
          or (rdgObservations.Cells[Ord(hocTime), RowIndex] = ''))
          and (TryStrToFloat(rdgObservations.Cells[Ord(hocHead), RowIndex], Head)
          or (rdgObservations.Cells[Ord(hocHead), RowIndex] = '')) then
        begin
          Inc(ValueCount);
        end;
      end;
      if FTimesCountChanged then
      begin
        while Observations.Values.Count < ValueCount do
        begin
          Observations.Values.Add;
        end;
        while Observations.Values.Count > ValueCount do
        begin
          Observations.Values.Delete(Observations.Values.Count-1);
        end;
      end;
      ValueCount := 0;
      for RowIndex := 1 to seTimes.AsInteger do
      begin
        if (TryStrToFloat(rdgObservations.Cells[Ord(hocTime), RowIndex], Time)
          or (rdgObservations.Cells[Ord(hocTime), RowIndex] = ''))
          and (TryStrToFloat(rdgObservations.Cells[Ord(hocHead), RowIndex], Head)
          or (rdgObservations.Cells[Ord(hocHead), RowIndex] = '')) then
        begin
          if ValueCount < Observations.Values.Count then
          begin
            ObsHead := Observations.Values.HobItems[ValueCount];
            if (rdgObservations.Cells[Ord(hocTime), RowIndex] <> '') then
            begin
              ObsHead.Time := Time;
            end;
            if (rdgObservations.Cells[Ord(hocHead), RowIndex] <> '') then
            begin
              ObsHead.Head := Head;
            end;
            if TryStrToFloat(rdgObservations.Cells[Ord(hocStatistic), RowIndex], Statistic) then
            begin
              ObsHead.Statistic := Statistic;
            end;
            if (rdgObservations.Cells[Ord(hocStatFlag), RowIndex] <> '') then
            begin
              ObsHead.StatFlag := TStatFlag(rdgObservations.Columns[Ord(hocStatFlag)].
                PickList.IndexOf(rdgObservations.Cells[Ord(hocStatFlag), RowIndex]));
            end;
            NewComment := rdgObservations.Cells[Ord(hocComment), RowIndex];
            if (List.Count = 1) or (NewComment <> '') then
            begin
              ObsHead.Comment := NewComment;
            end;
          end;
          Inc(ValueCount);
        end;
      end;

      ValueCount := 0;
      for RowIndex := 1 to seLayers.AsInteger do
      begin
        if TryStrToInt(rdgLayers.Cells[Ord(hlLayer), RowIndex], Layer)
          and TryStrToFloat(rdgLayers.Cells[Ord(hlFraction), RowIndex], Fraction) then
        begin
          Inc(ValueCount);
        end;
      end;
      if FLayerCountChanged then
      begin
        while Observations.LayerFractions.Count < ValueCount do
        begin
          Observations.LayerFractions.Add;
        end;
        while Observations.LayerFractions.Count > ValueCount do
        begin
          Observations.LayerFractions.Delete(Observations.LayerFractions.Count-1);
        end;
      end;
      ValueCount := 0;
      for RowIndex := 1 to rdgLayers.RowCount - 1 do
      begin
        if TryStrToInt(rdgLayers.Cells[Ord(hlLayer), RowIndex], Layer)
          and TryStrToFloat(rdgLayers.Cells[Ord(hlFraction), RowIndex], Fraction) then
        begin
          if ValueCount < Observations.LayerFractions.Count then
          begin
            ObsLayer := Observations.LayerFractions.MultiHeadItems[ValueCount];
            ObsLayer.Layer := Layer;
            ObsLayer.Proportion := Fraction;
          end;
          Inc(ValueCount);
        end;
      end;
    end;
  end;
end;

procedure TframeHeadObservations.seTimesChange(Sender: TObject);
begin
  inherited;
  rgMultiObsMethod.Enabled := seTimes.AsInteger > 1;
end;

end.
