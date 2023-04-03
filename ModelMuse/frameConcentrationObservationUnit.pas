unit frameConcentrationObservationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameCustomCellObservationUnit, ExtCtrls, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, ArgusDataEntry, ComCtrls, JvExComCtrls,
  JvComCtrls, UndoItemsScreenObjects;

type
  TframeConcentrationObservation = class(TframeCustomCellObservation)
    comboSpeciesNames: TComboBox;
    procedure comboSpeciesNamesChange(Sender: TObject);
    procedure rdeMultiValueEditChange(Sender: TObject);
    procedure rdgObservationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean); override;
    procedure rdgObservationsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    { Private declarations }
  protected
     procedure LayoutMultiCellEditControls; override;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    procedure InitializeControls; override;
    { Public declarations }
  end;

type
  TConcObsCol = (cocObsType, cocTime, cocSpecies, cocConcentration, cocWeight, cocComment);

var
  frameConcentrationObservation: TframeConcentrationObservation;

implementation

uses
  frmGoPhastUnit, Mt3dmsChemSpeciesUnit, frmCustomGoPhastUnit, Mt3dmsTobUnit,
  GoPhastTypes, frmErrorsAndWarningsUnit, ModflowBoundaryUnit;

resourcestring
  StrSpecies = 'Species (iComp)';
  StrConcentration = 'Concentration (COBS)';
  StrWeight = 'Weight';
  StrTimeTimeObs = 'Time or frequency (TimeObs)';
  StrObservationMethod = 'Observation method';
  StrWeightPrLayer = 'Weight (prLayer)';

{$R *.dfm}

{ TframeConcentrationObservation }

procedure TframeConcentrationObservation.comboSpeciesNamesChange(
  Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(comboSpeciesNames.Text, rdgObservations,
    Ord(cocSpecies), Ord(cocSpecies));
end;

procedure TframeConcentrationObservation.GetData(
  List: TScreenObjectEditCollection);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  FoundFirst: Boolean;
  ItemIndex: Integer;
  Observations: TMt3dmsTransObservations;
  TobItem: TMt3dmsTobItem;
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
      Observations := Item.ScreenObject.Mt3dmsTransObservations;
      if (Observations <> nil) and Observations.Used then
      begin
        if not FoundFirst then
        begin
          edObsName.Text := Observations.ObservationName;
          seTimes.AsInteger := Observations.Values.Count;
          for ItemIndex := 0 to Observations.Values.Count - 1 do
          begin
            TobItem := Observations.Values.TobItems[ItemIndex];
            rdgObservations.ItemIndex[Ord(cocObsType),ItemIndex +1]
              := Ord(TobItem.ObservationType);
            rdgObservationsSetEditText(rdgObservations, Ord(cocObsType),ItemIndex +1,
              rdgObservations.Cells[Ord(cocObsType),ItemIndex +1]);
            case TobItem.ObservationType of
              otTime:
                begin
                  rdgObservations.Cells[Ord(cocTime),ItemIndex +1] :=
                    FloatToStr(TobItem.Time);
                end;
              otFrequency:
                begin
                  rdgObservations.Cells[Ord(cocTime),ItemIndex +1] :=
                    IntToStr(TobItem.ObservationFrequency);
                end;
              else
                Assert(False);
            end;
            rdgObservations.Cells[Ord(cocSpecies),ItemIndex +1] :=
              TobItem.SpeciesName;
            rdgObservations.Cells[Ord(cocConcentration),ItemIndex +1] :=
              FloatToStr(TobItem.Concentration);
            rdgObservations.Cells[Ord(cocWeight),ItemIndex +1] :=
              FloatToStr(TobItem.Weight);
            rdgObservations.Cells[Ord(cocComment),ItemIndex +1] := TobItem.Comment;
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
          if seTimes.AsInteger <> Observations.Values.Count then
          begin
            ClearGrid(rdgObservations);
          end
          else
          begin
            for ItemIndex := 0 to Observations.Values.Count - 1 do
            begin
              TobItem := Observations.Values.TobItems[ItemIndex];

              if rdgObservations.ItemIndex[Ord(cocObsType),ItemIndex +1]
                <> Ord(TobItem.ObservationType) then
              begin
                rdgObservations.Cells[Ord(cocObsType),ItemIndex +1] := '';
                rdgObservationsSetEditText(rdgObservations, Ord(cocObsType),ItemIndex +1,
                  rdgObservations.Cells[Ord(cocObsType),ItemIndex +1]);
                rdgObservations.Cells[Ord(cocTime),ItemIndex +1] := '';
              end
              else
              begin
                case TobItem.ObservationType of
                  otTime:
                    begin
                      if rdgObservations.Cells[Ord(cocTime),ItemIndex +1]
                        <> FloatToStr(TobItem.Time) then
                      begin
                        rdgObservations.Cells[Ord(cocTime),ItemIndex +1] := '';
                      end;
                    end;
                  otFrequency:
                    begin
                      if rdgObservations.Cells[Ord(cocTime),ItemIndex +1]
                        <> IntToStr(TobItem.ObservationFrequency) then
                      begin
                        rdgObservations.Cells[Ord(cocTime),ItemIndex +1] := '';
                      end;
                    end;
                  else Assert(False);
                end;
              end;


              if rdgObservations.Cells[Ord(cocSpecies),ItemIndex +1]
                <> TobItem.SpeciesName then
              begin
                rdgObservations.Cells[Ord(cocSpecies),ItemIndex +1] := '';
              end;
              if rdgObservations.Cells[Ord(cocConcentration),ItemIndex +1]
                <> FloatToStr(TobItem.Concentration) then
              begin
                rdgObservations.Cells[Ord(cocConcentration),ItemIndex +1] := '';
              end;
              if rdgObservations.Cells[Ord(cocWeight),ItemIndex +1]
                <> FloatToStr(TobItem.Weight) then
              begin
                rdgObservations.Cells[Ord(cocWeight),ItemIndex +1] := '';
              end;
              if rdgObservations.Cells[Ord(cocComment),ItemIndex +1]
                <> TobItem.Comment then
              begin
                rdgObservations.Cells[Ord(cocComment),ItemIndex +1] := '';
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
              end;
              if rdgLayers.Cells[Ord(hlFraction),ItemIndex +1]
                <> FloatToStr(LayerItem.Proportion) then
              begin
                rdgLayers.Cells[Ord(hlFraction),ItemIndex +1] := '';
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
    end
  finally
    rdgObservations.EndUpdate;
    rdgLayers.EndUpdate;
  end;
  FChanged := False;
  FTimesCountChanged := False;
  FLayerCountChanged := False;
end;

procedure TframeConcentrationObservation.InitializeControls;
var
  Index: Integer;
  AnItem: TChemSpeciesItem;
  AColumn: TRbwColumn4;
begin
  inherited;
  AColumn := rdgObservations.Columns[Ord(cocSpecies)];
  AColumn.PickList.Clear;
  for Index := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
  begin
    AnItem := frmGoPhast.PhastModel.MobileComponents[Index];
    AColumn.PickList.Add(AnItem.Name);
  end;
  for Index := 0 to frmGoPhast.PhastModel.ImmobileComponents.Count - 1 do
  begin
    AnItem := frmGoPhast.PhastModel.ImmobileComponents[Index];
    AColumn.PickList.Add(AnItem.Name);
  end;
  comboSpeciesNames.Items.Clear;
  for Index := 0 to AColumn.PickList.Count - 1 do
  begin
    comboSpeciesNames.Items.Add(AColumn.PickList[Index]);
  end;
  rdgObservations.Cells[Ord(cocObsType),0] := StrObservationMethod;
  rdgObservations.Cells[Ord(cocTime),0] := StrTimeTimeObs;
  rdgObservations.Cells[Ord(cocSpecies),0] := StrSpecies;
  rdgObservations.Cells[Ord(cocConcentration),0] := StrConcentration;
  rdgObservations.Cells[Ord(cocWeight),0] := StrWeight;
  rdgObservations.Cells[Ord(cocComment),0] := StrComment;
  rdgLayers.Cells[Ord(hlFraction),0] := StrWeightPrLayer;

end;

procedure TframeConcentrationObservation.LayoutMultiCellEditControls;
var
  Index: Integer;
  AColVisible: Boolean;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  AColVisible := False;
  for Index := Ord(cocTime) to Ord(cocWeight) do
  begin
    if Index = Ord(cocSpecies) then
    begin
      Continue;
    end;
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
  LayoutControls(rdgObservations, comboSpeciesNames, nil, Ord(cocSpecies));

end;

procedure TframeConcentrationObservation.rdeMultiValueEditChange(
  Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(rdeMultiValueEdit.Text, rdgObservations,
    Ord(cocTime), Ord(cocTime));
  AssignValuesToSelectedGridCells(rdeMultiValueEdit.Text, rdgObservations,
    Ord(cocConcentration), Ord(cocWeight));
end;

procedure TframeConcentrationObservation.rdgObservationsSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  EnableMultiEditControl(rdgObservations, rdeMultiValueEdit,
    [Ord(cocTime), Ord(cocConcentration), Ord(cocWeight)]);
  EnableMultiEditControl(rdgObservations, comboSpeciesNames,
    Ord(cocSpecies));
end;

procedure TframeConcentrationObservation.rdgObservationsSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  ItemIndex: Integer;
  ObservationType: TObservationType;
  AColumn: TRbwColumn4;
begin
  inherited;
  if (ACol = Ord(cocObsType)) and (ARow >= rdgObservations.FixedRows) then
  begin
    AColumn := rdgObservations.Columns[Ord(cocTime)];
    ItemIndex := rdgObservations.ItemIndex[ACol, ARow];
    if ItemIndex >= 0 then
    begin
      ObservationType := TObservationType(ItemIndex);
      case ObservationType of
        otTime:
          begin
            AColumn.CheckMin := False;
            rdgObservations.UseSpecialFormat[Ord(cocTime), ARow] := False;
            AColumn.Min := frmGoPhast.PhastModel.ModflowStressPeriods[0].StartTime;
            AColumn.CheckMin := True;
          end;
        otFrequency:
          begin
            AColumn.CheckMin := False;
            rdgObservations.SpecialFormat[Ord(cocTime), ARow] := rcf4Integer;
            rdgObservations.UseSpecialFormat[Ord(cocTime), ARow] := True;
            AColumn.Min := 1;
            AColumn.CheckMin := True;
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      AColumn.CheckMin := False;
      rdgObservations.UseSpecialFormat[Ord(cocTime), ARow] := False;
    end;
  end;
end;

procedure TframeConcentrationObservation.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  ObservationUsed: boolean;
  ValueCount: integer;
  RowIndex: Integer;
  Concentration, Time, Weight: double;
  Layer: integer;
  Fraction: double;
  NewComment: string;
  Observations: TMt3dmsTransObservations;
  ObsItem: TMt3dmsTobItem;
  ObsLayer: TMultiHeadItem;
  ItemIndex: Integer;
  ComponentError: boolean;
begin
  if not FChanged then
  begin
    Exit;
  end;
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Observations := Item.ScreenObject.Mt3dmsTransObservations;
    ObservationUsed := (Observations <> nil) and Observations.Used;
    ComponentError := False;

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
        Item.ScreenObject.CreateMt3dmsTransObservations;
        Observations := Item.ScreenObject.Mt3dmsTransObservations;
      end;

      if edObsName.Text <> '' then
      begin
        Observations.ObservationName := edObsName.Text;
      end;
      if comboTreatment.ItemIndex >= 0 then
      begin
        Observations.Purpose :=
          TObservationPurpose(comboTreatment.ItemIndex);
      end;
      ValueCount := 0;
      for RowIndex := 1 to rdgObservations.RowCount - 1 do
      begin
        if (TryStrToFloat(rdgObservations.Cells[Ord(cocTime), RowIndex], Time)
          or (rdgObservations.Cells[Ord(cocTime), RowIndex] = ''))
          and (TryStrToFloat(rdgObservations.Cells[Ord(cocConcentration), RowIndex], Concentration)
          or (rdgObservations.Cells[Ord(cocConcentration), RowIndex] = ''))
          and (TryStrToFloat(rdgObservations.Cells[Ord(cocWeight), RowIndex], Weight)
          or (rdgObservations.Cells[Ord(cocWeight), RowIndex] = ''))
          then
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
        if (TryStrToFloat(rdgObservations.Cells[Ord(cocTime), RowIndex], Time)
          or (rdgObservations.Cells[Ord(cocTime), RowIndex] = ''))
          and (TryStrToFloat(rdgObservations.Cells[Ord(cocConcentration), RowIndex], Concentration)
          or (rdgObservations.Cells[Ord(cocConcentration), RowIndex] = ''))
          and (TryStrToFloat(rdgObservations.Cells[Ord(cocWeight), RowIndex], Weight)
          or (rdgObservations.Cells[Ord(cocWeight), RowIndex] = ''))
          then
        begin
          if ValueCount < Observations.Values.Count then
          begin
            ObsItem := Observations.Values.TobItems[ValueCount];
            ItemIndex := rdgObservations.ItemIndex[Ord(cocObsType), RowIndex];
            if ItemIndex >= 0 then
            begin
              ObsItem.ObservationType := TObservationType(ItemIndex);
            end;
            if (rdgObservations.Cells[Ord(cocTime), RowIndex] <> '') then
            begin
              case ObsItem.ObservationType of
                otTime: ObsItem.Time := Time;
                otFrequency: ObsItem.ObservationFrequency :=
                  StrToInt(rdgObservations.Cells[Ord(cocTime), RowIndex]);
                else Assert(False);
              end;
            end;
            if (rdgObservations.Cells[Ord(cocConcentration), RowIndex] <> '') then
            begin
              ObsItem.Concentration := Concentration;
            end;
            if TryStrToFloat(rdgObservations.Cells[Ord(cocWeight), RowIndex], Weight) then
            begin
              ObsItem.Weight := Weight;
            end;
            if (rdgObservations.Cells[Ord(cocSpecies), RowIndex] <> '') then
            begin
              ObsItem.SpeciesName := rdgObservations.Cells[Ord(cocSpecies), RowIndex];
            end;
            if ObsItem.SpeciesName = '' then
            begin
              ComponentError := True;
            end;
            NewComment := rdgObservations.Cells[Ord(cocComment), RowIndex];
            if (List.Count = 1) or (NewComment <> '') then
            begin
              ObsItem.Comment := NewComment;
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
    if ComponentError then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
        StrInTheFollowingObj, Item.ScreenObject.Name, Item.ScreenObject);
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

end.
