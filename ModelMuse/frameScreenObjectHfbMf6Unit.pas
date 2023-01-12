unit frameScreenObjectHfbMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, JvExStdCtrls, JvCombobox,
  JvListComb;

type
  THfbColumns = (hcStartTime,  hcEndTime, hcThickness, hcHydraulicConductivity);

  TframeScreenObjectHfbMf6 = class(TframeScreenObjectNoParam)
    rgAngleAdjustment: TRadioGroup;
    lblParameterName: TLabel;
    comboHfbParameters: TJvImageComboBox;
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure comboHfbParametersChange(Sender: TObject);
  private
    procedure InitializeControls;
    procedure FillListOfScreenObjects(ListOfScreenObjects: TList;
      List: TScreenObjectEditCollection);
    procedure FillListOfParameterNames(ParameterNames: TStringList);
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectHfbMf6: TframeScreenObjectHfbMf6;

implementation

uses
  ModflowHfbUnit, ScreenObjectUnit, GoPhastTypes, frmGoPhastUnit,
  ModflowParameterUnit, OrderedCollectionUnit;

resourcestring
  StrNone = 'none';
  StrBarrierThickness = 'Barrier Thickness';
  StrBarrierHydraulicCo = 'Barrier Hydraulic Conductivity';

{$R *.dfm}

{ TframeScreenObjectHfbMf6 }

procedure TframeScreenObjectHfbMf6.comboHfbParametersChange(Sender: TObject);
begin
  inherited;
  rdgModflowBoundary.Invalidate;
end;

procedure TframeScreenObjectHfbMf6.FillListOfParameterNames(
  ParameterNames: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to comboHfbParameters.Items.Count - 1 do
  begin
    ParameterNames.Add(comboHfbParameters.Items[Index].Text);
  end;
end;

procedure TframeScreenObjectHfbMf6.FillListOfScreenObjects(
  ListOfScreenObjects: TList; List: TScreenObjectEditCollection);
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ViewDirection = vdTop)
      and (ScreenObject.Count > ScreenObject.SectionCount) then
    begin
      ListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TframeScreenObjectHfbMf6.GetData(List: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TList;
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  Values: THfbCollection;
  FirstValues: THfbCollection;
  Index: Integer;
  TimeIndex: Integer;
  AnItem: THfbItem;
  ParameterNames: TStringList;
  ParamIndex: Integer;
  FirstBoundary: THfbBoundary;
begin
  ClearGrid(rdgModflowBoundary);

  InitializeControls;
  ParameterNames := TStringList.Create;
  ListOfScreenObjects := TList.Create;
  try
    Assert(List.Count >= 1);
    FillListOfScreenObjects(ListOfScreenObjects, List);
    FillListOfParameterNames(ParameterNames);

    Enabled := False;
    if ListOfScreenObjects.Count > 0 then
    begin
      ScreenObject := ListOfScreenObjects[0];
      Boundary := ScreenObject.ModflowHfbBoundary;
      Assert(Boundary <> nil);
      if Boundary.UsedMf6 then
      begin
        Enabled := True;
      end;
      rgAngleAdjustment.ItemIndex := Ord(Boundary.AdjustmentMethod);

      PestModifier[Ord(hcThickness)] := Boundary.PestThicknessFormula;
      PestMethod[Ord(hcThickness)] := Boundary.PestThicknessMethod;

      PestModifier[Ord(hcHydraulicConductivity)] := Boundary.PestHydraulicConductivityFormula;
      PestMethod[Ord(hcHydraulicConductivity)] := Boundary.PestHydraulicConductivityMethod;

      Values := Boundary.Values;
      FirstValues := Values;

      seNumberOfTimes.AsInteger := Values.Count;
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        AnItem := Values[TimeIndex] as THfbItem;
        rdgModflowBoundary.Cells[Ord(hcStartTime), TimeIndex+1+PestRowOffset] :=
          AnItem.StartTime.ToString;
        rdgModflowBoundary.Cells[Ord(hcEndTime), TimeIndex+1+PestRowOffset] :=
          AnItem.EndTime.ToString;
        rdgModflowBoundary.Cells[Ord(hcThickness), TimeIndex+1+PestRowOffset] :=
          AnItem.Thickness;
        rdgModflowBoundary.Cells[Ord(hcHydraulicConductivity), TimeIndex+1+PestRowOffset] :=
          AnItem.HydraulicConductivity;
      end;

      if Boundary.ParameterName = '' then
      begin
        ParamIndex := 0;
      end
      else
      begin
        ParamIndex := ParameterNames.IndexOf(Boundary.ParameterName);
      end;
      comboHfbParameters.ItemIndex := ParamIndex;

      FirstBoundary := Boundary;

      for Index := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ScreenObject := ListOfScreenObjects[Index];
        Boundary := ScreenObject.ModflowHfbBoundary;

        if Boundary.PestThicknessFormula <>
          FirstBoundary.PestThicknessFormula then
        begin
          PestModifierAssigned[Ord(hcThickness)] := False
        end;
        if Boundary.PestThicknessMethod <>
          FirstBoundary.PestThicknessMethod then
        begin
          PestMethodAssigned[Ord(hcThickness)] := False
        end;

        if Boundary.PestHydraulicConductivityFormula <>
          FirstBoundary.PestHydraulicConductivityFormula then
        begin
          PestModifierAssigned[Ord(hcHydraulicConductivity)] := False
        end;
        if Boundary.PestHydraulicConductivityMethod <>
          FirstBoundary.PestHydraulicConductivityMethod then
        begin
          PestMethodAssigned[Ord(hcHydraulicConductivity)] := False
        end;

        Assert(Boundary <> nil);
        if Boundary.UsedMf6 then
        begin
          Enabled := True;
        end;
        Values := Boundary.Values;// as THfbCollection;
        if not Values.IsSame(FirstValues) then
        begin
          ClearGrid(rdgModflowBoundary);
        end;

        if rgAngleAdjustment.ItemIndex <> Ord(Boundary.AdjustmentMethod) then
        begin
          rgAngleAdjustment.ItemIndex := -1;
        end;

        if Boundary.ParameterName = '' then
        begin
          ParamIndex := 0;
        end
        else
        begin
          ParamIndex := ParameterNames.IndexOf(Boundary.ParameterName);
        end;
        if comboHfbParameters.ItemIndex <> ParamIndex then
        begin
          comboHfbParameters.ItemIndex := -1;
        end;

      end
    end;

  finally
    ListOfScreenObjects.Free;
    ParameterNames.Free;
  end;
end;

procedure TframeScreenObjectHfbMf6.InitializeControls;
var
  Item: TJvImageItem;
  Index: Integer;
  Param: TModflowSteadyParameter;
begin

  {$IF CompilerVersion > 28}
  comboHfbParameters.Items.ClearAndResetID;
  {$ENDIF}
  Item := comboHfbParameters.Items.Add;
  Item.Text := StrNone;
  for Index := 0 to frmGoPhast.PhastModel.ModflowSteadyParameters.Count - 1 do
  begin
    Param := frmGoPhast.PhastModel.ModflowSteadyParameters[Index];
    if Param.ParameterType = ptHFB then
    begin
      Item := comboHfbParameters.Items.Add;
      Item.Text := Param.ParameterName;
    end;
  end;
  comboHfbParameters.ItemIndex := 0;

  rdgModflowBoundary.Cells[Ord(hcStartTime), 0] := StrStartingTime;
  rdgModflowBoundary.Cells[Ord(hcEndTime), 0] := StrEndingTime;
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithStartTimes(rdgModflowBoundary, Ord(hcStartTime));
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithEndTimes(rdgModflowBoundary, Ord(hcEndTime));
  rdgModflowBoundary.Cells[Ord(hcThickness), 0] := StrBarrierThickness;
  rdgModflowBoundary.Cells[Ord(hcHydraulicConductivity), 0] := StrBarrierHydraulicCo;

  seNumberOfTimes.AsInteger := 0;
  if Assigned(seNumberOfTimes.OnChange) then
  begin
    seNumberOfTimes.OnChange(seNumberOfTimes);
  end;
  rdgModflowBoundary.UseSpecialFormat[0, PestModifierRow] := True;
  rdgModflowBoundary.UseSpecialFormat[0, PestMethodRow] := True;
  rdgModflowBoundary.SpecialFormat[0, PestModifierRow] := rcf4String;
  rdgModflowBoundary.SpecialFormat[0, PestMethodRow] := rcf4String;
  rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
  rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;

  PestMethod[Ord(hcThickness)] :=
    THfbBoundary.DefaultBoundaryMethod(HfbPestThicknessPosition);
  PestMethod[Ord(hcHydraulicConductivity)] :=
    THfbBoundary.DefaultBoundaryMethod(HfbPestHydraulicConductivityPosition);
end;

procedure TframeScreenObjectHfbMf6.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if ARow <= PestRowOffset then
  begin
    if comboHfbParameters.ItemIndex > 0 then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeScreenObjectHfbMf6.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  BoundaryUsed: Boolean;
  Index: Integer;
  NewValues: THfbCollection;
  TimeIndex: Integer;
  NewItem: THfbItem;
  StartTime: double;
  EndTime: double;
//  ItemCount: Integer;
begin
  NewValues := nil;
  try
    if ClearAll then
    begin
      NewValues := nil;
    end
    else
    begin
      NewValues := THfbCollection.Create(nil, nil, nil);
      for TimeIndex := 1 to seNumberOfTimes.AsInteger do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(hcStartTime), TimeIndex+PestRowOffset], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[Ord(hcEndTime), TimeIndex+PestRowOffset], EndTime)
          and (rdgModflowBoundary.Cells[Ord(hcThickness), TimeIndex+PestRowOffset] <> '')
          and (rdgModflowBoundary.Cells[Ord(hcHydraulicConductivity), TimeIndex+PestRowOffset] <> '') then
        begin
          NewItem := NewValues.Add;
          NewItem.StartTime := StartTime;
          NewItem.EndTime := EndTime;
          NewItem.Thickness := rdgModflowBoundary.Cells[Ord(hcThickness), TimeIndex+PestRowOffset];
          NewItem.HydraulicConductivity := rdgModflowBoundary.Cells[Ord(hcHydraulicConductivity), TimeIndex+PestRowOffset];
        end;
      end;
    end;

    for Index := 0 to List.Count - 1 do
    begin
      ScreenObject := List[Index].ScreenObject;
      Boundary := ScreenObject.ModflowHfbBoundary;
      BoundaryUsed := (Boundary <> nil) and Boundary.UsedMf6;

      if ClearAll then
      begin
        if BoundaryUsed then
        begin
          Boundary.Values.Clear;
        end;
      end
      else if SetAll or BoundaryUsed then
      begin
        if Boundary = nil then
        begin
          ScreenObject.CreateHfbBoundary;
          Boundary := ScreenObject.ModflowHfbBoundary;
        end;
      end;

      if Boundary <> nil then
      begin
        if rgAngleAdjustment.ItemIndex >= 0 then
        begin
          Boundary.AdjustmentMethod := TAdjustmentMethod(rgAngleAdjustment.ItemIndex);
        end;

        if (NewValues <> nil) and (NewValues.Count > 0) then
        begin
          Boundary.Values := NewValues;
        end;

        if PestModifierAssigned[Ord(hcThickness)] then
        begin
          Boundary.PestThicknessFormula := PestModifier[Ord(hcThickness)];
        end;
        if PestMethodAssigned[Ord(hcThickness)] then
        begin
          Boundary.PestThicknessMethod := PestMethod[Ord(hcThickness)];
        end;

        if PestModifierAssigned[Ord(hcHydraulicConductivity)] then
        begin
          Boundary.PestHydraulicConductivityFormula :=
            PestModifier[Ord(hcHydraulicConductivity)];
        end;
        if PestMethodAssigned[Ord(hcHydraulicConductivity)] then
        begin
          Boundary.PestHydraulicConductivityMethod :=
            PestMethod[Ord(hcHydraulicConductivity)];
        end;

        if comboHfbParameters.ItemIndex >= 0 then
        begin
          if comboHfbParameters.ItemIndex = 0 then
          begin
            Boundary.ParameterName := ''
          end
          else
          begin
            Boundary.ParameterName := comboHfbParameters.Text;
          end;
        end;
      end;

    end;
  finally
    NewValues.Free;
  end;
end;

procedure TframeScreenObjectHfbMf6.SetEnabled(Value: boolean);
var
  Index: Integer;
  Component: TComponent;
begin
  inherited;
  for Index := 0 to ComponentCount - 1 do
  begin
    Component := Components[Index];
    if Component is TControl then
    begin
      TControl(Component).Enabled := Value;
    end;
  end;
end;

end.
