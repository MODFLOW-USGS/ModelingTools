{ @abstract(@name defines @link(TframeHfbScreenObject) where it is used to
define the characteristics of horizontal flow boundaries.)
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frameHfbScreenObjectUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, JvExStdCtrls, JvCombobox, JvListComb, ExtCtrls,
  ArgusDataEntry, UndoItemsScreenObjects, RbwEdit;

type
  TframeHfbScreenObject = class(TFrame)
    lblParameterName: TLabel;
    comboHfbParameters: TJvImageComboBox;
    lblHydraulicConductivity: TLabel;
    lblBarrierThickness: TLabel;
    rgAngleAdjustment: TRadioGroup;
    pnlCaption: TPanel;
    edHydraulicConductivity: TRbwEdit;
    edBarrierThickness: TRbwEdit;
    btnEditHfbHydraulicConductivityFormula: TButton;
    btnEditHfbThicknessyFormula: TButton;
    procedure comboHfbParametersChange(Sender: TObject);
  private
    procedure FillListOfScreenObjects(ListOfScreenObjects: TList;
      List: TScreenObjectEditCollection);
    procedure FillListOfParameterNames(ParameterNames: TStringList);
    procedure FillListOfInvalidScreenObjects(ListOfScreenObjects: TList;
      List: TScreenObjectEditCollection);
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    procedure ClearInvalidHfbs(List: TScreenObjectEditCollection);
    { Public declarations }
  end;

implementation

uses ScreenObjectUnit, GoPhastTypes, ModflowHfbUnit, frmGoPhastUnit,
  ModflowParameterUnit, OrderedCollectionUnit;

resourcestring
  StrNone = 'none';
  StrBarrierHydraulicCoFact = 'Barrier hydraulic conductivity factor';
  StrBarrierHydraulicCo = 'Barrier hydraulic conductivity';

{$R *.dfm}

procedure TframeHfbScreenObject.GetData(List: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TList;
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  ParamIndex: integer;
  ParameterNames: TStringList;
  Index: Integer;
  Item: TJvImageItem;
  Param: TModflowSteadyParameter;
begin
  comboHfbParameters.Items.Clear;
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
      if Boundary.Used then
      begin
        Enabled := True;
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

      edHydraulicConductivity.Text := Boundary.HydraulicConductivityFormula;
      edBarrierThickness.Text := Boundary.ThicknessFormula;
      rgAngleAdjustment.ItemIndex := Ord(Boundary.AdjustmentMethod);

      for Index := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ScreenObject := ListOfScreenObjects[Index];
        Boundary := ScreenObject.ModflowHfbBoundary;
        Assert(Boundary <> nil);
        if Boundary.Used then
        begin
          Enabled := True;
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

        if edHydraulicConductivity.Text <>
          Boundary.HydraulicConductivityFormula then
        begin
          edHydraulicConductivity.Text := ''
        end;

        if edBarrierThickness.Text <>
          Boundary.ThicknessFormula then
        begin
          edBarrierThickness.Text := ''
        end;

        if rgAngleAdjustment.ItemIndex <> Ord(Boundary.AdjustmentMethod) then
        begin
          rgAngleAdjustment.ItemIndex := -1;
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
    ParameterNames.Free;
  end;
  comboHfbParametersChange(nil);
end;

procedure TframeHfbScreenObject.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ListOfScreenObjects: TList;
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  Index: Integer;
begin
  ListOfScreenObjects := TList.Create;
  try
    Assert(List.Count >= 1);
    FillListOfScreenObjects(ListOfScreenObjects, List);

    for Index := 0 to ListOfScreenObjects.Count - 1 do
    begin
      ScreenObject := ListOfScreenObjects[Index];
      Boundary := ScreenObject.ModflowHfbBoundary;
      Assert(Boundary <> nil);

      if SetAll then
      begin
        Boundary.IsUsed := True;
      end
      else if ClearAll then
      begin
        Boundary.IsUsed := False;
      end;

      if Boundary.IsUsed then
      begin
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

        if edHydraulicConductivity.Text <> '' then
        begin
          Boundary.HydraulicConductivityFormula :=
            edHydraulicConductivity.Text;
        end;

        if edBarrierThickness.Text <> '' then
        begin
          Boundary.ThicknessFormula :=
            edBarrierThickness.Text;
        end;

        if rgAngleAdjustment.ItemIndex >= 0 then
        begin
          Boundary.AdjustmentMethod :=
            TAdjustmentMethod(rgAngleAdjustment.ItemIndex);
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
//    ParameterNames.Free;
  end;
end;

procedure TframeHfbScreenObject.SetEnabled(Value: boolean);
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

procedure TframeHfbScreenObject.ClearInvalidHfbs(
  List: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TList;
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  Index: Integer;
begin
  ListOfScreenObjects := TList.Create;
  try
    Assert(List.Count >= 1);
    FillListOfInvalidScreenObjects(ListOfScreenObjects, List);

    for Index := 0 to ListOfScreenObjects.Count - 1 do
    begin
      ScreenObject := ListOfScreenObjects[Index];
      Boundary := ScreenObject.ModflowHfbBoundary;

      if Boundary <> nil then
      begin
        Boundary.IsUsed := False;
        Boundary.Values.Clear;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

procedure TframeHfbScreenObject.comboHfbParametersChange(Sender: TObject);
begin
  if comboHfbParameters.ItemIndex > 0 then
  begin
    lblHydraulicConductivity.Caption := StrBarrierHydraulicCoFact
  end
  else
  begin
    lblHydraulicConductivity.Caption := StrBarrierHydraulicCo
  end;
end;

procedure TframeHfbScreenObject.FillListOfParameterNames(ParameterNames: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to comboHfbParameters.Items.Count - 1 do
  begin
    ParameterNames.Add(comboHfbParameters.Items[Index].Text);
  end;
end;

procedure TframeHfbScreenObject.FillListOfScreenObjects(
  ListOfScreenObjects: TList; List: TScreenObjectEditCollection);
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ViewDirection = vdTop) and (ScreenObject.Count > ScreenObject.SectionCount) then
    begin
      ListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TframeHfbScreenObject.FillListOfInvalidScreenObjects(
  ListOfScreenObjects: TList; List: TScreenObjectEditCollection);
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ViewDirection <> vdTop)
      or (ScreenObject.Count <= ScreenObject.SectionCount)
      and (ScreenObject.ModflowHfbBoundary <> nil) then
    begin
      ListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

end.
