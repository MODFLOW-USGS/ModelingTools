unit frameCustomGwtBoundaryUnit;

interface

uses
  ModflowBoundaryUnit, undoitemsscreenobjects, GoPhastTypes,
  Mt3dmsChemSpeciesUnit,
  OrderedCollectionUnit,
  PhastModelUnit,
  frmGoPhastUnit,
  frameScreenObjectUnit,
  screenobjectunit,
  modflowgwtspecifiedconcunit,
  Winapi.Windows,
  Winapi.Messages, System.SysUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls;

type
  TCncColumns = (ccStartTime, ccEndTime, ccActive, ccConcentration, ccMultiplier);

  TframeCustomGwtBoundary = class abstract(TframeScreenObjectNoParam)
    comboChemSpecies: TComboBox;
    lblChemSpecies: TLabel;
  protected
    function GetVariableName: string; virtual; abstract;
    function GetMultiplierName: string; virtual; abstract;
    function GetActiveName: string; virtual; abstract;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary;
      virtual; abstract;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); virtual; abstract;
    procedure InitializeControls;
    procedure AssignFirstItem(ScreenObject: TScreenObject);
  private
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameCustomGwtBoundary: TframeCustomGwtBoundary;

implementation

{$R *.dfm}

procedure TframeCustomGwtBoundary.InitializeControls;
var
  MobileCompenentNames: TStringList;
  index: Integer;
begin
  ClearGrid(rdgModflowBoundary);
  MobileCompenentNames := TStringList.Create;
  try
    for index := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
    begin
      MobileCompenentNames.Add(frmGoPhast.PhastModel.MobileComponents
        [index].Name);
    end;
    comboChemSpecies.Items.Assign(MobileCompenentNames);
    if comboChemSpecies.Items.Count >= 1 then
    begin
      comboChemSpecies.ItemIndex := 0;
    end;
  finally
    MobileCompenentNames.Free;
  end;
  rdgModflowBoundary.Columns[Ord(ccStartTime)].AutoAdjustColWidths := False;
  rdgModflowBoundary.Columns[Ord(ccEndTime)].AutoAdjustColWidths := False;
  rdgModflowBoundary.Columns[Ord(ccActive)].AutoAdjustColWidths := False;
  rdgModflowBoundary.Columns[Ord(ccConcentration)].AutoAdjustColWidths := False;
  rdgModflowBoundary.Columns[Ord(ccMultiplier)].AutoAdjustColWidths := False;

  seNumberOfTimes.AsInteger := 0;
  seNumberOfTimes.OnChange(nil);
  rdgModflowBoundary.Cells[Ord(ccStartTime), 1] := '';
  rdgModflowBoundary.Cells[Ord(ccEndTime), 1] := '';
  rdgModflowBoundary.Cells[Ord(ccActive), 1] := '';
  rdgModflowBoundary.Cells[Ord(ccConcentration), 1] := '';
  rdgModflowBoundary.Cells[Ord(ccMultiplier), 1] := '';

  rdgModflowBoundary.Columns[Ord(ccStartTime)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(ccEndTime)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(ccActive)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(ccConcentration)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(ccMultiplier)].AutoAdjustColWidths := True;

  rdgModflowBoundary.Cells[Ord(ccStartTime), 0] := StrStartingTime;
  rdgModflowBoundary.Cells[Ord(ccEndTime), 0] := StrEndingTime;
  rdgModflowBoundary.Cells[Ord(ccActive), 0] := GetActiveName;
  rdgModflowBoundary.Cells[Ord(ccConcentration), 0] := GetVariableName;
  rdgModflowBoundary.Cells[Ord(ccMultiplier), 0] := GetMultiplierName;

  rdgModflowBoundary.Cells[Ord(ccStartTime), PestModifierRow] :=
    StrPestModifier;
  rdgModflowBoundary.Cells[Ord(ccStartTime), PestMethodRow] :=
    StrModificationMethod;
end;

procedure TframeCustomGwtBoundary.AssignFirstItem(ScreenObject: TScreenObject);
const
  BoundaryValuePosition = 0;
var
  ABoundary: TCncBoundary;
  ItemIndex: Integer;
  CncItems: TCncCollection;
  AnItem: TCncItem;
begin
  ABoundary := GetBoundary(ScreenObject);
  comboChemSpecies.ItemIndex := comboChemSpecies.Items.IndexOf
    (ABoundary.ChemSpecies);
  Assert(ABoundary <> nil);
  seNumberOfTimes.AsInteger := ABoundary.Values.Count;
  seNumberOfTimes.OnChange(nil);
  Assert(rdgModflowBoundary.RowCount - 1 = ABoundary.Values.Count +
    PestRowOffset);
  CncItems := ABoundary.Values as TCncCollection;
  for ItemIndex := 0 to CncItems.Count - 1 do
  begin
    AnItem := CncItems[ItemIndex] as TCncItem;
    rdgModflowBoundary.Cells[Ord(ccStartTime), ItemIndex + PestRowOffset + 1] :=
      FloatToStr(AnItem.StartTime);
    rdgModflowBoundary.Cells[Ord(ccEndTime), ItemIndex + PestRowOffset + 1] :=
      FloatToStr(AnItem.EndTime);
    rdgModflowBoundary.Cells[Ord(ccActive), ItemIndex + PestRowOffset +
      1] := AnItem.Active;
    rdgModflowBoundary.Cells[Ord(ccConcentration), ItemIndex + PestRowOffset +
      1] := AnItem.Concentration;
    rdgModflowBoundary.Cells[Ord(ccMultiplier), ItemIndex + PestRowOffset +
      1] := AnItem.Multiplier;
  end;
  PestMethod[Ord(ccConcentration)] := ABoundary.PestBoundaryMethod
    [BoundaryValuePosition];
  PestModifier[Ord(ccConcentration)] := ABoundary.PestBoundaryFormula
    [BoundaryValuePosition];
  PestMethod[Ord(ccMultiplier)] := ABoundary.PestBoundaryMethod
    [BoundaryValuePosition];
  PestModifier[Ord(ccMultiplier)] := ABoundary.PestBoundaryFormula
    [BoundaryValuePosition];
end;

procedure TframeCustomGwtBoundary.GetData(ScreenObjectList
  : TScreenObjectEditCollection);
var
  ListOfScreenObjects: TScreenObjectList;
  index: Integer;
  AScreenObject: TScreenObject;
  FirstBoundary: TCncBoundary;
  ScreenObjectIndex: Integer;
  ABoundary: TCncBoundary;
  CncItems: TCncCollection;
  ItemIndex: Integer;
  AnItem: TCncItem;
  procedure ClearGrid;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
      begin
        rdgModflowBoundary.Cells[ColIndex, RowIndex] := '';
      end;
    end;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  end;

begin
  InitializeControls;
  ListOfScreenObjects := TScreenObjectList.Create;
  try
    for index := 0 to ScreenObjectList.Count - 1 do
    begin
      AScreenObject := ScreenObjectList[index].ScreenObject;
      ABoundary := GetBoundary(AScreenObject);
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        ListOfScreenObjects.Add(AScreenObject);
      end;
    end;
    if ListOfScreenObjects.Count > 0 then
    begin
      AssignFirstItem(ListOfScreenObjects[0]);
      FirstBoundary := GetBoundary(ListOfScreenObjects[0]);
      for ScreenObjectIndex := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ABoundary := GetBoundary(ListOfScreenObjects[ScreenObjectIndex]);
        if ABoundary.ChemSpecies <> comboChemSpecies.Text then
        begin
          comboChemSpecies.ItemIndex := -1;
        end;
        if (ABoundary.PestConcentrationFormula <>
          FirstBoundary.PestConcentrationFormula) or
          (ABoundary.PestConcentrationMethod <>
          FirstBoundary.PestConcentrationMethod)
          or (ABoundary.PestMultiplierFormula <>
          FirstBoundary.PestMultiplierFormula) or
          (ABoundary.PestMultiplierMethod <>
          FirstBoundary.PestMultiplierMethod)
          then
        begin
          ClearGrid;
          Exit;
        end;
        CncItems := ABoundary.Values as TCncCollection;
        if CncItems.Count = seNumberOfTimes.AsInteger then
        begin
          for ItemIndex := 0 to CncItems.Count - 1 do
          begin
            AnItem := CncItems[ItemIndex] as TCncItem;
            if (rdgModflowBoundary.Cells[Ord(ccStartTime), ItemIndex + 1] <>
              FloatToStr(AnItem.StartTime)) or
              (rdgModflowBoundary.Cells[Ord(ccEndTime), ItemIndex + 1] <>
              FloatToStr(AnItem.EndTime)) or
              (rdgModflowBoundary.Cells[Ord(ccActive), ItemIndex + 1] <>
              AnItem.Active) or
              (rdgModflowBoundary.Cells[Ord(ccConcentration), ItemIndex + 1] <>
              AnItem.Concentration) or
              (rdgModflowBoundary.Cells[Ord(ccMultiplier), ItemIndex + 1] <>
              AnItem.Multiplier)
              then
            begin
              ClearGrid;
              Exit;
            end;
          end;
        end
        else
        begin
          ClearGrid;
          break;
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

procedure TframeCustomGwtBoundary.SetData(List: TScreenObjectEditCollection;
  SetAll: boolean; ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TCncBoundary;
  BoundaryUsed: boolean;
  NewBoundary: TCncBoundary;
  RowIndex: Integer;
  CncItem: TCncItem;
  StartTime: Double;
  EndTime: Double;
begin
  NewBoundary := nil;
  try
    if SetAll or not ClearAll then
    begin
      NewBoundary := TCncBoundary.Create(nil, nil);
      for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(ccStartTime), RowIndex], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[Ord(ccEndTime), RowIndex], EndTime)
          and (rdgModflowBoundary.Cells[Ord(ccConcentration), RowIndex] <> '') then
        begin
          CncItem := NewBoundary.Values.Add as TCncItem;
          CncItem.StartTime := StartTime;
          CncItem.EndTime := EndTime;
          CncItem.Concentration := rdgModflowBoundary.Cells
            [Ord(ccConcentration), RowIndex];
          if rdgModflowBoundary.Cells[Ord(ccMultiplier), RowIndex] <> '' then
          begin
            CncItem.Multiplier := rdgModflowBoundary.Cells
              [Ord(ccMultiplier), RowIndex];
          end
          else
          begin
            CncItem.Multiplier := '1';
          end;
          if rdgModflowBoundary.Cells[Ord(ccActive), RowIndex] <> '' then
          begin
            CncItem.Active := rdgModflowBoundary.Cells
              [Ord(ccActive), RowIndex];
          end;
        end;
      end;
      if NewBoundary.Values.Count = 0 then
      begin
        Exit;
      end;
    end;
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      Item := List[ScreenObjectIndex];
      ScreenObject := Item.ScreenObject;
      Boundary := GetBoundary(ScreenObject);
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
        if (Boundary = nil) then
        begin
          CreateNewBoundary(ScreenObject);
          Boundary := GetBoundary(ScreenObject);
        end;
        if Boundary <> nil then
        begin
          if rdgModflowBoundary.Cells[Ord(ccConcentration), PestMethodRow] = ''
          then
          begin
            NewBoundary.PestConcentrationMethod :=
              Boundary.PestConcentrationMethod;
          end
          else
          begin
            NewBoundary.PestConcentrationMethod :=
              PestMethod[Ord(ccConcentration)];
          end;
          if rdgModflowBoundary.Cells[Ord(ccConcentration), PestModifierRow] = ''
          then
          begin
            NewBoundary.PestConcentrationFormula :=
              Boundary.PestConcentrationFormula;
          end
          else
          begin
            NewBoundary.PestConcentrationFormula :=
              PestModifier[Ord(ccConcentration)];
          end;

          if rdgModflowBoundary.Cells[Ord(ccMultiplier), PestMethodRow] = ''
          then
          begin
            NewBoundary.PestMultiplierMethod :=
              Boundary.PestMultiplierMethod;
          end
          else
          begin
            NewBoundary.PestMultiplierMethod :=
              PestMethod[Ord(ccMultiplier)];
          end;
          if rdgModflowBoundary.Cells[Ord(ccMultiplier), PestModifierRow] = ''
          then
          begin
            NewBoundary.PestMultiplierFormula :=
              Boundary.PestMultiplierFormula;
          end
          else
          begin
            NewBoundary.PestMultiplierFormula :=
              PestModifier[Ord(ccMultiplier)];
          end;

          if comboChemSpecies.ItemIndex >= 0 then
          begin
            NewBoundary.ChemSpecies := comboChemSpecies.Text;
          end;
          Boundary.Assign(NewBoundary);
        end;
      end;
    end;
  finally
    NewBoundary.Free;
  end;
end;

end.
