unit frameScreenObjectCncUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ScreenObjectUnit;

type
  TCncColumns = (ccStartTime, ccEndTime, ccConcentration);

  TframeScreenObjectCnc = class(TframeScreenObjectNoParam)
    comboChemSpecies: TComboBox;
    lblChemSpecies: TLabel;
  private
    procedure InitializeControls;
    procedure AssignFirstItem(ScreenObject: TScreenObject);
    procedure PestAllowed(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectCnc: TframeScreenObjectCnc;

implementation

uses
  GoPhastTypes, ModflowGwtSpecifiedConcUnit, frmGoPhastUnit, ModflowTimeUnit,
  frmCustomGoPhastUnit;

resourcestring
  StrSpecifiedConcentrat = 'Specified Concentration';

{$R *.dfm}

{ TframeScreenObjectCnc }

procedure TframeScreenObjectCnc.AssignFirstItem(ScreenObject: TScreenObject);
const
  BoundaryValuePosition = 0;
var
  ABoundary: TCncBoundary;
  ItemIndex: Integer;
  CncItems: TCncCollection;
  AnItem: TCncItem;
begin
  ABoundary := ScreenObject.GwtCncBoundary;

  comboChemSpecies.ItemIndex := comboChemSpecies.Items.IndexOf(ABoundary.ChemSpecies);
  Assert(ABoundary <> nil);
  seNumberOfTimes.AsInteger := ABoundary.Values.Count;
  seNumberOfTimes.OnChange(nil);
  Assert(rdgModflowBoundary.RowCount -1 = ABoundary.Values.Count + PestRowOffset);
  CncItems := ABoundary.Values as TCncCollection;
  for ItemIndex := 0 to CncItems.Count - 1 do
  begin
    AnItem := CncItems[ItemIndex] as TCncItem;
    rdgModflowBoundary.Cells[Ord(ccStartTime), ItemIndex+PestRowOffset+1] := FloatToStr(AnItem.StartTime);
    rdgModflowBoundary.Cells[Ord(ccEndTime), ItemIndex+PestRowOffset+1] := FloatToStr(AnItem.EndTime);
    rdgModflowBoundary.Cells[Ord(ccConcentration), ItemIndex+PestRowOffset+1] := AnItem.Concentration;
  end;

  PestMethod[Ord(ccConcentration)] := ABoundary.PestBoundaryMethod[BoundaryValuePosition];
  PestModifier[Ord(ccConcentration)] := ABoundary.PestBoundaryFormula[BoundaryValuePosition];
end;

procedure TframeScreenObjectCnc.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
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
        rdgModflowBoundary.Cells[ColIndex,RowIndex] := '';
      end;
    end;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  end;
begin
  InitializeControls;
  ListOfScreenObjects:= TScreenObjectList.Create;
  try
    for index := 0 to ScreenObjectList.Count - 1 do
    begin
      AScreenObject := ScreenObjectList[index].ScreenObject;
      if (AScreenObject.GwtCncBoundary <> nil)
        and AScreenObject.GwtCncBoundary.Used then
      begin
        ListOfScreenObjects.Add(AScreenObject);
      end;
    end;
    if ListOfScreenObjects.Count > 0 then
    begin
      AssignFirstItem(ListOfScreenObjects[0]);

      FirstBoundary := ListOfScreenObjects[0].GwtCncBoundary;

      for ScreenObjectIndex := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ABoundary := ListOfScreenObjects[ScreenObjectIndex].GwtCncBoundary;

        if ABoundary.ChemSpecies <> comboChemSpecies.Text  then
        begin
          comboChemSpecies.ItemIndex := -1;
        end;
        if (ABoundary.PestConcentrationFormula
          <> FirstBoundary.PestConcentrationFormula)
          or (ABoundary.PestConcentrationMethod
          <> FirstBoundary.PestConcentrationMethod) then
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
            if (rdgModflowBoundary.Cells[Ord(ccStartTime), ItemIndex+1]
              <> FloatToStr(AnItem.StartTime))
             or (rdgModflowBoundary.Cells[Ord(ccEndTime), ItemIndex+1]
              <> FloatToStr(AnItem.EndTime))
              or (rdgModflowBoundary.Cells[Ord(ccConcentration), ItemIndex+1]
              <> AnItem.Concentration) then
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

procedure TframeScreenObjectCnc.InitializeControls;
var
  MobileCompenentNames: TStringList;
  index: Integer;
begin
  ClearGrid(rdgModflowBoundary);
  MobileCompenentNames := TStringList.Create;
  try
    for index := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
    begin
      MobileCompenentNames.Add(frmGoPhast.PhastModel.MobileComponents[index].Name);
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
  rdgModflowBoundary.Columns[Ord(ccConcentration)].AutoAdjustColWidths := False;
  seNumberOfTimes.AsInteger := 0;
  seNumberOfTimes.OnChange(nil);
  rdgModflowBoundary.Cells[Ord(ccStartTime), 1] := '';
  rdgModflowBoundary.Cells[Ord(ccEndTime), 1] := '';
  rdgModflowBoundary.Cells[Ord(ccConcentration), 1] := '';
  rdgModflowBoundary.Columns[Ord(ccStartTime)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(ccEndTime)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(ccConcentration)].AutoAdjustColWidths := True;

  rdgModflowBoundary.Cells[Ord(ccStartTime), 0] := StrStartingTime;
  rdgModflowBoundary.Cells[Ord(ccEndTime), 0] := StrEndingTime;
  rdgModflowBoundary.Cells[Ord(ccConcentration), 0] := StrSpecifiedConcentrat;
  rdgModflowBoundary.Cells[Ord(ccStartTime), PestModifierRow] := StrPestModifier;
  rdgModflowBoundary.Cells[Ord(ccStartTime), PestMethodRow] := StrModificationMethod;
end;

procedure TframeScreenObjectCnc.PestAllowed(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ACol in [Ord(ccStartTime), Ord(ccEndTime)])
    and (ARow in [PestMethodRow, PestModifierRow]) then
  begin
    CanSelect := False;
  end;
end;

procedure TframeScreenObjectCnc.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TCncBoundary;
  BoundaryUsed: Boolean;
  NewBoundary: TCncBoundary;
  RowIndex: Integer;
//  ATime: double;
  CncItem: TCncItem;
  FirstTime: Double;
  StressPeriods: TModflowStressPeriods;
  LastTime: Double;
  PriorItem: TCncItem;
  StartTime: double;
  EndTime: double;
begin
  NewBoundary := nil;
  try
    if SetAll or not ClearAll then
    begin
      NewBoundary := TCncBoundary.Create(nil, nil);

      StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
      FirstTime := StressPeriods.First.StartTime;
      LastTime :=  StressPeriods.Last.EndTime;

      for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(ccStartTime), RowIndex], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[Ord(ccEndTime), RowIndex], EndTime)
          and (rdgModflowBoundary.Cells[Ord(ccConcentration), RowIndex] <> '') then
        begin
          CncItem := NewBoundary.Values.Add as TCncItem;
          CncItem.StartTime := StartTime;
          CncItem.EndTime := EndTime;
          CncItem.Concentration :=
            rdgModflowBoundary.Cells[Ord(ccConcentration), RowIndex];
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
      Boundary := ScreenObject.GwtCncBoundary;
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
          ScreenObject.CreateGwtCncBoundary;
          Boundary := ScreenObject.GwtCncBoundary;
        end;
        if Boundary <> nil then
        begin
          if rdgModflowBoundary.Cells[Ord(ccConcentration),PestMethodRow] = '' then
          begin
            NewBoundary.PestConcentrationMethod := Boundary.PestConcentrationMethod;
          end
          else
          begin
            NewBoundary.PestConcentrationMethod := PestMethod[Ord(ccConcentration)];
          end;
          if rdgModflowBoundary.Cells[Ord(ccConcentration),PestModifierRow] = '' then
          begin
            NewBoundary.PestConcentrationFormula := Boundary.PestConcentrationFormula;
          end
          else
          begin
            NewBoundary.PestConcentrationFormula := PestModifier[Ord(ccConcentration)];
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
