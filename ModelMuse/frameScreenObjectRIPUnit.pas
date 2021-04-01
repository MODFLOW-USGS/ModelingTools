unit frameScreenObjectRIPUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects;

type
  TRipColumns = (rcStartTime, rcEndTime, rcGroundElevation, rcCoverageStart);

  TframeScreenObjectRIP = class(TframeScreenObjectNoParam)
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FChanging: Boolean;
    FOnChange: TNotifyEvent;
    FMultipleDifferentObjects: boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    procedure InitializeGrid;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
  frameScreenObjectRIP: TframeScreenObjectRIP;

implementation

uses
  frmGoPhastUnit, GoPhastTypes, ModflowRipPlantGroupsUnit, frmCustomGoPhastUnit,
  ModflowRipUnit, ScreenObjectUnit, System.Generics.Collections;

resourcestring
  StrGroundElevationHS = 'Ground Elevation (HSURF)';

{$R *.dfm}

{ TframeScreenObjectRIP }

procedure TframeScreenObjectRIP.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectRIP.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TframeScreenObjectRIP.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  RipList: TList<TRipBoundary>;
  AScreenObject: TScreenObject;
  ObjectIndex: Integer;
  FirstRip: TRipBoundary;
  RipCollection: TRipCollection;
  TimeIndex: Integer;
  RipItem: TRipItem;
  RipPlantGroups: TRipPlantGroups;
  PlantGroupIndex: Integer;
  ColIndex: Integer;
  AnRipGroup: TRipPlantGroup;
  CoverageFormulas: TStrings;
  RipBoundary: TRipBoundary;
  BoundaryIndex: Integer;
begin
  FMultipleDifferentObjects := False;
  Changing := true;
  try
    ClearGrid(rdgModflowBoundary);
    InitializeGrid;

    RipList := TList<TRipBoundary>.Create;
    try
      for ObjectIndex := 0 to ScreenObjectList.Count - 1 do
      begin
        AScreenObject := ScreenObjectList[ObjectIndex].ScreenObject;
        if (AScreenObject.ModflowRipBoundary <> nil)
          and AScreenObject.ModflowRipBoundary.Used then
        begin
          RipList.Add(AScreenObject.ModflowRipBoundary);
        end;
      end;

      if RipList.Count = 0 then
      begin
        Exit;
      end;

      rdgModflowBoundary.BeginUpdate;
      try
        FirstRip := RipList[0];
        RipCollection := FirstRip.Values as TripCollection;
        seNumberOfTimes.AsInteger := RipCollection.Count;
        seNumberOfTimes.OnChange(nil);

        RipPlantGroups := frmGoPhast.PhastModel.RipPlantGroups;
        for TimeIndex := 0 to RipCollection.Count - 1 do
        begin
          RipItem := RipCollection[TimeIndex] as TRipItem;
          Assert(RipItem.CoverageID.Count = RipPlantGroups.Count);
          rdgModflowBoundary.RealValue[Ord(rcStartTime),TimeIndex+1+PestRowOffset] :=
            RipItem.StartTime;
          rdgModflowBoundary.RealValue[Ord(rcEndTime),TimeIndex+1+PestRowOffset] :=
            RipItem.EndTime;
          rdgModflowBoundary.Cells[Ord(rcGroundElevation),TimeIndex+1+PestRowOffset] :=
            RipItem.LandElevation;
          CoverageFormulas := RipItem.Coverages;
          Assert(CoverageFormulas.Count = RipPlantGroups.Count);
          for PlantGroupIndex := 0 to RipPlantGroups.Count - 1 do
          begin
            ColIndex := Ord(rcCoverageStart) + PlantGroupIndex;
            AnRipGroup := RipPlantGroups[PlantGroupIndex];
            (Assert(RipItem.CoverageID[PlantGroupIndex].Value = AnRipGroup.ID));
            rdgModflowBoundary.Cells[ColIndex, TimeIndex+1+PestRowOffset] :=
              CoverageFormulas[PlantGroupIndex];
          end;
        end;

        for BoundaryIndex := 1 to RipList.Count - 1 do
        begin
          RipBoundary := RipList[BoundaryIndex];
          if not FirstRip.IsSame(RipBoundary) then
          begin
            ClearGrid(rdgModflowBoundary);
            FMultipleDifferentObjects := True;
            break;
          end;
        end;

      finally
        rdgModflowBoundary.EndUpdate;
      end;


    finally
      RipList.Free;
    end;
  finally
    Changing := False;
  end;
end;

procedure TframeScreenObjectRIP.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  BoundaryUsed: Boolean;
  RowIndex: Integer;
  StartTime: double;
  EndTime: double;
  AFormula: string;
  Boundary: TRipBoundary;
  RipCollection: TRipCollection;
  AnItem: TRipItem;
  CoverageFormulas: TStringList;
  ColIndex: Integer;
  CoverageIDs: TIntegerCollection;
  PlantGroups: TRipPlantGroups;
  InvalidateModelEvent: TNotifyEvent;
begin
  InvalidateModelEvent := nil;
  CoverageIDs := TIntegerCollection.Create(InvalidateModelEvent);
  try
    PlantGroups := frmGoPhast.PhastModel.RipPlantGroups;
    for index := 0 to PlantGroups.Count - 1 do
    begin
      CoverageIDs.Add.Value := PlantGroups[index].ID;
    end;
    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      Boundary := Item.ScreenObject.ModflowRipBoundary;
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
        if FMultipleDifferentObjects
          and (rdgModflowBoundary.Cells[Ord(rcStartTime), 1+PestRowOffset] = '') then
        begin
          Exit;
        end;
        if Boundary = nil then
        begin
          Item.ScreenObject.CreateRipBoundary;
          Boundary := Item.ScreenObject.ModflowRipBoundary;
        end;

        RipCollection := Boundary.Values as TRipCollection;
        while RipCollection.Count > seNumberOfTimes.AsInteger do
        begin
          RipCollection.Last.Free;
        end;
        while RipCollection.Count < seNumberOfTimes.AsInteger do
        begin
          RipCollection.Add;
        end;
        for RowIndex := seNumberOfTimes.AsInteger downto 1 do
        begin
          if TryStrToFloat(rdgModflowBoundary.Cells[Ord(rcStartTime), RowIndex+PestRowOffset], StartTime)
            and TryStrToFloat(rdgModflowBoundary.Cells[Ord(rcEndTime), RowIndex+PestRowOffset], EndTime) then
          begin
            AnItem := RipCollection.Items[RowIndex-1] as TRipItem;
            AnItem.StartTime := StartTime;
            AnItem.EndTime := EndTime;

            AFormula := rdgModflowBoundary.Cells[Ord(rcGroundElevation), RowIndex+PestRowOffset];
            if (AFormula <> '')  then
            begin
              AnItem.LandElevation := AFormula;
            end
            else if AnItem.LandElevation = '' then
            begin
              AnItem.LandElevation := '0';
            end;

            CoverageFormulas := TStringList.Create;
            try
              CoverageFormulas.Assign(AnItem.Coverages);

              for ColIndex := Ord(rcCoverageStart) to rdgModflowBoundary.ColCount - 1 do
              begin
                AFormula := rdgModflowBoundary.Cells[ColIndex, RowIndex+PestRowOffset];
                while CoverageFormulas.Count <= ColIndex-Ord(rcCoverageStart) do
                begin
                  CoverageFormulas.Add('');
                end;
                if (AFormula <> '')  then
                begin
                  CoverageFormulas[ColIndex-Ord(rcCoverageStart)] :=AFormula ;
                end
                else if CoverageFormulas[ColIndex-Ord(rcCoverageStart)] = '' then
                begin
                  CoverageFormulas[ColIndex-Ord(rcCoverageStart)] := '0';
                end;
              end;
              AnItem.Coverages := CoverageFormulas;
              AnItem.CoverageID := CoverageIDs;
            finally
              CoverageFormulas.Free;
            end;
          end
          else
          begin
            RipCollection.Items[RowIndex-1].Free;
          end;
        end
      end;
    end;
  finally
    CoverageIDs.Free;
  end;

end;

procedure TframeScreenObjectRIP.InitializeGrid;
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
  RipPlantGroups: TRipPlantGroups;
  PlantGroupIndex: Integer;
  AnRipGroup: TRipPlantGroup;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    RipPlantGroups := frmGoPhast.PhastModel.RipPlantGroups;
    rdgModflowBoundary.ColCount := Ord(rcCoverageStart) + RipPlantGroups.Count;
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithStartTimes(rdgModflowBoundary, Ord(rcStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithEndTimes(rdgModflowBoundary, Ord(rcEndTime));
    for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.WordWrapCaptions := True;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
    end;
    for ColIndex := Ord(rcGroundElevation) to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.ButtonCaption := StrFormulaButtonCaption;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonUsed := True;
      AColumn.ComboUsed := False;
    end;
    AColumn := rdgModflowBoundary.Columns[Ord(rcStartTime)];
    AColumn.ComboUsed := True;
//    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;
    AColumn := rdgModflowBoundary.Columns[Ord(rcEndTime)];
    AColumn.ComboUsed := True;
//    AColumn.LimitToList := True;
    AColumn.ButtonUsed := False;
    rdgModflowBoundary.Cells[Ord(rcStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(rcEndTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(rcGroundElevation), 0] := StrGroundElevationHS;
    for PlantGroupIndex := 0 to RipPlantGroups.Count - 1 do
    begin
      ColIndex := Ord(rcCoverageStart) + PlantGroupIndex;
      AnRipGroup := RipPlantGroups[PlantGroupIndex];
      rdgModflowBoundary.Cells[ColIndex, 0] :=
        AnRipGroup.Name + Format(' (fCov(%d))', [PlantGroupIndex + 1]);
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  rdgModflowBoundary.BeginUpdate;
  try
    for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.AutoAdjustColWidths := False;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
end;

procedure TframeScreenObjectRIP.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

end.
