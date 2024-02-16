unit frameScreenObjectTransientSUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects;

type
  TTvsColumns = (tcStartTime, tcEndTime, tcSS, tcSY);

  TframeScreenObjectTransientS = class(TframeScreenObjectNoParam)
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
        const Value: string);
  private
    FTimeDataCleared: Boolean;
    FGettingData: Boolean;
    procedure InitializeControls;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
  end;

var
  frameScreenObjectTransientS: TframeScreenObjectTransientS;

implementation

uses
  GoPhastTypes, ScreenObjectUnit, ModflowTvsUnit, frmGoPhastUnit,
  DataSetNamesUnit;

{$R *.dfm}

{ TframeScreenObjectTransientS }

procedure TframeScreenObjectTransientS.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  FoundFirst: Boolean;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  TvsBound: TTvsBoundary;
  FirstTvkBound: TTvsBoundary;
  TimeIndex: Integer;
  TvkItem: TTvsItem;
begin
  FGettingData := True;
  InitializeControls;
  rdgModflowBoundary.BeginUpdate;
  try
    FirstTvkBound := nil;
    FoundFirst := False;
    FTimeDataCleared := False;
    for ScreenObjectIndex := 0 to ScreenObjectList.Count - 1 do
    begin
      AScreenObject := ScreenObjectList[ScreenObjectIndex].ScreenObject;
      if (AScreenObject.ModflowTvsBoundary <> nil)
        and AScreenObject.ModflowTvsBoundary.Used then
      begin
        TvsBound := AScreenObject.ModflowTvsBoundary;
        if not FoundFirst then
        begin
          FoundFirst := True;
          FirstTvkBound := TvsBound;
          seNumberOfTimes.AsInteger := TvsBound.Values.Count;
          for TimeIndex := 0 to TvsBound.Values.Count - 1 do
          begin
            TvkItem := TvsBound.Values[TimeIndex] as TTvsItem;
            rdgModflowBoundary.RealValue[Ord(tcStartTime), TimeIndex+1 + PestRowOffset]
              := TvkItem.StartTime;
            rdgModflowBoundary.RealValue[Ord(tcEndTime), TimeIndex+1 + PestRowOffset]
              := TvkItem.EndTime;
            rdgModflowBoundary.Cells[Ord(tcSS), TimeIndex+1 + PestRowOffset]
              := TvkItem.SS;
            rdgModflowBoundary.Cells[Ord(tcSY), TimeIndex+1 + PestRowOffset]
              := TvkItem.SY;
          end;

          PestModifier[Ord(tcSS)] := TvsBound.PestSSFormula;
          PestMethod[Ord(tcSS)] := TvsBound.PestSSMethod;

          PestModifier[Ord(tcSY)] := TvsBound.PestSYFormula;
          PestMethod[Ord(tcSY)] := TvsBound.PestSYMethod;
        end
        else
        begin
          if FirstTvkBound.PestSSFormula <> TvsBound.PestSSFormula then
          begin
            PestModifierAssigned[Ord(tcSS)] := False
          end;
          if FirstTvkBound.PestSSMethod <> TvsBound.PestSSMethod then
          begin
            PestMethodAssigned[Ord(tcSS)] := False;
          end;

          if FirstTvkBound.PestSYFormula <> TvsBound.PestSYFormula then
          begin
            PestModifierAssigned[Ord(tcSY)] := False
          end;
          if FirstTvkBound.PestSYMethod <> TvsBound.PestSYMethod then
          begin
            PestMethodAssigned[Ord(tcSY)] := False;
          end;

          if not FTimeDataCleared then
          begin
            if not FirstTvkBound.Values.IsSame(TvsBound.Values) then
            begin
              FTimeDataCleared := True;
              ClearGrid(rdgModflowBoundary);
            end;
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
    FGettingData := False;
  end;
end;

procedure TframeScreenObjectTransientS.InitializeControls;
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithStartTimes(rdgModflowBoundary, Ord(tcStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithStartTimes(rdgModflowBoundary, Ord(tcStartTime));
    ClearGrid(rdgModflowBoundary);
    for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.AutoAdjustColWidths := True;
    end;
    rdgModflowBoundary.Cells[Ord(tcStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(tcEndTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(tcSS), 0] := StrTransientSS;
    rdgModflowBoundary.Cells[Ord(tcSY), 0] := StrTransientSY;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  finally
    rdgModflowBoundary.EndUpdate
  end;

  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    AColumn := rdgModflowBoundary.Columns[ColIndex];
    AColumn.AutoAdjustColWidths := False;
  end;
end;

procedure TframeScreenObjectTransientS.rdgModflowBoundarySetEditText(Sender:
    TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if not FGettingData then
  begin
    FTimeDataCleared := False;
  end;
end;

procedure TframeScreenObjectTransientS.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TTvsBoundary;
  BoundaryUsed: Boolean;
  TimeIndex: Integer;
  TvkItem: TTvsItem;
  function NonBlank(const Formula: string): string;
  begin
//    if Formula = '' then
//    begin
//      result := '0';
//    end
//    else
//    begin
      result := Formula;
//    end;
  end;
begin
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Boundary := Item.ScreenObject.ModflowTvsBoundary;
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
        Item.ScreenObject.CreateTvkBoundary;
        Boundary := Item.ScreenObject.ModflowTvsBoundary;
      end;
    end;

    if PestModifierAssigned[ Ord(tcSS)] then
    begin
      Boundary.PestSSFormula := PestModifier[ Ord(tcSS)];
    end;
    if PestMethodAssigned[ Ord(tcSS)] then
    begin
      Boundary.PestSSMethod := PestMethod[ Ord(tcSS)];
    end;

    if PestModifierAssigned[ Ord(tcSY)] then
    begin
      Boundary.PestSYFormula := PestModifier[ Ord(tcSY)];
    end;
    if PestMethodAssigned[ Ord(tcSY)] then
    begin
      Boundary.PestSYMethod := PestMethod[ Ord(tcSY)];
    end;

    if not FTimeDataCleared then
    begin
      Boundary.Values.Count := seNumberOfTimes.AsInteger;
      for TimeIndex := 0 to Boundary.Values.Count - 1 do
      begin
        TvkItem := Boundary.Values[TimeIndex] as TTvsItem;
        TvkItem.StartTime := rdgModflowBoundary.RealValueDefault[Ord(tcStartTime), TimeIndex+1 + PestRowOffset, 0];
        TvkItem.EndTime := rdgModflowBoundary.RealValueDefault[Ord(tcEndTime), TimeIndex+1 + PestRowOffset, 0];
        TvkItem.SS := NonBlank(rdgModflowBoundary.Cells[Ord(tcSS), TimeIndex+1 + PestRowOffset]);
        TvkItem.SY := NonBlank(rdgModflowBoundary.Cells[Ord(tcSY), TimeIndex+1 + PestRowOffset]);
      end;
    end;

  end;
end;

end.

