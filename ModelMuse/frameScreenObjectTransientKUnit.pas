unit frameScreenObjectTransientKUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects;

type
  TTvkColumns = (tcStartTime, tcEndTime, tcKx, tcKy, tcKz);

  TframeScreenObjectTransientK = class(TframeScreenObjectNoParam)
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
    { Public declarations }
  end;

var
  frameScreenObjectTransientK: TframeScreenObjectTransientK;

implementation

uses
  GoPhastTypes, ScreenObjectUnit, ModflowTvkUnit, frmGoPhastUnit,
  DataSetNamesUnit, ModflowPackageSelectionUnit;

{$R *.dfm}

{ TframeScreenObjectTransientK }

procedure TframeScreenObjectTransientK.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  FoundFirst: Boolean;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  TvkBound: TTvkBoundary;
  FirstTvkBound: TTvkBoundary;
  TimeIndex: Integer;
  TvkItem: TTvkItem;
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
      if (AScreenObject.ModflowTvkBoundary <> nil)
        and AScreenObject.ModflowTvkBoundary.Used then
      begin
        TvkBound := AScreenObject.ModflowTvkBoundary;
        if not FoundFirst then
        begin
          FoundFirst := True;
          FirstTvkBound := TvkBound;
          seNumberOfTimes.AsInteger := TvkBound.Values.Count;
          for TimeIndex := 0 to TvkBound.Values.Count - 1 do
          begin
            TvkItem := TvkBound.Values[TimeIndex] as TTvkItem;
            rdgModflowBoundary.RealValue[Ord(tcStartTime), TimeIndex+1 + PestRowOffset]
              := TvkItem.StartTime;
            rdgModflowBoundary.RealValue[Ord(tcEndTime), TimeIndex+1 + PestRowOffset]
              := TvkItem.EndTime;
            rdgModflowBoundary.Cells[Ord(tcKx), TimeIndex+1 + PestRowOffset]
              := TvkItem.K;
            rdgModflowBoundary.Cells[Ord(tcKy), TimeIndex+1 + PestRowOffset]
              := TvkItem.K22;
            rdgModflowBoundary.Cells[Ord(tcKz), TimeIndex+1 + PestRowOffset]
              := TvkItem.K33;
          end;

          PestModifier[Ord(tcKx)] := TvkBound.PestKFormula;
          PestMethod[Ord(tcKx)] := TvkBound.PestKMethod;

          PestModifier[Ord(tcKy)] := TvkBound.PestK22Formula;
          PestMethod[Ord(tcKy)] := TvkBound.PestK22Method;

          PestModifier[Ord(tcKz)] := TvkBound.PestK33Formula;
          PestMethod[Ord(tcKz)] := TvkBound.PestK33Method;
        end
        else
        begin
          if FirstTvkBound.PestKFormula <> TvkBound.PestKFormula then
          begin
            PestModifierAssigned[Ord(tcKx)] := False
          end;
          if FirstTvkBound.PestKMethod <> TvkBound.PestKMethod then
          begin
            PestMethodAssigned[Ord(tcKx)] := False;
          end;

          if FirstTvkBound.PestK22Formula <> TvkBound.PestK22Formula then
          begin
            PestModifierAssigned[Ord(tcKy)] := False
          end;
          if FirstTvkBound.PestK22Method <> TvkBound.PestK22Method then
          begin
            PestMethodAssigned[Ord(tcKy)] := False;
          end;

          if FirstTvkBound.PestK33Formula <> TvkBound.PestK33Formula then
          begin
            PestModifierAssigned[Ord(tcKz)] := False
          end;
          if FirstTvkBound.PestK33Method <> TvkBound.PestK33Method then
          begin
            PestMethodAssigned[Ord(tcKz)] := False;
          end;

          if not FTimeDataCleared then
          begin
            if not FirstTvkBound.Values.IsSame(TvkBound.Values) then
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

procedure TframeScreenObjectTransientK.InitializeControls;
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
  NpfPackage: TNpfPackage;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    NpfPackage := frmGoPhast.PhastModel.ModflowPackages.NpfPackage;
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
    rdgModflowBoundary.Cells[Ord(tcKx), 0] := StrTransientKx;
    if NpfPackage.UseHorizontalAnisotropy then
    begin
      rdgModflowBoundary.Cells[Ord(tcKy), 0] := StrTransientKyKx;
    end
    else
    begin
      rdgModflowBoundary.Cells[Ord(tcKy), 0] := StrTransientKy;
    end;
    if NpfPackage.UseVerticalAnisotropy then
    begin
      rdgModflowBoundary.Cells[Ord(tcKz), 0] := StrTransientKzKx;
    end
    else
    begin
      rdgModflowBoundary.Cells[Ord(tcKz), 0] := StrTransientKz;
    end;
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

procedure TframeScreenObjectTransientK.rdgModflowBoundarySetEditText(Sender:
    TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if not FGettingData then
  begin
    FTimeDataCleared := False;
  end;
end;

procedure TframeScreenObjectTransientK.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TTvkBoundary;
  BoundaryUsed: Boolean;
  TimeIndex: Integer;
  TvkItem: TTvkItem;
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
    Boundary := Item.ScreenObject.ModflowTvkBoundary;
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
        Boundary := Item.ScreenObject.ModflowTvkBoundary;
      end;
    end;

    if PestModifierAssigned[ Ord(tcKx)] then
    begin
      Boundary.PestKFormula := PestModifier[ Ord(tcKx)];
    end;
    if PestMethodAssigned[ Ord(tcKx)] then
    begin
      Boundary.PestKMethod := PestMethod[ Ord(tcKx)];
    end;

    if PestModifierAssigned[ Ord(tcKy)] then
    begin
      Boundary.PestK22Formula := PestModifier[ Ord(tcKy)];
    end;
    if PestMethodAssigned[ Ord(tcKy)] then
    begin
      Boundary.PestK22Method := PestMethod[ Ord(tcKy)];
    end;

    if PestModifierAssigned[ Ord(tcKz)] then
    begin
      Boundary.PestK33Formula := PestModifier[ Ord(tcKz)];
    end;
    if PestMethodAssigned[ Ord(tcKz)] then
    begin
      Boundary.PestK33Method := PestMethod[ Ord(tcKz)];
    end;
    if not FTimeDataCleared then
    begin
      Boundary.Values.Count := seNumberOfTimes.AsInteger;
      for TimeIndex := 0 to Boundary.Values.Count - 1 do
      begin
        TvkItem := Boundary.Values[TimeIndex] as TTvkItem;
        TvkItem.StartTime := rdgModflowBoundary.RealValueDefault[Ord(tcStartTime), TimeIndex+1 + PestRowOffset, 0];
        TvkItem.EndTime := rdgModflowBoundary.RealValueDefault[Ord(tcEndTime), TimeIndex+1 + PestRowOffset, 0];
        TvkItem.K := NonBlank(rdgModflowBoundary.Cells[Ord(tcKx), TimeIndex+1 + PestRowOffset]);
        TvkItem.K22 := NonBlank(rdgModflowBoundary.Cells[Ord(tcKy), TimeIndex+1 + PestRowOffset]);
        TvkItem.K33 := NonBlank(rdgModflowBoundary.Cells[Ord(tcKz), TimeIndex+1 + PestRowOffset]);
      end;
    end;

  end;
end;

end.
