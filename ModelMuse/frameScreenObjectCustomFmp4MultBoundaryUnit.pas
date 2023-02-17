unit frameScreenObjectCustomFmp4MultBoundaryUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ModflowFmp4LandUseBoundaryUnit, UndoItemsScreenObjects,
  ModflowFmpCropUnit;

type
   TFmpLanduseColumns = (fclStartTime, fclEndTime, fclValue);

  TframeScreenObjectCustomFmp4MultBoundary = class(TframeScreenObjectNoParam)
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    FOnEdited: TNotifyEvent;
    FGettingData: Boolean;
    FCleared: Boolean;
    FValuesCleared: Boolean;
    FCrops: TCropCollection;
    { Private declarations }
    procedure Edited;
  protected
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4LandUseBoundary; virtual; abstract;
    function CreateBoundary: TFmp4LandUseBoundary; virtual; abstract;
    procedure InitializeGrid;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    { Public declarations }
  end;

var
  frameScreenObjectCustomFmp4MultBoundary: TframeScreenObjectCustomFmp4MultBoundary;

implementation

uses
  frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4MultBoundary }

procedure TframeScreenObjectCustomFmp4MultBoundary.Edited;
begin
  if Assigned(FOnEdited) and not FGettingData then
  begin
    FOnEdited(self);
    FCleared := False;
  end;
end;

procedure TframeScreenObjectCustomFmp4MultBoundary.GetData(
  List: TScreenObjectEditCollection);
var
  Index: Integer;
  Boundary: TFmp4LandUseBoundary;
  Item: TScreenObjectEditItem;
  FirstBoundary: TFmp4LandUseBoundary;
  ValueIndex: Integer;
  FmpItem: TFmp4LandUseItem;
  RowIndex: Integer;
  ColIndex: Integer;
  CropIndex: Integer;
begin
  FGettingData := True;
  rdgModflowBoundary.BeginUpdate;
  try
    InitializeGrid;
    FValuesCleared := False;
    Assert(List.Count >= 1);
    FirstBoundary := nil;
    FCleared := False;
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Boundary := GetBoundary(Item);
      if (Boundary <> nil) and Boundary.Used then
      begin
        if FirstBoundary <> nil then
        begin
          if not FirstBoundary.IsSame(Boundary) then
          begin
            FCleared := True;
            for CropIndex := 0 to FCrops.Count -1 do
            begin
              PestMethodAssigned[Ord(fclValue)+CropIndex] := False;
              PestModifierAssigned[Ord(fclValue)+CropIndex] := False;
            end;
            for RowIndex := 1 + PestRowOffset to rdgModflowBoundary.RowCount - 1 do
            begin
              for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
              begin
                rdgModflowBoundary.Cells[ColIndex, RowIndex] := '';
              end;
            end;
          end;
        end
        else
        begin
          FirstBoundary := Boundary;
          for CropIndex := 0 to FCrops.Count -1 do
          begin
            PestMethod[Ord(fclValue)+CropIndex] := Boundary.PestBoundaryMethod[CropIndex];
            PestModifier[Ord(fclValue)+CropIndex] := Boundary.PestBoundaryFormula[CropIndex];
          end;
          seNumberOfTimes.AsInteger := FirstBoundary.Values.Count;
          for ValueIndex := 0 to FirstBoundary.Values.Count - 1 do
          begin
            FmpItem := FirstBoundary.Values[ValueIndex] as TFmp4LandUseItem;
            RowIndex := ValueIndex + 1 + PestRowOffset;
            rdgModflowBoundary.Cells[Ord(fclStartTime), RowIndex] := FloatToStr(FmpItem.StartTime);
            rdgModflowBoundary.Cells[Ord(fclEndTime), RowIndex] := FloatToStr(FmpItem.EndTime);
            for CropIndex := 0 to FCrops.Count -1 do
            begin
              rdgModflowBoundary.Cells[Ord(fclValue)+CropIndex, RowIndex] := FmpItem.BoundaryFormula[CropIndex];
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

procedure TframeScreenObjectCustomFmp4MultBoundary.InitializeGrid;
var
  CropIndex: Integer;
begin
  ConductanceColumn := -1;
  rdgModflowBoundary.BeginUpdate;
  try
    FCrops := frmGoPhast.PhastModel.FmpCrops;
    rdgModflowBoundary.ColCount := FCrops.Count +2;
    InitializeNoParamFrame(nil);
    for CropIndex := 0 to FCrops.Count - 1 do
    begin
      rdgModflowBoundary.Cells[CropIndex+2, 0] := FCrops[CropIndex].CropName;
    end;
    GetStartTimes(0);
    GetEndTimes(1);
  finally
    rdgModflowBoundary.EndUpdate;
  end;
end;

procedure TframeScreenObjectCustomFmp4MultBoundary.rdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCustomFmp4MultBoundary.seNumberOfTimesChange(
  Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCustomFmp4MultBoundary.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  NewBoundary: TFmp4LandUseBoundary;
  RowIndex: Integer;
  StartTime: double;
  Endtime: double;
  FmpItem: TFmp4LandUseItem;
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TFmp4LandUseBoundary;
  BoundaryUsed: Boolean;
  CropIndex: Integer;
begin
  if FCleared then
  begin
    Exit;
  end;

  NewBoundary := nil;
  try
    if SetAll or not ClearAll then
    begin
      NewBoundary := CreateBoundary;
      for RowIndex := 1+PestRowOffset to rdgModflowBoundary.RowCount - 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(fclStartTime), RowIndex], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[Ord(fclEndTime), RowIndex], Endtime)
//          and (rdgModflowBoundary.Cells[Ord(fclValue), RowIndex] <> '')
          then
        begin
          FmpItem := NewBoundary.Values.Add as TFmp4LandUseItem;
          FmpItem.StartTime := StartTime;
          FmpItem.EndTime := Endtime;
          for CropIndex := 0 to FCrops.Count -1 do
          begin
            FmpItem.Fmp4LandUseValues[CropIndex].Value :=
              rdgModflowBoundary.Cells[Ord(fclValue)+CropIndex, RowIndex];
          end;
        end;
      end;
      for CropIndex := 0 to FCrops.Count -1 do
      begin
        NewBoundary.PestBoundaryMethod[CropIndex] := PestMethod[Ord(fclValue)+CropIndex];
        NewBoundary.PestBoundaryFormula[CropIndex] := PestModifier[Ord(fclValue)+CropIndex];
      end;
    end;
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      Item := List[ScreenObjectIndex];
      Boundary := GetBoundary(Item);
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
          Boundary := CreateBoundary;
        end;
        Boundary.Assign(NewBoundary);
      end;
    end;
  finally
    NewBoundary.Free;
  end;
end;

end.
