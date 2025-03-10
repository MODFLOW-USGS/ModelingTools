unit frameScreenObjectCustomFmp4IntBoundaryUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ModflowFmp4IntBoundaryUnit, UndoItemsScreenObjects;

type
   TFmpIntColumns = (fcStartTime, fcEndTime, fcValue);

  TframeScreenObjectCustomFmp4IntBoundary = class(TframeScreenObjectNoParam)
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure seNumberOfTimesChange(Sender: TObject);
  private
    FValuesCleared: Boolean;
    FOnEdited: TNotifyEvent;
    FGettingData: Boolean;
    FCleared: Boolean;
    procedure Edited;
    { Private declarations }
  protected
    function GetValueDescription: string; virtual; abstract;
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4IntBoundary; virtual; abstract;
    function CreateBoundary: TFmp4IntBoundary; virtual; abstract;
    procedure InitializeGrid;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    { Public declarations }
  end;

var
  frameScreenObjectCustomFmp4IntBoundary: TframeScreenObjectCustomFmp4IntBoundary;

implementation

uses
  ModflowBoundaryUnit, GoPhastTypes;

{$R *.dfm}

procedure TframeScreenObjectCustomFmp4IntBoundary.Edited;
begin
  if Assigned(FOnEdited) and not FGettingData then
  begin
    FOnEdited(self);
    FCleared := False;
  end;
end;

procedure TframeScreenObjectCustomFmp4IntBoundary.GetData(
  List: TScreenObjectEditCollection);
var
  Index: Integer;
  Boundary: TFmp4IntBoundary;
  Item: TScreenObjectEditItem;
  FirstBoundary: TFmp4IntBoundary;
  ValueIndex: Integer;
  FmpItem: TFmp4IntItem;
  RowIndex: Integer;
  ColIndex: Integer;
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
            PestMethodAssigned[Ord(fcValue)] := False;
            PestModifierAssigned[Ord(fcValue)] := False;
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
          PestMethod[Ord(fcValue)] := Boundary.PestBoundaryMethod[0];
          PestModifier[Ord(fcValue)] := Boundary.PestBoundaryFormula[0];
          seNumberOfTimes.AsInteger := FirstBoundary.Values.Count;
          for ValueIndex := 0 to FirstBoundary.Values.Count - 1 do
          begin
            FmpItem := FirstBoundary.Values[ValueIndex] as TFmp4IntItem;
            RowIndex := ValueIndex + 1 + PestRowOffset;
            rdgModflowBoundary.Cells[Ord(fcStartTime), RowIndex] := FloatToStr(FmpItem.StartTime);
            rdgModflowBoundary.Cells[Ord(fcEndTime), RowIndex] := FloatToStr(FmpItem.EndTime);
            rdgModflowBoundary.Cells[Ord(fcValue), RowIndex] := FmpItem.BoundaryFormula[0];
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
    FGettingData := False;
  end;
end;

procedure TframeScreenObjectCustomFmp4IntBoundary.InitializeGrid;
begin
  ConductanceColumn := -1;
  rdgModflowBoundary.BeginUpdate;
  try
    InitializeNoParamFrame(nil);
    rdgModflowBoundary.Cells[2,0] := GetValueDescription;
    GetStartTimes(0);
    GetEndTimes(1);
  finally
    rdgModflowBoundary.EndUpdate;
  end;

end;

procedure TframeScreenObjectCustomFmp4IntBoundary.rdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCustomFmp4IntBoundary.seNumberOfTimesChange(
  Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCustomFmp4IntBoundary.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  NewBoundary: TFmp4IntBoundary;
  RowIndex: Integer;
  StartTime: double;
  Endtime: double;
  FmpItem: TFmp4IntItem;
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TFmp4IntBoundary;
  BoundaryUsed: Boolean;
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
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(fcStartTime), RowIndex], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[Ord(fcEndTime), RowIndex], Endtime)
          and (rdgModflowBoundary.Cells[Ord(fcValue), RowIndex] <> '') then
        begin
          FmpItem := NewBoundary.Values.Add as TFmp4IntItem;
          FmpItem.StartTime := StartTime;
          FmpItem.EndTime := Endtime;
          FmpItem.FmpIntValue :=
            rdgModflowBoundary.Cells[Ord(fcValue), RowIndex];
        end;
      end;
      NewBoundary.PestBoundaryMethod[0] := PestMethod[Ord(fcValue)];
      NewBoundary.PestBoundaryFormula[0] := PestModifier[Ord(fcValue)];
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

        end;
        Boundary.Assign(NewBoundary);
      end;
    end;
  finally
    NewBoundary.Free;
  end;
end;

end.
