unit frameScreenObjectCfpRechargeFractionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects;

type
  TCfpRechargeFracColumns = (crcStartTime, crcEndTime, crcFrac, crcCADS);

  TframeScreenObjectCfpRechargeFraction = class(TframeScreenObjectNoParam)
    procedure edCadsChange(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
        const Value: string);
    procedure seNumberOfTimesChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FGridCleared: Boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    procedure InitializeControls;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameScreenObjectCfpRechargeFraction: TframeScreenObjectCfpRechargeFraction;

implementation

uses
  ScreenObjectUnit, ModflowCfpRechargeUnit, GoPhastTypes, ModflowBoundaryUnit;

{$R *.dfm}

{ TframeScreenObjectCfpRechargeFraction }

procedure TframeScreenObjectCfpRechargeFraction.DoChange;
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

procedure TframeScreenObjectCfpRechargeFraction.edCadsChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpRechargeFraction.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TScreenObjectList;
  Index: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  ABoundary: TCfpRchFractionBoundary;
  ScreenObjectIndex: Integer;
  TimeIndex: Integer;
  AnItem: TCfpRchFractionItem;
  FirstBoundary: TCfpRchFractionBoundary;
begin
  FGridCleared := False;
  Assert(ScreenObjectList.Count >= 1);
  Changing := True;
  try
    InitializeControls;
    ListOfScreenObjects := TScreenObjectList.Create;
    try

      Item := ScreenObjectList[0];
      AScreenObject := Item.ScreenObject;
      AScreenObject.CreateCfpRchFraction;
      ABoundary := AScreenObject.ModflowCfpRchFraction;
      Assert(ABoundary <> nil);
      InitializeNoParamFrame(ABoundary);
      if not ABoundary.Used then
      begin
        AScreenObject.ModflowCfpRchFraction := nil;
      end;

      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        ABoundary := AScreenObject.ModflowCfpRchFraction;
        if (ABoundary <> nil) and ABoundary.Used then
        begin
          ListOfScreenObjects.Add(AScreenObject);
        end;
      end;
      FirstBoundary := nil;
      if ListOfScreenObjects.Count > 0 then
      begin
        ABoundary := ListOfScreenObjects[0].ModflowCfpRchFraction;
        FirstBoundary := ABoundary;
//        if edCads.Enabled then
//        begin
//          edCads.Text := ABoundary.DrainableStorageWidth;
//        end;
        seNumberOfTimes.AsInteger := ABoundary.Values.Count;
        for TimeIndex := 0 to ABoundary.Values.Count - 1 do
        begin
          AnItem := ABoundary.Values[TimeIndex] as TCfpRchFractionItem;
          rdgModflowBoundary.Cells[Ord(crcStartTime), TimeIndex+PestRowOffset+1]
            := FloatToStr(AnItem.StartTime);
          rdgModflowBoundary.Cells[Ord(crcEndTime), TimeIndex+PestRowOffset+1]
            := FloatToStr(AnItem.EndTime);
          rdgModflowBoundary.Cells[Ord(crcFrac), TimeIndex+PestRowOffset+1]
            := AnItem.CfpRechargeFraction;
          rdgModflowBoundary.Cells[Ord(crcCADS), TimeIndex+PestRowOffset+1]
            := AnItem.CfpCadsRechargeFraction;
        end;
      end;
      for ScreenObjectIndex := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ABoundary := ListOfScreenObjects[ScreenObjectIndex].ModflowCfpRchFraction;
//        if edCads.Enabled and (edCads.Text <> ABoundary.DrainableStorageWidth) then
//        begin
//          edCads.Text := ''
//        end;
        if FGridCleared then
        begin
          Continue;
        end;
        if not FirstBoundary.Values.IsSame(ABoundary.Values) then
        begin
          ClearGrid(rdgModflowBoundary);
          FGridCleared := True;
          InitializeNoParamFrame(ABoundary);
        end;
      end;
    finally
      ListOfScreenObjects.Free;
    end;
  finally
    Changing := False;
  end;
end;


procedure TframeScreenObjectCfpRechargeFraction.InitializeControls;
begin
  rdgModflowBoundary.RowHeights[0] := rdgModflowBoundary.DefaultRowHeight;
end;

procedure TframeScreenObjectCfpRechargeFraction.rdgModflowBoundarySetEditText(
    Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if not Changing then
  begin
    FGridCleared := False;
  end;
  DoChange;
  inherited;
end;

procedure TframeScreenObjectCfpRechargeFraction.seNumberOfTimesChange(Sender:
    TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpRechargeFraction.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TCfpRchFractionBoundary;
  BoundaryUsed: Boolean;
  TimeIndex: Integer;
  TimeCount: Integer;
  StartTime: Double;
  EndTime: Double;
  AnItem: TCfpRchFractionItem;
begin
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    Item := List[ScreenObjectIndex];
    ScreenObject := Item.ScreenObject;
    Boundary := ScreenObject.ModflowCfpRchFraction;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;
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
        ScreenObject.CreateCfpRchFraction;
        Boundary := ScreenObject.ModflowCfpRchFraction;
      end;
      if Boundary <> nil then
      begin
//        if edCads.Enabled and (edCads.Text <> '') then
//        begin
//          Boundary.DrainableStorageWidth := edCads.Text;
//        end;
      end;
      if not FGridCleared then
      begin
        TimeCount := 0;
        for TimeIndex := 0 to seNumberOfTimes.AsInteger - 1 do
        begin
          if TryStrToFloat(rdgModflowBoundary.Cells[Ord(crcStartTime),
            TimeIndex+PestRowOffset+1], StartTime)
            and TryStrToFloat(rdgModflowBoundary.Cells[Ord(crcEndTime),
            TimeIndex+PestRowOffset+1], EndTime) then
          begin
            if TimeCount  < Boundary.Values.Count then
            begin
              AnItem := Boundary.Values[TimeCount] as TCfpRchFractionItem;
            end
            else
            begin
              AnItem := Boundary.Values.Add as TCfpRchFractionItem;
            end;
            AnItem.StartTime := StartTime;
            AnItem.EndTime := EndTime;
            AnItem.CfpRechargeFraction := rdgModflowBoundary.Cells[Ord(crcFrac), TimeIndex+PestRowOffset+1];
            AnItem.CfpCadsRechargeFraction := rdgModflowBoundary.Cells[Ord(crcCADS), TimeIndex+PestRowOffset+1];
            Inc(TimeCount);
          end;
        end;
        if TimeCount < Boundary.Values.Count then
        begin
          Boundary.Values.Count := TimeCount;
        end;
      end;
    end;
  end;
end;

end.
