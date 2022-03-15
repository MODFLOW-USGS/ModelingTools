unit frameScreenObjectFhbHeadUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectNoParamUnit, Grids,
  RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls, UndoItemsScreenObjects, ScreenObjectUnit,
  ModflowFhbUnit;

type
  TFhbHeadColumns = (fhcTime, fhcHead);

  TframeScreenObjectFhbHead = class(TframeScreenObjectNoParam)
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgModflowBoundaryBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
  private
    FChanging: Boolean;
    FOnChange: TNotifyEvent;
    { Private declarations }
    procedure AssignFirstItem(ScreenObject: TScreenObject);
  protected
    FInitialListOfScreenObjects: TScreenObjectList;
    procedure InitializeControls; virtual;

    procedure DoChange; virtual;
    property Changing: Boolean read FChanging write FChanging;
    function GetBoundary(AScreenObject: TScreenObject): TFhbHeadBoundary; virtual;
    function CreateNewBoundary: TFhbHeadBoundary; virtual;
    procedure CreateScreenObjectBoundary(AScreenObject: TScreenObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameScreenObjectFhbHead: TframeScreenObjectFhbHead;

implementation

uses
  frmGoPhastUnit, ModflowTimeUnit, GoPhastTypes;

{$R *.dfm}

{ TframeScreenObjectFhbHead }

procedure TframeScreenObjectFhbHead.AssignFirstItem(
  ScreenObject: TScreenObject);
const
  BoundaryValuePosition = 0;
var
  ABoundary: TFhbHeadBoundary;
  ItemIndex: Integer;
  FhbItems: TFhbHeadCollection;
  AnItem: TFhbItem;
begin
  ABoundary := GetBoundary(ScreenObject);
  Assert(ABoundary <> nil);
  seNumberOfTimes.AsInteger := ABoundary.Values.Count;
  seNumberOfTimes.OnChange(nil);
  Assert(rdgModflowBoundary.RowCount -1 = ABoundary.Values.Count + PestRowOffset);
  FhbItems := ABoundary.Values as TFhbHeadCollection;
  for ItemIndex := 0 to FhbItems.Count - 1 do
  begin
    AnItem := FhbItems[ItemIndex];
    rdgModflowBoundary.Cells[Ord(fhcTime), ItemIndex+PestRowOffset+1] := FloatToStr(AnItem.StartTime);
    rdgModflowBoundary.Cells[Ord(fhcHead), ItemIndex+PestRowOffset+1] := AnItem.BoundaryValue;
  end;

  PestMethod[Ord(fhcHead)] := ABoundary.PestBoundaryMethod[BoundaryValuePosition];
  PestModifier[Ord(fhcHead)] := ABoundary.PestBoundaryFormula[BoundaryValuePosition];
end;

constructor TframeScreenObjectFhbHead.Create(AOwner: TComponent);
begin
  inherited;
  FLastTimeColumn := 0;
  FInitialListOfScreenObjects:= TScreenObjectList.Create;
end;

function TframeScreenObjectFhbHead.CreateNewBoundary: TFhbHeadBoundary;
begin
  result := TFhbHeadBoundary.Create(nil, nil);
end;

procedure TframeScreenObjectFhbHead.CreateScreenObjectBoundary(
  AScreenObject: TScreenObject);
begin
  AScreenObject.CreateFhbHeadBoundary;
end;

destructor TframeScreenObjectFhbHead.Destroy;
begin
  FInitialListOfScreenObjects.Free;
  inherited;
end;

procedure TframeScreenObjectFhbHead.rdgModflowBoundaryBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
begin
  // Skip inherited.
// inherited;
end;

procedure TframeScreenObjectFhbHead.rdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectFhbHead.DoChange;
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

function TframeScreenObjectFhbHead.GetBoundary(
  AScreenObject: TScreenObject): TFhbHeadBoundary;
begin
  result := AScreenObject.ModflowFhbHeadBoundary;
end;

procedure TframeScreenObjectFhbHead.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  Index: Integer;
  ScreenObjectIndex: integer;
  ABoundary: TFhbHeadBoundary;
  FhbItems: TFhbHeadCollection;
  ItemIndex: Integer;
  AnItem: TFhbItem;
  FirstBoundary: TFhbHeadBoundary;
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
  end;
begin
  Changing := True;
  try
    InitializeControls;

    Assert(ScreenObjectList.Count >= 1);
    FInitialListOfScreenObjects.Clear;
    for Index := 0 to ScreenObjectList.Count - 1 do
    begin
      Item := ScreenObjectList[Index];
      AScreenObject := Item.ScreenObject;
      ABoundary := GetBoundary(AScreenObject);
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        FInitialListOfScreenObjects.Add(AScreenObject);
      end;
    end;
    if FInitialListOfScreenObjects.Count > 0 then
    begin
      AssignFirstItem(FInitialListOfScreenObjects[0]);
      FirstBoundary := GetBoundary(FInitialListOfScreenObjects[0]);

      for ScreenObjectIndex := 1 to FInitialListOfScreenObjects.Count - 1 do
      begin
        ABoundary := GetBoundary(FInitialListOfScreenObjects[ScreenObjectIndex]);
        if (ABoundary.PestFhbBoundaryFormula
          <> FirstBoundary.PestFhbBoundaryFormula)
          or (ABoundary.PestFhbBoundaryMethod
          <> FirstBoundary.PestFhbBoundaryMethod) then
        begin
          ClearGrid;
          Exit;
        end;
        FhbItems := ABoundary.Values as TFhbHeadCollection;
        if FhbItems.Count = seNumberOfTimes.AsInteger then
        begin
          for ItemIndex := 0 to FhbItems.Count - 1 do
          begin
            AnItem := FhbItems[ItemIndex];
            if (rdgModflowBoundary.Cells[Ord(fhcTime), ItemIndex+1]
              <> FloatToStr(AnItem.StartTime))
              or (rdgModflowBoundary.Cells[Ord(fhcHead), ItemIndex+1]
              <> AnItem.BoundaryValue) then
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
    Changing := False;
  end;

end;

procedure TframeScreenObjectFhbHead.InitializeControls;
const
  BoundaryValuePosition = 0;
begin
  rdgModflowBoundary.Columns[Ord(fhcTime)].AutoAdjustColWidths := False;
  rdgModflowBoundary.Columns[Ord(fhcHead)].AutoAdjustColWidths := False;
  seNumberOfTimes.AsInteger := 0;
  seNumberOfTimes.OnChange(nil);
  rdgModflowBoundary.Cells[Ord(fhcTime), 1] := '';
  rdgModflowBoundary.Cells[Ord(fhcHead), 1] := '';
  rdgModflowBoundary.Columns[Ord(fhcTime)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Columns[Ord(fhcHead)].AutoAdjustColWidths := True;
  rdgModflowBoundary.Cells[Ord(fhcTime), 0] := 'Time';
  rdgModflowBoundary.Cells[Ord(fhcHead), 0] := 'Head';
  rdgModflowBoundary.Columns[Ord(fhcHead)].AutoAdjustColWidths := False;
  LayoutMultiRowEditControls;

  rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
  rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  PestMethod[Ord(fhcHead)]
    := TFhbHeadBoundary.DefaultBoundaryMethod(BoundaryValuePosition);
end;

procedure TframeScreenObjectFhbHead.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectFhbHead.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TFhbHeadBoundary;
  BoundaryUsed: Boolean;
  NewBoundary: TFhbHeadBoundary;
  RowIndex: Integer;
  ATime: double;
  FhbItem: TFhbItem;
  FirstTime: Double;
  StressPeriods: TModflowStressPeriods;
  LastTime: Double;
  PriorItem: TFhbItem;
begin
  NewBoundary := nil;
  try
    if SetAll or not ClearAll then
    begin
      NewBoundary := CreateNewBoundary;

      StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
      FirstTime := StressPeriods.First.StartTime;
      LastTime :=  StressPeriods.Last.EndTime;

      PriorItem := nil;
      for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[Ord(fhcTime), RowIndex], ATime)
          and (rdgModflowBoundary.Cells[Ord(fhcHead), RowIndex] <> '') then
        begin
          if PriorItem <> nil then
          begin
            PriorItem.EndTime := ATime;
          end;
          if (NewBoundary.Values.Count = 0) and (ATime > FirstTime) then
          begin
            FhbItem := NewBoundary.Values.Add as TFhbItem;
            FhbItem.StartTime := FirstTime;
            FhbItem.EndTime := ATime;
            FhbItem.BoundaryValue :=
              rdgModflowBoundary.Cells[Ord(fhcHead), RowIndex];
          end;

          FhbItem := NewBoundary.Values.Add as TFhbItem;
          FhbItem.StartTime := ATime;
          FhbItem.BoundaryValue :=
            rdgModflowBoundary.Cells[Ord(fhcHead), RowIndex];
          PriorItem := FhbItem;
        end;
      end;
      if (PriorItem <> nil) then
      begin
        if PriorItem.StartTime < LastTime then
        begin
          PriorItem.EndTime := LastTime;
          FhbItem := NewBoundary.Values.Add as TFhbItem;
          FhbItem.Assign(PriorItem);
          FhbItem.StartTime := LastTime;
        end
        else
        begin
          PriorItem.EndTime := PriorItem.StartTime;
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
          CreateScreenObjectBoundary(Item.ScreenObject);
          Boundary := GetBoundary(Item.ScreenObject);
        end;
        if Boundary <> nil then
        begin
          if rdgModflowBoundary.Cells[Ord(fhcHead),PestMethodRow] = '' then
          begin
            NewBoundary.PestFhbBoundaryMethod := Boundary.PestFhbBoundaryMethod;
          end
          else
          begin
            NewBoundary.PestFhbBoundaryMethod := PestMethod[Ord(fhcHead)];
          end;
          if rdgModflowBoundary.Cells[Ord(fhcHead),PestModifierRow] = '' then
          begin
            NewBoundary.PestFhbBoundaryFormula := Boundary.PestFhbBoundaryFormula;
          end
          else
          begin
            NewBoundary.PestFhbBoundaryFormula := PestModifier[Ord(fhcHead)];
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
