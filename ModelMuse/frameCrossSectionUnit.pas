// @name defines a frame in which the user specifies the coordinates of
// a stream cross section in the SFR package.
unit frameCrossSectionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, Generics.Collections,
  ModflowSfrChannelUnit, UndoItemsScreenObjects, Math;

type
  // @name represents the columns of @link(TframeCrossSection.dg8Point)
  // used to specify the X and Z coordinates of a cross section.
  TSfr8Point = (s8pX, s8pZ);

  TChannelValuesList = TObjectList<TSfrChannelCollection>;

  // @name is a frame in which the user specifies the coordinates of
  // a stream cross section in the SFR package.
  TframeCrossSection = class(TFrame)
    // @name is the grid in which the user specifies the coordinates of
    // a stream cross section in the SFR package.
    dg8Point: TRbwDataGrid4;
    procedure dg8PointSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FItemIndex: Integer;
    FChannelValuesList: TChannelValuesList;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearTable;
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    procedure DisplayData(ItemIndex: Integer);
    procedure DeleteItem(ItemIndex: Integer);
    procedure InsertItem(ItemIndex: Integer);
    procedure AddItem;
    { Public declarations }
  end;

implementation

uses
  ModflowSfrUnit, ModflowSfrParamIcalcUnit;

{$R *.dfm}

resourcestring
  StrX = 'X';
  StrZ = 'Z';

{ TframeCrossSection }

constructor TframeCrossSection.Create(AOwner: TComponent);
begin
  inherited;
  dg8Point.Cells[Ord(s8pX),0] := StrX;
  dg8Point.Cells[Ord(s8pZ),0] := StrZ;
  FChannelValuesList := TChannelValuesList.Create;
end;

procedure TframeCrossSection.DeleteItem(ItemIndex: Integer);
var
  index: Integer;
  ChannelValues: TSfrChannelCollection;
begin
  for index := 0 to FChannelValuesList.Count - 1 do
  begin
    ChannelValues := FChannelValuesList[index];
    if ChannelValues.Count > ItemIndex then
    begin
      ChannelValues.Delete(ItemIndex);
    end;
  end;
end;

destructor TframeCrossSection.Destroy;
begin
  FChannelValuesList.Free;
  inherited;
end;

procedure TframeCrossSection.dg8PointSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  FloatValue: Extended;
  index: Integer;
  SfrBoundary: TSfrChannelCollection;
  Item: TSfrChannelItem;
begin
  if (ACol in [0,1]) and (ARow in [1..8])
    and TryStrToFloat(Value, FloatValue) then
  begin
    for index := 0 to FChannelValuesList.Count - 1 do
    begin
      SfrBoundary := FChannelValuesList[index];
      While SfrBoundary.Count <= FItemIndex do
      begin
        SfrBoundary.Add;
      end;
      Item := SfrBoundary[FItemIndex];
      if ACol = 0 then
      begin
        Item.X[ARow-1] := Value;
      end
      else
      begin
        Item.Z[ARow-1] := Value;
      end;
    end;
  end;
end;

procedure TframeCrossSection.AddItem;
var
  index: Integer;
  ChannelValues: TSfrChannelCollection;
  NewItem: TCollectionItem;
  PriorItem: TSfrChannelItem;
begin
  for index := 0 to FChannelValuesList.Count - 1 do
  begin
    ChannelValues := FChannelValuesList[index];
    NewItem := ChannelValues.Add;
    if NewItem.Index > 0 then
    begin
      PriorItem := ChannelValues[NewItem.Index-1];
      NewItem.Assign(PriorItem);
    end;
  end;
end;

procedure TframeCrossSection.ClearTable;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := dg8Point.FixedRows to dg8Point.RowCount - 1 do
  begin
    for ColIndex := dg8Point.FixedCols to dg8Point.ColCount - 1 do
    begin
      dg8Point.Cells[ColIndex,RowIndex] := '';
    end;
  end;
end;

procedure TframeCrossSection.DisplayData(ItemIndex: Integer);
var
  index: Integer;
  SfrBoundary: TSfrChannelCollection;
  Item: TSfrChannelItem;
  XS_Index: Integer;
  Row: Integer;
begin
  if FChannelValuesList.Count = 0 then
  begin
    Exit;
  end;
  FItemIndex := ItemIndex;
  SfrBoundary := FChannelValuesList[0];
  if SfrBoundary.Count > ItemIndex then
  begin
    Item := SfrBoundary[ItemIndex];
    for XS_Index := 0 to 7 do
    begin
      Row := XS_Index + 1;
      dg8Point.Cells[Ord(s8pX), Row] := Item.X[XS_Index];
      dg8Point.Cells[Ord(s8pZ), Row] := Item.Z[XS_Index];
    end;
    for index := 1 to FChannelValuesList.Count - 1 do
    begin
      SfrBoundary := FChannelValuesList[index];
      if SfrBoundary.Count > ItemIndex then
      begin
        Item := SfrBoundary[ItemIndex];
        for XS_Index := 0 to 7 do
        begin
          Row := XS_Index + 1;
          if dg8Point.Cells[Ord(s8pX), Row] <> Item.X[XS_Index] then
          begin
            dg8Point.Cells[Ord(s8pX), Row] := '';
          end;
          if dg8Point.Cells[Ord(s8pZ), Row] <> Item.Z[XS_Index] then
          begin
            dg8Point.Cells[Ord(s8pZ), Row] := '';
          end;
        end;
      end
      else
      begin
        ClearTable;
      end;
    end;
  end
  else
  begin
    ClearTable;
  end;
end;

procedure TframeCrossSection.GetData(List: TScreenObjectEditCollection);
var
  index: Integer;
  SfrBoundary: TSfrBoundary;
  ChannelValues: TSfrChannelCollection;
begin
  FChannelValuesList.Clear;
  FChannelValuesList.Capacity := List.Count;
  for index := 0 to List.Count - 1 do
  begin
    SfrBoundary := List[index].ScreenObject.ModflowSfrBoundary;
    ChannelValues := TSfrChannelCollection.Create(nil, nil, nil);
    FChannelValuesList.Add(ChannelValues);
    if SfrBoundary <> nil then
    begin
      ChannelValues.Assign(SfrBoundary.ChannelValues);
    end;
  end;
end;

procedure TframeCrossSection.InsertItem(ItemIndex: Integer);
var
  index: Integer;
  ChannelValues: TSfrChannelCollection;
  Item: TCollectionItem;
  NewItem: TCollectionItem;
  OtherItem: TSfrChannelItem;
begin
  for index := 0 to FChannelValuesList.Count - 1 do
  begin
    ChannelValues := FChannelValuesList[index];
    while ChannelValues.Count < ItemIndex do
    begin
      NewItem := ChannelValues.Add;
      if NewItem.Index > 0 then
      begin
        OtherItem := ChannelValues[NewItem.Index-1];
        NewItem.Assign(OtherItem);
      end;
    end;
    Item := ChannelValues.Add;
    Item.Index := ItemIndex;
    if Item.Index > 0 then
    begin
      OtherItem := ChannelValues[Item.Index-1];
      Item.Assign(OtherItem);
    end
    else if Item.Index < ChannelValues.Count -2 then
    begin
      OtherItem := ChannelValues[Item.Index+1];
      Item.Assign(OtherItem);
    end;

  end;
end;

procedure TframeCrossSection.SetData(List: TScreenObjectEditCollection; SetAll,
  ClearAll: boolean);
var
  index: integer;
  SfrBoundary: TSfrBoundary;
  ChannelValues: TSfrChannelCollection;
  StoredItem, BoundaryItem: TSfrChannelItem;
  ItemIndex: Integer;
begin
  Assert(FChannelValuesList.Count = List.Count);
  for index := 0 to List.Count - 1 do
  begin
    SfrBoundary := List[index].ScreenObject.ModflowSfrBoundary;
    if SfrBoundary <> nil then
    begin

      ChannelValues := FChannelValuesList[index];
      while SfrBoundary.ChannelValues.Count < ChannelValues.Count do
      begin
        SfrBoundary.ChannelValues.Add;
      end;
      while SfrBoundary.ChannelValues.Count > ChannelValues.Count do
      begin
        if (SfrBoundary.ParamIcalc.Last as TSfrParamIcalcItem).ICalc = 2 then
        begin
          SfrBoundary.ChannelValues.Last.Free;
        end
        else
        begin
          break;
        end;
      end;
      for ItemIndex := 0 to ChannelValues.Count - 1 do
      begin
        StoredItem := ChannelValues[ItemIndex];
        BoundaryItem := SfrBoundary.ChannelValues[ItemIndex];
        BoundaryItem.AssignGeometry(StoredItem);
      end;
    end;
  end;
end;

end.
