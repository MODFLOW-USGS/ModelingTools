{@name is used for specifying a table that relates stream depth and width
to stream flow in the MODFLOW SFR package.}
unit frameFlowTableUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons,
  ModflowSfrTable, Generics.Collections, UndoItemsScreenObjects,
  Math, Vcl.ExtCtrls;

type
  // @name identifies the columns in @link(TframeFlowTable.dgSfrTable
  // TframeFlowTable.dgSfrTable).
  TSfrTableColumns = (stcFlow, stcDepth, stcWidth);

  TTableObjectList = TObjectList<TSfrTableCollection>;

  {@name is used for specifying a table that relates stream depth and width
  to stream flow in the MODFLOW SFR package.

  @member(seTableCount @name controls the number of rows in @link(dgSfrTable).
    See @link(seTableCountChange).)

  @member(lblNumberOfPoints @name is a label for seTableCount.)

  @member(dgSfrTable Each row of @name contains a data point specifying
  how stream depth and width depend of flow.)

  @member(btnInsertFlowTableRow @name is used for inserting a new row into @link(dgSfrTable).
  See @link(btnInsertFlowTableRowClick).)

  @member(btnDeleteFlowTableRow @name is used for deleting a row from @link(dgSfrTable).
  See @link(btnDeleteFlowTableRowClick).)

  @member(seTableCountChange @name changes the number of rows
  in @link(dgSfrTable).)

  @member(btnInsertFlowTableRowClick @name inserts a row
  in @link(dgSfrTable).)

  @member(btnDeleteFlowTableRowClick @name deletes a row
  from @link(dgSfrTable).)

  @member(dgSfrTableSetEditText @name adjusts @link(seTableCount) to
  reflect the number of rows in @link(dgSfrTable))
  }
  TframeFlowTable = class(TFrame)
    seTableCount: TJvSpinEdit;
    lblNumberOfPoints: TLabel;
    dgSfrTable: TRbwDataGrid4;
    btnInsertFlowTableRow: TBitBtn;
    btnDeleteFlowTableRow: TBitBtn;
    pnl1: TPanel;
    procedure seTableCountChange(Sender: TObject);
    procedure btnInsertFlowTableRowClick(Sender: TObject);
    procedure btnDeleteFlowTableRowClick(Sender: TObject);
    procedure dgSfrTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FTableCountChanged: boolean;
    FTableList: TTableObjectList;
    FItemIndex: Integer;
    { Private declarations }
  public
    property TableCountChanged: boolean read FTableCountChanged
      write FTableCountChanged;
    procedure EnableDelete;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearTable;
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    procedure DisplayData(ItemIndex: Integer);
    procedure DeleteItem(ItemIndex: Integer);
    procedure InsertIntem(ItemIndex: Integer);
    procedure AddItem;
    { Public declarations }
  end;

implementation

uses
  ModflowSfrUnit;

{$R *.dfm}

resourcestring
  StrFlow = 'Flow';
  StrDepth = 'Depth';
  StrWidth = 'Width';

{ TFrame23 }

procedure TframeFlowTable.AddItem;
var
  index: Integer;
  TableCollection: TSfrTableCollection;
  NewItem: TCollectionItem;
  PriorItem: TSfrTablelItem;
begin
  for index := 0 to FTableList.Count - 1 do
  begin
    TableCollection := FTableList[index];
    NewItem := TableCollection.Add;
    if NewItem.Index > 0 then
    begin
      PriorItem := TableCollection[NewItem.Index-1];
      NewItem.Assign(PriorItem);
    end;
  end;
end;

procedure TframeFlowTable.btnDeleteFlowTableRowClick(Sender: TObject);
var
  index: Integer;
  SfrBoundary: TSfrTableCollection;
  Item: TSfrTablelItem;
  Table: TSfrTable;
//  TableItem: TCollectionItem;
  NewItem: TCollectionItem;
  PriorItem: TSfrTablelItem;
begin
  if dgSfrTable.SelectedRow > 0 then
  begin
    for index := 0 to FTableList.Count - 1 do
    begin
      SfrBoundary := FTableList[index];
      While SfrBoundary.Count <= FItemIndex do
      begin
        NewItem := SfrBoundary.Add;
        if NewItem.Index > 0 then
        begin
          PriorItem := SfrBoundary[NewItem.Index-1];
          NewItem.Assign(PriorItem);
        end;
      end;
      Item := SfrBoundary[FItemIndex];
      Table := Item.SfrTable;
      if Table.Count > dgSfrTable.SelectedRow-1 then
      begin
        Table.Delete(dgSfrTable.SelectedRow-1);
      end;
    end;

    dgSfrTable.DeleteRow(dgSfrTable.SelectedRow);
    seTableCount.AsInteger := seTableCount.AsInteger -1;
    seTableCount.OnChange(nil);

  end;
end;

procedure TframeFlowTable.btnInsertFlowTableRowClick(Sender: TObject);
var
  TableItem: TCollectionItem;
  index: Integer;
  SfrBoundary: TSfrTableCollection;
  Item: TSfrTablelItem;
  Table: TSfrTable;
  OtherItem: TSfrTableRowItem;
begin
  if dgSfrTable.SelectedRow > 0 then
  begin
    dgSfrTable.InsertRow(dgSfrTable.SelectedRow);
    seTableCount.AsInteger := seTableCount.AsInteger +1;
    seTableCount.OnChange(nil);

    for index := 0 to FTableList.Count - 1 do
    begin
      SfrBoundary := FTableList[index];
      Assert(FItemIndex < SfrBoundary.Count);
      Item := SfrBoundary[FItemIndex];
      Table := Item.SfrTable;
      Assert( seTableCount.AsInteger = Table.Count);
      TableItem := Table.Last;
      TableItem.Index := dgSfrTable.SelectedRow-1;
      if TableItem.Index > 0 then
      begin
        OtherItem := Table[TableItem.Index-1];
        TableItem.Assign(OtherItem);
      end
      else if Table.Count > 1 then
      begin
        OtherItem := Table[1];
        TableItem.Assign(OtherItem);
      end;
    end
  end;
end;

constructor TframeFlowTable.Create(AOwner: TComponent);
begin
  inherited;
  dgSfrTable.Cells[Ord(stcFlow),0] := StrFlow;
  dgSfrTable.Cells[Ord(stcDepth),0] := StrDepth;
  dgSfrTable.Cells[Ord(stcWidth),0] := StrWidth;
  FTableList := TTableObjectList.Create;
end;

procedure TframeFlowTable.DeleteItem(ItemIndex: Integer);
var
  index: Integer;
  TableCollection: TSfrTableCollection;
begin
  for index := 0 to FTableList.Count - 1 do
  begin
    TableCollection := FTableList[index];
    if TableCollection.Count > ItemIndex then
    begin
      TableCollection.Delete(ItemIndex);
    end;
  end;
end;

destructor TframeFlowTable.Destroy;
begin
  FTableList.Free;
  inherited;
end;

procedure TframeFlowTable.EnableDelete;
begin
  btnDeleteFlowTableRow.Enabled := seTableCount.Enabled
    and (seTableCount.AsInteger > 2);
end;

procedure TframeFlowTable.GetData(List: TScreenObjectEditCollection);
var
  index: Integer;
  SfrBoundary: TSfrBoundary;
  TableCollection: TSfrTableCollection;
begin
  FTableList.Clear;
  FTableList.Capacity := List.Count;
  for index := 0 to List.Count - 1 do
  begin
    SfrBoundary := List[index].ScreenObject.ModflowSfrBoundary;
    TableCollection := TSfrTableCollection.Create(nil, nil, nil);
    FTableList.Add(TableCollection);
    if SfrBoundary <> nil then
    begin
      TableCollection.Assign(SfrBoundary.TableCollection);
    end;
  end;
end;

procedure TframeFlowTable.InsertIntem(ItemIndex: Integer);
var
  index: Integer;
  TableCollection: TSfrTableCollection;
  Item: TCollectionItem;
  NewItem: TCollectionItem;
  OtherItem: TSfrTablelItem;
begin
  for index := 0 to FTableList.Count - 1 do
  begin
    TableCollection := FTableList[index];
    while TableCollection.Count < ItemIndex do
    begin
      NewItem := TableCollection.Add;
      if NewItem.Index > 0 then
      begin
        OtherItem := TableCollection[NewItem.Index-1];
        NewItem.Assign(OtherItem);
      end;
    end;
    Item := TableCollection.Add;
    Item.Index := ItemIndex;
    if Item.Index > 0 then
    begin
      OtherItem := TableCollection[Item.Index-1];
      Item.Assign(OtherItem);
    end
    else if Item.Index < TableCollection.Count -2 then
    begin
      OtherItem := TableCollection[Item.Index+1];
      Item.Assign(OtherItem);
    end;

  end;
end;

procedure TframeFlowTable.dgSfrTableSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  SfrBoundary: TSfrTableCollection;
  Item: TSfrTablelItem;
  Table: TSfrTable;
  TableItem: TSfrTableRowItem;
  index: Integer;
  NewItem: TCollectionItem;
  PriorItem: TSfrTablelItem;
  NewTableItem: TCollectionItem;
  PriorTableItem: TSfrTableRowItem;
begin
  if seTableCount.AsInteger < dgSfrTable.RowCount - 1 then
  begin
    seTableCount.AsInteger := dgSfrTable.RowCount - 1;
    seTableCount.OnChange(seTableCount);
  end;
  if (ARow >= dgSfrTable.FixedRows) and (ACol in [0..2])
    and (Value <> '') then
  begin
    for index := 0 to FTableList.Count - 1 do
    begin
      SfrBoundary := FTableList[index];
      While SfrBoundary.Count <= FItemIndex do
      begin
        NewItem := SfrBoundary.Add;
        if NewItem.Index > 0 then
        begin
          PriorItem := SfrBoundary[NewItem.Index-1];
          NewItem.Assign(PriorItem);
        end;
      end;
      Item := SfrBoundary[FItemIndex];
      Table := Item.SfrTable;
      while ARow-1 >= Table.Count do
      begin
        NewTableItem := Table.Add;
        if NewTableItem.Index > 0 then
        begin
          PriorTableItem := Table[NewTableItem.Index-1];
          NewTableItem.Assign(PriorTableItem);
        end;
      end;
      TableItem := Table[ARow-1];
      case TSfrTableColumns(ACol) of
        stcFlow:
          begin
            TableItem.Flow := Value;
          end;
        stcDepth:
          begin
            TableItem.Depth := Value;
          end;
        stcWidth:
          begin
            TableItem.Width := Value;
          end;
        else Assert(False)
      end;
    end;
  end;
end;

procedure TframeFlowTable.ClearTable;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := dgSfrTable.FixedRows to dgSfrTable.RowCount - 1 do
  begin
    for ColIndex := dgSfrTable.FixedCols to dgSfrTable.ColCount - 1 do
    begin
      dgSfrTable.Cells[ColIndex,RowIndex] := '';
    end;
  end;
end;


procedure TframeFlowTable.DisplayData(ItemIndex: Integer);
var
  index: Integer;
  SfrBoundary: TSfrTableCollection;
  Item: TSfrTablelItem;
  Row: Integer;
  FlowTableIndex: Integer;
  FlowTableItem: TSfrTableRowItem;
begin
  if FTableList.Count = 0 then
  begin
    Exit;
  end;
  FItemIndex := ItemIndex;
  SfrBoundary := FTableList[0];
  if SfrBoundary.Count > ItemIndex then
  begin
    Item := SfrBoundary[ItemIndex];
    seTableCount.AsInteger := Item.SfrTable.Count;
    seTableCount.OnChange(seTableCount);

    for FlowTableIndex := 0 to Item.SfrTable.Count - 1 do
    begin
      FlowTableItem := Item.SfrTable.Items[FlowTableIndex] as TSfrTableRowItem;
      Row := FlowTableIndex + 1;
      dgSfrTable.Cells[Ord(stcFlow), Row] := FlowTableItem.Flow;
      dgSfrTable.Cells[Ord(stcDepth), Row] := FlowTableItem.Depth;
      dgSfrTable.Cells[Ord(stcWidth), Row] := FlowTableItem.Width;
    end;


    for index := 1 to FTableList.Count - 1 do
    begin
      SfrBoundary := FTableList[index];
      Item := SfrBoundary[ItemIndex];

      if seTableCount.AsInteger = Item.SfrTable.Count then
      begin
        for FlowTableIndex := 0 to Item.SfrTable.Count - 1 do
        begin
          FlowTableItem := Item.SfrTable.Items[FlowTableIndex] as TSfrTableRowItem;
          Row := FlowTableIndex + 1;
          if dgSfrTable.Cells[Ord(stcFlow), Row] <> FlowTableItem.Flow then
          begin
            dgSfrTable.Cells[Ord(stcFlow), Row] := '';
          end;
          if dgSfrTable.Cells[Ord(stcDepth), Row] <> FlowTableItem.Depth then
          begin
            dgSfrTable.Cells[Ord(stcDepth), Row] := '';
          end;
          if dgSfrTable.Cells[Ord(stcWidth), Row] <> FlowTableItem.Width then
          begin
            dgSfrTable.Cells[Ord(stcWidth), Row] := '';
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

procedure TframeFlowTable.seTableCountChange(Sender: TObject);
var
  index: Integer;
  SfrBoundary: TSfrTableCollection;
  Item: TSfrTablelItem;
  Table: TSfrTable;
  NewItem: TCollectionItem;
  PriorItem: TSfrTablelItem;
  NewTableItem: TCollectionItem;
  PriorTableItem: TSfrTableRowItem;
begin
  dgSfrTable.RowCount := seTableCount.AsInteger + 1;
  EnableDelete;
  dgSfrTable.OnSetEditText(Sender, 0, dgSfrTable.RowCount -1,
    dgSfrTable.Cells[0,dgSfrTable.RowCount -1]);
  TableCountChanged := True;
  for index := 0 to FTableList.Count - 1 do
  begin
    SfrBoundary := FTableList[index];
    While SfrBoundary.Count <= FItemIndex do
    begin
      NewItem := SfrBoundary.Add;
      if NewItem.Index > 0 then
      begin
        PriorItem := SfrBoundary[NewItem.Index-1];
        NewItem.Assign(PriorItem);
      end;
    end;
    Item := SfrBoundary[FItemIndex];
    Table := Item.SfrTable;
    while seTableCount.AsInteger > Table.Count do
    begin
      NewTableItem := Table.Add;
      if NewTableItem.Index > 0 then
      begin
        PriorTableItem := Table[NewTableItem.Index-1];
        NewTableItem.Assign(PriorTableItem);
      end;
    end;
    while seTableCount.AsInteger < Table.Count do
    begin
      Table.Last.Free;
    end;
  end
end;

procedure TframeFlowTable.SetData(List: TScreenObjectEditCollection; SetAll,
  ClearAll: boolean);
var
  index: integer;
  SfrBoundary: TSfrBoundary;
  TableCollection: TSfrTableCollection;
  StoredItem, BoundaryItem: TSfrTablelItem;
  ItemIndex: Integer;
begin
  Assert(FTableList.Count = List.Count);
  for index := 0 to List.Count - 1 do
  begin
    SfrBoundary := List[index].ScreenObject.ModflowSfrBoundary;
    if SfrBoundary <> nil then
    begin
      TableCollection := FTableList[index];
      while SfrBoundary.TableCollection.Count < TableCollection.Count do
      begin
        SfrBoundary.TableCollection.Add;
      end;
      while SfrBoundary.TableCollection.Count > TableCollection.Count do
      begin
        SfrBoundary.TableCollection.Last.Free;
      end;

      for ItemIndex := 0 to TableCollection.Count - 1 do
      begin
        StoredItem := TableCollection[ItemIndex];
        BoundaryItem := SfrBoundary.TableCollection[ItemIndex];
        BoundaryItem.AssignTable(StoredItem);
      end;
    end;
  end;
end;

end.
