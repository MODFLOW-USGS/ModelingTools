unit frmPointValuesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, Grids, RbwDataGrid4, ExtCtrls, StdCtrls,
  Buttons, ScreenObjectUnit, UndoItems;

type
  TPointValueColumn = (pvcName, pvcValue, pvcDefineStructure);

  TfrmPointValues = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    rdgValues: TRbwDataGrid4;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnAdd: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure btnAddClick(Sender: TObject);
    procedure rdgValuesKeyPress(Sender: TObject; var Key: Char);
    procedure rdgValuesButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure rdgValuesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgValuesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgValuesEndUpdate(Sender: TObject);
  private
    FScreenObject: TScreenObject;
    FSelectedVertex: integer;
    FEditedItem: TPointValuesItem;
    FEditedValues: TPointValues;
    procedure SetData;
    { Private declarations }
  public
    procedure GetData(ScreenObject: TScreenObject; SelectedVertex: integer);
    { Public declarations }
  end;

  TUndoPointPositions = class(TCustomUndo)
  private
    FOldPointPositions: TPointPositionValues;
    FNewPointPositions: TPointPositionValues;
    FScreenObject: TScreenObject;
    procedure InvalidateZoomBox;
  protected
    function Description: string; override;
  public
    Constructor Create(ScreenObject: TScreenObject;
      var NewPointPositions: TPointPositionValues);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses
  frmGoPhastUnit, ZoomBox2, GoPhastTypes, ModflowSwrStructureUnit,
  frmSwrStructuresUnit, ModelMuseUtilities;

resourcestring
  StrChangeVertexValues = 'change vertex values';
  StrKey = 'Key';
  StrValue = 'Value';

{$R *.dfm}

{ TfrmPointValues }

procedure TfrmPointValues.btnAddClick(Sender: TObject);
begin
  inherited;
  rdgValues.RowCount := rdgValues.RowCount + 1;
end;

procedure TfrmPointValues.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmPointValues.FormCreate(Sender: TObject);
var
  AColumn: TRbwColumn4;
begin
  inherited;
  rdgValues.Cells[Ord(pvcName),0] := StrKey;
  rdgValues.Cells[Ord(pvcValue),0] := StrValue;
  if frmGoPhast.PhastModel.SwrIsSelected then
  begin
    AColumn := rdgValues.Columns[0];
    AColumn.ComboUsed := True;
    AColumn.PickList.Add('Reach');
    AColumn.PickList.Add('TabFile');

    AColumn := rdgValues.Columns.Add;
    rdgValues.Cells[Ord(pvcDefineStructure),0] := 'Define or edit structure';
    AColumn.ButtonUsed := True;
    AColumn.AutoAdjustColWidths := True;
    AColumn.ButtonWidth := rdgValues.ColWidths[Ord(pvcDefineStructure)];
    AColumn.ButtonCaption := 'Edit structure';
  end;
end;

procedure TfrmPointValues.FormDestroy(Sender: TObject);
begin
  inherited;
  FEditedValues.Free;
end;

procedure TfrmPointValues.GetData(ScreenObject: TScreenObject;
  SelectedVertex: integer);
var
  Names: TStringList;
  ItemIndex: Integer;
  Item: TPointValuesItem;
  NameIndex: Integer;
  ValueItem: TPointValue;
  PointValueItem: TPointValue;
  Row: Integer;
  PointPositionValues: TPointPositionValues;
  AColumn: TRbwColumn4;
begin
  FScreenObject := ScreenObject;

  if (frmGoPhast.ModelSelection in SutraSelection) and FScreenObject.CellSizeUsed then
  begin
    AColumn := rdgValues.Columns[0];
    AColumn.ComboUsed := True;
    AColumn.PickList.Add('Mesh Element Size');
  end;


  FSelectedVertex := SelectedVertex;
  Names := TStringList.Create;
  try
    Names.Sorted := True;
    Names.CaseSensitive := False;
    Names.Duplicates := dupIgnore;
    FEditedItem := nil;

    PointPositionValues := ScreenObject.PointPositionValues;

    if PointPositionValues <> nil then
    begin
      for ItemIndex := 0 to PointPositionValues.Count - 1 do
      begin
        Item := PointPositionValues.Items[ItemIndex]
          as TPointValuesItem;
        if Item.Position = SelectedVertex then
        begin
          FEditedItem := Item;
        end;
        for NameIndex := 0 to Item.Values.Count - 1 do
        begin
          ValueItem := Item.Values.Items[NameIndex] as TPointValue;
          Names.AddObject(ValueItem.Name, ValueItem)
        end;
      end;
    end;
    Names.Sorted := False;
    Names.CustomSort(CompareNames);
    rdgValues.BeginUpdate;
    try
      if Names.Count > 0 then
      begin
        rdgValues.RowCount := Names.Count + 1;
        for NameIndex := 0 to Names.Count - 1 do
        begin
          rdgValues.Cells[Ord(pvcName),NameIndex+1] := Names[NameIndex];
        end;
      end;
      FEditedValues := TPointValues.Create(nil);
      if FEditedItem <> nil then
      begin
        FEditedValues.Assign(FEditedItem.Values);
        for NameIndex := 0 to FEditedValues.Count - 1 do
        begin
          PointValueItem := FEditedValues.Items[NameIndex] as TPointValue;
          Row := Names.IndexOf(PointValueItem.Name) + 1;
          Assert(Row > 0);
          rdgValues.Cells[Ord(pvcValue),Row] :=
            FloatToStr(PointValueItem.Value);
          rdgValues.Objects[Ord(pvcValue),Row] := PointValueItem;
        end;
      end;
    finally
      rdgValues.EndUpdate;
    end;
  finally
    Names.Free;
  end;
end;

procedure TfrmPointValues.rdgValuesButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  SwrStructures: TStructureCollection;
  Reach: integer;
  Index: Integer;
  AStructure: TStructure;
begin
  inherited;
  SwrStructures := frmGoPhast.PhastModel.SwrStructures;
  Reach := Round(StrToFloat(rdgValues.Cells[Ord(pvcValue),ARow]));
  for Index := 0 to SwrStructures.Count - 1 do
  begin
    AStructure := SwrStructures[Index];
    if AStructure.Reach = Reach then
    begin
      TfrmSwrStructures.AddNewStructure := False;
      TfrmSwrStructures.SelectedStructureIndex := Index;
      ShowAForm(TfrmSwrStructures);
      Exit;
    end;
  end;
  TfrmSwrStructures.AddNewStructure := True;
  TfrmSwrStructures.SelectedStructureIndex := SwrStructures.Count;
  TfrmSwrStructures.Reach := Reach;
  ShowAForm(TfrmSwrStructures);
end;

procedure TfrmPointValues.rdgValuesEndUpdate(Sender: TObject);
begin
  inherited;
  rdgValues.Invalidate;
end;

procedure TfrmPointValues.rdgValuesKeyPress(Sender: TObject; var Key: Char);
var
  Grid: TRbwDataGrid4;
begin
  // Move to next row when the user depresses the return key.
  if Key = #9 then
  begin
    if (ActiveControl is TInplaceEdit) or (ActiveControl = rdgValues) then
    begin
      if ActiveControl = rdgValues then
      begin
        Grid := rdgValues;
        if (Grid.Row = Grid.FixedRows) and (Grid.Col = Grid.FixedCols) then
        begin
          ActiveControl := btnAdd;
        end;
      end
      else
      begin
        // This part may never get called.
        Grid := ActiveControl.Owner as TRbwDataGrid4;
        if (Grid.Row = Grid.RowCount - 1) and (Grid.Col = Grid.ColCount - 1) then
        begin
          ActiveControl := btnAdd;
        end;
      end;

    end;
  end;
  inherited;
end;

procedure TfrmPointValues.rdgValuesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  AValue: double;
begin
  inherited;
  if (ARow >= rdgValues.FixedRows) and (ACol = Ord(pvcDefineStructure)) then
  begin
    CanSelect := LowerCase(rdgValues.Cells[Ord(pvcName), ARow]) = 'reach';
    if CanSelect then
    begin
      CanSelect := TryStrToFloat(rdgValues.Cells[Ord(pvcValue),ARow], AValue)
    end;
  end;
end;

procedure TfrmPointValues.rdgValuesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if (ACol = Ord(pvcName)) and (ARow >= rdgValues.FixedRows) then
  begin
    rdgValues.Invalidate;
  end;
end;

procedure TfrmPointValues.SetData;
var
  RowIndex: Integer;
  AValue: double;
  PointValueItem: TPointValue;
  NewPointPositions: TPointPositionValues;
  ValuesItem: TPointValuesItem;
  Undo: TUndoPointPositions;
  ShouldChange: Boolean;
  PointPositionValues: TPointPositionValues;
begin
  for RowIndex := 1 to rdgValues.RowCount - 1 do
  begin
    if (Trim(rdgValues.Cells[Ord(pvcName),RowIndex]) <> '')
      and TryStrToFloat(rdgValues.Cells[Ord(pvcValue),RowIndex], AValue) then
    begin
      if rdgValues.Objects[Ord(pvcValue),RowIndex] = nil then
      begin
        PointValueItem := FEditedValues.Add as TPointValue;
      end
      else
      begin
        PointValueItem := rdgValues.Objects[Ord(pvcValue),RowIndex] as TPointValue;
      end;
      PointValueItem.Name := Trim(rdgValues.Cells[Ord(pvcName),RowIndex]);
      PointValueItem.Value := AValue;
    end
    else
    begin
      if rdgValues.Objects[Ord(pvcValue),RowIndex] <> nil then
      begin
        PointValueItem := rdgValues.Objects[Ord(pvcValue),RowIndex] as TPointValue;
        FEditedValues.Remove(PointValueItem);
      end;
    end;
  end;
  if (FEditedItem = nil) then
  begin
    ShouldChange := (FEditedValues.Count > 0)
  end
  else
  begin
    ShouldChange := not FEditedValues.IsSame(FEditedItem.Values)
  end;
  if ShouldChange then
  begin
    NewPointPositions:= TPointPositionValues.Create(nil);
    PointPositionValues := FScreenObject.PointPositionValues;
    if PointPositionValues <> nil then
    begin
      NewPointPositions.Assign(PointPositionValues);
    end;
    ValuesItem := NewPointPositions.GetItemByPosition(FSelectedVertex);
    if ValuesItem = nil then
    begin
      ValuesItem := NewPointPositions.Add as TPointValuesItem;
      ValuesItem.Position := FSelectedVertex;
    end;
    if FEditedValues.Count = 0 then
    begin
      NewPointPositions.Remove(ValuesItem);
    end
    else
    begin
      ValuesItem.Values.Assign(FEditedValues);
    end;
    Undo := TUndoPointPositions.Create(FScreenObject, NewPointPositions);
    frmGoPhast.UndoStack.Submit(Undo);
  end;
end;

{ TUndoPointPositions }

constructor TUndoPointPositions.Create(ScreenObject: TScreenObject;
  var NewPointPositions: TPointPositionValues);
var
  PointPositionValues: TPointPositionValues;
begin
  FScreenObject := ScreenObject;
  FOldPointPositions := TPointPositionValues.Create(nil);
  PointPositionValues := FScreenObject.PointPositionValues;
  if PointPositionValues <> nil then
  begin
    FOldPointPositions.Assign(FScreenObject.PointPositionValues);
  end;
  FNewPointPositions := NewPointPositions;
  NewPointPositions := nil;
end;

function TUndoPointPositions.Description: string;
begin
  result := StrChangeVertexValues;
end;

destructor TUndoPointPositions.Destroy;
begin
  FNewPointPositions.Free;
  FOldPointPositions.Free;
  inherited;
end;

procedure TUndoPointPositions.DoCommand;
begin
  FScreenObject.PointPositionValues := FNewPointPositions;
  InvalidateZoomBox;

end;

procedure TUndoPointPositions.Undo;
begin
  FScreenObject.PointPositionValues := FOldPointPositions;
  InvalidateZoomBox;
end;

procedure TUndoPointPositions.InvalidateZoomBox;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := nil;
  case FScreenObject.ViewDirection of
    vdTop:
      ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    vdFront:
      ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
    vdSide:
      ZoomBox := frmGoPhast.frameSideView.ZoomBox;
  end;
  ZoomBox.InvalidateImage32;
end;

end.
