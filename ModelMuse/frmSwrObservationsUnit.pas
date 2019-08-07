unit frmSwrObservationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, frameGridUnit,
  StdCtrls, Buttons, ExtCtrls, ModflowSwrObsUnit, RbwDataGrid4, UndoItems,
  JvExStdCtrls, JvCombobox, JvListComb;

type
  TObsColumns = (ocName, ocType, ocReach, ocConnectedReach, ocStructure, ocLayer);

  TUndoEditSwrObservations = class(TCustomUndo)
  private
    FOldSwrObsservations: TSwrObsCollection;
    FNewSwrObsservations: TSwrObsCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var SwrObsservations: TSwrObsCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmSwrObservations = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    frameObservations: TframeGrid;
    pnlTop: TPanel;
    comboType: TJvImageComboBox;
    procedure frameObservationsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure frameObservationsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure FormResize(Sender: TObject);
    procedure frameObservationsGridHorizontalScroll(Sender: TObject);
    procedure frameObservationsGridColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure comboTypeChange(Sender: TObject);
    procedure frameObservationsGridMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    procedure GetData;
    procedure SetData;
    procedure LayOutMulitRowEditControls;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSwrObservations: TfrmSwrObservations;

implementation

uses
  frmGoPhastUnit, ModflowSwrStructureUnit;

resourcestring
  StrObservationNameCO = 'Observation Name (COBSNAME)';
  StrObservationTypeCO = 'Observation Type (COBSTYPE)';
  StrObservationReachI = 'Observation Reach (IOBSLOC)';
  StrConnectedReachFor = 'Connected Reach for Observation (IOBSLOC2)';
  StrObservationLayerI = 'Observation Layer (IOBSLAY)';
  StrStructure = 'Structure (IOBSLOC2)';

{$R *.dfm}

procedure TfrmSwrObservations.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSwrObservations.comboTypeChange(Sender: TObject);
var
  ACol: Integer;
  RowIndex: Integer;
begin
  inherited;
  ACol := Ord(ocType);
  for RowIndex := frameObservations.Grid.FixedRows to frameObservations.Grid.RowCount - 1 do
  begin
    if frameObservations.Grid.IsSelectedCell(ACol, RowIndex) then
    begin
      frameObservations.Grid.Cells[ACol, RowIndex] := comboType.Text;
    end;
  end;
end;

procedure TfrmSwrObservations.FormCreate(Sender: TObject);
var
  Grid: TRbwDataGrid4;
  PickList: TStrings;
  SwrStructures: TStructureCollection;
  StructureIndex: Integer;
begin
  inherited;
  Grid := frameObservations.Grid;
  Grid.Cells[Ord(ocName), 0] := StrObservationNameCO;
  Grid.Cells[Ord(ocType), 0] := StrObservationTypeCO;
  Grid.Cells[Ord(ocReach), 0] := StrObservationReachI;
  Grid.Cells[Ord(ocConnectedReach), 0] := StrConnectedReachFor;
  Grid.Cells[Ord(ocStructure), 0] := StrStructure;
  Grid.Cells[Ord(ocLayer), 0] := StrObservationLayerI;
  Grid.Columns[Ord(ocLayer)].Max := frmGoPhast.PhastModel.Grid.LayerCount;
  Grid.Columns[Ord(ocLayer)].CheckMax := True;
  comboType.Items.Assign(Grid.Columns[Ord(ocType)].PickList);

  PickList := Grid.Columns[Ord(ocStructure)].PickList;
  SwrStructures := frmGoPhast.PhastModel.SwrStructures;
  PickList.Capacity := SwrStructures.Count;
  for StructureIndex := 0 to SwrStructures.Count - 1 do
  begin
    PickList.Add(SwrStructures[StructureIndex].Name)
  end;

  GetData;
end;

procedure TfrmSwrObservations.FormResize(Sender: TObject);
begin
  inherited;
  LayOutMulitRowEditControls;
end;

procedure TfrmSwrObservations.frameObservationsGridColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayOutMulitRowEditControls;
end;

procedure TfrmSwrObservations.frameObservationsGridHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayOutMulitRowEditControls;
end;

procedure TfrmSwrObservations.frameObservationsGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  RowIndex: integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := frameObservations.Grid.FixedRows to frameObservations.Grid.RowCount -1 do
  begin
      ShouldEnable := frameObservations.Grid.IsSelectedCell(Ord(ocType),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboType.Enabled := ShouldEnable;

end;

procedure TfrmSwrObservations.frameObservationsGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  ItemIndex: Integer;
begin
  inherited;
  if (ARow >= frameObservations.Grid.FixedRows) then
  begin
    if (ACol = Ord(ocConnectedReach)) then
    begin
      ItemIndex := frameObservations.Grid.ItemIndex[Ord(ocType), ARow];
      CanSelect := (ItemIndex >= 0)
        and (TSwrObsType(ItemIndex) = sotFlow);
    end
    else if (ACol = Ord(ocStructure)) then
    begin
      ItemIndex := frameObservations.Grid.ItemIndex[Ord(ocType), ARow];
      CanSelect := (ItemIndex >= 0)
        and (TSwrObsType(ItemIndex) = sotStructure);
    end
    else if (ACol = Ord(ocLayer)) then
    begin
      ItemIndex := frameObservations.Grid.ItemIndex[Ord(ocType), ARow];
      CanSelect := (ItemIndex >= 0)
        and (TSwrObsType(ItemIndex) = sotBaseFlow);
    end;
  end;
end;

procedure TfrmSwrObservations.frameObservationsGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameObservations.Grid.invalidate;
end;

procedure TfrmSwrObservations.GetData;
var
  SwrObservations: TSwrObsCollection;
  ItemIndex: Integer;
  Item: TSwrObsItem;
  Grid: TRbwDataGrid4;
begin
  SwrObservations := frmGoPhast.PhastModel.SwrObservations;
  frameObservations.seNumber.AsInteger := SwrObservations.Count;
  Grid := frameObservations.Grid;
  for ItemIndex := 0 to SwrObservations.Count - 1 do
  begin
    Item := SwrObservations[ItemIndex];
    Grid.Cells[Ord(ocName), ItemIndex+1] := Item.ObsName;
    Grid.ItemIndex[Ord(ocType), ItemIndex+1] := Ord(Item.ObsType);
    Grid.IntegerValue[Ord(ocReach), ItemIndex+1] := Item.ObservationReach;
    Grid.IntegerValue[Ord(ocConnectedReach), ItemIndex+1] := Item.ConnectedReachOrStructure;
    Grid.Cells[Ord(ocStructure), ItemIndex+1] := Item.StructureName;
    Grid.IntegerValue[Ord(ocLayer), ItemIndex+1] := Item.ObservationLayer;
  end;
end;

procedure TfrmSwrObservations.LayOutMulitRowEditControls;
begin
  LayoutControls(frameObservations.Grid, comboType, nil, Ord(ocType));
end;

procedure TfrmSwrObservations.SetData;
var
  DummyEvent: TNotifyEvent;
  SwrObservations: TSwrObsCollection;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  ObsName: string;
  ObsLocation: integer;
  ObsType: TSwrObsType;
  ObsItem: TSwrObsItem;
  ItemIndex: Integer;
  ObsConnected: integer;
  ObsLayer: Integer;
  StructureName: string;
begin
  Grid := frameObservations.Grid;
  DummyEvent := nil;
  SwrObservations := TSwrObsCollection.Create(DummyEvent);
  try
    for RowIndex := 1 to frameObservations.seNumber.AsInteger do
    begin
      ObsName := Grid.Cells[Ord(ocName), RowIndex];
      ItemIndex := Grid.ItemIndex[Ord(ocType), RowIndex];
      if (ObsName <> '') and (ItemIndex >= 0)
        and TryStrToInt(Grid.Cells[Ord(ocReach), RowIndex], ObsLocation) then
      begin
        ObsType := TSwrObsType(ItemIndex);
        if ObsType = sotFlow then
        begin
          if not TryStrToInt(Grid.Cells[Ord(ocConnectedReach), RowIndex], ObsConnected) then
          begin
            Continue;
          end;
        end
        else
        begin
          ObsConnected := 0;
        end;
        if ObsType = sotStructure then
        begin
          StructureName := Trim(Grid.Cells[Ord(ocStructure), RowIndex]);
          if StructureName = '' then
          begin
            Continue;
          end;
        end
        else
        begin
          StructureName := '';
        end;
        if ObsType = sotBaseFlow then
        begin
          if not TryStrToInt(Grid.Cells[Ord(ocLayer), RowIndex], ObsLayer) then
          begin
            Continue;
          end;
        end
        else
        begin
          ObsLayer := 0;
        end;
        ObsName := StringReplace(ObsName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
        ObsItem := SwrObservations.Add;
        ObsItem.ObsName := ObsName;
        ObsItem.ObsType := ObsType;
        ObsItem.ObservationReach := ObsLocation;
        ObsItem.ConnectedReachOrStructure := ObsConnected;
        ObsItem.StructureName := StructureName;
        ObsItem.ObservationLayer := ObsLayer;
      end;
    end;
    frmGoPhast.UndoStack.Submit(TUndoEditSwrObservations.Create(SwrObservations));
  finally
    SwrObservations.Free;
  end;
end;

{ TUndoEditSwrObservations }

constructor TUndoEditSwrObservations.Create(
  var SwrObsservations: TSwrObsCollection);
var
  Dummy: TNotifyEvent;
begin
  Dummy := nil;
  FOldSwrObsservations := TSwrObsCollection.Create(Dummy);
  FOldSwrObsservations.Assign(frmGoPhast.PhastModel.SwrObservations);
  FNewSwrObsservations := SwrObsservations;
  SwrObsservations := nil;
end;

function TUndoEditSwrObservations.Description: string;
begin
  result := 'edit SWR observations';
end;

destructor TUndoEditSwrObservations.Destroy;
begin
  FOldSwrObsservations.Free;
  FNewSwrObsservations.Free;
  inherited;
end;

procedure TUndoEditSwrObservations.DoCommand;
begin
  frmGoPhast.PhastModel.SwrObservations := FNewSwrObsservations;
end;

procedure TUndoEditSwrObservations.Undo;
begin
  frmGoPhast.PhastModel.SwrObservations := FOldSwrObsservations;
end;

end.
