unit frmChildModelsUnit;

interface

uses System.UITypes, System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, ExtCtrls, Buttons, ComCtrls, Grids,
  RbwDataGrid4, Mask, JvExMask, JvSpin, PhastModelUnit, UndoItems,
  OrderedCollectionUnit, ArgusDataEntry, ScreenObjectUnit,
  Generics.Collections, GrayTabs;

type
  TDisColumn = (dsLayerGroup, dsParentLayer, dsDiscretization);

  TfrmChildModels = class(TfrmCustomGoPhast)
    Panel1: TPanel;
    tvChildModels: TTreeView;
    Panel3: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    pcMain: TPageControl;
    tabBasic: TTabSheet;
    lblBottomUnit: TLabel;
    lblBottomLayer: TLabel;
    edModelName: TLabeledEdit;
    comboBottomUnit: TComboBox;
    seBottomLayer: TJvSpinEdit;
    tabDiscretization: TTabSheet;
    rdgDiscretization: TRbwDataGrid4;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblCellCount: TLabel;
    seCellCount: TJvSpinEdit;
    rgStartingHeads: TRadioGroup;
    tabSolution: TTabSheet;
    lblMaxIterations: TLabel;
    seMaxIterations: TJvSpinEdit;
    rgPrintIterations: TRadioGroup;
    rdeRelaxHeads: TRbwDataEntry;
    lblRelaxHeads: TLabel;
    lblRelaxFlux: TLabel;
    rdeRelaxFlux: TRbwDataEntry;
    lblHeadClosure: TLabel;
    rdeHeadClosure: TRbwDataEntry;
    lblFluxClosure: TLabel;
    rdeFluxClosure: TRbwDataEntry;
    rgCouplingMethod: TRadioGroup;
    Panel2: TPanel;
    rdeDiscretization: TRbwDataEntry;
    cbSaveBFH: TCheckBox;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure tvChildModelsChange(Sender: TObject; Node: TTreeNode);
    procedure btnOKClick(Sender: TObject);
    procedure edModelNameChange(Sender: TObject);
    procedure comboBottomUnitChange(Sender: TObject);
    procedure seBottomLayerChange(Sender: TObject);
    procedure comboBottomUnitExit(Sender: TObject);
    procedure seBottomLayerExit(Sender: TObject);
    procedure rdgDiscretizationExit(Sender: TObject);
    procedure tvChildModelsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure rdgDiscretizationEnter(Sender: TObject);
    procedure rgStartingHeadsClick(Sender: TObject);
    procedure rgPrintIterationsClick(Sender: TObject);
    procedure rdeRelaxHeadsChange(Sender: TObject);
    procedure rdeRelaxFluxChange(Sender: TObject);
    procedure seMaxIterationsChange(Sender: TObject);
    procedure rdeHeadClosureChange(Sender: TObject);
    procedure rdeFluxClosureChange(Sender: TObject);
    procedure cbOneWayCouplingClick(Sender: TObject);
    procedure rdgDiscretizationColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgDiscretizationHorizontalScroll(Sender: TObject);
    procedure rdeDiscretizationChange(Sender: TObject);
    procedure rdgDiscretizationMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rgCouplingMethodClick(Sender: TObject);
    procedure seCellCountExit(Sender: TObject);
  private
    FChildModels: TChildModelEditCollection;
    FAlreadyHandled: Boolean;
    procedure GetData;
    procedure SetData;
    procedure FillListWithLayerGroups(List: TList);
    procedure ReadSublayerDiscretization;
    procedure WriteSublayerDiscretization;
    procedure LayoutMultiRowEditControl;
    procedure EnableControls;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUseChanges = class(TObject)
  private
    FUsedWithModelCollection: TUsedWithModelCollection;
    FScreenObject: TScreenObject;
    procedure SetUsedWithModelCollection(const Value: TUsedWithModelCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property ScreenObject: TScreenObject read FScreenObject write FScreenObject;
    property UsedWithModelCollection: TUsedWithModelCollection
      read FUsedWithModelCollection write SetUsedWithModelCollection;
  end;

  type TUndoChildModelChange = class(TCustomUndo)
  private
    FNewChildModels: TChildModelEditCollection;
    FOldChildModels: TChildModelCollection;
    FScreenObjects: TList;
    FNewSaveBfhBoundaries: Boolean;
    FOldSaveBfhBoundaries: Boolean;
    FDeletedModels: TList;
    FChangedUsedWithModels: TObjectList<TUseChanges>;
    procedure ChangeChildModel(Source: TCollection);
  protected
    function Description: string; override;
  public
    constructor Create(SaveBfhBoundaries: boolean;
      var NewChildModels: TChildModelEditCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmChildModels: TfrmChildModels;

implementation

uses
  frmGoPhastUnit, LayerStructureUnit, frmDisplayDataUnit,
  GoPhastTypes;

resourcestring
  StrChangeChildModels = 'change child models';
  StrChild = 'Child ';
  StrLayerGroup = 'Layer group';
  StrLayer = 'Layer';
  StrChildModelDiscreti = 'Child model discretization (NCPPL)';
  StrYouMustDefineThe = 'You must define the parent model grid before crea' +
  'ting child models.';

{$R *.dfm}

procedure TfrmChildModels.btnAddClick(Sender: TObject);
var
  Edit: TChildModelEdit;
  Node: TTreeNode;
begin
  inherited;
  if (frmGoPhast.PhastModel.LayerStructure.Count = 0)
    or (frmGoPhast.PhastModel.ModflowGrid.RowCount <= 0)
    or (frmGoPhast.PhastModel.ModflowGrid.ColumnCount <= 0) then
  begin
    Beep;
    MessageDlg(StrYouMustDefineThe, mtError, [mbOK], 0);
    Exit;
  end;
  Edit := FChildModels.Add as TChildModelEdit;
  Edit.ModelName := StrChild + IntToStr(FChildModels.Count);
  Edit.Discretization.BottomLayerGroup :=
    frmGoPhast.PhastModel.LayerStructure.Last;
  Edit.Discretization.BottomLayerInUnit := Edit.Discretization.
    BottomLayerGroup.LayerCount-1;
  Node := tvChildModels.Items.AddObject(nil, Edit.ModelName, Edit);
  Node.Selected := True;
end;

procedure TfrmChildModels.btnDeleteClick(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if tvChildModels.Selected <> nil then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.Free;
    tvChildModels.Selected.Free;
  end;
end;

procedure TfrmChildModels.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmChildModels.cbOneWayCouplingClick(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.CouplingMethod := TCouplingMethod(rgCouplingMethod.ItemIndex);
//    Edit.OneWayCoupling := cbOneWayCoupling.Checked;
  end;
  EnableControls;
end;

procedure TfrmChildModels.comboBottomUnitChange(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) and (comboBottomUnit.ItemIndex >= 0) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.Discretization.BottomLayerGroup :=
      comboBottomUnit.Items.Objects[comboBottomUnit.ItemIndex]
      as TLayerGroup;
    seBottomLayer.MaxValue :=
      Edit.Discretization.BottomLayerGroup.LayerCount;
  end;
  EnableControls;
end;

procedure TfrmChildModels.comboBottomUnitExit(Sender: TObject);
begin
  inherited;
  ReadSublayerDiscretization;
end;

procedure TfrmChildModels.edModelNameChange(Sender: TObject);
var
  Edit: TChildModelEdit;
  AName: AnsiString;
//  AName
begin
  inherited;
  if tvChildModels.Selected <> nil then
  begin
    Edit := tvChildModels.Selected.Data;
    AName := AnsiString(edModelName.Text);
    Edit.ModelName := string(AName);
    tvChildModels.Selected.Text := string(AName);
    if edModelName.Text <> string(AName) then
    begin
      edModelName.Text := string(AName);
    end;
  end;
end;

procedure TfrmChildModels.EnableControls;
var
  ShouldEnable: Boolean;
begin
  ShouldEnable := tvChildModels.Selected <> nil;
  edModelName.Enabled := ShouldEnable;
  comboBottomUnit.Enabled := ShouldEnable;
  seCellCount.Enabled := ShouldEnable;
  rgStartingHeads.Enabled := ShouldEnable;
  rdgDiscretization.Enabled := ShouldEnable;
  rdgDiscretization.Enabled := ShouldEnable;
  rgPrintIterations.Enabled := ShouldEnable;
  rdeRelaxHeads.Enabled := ShouldEnable;
  rdeRelaxFlux.Enabled := ShouldEnable;
  rdeHeadClosure.Enabled := ShouldEnable;
  rdeFluxClosure.Enabled := ShouldEnable;
  rgCouplingMethod.Enabled := ShouldEnable;

  if ShouldEnable then
  begin
    seBottomLayer.Enabled := seBottomLayer.MaxValue > 1;
    seMaxIterations.Enabled := (rgCouplingMethod.ItemIndex = 1);
  end
  else
  begin
    seBottomLayer.Enabled := False;
    seMaxIterations.Enabled := False;
  end;
end;

procedure TfrmChildModels.FormCreate(Sender: TObject);
var
  LayerGroupIndex: Integer;
  Group: TLayerGroup;
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  FChildModels:= TChildModelEditCollection.Create;

  comboBottomUnit.Items.Capacity :=
    frmGoPhast.PhastModel.LayerStructure.Count - 1;
  for LayerGroupIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
  begin
    Group := frmGoPhast.PhastModel.LayerStructure[LayerGroupIndex];
    if Group.RunTimeSimulated then
    begin
      comboBottomUnit.Items.AddObject(Group.AquiferName, Group)
    end;
  end;

  rdgDiscretization.Cells[Ord(dsLayerGroup),0] := StrLayerGroup;
  rdgDiscretization.Cells[Ord(dsParentLayer),0] := StrLayer;
  rdgDiscretization.Cells[Ord(dsDiscretization),0] := StrChildModelDiscreti;

  GetData;
end;

procedure TfrmChildModels.FormDestroy(Sender: TObject);
begin
  FChildModels.Free;
  inherited;
end;

procedure TfrmChildModels.GetData;
var
  ChildIndex: Integer;
  Edit: TChildModelEdit;
  Node: TTreeNode;
begin
  case frmGoPhast.ModelSelection of
      msModflowLGR:
        begin
          seCellCount.Increment := 2;
        end;
      msModflowLGR2, msModflowFmp, msModflowOwhm2:
        begin
          seCellCount.Increment := 1;
        end;
      else
        Assert(False);
  end;
  FChildModels.Capacity := frmGoPhast.PhastModel.ChildModels.Count;
  FChildModels.Assign(frmGoPhast.PhastModel.ChildModels);
  Node := nil;
  for ChildIndex := 0 to FChildModels.Count - 1 do
  begin
    Edit := FChildModels.Items[ChildIndex] as TChildModelEdit;
    Node := tvChildModels.Items.AddObject(Node, Edit.ModelName, Edit);
  end;
  cbSaveBFH.Checked := frmGoPhast.PhastModel.SaveBfhBoundaryConditions;
  EnableControls;
end;

procedure TfrmChildModels.LayoutMultiRowEditControl;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgDiscretization, rdeDiscretization, nil, Ord(dsDiscretization));

end;

procedure TfrmChildModels.rdeDiscretizationChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgDiscretization, Ord(dsDiscretization),
    rdeDiscretization.Text);
  rdgDiscretizationExit(nil);
end;

procedure TfrmChildModels.rdeFluxClosureChange(Sender: TObject);
var
  Value: Extended;
  Edit: TChildModelEdit;
begin
  inherited;
  if tvChildModels.Selected <> nil then
  begin
    if TryStrToFloat(rdeFluxClosure.Text, Value) then
    begin
      Edit := tvChildModels.Selected.Data;
      Edit.FluxClosureCriterion := Value;
    end;
  end;
end;

procedure TfrmChildModels.rdeHeadClosureChange(Sender: TObject);
var
  Value: Extended;
  Edit: TChildModelEdit;
begin
  inherited;
  if tvChildModels.Selected <> nil then
  begin
    if TryStrToFloat(rdeHeadClosure.Text, Value) then
    begin
      Edit := tvChildModels.Selected.Data;
      Edit.HeadClosureCriterion := Value;
    end;
  end;
end;

procedure TfrmChildModels.rdeRelaxFluxChange(Sender: TObject);
var
  Value: Extended;
  Edit: TChildModelEdit;
begin
  inherited;
  if tvChildModels.Selected <> nil then
  begin
    if TryStrToFloat(rdeRelaxFlux.Text, Value) then
    begin
      Edit := tvChildModels.Selected.Data;
      Edit.FluxRelaxationFactor := Value;
    end;
  end;
end;

procedure TfrmChildModels.rdeRelaxHeadsChange(Sender: TObject);
var
  Value: Extended;
  Edit: TChildModelEdit;
begin
  inherited;
  if tvChildModels.Selected <> nil then
  begin
    if TryStrToFloat(rdeRelaxHeads.Text, Value) then
    begin
      Edit := tvChildModels.Selected.Data;
      Edit.HeadRelaxationFactor := Value;
    end;
  end;
end;

procedure TfrmChildModels.rdgDiscretizationColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControl
end;

procedure TfrmChildModels.rdgDiscretizationEnter(Sender: TObject);
begin
  inherited;
  FAlreadyHandled := False;
end;

procedure TfrmChildModels.rdgDiscretizationExit(Sender: TObject);
begin
  inherited;
  if FAlreadyHandled then
  begin
    FAlreadyHandled := False;
  end
  else
  begin
    WriteSublayerDiscretization;
  end;
end;

procedure TfrmChildModels.rdgDiscretizationHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControl;
end;

procedure TfrmChildModels.rdgDiscretizationMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(rdgDiscretization, rdeDiscretization, Ord(dsDiscretization));
end;

procedure TfrmChildModels.seBottomLayerChange(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.Discretization.BottomLayerInUnit := seBottomLayer.AsInteger-1;
  end;
end;

procedure TfrmChildModels.seBottomLayerExit(Sender: TObject);
begin
  inherited;
  ReadSublayerDiscretization;
end;

procedure TfrmChildModels.seCellCountExit(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    case frmGoPhast.ModelSelection of
      msModflowLGR:
        begin
          if seCellCount.AsInteger <= 2 then
          begin
            seCellCount.AsInteger := 3;
          end;
        end;
      msModflowLGR2, msModflowFmp, msModflowOwhm2:
        begin
          if seCellCount.AsInteger <= 1 then
          begin
            seCellCount.AsInteger := 2;
          end;
        end;
      else
        Assert(False);
    end;
    if (frmGoPhast.ModelSelection in [msModflowLGR])
      and not Odd(seCellCount.AsInteger) then
    begin
      seCellCount.AsInteger := seCellCount.AsInteger + 1;
    end;
    Edit.ChildCellsPerParentCell := seCellCount.AsInteger;
  end;
end;

procedure TfrmChildModels.seMaxIterationsChange(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.MaxIterations := seMaxIterations.AsInteger;
  end;
end;

procedure TfrmChildModels.ReadSublayerDiscretization;
var
  List: TList;
  ParentLayerCount: Integer;
  RowIndex: Integer;
  DisIndex: Integer;
  EndLayer: Integer;
  Item: TChildDiscretization;
  LayerGroup: TLayerGroup;
  LayerGroupIndex: Integer;
  Edit: TChildModelEdit;
begin
  if (tvChildModels.Selected = nil) then
  begin
    Exit;
  end;
  List := TList.Create;
  try
    FillListWithLayerGroups(List);
    Edit := tvChildModels.Selected.Data;
    ParentLayerCount := 0;
    for LayerGroupIndex := 0 to List.Count - 1 do
    begin
      LayerGroup := List[LayerGroupIndex];
      if (LayerGroupIndex = List.Count - 1) then
      begin
        ParentLayerCount := ParentLayerCount
          + Edit.Discretization.BottomLayerInUnit+1;
      end
      else
      begin
        ParentLayerCount := ParentLayerCount + LayerGroup.LayerCount;
      end;
    end;
    rdgDiscretization.BeginUpdate;
    try
      RowIndex := 0;
      rdgDiscretization.RowCount := ParentLayerCount + 1;
      for LayerGroupIndex := 0 to List.Count - 1 do
      begin
        LayerGroup := List[LayerGroupIndex];
        if LayerGroupIndex = List.Count - 1 then
        begin
          EndLayer := Edit.Discretization.BottomLayerInUnit;
        end
        else
        begin
          EndLayer := LayerGroup.LayerCount - 1;
        end;
        for DisIndex := 0 to EndLayer do
        begin
          Inc(RowIndex);
          rdgDiscretization.Cells[Ord(dsLayerGroup),RowIndex] := LayerGroup.AquiferName;
          rdgDiscretization.Cells[Ord(dsParentLayer),RowIndex] := IntToStr(DisIndex+1);
          Item := Edit.Discretization.
            GetAnItemByGroupAndLayer(LayerGroup, DisIndex);
          Assert(Item <> nil);
          rdgDiscretization.Cells[Ord(dsDiscretization),RowIndex] :=
            IntToStr(Item.Discretization);
          rdgDiscretization.Objects[Ord(dsLayerGroup),RowIndex] := LayerGroup;
        end;
      end;
    finally
      rdgDiscretization.EndUpdate;
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmChildModels.rgCouplingMethodClick(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.CouplingMethod := TCouplingMethod(
      rgCouplingMethod.ItemIndex);
  end;
  EnableControls;
end;

procedure TfrmChildModels.rgPrintIterationsClick(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.LgrPrintChoice := TLgrPrintChoice(
      rgPrintIterations.ItemIndex);
  end;
end;

procedure TfrmChildModels.rgStartingHeadsClick(Sender: TObject);
var
  Edit: TChildModelEdit;
begin
  inherited;
  if (tvChildModels.Selected <> nil) then
  begin
    Edit := tvChildModels.Selected.Data;
    Edit.StartingHeadSource := TStartingHeadSource(
      rgStartingHeads.ItemIndex);
  end;
end;

procedure TfrmChildModels.FillListWithLayerGroups(List: TList);
var
  LayerGroup: TLayerGroup;
  LayerGroupIndex: Integer;
  Edit: TChildModelEdit;
begin
  Edit := tvChildModels.Selected.Data;
  for LayerGroupIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := frmGoPhast.PhastModel.LayerStructure[LayerGroupIndex];
    if LayerGroup.RunTimeSimulated then
    begin
      List.Add(LayerGroup);
    end;
    if Edit.Discretization.BottomLayerGroup = LayerGroup then
    begin
      break;
    end;
  end;
end;

procedure TfrmChildModels.SetData;
var
  Undo: TUndoChildModelChange;
begin
  Undo := TUndoChildModelChange.Create(cbSaveBFH.Checked, FChildModels);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmChildModels.tvChildModelsChange(Sender: TObject; Node: TTreeNode);
var
  Edit: TChildModelEdit;
begin
  inherited;
  Assert(Node <> nil);
  Edit := Node.Data;
  edModelName.Text := Edit.ModelName;
  comboBottomUnit.ItemIndex := comboBottomUnit.Items.IndexOfObject(
    Edit.Discretization.BottomLayerGroup);
  if Edit.Discretization.BottomLayerGroup <> nil then
  begin
    seBottomLayer.MaxValue :=
      Edit.Discretization.BottomLayerGroup.LayerCount;
    seBottomLayer.AsInteger := Edit.Discretization.BottomLayerInUnit+1;
  end;
  seCellCount.AsInteger := Edit.ChildCellsPerParentCell;
  ReadSublayerDiscretization;
  rgStartingHeads.ItemIndex := Ord(Edit.StartingHeadSource);
//  cbOneWayCoupling.Checked := Edit.OneWayCoupling;
  rgCouplingMethod.ItemIndex := Ord(Edit.CouplingMethod);
  seMaxIterations.AsInteger := Edit.MaxIterations;
  rgPrintIterations.ItemIndex := Ord(Edit.LgrPrintChoice);
  rdeRelaxHeads.Text := FloatToStr(Edit.HeadRelaxationFactor);
  rdeRelaxFlux.Text := FloatToStr(Edit.FluxRelaxationFactor);
  rdeHeadClosure.Text := FloatToStr(Edit.HeadClosureCriterion);
  rdeFluxClosure.Text := FloatToStr(Edit.FluxClosureCriterion);
  EnableControls;
end;

procedure TfrmChildModels.tvChildModelsChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  inherited;
  FAlreadyHandled := False;
  rdgDiscretizationExit(Sender);
  FAlreadyHandled := True;
end;

procedure TfrmChildModels.WriteSublayerDiscretization;
var
  Edit: TChildModelEdit;
  RowIndex: Integer;
  LayerGroup: TLayerGroup;
  Layer: Integer;
  Item: TChildDiscretization;
  AValue: Integer;
begin
  if (tvChildModels.Selected = nil) then
  begin
    Exit;
  end;
  Edit := tvChildModels.Selected.Data;
  for RowIndex := 1 to rdgDiscretization.RowCount - 1 do
  begin
    LayerGroup := rdgDiscretization.Objects[Ord(dsLayerGroup),RowIndex] as TLayerGroup;
    Layer := StrToInt(rdgDiscretization.Cells[Ord(dsParentLayer),RowIndex]) -1;
    Item := Edit.Discretization.GetAnItemByGroupAndLayer(LayerGroup, Layer);
    Assert(Item <> nil);
    AValue := StrToInt(rdgDiscretization.Cells[Ord(dsDiscretization),RowIndex]);
    If (frmGoPhast.ModelSelection in [msModflowLGR]) and not Odd(AValue) then
    begin
      Inc(AValue);
      rdgDiscretization.Cells[Ord(dsDiscretization),RowIndex] := IntToStr(AValue);
    end;
    Item.Discretization := AValue;
  end;
end;

{ TUndoChildModelChange }

procedure TUndoChildModelChange.ChangeChildModel(Source: TCollection);
var
  ChildIndex: Integer;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.PhastModel.ChildModels.Assign(Source);
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      frmGoPhast.PhastModel.ChildModels[ChildIndex].
        ChildModel.OnHeadOBsChanged := frmGoPhast.EnableExportHeadObs;
    end;
    if frmDisplayData <> nil then
    begin
      frmDisplayData.frameHeadObservationResults.UpdateSelectedModel;
    end;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

constructor TUndoChildModelChange.Create(SaveBfhBoundaries: boolean;
  var NewChildModels: TChildModelEditCollection);
var
  ChildIndex: Integer;
//  AModel: TChildModel;
  AnItem: TChildModelItem;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  UsedModels: TUsedWithModelCollection;
  ChildModel: TChildModel;
  AUseChanges: TUseChanges;
  NItem: TChildModelEdit;
  DeletedItems: TList;
  ChildModels: TChildModelCollection;
  ChildItem: TOrderedItem;
begin
  FNewSaveBfhBoundaries := SaveBfhBoundaries;
  FOldSaveBfhBoundaries := frmGoPhast.PhastModel.SaveBfhBoundaryConditions;
  FOldChildModels := TChildModelCollection.Create(nil);
  FOldChildModels.Assign(frmGoPhast.PhastModel.ChildModels);

  FNewChildModels := NewChildModels;
  NewChildModels := nil;

  FScreenObjects := TList.Create;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    FScreenObjects.Add(frmGoPhast.PhastModel.ChildModels[ChildIndex].
      ChildModel.HorizontalPositionScreenObject);
  end;

  FDeletedModels := TList.Create;

  DeletedItems := TList.Create;
  try
    ChildModels := frmGoPhast.PhastModel.ChildModels;
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      DeletedItems.Add(ChildModels[ChildIndex]);
    end;
    for ChildIndex := 0 to FNewChildModels.Count - 1 do
    begin
      NItem := FNewChildModels[ChildIndex];
      ChildItem := frmGoPhast.PhastModel.ChildModels.FindMatchingItem(NItem);
      if ChildItem <> nil then
      begin
        DeletedItems.Remove(ChildItem)
      end;
    end;
    for ChildIndex := 0 to DeletedItems.Count - 1 do
    begin
      AnItem := DeletedItems[ChildIndex];
      FDeletedModels.Add(AnItem.ChildModel);
    end;
  finally
    DeletedItems.Free;
  end;

//  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
//  begin
//    AnItem := frmGoPhast.PhastModel.ChildModels[ChildIndex];
//    if FNewChildModels.FindMatchingItem(AnItem) = nil then
//    begin
//      FDeletedModels.Add(AnItem.ChildModel);
//    end;
//  end;

  FChangedUsedWithModels := TObjectList<TUseChanges>.Create;
  if FDeletedModels.Count > 0 then
  begin
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      UsedModels := AScreenObject.UsedModels;
      if not UsedModels.UsedWithAllModels then
      begin
        for ChildIndex := 0 to FDeletedModels.Count - 1 do
        begin
          ChildModel := FDeletedModels[ChildIndex];
          if UsedModels.UsesModel(ChildModel) then
          begin
            AUseChanges := TUseChanges.Create;
            FChangedUsedWithModels.Add(AUseChanges);
            AUseChanges.ScreenObject := AScreenObject;
            AUseChanges.UsedWithModelCollection := UsedModels;
          end;
        end;
      end;
    end;
  end;
end;

function TUndoChildModelChange.Description: string;
begin
  result := StrChangeChildModels;
end;

destructor TUndoChildModelChange.Destroy;
begin
  FChangedUsedWithModels.Free;
  FDeletedModels.Free;
  FScreenObjects.Free;
  FOldChildModels.Free;
  FNewChildModels.Free;
  inherited;
end;

procedure TUndoChildModelChange.DoCommand;
var
  ChangedIndex: Integer;
  AnItem: TUseChanges;
  AModel: TChildModel;
  DeleteIndex: Integer;
begin
  inherited;
  frmGoPhast.PhastModel.SaveBfhBoundaryConditions := FNewSaveBfhBoundaries;
  ChangeChildModel(FNewChildModels);
  for DeleteIndex := 0 to FDeletedModels.Count-1 do
  begin
    AModel := FDeletedModels[DeleteIndex];
    for ChangedIndex := 0 to FChangedUsedWithModels.Count - 1 do
    begin
      AnItem := FChangedUsedWithModels[ChangedIndex];
      AnItem.ScreenObject.UsedModels.RemoveModel(AModel);
    end;
  end;
  frmGoPhast.UpdateModelCubeBreaks;
end;

procedure TUndoChildModelChange.Undo;
var
  ChildIndex: Integer;
  ScreenObject: TScreenObject;
  ChangedIndex: Integer;
  AnItem: TUseChanges;
begin
  inherited;
  frmGoPhast.PhastModel.SaveBfhBoundaryConditions := FOldSaveBfhBoundaries;
  ChangeChildModel(FOldChildModels);
  Assert(frmGoPhast.PhastModel.ChildModels.Count = FScreenObjects.Count);
  for ChildIndex := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[ChildIndex];
    frmGoPhast.PhastModel.ChildModels[ChildIndex].
      ChildModel.HorizontalPositionScreenObject := ScreenObject;
  end;
  for ChangedIndex := 0 to FChangedUsedWithModels.Count - 1 do
  begin
    AnItem := FChangedUsedWithModels[ChangedIndex];
    AnItem.ScreenObject.UsedModels := AnItem.UsedWithModelCollection;
  end;

  frmGoPhast.UpdateModelCubeBreaks;
end;

{ TUseChanges }

constructor TUseChanges.Create;
begin
  FUsedWithModelCollection := TUsedWithModelCollection.Create(nil);
end;

destructor TUseChanges.Destroy;
begin
  FUsedWithModelCollection.Free;
  inherited;
end;

procedure TUseChanges.SetUsedWithModelCollection(
  const Value: TUsedWithModelCollection);
begin
  FUsedWithModelCollection.Assign(Value);
end;

end.
