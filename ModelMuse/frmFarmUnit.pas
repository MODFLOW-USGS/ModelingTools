unit frmFarmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls, frameScreenObjectUnit, frameFarmUnit, VirtualTrees,
  Mask, JvExMask, JvSpin, ModflowFmpFarmUnit, UndoItems,
  ModelMuseFarmFormInterfacesUnit, GoPhastTypes, System.Win.VCLCom,
  Vcl.ComCtrls;

type
  TUndoEditFarms = class(TCustomUndo)
  private
    FOldFarms: TFarmCollection;
    FNewFarms: TFarmCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var NewFarms: TFarmCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

  PFarmNodeData = ^TFarmNodeData;
  TFarmNodeData = record
    Farm: TFarm;
  end;


  TfrmFarm = class(TfrmCustomGoPhast)
    vstFarms: TVirtualStringTree;
    frameFarm: TframeFarm;
    pnlFarms: TPanel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlControls: TPanel;
    sbAddUnit: TSpeedButton;
    sbInsertUnit: TSpeedButton;
    sbDeleteUnit: TSpeedButton;
    seFarms: TJvSpinEdit;
    lblFarmCount: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure vstFarmsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstFarmsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstFarmsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure sbAddUnitClick(Sender: TObject);
    procedure vstFarmsAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstFarmsRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure sbInsertUnitClick(Sender: TObject);
    procedure sbDeleteUnitClick(Sender: TObject);
    procedure seFarmsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure frameFarmpcMainChange(Sender: TObject);
    procedure frameFormulaGridCostsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridCropsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridDiversionGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridReturnFlowGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameDeliveryGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameFormulaGridWaterRightsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameGW_AllocationGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridCropsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameFarmedFarmNameChange(Sender: TObject);
    procedure frameFormulaGridCropsedFormulaChange(Sender: TObject);
    procedure frameFarmseFarmIdChange(Sender: TObject);
  private
    FFarms: TFarmCollection;
    FSelectedFarms: TFarmList;
    FChangingSelection: Boolean;
    FModel: IModelMuseModel;
    procedure GetData;
    procedure SetData;
    function AddAFarm: TFarm;
    procedure ClearSelection;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; Model: IModelMuseModel); reintroduce;
    { Public declarations }
  end;

var
  frmFarm: TfrmFarm;

implementation

uses
  frmGoPhastUnit, ModflowFmpCropUnit, ModflowFmpIrrigationUnit;

{$R *.dfm}

procedure TfrmFarm.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFarm.FormCreate(Sender: TObject);
begin
  inherited;
  frmGoPhast.PhastModel.RegisterGlobalVariables(frameFarm.rbwprsrFarmParser);
  frameFarm.InitializeControls;
  FFarms:= TFarmCollection.Create(nil);
  FSelectedFarms := TFarmList.Create;
  frameFarm.pcMain.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmFarm.FormDestroy(Sender: TObject);
begin
  inherited;
  FFarms.Free;
  FreeAndNil(FSelectedFarms);
end;

procedure TfrmFarm.FormShow(Sender: TObject);
var
  PageIndex: Integer;
  APage: TTabSheet;
begin
  inherited;
  if GlobalFont <> nil then
  begin
    for PageIndex := 0 to frameFarm.pcMain.PageCount - 1 do
    begin
      APage := frameFarm.pcMain.Pages[PageIndex];
      APage.Font := GlobalFont;
//      APage.Color := GlobalColor;
      UpdateSubComponents(APage);
    end;

//    frameFarm.PanelOwhm2.Font := GlobalFont;
//    frameFarm.PanelOwhm2.Color := GlobalColor;
//    UpdateSubComponents(frameFarm.PanelOwhm2);

    frameFarm.frameFormulaGridCosts.Font := GlobalFont;
    frameFarm.frameFormulaGridCosts.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameFormulaGridCosts);

    frameFarm.frameFormulaGridCrops.Font := GlobalFont;
    frameFarm.frameFormulaGridCrops.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameFormulaGridCrops);

    frameFarm.pnlTop.Font := GlobalFont;
    frameFarm.pnlTop.Color := GlobalColor;
    UpdateSubComponents(frameFarm.pnlTop);

    frameFarm.frameFormulaGridDiversion.Font := GlobalFont;
    frameFarm.frameFormulaGridDiversion.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameFormulaGridDiversion);

    frameFarm.frameGW_Allocation.Font := GlobalFont;
    frameFarm.frameGW_Allocation.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameGW_Allocation);

    frameFarm.frameDelivery.Font := GlobalFont;
    frameFarm.frameDelivery.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameDelivery);

    frameFarm.frameFormulaGridReturnFlow.Font := GlobalFont;
    frameFarm.frameFormulaGridReturnFlow.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameFormulaGridReturnFlow);

    frameFarm.frameFormulaGridWaterRights.Font := GlobalFont;
    frameFarm.frameFormulaGridWaterRights.Color := GlobalColor;
    UpdateSubComponents(frameFarm.frameFormulaGridWaterRights);
  end;

end;

procedure TfrmFarm.frameDeliveryGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameDeliveryGridSetEditText(Sender, ACol, ARow, Value);

end;

procedure TfrmFarm.frameFarmedFarmNameChange(Sender: TObject);
begin
  inherited;
  frameFarm.seFarmIdChange(Sender);

end;

procedure TfrmFarm.frameFarmpcMainChange(Sender: TObject);
begin
  inherited;
  Self.HelpKeyword := frameFarm.pcMain.ActivePage.HelpKeyword;
end;

procedure TfrmFarm.frameFarmseFarmIdChange(Sender: TObject);
begin
  inherited;
  frameFarm.seFarmIdChange(Sender);

end;

procedure TfrmFarm.frameFormulaGridCostsGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameFormulaGridCostsGridSetEditText(Sender, ACol, ARow, Value);

end;

procedure TfrmFarm.frameFormulaGridCropsedFormulaChange(Sender: TObject);
begin
  inherited;
  frameFarm.frameFormulaGridCropsedFormulaChange(Sender);

end;

procedure TfrmFarm.frameFormulaGridCropsGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  frameFarm.frameFormulaGridCropsGridButtonClick(Sender, ACol, ARow);

end;

procedure TfrmFarm.frameFormulaGridCropsGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameFormulaGridCropsGridSetEditText(Sender, ACol, ARow, Value);

end;

procedure TfrmFarm.frameFormulaGridDiversionGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameFormulaGridDiversionGridSetEditText(Sender, ACol, ARow,
    Value);

end;

procedure TfrmFarm.frameFormulaGridReturnFlowGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameFormulaGridReturnFlowGridSetEditText(Sender, ACol, ARow,
    Value);

end;

procedure TfrmFarm.frameFormulaGridWaterRightsGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameFormulaGridWaterRightsGridSetEditText(Sender, ACol, ARow,
    Value);

end;

procedure TfrmFarm.frameGW_AllocationGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  frameFarm.frameGW_AllocationGridSetEditText(Sender, ACol, ARow, Value);

end;

procedure TfrmFarm.GetData;
var
  ANode: PVirtualNode;
begin
  if FModel.ModelSelection = msModflowFMP then
  begin
    frameFarm.tabCrops.Caption := 'Crop Efficiencies';
  end
  else
  begin
    frameFarm.tabCrops.Caption := 'Irrigation Efficiencies';
  end;
  frameFarm.PanelOwhm2.Visible := FModel.ModelSelection <> msModflowFMP;
  FFarms.Assign(frmGoPhast.PhastModel.Farms);
  vstFarms.HasChildren[vstFarms.RootNode] := FFarms.Count > 0;
  vstFarms.ChildCount[vstFarms.RootNode] := FFarms.Count;
  if FFarms.Count > 0 then
  begin
    ANode := vstFarms.GetFirstChild(vstFarms.RootNode);
    vstFarms.Selected[ANode] := True;
  end;
  seFarms.AsInteger := FFarms.Count;
end;

procedure TfrmFarm.sbAddUnitClick(Sender: TObject);
var
  ANode: PVirtualNode;
begin
  inherited;
  frameFarm.SetData(FSelectedFarms);
  AddAFarm;
  ClearSelection;
  ANode := vstFarms.GetLastChild(vstFarms.RootNode);
  vstFarms.Selected[ANode] := True;
  seFarms.AsInteger := FFarms.Count;
  frameFarm.Enabled := (vstFarms.SelectedCount > 0);
  vstFarms.Invalidate
end;

procedure TfrmFarm.sbDeleteUnitClick(Sender: TObject);
var
  ANode: PVirtualNode;
  FarmData: PFarmNodeData;
begin
  inherited;
  FChangingSelection := True;
  try
    for ANode in vstFarms.SelectedNodes do
    begin
      FarmData := vstFarms.GetNodeData(ANode);
      FarmData.Farm.Free;
    end;
    vstFarms.HasChildren[vstFarms.RootNode] := FFarms.Count > 0;
    vstFarms.ChildCount[vstFarms.RootNode] := FFarms.Count;
    vstFarms.ReinitNode(vstFarms.RootNode, True);
    FSelectedFarms.Clear;
    seFarms.AsInteger := FFarms.Count;
    for ANode in vstFarms.SelectedNodes do
    begin
      FarmData := vstFarms.GetNodeData(ANode);
      FSelectedFarms.Add(FarmData.Farm);
    end;
    frameFarm.GetData(FSelectedFarms);
  finally
    FChangingSelection := False;
  end;
  frameFarm.Enabled := (seFarms.AsInteger > 0) and (vstFarms.SelectedCount > 0);
  vstFarms.Invalidate;
end;

procedure TfrmFarm.sbInsertUnitClick(Sender: TObject);
var
  SelectedNode: PVirtualNode;
  FarmData: PFarmNodeData;
  NewFarm: TFarm;
  SelectedIndex: Integer;
begin
  inherited;
  frameFarm.SetData(FSelectedFarms);
  SelectedNode := vstFarms.GetFirstSelected;
  if SelectedNode = nil then
  begin
    SelectedIndex := 0;
  end
  else
  begin
    FarmData := vstFarms.GetNodeData(SelectedNode);
    SelectedIndex := FarmData.Farm.Index;
  end;

  NewFarm := AddAFarm;
  NewFarm.Index := SelectedIndex;

  ClearSelection;

  vstFarms.ReinitNode(vstFarms.RootNode, True);

  if SelectedNode = nil then
  begin
    SelectedNode := vstFarms.GetFirstChild(vstFarms.RootNode);
  end;
  vstFarms.Selected[SelectedNode] := True;
  seFarms.AsInteger := FFarms.Count;
  frameFarm.Enabled := (vstFarms.SelectedCount > 0);
  vstFarms.Invalidate

end;

procedure TfrmFarm.seFarmsChange(Sender: TObject);
var
  AFarm: TFarm;
begin
  inherited;
  if FFarms.Count <> seFarms.AsInteger then
  begin
    while FFarms.Count > seFarms.AsInteger do
    begin
      AFarm := FFarms.Last;
      FSelectedFarms.Remove(AFarm);
      AFarm.Free;
    end;
    while FFarms.Count < seFarms.AsInteger do
    begin
      AddAFarm;
    end;
    vstFarms.HasChildren[vstFarms.RootNode] := FFarms.Count > 0;
    vstFarms.ChildCount[vstFarms.RootNode] := FFarms.Count;
  end;
  frameFarm.Enabled := (seFarms.AsInteger > 0) and (vstFarms.SelectedCount > 0);

end;

procedure TfrmFarm.SetData;
var
  Undo: TUndoEditFarms;
begin
  frameFarm.SetData(FSelectedFarms);
  FSelectedFarms.Clear;
  Undo := TUndoEditFarms.Create(FFarms);
  frmGoPhast.UndoStack.Submit(Undo);
end;

function TfrmFarm.AddAFarm: TFarm;
var
  CropIndex: Integer;
  Crops: TCropCollection;
  EfficiencyItem: TFarmEfficienciesItem;
  ACrop: TCropItem;
  IrrigationTypes: TIrrigationCollection;
  IrrIndex: Integer;
  IrrigationItem: TIrrigationItem;
begin
  result := FFarms.Add;
  result.FarmId := FFarms.Count;
  Crops := frmGoPhast.PhastModel.FmpCrops;
  for CropIndex := 0 to Crops.Count - 1 do
  begin
    ACrop := Crops[CropIndex];

    EfficiencyItem := result.FarmEfficiencyCollection.Add;
    EfficiencyItem.CropEfficiency.CropName := ACrop.CropName;

    EfficiencyItem := result.AddedCropDemandFlux.Add;
    EfficiencyItem.CropEfficiency.CropName := ACrop.CropName;

    EfficiencyItem := result.AddedCropDemandRate.Add;
    EfficiencyItem.CropEfficiency.CropName := ACrop.CropName;
  end;

  IrrigationTypes := frmGoPhast.PhastModel.IrrigationTypes;
  for IrrIndex := 0 to IrrigationTypes.Count - 1 do
  begin
    IrrigationItem := IrrigationTypes[IrrIndex];

    EfficiencyItem := result.FarmIrrigationEfficiencyCollection.Add;
    EfficiencyItem.CropEfficiency.CropName := IrrigationItem.Name;

    EfficiencyItem := result.FarmIrrigationEfficiencyImprovementCollection.Add;
    EfficiencyItem.CropEfficiency.CropName := IrrigationItem.Name;

    EfficiencyItem := result.AddedDemandRunoffSplitCollection.Add;
    EfficiencyItem.CropEfficiency.CropName := IrrigationItem.Name;

    EfficiencyItem := result.IrrigationUniformity.Add;
    EfficiencyItem.CropEfficiency.CropName := IrrigationItem.Name;
  end;
end;

procedure TfrmFarm.ClearSelection;
var
  ANode: PVirtualNode;
begin
  vstFarms.HasChildren[vstFarms.RootNode] := FFarms.Count > 0;
  vstFarms.ChildCount[vstFarms.RootNode] := FFarms.Count;
  FChangingSelection := True;
  try
    for ANode in vstFarms.SelectedNodes do
    begin
      vstFarms.Selected[ANode] := false;
    end;
  finally
    FChangingSelection := False;
  end;
  FSelectedFarms.Clear;
end;

constructor TfrmFarm.Create(AOwner: TComponent; Model: IModelMuseModel);
begin
  inherited Create(AOwner);
  FModel := Model;
end;

procedure TfrmFarm.vstFarmsAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PFarmNodeData;
begin
  inherited;
  if FChangingSelection then
  begin
    Exit;
  end;
  frameFarm.SetData(FSelectedFarms);
  NodeData := vstFarms.GetNodeData(Node);
  FSelectedFarms.Add(NodeData.Farm);
  frameFarm.GetData(FSelectedFarms);
end;

procedure TfrmFarm.vstFarmsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TFarmNodeData);
end;

procedure TfrmFarm.vstFarmsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PFarmNodeData;
begin
  inherited;
  NodeData := vstFarms.GetNodeData(Node);
  if NodeData.Farm <> nil then
  begin
    CellText := IntToStr(NodeData.Farm.FarmId) + ' ' + NodeData.Farm.FarmName;
  end
  else
  begin
    CellText := '';
  end;
end;

procedure TfrmFarm.vstFarmsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  FarmIndex: Cardinal;
  AFarm: TFarm;
  NodeData: PFarmNodeData;
begin
  inherited;
  if Node <> vstFarms.RootNode then
  begin
    FarmIndex := Node.Index;
    AFarm := FFarms.Items[FarmIndex];
    NodeData := vstFarms.GetNodeData(Node);
    NodeData.Farm := AFarm;
  end;
end;

procedure TfrmFarm.vstFarmsRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PFarmNodeData;
begin
  inherited;
  if FChangingSelection then
  begin
    Exit;
  end;
  if FSelectedFarms <> nil then
  begin
    frameFarm.SetData(FSelectedFarms);
    NodeData := vstFarms.GetNodeData(Node);
    FSelectedFarms.Remove(NodeData.Farm);
    frameFarm.GetData(FSelectedFarms);
  end;
end;

{ TUndoEditFarms }

constructor TUndoEditFarms.Create(var NewFarms: TFarmCollection);
begin
  FNewFarms := NewFarms;
  NewFarms := nil;
  FOldFarms := TFarmCollection.Create(nil);
  FOldFarms.Assign(frmGoPhast.PhastModel.Farms);
end;

function TUndoEditFarms.Description: string;
begin
  result := 'edit farms';
end;

destructor TUndoEditFarms.Destroy;
begin
  FNewFarms.Free;
  FOldFarms.Free;
  inherited;
end;

procedure TUndoEditFarms.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.Farms := FNewFarms;
end;

procedure TUndoEditFarms.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.Farms := FOldFarms;
end;

end.
