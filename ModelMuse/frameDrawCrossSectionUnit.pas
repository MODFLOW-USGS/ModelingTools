unit frameDrawCrossSectionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, VirtualTrees, Buttons,
  StdCtrls, DataSetUnit, GoPhastTypes, JvExControls, JvColorBox,
  JvColorButton, Generics.Collections, CheckLst, JvExCheckLst,
  JvCheckListBox, ExtCtrls, Vcl.Samples.Spin, Vcl.Mask, JvExMask, JvSpin;

type
  TframeDrawCrossSection = class(TFrame)
    vstAvailableDataSets: TVirtualStringTree;
    btnAddDataSet: TSpeedButton;
    btnRemoveDataSet: TSpeedButton;
    clrbtnSelectedColor: TJvColorButton;
    pnlUsed: TPanel;
    pnlTop: TPanel;
    lstSelectedDataSets: TListBox;
    lblDataSets: TLabel;
    pnlBottom: TPanel;
    spl1: TSplitter;
    clbLayers: TJvCheckListBox;
    lblLayers: TLabel;
    lblLineThickness: TLabel;
    seLineThickness: TJvSpinEdit;
    procedure vstAvailableDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstAvailableDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstAvailableDataSetsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FrameResize(Sender: TObject);
    procedure btnAddDataSetClick(Sender: TObject);
    procedure btnRemoveDataSetClick(Sender: TObject);
    procedure vstAvailableDataSetsChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure lstSelectedDataSetsClick(Sender: TObject);
    procedure lstSelectedDataSetsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure clrbtnSelectedColorChange(Sender: TObject);
    procedure lstSelectedDataSetsExit(Sender: TObject);
    procedure vstAvailableDataSetsDblClick(Sender: TObject);
    procedure lstSelectedDataSetsDblClick(Sender: TObject);
  private
    FDataSetDummyObjects: TList;
    FColors: TList<TColor>;
    FColorIndex: Integer;
    procedure GetAvailableDataSets;
    procedure GetSelectedDataSets;
    procedure GetLayers;
    function CanDrawCrossSection(DataArray: TDataArray): boolean;
    procedure SetCrossSectionLayers;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData;
    procedure SetData;
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

uses
  ClassificationUnit, frmCustomGoPhastUnit, RbwParser, PhastDataSets,
  Contnrs, frmGoPhastUnit, PhastModelUnit, Math;

{$R *.dfm}

procedure TframeDrawCrossSection.btnAddDataSetClick(Sender: TObject);
var
  CellText: string;
  ANode: PVirtualNode;
begin
  for ANode in vstAvailableDataSets.SelectedNodes do
  begin
    if not vstAvailableDataSets.HasChildren[ANode] then
    begin
      GetNodeCaption(ANode, CellText, vstAvailableDataSets);
      if lstSelectedDataSets.Items.IndexOf(CellText) < 0 then
      begin
        lstSelectedDataSets.Items.Add(CellText);
        FColors.Add(clBlack);
      end;
    end;
  end;
end;

procedure TframeDrawCrossSection.btnRemoveDataSetClick(Sender: TObject);
var
  SelectIndex: Integer;
begin
  for SelectIndex := lstSelectedDataSets.Items.Count - 1 downto 0 do
  begin
    if lstSelectedDataSets.Selected[SelectIndex] then
    begin
      lstSelectedDataSets.Items.Delete(SelectIndex);
      FColors.Delete(SelectIndex);
      clrbtnSelectedColor.Visible := False;
    end;
  end;
end;

function TframeDrawCrossSection.CanDrawCrossSection(
  DataArray: TDataArray): boolean;
begin
  result := (DataArray.EvaluatedAt = eaBlocks)
    and (DataArray.DataType = rdtDouble)
    and (DataArray.Orientation in [dsoTop, dso3D])
    and DataArray.Visible;
  if not Result then
  begin
    Exit;
  end;
  if DataArray is TCustomSparseDataSet then
  begin
    result := False;
    Exit;
  end;
  if DataArray is TSparseArrayPhastInterpolationDataSet then
  begin
    result := False;
    Exit;
  end;
end;

procedure TframeDrawCrossSection.clrbtnSelectedColorChange(Sender: TObject);
begin
  if (FColorIndex >= 0) and (FColorIndex < FColors.Count) then
  begin
    FColors[FColorIndex] := clrbtnSelectedColor.Color;
  end;
end;

destructor TframeDrawCrossSection.Destroy;
begin
  FreeAndNil(vstAvailableDataSets);
  FColors.Free;
  FDataSetDummyObjects.Free;
  inherited;
end;

procedure TframeDrawCrossSection.SetCrossSectionLayers;
var
  CrossSection: TCrossSection;
  AllChecked: Boolean;
  index: Integer;
begin
  AllChecked := True;
  for index := 0 to clbLayers.Items.Count - 1 do
  begin
    if not clbLayers.Checked[index] then
    begin
      AllChecked := False;
      break;
    end;
  end;
  CrossSection := frmGoPhast.PhastModel.CrossSection;
  CrossSection.AllLayers := AllChecked;
  if not AllChecked then
  begin
    CrossSection.LayersToUse.Clear;
    for index := 0 to clbLayers.Items.Count - 1 do
    begin
      if clbLayers.Checked[index] then
      begin
        CrossSection.LayersToUse.Add(index);
      end;
    end;
  end;
end;

procedure TframeDrawCrossSection.FrameResize(Sender: TObject);
begin
  btnAddDataSet.Left := (Width - btnAddDataSet.Width) div 2;
  btnRemoveDataSet.Left := btnAddDataSet.Left;
  vstAvailableDataSets.Width := btnAddDataSet.Left - vstAvailableDataSets.Left - 8;
  pnlUsed.Width := vstAvailableDataSets.Width;
end;

procedure TframeDrawCrossSection.GetData;
begin
  if Font.Height < 0 then
  begin
    lstSelectedDataSets.ItemHeight := -Font.Height + 2;
  end
  else
  begin
    lstSelectedDataSets.ItemHeight := Font.Height;
  end;
  GetAvailableDataSets;
  GetSelectedDataSets;
  GetLayers;
  seLineThickness.AsInteger := frmGoPhast.PhastModel.CrossSection.LineThickness;
end;

procedure TframeDrawCrossSection.GetLayers;
var
  index: Integer;
  LayerCount: integer;
  ChildIndex: Integer;
  AModel: TChildModel;
  LayerIndex: Integer;
  CrossSection: TCrossSection;
  ALayer: integer;
begin
  clbLayers.Items.BeginUpdate;
  try
    clbLayers.Items.Clear;
    LayerCount := frmGoPhast.PhastModel.LayerCount;
    if frmGoPhast.PhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        AModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if AModel <> nil then
        begin
          LayerCount := Max(LayerCount, AModel.LayerCount);
        end;
      end;
    end;
    for LayerIndex := 1 to LayerCount do
    begin
      clbLayers.Items.Add(IntToStr(LayerIndex));
    end;
    CrossSection := frmGoPhast.PhastModel.CrossSection;
    if CrossSection.AllLayers then
    begin
      clbLayers.CheckAll
    end
    else
    begin
      for index := 0 to CrossSection.LayersToUse.Count - 1 do
      begin
        ALayer := CrossSection.LayersToUse[index];
        if ALayer < clbLayers.Items.Count then
        begin
          clbLayers.Checked[ALayer] := True;
        end;
      end;
    end;
  finally
    clbLayers.Items.EndUpdate;
  end;
end;

procedure TframeDrawCrossSection.GetSelectedDataSets;
var
  CrossSection: TCrossSection;
  index: Integer;
  ADataArray: TDataArray;
begin
  lstSelectedDataSets.Items.Clear;
  CrossSection := frmGoPhast.PhastModel.CrossSection;
  lstSelectedDataSets.Items.Capacity := CrossSection.DataArrays.Count;
  FColors.Clear;
  FColors.Capacity := CrossSection.DataArrays.Count;
  for index := 0 to CrossSection.DataArrays.Count - 1 do
  begin
    ADataArray := CrossSection.DataArrays[index];
    lstSelectedDataSets.Items.AddObject(ADataArray.Name, ADataArray);
    FColors.Add(CrossSection.Colors[index]);
  end;
end;

procedure TframeDrawCrossSection.GetAvailableDataSets;
begin
  vstAvailableDataSets.Clear;
  FillVirtualStringTreeWithDataSets(vstAvailableDataSets,
    FDataSetDummyObjects, nil, CanDrawCrossSection);
end;

procedure TframeDrawCrossSection.Loaded;
begin
  inherited;
  FDataSetDummyObjects := TObjectList.Create;
  FColors:= TList<TColor>.Create;
end;

procedure TframeDrawCrossSection.lstSelectedDataSetsClick(Sender: TObject);
var
  lRect: TRect;
begin
  FColorIndex := lstSelectedDataSets.ItemIndex;
  if FColorIndex = -1 then
  begin
    clrbtnSelectedColor.Visible := False;
    Exit;
  end;

  clrbtnSelectedColor.Color := FColors[FColorIndex];
  clrbtnSelectedColor.Parent := lstSelectedDataSets;

  lRect := lstSelectedDataSets.ItemRect(FColorIndex) ;
  clrbtnSelectedColor.Top := lRect.Top;
  clrbtnSelectedColor.Left := lRect.Right - clrbtnSelectedColor.Width  -1;
  clrbtnSelectedColor.Height := (lRect.Bottom - lRect.Top) ;


  clrbtnSelectedColor.Visible := True;

  clrbtnSelectedColor.SetFocus;
  lstSelectedDataSets.Invalidate;
end;

procedure TframeDrawCrossSection.lstSelectedDataSetsDblClick(Sender: TObject);
begin
  btnRemoveDataSetClick(Sender);
end;

procedure TframeDrawCrossSection.lstSelectedDataSetsDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ButtonRect: TRect;
begin
  ButtonRect := Rect;
  ButtonRect.Left := ButtonRect.Right - clrbtnSelectedColor.Width  -1;
  with lstSelectedDataSets.Canvas do
  begin
    Brush.Color := FColors[Index];

    FillRect(ButtonRect);
    Rect.Right := ButtonRect.Left;
    Brush.Color := lstSelectedDataSets.Color;
    Font.Color := lstSelectedDataSets.Font.Color;

    FillRect(Rect);
    if odFocused In State then
    begin
      DrawFocusRect(Rect);
    end;
    TextOut(Rect.Left, Rect.Top, (Control as TListBox).Items[Index]);
  end;
end;

procedure TframeDrawCrossSection.lstSelectedDataSetsExit(Sender: TObject);
begin
  if not clrbtnSelectedColor.Focused then
  begin
    clrbtnSelectedColor.Visible := False;
    lstSelectedDataSets.Invalidate;
  end;
end;

procedure TframeDrawCrossSection.SetData;
var
  DataArrayList: TDataArrayObjectList;
  index: Integer;
  DataArrayManager: TDataArrayManager;
  ADataArray: TDataArray;
begin
  DataArrayList := TDataArrayObjectList.Create;
  try
    DataArrayList.OwnsObjects := False;
    DataArrayList.Capacity := lstSelectedDataSets.Items.Count;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for index := 0 to lstSelectedDataSets.Items.Count - 1 do
    begin
      ADataArray := DataArrayManager.
        GetDataSetByName(lstSelectedDataSets.Items[index]);
      if ADataArray <> nil then
      begin
        DataArrayList.Add(ADataArray);
      end;
    end;
    frmGoPhast.PhastModel.CrossSection.DataArrays := DataArrayList;
    frmGoPhast.PhastModel.CrossSection.Colors := FColors;
    SetCrossSectionLayers;
    frmGoPhast.PhastModel.CrossSection.LineThickness := seLineThickness.AsInteger;
  finally
    DataArrayList.Free;
  end;
end;

procedure TframeDrawCrossSection.vstAvailableDataSetsChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ANode: PVirtualNode;
begin
  if Sender.HasChildren[Node] then
  begin
    ANode := Sender.GetFirstChild(Node);
    while ANode <> nil do
    begin
      Sender.Selected[ANode] := Sender.Selected[Node];
      vstAvailableDataSetsChange(Sender, ANode);
      ANode := Sender.GetNextSibling(ANode)
    end;
  end;
end;

procedure TframeDrawCrossSection.vstAvailableDataSetsDblClick(Sender: TObject);
begin
  btnAddDataSetClick(Sender);
end;

procedure TframeDrawCrossSection.vstAvailableDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TframeDrawCrossSection.vstAvailableDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TframeDrawCrossSection.vstAvailableDataSetsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  GetNodeCaption(Node, CellText, Sender);
end;

end.
