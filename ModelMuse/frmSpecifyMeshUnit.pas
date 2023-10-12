unit frmSpecifyMeshUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, frameGridUnit, GrayTabs,
  System.Generics.Collections;

type
  TMeshKind = (mkComplete, mkQuadrilaterals);
  TNodeColumns = (ncLabel, ncX, ncY);
  TNode3DColumns = (n3cNumber, n3cIgnored, nc3X, nc3Y, nc3Z);
  TElementColumns = (ecLabel, ecNode1, ecNode2, ecNode3, ecNode4, ecCount1, ecCount2);
  TElement3DColumns = (e3cNumber, ec3Node1, ec3Node2, ec3Node3, ec3Node4,
    ec3Node5, ec3Node6, ec3Node7, ec3Node8);

  TNodeRecord = record
    Number: Integer;
    X: Extended;
    Y: Extended;
    Z: Extended;
  end;

  TImportNode2D = class(TObject)
    Number: Integer;
    Nodes: TList<TNodeRecord>;
    constructor Create;
    destructor Destroy; override;
  end;

  TElementRecord = record
    Number: Integer;
    Nodes: array[0..7] of Integer;
  end;

  TImportElement2D = class(TObject)
    Nodes: TList<TImportNode2D>;
    Elements: TList<TElementRecord>;
    constructor Create;
    destructor Destroy; override;
    function Key: string;
  end;


  TfrmSpecifyMesh = class(TfrmCustomGoPhast)
    pgcMeshDesign: TPageControl;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    tabNodes: TTabSheet;
    tabElements: TTabSheet;
    frameNodes: TframeGrid;
    frameElements: TframeGrid;
    rgMeshType: TRadioGroup;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure frameNodesseNumberChange(Sender: TObject);
    procedure frameElementsseNumberChange(Sender: TObject);
    procedure frameElementsGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure pgcMeshDesignChange(Sender: TObject);
    procedure rgMeshTypeClick(Sender: TObject);
  private
    FKind: TMeshKind;
    procedure SetData;
    procedure Get2DMesh;
    procedure GetQuadrilaterals;
    procedure Set2DMesh;
    procedure Set3DMesh;
    procedure SetQuadrilaterals;
    procedure Set2DLabels;
    procedure Set3DLabels;
    { Private declarations }
  public
    procedure GetData(Kind: TMeshKind);
    { Public declarations }
  end;


implementation

uses
  frmGoPhastUnit, SutraMeshUnit, UndoItems, FishnetMeshGenerator, RbwDataGrid4,
  QuadTreeClass, ModelMuseUtilities, LayerStructureUnit, frmSutraLayersUnit,
  frmImportPointsUnits, ScreenObjectUnit, GoPhastTypes, ValueArrayStorageUnit,
  RbwParser, FastGEO, DataSetUnit, GIS_Functions;

resourcestring
  StrSpecifyQuadrilatera = 'Specify Quadrilateral';
  StrNodeNumber = 'Node number';
  StrX = 'X';
  StrY = 'Y';
  StrElementNumber = 'Element number';
  StrNode1 = 'Node 1';
  StrNode2 = 'Node 2';
  StrNode3 = 'Node 3';
  StrNode4 = 'Node 4';
  StrIgnored = 'Ignored';
  StrZ = 'Z';
  StrNode5 = 'Node 5';
  StrNode6 = 'Node 6';
  StrNode7 = 'Node 7';
  StrNode8 = 'Node 8';

{$R *.dfm}

procedure TfrmSpecifyMesh.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData
end;

procedure TfrmSpecifyMesh.FormCreate(Sender: TObject);
begin
  inherited;
  pgcMeshDesign.ActivePageIndex := 0;
  Set2DLabels;
end;

procedure TfrmSpecifyMesh.frameElementsGridBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  ElCol: TElementColumns;
  Node1: Integer;
  Node2: integer;
  Node3: integer;
  Node4: integer;
begin
  inherited;
  if ARow > 0 then
  begin
    ElCol := TElementColumns(ACol);
    case ElCol of
      ecNode2:
        begin
          if TryStrToInt(frameElements.Grid.Cells[Ord(ecNode1),ARow], Node1)
            and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode2),ARow], Node2) then
          begin
            if Node1 = Node2 then
            begin
              frameElements.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        end;
      ecNode3:
        begin
          if TryStrToInt(frameElements.Grid.Cells[Ord(ecNode1),ARow], Node1)
            and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode2),ARow], Node2)
            and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode3),ARow], Node3) then
          begin
            if (Node3 = Node1) or (Node3 = Node2) then
            begin
              frameElements.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        end;
      ecNode4:
        begin
          if TryStrToInt(frameElements.Grid.Cells[Ord(ecNode1),ARow], Node1)
            and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode2),ARow], Node2)
            and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode3),ARow], Node3)
            and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode4),ARow], Node4) then
          begin
            if (Node4 = Node1) or (Node4 = Node2) or (Node4 = Node3) then
            begin
              frameElements.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        end;
    end;
  end;
end;

procedure TfrmSpecifyMesh.frameElementsseNumberChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameElements.seNumberChange(Sender);
  if rgMeshType.ItemIndex = 0 then
  begin
    frameElements.Grid.BeginUpdate;
    try
      for RowIndex := 1 to frameElements.Grid.RowCount - 1 do
      begin
        frameElements.Grid.Cells[Ord(ecLabel), RowIndex] := IntToStr(RowIndex)
      end;
    finally
      frameElements.Grid.EndUpdate
    end;
  end;
end;

procedure TfrmSpecifyMesh.frameNodesseNumberChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameNodes.seNumberChange(Sender);
  if rgMeshType.ItemIndex = 0 then
  begin
    frameNodes.Grid.BeginUpdate;
    try
      for RowIndex := 1 to frameNodes.Grid.RowCount - 1 do
      begin
        frameNodes.Grid.Cells[Ord(ncLabel), RowIndex] := IntToStr(RowIndex)
      end;
    finally
      frameNodes.Grid.EndUpdate
    end;
  end;
end;

procedure TfrmSpecifyMesh.Get2DMesh;
var
  Mesh: TSutraMesh2D;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
begin
  Mesh := frmGoPhast.PhastModel.SutraMesh.Mesh2D;
  rgMeshType.Enabled := frmGoPhast.PhastModel.SutraMesh.MeshType = mt3D;

  frameNodes.seNumber.AsInteger := Mesh.Nodes.Count;
  frameNodes.Grid.BeginUpdate;
  try
    for NodeIndex := 0 to Mesh.Nodes.Count - 1 do
    begin
      ANode := Mesh.Nodes[NodeIndex];
      frameNodes.Grid.Cells[Ord(ncX), NodeIndex+1] := FloatToStr(ANode.X);
      frameNodes.Grid.Cells[Ord(ncY), NodeIndex+1] := FloatToStr(ANode.Y);
    end;
  finally
    frameNodes.Grid.EndUpdate;
  end;

  frameElements.seNumber.AsInteger := Mesh.Elements.Count;
  frameElements.Grid.BeginUpdate;
  try
    for ElementIndex := 0 to Mesh.Elements.Count - 1 do
    begin
      AnElement := Mesh.Elements[ElementIndex];
      Assert(AnElement.Nodes.Count = 4);
      ANode := AnElement.Nodes[0].Node;
      frameElements.Grid.Cells[Ord(ecNode1), ElementIndex+1] :=
        IntToStr(ANode.Number+1);
      ANode := AnElement.Nodes[1].Node;
      frameElements.Grid.Cells[Ord(ecNode2), ElementIndex+1] :=
        IntToStr(ANode.Number+1);
      ANode := AnElement.Nodes[2].Node;
      frameElements.Grid.Cells[Ord(ecNode3), ElementIndex+1] :=
        IntToStr(ANode.Number+1);
      ANode := AnElement.Nodes[3].Node;
      frameElements.Grid.Cells[Ord(ecNode4), ElementIndex+1] :=
        IntToStr(ANode.Number+1);
    end;
  finally
    frameElements.Grid.EndUpdate;
  end;
end;

procedure TfrmSpecifyMesh.GetData(Kind: TMeshKind);
begin
  FKind := Kind;
  case Kind of
    mkComplete:
      begin
        Get2DMesh;
      end;
    mkQuadrilaterals:
      begin
        Caption := StrSpecifyQuadrilatera;
        GetQuadrilaterals;
        rgMeshType.Enabled := False;
      end;
    else Assert(False);
  end;
end;

procedure TfrmSpecifyMesh.Set2DLabels;
begin
  frameNodes.Grid.Cells[Ord(ncLabel), 0] := StrNodeNumber;
  frameNodes.Grid.Cells[Ord(ncX), 0] := StrX;
  frameNodes.Grid.Cells[Ord(ncY), 0] := StrY;
  frameElements.Grid.Cells[Ord(ecLabel), 0] := StrElementNumber;
  frameElements.Grid.Cells[Ord(ecNode1), 0] := StrNode1;
  frameElements.Grid.Cells[Ord(ecNode2), 0] := StrNode2;
  frameElements.Grid.Cells[Ord(ecNode3), 0] := StrNode3;
  frameElements.Grid.Cells[Ord(ecNode4), 0] := StrNode4;
end;

procedure TfrmSpecifyMesh.GetQuadrilaterals;
var
  Fishnet: TFishnetMeshGenerator;
  NodeIndex: Integer;
  ANode: TFishnetMeshNode;
  ElementIndex: Integer;
  AnElement: TFishnetMeshElement;
  AColumn: TRbwColumn4;
  ColIndex: Integer;
begin
  Fishnet := frmGoPhast.PhastModel.FishnetMeshGenerator;

  frameNodes.seNumber.AsInteger := Fishnet.Nodes.Count;
  frameNodes.Grid.BeginUpdate;
  try
    for NodeIndex := 0 to Fishnet.Nodes.Count - 1 do
    begin
      ANode := Fishnet.Nodes[NodeIndex];
      frameNodes.Grid.Cells[Ord(ncX), NodeIndex+1] := FloatToStr(ANode.X);
      frameNodes.Grid.Cells[Ord(ncY), NodeIndex+1] := FloatToStr(ANode.Y);
    end;
  finally
    frameNodes.Grid.EndUpdate;
  end;

  frameElements.seNumber.AsInteger := Fishnet.Elements.Count;
  frameElements.Grid.ColCount := 7;

  frameElements.Grid.BeginUpdate;
  try
    for ColIndex := Ord(ecCount1) to Ord(ecCount2) do
    begin
      AColumn := frameElements.Grid.Columns[Ord(ColIndex)];
      AColumn.Format := rcf4Integer;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.WordWrapCaptions := True;
    end;
    frameElements.Grid.Cells[Ord(ecCount1),0] := ' Element Count 1';
    frameElements.Grid.Cells[Ord(ecCount2),0] := ' Element Count 2';
    for ElementIndex := 0 to Fishnet.Elements.Count - 1 do
    begin
      AnElement := Fishnet.Elements[ElementIndex];
//      if AnElement.Nodes.Count <= 4 then
//      begin
//        Continue;
//      end;
      Assert(AnElement.Nodes.Count <= 4);
      if AnElement.Nodes.Count > 0 then
      begin
        ANode := AnElement.Nodes[0];
        frameElements.Grid.Cells[Ord(ecNode1), ElementIndex+1] :=
          IntToStr(ANode.Index+1);
      end;
      if AnElement.Nodes.Count > 1 then
      begin
        ANode := AnElement.Nodes[1];
        frameElements.Grid.Cells[Ord(ecNode2), ElementIndex+1] :=
          IntToStr(ANode.Index+1);
      end;
      if AnElement.Nodes.Count > 2 then
      begin
        ANode := AnElement.Nodes[2];
        frameElements.Grid.Cells[Ord(ecNode3), ElementIndex+1] :=
          IntToStr(ANode.Index+1);
      end;
      if AnElement.Nodes.Count > 3 then
      begin
        ANode := AnElement.Nodes[3];
        frameElements.Grid.Cells[Ord(ecNode4), ElementIndex+1] :=
          IntToStr(ANode.Index+1);
      end;

      frameElements.Grid.Cells[Ord(ecCount1), ElementIndex+1] :=
        IntToStr(AnElement.FirstControl.Count);
      frameElements.Grid.Cells[Ord(ecCount2), ElementIndex+1] :=
        IntToStr(AnElement.SecondControl.Count);
    end;
  finally
    frameElements.Grid.EndUpdate;
  end;

end;

procedure TfrmSpecifyMesh.pgcMeshDesignChange(Sender: TObject);
var
  ACol: TRbwColumn4;
  ColIndex: TElementColumns;
begin
  inherited;
  if pgcMeshDesign.ActivePage = tabElements then
  begin
    for ColIndex := ecNode1 to ecNode4 do
    begin
      ACol := frameElements.Grid.Columns[Ord(ecNode1)];
      ACol.Min := 1;
      ACol.Max := frameNodes.seNumber.AsInteger;
      ACol.CheckMin := True;
      ACol.CheckMax := True;
    end;
  end;
end;

procedure TfrmSpecifyMesh.rgMeshTypeClick(Sender: TObject);
begin
  inherited;
  case rgMeshType.ItemIndex of
    0:
      begin
        frameNodes.Grid.ColCount := 3;
        frameElements.Grid.ColCount := 5;
        frameNodes.Grid.FixedCols := 1;
        Set2DLabels;
      end;
    1:
      begin
        frameNodes.Grid.ColCount := 5;
        frameElements.Grid.ColCount := 9;
        ClearGrid(frameNodes.Grid);
        ClearGrid(frameElements.Grid);
        frameNodes.Grid.FixedCols := 0;
        frameElements.Grid.FixedCols := 0;
        Set3DLabels;
      end;
  end;
end;

procedure TfrmSpecifyMesh.Set2DMesh;
var
  Undo: TUndoMoveSutraNodes;
  NodeIndex: Integer;
  X: Extended;
  Mesh: TSutraMesh2D;
  ANode2D: TSutraNode2D;
  RowIndex: Integer;
  ElementIndex: Integer;
  Node1: Integer;
  Node2: Integer;
  Node3: Integer;
  Node4: Integer;
  Y: Extended;
  AnElement: TSutraElement2D;
begin
  Mesh := frmGoPhast.PhastModel.SutraMesh.Mesh2D;

  Undo := TUndoMoveSutraNodes.Create;
  try
    NodeIndex := 0;
    for RowIndex := 1 to frameNodes.Grid.RowCount -1 do
    begin
      if TryStrToFloat(frameNodes.Grid.Cells[Ord(ncX), RowIndex], X)
        and TryStrToFloat(frameNodes.Grid.Cells[Ord(ncY), RowIndex], Y) then
      begin
        if NodeIndex < Mesh.Nodes.Count then
        begin
          ANode2D := Mesh.Nodes[NodeIndex];
        end
        else
        begin
          ANode2D := Mesh.Nodes.Add;
        end;
        ANode2D.X := X;
        ANode2D.Y := Y;
        ANode2D.Number := NodeIndex;
        Inc(NodeIndex);
      end;
    end;
    while Mesh.Nodes.Count > NodeIndex do
    begin
      Mesh.Nodes.Delete(Mesh.Nodes.Count-1);
    end;

    ElementIndex := 0;
    for RowIndex := 1 to frameElements.Grid.RowCount - 1 do
    begin
      if TryStrToInt(frameElements.Grid.Cells[Ord(ecNode1), RowIndex], Node1)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode2), RowIndex], Node2)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode3), RowIndex], Node3)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode4), RowIndex], Node4)
        then
      begin
        Dec(Node1);
        Dec(Node2);
        Dec(Node3);
        Dec(Node4);
        if (Node1 >= 0) and (Node1 < Mesh.Nodes.Count)
          and (Node2 >= 0) and (Node2 < Mesh.Nodes.Count)
          and (Node3 >= 0) and (Node3 < Mesh.Nodes.Count)
          and (Node4 >= 0) and (Node4 < Mesh.Nodes.Count)
          then
        begin
          if (ElementIndex < Mesh.Elements.Count) then
          begin
            AnElement := Mesh.Elements[ElementIndex];
          end
          else
          begin
            AnElement := Mesh.Elements.Add;
            for NodeIndex := 1 to 4 do
            begin
              AnElement.Nodes.Add;
            end;
          end;
          ANode2D := Mesh.Nodes[Node1];
          AnElement.Nodes[0].Node := ANode2D;
          ANode2D := Mesh.Nodes[Node2];
          AnElement.Nodes[1].Node := ANode2D;
          ANode2D := Mesh.Nodes[Node3];
          AnElement.Nodes[2].Node := ANode2D;
          ANode2D := Mesh.Nodes[Node4];
          AnElement.Nodes[3].Node := ANode2D;
          AnElement.ElementNumber := ElementIndex;
          Inc(ElementIndex);
        end;
      end;
    end;
    while Mesh.Elements.Count > ElementIndex do
    begin
      Mesh.Elements.Delete(Mesh.Elements.Count-1);
    end;

    for NodeIndex := 0 to Mesh.Nodes.Count - 1 do
    begin
      ANode2D := Mesh.Nodes[NodeIndex];
      ANode2D.ClearElements;
    end;

    for ElementIndex := 0 to Mesh.Elements.Count - 1 do
    begin
      AnElement :=  Mesh.Elements[ElementIndex];
      for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
      begin
        ANode2D := AnElement.Nodes[NodeIndex].Node;
        ANode2D.AddElement(AnElement);
      end;
    end;

    Mesh.DeleteUnconnectedNodes;
    Mesh.SetCorrectElementOrientation;

    Undo.UpdateNewMesh(frmGoPhast.PhastModel.SutraMesh);
    frmGoPhast.UndoStack.Submit(Undo);
  except
    Undo.Free;
    raise;
  end;
end;

procedure TfrmSpecifyMesh.Set3DLabels;
begin
  frameNodes.Grid.Cells[Ord(n3cNumber), 0] := StrNodeNumber;
  frameNodes.Grid.Cells[Ord(n3cIgnored), 0] := StrIgnored;
  frameNodes.Grid.Cells[Ord(nc3X), 0] := StrX;
  frameNodes.Grid.Cells[Ord(nc3Y), 0] := StrY;
  frameNodes.Grid.Cells[Ord(nc3Z), 0] := StrZ;

  frameElements.Grid.Cells[Ord(e3cNumber), 0] := StrElementNumber;
  frameElements.Grid.Cells[Ord(ec3Node1), 0] := StrNode1;
  frameElements.Grid.Cells[Ord(ec3Node2), 0] := StrNode2;
  frameElements.Grid.Cells[Ord(ec3Node3), 0] := StrNode3;
  frameElements.Grid.Cells[Ord(ec3Node4), 0] := StrNode4;
  frameElements.Grid.Cells[Ord(ec3Node5), 0] := StrNode5;
  frameElements.Grid.Cells[Ord(ec3Node6), 0] := StrNode6;
  frameElements.Grid.Cells[Ord(ec3Node7), 0] := StrNode7;
  frameElements.Grid.Cells[Ord(ec3Node8), 0] := StrNode8;
end;

procedure TfrmSpecifyMesh.Set3DMesh;
var
  NodesQuadTree: TRbwQuadTree;
  ImportNodes: TObjectList<TImportNode2D>;
  RowIndex: Integer;
  NodeRecord: TNodeRecord;
  NodeRecords: TList<TNodeRecord>;
  MinX: Double;
  MaxX: Double;
  MinY: Double;
  MaxY: Double;
  NodeIndex: Integer;
  ImportNode2D: TImportNode2D;
  X: Double;
  Y: Double;
  OtherNodeRecord: TNodeRecord;
  NodeDictionary: TDictionary<Integer,TImportNode2D>;
  MaxNodeLayers: Integer;
  ElementRecord: TElementRecord;
  ElementRecords: TList<TElementRecord>;
  ImportElement2D: TImportElement2D;
  ImportElements: TObjectList<TImportElement2D>;
  ElementDictionary: TDictionary<string, TImportElement2D>;
  ElementIndex: Integer;
  TempImportElement2D: TImportElement2D;
  Mesh: TSutraMesh2D;
  Undo: TUndoMoveSutraNodes;
  Node2D: TSutraNode2D;
  NodeConnector: TDictionary<TImportNode2D, TSutraNode2D>;
  Element2D: TSutraElement2D;
  EpsilonX: double;
  EpsilonY: double;
  LayerStructure: TSutraLayerStructure;
  LayerIndex: Integer;
  ALayer: TSutraLayerGroup;
  UndoLayers: TUndoDefineSutraLayers;
  ScreenObjectList: TList;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ImportedItem: TValueArrayItem;
  APoint: TPoint2D;
  Z: double;
  ADataArray: TDataArray;
  FormulaIndex: Integer;
  UndoImportPoints: TUndoImportPoints;
begin
  Screen.Cursor := crHourGlass;
  frmGoPhast.PhastModel.SutraMesh.BeginUpdate;
  frmGoPhast.CanDraw := False;
  NodesQuadTree := TRbwQuadTree.Create(nil);
  ImportNodes := TObjectList<TImportNode2D>.Create;
  NodeRecords := TList<TNodeRecord>.Create;
  NodeDictionary := TDictionary<Integer,TImportNode2D>.Create;
  ElementRecords := TList<TElementRecord>.Create;
  ImportElements := TObjectList<TImportElement2D>.Create;
  ElementDictionary := TDictionary<string, TImportElement2D>.Create;
  NodeConnector := TDictionary<TImportNode2D, TSutraNode2D>.Create;
  try
    MaxNodeLayers := 1;
    for RowIndex := 1 to frameNodes.Grid.RowCount - 1 do
    begin
      if TryStrToInt(frameNodes.Grid.Cells[Ord(n3cNumber), RowIndex], NodeRecord.Number)
        and TryFortranStrToFloat(frameNodes.Grid.Cells[Ord(nc3X), RowIndex], NodeRecord.X)
        and TryFortranStrToFloat(frameNodes.Grid.Cells[Ord(nc3Y), RowIndex], NodeRecord.Y)
        and TryFortranStrToFloat(frameNodes.Grid.Cells[Ord(nc3Z), RowIndex], NodeRecord.Z) then
      begin
        NodeRecords.Add(NodeRecord);
      end;
    end;
    if NodeRecords.Count = 0 then
    begin
      Exit;
    end;
    NodeRecord := NodeRecords[0];
    MinX := NodeRecord.X;
    MaxX := NodeRecord.X;
    MinY := NodeRecord.Y;
    MaxY := NodeRecord.Y;
    for NodeIndex := 1 to NodeRecords.Count - 1 do
    begin
      NodeRecord := NodeRecords[NodeIndex];
      if NodeRecord.X < MinX then
      begin
        MinX := NodeRecord.X;
      end;
      if NodeRecord.X > MaxX then
      begin
        MaxX := NodeRecord.X;
      end;
      if NodeRecord.Y < MinY then
      begin
        MinY := NodeRecord.Y;
      end;
      if NodeRecord.Y > MaxY then
      begin
        MaxY := NodeRecord.Y;
      end;
    end;
    EpsilonX := Abs(MaxX/1E10);
    EpsilonY := Abs(MaxY/1E10);
    NodesQuadTree.XMax := MaxX;
    NodesQuadTree.XMin := MinX;
    NodesQuadTree.YMax := MaxY;
    NodesQuadTree.YMin := MinY;

    NodeRecord := NodeRecords[0];
    ImportNode2D := TImportNode2D.Create;
    ImportNode2D.Nodes.Add(NodeRecord);
    ImportNodes.Add(ImportNode2D);
    ImportNode2D.Number := ImportNodes.Count;
    NodesQuadTree.AddPoint(NodeRecord.X, NodeRecord.Y, ImportNode2D);
    NodeDictionary.Add(NodeRecord.Number, ImportNode2D);
    for NodeIndex := 1 to NodeRecords.Count - 1 do
    begin
      NodeRecord := NodeRecords[NodeIndex];
      X := NodeRecord.X;
      Y := NodeRecord.Y;
      ImportNode2D := NodesQuadTree.NearestPointsFirstData(NodeRecord.X, NodeRecord.Y);
      OtherNodeRecord := ImportNode2D.Nodes.First;
      if (Abs(NodeRecord.X - OtherNodeRecord.X) > EpsilonX)
        or (Abs(NodeRecord.Y - OtherNodeRecord.Y) > EpsilonY) then
      begin
        ImportNode2D := TImportNode2D.Create;
        ImportNode2D.Nodes.Add(NodeRecord);
        ImportNodes.Add(ImportNode2D);
        ImportNode2D.Number := ImportNodes.Count;
        NodesQuadTree.AddPoint(NodeRecord.X, NodeRecord.Y, ImportNode2D);
      end
      else
      begin
        ImportNode2D.Nodes.Add(NodeRecord);
      end;
      NodeDictionary.Add(NodeRecord.Number, ImportNode2D);
      if ImportNode2D.Nodes.Count > MaxNodeLayers then
      begin
        MaxNodeLayers := ImportNode2D.Nodes.Count;
      end;
    end;
    NodeRecords.Clear;

    for RowIndex := 1 to frameElements.Grid.RowCount - 1 do
    begin
      if TryStrToInt(frameElements.Grid.Cells[Ord(e3cNumber), RowIndex], ElementRecord.Number)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node1), RowIndex], ElementRecord.Nodes[0])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node2), RowIndex], ElementRecord.Nodes[1])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node3), RowIndex], ElementRecord.Nodes[2])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node4), RowIndex], ElementRecord.Nodes[3])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node5), RowIndex], ElementRecord.Nodes[4])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node6), RowIndex], ElementRecord.Nodes[5])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node7), RowIndex], ElementRecord.Nodes[6])
        and TryStrToInt(frameElements.Grid.Cells[Ord(ec3Node8), RowIndex], ElementRecord.Nodes[7])
        then
      begin
        ElementRecords.Add(ElementRecord);
      end;
    end;

    if ElementRecords.Count = 0 then
    begin
      Exit;
    end;

    ElementRecord := ElementRecords[0];
    ImportElement2D := TImportElement2D.Create;
    ImportElements.Add(ImportElement2D);
    ImportElement2D.Elements.Add(ElementRecord);

    for NodeIndex := 0 to 7 do
    begin
      if not NodeDictionary.TryGetValue(ElementRecord.Nodes[NodeIndex], ImportNode2D) then
      begin
        Assert(False);
      end;
      if ImportElement2D.Nodes.IndexOf(ImportNode2D) < 0 then
      begin
        ImportElement2D.Nodes.Add(ImportNode2D);
      end;
    end;
    Assert(ImportElement2D.Nodes.Count = 4);
    ElementDictionary.Add(ImportElement2D.Key, ImportElement2D);

    for ElementIndex := 1 to ElementRecords.Count - 1 do
    begin
      ElementRecord := ElementRecords[ElementIndex];
      TempImportElement2D := TImportElement2D.Create;
//      TempImportElement2D.Elements.Add(ElementRecord);

      for NodeIndex := 0 to 7 do
      begin
        if not NodeDictionary.TryGetValue(ElementRecord.Nodes[NodeIndex], ImportNode2D) then
        begin
          Assert(False);
        end;
        if TempImportElement2D.Nodes.IndexOf(ImportNode2D) < 0 then
        begin
          TempImportElement2D.Nodes.Add(ImportNode2D);
        end;
      end;
      try
        Assert(TempImportElement2D.Nodes.Count = 4);
      except
        TempImportElement2D.Free;
      end;
      if not ElementDictionary.TryGetValue(TempImportElement2D.Key, ImportElement2D) then
      begin
        ImportElement2D := TempImportElement2D;
        ElementDictionary.Add(TempImportElement2D.Key, TempImportElement2D);
        ImportElements.Add(ImportElement2D);
      end
      else
      begin
        TempImportElement2D.Free;
      end;
      ImportElement2D.Elements.Add(ElementRecord);
    end;
    NodeDictionary.Clear;
    ElementRecords.Clear;
    ElementDictionary.Clear;

    Mesh := frmGoPhast.PhastModel.SutraMesh.Mesh2D;

    Undo := TUndoMoveSutraNodes.Create;
    try
      while Mesh.Nodes.Count > ImportNodes.Count do
      begin
        Mesh.Nodes.Delete(Mesh.Nodes.Count-1);
      end;
      while Mesh.Nodes.Count < ImportNodes.Count do
      begin
        Mesh.Nodes.Add;
      end;
      for NodeIndex := 0 to ImportNodes.Count - 1 do
      begin
        NodeRecord := ImportNodes[NodeIndex].Nodes[0];
        Node2D := Mesh.Nodes[NodeIndex];
        Node2D.X := NodeRecord.X;
        Node2D.Y := NodeRecord.Y;
        Node2D.Number := NodeIndex;
        NodeConnector.Add(ImportNodes[NodeIndex], Node2D);
      end;

      while Mesh.Elements.Count > ImportElements.Count do
      begin
        Mesh.Elements.Delete(Mesh.Elements.Count-1);
      end;
      while Mesh.Elements.Count < ImportElements.Count do
      begin
        Mesh.Elements.Add;
      end;

      for NodeIndex := 0 to Mesh.Nodes.Count - 1 do
      begin
        Node2D := Mesh.Nodes[NodeIndex];
        Node2D.ClearElements;
      end;


      for ElementIndex := 0 to ImportElements.Count - 1 do
      begin
        ImportElement2D := ImportElements[ElementIndex];
        Element2D := Mesh.Elements[ElementIndex];
        while Element2D.Nodes.Count < 4 do
        begin
          Element2D.Nodes.Add;
        end;

        ImportNode2D := ImportElement2D.Nodes[0];
        if not NodeConnector.TryGetValue(ImportNode2D, Node2D) then
        begin
          Assert(False);
        end;
        Element2D.Nodes[0].Node := Node2D;

        ImportNode2D := ImportElement2D.Nodes[1];
        if not NodeConnector.TryGetValue(ImportNode2D, Node2D) then
        begin
          Assert(False);
        end;
        Element2D.Nodes[1].Node := Node2D;

        ImportNode2D := ImportElement2D.Nodes[2];
        if not NodeConnector.TryGetValue(ImportNode2D, Node2D) then
        begin
          Assert(False);
        end;
        Element2D.Nodes[2].Node := Node2D;

        ImportNode2D := ImportElement2D.Nodes[3];
        if not NodeConnector.TryGetValue(ImportNode2D, Node2D) then
        begin
          Assert(False);
        end;
        Element2D.Nodes[3].Node := Node2D;

        Element2D.ElementNumber := ElementIndex;
      end;
      NodeConnector.Clear;

      for ElementIndex := 0 to Mesh.Elements.Count - 1 do
      begin
        Element2D :=  Mesh.Elements[ElementIndex];
        for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
        begin
          Node2D := Element2D.Nodes[NodeIndex].Node;
          Node2D.AddElement(Element2D);
        end;
      end;

      Mesh.DeleteUnconnectedNodes;
      Mesh.SetCorrectElementOrientation;

      Undo.UpdateNewMesh(frmGoPhast.PhastModel.SutraMesh);
      frmGoPhast.UndoStack.Submit(Undo);
    except
      Undo.Free;
      raise;
    end;

    LayerStructure:= TSutraLayerStructure.Create(nil);
    try
      LayerStructure.Assign(frmGoPhast.PhastModel.SutraLayerStructure);
      while LayerStructure.Count > MaxNodeLayers do
      begin
        LayerStructure.Last.Free;
      end;
      while LayerStructure.Count < MaxNodeLayers do
      begin
        LayerStructure.Add;
      end;
      for LayerIndex := 1 to LayerStructure.Count - 1 do
      begin
        ALayer := LayerStructure[LayerIndex];
        ALayer.AquiferName := Format('Layer_%d', [LayerIndex]);
        ALayer.GrowthControls.LayerCollection.Clear;
      end;

      UndoLayers := TUndoDefineSutraLayers.Create(LayerStructure);
      frmGoPhast.UndoStack.Submit(UndoLayers);
    finally
      LayerStructure.Free;
    end;



    LayerStructure:= frmGoPhast.PhastModel.SutraLayerStructure;

    frmGoPhast.PhastModel.BeginScreenObjectUpdate;
    try
      UndoImportPoints := TUndoImportPoints.Create;
      ScreenObjectList := TList.Create;
      try
        ScreenObjectList.Capacity := 1;
        AScreenObject :=
          TScreenObject.CreateWithViewDirection(frmGoPhast.PhastModel,
          vdTop, UndoCreateScreenObject, False);
        ScreenObjectList.Add(AScreenObject);
        AScreenObject.Comment := 'Imported on ' + DateTimeToStr(Now);
        AScreenObject.EvaluatedAt := eaNodes;
        AScreenObject.ElevationCount := ecZero;
        AScreenObject.SetValuesOfIntersectedCells := True;
        AScreenObject.Visible := False;

        for NodeIndex := 0 to ImportNodes.Count - 1 do
        begin
          ImportNode2D := ImportNodes[NodeIndex];
          APoint.x := ImportNode2D.Nodes[0].x;
          APoint.y := ImportNode2D.Nodes[0].y;
          AScreenObject.AddPoint(APoint,True);
        end;

        for LayerIndex := 0 to LayerStructure.Count - 1 do
        begin
          ALayer := LayerStructure[LayerIndex];
          ImportedItem := AScreenObject.ImportedValues.Add;
          ImportedItem.Name := 'Imported_' + ALayer.DataArrayName;
          ImportedItem.Values.DataType := rdtDouble;
          ImportedItem.Values.Count := ImportNodes.Count;
          for NodeIndex := 0 to ImportNodes.Count - 1 do
          begin
            ImportNode2D := ImportNodes[NodeIndex];
            if LayerIndex <= ImportNode2D.Nodes.Count then
            begin
              Z := ImportNode2D.Nodes[LayerIndex].Z;
            end
            else
            begin
              Z := 0;
            end;
            ImportedItem.Values.RealValues[NodeIndex] := Z;
          end;
          ADataArray := frmGoPhast.PhastModel.DataArrayManager.
            GetDataSetByName(ALayer.DataArrayName);
          Assert(ADataArray <> nil);
          FormulaIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[FormulaIndex] := rsObjectImportedValuesR
            + '("' + ImportedItem.Name + '")';
          ImportedItem.CacheData;
        end;

        UndoImportPoints.StoreNewScreenObjects(ScreenObjectList);
        frmGoPhast.UndoStack.Submit(UndoImportPoints);

      finally
        ScreenObjectList.Free;
      end;

    finally
      frmGoPhast.PhastModel.EndScreenObjectUpdate;
    end;

  finally
    NodeConnector.Free;
    ElementDictionary.Free;
    ImportElements.Free;
    ElementRecords.Free;
    NodeDictionary.Free;
    NodeRecords.Free;
    ImportNodes.Free;
    NodesQuadTree.Free;
    Screen.Cursor := crDefault;
    frmGoPhast.PhastModel.SutraMesh.EndUpdate;
    frmGoPhast.CanDraw := True;
  end;

  frmGoPhast.RestoreDefault2DView1Click(nil);
end;

procedure TfrmSpecifyMesh.SetData;
begin
  case FKind of
    mkComplete:
      begin
        if rgMeshType.ItemIndex = 0 then
        begin
          Set2DMesh;
        end
        else
        begin
          Set3DMesh;
        end;
      end;
    mkQuadrilaterals:
      begin
        SetQuadrilaterals;
      end;
    else Assert(False);
  end;
end;

procedure TfrmSpecifyMesh.SetQuadrilaterals;
var
  Undo: TUndoFishnetMeshValues;
  NodeIndex: Integer;
  X: Extended;
  RowIndex: Integer;
  ElementIndex: Integer;
  Node1: Integer;
  Node2: Integer;
  Node3: Integer;
  Node4: Integer;
  Y: Extended;
  Fishnet: TFishnetMeshGenerator;
  ANode2D: TFishnetMeshNode;
  AnElement: TFishnetMeshElement;
  Count1: integer;
  Count2: integer;
begin
  Fishnet := frmGoPhast.PhastModel.FishnetMeshGenerator;

  Undo := TUndoFishnetMeshValues.Create;
  try
    NodeIndex := 0;
    for RowIndex := 1 to frameNodes.Grid.RowCount -1 do
    begin
      if TryStrToFloat(frameNodes.Grid.Cells[Ord(ncX), RowIndex], X)
        and TryStrToFloat(frameNodes.Grid.Cells[Ord(ncY), RowIndex], Y) then
      begin
        if NodeIndex < Fishnet.Nodes.Count then
        begin
          ANode2D := Fishnet.Nodes[NodeIndex];
        end
        else
        begin
          ANode2D := Fishnet.Nodes.Add;
        end;
        ANode2D.X := X;
        ANode2D.Y := Y;
        Inc(NodeIndex);
      end;
    end;
    while Fishnet.Nodes.Count > NodeIndex do
    begin
      Fishnet.Nodes.Delete(Fishnet.Nodes.Count-1);
    end;
    for NodeIndex := 0 to Fishnet.Nodes.Count - 1 do
    begin
      ANode2D := Fishnet.Nodes[NodeIndex];
      ANode2D.Elements.Clear;
    end;

    ElementIndex := 0;
    for RowIndex := 1 to frameElements.Grid.RowCount - 1 do
    begin
      if TryStrToInt(frameElements.Grid.Cells[Ord(ecNode1), RowIndex], Node1)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode2), RowIndex], Node2)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode3), RowIndex], Node3)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecNode4), RowIndex], Node4)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecCount1), RowIndex], Count1)
        and TryStrToInt(frameElements.Grid.Cells[Ord(ecCount2), RowIndex], Count2)
        then
      begin
        Dec(Node1);
        Dec(Node2);
        Dec(Node3);
        Dec(Node4);
        if (Node1 >= 0) and (Node1 < Fishnet.Nodes.Count)
          and (Node2 >= 0) and (Node2 < Fishnet.Nodes.Count)
          and (Node3 >= 0) and (Node3 < Fishnet.Nodes.Count)
          and (Node4 >= 0) and (Node4 < Fishnet.Nodes.Count)
          then
        begin
          if (ElementIndex < Fishnet.Elements.Count) then
          begin
            AnElement := Fishnet.Elements[ElementIndex];
            while AnElement.Nodes.Count < 4 do
            begin
              AnElement.Nodes.Add(nil);
            end;
          end
          else
          begin
            AnElement := Fishnet.Elements.Add;
            for NodeIndex := 1 to 4 do
            begin
              AnElement.Nodes.Add(nil);
            end;
          end;
          ANode2D := Fishnet.Nodes[Node1];
          AnElement.Nodes[0] := ANode2D;
          ANode2D.Elements.Add(AnElement);

          ANode2D := Fishnet.Nodes[Node2];
          AnElement.Nodes[1] := ANode2D;
          ANode2D.Elements.Add(AnElement);

          ANode2D := Fishnet.Nodes[Node3];
          AnElement.Nodes[2] := ANode2D;
          ANode2D.Elements.Add(AnElement);

          ANode2D := Fishnet.Nodes[Node4];
          AnElement.Nodes[3] := ANode2D;
          ANode2D.Elements.Add(AnElement);

          AnElement.FirstControl.Count := Count1;
          AnElement.SecondControl.Count := Count2;
          Inc(ElementIndex);
        end;
      end;
    end;
    while Fishnet.Elements.Count > ElementIndex do
    begin
      Fishnet.Elements.Delete(Fishnet.Elements.Count-1);
    end;
    for ElementIndex := 0 to Fishnet.Elements.Count - 1 do
    begin
      AnElement := Fishnet.Elements[ElementIndex];
      Fishnet.UpdateCount1(AnElement);
      Fishnet.UpdateCount2(AnElement);
    end;

    Undo.UpdateNewFishnetMesh;
    frmGoPhast.UndoStack.Submit(Undo);
  except
    Undo.Free;
    raise;
  end;
end;

{ TImportNode2D }

constructor TImportNode2D.Create;
begin
  Nodes := TList<TNodeRecord>.Create;
end;

destructor TImportNode2D.Destroy;
begin
  Nodes.Free;
  inherited;
end;

{ TImportElement2D }

constructor TImportElement2D.Create;
begin
  Nodes := TList<TImportNode2D>.Create;
  Elements := TList<TElementRecord>.Create;
end;

destructor TImportElement2D.Destroy;
begin
  Elements.Free;
  Nodes.Free;
  inherited;
end;

function TImportElement2D.Key: string;
begin
  Assert(Nodes.Count = 4);
  Result := Nodes[0].Number.ToString + '_'
    + Nodes[1].Number.ToString + '_'
    + Nodes[2].Number.ToString + '_'
    + Nodes[3].Number.ToString;
end;

end.
