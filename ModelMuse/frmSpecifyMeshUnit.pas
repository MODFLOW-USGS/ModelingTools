unit frmSpecifyMeshUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, frameGridUnit, GrayTabs;

type
  TMeshKind = (mkComplete, mkQuadrilaterals);
  TNodeColumns = (ncLabel, ncX, ncY);
  TElementColumns = (ecLabel, ecNode1, ecNode2, ecNode3, ecNode4, ecCount1, ecCount2);

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
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure frameNodesseNumberChange(Sender: TObject);
    procedure frameElementsseNumberChange(Sender: TObject);
    procedure frameElementsGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure pgcMeshDesignChange(Sender: TObject);
  private
    FKind: TMeshKind;
    procedure SetData;
    procedure Get2DMesh;
    procedure GetQuadrilaterals;
    procedure Set2DMesh;
    procedure SetQuadrilaterals;
    { Private declarations }
  public
    procedure GetData(Kind: TMeshKind);
    { Public declarations }
  end;


implementation

uses
  frmGoPhastUnit, SutraMeshUnit, UndoItems, FishnetMeshGenerator, RbwDataGrid4;

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

  frameNodes.Grid.Cells[Ord(ncLabel),0] := StrNodeNumber;
  frameNodes.Grid.Cells[Ord(ncX),0] := StrX;
  frameNodes.Grid.Cells[Ord(ncY),0] := StrY;

  frameElements.Grid.Cells[Ord(ecLabel),0] := StrElementNumber;
  frameElements.Grid.Cells[Ord(ecNode1),0] := StrNode1;
  frameElements.Grid.Cells[Ord(ecNode2),0] := StrNode2;
  frameElements.Grid.Cells[Ord(ecNode3),0] := StrNode3;
  frameElements.Grid.Cells[Ord(ecNode4),0] := StrNode4;
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

procedure TfrmSpecifyMesh.frameNodesseNumberChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameNodes.seNumberChange(Sender);
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

procedure TfrmSpecifyMesh.Get2DMesh;
var
  Mesh: TSutraMesh2D;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
begin
  Mesh := frmGoPhast.PhastModel.SutraMesh.Mesh2D;

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
      end;
    else Assert(False);
  end;
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

procedure TfrmSpecifyMesh.SetData;
begin
  case FKind of
    mkComplete:
      begin
        Set2DMesh;
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

end.
