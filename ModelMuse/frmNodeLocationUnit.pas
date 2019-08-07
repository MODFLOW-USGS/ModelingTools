unit frmNodeLocationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  ArgusDataEntry, Buttons, ExtCtrls, SutraMeshUnit, Grids,
  RbwDataGrid4;

type
  TfrmNodeLocation = class(TfrmCustomGoPhast)
    pnlBase: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rdgNodes: TRbwDataGrid4;
    procedure btnOKClick(Sender: TObject);
  private
    FNodes: TSutraNode2D_List;
    procedure SetData;

    { Private declarations }
  public
    procedure GetData(Nodes: TSutraNode2D_List);
    { Public declarations }
  end;

var
  frmNodeLocation: TfrmNodeLocation;

implementation

uses
  UndoItems, frmGoPhastUnit;

resourcestring
  StrNodeNumber = 'Node Number';
  StrX = 'X';
  StrY = 'Y';

{$R *.dfm}

{ TfrmNodeLocation }

procedure TfrmNodeLocation.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmNodeLocation.GetData(Nodes: TSutraNode2D_List);
var
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  Mesh: TSutraMesh3D;
  ANode3D: TSutraNode3D;
  NodeNumber: integer;

begin
  rdgNodes.Cells[0,0] := StrNodeNumber;
  rdgNodes.Cells[1,0] := StrX;
  rdgNodes.Cells[2,0] := StrY;

  Mesh := frmGoPhast.PhastModel.SutraMesh;
  FNodes := Nodes;
  rdgNodes.RowCount := Nodes.Count + 1;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    ANode2D := Nodes[NodeIndex];
    if Mesh.MeshType = mt3D then
    begin
      ANode3D := Mesh.NodeArray[Mesh.SelectedLayer,ANode2D.Number];
      NodeNumber := ANode3D.Number;
    end
    else
    begin
      NodeNumber := ANode2D.Number;
    end;
    rdgNodes.Cells[0,NodeIndex+1] := IntToStr(NodeNumber+1);
    rdgNodes.Cells[1,NodeIndex+1] := FloatToStr(ANode2D.X);
    rdgNodes.Cells[2,NodeIndex+1] := FloatToStr(ANode2D.Y);
  end;
end;

procedure TfrmNodeLocation.SetData;
var
  NodeX, NodeY: Extended;
  Undo: TUndoMoveSutraNodes;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
begin
  begin
    Undo := TUndoMoveSutraNodes.Create;
    try
      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        ANode2D := FNodes[NodeIndex];
        if TryStrToFloat(rdgNodes.Cells[1,NodeIndex+1], NodeX)
          and TryStrToFloat(rdgNodes.Cells[2,NodeIndex+1], NodeY) then
        begin
          ANode2D.X := NodeX;
          ANode2D.Y := NodeY;
        end;
      end;
      Undo.UpdateNewMesh(frmGoPhast.PhastModel.SutraMesh);
      frmGoPhast.UndoStack.Submit(Undo);
    except
      Undo.Free;
      raise;
    end;
  end;

end;

end.
