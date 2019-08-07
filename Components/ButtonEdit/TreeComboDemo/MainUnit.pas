unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SsButtonEd, RbwStringTreeCombo, ExtCtrls, VirtualTrees;

type
  TfrmMain = class(TForm)
    rgSelectionChoice: TRadioGroup;
    RbwStringTreeCombo1: TRbwStringTreeCombo;
    procedure FormCreate(Sender: TObject);
    procedure RbwStringTreeCombo1TreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure RbwStringTreeCombo1TreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure RbwStringTreeCombo1TreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure RbwStringTreeCombo1TreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure RbwStringTreeCombo1TreeFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
type
  TMyObject = class(TObject)
    Caption: string;
    Count: integer;
  end;
  PMyObject = ^TMyObject;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Count: Integer;
  ANode: PVirtualNode;
  Index: Integer;
  MyObject : TMyObject;
begin
  Count := 1;
  MyObject := TMyObject.Create;
  MyObject.Count := Count;
  ANode := RbwStringTreeCombo1.Tree.AddChild(nil, MyObject);
  for Index := 0 to 3 do
  begin
    Inc(Count);
    MyObject := TMyObject.Create;
    MyObject.Count := Count;
    RbwStringTreeCombo1.Tree.AddChild(ANode, MyObject);
  end;
  Inc(Count);
  MyObject := TMyObject.Create;
  MyObject.Count := Count;
  ANode := RbwStringTreeCombo1.Tree.AddChild(nil, MyObject);
  for Index := 0 to 3 do
  begin
    Inc(Count);
    MyObject := TMyObject.Create;
    MyObject.Count := Count;
    RbwStringTreeCombo1.Tree.AddChild(ANode, MyObject);
  end;
  Inc(Count);
  MyObject := TMyObject.Create;
  MyObject.Count := Count;
  ANode := RbwStringTreeCombo1.Tree.AddChild(nil, MyObject);
  for Index := 0 to 3 do
  begin
    Inc(Count);
    MyObject := TMyObject.Create;
    MyObject.Count := Count;
    RbwStringTreeCombo1.Tree.AddChild(ANode, MyObject);
  end;
end;

procedure TfrmMain.RbwStringTreeCombo1TreeFocusChanging(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  if rgSelectionChoice.ItemIndex = 1 then
  begin
    Allowed := not (vsHasChildren in NewNode.States)
  end;
end;

procedure TfrmMain.RbwStringTreeCombo1TreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  MyObject: PMyObject;
begin
  MyObject := Sender.GetNodeData(Node);
  MyObject^.Free;
end;

procedure TfrmMain.RbwStringTreeCombo1TreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TMyObject);
end;

procedure TfrmMain.RbwStringTreeCombo1TreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  NodeData: PMyObject;
begin
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData^.Caption;
end;

procedure TfrmMain.RbwStringTreeCombo1TreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  MyObject: PMyObject;
begin
  MyObject := Sender.GetNodeData(Node);
  MyObject^.Caption := 'This is Node # ' + IntToStr(MyObject^.Count);
end;

end.
