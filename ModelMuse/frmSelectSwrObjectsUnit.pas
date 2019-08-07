unit frmSelectSwrObjectsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls, VirtualTrees, ScreenObjectUnit;

type
  PScreenObjectNodeData = ^TScreenObjectNodeData;
  TScreenObjectNodeData = record
    ScreenObject: TScreenObject;
  end;


  TfrmSelectSwrObjects = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    vstAvailableObjects: TVirtualStringTree;
    btnAddScreenObject: TSpeedButton;
    btnRemoveScreenObject: TSpeedButton;
    vstSelectedObjects: TVirtualStringTree;
    lblAvailable: TLabel;
    lblSelected: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure vstAvailableObjectsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstAvailableObjectsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstAvailableObjectsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstSelectedObjectsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure btnAddScreenObjectClick(Sender: TObject);
    procedure btnRemoveScreenObjectClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    AvailableNames: TStringList;
    SelectedNames: TStringList;
    function OkObject(ScreenObject: TScreenObject): boolean;
    procedure MoveNodes(SourceTree, DestinationTree: TVirtualStringTree;
      SourceNames, DestinationNames: TStringList);
    { Private declarations }
  public

    procedure GetData(ObjectList: string);
    function SelectedObjects: string;
    { Public declarations }
  end;

implementation

uses
  Generics.Collections, frmGoPhastUnit, ModelMuseUtilities;

{$R *.dfm}

{ TfrmSelectSwrObjects }

procedure TfrmSelectSwrObjects.btnAddScreenObjectClick(Sender: TObject);
begin
  inherited;
  MoveNodes(vstAvailableObjects, vstSelectedObjects, AvailableNames, SelectedNames);
end;

procedure TfrmSelectSwrObjects.btnRemoveScreenObjectClick(Sender: TObject);
begin
  inherited;
  MoveNodes(vstSelectedObjects, vstAvailableObjects, SelectedNames, AvailableNames);
end;

procedure TfrmSelectSwrObjects.FormCreate(Sender: TObject);
begin
  inherited;
  AvailableNames := TStringList.Create;
  AvailableNames.CaseSensitive := False;
  SelectedNames := TStringList.Create;
  SelectedNames.CaseSensitive := False;
end;

procedure TfrmSelectSwrObjects.FormDestroy(Sender: TObject);
begin
  inherited;
  SelectedNames.Free;
  AvailableNames.Free;
end;

procedure TfrmSelectSwrObjects.FormResize(Sender: TObject);
begin
  inherited;
  btnAddScreenObject.Left := (ClientWidth - btnAddScreenObject.Width) div 2;
  btnRemoveScreenObject.Left := btnAddScreenObject.Left;
  vstAvailableObjects.Width := btnRemoveScreenObject.Left - vstAvailableObjects.Margins.Left - 8;
  vstSelectedObjects.Width := vstAvailableObjects.Width;
  lblSelected.Left := vstSelectedObjects.Left;
end;

procedure TfrmSelectSwrObjects.GetData(ObjectList: string);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  AName: string;
  ObjectPosition: Integer;
begin
  AvailableNames.Clear;

  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if OkObject(AScreenObject) then
    begin
      AvailableNames.AddObject(AScreenObject.Name, AScreenObject);
    end;
  end;

  AvailableNames.Sorted := True;

  SelectedNames.CommaText := ObjectList;
  SelectedNames.Sorted := True;

  for Index := SelectedNames.Count - 1 downto 0 do
  begin
    AName := SelectedNames[Index];
    ObjectPosition := AvailableNames.IndexOf(AName);
    if ObjectPosition < 0 then
    begin
      SelectedNames.Delete(Index);
    end
    else
    begin
      SelectedNames.Objects[Index] := AvailableNames.Objects[ObjectPosition];
      AvailableNames.Delete(ObjectPosition);
    end;
  end;

  AvailableNames.Sorted := False;
  AvailableNames.CustomSort(CompareNames);
  SelectedNames.Sorted := False;
  SelectedNames.CustomSort(CompareNames);

  vstAvailableObjects.HasChildren[nil] := AvailableNames.Count > 0;
  vstAvailableObjects.ChildCount[nil] := AvailableNames.Count;

  vstSelectedObjects.HasChildren[nil] := SelectedNames.Count > 0;
  vstSelectedObjects.ChildCount[nil] := SelectedNames.Count;
end;

function TfrmSelectSwrObjects.OkObject(ScreenObject: TScreenObject): boolean;
begin
  result := not ScreenObject.Deleted
    and (ScreenObject.ModflowSwrReaches <> nil)
    and ScreenObject.ModflowSwrReaches.Used;
end;

function TfrmSelectSwrObjects.SelectedObjects: string;
begin
  result := SelectedNames.CommaText;
  result := StringReplace(result, ',', ', ', [rfReplaceAll])
end;

procedure TfrmSelectSwrObjects.MoveNodes(SourceTree, DestinationTree: TVirtualStringTree;
  SourceNames, DestinationNames: TStringList);
var
  NodeData: PScreenObjectNodeData;
  NodesToMove: TVTVirtualNodeEnumeration;
  ObjectPosition: Integer;
  ANode: PVirtualNode;
begin
  NodesToMove := SourceTree.SelectedNodes;
  for ANode in NodesToMove do
  begin
    NodeData := SourceTree.GetNodeData(ANode);
    DestinationNames.AddObject(NodeData.ScreenObject.Name, NodeData.ScreenObject);
    ObjectPosition := SourceNames.IndexOf(NodeData.ScreenObject.Name);
    if ObjectPosition >= 0 then
    begin
      SourceNames.Delete(ObjectPosition);
    end;
  end;
  DestinationTree.HasChildren[nil] := DestinationNames.Count > 0;
  DestinationTree.ChildCount[nil] := DestinationNames.Count;
  DestinationTree.ReinitChildren(nil, False);
  SourceTree.HasChildren[nil] := SourceNames.Count > 0;
  SourceTree.ChildCount[nil] := SourceNames.Count;
  SourceTree.ReinitChildren(nil, False);
end;

procedure TfrmSelectSwrObjects.vstAvailableObjectsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TScreenObjectNodeData);

end;

procedure TfrmSelectSwrObjects.vstAvailableObjectsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData: PScreenObjectNodeData;
begin
  inherited;
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData.ScreenObject.Name;
end;

procedure TfrmSelectSwrObjects.vstAvailableObjectsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  NodeData: PScreenObjectNodeData;
begin
  inherited;
  NodeData := Sender.GetNodeData(Node);
  NodeData.ScreenObject := AvailableNames.Objects[Node.Index] as TScreenObject;
end;

procedure TfrmSelectSwrObjects.vstSelectedObjectsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  NodeData: PScreenObjectNodeData;
begin
  inherited;
  NodeData := Sender.GetNodeData(Node);
  NodeData.ScreenObject := SelectedNames.Objects[Node.Index] as TScreenObject;
end;

end.
