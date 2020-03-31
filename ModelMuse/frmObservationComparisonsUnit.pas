unit frmObservationComparisonsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, SsButtonEd, RbwStringTreeCombo,
  System.Generics.Collections, VirtualTrees, frameGridUnit, GoPhastTypes,
  ObservationComparisonsUnit, UndoItems, PestObsUnit;

type
  TfrmObservationComparisons = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    treecomboInPlaceEditor: TRbwStringTreeCombo;
    frameObsComparisons: TframeGrid;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure treecomboInPlaceEditorTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure treecomboInPlaceEditorTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treecomboInPlaceEditorTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure treecomboInPlaceEditorTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure treecomboInPlaceEditorTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure frameObsComparisonsGridColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure frameObsComparisonsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameObsComparisonsGridExit(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FObsDictionary: TDictionary<string, PVirtualNode>;
    FObsItemDictionary: TDictionary<string, TCustomObservationItem>;
    FObsItemList: TObservationList;

    FCol: Integer;
    FRow: Integer;
    procedure GetData;
    procedure SetData;
    procedure InitializeInPlaceEditor;
    procedure InitializeGrid;
    procedure InitializeObsItemDictionary;
    procedure InitializeItemList;
  public
    { Public declarations }
  end;

  TUndoGlobalObsComparisons = class(TCustomUndo)
  private
    FOldComparisons: TGlobalObservationComparisons;
    FExistingComparisons: TGlobalObservationComparisons;
    FNewComparisons: TGlobalObservationComparisons;
  protected
    function Description: string; override;
  public
    constructor Create(const ExistingComparisons: TGlobalObservationComparisons;
      var NewComparisons: TGlobalObservationComparisons);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmObservationComparisons: TfrmObservationComparisons;

implementation

uses
  frmGoPhastUnit, ScreenObjectUnit, ModflowMnw2Unit, frmErrorsAndWarningsUnit,
  ModflowLakUnit, ModflowSfrUnit;

resourcestring
  StrObject0sObse = 'Object: "%0:s"; Observation name: "%1:s".';
  StrObservationName = 'Observation Name';
  StrObservationValue = 'Observation Value';
  StrObservationWeight = 'Observation Weight';
  StrComment = 'Comment';
  StrFirstObservation = 'First Observation';
  StrSecondObservation = 'Second Observation';

{$R *.dfm}

type
  TObsCompColumns = (occName, occObs1, occObs2, occValue, occWeight, occComment);

  TObsTreeItem = class(TObject)
    ObsTypeName: string;
    ScreenObject: TScreenObject;
    Obs: TCustomObservationItem;
    function Caption: string;
    function Key: string; overload;
    class function Key(ObsTypeName: string; ScreenObject: TObject; Obs: TCustomObservationItem): string; overload;
  end;
  PObsTreeItem = ^TObsTreeItem;


{ TfrmObservationComparisons }

procedure TfrmObservationComparisons.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmObservationComparisons.FormCreate(Sender: TObject);
begin
  inherited;
  FObsDictionary := TDictionary<string, PVirtualNode>.Create;
  FObsItemDictionary := TDictionary<string, TCustomObservationItem>.Create;
  FObsItemList:= TObservationList.Create;

  GetData;
end;

procedure TfrmObservationComparisons.FormDestroy(Sender: TObject);
begin
  FObsItemList.Free;
  FObsItemDictionary.Free;
  FObsDictionary.Free;
  inherited;
end;

procedure TfrmObservationComparisons.frameObsComparisonsGridColSize(
  Sender: TObject; ACol, PriorWidth: Integer);
var
  CanSelect: Boolean;
begin
  inherited;
  CanSelect := True;
  frameObsComparisonsGridSelectCell(nil, FCol, FRow, CanSelect);
end;

procedure TfrmObservationComparisons.frameObsComparisonsGridExit(
  Sender: TObject);
begin
  inherited;
  treecomboInPlaceEditor.Visible := False;
end;

procedure TfrmObservationComparisons.frameObsComparisonsGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  CellRect: TRect;
  ANode: PVirtualNode;
//  MyObject: PObsTreeItem;
  ObItem : TCustomObservationItem;
  NodeParent: PVirtualNode;
begin
  inherited;
  if frameObsComparisons.Grid.Drawing then
  begin
    Exit;
  end;
  if (ACol < frameObsComparisons.Grid.FixedCols)
    or (ARow < frameObsComparisons.Grid.FixedRows) then
  begin
    Exit;
  end;
  CanSelect := True;

  if ACol in [Ord(occObs1),Ord(occObs2)] then
  begin
    CanSelect := False;
    FCol := ACol;
    FRow := ARow;
    treecomboInPlaceEditor.Visible := True;
    CellRect := frameObsComparisons.Grid.CellRect(ACol, ARow);
    treecomboInPlaceEditor.Width := CellRect.Right - CellRect.Left;
    treecomboInPlaceEditor.Parent := frameObsComparisons.Grid;
    treecomboInPlaceEditor.Left := CellRect.Left;
    treecomboInPlaceEditor.Top := CellRect.Top;
    if treecomboInPlaceEditor.Tree.FocusedNode <> nil then
    begin
      NodeParent := treecomboInPlaceEditor.Tree.NodeParent[
        treecomboInPlaceEditor.Tree.FocusedNode];
      if NodeParent <> nil then
      begin
        treecomboInPlaceEditor.Tree.Expanded[NodeParent]:= False;
        NodeParent := treecomboInPlaceEditor.Tree.NodeParent[NodeParent];
        if NodeParent <> nil then
        begin
          treecomboInPlaceEditor.Tree.Expanded[NodeParent]:= False;
        end;
      end;
    end;
    if frameObsComparisons.Grid.Objects[FCol, FRow] <> nil then
    begin
      ObItem := frameObsComparisons.Grid.Objects[FCol, FRow] as TCustomObservationItem;
      if FObsDictionary.TryGetValue(ObItem.GUID, ANode) then
      begin
        treecomboInPlaceEditor.Tree.Selected[ANode] := True;
        treecomboInPlaceEditor.Tree.FocusedNode := ANode;
        treecomboInPlaceEditor.Text := frameObsComparisons.Grid.Cells[FCol, FRow];
      end;
    end;
  end
  else
  begin
    treecomboInPlaceEditor.Visible := False;
  end;
end;

procedure TfrmObservationComparisons.GetData;
var
  ItemIndex: Integer;
  ObsComparisons: TGlobalObservationComparisons;
  ObsItem: TGlobalObsComparisonItem;
  RowIndex: Integer;
  Item: TCustomObservationItem;
  ScreenObject: TObject;
begin
  InitializeItemList;
  InitializeInPlaceEditor;
  InitializeGrid;
  InitializeObsItemDictionary;

  ObsComparisons := frmGoPhast.PhastModel.GlobalObservationComparisons;
  frameObsComparisons.seNumber.AsInteger := ObsComparisons.Count;
  for ItemIndex := 0 to ObsComparisons.Count - 1 do
  begin
    ObsItem := ObsComparisons[ItemIndex];
    RowIndex := ItemIndex+1;
    frameObsComparisons.Grid.Cells[Ord(occName), RowIndex] := ObsItem.Name;
    if FObsItemDictionary.TryGetValue(ObsItem.Guid1, Item) then
    begin
      ScreenObject := Item.ScreenObject;
      if ScreenObject <> nil then
      begin
        frameObsComparisons.Grid.Cells[Ord(occObs1), RowIndex] :=
          Format('%0:s.%1:s', [(ScreenObject as TScreenObject).Name, Item.Name]);
      end
      else
      begin
        frameObsComparisons.Grid.Cells[Ord(occObs1), RowIndex] := Item.Name
      end;
      frameObsComparisons.Grid.Objects[Ord(occObs1), RowIndex] := Item;
    end
    else
    begin
      frameObsComparisons.Grid.Cells[Ord(occObs1), RowIndex] := '';
      frameObsComparisons.Grid.Objects[Ord(occObs1), RowIndex] := nil;
    end;

    if FObsItemDictionary.TryGetValue(ObsItem.Guid2, Item) then
    begin
      ScreenObject := Item.ScreenObject;
      if ScreenObject <> nil then
      begin
        frameObsComparisons.Grid.Cells[Ord(occObs2), RowIndex] :=
          Format('%0:s.%1:s', [(ScreenObject as TScreenObject).Name, Item.Name]);
      end
      else
      begin
        frameObsComparisons.Grid.Cells[Ord(occObs2), RowIndex] := Item.Name
      end;
      frameObsComparisons.Grid.Objects[Ord(occObs2), RowIndex] := Item;
    end
    else
    begin
      frameObsComparisons.Grid.Cells[Ord(occObs2), RowIndex] := '';
      frameObsComparisons.Grid.Objects[Ord(occObs2), RowIndex] := nil;
    end;

    frameObsComparisons.Grid.RealValue[Ord(occValue), RowIndex] := ObsItem.ObservedValue;
    frameObsComparisons.Grid.RealValue[Ord(occWeight), RowIndex] := ObsItem.Weight;
    frameObsComparisons.Grid.Cells[Ord(occComment), RowIndex] := ObsItem.Comment;
  end;
end;

procedure TfrmObservationComparisons.InitializeGrid;
var
  ColIndex: Integer;
begin
  ClearGrid(frameObsComparisons.Grid);
  frameObsComparisons.Grid.ColWidths[Ord(occObs1)] := 200;
  frameObsComparisons.Grid.ColWidths[Ord(occObs2)] := 200;
  frameObsComparisons.Grid.BeginUpdate;
  try
    frameObsComparisons.Grid.Cells[Ord(occName), 0] := StrObservationName;
    frameObsComparisons.Grid.Cells[Ord(occObs1), 0] := StrFirstObservation;
    frameObsComparisons.Grid.Cells[Ord(occObs2), 0] := StrSecondObservation;
    frameObsComparisons.Grid.Cells[Ord(occValue), 0] := StrObservationValue;
    frameObsComparisons.Grid.Cells[Ord(occWeight), 0] := StrObservationWeight;
    frameObsComparisons.Grid.Cells[Ord(occComment), 0] := StrComment;
  finally
    frameObsComparisons.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameObsComparisons.Grid.ColCount - 1 do
  begin
    frameObsComparisons.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
end;

procedure TfrmObservationComparisons.InitializeInPlaceEditor;
var
  AnObs: TCustomObservationItem;
  UsedTypes: TStringList;
  TIndex: Integer;
  ObsTypeName: string;
  ObsTreeItem: TObsTreeItem;
  ANode: PVirtualNode;
  ObsTypeNode: PVirtualNode;
  ParentNodeKey: string;
  ParentNode: PVirtualNode;
  NodeCaption: string;
  ObsNode: PVirtualNode;
  ItemIndex: Integer;
begin
  treecomboInPlaceEditor.Tree.BeginUpdate;
  UsedTypes := TStringList.Create;
  try
    UsedTypes.Sorted := True;
    UsedTypes.Duplicates := dupIgnore;

    for ItemIndex := 0 to FObsItemList.Count - 1 do
    begin
      AnObs := FObsItemList[ItemIndex];
      UsedTypes.Add(AnObs.ObservationType);
    end;

    for TIndex := 0 to UsedTypes.Count - 1 do
    begin
      ObsTypeName := UsedTypes[TIndex];
      ObsTreeItem := TObsTreeItem.Create;
      ObsTreeItem.Obs := nil;
      ObsTreeItem.ObsTypeName := ObsTypeName;
      ObsTreeItem.ScreenObject := nil;
      ANode := treecomboInPlaceEditor.Tree.AddChild(nil, ObsTreeItem);
      FObsDictionary.Add(ObsTypeName, ANode);
    end;

    for ItemIndex := 0 to FObsItemList.Count - 1 do
    begin
      AnObs := FObsItemList[ItemIndex];

      ParentNodeKey :=
        TObsTreeItem.Key(AnObs.ObservationType, AnObs.ScreenObject, nil);
      if not FObsDictionary.TryGetValue(ParentNodeKey, ParentNode) then
      begin
        ObsTypeNode := FObsDictionary[AnObs.ObservationType];

        ObsTreeItem := TObsTreeItem.Create;
        ObsTreeItem.Obs := nil;
        ObsTreeItem.ObsTypeName := ObsTypeName;
        ObsTreeItem.ScreenObject := AnObs.ScreenObject as TScreenObject;
        ParentNode := treecomboInPlaceEditor.Tree.AddChild(ObsTypeNode, ObsTreeItem);
        FObsDictionary.Add(ParentNodeKey, ParentNode);
      end;

      NodeCaption := TObsTreeItem.Key(AnObs.ObservationType, AnObs.ScreenObject, AnObs);

      if FObsDictionary.TryGetValue(AnObs.GUID, ObsNode) then
      begin
        Assert(False);
      end
      else
      begin
        ObsTreeItem := TObsTreeItem.Create;
        ObsTreeItem.Obs := AnObs;
        ObsTreeItem.ObsTypeName := ObsTypeName;
        ObsTreeItem.ScreenObject := AnObs.ScreenObject as TScreenObject;
        ANode := treecomboInPlaceEditor.Tree.AddChild(ParentNode, ObsTreeItem);
        FObsDictionary.Add(AnObs.GUID, ANode);
      end;
    end;
  finally
    UsedTypes.Free;
    treecomboInPlaceEditor.Tree.EndUpdate;
  end;
end;

procedure TfrmObservationComparisons.InitializeItemList;
//var
//  ObjectIndex: Integer;
//  AScreenObject: TScreenObject;
//  Mnw2Observations: TMnw2Observations;
//  ObsIndex: Integer;
//  AnObs: TCustomObservationItem;
//  LakObservations: TLakeObservations;
//  SfrObservations: TSfrObservations;
begin
  frmGoPhast.PhastModel.FileObsItemList(FObsItemList);

//  for ObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
//  begin
//    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ObjectIndex];
//    if AScreenObject.Deleted then
//    begin
//      Continue;
//    end;
//
//    if (frmGoPhast.ModelSelection in ModflowSelection) then
//    begin
//      if frmGoPhast.ModelSelection <> msModflow2015 then
//      begin
//        if frmGoPhast.PhastModel.ModflowPackages.Mnw2Package.IsSelected then
//        begin
//          if (AScreenObject.ModflowMnw2Boundary <> nil)
//            and AScreenObject.ModflowMnw2Boundary.Used
//            and (AScreenObject.ModflowMnw2Boundary.Observations.Count > 0) then
//          begin
//            Mnw2Observations := AScreenObject.ModflowMnw2Boundary.Observations;
//            for ObsIndex := 0 to Mnw2Observations.Count - 1 do
//            begin
//              AnObs := Mnw2Observations[ObsIndex];
//              FObsItemList.Add(AnObs);
//            end;
//          end;
//        end;
//
//        if frmGoPhast.PhastModel.ModflowPackages.LakPackage.IsSelected then
//        begin
//          if (AScreenObject.ModflowLakBoundary <> nil)
//            and AScreenObject.ModflowLakBoundary.Used
//            and (AScreenObject.ModflowLakBoundary.Observations.Count > 0) then
//          begin
//            LakObservations := AScreenObject.ModflowLakBoundary.Observations;
//            for ObsIndex := 0 to LakObservations.Count - 1 do
//            begin
//              AnObs := LakObservations[ObsIndex];
//              FObsItemList.Add(AnObs);
//            end;
//          end;
//        end;
//
//        if frmGoPhast.PhastModel.ModflowPackages.SfrPackage.IsSelected then
//        begin
//          if (AScreenObject.ModflowSfrBoundary <> nil)
//            and AScreenObject.ModflowSfrBoundary.Used
//            and (AScreenObject.ModflowSfrBoundary.Observations.Count > 0) then
//          begin
//            SfrObservations := AScreenObject.ModflowSfrBoundary.Observations;
//            for ObsIndex := 0 to SfrObservations.Count - 1 do
//            begin
//              AnObs := SfrObservations[ObsIndex];
//              FObsItemList.Add(AnObs);
//            end;
//          end;
//        end;
//
//      end;
//    end;
//
//  end;

end;

procedure TfrmObservationComparisons.InitializeObsItemDictionary;
var
  AnObs: TCustomObservationItem;
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to FObsItemList.Count - 1 do
  begin
    AnObs := FObsItemList[ItemIndex];
    FObsItemDictionary.Add(AnObs.GUID, AnObs);
  end;
end;

procedure TfrmObservationComparisons.SetData;
var
  ObsComparisons: TGlobalObservationComparisons;
  RowIndex: Integer;
  RowOK: Boolean;
  ColumnIndex: Integer;
  ObsComp: TGlobalObsComparisonItem;
  Item: TCustomObservationItem;
  InvalidateEvent: TNotifyEvent;
begin
  InvalidateEvent := nil;
  ObsComparisons := TGlobalObservationComparisons.Create(InvalidateEvent);
  try
    for RowIndex := 1 to frameObsComparisons.seNumber.AsInteger do
    begin
      RowOK := True;
      for ColumnIndex := 0 to Ord(occWeight) do
      begin
        if frameObsComparisons.Grid.Cells[ColumnIndex,RowIndex] = ''  then
        begin
          RowOK := False;
          break;
        end;
      end;
      RowOK := RowOK
        and (frameObsComparisons.Grid.Objects[Ord(occObs1),RowIndex] <> nil)
        and (frameObsComparisons.Grid.Objects[Ord(occObs2),RowIndex] <> nil);
      if RowOK then
      begin
        ObsComp := ObsComparisons.Add;

        ObsComp.Name := frameObsComparisons.Grid.Cells[Ord(occName),RowIndex];
        Item := frameObsComparisons.Grid.Objects[Ord(occObs1),RowIndex] as TCustomObservationItem;
        ObsComp.Guid1 := Item.GUID;
        Item := frameObsComparisons.Grid.Objects[Ord(occObs2),RowIndex] as TCustomObservationItem;
        ObsComp.Guid2 := Item.GUID;
        ObsComp.ObservedValue := frameObsComparisons.Grid.RealValue[Ord(occValue),RowIndex];
        ObsComp.Weight := frameObsComparisons.Grid.RealValue[Ord(occWeight),RowIndex];
        ObsComp.Comment := frameObsComparisons.Grid.Cells[Ord(occComment),RowIndex];
      end;
    end;
    frmGoPhast.UndoStack.Submit(TUndoGlobalObsComparisons.Create(
      frmGoPhast.PhastModel.GlobalObservationComparisons, ObsComparisons));
  finally
    ObsComparisons.Free;
  end;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  MyObject: PObsTreeItem;
begin
  MyObject := Sender.GetNodeData(Node);
  if MyObject <> nil then
  begin
    if MyObject^.Obs <> nil then
    begin
      frameObsComparisons.Grid.Cells[FCol, FRow] := Format('%0:s.%1:s',
        [MyObject^.ScreenObject.Name, MyObject^.Obs.Name]);
      frameObsComparisons.Grid.Objects[FCol, FRow] := MyObject^.Obs;
    end
    else
    begin
      frameObsComparisons.Grid.Cells[FCol, FRow] := '';
      frameObsComparisons.Grid.Objects[FCol, FRow] := nil;
    end;
  end;

end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ObsTreeItem: PObsTreeItem;
begin
  ObsTreeItem := Sender.GetNodeData(Node);
  ObsTreeItem^.Free;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TObsTreeItem);
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData: PObsTreeItem;
begin
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData^.Caption;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  ObsTreeItem: PObsTreeItem;
begin
  ObsTreeItem := Sender.GetNodeData(Node);
  if ObsTreeItem^.Obs <> nil then
  begin
    Assert(ObsTreeItem^.ScreenObject <> nil);
  end;
end;

{ TObsTreeItem }

function TObsTreeItem.Caption: string;
begin
  if Obs <> nil then
  begin
    Result := Obs.Name;
  end
  else if ScreenObject <> nil then
  begin
    Result := ScreenObject.Name;
  end
  else
  begin
    Result := ObsTypeName;
  end;
end;

function TObsTreeItem.Key: string;
begin
  result := ObsTypeName;
  if ScreenObject <> nil then
  begin
    result := result + '.' + ScreenObject.Name;
    if Obs <> nil then
    begin
      result := result + '.' + Obs.Name;
    end;
  end;
end;

class function TObsTreeItem.Key(ObsTypeName: string;
  ScreenObject: TObject; Obs: TCustomObservationItem): string;
begin
  result := ObsTypeName;
  if ScreenObject <> nil then
  begin
    result := result + '.' + (ScreenObject as TScreenObject).Name;
    if Obs <> nil then
    begin
      result := result + '.' + Obs.Name;
    end;
  end;
end;

{ TUndoGlobalObsComparisons }

constructor TUndoGlobalObsComparisons.Create(
  const ExistingComparisons: TGlobalObservationComparisons;
  var NewComparisons: TGlobalObservationComparisons);

var
  InvalidateEvent: TNotifyEvent;
begin
  InvalidateEvent := nil;
  FOldComparisons := TGlobalObservationComparisons.Create(InvalidateEvent);
  FOldComparisons.Assign(ExistingComparisons);
  FExistingComparisons := ExistingComparisons;
  FNewComparisons := NewComparisons;
  NewComparisons := nil;
end;

function TUndoGlobalObsComparisons.Description: string;
begin
  result := 'edit observation comparisons';
end;

destructor TUndoGlobalObsComparisons.Destroy;
begin
  FOldComparisons.Free;
  FNewComparisons.Free;

  inherited;
end;

procedure TUndoGlobalObsComparisons.DoCommand;
begin
  inherited;
  FExistingComparisons.Assign(FNewComparisons);
end;

procedure TUndoGlobalObsComparisons.Undo;
begin
  inherited;
  FExistingComparisons.Assign(FOldComparisons);
end;

end.


