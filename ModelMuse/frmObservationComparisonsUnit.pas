unit frmObservationComparisonsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, SsButtonEd, RbwStringTreeCombo,
  System.Generics.Collections, VirtualTrees, frameGridUnit, GoPhastTypes,
  ObservationComparisonsUnit, UndoItems, PestObsUnit, ObsInterfaceUnit,
  FluxObservationUnit;

type
  TRefHolder = class(TObject)
    Ref: IObservationItem;
  end;

  TRefHolderObjectList = TObjectList<TRefHolder>;

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
    procedure treecomboInPlaceEditorTreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure treecomboInPlaceEditorCanClose(Sender: TObject;
      var CanClose: Boolean);
  private
    { Private declarations }
    FObsDictionary: TDictionary<string, PVirtualNode>;
    FObsItemDictionary: TDictionary<string, IObservationItem>;
    FObsItemList: TObservationInterfaceList;
    FRefHolders: TRefHolderObjectList;

    FCol: Integer;
    FRow: Integer;
    FCanClose: Boolean;
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
  ModflowLakUnit, ModflowSfrUnit, SutraPestObsUnit, PestObsGroupUnit;

resourcestring
  StrObject0sObse = 'Object: "%0:s"; Observation name: "%1:s".';
  StrObservationName = 'Observation Name (OBSNME)';
  StrObservationValue = 'Observation Value (OBSVAL)' + sLineBreak +  '(OBSVAL1 - OBSVAL2)';
  StrObservationWeight = 'Observation Weight (WEIGHT)';
  StrComment = 'Comment';
  StrFirstObservation = 'First Observation (OBSNME1)';
  StrSecondObservation = 'Second Observation (OBSNME2)';
  StrObservationGroup = 'Observation Group (OBGNME)';

{$R *.dfm}

type
  TObsCompColumns = (occName, occGroup, occObs1, occObs2, occValue, occWeight, occComment);

  TObsTreeItem = class(TObject)
    ObsTypeName: string;
    ScreenObject: TScreenObject;
    ObsCollection: TCustomSutraFluxObservations;
    FluxGroup: TFluxObservationGroup;
    Obs: IObservationItem;
    function Caption: string;
    function Key: string; overload;
    class function Key(ObsTypeName: string; ScreenObject: TObject;
      ObsCollection: TCustomSutraFluxObservations;
      FluxGroup: TFluxObservationGroup;
      Obs: IObservationItem): string; overload;
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
  FObsItemDictionary := TDictionary<string, IObservationItem>.Create;
  FObsItemList:= TObservationInterfaceList.Create;
  FRefHolders := TRefHolderObjectList.Create;

  GetData;
end;

procedure TfrmObservationComparisons.FormDestroy(Sender: TObject);
begin
  FObsItemList.Free;
  FObsItemDictionary.Free;
  FObsDictionary.Free;
  FRefHolders.Free;
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
  ObItem : IObservationItem;
  NodeParent: PVirtualNode;
  RefHolder: TRefHolder;
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
      RefHolder := frameObsComparisons.Grid.Objects[FCol, FRow] as TRefHolder;
      ObItem := RefHolder.Ref;
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
//  Item: TCustomObservationItem;
  ScreenObject: TObject;
  Item: IObservationItem;
  RefHolder: TRefHolder;
begin
  InitializeItemList;
  InitializeInPlaceEditor;
  InitializeGrid;
  InitializeObsItemDictionary;

  ObsComparisons := nil;
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    ObsComparisons := frmGoPhast.PhastModel.Modflow6GlobalObservationComparisons;
  end
  else if frmGoPhast.ModelSelection in Modflow2005Selection then
  begin
    ObsComparisons := frmGoPhast.PhastModel.ModflowGlobalObservationComparisons;
  end
  else if frmGoPhast.ModelSelection in SutraSelection then
  begin
    ObsComparisons := frmGoPhast.PhastModel.SutraGlobalObservationComparisons;
  end
  else
  begin
    Assert(False);
  end;
  frameObsComparisons.seNumber.AsInteger := ObsComparisons.Count;
  for ItemIndex := 0 to ObsComparisons.Count - 1 do
  begin
    ObsItem := ObsComparisons[ItemIndex];
    RowIndex := ItemIndex+1;
    frameObsComparisons.Grid.Cells[Ord(occName), RowIndex] := ObsItem.Name;
    frameObsComparisons.Grid.Cells[Ord(occGroup), RowIndex] := ObsItem.ObservationGroup;
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
      RefHolder := TRefHolder.Create;
      FRefHolders.Add(RefHolder);
      RefHolder.Ref := Item;
      frameObsComparisons.Grid.Objects[Ord(occObs1), RowIndex] := RefHolder;
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
      RefHolder := TRefHolder.Create;
      FRefHolders.Add(RefHolder);
      RefHolder.Ref := Item;
      frameObsComparisons.Grid.Objects[Ord(occObs2), RowIndex] := RefHolder;
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
  ObsGroups: TPestObservationGroups;
  PickList: TStrings;
  GroupIndex: Integer;
begin
  ClearGrid(frameObsComparisons.Grid);
  frameObsComparisons.Grid.ColWidths[Ord(occObs1)] := 200;
  frameObsComparisons.Grid.ColWidths[Ord(occObs2)] := 200;
  frameObsComparisons.Grid.BeginUpdate;
  try
    ObsGroups := frmGoPhast.PhastModel. PestProperties.ObservationGroups;
    PickList := frameObsComparisons.Grid.Columns[Ord(occGroup)].PickList;
    PickList.Clear;
    PickList.Capacity := ObsGroups.Count;
    for GroupIndex := 0 to ObsGroups.Count - 1 do
    begin
      PickList.Add(ObsGroups[GroupIndex].ObsGroupName);
    end;

    frameObsComparisons.Grid.Cells[Ord(occName), 0] := StrObservationName;
    frameObsComparisons.Grid.Cells[Ord(occGroup), 0] := StrObservationGroup;
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
//  AnObs: TCustomObservationItem;
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
  ObsCollection: TCustomSutraFluxObservations;
  AnObs: IObservationItem;
  FluxItem: TCustomFluxObsItem;
  Mf2005FluxObs: TFluxObservation;
  FluxGroup: TFluxObservationGroup;
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
      ObsTreeItem.ObsCollection := nil;
      ObsTreeItem.FluxGroup := nil;
      ANode := treecomboInPlaceEditor.Tree.AddChild(nil, ObsTreeItem);
      FObsDictionary.Add(ObsTypeName, ANode);
    end;

    for ItemIndex := 0 to FObsItemList.Count - 1 do
    begin
      AnObs := FObsItemList[ItemIndex];

      ObsTypeName := AnObs.ObservationType;
      ObsCollection := nil;
      FluxGroup := nil;
      if AnObs is TCustomFluxObsItem then
      begin
        FluxItem := TCustomFluxObsItem(AnObs);
        ObsCollection := FluxItem.Collection as TCustomSutraFluxObservations;
      end
      else if AnObs is TFluxObservation then
      begin
        Mf2005FluxObs := TFluxObservation(AnObs);
        FluxGroup := Mf2005FluxObs.FluxGroup;
      end;

      ParentNodeKey :=
        TObsTreeItem.Key(AnObs.ObservationType, AnObs.ScreenObject,
          ObsCollection, FluxGroup, nil);
      if not FObsDictionary.TryGetValue(ParentNodeKey, ParentNode) then
      begin
        ObsTypeNode := FObsDictionary[AnObs.ObservationType];

        ObsTreeItem := TObsTreeItem.Create;
        ObsTreeItem.Obs := nil;
        ObsTreeItem.ObsTypeName := ObsTypeName;
        ObsTreeItem.ScreenObject := AnObs.ScreenObject as TScreenObject;
        ObsTreeItem.ObsCollection := ObsCollection;
        ObsTreeItem.FluxGroup := FluxGroup;
        ParentNode := treecomboInPlaceEditor.Tree.AddChild(ObsTypeNode, ObsTreeItem);
        FObsDictionary.Add(ParentNodeKey, ParentNode);
      end;

      NodeCaption := TObsTreeItem.Key(AnObs.ObservationType, AnObs.ScreenObject,
        ObsCollection, FluxGroup, AnObs);

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
        ObsTreeItem.ObsCollection := ObsCollection;
        ObsTreeItem.FluxGroup := FluxGroup;
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
begin
  frmGoPhast.PhastModel.FillObsInterfaceItemList(FObsItemList);
end;

procedure TfrmObservationComparisons.InitializeObsItemDictionary;
var
  AnObs: IObservationItem;
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
  Item: IObservationItem;
  InvalidateEvent: TNotifyEvent;
  ExistingComparisons: TGlobalObservationComparisons;
  RefHolder: TRefHolder;
begin
  InvalidateEvent := nil;
  ObsComparisons := TGlobalObservationComparisons.Create(InvalidateEvent);
  try
    for RowIndex := 1 to frameObsComparisons.seNumber.AsInteger do
    begin
      RowOK := True;
      for ColumnIndex := 0 to Ord(occWeight) do
      begin
        if ColumnIndex = Ord(occGroup) then
        begin
          Continue;
        end;
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
        ObsComp.ObservationGroup := frameObsComparisons.Grid.Cells[Ord(occGroup), RowIndex];
        RefHolder := frameObsComparisons.Grid.Objects[Ord(occObs1),RowIndex] as TRefHolder;
        Item := RefHolder.Ref;
        ObsComp.Guid1 := Item.GUID;
        RefHolder := frameObsComparisons.Grid.Objects[Ord(occObs2),RowIndex] as TRefHolder;
        Item := RefHolder.Ref;
        ObsComp.Guid2 := Item.GUID;
        ObsComp.ObservedValue := frameObsComparisons.Grid.RealValue[Ord(occValue),RowIndex];
        ObsComp.Weight := frameObsComparisons.Grid.RealValue[Ord(occWeight),RowIndex];
        ObsComp.Comment := frameObsComparisons.Grid.Cells[Ord(occComment),RowIndex];
      end;
    end;
    ExistingComparisons := nil;
    if frmGoPhast.ModelSelection = msModflow2015 then
    begin
      ExistingComparisons := frmGoPhast.PhastModel.Modflow6GlobalObservationComparisons;
    end
    else if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      ExistingComparisons := frmGoPhast.PhastModel.ModflowGlobalObservationComparisons;
    end
    else if frmGoPhast.ModelSelection in SutraSelection then
    begin
      ExistingComparisons := frmGoPhast.PhastModel.SutraGlobalObservationComparisons;
    end;
    Assert(ExistingComparisons <> nil);
    frmGoPhast.UndoStack.Submit(TUndoGlobalObsComparisons.Create(
      ExistingComparisons, ObsComparisons));
  finally
    ObsComparisons.Free;
  end;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorCanClose(
  Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose := FCanClose;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  MyObject: PObsTreeItem;
  RefHolder: TRefHolder;
begin
  MyObject := Sender.GetNodeData(Node);
  if MyObject <> nil then
  begin
    if MyObject^.Obs <> nil then
    begin
      if MyObject^.ScreenObject <> nil then
      begin
        frameObsComparisons.Grid.Cells[FCol, FRow] := Format('%0:s.%1:s',
          [MyObject^.ScreenObject.Name, MyObject^.Obs.Name]);
      end
      else if MyObject^.ObsCollection <> nil then
      begin
        frameObsComparisons.Grid.Cells[FCol, FRow] := Format('%0:s.%1:s',
          [MyObject^.ObsCollection.ObservationName, MyObject^.Obs.Name]);
      end
      else
      begin
        frameObsComparisons.Grid.Cells[FCol, FRow] := Format('%0:s.%1:s',
          [MyObject^.FluxGroup.ObservationName, MyObject^.Obs.Name]);
      end;
      RefHolder := TRefHolder.Create;
      FRefHolders.Add(RefHolder);
      RefHolder.Ref := MyObject^.Obs;
      frameObsComparisons.Grid.Objects[FCol, FRow] := RefHolder;
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
    Assert((ObsTreeItem^.ScreenObject <> nil)
      or (ObsTreeItem^.ObsCollection <> nil)
      or (ObsTreeItem^.FluxGroup <> nil));
  end;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
  Node: PVirtualNode;
begin
  inherited;
  treecomboInPlaceEditor.Tree.GetHitTestInfoAt(X, Y, False, HitInfo);
  if (hiOnItemLabel in HitInfo.HitPositions) then
  begin
    Node := treecomboInPlaceEditor.Tree.GetNodeAt(X, Y);
    if treecomboInPlaceEditor.Tree.HasChildren[Node]  then
    begin
      treecomboInPlaceEditor.Tree.Expanded[Node] :=
        not treecomboInPlaceEditor.Tree.Expanded[Node];
    end;
    FCanClose := not treecomboInPlaceEditor.Tree.HasChildren[Node];
  end;
//
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
  else if ObsCollection <> nil then
  begin
    Result := ObsCollection.ObservationName;
  end
  else if FluxGroup <> nil then
  begin
    Result := FluxGroup.ObservationName;
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
  ScreenObject: TObject; ObsCollection: TCustomSutraFluxObservations;
  FluxGroup: TFluxObservationGroup;
  Obs: IObservationItem): string;
//var
//  ObsCollection: TCustomSutraFluxObservations;
begin
  result := ObsTypeName;
  if ScreenObject <> nil then
  begin
    result := result + '.' + (ScreenObject as TScreenObject).Name;
  end;
  if ObsCollection <> nil then
  begin
    result := result + '.' + ObsCollection.ObservationName;
  end;
  if FluxGroup <> nil then
  begin
    result := result + '.' + FluxGroup.ObservationName;
  end;
  if Obs <> nil then
  begin
//    if Obs is TCustomFluxObsItem then
//    begin
//      ObsCollection := Obs.Collection as TCustomSutraFluxObservations;
//      result := result + '.' + ObsCollection.ObservationName;
//    end;
    result := result + '.' + Obs.Name;
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


