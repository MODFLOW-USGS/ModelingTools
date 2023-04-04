{@name defines classes and procedures that are used to place objects
 in a TTreeView or TVirtualStringTree component
 in a way that reflects their classification.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit ClassificationUnit;

interface

uses SysUtils, StrUtils, Classes, ComCtrls, VirtualTrees, DataSetUnit,
  StdCtrls, PhastDataSets, RbwParser, EdgeDisplayUnit, RbwStringTreeCombo,
  System.Generics.Collections;

Type
  {@name is an abstract base class used in @link(TClassificationList),
  and @link(ClassifyListedObjects) to arrange items in a
  hierarchical classification.
  Descendants include @link(TDataArrayEdit) @link(TVariableEdit),
  @link(TScreenObjectDataEdit) and @link(TDataSetClassification).}
  TClassificationObject = class(TObject)
  public
    function ClassificationName: string; virtual; abstract;
    function FullClassification: string; virtual; abstract;
  end;

  TDummyClassification = class(TClassificationObject)
  private
    FClassificationName: string;
  public
    function ClassificationName: string; override;
    function FullClassification: string; override;
    Constructor Create(const StoredClassification: string);
  end;

  // @name is a list of @link(TClassificationObject)s. @name does not own the
  // @link(TClassificationObject)s it contains.
  TClassificationList = class(TObject)
  private
    // @name is a private TList that stores @link(TClassificationObject)s.
    FList: TList;
    // @name returns @link(FList).Count.
    function GetCount: integer;
    // @name returns @link(FList)[Index].
    function GetItems(Index: integer): TClassificationObject;
    // @name sets @link(FList)[Index].
    procedure SetItems(Index: integer; const Value: TClassificationObject);
  public
    // @name calls @link(FList).Count.
    procedure Add(Item: TClassificationObject);
    // @name calls @link(FList).Count.
    property Count: integer read GetCount;
    // @name restores and sets @link(TClassificationObject)s.
    property Items[Index: integer]: TClassificationObject read GetItems
      write SetItems; default;
    // @name creates @link(FList).
    constructor Create;
    // @name destroys @link(FList).
    Destructor Destroy; override;
    // @name returns @link(FList).IndexOf(Item)
    Function IndexOf(Item: TClassificationObject): integer;
    // @name calls FList.Pack.
    procedure Pack;
  end;

  PClassificationNodeData = ^TClassificationNodeData;
  TClassificationNodeData = record
    ClassificationObject: TClassificationObject;
  end;

  // @name is used to store edited values associated with a @link(TDataArray)
  // in a @link(TScreenObject)  while they are being edited
  // prior to actually changing those values.
  TScreenObjectDataEdit = class(TClassificationObject)
  private
    FDataArray: TDataArray;
    FUsed: TCheckBoxState;
    FFormula: string;
    FInterpValue: TInterpValuesCollection;
    FUsesList: TStringList;
    FVariable: TCustomVariable;
    FExpression: TExpression;
    FShouldUpdateDataSet: boolean;
    FUsedBy: TStringList;
    FNode: TTreeNode;
    procedure SetUsed(const Value: TCheckBoxState);
    procedure SetFormula(const Value: string);
  public
    property DataArray: TDataArray read FDataArray;
    property Used: TCheckBoxState read FUsed write SetUsed;
    property Formula: string read FFormula write SetFormula;
    // @name holds @link(TInterpValuesItem)s for @link(DataArray).
    // Each one in @name corresponds to a different
    // @link(TScreenObject).
    property InterpValue: TInterpValuesCollection read FInterpValue
      write FInterpValue;
    Constructor Create(ListOfScreenObjects: TList; DataArray: TDataArray);
    Destructor Destroy; override;
    property UsesList: TStringList read FUsesList;
    property Variable: TCustomVariable read FVariable write FVariable;
    property Expression: TExpression read FExpression write FExpression;
    property ShouldUpdateDataSet: boolean read FShouldUpdateDataSet;
    property UsedBy: TStringList read FUsedBy;
    procedure Invalidate;
    property Node: TTreeNode read FNode write FNode;
    function ClassificationName: string; override;
    function FullClassification: string; override;
  end;

    {@name fills ClassificationList with the classifications of all
    the @link(TClassificationObject)s in ClassificationObjects
    suitable for storage in
    a TTreeView component. The number of tabs before
    each item indicate parent child relationships.  when the classification
    represents a @link(TClassificationObject) rather than just a parent node,
    the @link(TClassificationObject) is stored in the corresponding Objects
    property of ClassificationList.

    Each member of SpecialObjects is a group of @link(TClassificationObject)s
    that must be classified in a special order that is different from
    alphabetical order.
    }
procedure ClassifyListedObjects(
  const ClassificationList: TStringList; ClassificationObjects: TClassificationList;
  SpecialObjects: array of TClassificationList);

{@name creates nodes in TreeView based on Classifications.
  Classifications is typically filled by @link(ClassifyListedObjects).
  each object in Classifications.Objects must be a @link(TClassificationObject)
  or nil. The Data property of each node will be the corresponding
  @link(TClassificationObject).
  IndentationOffset is the level of indentation of the first node.
  TreeView is the TTTreeView to which nodes are added.
  If any of the @link(TClassificationObject) that are added have
  a @link(TClassificationObject.ClassificationName) that is the same as
  SelectedName, that node will be selected.}
procedure CreateClassifiedNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TTreeView; const SelectedName: string);

procedure CreateClassifiedVirtualNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TVirtualStringTree;
  const SelectedName: string; DummyObjects: TList);

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;

procedure UpdateTreeComboText(SelectedNode: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo);

procedure SelectOnlyLeaves(Node: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo; Sender: TBaseVirtualTree;
  var SelectedNode: PVirtualNode);

type
  TEdgeDisplayEdit = class(TObject)
  private
    function GetDisplayTime: Double;
    procedure SetDisplayTime(const Value: Double);
  public
    Edge: TCustomModflowGridEdgeDisplay;
    DataIndex: integer;
    property DisplayTime: Double read GetDisplayTime write SetDisplayTime;
  end;

  TMnw2ItemID = class(TObject)
    ID: string;
  end;

  TMnw2ItemIDObjectList = TObjectList<TMnw2ItemID>;

  TBoundaryClassification = class(TClassificationObject)
  private
    FDataArray: TDataArray;
    FTimeList: TCustomTimeList;
    FEdgeDisplay: TEdgeDisplayEdit;
    FMnw2ItemID: TMnw2ItemID;
    FName: string;
    function GetClassifiedObject: TObject;
    function GetBoundaryType: TBoundaryType;
  public
    function ClassificationName: string; Override;
    function FullClassification: string; Override;
    Constructor Create(AnObject: TDataArray); overload;
    Constructor Create(AnObject: TCustomTimeList); overload;
    Constructor Create(const Name: string; AnObject: TEdgeDisplayEdit); overload;
    Constructor Create(const Name: string; AnObject: TMnw2ItemID); overload;
    Constructor Create(const Name: string; AnObject: TObject); overload;
    property ClassifiedObject: TObject read GetClassifiedObject;
    property BoundaryType: TBoundaryType read GetBoundaryType;
  end;

  TCanSelectBoundary = function (BoundaryClassification: TBoundaryClassification): Boolean of object;

  TDataSetAllowedEvent = function (DataArray: TDataArray): boolean of object;

  TDataSetClassification = class(TClassificationObject)
  private
    FDataArray: TDataArray;
  public
    function ClassificationName: string; Override;
    function FullClassification: string; Override;
    Constructor Create(ADataArray: TDataArray);
    property DataArray: TDataArray read FDataArray;
  end;

procedure GetNodeCaption(Node: PVirtualNode; var CellText: string;
  Sender: TBaseVirtualTree);

Procedure FillVirtualStringTreeWithDataSets(Tree : TVirtualStringTree;
  ClassificationObjectOwnerList: TList; SelectedDataArray: TDataArray;
  DataSetAllowed: TDataSetAllowedEvent);

procedure FillDataSetLists(HufDataArrays: TClassificationList;
  LayerGroupList: TClassificationList;
  SutraLayerGroupList: TClassificationList;
  ClassificationObjects: TClassificationList;
  ClassificationObjectOwnerList: TList;
  DataSetAllowed: TDataSetAllowedEvent = nil);

procedure FillVirtStrTreeWithBoundaryConditions(
  SelectedDataArray: TDataArray;
  SelectedTimeList: TCustomTimeList;
  SelectedEdgeDisplay: TCustomModflowGridEdgeDisplay;
  LocalBoundaryClassifications: TList; EdgeEdits: TList;
  ATree: TVirtualStringTree;
  CanSelectBoundary: TCanSelectBoundary = nil;
  IncludeMnw2TimeData: Boolean = false);

resourcestring
  StrMnw2PumpingRate = 'Pumping Rate';
  StrMnw2HeadCapacityMultip = 'Head Capacity Multiplier';
  StrMnw2LimitingWaterLevel = 'Limiting Water Level';
  StrMnw2InactivationPumping = 'Inactivation Pumping Rate';
  StrMnw2ReactivationPumping = 'Reactivation Pumping Rate';
resourcestring
  StrBoundaryConditions = 'Boundary Conditions, Observations, and Other Features';



implementation

uses IntListUnit, ScreenObjectUnit, PhastModelUnit, frmGoPhastUnit,
  GoPhastTypes;

var
  Mnw2Objects: TMnw2ItemIDObjectList = nil;

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
const
  NumDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
var
  String1: string;
  String2: string;
  Digits1: string;
  Digits2: string;
  LastChar: Integer;
  Number1: Integer;
  Number2: Integer;
  procedure ExtractDigits(var AString, Digits: string);
  var
    CharIndex: Integer;
    AChar: Char;
  begin
    LastChar := Length(AString)+1;
    for CharIndex := Length(AString) downto 1 do
    begin
      AChar := AString[CharIndex];
      if not CharInSet(AChar, NumDigits) then
      begin
        break;
      end;
      LastChar := CharIndex;
    end;
    Digits := Copy(AString, LastChar, MAXINT);
    AString := Copy(AString, 1, LastChar-1);
  end;
begin
  if Index1 = Index2 then
  begin
    result := 0;
  end
  else
  begin
    String1 := List[Index1];
    String2 := List[Index2];
    ExtractDigits(String1, Digits1);
    ExtractDigits(String2, Digits2);
    result := AnsiCompareText(String1, String2);
    if result = 0 then
    begin
      if Digits1 = '' then
      begin
        result := -1
      end
      else if Digits2 = '' then
      begin
        result := 1
      end
      else
      begin
        Number1 := StrToInt(Digits1);
        Number2 := StrToInt(Digits2);
        result := Number1 - Number2;
      end;
    end;
  end;
end;

procedure ClassifyListedObjects(
  const ClassificationList: TStringList; ClassificationObjects: TClassificationList;
  SpecialObjects: array of TClassificationList);
var
  FullClassificationList: TStringList;
  LocalClassificationList: TStringList;
  TabIndex: Integer;
  CIndex: Integer;
  Classification: string;
  FullClassificaton: string;
  Position: Integer;
  LayerGroupPositions: TIntegerList;
  ClassificationObject: TClassificationObject;
  Index: Integer;
  SortedObjects: TStringList;
  SpecialGroupList: TClassificationList;
  GroupIndex: integer;
  ItemIndex: Integer;
begin
  FullClassificationList := TStringList.Create;
  LocalClassificationList := TStringList.Create;
  SortedObjects := TStringList.Create;
  try
    for Index := 0 to ClassificationObjects.Count - 1 do
    begin
      ClassificationObject := ClassificationObjects[Index];
      SortedObjects.AddObject(
        ClassificationObject.FullClassification + '|'
        + ClassificationObject.ClassificationName, ClassificationObject);
    end;
    SortedObjects.CustomSort(CompareStrings);

    for GroupIndex := 0 to Length(SpecialObjects)-1 do
    begin
      SpecialGroupList := SpecialObjects[GroupIndex];
      if (SpecialGroupList.Count > 0) then
      begin
        LayerGroupPositions := TIntegerList.Create;
        try
          for Index := 0 to SpecialGroupList.Count - 1 do
          begin
            ClassificationObject := SpecialGroupList[Index];
            if ClassificationObject <> nil then
            begin
              Position := SortedObjects.IndexOfObject(ClassificationObject);
              Assert(Position >= 0);
              LayerGroupPositions.Add(Position);
//            end
//            else
//            begin
//              LayerGroupPositions.Add(-1);
            end;
          end;
          LayerGroupPositions.Sort;
          ItemIndex := 0;
          for Index := 0 to SpecialGroupList.Count - 1 do
          begin
            ClassificationObject := SpecialGroupList[Index];
            if ClassificationObject <> nil then
            begin
              Position := LayerGroupPositions[ItemIndex];
              SortedObjects[Position] :=
                ClassificationObject.FullClassification + '|'
                + ClassificationObject.ClassificationName;
              SortedObjects.Objects[Position] := ClassificationObject;
              Inc(ItemIndex);
            end;
          end;
        finally
          LayerGroupPositions.Free;
        end;
      end;
    end;

    for Index := 0 to SortedObjects.Count - 1 do
    begin
      ClassificationObject := SortedObjects.Objects[Index]
        as TClassificationObject;
      FullClassificaton := ClassificationObject.FullClassification;
      FullClassificaton := ReplaceStr(FullClassificaton, '|', ''#13'');
      LocalClassificationList.Text := FullClassificaton;
      Classification := '';
      FullClassificaton := '';
      for CIndex := 0 to LocalClassificationList.Count - 1 do
      begin
        Classification := LocalClassificationList[CIndex];
        if CIndex = 0 then
        begin
          FullClassificaton := Classification;
        end
        else
        begin
          FullClassificaton := FullClassificaton + '|' + Classification;
        end;
        for TabIndex := 0 to CIndex - 1 do
        begin
          Classification := ''#9'' + Classification;
        end;
        if FullClassificationList.IndexOf(FullClassificaton) < 0 then
        begin
          ClassificationList.Add(Classification);
          FullClassificationList.Add(FullClassificaton);
        end;
        if CIndex = LocalClassificationList.Count - 1 then
        begin
          Classification := ClassificationObject.ClassificationName;
          for TabIndex := 0 to CIndex do
          begin
            Classification := ''#9'' + Classification;
          end;
          ClassificationList.AddObject(Classification, ClassificationObject);
        end;
      end;
    end;
  finally
    SortedObjects.Free;
    LocalClassificationList.Free;
    FullClassificationList.Free;
  end;
end;

procedure CreateClassifiedNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TTreeView; const SelectedName: string);
var
  Node: TTreeNode;
  ClassifiedObject: TClassificationObject;
  ParentNode: TTreeNode;
  Indentation: Integer;
  TabCount: Integer;
  Classification: string;
  LocalIndex: Integer;
  NodeList: TList;
begin
  NodeList := TList.Create;
  try
    for LocalIndex := 0 to Classifications.Count - 1 do
    begin
      Classification := Classifications[LocalIndex];
      Assert(Length(Classification) > 0);
      TabCount := Length(Classification) - Length(Trim(Classification));
      Classification := Trim(Classification);
      Indentation := TabCount - IndentationOffset;
      Assert(Indentation >= 0);
      if NodeList.Count > Indentation then
      begin
        NodeList.Count := Indentation;
      end;
      if NodeList.Count > 0 then
      begin
        ParentNode := NodeList.Last;
      end
      else
      begin
        ParentNode := nil;
      end;
      ClassifiedObject := Classifications.Objects[LocalIndex]
        as TClassificationObject;
      if ClassifiedObject = nil then
      begin
        Node := TreeView.Items.AddChild(ParentNode, Classification);
        NodeList.Add(Node);
      end
      else
      begin
        Node := TreeView.Items.AddChildObject(ParentNode,
          ClassifiedObject.ClassificationName, ClassifiedObject);
        if ClassifiedObject.ClassificationName = SelectedName then
        begin
          TreeView.Selected := Node;
        end;
      end;
    end;
  finally
    NodeList.Free;
  end;
end;

procedure CreateClassifiedVirtualNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TVirtualStringTree;
  const SelectedName: string; DummyObjects: TList);
var
  ClassifiedObject: TClassificationObject;
  ParentNode: PVirtualNode;
  Indentation: Integer;
  TabCount: Integer;
  Classification: string;
  LocalIndex: Integer;
  NodeList: TList;
  Dummy : TDummyClassification;
  Node: PVirtualNode;
  NodeData: PClassificationNodeData;
  SelectParentNode: PVirtualNode;
begin
  NodeList := TList.Create;
  try
    for LocalIndex := 0 to Classifications.Count - 1 do
    begin
      Classification := Classifications[LocalIndex];
      Assert(Length(Classification) > 0);
      TabCount := Length(Classification) - Length(Trim(Classification));
      Classification := Trim(Classification);
      Indentation := TabCount - IndentationOffset;
      Assert(Indentation >= 0);
      if NodeList.Count > Indentation then
      begin
        NodeList.Count := Indentation;
      end;
      if NodeList.Count > 0 then
      begin
        ParentNode := NodeList.Last;
      end
      else
      begin
        ParentNode := nil;
      end;
      ClassifiedObject := Classifications.Objects[LocalIndex]
        as TClassificationObject;
      if ClassifiedObject = nil then
      begin
        Dummy := TDummyClassification.Create(Classification);
        DummyObjects.Add(Dummy);
        Node := TreeView.AddChild(ParentNode);
        NodeList.Add(Node);
        NodeData := TreeView.GetNodeData(Node);
        NodeData.ClassificationObject := Dummy;
      end
      else
      begin
        Node := TreeView.AddChild(ParentNode);
        NodeData := TreeView.GetNodeData(Node);
        NodeData.ClassificationObject := ClassifiedObject;
        TreeView.Selected[Node] :=
          ClassifiedObject.ClassificationName = SelectedName ;
        if TreeView.Selected[Node] then
        begin
          SelectParentNode := TreeView.NodeParent[Node];
          while SelectParentNode <> nil do
          begin
            if NodeList.IndexOf(SelectParentNode) >= 0 then
            begin
              TreeView.Expanded[SelectParentNode] := True;
              SelectParentNode := TreeView.NodeParent[SelectParentNode];
            end
            else
            begin
              break;
            end;
          end;
        end;
      end;
    end;
  finally
    NodeList.Free;
  end;
end;


{ TClassificationList }

procedure TClassificationList.Add(Item: TClassificationObject);
begin
  FList.Add(Item);
end;

constructor TClassificationList.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TClassificationList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TClassificationList.GetCount: integer;
begin
  result := FList.Count;
end;

function TClassificationList.GetItems(Index: integer): TClassificationObject;
begin
  result := FList[Index];
end;

function TClassificationList.IndexOf(Item: TClassificationObject): integer;
begin
  result := FList.IndexOf(Item);
end;

procedure TClassificationList.Pack;
begin
  FList.Pack;
end;

procedure TClassificationList.SetItems(Index: integer;
  const Value: TClassificationObject);
begin
  FList[Index] := Value;
end;

{ TDummyClassification }

function TDummyClassification.ClassificationName: string;
begin
  result := FClassificationName;
end;

constructor TDummyClassification.Create(const StoredClassification: string);
begin
  inherited Create;
  FClassificationName := StoredClassification;
end;

function TDummyClassification.FullClassification: string;
begin
  result := '';
end;

{ TClassificationObject }

{ TScreenObjectDataEdit }

function TScreenObjectDataEdit.ClassificationName: string;
begin
  Assert(DataArray <> nil);
  result := DataArray.DisplayName;
end;

constructor TScreenObjectDataEdit.Create(ListOfScreenObjects: TList;
  DataArray: TDataArray);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  FirstUsed: Boolean;
  FirstFormula: Boolean;
  NewUsed: TCheckBoxState;
  InterpItem: TInterpValuesItem;
  DataArrayUsed: boolean;
  DataArrayPosition: integer;
  NewFormula: string;
begin
  FUsesList := TStringList.Create;
  FUsesList.Sorted := True;
  FUsesList.Duplicates := dupIgnore;

  FUsedBy := TStringList.Create;
  FUsedBy.Sorted := True;
  FUsedBy.Duplicates := dupIgnore;

  FDataArray := DataArray;
  Assert(DataArray <> nil);
  if DataArray is TCustomPhastDataSet then
  begin
    FInterpValue := TInterpValuesCollection.Create(nil);
  end
  else
  begin
    FInterpValue := nil;
  end;
  FUsed := cbUnchecked;
  FFormula := '';
  FirstUsed := True;
  FirstFormula := True;
  for Index := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[Index];
    if DataArray is TCustomPhastDataSet then
    begin
      InterpItem := FInterpValue.Add as TInterpValuesItem;
    end
    else
    begin
      InterpItem := nil;
    end;
    DataArrayPosition := ScreenObject.IndexOfDataSet(DataArray);
    DataArrayUsed := DataArrayPosition >= 0;
    if FirstUsed then
    begin
      if DataArrayUsed then
      begin
        FUsed := cbChecked;
      end
      else
      begin
        FUsed := cbUnChecked;
      end;
      FirstUsed := False;
    end
    else
    begin
      if DataArrayUsed then
      begin
        NewUsed := cbChecked;
      end
      else
      begin
        NewUsed := cbUnChecked;
      end;
      if FUsed <> NewUsed then
      begin
        FUsed := cbGrayed;
      end;
    end;
    if DataArrayUsed then
    begin
      if FirstFormula then
      begin
        FFormula := ScreenObject.DataSetFormulas[DataArrayPosition];
      end
      else
      begin
        NewFormula := ScreenObject.DataSetFormulas[DataArrayPosition];
        if FFormula <> NewFormula then
        begin
          FFormula := '';
        end;
      end;
      if DataArray is TCustomPhastDataSet then
      begin
        ScreenObject.InterpValues.ItemOfDataSet[DataArray];
        InterpItem.Assign(ScreenObject.InterpValues.ItemOfDataSet[DataArray]);
      end;
      FirstFormula := False;
    end
    else
    begin
      if DataArray is TCustomPhastDataSet then
      begin
        InterpItem.Values.Assign(DataArray);
      end;
    end;
  end;
end;

destructor TScreenObjectDataEdit.Destroy;
begin
  FInterpValue.Free;
  FUsesList.Free;
  FUsedBy.Free;
  inherited;
end;

function TScreenObjectDataEdit.FullClassification: string;
begin
  Assert(DataArray <> nil);
  result := DataArray.FullClassification;
end;

procedure TScreenObjectDataEdit.Invalidate;
begin
  FShouldUpdateDataSet := True;
end;

procedure TScreenObjectDataEdit.SetFormula(const Value: string);
begin
  if FFormula <> Value then
  begin
    FFormula := Value;
    FShouldUpdateDataSet := True;
  end;
end;

procedure TScreenObjectDataEdit.SetUsed(const Value: TCheckBoxState);
begin
  if FUsed <> Value then
  begin
    FUsed := Value;
    FShouldUpdateDataSet := True;
    if Node <> nil then
    begin
      Node.StateIndex := Ord(FUsed) + 1;
    end;
  end;
end;

{ TEdgeDisplayEdit }

function TEdgeDisplayEdit.GetDisplayTime: Double;
begin
  result := Edge.DisplayTime;
end;

procedure TEdgeDisplayEdit.SetDisplayTime(const Value: Double);
begin
  Edge.DisplayTime := Value;
end;

{ TBoundaryClassification }

function TBoundaryClassification.ClassificationName: string;
begin
  if FDataArray <> nil then
  begin
    result := FDataArray.DisplayName;
    Assert(FTimeList = nil);
    Assert(FEdgeDisplay = nil);
  end
  else if FTimeList <> nil then
  begin
    result := FTimeList.Name;
    Assert(FEdgeDisplay = nil);
  end
  else
  begin
    result := FName;
  end;
end;

constructor TBoundaryClassification.Create(AnObject: TCustomTimeList);
begin
  FTimeList := AnObject;
  FDataArray := nil;
  FEdgeDisplay := nil;
  FMnw2ItemID := nil;
end;

constructor TBoundaryClassification.Create(AnObject: TDataArray);
begin
  FDataArray := AnObject;
  FTimeList := nil;
  FEdgeDisplay := nil;
  FMnw2ItemID := nil;
end;

constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TObject);
begin
  FName := Name;
  if AnObject is TDataArray then
  begin
    Create(TDataArray(AnObject));
  end
  else if AnObject is TCustomTimeList then
  begin
    Create(TCustomTimeList(AnObject));
  end
  else if AnObject is TEdgeDisplayEdit then
  begin
    Create(Name, TEdgeDisplayEdit(AnObject));
  end
  else if AnObject is TMnw2ItemID then
  begin
    Create(Name, TMnw2ItemID(AnObject));
  end
  else
  begin
    Assert(False);
  end;
end;

constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TMnw2ItemID);
begin
  FName := Name;
  FEdgeDisplay := nil;
  FDataArray := nil;
  FTimeList := nil;
  FMnw2ItemID := AnObject;
end;

constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TEdgeDisplayEdit);
begin
  FName := Name;
  FEdgeDisplay := AnObject;
  FDataArray := nil;
  FTimeList := nil;
  FMnw2ItemID := nil;
end;

function TBoundaryClassification.FullClassification: string;
begin
  result := ''
end;

function TBoundaryClassification.GetBoundaryType: TBoundaryType;
begin
  if FMnw2ItemID <> nil then
  begin
    result := btMfMnw;
  end
  else if FEdgeDisplay <> nil then
  begin
    result := btMfHfb;
  end
  else
  begin
    result := FTimeList.BoundaryType;
  end;
end;

function TBoundaryClassification.GetClassifiedObject: TObject;
begin
  result := nil;
  if FDataArray <> nil then
  begin
    result := FDataArray;
  end
  else if FTimeList <> nil then
  begin
    result := FTimeList;
  end
  else if FEdgeDisplay <> nil then
  begin
    result := FEdgeDisplay;
  end
  else if FMnw2ItemID <> nil then
  begin
    result := FMnw2ItemID;
    Assert(result <> nil);
  end;
end;

procedure FileMnw2Objects;
var
  Mnw2Item: TMnw2ItemID;
begin
  Mnw2Item := TMnw2ItemID.Create;
  Mnw2Item.ID := StrMnw2PumpingRate;
  Mnw2Objects.Add(Mnw2Item);

  Mnw2Item := TMnw2ItemID.Create;
  Mnw2Item.ID := StrMnw2HeadCapacityMultip;
  Mnw2Objects.Add(Mnw2Item);

  Mnw2Item := TMnw2ItemID.Create;
  Mnw2Item.ID := StrMnw2LimitingWaterLevel;
  Mnw2Objects.Add(Mnw2Item);

  Mnw2Item := TMnw2ItemID.Create;
  Mnw2Item.ID := StrMnw2InactivationPumping;
  Mnw2Objects.Add(Mnw2Item);

  Mnw2Item := TMnw2ItemID.Create;
  Mnw2Item.ID := StrMnw2ReactivationPumping;
  Mnw2Objects.Add(Mnw2Item);
end;

Procedure FillVirtualStringTreeWithDataSets(Tree : TVirtualStringTree;
  ClassificationObjectOwnerList: TList; SelectedDataArray: TDataArray;
  DataSetAllowed: TDataSetAllowedEvent);
var
  ClassificationList: TStringList;
  ClassificationObjects: TClassificationList;
  LayerGroupList: TClassificationList;
  SutraLayerGroupList: TClassificationList;
  SelectedName: string;
  HufDataArrays: TClassificationList;
begin

  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }

  // ClassificationObjectOwnerList will be filled with ClassificationObjects
  // for all the TDataArrays.
  ClassificationObjectOwnerList.Clear;

  // get the name of the selected TDataArray.
  if SelectedDataArray = nil then
  begin
    SelectedName := '';
  end
  else
  begin
    SelectedName := SelectedDataArray.DisplayName;
  end;

  // Create lists used for sorting the nodes.
  HufDataArrays := TClassificationList.Create;
  ClassificationObjects:= TClassificationList.Create;
  LayerGroupList := TClassificationList.Create;
  SutraLayerGroupList := TClassificationList.Create;
  try
    FillDataSetLists(HufDataArrays,
      LayerGroupList, SutraLayerGroupList,
      ClassificationObjects,
      ClassificationObjectOwnerList, DataSetAllowed);

    ClassificationList := TStringList.Create;
    try

      ClassifyListedObjects(ClassificationList, ClassificationObjects,
        [LayerGroupList, SutraLayerGroupList, HufDataArrays]);

      CreateClassifiedVirtualNodes(ClassificationList, 0,
        Tree, SelectedName, ClassificationObjectOwnerList);
    finally
      ClassificationList.Free;
    end;

  finally
    SutraLayerGroupList.Free;
    LayerGroupList.Free;
    ClassificationObjects.Free;
    HufDataArrays.Free;
  end;
end;

procedure FillDataSetLists(HufDataArrays: TClassificationList;
  LayerGroupList: TClassificationList;
  SutraLayerGroupList: TClassificationList;
  ClassificationObjects: TClassificationList;
  ClassificationObjectOwnerList: TList;
  DataSetAllowed: TDataSetAllowedEvent = nil);
var
  Index: Integer;
  DataSet: TDataArray;
  ClassificationObject: TDataSetClassification;
  Position: Integer;
  HydrogeologicUnitNames: TStringList;
  LayerGroupsDataSets: TList;
  SutraLayerGroupsDataSets: TList;
  DataArrayManager: TDataArrayManager;
begin
  LayerGroupsDataSets := TList.Create;
  HydrogeologicUnitNames := TStringList.Create;
  SutraLayerGroupsDataSets := TList.Create;
  try
    // HufDataArrays will be filled with TDataSetClassifications for the
    // TDataArrays used to define HUF data.
    frmGoPhast.PhastModel.HydrogeologicUnits.
      FillDataArrayNames(HydrogeologicUnitNames);
    HydrogeologicUnitNames.CaseSensitive := False;
    for Index := 0 to HydrogeologicUnitNames.Count - 1 do
    begin
      HufDataArrays.Add(nil);
    end;
    // LayerGroupList will be filled with TDataSetClassifications for the
    // TDataArrays used to define the layer geometry.
    frmGoPhast.PhastModel.GetModflowLayerGroupDataSets(LayerGroupsDataSets);
    for Index := 0 to LayerGroupsDataSets.Count - 1 do
    begin
      LayerGroupList.Add(nil);
    end;

    frmGoPhast.PhastModel.GetSutraLayerGroupDataSets(SutraLayerGroupsDataSets);
    for Index := 0 to SutraLayerGroupsDataSets.Count - 1 do
    begin
      SutraLayerGroupList.Add(nil);
    end;

    // DataSetClassificationList will be filled with TDataSetClassifications
    // for the all the TDataArrays
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      if Assigned(DataSetAllowed) and not DataSetAllowed(DataSet) then
      begin
        Continue;
      end;
      ClassificationObject := TDataSetClassification.Create(DataSet);
      ClassificationObjects.Add(ClassificationObject);
      ClassificationObjectOwnerList.Add(ClassificationObject);

      Position := LayerGroupsDataSets.IndexOf(DataSet);
      if Position >= 0 then
      begin
        LayerGroupList[Position] := ClassificationObject;
      end;

      Position := SutraLayerGroupsDataSets.IndexOf(DataSet);
      if Position >= 0 then
      begin
        SutraLayerGroupList[Position] := ClassificationObject;
      end;

      Position := HydrogeologicUnitNames.IndexOf(DataSet.Name);
      if Position >= 0 then
      begin
        HufDataArrays[Position] := ClassificationObject;
      end;
    end;
  finally
    LayerGroupsDataSets.Free;
    HydrogeologicUnitNames.Free;
    SutraLayerGroupsDataSets.Free;
  end;
end;

{ TDataSetClassification }

constructor TDataSetClassification.Create(ADataArray: TDataArray);
begin
  inherited Create;
  FDataArray := ADataArray;
end;

function TDataSetClassification.FullClassification: string;
begin
  result := FDataArray.FullClassification;
end;

function TDataSetClassification.ClassificationName: string;
begin
  result := FDataArray.DisplayName;
end;

procedure FillVirtStrTreeWithBoundaryConditions(
  SelectedDataArray: TDataArray;
  SelectedTimeList: TCustomTimeList;
  SelectedEdgeDisplay: TCustomModflowGridEdgeDisplay;
  LocalBoundaryClassifications: TList; EdgeEdits: TList;
  ATree: TVirtualStringTree;
  CanSelectBoundary: TCanSelectBoundary = nil;
  IncludeMnw2TimeData: Boolean = false);
var
  List: TStringList;
  ClassificationPosition: Integer;
  DataSet: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
  Classifications: TStringList;
  NodeData: PClassificationNodeData;
  RootNode: PVirtualNode;
  ANode: PVirtualNode;
  NextNode: PVirtualNode;
  DummyRootClassification: TDummyClassification;
  ParentNode: PVirtualNode;
  AnObject: TObject;
  VirtualNode: PVirtualNode;
  BounddaryClassification: TBoundaryClassification;
  BoundaryIndex: Integer;
  VirtualClassificationNode: PVirtualNode;
  DummyClassification: TDummyClassification;
  TimeList: TCustomTimeList;
  EdgeEdit: TEdgeDisplayEdit;
  UsedByModel: Boolean;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildTimeList: TCustomTimeList;
  NodeDeleted: Boolean;
  Mnw2Item: TMnw2ItemID;
  Mnw2Index: integer;
begin
  LocalBoundaryClassifications.Clear;
  DummyRootClassification := TDummyClassification.Create(StrBoundaryConditions);
  LocalBoundaryClassifications.Add(DummyRootClassification);
  RootNode := ATree.AddChild(nil);
  NodeData := ATree.GetNodeData(RootNode);
  NodeData.ClassificationObject := DummyRootClassification;
  Classifications := TStringList.Create;
  try
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
      for Index := 0 to DataArrayManager.BoundaryDataSetCount - 1 do
      begin
        DataSet := DataArrayManager.BoundaryDataSets[Index];
        ClassificationPosition := Classifications.IndexOf(
          DataSet.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          Classifications.AddObject(DataSet.Classification, List);
        end
        else
        begin
          List := Classifications.Objects[ClassificationPosition]
            as TStringList;
        end;
        List.AddObject(DataSet.Name, DataSet);
      end;
    end;
    EdgeEdits.Clear;
    if (frmGoPhast.PhastModel.ModelSelection in ModflowSelection)
      and frmGoPhast.PhastModel.HfbIsSelected then
    begin
      List := TStringList.Create;
      Classifications.AddObject('MODFLOW Horizontal Flow Barrier', List);
      for Index := 0 to frmGoPhast.PhastModel.
        HfbDisplayer.RealValueTypeCount - 1 do
      begin
        EdgeEdit := TEdgeDisplayEdit.Create;
        EdgeEdits.Add(EdgeEdit);
        EdgeEdit.DataIndex := Index;
        EdgeEdit.Edge := frmGoPhast.PhastModel.HfbDisplayer;
        List.AddObject(EdgeEdit.Edge.RealDescription[Index], EdgeEdit);
      end;
    end;
    for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
    begin
      TimeList := frmGoPhast.PhastModel.TimeLists[Index];
      UsedByModel := TimeList.UsedByModel;
      if not UsedByModel and frmGoPhast.PhastModel.LgrUsed then
      begin
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          if ChildModel <> nil then
          begin
            ChildTimeList := ChildModel.GetTimeListByName(TimeList.Name);
            if (ChildTimeList <> nil) and ChildTimeList.UsedByModel then
            begin
              UsedByModel := True;
              break;
            end;
          end;
        end;
      end;
      if UsedByModel then
      begin
        ClassificationPosition := Classifications.IndexOf(
          TimeList.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          Classifications.AddObject(TimeList.Classification, List);
        end
        else
        begin
          List := Classifications.Objects[ClassificationPosition]
            as TStringList;
        end;
        List.AddObject(TimeList.Name, TimeList);
      end;
    end;
    if IncludeMnw2TimeData and frmGoPhast.PhastModel.Mnw2IsSelected then
    begin
      ClassificationPosition := Classifications.IndexOf(StrMODFLOWMultinodeWe);
      if ClassificationPosition < 0 then
      begin
        List := TStringList.Create;
        Classifications.AddObject(StrMODFLOWMultinodeWe, List);
      end
      else
      begin
        List := Classifications.Objects[ClassificationPosition]
          as TStringList;
      end;

      for Mnw2Index := 0 to Mnw2Objects.Count - 1 do
      begin
        Mnw2Item := Mnw2Objects[Mnw2Index];
        List.AddObject(Mnw2Item.ID, Mnw2Item);
      end;
    end;
    Classifications.Sort;
    for Index := 0 to Classifications.Count - 1 do
    begin
      DummyClassification := TDummyClassification.Create(Classifications[Index]);
      LocalBoundaryClassifications.Add(DummyClassification);
      VirtualClassificationNode := ATree.AddChild(RootNode);
      NodeData := ATree.GetNodeData(VirtualClassificationNode);
      NodeData.ClassificationObject := DummyClassification;
      List := Classifications.Objects[Index] as TStringList;
      List.Sort;
      for BoundaryIndex := 0 to List.Count - 1 do
      begin
        BounddaryClassification := TBoundaryClassification.Create(
          List[BoundaryIndex], List.Objects[BoundaryIndex]);
        if Assigned(CanSelectBoundary) then
        begin
          if not CanSelectBoundary(BounddaryClassification) then
          begin
            BounddaryClassification.Free;
            Continue;
          end;
        end;
        LocalBoundaryClassifications.Add(BounddaryClassification);
        VirtualNode := ATree.AddChild(VirtualClassificationNode);
        NodeData := ATree.GetNodeData(VirtualNode);
        NodeData.ClassificationObject := BounddaryClassification;
        AnObject := BounddaryClassification.ClassifiedObject;
        if AnObject is TEdgeDisplayEdit then
        begin
          EdgeEdit := TEdgeDisplayEdit(AnObject);
          ATree.Selected[VirtualNode] := (EdgeEdit.Edge = SelectedEdgeDisplay)
            and (EdgeEdit.DataIndex = SelectedEdgeDisplay.DataToPlot);
        end
        else
        begin
          ATree.Selected[VirtualNode] := (AnObject = SelectedDataArray)
            or (AnObject = SelectedTimeList);
        end;
        if ATree.Selected[VirtualNode] then
        begin
          ParentNode := ATree.NodeParent[VirtualNode];
          while ParentNode <> nil do
          begin
            ATree.Expanded[ParentNode] := True;
            if ParentNode = RootNode then
            begin
              break;
            end;
            ParentNode := ATree.NodeParent[ParentNode];
          end;
        end;
      end;
    end;

    repeat
      ANode := RootNode;
      NodeDeleted := False;
      while ANode <> nil do
      begin
        NextNode := ATree.GetNext(ANode);
        NodeData := ATree.GetNodeData(ANode);
        if NodeData.ClassificationObject is TDummyClassification then
        begin
          if not ATree.HasChildren[ANode] then
          begin
            if ANode = RootNode then
            begin
              RootNode := nil;
            end;
            ATree.DeleteNode(ANode);
            NodeDeleted := True;
          end;
        end;
        ANode:= NextNode;
      end;
    until not NodeDeleted;


  finally
    for Index := 0 to Classifications.Count - 1 do
    begin
      Classifications.Objects[Index].Free;
    end;
    Classifications.Free;
  end;
end;

procedure GetNodeCaption(Node: PVirtualNode;
  var CellText: string; Sender: TBaseVirtualTree);
var
  ClassificationNodeData: PClassificationNodeData;
begin
  ClassificationNodeData := Sender.GetNodeData(Node);
  if not Assigned(ClassificationNodeData)
    or not Assigned(ClassificationNodeData.ClassificationObject) then
  begin
    CellText := StrNone;
  end
  else
  begin
    CellText := ClassificationNodeData.ClassificationObject.ClassificationName;
  end;
end;

procedure UpdateTreeComboText(SelectedNode: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo);
var
  CellText: string;
begin
  if TreeCombo.Tree.SelectedCount = 0 then
  begin
    if TreeCombo.Text <> 'none' then
    begin
      TreeCombo.Text := 'none';
    end;
  end
  else
  begin
    GetNodeCaption(SelectedNode, CellText, TreeCombo.Tree);
    TreeCombo.Text := CellText;
  end;
end;

procedure SelectOnlyLeaves(Node: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo; Sender: TBaseVirtualTree;
  var SelectedNode: PVirtualNode); overload;
var
  CellText: string;
begin
  if Sender.Selected[Node] and Sender.HasChildren[Node] then
  begin
    Sender.Selected[Node] := False;
    Sender.FocusedNode := nil;
    if TreeCombo <> nil then
    begin
      TreeCombo.Text := '';
    end;
  end;
  if Sender.Selected[Node] then
  begin
    SelectedNode := Node;
    GetNodeCaption(Node, CellText, Sender);
    if TreeCombo <> nil then
    begin
      TreeCombo.Text := CellText;
    end;
  end
  else
  begin
    SelectedNode := nil;
  end;
end;


initialization

Mnw2Objects := TMnw2ItemIDObjectList.Create;
  FileMnw2Objects;

finalization
  Mnw2Objects.Free;
//  KillApp('ModelMuse Help');
//  GlobalFont.Free;


end.
