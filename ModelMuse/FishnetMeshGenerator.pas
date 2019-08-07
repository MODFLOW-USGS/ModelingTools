unit FishnetMeshGenerator;

interface

uses
  Windows, Generics.Collections, Classes, GoPhastTypes, FastGEO, GR32, ZoomBox2,
  SysUtils, Types, Graphics, GR32_Backends, UndoItems, LayerStructureUnit,
  OrderedCollectionUnit;

type
  TFishnetMeshGenerator = class;
  TFishnetMeshElement = class;
  TFishnetMeshElementCollection = class;
  TFishnetElementList = TList<TFishnetMeshElement>;

  TFishnetMeshNode = class(TPhastCollectionItem)
  private
    FLocation: TPoint2D;
    FElements: TFishnetElementList;
    FSelected: boolean;
    procedure SetLocation(const Value: TPoint2D);
    procedure SetX(const Value: Double);
    procedure SetY(const Value: Double);
    procedure SetSelected(const Value: boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Elements: TFishnetElementList read FElements;
    property Selected: boolean read FSelected write SetSelected;
    function SameLocation(Node: TFishnetMeshNode): boolean;
  published
    property Location: TPoint2D read FLocation write SetLocation;
    property X: Double read FLocation.X write SetX;
    property Y: Double read FLocation.Y write SetY;
  end;

  TFishnetNodeList = TList<TFishnetMeshNode>;

  TFishnetMeshNodeCollection = class(TPhastCollection)
  private
    FGenerator: TFishnetMeshGenerator;
    function GetItems(Index: Integer): TFishnetMeshNode;
    procedure SetItems(Index: Integer; const Value: TFishnetMeshNode);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent; Generator: TFishnetMeshGenerator);
    function Add: TFishnetMeshNode;
    property Items[Index: Integer]: TFishnetMeshNode read GetItems
      write SetItems; default;
  end;

  TMeshControl = class(TGrowthControls)
  private
    procedure SetCount(const Value: Integer);
    function GetCount: Integer;
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure AssignOpposite(Source: TMeshControl);
    function Equals(OtherMeshControl: TMeshControl): Boolean; reintroduce;
    function Opposite(OtherMeshControl: TMeshControl): Boolean;
    property Count: Integer read GetCount write SetCount;
  end;


  TFishnetMeshElement = class(TPhastCollectionItem)
  private
    FNodeNumbers: TIntegerCollection;
    FNodes: TFishnetNodeList;
    FLabelLocation1: TRect;
    FLabelLocation2: TRect;
    FSelected: boolean;
    FCenterLocation1: TPoint;
    FCenterLocation2: TPoint;
    FMarked: boolean;
    FFirstControl: TMeshControl;
    FSecondControl: TMeshControl;
    procedure SetNodeNumbers(const Value: TIntegerCollection);
    function ElCollection: TFishnetMeshElementCollection;
    function GetNodeNumbers: TIntegerCollection;
    procedure DrawEdges(const BitMap: TBitmap32; const ZoomBox: TQRbwZoomBox2);
    procedure DrawNumbers(const BitMap: TBitmap32; const ZoomBox: TQRbwZoomBox2);
    procedure SetSelected(const Value: boolean);
    procedure SetFirstControl(const Value: TMeshControl);
    procedure SetSecondControl(const Value: TMeshControl);
    function GetGenerator: TFishnetMeshGenerator;
    function GetFractions(MeshControls: TMeshControl): TRealArray;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Nodes: TFishnetNodeList read FNodes;
    procedure UpdateNodeNumbers;
    procedure RestoreNodes;
    property LabelLocation1: TRect read FLabelLocation1;
    property LabelLocation2: TRect read FLabelLocation2;
    property Selected: boolean read FSelected write SetSelected;
    function IsInside(APoint: TPoint2D): Boolean;
    function Intersect(Segment: TSegment2D; SkipNodes: TFishnetNodeList): Boolean;
    procedure ComputeNumberCenterLocations(const ZoomBox: TQRbwZoomBox2);
    property CenterLocation1: TPoint read FCenterLocation1;
    property CenterLocation2: TPoint read FCenterLocation2;
    property Generator: TFishnetMeshGenerator read GetGenerator;
    function Fractions1: TRealArray;
    function Fractions2: TRealArray;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    function Model: TBaseModel;
    function ValidElement: boolean;
  published
    property NodeNumbers: TIntegerCollection read GetNodeNumbers
      write SetNodeNumbers;
    property FirstControl: TMeshControl read FFirstControl write SetFirstControl;
    property SecondControl: TMeshControl read FSecondControl write SetSecondControl;
  end;

  TFishnetMeshElementCollection = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FGenerator: TFishnetMeshGenerator;
    function GetItems(Index: Integer): TFishnetMeshElement;
    procedure SetItems(Index: Integer; const Value: TFishnetMeshElement);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TBaseModel; Generator: TFishnetMeshGenerator);
    function Add: TFishnetMeshElement;
    property Items[Index: Integer]: TFishnetMeshElement read GetItems
      write SetItems; default;
    property Generator: TFishnetMeshGenerator read FGenerator;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TFishnetMeshGenerator = class(TGoPhastPersistent)
  private
    FElements: TFishnetMeshElementCollection;
    FNodes: TFishnetMeshNodeCollection;
    FUpdating: boolean;
    FFont: TFont;
    procedure SetElements(const Value: TFishnetMeshElementCollection);
    procedure SetNodes(const Value: TFishnetMeshNodeCollection);
    procedure MarkElements;
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Loaded;
    procedure DrawTop(const BitMap: TBitmap32; const ZoomBox: TQRbwZoomBox2);
    procedure UpdateCount1(Element: TFishnetMeshElement);
    procedure UpdateCount2(Element: TFishnetMeshElement);
    procedure GetCountsFromNeighbors(Element: TFishnetMeshElement);
    procedure EliminateDuplicateNodes;
  published
    property Nodes: TFishnetMeshNodeCollection read FNodes write SetNodes;
    property Elements: TFishnetMeshElementCollection read FElements
      write SetElements;
  end;

  TUndoFishnetMeshValues = class(TCustomUndo)
  private
    FOldFishNetMesh: TFishnetMeshGenerator;
    FNewFishNetMesh: TFishnetMeshGenerator;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
    constructor Create;
    destructor Destroy; override;
    procedure UpdateNewFishnetMesh;
    procedure UpdateOldFishnetMesh;
  end;

  TUndoFishnetMesh = class(TUndoFishnetMeshValues)
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses
  AbstractGridUnit, BigCanvasMethods, {ScreenObjectUnit,} frmGoPhastUnit,
  InteractiveTools, ModelMuseUtilities;

{ TFishnetMeshNode }

procedure TFishnetMeshNode.Assign(Source: TPersistent);
begin
  if Source is TFishnetMeshNode then
  begin
    Location := TFishnetMeshNode(Source).Location;
  end
  else
  begin
    inherited;
  end;
end;

constructor TFishnetMeshNode.Create(Collection: TCollection);
begin
  inherited;
  FElements:= TFishnetElementList.Create;
end;

destructor TFishnetMeshNode.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TFishnetMeshNode.SameLocation(Node: TFishnetMeshNode): boolean;
begin
  Result := (X = Node.X) and (Y = Node.Y);
end;

procedure TFishnetMeshNode.SetLocation(const Value: TPoint2D);
begin
  X := Value.x;
  Y := Value.y;
end;

procedure TFishnetMeshNode.SetSelected(const Value: boolean);
begin
  FSelected := Value;
end;

procedure TFishnetMeshNode.SetX(const Value: Double);
begin
  SetRealProperty(FLocation.X, Value);
end;

procedure TFishnetMeshNode.SetY(const Value: Double);
begin
  SetRealProperty(FLocation.Y, Value);
end;

{ TFishnetMeshElement }

procedure TFishnetMeshElement.Assign(Source: TPersistent);
var
  SourceItem: TFishnetMeshElement;
begin
  if Source is TFishnetMeshElement then
  begin
    SourceItem := TFishnetMeshElement(Source);
    NodeNumbers := SourceItem.NodeNumbers;
    FirstControl := SourceItem.FirstControl;
    SecondControl := SourceItem.SecondControl;
  end
  else
  begin
    inherited;
  end;
end;

constructor TFishnetMeshElement.Create(Collection: TCollection);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.Invalidate;
  end;
  inherited;
  FNodeNumbers := TIntegerCollection.Create(OnInvalidateModelEvent);
  FNodes := TFishnetNodeList.Create;
  FFirstControl := TMeshControl.Create(OnInvalidateModelEvent);
  FSecondControl := TMeshControl.Create(OnInvalidateModelEvent);
end;

destructor TFishnetMeshElement.Destroy;
begin
  FNodes.Free;
  FNodeNumbers.Free;
  FFirstControl.Free;
  FSecondControl.Free;
  inherited;
end;

procedure TFishnetMeshElement.DrawEdges(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
const
  NodeRadius = 3;
var
  Points: GoPhastTypes.TPointArray;
  LinePoints: GoPhastTypes.TPointArray;
  NodeIndex: Integer;
  ANode: TFishnetMeshNode;
  APoint: TPoint;
  ACanvas: TCanvas;
  L1: TPoint2D;
  L2: TPoint2D;
  L3: TPoint2D;
  L4: TPoint2D;
  F1: TRealArray;
  F2: TRealArray;
  StartPoint: TPoint2D;
  EndPoint: TPoint2D;
  Distance1: TFloat;
  Distance2: TFloat;
  InteriorIndex: Integer;
begin
  if Nodes.Count > 0 then
  begin
    SetLength(Points, Nodes.Count+1);
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex];
      Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, ANode.Location);
    end;
    Points[Nodes.Count] := Points[0];
    DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
      Points, True);
  end;

  if Nodes.Count = 4 then
  begin
    SetLength(LinePoints, 2);
    L1 := Nodes[0].Location;
    L2 := Nodes[1].Location;
    L3 := Nodes[2].Location;
    L4 := Nodes[3].Location;
    Distance1 := Distance(L1, L2);
    Distance2 := Distance(L3, L4);
    F1 := Fractions1;
    for InteriorIndex := 0 to Length(F1) - 1 do
    begin
      if (Distance(L1,L2) > 0) and (Distance(L4,L3) > 0) then
      begin
        StartPoint := ProjectPoint(L1,L2, Distance1 * F1[InteriorIndex]);
        EndPoint := ProjectPoint(L4,L3, Distance2 * F1[InteriorIndex]);
        LinePoints[0] := ConvertTop2D_Point(ZoomBox, StartPoint);
        LinePoints[1] := ConvertTop2D_Point(ZoomBox, EndPoint);
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          LinePoints, True, True);
      end;
    end;
    Distance1 := Distance(L2, L3);
    Distance2 := Distance(L4, L1);
    F2 := Fractions2;
    for InteriorIndex := 0 to Length(F2) - 1 do
    begin
      if (Distance(L2,L3) > 0) and (Distance(L1,L4) > 0) then
      begin
        StartPoint := ProjectPoint(L2,L3, Distance1 * F2[InteriorIndex]);
        EndPoint := ProjectPoint(L1,L4, Distance2 * F2[InteriorIndex]);
        LinePoints[0] := ConvertTop2D_Point(ZoomBox, StartPoint);
        LinePoints[1] := ConvertTop2D_Point(ZoomBox, EndPoint);
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          LinePoints, True, True);
      end;
    end;
  end;

  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := BitMap.Handle;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Brush.Style := bsClear;
    if Selected then
    begin
      // Draw hollow circles at nodes in selected elements.
      for NodeIndex := 0 to Nodes.Count - 1 do
      begin
        APoint := Points[NodeIndex];
        if (APoint.x >= 0) and (APoint.x < BitMap.Width)
          and (APoint.y >= 0) and (APoint.y < BitMap.Height) then
        begin
          ACanvas.Ellipse(APoint.X-NodeRadius,APoint.Y-NodeRadius,
            APoint.X+NodeRadius+1,APoint.Y+NodeRadius+1);
        end;
      end;
    end;
      // Draw solid circles at selected nodes.
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clBlack;
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex];
      if ANode.Selected then
      begin
        APoint := Points[NodeIndex];
        if (APoint.x >= 0) and (APoint.x < BitMap.Width)
          and (APoint.y >= 0) and (APoint.y < BitMap.Height) then
        begin
          ACanvas.Ellipse(APoint.X-NodeRadius,APoint.Y-NodeRadius,
            APoint.X+NodeRadius+1,APoint.Y+NodeRadius+1);
        end;
      end;
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TFishnetMeshElement.DrawNumbers(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
const
  LowInteger = -2147483648;
  HighInteger = 2147483647;
var
  NumString: string;
  Extent: TSize;
begin
  ComputeNumberCenterLocations(ZoomBox);

  if Nodes.Count = 4 then
  begin
    NumString := IntToStr(FirstControl.Count);
    Extent := BitMap.TextExtent(NumString);
    try
      FLabelLocation1.Left := FCenterLocation1.x - (Extent.cx div 2);
      FLabelLocation1.Right := FLabelLocation1.Left + Extent.cx;
      FLabelLocation1.Top := FCenterLocation1.y - (Extent.cy div 2);
      FLabelLocation1.Bottom := FLabelLocation1.Top + Extent.cy;
      BitMap.FillRectS(FLabelLocation1, clWhite32);
      BitMap.Textout(FLabelLocation1.Left, FLabelLocation1.Top, NumString);
    except on EIntOverflow do
      // do nothing
    end;

    NumString := IntToStr(SecondControl.Count);
    Extent := BitMap.TextExtent(NumString);
    try
      FLabelLocation2.Left := FCenterLocation2.x - (Extent.cx div 2);
      FLabelLocation2.Right := FLabelLocation2.Left + Extent.cx;
      FLabelLocation2.Top := FCenterLocation2.y - (Extent.cy div 2);
      FLabelLocation2.Bottom := FLabelLocation2.Top + Extent.cy;
      BitMap.FillRectS(FLabelLocation2, clWhite32);
      BitMap.Textout(FLabelLocation2.Left, FLabelLocation2.Top, NumString);
    except on EIntOverflow do
      // do nothing
    end;
  end;
end;

function TFishnetMeshElement.ElCollection: TFishnetMeshElementCollection;
begin
  result:= Collection as TFishnetMeshElementCollection;
end;

function TFishnetMeshElement.Fractions1: TRealArray;
begin
  Result := GetFractions(FirstControl);
end;

function TFishnetMeshElement.Fractions2: TRealArray;
begin
  Result := GetFractions(SecondControl);
end;

function TFishnetMeshElement.GetFractions(MeshControls: TMeshControl): TRealArray;
var
  InteriorIndex: Integer;
  Sum: double;
  CurrentLength: double;
  StopIndex: Integer;
  StartIndex: Integer;
begin
  Assert(MeshControls.Count >= 1);
  SetLength(Result, MeshControls.Count + 1);
  Result[0] := 0;
  if MeshControls.Count > 1 then
  begin
    case MeshControls.GrowthMethod of
      gmUniform:
        begin
          for InteriorIndex := 1 to MeshControls.Count - 1 do
          begin
            result[InteriorIndex] := (InteriorIndex)/MeshControls.Count;
          end;
        end;
      gmUp:
        begin
          Sum := 1;
          CurrentLength := 1;
          for InteriorIndex := 1 to MeshControls.Count-1 do
          begin
            result[MeshControls.Count - InteriorIndex] := Sum;
            CurrentLength := CurrentLength * MeshControls.GrowthRate;
            Sum := Sum + CurrentLength;
          end;
          for InteriorIndex := 1 to MeshControls.Count-1 do
          begin
            result[InteriorIndex] := result[InteriorIndex]/ Sum;
          end;
        end;
      gmDown:
        begin
          Sum := 1;
          CurrentLength := 1;
          for InteriorIndex := 1 to MeshControls.Count-1 do
          begin
            result[MeshControls.Count - InteriorIndex] := Sum;
            CurrentLength := CurrentLength / MeshControls.GrowthRate;
            Sum := Sum + CurrentLength;
          end;
          for InteriorIndex := 1 to MeshControls.Count-1 do
          begin
            result[InteriorIndex] := result[InteriorIndex]/ Sum;
          end;
        end;
      gmMiddle, gmEdge:
        begin
          if Odd(MeshControls.Count) then
          begin
            StopIndex := (MeshControls.Count div 2)+1;
          end
          else
          begin
            StopIndex := (MeshControls.Count div 2);
          end;

          Sum := 1;
          CurrentLength := 1;
          for InteriorIndex := 1 to StopIndex-1 do
          begin
            result[InteriorIndex] := Sum;
            case MeshControls.GrowthMethod of
              gmMiddle:
                begin
                  CurrentLength := CurrentLength * MeshControls.GrowthRate;
                end;
              gmEdge:
                begin
                  CurrentLength := CurrentLength / MeshControls.GrowthRate;
                end;
              else Assert(False);
            end;
            Sum := Sum + CurrentLength;
          end;
          StartIndex := StopIndex;
          if not Odd(MeshControls.Count) then
          begin
            result[StartIndex] := Sum;
            Sum := Sum + CurrentLength;
            Inc(StartIndex);
          end;
          for InteriorIndex := StartIndex to MeshControls.Count-1 do
          begin
            result[InteriorIndex] := Sum;
            case MeshControls.GrowthMethod of
              gmMiddle:
                begin
                  CurrentLength := CurrentLength / MeshControls.GrowthRate;
                end;
              gmEdge:
                begin
                  CurrentLength := CurrentLength * MeshControls.GrowthRate;
                end;
              else Assert(False);
            end;
            Sum := Sum + CurrentLength;
          end;
          for InteriorIndex := 0 to MeshControls.Count - 1 do
          begin
            result[InteriorIndex] := result[InteriorIndex]/ Sum;
          end;
        end;
      gmCustom:
        begin
          for InteriorIndex := 0 to MeshControls.LayerCollection.Count - 1 do
          begin
            Result[MeshControls.LayerCollection.Count -InteriorIndex] :=
              MeshControls.LayerCollection[InteriorIndex].Fraction;
          end;
        end;
      else Assert(False);
    end;
  end;
  Result[Length(Result) - 1] := 1;
end;

function TFishnetMeshElement.GetGenerator: TFishnetMeshGenerator;
begin
  if Collection = nil then
  begin
    result := nil
  end
  else
  begin
    result := (Collection as TFishnetMeshElementCollection).Generator;
  end;
end;

function TFishnetMeshElement.GetNodeNumbers: TIntegerCollection;
begin
  if (Model <> nil) then
  begin
    if [csLoading, csReading] * Model.ComponentState = [] then
    begin
      UpdateNodeNumbers;
    end;
  end;
  result := FNodeNumbers
end;

function TFishnetMeshElement.Intersect(Segment: TSegment2D; SkipNodes: TFishnetNodeList): Boolean;
var
  PriorIndex: Integer;
  NodeIndex: Integer;
  EdgeSegment: TSegment2D;
  Node1: TFishnetMeshNode;
  Node2: TFishnetMeshNode;
begin
  result := False;
  PriorIndex := Nodes.Count-1;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node1 := Nodes.Items[NodeIndex];
    Node2 := Nodes.Items[PriorIndex];
    if SkipNodes <> nil then
    begin
      if (SkipNodes.IndexOf(Node1) >= 0)
        or (SkipNodes.IndexOf(Node2) >= 0) then
      begin
        Continue;
      end;
    end;
    EdgeSegment := EquateSegment(Node1.Location, Node2.Location);
    result := FastGEO.Intersect(Segment, EdgeSegment);
    if result then
    begin
      Break;
    end;
    PriorIndex := NodeIndex;
  end;
end;

procedure TFishnetMeshElement.ComputeNumberCenterLocations(const ZoomBox: TQRbwZoomBox2);
var
  LabelLocationReal2: TPoint2D;
  LabelLocationReal1: TPoint2D;
  Location2: TPoint2D;
  Location1: TPoint2D;
  Location3: TPoint2D;
begin
  if Nodes.Count = 4 then
  begin
    Location1 := Nodes[0].Location;
    Location2 := Nodes[1].Location;
    Location3 := Nodes[2].Location;
    SegmentMidPoint(Location1.x, Location1.y, Location2.x, Location2.y,
      LabelLocationReal1.x, LabelLocationReal1.y);
    SegmentMidPoint(Location2.x, Location2.y, Location3.x, Location3.y,
      LabelLocationReal2.x, LabelLocationReal2.y);
    // At this point, the top left point is being assigned the center point.
    FCenterLocation1 := ConvertTop2D_Point(ZoomBox, LabelLocationReal1);
    FCenterLocation2 := ConvertTop2D_Point(ZoomBox, LabelLocationReal2);
//    FLabelLocation1.TopLeft := ConvertTop2D_Point(ZoomBox, LabelLocationReal1);
//    FLabelLocation2.TopLeft := ConvertTop2D_Point(ZoomBox, LabelLocationReal2);
  end;
end;

function TFishnetMeshElement.IsInside(APoint: TPoint2D): Boolean;
var
  Polygon:TPolygon2D;
  index: integer;
begin
  result := False;
  if FNodes.Count < 3 then
  begin
    Exit;
  end;
  SetLength(Polygon, FNodes.Count);
  for index := 0 to FNodes.Count - 1 do
  begin
    Polygon[index] := FNodes[index].Location;
  end;
  result := ModelMuseUtilities.PointInConcavePolygon(APoint, Polygon);
end;

function TFishnetMeshElement.Model: TBaseModel;
begin
  if Collection = nil then
  begin
    result := nil;
  end
  else
  begin
    result := (Collection as TFishnetMeshElementCollection).Model;
  end;
end;

procedure TFishnetMeshElement.RestoreNodes;
var
  Generator: TFishnetMeshGenerator;
  index: integer;
  ANode: TFishnetMeshNode;
begin
  Generator := ElCollection.FGenerator;
  FNodes.Clear;
  FNodes.Capacity := FNodeNumbers.Count;
  for index := 0 to FNodeNumbers.Count - 1 do
  begin
    ANode:= Generator.Nodes.Items[FNodeNumbers[index].Value];
    FNodes.Add(ANode);
    if ANode.Elements.IndexOf(Self) < 0 then
    begin
      ANode.Elements.Add(Self);
    end;
  end;
end;

procedure TFishnetMeshElement.SetFirstControl(const Value: TMeshControl);
begin
  FFirstControl.Assign(Value);
end;

procedure TFishnetMeshElement.SetNodeNumbers(const Value: TIntegerCollection);
begin
  FNodeNumbers.Assign(Value);
end;

procedure TFishnetMeshElement.SetSecondControl(const Value: TMeshControl);
begin
  FSecondControl.Assign(Value);
end;

procedure TFishnetMeshElement.SetSelected(const Value: boolean);
begin
  FSelected := Value;
end;

procedure TFishnetMeshElement.UpdateNodeNumbers;
var
  Index: Integer;
begin
  FNodeNumbers.Clear;
  FNodeNumbers.Capacity := FNodes.Count;
  for Index := 0 to FNodes.Count - 1 do
  begin
    FNodeNumbers.Add.Value := FNodes[Index].Index;
  end;
end;

function TFishnetMeshElement.ValidElement: boolean;
var
  Segment1: TSegment2D;
  Segment2: TSegment2D;
begin
  result := Nodes.Count = 4;
  if result then
  begin
    Segment1[1] := Nodes[0].FLocation;
    Segment1[2] := Nodes[2].FLocation;
    Segment2[1] := Nodes[1].FLocation;
    Segment2[2] := Nodes[3].FLocation;
    result := FastGeo.Intersect(Segment1, Segment2);
  end;
  if result then
  begin
    result := not Coincident(Nodes[0].FLocation, Nodes[1].FLocation)
      and not Coincident(Nodes[1].FLocation, Nodes[2].FLocation)
      and not Coincident(Nodes[2].FLocation, Nodes[3].FLocation)
      and not Coincident(Nodes[3].FLocation, Nodes[0].FLocation)
      and not Coincident(Nodes[0].FLocation, Nodes[2].FLocation)
      and not Coincident(Nodes[1].FLocation, Nodes[3].FLocation)
  end;
end;

{ TFishnetMeshNodeCollection }

function TFishnetMeshNodeCollection.Add: TFishnetMeshNode;
begin
  result := inherited Add as TFishnetMeshNode
end;

constructor TFishnetMeshNodeCollection.Create(InvalidateModelEvent: TNotifyEvent;
  Generator: TFishnetMeshGenerator);
begin
  inherited Create(TFishnetMeshNode, InvalidateModelEvent);
  FGenerator := Generator;
end;

function TFishnetMeshNodeCollection.GetItems(Index: Integer): TFishnetMeshNode;
begin
  Result := inherited Items[Index] as TFishnetMeshNode;
end;

procedure TFishnetMeshNodeCollection.SetItems(Index: Integer;
  const Value: TFishnetMeshNode);
begin
  inherited Items[Index] := Value;
end;

{ TFishnetMeshElementCollection }

function TFishnetMeshElementCollection.Add: TFishnetMeshElement;
begin
  Result := inherited Add as TFishnetMeshElement;
end;

procedure TFishnetMeshElementCollection.Assign(Source: TPersistent);
var
  SourceCollection: TFishnetMeshElementCollection;
  index: Integer;
begin
  if Source is TFishnetMeshElementCollection then
  begin
    SourceCollection := TFishnetMeshElementCollection(Source);
    while Count > SourceCollection.Count do
    begin
      Delete(Count-1);
    end;
    while Count < SourceCollection.Count do
    begin
      Add;
    end;
    for index := 0 to Count - 1 do
    begin
      Items[index].Assign(SourceCollection.Items[index]);
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TFishnetMeshElementCollection.Create(Model: TBaseModel;
  Generator: TFishnetMeshGenerator);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TFishnetMeshElement, InvalidateModelEvent);
  FGenerator := Generator;
end;

function TFishnetMeshElementCollection.GetItems(
  Index: Integer): TFishnetMeshElement;
begin
  result := inherited Items[Index] as TFishnetMeshElement;
end;

procedure TFishnetMeshElementCollection.SetItems(Index: Integer;
  const Value: TFishnetMeshElement);
begin
  inherited Items[Index] := Value;
end;

{ TFishnetMeshGenerator }

procedure TFishnetMeshGenerator.Assign(Source: TPersistent);
var
  SourceGen: TFishnetMeshGenerator;
begin
  if Source is TFishnetMeshGenerator then
  begin
    SourceGen := Source as TFishnetMeshGenerator;
    Nodes := SourceGen.Nodes;
    Elements := SourceGen.Elements;
  end
  else
  begin
    inherited;
  end;
end;

procedure TFishnetMeshGenerator.Clear;
begin
  Elements.Clear;
  Nodes.Clear;
end;

constructor TFishnetMeshGenerator.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  FElements := TFishnetMeshElementCollection.Create(Model, self);
  FNodes := TFishnetMeshNodeCollection.Create(InvalidateModelEvent, self);
  FFont:= TFont.Create;
end;

destructor TFishnetMeshGenerator.Destroy;
begin
  FFont.Free;
  FNodes.Free;
  FElements.Free;
  inherited;
end;

procedure TFishnetMeshGenerator.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  index: Integer;
begin
  for index := 0 to Elements.Count - 1 do
  begin
    Elements[index].DrawEdges(BitMap, ZoomBox);
  end;
  BitMap.Font := FFont;
  for index := 0 to Elements.Count - 1 do
  begin
    Elements[index].DrawNumbers(BitMap, ZoomBox);
  end;
end;

procedure TFishnetMeshGenerator.EliminateDuplicateNodes;
var
  OuterNodeIndex: Integer;
  OuterNode: TFishnetMeshNode;
  InnerNodeIndex: Integer;
  InnerNode: TFishnetMeshNode;
  ElementIndex: Integer;
  AnElement: TFishnetMeshElement;
  NodePosition: Integer;
  ChangedElements: TList<TFishnetMeshElement>;
begin
  ChangedElements := TList<TFishnetMeshElement>.Create;
  try
    for OuterNodeIndex := Nodes.Count - 1 downto 1 do
    begin
      OuterNode := Nodes[OuterNodeIndex];
      for InnerNodeIndex := OuterNodeIndex - 1 downto 0 do
      begin
        InnerNode := Nodes[InnerNodeIndex];
        if (OuterNode.Location.X = InnerNode.Location.X)
          and (OuterNode.Location.Y = InnerNode.Location.Y) then
        begin
          // duplicate found. Replace OuterNode with InnerNode in all elements and delete OuterNode.
          for ElementIndex := 0 to OuterNode.Elements.Count - 1 do
          begin
            AnElement := OuterNode.Elements[ElementIndex];
            InnerNode.Elements.Add(AnElement);
            NodePosition := AnElement.FNodes.IndexOf(OuterNode);
            Assert(NodePosition >= 0);
            AnElement.FNodes[NodePosition] := InnerNode;
            AnElement.UpdateNodeNumbers;
            ChangedElements.Add(AnElement);
          end;
          OuterNode.Free;
          break;
        end;
      end;
    end;
    for ElementIndex := 0 to ChangedElements.Count -1 do
    begin
      AnElement := ChangedElements[ElementIndex];
      UpdateCount1(AnElement);
      UpdateCount2(AnElement);
    end;
  finally
    ChangedElements.Free;
  end;
end;

procedure TFishnetMeshGenerator.GetCountsFromNeighbors(
  Element: TFishnetMeshElement);
var
  NodeIndex: Integer;
  Node1: TFishnetMeshNode;
  Node2: TFishnetMeshNode;
  ElementIndex: Integer;
  AnotherElement: TFishnetMeshElement;
  Node1Position: Integer;
  Node2Position: Integer;
  PriorIndex: Integer;
  Opposite: boolean;
  EOrientation: Integer;
  AnOrientation: Integer;
begin
  EOrientation := Orientation(Element.Nodes[0].Location,
    Element.Nodes[1].Location, Element.Nodes[2].Location);
  PriorIndex := Element.Nodes.Count-1;
  for NodeIndex := 0 to Element.Nodes.Count - 1 do
  begin
    if NodeIndex in [1,3] then
    begin
      Node1 := Element.Nodes[NodeIndex];
      Node2 := Element.Nodes[PriorIndex];
      for ElementIndex := 0 to Node1.Elements.Count - 1 do
      begin
        AnotherElement := Node1.Elements[ElementIndex];
        if AnotherElement = Element then
        begin
          Continue;
        end;
        Node1Position := AnotherElement.Nodes.IndexOf(Node1);
        Node2Position := AnotherElement.Nodes.IndexOf(Node2);
        if (Node1Position >= 0) and (Node2Position >= 0) then
        begin
          AnOrientation := Orientation(AnotherElement.Nodes[0].Location,
            AnotherElement.Nodes[1].Location, AnotherElement.Nodes[2].Location);
          if ((Node1Position in [0,1]) and (Node2Position in [0,1]))
            or ((Node1Position in [2,3]) and (Node2Position in [2,3])) then
          begin
            Opposite := (NodeIndex = 1) = (Node1Position in [0,1]);
            if EOrientation <> AnOrientation then
            begin
              Opposite := not Opposite;
            end;
            if Opposite then
            begin
              if not AnotherElement.FirstControl.Opposite(Element.FirstControl) then
              begin
                Element.FirstControl.AssignOpposite(AnotherElement.FirstControl);
              end;
            end
            else
            begin
              if not AnotherElement.FirstControl.Equals(Element.FirstControl) then
              begin
                Element.FirstControl := AnotherElement.FirstControl;
              end;
            end;
          end
          else
          begin
            Opposite := (NodeIndex = 1) <> (Node1Position in [0,3]);
            if EOrientation <> AnOrientation then
            begin
              Opposite := not Opposite;
            end;
            if Opposite then
            begin
              if not AnotherElement.SecondControl.Opposite(Element.FirstControl) then
              begin
                Element.FirstControl.AssignOpposite(AnotherElement.SecondControl);
              end;
            end
            else
            begin
              if not AnotherElement.SecondControl.Equals(Element.FirstControl) then
              begin
                Element.FirstControl := AnotherElement.SecondControl;
              end;
            end;
          end;
          Break;
        end;
      end;
    end;

    PriorIndex := NodeIndex;
  end;

  PriorIndex := Element.Nodes.Count-1;
  for NodeIndex := 0 to Element.Nodes.Count - 1 do
  begin
    if NodeIndex in [0,2] then
    begin
      Node1 := Element.Nodes[NodeIndex];
      Node2 := Element.Nodes[PriorIndex];
      for ElementIndex := 0 to Node1.Elements.Count - 1 do
      begin
        AnotherElement := Node1.Elements[ElementIndex];
        if AnotherElement = Element then
        begin
          Continue;
        end;
        Node1Position := AnotherElement.Nodes.IndexOf(Node1);
        Node2Position := AnotherElement.Nodes.IndexOf(Node2);
        if (Node1Position >= 0) and (Node2Position >= 0) then
        begin
          AnOrientation := Orientation(AnotherElement.Nodes[0].Location,
            AnotherElement.Nodes[1].Location, AnotherElement.Nodes[2].Location);
          if ((Node1Position in [0,1]) and (Node2Position in [0,1]))
            or ((Node1Position in [2,3]) and (Node2Position in [2,3])) then
          begin
            Opposite := (NodeIndex = 0) <> (Node1Position in [0,1]);
            if EOrientation <> AnOrientation then
            begin
              Opposite := not Opposite;
            end;
            if Opposite then
            begin
              if not AnotherElement.FirstControl.Opposite(Element.SecondControl) then
              begin
                Element.SecondControl.AssignOpposite(AnotherElement.FirstControl);
              end;
            end
            else
            begin
              if not AnotherElement.FirstControl.Equals(Element.SecondControl) then
              begin
                Element.SecondControl := AnotherElement.FirstControl;
              end;
            end;
          end
          else
          begin
            Opposite := (NodeIndex = 0) = (Node1Position in [0,3]);
            if EOrientation <> AnOrientation then
            begin
              Opposite := not Opposite;
            end;
            if Opposite then
            begin
              if not AnotherElement.SecondControl.Opposite(Element.SecondControl) then
              begin
                Element.SecondControl.AssignOpposite(AnotherElement.SecondControl);
              end;
            end
            else
            begin
              if not AnotherElement.SecondControl.Equals(Element.SecondControl) then
              begin
                Element.SecondControl := AnotherElement.SecondControl;
              end;
            end;
          end;
          Break;
        end;
      end;
    end;

    PriorIndex := NodeIndex;
  end;

  UpdateCount1(Element);
  UpdateCount2(Element);

end;

procedure TFishnetMeshGenerator.Loaded;
var
  index: Integer;
begin
  inherited;
  for index := 0 to Elements.Count - 1 do
  begin
    Elements[index].RestoreNodes;
  end;
  EliminateDuplicateNodes;
end;

procedure TFishnetMeshGenerator.SetElements(
  const Value: TFishnetMeshElementCollection);
begin
  FElements.Assign(Value);
end;

procedure TFishnetMeshGenerator.SetNodes(
  const Value: TFishnetMeshNodeCollection);
begin
  FNodes.Assign(Value);
end;

procedure TFishnetMeshGenerator.UpdateCount1(Element: TFishnetMeshElement);
var
  NodeIndex: Integer;
  Node1: TFishnetMeshNode;
  Node2: TFishnetMeshNode;
  ElementIndex: Integer;
  AnotherElement: TFishnetMeshElement;
  Node1Position: Integer;
  Node2Position: Integer;
  PriorIndex: Integer;
  MarkedElements: Boolean;
  EOrientation: Integer;
  AnOrientation: Integer;
  Opposite: boolean;
begin
  MarkedElements := False;
  if not FUpdating then
  begin
    MarkElements;
    MarkedElements := True;
  end;
  FUpdating := True;
  try
    EOrientation := Orientation(Element.Nodes[0].Location,
      Element.Nodes[1].Location, Element.Nodes[2].Location);
    Element.FMarked := True;
    PriorIndex := Element.Nodes.Count-1;
    for NodeIndex := 0 to Element.Nodes.Count - 1 do
    begin
      if NodeIndex in [1,3] then
      begin
        Node1 := Element.Nodes[NodeIndex];
        Node2 := Element.Nodes[PriorIndex];
        for ElementIndex := 0 to Node1.Elements.Count - 1 do
        begin
          AnotherElement := Node1.Elements[ElementIndex];
          if (AnotherElement = Element) or AnotherElement.FMarked then
          begin
            Continue;
          end;
          Node1Position := AnotherElement.Nodes.IndexOf(Node1);
          Node2Position := AnotherElement.Nodes.IndexOf(Node2);
          if (Node1Position >= 0) and (Node2Position >= 0) then
          begin
            AnOrientation := Orientation(AnotherElement.Nodes[0].Location,
              AnotherElement.Nodes[1].Location, AnotherElement.Nodes[2].Location);
            AnotherElement.FMarked := True;
            if ((Node1Position in [0,1]) and (Node2Position in [0,1]))
              or ((Node1Position in [2,3]) and (Node2Position in [2,3])) then
            begin
              Opposite := (NodeIndex = 1) = (Node1Position in [0,1]);
              if EOrientation <> AnOrientation then
              begin
                Opposite := not Opposite;
              end;
              if Opposite then
              begin
                if not AnotherElement.FirstControl.Opposite(Element.FirstControl) then
                begin
                  AnotherElement.FirstControl.AssignOpposite(Element.FirstControl);
                  UpdateCount1(AnotherElement);
                end;
              end
              else
              begin
                if not AnotherElement.FirstControl.Equals(Element.FirstControl) then
                begin
                  AnotherElement.FirstControl := Element.FirstControl;
                  UpdateCount1(AnotherElement);
                end;
              end;
            end
            else
            begin
              Opposite := (NodeIndex = 1) = (Node1Position in [1,2]);
              if EOrientation <> AnOrientation then
              begin
                Opposite := not Opposite;
              end;
              if Opposite then
              begin
                if not AnotherElement.SecondControl.Opposite(Element.FirstControl) then
                begin
                  AnotherElement.SecondControl.AssignOpposite(Element.FirstControl);
                  UpdateCount2(AnotherElement);
                end;
              end
              else
              begin
                if not AnotherElement.SecondControl.Equals(Element.FirstControl) then
                begin
                  AnotherElement.SecondControl := Element.FirstControl;
                  UpdateCount2(AnotherElement);
                end;
              end;
            end;
          end;
        end;
      end;

      PriorIndex := NodeIndex;
    end;
  finally
    if MarkedElements then
    begin
      FUpdating := False;
    end;
  end;
end;

procedure TFishnetMeshGenerator.UpdateCount2(Element: TFishnetMeshElement);
var
  NodeIndex: Integer;
  Node1: TFishnetMeshNode;
  Node2: TFishnetMeshNode;
  ElementIndex: Integer;
  AnotherElement: TFishnetMeshElement;
  Node1Position: Integer;
  Node2Position: Integer;
  PriorIndex: Integer;
  MarkedElements: Boolean;
  EOrientation: Integer;
  AnOrientation: Integer;
  Opposite: boolean;
begin
  MarkedElements := False;
  if not FUpdating then
  begin
    MarkElements;
    MarkedElements := True;
  end;
  FUpdating := True;
  try
    EOrientation := Orientation(Element.Nodes[0].Location,
      Element.Nodes[1].Location, Element.Nodes[2].Location);
    Element.FMarked := True;
    PriorIndex := Element.Nodes.Count-1;
    for NodeIndex := 0 to Element.Nodes.Count - 1 do
    begin
      if NodeIndex in [0,2] then
      begin
        Node1 := Element.Nodes[NodeIndex];
        Node2 := Element.Nodes[PriorIndex];
        for ElementIndex := 0 to Node1.Elements.Count - 1 do
        begin
          AnotherElement := Node1.Elements[ElementIndex];
          if (AnotherElement = Element) or AnotherElement.FMarked then
          begin
            Continue;
          end;
          Node1Position := AnotherElement.Nodes.IndexOf(Node1);
          Node2Position := AnotherElement.Nodes.IndexOf(Node2);
          if (Node1Position >= 0) and (Node2Position >= 0) then
          begin
            AnOrientation := Orientation(AnotherElement.Nodes[0].Location,
              AnotherElement.Nodes[1].Location, AnotherElement.Nodes[2].Location);
            AnotherElement.FMarked := True;
            if ((Node1Position in [0,1]) and (Node2Position in [0,1]))
              or ((Node1Position in [2,3]) and (Node2Position in [2,3])) then
            begin
              Opposite := (NodeIndex = 0) = (Node1Position in [0,1]);
              if EOrientation = AnOrientation then
              begin
                Opposite := not Opposite;
              end;
              if Opposite then
              begin
                if not AnotherElement.FirstControl.Opposite(Element.SecondControl) then
                begin
                  AnotherElement.FirstControl.AssignOpposite(Element.SecondControl);
                  UpdateCount1(AnotherElement);
                end;
              end
              else
              begin
                if not AnotherElement.FirstControl.Equals(Element.SecondControl) then
                begin
                  AnotherElement.FirstControl := Element.SecondControl;
                  UpdateCount1(AnotherElement);
                end;
              end;
            end
            else
            begin
              Opposite := (NodeIndex = 0) = (Node1Position in [0,3]);
              if EOrientation <> AnOrientation then
              begin
                Opposite := not Opposite;
              end;
              if Opposite then
              begin
                if not AnotherElement.SecondControl.Opposite(Element.SecondControl) then
                begin
                  AnotherElement.SecondControl.AssignOpposite(Element.SecondControl);
                  UpdateCount2(AnotherElement);
                end;
              end
              else
              begin
                if not AnotherElement.SecondControl.Equals(Element.SecondControl) then
                begin
                  AnotherElement.SecondControl := Element.SecondControl;
                  UpdateCount2(AnotherElement);
                end;
              end;
            end;
          end;
        end;
      end;

      PriorIndex := NodeIndex;
    end;
  finally
    if MarkedElements then
    begin
      FUpdating := False;
    end;
  end;
end;

procedure TFishnetMeshGenerator.MarkElements;
var
  ElementIndex: Integer;
  AnElement: TFishnetMeshElement;
begin
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    AnElement := Elements[ElementIndex];
    AnElement.FMarked := False;
  end;
end;

{ TUndoFishnetMeshValues }

constructor TUndoFishnetMeshValues.Create;
begin
  FOldFishNetMesh:= TFishnetMeshGenerator.Create(nil);
  FNewFishNetMesh:= TFishnetMeshGenerator.Create(nil);
  FOldFishNetMesh.Assign(frmGoPhast.PhastModel.FishnetMeshGenerator);
end;

function TUndoFishnetMeshValues.Description: string;
begin
  result := 'change values of fishnet mesh quadrilaterals';
end;

destructor TUndoFishnetMeshValues.Destroy;
begin
  FOldFishNetMesh.Free;
  FNewFishNetMesh.Free;
  inherited;
end;

procedure TUndoFishnetMeshValues.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.FishnetMeshGenerator := FNewFishNetMesh;
  frmGoPhast.PhastModel.FishnetMeshGenerator.Loaded;
  FishnetTool.UpdateQuadTree;
  frmGoPhast.InvalidateTop;
end;

procedure TUndoFishnetMeshValues.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.FishnetMeshGenerator := FOldFishNetMesh;
  frmGoPhast.PhastModel.FishnetMeshGenerator.Loaded;
  FishnetTool.UpdateQuadTree;
  frmGoPhast.InvalidateTop;
end;

procedure TUndoFishnetMeshValues.UpdateNewFishnetMesh;
begin
  FNewFishNetMesh.Assign(frmGoPhast.PhastModel.FishnetMeshGenerator);
end;

procedure TUndoFishnetMeshValues.UpdateOldFishnetMesh;
begin
  FOldFishNetMesh.Assign(frmGoPhast.PhastModel.FishnetMeshGenerator);
end;


{ TUndoFishnetMesh }

function TUndoFishnetMesh.Description: string;
begin
  result := 'change fishnet mesh quadrilaterals';
end;

procedure TUndoFishnetMesh.DoCommand;
begin
  FishnetTool.HideEdits;
  inherited;
  FishnetTool.NilElement;
end;

procedure TUndoFishnetMesh.Undo;
begin
  FishnetTool.HideEdits;
  inherited;
  FishnetTool.NilElement;
end;

{ TMeshControl }

procedure TMeshControl.AssignOpposite(Source: TMeshControl);
var
  index: Integer;
begin
  GrowthRate := Source.GrowthRate;
  case Source.GrowthMethod of
    gmUniform, gmMiddle, gmEdge, gmCustom: GrowthMethod := Source.GrowthMethod;
    gmUp: GrowthMethod := gmDown;
    gmDown: GrowthMethod := gmUp;
    else Assert(False);
  end;
  LayerCollection.Assign(Source.LayerCollection);
  if GrowthMethod in [gmCustom, gmUp, gmDown] then
  begin
    for index := 0 to LayerCollection.Count - 1 do
    begin
      LayerCollection[index].Fraction :=
        1 - Source.LayerCollection[LayerCollection.Count - 1 - index].Fraction;
    end;
  end
  else
  begin
    Count := Source.Count;
  end;
end;

constructor TMeshControl.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  Count := 2;
end;

function TMeshControl.Equals(OtherMeshControl: TMeshControl): Boolean;
begin
  result := IsSame(OtherMeshControl);
end;

function TMeshControl.GetCount: Integer;
begin
  result := LayerCount;
end;

function TMeshControl.Opposite(OtherMeshControl: TMeshControl): Boolean;
var
  index: Integer;
begin
  result := Count = OtherMeshControl.Count;
  if result then
  begin
    case GrowthMethod of
      gmUniform:
        begin
          result := OtherMeshControl.GrowthMethod = gmUniform;
        end;
      gmUp:
        begin
          result := (OtherMeshControl.GrowthMethod = gmDown)
            and (OtherMeshControl.GrowthRate = GrowthRate);
        end;
      gmDown:
        begin
          result := (OtherMeshControl.GrowthMethod = gmUp)
            and (OtherMeshControl.GrowthRate = GrowthRate);
        end;
      gmMiddle, gmEdge:
        begin
          result := (OtherMeshControl.GrowthMethod = GrowthMethod)
            and (OtherMeshControl.GrowthRate = GrowthRate);
        end;
      gmCustom:
        begin
          Assert(LayerCollection.Count = OtherMeshControl.LayerCollection.Count);
          for index := 0 to LayerCollection.Count - 1 do
          begin
            result := LayerCollection[index].Fraction =
              1 - OtherMeshControl.LayerCollection[
              LayerCollection.Count - 1- index].Fraction;
            if not Result then
            begin
              break;
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TMeshControl.SetCount(const Value: Integer);
var
  Index: Integer;
  Sum: double;
  CurrentLength: double;
  StopIndex: Integer;
  StartIndex: Integer;
begin
  Assert(Value >= 1);

  if Value <> Count then
  begin
    while Value < Count  do
    begin
      LayerCollection.Delete(LayerCollection.Count-1);
    end;

    if (Value < Count) and (GrowthMethod = gmCustom) then
    begin
      GrowthMethod := gmUniform
    end;
    while Value > Count do
    begin
      LayerCollection.Add;
    end;
    if Value > 1 then
    begin
//      SetLength(Fractions, Value-1);
      case GrowthMethod of
        gmUniform:
          begin
            for Index := 0 to LayerCollection.Count - 1 do
            begin
              LayerCollection[Index].Fraction := (Index+1)/Value;
            end;
          end;
        gmUp:
          begin
            Sum := 1;
            CurrentLength := 1;
            for Index := 0 to LayerCollection.Count - 1 do
            begin
              LayerCollection[Index].Fraction := Sum;
              CurrentLength := CurrentLength * GrowthRate;
              Sum := Sum + CurrentLength;
            end;
            for Index := 0 to LayerCollection.Count - 1 do
            begin
              LayerCollection[Index].Fraction :=
                LayerCollection[Index].Fraction/ Sum;
            end;
          end;
        gmDown:
          begin
            Sum := 1;
            CurrentLength := 1;
            for Index := 0 to LayerCollection.Count - 1 do
            begin
              LayerCollection[Index].Fraction := Sum;
              CurrentLength := CurrentLength / GrowthRate;
              Sum := Sum + CurrentLength;
            end;
            for Index := 0 to LayerCollection.Count - 1 do
            begin
              LayerCollection[Index].Fraction :=
                LayerCollection[Index].Fraction/ Sum;
            end;
          end;
        gmMiddle, gmEdge:
          begin
            if Odd(Value) then
            begin
              StopIndex := (Value div 2);
            end
            else
            begin
              StopIndex := (Value div 2)-1;
            end;
            Sum := 1;
            CurrentLength := 1;
            for Index := 0 to StopIndex-1 do
            begin
              LayerCollection[Index].Fraction := Sum;
              case GrowthMethod of
                gmMiddle:
                  begin
                    CurrentLength := CurrentLength * GrowthRate;
                  end;
                gmEdge:
                  begin
                    CurrentLength := CurrentLength / GrowthRate;
                  end;
                else Assert(False);
              end;
              Sum := Sum + CurrentLength;
            end;
            StartIndex := StopIndex;
            if not Odd(Value) then
            begin
              LayerCollection[StartIndex].Fraction := Sum;
              Sum := Sum + CurrentLength;
              Inc(StartIndex);
            end;
            for Index := StartIndex to LayerCollection.Count -1 do
            begin
              LayerCollection[Index].Fraction := Sum;
              case GrowthMethod of
                gmMiddle:
                  begin
                    CurrentLength := CurrentLength / GrowthRate;
                  end;
                gmEdge:
                  begin
                    CurrentLength := CurrentLength * GrowthRate;
                  end;
                else Assert(False);
              end;
              Sum := Sum + CurrentLength;
            end;
            for Index := 0 to LayerCollection.Count - 1 do
            begin
              LayerCollection[Index].Fraction :=
                LayerCollection[Index].Fraction/ Sum;
            end;
          end;
        gmCustom:
          begin
            // do nothing.
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

end.
