{ This unit is used to determine the Delaunay triangulation
 and Voronoi diagram of a set of points.

 @author(Richard B. Winston <rbwinst@usgs.gov>)

 Modified from source code on http://www.kirby-uk.com/voronoi/VoronoiDiagram.html

 Dec. 9, 2010.
 }

unit Delaunay;

interface

uses Classes, SysUtils, QuadTreeClass, TripackTypes, Types;

type
  TLongIntList = class;

  TVoronoiVertex = record
    x : extended;
    y : extended;
    SquareOfRadius : extended;
    FormingDelaunayPointIndices : array[0..2] of longint;
    NeighborVoronoiVertexIndices : array[0..2] of longint;
  end;

  TDelaunayPoint = record
    x : extended;
    y : extended;
  end;

  TTreeData = longint;

  TTreeNode = record
    Value : TTreeData;
    Parent : longint;
    Left  : longint;
    Right : longint;
    Tag : longint;
  end;

  TVertexList = class(TObject)
  private
    FItems : array of TVoronoiVertex;
    FCount : longint;
    FCapacity : longint;
    procedure Grow;
    procedure SetItem(index : longint; item : TVoronoiVertex);
    function GetItem(index: Integer): TVoronoiVertex;
    procedure SetCapacity(const Value: integer);
    procedure SetCount(const Value: longint);
  public
    procedure DeleteLast;
    procedure Add(item : TVoronoiVertex);
    property Capacity: integer read FCapacity write SetCapacity;
    property Count : longint read FCount write SetCount;
    constructor Create;
    destructor Destroy; override;
    property Items[index : longint] : TVoronoiVertex read GetItem write SetItem; default;
  end;

  TPointList = class(TObject)
  private
    FItems : array of TDelaunayPoint;
    FCount : longint;
    FCapacity : longint;
    procedure Grow;
    procedure SetCapacity(const Value: integer);
    procedure DeleteLast;
    function GetItem(index: Integer): TDelaunayPoint;
    procedure SetCount(const Value: longint);
    procedure SetItem(index: Integer; const Value: TDelaunayPoint);
  public
    function Last : TDelaunayPoint;
    procedure Add(item : TDelaunayPoint);
    property Capacity : integer read FCapacity write SetCapacity;
    property Count : longint read FCount write SetCount;
    property Items[index : longint] : TDelaunayPoint read GetItem write SetItem;
    constructor Create;
    destructor Destroy; override;
  end;

  TVoronoiDiagram = class(TObject)
  private
    FEpsilon : extended;
    FVerticies : TVertexList;
    FPoints : TPointList;
    xmin, xmax, ymin, ymax : extended;
    Rxmin,Rxmax,Rymin,Rymax : extended;
    FQuadTree: TRbwQuadTree;
    // @name finds the vertex number of a Voronoi vertex that
    // is closer to the last Delaunay point than to the closest of its
    // forming points.
    function StartingVertex : longint;
    // @name adds the positions of all the Voronoi vertices that
    // are closer to the last
    // Delaunay point than to the closest of their forming points to
    // VerticesToDelete.
    procedure AssignVerticesToDelete(VerticesToDelete : TLongIntList);
    // @name fills NewVertices with the Voronoi vertices for the last
    // Delaunay point.
    procedure AssignNewVertices(VerticesToDelete : TLongIntList;
      NewVertices : TVertexList);
    // @name inserts the new Voronoi vertices in NewVertices into the
    // Voronoi diagram.  First, existing Voronoi vertices whose indices are
    // listed in VerticesToDelete are replaced. Any additional
    // Voronoi vertices are added to the end of @link(FVerticies).
    procedure AddNewVertices(NewVertices : TVertexList;
      VerticesToDelete : TLongIntList);
    procedure TestNewVertices(NewVertices: TVertexList;
      NewVertexPositions: TLongIntList);
    function GetPoint(Index: integer): TDelaunayPoint;
    function GetPointCount: integer;
    function GetVertexCount: integer;
    function GetVoronoiVertex(Index: integer): TVoronoiVertex;
    procedure ReplaceExistingVertices(NewVertices: TVertexList;
      VerticesToDelete: TLongIntList; vertex_mapping: TLongIntList);
    procedure AddAdditionaVerticesAtEnd(vertex_mapping: TLongIntList;
      NewVertices: TVertexList; VerticesToDelete: TLongIntList);
    procedure UpdateLinkagesForNewVertices(
      VertexIndicesToRestore: TLongIntList; VerticesToRestore: TVertexList;
      vertex_mapping: TLongIntList; NewVertices: TVertexList);
    procedure UpdateRemainingLinkages(vertex_mapping: TLongIntList;
      VertexIndicesToRestore: TLongIntList; VerticesToRestore: TVertexList);
    procedure SetPoint(Index: integer; const Value: TDelaunayPoint);
    procedure SetPointCount(const Value: integer);
    procedure SetVertexCount(const Value: integer);
    procedure SetVoronoiVertex(Index: integer; const Value: TVoronoiVertex);
    function StartingVertex2: longint;
    function StartingVertex3: longint;
  public
    constructor Create(XLow,YLow,XHigh,YHigh : extended; Epsilon: extended = 1E-5);
    destructor Destroy; override;
    procedure AddPoint(DelaunayPoint : TDelaunayPoint);
    procedure CheckNewPoint(DelaunayPoint: TDelaunayPoint;
      NewVertexPositions: TLongIntList; New_Vertices: TVertexList);
    property PointCount: integer read GetPointCount write SetPointCount;
    property Points[Index: integer]: TDelaunayPoint read GetPoint write SetPoint;
    property VertexCount: integer read GetVertexCount write SetVertexCount;
    property VoronoiVertices[Index: integer]: TVoronoiVertex read GetVoronoiVertex write SetVoronoiVertex;
  end;

  TLongIntList = class(TObject)
  private
    FItems : array of longint;
    FCount : Longint;
    FCapacity : Longint;
    procedure Grow;
    function GetItems(index : Longint) : Longint;
  public
    constructor Create;
    destructor Destroy; override;
    property Count : Longint read FCount;
    property Items[index : Longint] : Longint read GetItems; default;
    function IndexOf(item : Longint) : Longint;
    procedure Add(item : Longint);
  end;

  TDelaunayException = class(Exception);

function GetEpsilon(const X, Y: TFloatArray): double;

procedure GetVoronoiDiagram(var AVoronoiDiagram: TVoronoiDiagram;
  var YArray, XArray: TFloatArray);

implementation

uses
  Math, FastGEO, TripackProcedures;

resourcestring
  StrErrorInTRMESH = 'Error in TRMESH';
  StrErrorInTRLIST2 = 'Error in TRLIST2';

const
  NROW=9;

function GetLimitsAndEpsilon(const X, Y: TFloatArray;
  out XMax, XMin, YMax, YMin: double): double;
var
  Index: Integer;
  Value: TFloat;
begin
  XMin := X[0];
  XMax := XMin;
  YMin := Y[0];
  YMax := YMin;
  for Index := 0 to Length(X) - 1 do
  begin
    Value := X[Index];
    if Value < XMin then
    begin
      XMin := Value;
    end
    else if Value > XMax then
    begin
      XMax := Value;
    end;
    Value := Y[Index];
    if Value < YMin then
    begin
      YMin := Value;
    end
    else if Value > YMax then
    begin
      YMax := Value;
    end;
  end;
  result := Sqrt((YMax-YMin)*(XMax-XMin)/Length(X))/100000;
end;

function GetEpsilon(const X, Y: TFloatArray): double;
var
  XMax: Double;
  XMin: Double;
  YMax: Double;
  YMin: Double;
begin
  result := GetLimitsAndEpsilon(X, Y, XMax, XMin, YMax, YMin);
end;

procedure GetVoronoiDiagram(var AVoronoiDiagram: TVoronoiDiagram;
  var YArray, XArray: TFloatArray);
var
  APointIndex: Integer;
  LocalTriangle: TTriangle2D;
  APoint2d: TPoint2D;
  TempValue: Integer;
  AVertex: TVoronoiVertex;
  AY: TFloat;
  AnX: TFloat;
  PInd: Integer;
  PIndex: Integer;
  TriangleIndex: Integer;
  APoint: TDelaunayPoint;
  NT: Integer;
  LCT: TNcmaxIntArray;
  LTRI: TNtmx_LwkIntArray;
  IER: Integer;
  LNEW: Integer;
  DIST: array of TFloat;
  NEAR: TLwkIntArray;
  NEXT: array of Integer;
  LEND: TNmaxIntArray;
  LPTR: TN6IntArray;
  LIST: TN6IntArray;
  N: Integer;
  PointIndex: Integer;
  LCC: TNcmaxIntArray;
  XMax: Double;
  XMin: Double;
  YMax: Double;
  YMin: Double;
  LocalEpsilon: Double;
begin
  N := Length(XArray);
  Assert(Length(YArray) = N);
  SetLength(LIST, 6 * N - 12);
  SetLength(LPTR, 6 * N - 12);
  SetLength(LEND, N);
  SetLength(NEXT, 2 * N);
  SetLength(NEAR, 2 * N);
  SetLength(DIST, 2 * N);
  TRMESH(N, XArray, YArray, LIST, LPTR, LEND, LNEW, NEAR, NEXT, DIST, IER);
  if IER <> 0 then
  begin
    Raise TDelaunayException.Create(StrErrorInTRMESH);
  end;
  NT := 2*N-5;
  SetLength(LTRI, NT, NROW);
  SetLength(LCT, 2);
  SetLength(LCC, 2);
  TRLIST2(0, LCC, N, LIST, LPTR, LEND, NROW, NT, LTRI, LCT, IER);
  Assert(Length(LTRI) >= NT);
  if IER <> 0 then
  begin
    Raise TDelaunayException.Create(StrErrorInTRLIST2);
  end;
  AVoronoiDiagram.Free;
  LocalEpsilon := GetLimitsAndEpsilon(XArray, YArray, XMax, XMin, YMax, YMin);
  AVoronoiDiagram := TVoronoiDiagram.Create(XMin, YMin, XMax, YMax, LocalEpsilon);
  AVoronoiDiagram.PointCount := N;
  for PointIndex := 0 to N - 1 do
  begin
    APoint.x := XArray[PointIndex];
    APoint.y := YArray[PointIndex];
    AVoronoiDiagram.Points[PointIndex] := APoint;
  end;
  //  SetLength(VCenters, NT);
  AVoronoiDiagram.VertexCount := NT;
  for TriangleIndex := 0 to NT - 1 do
  begin
    for PIndex := 0 to 2 do
    begin
      PInd := LTRI[TriangleIndex, PIndex] - 1;
      Assert(PInd < N);
      Assert(PInd >= 0);
      AnX := XArray[PInd];
      LocalTriangle[PIndex + 1].x := AnX;
      AY := YArray[PInd];
      LocalTriangle[PIndex + 1].y := AY;
      AVertex.FormingDelaunayPointIndices[PIndex] := PInd;
      AVertex.NeighborVoronoiVertexIndices[PIndex] :=
        LTRI[TriangleIndex, PIndex + 3] - 1;
    end;
    while AVertex.FormingDelaunayPointIndices[2] <
      Max(AVertex.FormingDelaunayPointIndices[0],
      AVertex.FormingDelaunayPointIndices[1]) do
    begin
      TempValue := AVertex.FormingDelaunayPointIndices[0];
      AVertex.FormingDelaunayPointIndices[0] := AVertex.FormingDelaunayPointIndices[1];
      AVertex.FormingDelaunayPointIndices[1] := AVertex.FormingDelaunayPointIndices[2];
      AVertex.FormingDelaunayPointIndices[2] := TempValue;
      TempValue := AVertex.NeighborVoronoiVertexIndices[0];
      AVertex.NeighborVoronoiVertexIndices[0] := AVertex.NeighborVoronoiVertexIndices[1];
      AVertex.NeighborVoronoiVertexIndices[1] := AVertex.NeighborVoronoiVertexIndices[2];
      AVertex.NeighborVoronoiVertexIndices[2] := TempValue;
    end;
    TempValue := AVertex.NeighborVoronoiVertexIndices[2];
    AVertex.NeighborVoronoiVertexIndices[2] := AVertex.NeighborVoronoiVertexIndices[1];
    AVertex.NeighborVoronoiVertexIndices[1] := AVertex.NeighborVoronoiVertexIndices[0];
    AVertex.NeighborVoronoiVertexIndices[0] := TempValue;
    APoint2d := Circumcenter(LocalTriangle);
    AVertex.x := APoint2d.x;
    AVertex.y := APoint2d.y;
    APointIndex := AVertex.FormingDelaunayPointIndices[0];
    AVertex.SquareOfRadius := Sqr(XArray[APointIndex] - AVertex.x) + Sqr(YArray[APointIndex] - AVertex.y);
    AVoronoiDiagram.VoronoiVertices[TriangleIndex] := AVertex;
  end;
end;

{TVertexList}

constructor TVertexList.Create;
begin
  FItems:= nil;
  FCount:= 0;
  FCapacity:= 0;
end;

procedure TVertexList.DeleteLast;
begin
  Dec(FCount);
end;

destructor TVertexList.Destroy;
begin
  SetLength(FItems,0);
  inherited Destroy;
end;

function TVertexList.GetItem(index : longint) : TVoronoiVertex;
begin
  Result:= FItems[index];
end;

procedure TVertexList.Grow;
var
  Increment: integer;
begin
  Increment := Max(1024, FCapacity div 4);
  FCapacity:= FCapacity+Increment;
  SetLength(FItems,FCapacity);
end;

procedure TVertexList.Add(item : TVoronoiVertex);
begin
  if FCount = FCapacity then
  begin
    Grow;
  end;
  FItems[FCount]:= item;
  Inc(FCount);
end;

procedure TVertexList.SetCapacity(const Value: integer);
begin
  FCapacity := Value;
  SetLength(FItems, FCapacity);
end;

procedure TVertexList.SetCount(const Value: longint);
begin
  if Capacity < Value then
  begin
    Capacity := Value
  end;
  FCount := Value;
end;

procedure TVertexList.SetItem(index : longint; item : TVoronoiVertex);
//var
//  v : TVoronoiVertex;
begin
//  epsilon := 1E-12;
//  v := Items[index];
  {
  NeedToUpdateTree := (Abs(v.x - item.x) > epsilon) or (Abs(v.y - item.y) > epsilon);
  if NeedToUpdateTree then
  begin
    tree_index := FindTreeNode(index);
    if (NoElements = 5) then
    begin
      epsilon := 1;
    end;
    DelTreeNode(tree_index);
  end;
  }
  if index>=FCapacity then
  begin
    Capacity := index+1;
  end;
  FItems[index]:= item;
end;

//function TVertexList.Copy : TVertexList;
//var
//  i : longint;
//begin
//  Result:= TVertexList.Create;
//  Result.Capacity := Count;
//  for i:= 0 to Count-1 do
//  begin
//    Result.Add(Items[i]);
//  end;
//end;


{TPointList}

constructor TPointList.Create;
begin
  FItems:= nil;
  FCount:= 0;
  FCapacity:= 0;
end;

procedure TPointList.DeleteLast;
begin
  Dec(FCount);
end;

destructor TPointList.Destroy;
begin
  SetLength(FItems,0);
  inherited Destroy;
end;

function TPointList.Last : TDelaunayPoint;
begin
  Result:= FItems[FCount-1];
end;

procedure TPointList.SetCapacity(const Value: integer);
begin
  FCapacity := Value;
  SetLength(FItems, FCapacity);
end;

procedure TPointList.SetCount(const Value: longint);
begin
  if Capacity < Value then
  begin
    Capacity := Value;
  end;
  FCount := Value;
end;

procedure TPointList.SetItem(index: Integer; const Value: TDelaunayPoint);
begin
  FItems[index] := Value;
end;

function TPointList.GetItem(index : longint) : TDelaunayPoint;
begin
  Result := FItems[index];
end;

procedure TPointList.Grow;
var
  Increment: integer;
begin
  Increment := Max(1024, FCapacity div 4);
  FCapacity:= FCapacity+Increment;
  SetLength(FItems,FCapacity);
end;

procedure TPointList.Add(item : TDelaunayPoint);
begin
  if FCount = FCapacity then
  begin
    Grow;
  end;
  FItems[FCount]:= item;
  Inc(FCount);
end;

//function TPointList.Copy : TPointList;
//var
//  i : longint;
//begin
//  result:= TPointList.Create;
//  result.Capacity := Count;
//  for i:= 0 to Count-1 do
//  begin
//    result.Add(Items[i]);
//  end;
//end;

{TVoronoiDiagram}

constructor TVoronoiDiagram.Create(XLow,YLow,XHigh,YHigh : extended; Epsilon: extended = 1E-5);
var
  p1, p2, p3, p4 : TDelaunayPoint;
  v1, v2 : TVoronoiVertex;
begin
  FEpsilon:= Epsilon;
  FQuadTree := TRbwQuadTree.Create(nil);
  FQuadTree.XMax := XHigh;
  FQuadTree.XMin := XLow;
  FQuadTree.YMax := YHigh;
  FQuadTree.YMin := YLow;
  FVerticies:= TVertexList.Create;
  FPoints:= TPointList.Create;
  p1.x:= XLow;
  p1.y:= YLow;
  p2.x:= XHigh;
  p2.y:= YLow;
  p3.x:= XHigh;
  p3.y:= YHigh;
  p4.x:= XLow;
  p4.y:= YHigh;
  FPoints.Add(p1);
  FPoints.Add(p2);
  FPoints.Add(p3);
  FPoints.Add(p4);
  v1.x:= (XLow+XHigh)/2;
  v1.y:= (YLow+YHigh)/2;
  v1.SquareOfRadius:= (Sqr(XHigh-XLow) + Sqr(YHigh-YLow))/4;
  v1.FormingDelaunayPointIndices[0]:= 0;
  v1.FormingDelaunayPointIndices[1]:= 1;
  v1.FormingDelaunayPointIndices[2]:= 2;
  v1.NeighborVoronoiVertexIndices[0]:= -1;
  v1.NeighborVoronoiVertexIndices[1]:= -1;
  v1.NeighborVoronoiVertexIndices[2]:= 1;
  v2.x:= v1.x;
  v2.y:= v1.y;
  v2.SquareOfRadius:= v1.SquareOfRadius;
  v2.FormingDelaunayPointIndices[0]:= 0;
  v2.FormingDelaunayPointIndices[1]:= 2;
  v2.FormingDelaunayPointIndices[2]:= 3;
  v2.NeighborVoronoiVertexIndices[0]:= 0;
  v2.NeighborVoronoiVertexIndices[1]:= -1;
  v2.NeighborVoronoiVertexIndices[2]:= -1;
  FVerticies.Add(v1);
  FVerticies.Add(v2);
  xmin:= XLow;
  xmax:= XHigh;
  ymin:= YLow;
  ymax:= YHigh;
  Rxmin:= xmin;
  Rxmax:= xmax;
  Rymin:= ymin;
  Rymax:= ymax;
end;

destructor TVoronoiDiagram.Destroy;
begin
  FQuadTree.Free;
  FVerticies.Free;
  FPoints.Free;
  inherited Destroy;
end;

//function TVoronoiDiagram.Copy : TVoronoiDiagram;
//var
//  VD : TVoronoiDiagram;
//begin
//  VD:= TVoronoiDiagram.Create(-1,1,-1, 1);
//  VD.FVerticies.Free;
//  VD.FPoints.Free;
//  VD.FVerticies:= FVerticies.Copy;
//  VD.FPoints:= FPoints.Copy;
//  Result:= VD;
//end;

procedure TVoronoiDiagram.AddPoint(DelaunayPoint : TDelaunayPoint);
var
  VerticesToDelete : TLongIntList;
  NewVertices : TVertexList;
  Index: Integer;
  VVertex: TVoronoiVertex;
  X: Extended;
  Y: Extended;
  Data: Pointer;
  Position: integer;
begin
  FPoints.Add(DelaunayPoint);
  if DelaunayPoint.x<xmin then xmin:= DelaunayPoint.x;
  if DelaunayPoint.x>xmax then xmax:= DelaunayPoint.x;
  if DelaunayPoint.y<ymin then ymin:= DelaunayPoint.y;
  if DelaunayPoint.y>ymax then ymax:= DelaunayPoint.y;
  VerticesToDelete := TLongIntList.Create;
  try
    AssignVerticesToDelete(VerticesToDelete);
    NewVertices := TVertexList.Create;
    try
      AssignNewVertices(VerticesToDelete, NewVertices);
      for Index := 0 to VerticesToDelete.Count - 1 do
      begin
        VVertex := VoronoiVertices[VerticesToDelete[Index]];
        X := VVertex.x;
        Y := VVertex.y;

        Data := FQuadTree.NearestPointsFirstData(X, Y);
        if (X = VVertex.x) and (Y = VVertex.y) then
        begin
          FQuadTree.RemovePoint(X, Y, Data)
        end;
      end;
      for Index := 0 to VerticesToDelete.Count - 1 do
      begin
        VVertex := NewVertices[Index];
        FQuadTree.AddPoint(VVertex.x, VVertex.y,
          Pointer(VerticesToDelete[Index]));
      end;
      Position := VertexCount;
      for Index := VerticesToDelete.Count to NewVertices.Count - 1 do
      begin
        VVertex := NewVertices[Index];
        FQuadTree.AddPoint(VVertex.x, VVertex.y,
          Pointer(Position));
        Inc(Position);
      end;
      AddNewVertices(NewVertices, VerticesToDelete);
    finally
      NewVertices.Free;
    end;
  finally
    VerticesToDelete.Free;
  end;
end;

procedure TVoronoiDiagram.CheckNewPoint(DelaunayPoint: TDelaunayPoint;
      NewVertexPositions: TLongIntList; New_Vertices: TVertexList);
begin
  FPoints.Add(DelaunayPoint);
  if DelaunayPoint.x<xmin then xmin:= DelaunayPoint.x;
  if DelaunayPoint.x>xmax then xmax:= DelaunayPoint.x;
  if DelaunayPoint.y<ymin then ymin:= DelaunayPoint.y;
  if DelaunayPoint.y>ymax then ymax:= DelaunayPoint.y;
//  if VertexNumber < 0 then VertexNumber:= StartingVertex;
  AssignVerticesToDelete(NewVertexPositions);
  AssignNewVertices(NewVertexPositions, New_Vertices);
  TestNewVertices(New_Vertices, NewVertexPositions);
  FPoints.DeleteLast;
end;

procedure TVoronoiDiagram.SetPoint(Index: integer; const Value: TDelaunayPoint);
begin
  FPoints.Items[Index] := Value;
end;

procedure TVoronoiDiagram.SetPointCount(const Value: integer);
begin
  FPoints.Count := Value;
end;

procedure TVoronoiDiagram.SetVertexCount(const Value: integer);
begin
  FVerticies.Count := Value;
end;

procedure TVoronoiDiagram.SetVoronoiVertex(Index: integer;
  const Value: TVoronoiVertex);
begin
  FVerticies.Items[Index] := Value;
  FQuadTree.AddPoint(Value.x, Value.y, Pointer(Index));
end;

function TVoronoiDiagram.StartingVertex : longint;
var
  found_vertex : boolean;
  SqrDistPointToVertex : extended;
  LastDelaunayPoint : TDelaunayPoint;
  VoronoiVertex : TVoronoiVertex;
begin
  result:= FVerticies.Count-1;
  LastDelaunayPoint:= FPoints.Last;
  found_vertex:= false;
  while (not found_vertex) do
  begin
    Assert(result >= 0);
    VoronoiVertex:= FVerticies.Items[result];
    SqrDistPointToVertex:= Sqr(VoronoiVertex.x-LastDelaunayPoint.x)
      + Sqr(VoronoiVertex.y-LastDelaunayPoint.y);
    found_vertex:= SqrDistPointToVertex<(VoronoiVertex.SquareOfRadius+FEpsilon);
    if (not found_vertex) then Dec(result);
  end;
end;

function TVoronoiDiagram.StartingVertex3 : longint;
var
  LastDelaunayPoint: TDelaunayPoint;
  X: Extended;
  Y: Extended;
  VoronoiVertex: TVoronoiVertex;
  SqrDistPointToVertex: Extended;
  found_vertex: Boolean;
begin
  if FQuadTree.Count > 0 then
  begin
    LastDelaunayPoint:= FPoints.Last;
    X := LastDelaunayPoint.x;
    Y := LastDelaunayPoint.y;
    result := Integer(FQuadTree.NearestPointsFirstData(X, Y));
    VoronoiVertex:= FVerticies.Items[result];
    SqrDistPointToVertex:= Sqr(VoronoiVertex.x-LastDelaunayPoint.x)
      + Sqr(VoronoiVertex.y-LastDelaunayPoint.y);
    found_vertex:= SqrDistPointToVertex<(VoronoiVertex.SquareOfRadius+FEpsilon);
    if (not found_vertex) then
    begin
      result := StartingVertex2;
    end;
  end
  else
  begin
    result := StartingVertex2;
  end;
end;


function TVoronoiDiagram.StartingVertex2 : longint;
var
  found_vertex : boolean;
  SqrDistPointToVertex : extended;
  LastDelaunayPoint : TDelaunayPoint;
  VoronoiVertex : TVoronoiVertex;
  PriorResult: Integer;
  PriorDistance: Extended;
  VIndex: Integer;
  NextVertexIndex: integer;
  NextVertex: TVoronoiVertex;
//  P1: TPoint2D;
//  Segment: TSegment2D;
//  CloseX: TFloat;
//  CloseY: TFloat;
  ClosePoint: TPoint2D;
  TestedVertices: TLongIntList;
  PriorVertex: TVoronoiVertex;
  FoundNextPoint: Boolean;
//  PriorClosestDistance: extended;
begin
  TestedVertices := TLongIntList.Create;
  try

    PriorVertex.x := 0;
    PriorVertex.y := 0;
    result:= FVerticies.Count-1;
    LastDelaunayPoint:= FPoints.Last;
    PriorResult := -1;
  //  PriorClosestDistance := -1;
  //  P1.x := LastDelaunayPoint.x;
  //  P1.y := LastDelaunayPoint.y;
    found_vertex:= false;
    while (not found_vertex) do
    begin
      VoronoiVertex:= FVerticies.Items[result];
      SqrDistPointToVertex:= Sqr(VoronoiVertex.x-LastDelaunayPoint.x)
        + Sqr(VoronoiVertex.y-LastDelaunayPoint.y);
      found_vertex:= SqrDistPointToVertex<(VoronoiVertex.SquareOfRadius+FEpsilon);
      if (not found_vertex) then
      begin
        if PriorResult >= 0 then
        begin
          ClosePoint := ClosestPointOnSegmentFromPoint(VoronoiVertex.x, VoronoiVertex.y,
            PriorVertex.x, PriorVertex.y, LastDelaunayPoint.x, LastDelaunayPoint.y);
          PriorDistance:= Sqr(ClosePoint.x-LastDelaunayPoint.x)
            + Sqr(ClosePoint.y-LastDelaunayPoint.y);
        end
        else
        begin
          PriorDistance := SqrDistPointToVertex;
        end;
        FoundNextPoint := False;
  //      Segment[1].x := VoronoiVertex.x;
  //      Segment[1].y := VoronoiVertex.y;
        for VIndex := 0 to 2 do
        begin
          NextVertexIndex := VoronoiVertex.NeighborVoronoiVertexIndices[VIndex];
          if (NextVertexIndex <> result)
            and (NextVertexIndex <> PriorResult)
            and (NextVertexIndex >= 0)
            and (TestedVertices.IndexOf(NextVertexIndex) < 0) then
          begin
            NextVertex := FVerticies.Items[NextVertexIndex];

            ClosePoint := ClosestPointOnSegmentFromPoint(VoronoiVertex.x, VoronoiVertex.y,
              NextVertex.x, NextVertex.y, LastDelaunayPoint.x, LastDelaunayPoint.y);
            SqrDistPointToVertex:= Sqr(ClosePoint.x-LastDelaunayPoint.x)
              + Sqr(ClosePoint.y-LastDelaunayPoint.y);
            if SqrDistPointToVertex <= PriorDistance then
            begin
              PriorVertex := VoronoiVertex;
              TestedVertices.Add(result);
              PriorResult := result;
              result := NextVertexIndex;
              FoundNextPoint := True;
              break;
            end;
          end;
        end;
        if not FoundNextPoint then
        begin
          result := StartingVertex;
          Exit;
        end;
      end;
    end;
  finally
    TestedVertices.Free;
  end;
end;

procedure TVoronoiDiagram.AssignVerticesToDelete(VerticesToDelete : TLongIntList);
var
  VertexNumber : longint;
  VerticesTested : TLongIntList;
  NeighborIndex,NeighborVertexNumber,VertexToTextIndex : integer;
  LastDelaunayPoint : TDelaunayPoint;
  AVertex,NeighborVertex : TVoronoiVertex;
  SqrPointVertexDistance : extended;
begin
  VerticesTested:= TLongIntList.Create;
  try
    // Find a point to delete.
    VertexNumber := StartingVertex3;
    VerticesToDelete.Add(VertexNumber);
    VerticesTested.Add(VertexNumber);

    LastDelaunayPoint := FPoints.Last;
    VertexToTextIndex:= 0;
    // Check whether any of the neighboring vertices are closer to
    // the last Delaunay then to any of their forming points.
    // If so, add those vertex positions to VerticesToDelete.
    while (VertexToTextIndex < VerticesToDelete.Count) do
    begin
      VertexNumber:= VerticesToDelete.Items[VertexToTextIndex];
      AVertex:= FVerticies.Items[VertexNumber];
      for NeighborIndex:= 0 to 2 do
      begin
        NeighborVertexNumber:= AVertex.NeighborVoronoiVertexIndices[NeighborIndex];
        if ((NeighborVertexNumber>=0)
          and (VerticesTested.IndexOf(NeighborVertexNumber)<0)) then
        begin
          NeighborVertex:= FVerticies.Items[NeighborVertexNumber];
          SqrPointVertexDistance:= Sqr(LastDelaunayPoint.x-NeighborVertex.x)+Sqr(LastDelaunayPoint.y-NeighborVertex.y);
          if SqrPointVertexDistance<(NeighborVertex.SquareOfRadius+FEpsilon) then
          begin
            VerticesToDelete.Add(NeighborVertexNumber);
          end;
          VerticesTested.Add(NeighborVertexNumber);
        end;
      end;
      VertexToTextIndex:= VertexToTextIndex+1;
//      Application.ProcessMessages;
    end;
  finally
    VerticesTested.Free;
  end;
end;

procedure TVoronoiDiagram.AssignNewVertices(VerticesToDelete : TLongIntList;
      NewVertices : TVertexList);
var
  VertexToDelete, NewVertex : TVoronoiVertex;
  Vertexindex, VertexToDeleteIndex, NeighborIndex, PointIndex : integer;
  DelaunayTriangle : array[0..2] of TDelaunayPoint;
  x1,x2,x3,y1,y2,y3,c1,c2 : extended;
begin
  for VertexToDeleteIndex:=0 to VerticesToDelete.Count-1 do
  begin
    Vertexindex:= VerticesToDelete.Items[VertexToDeleteIndex];
    VertexToDelete:= FVerticies.Items[Vertexindex];
    for NeighborIndex:= 0 to 2 do
    begin
      if (VerticesToDelete.IndexOf(
        VertexToDelete.NeighborVoronoiVertexIndices[NeighborIndex])<0) then
      begin
        NewVertex.FormingDelaunayPointIndices[0]:=
          VertexToDelete.FormingDelaunayPointIndices[NeighborIndex];
        if (NeighborIndex=2) then
        begin
          NewVertex.FormingDelaunayPointIndices[1]:= VertexToDelete.FormingDelaunayPointIndices[0]
        end
        else
        begin
          NewVertex.FormingDelaunayPointIndices[1]:=
            VertexToDelete.FormingDelaunayPointIndices[NeighborIndex+1];
        end;
        NewVertex.FormingDelaunayPointIndices[2]:= FPoints.Count-1;
        NewVertex.NeighborVoronoiVertexIndices[0] :=
          VertexToDelete.NeighborVoronoiVertexIndices[NeighborIndex];
        for PointIndex:= 0 to 2 do
        begin
          DelaunayTriangle[PointIndex]:=
            FPoints.Items[NewVertex.FormingDelaunayPointIndices[PointIndex]];
        end;
        x1:= DelaunayTriangle[0].x;
        x2:= DelaunayTriangle[1].x;
        x3:= DelaunayTriangle[2].x;
        y1:= DelaunayTriangle[0].y;
        y2:= DelaunayTriangle[1].y;
        y3:= DelaunayTriangle[2].y;
        if ((x1=x2) and (x1=x3)) or ((y1=y2) and (y1=y3)) then
        begin
          NewVertex.x := x2;
          NewVertex.y := y2;
        end
        else
        begin
        c1:= Sqr(x1)-Sqr(x2)+Sqr(y1)-Sqr(y2);
        c2:= Sqr(x2)-Sqr(x3)+Sqr(y2)-Sqr(y3);
        NewVertex.x:= 0.5*(c1*(y2-y3)-c2*(y1-y2))/((x1-x2)*(y2-y3)-(x2-x3)*(y1-y2));
        NewVertex.y:= 0.5*(c1*(x2-x3)-c2*(x1-x2))/((y1-y2)*(x2-x3)-(y2-y3)*(x1-x2));
        end;
        NewVertex.SquareOfRadius:= Sqr(x1-NewVertex.x)+Sqr(y1-NewVertex.y);
        NewVertices.Add(NewVertex);
        if NewVertex.x<xmin then xmin:= NewVertex.x;
        if NewVertex.x>xmax then xmax:= NewVertex.x;
        if NewVertex.y<ymin then ymin:= NewVertex.y;
        if NewVertex.y>ymax then ymax:= NewVertex.y;
      end; {if}
    end; {for}
  end; {for}
end;

procedure TVoronoiDiagram.AddNewVertices(NewVertices : TVertexList;
      VerticesToDelete : TLongIntList);
var
  vertex_mapping : TLongintList;
//  InnerVertexMappingIndex, OuterVertexNumber,
//  neigh_vertex_no, p1, p2, q1, q2, InnerVertexNumber : integer;
//  OuterVertex, InnerVertex : TVoronoiVertex;
//  NewVertexIndex: Integer;
//  OuterVertexMappingIndex: Integer;
begin
  vertex_mapping := TLongIntList.Create;
  ReplaceExistingVertices(NewVertices, VerticesToDelete, vertex_mapping);
  AddAdditionaVerticesAtEnd(vertex_mapping, NewVertices, VerticesToDelete);
  UpdateLinkagesForNewVertices(nil, nil, vertex_mapping, NewVertices);
//  for NewVertexIndex := 0 to NewVertices.Count-1 do
//  begin
//    OuterVertexNumber := vertex_mapping.Items[NewVertexIndex];
//    OuterVertex := FVerticies.Items[OuterVertexNumber];
//    p1 := OuterVertex.forming_points[0];
////    p2 := v.forming_points[1];
//    neigh_vertex_no := OuterVertex.neigh_vertices[0];
//    if neigh_vertex_no >= 0 then
//    begin
//      InnerVertex := FVerticies.Items[neigh_vertex_no];
//      if InnerVertex.forming_points[0] = p1 then
//        InnerVertex.neigh_vertices[2] := OuterVertexNumber
//      else if InnerVertex.forming_points[1] = p1 then
//        InnerVertex.neigh_vertices[0] := OuterVertexNumber
//      else
//        InnerVertex.neigh_vertices[1] := OuterVertexNumber;
//      FVerticies.Items[neigh_vertex_no] := InnerVertex;
//    end;
//  end;
  UpdateRemainingLinkages(vertex_mapping, nil, nil);

//  for OuterVertexMappingIndex := 0 to vertex_mapping.Count-1 do
//  begin
//    OuterVertexNumber := vertex_mapping.Items[OuterVertexMappingIndex];
//    OuterVertex := FVerticies.Items[OuterVertexNumber];
//    p1 := OuterVertex.forming_points[1];
//    p2 := OuterVertex.forming_points[2];
//    InnerVertexMappingIndex := 0;
//    while InnerVertexMappingIndex <= vertex_mapping.Count-1 do
//    begin
//      if OuterVertexMappingIndex <> InnerVertexMappingIndex then
//      begin
//        InnerVertexNumber := vertex_mapping.Items[InnerVertexMappingIndex];
//        InnerVertex := FVerticies.Items[InnerVertexNumber];
//        q1 := InnerVertex.forming_points[2];
//        q2 := InnerVertex.forming_points[0];
//        if (p1=q2) and (p2=q1) then
//        begin
//          OuterVertex.neigh_vertices[1] := InnerVertexNumber;
//          InnerVertex.neigh_vertices[2] := OuterVertexNumber;
//          FVerticies.Items[OuterVertexNumber] := OuterVertex;
//          FVerticies.Items[InnerVertexNumber] := InnerVertex;
//        end;
//      end;
//      inc(InnerVertexMappingIndex);
//    end;
//  end;
  vertex_mapping.Free;
end;

procedure TVoronoiDiagram.TestNewVertices(NewVertices: TVertexList;
      NewVertexPositions: TLongIntList);
var
  vertex_mapping : TLongintList;
  VertexToDeleteIndex, vertex_no : integer;
  v : TVoronoiVertex;
  VerticesToRestore: TVertexList;
  VCount: Integer;
  Position: Integer;
  VertexIndicesToRestore: TLongintList;
  NewVertexIndex: Integer;
begin
  VertexIndicesToRestore := TLongIntList.Create;
  VerticesToRestore := TVertexList.Create;
  vertex_mapping := TLongIntList.Create;
  try
    for VertexToDeleteIndex:= 0 to NewVertexPositions.Count-1 do
    begin
      vertex_no:= NewVertexPositions.Items[VertexToDeleteIndex];
      v := FVerticies.Items[vertex_no];
      VerticesToRestore.Add(v);
      VertexIndicesToRestore.Add(vertex_no);
    end;
    ReplaceExistingVertices(NewVertices, NewVertexPositions, vertex_mapping);
//    for VertexToDeleteIndex:= 0 to VerticesToDelete.Count-1 do
//    begin
//      vertex_no:= VerticesToDelete.Items[VertexToDeleteIndex];
//      v:= NewVertices.Items[VertexToDeleteIndex];
//      FVerticies.Items[vertex_no] := v;
//      vertex_mapping.Add(vertex_no);
//    end;
    VCount := FVerticies.Count;
    AddAdditionaVerticesAtEnd(vertex_mapping, NewVertices, NewVertexPositions);
//    for NewVertexIndex:= VerticesToDelete.Count to NewVertices.Count-1 do
//    begin
//      v:= NewVertices.Items[NewVertexIndex];
//      FVerticies.Add(v);
//      vertex_no:= FVerticies.Count-1;
//      vertex_mapping.Add(vertex_no);
//    end;
    UpdateLinkagesForNewVertices(VertexIndicesToRestore, VerticesToRestore, vertex_mapping, NewVertices);
    UpdateRemainingLinkages(vertex_mapping, VertexIndicesToRestore, VerticesToRestore);
    for VertexToDeleteIndex:= 0 to NewVertexPositions.Count-1 do
    begin
      vertex_no:= NewVertexPositions.Items[VertexToDeleteIndex];
      v:= FVerticies.Items[vertex_no];
      NewVertices.Items[VertexToDeleteIndex] := v;
    end;
    Position := NewVertexPositions.Count;
    for NewVertexIndex:= VCount to FVerticies.Count-1 do
    begin
      v:= FVerticies.Items[NewVertexIndex];
      NewVertices.Items[Position] := v;
      Inc(Position);
      NewVertexPositions.Add(NewVertexIndex);
    end;
    for VertexToDeleteIndex:= VerticesToRestore.Count-1 downto 0 do
    begin
      vertex_no:= VertexIndicesToRestore.Items[VertexToDeleteIndex];
      v:= VerticesToRestore.Items[VertexToDeleteIndex];
      FVerticies.Items[vertex_no] := v;
    end;
    while FVerticies.Count > VCount do
    begin
      FVerticies.DeleteLast
    end;
  finally
    VerticesToRestore.Free;
    vertex_mapping.Free;
    VertexIndicesToRestore.Free;
  end;
end;

procedure TVoronoiDiagram.UpdateRemainingLinkages(vertex_mapping: TLongIntList;
      VertexIndicesToRestore: TLongIntList; VerticesToRestore: TVertexList);
var
  OuterVertexMappingIndex: Integer;
  OuterVertexNumber: Integer;
  OuterVertex: TVoronoiVertex;
  p1: Integer;
  p2: Integer;
  InnerVertexMappingIndex: Integer;
  InnerVertexNumber: Integer;
  InnerVertex: TVoronoiVertex;
  q1: Integer;
  q2: Integer;
begin
  for OuterVertexMappingIndex := 0 to vertex_mapping.Count - 1 do
  begin
    OuterVertexNumber := vertex_mapping.Items[OuterVertexMappingIndex];
    OuterVertex := FVerticies.Items[OuterVertexNumber];
    p1 := OuterVertex.FormingDelaunayPointIndices[1];
    p2 := OuterVertex.FormingDelaunayPointIndices[2];
    InnerVertexMappingIndex := 0;
    while InnerVertexMappingIndex <= vertex_mapping.Count - 1 do
    begin
      if OuterVertexMappingIndex <> InnerVertexMappingIndex then
      begin
        InnerVertexNumber := vertex_mapping.Items[InnerVertexMappingIndex];
        InnerVertex := FVerticies.Items[InnerVertexNumber];
        q1 := InnerVertex.FormingDelaunayPointIndices[2];
        q2 := InnerVertex.FormingDelaunayPointIndices[0];
        if (p1 = q2) and (p2 = q1) then
        begin
          OuterVertex.NeighborVoronoiVertexIndices[1] := InnerVertexNumber;
          InnerVertex.NeighborVoronoiVertexIndices[2] := OuterVertexNumber;
          if VerticesToRestore <> nil then
          begin
            VerticesToRestore.Add(FVerticies.Items[OuterVertexNumber]);
            VertexIndicesToRestore.Add(OuterVertexNumber);
            VerticesToRestore.Add(FVerticies.Items[InnerVertexNumber]);
            VertexIndicesToRestore.Add(InnerVertexNumber);
          end;
          FVerticies.Items[OuterVertexNumber] := OuterVertex;
          FVerticies.Items[InnerVertexNumber] := InnerVertex;
        end;
      end;
      inc(InnerVertexMappingIndex);
    end;
  end;
end;

procedure TVoronoiDiagram.UpdateLinkagesForNewVertices(
      VertexIndicesToRestore: TLongIntList; VerticesToRestore: TVertexList;
      vertex_mapping: TLongIntList; NewVertices: TVertexList);
var
  NewVertexIndex: Integer;
  VertexNumber: Integer;
  Vertex: TVoronoiVertex;
  p1: Integer;
  NeighborVertexNumber: Integer;
  NeighborVertex: TVoronoiVertex;
begin
  for NewVertexIndex := 0 to NewVertices.Count - 1 do
  begin
    VertexNumber := vertex_mapping.Items[NewVertexIndex];
    Vertex := FVerticies.Items[VertexNumber];
    p1 := Vertex.FormingDelaunayPointIndices[0];
    //    p2 := Vertex.forming_points[1];
    NeighborVertexNumber := Vertex.NeighborVoronoiVertexIndices[0];
    if NeighborVertexNumber >= 0 then
    begin
      NeighborVertex := FVerticies.Items[NeighborVertexNumber];
      if VerticesToRestore <> nil then
      begin
        VerticesToRestore.Add(NeighborVertex);
        VertexIndicesToRestore.Add(NeighborVertexNumber);
      end;
      if NeighborVertex.FormingDelaunayPointIndices[0] = p1 then
        NeighborVertex.NeighborVoronoiVertexIndices[2] := VertexNumber
      else if NeighborVertex.FormingDelaunayPointIndices[1] = p1 then
        NeighborVertex.NeighborVoronoiVertexIndices[0] := VertexNumber
      else
        NeighborVertex.NeighborVoronoiVertexIndices[1] := VertexNumber;
      FVerticies.Items[NeighborVertexNumber] := NeighborVertex;
    end;
  end;
end;

procedure TVoronoiDiagram.AddAdditionaVerticesAtEnd(vertex_mapping: TLongIntList;
      NewVertices: TVertexList; VerticesToDelete: TLongIntList);
var
  VertexToDeleteIndex: Integer;
  Vertex: TVoronoiVertex;
  VertexNumber: Integer;
begin
  for VertexToDeleteIndex := VerticesToDelete.Count to NewVertices.Count - 1 do
  begin
    Vertex := NewVertices.Items[VertexToDeleteIndex];
    FVerticies.Add(Vertex);
    VertexNumber := FVerticies.Count - 1;
    vertex_mapping.Add(VertexNumber);
  end;
end;

procedure TVoronoiDiagram.ReplaceExistingVertices(NewVertices: TVertexList;
      VerticesToDelete: TLongIntList; vertex_mapping: TLongIntList);
var
  Vertex: TVoronoiVertex;
  VertexNumber: Integer;
  VertexToDeleteIndex: Integer;
begin
  for VertexToDeleteIndex := 0 to VerticesToDelete.Count - 1 do
  begin
    VertexNumber := VerticesToDelete.Items[VertexToDeleteIndex];
    Vertex := NewVertices.Items[VertexToDeleteIndex];
    FVerticies.Items[VertexNumber] := Vertex;
    vertex_mapping.Add(VertexNumber);
  end;
end;

function TVoronoiDiagram.GetPoint(Index: integer): TDelaunayPoint;
begin
  result := FPoints.Items[Index];
end;

function TVoronoiDiagram.GetPointCount: integer;
begin
  result := FPoints.Count;
end;

function TVoronoiDiagram.GetVertexCount: integer;
begin
  result := FVerticies.Count;
end;

function TVoronoiDiagram.GetVoronoiVertex(Index: integer): TVoronoiVertex;
begin
  result := FVerticies.Items[Index];
end;

constructor TLongIntList.Create;
begin
  FItems:= nil;
  FCount:= 0;
  FCapacity:= 0;
end;

destructor TLongIntList.Destroy;
begin
  SetLength(FItems, 0);
  inherited Destroy;
end;

function TLongIntList.GetItems(index : Longint) : Longint;
begin
  Result:= FItems[index];
end;

function TLongIntList.IndexOf(item : Longint) : Longint;
var
  Index: Integer;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    if Items[index] = item then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

procedure TLongIntList.Grow;
var
  Increment: integer;
begin
  Increment := Max(1024, FCapacity div 4);
  FCapacity:= FCapacity+Increment;
  SetLength(FItems,FCapacity);
end;

procedure TLongIntList.Add(item : Longint);
begin
  if FCount=FCapacity then
  begin
    Grow;
  end;
  FItems[FCount]:= item;
  Inc(FCount);
end;

end.
