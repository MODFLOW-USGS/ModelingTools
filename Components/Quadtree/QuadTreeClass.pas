{@name registers @link(TRbwQuadTree) which is used to
quickly retrieve data by their X and Y coordinates.

@author(Richard B. Winston <rbwinst@usgs.gov>).

This file is in the public domain.
}
unit QuadTreeClass;

interface

uses Types, SysUtils, Classes, contnrs;

type
  // @name is used in defining TQtreeNode.@link(TQtreeNode.FChildren).
  TNorthOrSouth = (North, South);
  // @name is used in defining TQtreeNode.@link(TQtreeNode.FChildren).
  TEastOrWest = (East, West);

  {@abstract(@name is used to store data at a particular location.
   See TQtreeNode.@link(TQtreeNode.FPts).)

  @longcode(#
  TQPoint = record
    Data : TList;
    X: double;
    Y: double;
  end;
  #)
  }
  TQPoint = record
    // @name is a list of the data at @link(X) and @link(Y).
    Data: TList;
    // @name is the X coordinate of the data.
    X: double;
    // @name is the Y coordinate of the data.
    Y: double;
  end;

  // See @link(TQPoint).
  PQPoint = ^TQPoint;

  {@longcode(#TPointerArray = array of Pointer;#)}
  TPointerArray = array of Pointer;

  {@abstract(@name records contain the X and Y coordinates of a location in a
  @link(TRbwQuadTree), the distance from that point to a location for which a
  search was performed, and an array of pointers to the data associated
  with the location.)

  @longcode(#
  TQuadPoint =  record
    Data : TPointerArray;
    Distance : double;
    X: double;
    Y: double;
  end;
  #)
  }
  TQuadPoint = record
    // @name is an array of pointers to the data associated
    // with the location.
    Data: TPointerArray;
    // @name is the distance from this location to the
    // one for which a search was performed..
    Distance: double;
    // @name is the X coordinate of the data.
    X: double;
    // @name is the Y coordinate of the data.
    Y: double;
  end;

  {@longcode(#TQuadPointArray = array of TQuadPoint;#)}
  TQuadPointArray = array of TQuadPoint;

  {@abstract(@name records contain the X and Y coordinates of a location
  in a @link(TRbwQuadTree), and an array of pointers to the data associated
  with the location. These points are all located inside the extent of a
  @link(T2DBlock) that was used in a search.)

  @longcode(#
  TQuadPointInRegion =  record
    Data : TPointerArray;
    X: double;
    Y: double;
  end;
  #)
  }
  TQuadPointInRegion = record
    // @name is an array of pointers to the data associated
    // with the location.
    Data: TPointerArray;
    // @name is the X coordinate of the data.
    X: double;
    // @name is the Y coordinate of the data.
    Y: double;
  end;

  {@longcode(#TQuadPointInRegionArray = array of TQuadPointInRegion;#)}
  TQuadPointInRegionArray = array of TQuadPointInRegion;

  // See TQtreeNode.@link(TQtreeNode.ExpandBounds).
  TExpandDirection = (edNorth, edSouth, edEast, edWest);

  {@abstract(@name delineates a region of space. @name's are used to perform
  a search for locations inside a @link(TRbwQuadTree).)

  @longcode(#
  T3DBlock = record
    XMax : double;
    XMin : double;
    YMax : double;
    YMin : double;
  end;
  #)

  }
  T2DBlock = record
    // @name is the maximum coordinate
    // in the X direction to be included in the search.
    XMax: double;
    // @name is the minimum coordinate
    // in the X direction to be included in the search.
    XMin: double;
    // @name is the maximum coordinate
    // in the Y direction to be included in the search.
    YMax: double;
    // @name is the minimum coordinate
    // in the Y direction to be included in the search.
    YMin: double;
  end;

  //  @abstract(@name is a class for exceptions raised by @link(TRbwQuadTree).)
  EQTreeError = class(Exception);

  TRbwQuadTree = class;

  // @abstract(@name does the most of the real work of a @link(TRbwQuadTree).)
  TQtreeNode = class(TObject)
  private
    // @longcode(#FNumPts: Integer;#)
    // @name is the number of locations stored by this @classname
    // or its children. See @link(NumPts).
    FNumPts: Integer;
    // @name finds the Count nearest locations to the location
    // (CenterX, CenterY) and store TSelectNodes representing
    // those locations in List.  (TSelectNode is defined in
    // the implementation section.)  The number of objects
    // in List may be greater then Count because several points
    // may be the same distance from (CenterX, CenterY).
    procedure FindNearestPoints(const CenterX, CenterY: double;
      const Count: Integer; const List: TObjectList);
    // See @link(Points).
    function GetPoints(Index: Integer): TQPoint;
    // If a @classname has children, @name sets @link(NumPts)
    // to the sum of its childrens' @link(NumPts).
    procedure ResetCount;
    // See @link(NumPts).
    // @name may free child nodes if
    // Value < FTree.@link(TRbwQuadTree.MaxPoints)
    procedure SetNumPts(const Value: Integer);
  protected
    {@name: array [@link(TNorthOrSouth), @link(TEastOrWest)] of @link(TQtreeNode);
     If @link(NumPts) <= FTree.@link(TRbwQuadTree.MaxPoints), then
     FChildren will normally contain all nil values and the data
     in the @classname will be stored in @link(FPts).
     Otherwise, @name will have all instances of @link(TQtreeNode)
     and they will hold the data.
    }
    FChildren: array[TNorthOrSouth, TEastOrWest] of TQtreeNode;
    // @longcode(#FParent : TQtreeNode;#)
    // @name is the @link(TQtreeNode) (if one exists) that has the current instance
    // in @link(FChildren).
    FParent: TQtreeNode;
    { longcode(#FPts: array of TQPoint;#)
    If @link(NumPts) <= FTree.@link(TRbwQuadTree.MaxPoints), then
    the data in the @classname will be stored in @link(FPts).
    See @link(FChildren).}
    FPts: array of TQPoint;
    {@longcode(#FTree: TRbwQuadTree;#)
     @name is the @link(TRbwQuadTree) that is the ultimate root
     for the current @link(TQtreeNode).}
    FTree: TRbwQuadTree;
    // @longcode(#FXmax : double;#)
    // @name is the largest X coordinate that should be
    // stored in this @link(TQtreeNode).  However, see @link(ExpandBounds).
    FXmax: double;
    // @longcode(#FXmin : double;#)
    // @name is the smallest X coordinate that should be
    // stored in this @link(TQtreeNode).  However, see @link(ExpandBounds).
    FXmin: double;
    // @longcode(#FYmax : double;#)
    // @name is the largest Y coordinate that should be
    // stored in this @link(TQtreeNode).  However, see @link(ExpandBounds).
    FYmax: double;
    // @longcode(#FYmin : double;#)
    // @name is the smallest Y coordinate that should be
    // stored in this @link(TQtreeNode).  However, see @link(ExpandBounds).
    FYmin: double;
    // @name adds the location (X,Y) and it associated data to the
    // data stored by this @link(TQtreeNode).
    procedure AddPoint(X, Y: double; Data: pointer);
    // @name is used when searching for the closest points
    // to X, Y.
    // If exclude is self, the procedure immediately
    // exits.  This prevents infinite recursion.
    // Best_leaf is changed to the @link(TQtreeNode) that
    // contains the closest point to X, Y.
    // Best_i is changed to the index of the data point in
    // best_leaf that contains the closest point to X, Y.
    // Best_dist is the distance from X, Y to the closest point
    // in best_leaf.
    // Best_dist2 is best_dist squared.
    procedure CheckNearbyLeaves(const exclude: TQtreeNode;
      var best_leaf: TQtreeNode; const X, Y: double;
      var best_i: integer;
      var best_dist2, best_dist: double);
    // Clear removes all the data stored by this @link(TQtreeNode).
    // Members of @link(FChildren), if any, will be freed.
    procedure Clear;
    // @name creates an instance of @classname
    constructor Create(ATree: TRbwQuadTree); overload;
    // @name creates an instance of @classname and sets @link(FXMax),
    // @link(FXMin), @link(FYMax), and @link(FYMin).
    constructor Create(const x_min, x_max, y_min, y_max: double;
      const ParentNode: TQtreeNode; ATree: TRbwQuadTree); overload;
    // @name is used to expand the range of X and Y coordinates that
    // can be stored in this @link(TQtreeNode) and its children.
    // See @link(FXMax), @link(FXMin), @link(FYMax), and @link(FYMin).
    procedure ExpandBounds(XY: double; ExpandDirection: TExpandDirection);
    // @name returns a @link(TQPoint) that has the X,Y, locations
    // and data in the
    // point closest to the value specified for X and Y.
    // the @link(TQPoint) is owned by a @link(TQtreeNode).
    function FindClosestPoint(const X, Y: double): TQPoint;
    { @name, sets X and Y to the closest location to the original
      X, Y and sets Data to hold the data at that location.}
    procedure FindClosestPointsData(var X, Y: double; var Data: TPointerArray);
    // @name sets X and Y to the closest location in the @link(TQtreeNode)
    // to the original (X, Y) and sets Data to the first data at that location.
    procedure FindPoint(var X, Y: double; var Data: pointer);
    // @name, fills List with PQPoint that point to locations within Block.
    procedure FindPointsInBlock(const Block: T2DBlock; const List: TList);
    // @name, fills List with @link(PQPoint)s that point to locations within
    // the circle defined by CenterX, CenterY, and Radius.
    procedure FindPointsInCircle(const CenterX, CenterY, Radius,
      RadiusSquared: double; const List: TList);
    // @name returns the TQtreeNode that contains the location (X,Y).
    // Siblings is filled with other @classname that are siblings of the
    // one found or its ancestors.
    function LocateLeaf(const X, Y: double;
      const Siblings: TStack): TQtreeNode;
    // @name sets best_i to indicate the point closest to (X,Y)
    // in this leaf. best_dist2 is set to the square of the distance
    // to that point.
    procedure NearPointInLeaf(const X, Y: double;
      out best_i: integer; out best_dist2: double);
    // @name is the number of locations stored by this @link(TQtreeNode).
    property NumPts: Integer read FNumPts write SetNumPts;
    // @name returns the @link(TQPoint) designated by Index.  The
    // @link(TQPoint) may be stored either in this @link(TQtreeNode)
    // or its children.
    property Points[Index: Integer]: TQPoint read GetPoints;
    // @name removes Data at location (X,Y).  If this is the only
    // data at that point, it also removes the point.
    function RemovePoint(const X, Y: double; const Data: pointer): boolean;
    // @name is the location in the X coordinate direction at which
    // the where eastern children meet the western children.  If no
    // children exist, it is the middle of the node in the X direction.
    function Xmid: double;
    // @name is the location in the Y coordinate direction at which
    // the where northern children meet the southern children.  If no
    // children exist, it is the middle of the node in the Y direction.
    function Ymid: double;
  public
    // @name destroys the current instance of
    // @link(TQtreeNode) and its children.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  end;

  { @abstract(@name is used to quickly retrieve data
    by their X and Y coordinates.)
    Methods are provided to search at a
    location or within a defined region}
  TRbwQuadTree = class(TComponent)
  private
    //@name: integer;
    // See @link(MaxPoints).
    FMaxPoints: integer;
    //@name: @link(TQtreeNode);
    // @name does the actual work of the @classname.
    FQTreeNode: TQtreeNode;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Points).
    function GetPoints(Index: Integer): TQuadPoint;
    // See @link(XMax).
    function GetXMax: double;
    // See @link(XMin).
    function GetXMin: double;
    // See @link(YMax).
    function GetYMax: double;
    // See @link(YMin).
    function GetYMin: double;
    // See @link(XMax).
    procedure SetXMax(const AValue: double);
    // See @link(XMin).
    procedure SetXMin(const AValue: double);
    // See @link(YMax).
    procedure SetYMax(const AValue: double);
    // See @link(YMin).
    procedure SetYMin(const AValue: double);
    // See @link(MaxPoints).
    procedure SetMaxPoints(const Value: integer);
  public
    // @name stores Data at location X, Y.
    //
    // If there is already a point at that location the number of pointers
    // stored at that point will be increased by one and the new data will be
    // added at the end of the list of data for the point. Count will be
    // unaffected.
    //
    // If there is not already a point at that location, one will be created
    // and the data will be associated with it, and the Count will be
    // incremented by one.
    //
    // If required, @link(XMax), @link(XMin), @link(YMax), or @link(YMin)
    // will be adjusted to accomodate the new data point.
    //
    // The Data associated with a location is not owned by the
    // @link(TRbwQuadTree); users are responsible for freeing that
    // memory when it is no longer needed.
    procedure AddPoint(X, Y: double; Data: pointer);
    // @name deletes all data locations in the @link(TRbwQuadTree) and resets the
    // @link(Count) to 0.
    //
    // The Data associated with a location is not owned by the
    // @link(TRbwQuadTree); users are responsible for freeing that
    // memory when it is no longer needed.
    procedure Clear;
    // @name is the number of unique locations stored in a @link(TRbwQuadTree).
    property Count: integer read GetCount;
    // Creates an instance of @link(TRbwQuadTree);
    constructor Create(AOwner: TComponent); override;
    // @name and instance of @link(TRbwQuadTree). Applications should not call
    // @name directly. Instead call Free;
    destructor Destroy; override;
    // @name locates the location nearest to the X, Y values
    // provided as input.  It than returns the X and Y values of that location.
    // The length of Data is changed to the number of pointers stored at
    // the location that was found and the Data pointers are copied into Data.
    procedure FindClosestPointsData(var X, Y: double;
      var Data: TPointerArray);
    // @name finds the Count locations in the @link(TRbwQuadTree) that are
    // closest to CenterX, CenterY and returns them and their associated data
    // in Points. In the event of ties, the length of Points may be larger than
    // Count.  If the number of locations in the TRbwQuadTree is
    // less than Count, all the locations will be returned.  Points
    // are sorted from the closest locations to CenterX, CenterY to those
    // that are most distant.
    //
    // See also @link(FirstNearestPoint), @link(NearestPointsFirstData)
    procedure FindNearestPoints(const CenterX, CenterY: double;
      const Count: Integer; var Points: TQuadPointArray);
    // @name finds all locations stored in the @link(TRbwQuadTree) that
    // are inside Block and returns them in Points along with their associated
    // data. Points is not sorted.
    procedure FindPointsInBlock(const Block: T2DBlock;
      var Points: TQuadPointInRegionArray);
    // @name finds all the locations stored in the @link(TRbwQuadTree)
    // that are within a distance of Radius from CenterX, CenterY and returns
    // them and their associated data in Points.  Points is not sorted.
    procedure FindPointsInCircle(const CenterX, CenterY, Radius: double;
      var Points: TQuadPointInRegionArray);
    // @name, finds the location in the @link(TRbwQuadTree) that is
    // closest to X, Y and returns it in X, Y.  The associated data is returned
    // in Data. In the event of ties in location, the location that is returned
    // is chosen arbitrarily from the locations that are closest. If more than
    // one piece of data is stored at X, Y, the one that is returned is chosen
    // arbitrarily.
    //
    // See also: @link(FindNearestPoints) and @link(NearestPointsFirstData).
    procedure FirstNearestPoint(var X, Y: double; var Data: pointer);
    // @name finds the location stored in the @link(TRbwQuadTree)
    // that is closest to X, Y and returns the first piece of data associated
    // with that location.
    //
    // See also: @link(FindNearestPoints) and @link(FirstNearestPoint).
    function NearestPointsFirstData(X, Y: double): pointer;
    // @name can be treated as an indexed array of the locations stored
    // in the @link(TRbwQuadTree).
    property Points[Index: Integer]: TQuadPoint read GetPoints;
    // @name removes Data at location X, Y.
    //
    // If the number of data pointers stored at X, Y is greater than 1, Data
    // will be deleted from the list of data pointers and @link(Count) will be
    // unaffected.
    //
    // If there is only one data pointer stored at X, Y, that location will
    // be deleted and the @link(Count) will be decremented by one.
    //
    // The Data associated with a location is not owned by the
    // @link(TRbwQuadTree); users are responsible for freeing that
    // memory when it is no longer needed.
    function RemovePoint(X, Y: double; Data: pointer): boolean;
  published
    // MaxPoints is the maximum number of data locations in any node of a
    // TRbwQuadTree. MaxPoints affects the performance of the TRbwQuadTree
    // in the retrieval of data in a manner that depends on the particular
    // application.
    property MaxPoints: integer read FMaxPoints write SetMaxPoints;
    // @name is the largest X value of any of the data points that are expected
    // to be or have already been added to the TRbwQuadTree.  Although it is
    // not required that @name be greater than the X coordinate of all locations
    // added to the TRbwQuadTree, performance may suffer if too many of them
    // are greater than @name.
    property XMax: double read GetXMax write SetXMax;
    // @name is the smallest X value of any of the data points that are expected
    // to be or have already been added to the TRbwQuadTree.  Although it is
    // not required that @name be less than the X coordinate of all locations
    // added to the TRbwQuadTree, performance may suffer if too many of them
    // are less than @name.
    property XMin: double read GetXMin write SetXMin;
    // @name is the largest Y value of any of the data points that are expected
    // to be or have already been added to the TRbwQuadTree.  Although it is
    // not required that @name be greater than the Y coordinate of all locations
    // added to the TRbwQuadTree, performance may suffer if too many of them
    // are greater than @name.
    property YMax: double read GetYMax write SetYMax;
    // @name is the smallest Y value of any of the data points that are expected
    // to be or have already been added to the TRbwQuadTree.  Although it is
    // not required that @name be less than the Y coordinate of all locations
    // added to the TRbwQuadTree, performance may suffer if too many of them
    // are less than @name.
    property YMin: double read GetYMin write SetYMin;
  end;

  //@name registers @link(TRbwQuadTree).
procedure Register;

implementation

resourcestring
  StrErrorNoDataPoint = 'Error: No data points in QuadTree.';
  StrInvalidPointIndex = 'Invalid point index < 0.';
  StrInvalidPointIndex2 = 'Invalid point index >= Count.';
  StrTheMaximumNumberO = 'The maximum number of points in a Quadtree must be' +
  ' greater than zero.';
  StrYouMustSetTheNum = 'You must set the number of points before adding any' +
  ' points to a Quadtree.';

type
  TSelectNode = class(TObject)
    Distance: double;
    Point: TQPoint;
  end;

function SelectNodeCompare(Item1, Item2: Pointer): Integer;
var
  Node1, Node2: TSelectNode;
  Delta: double;
begin
  Node1 := Item1;
  Node2 := Item2;
  Delta := Node2.Distance - Node1.Distance;
  if Delta > 0 then
  begin
    result := -1;
  end
  else if Delta < 0 then
  begin
    result := 1;
  end
  else
  begin
    result := 0;
  end;

end;

// Create the node.

constructor TQtreeNode.Create(ATree: TRbwQuadTree);
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
begin
  inherited Create;
  FParent := nil;
  FXmin := 0;
  FXmax := 0;
  FYmin := 0;
  FYmax := 0;
  FTree := ATree;
  for ns := North to South do
  begin
    for ew := East to West do
    begin
      FChildren[ns, ew] := nil;
    end;

  end;
  FNumPts := 0;

  // Allocate space for the points.
  SetLength(FPts, FTree.MaxPoints);
end;

// Create and initialize the node.

constructor TQtreeNode.Create(const x_min, x_max, y_min, y_max: double;
  const ParentNode: TQtreeNode; ATree: TRbwQuadTree);
begin
  Create(ATree);
  if not ((x_min <= x_max) and (y_min <= y_max)) then
  begin
    raise EQTreeError.Create('Error: attempt to create a TQtreeNode with '
      + 'an invalid range.');
  end;
  FParent := ParentNode;
  FXmin := x_min;
  FXmax := x_max;
  FYmin := y_min;
  FYmax := y_max;
end;

// Free all children.

destructor TQtreeNode.Destroy;
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  Index: integer;
  //  ListIndex : integer;
begin
  // If we have no children, free the point memory.
  if (FChildren[North, West] = nil) then
  begin
    // If required, destroy the data.
    for Index := 0 to NumPts - 1 do
    begin
      FPts[Index].Data.Free;
    end;
    SetLength(FPts, 0);
    FNumPts := 0;
  end
  else
  begin
    // Otherwise free the children.
    for ns := North to South do
      for ew := East to West do
        FChildren[ns, ew].Free;
    // The Point memory doesn't have to be freed because it was
    // freed when the children were created;
  end;

  inherited Destroy;
end;

function TQtreeNode.RemovePoint(const X, Y: double; const Data: pointer): boolean;
var
  xmidLocal, ymidLocal: double;
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  AQuadNode: TQtreeNode;
  Index: integer;
  DataCount: integer;
begin
  result := False;
  // If the point is outside the range of the TQtreeNode, quit.
  if (X < FXmin) or (X > FXmax) or (Y < FYmin) or (Y > FYmax) then
    Exit;

  AQuadNode := self;
  // See if the node belongs in a child.
  while (AQuadNode.FChildren[North, West] <> nil) do

  begin
    xmidLocal := AQuadNode.Xmid;
    ymidLocal := AQuadNode.Ymid;

    // Get the correct child.
    if (Y <= ymidLocal) then
      ns := South
    else
      ns := North;
    if (X <= xmidLocal) then
      ew := West
    else
      ew := East;
    AQuadNode := AQuadNode.FChildren[ns, ew];
  end;

  // find point.
  for Index := 0 to AQuadNode.NumPts - 1 do
  begin
    if (AQuadNode.FPts[Index].X = X) and (AQuadNode.FPts[Index].Y = Y) then
    begin
      // Remove the data
      DataCount := AQuadNode.FPts[Index].Data.Count;
      AQuadNode.FPts[Index].Data.Remove(Data);
      result := AQuadNode.FPts[Index].Data.Count = DataCount -1;
      // If the number of data items associated with the data
      // point has bee reduced to 0, remove the data point.
      if AQuadNode.FPts[Index].Data.Count = 0 then
      begin
        AQuadNode.FPts[Index].Data.Free;
        AQuadNode.FPts[Index].Data := nil;
        if Index+1 < Length(AQuadNode.FPts) then
        begin
          Move(AQuadNode.FPts[Index+1], AQuadNode.FPts[Index],
            (AQuadNode.NumPts - Index - 1) * SizeOf(TQPoint));
        end;
//        for PointIndex := Index + 1 to AQuadNode.NumPts - 1 do
//        begin
//          AQuadNode.FPts[PointIndex - 1] := AQuadNode.FPts[PointIndex];
//        end;
        AQuadNode.NumPts := AQuadNode.NumPts - 1;
      end;
      Exit;
    end;
  end;
end;

procedure TQtreeNode.AddPoint(X, Y: double; Data: pointer);
var
  xmidLocal, ymidLocal: double;
  i: integer;
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  AQuadNode, Child: TQtreeNode;
  Index: integer;
  APoint: TQPoint;
begin
  // If the point is outside the range of the TQtreeNode, expand the TQtreeNode to encompass it.
  if X < FXmin then
    ExpandBounds(X, edWest);
  if X > FXmax then
    ExpandBounds(X, edEast);
  if Y < FYmin then
    ExpandBounds(Y, edSouth);
  if Y > FYmax then
    ExpandBounds(Y, edNorth);

  AQuadNode := self;
  // See if the node belongs in a child.
  while (AQuadNode.NumPts >= FTree.MaxPoints) do

  begin
    xmidLocal := AQuadNode.Xmid;
    ymidLocal := AQuadNode.Ymid;

    // See if we need to create new children.
    if (AQuadNode.FChildren[North, West] = nil) then
    begin

      // Don't add duplicate point. Instead add data to existing point.
      for Index := 0 to AQuadNode.NumPts - 1 do
      begin
        if (AQuadNode.FPts[Index].X = X) and (AQuadNode.FPts[Index].Y = Y) then
        begin
          AQuadNode.FPts[Index].Data.Add(Data);
          Exit;
        end;
      end;

      // Split the node into four children.
      AQuadNode.FChildren[North, West] := TQtreeNode.Create
        (AQuadNode.FXmin, xmidLocal, ymidLocal, AQuadNode.FYmax,
        AQuadNode, FTree);
      AQuadNode.FChildren[North, East] := TQtreeNode.Create
        (xmidLocal, AQuadNode.FXmax, ymidLocal, AQuadNode.FYmax,
        AQuadNode, FTree);
      AQuadNode.FChildren[South, West] := TQtreeNode.Create
        (AQuadNode.FXmin, xmidLocal, AQuadNode.FYmin, ymidLocal,
        AQuadNode, FTree);
      AQuadNode.FChildren[South, East] := TQtreeNode.Create
        (xmidLocal, AQuadNode.FXmax, AQuadNode.FYmin, ymidLocal,
        AQuadNode, FTree);

      // Move the old points into the new children.
      for i := 0 to AQuadNode.NumPts - 1 do
      begin
        APoint := AQuadNode.FPts[i];
        if (APoint.Y <= ymidLocal) then
          ns := South
        else
          ns := North;
        if (APoint.X <= xmidLocal) then
          ew := West
        else
          ew := East;
        Child := AQuadNode.FChildren[ns, ew];
        Assert(Child.NumPts < FTree.MaxPoints);
        Child.FPts[Child.NumPts] := APoint;
        Child.FNumPts := Child.FNumPts + 1;
      end; // End moving points to the new children.

      // Free point memory.
      SetLength(AQuadNode.FPts, 0);

    end; // End creating new children.

    // Add the point to the correct child.
    if (Y <= ymidLocal) then
      ns := South
    else
      ns := North;
    if (X <= xmidLocal) then
      ew := West
    else
      ew := East;
    AQuadNode := AQuadNode.FChildren[ns, ew];
  end;

  // Don't add duplicate point. Instead add data to existing point.
  for Index := 0 to AQuadNode.NumPts - 1 do
  begin
    if (AQuadNode.FPts[Index].X = X) and (AQuadNode.FPts[Index].Y = Y) then
    begin
      AQuadNode.FPts[Index].Data.Add(Data);
      Exit;
    end;
  end;

  // Place the point in this node.
  AQuadNode.FPts[AQuadNode.NumPts].X := X;
  AQuadNode.FPts[AQuadNode.NumPts].Y := Y;
  AQuadNode.FPts[AQuadNode.NumPts].Data := TList.Create;
  AQuadNode.FPts[AQuadNode.NumPts].Data.Add(Data);
  AQuadNode.NumPts := AQuadNode.NumPts + 1;
end;

Type
  TStackCrack = class(TStack);

// Find the point closest to the given coordinates.

function TQtreeNode.FindClosestPoint(const X, Y: double): TQPoint;
var
  best_dist2, best_dist: double;
  best_i: Integer;
  leaf, Aleaf: TQtreeNode;
  Siblings: TStack;
  ChildLeaf: TQtreeNode;
  Depth: Integer;
begin
  // See which leaf contains the point.
  Siblings := TStack.Create;
  try
    ChildLeaf := self;
    Depth := 1;
    while ChildLeaf <> nil do
    begin
      ChildLeaf := ChildLeaf.FChildren[North, West];
      Inc(Depth);
    end;
    TStackCrack(Siblings).List.Capacity:= Depth * 3;

    leaf := LocateLeaf(X, Y, Siblings);

    // Find the closest point within the leaf.
    leaf.NearPointInLeaf(X, Y, best_i, best_dist2);

    // If no point was found, check the siblings of the leaf.
    if best_i < 0 then
    begin
      while Siblings.Count > 0 do
      begin
        Aleaf := Siblings.Pop;
        leaf := Aleaf.LocateLeaf(X, Y, Siblings);

        // Find the closest point within the leaf.
        leaf.NearPointInLeaf(X, Y, best_i, best_dist2);
        if best_i >= 0 then
        begin
          Break;
        end;
      end;
    end;

    best_dist := Sqrt(best_dist2);
    // Check nearby leaves for closer points.
    CheckNearbyLeaves(leaf, leaf, X, Y, best_i, best_dist2, best_dist);

    Assert((best_i >= 0) and (best_i < Length(leaf.FPts)));
    result := leaf.FPts[best_i];
  finally
    Siblings.Free;
  end;
end;

procedure TQtreeNode.FindPoint(var X, Y: double; var Data: pointer);
var
  APoint: TQPoint;
begin
  APoint := FindClosestPoint(X, Y);
  X := APoint.X;
  Y := APoint.Y;
  if (APoint.Data <> nil) and (APoint.Data.Count > 0) then
  begin
    Data := APoint.Data[0];
  end
  else
  begin
    Data := nil;
  end;
end;

// See what leaf contains the point.

function TQtreeNode.LocateLeaf(const X, Y: double;
  const Siblings: TStack): TQtreeNode;
var
  ns, nsi: TNorthOrSouth;
  ew, ewi: TEastOrWest;
begin
  Result := Self;

  // Search the appropriate child.
  while result.FChildren[North, West] <> nil do
  begin
    if (Y <= result.ymid) then
      ns := South
    else
      ns := North;
    if (X <= result.xmid) then
      ew := West
    else
      ew := East;

    // if the appropriate child doesn't have any points, we may
    // have to seach the child's siblings so save them.
    for nsi := North to South do
    begin
      for ewi := East to West do
      begin
        if (nsi <> ns) or (ewi <> ew) then
        begin
          Siblings.Push(result.FChildren[nsi, ewi]);
        end;
      end;
    end;

    Result := result.FChildren[ns, ew];
  end;
end;

// Return the index of the point closest to the given
// coordinates in this leaf.

procedure TQtreeNode.NearPointInLeaf(const X, Y: double; out best_i: Integer;
  out best_dist2: double);
var
  i: Longint;
  dist2: double;
  dx, dy: double;
begin
  // If there are no points in the node, set best_i to -1 to indicate no
  // points were found.
  best_dist2 := 0;
  best_i := -1;

  if NumPts > 0 then
  begin
    dx := X - FPts[0].X;
    dy := Y - FPts[0].Y;
    best_dist2 := dx * dx + dy * dy;
    best_i := 0;

    for i := 1 to NumPts - 1 do
    begin
      dx := X - FPts[i].X;
      dy := Y - FPts[i].Y;
      dist2 := dx * dx + dy * dy;
      if (best_dist2 > dist2) then
      begin
        best_i := i;
        best_dist2 := dist2;
      end;
    end;
  end;
end;

// Check nearby leaves to see if there is a better point.

procedure TQtreeNode.CheckNearbyLeaves(const exclude: TQtreeNode;
  var best_leaf: TQtreeNode; const X, Y: double;
  var best_i: integer;
  var best_dist2, best_dist: double);
var
  xmidLocal, ymidLocal: double;
  i: Integer;
  dist2: double;
begin
  // If we are excluding this leaf, do nothing.
  if (exclude = Self) then
    exit;

  // If this is a leaf node, look for close nodes.
  if (FChildren[North, West] = nil) then
  begin
    NearPointInLeaf(X, Y, i, dist2);
    // Use the closest point in this leaf if
    // 1. there is at least one point in this leaf (i > -1) and either
    // 2. the closest point in this leaf is closer to the search point than
    //    the point that was already found (best_dist2 > dist2) or
    // 3. no point has been found yet (best_i < 0).
    if (i > -1) and ((best_dist2 > dist2) or (best_i < 0)) then
    begin
      best_dist2 := dist2;
      best_dist := Sqrt(best_dist2);
      best_leaf := Self;
      best_i := i;
    end;
  end
  else
  begin
    // Examine children that lie within best_dist
    // of the point.
    xmidLocal := Xmid;
    ymidLocal := Ymid;
    if (best_i < 0) or (X - best_dist <= xmidLocal) then
    begin
      // The West children may be close enough.
      // See if the SouthWest child is close enough.
      if (Y - best_dist <= ymidLocal) or (best_i < 0) then
        FChildren[South, West].CheckNearbyLeaves(
          exclude, best_leaf, X, Y, best_i,
          best_dist2, best_dist);
    end;

    if (best_i < 0) or (X - best_dist <= xmidLocal) then
    begin
      // The West children may be close enough.
      // See if the NorthWest child is close enough.
      if (Y + best_dist > ymidLocal) or (best_i < 0) then
        FChildren[North, West].CheckNearbyLeaves(
          exclude, best_leaf, X, Y, best_i,
          best_dist2, best_dist);
    end;

    if (best_i < 0) or (X + best_dist > xmidLocal) then
    begin
      // The East children may be close enough.
      // See if the SouthEast child is close enough.
      if (Y - best_dist <= ymidLocal) or (best_i < 0) then
        FChildren[South, East].CheckNearbyLeaves(
          exclude, best_leaf, X, Y, best_i,
          best_dist2, best_dist);
    end;

    if (best_i < 0) or (X + best_dist > xmidLocal) then
    begin
      // The East children may be close enough.
      // See if the NorthEast child is close enough.
      if (Y + best_dist > ymidLocal) or (best_i < 0) then
        FChildren[North, East].CheckNearbyLeaves(
          exclude, best_leaf, X, Y, best_i,
          best_dist2, best_dist);
    end;
  end; // End if a leaf ... else check children ...
end;

procedure TQtreeNode.ExpandBounds(XY: double;
  ExpandDirection: TExpandDirection);
begin
  case ExpandDirection of
    edNorth:
      begin
        FYmax := XY;
        if FChildren[North, East] <> nil then
        begin
          FChildren[North, East].ExpandBounds(XY, ExpandDirection);
          FChildren[North, West].ExpandBounds(XY, ExpandDirection);
        end;
      end;
    edSouth:
      begin
        FYmin := XY;
        if FChildren[North, East] <> nil then
        begin
          FChildren[South, East].ExpandBounds(XY, ExpandDirection);
          FChildren[South, West].ExpandBounds(XY, ExpandDirection);
        end;
      end;
    edEast:
      begin
        FXmax := XY;
        if FChildren[North, East] <> nil then
        begin
          FChildren[North, East].ExpandBounds(XY, ExpandDirection);
          FChildren[South, East].ExpandBounds(XY, ExpandDirection);
        end;
      end;
    edWest:
      begin
        FXmin := XY;
        if FChildren[North, East] <> nil then
        begin
          FChildren[North, West].ExpandBounds(XY, ExpandDirection);
          FChildren[South, West].ExpandBounds(XY, ExpandDirection);
        end;
      end;
  end;
end;

function TQtreeNode.Xmid: double;
begin
  if FChildren[North, West] = nil then
  begin
    result := (FXmax + FXmin) / 2
  end
  else
  begin
    result := FChildren[North, West].FXmax;
  end;
end;

function TQtreeNode.Ymid: double;
begin
  if FChildren[North, East] = nil then
  begin
    result := (FYmax + FYmin) / 2
  end
  else
  begin
    result := FChildren[North, East].FYmin;
  end;
end;

{ TRbwQuadTree }

function TRbwQuadTree.GetXMax: double;
begin
  result := FQTreeNode.FXmax;
end;

function TRbwQuadTree.GetXMin: double;
begin
  result := FQTreeNode.FXmin;
end;

function TRbwQuadTree.GetYMax: double;
begin
  result := FQTreeNode.FYmax;
end;

function TRbwQuadTree.GetYMin: double;
begin
  result := FQTreeNode.FYmin;
end;

procedure TRbwQuadTree.SetXMax(const AValue: double);
begin
  if Count = 0 then
  begin
    FQTreeNode.FXmax := AValue;
  end
  else
  begin
    if (AValue < XMax) then
    begin
      raise
        EQTreeError.Create('Error: The maximum X value can not be decreased; '
        + 'only increased.');
    end;
    FQTreeNode.ExpandBounds(AValue, edWest);
  end;
end;

procedure TRbwQuadTree.SetXMin(const AValue: double);
begin
  if Count = 0 then
  begin
    FQTreeNode.FXmin := AValue;
  end
  else
  begin
    if (AValue > XMin) then
    begin
      raise
        EQTreeError.Create('Error: The minimum X value can not be increased; '
        + 'only decreased.');
    end;
    FQTreeNode.ExpandBounds(AValue, edEast);
  end;
end;

procedure TRbwQuadTree.SetYMax(const AValue: double);
begin
  if Count = 0 then
  begin
    FQTreeNode.FYmax := AValue;
  end
  else
  begin
    if (AValue < YMax) then
    begin
      raise
        EQTreeError.Create('Error: The maximum Y value can not be decreased; '
        + 'only increased.');
    end;
    FQTreeNode.ExpandBounds(AValue, edNorth);
  end;
end;

procedure TRbwQuadTree.SetYMin(const AValue: double);
begin
  if Count = 0 then
  begin
    FQTreeNode.FYmin := AValue;
  end
  else
  begin
    if (AValue > YMin) then
    begin
      raise
        EQTreeError.Create('Error: The minimum Y value can not be increased; '
        + 'only decreased.');
    end;
    FQTreeNode.ExpandBounds(AValue, edSouth);
  end;
end;

procedure TRbwQuadTree.AddPoint(X, Y: double; Data: pointer);
begin
  FQTreeNode.AddPoint(X, Y, Data);
end;

constructor TRbwQuadTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxPoints := 100;
  //  FDestroyProcedure := nil;
  FQTreeNode := TQTreeNode.Create(self);
end;

destructor TRbwQuadTree.Destroy;
begin
  FQTreeNode.Free;
  inherited;
end;

function TRbwQuadTree.NearestPointsFirstData(X, Y: double): pointer;
begin
  if FQTreeNode.NumPts > 0 then
  begin
    FirstNearestPoint(X, Y, result);
  end
  else
  begin
    result := nil;
  end;
end;

procedure TRbwQuadTree.FirstNearestPoint(var X, Y: double; var Data: pointer);
begin
  if FQTreeNode.NumPts > 0 then
  begin
    FQTreeNode.FindPoint(X, Y, Data);
  end
  else
  begin
    raise EQTreeError.Create(StrErrorNoDataPoint);
  end;
end;

procedure TQtreeNode.SetNumPts(const Value: Integer);
var
  OldNumPoints: Integer;
  ew: TEastOrWest;
  ns: TNorthOrSouth;
  AChild: TQtreeNode;
  Index: integer;
  PtCount: integer;
begin
  if FNumPts <> Value then
  begin
    OldNumPoints := FNumPts;
    FNumPts := Value;

    if (FNumPts <= FTree.MaxPoints)
      and (OldNumPoints > FTree.MaxPoints) then
    begin
      SetLength(FPts, FTree.MaxPoints);
      PtCount := 0;
      for ns := North to South do
      begin
        for ew := East to West do
        begin
          AChild := FChildren[ns, ew];
          for Index := 0 to AChild.NumPts - 1 do
          begin
            Assert(PtCount < FTree.MaxPoints);
            FPts[PtCount] := AChild.FPts[Index];
            Inc(PtCount);
          end;
          AChild.FNumPts := 0;
          SetLength(AChild.FPts, 0);
          AChild.Free;
          FChildren[ns, ew] := nil;
        end;
      end;
      Assert(PtCount = FNumPts);
    end;

    if FParent <> nil then
    begin
      FParent.ResetCount;
    end;
  end;
end;

function TQtreeNode.GetPoints(Index: Integer): TQPoint;
var
  ew: TEastOrWest;
  ns: TNorthOrSouth;
  AChild: TQtreeNode;
begin
  if FChildren[North, East] = nil then
  begin
    result := FPts[Index];
  end
  else
  begin
    for ew := West downto East do
    begin
      for ns := North to South do
      begin
        AChild := FChildren[ns, ew];
        if AChild.NumPts > Index then
        begin
          result := AChild.GetPoints(Index);
          Exit;
        end
        else
        begin
          Index := Index - AChild.NumPts;
        end;
      end;
    end;
  end;
end;

procedure TQtreeNode.FindClosestPointsData(var X, Y: double;
  var Data: TPointerArray);
var
  APoint: TQPoint;
  Index: integer;
begin
  APoint := FindClosestPoint(X, Y);
  X := APoint.X;
  Y := APoint.Y;
  SetLength(Data, APoint.Data.Count);
  for Index := 0 to APoint.Data.Count - 1 do
  begin
    Data[Index] := APoint.Data[Index];
  end;
end;

procedure TRbwQuadTree.FindClosestPointsData(var X, Y: double;
  var Data: TPointerArray);
begin
  if FQTreeNode.NumPts > 0 then
  begin
    FQTreeNode.FindClosestPointsData(X, Y, Data);
  end
  else
  begin
    raise EQTreeError.Create(StrErrorNoDataPoint);
  end;
end;

function TRbwQuadTree.GetCount: integer;
begin
  result := FQTreeNode.NumPts;
end;

function TRbwQuadTree.RemovePoint(X, Y: double; Data: pointer): boolean;
begin
  result := FQTreeNode.RemovePoint(X, Y, Data);
end;

function TRbwQuadTree.GetPoints(Index: Integer): TQuadPoint;
var
  QPoint: TQPoint;
  DataIndex: integer;
begin
  if Index < 0 then
  begin
    raise EQTreeError.Create(StrInvalidPointIndex);
  end;
  if Index >= Count then
  begin
    raise EQTreeError.Create(StrInvalidPointIndex2);
  end;
  QPoint := FQTreeNode.Points[Index];
  result.X := QPoint.X;
  result.Y := QPoint.Y;
  result.Distance := 0;
  SetLength(result.Data, QPoint.Data.Count);
  for DataIndex := 0 to QPoint.Data.Count - 1 do
  begin
    result.Data[DataIndex] := QPoint.Data[DataIndex];
  end;
end;

procedure TQtreeNode.ResetCount;
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  Count: integer;
begin
  if FChildren[North, West] <> nil then
  begin
    Count := 0;
    for ns := North to South do
    begin
      for ew := East to West do
      begin
        Count := Count + FChildren[ns, ew].NumPts;
      end;
    end;
    NumPts := Count;
  end;
end;

procedure TQtreeNode.FindNearestPoints(const CenterX, CenterY: double;
  const Count: Integer; const List: TObjectList);
var
  Radius: double;
  Node: TSelectNode;
  distance: double;
  APoint: TQPoint;
  dx, dy: double;
  ALeaf: TQtreeNode;
  Siblings: TStack;
  ChildLeaf: TQtreeNode;
  Depth: integer;
  procedure GetPoints;
  var
    Index, InnerIndex: integer;
  begin
    for Index := 0 to ALeaf.NumPts - 1 do
    begin
      APoint := ALeaf.FPts[Index];
      dx := APoint.X - CenterX;
      dy := APoint.Y - CenterY;
      distance := Sqrt(Sqr(dx) + Sqr(dy));
      if (List.Count < Count) or (distance <= Radius) then
      begin
        Node := TSelectNode.Create;
        Node.Distance := distance;
        Node.Point := APoint;
        List.Add(Node);
        if List.Count mod Count = 0 then
        begin
          List.Sort(SelectNodeCompare);
          if List.Count >= Count then
          begin
            Node := List[Count - 1] as TSelectNode;
            Radius := Node.Distance;
          end;
          for InnerIndex := Count to List.Count - 1 do
          begin
            Node := List[InnerIndex] as TSelectNode;
            if Node.Distance > Radius then
            begin
              List.Count := InnerIndex;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
begin
  Siblings := TStack.Create;
  try
    ChildLeaf := self;
    Depth := 1;
    while ChildLeaf <> nil do
    begin
      ChildLeaf := ChildLeaf.FChildren[North, West];
      Inc(Depth);
    end;
    TStackCrack(Siblings).List.Capacity:= Depth * 3;

    ALeaf := LocateLeaf(CenterX, CenterY, Siblings);
    radius := 0;
    GetPoints;
    while Siblings.Count > 0 do
    begin
      ALeaf := Siblings.Pop;

      if (List.Count < Count) or
        ((CenterX + radius >= ALeaf.FXmin) and
        (CenterX - radius <= ALeaf.FXmax) and
        (CenterY + radius >= ALeaf.FYmin) and
        (CenterY - radius <= ALeaf.FYmax)) then
      begin
        ALeaf := ALeaf.LocateLeaf(CenterX, CenterY, Siblings);
        GetPoints;
      end;
    end;
  finally
    Siblings.Free;
  end;
end;

procedure TQtreeNode.FindPointsInCircle(const CenterX, CenterY,
  Radius, RadiusSquared: double; const List: TList);
var
  AChild: TQtreeNode;
  xmidLocal, ymidLocal: double;
  Index: integer;
  dx, dy: double;
  APoint: TQPoint;
  WholeBlockInside: boolean;
begin
  if FChildren[North, West] = nil then
  begin
    // see if it is worth trying to determine if all the points
    // are inside the circle;
    WholeBlockInside := NumPts > 4;

    // Test each corner to see whether it is inside the circle
    if WholeBlockInside then
    begin
      dx := FXmax - CenterX;
      dy := FYmax - CenterY;
      if Sqr(dx) + Sqr(dy) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXmax - CenterX;
      dy := FYmin - CenterY;
      if Sqr(dx) + Sqr(dy) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXmin - CenterX;
      dy := FYmax - CenterY;
      if Sqr(dx) + Sqr(dy) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXmin - CenterX;
      dy := FYmin - CenterY;
      if Sqr(dx) + Sqr(dy) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    // If the entire block is inside the circle, add all the points.
    // Otherwise, test each point to see if it is inside the circle and
    // add those points.
    if WholeBlockInside then
    begin
      for Index := 0 to NumPts - 1 do
      begin
        List.Add(Addr(FPts[Index]));
      end;
    end
    else
    begin
      for Index := 0 to NumPts - 1 do
      begin
        APoint := FPts[Index];
        dx := APoint.X - CenterX;
        dy := APoint.Y - CenterY;
        if Sqr(dx) + Sqr(dy) <= RadiusSquared then
        begin
          List.Add(Addr(FPts[Index]));
        end;
      end;
    end;
  end
  else
  begin
    xmidLocal := Xmid;
    ymidLocal := Ymid;
    if (CenterX - Radius <= xmidLocal) then
    begin
      // West Children are candidates.
      if CenterY - Radius <= ymidLocal then
      begin
        //South Children are candidates.
        AChild := FChildren[South, West];
        AChild.FindPointsInCircle(CenterX, CenterY, Radius, RadiusSquared,
          List);
      end;

      if CenterY + Radius >= ymidLocal then
      begin
        //North Children are candidates.
        AChild := FChildren[North, West];
        AChild.FindPointsInCircle(CenterX, CenterY, Radius, RadiusSquared,
          List);
      end;
    end;

    if (CenterX + Radius >= xmidLocal) then
    begin
      // East Children are candidates.
      if CenterY - Radius <= ymidLocal then
      begin
        //South Children are candidates.
        AChild := FChildren[South, East];
        AChild.FindPointsInCircle(CenterX, CenterY, Radius, RadiusSquared,
          List);
      end;

      if CenterY + Radius >= ymidLocal then
      begin
        //North Children are candidates.
        AChild := FChildren[North, East];
        AChild.FindPointsInCircle(CenterX, CenterY, Radius, RadiusSquared,
          List);
      end;
    end
  end;
end;

procedure TQtreeNode.FindPointsInBlock(const Block: T2DBlock;
  const List: TList);
var
  AChild: TQtreeNode;
  xmidLocal, ymidLocal: double;
  Index: integer;
  //  dx, dy : double;
  APoint: TQPoint;
begin
  if FChildren[North, West] = nil then
  begin
    if (Block.XMin <= FXmin) and (Block.XMax >= FXmax)
      and (Block.YMin <= FYmin) and (Block.YMax >= FYmax) then
    begin
      for Index := 0 to NumPts - 1 do
      begin
        List.Add(Addr(FPts[Index]));
      end;
    end
    else
    begin
      for Index := 0 to NumPts - 1 do
      begin
        APoint := FPts[Index];
        if (Block.XMin <= APoint.X) and (Block.XMax >= APoint.X)
          and (Block.YMin <= APoint.Y) and (Block.YMax >= APoint.Y) then
        begin
          List.Add(Addr(FPts[Index]));
        end;
      end;
    end;
  end
  else
  begin
    xmidLocal := Xmid;
    ymidLocal := Ymid;
    if (Block.XMin <= xmidLocal) then
    begin
      // West Children are candidates.
      if Block.YMin <= ymidLocal then
      begin
        //South Children are candidates.
        AChild := FChildren[South, West];
        AChild.FindPointsInBlock(Block, List);
      end;

      if Block.YMax >= ymidLocal then
      begin
        //North Children are candidates.
        AChild := FChildren[North, West];
        AChild.FindPointsInBlock(Block, List);
      end;
    end;

    if (Block.XMax >= xmidLocal) then
    begin
      // East Children are candidates.
      if Block.YMin <= ymidLocal then
      begin
        //South Children are candidates.
        AChild := FChildren[South, East];
        AChild.FindPointsInBlock(Block, List);
      end;

      if Block.YMax >= ymidLocal then
      begin
        //North Children are candidates.
        AChild := FChildren[North, East];
        AChild.FindPointsInBlock(Block, List);
      end;
    end
  end;
end;

procedure TRbwQuadTree.FindPointsInBlock(const Block: T2DBlock;
  var Points: TQuadPointInRegionArray);
var
  List: TList;
  PointIndex, DataIndex: integer;
  APoint: TQPoint;
  PointAddress: PQPoint;
begin
  List := TList.Create;
  try
    List.Capacity := Count;
    FQTreeNode.FindPointsInBlock(Block, List);
    SetLength(Points, List.Count);
    for PointIndex := 0 to List.Count - 1 do
    begin
      PointAddress := List[PointIndex];
      APoint := PointAddress^;
      Points[PointIndex].X := APoint.X;
      Points[PointIndex].Y := APoint.Y;
      SetLength(Points[PointIndex].Data, APoint.Data.Count);
      for DataIndex := 0 to APoint.Data.Count - 1 do
      begin
        Points[PointIndex].Data[DataIndex] := APoint.Data[DataIndex];
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TRbwQuadTree.FindPointsInCircle(const CenterX, CenterY,
  Radius: double; var Points: TQuadPointInRegionArray);
var
  List: TList;
  PointIndex, DataIndex: integer;
  APoint: TQPoint;
  PointAddress: PQPoint;
begin
  List := TList.Create;
  try
    FQTreeNode.FindPointsInCircle(CenterX, CenterY, Radius, Sqr(Radius), List);
    SetLength(Points, List.Count);
    for PointIndex := 0 to List.Count - 1 do
    begin
      PointAddress := List[PointIndex];
      APoint := PointAddress^;
      Points[PointIndex].X := APoint.X;
      Points[PointIndex].Y := APoint.Y;
      SetLength(Points[PointIndex].Data, APoint.Data.Count);
      for DataIndex := 0 to APoint.Data.Count - 1 do
      begin
        Points[PointIndex].Data[DataIndex] := APoint.Data[DataIndex];
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TRbwQuadTree.FindNearestPoints(const CenterX, CenterY: double;
  const Count: Integer; var Points: TQuadPointArray);
var
  List: TObjectList;
  PointIndex, DataIndex: integer;
  APoint: TQPoint;
  Node: TSelectNode;
  Radius: double;
begin
  List := TObjectList.Create;
  try
    FQTreeNode.FindNearestPoints(CenterX, CenterY, Count, List);

    List.Sort(SelectNodeCompare);
    if List.Count > Count then
    begin
      Node := List[Count - 1] as TSelectNode;
      Radius := Node.Distance;

      for PointIndex := Count to List.Count - 1 do
      begin
        Node := List[PointIndex] as TSelectNode;
        if Node.Distance > Radius then
        begin
          List.Count := PointIndex;
          break;
        end;
      end;
    end;

    SetLength(Points, List.Count);
    for PointIndex := 0 to List.Count - 1 do
    begin
      Node := List[PointIndex] as TSelectNode;
      APoint := Node.Point;
      Points[PointIndex].X := APoint.X;
      Points[PointIndex].Y := APoint.Y;
      Points[PointIndex].Distance := Node.Distance;
      SetLength(Points[PointIndex].Data, APoint.Data.Count);
      for DataIndex := 0 to APoint.Data.Count - 1 do
      begin
        Points[PointIndex].Data[DataIndex] := APoint.Data[DataIndex];
      end;
    end;

  finally
    List.Free;
  end;
end;

procedure TRbwQuadTree.SetMaxPoints(const Value: integer);
begin
  if Count = 0 then
  begin
    if Value <= 0 then
    begin
      raise EQTreeError.Create(StrTheMaximumNumberO);
    end;
    FMaxPoints := Value;
    SetLength(FQTreeNode.FPts, FMaxPoints);
  end
  else
  begin
    raise EQTreeError.Create(StrYouMustSetTheNum);
  end;

end;

procedure Register;
begin
  RegisterComponents('RBW', [TRbwQuadTree]);
end;

procedure TQtreeNode.Clear;
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  Index: integer;
begin
  if FChildren[North, East] = nil then
  begin
    for Index := 0 to NumPts - 1 do
    begin
      FPts[Index].Data.Free;
      FPts[Index].Data := nil;
    end;
  end
  else
  begin
    for ns := North to South do
    begin
      for ew := East to West do
      begin
        FChildren[ns, ew].Clear;
        FChildren[ns, ew].Free;
        FChildren[ns, ew] := nil;
      end;
    end;
  end;
  FNumPts := 0;
end;

procedure TRbwQuadTree.Clear;
begin
  FQTreeNode.Clear;
//  FQTreeNode.FNumPts := 0;
  SetLength(FQTreeNode.FPts, MaxPoints);
end;

end.

