{@name registers @link(TRbwOctTree) which is used to
quickly retrieve data by their X, Y, and Z coordinates.

@author(Richard B. Winston <rbwinst@usgs.gov>).

This file is in the public domain.
}
unit OctTreeClass;

interface

uses Types, SysUtils, Classes, contnrs;

type
  // @name is used in defining TOtreeNode.@link(TOtreeNode.FChildren).
  TNorthOrSouth = (North, South);
  // @name is used in defining TOtreeNode.@link(TOtreeNode.FChildren).
  TEastOrWest = (East, West);
  // @name is used in defining TOtreeNode.@link(TOtreeNode.FChildren).
  TUpOrDown = (Up, Down);

  {@abstract(@name is used to store data at a particular location.
   See TOtreeNode.@link(TOtreeNode.FPts).)

  @longcode(#
  TOPoint = record
    Data : TList;
    X: double;
    Y: double;
    Z: double;
  end;
  #)
  }
  TOPoint = record
    // @name is a list of the data at @link(X), @link(Y), @link(Z).
    Data: TList;
    // @name is the X coordinate of the data.
    X: double;
    // @name is the Y coordinate of the data.
    Y: double;
    // @name is the Z coordinate of the data.
    Z: double;
  end;

  // See @link(TOPoint).
  POPoint = ^TOPoint;

  {@longcode(#TPointerArray = array of Pointer;#)}
  TPointerArray = array of Pointer;

  {@abstract(@name records contain the X, Y, and Z coordinates of a location in a
  @link(TRbwOctTree), the distance from that point to a location for which a
  search was performed, and an array of pointers to the data associated
  with the location.)

  @longcode(#
  TOctPoint =  record
    Data : TPointerArray;
    Distance : double;
    X: double;
    Y: double;
    Z: double;
  end;
  #)
  }
  TOctPoint = record
    Data: TPointerArray;
    Distance: double;
    X: double;
    Y: double;
    Z: double;
  end;

  //@longcode(#TOctPointArray = array of TOctPoint;#)
  TOctPointArray = array of TOctPoint;

  {@abstract(@name records contain the X, Y, and Z coordinates of a location
  in a TRbwOctTree, and an array of pointers to the data associated
  with the location. These points are all located inside the extent of a
  @link(T3DBlock) that was used in a search.)

  @longcode(#
  TOctPointInRegion =  record
    Data : TPointerArray;
    X: double;
    Y: double;
    Z: double;
  end;
  #)
  }
  TOctPointInRegion = record
    // @name is an array of pointers to the data associated
    // with the location.
    Data: TPointerArray;
    // @name is the X coordinate of the data.
    X: double;
    // @name is the Y coordinate of the data.
    Y: double;
    // @name is the Z coordinate of the data.
    Z: double;
  end;

  //@longcode(#TOctPointInRegionArray = array of TOctPointInRegion;#)
  TOctPointInRegionArray = array of TOctPointInRegion;

  // See TQtreeNode.@link(TOtreeNode.ExpandBounds).
  TExpandDirection3 = (ed3North, ed3South, ed3East, ed3West, ed3Up, ed3Down);

  {@abstract(@name delineates a region of space. T3DBlock's are used to perform
  a search for locations inside a @link(TRbwOctTree).)

  @longcode(#
  T3DBlock = record
    XMax : double;
    XMin : double;
    YMax : double;
    YMin : double;
    ZMax : double;
    ZMin : double;
  end;
  #)
  }
  T3DBlock = record
    XMax: double;
    XMin: double;
    YMax: double;
    YMin: double;
    ZMax: double;
    ZMin: double;
  end;

  //  EOTreeError is a class for exceptions raised by @link(TRbwOctTree).
  EOTreeError = class(Exception);

  TRbwOctTree = class;

  // @name does the most of the real work of a TRbwOctTree.
  TOtreeNode = class(TObject)
  private
    // @longcode(#FNumPts: Integer;#)
    // @name is the number of locations stored by this @classname
    // or its children. See @link(NumPts).
    FNumPts: Integer;
    // @name finds the Count nearest locations to the location
    // (CenterX, CenterY, CenterZ) and store TSelectNode's representing
    // those locations in List.  (TSelectNode is defined in
    // the implementation section.)  The number of objects
    // in List may be greater then Count because several points
    // may be the same distance from (CenterX, CenterY, CenterZ).
    procedure FindNearestPoints(const CenterX, CenterY, CenterZ: double;
      const Count: Integer; const List: TObjectList);
    // See @link(Points).
    function GetPoints(Index: Integer): TOPoint;
    // If a @classname has children, @name sets @link(NumPts)
    // to the sum of its childrens' @link(NumPts).
    procedure ResetCount;
    // See @link(NumPts).
    // @name may free child nodes if
    // Value < FTree.@link(TRbwOctTree.MaxPoints)
    procedure SetNumPts(const Value: Integer);
  protected
    {@name: array [@link(TNorthOrSouth), @link(TEastOrWest), @link(TUpOrDown)]
     of @link(TOtreeNode);
     If @link(NumPts) <= FTree.@link(TRbwOctTree.MaxPoints), then
     FChildren will normally contain all nil values and the data
     in the @classname will be stored in @link(FPts).
     Otherwise, @name will have all instances of @link(TOtreeNode)
     and they will hold the data.
    }
    FChildren: array[TNorthOrSouth, TEastOrWest, TUpOrDown] of TOtreeNode;
    // @longcode(#FParent : TOtreeNode;#)
    // @name is the @link(TOtreeNode) (if one exists) that has the
    // current instance in @link(FChildren).
    FParent: TOtreeNode;
    { longcode(#FPts: array of TOPoint;#)
    If @link(NumPts) <= FTree.@link(TRbwOctTree.MaxPoints), then
    the data in the @classname will be stored in @link(FPts).
    See @link(FChildren).}
    FPts: array of TOPoint;
    // @longcode(#FTree: TRbwOctTree;#)
    // @name is the @link(TRbwOctTree) that is the ultimate root
    // for the current @link(TOtreeNode).
    FTree: TRbwOctTree;
    // @longcode(#FXMax : double;#)
    // @name is the largest X coordinate that should be
    // stored in this @link(TOtreeNode).  However, see @link(ExpandBounds).
    FXMax: double;
    // @longcode(#FXMin : double;#)
    // @name is the smallest X coordinate that should be
    // stored in this @link(TOtreeNode).  However, see @link(ExpandBounds).
    FXMin: double;
    // @longcode(#FYMax : double;#)
    // @name is the largest Y coordinate that should be
    // stored in this @link(TOtreeNode).  However, see @link(ExpandBounds).
    FYMax: double;
    // @longcode(#FYMin : double;#)
    // @name is the smallest Y coordinate that should be
    // stored in this @link(TOtreeNode).  However, see @link(ExpandBounds).
    FYMin: double;
    // @longcode(#FZMax : double;#)
    // @name is the largest Z coordinate that should be
    // stored in this @link(TOtreeNode).  However, see @link(ExpandBounds).
    FZMax: double;
    // @longcode(#FZMin : double;#)
    // @name is the smallest Z coordinate that should be
    // stored in this @link(TOtreeNode).  However, see @link(ExpandBounds).
    FZMin: double;
    // @name adds the location (X,Y,Z) and it associated data to the
    // data stored by this @link(TOtreeNode).
    procedure AddPoint(const X, Y, Z: double; const Data: pointer);
    // @name is used when searching for the closest points
    // to X, Y, Z.
    // If exclude is self, the procedure immediately
    // exits.  This prevents infinite recursion.
    // Best_leaf is changed to the @link(TOtreeNode) that
    // contains the closest point to X, Y, Z.
    // Best_i is changed to the index of the data point in
    // best_leaf that contains the closest point to X, Y, Z.
    // Best_dist is the distance from X, Y, Z to the closest point
    // in best_leaf.
    // Best_dist2 is best_dist squared.
    procedure CheckNearbyLeaves(const exclude: TOtreeNode;
      var best_leaf: TOtreeNode; X, Y, Z: double; var best_i: integer;
      var best_dist2, best_dist: double);
    // Clear removes all the data stored by this @link(TOtreeNode).
    // Members of @link(FChildren), if any, will be freed.
    procedure Clear;
    // @name creates an instance of @classname
    constructor Create(const ATree: TRbwOctTree); overload;
    // @name creates an instance of @classname and sets @link(FXMax),
    // @link(FXMin), @link(FYMax), and @link(FYMin).
    constructor Create(const x_min, x_max, y_min, y_max, z_min, z_max: double;
      const ParentNode: TOtreeNode; const ATree: TRbwOctTree); overload;
    // @name is used to expand the range of X and Y coordinates that
    // can be stored in this @link(TOtreeNode) and its children.
    // See @link(FXMax), @link(FXMin), @link(FYMax), @link(FYMin),
    // @link(FZMax), and @link(FZMin).
    procedure ExpandBounds(XYZ: double; ExpandDirection: TExpandDirection3);
    // FindClosestPoint returns the X, Y, Z locations and first piece of data in the
    // point closest to the value specified for X, Y, and Z.
    // @name returns a @link(TOPoint) that has the X,Y,Z locations
    // and data in the
    // point closest to the value specified for X, Y, and Z.
    // the @link(TOPoint) is owned by a @link(TOtreeNode).
    function FindClosestPoint(const X, Y, Z: double): TOPoint;
    { @name, sets X, Y, and Z to the closest location to the original
      X, Y, Z and sets Data to hold the data at that location.}
    procedure FindClosestPointsData(var X, Y, Z: double; out Data:
      TPointerArray);
    // @name sets X, Y, and Z to the closest location in the @link(TOtreeNode)
    // to the original (X, Y, Z) and sets Data to the first data at that location.
    procedure FindPoint(var X, Y, Z: double; out Data: pointer);
    // @name, fills List with POPoint that point to locations within Block.
    procedure FindPointsInBlock(const Block: T3DBlock; const List: TList);
    // @name, fills List with @link(POPoint)s that point to locations within
    // the sphere defined by CenterX, CenterY, CenterZ, and Radius.
    procedure FindPointsInSphere(const CenterX, CenterY, CenterZ, Radius,
      RadiusSquared: double; const List: TList);
    // @name returns the TOtreeNode that contains the location (X,Y,Z).
    // Siblings is filled with other @classname that are siblings of the
    // one found or its ancestors.
    function LocateLeaf(const X, Y, Z: double; const Siblings: TStack):
      TOtreeNode;
    // @name sets best_i to indicate the point closest to (X,Y,Z)
    // in this leaf. best_dist2 is set to the square of the distance
    // to that point.
    procedure NearPointInLeaf(const X, Y, Z: double; out best_i: integer;
      out best_dist2: double);
    // @name is the number of locations stored by this @link(TOtreeNode).
    property NumPts: Integer read FNumPts write SetNumPts;
    // @name returns the @link(TOPoint) designated by Index.  The
    // @link(TOPoint) may be stored either in this @link(TOtreeNode)
    // or its children.
    property Points[Index: Integer]: TOPoint read GetPoints;
    // @name removes Data at location (X,Y,Z).  If this is the only
    // data at that point, it also removes the point.
    function RemovePoint(const X, Y, Z: double; const Data: pointer): boolean;
    // @name is the location in the X coordinate direction at which
    // the where eastern children meet the western children.  If no
    // children exist, it is the middle of the node in the X direction.
    function Xmid: double;
    // @name is the location in the Y coordinate direction at which
    // the where northern children meet the southern children.  If no
    // children exist, it is the middle of the node in the Y direction.
    function Ymid: double;
    // @name is the location in the Z coordinate direction at which
    // the where up children meet the down children.  If no
    // children exist, it is the middle of the node in the Z direction.
    function Zmid: double;
  public
    // @name destroys the current instance of
    // @link(TOtreeNode) and its children.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  end;

  { @abstract(@name is used to quickly retrieve data
    by their X, Y, and Z coordinates.)
    Methods are provided to search at a
    location or within a defined region}
  TRbwOctTree = class(TComponent)
  private
    //@name: integer;
    // See @link(MaxPoints).
    FMaxPoints: integer;
    //@name: @link(TOtreeNode);
    // @name does the actual work of the @classname.
    FOTreeNode: TOtreeNode;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Points).
    function GetPoints(const Index: Integer): TOctPoint;
    // See @link(XMax).
    function GetXMax: double;
    // See @link(XMin).
    function GetXMin: double;
    // See @link(YMax).
    function GetYMax: double;
    // See @link(YMin).
    function GetYMin: double;
    // See @link(ZMax).
    function GetZMax: double;
    // See @link(Count).
    function GetZMin: double;
    // See @link(MaxPoints).
    procedure SetMaxPoints(const Value: integer);
    // See @link(XMax).
    procedure SetXMax(const AValue: double);
    // See @link(XMin).
    procedure SetXMin(const AValue: double);
    // See @link(YMax).
    procedure SetYMax(const AValue: double);
    // See @link(YMin).
    procedure SetYMin(const AValue: double);
    // See @link(ZMax).
    procedure SetZMax(const AValue: double);
    // See @link(ZMin).
    procedure SetZMin(const AValue: double);
  public
    // @name stores Data at location X, Y, Z.
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
    // If required, @link(Xmax), @link(XMin), @link(Ymax), @link(Ymin),
    // @link(Zmax), or @link(Zmin) will be adjusted to
    // accomodate the new data point.
    //
    // The Data associated with a location is not owned by the
    // @link(TRbwOctTree);
    // users are responsible for freeing that memory when it is no longer
    // needed.
    procedure AddPoint(const X, Y, Z: double; const Data: pointer);
    // @name deletes all data locations in the @link(TRbwOctTree) and resets
    // the @link(Count) to 0.
    //
    // The Data associated with a location is not owned by the
    // @link(TRbwOctTree);
    // users are responsible for freeing that memory when it is no longer
    // needed.
    procedure Clear;
    // @name is the number of unique locations stored in a
    // @link(TRbwOctTree).
    property Count: integer read GetCount;
    // @name Creates an instance of @link(TRbwOctTree);
    constructor Create(AOwner: TComponent); override;
    // @name Destroys an instance of @link(TRbwOctTree).
    // Applications should not call
    // @name directly. Instead call Free;
    destructor Destroy; override;
    // @name locates the location nearest to the X, Y, Z values
    // provided as input.  It than returns the X, Y and Z values of that
    // location. The length of Data is changed to the number of pointers stored
    // at the location that was found and the Data pointers are copied into
    // Data.
    procedure FindClosestPointsData(var X, Y, Z: double;
      out Data: TPointerArray);
    // @name finds the Count locations in the @link(TRbwOctTree) that are
    // closest to CenterX, CenterY, CenterZ and returns them and their
    // associated data in Points. In the event of ties, the length of Points
    // may be larger than Count.  If the number of locations in the
    // @link(TRbwOctTree)
    // is less than Count, all the locations will be returned.  Points is
    // sorted from the closest locations to CenterX, CenterY, CenterZ to those
    // that are most distant.
    //
    // See also @link(FirstNearestPoint), @link(NearestPointsFirstData).
    procedure FindNearestPoints(const CenterX, CenterY, CenterZ: double;
      const Count: Integer; out Points: TOctPointArray);
    // @name finds all locations stored in the @link(TRbwOctTree) that
    // are inside Block and returns them in Points along with their associated
    // data. Points is not sorted.
    procedure FindPointsInBlock(const Block: T3DBlock;
      out Points: TOctPointInRegionArray);
    // @name finds all the locations stored in the @link(TRbwOctTree)
    // that are within a distance of Radius from CenterX, CenterY, CenterZ and
    // returns them and their associated data in Points.  Points is not sorted.
    procedure FindPointsInSphere(const CenterX, CenterY, CenterZ,
      Radius: double; out Points: TOctPointInRegionArray);
    // @name, finds the location in the @link(TRbwOctTree) that is
    // closest to X, Y, Z and returns it in X, Y, Z.  The associated data is
    // returned in Data. In the event of ties in location, the location that
    // is returned is chosen arbitrarily from the locations that are closest.
    // If more than one piece of data is stored at X, Y, Z, the one that is
    // returned is chosen arbitrarily.
    //
    // See also: @link(FindNearestPoints), @link(NearestPointsFirstData).
    procedure FirstNearestPoint(var X, Y, Z: double; out Data: pointer);
    // @name finds the location stored in the @link(TRbwOctTree)
    // that is closest to X, Y, Z and returns the first piece of data
    // associated with that location.
    //
    // See also: @link(FindNearestPoints), @link(FirstNearestPoint).
    function NearestPointsFirstData(X, Y, Z: double): pointer;
    // @name can be treated as an indexed array of the locations stored
    // in the @link(TRbwOctTree).
    property Points[const Index: Integer]: TOctPoint read GetPoints;
    // @name removes Data at location X, Y, Z.
    //
    // If the number of data pointers stored at X, Y, Z is greater than 1, Data
    // will be deleted from the list of data pointers and Count will be
    // unaffected.
    //
    // If there is only one data pointer stored at X, Y, Z, that location will
    // be deleted and the Count will be decremented by one.
    //
    // The Data associated with a location is not owned by the
    // @link(TRbwOctTree);
    // users are responsible for freeing that memory when it is no longer
    // needed.
    function RemovePoint(const X, Y, Z: double; const Data: pointer): boolean;
  published
    // @name is the maximum number of data locations in any node of a
    // TRbwOctTree. MaxPoints affects the performance of the @link(TRbwOctTree)
    // in the retrieval of data in a manner that depends on the particular
    // application.
    property MaxPoints: integer read FMaxPoints write SetMaxPoints;
    // @name is the largest X value of any of the data points that are expected
    // to be or have already been added to the @link(TRbwOctTree).  Although it is
    // not required that @name be greater than the X coordinate of all locations
    // added to the @link(TRbwOctTree), performance may suffer if too many of them
    // are greater than @name.
    property XMax: double read GetXMax write SetXMax;
    // @name is the smallest X value of any of the data points that are expected
    // to be or have already been added to the @link(TRbwOctTree).  Although it is
    // not required that @name be less than the X coordinate of all locations
    // added to the @link(TRbwOctTree), performance may suffer if too many of them
    // are less than @name.
    property XMin: double read GetXMin write SetXMin;
    // @name is the largest Y value of any of the data points that are expected
    // to be or have already been added to the @link(TRbwOctTree).  Although it is
    // not required that @name be greater than the Y coordinate of all locations
    // added to the @link(TRbwOctTree), performance may suffer if too many of them
    // are greater than @name.
    property YMax: double read GetYMax write SetYMax;
    // @name is the smallest Y value of any of the data points that are expected
    // to be or have already been added to the @link(TRbwOctTree).  Although it is
    // not required that @name be less than the Y coordinate of all locations
    // added to the @link(TRbwOctTree), performance may suffer if too many of them
    // are less than @name.
    property YMin: double read GetYMin write SetYMin;
    // @name is the largest Z value of any of the data points that are expected
    // to be or have already been added to the @link(TRbwOctTree).  Although it is
    // not required that @name be greater than the Z coordinate of all locations
    // added to the @link(TRbwOctTree), performance may suffer if too many of them
    // are greater than @name.
    property ZMax: double read GetZMax write SetZMax;
    // @name is the smallest Z value of any of the data points that are expected
    // to be or have already been added to the @link(TRbwOctTree).  Although it is
    // not required that @name be less than the Z coordinate of all locations
    // added to the @link(TRbwOctTree), performance may suffer if too many of them
    // are less than @name.
    property ZMin: double read GetZMin write SetZMin;
  end;

  // @name registers @link(TRbwOctTree).
procedure Register;

implementation

resourcestring
  StrErrorAttemptToCr = 'Error: attempt to create a TOtreeNode with an inval' +
  'id range.';
  StrErrorNoDataPoint = 'Error: No data points in OctTree.';
  StrInvalidPointIndex = 'Invalid point index < 0.';
  StrInvalidPointIndex2 = 'Invalid point index >= Count.';
  StrTheMaximumNumberO = 'The maximum number of points in an Octtree must be' +
  ' greater than zero.';
  StrYouMustSetTheNum = 'You must set the number of points before adding any' +
  ' points to a Octtree.';

type
  TSelectNode = class(TObject)
    Distance: double;
    Point: TOPoint;
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

constructor TOtreeNode.Create(const ATree: TRbwOctTree);
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  ud: TUpOrDown;
begin
  inherited Create;
  FParent := nil;
  FXMin := 0;
  FXMax := 0;
  FYMin := 0;
  FYMax := 0;
  FTree := ATree;
  for ns := North to South do
  begin
    for ew := East to West do
    begin
      for ud := Up to Down do
      begin
        FChildren[ns, ew, ud] := nil;
      end;
    end;
  end;
  FNumPts := 0;

  // Allocate space for the points.
  SetLength(FPts, FTree.MaxPoints);
end;

// Create and initialize the node.

constructor TOtreeNode.Create(const x_min, x_max, y_min, y_max, z_min, z_max:
  double;
  const ParentNode: TOtreeNode; const ATree: TRbwOctTree);
begin
  Create(ATree);
  if not ((x_min <= x_max) and (y_min <= y_max) and (z_min <= z_max)) then
  begin
    raise EOTreeError.Create(StrErrorAttemptToCr);
  end;
  FParent := ParentNode;
  FXMin := x_min;
  FXMax := x_max;
  FYMin := y_min;
  FYMax := y_max;
  FZMin := z_min;
  FZMax := z_max;
end;

// Free all children.

destructor TOtreeNode.Destroy;
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  ud: TUpOrDown;
  Index: integer;
  //  ListIndex : integer;
begin
  // If we have no children, free the point memory.
  if (FChildren[North, West, Up] = nil) then
  begin
    // If required, destroy the data.
    for Index := 0 to NumPts - 1 do
    begin
      FPts[Index].Data.Free;
    end;
    SetLength(FPts, 0);
  end
  else
  begin
    // Otherwise free the children.
    for ns := North to South do
      for ew := East to West do
        for ud := Up to Down do
          FChildren[ns, ew, ud].Free;
    // The Point memory doesn't have to be freed because it was
    // freed when the children were created;
  end;

  inherited Destroy;
end;

function TOtreeNode.RemovePoint(const X, Y, Z: double; const Data: pointer): boolean;
var
  xmidLocal, ymidLocal, zmidLocal: double;
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  ud: TUpOrDown;
  AnOctNode: TOtreeNode;
  Index: integer;
  DataCount: integer;
begin
  result := False;
  // If the point is outside the range of the TOtreeNode, expand the TOtreeNode to encompass it.
  if (X < FXMin) or (X > FXMax) or (Y < FYMin) or (Y > FYMax) then
    Exit;

  AnOctNode := self;
  // See if the node belongs in a child.
  while (AnOctNode.FChildren[North, West, Up] <> nil) do

  begin
    xmidLocal := AnOctNode.Xmid;
    ymidLocal := AnOctNode.Ymid;
    zmidLocal := AnOctNode.Zmid;

    // Check that children exist.

    // Add the point to the correct child.
    if (X <= xmidLocal) then
      ew := West
    else
      ew := East;
    if (Y <= ymidLocal) then
      ns := South
    else
      ns := North;
    if (Z <= zmidLocal) then
      ud := Down
    else
      ud := Up;
    AnOctNode := AnOctNode.FChildren[ns, ew, ud];
  end;

  // find point.
  for Index := 0 to AnOctNode.NumPts - 1 do
  begin
    if (AnOctNode.FPts[Index].X = X)
      and (AnOctNode.FPts[Index].Y = Y)
      and (AnOctNode.FPts[Index].Z = Z) then
    begin
      DataCount := AnOctNode.FPts[Index].Data.Count;
      // Remove the data
      AnOctNode.FPts[Index].Data.Remove(Data);
      result := AnOctNode.FPts[Index].Data.Count = DataCount -1;
      {      if Assigned(Tree.FDestroyProcedure) then
            begin
              Tree.FDestroyProcedure(Tree, Data);
            end;   }
            // if the number of data items associated with the data
            // point has bee reduced to 0, remove the data point.
      if AnOctNode.FPts[Index].Data.Count = 0 then
      begin
        AnOctNode.FPts[Index].Data.Free;
        AnOctNode.FPts[Index].Data := nil;
        if Index+1 < Length(AnOctNode.FPts) then
        begin
          Move(AnOctNode.FPts[Index+1], AnOctNode.FPts[Index],
            (AnOctNode.NumPts - Index - 1) * SizeOf(TOPoint));
        end;
//        for PointIndex := Index + 1 to AnOctNode.NumPts - 1 do
//        begin
//          AnOctNode.FPts[PointIndex - 1] := AnOctNode.FPts[PointIndex];
//        end;
        AnOctNode.NumPts := AnOctNode.NumPts - 1;
      end;
      Exit;
    end;
  end;
end;

procedure TOtreeNode.AddPoint(const X, Y, Z: double; const Data: pointer);
var
  xmidLocal, ymidLocal, zmidLocal: double;
  i: integer;
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  ud: TUpOrDown;
  AnOctNode, Child: TOtreeNode;
  Index: integer;
  APoint: TOPoint;
begin
  // If the point is outside the range of the TOtreeNode, expand the TOtreeNode to encompass it.
  if X < FXMin then
    ExpandBounds(X, ed3West);
  if X > FXMax then
    ExpandBounds(X, ed3East);
  if Y < FYMin then
    ExpandBounds(Y, ed3South);
  if Y > FYMax then
    ExpandBounds(Y, ed3North);
  if Z < FZMin then
    ExpandBounds(Z, ed3Down);
  if Z > FZMax then
    ExpandBounds(Z, ed3Up);

  AnOctNode := self;
  // See if the node belongs in a child.
  while (AnOctNode.NumPts >= FTree.MaxPoints) do

  begin
    xmidLocal := AnOctNode.Xmid;
    ymidLocal := AnOctNode.Ymid;
    zmidLocal := AnOctNode.Zmid;

    // See if we need to create new children.
    if (AnOctNode.FChildren[North, West, Up] = nil) then
    begin
      // Don't add duplicate point. Instead add data to existing point.
      for Index := 0 to AnOctNode.NumPts - 1 do
      begin
        if (AnOctNode.FPts[Index].X = X)
          and (AnOctNode.FPts[Index].Y = Y)
          and (AnOctNode.FPts[Index].Z = Z) then
        begin
          AnOctNode.FPts[Index].Data.Add(Data);
          Exit;
        end;
      end;
      
      // Split the node into eight children.
      AnOctNode.FChildren[North, West, Up] := TOtreeNode.Create
        (AnOctNode.FXMin, xmidLocal, ymidLocal, AnOctNode.FYMax,
        zmidLocal, AnOctNode.FZMax, AnOctNode, FTree);
      AnOctNode.FChildren[North, East, Up] := TOtreeNode.Create
        (xmidLocal, AnOctNode.FXMax, ymidLocal, AnOctNode.FYMax,
        zmidLocal, AnOctNode.FZMax, AnOctNode, FTree);
      AnOctNode.FChildren[South, West, Up] := TOtreeNode.Create
        (AnOctNode.FXMin, xmidLocal, AnOctNode.FYMin, ymidLocal,
        zmidLocal, AnOctNode.FZMax, AnOctNode, FTree);
      AnOctNode.FChildren[South, East, Up] := TOtreeNode.Create
        (xmidLocal, AnOctNode.FXMax, AnOctNode.FYMin, ymidLocal,
        zmidLocal, AnOctNode.FZMax, AnOctNode, FTree);
      AnOctNode.FChildren[North, West, Down] := TOtreeNode.Create
        (AnOctNode.FXMin, xmidLocal, ymidLocal, AnOctNode.FYMax,
        AnOctNode.FZMin, zmidLocal, AnOctNode, FTree);
      AnOctNode.FChildren[North, East, Down] := TOtreeNode.Create
        (xmidLocal, AnOctNode.FXMax, ymidLocal, AnOctNode.FYMax,
        AnOctNode.FZMin, zmidLocal, AnOctNode, FTree);
      AnOctNode.FChildren[South, West, Down] := TOtreeNode.Create
        (AnOctNode.FXMin, xmidLocal, AnOctNode.FYMin, ymidLocal,
        AnOctNode.FZMin, zmidLocal, AnOctNode, FTree);
      AnOctNode.FChildren[South, East, Down] := TOtreeNode.Create
        (xmidLocal, AnOctNode.FXMax, AnOctNode.FYMin, ymidLocal,
        AnOctNode.FZMin, zmidLocal, AnOctNode, FTree);

      // Move the old points into the new children.
      for i := 0 to AnOctNode.NumPts - 1 do
      begin
        APoint := AnOctNode.FPts[i];
        if (APoint.X <= xmidLocal) then
          ew := West
        else
          ew := East;
        if (APoint.Y <= ymidLocal) then
          ns := South
        else
          ns := North;
        if (APoint.Z <= zmidLocal) then
          ud := Down
        else
          ud := Up;
        Child := AnOctNode.FChildren[ns, ew, ud];
        Assert(Child.NumPts < FTree.MaxPoints);
        Child.FPts[Child.NumPts] := APoint;
        Child.FNumPts := Child.FNumPts + 1;
      end; // End moving points to the new children.

      // Free point memory.
      SetLength(AnOctNode.FPts, 0);

    end; // End creating new children.

    // Add the point to the correct child.
    if (X <= xmidLocal) then
      ew := West
    else
      ew := East;
    if (Y <= ymidLocal) then
      ns := South
    else
      ns := North;
    if (Z <= zmidLocal) then
      ud := Down
    else
      ud := Up;
    AnOctNode := AnOctNode.FChildren[ns, ew, ud];
  end;

  // Don't add duplicate point. Instead add data to existing point.
  for Index := 0 to AnOctNode.NumPts - 1 do
  begin
    if (AnOctNode.FPts[Index].X = X)
      and (AnOctNode.FPts[Index].Y = Y)
      and (AnOctNode.FPts[Index].Z = Z) then
    begin
      AnOctNode.FPts[Index].Data.Add(Data);
      Exit;
    end;
  end;

  // Place the point in this node.
  AnOctNode.FPts[AnOctNode.NumPts].X := X;
  AnOctNode.FPts[AnOctNode.NumPts].Y := Y;
  AnOctNode.FPts[AnOctNode.NumPts].Z := Z;
  AnOctNode.FPts[AnOctNode.NumPts].Data := TList.Create;
  AnOctNode.FPts[AnOctNode.NumPts].Data.Add(Data);
  AnOctNode.NumPts := AnOctNode.NumPts + 1;
end;

Type
  TStackCrack = class(TStack);

// Find the point closest to the given coordinates.

function TOtreeNode.FindClosestPoint(const X, Y, Z: double): TOPoint;
var
  best_dist2, best_dist: double;
  best_i: Integer;
  leaf, Aleaf: TOtreeNode;
  Siblings: TStack;
  ChildLeaf: TOtreeNode;
  Depth: Integer;
begin
  // See which leaf contains the point.
  Siblings := TStack.Create;
  try
    ChildLeaf := self;
    Depth := 1;
    while ChildLeaf <> nil do
    begin
      ChildLeaf := ChildLeaf.FChildren[North, West, Up];
      Inc(Depth);
    end;
    TStackCrack(Siblings).List.Capacity:= Depth * 7;

    leaf := LocateLeaf(X, Y, Z, Siblings);

    // Find the closest point within the leaf.
    leaf.NearPointInLeaf(X, Y, Z, best_i, best_dist2);

    // If no point was found, check the siblings of the leaf.
    if best_i < 0 then
    begin
      while Siblings.Count > 0 do
      begin
        Aleaf := Siblings.Pop;
        leaf := Aleaf.LocateLeaf(X, Y, Z, Siblings);

        // Find the closest point within the leaf.
        leaf.NearPointInLeaf(X, Y, Z, best_i, best_dist2);
        if best_i >= 0 then
        begin
          Break;
        end;
      end;
    end;

    best_dist := Sqrt(best_dist2);
    // Check nearby leaves for closer points.
    CheckNearbyLeaves(leaf, leaf, X, Y, Z, best_i, best_dist2, best_dist);

    Assert((best_i >= 0) and (best_i < Length(leaf.FPts)));
    result := leaf.FPts[best_i];
  finally
    Siblings.Free;
  end;
end;

procedure TOtreeNode.FindPoint(var X, Y, Z: double; out Data: pointer);
var
  APoint: TOPoint;
begin
  APoint := FindClosestPoint(X, Y, Z);
  X := APoint.X;
  Y := APoint.Y;
  Z := APoint.Z;
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

function TOtreeNode.LocateLeaf(const X, Y, Z: double; const Siblings: TStack):
  TOtreeNode;
var
  ns, nsi: TNorthOrSouth;
  ew, ewi: TEastOrWest;
  ud, udi: TUpOrDown;
begin
  Result := Self;

  // Search the appropriate child.
  while result.FChildren[North, West, Up] <> nil do
  begin
    if (X <= result.xmid) then
      ew := West
    else
      ew := East;
    if (Y <= result.ymid) then
      ns := South
    else
      ns := North;
    if (Z <= result.zmid) then
      ud := Down
    else
      ud := Up;

    // if the appropriate child doesn't have any points, we may
    // have to seach the child's siblings so save them.
    for nsi := North to South do
    begin
      for ewi := East to West do
      begin
        for udi := Up to Down do
        begin
          if (nsi <> ns) or (ewi <> ew) or (udi <> ud) then
          begin
            Siblings.Push(result.FChildren[nsi, ewi, udi]);
          end;
        end;
      end;
    end;
    Result := result.FChildren[ns, ew, ud];
  end;
end;

// Return the index of the point closest to the given
// coordinates in this leaf.

procedure TOtreeNode.NearPointInLeaf(const X, Y, Z: double; out best_i: Integer;
  out best_dist2: double);
var
  i: Longint;
  dist2: double;
  dx, dy, dz: double;
begin
  // If there are no points in the node, set best_i to -1 to indicate no
  // points were found.
  best_dist2 := 0;
  best_i := -1;

  if NumPts > 0 then
  begin
    dx := X - FPts[0].X;
    dy := Y - FPts[0].Y;
    dz := Z - FPts[0].Z;
    best_dist2 := dx * dx + dy * dy + dz * dz;
    best_i := 0;

    for i := 1 to NumPts - 1 do
    begin
      dx := X - FPts[i].X;
      dy := Y - FPts[i].Y;
      dz := Z - FPts[i].Z;
      dist2 := dx * dx + dy * dy + dz * dz;
      if (best_dist2 > dist2) then
      begin
        best_i := i;
        best_dist2 := dist2;
      end;
    end;
  end;
end;

// Check nearby leaves to see if there is a better point.

procedure TOtreeNode.CheckNearbyLeaves(const exclude: TOtreeNode;
  var best_leaf: TOtreeNode; X, Y, Z: double; var best_i: integer;
  var best_dist2, best_dist: double);
var
  xmidLocal, ymidLocal, zmidLocal: double;
  i: Integer;
  dist2: double;
begin
  // If we are excluding this leaf, do nothing.
  if (exclude = Self) then
    exit;

  // If this is a leaf node, look for close nodes.
  if (FChildren[North, West, Up] = nil) then
  begin
    NearPointInLeaf(X, Y, Z, i, dist2);
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
    zmidLocal := Zmid;
    if (best_i < 0) or (X - best_dist <= xmidLocal) then
    begin
      // The West children may be close enough.
      // See if the SouthWest child is close enough.
      if (Y - best_dist <= ymidLocal) or (best_i < 0) then
      begin
        // See if the SouthWestDown child is close enough.

        if (Z - best_dist <= ZmidLocal) or (best_i < 0) then
        begin

          FChildren[South, West, Down].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X - best_dist <= xmidLocal) then
    begin
      // The West children may be close enough.
      // See if the SouthWest child is close enough.
      if (Y - best_dist <= ymidLocal) or (best_i < 0) then
      begin
        // See if the SouthWestUp child is close enough.

        if (Z + best_dist >= ZmidLocal) or (best_i < 0) then
        begin

          FChildren[South, West, Up].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X - best_dist <= xmidLocal) then
    begin
      // The West children may be close enough.
      // See if the NorthWest child is close enough.
      if (Y + best_dist >= ymidLocal) or (best_i < 0) then
      begin
        // See if the NorthWestDown child is close enough.
        if (Z - best_dist <= zmidLocal) or (best_i < 0) then
        begin
          FChildren[North, West, Down].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X - best_dist <= xmidLocal) then
    begin
      // The West children may be close enough.
      // See if the NorthWest child is close enough.
      if (Y + best_dist >= ymidLocal) or (best_i < 0) then
      begin
        // See if the NorthWestUp child is close enough.
        if (Z + best_dist >= zmidLocal) or (best_i < 0) then
        begin
          FChildren[North, West, Up].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X + best_dist >= xmidLocal) then
    begin
      // The East children may be close enough.
      // See if the SouthEast child is close enough.
      if (Y - best_dist <= ymidLocal) or (best_i < 0) then
      begin
        // See if the SouthEastDown child is close enough.
        if (Z - best_dist <= zmidLocal) or (best_i < 0) then
        begin
          FChildren[South, East, Down].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X + best_dist >= xmidLocal) then
    begin
      // The East children may be close enough.
      // See if the SouthEast child is close enough.
      if (Y - best_dist <= ymidLocal) or (best_i < 0) then
      begin
        // See if the SouthEastUp child is close enough.
        if (Z + best_dist >= zmidLocal) or (best_i < 0) then
        begin
          FChildren[South, East, Up].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X + best_dist >= xmidLocal) then
    begin
      // The East children may be close enough.
      // See if the NorthEast child is close enough.
      if (Y + best_dist >= ymidLocal) or (best_i < 0) then
      begin
        if (Z - best_dist <= zmidLocal) or (best_i < 0) then
        begin
          FChildren[North, East, Down].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

    if (best_i < 0) or (X + best_dist >= xmidLocal) then
    begin
      // The East children may be close enough.
      // See if the NorthEast child is close enough.
      if (Y + best_dist >= ymidLocal) or (best_i < 0) then
      begin
        if (Z + best_dist >= zmidLocal) or (best_i < 0) then
        begin
          FChildren[North, East, Up].CheckNearbyLeaves(
            exclude, best_leaf, X, Y, Z, best_i,
            best_dist2, best_dist);
        end;
      end;
    end;

  end; // End if a leaf ... else check children ...
end;

procedure TOtreeNode.ExpandBounds(XYZ: double;
  ExpandDirection: TExpandDirection3);
begin
  case ExpandDirection of
    ed3North:
      begin
        FYMax := XYZ;
        if FChildren[North, East, Up] <> nil then
        begin
          FChildren[North, East, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, West, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, East, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, West, Down].ExpandBounds(XYZ, ExpandDirection);
        end;
      end;
    ed3South:
      begin
        FYMin := XYZ;
        if FChildren[North, East, Up] <> nil then
        begin
          FChildren[South, East, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, West, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, East, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, West, Down].ExpandBounds(XYZ, ExpandDirection);
        end;
      end;
    ed3East:
      begin
        FXMax := XYZ;
        if FChildren[North, East, Up] <> nil then
        begin
          FChildren[North, East, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, East, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, East, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, East, Down].ExpandBounds(XYZ, ExpandDirection);
        end;
      end;
    ed3West:
      begin
        FXMin := XYZ;
        if FChildren[North, East, Up] <> nil then
        begin
          FChildren[North, West, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, West, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, West, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, West, Down].ExpandBounds(XYZ, ExpandDirection);
        end;
      end;
    ed3Up:
      begin
        FZMax := XYZ;
        if FChildren[North, East, Up] <> nil then
        begin
          FChildren[North, East, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, East, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, West, Up].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, West, Up].ExpandBounds(XYZ, ExpandDirection);
        end;
      end;
    ed3Down:
      begin
        FZMin := XYZ;
        if FChildren[North, East, Up] <> nil then
        begin
          FChildren[North, East, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, East, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[North, West, Down].ExpandBounds(XYZ, ExpandDirection);
          FChildren[South, West, Down].ExpandBounds(XYZ, ExpandDirection);
        end;
      end;
  end;
end;

function TOtreeNode.Xmid: double;
begin
  if FChildren[North, West, Up] = nil then
  begin
    result := (FXMax + FXMin) / 2
  end
  else
  begin
    result := FChildren[North, West, Up].FXMax;
  end;
end;

function TOtreeNode.Ymid: double;
begin
  if FChildren[North, East, Up] = nil then
  begin
    result := (FYMax + FYMin) / 2
  end
  else
  begin
    result := FChildren[North, East, Up].FYMin;
  end;
end;

function TOtreeNode.Zmid: double;
begin
  if FChildren[North, East, Up] = nil then
  begin
    result := (FZMax + FZMin) / 2
  end
  else
  begin
    result := FChildren[North, East, Up].FZMin;
  end;
end;

{ TRbwOctTree }

function TRbwOctTree.GetXMax: double;
begin
  result := FOTreeNode.FXMax;
end;

function TRbwOctTree.GetXMin: double;
begin
  result := FOTreeNode.FXMin;
end;

function TRbwOctTree.GetYMax: double;
begin
  result := FOTreeNode.FYMax;
end;

function TRbwOctTree.GetYMin: double;
begin
  result := FOTreeNode.FYMin;
end;

function TRbwOctTree.GetZMax: double;
begin
  result := FOTreeNode.FZMax;
end;

function TRbwOctTree.GetZMin: double;
begin
  result := FOTreeNode.FZMin;
end;

procedure TRbwOctTree.SetXMax(const AValue: double);
begin
  if Count = 0 then
  begin
    FOTreeNode.FXMax := AValue;
  end
  else
  begin
    if (AValue < XMax) then
    begin
      raise
        EOTreeError.Create('Error: The maximum X value can not be decreased; '
        + 'only increased.');
    end;
    FOTreeNode.ExpandBounds(AValue, ed3West);
  end;
end;

procedure TRbwOctTree.SetXMin(const AValue: double);
begin
  if Count = 0 then
  begin
    FOTreeNode.FXMin := AValue;
  end
  else
  begin
    if (AValue > XMin) then
    begin
      raise
        EOTreeError.Create('Error: The minimum X value can not be increased; '
        + 'only decreased.');
    end;
    FOTreeNode.ExpandBounds(AValue, ed3East);
  end;
end;

procedure TRbwOctTree.SetYMax(const AValue: double);
begin
  if Count = 0 then
  begin
    FOTreeNode.FYMax := AValue;
  end
  else
  begin
    if (AValue < YMax) then
    begin
      raise
        EOTreeError.Create('Error: The maximum Y value can not be decreased; '
        + 'only increased.');
    end;
    FOTreeNode.ExpandBounds(AValue, ed3North);
  end;
end;

procedure TRbwOctTree.SetYMin(const AValue: double);
begin
  if Count = 0 then
  begin
    FOTreeNode.FYMin := AValue;
  end
  else
  begin
    if (AValue > YMin) then
    begin
      raise
        EOTreeError.Create('Error: The minimum Y value can not be increased; '
        + 'only decreased.');
    end;
    FOTreeNode.ExpandBounds(AValue, ed3South);
  end;
end;

procedure TRbwOctTree.SetZMax(const AValue: double);
begin
  if Count = 0 then
  begin
    FOTreeNode.FZMax := AValue;
  end
  else
  begin
    if (AValue < ZMax) then
    begin
      raise
        EOTreeError.Create('Error: The maximum X value can not be decreased; '
        + 'only increased.');
    end;
    FOTreeNode.ExpandBounds(AValue, ed3Up);
  end;
end;

procedure TRbwOctTree.SetZMin(const AValue: double);
begin
  if Count = 0 then
  begin
    FOTreeNode.FZMin := AValue;
  end
  else
  begin
    if (AValue > ZMin) then
    begin
      raise
        EOTreeError.Create('Error: The minimum X value can not be increased; '
        + 'only decreased.');
    end;
    FOTreeNode.ExpandBounds(AValue, ed3Down);
  end;
end;

procedure TRbwOctTree.AddPoint(const X, Y, Z: double; const Data: pointer);
begin
  FOTreeNode.AddPoint(X, Y, Z, Data);
end;

constructor TRbwOctTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxPoints := 100;
  //  FDestroyProcedure := nil;
  FOTreeNode := TOtreeNode.Create(self);
end;

destructor TRbwOctTree.Destroy;
begin
  FOTreeNode.Free;
  inherited;
end;

function TRbwOctTree.NearestPointsFirstData(X, Y, Z: double): pointer;
begin
  if FOTreeNode.NumPts > 0 then
  begin
    FirstNearestPoint(X, Y, Z, result);
  end
  else
  begin
    result := nil;
  end;
end;

procedure TRbwOctTree.FirstNearestPoint(var X, Y, Z: double; out Data: pointer);
begin
  if FOTreeNode.NumPts > 0 then
  begin
    FOTreeNode.FindPoint(X, Y, Z, Data);
  end
  else
  begin
    raise EOTreeError.Create(StrErrorNoDataPoint);
  end;
end;

procedure TOtreeNode.SetNumPts(const Value: Integer);
var
  OldNumPoints {, Delta}: Integer;
  ew: TEastOrWest;
  ns: TNorthOrSouth;
  ud: TUpOrDown;
  AChild: TOtreeNode;
  Index: integer;
  PtCount: integer;
begin
  if FNumPts <> Value then
  begin
    OldNumPoints := FNumPts;
    FNumPts := Value;

    if (FNumPts <= FTree.MaxPoints) and (OldNumPoints > FTree.MaxPoints) then
    begin
      SetLength(FPts, FTree.MaxPoints);
      PtCount := 0;
      for ns := North to South do
      begin
        for ew := East to West do
        begin
          for ud := Up to Down do
          begin
            AChild := FChildren[ns, ew, ud];
            for Index := 0 to AChild.NumPts - 1 do
            begin
              Assert(PtCount < FTree.MaxPoints);
              FPts[PtCount] := AChild.FPts[Index];
              Inc(PtCount);
            end;
            AChild.FNumPts := 0;
            SetLength(AChild.FPts, 0);
            AChild.Free;
            FChildren[ns, ew, ud] := nil;
          end;
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

function TOtreeNode.GetPoints(Index: Integer): TOPoint;
var
  ew: TEastOrWest;
  ns: TNorthOrSouth;
  ud: TUpOrDown;
  AChild: TOtreeNode;
begin
  if FChildren[North, East, Up] = nil then
  begin
    result := FPts[Index];
  end
  else
  begin
    for ew := West downto East do
    begin
      for ns := North to South do
      begin
        for ud := Up to Down do
        begin
          AChild := FChildren[ns, ew, ud];
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
end;

procedure TOtreeNode.FindClosestPointsData(var X, Y, Z: double;
  out Data: TPointerArray);
var
  APoint: TOPoint;
  Index: integer;
begin
  APoint := FindClosestPoint(X, Y, Z);
  X := APoint.X;
  Y := APoint.Y;
  Z := APoint.Z;
  SetLength(Data, APoint.Data.Count);
  for Index := 0 to APoint.Data.Count - 1 do
  begin
    Data[Index] := APoint.Data[Index];
  end;
end;

procedure TRbwOctTree.FindClosestPointsData(var X, Y, Z: double;
  out Data: TPointerArray);
begin
  if FOTreeNode.NumPts > 0 then
  begin
    FOTreeNode.FindClosestPointsData(X, Y, Z, Data);
  end
  else
  begin
    raise EOTreeError.Create(StrErrorNoDataPoint);
  end;
end;

function TRbwOctTree.GetCount: integer;
begin
  result := FOTreeNode.NumPts;
end;

function TRbwOctTree.RemovePoint(const X, Y, Z: double; const Data: pointer): boolean;
begin
  result := FOTreeNode.RemovePoint(X, Y, Z, Data);
end;

function TRbwOctTree.GetPoints(const Index: Integer): TOctPoint;
var
  OPoint: TOPoint;
  DataIndex: integer;
begin
  if Index < 0 then
  begin
    raise EOTreeError.Create(StrInvalidPointIndex);
  end;
  if Index >= Count then
  begin
    raise EOTreeError.Create(StrInvalidPointIndex2);
  end;
  OPoint := FOTreeNode.Points[Index];
  result.X := OPoint.X;
  result.Y := OPoint.Y;
  result.Z := OPoint.Z;
  SetLength(result.Data, OPoint.Data.Count);
  for DataIndex := 0 to OPoint.Data.Count - 1 do
  begin
    result.Data[DataIndex] := OPoint.Data[DataIndex];
  end;
end;

procedure TOtreeNode.ResetCount;
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  ud: TUpOrDown;
  Count: integer;
begin
  if FChildren[North, West, Up] <> nil then
  begin
    Count := 0;
    for ns := North to South do
    begin
      for ew := East to West do
      begin
        for ud := Up to Down do
        begin
          Count := Count + FChildren[ns, ew, ud].NumPts;
        end;
      end;
    end;
    NumPts := Count;
  end;
end;

procedure TOtreeNode.FindNearestPoints(const CenterX, CenterY, CenterZ: double;
  const Count: Integer; const List: TObjectList);
var
  Radius: double;
  Node: TSelectNode;
  distance: double;
  APoint: TOPoint;
  dx, dy, dz: double;
  ALeaf: TOtreeNode;
  Siblings: TStack;
  ChildLeaf: TOtreeNode;
  Depth: Integer;
  procedure GetPoints;
  var
    Index, InnerIndex: integer;
  begin
    for Index := 0 to ALeaf.NumPts - 1 do
    begin
      APoint := ALeaf.FPts[Index];
      dx := APoint.X - CenterX;
      dy := APoint.Y - CenterY;
      dz := APoint.Z - CenterZ;
      distance := Sqrt(Sqr(dx) + Sqr(dy) + Sqr(dz));
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
      ChildLeaf := ChildLeaf.FChildren[North, West, Up];
      Inc(Depth);
    end;
    TStackCrack(Siblings).List.Capacity:= Depth * 7;

    ALeaf := LocateLeaf(CenterX, CenterY, CenterZ, Siblings);
    radius := 0;
    GetPoints;
    while Siblings.Count > 0 do
    begin
      ALeaf := Siblings.Pop;

      if (List.Count < Count) or
        ((CenterX + radius >= ALeaf.FXMin) and
        (CenterX - radius <= ALeaf.FXMax) and
        (CenterY + radius >= ALeaf.FYMin) and
        (CenterY - radius <= ALeaf.FYMax) and
        (CenterZ + radius >= ALeaf.FZMin) and
        (CenterZ - radius <= ALeaf.FZMax)) then
      begin
        ALeaf := ALeaf.LocateLeaf(CenterX, CenterY, CenterZ, Siblings);
        GetPoints;
      end;
    end;
  finally
    Siblings.Free;
  end;
end;

procedure TOtreeNode.FindPointsInSphere(const CenterX, CenterY, CenterZ,
  Radius, RadiusSquared: double; const List: TList);
var
  AChild: TOtreeNode;
  xmidLocal, ymidLocal, zmidLocal: double;
  Index: integer;
  dx, dy, dz: double;
  APoint: TOPoint;
  WholeBlockInside: boolean;
begin
  if FChildren[North, West, Up] = nil then
  begin
    // see if it is worth trying to determine if all the points
    // are inside the circle;
    WholeBlockInside := NumPts > 8;

    // Test each corner to see whether it is inside the circle
    if WholeBlockInside then
    begin
      dx := FXMax - CenterX;
      dy := FYMax - CenterY;
      dZ := FZMax - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMax - CenterX;
      dy := FYMin - CenterY;
      dZ := FZMax - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMin - CenterX;
      dy := FYMax - CenterY;
      dZ := FZMax - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMin - CenterX;
      dy := FYMin - CenterY;
      dZ := FZMax - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMax - CenterX;
      dy := FYMax - CenterY;
      dZ := FZMin - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMax - CenterX;
      dy := FYMin - CenterY;
      dZ := FZMin - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMin - CenterX;
      dy := FYMax - CenterY;
      dZ := FZMin - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
      begin
        WholeBlockInside := False;
      end;
    end;

    if WholeBlockInside then
    begin
      dx := FXMin - CenterX;
      dy := FYMin - CenterY;
      dZ := FZMin - CenterZ;
      if Sqr(dx) + Sqr(dy) + Sqr(dz) > RadiusSquared then
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
        dz := APoint.Z - CenterZ;
        if Sqr(dx) + Sqr(dy) + Sqr(dz) <= RadiusSquared then
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
    zmidLocal := Zmid;
    if (CenterX - Radius <= xmidLocal) then
    begin
      // West Children are candidates.
      if CenterY - Radius <= ymidLocal then
      begin
        //South Children are candidates.
        if CenterZ - Radius <= zmidLocal then
        begin
          // Down Children are candidates
          AChild := FChildren[South, West, Down];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;

        if CenterZ + Radius >= zmidLocal then
        begin
          // Up Children are candidates
          AChild := FChildren[South, West, Up];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
      end;

      if CenterY + Radius >= ymidLocal then
      begin
        //North Children are candidates.
        if CenterZ - Radius <= zmidLocal then
        begin
          // Down Children are candidates
          AChild := FChildren[North, West, Down];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
        if CenterZ + Radius >= zmidLocal then
        begin
          // Up Children are candidates
          AChild := FChildren[North, West, Up];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
      end;
    end;

    if (CenterX + Radius >= xmidLocal) then
    begin
      // East Children are candidates.
      if CenterY - Radius <= ymidLocal then
      begin
        //South Children are candidates.
        if CenterZ - Radius <= zmidLocal then
        begin
          // Down Children are candidates
          AChild := FChildren[South, East, Down];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
        if CenterZ + Radius >= zmidLocal then
        begin
          // Up Children are candidates
          AChild := FChildren[South, East, Up];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
      end;

      if CenterY + Radius >= ymidLocal then
      begin
        //North Children are candidates.
        if CenterZ - Radius <= zmidLocal then
        begin
          // Down Children are candidates
          AChild := FChildren[North, East, Down];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
        if CenterZ + Radius >= zmidLocal then
        begin
          // Up Children are candidates
          AChild := FChildren[North, East, Up];
          AChild.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
            RadiusSquared, List);
        end;
      end;
    end
  end;
end;

procedure TOtreeNode.FindPointsInBlock(const Block: T3DBlock;
  const List: TList);
var
  AChild: TOtreeNode;
  xmidLocal, ymidLocal, zmidLocal: double;
  Index: integer;
  //  dx, dy : double;
  APoint: TOPoint;
begin
  if FChildren[North, West, Up] = nil then
  begin
    if (Block.XMin <= FXMin) and (Block.XMax >= FXMax)
      and (Block.YMin <= FYMin) and (Block.YMax >= FYMax)
      and (Block.ZMin <= FZMin) and (Block.ZMax >= FZMax) then
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
          and (Block.YMin <= APoint.Y) and (Block.YMax >= APoint.Y)
          and (Block.ZMin <= APoint.Z) and (Block.ZMax >= APoint.Z) then
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
    zmidLocal := Zmid;
    if (Block.XMin <= xmidLocal) then
    begin
      // West Children are candidates.
      if Block.YMin <= ymidLocal then
      begin
        //South Children are candidates.
        if Block.ZMin <= zmidLocal then
        begin
          //Down Children are candidates
          AChild := FChildren[South, West, Down];
          AChild.FindPointsInBlock(Block, List);
        end;
        if Block.ZMax >= zmidLocal then
        begin
          //Up Children are candidates
          AChild := FChildren[South, West, Up];
          AChild.FindPointsInBlock(Block, List);
        end;

      end;

      if Block.YMax >= ymidLocal then
      begin
        //North Children are candidates.
        if Block.ZMin <= zmidLocal then
        begin
          //Down Children are candidates
          AChild := FChildren[North, West, Down];
          AChild.FindPointsInBlock(Block, List);
        end;
        if Block.ZMax >= zmidLocal then
        begin
          //Up Children are candidates
          AChild := FChildren[North, West, Up];
          AChild.FindPointsInBlock(Block, List);
        end;
      end;
    end;

    if (Block.XMax >= xmidLocal) then
    begin
      // East Children are candidates.
      if Block.YMin <= ymidLocal then
      begin
        //South Children are candidates.
        if Block.ZMin <= zmidLocal then
        begin
          //Down Children are candidates
          AChild := FChildren[South, East, Down];
          AChild.FindPointsInBlock(Block, List);
        end;
        if Block.ZMax >= zmidLocal then
        begin
          //Up Children are candidates
          AChild := FChildren[South, East, Up];
          AChild.FindPointsInBlock(Block, List);
        end;
      end;

      if Block.YMax >= ymidLocal then
      begin
        //North Children are candidates.
        if Block.ZMin <= zmidLocal then
        begin
          //Down Children are candidates
          AChild := FChildren[North, East, Down];
          AChild.FindPointsInBlock(Block, List);
        end;
        if Block.ZMax >= zmidLocal then
        begin
          //Up Children are candidates
          AChild := FChildren[North, East, Up];
          AChild.FindPointsInBlock(Block, List);
        end;
      end;
    end
  end;
end;

procedure TRbwOctTree.FindPointsInBlock(const Block: T3DBlock;
  out Points: TOctPointInRegionArray);
var
  List: TList;
  PointIndex, DataIndex: integer;
  APoint: TOPoint;
  PointAddress: POPoint;
begin
  List := TList.Create;
  try
    List.Capacity := Count;
    FOTreeNode.FindPointsInBlock(Block, List);
    SetLength(Points, List.Count);
    for PointIndex := 0 to List.Count - 1 do
    begin
      PointAddress := List[PointIndex];
      APoint := PointAddress^;
      Points[PointIndex].X := APoint.X;
      Points[PointIndex].Y := APoint.Y;
      Points[PointIndex].Z := APoint.Z;
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

procedure TRbwOctTree.FindPointsInSphere(const CenterX, CenterY, CenterZ,
  Radius: double; out Points: TOctPointInRegionArray);
var
  List: TList;
  PointIndex, DataIndex: integer;
  APoint: TOPoint;
  PointAddress: POPoint;
begin
  List := TList.Create;
  try
    FOTreeNode.FindPointsInSphere(CenterX, CenterY, CenterZ, Radius,
      Sqr(Radius), List);
    SetLength(Points, List.Count);
    for PointIndex := 0 to List.Count - 1 do
    begin
      PointAddress := List[PointIndex];
      APoint := PointAddress^;
      Points[PointIndex].X := APoint.X;
      Points[PointIndex].Y := APoint.Y;
      Points[PointIndex].Z := APoint.Z;
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

procedure TRbwOctTree.FindNearestPoints(const CenterX, CenterY, CenterZ: double;
  const Count: Integer; out Points: TOctPointArray);
var
  List: TObjectList;
  PointIndex, DataIndex: integer;
  APoint: TOPoint;
  Node: TSelectNode;
  Radius: double;
begin
  List := TObjectList.Create;
  try
    FOTreeNode.FindNearestPoints(CenterX, CenterY, CenterZ, Count, List);

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
      Points[PointIndex].Z := APoint.Z;
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

procedure TRbwOctTree.SetMaxPoints(const Value: integer);
begin
  if Count = 0 then
  begin
    if Value <= 0 then
    begin
      raise EOTreeError.Create(StrTheMaximumNumberO);
    end;
    FMaxPoints := Value;
    SetLength(FOTreeNode.FPts, FMaxPoints);
  end
  else
  begin
    raise EOTreeError.Create(StrYouMustSetTheNum);
  end;

end;

procedure Register;
begin
  RegisterComponents('RBW', [TRbwOctTree]);
end;

procedure TOtreeNode.Clear;
var
  ns: TNorthOrSouth;
  ew: TEastOrWest;
  ud: TUpOrDown;
  Index: integer;
begin
  if FChildren[North, East, Up] = nil then
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
        for ud := Up to Down do
        begin
          FChildren[ns, ew, ud].Clear;
          FChildren[ns, ew, ud].Free;
          FChildren[ns, ew, ud] := nil;
        end;
      end;
    end;
  end;
  FNumPts := 0;
end;

procedure TRbwOctTree.Clear;
begin
  FOTreeNode.Clear;
  FOTreeNode.FNumPts := 0;
  SetLength(FOTreeNode.FPts, MaxPoints);
end;

end.

