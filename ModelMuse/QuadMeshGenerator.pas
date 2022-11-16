{ @name is used to generate a quadrilateral mesh using the algorithm
  derived from the one described in
  Sarrate, Josep, and Huerta, Antonio, 2000. Efficient unstructured
  quadrilateral mesh generation. International Journal of Numerical Methods
  in Engineering. 49:1327-1350.

  as modified in
  Bastian, M., and Li, B.Q., 2003. An efficient automatic mesh generator for
  quadrilateral elements implemented using C++. Finite Elements in Analysis
  and Design 39 (2003) 905–930.

  Other References:
  Zhu, J.Z., Zienkiewciz, O.C. Hinton, E. and Wu, J. 1991. A new approach to
  the development of automatic quadrilateral mesh generation. International
  Journal for Numerical Methods in Engineering. Vol: 849-866.

  Giuliani, S. 1982. An algorithm for continuous rezoning of the hydrodynamic
  grid in arbitrary Lagrangian-Eulerian computer codes. Nuclear Engineering
  and Design. 72: 205-212

  Sarrate, Josep, and Huerta, Antonio, 2001. An improved algorithm to smooth
  graded quadrilateral meshes preserving the prescribed element size.
  Communications in Numerical Methods in Engineering. 17:89-99.
}

unit QuadMeshGenerator;

interface

uses System.UITypes,
  Windows, Generics.Collections, Generics.Defaults, FastGEO, SysUtils,
  IntListUnit, MeshRenumbering, QuadTreeClass, MeshRenumberingTypes,
  JvCreateProcess, GoPhastTypes, SubPolygonUnit, System.Classes;

Type
  TPolygon2DArray = array of TPolygon2D;
  // @name indicates whether a @link(TSegment) is an interior segment created
  // during mesh generation or along either the exterior or an interior
  // @link(TBoundary).
  // @value(stInner The @link(TSegment) is an interior segment created
  // during mesh generation)
  // @value(stSubDomain The @link(TSegment) was created along one of the
  // interior @link(TBoundary) instances of the mesh.)
  // @value(stEdge The @link(TSegment) was created along the
  // exterior @link(TBoundary) instances of the mesh.)
  TSegmentType = (stInner, stSubDomain, stEdge);
  // @name indicates the method used to split @link(TBoundary) instances
  // with 6 edges into two @link(TBoundary) instances with four edges each.
  // @value(cmModifiedCost this is the method described by
  // Sarrate and Huerta, 2000.)
  // @value(cmTemplate This is the method described by Bastian, 2003.)
  // The default for @link(TQuadMeshCreator.SetSixNodeClosureMethod) is
  // cmTemplate.
  TSixNodeClosureMethod = (cmModifiedCost, cmTemplate);
  // @name indicates the method that is used to adjust the position of
  // nodes.
  // @value(namLagrange Move the @link(TNode) to the centroid of the elements
  // of which the @link(TNode) is a part.)
  // @value(namGiuliani Move the @link(TNode) using the algorithm described
  // by Giuliani, 1982.)
  // @value(namSarrateHuerta  Move the @link(TNode) using the algorithm described
  // by Sarrate and Huerta, 2001.)
  // @seealso(TNode.AdjustPositionLagrange)
  // @seealso(TNode.AdjustPositionGiuliani)
  TNodeAdjustmentMethod = (namLagrange, namGiuliani, namSarrateHuerta);

  TQuadMeshCreator = class;

  TNodeInBoundary = class;
  TNodeInBoundaryList = class(TList<TNodeInBoundary>);

  TBoundary = class;
  TBoundaryList = class(TList<TBoundary>);
  TBoundaryListSqr = TObjectList<TBoundaryList>;

  TSegment = class;
  TSegmentList = class(TList<TSegment>);
  TSegmentLists = TList<TSegmentList>;
  TSegmentObjectListsSqr = TObjectList<TSegmentLists>;

  TNode = class;
  TNodeList = class(TList<TNode>);

  // @name defines a node in the finite element mesh.
  TNode = class(TInterfacedObject, INode)
  private
    // See @link(Location).
    FLocation: TPoint2D;
    // See @link(NodeType).
    FNodeType: TNodeType;
    // @name is a list of @link(TBoundary) instances that have this @classname
    // as one of their nodes.
    FElements: TBoundaryList;
    // @name is a list of @link(TSegment) instances that contain this
    // @classname as one of their nodes.
    FSegments: TSegmentList;
    // @name is the owner of this @classname
    FQuadMeshCreator: TQuadMeshCreator;
    // See @link(NodeNumber).
    FNodeNumber: Integer;
    // See @link(DesiredSpacing).
    FDesiredSpacing: double;
    // See @link(DesiredElementCount).
    FDesiredElementCount: Integer;
    // @name is used internally to mark nodes that are at the location
    // where two or more of the original @link(TBoundary) instances cross
    // each other.
    FIntersection: Boolean;
    // @name is the sum of all the angles made at this @classname by
    // the edges of all the
    // elements that contain this @classname at this @classname.
    // It is used to assign @link(DesiredSpacing).
    FTotalAngle: double;
    // Move the node to the centroid of the elements
    // of which the node is a part.
    FConstraintNeighbors: TNodeList;
    FBypassUpdate: Boolean;
    procedure AdjustPositionLagrange;
    // Move the node using the algorithm described in Giuliani, 1982
    // or by Sarrate and Huerta, 2001.)
    procedure AdjustPositionGiuliani;
    // @name improves the topology of the mesh by
    // @link(NodeElimination),
    // @link(ElementElimination),
    // @link(SideElimination),
    // @link(InteriorElementElimination), and
    // @link(FanElimination).
    function ImproveTopology1: boolean;
    // @name improves the topology of the mesh by @link(DiagonalSwapping)
    function ImproveTopology2: boolean;
    // Eliminate interior nodes that are part of only two elements and
    // merge the two elements into a single element.
    // @name may result in triangular elements on the edge of the mesh which
    // must be cleaned up later.
    // (Zhu and others, 1991, figure 10).
    procedure NodeElimination;
    // When two interior nodes on opposite sides of an element both only
    // have three elements, move the two opposite nodes to a position
    // halfway between them and eliminate the element and one of the nodes.
    // (Zhu and others, 1991, figure 11).
    function ElementElimination: boolean;
    // @name merges elements that fan out from a single node.
    function FanElimination: boolean;
    // @name improves topology by combining elements when three of the nodes
    // of an element have only three elements each.
    function InteriorElementElimination: Boolean;
    // If two adjacent interior nodes both only
    // have three elements, eliminate both elements that are shared by
    // both nodes and from the merger of the two remaining elements, create
    // two new elements by inserting a new diagonal.
    // (Zhu and others, 1991, figure 13).
    function SideElimination: boolean;
    // Improve the topology by eliminating the shared side of two elements
    // and from the merger of the two remaining elements, create
    // two new elements by inserting a new diagonal.
    // (Zhu and others, 1991, figure 12).
    function DiagonalSwapping: boolean;
    // @name replaces all references to the current @classname in AnElement
    // with ReplacementNode.
    procedure ReplaceNodeInElement(ReplacementNode: TNode;
      AnElement: TBoundary);
    // @name returns the number of @link(IElement)s that have this @classname
    // as one of their boundary nodes.
    function GetActiveElementCount: Integer;
    // @name returns the @link(IElement) at position Index.
    function GetActiveElement(Index: Integer): IElement;
    // @name returns the node number of this @classname.
    function GetNodeNumber: Integer;
    // @name sets the node number of this @classname.
    procedure SetNodeNumber(Value: Integer);
    // @name returns the location of this @classname.
    function GetLocation: TPoint2D;
    // @name sets the location of this @classname.
    procedure SetLocation(const Value: TPoint2D);

    // @name returns the @link(TNodeType) of this node.
    function GetNodeType: TNodeType;
    // @name checks that a location is inside the meshing domain.
    function ValidPosition(ALocation: TPoint2D): boolean;
    // @name returns the highest and lowest angle of any element that
    // includes this @classname.
    procedure WorstAngles(var HighestAngle, LowestAngle: double);
//    procedure SetX(const Value: double);
//    procedure SetY(const Value: double);

    // @name fills NeighborNodes with a list of @link(TNode)s connected to the
    // central node in clockwise order around the central node.
    // @name also fills ElementList with a list of @link(TBoundary)s
    // connected to the central node in clockwise order around the central node.
    procedure GetNeighborNodesAndElements(NeighborNodes: TNodeList; ElementList: TBoundaryList);
    // @name fills NeighborNodes with a list of @link(TNode)s connected to the
    // central node.
    procedure GetNeighborNodes(FoundNodes: TNodeList);
  protected
    function GetBypassUpdate: Boolean;
    procedure SetBypassUpdate(const Value: Boolean);
    property BypassUpdate: Boolean read GetBypassUpdate write SetBypassUpdate;
  public
    { @name creates a @classname.
      @definitionlist(
        @itemLabel(QuadMeshCreator)
        @item(This is the @link(TQuadMeshCreator) that owns this @classname.)

        @itemLabel(DesiredSpacing)
        @item(This is the spacing that is desired between nodes for
         this @classname.)
      )
    }
    constructor Create(QuadMeshCreator: TQuadMeshCreator;
      DesiredSpacing: double);
    {@name frees @link(FSegments) and @link(FElements) and this @classname.}
    destructor Destroy; override;
    // @name is the desired spacing between nodes for this @classname.
    property DesiredSpacing: double read FDesiredSpacing write FDesiredSpacing;
    // @name is where this node is located.
    property Location: TPoint2D read FLocation write SetLocation;
    // @name is the X-coordinate of the @link(Location) of this @classname.
    property X: double read FLocation.X write FLocation.X;
    // @name is the Y-coordinate of the @link(Location) of this @classname.
    property Y: double read FLocation.Y write FLocation.Y;
    // @name indicates whether this @classname is an internal node,
    // and edge node or a node on an internal boundary of the mesh.
    property NodeType: TNodeType read GetNodeType;
    // @name indicates the number of elements that have this @classname
    // as one of their boundary nodes.
    property ElementCount: Integer read GetActiveElementCount;
    // @name is the optimal number of elements at this @classname.
    property DesiredElementCount: Integer read FDesiredElementCount;
    // @name is used to access @link(IElement) instances that contain this
    // @classname as one of their corners.
    property Elements[Index: Integer]: IElement read GetActiveElement;
    // @name is the number of this @classname.
    property NodeNumber: Integer read GetNodeNumber write SetNodeNumber;
  end;

  INodeComparer = IComparer<TNode>;

  // @name is used to sort @link(TNode)s based on the angle they make with
  // CenterNode in @link(Create).
  TNodeAngleComparer = class(TComparer<TNode>)
  private
    FCenterNode: TNode;
  public
    function Compare(const Left, Right: TNode): Integer; override;
    constructor Create(CenterNode: TNode);
  end;

  // @name is used to sort @link(TNode)s based on @link(TNode.DesiredSpacing).
  TNodeSpacingComparer = class(TComparer<TNode>)
  public
    function Compare(const Left, Right: TNode): Integer; override;
  end;

  TSegmentObjectList = class(TObjectList<TSegment>);

  { @name is a line segment connecting two @link(TNode)s. Additional
    @link(TNode)s may be inserted along its length.
  }
  TSegment = class(TObject)
  private
    // @name is the position of @link(FNode1) within @link(FBoundary).
    // It is set in @link(TBoundary.SetSegmentPositions).
    FPosition1: integer;
    // @name is the position of @link(FNode2) within @link(FBoundary).
    // It is set in @link(TBoundary.SetSegmentPositions).
    FPosition2: integer;
    // First endpoint @link(TNode) of the segment
    FNode1: TNode;
    // Second endpoint @link(TNode) of the segment
    FNode2: TNode;
    // @link(TNode)s inserted along length of @classname between @link(FNode1)
    // and @link(FNode2).
    FInnerNodes: TNodeList;
    // @name is the @link(TSegmentType) of this @classname. It is used to
    // set @link(TNode.NodeType) of @link(TNode) inserted into
    // @link(FInnerNodes)
    FSegmentType: TSegmentType;
    // @name is the @link(TBoundary) of which owns this @classname.
    FBoundary: TBoundary;
    // This is the @link(TQuadMeshCreator) that is creating the mesh.
    FQuadMeshCreator: TQuadMeshCreator;
    // @name returns the number of @link(TNode)s that ideally would be inserted
    // along this @classname. The actual number inserted might be larger
    // in order to ensure that the number of @link(TNode)s around a boundary
    // is even.
    function NodesToInsert: Integer;
    // @name reverses the direction of this @classname including inserted nodes.
    procedure Reverse;
    // @name creates a @classname and inserts itself on the
    // @link(TNode.FSegments) of @link(Node1) and @link(Node2).
    constructor Create(Node1, Node2: TNode; SegmentType: TSegmentType;
      Boundary: TBoundary; QuadMeshCreator: TQuadMeshCreator);
    // @name is the @link(TSegmentType) of this @classname. It is used to
    // set @link(TNode.NodeType) of @link(TNode) inserted into
    // @link(FInnerNodes)
    property SegmentType: TSegmentType read FSegmentType;
    // @name splits this @classname into two parts at ANode. ANode must be in
    // @link(FInnerNodes).
    function Split(ANode: TNode): TSegmentList;
    // @name creates a new @classname whose orientation is opposite this one.
    function CreateReversedSegment: TSegment;
    // @name is the distance between the end nodes of this @classname.
    // @seealso(FNode1)
    // @seealso(FNode2)
    function Length: double;
    // @name creates evenly spaced new nodes and inserts them in
    // @link(FInnerNodes).
    procedure InsertNodes(NumberToInsert: Integer);
    // @name returns @true if ANode is in this @classname.
    function ContainsNode(ANode: TNode): Boolean;
  {$IFDEF TEST}
  public
  {$ENDIF}
    // This is the first end node of this @classname.
    // Name is used only for testing.
    property Node1: TNode read FNode1;
    // This is the second end node of this @classname.
    // Name is used only for testing.
    property Node2: TNode read FNode2;
  public
  {$IFDEF TEST}
    // See @link(FInnerNodes).
    // Name is used only for testing.
    property InnerNodes: TNodeList read FInnerNodes;
  {$ENDIF}
    // @name destroys @link(FInnerNodes).
    destructor Destroy; override;
  end;

  // Each @classname represents a single vertex along the edge of a single
  // @link(TBoundary). While a @link(TNode) can be on many @link(TBoundary)
  // instances, each @classname is only on one @link(TBoundary).
  // Each @classname has a @link(TNode) as one of its members
  // (see @link(FNode)). Multiple instances of @classname on the same
  // @link(TBoundary) may have the same @link(TNode). However, they will
  // differ in @link(FPosition).
  // Each @name is owned by @link(TQuadMeshCreator.FBoundaryNodes).
  TNodeInBoundary = class(TObject)
  strict private
    // @name is a list of FSegments along a @link(TNodeInBoundary) that
    // will contain the @classname at @link(FPosition).
    FSegments: TSegmentList;
  private
    // @name is the @link(TNode) that defines the position in space of this
    // @Classname
    FNode: TNode;
    // @name is the position of this vertex of the @link(TBoundary) in the
    // list of vertices around the edge of the @link(TBoundary).
    FPosition: Integer;
    // @name is the @link(TBoundary) that contains this @classname.
    FBoundary: TBoundary;
    // @name returns true if this @classname and ANode are both along the same
    // @link(TSegment)
    function OnSameSegment(ANode: TNodeInBoundary): boolean;
    // See @link(DesiredSpacing).
    function GetDesiredSpacing: double;
    // See @link(X).
    function GetX: double;
    // See @link(Y).
    function GetY: double;
    // See @link(NodeType).
    function GetNodeType: TNodeType;
    // See @link(ElementCount).
    function GetElementCount: Integer;
    // See @link(Location).
    function GetLocation: TPoint2D;
    // See @link(SegmentCount)
    function GetSegmentCount: Integer;
    // See @link(Segments)
    function GetSegment(Index: Integer): TSegment;
    // See @link(Segments)
    procedure SetSegment(Index: Integer; const Value: TSegment);
    // @link(FPosition)
    property Position: Integer read FPosition;
    // See @link(TNode.DesiredSpacing).
    property DesiredSpacing: double read GetDesiredSpacing;
    { @name creates a new @classname.
      @definitionlist(
        @itemlabel(Node)
        @item(@name defines the spatial position of this @classname and
          a variety of other properties.)

        @itemlabel(Boundary)
        @item(@name is the @link(TBoundary) that includes this @classname.)

        @itemlabel(Segment)
        @item(@name is the @link(TSegment) along which this @classname lies.)
      )
    }
    Constructor Create(Node: TNode; Boundary: TBoundary; Segment: TSegment);
    // @name adds @link(TSegment)s to @link(FSegments).
    procedure AddSegment(ASegment: TSegment);
    // @name is the number of @link(TSegment)s in @link(FSegments).
    property SegmentCount: Integer read GetSegmentCount;
    // @name returns the @link(TSegment)s in @link(FSegments) at Index.
    property Segments[Index: Integer]: TSegment read GetSegment write SetSegment;
    // @name deletes the @link(TSegment) in @link(FSegments) at Index.
    procedure DeleteSegment(Index: Integer);
    // @name returns the position of @link(TSegment)s in @link(FSegments).
    function IndexOfSegment(ASegment: TSegment): integer;
    // @name reverses the order of @link(TSegment)s in @link(FSegments).
    procedure ReverseSegments;
    // @name inserts a @link(TSegment) into @link(FSegments) at Index.
    procedure InsertSegment(Index: Integer; ASegment: TSegment);
    // @name removes a @link(TSegment) from @link(FSegments).
    procedure RemoveSegment(ASegment: TSegment);
  public
  {$IFDEF TEST}
    // @name defines the position of this @classname.
    // @name is used only for testing.
    property Node: TNode read FNode;
  {$ENDIF}
    // @name destroys FSegments.
    destructor Destroy; override;
    // See @link(TNode.X).
    property X: double read GetX;
    // See @link(TNode.Y).
    property Y: double read GetY;
    // See @link(TNode.NodeType).
    property NodeType: TNodeType read GetNodeType;
    // See @link(TNode.ElementCount).
    property ElementCount: Integer read GetElementCount;
    // See @link(TNode.Location).
    property Location: TPoint2D read GetLocation;
  end;

  TNodeInBoundaryObjectList = TObjectList<TNodeInBoundary>;

  // @name is used to decide how to split a @link(TBoundary) into two
  // instances of @link(TBoundary). The @classname that is @link(Visible)
  // and has the lowest @link(Cost) is picked at the place to split the
  //  @link(TBoundary). The method used to compute the cost
  // (see @link(ComputeCost) has been modified from Sarrate and Huerta, 2000.
  TCost = class(TObject)
  private
    // @name is the first endpoint of the proposed location to split
    // a @link(TBoundary).
    FNode1: TNodeInBoundary;
    // @name is the second endpoint of the proposed location to split
    // a @link(TBoundary).
    FNode2: TNodeInBoundary;
    // @name is the @link(TQuadMeshCreator) that is creating the mesh.
    FQuadMeshCreator: TQuadMeshCreator;
    // See @link(Visible).
    FVisible: boolean;
    // See link(Cost).
    FCost: double;
    // @name is set to -1 in @link(Create) and then is set to the distance
    // between @link(FNode1) and @link(FNode2) in @link(ComputeCost).
    FNodeDistance: TFloat;
    // @name is used internally in @link(ComputeCost). It only needs to
    // be computed when the @classname is first created.
    FTheta1: TFloat;
    // @name is used internally in @link(ComputeCost). It only needs to
    // be computed when the @classname is first created.
    FTheta2: TFloat;
    // @name is used internally in @link(ComputeCost). It only needs to
    // be computed when the @classname is first created.
    FPhi: double;
    // @name is used internally in @link(ComputeCost). It only needs to
    // be computed when the @classname is first created.
    FNStar: double;
    // @name is used internally in @link(ComputeCost). It only needs to
    // be computed when the @classname is first created.
    F_l: double;
    // @name is used internally in @link(ComputeCost). It only needs to
    // be computed when the @classname is first created.
    FGamma: double;
    // @name is the area of the first polygon to be created by splitting a
    // @link(TBoundary).
    FArea1: double;
    // @name is the area of the second polygon to be created by splitting a
    // @link(TBoundary).
    FArea2: double;
    // @name is used to determine whether it would be OK to split the
    // @link(TBoundary) at @link(FNode1) and @link(FNode2). If it would be
    // OK, @link(Visible) is set to @true.
    // If the proposed splitting segment would cross another segment of the
    // boundary, the proposed split is not OK.
    procedure ComputeVisibility;
    // @name gets the instances of @link(TNodeInBoundary) that come just
    // before and just after @link(FNode1) and @link(FNode2).
    procedure GetNeighborNodes(var PriorNode1, SubsequentNode1, PriorNode2,
      SubsequentNode2: TNodeInBoundary);
    // Compute @link(Cost) using a method modified from
    // Sarrate and Huerta, 2000.
    procedure ComputeCost(LowestCost: TCost);
    // @name creates a @classname and calls @link(ComputeVisibility).
    Constructor Create(Node1, Node2: TNodeInBoundary;
      QuadMeshCreator: TQuadMeshCreator);
    // @name indicates whether or not the two @link(TNode)s that define this
    // @classname can be used to split the @link(TBoundary).
    property Visible: boolean read FVisible;
    // @name is a measure of the estimated quality of the position specified
    // for splitting a @link(TBoundary). Higher costs represent poorer
    // positions.
    property Cost: double read FCost;
  end;

  TNodeObjectList = class(TObjectList<TNode>);

  TBoundaryObjectList = class(TObjectList<TBoundary>);

  // @name represents a series of @link(TNode)s used to generate the mesh.
  // The nodes are listed in order in @link(FNodes). The boundary may
  // be either open or closed.  Closed boundaries have the same node at the
  // beginning and end of @link(FNodes).
  TBoundary = class(TNodeInBoundaryList, IElement)
  private
    // @name is the @link(TQuadMeshCreator) with which this @classname is
    // associated.
    FQuadMeshCreator: TQuadMeshCreator;
    // When a @classname is originally used to define a region where meshing
    // is to be performed, @name holds the @link(TNode)s that define the region.
    // Later on @name is no longer used.
    FNodes: TNodeList;
    // @name holds @link(TSegment)s around the edge of the @classname.
    FSegments: TSegmentObjectList;
    // @name is used with Gmsh
    FSubPoly: TSubPolygon;
{$IFDEF TEST}
  public
{$ENDIF}
    // When a @classname is @link(Split), @name is the @classname from which
    // it has been split.
    FParent: TBoundary;
    // When a @classname is @link(Split), @name holds the parts into which
    // it has been split.
    FSubParts: TBoundaryObjectList;
    // @name is the spacing between @link(TNode)s that would ideally be used
    // for this @classname.
    FDesiredSpacing: double;
    // @name is set to @true in @Link(ConvertToClosedBoundary) and tested in
    // @link(AssignOriginalEdgeAngles).
    FConverted: boolean;
    // @name is the number of this element.
    FElementNumber: Integer;
    // @name keeps track of how many references there are to this @classname.
    // This @classname will free itself if @name reaches zero.
    FRefCount: Integer;
    FBypassUpdate: Boolean;
    // @name makes sure that the @link(TSegment)s in @link(FSegments)
    // are correct.
    procedure FixSegments;
    // See @link(SubParts).
    function GetSubPart(Index: Integer): TBoundary;
    // @name inserts an even number of @link(TNode)s around the @classname
    procedure InsertNodesAlongBoundary;
    // @name converts and open to a closed boundary by adding @link(TSegment)s
    // that reverse the path from the beginning to the end.
    procedure ConvertToClosedBoundary;
    // @name assigns new boundary node numbers.
    procedure RenumberNodes;
    // @name sets the @link(TNodeType) of all @link(TNode)s in @link(FNodes).
    procedure SetNodeType(NodeType: TNodeType);
    // @name creates @link(TNodeInBoundary TNodeInBoundaries) along it's edge.
    procedure CreateBoundaryNodes;
    // @name finds the @link(TSegment) that contains ANode and splits the
    // segment if required.
    procedure SplitSegmentAtNode(ANode: TNodeInBoundary);
    // @name is used to split a @classname once the number of sides has
    // reached 6.
    function SpecialCase({$ifdef TEST}List: TBoundaryList {$ENDIF}) : boolean;
    // @name splits a @classname into three elements by adding a new point.
    procedure Split222(FirstIndex: Integer); overload;
    // @name splits a @classname with three sides each composed of 3 nodes
    //  into three elements with a new point at NewLocation.
    procedure Split222(FirstIndex: Integer; NewLocation: TPoint2D); overload;
    // @name splits a @classname that has three sides one of which has 4 points
    // in a line and another of which has 3 points in a line.
    procedure Split312(FirstIndex: Integer);
    // @name splits a @classname that has three sides one of which has 5 points
    // in a line.
    procedure Split411(FirstIndex: Integer
      {$IFDEF TEST}; List: TBoundaryList{$ENDIF});
    // @name splits a @classname that has four sides two of which have 3 points
    // in a line and which don't intersect.
    procedure Split2121(FirstIndex: Integer);
    // @name removes itself from all @link(TNodeInBoundary)s that contain it.
    procedure RemoveSelfFromAllNodes;
    // @name reverses itself or @link(FSubParts).
    procedure ReverseSubBoundaries;
    // @name indicates whether a @link(TBoundary) is in clockwise or
    // counter clockwise orientation.
    function Orientation: Integer;
    // @name returns the position of the @link(TNodeInBoundary) that contains
    // ANode.
    function IndexOfNode(ANode: TNode): Integer;
    procedure AssignConstraintNodes;
    // @name is used to remove straight line segments projecting from a boundary.
    // It is used in @link(TQuadMeshCreator.GenerateMeshWithGmsh).
    procedure RemoveProjections;
    // @name is used to convert straight line segments projecting from a
    // boundary into new boundaries
    // It is used  in @link(TQuadMeshCreator.GenerateMeshWithGeompackPP).
    procedure ConvertProjectionsToBoundaries;
{$IFDEF TEST}
  public
    function Center: TPoint2D;
{$ENDIF}
  {$IFDEF DEBUG}
    function NodeLocations: string;
  {$ENDIF}
    // @name removes itself from all of the @link(TNode)s that have it.
    procedure RemoveSelfFromOwnNodes;
    // See @link(SubPartCount).
    function GetSubPartCount: Integer;
    // @name sets the orientation of this @classname to be counterclockwise.
    procedure SetCounterClockwiseOrientation;
    // @name computes the angle for each node on the boundary and uses that
    // to assign @link(TNode.FDesiredElementCount).
    procedure AssignOriginalEdgeAngles;
    // @name creates @link(TNodeInBoundary)s based on the contents of
    // @link(FNodes).
    procedure GenerateSegments(DesiredOrientation: Integer);
    // @name splits a @classname into two or more parts and adds the new parts
    // to list if they might be split further.
    Procedure Split(List: TBoundaryList);
    // @name creates an instance of @classname.
    constructor Create(QuadMeshCreator: TQuadMeshCreator;
      Parent: TBoundary; DesiredSpacing: double);
    // @name returns Count -1. See @link(NodeCount).
    function GetActiveNodeCount: Integer;
    // @name returns @link(Clockwise), @link(CounterClockwise),
    // or @link(CollinearOrientation) depending on the
    // content of @link(FNodes).
    function NodeOrientation: Integer;
    function GetDisplayNumber: Integer;
    // The spacing between neighboring nodes in the final mesh
    // should be no greater than @name.
    property DesiredSpacing: double read FDesiredSpacing;
    // If a @classname intersects itself multiple times, @name splits it into
    // separate @classname s at the intersection points.
    procedure SplitMultiplyConnectedBoundary;
//    procedure SplitMultiplyConnectedBoundary2;
{$IFDEF TEST}
    // @name is only used in testing
    procedure CheckInvalidElement;
  public
    // @name is only used in testing
    ListCount: Integer;
{$ENDIF}
    // @name is the number of parts into which @name has been split.
    property SubPartCount: Integer read GetSubPartCount;
    // @name gives access to the parts into which a @classname has been split.
    property SubParts[Index: Integer]: TBoundary read GetSubPart;
    // @name sets @link(TSegment.FPosition1) and @link(TSegment.FPosition2).
    procedure SetSegmentPositions;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetActiveNode(Index: Integer): INode;
    function GetElementNumber: Integer;
    procedure SetElementNumber(Value: Integer);
{$IFDEF TEST}
  public
{$ENDIF}
    // @name is the area of a @classname.
    function Area: double;
    function GetBypassUpdate: Boolean;
    procedure SetBypassUpdate(const Value: Boolean);
    property BypassUpdate: Boolean read GetBypassUpdate write SetBypassUpdate;
  public
    // @name destroys an instance of @classname. Don't call @name.
    // Call Free instead.
    destructor Destroy; override;
    // @name adds a @link(TNode) to @link(FNodes).
    procedure AddNode(Node: TNode);
    // @name returns the @link(TNode) of the @link(TNodeInBoundary) at Index.
    property Nodes[Index: Integer]: INode read GetActiveNode;
    // @name is the number of the element.
    property ElementNumber: Integer read GetElementNumber
      write SetElementNumber;
    // @name is the number of active nodes in an IElement.
    property NodeCount: Integer read GetActiveNodeCount;
    // @name is used with Gmsh.
    function IsBoundaryInside(AnotherBoundary: TBoundary): boolean;
    // @name is used with Geompack++.
    function IsDetachedBoundaryInside(AnotherBoundary: TBoundary): boolean;
  {$IFDEF TEST}
    // name is only used in tests.
    property NodeList: TNodeList read FNodes;
    // name is only used in tests.
    property Segments: TSegmentObjectList read FSegments;
  {$ENDIF}
    property DisplayNumber: Integer read GetDisplayNumber;
  end;

  TBoundaryComparer = TComparer<TBoundary>;

  // @name is used in sorting @link(TBoundary)s based on the number of
  // @link(TNodeInBoundary) that they contain.
  TBoundaryCountComparer = class(TBoundaryComparer)
  public
    function Compare(const Left, Right: TBoundary): Integer; override;
  end;

  // Sort boundaries in order of decreasing area.
  TBoundaryAreaComparer = class(TBoundaryComparer)
  public
    function Compare(const Left, Right: TBoundary): Integer; override;
  end;

  TNodeConnection = class;
  TNodeConnectionsObjectList = TObjectList<TNodeConnection>;

  TGeompackShapeMeasure = (gsmMinAngle, gsmRadiusRatio, gsmMeanRatio,
    gsmQuadraticMeanRatio);

  TGeompackOptions = class(TGoPhastPersistent)
  private
    FStoredTolerance: TRealStorage;
    FStoredToleranceAngle: TRealStorage;
    FStoredSpacingAngle: TRealStorage;
    FStoredMeshDistributionVariation: TRealStorage;
    FStoredUniformnessParameter: TRealStorage;
    FElementGenerationParameter: integer;
    FMaxSmoothingIterations: integer;
    FShapeMeasure: TGeompackShapeMeasure;
    FMaxImprovementIterations: integer;
    FDesiredElementCount: integer;
    FKeepQuadsAlongEdges: Boolean;
    FStoredOptimizationBasedSmoothingCriterion: TRealStorage;
    FStoredQuadSplittingValue: TRealStorage;
    FAutomaticeElementCount: boolean;
    function GetTolerance: Double;
    procedure SetStoredTolerance(const Value: TRealStorage);
    procedure SetTolerance(const Value: Double);
    function GetMeshDistributionVariation: Double;
    function GetSpacingAngle: Double;
    function GetToleranceAngle: Double;
    function GetUniformnessParameter: Double;
    procedure SetMeshDistributionVariation(const Value: Double);
    procedure SetSpacingAngle(const Value: Double);
    procedure SetStoredMeshDistributionVariation(const Value: TRealStorage);
    procedure SetStoredSpacingAngle(const Value: TRealStorage);
    procedure SetStoredToleranceAngle(const Value: TRealStorage);
    procedure SetStoredUniformnessParameter(const Value: TRealStorage);
    procedure SetToleranceAngle(const Value: Double);
    procedure SetUniformnessParameter(const Value: Double);
    function GetOptimizationBasedSmoothingCriterion: double;
    function GetQuadSplittingValue: double;
    procedure SetDesiredElementCount(const Value: integer);
    procedure SetElementGenerationParameter(const Value: integer);
    procedure SetKeepQuadsAlongEdges(const Value: Boolean);
    procedure SetMaxImprovementIterations(const Value: integer);
    procedure SetMaxSmoothingIterations(const Value: integer);
    procedure SetOptimizationBasedSmoothingCriterion(const Value: double);
    procedure SetQuadSplittingValue(const Value: double);
    procedure SetShapeMeasure(const Value: TGeompackShapeMeasure);
    procedure SetStoredOptimizationBasedSmoothingCriterion(
      const Value: TRealStorage);
    procedure SetStoredQuadSplittingValue(const Value: TRealStorage);
    procedure SetAutomaticeElementCount(const Value: boolean);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Geompack++ toler. Must be > 0. Maximum = 0.001
    property Tolerance: Double read GetTolerance write SetTolerance;
    // Geompack++ angspc. Must be > 0.
    property SpacingAngle: Double read GetSpacingAngle write SetSpacingAngle;
    // Geompack++ angtol. Must be > 0.
    property ToleranceAngle: Double read GetToleranceAngle
      write SetToleranceAngle;
    // Geompack++ munif. Allowed values range from 0 to 1.
    property UniformnessParameter: Double read GetUniformnessParameter
      write SetUniformnessParameter;
    // Geompack++ dmin. Must be > 0.
    property MeshDistributionVariation: Double read GetMeshDistributionVariation
      write SetMeshDistributionVariation;
    // Absolute value of Geompack++ quadmu. Must be > 0.
    property QuadSplittingValue: double read GetQuadSplittingValue
      write SetQuadSplittingValue;
    // Absolute value of Geompack++ optmu
    property OptimizationBasedSmoothingCriterion: double
      read GetOptimizationBasedSmoothingCriterion
      write SetOptimizationBasedSmoothingCriterion;
    procedure Initialize;
  published
    property StoredTolerance: TRealStorage read FStoredTolerance
      write SetStoredTolerance;
    property StoredSpacingAngle: TRealStorage read FStoredSpacingAngle
      write SetStoredSpacingAngle;
    property StoredToleranceAngle: TRealStorage read FStoredToleranceAngle
      write SetStoredToleranceAngle;
    property StoredUniformnessParameter: TRealStorage
      read FStoredUniformnessParameter write SetStoredUniformnessParameter;
    property StoredMeshDistributionVariation: TRealStorage
      read FStoredMeshDistributionVariation
      write SetStoredMeshDistributionVariation;
    // Geompack++ nmin. Must be > 0.
    property ElementGenerationParameter: integer
      read FElementGenerationParameter write SetElementGenerationParameter;
    // Geompack++ nelemd. If @name is zero, it should be changed to an
    // appropriate value before it is used.
    property DesiredElementCount: integer read FDesiredElementCount
      write SetDesiredElementCount;
    // @name records whether the user is manually specifying the element
    // count or not.
    property AutomaticeElementCount: boolean read FAutomaticeElementCount
      write SetAutomaticeElementCount;
    // Geompack++ mtype
    property ShapeMeasure: TGeompackShapeMeasure read FShapeMeasure
      write SetShapeMeasure;
    property StoredQuadSplittingValue: TRealStorage
      read FStoredQuadSplittingValue write SetStoredQuadSplittingValue;
    // Sign of Geompack++ quadmu
    property KeepQuadsAlongEdges: Boolean read FKeepQuadsAlongEdges
      write SetKeepQuadsAlongEdges;
    // Geompack++ nimpiter
    property MaxImprovementIterations: integer read FMaxImprovementIterations
      write SetMaxImprovementIterations;
    // Geompack++ nsmpas
    property MaxSmoothingIterations: integer read FMaxSmoothingIterations
      write SetMaxSmoothingIterations;
    property StoredOptimizationBasedSmoothingCriterion: TRealStorage
      read FStoredOptimizationBasedSmoothingCriterion
      write SetStoredOptimizationBasedSmoothingCriterion;
  end;

  TQuadMeshCreator = class(TInterfacedObject, IMesh)
  private
    // @name is the minimum X coordinate of all of the points
    // on all of the @link(TBoundary boundaries).
    FMinX: double;
    // @name is the minimum Y coordinate of all of the points
    // on all of the @link(TBoundary boundaries).
    FMinY: double;
    // @name is the maximum X coordinate of all of the points
    // on all of the @link(TBoundary boundaries).
    FMaxX: double;
    // @name is the maximum Y coordinate of all of the points
    // on all of the @link(TBoundary boundaries).
    FMaxY: double;
    {$IFDEF TEST}
  public
    {$ENDIF}
    // @name is the length of the diagonal of a rectangle that encloses
    // all of the points on all of the @link(TBoundary boundaries).
    FCharacteristicLength: double;
    FDuplicateBoundaries: TBoundaryObjectList;
    {$IFDEF TEST}
  private
    {$ENDIF}
    // @name contains the original @link(TBoundary boundaries) that define
    // the meshing region or the @link(TBoundary boundaries) created by
    // intersecting them.
    FBoundaries: TBoundaryObjectList;
    // See @link(SixNodeClosureMethod).
    FSixNodeClosureMethod: TSixNodeClosureMethod;
    // See @link(NodeAdjustmentMethod).
    FNodeAdjustmentMethod: TNodeAdjustmentMethod;
    // See @link(GrowthRate)
    FGrowthRate: double;
    // @name holds all the @link(TNode)s in the mesh.
    FNodes: TNodeObjectList;
    // @name holds the @link(IElement)s in the mesh. It is filled in
    // @link(RenumberNodesAndElements)
    FElementList: TIElementList;
    // @name holds the @link(INode)s in the mesh. It is filled in
    // @link(RenumberNodesAndElements)
    FNodeList: TINodeList;
    // @name owns all the @link(TNodeInBoundary)s.
    FBoundaryNodes: TNodeInBoundaryObjectList;
    // @name is used to search for @link(TNode)s.
    FNodeQuadTree: TRbwQuadTree;
    // @name stores the original polygons that were specified at the beginning
    // of the mesh generation process.
    FPolygonArray: TPolygon2DArray;
    // @name stores a list of the @link(TNode)s that form the outermost
    // boundary of the mesh that will be generated. They may not be in order.
    FBNodes: TNodeList;
    // @name is used to create lists of @link(TBoundary)s that are inside
    // another @link(TBoundary).
    FIncludedBoundaries: TObjectList<TBoundaryList>;
    FRenumberingAlgorithm: TRenumberingAlgorithm;
    FGmshTerminated: Boolean;
    procedure GetConnectedOpenBoundaries(OpenBoundaryList: TBoundaryList; BlindEndsBoundaryList: TBoundaryList);
    // @name fills ClosedBoundaries with all the closed @link(TBoundary)s.
    // LinkedClosedBoundaries and LinkedOpenBoundaries are filled with lists
    // of open of closed @link(TBoundary)s that correspond to the nodes in
    // IntersectionNodes.
    procedure InitializeIntersectLists(IntersectionNodes: TNodeList;
      LinkedClosedBoundaries, LinkedOpenBoundaries: TBoundaryListSqr;
      ClosedBoundaries: TBoundaryList);
    // @name stores a copy of the @link(TNode)s on the very first
    // @link(TBoundary) in @link(FBNodes).
    procedure StoreBoundaryNodes;
    procedure GetBestLinkWhenSameNode(var ClosestCost: TCost; FirstBoundary,
      ABoundary: TBoundary);
    procedure GetConnectionsBetweenBoundaries(OpenBoundaryList,
      BlindEndsBoundaryList: TBoundaryList; NodeList: TNodeList;
      ConnectionsList: TNodeConnectionsObjectList);

    {$IFDEF TEST}
  public
    {$ENDIF}
    // @name stores polygons representing the original closed boundaries
    // of the meshing area.
    procedure StoreClosedBoundaryPolygons;
    // @name checks the interior nodes and if any of them have concave
    // angles in an element, it fixes them.
    function FixEdgeTriangles: boolean;
    // The first @link(TBoundary) is the edge of the meshing area. If any
    // other closed boundaries intersect it, they are broken into
    // an open @link(TBoundary).
    procedure BreakClosedBoundariesThatIntersectOuterBoundary;
    // @name calls @link(TBoundary.FixSegments) for all the @link(TBoundary)s.
    procedure FixSegments;
    { @name computes @link(FCharacteristicLength), @link(FMinX),
    @link(FMinY), @link(FMaxX), and @link(FMaxY).}
    procedure ComputeCharacteristicLength;
    {$IFDEF TEST}
    // @name is only used in testing.
    function GetBoundaryCount: Integer;
    {$ENDIF}
    // See @link(SixNodeClosureMethod).
    procedure SetSixNodeClosureMethod(const Value: TSixNodeClosureMethod);
    // See @link(NodeAdjustmentMethod).
    procedure SetNodeAdjustmentMethod(const Value: TNodeAdjustmentMethod);
    // @name moves the node to the centroid of a polygon made up of the
    // @classname s neighboring this @classname.
    // See @link(TNode.AdjustPositionLagrange).
    procedure AdjustPositionLagrange;
    // @name calls @link(TNode.AdjustPositionGiuliani) for all the
    // @link(TNode)s.
    procedure AdjustPositionGiuliani;
    // @name returns the number of @link(TNode)s.
    function GetActiveNodeCount: Integer;
    // See @link(NodeObjects).
    function GetNodeObject(Index: Integer): TNode;
    // See link(ElementCount).
    function GetActiveElementCount: Integer;
    // See link(Elements).
    function GetActiveElement(Index: Integer): IElement;
    // See link(Nodes).
    function GetActiveNode(Index: Integer): INode;
    procedure SetRenumberingAlgorithm(const Value: TRenumberingAlgorithm);
    procedure AssignConstraintNodes;
//    procedure GmshTerminate(Sender: TObject; ExitCode: DWORD);
    {$IFDEF TEST}
    // @name is only used in testing.
    property BoundaryCount: Integer read GetBoundaryCount;
    {$ENDIF}
    // @name access @link(FNodes).
    property NodeObjects[Index: Integer]: TNode read GetNodeObject;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // @name is used to create lists of @link(TBoundary)s that are inside
    // another @link(TBoundary).
    property IncludedBoundaries: TObjectList<TBoundaryList>
      read FIncludedBoundaries;
    // @name calls @link(ComputeCharacteristicLength) and
    // @link(StoreBoundaryNodes). This results in the computation of
    // a bounding rectangle encompassing all the points on all the
    // @link(TBoundary boundaries) and of @link(FCharacteristicLength).
    // It also stores the @link(TNode)s in the first @link(TBoundary) in
    // @link(FBNodes). Initially, the first boundary defines the outer edge
    // of the mesh that will be created. However, during the process of
    // creating the mesh, both the boundaries themselves and the order of the
    // boundaries will be modified. See @link(FBoundaries).
    procedure Initialize;
    {@name finds all points of intersection between the boundaries used to
    control the mesh generation. It then adds the intersection points to the
    boundaries. }
    procedure IntersectBoundaries;
    // @name looks for intersections between closed boundaries (except the
    // outermost closed boundary) and if it finds intersections, merges the
    // intersecting closed boundaries into a single boundary. If in the
    // process of merging the boundaries, some @link(TNode)s are inside the
    // resulting closed boundary, they are removed.
    procedure MergeClosedBoundaries;
    // @name breaks open @link(TBoundary boundaries) into separate
    // @link(TBoundary boundaries) at points of intersection.
    procedure BreakOpenBoundaries;
    // @name deletes boundaries outside the first boundary
    // or inside internal closed boundaries.
    procedure DeleteExternalBoundaries;
    // @name deletes @link(TBoundary) that are outside the meshing area.
    procedure DeleteDisconnectedBoundaries;
    // @name traces open @link(TBoundary boundaries) linked to the
    // closed @link(TBoundary boundaries) and
    // extracts new closed @link(TBoundary boundaries) where possible.
    procedure ExtractClosedBoundaries;
    // Where open @link(TBoundary boundaries) intersect closed
    // @link(TBoundary boundaries), they are merged with the closed
    // @link(TBoundary boundaries). At each intersection point, the
    // boundaries are sorted in the order of the direction of the
    // boundary away from the intersection point. The closed boundaries
    // will be in the list twice and the open boundaries will only be in
    // the list once. The order of the @link(TBoundary boundaries).
    // determines the order in which the boundaries will be merged.
    procedure MergeOpenWithClosedBoundaries;
    // @name merges open @link(TBoundary boundaries) that intersect with other
    // open @link(TBoundary boundaries)
    // into a single closed @link(TBoundary). At each intersection point, the
    // boundaries are sorted in the order of the direction of the
    // boundary away from the intersection point.
    // The order of the @link(TBoundary boundaries).
    // determines the order in which the boundaries will be merged.
//    procedure MergeOpenNodeBoundaries;
//    procedure MergeOpenNodeBoundaries2;
    procedure MergeOpenNodeBoundaries3;
    // @name fills @link(FIncludedBoundaries). Each @link(TBoundaryList) in
    // @link(FIncludedBoundaries) has a closed @link(TBoundary) as its
    // first member. All the other @link(TBoundary boundaries) are ones that
    // are inside the first one.
    procedure ArrangeBoundaries;
    // @name sets the @link(TNode.FNodeType) of the @link(TNode)s in the
    // outermost boundary to ntEdge and for the others to ntSubDomain.
    procedure SetNodeTypes;
    // @name creates @link(TSegment)s in a counterclockwise orientation
    // around the outer @link(TBoundary) and in a clockwise orientation
    // around the other boundaries.
    procedure GenerateSegments;
    {@name assigns the @link(TNode.DesiredSpacing) to each @link(TNode) based
    on the the distance of the @link(TNode) to other @link(TNode)s with
    smaller @link(TNode.DesiredSpacing)s.}
    procedure AssignDesiredSpacings;
    // @name adds every @link(TNode) to @link(FNodeQuadTree).
    procedure FillNodeTree;
    // @name inserts nodes along boundaries to ensure that the spacing between
    // nodes is never larger than the desired spacing and that the number of
    // nodes inserted is even.
    procedure InsertNodesAlongBoundaries;
    // @name converts an open boundary to a closed one by adding the existing
    // @link(TSegment)s in reverse order to the boundary.
    procedure ConvertToClosedBoundaries;
    // @name creates @link(TNodeInBoundary TNodeInBoundaries)
    // along each @link(TBoundary).
    procedure CreateBoundaryNodes;
    {@name calls @link(TBoundary.AssignOriginalEdgeAngles) for each
    @link(TBoundary) to assign @link(TNode.FTotalAngle) for each @link(TNode)
    // and uses the angles to assign
    @link(TNode.FDesiredElementCount) for each @link(TNode).}
    procedure AssignOriginalEdgeAngles;
    // If there is more than one @link(TBoundary) in each @link(TBoundaryList)
    // in @link(FIncludedBoundaries), @name joins them together to convert
    // them to a single boundary.
    procedure MakeSingleBoundary;
    // @name calls @link(TBoundary.SplitMultiplyConnectedBoundary) for each
    // @link(TBoundary).
    procedure SplitMultiplyConnectedBoundaries;
    // @name improves the topology of the mesh by
    // @link(TNode.NodeElimination),
    // @link(TNode.ElementElimination),
    // @link(TNode.SideElimination), and
    // @link(TNode.DiagonalSwapping)
    function ImproveTopology: boolean;
    // @name adds the @link(TBoundary boundaries) to @link(FElementList)
    // and the @link(TNode)s to @link(FNodeList). Then it calls
    // @link(RenumberMesh) and sorts @link(FElementList) and @link(FNodeList)
    // according to their node or element number.
    procedure RenumberNodesAndElements;
    // @name fixes triangular boundaries on the edge of the mesh.
    procedure FixFinalTriangularElements;
    // @name is used in @link(GenerateMeshWithGeomPackPP) to convert
    // projections off of polygons to separate boundaries.
    procedure ConvertProjectionsToBoundaries;
  public
    // @name creates an instance of @classname.
    Constructor Create;
    // @name destroys an instance of @classname. Don't call @name.
    // Call Free instead.
    destructor Destroy; override;
    // After the mesh generation area has been defined by calls to
    // @link(AddBoundary), @link(TBoundary.AddNode) and
    // @link(SixNodeClosureMethod), @link(GrowthRate),
    // and @link(NodeAdjustmentMethod) have been set, call @name to
    // generate a finite element mesh composed or quadrilaterals.
    // Use @link(NodeCount), @link(Nodes), @link(ElementCount) and
    // @link(Elements) to read the generated mesh. You may call
    // @link(AdjustNodes) to adjust the positions of the nodes according
    // to the method specified in @link(NodeAdjustmentMethod).
    procedure GenerateMesh;
    procedure GenerateMeshWithGmsh(const GMshLocation: string; Exaggeration: double);
    procedure GenerateMeshWithGeomPackPP(const GeompackLocation: string;
      Exaggeration: double; Options: TGeompackOptions; var ErrorMessage: string);
    // @name adjusts the positions of the nodes according
    // to the method specified in @link(NodeAdjustmentMethod).
    procedure AdjustNodes;
    // @name is used to add a new boundary to the meshing area. The first
    // boundary added will define the extent of the final mesh.
    function AddBoundary(DesiredSpacing: double): TBoundary;
    // @name is the method used to fill in the boundaries with six sides.
    property SixNodeClosureMethod: TSixNodeClosureMethod
      read FSixNodeClosureMethod write SetSixNodeClosureMethod;
    // When the desired spacing between nodes is different for different
    // boundaries, @name controls how quickly the elements change in size
    // between small and large elements.
    // @name should be greater than 1.
    property GrowthRate: double read FGrowthRate write FGrowthRate;
    // @name determines what method is used to adjust the position of
    // @link(TNode)s.
    property NodeAdjustmentMethod: TNodeAdjustmentMethod
      read FNodeAdjustmentMethod write SetNodeAdjustmentMethod;
    // @name is the number of nodes in the mesh.
    property NodeCount: Integer read GetActiveNodeCount;
    // Use @name to access the @link(INode)s of the generated mesh.
    property Nodes[Index: Integer]: INode read GetActiveNode;
    // @name is the number of elements in the mesh.
    property ElementCount: Integer read GetActiveElementCount;
    // Use @name to access the @link(IElement)s of the generated mesh.
    property Elements[Index: Integer]: IElement read GetActiveElement;
    property RenumberingAlgorithm: TRenumberingAlgorithm
      read FRenumberingAlgorithm write SetRenumberingAlgorithm;
    {$IFDEF TEST}
    // @name is used only for testing.
    property Boundaries: TBoundaryObjectList read FBoundaries;
    // @name is used only for testing.
    property NodeList: TNodeObjectList read FNodes;
    {$ENDIF}
  end;

  TBoundaryLink = class;
  TBoundaryLinkList = TList<TBoundaryLink>;
  TBoundaryLinkObjectList = TObjectList<TBoundaryLink>;

  // @name is used in @link(TQuadMeshCreator.ExtractClosedBoundaries)
  // to trace open @link(TBoundary)s that are connected to closed
  // @link(TBoundary)s
  TBoundaryLink = class(TObject)
  private
    Parent: TBoundaryLink;
    Children: TBoundaryLinkObjectList;
    Boundary: TBoundary;
    PositionInBoundary: integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDirection = (dForward, dBackward);

  // @name is used in
  // @link(TQuadMeshCreator.MergeOpenWithClosedBoundaries) together
  // with @link(TAngleComparer) to ensure that open @Link(TBoundary)s are
  // merged in the correct order around an intersection point.
  TAngleCompareItem = class(TObject)
    Direction: TDirection;
    Boundary: TBoundary;
    NodePosition: Integer;
    Angle: double;
  end;

    // sort in order of descending angles (= clockwise).

  TAngleComparer = class(TComparer<TAngleCompareItem>)
    function Compare(const Left, Right: TAngleCompareItem): Integer; override;
  end;

  TAngleList = TObjectList<TAngleCompareItem>;

  // @name is used in
  // @link(TQuadMeshCreator.MergeOpenWithClosedBoundaries) together
  // with @link(TAngleCompareItem) to ensure that open @Link(TBoundary)s are
  // merged in the correct order around an intersection point.
  TNodeConnection = class(TObject)
  private
    FNode: TNode;
    FConnections: TAngleList;
    procedure AddLink(ABoundary: TBoundary);
    procedure RemoveLink(ABoundary: TBoundary);
    procedure Sort;
  public
    Constructor Create(ANode: TNode);
    destructor Destroy; override;
  end;


// name returns a vertex angle in radians instead of degrees.
function VertexAngleRadians(x1, y1, x2, y2, x3, y3: TFloat): TFloat; overload;
// name returns a vertex angle in radians instead of degrees.
function VertexAngleRadians(const Point1, Point2, Point3: TPoint2D)
  : TFloat; overload;

var
  C1: double;
  C2: double;
  C3: double;
  C4: double;
  C5: double;
  C6: double;
  AltC1: double;
  AltC2: double;
  AltC3: double;
  AltC4: double;
  AltC5: double;
  AltC6: double;
  ElementGrowthRate: double;

// Set default values for C1..C6, AltC1..AltC6, and ElementGrowthRate.
procedure SetDefaults;

implementation

uses
  Math, ConvexHullUnit, Dialogs, RealListUnit, GPC_Classes, gpc,
  CuthillMcKeeRenumbering, TempFiles, ImportQuadMesh, IOUtils,
  frmGoPhastUnit, SutraMeshUnit, ModelMuseUtilities;

type
  TColliniears = array [0 .. 5] of boolean;

const
  PiDiv2 = Pi / 2;
  TwoPiDiv3 = 2 * Pi / 3;
  PiDiv6 = Pi / 6;
  TwoPi = 2 * Pi;

var
  InvalidElement: boolean = False;


procedure SetDefaults;
begin
  C1 := 0.52;
  C2 := 0.17;
  C3 := 0;
  C4 := 0.17;
  C5 := 0.14;
  C6 := 0.36;
  AltC1 := 0.56;
  AltC2 := 0.33;
  AltC3 := 0.11;
  AltC4 := 0.0;
  AltC5 := 0.0;
  AltC6 := 1.2;
  ElementGrowthRate := 1.05;
end;

function NearlyStraightAngle(P1, P2, P3: TPoint2D): boolean;
const
  Epsilon: double = 0.1;
var
  AnAngle: TFloat;
begin
  AnAngle := VertexAngle(P1, P2, P3);
  result := (AnAngle < Epsilon) and (AnAngle > -Epsilon);
  if not result then
  begin
    result := (AnAngle < 180 + Epsilon) and (AnAngle > 180 - Epsilon);
    if not result then
    begin
      result := (AnAngle > -180 - Epsilon) and (AnAngle < -180 + Epsilon);
    end;
  end;
end;

function CalcNodesToInsert(NStar: double): Integer;
begin
  result := Trunc(NStar * (1 + Epsilon)) - 1;
  if not IsEqual(result, NStar - 1, Epsilon) then
  begin
    Inc(result);
  end;
end;

function VertexAngleRadians(x1, y1, x2, y2, x3, y3: TFloat): TFloat;
var
  Dist: TFloat;
  InputTerm: TFloat;
begin
  (*
    Using the cosine identity:
    cosA = (b^2 + c^2 - a^2) / (2*b*c)
    A    = Cos'((b^2 + c^2 - a^2) / (2*b*c))

    Where:

    a,b and c : are edges in the triangle
    A         : is the angle at the vertex opposite edge 'a'
    aka the edge defined by the vertex <x1y1-x2y2-x3y3>

  *)
  (* Quantify coordinates *)
  x1 := x1 - x2;
  x3 := x3 - x2;
  y1 := y1 - y2;
  y3 := y3 - y2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1) * (x3 * x3 + y3 * y3);

  if IsEqual(Dist, Zero) then
    result := Zero
  else
  begin
    InputTerm := (x1 * x3 + y1 * y3) / sqrt(Dist);
    if IsEqual(InputTerm, 1.0) then
      result := Zero
    else if IsEqual(InputTerm, -1.0) then
      result := Pi
    else
      result := ArcCos(InputTerm)
  end;
end;

function VertexAngleRadians(const Point1, Point2, Point3: TPoint2D): TFloat;
begin
  result := VertexAngleRadians(Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X,
    Point3.Y);
  if Orientation(Point1, Point2, Point3) = Clockwise then
  begin
    result := 2 * Pi - result;
  end;
end;

var
  XEpsilon: double;
  YEpsilon: double;

function NearlyTheSame(const A, B: double; Epsilon: double): boolean; overload;
var
  Delta: double;
  AbsA: double;
  AbsB: double;
begin
  result := (A = B);

  if not result then
  begin
    Delta := A-B;
    Delta := Abs(Delta);
    result := Delta < Epsilon;

    if not result then
    begin
      AbsA := Abs(A);
      AbsB := Abs(B);
      result := Delta/(AbsA+AbsB) < Epsilon;
    end;
  end;
end;

function NearlyTheSame(const A, B: TPoint2D): boolean; overload;
begin
  result := NearlyTheSame(A.x, B.x, XEpsilon)
    and NearlyTheSame(A.y, B.y, YEpsilon);
end;

function PointInConcavePolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;
//var
//  PriorIndex: Integer;
//  VertexIndex: Integer;
//  APoint: TPoint2D;
//  AnotherPoint: TPoint2D;
begin
  Result := ModelMuseUtilities.PointInConcavePolygon(Point, Polygon);
//  PriorIndex := Length(Polygon) - 1;
//  for VertexIndex := 0 to Length(Polygon) - 1 do
//  begin
//    APoint := Polygon[VertexIndex];
//    AnotherPoint := Polygon[PriorIndex];
//    if ((Point.Y <= APoint.Y) = (Point.Y > AnotherPoint.Y)) and
//      (Point.X - APoint.X - (Point.Y - APoint.Y) *
//      (AnotherPoint.X - APoint.X) /
//      (AnotherPoint.Y - APoint.Y) < 0) then
//    begin
//      result := not result;
//    end;
//    PriorIndex := VertexIndex;
//  end;
end;

procedure TCost.ComputeVisibility;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  PriorNode1: TNodeInBoundary;
  SubsequentNode1: TNodeInBoundary;
  PriorNode2: TNodeInBoundary;
  SubsequentNode2: TNodeInBoundary;
  NodeIndex: Integer;
  Node1: TNodeInBoundary;
  Node2: TNodeInBoundary;
  Angle1: TFloat;
  Angle2: TFloat;
  Segment: TSegment2D;
  APoint: TPoint2D;
  Poly: TPolygon2D;
  Index: Integer;
  ANode: TNodeInBoundary;
  Epsilon: double;
  SegmentIndex: Integer;
  ASegment: TSegment;
  Boundary: TBoundary;
  HigherNode: TNodeInBoundary;
  LowerNode: TNodeInBoundary;
  LowerSegmentIndex: Integer;
  HigherSegmentIndex: Integer;
  Polygon1: TPolygon2D;
  NNode: TNodeInBoundary;
  Polygon2: TPolygon2D;
  NodeList: TNodeList;
  ANode1: TNode;
  ANode2: TNode;
  ANode3: TNode;
  procedure Compute(ABoundary: TBoundary);
  var
    NodeIndex: Integer;
    NodeA: TNode;
    NodeB: TNode;
    SubIndex: Integer;
    SubBoundary: TBoundary;
  begin
    for NodeIndex := 0 to ABoundary.Count - 2 do
    begin
      NodeA := ABoundary[NodeIndex].FNode;
      NodeB := ABoundary[NodeIndex + 1].FNode;
      if (NodeA <> FNode1.FNode) and (NodeA <> FNode2.FNode) and
        (NodeB <> FNode1.FNode) and (NodeB <> FNode2.FNode) then
      begin
        if Intersect(FNode1.FNode.Location, FNode2.FNode.Location,
          NodeA.Location, NodeB.Location) then
        begin
          // Don't allow the split if the new segment will cross one of the
          // other edges of and existing polygon.
          FVisible := False;
          Exit;
        end;
        // Don't allow the split if the new segment will
        // nearly intersect one of the
        // other nodes of the existing polygon.
        APoint := ClosestPointOnSegmentFromPoint(Segment, NodeA.Location);
        if IsEqual(NodeA.Location, APoint) then
        begin
          FVisible := False;
          Exit;
        end;
        APoint := ClosestPointOnSegmentFromPoint(Segment, NodeB.Location);
        if IsEqual(NodeB.Location, APoint) then
        begin
          FVisible := False;
          Exit;
        end;
      end;
    end;
    for SubIndex := 0 to ABoundary.FSubParts.Count - 1 do
    begin
      SubBoundary := ABoundary.FSubParts[SubIndex];
      Compute(SubBoundary);
      if not FVisible then
      begin
        Exit;
      end;
    end;
  end;

begin
  GetNeighborNodes(PriorNode1, SubsequentNode1, PriorNode2, SubsequentNode2);

  // First do a quick check to make sure that the proposed split will be
  // inside the TBoundary at the FNode1 by comparing the angles that would
  // be formed at FNode1.
  if PriorNode1.FNode <> SubsequentNode1.FNode then
  begin
    Angle1 := VertexAngleRadians(PriorNode1.FNode.Location,
      FNode1.FNode.Location, FNode2.FNode.Location);
    Angle2 := VertexAngleRadians(PriorNode1.FNode.Location,
      FNode1.FNode.Location, SubsequentNode1.FNode.Location);
    if Angle1 > Angle2 then
    begin
      FVisible := False;
      Exit;
    end;
  end;

  // Do the same check at FNode2.
  if PriorNode2.FNode <> SubsequentNode2.FNode then
  begin
    Angle1 := VertexAngleRadians(PriorNode2.FNode.Location,
      FNode2.FNode.Location, FNode1.FNode.Location);
    Angle2 := VertexAngleRadians(PriorNode2.FNode.Location,
      FNode2.FNode.Location, SubsequentNode2.FNode.Location);
    if Angle1 > Angle2 then
    begin
      FVisible := False;
      Exit;
    end;
  end;

  Segment[1] := FNode1.FNode.Location;
  Segment[2] := FNode2.FNode.Location;
  FVisible := True;
  if (FNode1.FBoundary = FNode2.FBoundary) then
  begin
    // We are splitting a TBoundary into two.
    ABoundary := FNode1.FBoundary;

    Epsilon := FQuadMeshCreator.FCharacteristicLength/1e7;
    if FNode1.FNode = FNode2.FNode then
    begin
      // If both FNode1 and FNode2 are at the same position
      // don't split if one of the resulting polygons will have
      // an area of zero (within rounding error). If both polygons
      // will have areas greater than zero, allow the polygon to be split.
      if FNode2.Position > FNode1.Position then
      begin
        Node1 := FNode1;
        Node2 := FNode2;
      end
      else
      begin
        Node2 := FNode1;
        Node1 := FNode2;
      end;
      SetLength(Poly, Node2.Position-Node1.Position);
      Index := 0;
      for NodeIndex := Node1.Position to Node2.Position - 1 do
      begin
        ANode := ABoundary[NodeIndex];
        Poly[Index] := ANode.FNode.Location;
        Inc(Index);
      end;
      Assert(index = Length(Poly));
      if Abs(Area(Poly)) < Epsilon then
      begin
        FVisible := False;
        Exit;
      end;
      SetLength(Poly, ABoundary.Count - Node2.Position + Node1.Position);
      Index := 0;
      for NodeIndex := Node2.Position to ABoundary.Count - 1 do
      begin
        ANode := ABoundary[NodeIndex];
        Poly[Index] := ANode.FNode.Location;
        Inc(Index);
      end;
      for NodeIndex := 0 to Node1.Position - 1 do
      begin
        ANode := ABoundary[NodeIndex];
        Poly[Index] := ANode.FNode.Location;
        Inc(Index);
      end;
      Assert(index = Length(Poly));
      if Abs(Area(Poly)) < Epsilon then
      begin
        FVisible := False;
        Exit;
      end;
      Exit;
    end;


    for SegmentIndex := 0 to ABoundary.FSegments.Count - 1 do
    begin
      ASegment := ABoundary.FSegments[SegmentIndex];
      if ASegment.ContainsNode(FNode1.FNode) then
      begin
        if ASegment.ContainsNode(FNode2.FNode) then
        begin
          FVisible := False;
          Exit;
        end
        else
        begin
          Continue;
        end;
      end
      else if ASegment.ContainsNode(FNode2.FNode) then
      begin
        Continue;
      end;

      if Intersect(Segment, EquateSegment(ASegment.FNode1.Location,
        ASegment.FNode2.Location)) then
      begin
        // Don't allow the split if the new segment will cross one of the
        // other edges of the existing polygon.
        FVisible := False;
        Exit;
      end;
      // Don't allow the split if the new segment will
      // nearly intersect one of the
      // other nodes of the existing polygon.
      APoint := ClosestPointOnSegmentFromPoint(Segment,
        ASegment.FNode1.Location);
      if IsEqual(ASegment.FNode1.Location, APoint) then
      begin
        FVisible := False;
        Exit;
      end;
      if Distance(APoint, ASegment.FNode1.Location) < Epsilon then
      begin
        FVisible := False;
        Exit;
      end;
    end;


    Boundary := FNode1.FBoundary;
    if FNode1.Position > FNode2.Position then
    begin
      HigherNode := FNode1;
      LowerNode := FNode2;
    end
    else
    begin
      HigherNode := FNode2;
      LowerNode := FNode1;
    end;

    LowerSegmentIndex := -1;
    HigherSegmentIndex := -1;
    for SegmentIndex := 0 to Boundary.FSegments.Count - 1 do
    begin
      ASegment := Boundary.FSegments[SegmentIndex];
      if (LowerSegmentIndex < 0) and (ASegment.FPosition2 >= LowerNode.Position) then
      begin
        LowerSegmentIndex := SegmentIndex;
      end;
      if ASegment.FPosition2 >= HigherNode.Position then
      begin
        HigherSegmentIndex := SegmentIndex;
        Break;
      end;
    end;
    Assert(LowerSegmentIndex >= 0);
  //  Assert(HigherSegmentIndex >= 0);
    Assert(HigherSegmentIndex > LowerSegmentIndex);


//    NNode := nil;
    NodeIndex := 0;
    SetLength(Polygon1, HigherNode.Position - LowerNode.Position + 1);
    for SegmentIndex := LowerSegmentIndex to HigherSegmentIndex do
    begin
      ASegment := Boundary.FSegments[SegmentIndex];
      if SegmentIndex = LowerSegmentIndex then
      begin
        NNode := Boundary[LowerNode.Position];
        if LowerNode.Position <> ASegment.FPosition2 then
        begin
          Polygon1[NodeIndex] := NNode.FNode.Location;
          Inc(NodeIndex);
          NNode := Boundary[ASegment.FPosition2];
        end;
      end
      else if SegmentIndex = HigherSegmentIndex then
      begin
        NNode := Boundary[HigherNode.Position];
      end
      else
      begin
        NNode := Boundary[ASegment.FPosition2];
      end;
      Polygon1[NodeIndex] := NNode.FNode.Location;
      Inc(NodeIndex);
    end;
    SetLength(Polygon1, NodeIndex);

    SetLength(Polygon2, Boundary.Count - HigherNode.Position +
      LowerNode.Position);
    NodeIndex := 0;
    for SegmentIndex := HigherSegmentIndex to Boundary.FSegments.Count-1 do
    begin
      ASegment := Boundary.FSegments[SegmentIndex];
      if SegmentIndex = HigherSegmentIndex then
      begin
        NNode := Boundary[HigherNode.Position];
        if HigherNode.Position <> ASegment.FPosition2 then
        begin
          Polygon2[NodeIndex] := NNode.FNode.Location;
          Inc(NodeIndex);
          NNode := Boundary[ASegment.FPosition2];
        end;
      end
      else
      begin
        NNode := Boundary[ASegment.FPosition2];
      end;
      Polygon2[NodeIndex] := NNode.FNode.Location;
      Inc(NodeIndex);
    end;

    for SegmentIndex := 0 to LowerSegmentIndex do
    begin
      ASegment := Boundary.FSegments[SegmentIndex];
      if SegmentIndex = LowerSegmentIndex then
      begin
        if ASegment.FPosition1 = LowerNode.Position then
        begin
          break;
        end;
        NNode := Boundary[LowerNode.Position];
      end
      else
      begin
        NNode := Boundary[ASegment.FPosition2];
      end;
      Polygon2[NodeIndex] := NNode.FNode.Location;
      Inc(NodeIndex);
    end;

    SetLength(Polygon2, NodeIndex);

    FArea1 := Abs(Area(Polygon1));
    FArea2 := Abs(Area(Polygon2));
    if (FArea1 < Epsilon) or (FArea2 < Epsilon) then
    begin
      FVisible := False;
      Exit;
    end;

  end
  else
  begin
    // We are joining two separate boundaries.
    for BoundaryIndex := 0 to FQuadMeshCreator.FBoundaries.Count - 1 do
    begin
      ABoundary := FQuadMeshCreator.FBoundaries[BoundaryIndex];
      Compute(ABoundary);
      if not FVisible then
      begin
        Exit;
      end;
    end;
  end;

  if FVisible and (FNode1.FBoundary = FNode2.FBoundary) then
  begin
    NodeList := TNodeList.Create;
    try
      if FNode1.FPosition + 3 = FNode2.Position then
      begin
        for NodeIndex := FNode1.FPosition to FNode2.Position do
        begin
          NodeList.Add(FNode1.FBoundary[NodeIndex].FNode);
        end;
      end
      else if FNode2.Position + 3 - FNode1.FBoundary.Count =  FNode1.FPosition then
      begin
        for NodeIndex := FNode2.FPosition to FNode1.FBoundary.Count-1 do
        begin
          NodeList.Add(FNode1.FBoundary[NodeIndex].FNode);
        end;
        for NodeIndex := 1 to FNode1.Position do
        begin
          NodeList.Add(FNode1.FBoundary[NodeIndex].FNode);
        end;
      end
      else if FNode2.FPosition + 3 = FNode1.Position then
      begin
        for NodeIndex := FNode2.FPosition to FNode1.Position do
        begin
          NodeList.Add(FNode2.FBoundary[NodeIndex].FNode);
        end;
      end
      else if FNode1.Position + 3 - FNode2.FBoundary.Count =  FNode2.FPosition then
      begin
        for NodeIndex := FNode1.FPosition to FNode2.FBoundary.Count-1 do
        begin
          NodeList.Add(FNode2.FBoundary[NodeIndex].FNode);
        end;
        for NodeIndex := 1 to FNode2.Position do
        begin
          NodeList.Add(FNode2.FBoundary[NodeIndex].FNode);
        end;
      end;

      if NodeList.Count = 4 then
      begin
        ANode1 := NodeList[2];
        ANode2 := NodeList[3];
        for NodeIndex := 0 to NodeList.Count - 1 do
        begin
          ANode3 := NodeList[NodeIndex];
          if Orientation(ANode1.Location, ANode2.Location,
            ANode3.Location) = ClockWise then
          begin
            ASegment := TSegment.Create(FNode1.FNode, FNode2.FNode, stInner,
              FNode1.FBoundary, FQuadMeshCreator);
            try
              if ASegment.NodesToInsert = 0 then
              begin
                FVisible := False;
                Exit;
              end;
            finally
              FNode1.RemoveSegment(ASegment);
              FNode2.RemoveSegment(ASegment);
              ASegment.Free;
            end;
          end;
          ANode1 := ANode2;
          ANode2 := ANode3;
        end;
      end;
    finally
      NodeList.Free;
    end;
  end
end;

procedure TCost.GetNeighborNodes(var PriorNode1, SubsequentNode1, PriorNode2,
  SubsequentNode2: TNodeInBoundary);
begin
  // Compute first factor: splitting angles.
  if FNode1.Position = 0 then
  begin
    Assert(FNode1.FBoundary.Items[FNode1.FBoundary.Count - 1] = FNode1);
    if FNode1.FBoundary.Count = 1 then
    begin
      PriorNode1 := FNode1.FBoundary.Items[0];
    end
    else
    begin
      PriorNode1 := FNode1.FBoundary.Items[FNode1.FBoundary.Count - 2];
    end;
  end
  else
  begin
    PriorNode1 := FNode1.FBoundary.Items[FNode1.Position - 1];
  end;
  if FNode1.FBoundary.Count = 1 then
  begin
    SubsequentNode1 := FNode1.FBoundary.Items[0];
  end
  else
  begin
    Assert(FNode1.Position < FNode1.FBoundary.Count - 1);
    SubsequentNode1 := FNode1.FBoundary.Items[FNode1.Position + 1];
  end;

  if FNode2.Position = 0 then
  begin
    Assert(FNode2.FBoundary.Items[FNode2.FBoundary.Count - 1] = FNode2);
    if FNode2.FBoundary.Count = 1 then
    begin
      PriorNode2 := FNode2.FBoundary.Items[0];
    end
    else
    begin
      PriorNode2 := FNode2.FBoundary.Items[FNode2.FBoundary.Count - 2];
    end;
  end
  else
  begin
    PriorNode2 := FNode2.FBoundary.Items[FNode2.Position - 1];
  end;
  if FNode2.FBoundary.Count = 1 then
  begin
    SubsequentNode2 := FNode2.FBoundary.Items[0];
  end
  else
  begin
    Assert(FNode2.Position < FNode2.FBoundary.Count - 1);
    SubsequentNode2 := FNode2.FBoundary.Items[FNode2.Position + 1];
  end;
end;

function ComputeGamma(Theta: TFloat): double;
const
  StraightAngle = Pi * (1 + Epsilon);
begin
  if Theta > StraightAngle then
  begin
    result := (2*Pi - Theta)/Pi;
  end
  else
  begin
    result := 1;
  end;
end;

{ TCost }

procedure TCost.ComputeCost(LowestCost: TCost);
// Constants from page 1334.
var
  PriorNode1: TNodeInBoundary;
  SubsequentNode1: TNodeInBoundary;
  Angle1: TFloat;
  Angle2: TFloat;
  PriorNode2: TNodeInBoundary;
  SubsequentNode2: TNodeInBoundary;
  Angle3: TFloat;
  Angle4: TFloat;
  Zeta1: double;
  Zeta2: double;
  Psi: Extended;
  Zeta1TimesZeta2: double;
  Sigma1: double;
  Sigma2: double;
  Sigma: Extended;
  N: Integer;
  Eta: double;
  Alpha: double;
  ComputeAll: boolean;
  SixNodeCost: boolean;
  LocalC1: Double;
  LocalC2: Double;
  LocalC3: Double;
  LocalC4: Double;
  LocalC5: Double;
  LocalC6: Double;
  function GetSigma(ANode: TNodeInBoundary; Theta: TFloat): double;
  var
    DeltaElement: Integer;
  begin
    DeltaElement :=
      ANode.FNode.FElements.Count + 1
        - ANode.FNode.FDesiredElementCount;
    if DeltaElement < 0 then
    begin
      DeltaElement := 0;
    end;
    case DeltaElement of
      0:
        begin
          result := 4;
        end;
      1:
        begin
          result := 36;
        end;
    else
      begin
        result := 36*Sqr(DeltaElement);
      end;
    end;
  end;

begin
  ComputeAll := FNodeDistance < 0;
  SixNodeCost := (FNode1.FBoundary = FNode2.FBoundary) and
    (FNode1.FBoundary.Count = 7);

  if SixNodeCost then
  begin
    LocalC1 := AltC1;
    LocalC2 := AltC2;
    LocalC3 := AltC3;
    LocalC4 := AltC4;
    LocalC5 := AltC5;
    LocalC6 := AltC6;
  end
  else
  begin
    LocalC1 := C1;
    LocalC2 := C2;
    LocalC3 := C3;
    LocalC4 := C4;
    LocalC5 := C5;
    LocalC6 := C6;
  end;

  if FNode1.FNode.FNodeNumber = FNode2.FNode.FNodeNumber then
  begin
    FCost := 0;
    Exit;
  end;
  if ComputeAll or SixNodeCost then
  begin
    GetNeighborNodes(PriorNode1, SubsequentNode1, PriorNode2, SubsequentNode2);

    Angle1 := VertexAngleRadians(PriorNode1.FNode.Location,
      FNode1.FNode.Location, FNode2.FNode.Location);
    Angle2 := VertexAngleRadians(FNode2.FNode.Location, FNode1.FNode.Location,
      SubsequentNode1.FNode.Location);
    Angle3 := VertexAngleRadians(PriorNode2.FNode.Location,
      FNode2.FNode.Location, FNode1.FNode.Location);
    Angle4 := VertexAngleRadians(FNode1.FNode.Location, FNode2.FNode.Location,
      SubsequentNode2.FNode.Location);
    if SixNodeCost then
    begin
      FPhi := (Abs(Angle1 - PiDiv2) + Abs(Angle2 - PiDiv2) +
        Abs(Angle3 - PiDiv2) + Abs(Angle4 - PiDiv2)) / TwoPi;
      FTheta1 := Angle1 + Angle2;
      FTheta2 := Angle3 + Angle4;
    end
    else
    begin

      FTheta1 := Angle1 + Angle2;
      FTheta2 := Angle3 + Angle4;

      // Evaluate equation2.
      if (FTheta1 < PiDiv2) and (FTheta2 < PiDiv2) then
      begin
        FPhi := 1.0;
      end
      else
      begin
        // Equation 3
        Psi := (Abs(Angle1 - Angle2) + Abs(Angle3 - Angle4)) /
          (FTheta1 + FTheta2);
        if ((PiDiv2 <= FTheta1) and (FTheta1 <= TwoPiDiv3)) or
          ((PiDiv2 <= FTheta2) and (FTheta2 <= TwoPiDiv3)) then
        begin
          // Equation 4
          if ((PiDiv2 < FTheta1) and (FTheta1 < TwoPiDiv3)) then
          begin
            Zeta1 := (FTheta1 - TwoPiDiv3) / PiDiv6;
          end
          else
          begin
            Zeta1 := 1
          end;
          if ((PiDiv2 < FTheta2) and (FTheta2 < TwoPiDiv3)) then
          begin
            Zeta2 := (FTheta2 - TwoPiDiv3) / PiDiv6;
          end
          else
          begin
            Zeta2 := 1
          end;
          Zeta1TimesZeta2 := Zeta1 * Zeta2;
          FPhi := (1 - Zeta1TimesZeta2) + Zeta1TimesZeta2 * Psi;
        end
        else
        begin
          FPhi := Psi;
        end;
      end;
    end;
  end;

  FCost := LocalC1 * FPhi {+ LocalC2 * Sigma + LocalC3 * Eta + LocalC4 * F_l
    + LocalC5 * Alpha + LocalC6 * FGamma};
  if (LowestCost <> nil) and (FCost > LowestCost.Cost) then
  begin
    Exit;
  end;

  // Compute second factor: Structuring index
  Sigma1 := GetSigma(FNode1, FTheta1);
  Sigma2 := GetSigma(FNode2, FTheta2);
  Sigma := (Sigma1 + Sigma2) / 200;

  FCost := FCost + LocalC2 * Sigma {+ LocalC3 * Eta + LocalC4 * F_l
    + LocalC5 * Alpha + LocalC6 * FGamma};
  if (LowestCost <> nil) and (FCost > LowestCost.Cost) then
  begin
    Exit;
  end;

  // RBW modification: Favor splitting at  concave and straight locations.
  if ComputeAll then
  begin
    FGamma := (ComputeGamma(FTheta1) +
      ComputeGamma(FTheta2)) / 2;
  end;

  FCost := FCost + LocalC6 * FGamma {+ LocalC3 * Eta + LocalC4 * F_l
    + LocalC5 * Alpha };
  if (LowestCost <> nil) and (FCost > LowestCost.Cost) then
  begin
    Exit;
  end;

  // Compute third factor: Node placement error.
  if ComputeAll then
  begin
    FNodeDistance := Distance(FNode1.FNode.Location, FNode2.FNode.Location);
    FNStar := FNodeDistance /
      ((FNode1.DesiredSpacing + FNode2.DesiredSpacing) / 2);
  end;
  N := CalcNodesToInsert(FNStar);
  Assert(FNode1.FBoundary = FNode2.FBoundary);

  if Odd(N + Abs(FNode1.FPosition - FNode2.FPosition)) then
  begin
    Inc(N);
  end;

  // equation 6
  Eta := Abs(FNStar - N);

  FCost := FCost + LocalC3 * Eta {+ LocalC4 * F_l
    + LocalC5 * Alpha };
  if (LowestCost <> nil) and (FCost > LowestCost.Cost) then
  begin
    Exit;
  end;

  // Compute fourth factor: splitting line length
  // equation 8
  if ComputeAll then
  begin
    F_l := FNodeDistance / FQuadMeshCreator.FCharacteristicLength;
  end;

  FCost := FCost + LocalC4 * F_l
    {+ LocalC5 * Alpha };
  if (LowestCost <> nil) and (FCost > LowestCost.Cost) then
  begin
    Exit;
  end;

  // Compute fifth factor: Symmetry.
  if (FArea1 = 0) and (FArea2 = 0) then
  begin
    Alpha := MAXINT;
  end
  else
  begin
    Alpha := Abs(FArea2 - FArea1) / (FArea2 + FArea1);
  end;

  FCost := FCost + LocalC5 * Alpha;
//
//
//  FCost := LocalC1 * FPhi + LocalC2 * Sigma + LocalC3 * Eta + LocalC4 * F_l
//    + LocalC5 * Alpha + LocalC6 * FGamma;
end;


constructor TCost.Create(Node1, Node2: TNodeInBoundary;
  QuadMeshCreator: TQuadMeshCreator{; InitialVisibility: TInitialVisibility});
begin
  inherited Create;
  FNode1 := Node1;
  FNode2 := Node2;
  FQuadMeshCreator := QuadMeshCreator;
  FNodeDistance := -1;
  ComputeVisibility
end;

procedure TBoundary.AddNode(Node: TNode);
begin
  FNodes.Add(Node);
end;

procedure TBoundary.SetSegmentPositions;
var
  NodePosition: Integer;
  SegIndex: Integer;
  ANode: TNodeInBoundary;
  ASegment: TSegment;
begin
  NodePosition := 0;
  ANode := Items[NodePosition];
  for SegIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegIndex];
    Assert(ASegment.FNode1 = ANode.FNode);
    ASegment.FPosition1 := NodePosition;
    NodePosition := NodePosition + ASegment.FInnerNodes.Count + 1;
    ANode := Items[NodePosition];
    Assert(ASegment.FNode2 = ANode.FNode);
    ASegment.FPosition2 := NodePosition;
  end;
  Assert(NodePosition = Count-1);
end;

procedure TBoundary.FixSegments;
var
  NodeIndex: Integer;
  SegIndex: Integer;
  ASegment: TSegment;
  SubsequentSegIndex: Integer;
  PriorSegIndex: Integer;
  ANode: TNodeInBoundary;
  UnusedSegs: array of Boolean;
  SegPosition: Integer;
begin
  SetLength(UnusedSegs, FSegments.Count);
  for SegIndex := 0 to Length(UnusedSegs) - 1 do
  begin
    UnusedSegs[SegIndex] := True;
  end;
  for NodeIndex := 0 to Count - 1 do
  begin
    ANode := Items[NodeIndex];
    for SegIndex := ANode.SegmentCount - 1 downto 0 do
    begin
      ASegment := ANode.Segments[SegIndex];
      SegPosition := FSegments.IndexOf(ASegment);
      if SegPosition < 0 then
      begin
        ANode.DeleteSegment(SegIndex);
      end
      else
      begin
        UnusedSegs[SegPosition]:= False;
      end;
    end;
  end;
  for SegIndex := FSegments.Count - 1 downto 0 do
  begin
    if UnusedSegs[SegIndex] then
    begin
      FSegments.Extract(FSegments[SegIndex]);
    end;
  end;

  for NodeIndex := 0 to Count - 2 do
  begin
    ANode := Items[NodeIndex];
    for SegIndex := 0 to ANode.SegmentCount - 1 do
    begin
      ASegment := ANode.Segments[SegIndex];
      if ANode.SegmentCount = 1 then
      begin
        if (ASegment.FInnerNodes.IndexOf(ANode.FNode) < 0) then
        begin
          if ASegment.FNode1 = ANode.FNode then
          begin
            PriorSegIndex := FSegments.IndexOf(ASegment);
            Assert(PriorSegIndex >= 0);
            Dec(PriorSegIndex);
            if PriorSegIndex < 0 then
            begin
              PriorSegIndex := FSegments.Count - 1;
            end;
            ANode.InsertSegment(0, FSegments[PriorSegIndex]);
          end
          else if ASegment.FNode2 = ANode.FNode then
          begin
            SubsequentSegIndex := FSegments.IndexOf(ASegment);
            Assert(SubsequentSegIndex >= 0);
            Inc(SubsequentSegIndex);
            if SubsequentSegIndex >= FSegments.Count then
            begin
              SubsequentSegIndex := 0;
            end;
            ANode.AddSegment(FSegments[SubsequentSegIndex]);
          end
          else
          begin
            Assert(False);
          end;
        end;
      end
      else
      begin
        Assert((ASegment.FNode1 = ANode.FNode) or (ASegment.FNode2 = ANode.FNode));
      end;
    end;
  end;
end;

procedure TBoundary.SetBypassUpdate(const Value: Boolean);
begin
  FBypassUpdate := Value;
end;

procedure TBoundary.SetCounterClockwiseOrientation;
var
  SegIndex: Integer;
  BoundaryIndex: Integer;
begin
  if Count = 0 then
  begin
    for BoundaryIndex := 0 to SubPartCount - 1 do
    begin
      SubParts[BoundaryIndex].SetCounterClockwiseOrientation;
    end;
  end
  else
  begin
    if (Orientation <> CounterClockwise) then
    begin
      Reverse;
      FSegments.Reverse;
      if FSegments.Count >= 2 then
      begin
        for SegIndex := 0 to FSegments.Count - 1 do
        begin
          FSegments[SegIndex].Reverse;
        end;
      end;
      RenumberNodes;
    end;
  end;
end;

procedure TBoundary.SetElementNumber(Value: Integer);
begin
  FElementNumber := Value;
end;

procedure TBoundary.RemoveSelfFromOwnNodes;
var
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to Count - 1 do
  begin
    Items[NodeIndex].FNode.FElements.Remove(self);
  end;
end;

procedure TBoundary.RemoveProjections;
var
  ProjectionsDeleted: Boolean;
  NodeIndex: integer;
begin
  repeat
    ProjectionsDeleted := False;
    for NodeIndex := FNodes.Count - 1 downto 0 do
    begin
      if (NodeIndex < FNodes.Count) and (NodeIndex >= 2) then
      begin
        if FNodes[NodeIndex] = FNodes[NodeIndex-2] then
        begin
          FNodes.Delete(NodeIndex);
          FNodes.Delete(NodeIndex-1);
          ProjectionsDeleted := True;
        end;
      end;
    end;
    if FNodes.Count >= 3 then
    begin
      if FNodes[1] = FNodes[FNodes.Count-2] then
      begin
        FNodes.Delete(FNodes.Count-1);
        FNodes.Delete(0);
        ProjectionsDeleted := True;
      end;
    end;
  until not ProjectionsDeleted;
end;

procedure TBoundary.RemoveSelfFromAllNodes;
var
  NodeIndex: Integer;
  ANode: TNode;
begin
  for NodeIndex := 0 to FQuadMeshCreator.FNodes.Count - 1 do
  begin
    ANode := FQuadMeshCreator.FNodes[NodeIndex];
    ANode.FElements.Remove(self);
  end;
end;

function TBoundary.Area: double;
var
  Poly: TPolygon2D;
  index: Integer;
begin
  result := 0;
  if FSegments.Count > 0 then
  begin
    SetLength(Poly, FSegments.Count);
    for index := 0 to FSegments.Count - 1 do
    begin
      Poly[index] := FSegments[index].FNode1.Location;
    end;
    Result := Abs(FastGEO.Area(Poly));
  end
  else if FNodes.Count > 0 then
  begin
    SetLength(Poly, FNodes.Count-1);
    for index := 0 to FNodes.Count - 2 do
    begin
      Poly[index] := FNodes[index].Location;
    end;
    Result := Abs(FastGEO.Area(Poly));
  end;

end;

procedure TBoundary.AssignConstraintNodes;
var
  Index: Integer;
  PriorNode: TNode;
  ANode: TNode;
begin
  Assert(Count > 0);
  PriorNode := Items[0].FNode;
  Assert(PriorNode.FNodeType <> ntInner);
  for Index := 1 to Count - 1 do
  begin
    ANode := Items[Index].FNode;
    Assert(ANode.FNodeType <> ntInner);
    ANode.FConstraintNeighbors.Add(PriorNode);
    PriorNode.FConstraintNeighbors.Add(ANode);
    PriorNode := ANode;
  end;
end;

procedure TBoundary.AssignOriginalEdgeAngles;
var
  PriorNode: TNode;
  CurrentNode: TNode;
  NextNode: TNode;
  SegmentIndex: Integer;
  ASegment: TSegment;
  NodeIndex: Integer;
  InnerNode: TNode;
  NodeAngle: double;
begin
  if FConverted then
  begin
    Exit
  end;

  PriorNode := FSegments[FSegments.Count - 1].FNode1;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    CurrentNode := ASegment.Node1;
    NextNode := ASegment.Node2;
    NodeAngle := VertexAngleRadians(PriorNode.Location, CurrentNode.Location,
      NextNode.Location);
    if (NodeAngle = 0) and not CurrentNode.FIntersection then
    begin
      NodeAngle := 2 * Pi;
    end;
    CurrentNode.FTotalAngle := CurrentNode.FTotalAngle + NodeAngle;
    PriorNode := CurrentNode;
    for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
    begin
      InnerNode := ASegment.FInnerNodes[NodeIndex];
      InnerNode.FTotalAngle := InnerNode.FTotalAngle + Pi;
    end;
  end;
end;

{$IFDEF TEST}
{$IFDEF TEST}
function TBoundary.Center: TPoint2D;
var
  index: Integer;
begin
  Assert(Count = 5);
  Result.x := 0;
  Result.y := 0;
  for index := 0 to 3 do
  begin
    Result.x := Result.x + Items[index].FNode.FLocation.x;
    Result.y := Result.y + Items[index].FNode.FLocation.y;
  end;
  Result.x := Result.x/4;
  Result.y := Result.y/4;
end;
{$ENDIF}

procedure TBoundary.CheckInvalidElement;
var
  ANode: TNode;
  NodeIndex: Integer;
  BoundaryNodes: TNodeList;
  Node1: TNode;
  Node0: TNode;
  Node2: TNode;
  Node3: TNode;
begin
  if Count <> 5 then
  begin
    Exit;
  end;
  BoundaryNodes := TNodeList.Create;
  try
    for NodeIndex := 0 to Count - 2 do
    begin
      ANode := Items[NodeIndex].FNode;
      if ANode.FNodeType in [ntEdge, ntSubDomain] then
      begin
        BoundaryNodes.Add(ANode);
      end;
    end;
    if BoundaryNodes.Count = 3 then
    begin
      if BadAngle(BoundaryNodes[0].FLocation, BoundaryNodes[1].FLocation,
        BoundaryNodes[2].FLocation) then
      begin
        InvalidElement := True;
      end;
    end;
  finally
    BoundaryNodes.Free;
  end;
  Node0 := Items[0].FNode;
  Node1 := Items[1].FNode;
  Node2 := Items[2].FNode;
  Node3 := Items[3].FNode;
  if not Intersect(EquateSegment(Node0.Location, Node2.Location),
    EquateSegment(Node1.Location, Node3.Location)) then
  begin
    InvalidElement := True;
  end;
end;
{$ENDIF}

procedure TBoundary.ConvertProjectionsToBoundaries;
var
  ProjectionsDeleted: Boolean;
  NodeIndex: integer;
  Node1: TNode;
  Node2: TNode;
  BoundaryList: TBoundaryList;
  BIndex: Integer;
  ABoundary: TBoundary;
  procedure CreateNewBoundary;
  var
    NewBoundary: TBoundary;
    ASegment: TSegment;
    NodeInBoundary: TNodeInBoundary;
  begin
    NewBoundary := TBoundary.Create(FQuadMeshCreator, nil, FDesiredSpacing);
    BoundaryList.Add(NewBoundary);
    FQuadMeshCreator.FBoundaries.Add(NewBoundary);
    ASegment := TSegment.Create(Node1, Node2, stSubDomain, NewBoundary, FQuadMeshCreator);
    NewBoundary.FSegments.Add(ASegment);
    NodeInBoundary := TNodeInBoundary.Create(Node1, NewBoundary, ASegment);
    NodeInBoundary.FPosition := NewBoundary.Add(NodeInBoundary);
    NodeInBoundary := TNodeInBoundary.Create(Node2, NewBoundary, ASegment);
    NodeInBoundary.FPosition := NewBoundary.Add(NodeInBoundary);
    NewBoundary.FNodes.Add(Node1);
    NewBoundary.FNodes.Add(Node2);
  end;
begin
  BoundaryList := TBoundaryList.Create;
  try
    repeat
      ProjectionsDeleted := False;
      for NodeIndex := Count - 1 downto 0 do
      begin
        if (NodeIndex < Count) and (NodeIndex >= 2) then
        begin
          if Items[NodeIndex].FNode = Items[NodeIndex-2].FNode then
          begin
            Node1 := Items[NodeIndex].FNode;
            Node2 := Items[NodeIndex-1].FNode;
            Delete(NodeIndex);
            Delete(NodeIndex-1);
            FNodes.Remove(Node1);
            FNodes.Remove(Node2);
            ProjectionsDeleted := True;
            CreateNewBoundary;
          end;
        end;
      end;
      if Count >= 3 then
      begin
        if Items[1].FNode = Items[Count-2].FNode then
        begin
          Node1 := Items[Count-1].FNode;
          Node2 := Items[0].FNode;
          Delete(Count-1);
          Delete(0);
          FNodes.Remove(Node1);
          FNodes.Remove(Node2);
          ProjectionsDeleted := True;
          CreateNewBoundary;
        end;
      end;
    until not ProjectionsDeleted;

    for BIndex := 0 to BoundaryList.Count - 1 do
    begin
      ABoundary := BoundaryList[BIndex];
      if (IndexOfNode(ABoundary.FNodes[0]) >= 0)
        or (IndexOfNode(ABoundary.FNodes[1]) >= 0) then
      begin
        FQuadMeshCreator.FBoundaries.Remove(ABoundary);
      end;
    end;
  finally
    BoundaryList.Free;
  end;
end;

procedure TBoundary.ConvertToClosedBoundary;
var
  FirstSegment: TSegment;
  LastSegment: TSegment;
  SegmentIndex: Integer;
  ASegment: TSegment;
begin
  Assert(FSegments.Count > 0);
  FirstSegment := FSegments[0];
  LastSegment := FSegments[FSegments.Count - 1];
  if FirstSegment.Node1 <> LastSegment.Node2 then
  begin
    FConverted := True;
    for SegmentIndex := FSegments.Count - 1 downto 0 do
    begin
      ASegment := FSegments[SegmentIndex];
      ASegment := ASegment.CreateReversedSegment;
      FSegments.Add(ASegment);
    end;
  end;
end;

constructor TBoundary.Create(QuadMeshCreator: TQuadMeshCreator;
  Parent: TBoundary; DesiredSpacing: double);
begin
  inherited Create;
  FParent := Parent;
  FQuadMeshCreator := QuadMeshCreator;
  FNodes := TNodeList.Create;
  FSubParts := TBoundaryObjectList.Create;
  FSegments := TSegmentObjectList.Create;
  Assert(DesiredSpacing > 0);
  FDesiredSpacing := DesiredSpacing;
end;

procedure TBoundary.CreateBoundaryNodes;
var
  SegmentIndex: Integer;
  ASegment: TSegment;
  ANode: TNodeInBoundary;
  PriorSegment: TSegment;
  NodeIndex: Integer;
begin
  Assert(Count = 0);
  if FSegments.Count = 1 then
  begin
    PriorSegment := nil;
  end
  else
  begin
    PriorSegment := FSegments[FSegments.Count - 1];
    Assert(FSegments[0].Node1 = PriorSegment.Node2);
  end;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    ANode := TNodeInBoundary.Create(ASegment.Node1, self, ASegment);
    ANode.FPosition := Add(ANode);
    if PriorSegment <> nil then
    begin
      Assert(PriorSegment.Node2 = ASegment.Node1);
      ANode.AddSegment(PriorSegment);
    end;
    for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
    begin
      ANode := TNodeInBoundary.Create(ASegment.FInnerNodes[NodeIndex], self,
        ASegment);
      ANode.FPosition := Add(ANode);
    end;
    PriorSegment := ASegment;
  end;
  if Count > 1 then
  begin
    Add(Items[0]);
  end;
end;

destructor TBoundary.Destroy;
begin
  FSubPoly.Free;
  FSegments.Free;
  FSubParts.Free;
  FNodes.Free;
  inherited;
end;

procedure TBoundary.GenerateSegments(DesiredOrientation: Integer);
var
  NodeIndex: Integer;
  Segment: TSegment;
  Points: TPolygon2D;
  AnOrientation: Integer;
  OutputPoints: TPolygon2D;
  Node1: TNode;
  Node2: TNode;
  SegmentType: TSegmentType;
  SegmentType1: TSegmentType;
  SegmentType2: TSegmentType;
begin
  Assert(FSegments.Count = 0);
  if FNodes.Count = 1 then
  begin
    SegmentType := stInner;
    case FNodes[0].NodeType of
      ntInner: SegmentType := stInner;
      ntEdge: SegmentType := stEdge;
      ntSubDomain: SegmentType := stSubDomain;
      else Assert(False);
    end;
    Segment := TSegment.Create(FNodes[0], FNodes[0], SegmentType, self,
      FQuadMeshCreator);
    FSegments.Add(Segment);
  end
  else
  begin
    if FNodes[0] = FNodes[FNodes.Count - 1] then
    begin
      SetLength(Points, FNodes.Count);
      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        Points[NodeIndex] := FNodes[NodeIndex].Location;
      end;
      ConvexHull2(Points, AnOrientation, OutputPoints);
      if (AnOrientation <> CollinearOrientation) and
        (AnOrientation <> DesiredOrientation) then
      begin
        FNodes.Reverse;
      end;
    end;
    for NodeIndex := 0 to FNodes.Count - 2 do
    begin
      Node1 := FNodes[NodeIndex];
      Node2 := FNodes[NodeIndex + 1];
      SegmentType1 := stInner;
      case Node1.NodeType of
        ntInner: SegmentType1 := stInner;
        ntEdge: SegmentType1 := stEdge;
        ntSubDomain: SegmentType1 := stSubDomain;
        else Assert(False);
      end;
      SegmentType2 := stInner;
      case Node2.NodeType of
        ntInner: SegmentType2 := stInner;
        ntEdge: SegmentType2 := stEdge;
        ntSubDomain: SegmentType2 := stSubDomain;
        else Assert(False);
      end;
      if SegmentType1 < SegmentType2 then
      begin
        SegmentType := SegmentType1;
      end
      else
      begin
        SegmentType := SegmentType2;
      end;
      Segment := TSegment.Create(Node1, Node2, SegmentType, self,
        FQuadMeshCreator);
      FSegments.Add(Segment);
    end;
  end;
end;

function TBoundary.GetElementNumber: Integer;
begin
  result := FElementNumber;
end;

function TBoundary.GetActiveNode(Index: Integer): INode;
begin
  result := Items[Index].FNode;
end;

function TBoundary.GetActiveNodeCount: Integer;
begin
  result := Count - 1;
end;

function TBoundary.GetBypassUpdate: Boolean;
begin
  result := FBypassUpdate;
end;

function TBoundary.GetDisplayNumber: Integer;
begin
  result := ElementNumber + 1;
end;

function TBoundary.GetSubPart(Index: Integer): TBoundary;
begin
  result := FSubParts[Index];
end;

function TBoundary.GetSubPartCount: Integer;
begin
  result := FSubParts.Count;
end;

function TBoundary.IndexOfNode(ANode: TNode): Integer;
var
  NodeIndex: Integer;
begin
  result := -1;
  for NodeIndex := 0 to Count - 1 do
  begin
    if Items[NodeIndex].FNode = ANode then
    begin
      result := NodeIndex;
      break;
    end;
  end;
end;

procedure TBoundary.InsertNodesAlongBoundary;
var
  NodeDistance: TFloat;
  NumberOfNodesToInsert: Integer;
  SegmentIndex: Integer;
  ASegment: TSegment;
begin

  Assert(FSegments.Count > 0);

  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    if ASegment.Node1 = ASegment.Node2 then
    begin
      Continue;
    end;
    NodeDistance := ASegment.Length;
    if (NodeDistance > Min(ASegment.Node1.DesiredSpacing,
      ASegment.Node2.DesiredSpacing)) then
    begin
      NumberOfNodesToInsert := ASegment.NodesToInsert;
    end
    else
    begin
      NumberOfNodesToInsert := 1;
    end;
    if not Odd(NumberOfNodesToInsert) then
    begin
      Inc(NumberOfNodesToInsert);
    end;
    Assert(NumberOfNodesToInsert >= 1);
    FSegments[SegmentIndex].InsertNodes(NumberOfNodesToInsert);
  end;
end;

function TBoundary.IsBoundaryInside(AnotherBoundary: TBoundary): boolean;
var
  NodeIndex: Integer;
  FPoints: TRealPointArray;
  APoint: TPoint2D;
  ANode: TNode;
begin
  for NodeIndex := 0 to AnotherBoundary.FNodes.Count - 1 do
  begin
    ANode := AnotherBoundary.FNodes[NodeIndex];
    if FNodes.IndexOf(ANode) >= 0 then
    begin
      result := False;
      Exit;
    end;
  end;
  if FSubPoly = nil then
  begin
    SetLength(FPoints, FNodes.Count);
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      FPoints[NodeIndex] := FNodes[NodeIndex].FLocation;
    end;
    FSubPoly := TSubPolygon.Create(FPoints, FNodes.Count, 0, 0)
  end;
  APoint := AnotherBoundary.FNodes[0].FLocation;
  Result := FSubPoly.IsPointInside(APoint.X, APoint.y);
  if not result and (AnotherBoundary.FNodes.Count > 1) then
  begin
    APoint := AnotherBoundary.FNodes[1].FLocation;
    Result := FSubPoly.IsPointInside(APoint.X, APoint.y);
  end;
end;

function TBoundary.IsDetachedBoundaryInside(
  AnotherBoundary: TBoundary): boolean;
var
  NodeIndex: Integer;
  FPoints: TRealPointArray;
  APoint: TPoint2D;
  ANode: TNode;
//  NodesMatch: Boolean;
  MatchingNode: Integer;
begin
  MatchingNode := -1;
  for NodeIndex := 0 to AnotherBoundary.FNodes.Count - 1 do
  begin
    ANode := AnotherBoundary.FNodes[NodeIndex];
    if FNodes.IndexOf(ANode) >= 0 then
    begin
      MatchingNode := NodeIndex;
      break;
    end;
  end;
  if MatchingNode < 0 then
  begin
    result := False;
  end
  else
  begin
    if FSubPoly = nil then
    begin
      SetLength(FPoints, FNodes.Count);
      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        FPoints[NodeIndex] := FNodes[NodeIndex].FLocation;
      end;
      FSubPoly := TSubPolygon.Create(FPoints, FNodes.Count, 0, 0);
    end;
    Assert(MatchingNode in [0,1]);
    APoint := AnotherBoundary.FNodes[1-MatchingNode].FLocation;
    Result := FSubPoly.IsPointInside(APoint.X, APoint.y);
  end;
end;

procedure TBoundary.RenumberNodes;
var
  Index: Integer;
  SegIndex: Integer;
  PriorSegment: TSegment;
  ASegment: TSegment;
  ANode: TNodeInBoundary;
begin
  for Index := Count - 1 downto 0 do
  begin
    ANode := Items[Index];
    ANode.FPosition := Index;
    Assert(ANode.SegmentCount in [1,2]);
  end;
  PriorSegment := FSegments[FSegments.Count - 1];
  for SegIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegIndex];
    Assert(ASegment.Node1 = PriorSegment.Node2);
    Assert(ASegment.FBoundary = self);
    PriorSegment := ASegment;
  end;
end;

procedure TBoundary.SetNodeType(NodeType: TNodeType);
var
  Index: Integer;
begin
  for Index := FNodes.Count - 1 downto 0 do
  begin
    FNodes[Index].FNodeType := NodeType;
  end;
end;

procedure TBoundary.Split222(FirstIndex: Integer; NewLocation: TPoint2D);
var
  NewNode: TNode;
  procedure CreateSubBoundary(OffsetIndex: Integer);
  var
    NodeIndex: Integer;
    BoundNode1: TNodeInBoundary;
    BoundNode2: TNodeInBoundary;
    BoundNode3: TNodeInBoundary;
    SubBoundary: TBoundary;
    ASegment: TSegment;
    NewBoundNode: TNodeInBoundary;
  begin
    SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
      DesiredSpacing);
    FSubParts.Add(SubBoundary);
    NodeIndex := FirstIndex + OffsetIndex;
    if NodeIndex >= Count then
    begin
      NodeIndex := NodeIndex - Count;
    end;
    BoundNode1 := Items[NodeIndex];

    Inc(NodeIndex);
    if NodeIndex >= Count then
    begin
      NodeIndex := NodeIndex - Count;
    end;
    BoundNode2 := Items[NodeIndex];

    Inc(NodeIndex);
    if NodeIndex >= Count then
    begin
      NodeIndex := NodeIndex - Count;
    end;
    BoundNode3 := Items[NodeIndex];

    ASegment := TSegment.Create(BoundNode1.FNode, BoundNode2.FNode, stInner,
      SubBoundary, FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(BoundNode1.FNode, SubBoundary,
      ASegment);
    SubBoundary.Add(NewBoundNode);

    ASegment := TSegment.Create(BoundNode2.FNode, BoundNode3.FNode, stInner,
      SubBoundary, FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(BoundNode2.FNode, SubBoundary,
      ASegment);
    SubBoundary.Add(NewBoundNode);

    ASegment := TSegment.Create(BoundNode3.FNode, NewNode, stInner, SubBoundary,
      FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(BoundNode3.FNode, SubBoundary,
      ASegment);
    SubBoundary.Add(NewBoundNode);

    ASegment := TSegment.Create(NewNode, BoundNode1.FNode, stInner, SubBoundary,
      FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(NewNode, SubBoundary, ASegment);
    SubBoundary.Add(NewBoundNode);

    SubBoundary.Add(SubBoundary.Items[0]);
  end;

begin
  // 2-2-2
  // 2-1-1-2
  // 2-2-1-1
  if Count = 7 then
  begin
    Delete(Count - 1);
  end;
  Assert(Count = 6);
//  Delete(Count - 1);
  NewNode := TNode.Create(FQuadMeshCreator, DesiredSpacing);
  NewNode.Location := NewLocation;

  // First element
  CreateSubBoundary(1);

  // second element
  CreateSubBoundary(3);

  // Third element
  CreateSubBoundary(5);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;
end;


procedure TBoundary.Split222(FirstIndex: Integer);
var
  NodeIndex: Integer;
  BoundNode1: TNodeInBoundary;
  BoundNode2: TNodeInBoundary;
  BoundNode3: TNodeInBoundary;
  NewLocation: TPoint2D;
begin
  // 2-2-2
  // 2-1-1-2
  // 2-2-1-1
  Assert(Count = 7);
  Delete(Count - 1);
  NodeIndex := FirstIndex + 1;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BoundNode1 := Items[NodeIndex];
  Inc(NodeIndex, 2);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BoundNode2 := Items[NodeIndex];
  Inc(NodeIndex, 2);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BoundNode3 := Items[NodeIndex];

  NewLocation.X := (BoundNode1.FNode.X + BoundNode2.FNode.X +
    BoundNode3.FNode.X) / 3;
  NewLocation.Y := (BoundNode1.FNode.Y + BoundNode2.FNode.Y +
    BoundNode3.FNode.Y) / 3;

  Split222(FirstIndex, NewLocation);
end;

procedure TBoundary.Split312(FirstIndex: Integer);
var
  NodeIndex: Integer;
  BaseNode1: TNodeInBoundary;
  BaseNode2: TNodeInBoundary;
  BaseNode3: TNodeInBoundary;
  BaseNode4: TNodeInBoundary;
  Vec1: TVector2D;
  Vec2: TVector2D;
  VectorLength1: double;
  VectorLength2: double;
  BaseLength: TFloat;
  TriangleNode: TNodeInBoundary;
  SideNode: TNodeInBoundary;
  NewNode1: TNode;
  NewNode2: TNode;
  SubBoundary: TBoundary;
  ASegment: TSegment;
  NewBoundaryNode: TNodeInBoundary;
  Factor: double;
begin
  // 3-1-2
  // 3-1-1-1
  Assert(Count = 7);
  Delete(Count - 1);

  NodeIndex := FirstIndex;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode1 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode4 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TriangleNode := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  SideNode := Items[NodeIndex];

  Vec1.X := TriangleNode.FNode.X - BaseNode4.FNode.X;
  Vec1.Y := TriangleNode.FNode.Y - BaseNode4.FNode.Y;
  VectorLength1 := sqrt(Sqr(Vec1.X) + Sqr(Vec1.Y));

  Vec2.X := SideNode.FNode.X - BaseNode1.FNode.X;
  Vec2.Y := SideNode.FNode.Y - BaseNode1.FNode.Y;
  VectorLength2 := sqrt(Sqr(Vec2.X) + Sqr(Vec2.Y));

  BaseLength := Distance(BaseNode1.FNode.Location,
    BaseNode4.FNode.Location) / 3;

  Factor := Min(VectorLength1 / BaseLength, VectorLength2 / BaseLength) / 4;

  Vec1 := Scale(Vec1, Factor);
  Vec2 := Scale(Vec2, Factor);

  NewNode1 := TNode.Create(FQuadMeshCreator, DesiredSpacing);
  NewNode1.X := BaseNode3.FNode.X + Vec1.X;
  NewNode1.Y := BaseNode3.FNode.Y + Vec1.Y;

  NewNode2 := TNode.Create(FQuadMeshCreator, DesiredSpacing);
  NewNode2.X := BaseNode2.FNode.X + Vec2.X;
  NewNode2.Y := BaseNode2.FNode.Y + Vec2.Y;

  while not Intersect(EquateSegment(BaseNode2.FNode.Location,
    NewNode1.Location), EquateSegment(BaseNode3.FNode.Location,
    NewNode2.Location)) do
  begin
    Vec1 := Scale(Vec1, 0.8);
    Vec2 := Scale(Vec2, 0.8);
    NewNode1.X := BaseNode3.FNode.X + Vec1.X;
    NewNode1.Y := BaseNode3.FNode.Y + Vec1.Y;
    NewNode2.X := BaseNode2.FNode.X + Vec2.X;
    NewNode2.Y := BaseNode2.FNode.Y + Vec2.Y;
  end;

  // element 1
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode4.FNode, BaseNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode4.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode3.FNode, NewNode1, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode1, TriangleNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode1, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TriangleNode.FNode, BaseNode4.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 2
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode3.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode2.FNode, NewNode2, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode2, NewNode1, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode2, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode1, BaseNode3.FNode, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode1, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 3
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode2.FNode, BaseNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode1.FNode, SideNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(SideNode.FNode, NewNode2, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(SideNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode2, BaseNode2.FNode, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode2, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 4
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(SideNode.FNode, TriangleNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(SideNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TriangleNode.FNode, NewNode1, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode1, NewNode2, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode1, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode2, SideNode.FNode, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode2, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;
end;

procedure TBoundary.Split411(FirstIndex: Integer
  {$IFDEF TEST}; List: TBoundaryList{$ENDIF});
var
  NodeIndex: Integer;
  BaseNode1: TNodeInBoundary;
  BaseNode2: TNodeInBoundary;
  BaseNode3: TNodeInBoundary;
  BaseNode4: TNodeInBoundary;
  BaseNode5: TNodeInBoundary;
  TriangleNode: TNodeInBoundary;
  SubBoundary: TBoundary;
  ASegment: TSegment;
  NewBoundaryNode: TNodeInBoundary;
  SubIndex: Integer;
  EdgeSegment: TSegment;
  ReverseSegment: TSegment;
  InnerNodeIndex: Integer;
  ANode: TNode;
begin
  Assert(Count = 7);
  Delete(Count - 1);
  NodeIndex := FirstIndex;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode1 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode4 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode5 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TriangleNode := Items[NodeIndex];

  EdgeSegment := TSegment.Create(BaseNode3.FNode, TriangleNode.FNode, stInner,
    self, FQuadMeshCreator);
  EdgeSegment.InsertNodes(2);
  ReverseSegment := EdgeSegment.CreateReversedSegment;

  // element 1
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode5.FNode, BaseNode4.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode5.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode4.FNode, BaseNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode4.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  EdgeSegment.FBoundary := SubBoundary;
  SubBoundary.FSegments.Add(EdgeSegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    EdgeSegment);
  SubBoundary.Add(NewBoundaryNode);
  for InnerNodeIndex := 0 to EdgeSegment.FInnerNodes.Count - 1 do
  begin
    ANode := EdgeSegment.FInnerNodes[InnerNodeIndex];
    NewBoundaryNode := TNodeInBoundary.Create(ANode, SubBoundary, EdgeSegment);
    SubBoundary.Add(NewBoundaryNode);
  end;

  ASegment := TSegment.Create(TriangleNode.FNode, BaseNode5.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 2
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode3.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode2.FNode, BaseNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode1.FNode, TriangleNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ReverseSegment.FBoundary := SubBoundary;
  SubBoundary.FSegments.Add(ReverseSegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  for InnerNodeIndex := 0 to ReverseSegment.FInnerNodes.Count - 1 do
  begin
    ANode := ReverseSegment.FInnerNodes[InnerNodeIndex];
    NewBoundaryNode := TNodeInBoundary.Create(ANode, SubBoundary,
      ReverseSegment);
    SubBoundary.Add(NewBoundaryNode);
  end;

  SubBoundary.Add(SubBoundary.Items[0]);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;

  for SubIndex := 0 to FSubParts.Count - 1 do
  begin
    FSubParts[SubIndex].SpecialCase{$IFDEF TEST}(List){$ENDIF};
  end;
end;

procedure TBoundary.SplitMultiplyConnectedBoundary;
var
  NodeIndex: Integer;
  ANode: TNode;
  PriorIndex: Integer;
  APolygon1: TPolygon2D;
  InnerIndex: Integer;
  Epsilon: double;
  NIndex: Integer;
  Changed: Boolean;
  NewBoundary: TBoundary;
  ASegment: TSegment;
  StartNode: TNode;
  NewNode: TNodeInBoundary;
  ExistingNode: TNodeInBoundary;
  SegIndex: integer;
  NewSegment: TSegment;
  LastNode: TNodeInBoundary;
  OtherNode: TNodeInBoundary;
  SegmentIndex: integer;
  InnerSegIndex: integer;
  TestNode: TNodeInBoundary;
  InnerSeg: TSegment;
  InnerSegPos: integer;
  ABoundNode1: TNodeInBoundary;
  PriorSegment: TSegment;
  APolygon2: TPolygon2D;
  Nodes1: TNodeList;
  Nodes2: TNodeList;
  InternalPoly: boolean;
  InnerNode: TNode;
  InnerNodeIndex: Integer;
  EndPriorIndex: integer;
  StartIndex: integer;
  procedure ReplaceSegInBoundary(ABoundary: TBoundary; OldSeg, NewSeg: TSegment);
  var
    NIndex: integer;
    ANode: TNodeInBoundary;
    SegIndex: Integer;
  begin
    for NIndex := 0 to ABoundary.Count - 1 do
    begin
      ANode := Items[NIndex];
      SegIndex := ANode.IndexOfSegment(OldSeg);
      if SegIndex >= 0 then
      begin
        ANode.DeleteSegment(SegIndex);
        ANode.InsertSegment(SegIndex, NewSeg);
      end;
    end;
  end;
  procedure RemoveSegFromNodes(ABoundary: TBoundary; ASeg: TSegment);
  var
    NIndex: integer;
    ANode: TNodeInBoundary;
  begin
    for NIndex := 0 to ABoundary.Count - 1 do
    begin
      ANode := Items[NIndex];
      ANode.RemoveSegment(ASeg);
    end;
  end;
begin
  RenumberNodes;
  Epsilon := FQuadMeshCreator.FCharacteristicLength/1e7;
  repeat
    Changed := False;
    for NodeIndex := 0 to Count - 2 do
    begin
      if Changed then
      begin
        break;
      end;
      ANode := Items[NodeIndex].FNode;
      if ANode.FIntersection then
      begin
        EndPriorIndex := IndexOfNode(ANode);
        PriorIndex := EndPriorIndex;
        if (PriorIndex < NodeIndex) then
        begin
          StartIndex := NodeIndex-1;
          while not Changed do
          begin
            if StartIndex < EndPriorIndex then
            begin
              break;
            end;
            for InnerNodeIndex := StartIndex downto EndPriorIndex do
            begin
              if Items[InnerNodeIndex].FNode = ANode then
              begin
                PriorIndex := InnerNodeIndex;
                break;
              end;
            end;
            StartIndex := PriorIndex -1;

            Nodes1 := TNodeList.Create;
            Nodes2 := TNodeList.Create;
            try
              SetLength(APolygon1, NodeIndex-PriorIndex);
              Nodes1.Capacity := Length(APolygon1);
              NIndex := 0;
              for InnerIndex := PriorIndex to NodeIndex - 1 do
              begin
                Nodes1.Add(Items[InnerIndex].FNode);
                APolygon1[NIndex] := Items[InnerIndex].FNode.Location;
                Inc(NIndex);
              end;
              if Abs(FastGeo.Area(APolygon1)) < Epsilon then
              begin
                Continue;
              end;

              SetLength(APolygon2, Count);
              Nodes2.Capacity := Count;

              NIndex := 0;
              for InnerIndex := 0 to PriorIndex do
              begin
                Nodes2.Add(Items[NIndex].FNode);
                APolygon2[NIndex] := Items[NIndex].FNode.Location;
                Inc(NIndex);
              end;
              for InnerIndex := NodeIndex to Count - 2 do
              begin
                Nodes2.Add(Items[InnerIndex].FNode);
                APolygon2[NIndex] := Items[InnerIndex].FNode.Location;
                Inc(NIndex);
              end;
              SetLength(APolygon2, NIndex);
              if Abs(FastGeo.Area(APolygon1)) < Epsilon then
              begin
                Continue;
              end;

              InternalPoly := False;
              for InnerIndex := 0 to Nodes1.Count -1 do
              begin
                InnerNode := Nodes1[InnerIndex];
                if Nodes2.IndexOf(InnerNode) < 0 then
                begin
                  InternalPoly := PointInConcavePolygon(
                    InnerNode.Location, APolygon2);
                  Break;
                end;
              end;
              if InternalPoly then
              begin
                Continue;
              end;

              for InnerIndex := 0 to Nodes2.Count -1 do
              begin
                InnerNode := Nodes2[InnerIndex];
                if Nodes1.IndexOf(InnerNode) < 0 then
                begin
                  InternalPoly := PointInConcavePolygon(
                    InnerNode.Location, APolygon1);
                  Break;
                end;
              end;
              if InternalPoly then
              begin
                Continue;
              end;

            finally
              Nodes1.Free;
              Nodes2.Free;
            end;

            SetSegmentPositions;

            LastNode := Items[NodeIndex];
            for SegmentIndex := 0 to LastNode.SegmentCount - 1 do
            begin
              Assert(LastNode.Segments[SegmentIndex].FBoundary = self);
            end;

            NewBoundary := TBoundary.Create(FQuadMeshCreator, nil, FDesiredSpacing);
            FQuadMeshCreator.FBoundaries.Add(NewBoundary);

            StartNode := Items[PriorIndex].FNode;

            SegIndex := -1;
            ASegment := nil;
            begin
              for InnerIndex := 0 to FSegments.Count - 1 do
              begin
                if FSegments[InnerIndex].FNode1 = StartNode then
                begin
                  if FSegments[InnerIndex].FPosition1 = PriorIndex then
                  begin
                    ASegment := FSegments[InnerIndex];
                    SegIndex := InnerIndex;
                    Break;
                  end;
                end;
              end;
            end;
            Assert(ASegment <> nil);

            ASegment.FBoundary := NewBoundary;

            NewNode := TNodeInBoundary.Create(StartNode, NewBoundary, ASegment);
            NewNode.FPosition := NewBoundary.Add(NewNode);

            for InnerIndex := PriorIndex +1 to NodeIndex - 1 do
            begin
              ExistingNode := Items[InnerIndex];
              ExistingNode.FBoundary := NewBoundary;
              Assert(ExistingNode.SegmentCount in [1,2]);
              for InnerSegIndex := 0 to ExistingNode.SegmentCount - 1 do
              begin
                ExistingNode.Segments[InnerSegIndex].FBoundary := NewBoundary;
              end;
              ExistingNode.FPosition := NewBoundary.Add(ExistingNode);
            end;

            ExistingNode := NewBoundary[NewBoundary.Count-1];

            for InnerIndex := SegIndex to FSegments.Count - 1 do
            begin
              ASegment := FSegments[InnerIndex];
              Assert(ASegment.FBoundary = NewBoundary);
              NewBoundary.FSegments.Add(ASegment);
              if (ASegment.FNode2 = StartNode)
                and (ExistingNode.IndexOfSegment(ASegment) >= 0)
                then
              begin
                if (ASegment.FPosition2 = NodeIndex) then
                begin
                  NewNode.AddSegment(ASegment);
                  break;
                end;
              end;
            end;

            NewBoundary.Add(NewNode);



            for InnerIndex := 1 to NewBoundary.Count - 2 do
            begin
              Extract(NewBoundary.Items[InnerIndex]);
            end;

            for InnerIndex := 0 to NewBoundary.FSegments.Count - 1 do
            begin
              ASegment := NewBoundary.FSegments[InnerIndex];
              FSegments.Extract(ASegment);
            end;

            OtherNode := Items[PriorIndex];
            for SegIndex := OtherNode.SegmentCount - 1 downto 0 do
            begin
              ASegment := OtherNode.Segments[SegIndex];
              begin
                if (ASegment.FBoundary = self)
                  and (LastNode.IndexOfSegment(ASegment) < 0)
                  and ((ASegment.FPosition1 = PriorIndex)
                  or (ASegment.FPosition2 = PriorIndex))
                  then
                begin
                  LastNode.AddSegment(ASegment)
                end;
              end;
            end;
            Delete(PriorIndex);

            for SegIndex := LastNode.SegmentCount - 1 downto 0 do
            begin
              ASegment := LastNode.Segments[SegIndex];
              if (ASegment.FBoundary <> self) then
              begin
                LastNode.DeleteSegment(SegIndex);
              end;
            end;

            Assert(LastNode.SegmentCount = 2);

            NewBoundary.RenumberNodes;

            for InnerIndex := Count - 1 downto 0 do
            begin
              TestNode := Items[InnerIndex];
              begin
                for InnerSegIndex := TestNode.SegmentCount - 1 downto 0 do
                begin
                  InnerSeg := TestNode.Segments[InnerSegIndex];
                  if (InnerSeg.FBoundary <> self) then
                  begin
                    if InnerSeg.FBoundary <> NewBoundary then
                    begin
                      Assert(False);
                    end
                    else
                    begin
                      if NewBoundary.FSegments.IndexOf(InnerSeg) < 0 then
                      begin
                        NewSegment := InnerSeg.CreateReversedSegment;
                        NewSegment.FBoundary := self;
                        NewSegment.Reverse;
                        ReplaceSegInBoundary(self, InnerSeg, NewSegment);
                        RemoveSegFromNodes(NewBoundary, InnerSeg);
                        InnerSegPos := FSegments.IndexOf(InnerSeg);
                        Assert(InnerSegPos >= 0);
                        FSegments[InnerSegPos] := NewSegment;
                      end
                      else
                      begin
                        Assert(FSegments.IndexOf(InnerSeg) < 0);
                        TestNode.DeleteSegment(InnerSegIndex);
                      end;
                    end;
                  end;
                end;
              end;
            end;

            ABoundNode1 := Items[0];
            if ABoundNode1.SegmentCount = 2 then
            begin
              if ABoundNode1.Segments[0].FNode2
                <> ABoundNode1.Segments[1].FNode1 then
              begin
                ABoundNode1.ReverseSegments;
              end;
            end;

            PriorSegment := ABoundNode1.Segments[0];
            for InnerIndex := 0 to Count - 2 do
            begin
              ABoundNode1 := Items[InnerIndex];
              if ABoundNode1.SegmentCount = 2 then
              begin
                if ABoundNode1.Segments[0].FNode2
                  <> ABoundNode1.Segments[1].FNode1 then
                begin
                  ABoundNode1.ReverseSegments;
                end;
              end;
              Assert(ABoundNode1.SegmentCount in [1,2]);
            end;

            FixSegments;


            RenumberNodes;
            NewBoundary.FixSegments;

            Changed := True;
            break;
          end;
        end;
      end;
    end;
  until not Changed;

end;

//procedure TBoundary.SplitMultiplyConnectedBoundary2;
//var
//  Epsilon: double;
//  SegmentLists: TObjectList<TSegmentList>;
//  index: Integer;
//  ASegment: TSegment;
//  ASegmentList: TSegmentList;
//  FirstList: TSegmentList;
//  SegmentIndex: Integer;
//  IntersectionNodeList: TNodeList;
//  StartLists: TSegmentObjectListsSqr;
//  EndLists: TSegmentObjectListsSqr;
//  Angles: TAngleList;
//  SegmentListIndex: Integer;
//  ANode: TNode;
//  NodePostion: Integer;
//  NodeSegmentList: TSegmentLists;
//  NodeIndex: Integer;
//  CompareItem: TAngleCompareItem;
//  Changed: Boolean;
//  procedure FindPolygon(PolyList: TNodeList; NodePosition: integer;
//    SegmentLists: TSegmentObjectListsSqr);
//  var
//    NewPolyList: TNodeList;
//    ANode: TNode;
//    PositionInList: Integer;
//    Poly1: TPolygon2D;
//    NodeIndex: Integer;
//    PolySegments1: TSegmentList;
//    SegIndex: Integer;
//    SegListIndex: Integer;
//    ASegLists: TSegmentLists;
//    ASegList: TSegmentList;
//    StartIndex: Integer;
//    ASeg: TSegment;
//    PolySegments2: TSegmentList;
//    Poly2: TPolygon2D;
//    PriorSeg: TSegment;
//  begin
//    NewPolyList := TNodeList.Create;
//    try
//      NewPolyList.AddRange(PolyList);
//      ANode := IntersectionNodeList[NodePosition];
//      PositionInList := NewPolyList.IndexOf(ANode);
//      if PositionInList >= 0 then
//      begin
//        SetLength(Poly1, PolyList.Count - PositionInList);
//        for NodeIndex := PositionInList to NewPolyList.Count - 1 do
//        begin
//          Poly1[NodeIndex-PositionInList] := NewPolyList[NodeIndex].Location;
//        end;
//        if Abs(FastGeo.Area(Poly1)) > 0 then
//        begin
//          PolySegments1 := TSegmentList.Create;
//          try
//            for SegListIndex := 0 to SegmentLists.Count - 1 do
//            begin
//              ASegLists := SegmentLists[SegListIndex];
//              for SegIndex := 0 to ASegLists.Count - 1 do
//              begin
//                ASegList := ASegLists[SegIndex];
//                PolySegments1.AddRange(ASegList);
//              end;
//            end;
//            StartIndex := Segments.IndexOf(PolySegments1[0]);
//            PolySegments2 := TSegmentList.Create;
//            try
//              for SegIndex := StartIndex+1 to FSegments.Count - 1 do
//              begin
//                ASeg := FSegments[SegIndex];
//                if PolySegments1.IndexOf(ASeg) < 0 then
//                begin
//                  PolySegments2.Add(ASeg);
//                end;
//              end;
//              for SegIndex := 0 to StartIndex - 1 do
//              begin
//                ASeg := FSegments[SegIndex];
//                if PolySegments1.IndexOf(ASeg) < 0 then
//                begin
//                  PolySegments2.Add(ASeg);
//                end;
//              end;
//              if PolySegments2.Count > 0 then
//              begin
//                PriorSeg := PolySegments2[PolySegments2.Count-1];
//                SetLength(Poly2, PolySegments2.Count);
//                for SegIndex := 0 to PolySegments2.Count - 1 do
//                begin
//                  ASeg := PolySegments2[SegIndex];
//                  Assert(ASeg.FNode1 = PriorSeg.FNode2);
//                  Poly2[SegIndex] := ASeg.FNode1.Location;
//                  PriorSeg := ASeg;
//                end;
//                if Abs(FastGeo.Area(Poly2)) > 0 then
//                begin
//                  // break out boundary here and Set Changed := True;
//                  Exit;
//                end;
//              end;
//            finally
//              PolySegments2.Free;
//            end;
//          finally
//            PolySegments1.Free;
//          end;
//        end;
//      end;
//      // if you reach here find the next nodes and call this recursively.
//    finally
//      NewPolyList.Free;
//    end;
//  end;
//begin
//  RenumberNodes;
//  Epsilon := FQuadMeshCreator.FCharacteristicLength/1e7;
//  SetSegmentPositions;
//  Changed := False;
//  SegmentLists := TObjectList<TSegmentList>.Create;
//  try
//    repeat
//      ASegment := nil;
//      ASegmentList := nil;
//      for index := 0 to Segments.Count - 1 do
//      begin
//        ASegment := Segments[index];
//        if (index = 0) or ASegment.FNode1.FIntersection then
//        begin
//          ASegmentList := TSegmentList.Create;
//          SegmentLists.Add(ASegmentList);
//        end;
//        ASegmentList.Add(ASegment);
//      end;
//      if (SegmentLists.Count > 1) and not ASegment.FNode2.FIntersection then
//      begin
//        FirstList := SegmentLists[0];
//        for SegmentIndex := 0 to FirstList.Count - 1 do
//        begin
//          ASegmentList.Add(FirstList[SegmentIndex]);
//        end;
//        SegmentLists.Delete(0);
//      end;
//      if SegmentLists.Count > 1 then
//      begin
//        IntersectionNodeList := TNodeList.Create;
//        StartLists := TSegmentObjectListsSqr.Create;
//        EndLists := TSegmentObjectListsSqr.Create;
//        try
//          for SegmentListIndex := 0 to SegmentLists.Count - 1 do
//          begin
//            ASegmentList := SegmentLists[SegmentListIndex];
//            ASegment := ASegmentList[0];
//            ANode := ASegment.FNode1;
//            NodePostion := IntersectionNodeList.IndexOf(ANode);
//            if NodePostion < 0 then
//            begin
//              NodePostion := IntersectionNodeList.Add(ANode);
//              NodeSegmentList := TSegmentLists.Create;
//              StartLists.Add(NodeSegmentList)
//            end
//            else
//            begin
//              NodeSegmentList := StartLists[NodePostion];
//            end;
//            NodeSegmentList.Add(ASegmentList);
//
//            ASegment := ASegmentList[ASegmentList.Count-1];
//            ANode := ASegment.FNode2;
//            NodePostion := IntersectionNodeList.IndexOf(ANode);
//            if NodePostion < 0 then
//            begin
//              NodePostion := IntersectionNodeList.Add(ANode);
//              NodeSegmentList := TSegmentLists.Create;
//              EndLists.Add(NodeSegmentList)
//            end
//            else
//            begin
//              NodeSegmentList := EndLists[NodePostion];
//            end;
//            NodeSegmentList.Add(ASegmentList);
//          end;
//
//          Assert(IntersectionNodeList.Count = StartLists.Count);
//          Assert(IntersectionNodeList.Count = EndLists.Count);
//
//          Angles := TAngleList.Create;
//          try
//            for NodeIndex := 0 to IntersectionNodeList.Count - 1 do
//            begin
//              Angles.Clear;
//              ANode := IntersectionNodeList[NodeIndex];
//              NodeSegmentList := StartLists[NodeIndex];
//              for SegmentIndex := 0 to NodeSegmentList.Count - 1 do
//              begin
//                ASegmentList := NodeSegmentList[SegmentIndex];
//                ASegment := ASegmentList[0];
//                Assert(ASegment.FNode1 = ANode);
//                CompareItem := TAngleCompareItem.Create;
//                Angles.Add(CompareItem);
//
//                CompareItem.Angle := ArcTan2(ASegment.FNode2.Y-ASegment.FNode2.Y, ASegment.FNode2.X-ASegment.FNode2.X);
//                CompareItem.NodePosition := ASegment.FPosition1;
//                CompareItem.Boundary := self;
//                CompareItem.Direction := dForward;
//              end;
//
//              NodeSegmentList := EndLists[NodeIndex];
//              for SegmentIndex := 0 to NodeSegmentList.Count - 1 do
//              begin
//                ASegmentList := NodeSegmentList[SegmentIndex];
//                ASegment := ASegmentList[ASegmentList.Count-1];
//                Assert(ASegment.FNode2 = ANode);
//                CompareItem := TAngleCompareItem.Create;
//                Angles.Add(CompareItem);
//
//                CompareItem.Angle := ArcTan2(ASegment.FNode2.Y-ASegment.FNode2.Y, ASegment.FNode2.X-ASegment.FNode2.X);
//                CompareItem.NodePosition := ASegment.FPosition2;
//                CompareItem.Boundary := self;
//                CompareItem.Direction := dBackward;
//              end;
//
//
//              // sort in order of descending angles.
//              Angles.Sort(TAngleComparer.Construct(
//                function (const L, R: TAngleCompareItem): integer
//                begin
//                  result := 0;
//                  if R = L then
//                  begin
//                    Exit;
//                  end;
//                  result := Sign(R.Angle - L.Angle);
//                  if result = 0 then
//                  begin
//                    Result := Ord(R.Direction) - Ord(L.Direction);
//                  end;
//                end
//                )) ;
//            end;
//          finally
//            Angles.Free;
//          end;
//
//          for NodeIndex := 0 to IntersectionNodeList.Count - 1 do
//          begin
//            ANode := IntersectionNodeList[NodeIndex];
//            // call FindPolygon here
//          end;
//
//        finally
//          EndLists.Free;
//          StartLists.Free;
//          IntersectionNodeList.Free;
//        end;
//      end;
//    until not Changed;
//  finally
//    SegmentLists.Free;
//  end;
//end;

procedure TBoundary.Split2121(FirstIndex: Integer);
var
  NodeIndex: Integer;
  BaseNode1: TNodeInBoundary;
  BaseNode2: TNodeInBoundary;
  BaseNode3: TNodeInBoundary;
  TopNode3: TNodeInBoundary;
  TopNode2: TNodeInBoundary;
  TopNode1: TNodeInBoundary;
  SubBoundary: TBoundary;
  ASegment: TSegment;
  NewBoundaryNode: TNodeInBoundary;
begin
  Assert(Count = 7);
  Delete(Count - 1);

  NodeIndex := FirstIndex;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode1 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TopNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TopNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TopNode1 := Items[NodeIndex];

  // element 1
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode3.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode2.FNode, TopNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode2.FNode, TopNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode3.FNode, BaseNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 2
  SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode2.FNode, BaseNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode1.FNode, TopNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode1.FNode, TopNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode2.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;
end;

procedure TBoundary.ReverseSubBoundaries;
var
  SubIndex: Integer;
begin
  if Count > 0 then
  begin
    Reverse;
  end
  else
  begin
    for SubIndex := 0 to FSubParts.Count - 1 do
    begin
      FSubParts[SubIndex].ReverseSubBoundaries;
    end;
  end;
end;

function TBoundary.SpecialCase({$ifdef TEST}List: TBoundaryList {$ENDIF}): boolean;
var
  Colinears: TColliniears;
  NodeIndex: Integer;
  Node1: TNodeInBoundary;
  Node2: TNodeInBoundary;
  Node3: TNodeInBoundary;
  ColinIndex: Integer;
  FirstIndex: Integer;
  ColinCount: Integer;
  IntList: TIntegerList;
  NodeIndex2: Integer;
  AddedLast: boolean;
  BoundNode1: TNodeInBoundary;
  BoundNode2: TNodeInBoundary;
  BoundNode3: TNodeInBoundary;
  AnOrientation: Integer;
  Delta: Integer;
    {$IFDEF TEST}
  SubIndex: Integer;
  {$ENDIF}
begin
  Assert(Count = 7);
  result := False;
  for NodeIndex := 0 to Count - 3 do
  begin
    Node1 := Items[NodeIndex];
    Node2 := Items[NodeIndex + 1];
    Node3 := Items[NodeIndex + 2];
    Colinears[NodeIndex] := NearlyStraightAngle(Node1.FNode.Location, Node2.FNode.Location,
      Node3.FNode.Location);
    result := result or Colinears[NodeIndex]
  end;
  NodeIndex := Count - 2;
  Node1 := Items[NodeIndex];
  Node2 := Items[NodeIndex + 1];
  Node3 := Items[1];
  Colinears[NodeIndex] := NearlyStraightAngle(Node1.FNode.Location,
    Node2.FNode.Location, Node3.FNode.Location);
  result := result or Colinears[NodeIndex];
  if not result then
  begin
    Exit;
  end;
  // Find the position of the first node that is colinear with the two nodes
  // after it.
  FirstIndex := -1;
  for ColinIndex := 0 to Length(Colinears) - 1 do
  begin
    if Colinears[ColinIndex] then
    begin
      FirstIndex := ColinIndex;
      break;
    end;
  end;
  if FirstIndex = 0 then
  begin
    for ColinIndex := Length(Colinears) - 1 downto 0 do
    begin
      if not Colinears[ColinIndex] then
      begin
        FirstIndex := ColinIndex + 1;
        if FirstIndex = Length(Colinears) then
        begin
          FirstIndex := 0;
        end;
        break;
      end;
    end;
  end;

  ColinCount := 1;
  AddedLast := False;
  IntList := TIntegerList.Create;
  try
    for ColinIndex := FirstIndex to Length(Colinears) - 1 do
    begin
      if Colinears[ColinIndex] then
      begin
        Inc(ColinCount);
        AddedLast := False;
      end
      else
      begin
        IntList.Add(ColinCount);
        ColinCount := 1;
        AddedLast := True;
      end;
    end;
    for ColinIndex := 0 to FirstIndex - 1 do
    begin
      if Colinears[ColinIndex] then
      begin
        Inc(ColinCount);
        AddedLast := False;
      end
      else
      begin
        IntList.Add(ColinCount);
        ColinCount := 1;
        AddedLast := True;
      end;
    end;
    if not AddedLast then
    begin
      IntList.Add(ColinCount);
    end;
    case IntList.Count of
      1:
        begin
          // all points colinear.
          Split2121(FirstIndex);
        end;
     { 2:
        begin
          // bad geometry
          BoundNode3 := Items[Count - 3];
          BoundNode2 := Items[Count - 2];
          for NodeIndex := 0 to Count - 2 do
          begin
            BoundNode1 := Items[NodeIndex];

            AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
              BoundNode2.FNode.Location, BoundNode1.FNode.Location);
            if AnOrientation = Clockwise then
            begin
              // The polygon is concave so break at the concave point.

              Delta := NodeIndex - FirstIndex;
              if Delta < 0 then
              begin
                Delta := Delta + 6;
              end;

              ColinIndex := NodeIndex-2;
              if ColinIndex < 0 then
              begin
                ColinIndex := ColinIndex + Length(Colinears);
              end;

              if not Colinears[ColinIndex] then
              begin
                FirstIndex := NodeIndex - 2;
                if FirstIndex < 0 then
                begin
                  FirstIndex := FirstIndex + 6;
                end;
                Split2121(FirstIndex);
                Exit
              end;
            end;
            BoundNode3 := BoundNode2;
            BoundNode2 := BoundNode1;
          end;

          Split2121(FirstIndex);
        end;   }
      3:
        begin
          case IntList[0] of
            2:
              begin
                // 2-2-2, 2-1-3, or 2-3-1

                {These and similar comments identify the pattern of colinear
                 sides in the boundary. 2-2-2, for example, means that
                 sides 1 and 2 are colinear, sides 3 and 4 are colinear
                 and sides 5 and 6 are colinear so the boundary has a
                 triangular shape with each of the triangles sides comprising
                 two edges of the boundary as shown below.

                            *
                          /  \
                         /    \
                        *      *
                       /        \
                      /          \
                     *------*-----*
                }
                case IntList[1] of
                  1:
                    begin
                      // 2-1-3
                      Assert(IntList[2] = 3);
                      // Convert to 3-1-2 and process again
                      Reverse;
                      SpecialCase{$IFDEF TEST}(List){$ENDIF};
                      ReverseSubBoundaries;
                    end;
                  2:
                    begin
                      // 2-2-2
                      Assert(IntList[2] = 2);
                      Split222(FirstIndex);
                    end;
                  3:
                    begin
                      // 2-3-1
                      Assert(IntList[2] = 1);
                      // Convert to 3-1-2 and process again
                      Delete(0);
                      for NodeIndex2 := 0 to FirstIndex - 1 do
                      begin
                        Add(Items[0]);
                        Delete(0);
                      end;
                      Add(Items[0]);
                      Delete(0);
                      Add(Items[0]);
                      Delete(0);
                      Add(Items[0]);
                      SpecialCase{$IFDEF TEST}(List){$ENDIF};
                    end;
                else
                  Assert(False);
                end;
              end;
            3:
              begin
                // 3-1-2 or 3-2-1
                case IntList[1] of
                  1:
                    begin
                      // 3-1-2
                      Assert(IntList[2] = 2);

                      if Items[0].FNode = Items[2].FNode then
                      begin
                        Split2121(FirstIndex);
                      end
                      else
                      begin
                        Split312(FirstIndex);
                      end;


                    end;
                  2:
                    begin
                      // 3-2-1
                      Assert(IntList[2] = 1);
                      // convert to 2-3-1 and process again
                      Reverse;
                      SpecialCase{$IFDEF TEST}(List){$ENDIF};
                      ReverseSubBoundaries;
                    end;
                else
                  Assert(False);
                end;
              end;
            4:
              begin
                // 4-1-1
                Assert(IntList[1] = 1);
                Assert(IntList[2] = 1);
                Split411(FirstIndex{$IFDEF TEST}, List{$ENDIF});
              end;
          else
            Assert(False);
          end;
        end;
      4:
        begin
          case IntList[0] of
            2:
              begin
                // 2-2-1-1, 2-1-1-2, 2-1-2-1
                case IntList[2] of
                  1:
                    begin
                      // 2-2-1-1, 2-1-1-2
                      case IntList[1] of
                        1:
                          begin
                            // 2-1-1-2
                            Assert(IntList[3] = 2);

                            BoundNode3 := Items[Count - 3];
                            BoundNode2 := Items[Count - 2];
                            for NodeIndex := 0 to Count - 2 do
                            begin
                              BoundNode1 := Items[NodeIndex];

                              AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
                                BoundNode2.FNode.Location, BoundNode1.FNode.Location);
                              if AnOrientation = Clockwise then
                              begin
                                // The polygon is concave so break at the concave point.

                                Delta := NodeIndex - FirstIndex;
                                if Delta < 0 then
                                begin
                                  Delta := Delta + 6;
                                end;

                                if Delta in [3,4,5] then
                                begin
                                  FirstIndex := NodeIndex - 2;
                                  if FirstIndex < 0 then
                                  begin
                                    FirstIndex := FirstIndex + 6;
                                  end;
                                  Split2121(FirstIndex);
                                  Exit
                                end;
                              end;
                              BoundNode3 := BoundNode2;
                              BoundNode2 := BoundNode1;
                            end;

                            Split222(FirstIndex);
                          end;
                        2:
                          begin
                            // 2-2-1-1
                            Assert(IntList[3] = 1);

                            BoundNode3 := Items[Count - 3];
                            BoundNode2 := Items[Count - 2];
                            for NodeIndex := 0 to Count - 2 do
                            begin
                              BoundNode1 := Items[NodeIndex];

                              AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
                                BoundNode2.FNode.Location, BoundNode1.FNode.Location);
                              if (AnOrientation = Clockwise) and not NearlyStraightAngle(
                                BoundNode3.FNode.Location, BoundNode2.FNode.Location,
                                BoundNode1.FNode.Location) then
                              begin
                                // The polygon is concave so break at the concave point.

                                Delta := NodeIndex - FirstIndex;
                                if Delta < 0 then
                                begin
                                  Delta := Delta + 6;
                                end;

                                if Delta in [0,4,5] then
                                begin
                                  FirstIndex := NodeIndex - 2;
                                  if FirstIndex < 0 then
                                  begin
                                    FirstIndex := FirstIndex + 6;
                                  end;
                                  Split2121(FirstIndex);
                                  Exit
                                end;
                              end;
                              BoundNode3 := BoundNode2;
                              BoundNode2 := BoundNode1;
                            end;

                            Split222(FirstIndex);
                          end;
                      else
                        Assert(False);
                      end;
                    end;
                  2:
                    begin
                      // 2-1-2-1
                      Assert(IntList[1] = 1);
                      Assert(IntList[2] = 2);
                      Assert(IntList[3] = 1);

                      BoundNode3 := Items[Count - 3];
                      BoundNode2 := Items[Count - 2];
                      for NodeIndex := 0 to Count - 2 do
                      begin
                        BoundNode1 := Items[NodeIndex];

                        AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
                          BoundNode2.FNode.Location, BoundNode1.FNode.Location);
                        if (AnOrientation = Clockwise) and not NearlyStraightAngle(
                          BoundNode3.FNode.Location, BoundNode2.FNode.Location,
                          BoundNode1.FNode.Location) then
                        begin
                          // The polygon is concave so break at the concave point.

                          ColinIndex := NodeIndex-2;
                          if ColinIndex < 0 then
                          begin
                            ColinIndex := ColinIndex + Length(Colinears);
                          end;

                          if not Colinears[ColinIndex] then
                          begin
                            FirstIndex := NodeIndex - 2;
                            if FirstIndex < 0 then
                            begin
                              FirstIndex := FirstIndex + 6;
                            end;
                            Split2121(FirstIndex);
                            Exit
                          end;
                        end;
                        BoundNode3 := BoundNode2;
                        BoundNode2 := BoundNode1;
                      end;

                      Split2121(FirstIndex);
                    end;
                else
                  Assert(False);
                end;
              end;
            3:
              begin
                // 3-1-1-1
                Assert(IntList[1] = 1);
                Assert(IntList[2] = 1);
                Assert(IntList[3] = 1);

                BoundNode3 := Items[Count - 3];
                BoundNode2 := Items[Count - 2];
                for NodeIndex := 0 to Count - 2 do
                begin
                  BoundNode1 := Items[NodeIndex];

                  AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
                    BoundNode2.FNode.Location, BoundNode1.FNode.Location);
                  if (AnOrientation = Clockwise)  and not NearlyStraightAngle(
                    BoundNode3.FNode.Location, BoundNode2.FNode.Location,
                    BoundNode1.FNode.Location) then
                  begin
                    // The polygon is concave so break at the concave point.

                    Delta := NodeIndex - FirstIndex;
                    if Delta < 0 then
                    begin
                      Delta := Delta + 6;
                    end;

                    if Delta in [0,3,4,5] then
                    begin
                      FirstIndex := NodeIndex - 2;
                      if FirstIndex < 0 then
                      begin
                        FirstIndex := FirstIndex + 6;
                      end;
                      Split2121(FirstIndex);
                      Exit
                    end;
                  end;
                  BoundNode3 := BoundNode2;
                  BoundNode2 := BoundNode1;
                end;

                Split312(FirstIndex);
              end;
          else
            Assert(False);
          end;
        end;
      5:
        begin
          // 2-1-1-1-1
          Assert(IntList[0] = 2);
          Assert(IntList[1] = 1);
          Assert(IntList[2] = 1);
          Assert(IntList[3] = 1);
          Assert(IntList[4] = 1);

          BoundNode3 := Items[Count - 3];
          BoundNode2 := Items[Count - 2];
          for NodeIndex := 0 to Count - 2 do
          begin
            BoundNode1 := Items[NodeIndex];

            AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
              BoundNode2.FNode.Location, BoundNode1.FNode.Location);
            if (AnOrientation = Clockwise) and not NearlyStraightAngle(
              BoundNode3.FNode.Location, BoundNode2.FNode.Location,
              BoundNode1.FNode.Location) then
            begin
              // The polygon is concave so break at the concave point.

              Delta := NodeIndex - FirstIndex;
              if Delta < 0 then
              begin
                Delta := Delta + 6;
              end;

              if Delta in [0,1,3,4,5] then
              begin
                FirstIndex := NodeIndex - 2;
                if FirstIndex < 0 then
                begin
                  FirstIndex := FirstIndex + 6;
                end;
                break;
              end;
            end;
            BoundNode3 := BoundNode2;
            BoundNode2 := BoundNode1;
          end;

          Split2121(FirstIndex);
        end;
    else
      Assert(False);
    end;
  finally
    IntList.Free;
    {$IFDEF TEST}
      for SubIndex := 0 to FSubParts.Count - 1 do
      begin
        FSubParts[SubIndex].ListCount := List.Count;
      end;
    {$ENDIF}
  end;

  Clear;
  FSegments.Clear;
end;

function TBoundary.Orientation: Integer;
var
  InputPoints: TPolygon2D;
  OutputPoints: TPolygon2D;
  NodeIndex: Integer;
  ANode: TNodeInBoundary;
begin
  Assert(Count > 0);
  SetLength(InputPoints, Count - 1);
  for NodeIndex := 0 to Count - 2 do
  begin
    ANode := Items[NodeIndex];
    InputPoints[NodeIndex] := ANode.FNode.Location;
  end;
  ConvexHull2(InputPoints, result, OutputPoints);
end;

{$IFDEF DEBUG}
function TBoundary.NodeLocations: string;
var
  AStringList: TStringBuilder;
  NodeIndex: Integer;
  ANode: TNode;
begin
  AStringList := TStringBuilder.Create;
  try
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      AStringList.Append(ANode.Location.x);
      AStringList.Append(#9);
      AStringList.Append(ANode.Location.y);
      AStringList.Append(#13#10);
    end;
    result := AStringList.ToString
  finally
    AStringList.Free;
  end;
end;
{$ENDIF}

function TBoundary.NodeOrientation: Integer;
var
  InputPoints: TPolygon2D;
  OutputPoints: TPolygon2D;
  NodeIndex: Integer;
  ANode: TNode;
begin
  Assert(FNodes.Count > 0);
  SetLength(InputPoints, FNodes.Count - 1);
  for NodeIndex := 0 to FNodes.Count - 2 do
  begin
    ANode := FNodes[NodeIndex];
    InputPoints[NodeIndex] := ANode.Location;
  end;
  ConvexHull2(InputPoints, result, OutputPoints);
end;

function TBoundary.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

procedure TBoundary.Split(List: TBoundaryList);
const
  TooManyPoints = 100;
var
  OuterNodeIndex: Integer;
  Node1: TNodeInBoundary;
  InnerNodeIndex: Integer;
  Node2: TNodeInBoundary;
  ACost: TCost;
  LowestCost: TCost;
  SubBoundary: TBoundary;
  NodeIndex: Integer;
  NewNode: TNodeInBoundary;
  SegIndex: Integer;
  ASeg: TSegment;
  PriorSegment: TSegment;
  SegmentInsertPosition1: Integer;
  SegmentInsertPosition2: Integer;
  NewSegment: TSegment;
  ReversedSegment: TSegment;
  SegmentBetweenCount: Integer;
  NodesToInsert: Integer;
  SegmentIndex: Integer;
  ASegment: TSegment;
  AnInnerNode: TNode;
  SubBoundaryIndex: Integer;
  FirstNode: TNodeInBoundary;
  ShouldSkip: boolean;
  ASubPart: TBoundary;
  HiddenSegments: TSegmentList;
  OuterSegIndex: Integer;
  OuterSegment: TSegment;
  InnerSegIndex: Integer;
  Seg1: TSegment2D;
  InnerSegment: TSegment;
//  StartingIndex: Integer;
//  EndingIndex: Integer;
  TempCost: TCost;
//  SplitSegment1: TSegment;
//  SplitSegment2: TSegment;
//  SplitIndex: Integer;
//  SplitNode1: TNodeInBoundary;
//  SplitNode2: TNodeInBoundary;
begin
  SetCounterClockwiseOrientation;

  if Count = 5 then
  begin
    Exit;
  end;
  Assert(Odd(Count));
  Assert(Count >= 7);
  if (Count = 7) and (FQuadMeshCreator.SixNodeClosureMethod = cmTemplate) then
  begin
    if SpecialCase{$IFDEF TEST}(List){$ENDIF} then
    begin
      Exit;
    end;
  end;
  SetSegmentPositions;

  HiddenSegments:= TSegmentList.Create;
  LowestCost := nil;
  try
    // This doesn't appear to improve overall mesh quality.
    {
    if (FSegments.Count = 4)
      and (FSegments[0].FInnerNodes.Count = FSegments[2].FInnerNodes.Count)
      and (FSegments[1].FInnerNodes.Count = FSegments[3].FInnerNodes.Count)
      then
    begin
      if (OrientedVertexAngle(FSegments[0].FNode1.Location,
        FSegments[0].FNode2.Location, FSegments[1].FNode2.Location,
        CounterClockwise) < 120)
        and
        (OrientedVertexAngle(FSegments[1].FNode1.Location,
        FSegments[1].FNode2.Location, FSegments[2].FNode2.Location,
        CounterClockwise) < 120)
        and
        (OrientedVertexAngle(FSegments[2].FNode1.Location,
        FSegments[2].FNode2.Location, FSegments[3].FNode2.Location,
        CounterClockwise) < 120)
        and
        (OrientedVertexAngle(FSegments[3].FNode1.Location,
        FSegments[3].FNode2.Location, FSegments[0].FNode2.Location,
        CounterClockwise) < 120)
        then
      begin
        if FSegments[0].FInnerNodes.Count > FSegments[1].FInnerNodes.Count then
        begin
          SplitSegment1 := FSegments[0];
          SplitSegment2 := FSegments[2];
        end
        else
        begin
          SplitSegment1 := FSegments[1];
          SplitSegment2 := FSegments[3];
        end;
        SplitIndex := SplitSegment1.FInnerNodes.Count div 2;
        SplitNode1 := Items[SplitSegment1.FPosition1 + SplitIndex+1];
        SplitNode2 := Items[SplitSegment2.FPosition2 - SplitIndex-1];
        LowestCost := TCost.Create(SplitNode1, SplitNode2, FQuadMeshCreator);
        Assert(LowestCost.Visible);
      end;
    end;
    if LowestCost = nil then
    }
    begin
      // Go to Count-2 instead of Count-1 because
      // the last point is a duplicate of the first point
      for OuterNodeIndex := 0 to Count-2 do
      begin
        Node1 := Items[OuterNodeIndex];
        HiddenSegments.Clear;
        if (Count > FSegments.Count*2)
          and (FSegments.Count > 3) then
        begin
          for OuterSegIndex := 0 to FSegments.Count - 1 do
          begin
            OuterSegment := FSegments[OuterSegIndex];
            if (OuterSegment.FNode1 = Node1.FNode)
              or (OuterSegment.FNode2 = Node1.FNode)
              or (Node1.IndexOfSegment(OuterSegment) >= 0) then
            begin
              Continue;
            end;
            if HiddenSegments.IndexOf(OuterSegment) >= 0 then
            begin
              Continue;
            end;
            Seg1 := EquateSegment(OuterSegment.FNode1.Location,
              OuterSegment.FNode2.Location);
            for InnerSegIndex := 0 to FSegments.Count - 1 do
            begin
              if Abs(OuterSegIndex-InnerSegIndex) <= 1 then
              begin
                Continue;
              end;
              InnerSegment := FSegments[InnerSegIndex];
              if (OuterSegment.FNode1 = InnerSegment.FNode1)
                or (OuterSegment.FNode1 = InnerSegment.FNode2)
                or (OuterSegment.FNode2 = InnerSegment.FNode1)
                or (OuterSegment.FNode2 = InnerSegment.FNode2)
                then
              begin
                Continue;
              end;
              if (InnerSegment.FNode1 = Node1.FNode)
                or (InnerSegment.FNode2 = Node1.FNode)
                or (Node1.IndexOfSegment(InnerSegment) >= 0) then
              begin
                Continue;
              end;
              if HiddenSegments.IndexOf(InnerSegment) >= 0 then
              begin
                Continue;
              end;
              if Intersect(Seg1, EquateSegment(Node1.Location,
                InnerSegment.FNode1.Location))
                and Intersect(Seg1, EquateSegment(Node1.Location,
                InnerSegment.FNode2.Location)) then
              begin
                HiddenSegments.Add(InnerSegment);
              end;
            end;
          end;
        end;

        for InnerNodeIndex := OuterNodeIndex + 1 to Count-1 do
        begin

          // skip neighboring points
          if Abs(OuterNodeIndex - InnerNodeIndex) <= 1 then
          begin
            Continue;
          end;
          if (OuterNodeIndex = 0) and (InnerNodeIndex >= Count - 2) then
          begin
            Continue;
          end;
          if (OuterNodeIndex = 1) and (InnerNodeIndex = Count - 1) then
          begin
            Continue;
          end;
          Node2 := Items[InnerNodeIndex];
          if Node1.OnSameSegment(Node2) then
          begin
            Continue;
          end;
          if Node1.FNode = Node2.FNode then
          begin
            Continue;
          end;
          if HiddenSegments.IndexOf(Node2.Segments[0]) >= 0 then
          begin
            Continue;
          end;
          if (Node2.SegmentCount > 1)
            and (HiddenSegments.IndexOf(Node2.Segments[1]) >= 0) then
          begin
            Continue;
          end;


          ACost := TCost.Create(Node1, Node2, FQuadMeshCreator{, ivUnknown});
          try
          if ACost.Visible then
          begin
            ACost.ComputeCost(LowestCost);
            if (LowestCost = nil) or (ACost.Cost < LowestCost.Cost) then
            begin
              TempCost := LowestCost;
              LowestCost := ACost;
              ACost := TempCost;

              if LowestCost.Cost = 0  then
              begin
                break;
              end;
            end;
          end;
          finally
            ACost.Free;
          end;
        end;
      end;
    end;
    Assert(LowestCost <> nil);

    if LowestCost.FNode1.Position < LowestCost.FNode2.Position then
    begin
      Node1 := LowestCost.FNode1;
      Node2 := LowestCost.FNode2;
    end
    else
    begin
      Node1 := LowestCost.FNode2;
      Node2 := LowestCost.FNode1;
    end;

    SplitSegmentAtNode(Node1);
    SplitSegmentAtNode(Node2);

    PriorSegment := nil;
    for SegIndex := 0 to Node1.SegmentCount - 1 do
    begin
      ASeg := Node1.Segments[SegIndex];
      if ASeg.Node2 = Node1.FNode then
      begin
        PriorSegment := ASeg;
        Assert(PriorSegment.FBoundary = self);
        break;
      end;
    end;
    Assert(PriorSegment <> nil);
    ShouldSkip := False;
    SegmentInsertPosition1 := FSegments.IndexOf(PriorSegment) + 1;
    if SegmentInsertPosition1 = FSegments.Count then
    begin
      SegmentInsertPosition1 := 0;
      ShouldSkip := True;
    end;

    PriorSegment := nil;
    for SegIndex := 0 to Node2.SegmentCount - 1 do
    begin
      ASeg := Node2.Segments[SegIndex];
      if ASeg.Node1 = Node2.FNode then
      begin
        PriorSegment := ASeg;
        Assert(PriorSegment.FBoundary = self);
        break;
      end;
    end;
    Assert(PriorSegment <> nil);
    SegmentInsertPosition2 := FSegments.IndexOf(PriorSegment);
    Assert(SegmentInsertPosition2 >= 0);

    NewSegment := nil;
    ReversedSegment := nil;
    if Node1.FNode <> Node2.FNode then
    begin
      NewSegment := TSegment.Create(Node1.FNode, Node2.FNode, stInner, self,
        FQuadMeshCreator);

      SegmentBetweenCount := Node2.Position - Node1.Position;
      NodesToInsert := NewSegment.NodesToInsert;
      if not Odd(SegmentBetweenCount + NodesToInsert) then
      begin
        Inc(NodesToInsert);
      end;
      if NodesToInsert > 0 then
      begin
        NewSegment.InsertNodes(NodesToInsert);
      end;

      ReversedSegment := NewSegment.CreateReversedSegment;
    end;

    SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
      (Node1.DesiredSpacing + Node2.DesiredSpacing) / 2);
    FSubParts.Add(SubBoundary);

    PriorSegment := nil;
    FirstNode := nil;

    if not ShouldSkip then
    begin
      for SegmentIndex := 0 to SegmentInsertPosition1 - 1 do
      begin
        ASegment := FSegments[SegmentIndex];
        NewNode := TNodeInBoundary.Create(ASegment.Node1, SubBoundary, ASegment);
        if FirstNode = nil then
        begin
          FirstNode := NewNode;
        end;
        SubBoundary.Add(NewNode);
        if PriorSegment <> nil then
        begin
          NewNode.AddSegment(PriorSegment);
        end;
        SubBoundary.FSegments.Add(ASegment);
        ASegment.FBoundary := SubBoundary;
        for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
        begin
          AnInnerNode := ASegment.FInnerNodes[NodeIndex];
          NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, ASegment);
          SubBoundary.Add(NewNode);
        end;
        PriorSegment := ASegment;
      end;
    end;

    if NewSegment <> nil then
    begin
      NewNode := TNodeInBoundary.Create(NewSegment.Node1, SubBoundary, NewSegment);
      if FirstNode = nil then
      begin
        FirstNode := NewNode;
      end;
      SubBoundary.Add(NewNode);
      if PriorSegment <> nil then
      begin
        NewNode.AddSegment(PriorSegment);
      end;
      SubBoundary.FSegments.Add(NewSegment);
      NewSegment.FBoundary := SubBoundary;
      for NodeIndex := 0 to NewSegment.FInnerNodes.Count - 1 do
      begin
        AnInnerNode := NewSegment.FInnerNodes[NodeIndex];
        NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, NewSegment);
        SubBoundary.Add(NewNode);
      end;
      PriorSegment := NewSegment;
    end;

    for SegmentIndex := SegmentInsertPosition2 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      NewNode := TNodeInBoundary.Create(ASegment.Node1, SubBoundary, ASegment);
      SubBoundary.Add(NewNode);
      if PriorSegment <> nil then
      begin
        NewNode.AddSegment(PriorSegment);
      end;
      SubBoundary.FSegments.Add(ASegment);
      ASegment.FBoundary := SubBoundary;
      for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
      begin
        AnInnerNode := ASegment.FInnerNodes[NodeIndex];
        NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, ASegment);
        SubBoundary.Add(NewNode);
      end;
      PriorSegment := ASegment;
    end;
    SubBoundary.Add(SubBoundary.Items[0]);

    if FirstNode <> nil then
    begin
      FirstNode.AddSegment(PriorSegment);
    end;

    SubBoundary.RenumberNodes;

    SubBoundary := TBoundary.Create(FQuadMeshCreator, self,
      (Node1.DesiredSpacing + Node2.DesiredSpacing) / 2);
    FSubParts.Add(SubBoundary);

    FirstNode := nil;
    PriorSegment := nil;
    if ReversedSegment <> nil then
    begin
      NewNode := TNodeInBoundary.Create(ReversedSegment.Node1, SubBoundary,
        ReversedSegment);
      FirstNode := NewNode;
      SubBoundary.Add(NewNode);
      SubBoundary.FSegments.Add(ReversedSegment);
      ReversedSegment.FBoundary := SubBoundary;
      PriorSegment := ReversedSegment;
      for NodeIndex := 0 to ReversedSegment.FInnerNodes.Count - 1 do
      begin
        AnInnerNode := ReversedSegment.FInnerNodes[NodeIndex];
        NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary,
          ReversedSegment);
        SubBoundary.Add(NewNode);
      end;
    end;

    for SegmentIndex := SegmentInsertPosition1 to SegmentInsertPosition2 - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      NewNode := TNodeInBoundary.Create(ASegment.Node1, SubBoundary, ASegment);
      if PriorSegment <> nil then
      begin
        NewNode.AddSegment(PriorSegment);
      end;
      SubBoundary.Add(NewNode);
      SubBoundary.FSegments.Add(ASegment);
      ASegment.FBoundary := SubBoundary;
      for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
      begin
        AnInnerNode := ASegment.FInnerNodes[NodeIndex];
        NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, ASegment);
        SubBoundary.Add(NewNode);
      end;
      PriorSegment := ASegment;
    end;
    SubBoundary.Add(SubBoundary.Items[0]);
    if FirstNode <> nil then
    begin
      FirstNode.AddSegment(PriorSegment);
    end;
    SubBoundary.RenumberNodes;
    FSegments.OwnsObjects := False;
    RemoveSelfFromOwnNodes;

    Clear;
    FSegments.Clear;

    for SubBoundaryIndex := 0 to FSubParts.Count - 1 do
    begin
      ASubPart := FSubParts[SubBoundaryIndex];
      Assert(Odd(ASubPart.Count));

      List.Add(ASubPart);
      // Sorting the elements doesn't appear to help much if at all.
//      FirstPosition := ListPosition + 1;
//      LastPosition := List.Count-1;
//      if FirstPosition > LastPosition then
//      begin
//        List.Add(ASubPart);
//      end
//      else if List[FirstPosition].Count < ASubPart.Count then
//      begin
//        List.Insert(FirstPosition, ASubPart);
//      end
//      else if List[LastPosition].Count > ASubPart.Count then
//      begin
//        List.Add(ASubPart);
//      end
//      else
//      begin
//        while LastPosition - FirstPosition > 1 do
//        begin
//          MidPosition := (LastPosition + FirstPosition) div 2;
//          if List[MidPosition].Count < ASubPart.Count then
//          begin
//            LastPosition := MidPosition;
//          end
//          else
//          begin
//            FirstPosition := MidPosition;
//          end;
//        end;
//        if List[FirstPosition].Count < ASubPart.Count then
//        begin
//          List.Insert(FirstPosition, ASubPart);
//        end
//        else
//        begin
//          List.Insert(LastPosition, ASubPart);
//        end;
//      end;
      {$IFDEF TEST}
      ASubPart.ListCount := List.Count;
      {$ENDIF}
    end;
  finally
    HiddenSegments.Free;
    LowestCost.Free;
  end;
end;

procedure TBoundary.SplitSegmentAtNode(ANode: TNodeInBoundary);
var
  ASegment: TSegment;
  ASegmentList: TSegmentList;
  SegmentPosition: Integer;
  SegmentIndex: Integer;
  SubSegment: TSegment;
  NodeIndex: Integer;
  AnotherNode: TNodeInBoundary;
  SubSeg1: TSegment;
  SubSeg2: TSegment;
  Seg1: TSegment;
  Seg2: TSegment;
begin
  if ANode.SegmentCount = 1 then
  begin
    ASegment := ANode.Segments[0];
    ANode.DeleteSegment(0);
    ASegmentList := ASegment.Split(ANode.FNode);
    try
      Assert(ASegmentList.Count = 2);
      SegmentPosition := FSegments.IndexOf(ASegment);
      FSegments.Delete(SegmentPosition);

      for SegmentIndex := 0 to ASegmentList.Count - 1 do
      begin
        SubSegment := ASegmentList[SegmentIndex];
        FSegments.Insert(SegmentPosition + SegmentIndex, SubSegment);
        ANode.AddSegment(SubSegment);
      end;

      SubSeg1 := ASegmentList[0];
      SubSeg2 := ASegmentList[1];
      Assert(SubSeg1.Node2 = SubSeg2.Node1);
    finally
      ASegmentList.Free;
    end;

    for NodeIndex := ANode.Position - 1 downto 0 do
    begin
      AnotherNode := Items[NodeIndex];
      SegmentPosition := AnotherNode.IndexOfSegment(ASegment);
      if SegmentPosition >= 0 then
      begin
        AnotherNode.DeleteSegment(SegmentPosition);
        AnotherNode.AddSegment(SubSeg1);
      end
      else
      begin
        break;
      end;
    end;

    for NodeIndex := ANode.Position + 1 to Count - 1 do
    begin
      AnotherNode := Items[NodeIndex];
      SegmentPosition := AnotherNode.IndexOfSegment(ASegment);
      if SegmentPosition >= 0 then
      begin
        AnotherNode.DeleteSegment(SegmentPosition);
        AnotherNode.AddSegment(SubSeg2);
      end
      else
      begin
        break;
      end;
    end;
  end;
  Assert(ANode.SegmentCount = 2);
  Seg1 := ANode.Segments[0];
  Seg2 := ANode.Segments[1];
  if Seg1.Node1 = ANode.FNode then
  begin
    Assert(Seg2.Node2 = ANode.FNode);
    ANode.ReverseSegments;
  end
  else
  begin
    Assert(Seg1.Node2 = ANode.FNode);
    Assert(Seg2.Node1 = ANode.FNode);
  end;
end;

function TBoundary._AddRef: Integer;
begin
  Inc(FRefCount);
  result := FRefCount;
end;

function TBoundary._Release: Integer;
begin
  Dec(FRefCount);
  result := FRefCount;
  if result = 0 then
    Destroy;
end;

{ TQuadMeshCreator }

function TQuadMeshCreator.AddBoundary(DesiredSpacing: double): TBoundary;
begin
  result := TBoundary.Create(self, nil, DesiredSpacing);
  FBoundaries.Add(result);
end;

procedure TQuadMeshCreator.GetConnectedOpenBoundaries(
  OpenBoundaryList: TBoundaryList; BlindEndsBoundaryList: TBoundaryList);
var
  ABoundary: TBoundary;
  BoundaryIndex: Integer;
begin
  OpenBoundaryList.Clear;
  BlindEndsBoundaryList.Clear;
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    if (ABoundary.FNodes.Count > 1)
      and (ABoundary.FNodes[0] <> ABoundary.FNodes[ABoundary.FNodes.Count - 1])
      then
    begin
      if ABoundary.FNodes[0].FIntersection
        and ABoundary.FNodes[ABoundary.FNodes.Count - 1].FIntersection then
      begin
        OpenBoundaryList.Add(ABoundary);
      end
      else if ABoundary.FNodes[0].FIntersection
        or ABoundary.FNodes[ABoundary.FNodes.Count - 1].FIntersection then
      begin
        BlindEndsBoundaryList.Add(ABoundary);
      end;
    end;
  end;
end;

function TQuadMeshCreator.FixEdgeTriangles: boolean;
var
  NodeIndex: Integer;
  ANode: TNode;
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  NodePosition: integer;
  NextNode: TNode;
  PriorNode: TNode;
  OppositeNode: TNode;
  OppositePosition: Integer;
  PriorSharedBoundary: TBoundary;
  NextSharedBoundary: TBoundary;
  NewLocation: TPoint2D;
  NewDesiredSpacing: Double;
  NewNode: TNode;
//  OppositeElement: TBoundary;
  procedure FixConstrainedTriangle;
  var
    UnconstrainedNeighbor: TNode;
    BoundaryIndex: integer;
    TestBoundary: TBoundary;
    InsertPosition: integer;
    CopyPosition: integer;
    NodePosition: Integer;
    CopyNodeIB: TNodeInBoundary;
    SegmentIndex: Integer;
    ASegment: TSegment;
    OppositeElement: TBoundary;
    UnConIndex: Integer;
    TempBoundary: TBoundary;
    OppositeIndex: Integer;
  begin
//    Beep;
//    Exit;
    // All the nodes are constrained nodes. Find a pair of nodes that are
    // not constrained to be neighbors and find the element on the opposite
    // side of the edge defined by those two nodes. Merge the elements
    // and then split into 3 elements.
    UnconstrainedNeighbor := nil;
    if OppositeNode.FConstraintNeighbors.IndexOf(PriorNode) < 0 then
    begin
      UnconstrainedNeighbor := PriorNode;
      Assert(OppositeNode.FConstraintNeighbors.IndexOf(NextNode) >= 0);
    end
    else if OppositeNode.FConstraintNeighbors.IndexOf(NextNode) < 0 then
    begin
      UnconstrainedNeighbor := NextNode;
    end;
    if UnconstrainedNeighbor <> nil then
    begin
      OppositeElement := nil;
      for BoundaryIndex := 0 to UnconstrainedNeighbor.FElements.Count - 1 do
      begin
        TestBoundary := UnconstrainedNeighbor.FElements[BoundaryIndex];
        if (TestBoundary = ABoundary) then
        begin
          Continue;
        end;
        if OppositeNode.FElements.IndexOf(TestBoundary) >= 0 then
        begin
          OppositeElement := TestBoundary;
          break;
        end;
      end;
      if OppositeElement <> nil then
      begin
        ABoundary.SetCounterClockwiseOrientation;
        OppositeElement.SetCounterClockwiseOrientation;

        ABoundary.FixSegments;
        OppositeElement.FixSegments;

        // make sure neither OppositeNode nor UnconstrainedNeighbor
        // are in the middle of segment
        NodePosition := ABoundary.IndexOfNode(OppositeNode);
        Assert(NodePosition >= 0);
        ABoundary.SplitSegmentAtNode(ABoundary[NodePosition]);

        NodePosition := ABoundary.IndexOfNode(UnconstrainedNeighbor);
        Assert(NodePosition >= 0);
        ABoundary.SplitSegmentAtNode(ABoundary[NodePosition]);

        NodePosition := OppositeElement.IndexOfNode(OppositeNode);
        Assert(NodePosition >= 0);
        OppositeElement.SplitSegmentAtNode(OppositeElement[NodePosition]);

        NodePosition := OppositeElement.IndexOfNode(UnconstrainedNeighbor);
        Assert(NodePosition >= 0);
        OppositeElement.SplitSegmentAtNode(OppositeElement[NodePosition]);

        ABoundary.Delete(ABoundary.Count-1);
        OppositeElement.Delete(OppositeElement.Count-1);

        OppositeIndex := OppositeElement.IndexOfNode(OppositeNode);
        UnConIndex := OppositeElement.IndexOfNode(UnconstrainedNeighbor);
        Inc(UnConIndex);
        if OppositeIndex >= OppositeElement.Count then
        begin
          OppositeIndex := 0;
        end;

        if OppositeIndex = UnConIndex then
        begin
          TempBoundary := OppositeElement;
          OppositeElement := ABoundary;
          ABoundary := TempBoundary;
        end;

        InsertPosition := ABoundary.IndexOfNode(UnconstrainedNeighbor);
        Assert(InsertPosition >= 0);

        CopyPosition := OppositeElement.IndexOfNode(UnconstrainedNeighbor);
        Assert(CopyPosition >= 0);

        Inc(InsertPosition);
        Inc(CopyPosition);
        if CopyPosition >= OppositeElement.Count then
        begin
          CopyPosition := 0;
        end;
        CopyNodeIB := OppositeElement[CopyPosition];
        Assert(CopyNodeIB.FNode <> OppositeNode);
        Assert(CopyNodeIB.FNode <> UnconstrainedNeighbor);
        ABoundary.Insert(InsertPosition, CopyNodeIB);
        CopyNodeIB.FNode.FElements.Remove(OppositeElement);
        CopyNodeIB.FNode.FElements.Add(ABoundary);
        CopyNodeIB.FBoundary := ABoundary;
        CopyNodeIB.FPosition := InsertPosition;

        Inc(InsertPosition);
        Inc(CopyPosition);
        if CopyPosition >= OppositeElement.Count then
        begin
          CopyPosition := 0;
        end;
        CopyNodeIB := OppositeElement[CopyPosition];
        Assert(CopyNodeIB.FNode <> OppositeNode);
        Assert(CopyNodeIB.FNode <> UnconstrainedNeighbor);
        ABoundary.Insert(InsertPosition, CopyNodeIB);
        CopyNodeIB.FNode.FElements.Remove(OppositeElement);
        CopyNodeIB.FNode.FElements.Add(ABoundary);
        CopyNodeIB.FBoundary := ABoundary;
        CopyNodeIB.FPosition := InsertPosition;

        InsertPosition := -1;
        for SegmentIndex := 0 to ABoundary.FSegments.Count - 1 do
        begin
          ASegment := ABoundary.FSegments[SegmentIndex];
          // shouldn't need to test for the segment being oriented the opposite way.
          if (ASegment.FNode1 = UnconstrainedNeighbor)
            and (ASegment.FNode2 = OppositeNode) then
          begin
            InsertPosition := SegmentIndex;
            ABoundary.FSegments.Delete(SegmentIndex);
            Break;
          end;
        end;
        Assert(InsertPosition >= 0);


        for SegmentIndex := 0 to OppositeElement.FSegments.Count - 1 do
        begin
          ASegment := OppositeElement.FSegments[SegmentIndex];
          // shouldn't need to test for the segment being oriented the opposite way.
          if (ASegment.FNode1 = OppositeNode)
            and (ASegment.FNode2 = UnconstrainedNeighbor) then
          begin
//          delete this segment and insert the others.
            OppositeElement.FSegments.Delete(SegmentIndex);
            CopyPosition := SegmentIndex;
            break;
          end;
        end;
        Assert(CopyPosition >= 0);

        for SegmentIndex := CopyPosition to OppositeElement.FSegments.Count - 1 do
        begin
          ASegment := OppositeElement.FSegments[SegmentIndex];
          ASegment.FBoundary := ABoundary;
          ABoundary.FSegments.Insert(InsertPosition, ASegment);
          Inc(InsertPosition);
        end;

        for SegmentIndex := 0 to CopyPosition - 1 do
        begin
          ASegment := OppositeElement.FSegments[SegmentIndex];
          ASegment.FBoundary := ABoundary;
          ABoundary.FSegments.Insert(InsertPosition, ASegment);
          Inc(InsertPosition);
        end;
        OppositeElement.FSegments.OwnsObjects := False;

        OppositeElement.RemoveSelfFromOwnNodes;
        OppositeElement.Clear;


        ABoundary.Add(ABoundary[0]);
        Assert(ABoundary.SpecialCase);

      end;
    end;
  end;
  function SharedBoundary(EdgeNode: TNode): TBoundary;
  var
    InnerBoundaryIndex: Integer;
    AnotherBoundary: TBoundary;
  begin
    result := nil;
    for InnerBoundaryIndex := 0 to EdgeNode.FElements.Count - 1 do
    begin
      begin
        AnotherBoundary := EdgeNode.FElements[InnerBoundaryIndex];
        if AnotherBoundary = ABoundary then
        begin
          Continue;
        end;
        if (AnotherBoundary.IndexOfNode(EdgeNode) >= 0)
          and (AnotherBoundary.IndexOfNode(OppositeNode) >= 0)then
        begin
          result := AnotherBoundary;
          Exit;
        end;
      end;
    end;
  end;
  procedure ReconfigureBoundary(ABoundary: TBoundary;
    PreviousNode, NewNode, SubsequentNode: TNode; First: boolean);
  var
    SubsequentNodeIndex: integer;
    NewNodeIndex: integer;
    ReplaceIndex: integer;
    SegmentIndex: Integer;
    ASegment: TSegment;
    NewSegment: TSegment;
    NewBoundaryNode: TNodeInBoundary;
    FirstIndex: Integer;
    PriorNodeIndex: integer;
    NodeIndex: integer;
    SegIndex: Integer;
    OldSegment: TSegment;
  begin
    ABoundary.SetCounterClockwiseOrientation;
    ABoundary.FixSegments;

    ABoundary.Delete(ABoundary.Count-1);

    SubsequentNodeIndex := ABoundary.IndexOfNode(SubsequentNode);
    Assert(SubsequentNodeIndex >= 0);
    ABoundary.SplitSegmentAtNode(ABoundary[SubsequentNodeIndex]);

    ABoundary.Delete(SubsequentNodeIndex);

    PriorNodeIndex := ABoundary.IndexOfNode(PreviousNode);
    Assert(PriorNodeIndex >= 0);
    ABoundary.SplitSegmentAtNode(ABoundary[PriorNodeIndex]);

    NewNodeIndex := PriorNodeIndex + 1;

    ReplaceIndex := -1;
    for SegmentIndex := 0 to ABoundary.FSegments.Count - 1 do
    begin
      ASegment := ABoundary.FSegments[SegmentIndex];
      if (ASegment.FNode2 = SubsequentNode)
        and (ASegment.FNode1 = PreviousNode) then
      begin
        ReplaceIndex := SegmentIndex;
        break;
      end;
      if (ASegment.FNode1 = SubsequentNode)
        and (ASegment.FNode2 = PreviousNode) then
      begin
        ABoundary.FSegments.Reverse;
        if ABoundary.FSegments.Count >= 2 then
        begin
          for SegIndex := 0 to ABoundary.FSegments.Count - 1 do
          begin
            ABoundary.FSegments[SegIndex].Reverse;
          end;
        end;
        ReplaceIndex := ABoundary.FSegments.Count - 1 - SegmentIndex;
        break;
      end;
    end;
    Assert(ReplaceIndex >= 0);

    OldSegment := ABoundary.FSegments[ReplaceIndex];
    for NodeIndex := 0 to ABoundary.Count - 1 do
    begin
      ABoundary[NodeIndex].RemoveSegment(OldSegment);
    end;

    ABoundary.FSegments.Delete(ReplaceIndex);

    if First then
    begin
      NewSegment := TSegment.Create(PreviousNode, ANode, stInner,
        ABoundary, self);
      NewBoundaryNode := TNodeInBoundary.Create(ANode,
        ABoundary, NewSegment);
      ABoundary.FSegments.Insert(ReplaceIndex, NewSegment);
      Inc(ReplaceIndex);
      ABoundary.Insert(NewNodeIndex, NewBoundaryNode);
      Inc(NewNodeIndex);

      NewSegment := TSegment.Create(ANode, NewNode, stInner,
        ABoundary, self);
      NewBoundaryNode := TNodeInBoundary.Create(NewNode,
        ABoundary, NewSegment);
      ABoundary.FSegments.Insert(ReplaceIndex, NewSegment);
      Inc(ReplaceIndex);
      ABoundary.Insert(NewNodeIndex, NewBoundaryNode);
      Inc(NewNodeIndex);

      NewSegment := TSegment.Create(NewNode, SubsequentNode, stInner,
        ABoundary, self);
      NewBoundaryNode := TNodeInBoundary.Create(SubsequentNode,
        ABoundary, NewSegment);
      ABoundary.FSegments.Insert(ReplaceIndex, NewSegment);
      ABoundary.Insert(NewNodeIndex, NewBoundaryNode);
    end
    else
    begin
      NewSegment := TSegment.Create(PreviousNode, NewNode, stInner,
        ABoundary, self);
      NewBoundaryNode := TNodeInBoundary.Create(NewNode,
        ABoundary, NewSegment);
      ABoundary.FSegments.Insert(ReplaceIndex, NewSegment);
      Inc(ReplaceIndex);
      ABoundary.Insert(NewNodeIndex, NewBoundaryNode);
      Inc(NewNodeIndex);

      NewSegment := TSegment.Create(NewNode, ANode, stInner,
        ABoundary, self);
      NewBoundaryNode := TNodeInBoundary.Create(ANode,
        ABoundary, NewSegment);
      ABoundary.FSegments.Insert(ReplaceIndex, NewSegment);
      Inc(ReplaceIndex);
      ABoundary.Insert(NewNodeIndex, NewBoundaryNode);
      Inc(NewNodeIndex);

      NewSegment := TSegment.Create(ANode, SubsequentNode, stInner,
        ABoundary, self);
      NewBoundaryNode := TNodeInBoundary.Create(SubsequentNode,
        ABoundary, NewSegment);
      ABoundary.FSegments.Insert(ReplaceIndex, NewSegment);
      ABoundary.Insert(NewNodeIndex, NewBoundaryNode);
    end;


    ABoundary.Add(ABoundary[0]);

    ABoundary.FixSegments;
    ABoundary.RenumberNodes;

    if First then
    begin
      FirstIndex := ABoundary.IndexOfNode(PreviousNode);
      Assert(FirstIndex>= 0);
      Dec(FirstIndex);
      if FirstIndex < 0 then
      begin
        FirstIndex := 5;
      end;
    end
    else
    begin
      FirstIndex := ABoundary.IndexOfNode(PreviousNode);
    end;
    Assert(FirstIndex>= 0);

    ABoundary.Split222(FirstIndex,
      SegmentMidPoint(PreviousNode.Location, SubsequentNode.Location));
  end;
begin
  result := False;
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[NodeIndex];
    if ANode.NodeType <> ntInner then
    begin
      for BoundaryIndex := 0 to ANode.FElements.Count - 1 do
      begin
        ABoundary := ANode.FElements[BoundaryIndex];
        ABoundary.SetCounterClockwiseOrientation;
        Assert(ABoundary.Count = 5);
        NodePosition := ABoundary.IndexOfNode(ANode);
        Assert(NodePosition >= 0);
        NextNode := ABoundary[NodePosition+1].FNode;
        if NextNode.NodeType = ntInner then
        begin
          Continue;
        end;
        if NodePosition = 0 then
        begin
          NodePosition := ABoundary.Count-1;
        end;
        PriorNode := ABoundary[NodePosition-1].FNode;
        if PriorNode.NodeType = ntInner then
        begin
          Continue;
        end;
        if OrientedVertexAngle(PriorNode.Location, ANode.Location,
          NextNode.Location, CounterClockwise) > 179 then
        begin
          OppositePosition := NodePosition-2;
          if OppositePosition < 0 then
          begin
            OppositePosition := OppositePosition + 4;
          end;
          OppositeNode := ABoundary[OppositePosition].FNode;

          if VertexAngle(PriorNode.Location, ANode.Location, OppositeNode.Location) > 179 then
          begin
            Continue;
          end;
          if VertexAngle(NextNode.Location, ANode.Location, OppositeNode.Location) > 179 then
          begin
            Continue;
          end;

          if OppositeNode.NodeType <> ntInner then
          begin
            FixConstrainedTriangle;
            Continue;
          end;

          PriorSharedBoundary := SharedBoundary(PriorNode);
          NextSharedBoundary := SharedBoundary(NextNode);
          if (PriorSharedBoundary <> nil) and (NextSharedBoundary <> nil) then
          begin
            Assert(PriorSharedBoundary <> NextSharedBoundary);
            result := True;
            Assert(PriorSharedBoundary.Count = 5);
            Assert(NextSharedBoundary.Count = 5);
            NewLocation.X:= (ANode.X + OppositeNode.X)/2;
            NewLocation.Y:= (ANode.Y + OppositeNode.Y)/2;
            NewDesiredSpacing := Min(ANode.FDesiredSpacing,
              OppositeNode.FDesiredSpacing);

            NewNode := TNode.Create(self, NewDesiredSpacing);
            NewNode.Location := NewLocation;

            ReconfigureBoundary(PriorSharedBoundary, PriorNode,
              NewNode, OppositeNode, True);
            ReconfigureBoundary(NextSharedBoundary, OppositeNode,
              NewNode, NextNode, False);

            ABoundary.RemoveSelfFromAllNodes;

            ABoundary.FParent.FSubParts.Remove(ABoundary);

          end;
        end;
      end;
    end;
  end;
end;

procedure TQuadMeshCreator.AdjustNodes;
begin
  case NodeAdjustmentMethod of
    namLagrange:
      AdjustPositionLagrange;
    namGiuliani, namSarrateHuerta:
      AdjustPositionGiuliani;
  else
    Assert(False);
  end;
end;

procedure TQuadMeshCreator.AdjustPositionGiuliani;
var
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    FNodes[NodeIndex].AdjustPositionGiuliani;
  end;
end;

procedure TQuadMeshCreator.AdjustPositionLagrange;
var
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    FNodes[NodeIndex].AdjustPositionLagrange;
  end;
end;

procedure TQuadMeshCreator.FixFinalTriangularElements;
var
  NodeIndex: Integer;
  ANode: TNode;
  ElementIndex: Integer;
  AnElement: TBoundary;
  NodePosition: Integer;
  NextNode: TNode;
  PriorNode: TNode;
  NNodes: TNodeList;
  ClosestNode: TNode;
  NeighborIndex: Integer;
  TestNode: TNode;
  ClosestDistance: double;
  NewLocation: TPoint2D;
  TestDistance: double;
//  OppositeNode: TNode;
//  IntsectPt: TPoint2D;
//  NewLocDist: TFloat;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[NodeIndex];
    if ANode.NodeType <> ntInner then
    begin
      Continue;
    end;
    for ElementIndex := 0 to ANode.FElements.Count - 1 do
    begin
      AnElement := ANode.FElements[ElementIndex];
      AnElement.SetCounterClockwiseOrientation;
      Assert(AnElement.Count = 5);
      NodePosition := AnElement.IndexOfNode(ANode);
      Assert(NodePosition >= 0);
      NextNode := AnElement[NodePosition+1].FNode;
      Dec(NodePosition);
      if NodePosition < 0 then
      begin
        NodePosition := 3;
      end;
      PriorNode := AnElement[NodePosition].FNode;
      if OrientedVertexAngle(PriorNode.Location, ANode.Location,
        NextNode.Location, CounterClockwise) >= 180 then
      begin
//        Dec(NodePosition);
//        if NodePosition < 0 then
//        begin
//          NodePosition := 3;
//        end;
//        OppositeNode := AnElement[NodePosition].FNode;
        NNodes := TNodeList.Create;
        try
          ANode.GetNeighborNodes(NNodes);
          NNodes.Remove(NextNode);
          NNodes.Remove(PriorNode);
          ClosestNode := nil;
          ClosestDistance := -1;
          for NeighborIndex := 0 to NNodes.Count - 1 do
          begin
            TestNode := NNodes[NeighborIndex];
            TestDistance := Distance(ANode.Location, TestNode.Location);
            if (ClosestNode = nil) or (TestDistance < ClosestDistance) then
            begin
              ClosestNode := TestNode;
              ClosestDistance := TestDistance;
            end;
          end;
          if ClosestNode <> nil then
          begin
            NewLocation.X := (ANode.Location.X + ClosestNode.Location.X)/2;
            NewLocation.Y := (ANode.Location.Y + ClosestNode.Location.Y)/2;

            // The following is not needed so long as the tested angle is
            // greater than or equal to 180 degrees.
            {
            NewLocDist := Distance(ANode.Location, NewLocation);

            IntsectPt :=
              IntersectionPoint(
              EquateLine(OppositeNode.Location, PriorNode.Location),
              EquateLine(ANode.Location, NewLocation));
            if (Distance(ANode.Location, IntsectPt) < NewLocDist)
              and (Distance(NewLocation, IntsectPt) < NewLocDist) then
            begin
              NewLocation.X := (ANode.Location.X + IntsectPt.X)/2;
              NewLocation.Y := (ANode.Location.Y + IntsectPt.Y)/2;
              NewLocDist := Distance(ANode.Location, NewLocation);
            end;

            IntsectPt :=
              IntersectionPoint(
              EquateLine(OppositeNode.Location, NextNode.Location),
              EquateLine(ANode.Location, NewLocation));
            if (Distance(ANode.Location, IntsectPt) < NewLocDist)
              and (Distance(NewLocation, IntsectPt) < NewLocDist) then
            begin
              NewLocation.X := (ANode.Location.X + IntsectPt.X)/2;
              NewLocation.Y := (ANode.Location.Y + IntsectPt.Y)/2;
              NewLocDist := Distance(ANode.Location, NewLocation);
            end;
            }

            ANode.Location := NewLocation;
          end;
        finally
          NNodes.Free;
        end;

        break;
      end;
    end;
  end;
end;

procedure TQuadMeshCreator.ArrangeBoundaries;
var
  ClosedBoundaries: TBoundaryList;
  OpenBoundaries: TBoundaryList;
  BoundaryIndex: integer;
  ABoundary: TBoundary;
  InnerBoundaryIndex: Integer;
  OuterBoundary: TBoundary;
  InnerBoundary: TBoundary;
  NodeIndex: Integer;
  ANode: TNode;
  PolyList: TList<TPolygon2D>;
  APoly: TPolygon2D;
  OuterBoundaryIndex: Integer;
  TestNode: TNode;
  PolyNodeIndex: Integer;
  AList: TBoundaryList;
  index: Integer;
begin
  for index := 0 to FDuplicateBoundaries.Count - 1 do
  begin
    FBoundaries.Add(FDuplicateBoundaries[index]);
  end;
  FDuplicateBoundaries.OwnsObjects := False;
//  FDuplicateBoundaries.Clear;

  FIncludedBoundaries.Clear;
  ClosedBoundaries := TBoundaryList.Create;
  OpenBoundaries := TBoundaryList.Create;
  PolyList := TList<TPolygon2D>.Create;
  try
    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      if (ABoundary.FNodes.Count < 2)
        or (ABoundary.FNodes[0] <> ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
      begin
        OpenBoundaries.Add(ABoundary);
      end
      else
      begin
        ClosedBoundaries.Add(ABoundary);
        AList := TBoundaryList.Create;
        AList.Add(ABoundary);
        FIncludedBoundaries.Add(AList);
        PolyList.Add(APoly);
      end;
    end;
    for OuterBoundaryIndex := ClosedBoundaries.Count - 1 downto 0 do
    begin
      OuterBoundary := ClosedBoundaries[OuterBoundaryIndex];
      for InnerBoundaryIndex := ClosedBoundaries.Count - 1
        downto OuterBoundaryIndex+1 do
      begin
        InnerBoundary := ClosedBoundaries[InnerBoundaryIndex];
        if FDuplicateBoundaries.IndexOf(InnerBoundary) < 0 then
        begin
          TestNode := nil;
          for NodeIndex  := 0 to InnerBoundary.FNodes.Count - 1 do
          begin
            ANode := InnerBoundary.FNodes[NodeIndex];
            if OuterBoundary.FNodes.IndexOf(ANode) < 0 then
            begin
              TestNode := ANode;
              Break;
            end;
          end;
          if TestNode <> nil then
          begin
            APoly := PolyList[OuterBoundaryIndex];
            if Length(APoly) = 0 then
            begin
              SetLength(APoly, OuterBoundary.FNodes.Count-1);
              for PolyNodeIndex := 0 to OuterBoundary.FNodes.Count - 2 do
              begin
                APoly[PolyNodeIndex] := OuterBoundary.FNodes[PolyNodeIndex].Location;
              end;
              PolyList[OuterBoundaryIndex] := APoly;
            end;
            if PointInConcavePolygon(TestNode.Location, APoly) then
            begin
              FIncludedBoundaries[OuterBoundaryIndex].AddRange(
                FIncludedBoundaries[InnerBoundaryIndex]);
              ClosedBoundaries.Delete(InnerBoundaryIndex);
              FIncludedBoundaries.Delete(InnerBoundaryIndex);
              PolyList.Delete(InnerBoundaryIndex);
              Continue;
            end;
          end;
        end;

        TestNode := nil;
        for NodeIndex  := 0 to OuterBoundary.FNodes.Count - 1 do
        begin
          ANode := OuterBoundary.FNodes[NodeIndex];
          if InnerBoundary.FNodes.IndexOf(ANode) < 0 then
          begin
            TestNode := ANode;
            Break;
          end;
        end;
        if TestNode <> nil then
        begin
          APoly := PolyList[InnerBoundaryIndex];
          if Length(APoly) = 0 then
          begin
            SetLength(APoly, InnerBoundary.FNodes.Count-1);
            for PolyNodeIndex := 0 to InnerBoundary.FNodes.Count - 2 do
            begin
              APoly[PolyNodeIndex] := InnerBoundary.FNodes[PolyNodeIndex].Location;
            end;
            PolyList[InnerBoundaryIndex] := APoly;
          end;
          if PointInConcavePolygon(TestNode.Location, APoly) then
          begin
            FIncludedBoundaries[InnerBoundaryIndex].AddRange(
              FIncludedBoundaries[OuterBoundaryIndex]);
            ClosedBoundaries.Delete(OuterBoundaryIndex);
            FIncludedBoundaries.Delete(OuterBoundaryIndex);
            PolyList.Delete(OuterBoundaryIndex);
            OuterBoundary := nil;
            Break;
          end;
        end;
      end;
      if OuterBoundary = nil then
      begin
        Continue;
      end;
      for InnerBoundaryIndex := OpenBoundaries.Count - 1 downto 0 do
      begin
        InnerBoundary := OpenBoundaries[InnerBoundaryIndex];
        TestNode := nil;
        for NodeIndex  := 0 to InnerBoundary.FNodes.Count - 1 do
        begin
          ANode := InnerBoundary.FNodes[NodeIndex];
          if OuterBoundary.FNodes.IndexOf(ANode) < 0 then
          begin
            TestNode := ANode;
            Break;
          end;
        end;
        if TestNode <> nil then
        begin
          APoly := PolyList[OuterBoundaryIndex];
          if Length(APoly) = 0 then
          begin
            SetLength(APoly, OuterBoundary.FNodes.Count-1);
            for PolyNodeIndex := 0 to OuterBoundary.FNodes.Count - 2 do
            begin
              APoly[PolyNodeIndex] := OuterBoundary.FNodes[PolyNodeIndex].Location;
            end;
            PolyList[OuterBoundaryIndex] := APoly;
          end;
          if PointInConcavePolygon(TestNode.Location, APoly) then
          begin
            FIncludedBoundaries[OuterBoundaryIndex].Add(InnerBoundary);
            OpenBoundaries.Delete(InnerBoundaryIndex);
          end;
        end;
      end;
    end;
  finally
    PolyList.Free;
    OpenBoundaries.Free;
    ClosedBoundaries.Free;
  end;
end;

procedure TQuadMeshCreator.AssignOriginalEdgeAngles;
var
  Index: Integer;
  NodeIndex: Integer;
  NodeAngle: double;
  CurrentNode: TNode;
begin
  Assert(FBoundaries.Count > 0);
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    FNodes[NodeIndex].FTotalAngle := 0
  end;

  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].AssignOriginalEdgeAngles;
  end;

  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    CurrentNode := FNodes[NodeIndex];
    NodeAngle := CurrentNode.FTotalAngle;
    if NodeAngle < Pi*3 / 4 then
    begin
      CurrentNode.FDesiredElementCount := 1;
    end
    else if NodeAngle < Pi + Pi / 4 then
    begin
      CurrentNode.FDesiredElementCount := 2;
    end
    else if NodeAngle < Pi * 7 / 4 then
    begin
      CurrentNode.FDesiredElementCount := 3;
    end;
  end;
end;

procedure TQuadMeshCreator.BreakOpenBoundaries;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  NodeIndex: Integer;
  ANode: TNode;
  NewBoundary: TBoundary;
  InnerNodeIndex: Integer;
begin
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    // skip closed boundaries and point boundaries.
    if (ABoundary.FNodes.Count < 2)
      or (ABoundary.FNodes[0] = ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
    begin
      Continue;
    end;
    // There isn't a need to break a TBoundary at the first or last node
    // so don't process them.
    for NodeIndex := ABoundary.FNodes.Count - 2 downto 1 do
    begin
      ANode := ABoundary.FNodes[NodeIndex];
      if ANode.FIntersection then
      begin
        NewBoundary := AddBoundary(ABoundary.DesiredSpacing);
        for InnerNodeIndex := NodeIndex to ABoundary.FNodes.Count - 1 do
        begin
          NewBoundary.FNodes.Add(ABoundary.FNodes[InnerNodeIndex]);
        end;
        for InnerNodeIndex := ABoundary.FNodes.Count - 1 downto NodeIndex+1 do
        begin
          ABoundary.FNodes.Delete(InnerNodeIndex);
        end;
      end;
    end;
  end;
end;

procedure TQuadMeshCreator.ComputeCharacteristicLength;
var
  ABoundary: TBoundary;
  ANode: TNode;
  BoundaryIndex: Integer;
  NodeIndex: Integer;
begin
  Assert(FBoundaries.Count > 0);
  ABoundary := FBoundaries[0];
  Assert(ABoundary.FNodes.Count > 0);
  ANode := ABoundary.FNodes[0];
  FMinX := ANode.X;
  FMinY := ANode.Y;
  FMaxX := FMinX;
  FMaxY := FMinY;
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    for NodeIndex := 0 to ABoundary.FNodes.Count - 1 do
    begin
      ANode := ABoundary.FNodes[NodeIndex];
      if FMinX > ANode.X then
      begin
        FMinX := ANode.X
      end
      else if FMaxX < ANode.X then
      begin
        FMaxX := ANode.X
      end;
      if FMinY > ANode.Y then
      begin
        FMinY := ANode.Y
      end
      else if FMaxY < ANode.Y then
      begin
        FMaxY := ANode.Y
      end;
    end;
  end;
  // Equation 7.
  FCharacteristicLength := Distance(FMinX, FMinY, FMaxX, FMaxY);
end;

procedure TQuadMeshCreator.ConvertProjectionsToBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].ConvertProjectionsToBoundaries;
  end;
end;

procedure TQuadMeshCreator.ConvertToClosedBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].ConvertToClosedBoundary
  end;
end;

constructor TQuadMeshCreator.Create;
begin
  FNodeAdjustmentMethod := namSarrateHuerta;
  FBNodes:= TNodeList.Create;
  FNodes := TNodeObjectList.Create;
  FBoundaries := TBoundaryObjectList.Create;
  FDuplicateBoundaries := TBoundaryObjectList.Create;
  SixNodeClosureMethod := cmTemplate;
  FGrowthRate := ElementGrowthRate;
  FElementList := TIElementList.Create;
  FNodeList := TINodeList.Create;
  FBoundaryNodes := TNodeInBoundaryObjectList.Create;
  FNodeQuadTree := TRbwQuadTree.Create(nil);
  FIncludedBoundaries :=  TObjectList<TBoundaryList>.Create;
end;

procedure TQuadMeshCreator.CreateBoundaryNodes;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].CreateBoundaryNodes;
  end;
end;

procedure TQuadMeshCreator.DeleteDisconnectedBoundaries;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  ANode: TNode;
  ExternalBoundary: TPolygon2D;
//  ReverseExternalBoundary: TPolygon2D;
  NodeIndex: Integer;
  TestNode: TNode;
  PriorNode: TNode;
  HasInsidePoint: Boolean;
  TestPoint: TPoint2D;
  OuterBoundary: TBoundary;
  Index1: integer;
  Index2: integer;
begin
  if FBoundaries.Count = 1 then
  begin
    Exit;
  end;
  for BoundaryIndex := FBoundaries.Count - 1 downto 0 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];

    TestNode := ABoundary.FNodes[0];

    if FBNodes.IndexOf(TestNode) >= 0 then
    begin
      Continue;
    end;

    OuterBoundary := FBoundaries[0];
    if Length(ExternalBoundary) = 0 then
    begin
      Assert(OuterBoundary.FNodes.First = OuterBoundary.FNodes.Last);
      SetLength(ExternalBoundary, OuterBoundary.FNodes.Count-1);
      for NodeIndex := 0 to Length(ExternalBoundary) - 1 do
      begin
        ExternalBoundary[NodeIndex] := OuterBoundary.FNodes[NodeIndex].Location;
      end;
    end;

    HasInsidePoint := False;
    PriorNode := nil;
    for NodeIndex := 0 to ABoundary.FNodes.Count - 1 do
    begin
      ANode := ABoundary.FNodes[NodeIndex];
      if ANode.FIntersection then
      begin
        if (PriorNode <> nil) and PriorNode.FIntersection then
        begin
          TestPoint.X := (PriorNode.X + ANode.X)/2;
          TestPoint.Y := (PriorNode.Y + ANode.Y)/2;
          HasInsidePoint := PointInConcavePolygon(TestPoint, ExternalBoundary);
          if not HasInsidePoint then
          begin
            Index1 := OuterBoundary.FNodes.IndexOf(ANode);
            Index2 := OuterBoundary.FNodes.IndexOf(PriorNode);
            HasInsidePoint := (Index1 >= 0) and (Index2 >= 0)
              and (Abs(Index1-Index2) = 1);
          end;
        end;
      end
      else
      begin
        HasInsidePoint := PointInConcavePolygon(ANode.Location, ExternalBoundary);
      end;
      if HasInsidePoint then
      begin
        Break;
      end;
      PriorNode := ANode;
    end;

    if not HasInsidePoint then
    begin
      FBoundaries.Delete(BoundaryIndex);
    end;
  end;
end;

procedure TQuadMeshCreator.DeleteExternalBoundaries;
var
  ClosedBoundaries: TBoundaryList;
  BoundaryIndex: Integer;
  OpenBoundaries: TBoundaryList;
  ABoundary: TBoundary;
  Poly: TPolygon2D;
  NodeIndex: Integer;
  AClosedBoundary: TBoundary;
  TestBoundary: TBoundary;
  TestPoint: TPoint2D;
  ClosedBoundaryIndex: Integer;
  ShouldDelete: Boolean;
  ANode: TNode;
  procedure GetTestPoint(var DeleteImmediately: Boolean;
    var TestPoint: TPoint2D);
  begin
    DeleteImmediately := False;
    if TestBoundary.Count = 1 then
    begin
      if AClosedBoundary.FNodes.IndexOf(TestBoundary.FNodes[0]) >= 0 then
      begin
        DeleteImmediately := True;
      end;
      TestPoint := TestBoundary.FNodes[0].Location;
    end
    else
    begin
      if not TestBoundary.FNodes[0].FIntersection then
      begin
        TestPoint := TestBoundary.FNodes[0].Location;
      end
      else if not TestBoundary.FNodes[TestBoundary.FNodes.Count-1].FIntersection then
      begin
        TestPoint := TestBoundary.FNodes[TestBoundary.FNodes.Count-1].Location;
      end
      else if (TestBoundary.FNodes.Count > 2) and
        (not TestBoundary.FNodes[1].FIntersection) then
      begin
        TestPoint := TestBoundary.FNodes[1].Location;
      end
      else
      begin
        TestPoint.X := (TestBoundary.FNodes[0].X + TestBoundary.FNodes[1].X)/2;
        TestPoint.Y := (TestBoundary.FNodes[0].Y + TestBoundary.FNodes[1].Y)/2;
      end;
    end;
  end;
begin
  ClosedBoundaries := TBoundaryList.Create;
  OpenBoundaries := TBoundaryList.Create;
  try
    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      if (ABoundary.FNodes.Count < 2)
        or (ABoundary.FNodes[0] <> ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
      begin
        OpenBoundaries.Add(ABoundary);
      end
      else
      begin
        ClosedBoundaries.Add(ABoundary);
      end;
    end;
    AClosedBoundary := ClosedBoundaries[0];
    Assert(AClosedBoundary = FBoundaries[0]);
    if OpenBoundaries.Count > 0 then
    begin
      SetLength(Poly, AClosedBoundary.FNodes.Count-1);
      for NodeIndex := 0 to AClosedBoundary.FNodes.Count - 2 do
      begin
        Poly[NodeIndex] := AClosedBoundary.FNodes[NodeIndex].Location;
      end;
      for BoundaryIndex := OpenBoundaries.Count - 1 downto 0 do
      begin
        TestBoundary := OpenBoundaries[BoundaryIndex];
        GetTestPoint(ShouldDelete, TestPoint);
        if not ShouldDelete then
        begin
          ShouldDelete := not PointInConcavePolygon(TestPoint, Poly);
        end;
        if ShouldDelete then
        begin
          OpenBoundaries.Delete(BoundaryIndex);
          FBoundaries.Remove(TestBoundary);
        end;
      end;

      for ClosedBoundaryIndex := 1 to ClosedBoundaries.Count - 1 do
      begin
        AClosedBoundary := ClosedBoundaries[ClosedBoundaryIndex];
        if OpenBoundaries.Count > 0 then
        begin
          SetLength(Poly, AClosedBoundary.FNodes.Count-1);
          for NodeIndex := 0 to AClosedBoundary.FNodes.Count - 2 do
          begin
            Poly[NodeIndex] := AClosedBoundary.FNodes[NodeIndex].Location;
          end;
          for BoundaryIndex := OpenBoundaries.Count - 1 downto 0 do
          begin
            TestBoundary := OpenBoundaries[BoundaryIndex];
            GetTestPoint(ShouldDelete, TestPoint);
            if not ShouldDelete then
            begin
              ShouldDelete := PointInConcavePolygon(TestPoint, Poly);
            end;
            if ShouldDelete then
            begin
              OpenBoundaries.Delete(BoundaryIndex);
              FBoundaries.Remove(TestBoundary);
            end;
          end;
        end
      end;
    end;
  finally
    ClosedBoundaries.Free;
    OpenBoundaries.Free;
  end;
  for NodeIndex := FNodes.Count - 1 downto 0 do
  begin
    ANode := FNodes[NodeIndex];
    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      if ABoundary.FNodes.IndexOf(ANode) >= 0 then
      begin
        ANode := nil;
        break;
      end;
    end;
    if ANode <> nil then
    begin
      FNodes.Delete(NodeIndex);
    end;
  end;
end;

destructor TQuadMeshCreator.Destroy;
begin
  FIncludedBoundaries.Free;
  FNodeQuadTree.Free;
  FBoundaryNodes.Free;
  FElementList.Free;
  FNodeList.Free;
  FDuplicateBoundaries.Free;
  FBoundaries.Free;
  FNodes.Free;
  FBNodes.Free;
  inherited;
end;

procedure TQuadMeshCreator.ExtractClosedBoundaries;
var
  Changed: Boolean;
  IntersectionNodes: TNodeList;
  LinkedOpenBoundaries: TBoundaryListSqr;
  LinkedClosedBoundaries: TBoundaryListSqr;
  NodeIndex: Integer;
  ANode: TNode;
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  ClosedBoundaries: TBoundaryList;
  StartLink: TBoundaryLink;
  LoopLink: TBoundaryLink;
  NewBoundary1: TBoundary;
  BoundaryNodeIndex: integer;
  LastNodeIndex: integer;
  LastNode: TNode;
  LinkList: TBoundaryLinkList;
  LinkIndex: integer;
  NewBoundary2: TBoundary;
  LoopNode: TNode;
  LinkNodes: TNodeList;

  function GetLoopLink(Link: TBoundaryLink; ANode: TNode; LinkNodes: TNodeList): TBoundaryLink;
  var
    BList: TBoundaryList;
    ParentLink: TBoundaryLink;
    SearchBoundary: TBoundary;
    ABoundary: TBoundary;
    BoundaryIndex: Integer;
    OppositeNode: TNode;
    NodePosition: Integer;
    ClosedBoundaryList: TBoundaryList;
    ChildLink: TBoundaryLink;
    OpenBoundaries: TBoundaryList;
    BoundarySearchList:  TBoundaryList;
    SearchNodePosition: Integer;
    PriorNode: TNode;
    ParentAngle: double;
    SearchPoly: TPolygon2D;
    BNodeIndex: Integer;
    SearchLocation: TPoint2D;
  begin
    result := nil;
    OpenBoundaries := LinkedOpenBoundaries[IntersectionNodes.IndexOf(ANode)];
    if OpenBoundaries.Count > 0 then
    begin
      BList := TBoundaryList.Create;
      BoundarySearchList := TBoundaryList.Create;
      try
        ParentLink := Link;
        while ParentLink <> nil do
        begin
          BList.Add(ParentLink.Boundary);
          ParentLink := ParentLink.Parent;
        end;
        SearchBoundary := BList.Last;
        Assert(SearchBoundary <> nil);
        for BoundaryIndex := OpenBoundaries.Count - 1 downto 0 do
        begin
          if BoundaryIndex >= OpenBoundaries.Count then
          begin
            Continue;
          end;
          ABoundary := OpenBoundaries[BoundaryIndex];
          if (ABoundary.FNodes.Count > 1)
            and (ABoundary.FNodes[0] = ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
          begin
            Continue;
          end;
          if BList.IndexOf(ABoundary) >= 0 then
          begin
            Continue;
          end;
          NodePosition := ABoundary.FNodes.IndexOf(ANode);
          Assert(NodePosition >= 0);
          OppositeNode := ABoundary.FNodes[ABoundary.FNodes.Count-1-NodePosition];
          if OppositeNode.FIntersection
            and (LinkNodes.IndexOf(OppositeNode) < 0) then
          begin
            if Length(SearchPoly) = 0 then
            begin
              SetLength(SearchPoly, SearchBoundary.FNodes.Count-1);
              for BNodeIndex := 0 to SearchBoundary.FNodes.Count - 2 do
              begin
                SearchPoly[BNodeIndex] :=
                  SearchBoundary.FNodes[BNodeIndex].Location;
              end;
            end;
            if NodePosition = 0 then
            begin
              SearchLocation.X := (ANode.X + ABoundary.FNodes[1].X)/2;
              SearchLocation.Y := (ANode.Y + ABoundary.FNodes[1].Y)/2;
            end
            else
            begin
              SearchLocation.X := (ANode.X + ABoundary.FNodes[ABoundary.FNodes.Count - 2].X)/2;
              SearchLocation.Y := (ANode.Y + ABoundary.FNodes[ABoundary.FNodes.Count - 2].Y)/2;
            end;

            if PointInConcavePolygon(SearchLocation, SearchPoly) then
            begin
              BoundarySearchList.Add(ABoundary);
            end;
          end;
        end;


        if BoundarySearchList.Count > 1 then
        begin
          SearchNodePosition := Link.Boundary.FNodes.IndexOf(ANode);
          if SearchNodePosition = 0 then
          begin
            SearchNodePosition := Link.Boundary.FNodes.Count-1;
          end;
          PriorNode := Link.Boundary.FNodes[SearchNodePosition-1];
          ParentAngle := ArcTan2(ANode.Y - PriorNode.Y, ANode.X - PriorNode.X);

          // Sort the boundaries in counterclockwise order around ANode;
          BoundarySearchList.Sort(TBoundaryComparer.Construct
            (function (const L, R: TBoundary): integer
              var
                NodePositionL: Integer;
                NodePositionR: Integer;
                LAngle: double;
                RAngle: double;
                OtherNode: TNode;
              begin
                result := 0;
                if R = L then
                begin
                  Exit;
                end;
                if L.FNodes.Count = 1 then
                begin
                  if R.FNodes.Count = 1 then
                  begin
                    Result := NativeUInt(Pointer(L)) - NativeUInt(Pointer(R));
                  end
                  else
                  begin
                    Result := -1;
                  end;
                end
                else
                begin
                  if R.FNodes.Count = 1 then
                  begin
                    Result := 1;
                  end
                  else
                  begin
                    NodePositionL := L.FNodes.IndexOf(ANode);
                    if NodePositionL = 0 then
                    begin
                      OtherNode := L.FNodes[1];
                    end
                    else
                    begin
                      Assert(NodePositionL = L.FNodes.Count-1);
                      OtherNode := L.FNodes[L.FNodes.Count-2];
                    end;
                    LAngle := ArcTan2(ANode.Y - OtherNode.Y, ANode.X - OtherNode.X);
                    if LAngle < ParentAngle then
                    begin
                      LAngle := LAngle + 2*Pi;
                    end;

                    NodePositionR := R.FNodes.IndexOf(ANode);
                    if NodePositionR = 0 then
                    begin
                      OtherNode := R.FNodes[1];
                    end
                    else
                    begin
                      Assert(NodePositionR = R.FNodes.Count-1);
                      OtherNode := R.FNodes[R.FNodes.Count-2];
                    end;
                    RAngle := ArcTan2(ANode.Y - OtherNode.Y, ANode.X - OtherNode.X);
                    if RAngle < ParentAngle then
                    begin
                      RAngle := RAngle + 2*Pi;
                    end;
                    result := Sign(LAngle-RAngle);

                  end;
                end;

              end
              ));
        end;

        for BoundaryIndex := 0 to BoundarySearchList.Count - 1 do
        begin
          ABoundary := BoundarySearchList[BoundaryIndex];
          NodePosition := ABoundary.FNodes.IndexOf(ANode);
          Assert(NodePosition >= 0);
          OppositeNode := ABoundary.FNodes[ABoundary.FNodes.Count-1-NodePosition];
          ClosedBoundaryList := LinkedClosedBoundaries[IntersectionNodes.IndexOf(OppositeNode)];
          if ClosedBoundaryList.IndexOf(SearchBoundary) >= 0 then
          begin
            ChildLink := TBoundaryLink.Create;
            ChildLink.Parent := Link;
            ChildLink.Boundary := ABoundary;
            ChildLink.PositionInBoundary := NodePosition;
            Link.Children.Add(ChildLink) ;
            Result := ChildLink;
            Exit;
          end;

          begin
            ChildLink := TBoundaryLink.Create;
            ChildLink.Parent := Link;
            ChildLink.Boundary := ABoundary;
            ChildLink.PositionInBoundary := NodePosition;
            Link.Children.Add(ChildLink) ;
            LinkNodes.Add(OppositeNode);
            Result := GetLoopLink(ChildLink, OppositeNode, LinkNodes);
            if Result <> nil then
            begin
              Exit;
            end
            else
            begin
              LinkNodes.Remove(OppositeNode);
            end;
          end;
        end;
      finally
        BList.Free;
        BoundarySearchList.Free;
      end;
    end;
  end;
begin
  IntersectionNodes := TNodeList.Create;
  LinkedOpenBoundaries:= TBoundaryListSqr.Create;
  LinkedClosedBoundaries:= TBoundaryListSqr.Create;
  ClosedBoundaries:= TBoundaryList.Create;
  try
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      if ANode.FIntersection then
      begin
        IntersectionNodes.Add(ANode);
      end;
    end;
    repeat
      Changed:= false;

      InitializeIntersectLists(IntersectionNodes, LinkedClosedBoundaries,
        LinkedOpenBoundaries, ClosedBoundaries);

      for BoundaryIndex := 0 to ClosedBoundaries.Count - 1 do
      begin
        ABoundary := ClosedBoundaries[BoundaryIndex];
        for NodeIndex := 0 to ABoundary.FNodes.Count - 1 do
        begin
          ANode := ABoundary.FNodes[NodeIndex];
          if ANode.FIntersection then
          begin
            StartLink := TBoundaryLink.Create;
            LinkNodes := TNodeList.Create;
            try
              StartLink.Boundary := ABoundary;
              StartLink.PositionInBoundary := NodeIndex;
              StartLink.Parent := nil;
              LinkNodes.Add(ANode);
              LoopLink := GetLoopLink(StartLink, ANode, LinkNodes);
              if LoopLink <> nil then
              begin
                Changed := True;
                LastNodeIndex :=
                  LoopLink.Boundary.FNodes.Count-1-LoopLink.PositionInBoundary;
                LastNode :=  LoopLink.Boundary.FNodes[LastNodeIndex];
                LastNodeIndex := ABoundary.FNodes.IndexOf(LastNode);
                Assert(LastNodeIndex > StartLink.PositionInBoundary);
                LinkList := TBoundaryLinkList.Create;
                try
                  while LoopLink <> nil do
                  begin
                    LinkList.Add(LoopLink);
                    LoopLink := LoopLink.Parent;
                  end;
                  LinkList.Delete(LinkList.Count-1);
                  LinkList.Reverse;
                  NewBoundary1 := AddBoundary(ABoundary.DesiredSpacing);
                  for BoundaryNodeIndex := 0 to StartLink.PositionInBoundary-1 do
                  begin
                    NewBoundary1.FNodes.Add(ABoundary.FNodes[BoundaryNodeIndex]);
                  end;
                  NewBoundary2 := AddBoundary(ABoundary.DesiredSpacing);
                  for LinkIndex := 0 to LinkList.Count - 1 do
                  begin
                    LoopLink := LinkList[LinkIndex];
                    if LoopLink.PositionInBoundary = 0 then
                    begin
                      for BoundaryNodeIndex := 0 to LoopLink.Boundary.FNodes.Count - 2 do
                      begin
                        LoopNode := LoopLink.Boundary.FNodes[BoundaryNodeIndex];
                        NewBoundary1.FNodes.Add(LoopNode);
                        NewBoundary2.FNodes.Add(LoopNode);
                      end;
                    end
                    else
                    begin
                      Assert(LoopLink.PositionInBoundary =
                        LoopLink.Boundary.FNodes.Count-1);
                      for BoundaryNodeIndex := LoopLink.Boundary.FNodes.Count - 1 downto 1 do
                      begin
                        LoopNode := LoopLink.Boundary.FNodes[BoundaryNodeIndex];
                        NewBoundary1.FNodes.Add(LoopNode);
                        NewBoundary2.FNodes.Add(LoopNode);
                      end;
                    end;
                    FBoundaries.Remove(LoopLink.Boundary);
                  end;
                  for BoundaryNodeIndex := LastNodeIndex downto StartLink.PositionInBoundary do
                  begin
                    NewBoundary2.FNodes.Add(ABoundary.FNodes[BoundaryNodeIndex]);
                  end;
                  for BoundaryNodeIndex := LastNodeIndex to ABoundary.FNodes.Count - 1 do
                  begin
                    NewBoundary1.FNodes.Add(ABoundary.FNodes[BoundaryNodeIndex]);
                  end;

                  if NewBoundary1.NodeOrientation <> Clockwise then
                  begin
                    NewBoundary1.FNodes.Reverse;
                  end;
                  if NewBoundary2.NodeOrientation <> Clockwise then
                  begin
                    NewBoundary2.FNodes.Reverse;
                  end;

                  FBoundaries.Remove(ABoundary);

                finally
                  LinkList.Free;
                end;
              end;
            finally
              StartLink.Free;
              LinkNodes.Free;
            end;
          end;
          if Changed then
          begin
            break;
          end;
        end;
        if Changed then
        begin
          break;
        end;
      end;

    until (not Changed);
  finally
    ClosedBoundaries.Free;
    LinkedOpenBoundaries.Free;
    LinkedClosedBoundaries.Free;
    IntersectionNodes.Free;
  end;
end;

procedure TQuadMeshCreator.AssignDesiredSpacings;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  SegmentIndex: Integer;
  ASegment: TSegment;
  NodeList: TNodeList;
  NodeIndex: Integer;
  NodeComparer: TNodeSpacingComparer;
  CompareNode: TNode;
  InnerNodeIndex: Integer;
  AnotherNode: TNode;
  Separation: TFloat;
  CurrentSpacing: double;
  Radius: double;
  OuterSpacing: double;
  ModifiedNode: boolean;
  RadiusToNode: double;
  Epsilon: double;
begin
  Epsilon := FCharacteristicLength/1e7;
  NodeList := TNodeList.Create;
  try
    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      for SegmentIndex := 0 to ABoundary.FSegments.Count - 1 do
      begin
        ASegment := ABoundary.FSegments[SegmentIndex];
        NodeList.Add(ASegment.Node1);
        for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
        begin
          NodeList.Add(ASegment.FInnerNodes[NodeIndex]);
        end;
      end;
    end;

    NodeComparer := TNodeSpacingComparer.Create;
    try
      NodeList.Sort(NodeComparer);

      for NodeIndex := NodeList.Count - 1 downto 1 do
      begin
        CompareNode := NodeList[NodeIndex];
        OuterSpacing := CompareNode.FDesiredSpacing;
        Radius := 0.0;
        ModifiedNode := False;
        for InnerNodeIndex := NodeIndex - 1 downto 0 do
        begin
          AnotherNode := NodeList[InnerNodeIndex];
          if CompareNode.FDesiredSpacing = AnotherNode.FDesiredSpacing then
          begin
            Continue;
          end;
          if OuterSpacing <> AnotherNode.FDesiredSpacing then
          begin
            Radius := 0.0;
            CurrentSpacing := CompareNode.DesiredSpacing;
            OuterSpacing := AnotherNode.FDesiredSpacing;
            while CurrentSpacing < AnotherNode.FDesiredSpacing do
            begin
              Radius := Radius + CurrentSpacing;
              CurrentSpacing := CurrentSpacing * GrowthRate;
              if CurrentSpacing > AnotherNode.FDesiredSpacing then
              begin
                CurrentSpacing := AnotherNode.FDesiredSpacing;
              end;
            end;
          end;

          Separation := Distance(CompareNode.Location, AnotherNode.Location);
          if Separation < Radius then
          begin
            ModifiedNode := True;
            CurrentSpacing := CompareNode.DesiredSpacing;
            RadiusToNode := 0;
            while RadiusToNode < Separation do
            begin
              RadiusToNode := RadiusToNode + CurrentSpacing;
              if RadiusToNode >= Separation then
              begin
                if RadiusToNode > Separation then
                begin
                  CurrentSpacing := ln(AnotherNode.DesiredSpacing/
                    CompareNode.DesiredSpacing)/ln(GrowthRate);
                  CurrentSpacing := CurrentSpacing *Separation/RadiusToNode;
                  CurrentSpacing := CompareNode.DesiredSpacing*
                    Power(GrowthRate,CurrentSpacing);
                end;
                Assert(CurrentSpacing <= AnotherNode.DesiredSpacing+Epsilon);
                Assert(CurrentSpacing >= CompareNode.DesiredSpacing-Epsilon);
                AnotherNode.FDesiredSpacing := CurrentSpacing;
                break;
              end;
              CurrentSpacing := CurrentSpacing * GrowthRate;
            end;
          end;
        end;
        NodeList.Delete(NodeIndex);
        if ModifiedNode then
        begin
          NodeList.Sort(NodeComparer);
        end;
      end;
    finally
      NodeComparer.Free;
    end;

  finally
    NodeList.Free;
  end;
end;

procedure TQuadMeshCreator.BreakClosedBoundariesThatIntersectOuterBoundary;
var
  NodesTree: TRbwQuadTree;
  NodeIndex: Integer;
  ANode: TNode;
  OuterBoundary: TBoundary;
  BoundaryIndex: Integer;
  OuterNodeIndex: Integer;
  Node1: TNode;
  Node2: TNode;
  OuterSegment: TSegment2D;
  InnerNodeIndex: Integer;
  Node3: TNode;
  Node4: TNode;
  InnerSegment: TSegment2D;
  APoint: TPoint2D;
  X: TFloat;
  Y: TFloat;
  DataArray: TPointerArray;
  InsertOuter: Boolean;
  InsertInner: Boolean;
  IntesectNode: TNode;
  ABoundary: TBoundary;
  DesiredSpacing: Extended;
  OuterPolygon: TPolygon2D;
  MidPoint: TPoint2D;
  IntersectsBoundary: boolean;
  MoveNodeIndex: Integer;
  Test1: Boolean;
  Test2: Boolean;
  PreviouslyInserted: boolean;
  NodePosition: integer;
  NodeToDelete: TNode;
  procedure HandleIntersectionInsertion;
  begin
    PreviouslyInserted := False;
    IntersectsBoundary := True;
    APoint := IntersectionPoint(OuterSegment, InnerSegment);
    X := APoint.X;
    Y := APoint.Y;
    NodesTree.FindClosestPointsData(X,Y, DataArray);
    InsertOuter := True;
    InsertInner := True;
    IntesectNode := nil;
    if NearlyTheSame(X, APoint.X, XEpsilon)
      and NearlyTheSame(Y, APoint.Y, YEpsilon) then
    begin
      if NearlyTheSame(APoint, Node1.Location) then
      begin
        Node1.FIntersection := True;
        IntesectNode := Node1;
        InsertOuter := False;
      end;
      if NearlyTheSame(APoint, Node2.Location) then
      begin
        Node2.FIntersection := True;
        IntesectNode := Node2;
        InsertOuter := False;
      end;
      if NearlyTheSame(APoint, Node3.Location) then
      begin
        Node3.FIntersection := True;
        IntesectNode := Node3;
        InsertInner := False;
      end;
      if NearlyTheSame(APoint, Node4.Location) then
      begin
        Node4.FIntersection := True;
        IntesectNode := Node4;
        InsertInner := False;
      end;
    end;

    ANode := nil;
    if InsertOuter or InsertInner then
    begin
      if InsertOuter and InsertInner then
      begin
        DesiredSpacing := Min(OuterBoundary.DesiredSpacing,
          ABoundary.DesiredSpacing);
      end
      else if InsertOuter then
      begin
        DesiredSpacing := OuterBoundary.DesiredSpacing;
      end
      else
      begin
        DesiredSpacing := ABoundary.DesiredSpacing;
      end;
      if IntesectNode = nil then
      begin
        ANode := TNode.Create(self, DesiredSpacing);
        ANode.Location := APoint;
        if BoundaryIndex = 0 then
        begin
          FBNodes.Add(ANode);
          // FBNodes are now no longer in order.
        end;
      end
      else
      begin
        ANode := IntesectNode;
      end;
      ANode.FIntersection := True;
      ANode.FDesiredSpacing := DesiredSpacing;
      if InsertOuter then
      begin
        if OuterBoundary.FNodes[OuterNodeIndex+1] <> ANode then
        begin
          OuterBoundary.FNodes.Insert(OuterNodeIndex+1,ANode);
        end;
        OuterSegment[2] := ANode.Location;
      end;
      if InsertInner then
      begin
        if ABoundary.FNodes[InnerNodeIndex+1] <> ANode then
        begin
          ABoundary.FNodes.Insert(InnerNodeIndex+1,ANode);
        end;
        InnerSegment[2] := ANode.Location;
      end;
      NodesTree.AddPoint(ANode.X, ANode.Y, ANode);
    end
    else
    begin
      ANode := IntesectNode;
      PreviouslyInserted := True;
    end;
  end;
begin
  NodesTree := TRbwQuadTree.Create(nil);
  try
    NodesTree.XMax := FMaxX;
    NodesTree.XMin := FMinX;
    NodesTree.YMax := FMaxY;
    NodesTree.YMin := FMinY;
    YEpsilon := (NodesTree.YMax - NodesTree.YMin)/1e9;
    XEpsilon := (NodesTree.XMax - NodesTree.XMin)/1e9;
    for NodeIndex := 1 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      NodesTree.AddPoint(ANode.X, ANode.Y, ANode);
    end;
    OuterBoundary:= FBoundaries[0];
  {$IFDEF DEBUG}
    // Force inclusion of
    OuterBoundary.NodeLocations;
  {$ENDIF}

    SetLength(OuterPolygon, OuterBoundary.FNodes.Count-1);
    for OuterNodeIndex := 0 to OuterBoundary.FNodes.Count - 2 do
    begin
      OuterPolygon[OuterNodeIndex] :=
        OuterBoundary.FNodes[OuterNodeIndex].Location;
    end;

    for BoundaryIndex := 1 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      if (ABoundary.FNodes.Count > 1)
        and (ABoundary.FNodes[0] = ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
      begin
        IntersectsBoundary := False;
        if ABoundary.FNodes.Count > 2 then
        begin
          Test1 := False;
          Test2 := False;
          InnerNodeIndex := 0;
          Node3 := ABoundary.FNodes[InnerNodeIndex];
          Node4 := ABoundary.FNodes[InnerNodeIndex+1];
          InnerSegment[1] := Node3.Location;
          InnerSegment[2] := Node4.Location;
          for OuterNodeIndex := OuterBoundary.FNodes.Count - 2 downto 0 do
          begin
            Node1 := OuterBoundary.FNodes[OuterNodeIndex];
            Node2 := OuterBoundary.FNodes[OuterNodeIndex+1];
            OuterSegment[1] := Node1.Location;
            OuterSegment[2] := Node2.Location;
            if Intersect(OuterSegment, InnerSegment) then
            begin
              HandleIntersectionInsertion;
              Test1 := True;
            end;
          end;
          InnerNodeIndex := ABoundary.FNodes.Count - 2;
          Node3 := ABoundary.FNodes[InnerNodeIndex];
          Node4 := ABoundary.FNodes[InnerNodeIndex+1];
          InnerSegment[1] := Node3.Location;
          InnerSegment[2] := Node4.Location;
          for OuterNodeIndex := OuterBoundary.FNodes.Count - 2 downto 0 do
          begin
            Node1 := OuterBoundary.FNodes[OuterNodeIndex];
            Node2 := OuterBoundary.FNodes[OuterNodeIndex+1];
            OuterSegment[1] := Node1.Location;
            OuterSegment[2] := Node2.Location;
            if Intersect(OuterSegment, InnerSegment) then
            begin
              HandleIntersectionInsertion;
              Test2 := True;
            end;
          end;
          if Test1 and Test2 then
          begin
            ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
            ABoundary.FNodes.Delete(0);
            Continue;
          end
          else
          begin
            IntersectsBoundary := False;
          end;
//            if Intersect(OuterSegment, InnerSegment) then
//            begin
//              HandleIntersectionInsertion;
//            end;
        end;
//        end;
        for OuterNodeIndex := OuterBoundary.FNodes.Count - 2 downto 0 do
        begin
          Node1 := OuterBoundary.FNodes[OuterNodeIndex];
          Node2 := OuterBoundary.FNodes[OuterNodeIndex+1];
          OuterSegment[1] := Node1.Location;
          OuterSegment[2] := Node2.Location;
          for InnerNodeIndex := ABoundary.FNodes.Count - 2 downto 0 do
          begin
            Node3 := ABoundary.FNodes[InnerNodeIndex];
            Node4 := ABoundary.FNodes[InnerNodeIndex+1];
            InnerSegment[1] := Node3.Location;
            InnerSegment[2] := Node4.Location;
            if Intersect(OuterSegment, InnerSegment) then
            begin
              HandleIntersectionInsertion;

              if PreviouslyInserted then
              begin
                NodePosition := ABoundary.FNodes.IndexOf(ANode);
                Assert(NodePosition >= 0);
                if NodePosition > 0 then
                begin
                  Dec(NodePosition);
                end
                else
                begin
                  NodePosition := ABoundary.FNodes.Count - 2;
                end;
                NodeToDelete := ABoundary.FNodes[NodePosition];
                if PointInConcavePolygon(NodeToDelete.Location, OuterPolygon) then
                begin
                  NodePosition := ABoundary.FNodes.IndexOf(ANode);
                  Assert(NodePosition >= 0);
                  if NodePosition < ABoundary.FNodes.Count-1 then
                  begin
                    Inc(NodePosition)
                  end
                  else
                  begin
                    NodePosition := 1;
                  end;
                  NodeToDelete := ABoundary.FNodes[NodePosition];
                  if PointInConcavePolygon(NodeToDelete.Location, OuterPolygon) then
                  begin
                    Assert(False);
                  end
                  else
                  begin
                    ABoundary.FNodes.Delete(NodePosition);
                    if NodePosition = ABoundary.FNodes.Count then
                    begin
                      ABoundary.FNodes.Delete(0);
                    end;
                  end;
                end
                else
                begin
                  ABoundary.FNodes.Delete(NodePosition);
                  if NodePosition = 0 then
                  begin
                    ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  end;
                end;
              end
              else if (Node3 <> ANode) then
              begin
                MidPoint := SegmentMidPoint(Node3.Location, ANode.Location);
                if PointInConcavePolygon(MidPoint, OuterPolygon) then
                begin
                  // Node3 is inside the outer boundary
                  ABoundary.FNodes.Delete(InnerNodeIndex+2);
                  ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  for MoveNodeIndex := ABoundary.FNodes.Count-1 downto InnerNodeIndex + 2 do
                  begin
                    ABoundary.FNodes.Insert(0, ABoundary.FNodes[ABoundary.FNodes.Count-1]);
                    ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  end;
                end
                else
                begin
                  // Node3 is not inside the outer boundary
                  ABoundary.FNodes.Delete(InnerNodeIndex);
                  ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  for MoveNodeIndex := 0 to InnerNodeIndex - 1 do
                  begin
                    ABoundary.FNodes.Add(ABoundary.FNodes[0]);
                    ABoundary.FNodes.Delete(0);
                  end;
                end;
              end
              else
              begin
                MidPoint := SegmentMidPoint(Node4.Location, ANode.Location);
                if PointInConcavePolygon(MidPoint, OuterPolygon) then
                begin
                  // Node3 is not inside the outer boundary
                  ABoundary.FNodes.Delete(InnerNodeIndex);
                  ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  for MoveNodeIndex := 0 to InnerNodeIndex - 1 do
                  begin
                    ABoundary.FNodes.Add(ABoundary.FNodes[0]);
                    ABoundary.FNodes.Delete(0);
                  end;
                end
                else
                begin
                  // Node3 is inside the outer boundary
                  ABoundary.FNodes.Delete(InnerNodeIndex+1);
                  ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  for MoveNodeIndex := ABoundary.FNodes.Count-1 downto InnerNodeIndex + 1 do
                  begin
                    ABoundary.FNodes.Insert(0, ABoundary.FNodes[ABoundary.FNodes.Count-1]);
                    ABoundary.FNodes.Delete(ABoundary.FNodes.Count-1);
                  end;
                end;
              end;

              Break;
            end;
          end;
          if IntersectsBoundary then
          begin
            Break;
          end;

        end;
//        if IntersectsBoundary then
//        begin
//          Break;
//        end;

      end;
    end;
  finally
    NodesTree.Free;
  end;
end;

procedure TQuadMeshCreator.IntersectBoundaries;
var
  ABoundary: TBoundary;
  NodesTree: TRbwQuadTree;
  BoundaryIndex: Integer;
  NodeIndex: Integer;
  ANode: TNode;
  NextBIndex: Integer;
  NextBoundary: TBoundary;
  OuterNodeIndex: Integer;
  OuterSegment: TSegment2D;
  Node1: TNode;
  Node2: TNode;
  InnerNodeIndex: Integer;
  Node3: TNode;
  Node4: TNode;
  InnerSegment: TSegment2D;
  APoint: TPoint2D;
  X: double;
  Y: double;
  DataArray: TPointerArray;
  InsertOuter: Boolean;
  InsertInner: Boolean;
  DesiredSpacing: Extended;
  Changed: Boolean;
  IntesectNode: TNode;
begin
  NodesTree := TRbwQuadTree.Create(nil);
  try
    NodesTree.XMax := FMaxX;
    NodesTree.XMin := FMinX;
    NodesTree.YMax := FMaxY;
    NodesTree.YMin := FMinY;
    YEpsilon := (NodesTree.YMax - NodesTree.YMin)/1e7;
    XEpsilon := (NodesTree.XMax - NodesTree.XMin)/1e7;
    for NodeIndex := 1 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      NodesTree.AddPoint(ANode.X, ANode.Y, ANode);
    end;

    repeat
      Changed := False;
      for BoundaryIndex := 0 to FBoundaries.Count - 2 do
      begin
        ABoundary:= FBoundaries[BoundaryIndex];
        for NextBIndex := BoundaryIndex+1 to FBoundaries.Count - 1 do
        begin
          NextBoundary :=  FBoundaries[NextBIndex];
          for OuterNodeIndex := ABoundary.FNodes.Count - 2 downto 0 do
          begin
            Node1 := ABoundary.FNodes[OuterNodeIndex];
            Node2 := ABoundary.FNodes[OuterNodeIndex+1];
            OuterSegment[1] := Node1.Location;
            OuterSegment[2] := Node2.Location;
            for InnerNodeIndex := NextBoundary.FNodes.Count - 2 downto 0 do
            begin
              Node3 := NextBoundary.FNodes[InnerNodeIndex];
              Node4 := NextBoundary.FNodes[InnerNodeIndex+1];
              InnerSegment[1] := Node3.Location;
              InnerSegment[2] := Node4.Location;
              if Intersect(OuterSegment, InnerSegment) then
              begin
                APoint := IntersectionPoint(OuterSegment, InnerSegment);
                X := APoint.X;
                Y := APoint.Y;
                NodesTree.FindClosestPointsData(X,Y, DataArray);
                InsertOuter := True;
                InsertInner := True;
                IntesectNode := nil;
                if NearlyTheSame(X, APoint.X, XEpsilon)
                  and NearlyTheSame(Y, APoint.Y, YEpsilon) then
                begin
                  if NearlyTheSame(APoint, Node1.Location) then
                  begin
                    Node1.FIntersection := True;
                    IntesectNode := Node1;
                    InsertOuter := False;
                  end;
                  if NearlyTheSame(APoint, Node2.Location) then
                  begin
                    Node2.FIntersection := True;
                    IntesectNode := Node2;
                    InsertOuter := False;
                  end;
                  if NearlyTheSame(APoint, Node3.Location) then
                  begin
                    Node3.FIntersection := True;
                    IntesectNode := Node3;
                    InsertInner := False;
                  end;
                  if NearlyTheSame(APoint, Node4.Location) then
                  begin
                    Node4.FIntersection := True;
                    IntesectNode := Node4;
                    InsertInner := False;
                  end;
                end;
                if InsertOuter or InsertInner then
                begin
                  if InsertOuter and InsertInner then
                  begin
                    DesiredSpacing := Min(ABoundary.DesiredSpacing,
                      NextBoundary.DesiredSpacing);
                  end
                  else if InsertOuter then
                  begin
                    DesiredSpacing := ABoundary.DesiredSpacing;
                  end
                  else
                  begin
                    DesiredSpacing := NextBoundary.DesiredSpacing;
                  end;
                  if IntesectNode = nil then
                  begin
                    ANode := TNode.Create(self, DesiredSpacing);
                    ANode.Location := APoint;
                    if BoundaryIndex = 0 then
                    begin
                      FBNodes.Add(ANode);
                      // FBNodes are now no longer in order.
                    end;
                  end
                  else
                  begin
                    ANode := IntesectNode;
                  end;
                  ANode.FIntersection := True;
                  ANode.FDesiredSpacing := DesiredSpacing;
                  if InsertOuter then
                  begin
                    if ABoundary.FNodes[OuterNodeIndex+1] <> ANode then
                    begin
                      ABoundary.FNodes.Insert(OuterNodeIndex+1,ANode);
                    end;
                    OuterSegment[2] := ANode.Location;
                  end;
                  if InsertInner then
                  begin
                    if NextBoundary.FNodes[InnerNodeIndex+1] <> ANode then
                    begin
                      NextBoundary.FNodes.Insert(InnerNodeIndex+1,ANode);
                    end;
                    InnerSegment[2] := ANode.Location;
                  end;
                  NodesTree.AddPoint(ANode.X, ANode.Y, ANode);
                  Changed := True;
                end;
              end;
            end;
          end;
        end;
      end;
    until not Changed;
  finally
    NodesTree.Free;
  end;
end;

procedure TQuadMeshCreator.FillNodeTree;
var
  index: Integer;
  ANode: TNode;
begin
  FNodeQuadTree.XMax := FMaxX;
  FNodeQuadTree.XMin := FMinX;
  FNodeQuadTree.YMax := FMaxY;
  FNodeQuadTree.YMin := FMinY;
  for index := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[index];
    FNodeQuadTree.AddPoint(ANode.X, ANode.Y, ANode);
  end;
end;

procedure TQuadMeshCreator.StoreClosedBoundaryPolygons;
var
  BoundaryIndex: Integer;
  List: TBoundaryList;
  ABoundary: TBoundary;
  PointIndex: Integer;
begin
  List := TBoundaryList.Create;
  try
    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      if (ABoundary.FNodes.Count > 2) and (ABoundary.FNodes[0]
        = ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
      begin
        List.Add(ABoundary);
      end;
    end;
    SetLength(FPolygonArray, List.Count);
    for BoundaryIndex := 0 to List.Count - 1 do
    begin
      ABoundary := List[BoundaryIndex];
      SetLength(FPolygonArray[BoundaryIndex], ABoundary.FNodes.Count-1);
      for PointIndex := 0 to ABoundary.FNodes.Count - 2 do
      begin
        FPolygonArray[BoundaryIndex][PointIndex]
          := ABoundary.FNodes[PointIndex].Location;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TQuadMeshCreator.AssignConstraintNodes;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].AssignConstraintNodes
  end;
end;

procedure TQuadMeshCreator.GenerateMesh;
const
  MaxIterations = 10;
var
  List: TBoundaryList;
  Index: Integer;
  ABoundary: TBoundary;
  Iterations: integer;
begin
{$IFDEF DEBUG}
//  OutputDebugString('SAMPLING ON');
{$ENDIF}
  Initialize;
  BreakClosedBoundariesThatIntersectOuterBoundary;
  IntersectBoundaries;
  BreakOpenBoundaries;
  DeleteDisconnectedBoundaries;
  MergeClosedBoundaries;
  StoreClosedBoundaryPolygons;
  BreakOpenBoundaries;
  DeleteExternalBoundaries;
  ExtractClosedBoundaries;
//  MergeOpenNodeBoundaries2;
  MergeOpenWithClosedBoundaries;
//  MergeOpenNodeBoundaries;
  MergeOpenNodeBoundaries3;
  ArrangeBoundaries;
  SetNodeTypes;
  GenerateSegments;
  AssignDesiredSpacings;
  FillNodeTree;
  InsertNodesAlongBoundaries;
  ConvertToClosedBoundaries;
  CreateBoundaryNodes;
  AssignConstraintNodes;
  AssignDesiredSpacings;
  AssignOriginalEdgeAngles;
  FixSegments;
  MakeSingleBoundary;
  FixSegments;
  SplitMultiplyConnectedBoundaries;
  FixSegments;

  Assert(FBoundaries.Count >= 1);
  List := TBoundaryList.Create;
  try
    for index := 0 to FBoundaries.Count - 1 do
    begin
      List.Add(FBoundaries[index]);
    end;

    Index := 0;
    while Index < List.Count do
    begin
      ABoundary := List[Index];
      {$IFDEF DEBUG}
      try
      {$ENDIF}
      ABoundary.Split(List);
      {$IFDEF DEBUG}
      except
//        ShowMessage('Error at ' + IntToStr(Index));
        raise;
      end;
      {$ENDIF}

      Inc(Index);
    end;

  finally
    List.Free;
  end;

  FixEdgeTriangles;

  for index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[index].SetCounterClockwiseOrientation;
  end;

  Iterations := 0;
  repeat
    repeat
      AdjustNodes;
    until (not ImproveTopology);
    Inc(Iterations)
  until  (not FixEdgeTriangles) or (Iterations >= MaxIterations);

  FixFinalTriangularElements;

  RenumberNodesAndElements;
{$IFDEF DEBUG}
//  OutputDebugString('SAMPLING OFF');
{$ENDIF}
end;

//procedure TQuadMeshCreator.GmshTerminate(Sender: TObject; ExitCode: DWORD);
//begin
//  FGmshTerminated := True;
//end;

procedure TQuadMeshCreator.GenerateMeshWithGeomPackPP(
  const GeompackLocation: string; Exaggeration: double;
  Options: TGeompackOptions; var ErrorMessage: string);
const
  FiveSeconds = 1/24/3600*5;
var
  GeomPackOperationFileName: string;
  NodeIndex: Integer;
  ANode: TNode;
  OperationFile: TStringList;
  ALine: string;
  CommandLine: string;
  BoundaryIndex: Integer;
//  LineIndex: Integer;
  ABoundary: TBoundary;
//  PriorNode: TNode;
//  NewNodeIndex: Integer;
//  LineLoopIndex: Integer;
//  StartLineIndex: Integer;
//  StringBuilder: TStringBuilder;
//  PriorNodeIndex: Integer;
//  OptionFile: TStringList;
//  OptionFileName: string;
  AProcessComp: TJvCreateProcess;
  MeshFileName: string;
  StartTime: TDateTime;
  CurrentDir: string;
//  UsedLoops: array of boolean;
  AnotherBoundary: TBoundary;
//  InnerLinesStart: Integer;
  InnerBoundaryIndex: Integer;
//  BoundaryComparer: TBoundaryAreaComparer;
//  GroupIndex: Integer;
//  BoundaryList: TBoundaryList;
  Mesh: TSutraMesh3D;
  RegionFilename: string;
  CurveFileName: string;
  FSettings: TFormatSettings;
  quadmu: double;
  RegionFile: TStringList;
  BoundaryNodeCount: integer;
  StringBuilderNodeNumbers: TStringBuilder;
  StringBuilderVertexInfo: TStringBuilder;
  inreg: Integer;
  CurveFile: TStringList;
  ErrorFileName: string;
  ErrorFileS: TStringList;
  regcode: Integer;
  edgetype: Integer;
  AnotherNode: TNode;
//  ANodeIB: TNodeInBoundary;
begin
{$IFDEF DEBUG}
//  OutputDebugString('SAMPLING ON');
{$ENDIF}
  ErrorMessage := '';
  Initialize;
  BreakClosedBoundariesThatIntersectOuterBoundary;
  IntersectBoundaries;
  BreakOpenBoundaries;
  DeleteDisconnectedBoundaries;
  MergeClosedBoundaries;
  StoreClosedBoundaryPolygons;
  BreakOpenBoundaries;
  DeleteExternalBoundaries;
  ExtractClosedBoundaries;
//  MergeOpenNodeBoundaries2;
  MergeOpenWithClosedBoundaries;
//  MergeOpenNodeBoundaries;
  MergeOpenNodeBoundaries3;
  ArrangeBoundaries;

  SetNodeTypes;
  GenerateSegments;
  AssignDesiredSpacings;
  FillNodeTree;
  InsertNodesAlongBoundaries;
  ConvertToClosedBoundaries;
  CreateBoundaryNodes;
  AssignConstraintNodes;
  AssignDesiredSpacings;
  AssignOriginalEdgeAngles;
  FixSegments;
  MakeSingleBoundary;
  FixSegments;
  SplitMultiplyConnectedBoundaries;
  FixSegments;


  ConvertProjectionsToBoundaries;

  MeshFileName := TempFileName;
  GeomPackOperationFileName := TempFileName;
  RegionFilename := TempFileName;
  CurveFileName := TempFileName;
  ErrorFileName := TempFileName;
  try
    FSettings := TFormatSettings.Create;
    FSettings.DecimalSeparator := '.';
//    StringBuilder := TStringBuilder.Create;
    OperationFile := TStringList.Create;
    try
      OperationFile.Add('202 1 1 0 0 0 1');
      OperationFile.Add(RegionFilename);
      OperationFile.Add(CurveFileName);
      OperationFile.Add(MeshFileName);
      ALine := Format('%g 0 0', [Options.Tolerance]);
      OperationFile.Add(ALine);
      ALine := Format('4 %0:d %1:g %2:g %3:g %4:g %5:g %6:g %7:g %8:d %9:d',
        [0, 45., 0.04, 0.2, Options.SpacingAngle, Options.ToleranceAngle,
        Options.UniformnessParameter, Options.MeshDistributionVariation,
        Options.ElementGenerationParameter, Options.DesiredElementCount],
        FSettings);
      OperationFile.Add(ALine);
      if Options.KeepQuadsAlongEdges then
      begin
        quadmu := Options.QuadSplittingValue;
      end
      else
      begin
        quadmu := -Options.QuadSplittingValue;
      end;
      ALine := Format('4 %0:d %1:g %2:d %3:d %4:g',
        [Ord(Options.ShapeMeasure)+1, quadmu, Options.MaxImprovementIterations,
        Options.MaxSmoothingIterations, Options.OptimizationBasedSmoothingCriterion],
        FSettings);
      OperationFile.Add(ALine);

      OperationFile.SavetoFile(GeomPackOperationFileName);
    finally
      OperationFile.Free;
//      StringBuilder.Free;
    end;

    RegionFile := TStringList.Create;
    try
      RegionFile.Add(Format('%d', [FNodes.Count]));
      RegionFile.Add('');

      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        ANode := FNodes[NodeIndex];
        ALine := Format('%0:g %1:g 2', [ANode.X, ANode.Y], FSettings);
        RegionFile.Add(ALine);
      end;
      RegionFile.Add('');

      RegionFile.Add('0');
      RegionFile.Add('');

      RegionFile.Add(Format('%d', [FBoundaries.Count]));
      RegionFile.Add('');

      StringBuilderNodeNumbers := TStringBuilder.Create;
      StringBuilderVertexInfo := TStringBuilder.Create;
      try
        for BoundaryIndex := 0 to FBoundaries.Count - 1 do
        begin
          ABoundary := FBoundaries[BoundaryIndex];
          if ABoundary.FNodes[0] = ABoundary.FNodes[ABoundary.FNodes.Count-1] then
          begin
            BoundaryNodeCount := ABoundary.Count -1;
            ABoundary.SetCounterClockwiseOrientation;
            regcode := BoundaryIndex+1
          end
          else
          begin
            BoundaryNodeCount := ABoundary.Count;
            regcode := -2
          end;

          if BoundaryIndex = 0 then
          begin
            inreg := 0;
          end
          else
          begin
            inreg := 0;
            if regcode < 0 then
            begin
              for InnerBoundaryIndex := 0 to FBoundaries.Count - 1 do
              begin
                if InnerBoundaryIndex = BoundaryIndex then
                begin
                  Continue;
                end;
                AnotherBoundary := FBoundaries[InnerBoundaryIndex];
                if AnotherBoundary.IsBoundaryInside(ABoundary) then
                begin
                  inreg := InnerBoundaryIndex + 1;
                  break;
                end;
              end;
            end
            else
            begin
              inreg := 1;
              for NodeIndex := 0 to BoundaryNodeCount - 1 do
              begin
                ANode := ABoundary[NodeIndex].FNode;
                if ANode.ElementCount > 1 then
                begin
                  inreg := 0;
                  break;
                end;
              end;
            end;
          end;

          ALine := Format('%0:d %1:d %2:d',
            [BoundaryNodeCount, regcode, inreg]);
          RegionFile.Add(ALine);

//          StringBuilderNodeNumbers.Clear;
//          for NodeIndex := 0 to BoundaryNodeCount - 1 do
//          begin
//            ANode := ABoundary.FNodes[NodeIndex];
//            StringBuilderNodeNumbers.Append(' ');
//            StringBuilderNodeNumbers.Append(ANode.NodeNumber + 1);
//          end;
//          RegionFile.Add(StringBuilderNodeNumbers.ToString);

          StringBuilderNodeNumbers.Clear;
          StringBuilderVertexInfo.Clear;
          for NodeIndex := 0 to BoundaryNodeCount - 1 do
          begin
            ANode := ABoundary[NodeIndex].FNode;

            StringBuilderNodeNumbers.Append(' ');
            StringBuilderNodeNumbers.Append(ANode.NodeNumber + 1);

            StringBuilderVertexInfo.Append(' ');
            edgetype := 2;
            if (regcode < 0) then
            begin
              if (NodeIndex = BoundaryNodeCount - 1) then
              begin
                edgetype := -2;
              end;
            end
            else
            begin
              if ANode.ElementCount > 1 then
              begin
                if NodeIndex+1 < ABoundary.Count then
                begin
                  AnotherNode := ABoundary[NodeIndex+1].FNode;
                  if AnotherNode.ElementCount > 1 then
                  begin
                    edgetype := 4;
                  end;
                end;
              end;
            end;
            StringBuilderVertexInfo.Append(edgetype);
          end;
          RegionFile.Add(StringBuilderNodeNumbers.ToString);
          RegionFile.Add(StringBuilderVertexInfo.ToString);
          RegionFile.Add('');
        end;
      finally
        StringBuilderNodeNumbers.Free;
        StringBuilderVertexInfo.Free;
      end;


      RegionFile.SavetoFile(RegionFilename);
    finally
      RegionFile.Free;
    end;

    CurveFile := TStringlist.Create;
    try
      CurveFile.Add('0');
      CurveFile.SavetoFile(CurveFileName);
    finally
      CurveFile.Free;
    end;

//    LineLoopIndex := 0;
//    LineIndex := 0;
//    for GroupIndex := 0 to FIncludedBoundaries.Count - 1 do
//    begin
//      BoundaryList := FIncludedBoundaries[GroupIndex];
//
//      StartLineIndex := LineIndex;
//      ABoundary := BoundaryList[0];
//      Assert(ABoundary.FNodes[0] = ABoundary.FNodes[ABoundary.FNodes.Count-1]);
//      PriorNode := ABoundary.FNodes[0];
//      PriorNodeIndex := FNodes.IndexOf(PriorNode);
//      for NodeIndex := 1 to ABoundary.FNodes.Count - 1 do
//      begin
//        ANode := ABoundary.FNodes[NodeIndex];
//        if ANode <> PriorNode then
//        begin
//          NewNodeIndex := FNodes.IndexOf(ANode);
//          ALine := Format('Line(%0:d) = {%1:d, %2:d};',
//            [LineIndex + 1, PriorNodeIndex+1, NewNodeIndex+1]);
//          OperationFile.Add(ALine);
//          PriorNodeIndex := NewNodeIndex;
//          Inc(LineIndex);
//        end;
//        PriorNode := ANode;
//      end;
//      InnerLinesStart := LineIndex;
//      for InnerBoundaryIndex := 1 to BoundaryList.Count - 1 do
//      begin
//        AnotherBoundary := BoundaryList[InnerBoundaryIndex];
//        if AnotherBoundary.Area = 0 then
//        begin
//          Continue;
//        end;
//        AnotherBoundary.RemoveProjections;
//
//
//        PriorNode := AnotherBoundary.FNodes.Last;
//        PriorNodeIndex := FNodes.IndexOf(PriorNode);
//        for NodeIndex := AnotherBoundary.FNodes.Count - 2 downto 0 do
//        begin
//          ANode := AnotherBoundary.FNodes[NodeIndex];
//          if ANode <> PriorNode then
//          begin
//            NewNodeIndex := FNodes.IndexOf(ANode);
//            ALine := Format('Line(%0:d) = {%1:d, %2:d};',
//              [LineIndex + 1, PriorNodeIndex+1, NewNodeIndex+1]);
//            OperationFile.Add(ALine);
//            PriorNodeIndex := NewNodeIndex;
//            Inc(LineIndex);
//          end;
//          PriorNode := ANode;
//        end;
//      end;
//
////      StringBuilder.Clear;
////      StringBuilder.Append('Line Loop(');
////      StringBuilder.Append(LineLoopIndex+1);
////      StringBuilder.Append(') = {');
//      for NodeIndex := StartLineIndex to InnerLinesStart-1 do
//      begin
////        StringBuilder.Append(NodeIndex+1);
//        if NodeIndex < LineIndex-1 then
//        begin
////          StringBuilder.Append(', ');
//        end;
//      end;
//
//      for NodeIndex := InnerLinesStart to LineIndex-1 do
//      begin
////        StringBuilder.Append(-(NodeIndex+1));
//        if NodeIndex < LineIndex-1 then
//        begin
////          StringBuilder.Append(', ');
//        end;
//      end;
////      StringBuilder.Append('};');
////      ALine := StringBuilder.ToString;
//      OperationFile.Add(ALine);
//      ALine := Format('Plane Surface(%0:d) = {%0:d};', [LineLoopIndex+1]);
//      OperationFile.Add(ALine);
//      Inc(LineLoopIndex);
//    end;
//
//    OperationFile.SaveToFile(GeomPackOperationFileName);

//    OptionFile := TStringList.Create;
//    try
//      OptionFile.Add('Mesh.RecombineAll = 1;');
//      OptionFile.Add('Mesh.SubdivisionAlgorithm = 1;');
//      OptionFileName := GeomPackOperationFileName + '.opt';
//      OptionFile.SaveToFile(OptionFileName);
//    finally
//      OptionFile.Free;
//    end;

//    MeshFileName := ChangeFileExt(GeomPackOperationFileName, '.msh');
    CurrentDir := GetCurrentDir;

    DeleteFile(ErrorFileName);
    AProcessComp := TJvCreateProcess.Create(nil);
    try
      SetCurrentDir(ExtractFileDir(GeomPackOperationFileName));
      CommandLine := Format('%0:s %1:s %2:s', [GeompackLocation,
        ExtractFileName(GeomPackOperationFileName), ExtractFileName(ErrorFileName)]);
      AProcessComp.WaitForTerminate := True;
      AProcessComp.CommandLine := CommandLine;
//      FGmshTerminated := False;
      try
        AProcessComp.Run;
      except on E: EOSError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          ErrorMessage := E.message;
          Exit;
        end;
      end;
      StartTime := Now;
      while not FileExists(ErrorFileName) do
      begin
        if Now - StartTime > FiveSeconds then
        begin
          Beep;
          MessageDlg('Geompack++ failed.', mtError, [mbOK], 0);
          ErrorMessage := 'Geompack timed out';
          Exit;
  //        StartTime := Now;
        end;
        Sleep(100);
      end;
      if FileExists(ErrorFileName) then
      begin
        StartTime := TFile.GetLastWriteTime(ErrorFileName);
        while True do
        begin
          Sleep(100);
          if TFile.GetLastWriteTime(ErrorFileName) = StartTime then
          begin
            break;
          end
          else
          begin
            StartTime := TFile.GetLastWriteTime(ErrorFileName);
          end;
        end;
        ErrorFileS := TStringList.Create;
        try
          StartTime := Now;
          while (Now - StartTime) < FiveSeconds do
          begin
            try
              ErrorFileS.LoadFromFile(ErrorFileName);
            except on EFOpenError do
              begin
                Sleep(100);
                Continue;
              end;
            end;
            if ErrorFileS.Count > 0 then
            begin
              ErrorMessage := ErrorFileS.Text;
              Beep;
              MessageDlg('Geompack++ failed.', mtError, [mbOK], 0);
              Exit;
            end;
            break;
          end;
        finally
          ErrorFileS.Free;
        end;
      end;
      StartTime := TFile.GetLastWriteTime(MeshFileName);
      while True do
      begin
        Sleep(100);
        if TFile.GetLastWriteTime(MeshFileName) = StartTime then
        begin
          break;
        end
        else
        begin
          StartTime := TFile.GetLastWriteTime(MeshFileName);
        end;
      end;
    finally
      AProcessComp.Free;
      SetCurrentDir(CurrentDir);
    end;

    Assert(FileExists(MeshFileName));
    ImportSutraMeshFromFile(MeshFileName, Exaggeration, False);

//    DeleteFile(OptionFileName);
//    DeleteFile(MeshFileName);
  finally
    DeleteFile(GeomPackOperationFileName);
    DeleteFile(MeshFileName);
    DeleteFile(RegionFilename);
    DeleteFile(CurveFileName);
    DeleteFile(ErrorFileName);
  end;

//  RenumberNodesAndElements;
//  Exit;

  Mesh := frmGoPhast.PhastModel.SutraMesh;
  case RenumberingAlgorithm of
    raNone: ;
//      begin
//        if Mesh.MeshType = mt3D then
//        begin
//          Mesh.SimpleRenumber;
//        end;
//      end;
    CuthillMcKee:
      begin
        CuthillMcKeeRenumbering.RenumberMesh(Mesh.Mesh2D);
        Mesh.Mesh2D.Nodes.SortByNodeNumber;
        Mesh.Mesh2D.Elements.SortByElementNumber;
        if Mesh.MeshType = mt3D then
        begin
          Mesh.SimpleRenumber;
        end;
//        frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;
      end;
    raSloanRandolph:
      begin
        MeshRenumbering.RenumberMesh(Mesh.Mesh2D);
        Mesh.Mesh2D.Nodes.SortByNodeNumber;
        Mesh.Mesh2D.Elements.SortByElementNumber;
        if Mesh.MeshType = mt3D then
        begin
          Mesh.SimpleRenumber;
        end;
//        frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;
      end
    else Assert(False);
  end;
{$IFDEF DEBUG}
//  OutputDebugString('SAMPLING OFF');
{$ENDIF}
end;

procedure TQuadMeshCreator.GenerateMeshWithGmsh(const GMshLocation: string;
  Exaggeration: double);
const
  FiveSeconds = 1/24/3600*5;
var
  GmshFileName: string;
  NodeIndex: Integer;
  ANode: TNode;
  GmshFile: TStringList;
  ALine: string;
  CommandLine: string;
//  BoundaryIndex: Integer;
  LineIndex: Integer;
  ABoundary: TBoundary;
  PriorNode: TNode;
  NewNodeIndex: Integer;
  LineLoopIndex: Integer;
  StartLineIndex: Integer;
  StringBuilder: TStringBuilder;
  PriorNodeIndex: Integer;
  OptionFile: TStringList;
  OptionFileName: string;
  AProcessComp: TJvCreateProcess;
  MeshFileName: string;
  StartTime: TDateTime;
  CurrentDir: string;
//  UsedLoops: array of boolean;
  AnotherBoundary: TBoundary;
  InnerLinesStart: Integer;
  InnerBoundaryIndex: Integer;
//  BoundaryComparer: TBoundaryAreaComparer;
  GroupIndex: Integer;
  BoundaryList: TBoundaryList;
  Mesh: TSutraMesh3D;
begin
{$IFDEF DEBUG}
//  OutputDebugString('SAMPLING ON');
{$ENDIF}
  Initialize;
  BreakClosedBoundariesThatIntersectOuterBoundary;
  IntersectBoundaries;
  BreakOpenBoundaries;
  DeleteDisconnectedBoundaries;
  MergeClosedBoundaries;
  StoreClosedBoundaryPolygons;
  BreakOpenBoundaries;
  DeleteExternalBoundaries;
  ExtractClosedBoundaries;
//  MergeOpenNodeBoundaries2;
  MergeOpenWithClosedBoundaries;
//  MergeOpenNodeBoundaries;
  MergeOpenNodeBoundaries3;
  ArrangeBoundaries;

  GmshFileName := TempFileName;
  StringBuilder := TStringBuilder.Create;
  GmshFile := TStringList.Create;
  try
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      ALine := Format('Point(%0:d) = {%1:g, %2:g, 0, %3:g};',
        [NodeIndex+1, ANode.X, ANode.Y, ANode.DesiredSpacing*2]);
      GmshFile.Add(ALine);
    end;

    LineLoopIndex := 0;
    LineIndex := 0;
    for GroupIndex := 0 to FIncludedBoundaries.Count - 1 do
    begin
      BoundaryList := FIncludedBoundaries[GroupIndex];

      StartLineIndex := LineIndex;
      ABoundary := BoundaryList[0];
      Assert(ABoundary.FNodes[0] = ABoundary.FNodes[ABoundary.FNodes.Count-1]);
      PriorNode := ABoundary.FNodes[0];
      PriorNodeIndex := FNodes.IndexOf(PriorNode);
      for NodeIndex := 1 to ABoundary.FNodes.Count - 1 do
      begin
        ANode := ABoundary.FNodes[NodeIndex];
        if ANode <> PriorNode then
        begin
          NewNodeIndex := FNodes.IndexOf(ANode);
          ALine := Format('Line(%0:d) = {%1:d, %2:d};',
            [LineIndex + 1, PriorNodeIndex+1, NewNodeIndex+1]);
          GmshFile.Add(ALine);
          PriorNodeIndex := NewNodeIndex;
          Inc(LineIndex);
        end;
        PriorNode := ANode;
      end;
      InnerLinesStart := LineIndex;
      for InnerBoundaryIndex := 1 to BoundaryList.Count - 1 do
      begin
        AnotherBoundary := BoundaryList[InnerBoundaryIndex];
        if AnotherBoundary.Area = 0 then
        begin
          Continue;
        end;
        AnotherBoundary.RemoveProjections;


        PriorNode := AnotherBoundary.FNodes.Last;
        PriorNodeIndex := FNodes.IndexOf(PriorNode);
        for NodeIndex := AnotherBoundary.FNodes.Count - 2 downto 0 do
        begin
          ANode := AnotherBoundary.FNodes[NodeIndex];
          if ANode <> PriorNode then
          begin
            NewNodeIndex := FNodes.IndexOf(ANode);
            ALine := Format('Line(%0:d) = {%1:d, %2:d};',
              [LineIndex + 1, PriorNodeIndex+1, NewNodeIndex+1]);
            GmshFile.Add(ALine);
            PriorNodeIndex := NewNodeIndex;
            Inc(LineIndex);
          end;
          PriorNode := ANode;
        end;
      end;

      StringBuilder.Clear;
      StringBuilder.Append('Line Loop(');
      StringBuilder.Append(LineLoopIndex+1);
      StringBuilder.Append(') = {');
      for NodeIndex := StartLineIndex to InnerLinesStart-1 do
      begin
        StringBuilder.Append(NodeIndex+1);
        if NodeIndex < LineIndex-1 then
        begin
          StringBuilder.Append(', ');
        end;
      end;

      for NodeIndex := InnerLinesStart to LineIndex-1 do
      begin
        StringBuilder.Append(-(NodeIndex+1));
        if NodeIndex < LineIndex-1 then
        begin
          StringBuilder.Append(', ');
        end;
      end;
      StringBuilder.Append('};');
      ALine := StringBuilder.ToString;
      GmshFile.Add(ALine);
      ALine := Format('Plane Surface(%0:d) = {%0:d};', [LineLoopIndex+1]);
      GmshFile.Add(ALine);
      Inc(LineLoopIndex);
    end;

    GmshFile.SaveToFile(GmshFileName);

    OptionFile := TStringList.Create;
    try
      OptionFile.Add('Mesh.RecombineAll = 1;');
      OptionFile.Add('Mesh.SubdivisionAlgorithm = 1;');
      OptionFileName := GmshFileName + '.opt';
      OptionFile.SaveToFile(OptionFileName);
    finally
      OptionFile.Free;
    end;
  finally
    GmshFile.Free;
    StringBuilder.Free;
  end;

  MeshFileName := ChangeFileExt(GmshFileName, '.msh');
  CurrentDir := GetCurrentDir;

  AProcessComp := TJvCreateProcess.Create(nil);
  try
    SetCurrentDir(ExtractFileDir(GmshFileName));
    CommandLine := Format('%0:s %1:s -2 -o %2:s', [GMshLocation, ExtractFileName(GmshFileName), ExtractFileName(MeshFileName)]);
    AProcessComp.WaitForTerminate := False;
    AProcessComp.CommandLine := CommandLine;
    FGmshTerminated := False;
    try
      AProcessComp.Run;
    except on E: EOSError do
      begin
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    StartTime := Now;
    while not FileExists(MeshFileName) do
    begin
      if Now - StartTime > FiveSeconds then
      begin
        Beep;
        MessageDlg('Gmsh failed.', mtError, [mbOK], 0);
        Exit;
//        StartTime := Now;
      end;
      Sleep(100);
    end;
    StartTime := TFile.GetLastWriteTime(MeshFileName);
    while True do
    begin
      Sleep(100);
      if TFile.GetLastWriteTime(MeshFileName) = StartTime then
      begin
        break;
      end
      else
      begin
        StartTime := TFile.GetLastWriteTime(MeshFileName);
      end;
    end;
  finally
    AProcessComp.Free;
    SetCurrentDir(CurrentDir);
  end;

  Assert(FileExists(MeshFileName));
  ImportSutraMeshFromFile(MeshFileName, Exaggeration, False);

  DeleteFile(OptionFileName);
  DeleteFile(MeshFileName);

//  RenumberNodesAndElements;
//  Exit;

  Mesh := frmGoPhast.PhastModel.SutraMesh;
  case RenumberingAlgorithm of
    raNone: ;
//      begin
//        if Mesh.MeshType = mt3D then
//        begin
//          Mesh.SimpleRenumber;
//        end;
//      end;
    CuthillMcKee:
      begin
        CuthillMcKeeRenumbering.RenumberMesh(Mesh.Mesh2D);
        Mesh.Mesh2D.Nodes.SortByNodeNumber;
        Mesh.Mesh2D.Elements.SortByElementNumber;
        if Mesh.MeshType = mt3D then
        begin
          Mesh.SimpleRenumber;
        end;
//        frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;
      end;
    raSloanRandolph:
      begin
        MeshRenumbering.RenumberMesh(Mesh.Mesh2D);
        Mesh.Mesh2D.Nodes.SortByNodeNumber;
        Mesh.Mesh2D.Elements.SortByElementNumber;
        if Mesh.MeshType = mt3D then
        begin
          Mesh.SimpleRenumber;
        end;
//        frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;
      end
    else Assert(False);
  end;
{$IFDEF DEBUG}
//  OutputDebugString('SAMPLING OFF');
{$ENDIF}
end;

procedure TQuadMeshCreator.GenerateSegments;
var
  Index: Integer;
  ListIndex: Integer;
  AList: TBoundaryList;
begin
  Assert(IncludedBoundaries.Count > 0);
  for ListIndex := 0 to IncludedBoundaries.Count - 1 do
  begin
    AList := IncludedBoundaries[ListIndex];
    Assert(AList.Count > 0);
    AList[0].GenerateSegments(CounterClockwise);
    for Index := 1 to AList.Count - 1 do
    begin
      AList[Index].GenerateSegments(Clockwise);
    end;
  end;
end;

{$IFDEF TEST}
function TQuadMeshCreator.GetBoundaryCount: Integer;
begin
  result := FBoundaries.Count;
end;
{$ENDIF}

function TQuadMeshCreator.GetActiveElement(Index: Integer): IElement;
begin
  result := FElementList[Index];
end;

function TQuadMeshCreator.GetActiveElementCount: Integer;
begin
  result := FElementList.Count
end;

function TQuadMeshCreator.GetNodeObject(Index: Integer): TNode;
begin
  result := FNodes[Index];
end;

function TQuadMeshCreator.GetActiveNode(Index: Integer): INode;
begin
  result := FNodeList[Index];
end;

function TQuadMeshCreator.GetActiveNodeCount: Integer;
begin
  result := FNodes.Count;
end;

function TQuadMeshCreator.ImproveTopology: boolean;
var
  NodeIndex: Integer;
  ANode: TNode;
  ShouldRemove: boolean;
  ElementIndex: Integer;
  AnElement: TBoundary;
begin
  result := False;
  for NodeIndex := FNodes.Count - 1 downto 0 do
  begin
    ANode := FNodes[NodeIndex];
    for ElementIndex := ANode.FElements.Count - 1 downto 0 do
    begin
      AnElement := ANode.FElements[ElementIndex];
      if AnElement.Count = 0 then
      begin
        ANode.FElements.Delete(ElementIndex);
        result := True;
      end;
    end;
    if ANode.FElements.Count = 0 then
    begin
      FNodes.Delete(NodeIndex);
      result := True;
    end;
  end;
  for NodeIndex := FNodes.Count - 1 downto 0 do
  begin
    if NodeIndex < FNodes.Count then
    begin
      ANode := FNodes[NodeIndex];
      ShouldRemove := ANode.ImproveTopology1;

      result := result or ShouldRemove;
      if ShouldRemove then
      begin
        FNodes.Remove(ANode);
      end
      else
      begin
        result := ANode.ImproveTopology2 or result;
      end;
    end;
  end;
end;


procedure TQuadMeshCreator.Initialize;
begin
  ComputeCharacteristicLength;
  StoreBoundaryNodes;
end;

procedure TQuadMeshCreator.StoreBoundaryNodes;
var
  ABoundary: TBoundary;
begin
  ABoundary := FBoundaries[0];
  FBNodes.AddRange(ABoundary.FNodes);
end;

procedure TQuadMeshCreator.InitializeIntersectLists(
  IntersectionNodes: TNodeList;
  LinkedClosedBoundaries, LinkedOpenBoundaries: TBoundaryListSqr;
  ClosedBoundaries: TBoundaryList);
var
  ABoundary: TBoundary;
  BoundaryIndex: Integer;
  Index: Integer;
  NodeIndex: Integer;
  ANode: TNode;
begin
  ClosedBoundaries.Clear;
  if LinkedOpenBoundaries.Count = 0 then
  begin
    Assert(LinkedClosedBoundaries.Count = 0);
    for Index := 0 to IntersectionNodes.Count - 1 do
    begin
      LinkedOpenBoundaries.Add(TBoundaryList.Create);
      LinkedClosedBoundaries.Add(TBoundaryList.Create);
    end;
  end
  else
  begin
    Assert(LinkedOpenBoundaries.Count = IntersectionNodes.Count);
    Assert(LinkedClosedBoundaries.Count = IntersectionNodes.Count);
    for Index := 0 to IntersectionNodes.Count - 1 do
    begin
      LinkedOpenBoundaries[Index].Clear;
      LinkedClosedBoundaries[Index].Clear;
    end;
  end;
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    if (ABoundary.FNodes.Count < 2)
      or (ABoundary.FNodes[0] <> ABoundary.FNodes[ABoundary.FNodes.Count - 1])
      then
    begin
      for NodeIndex := 0 to IntersectionNodes.Count - 1 do
      begin
        ANode := IntersectionNodes[NodeIndex];
        if ABoundary.FNodes.IndexOf(ANode) >= 0 then
        begin
          LinkedOpenBoundaries[NodeIndex].Add(ABoundary);
        end;
      end;
    end
    else
    begin
      ClosedBoundaries.Add(ABoundary);
      for NodeIndex := 0 to IntersectionNodes.Count - 1 do
      begin
        ANode := IntersectionNodes[NodeIndex];
        if ABoundary.FNodes.IndexOf(ANode) >= 0 then
        begin
          LinkedClosedBoundaries[NodeIndex].Add(ABoundary);
        end;
      end;
    end;
  end;
end;

procedure TQuadMeshCreator.InsertNodesAlongBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].InsertNodesAlongBoundary
  end;
end;

procedure TQuadMeshCreator.GetBestLinkWhenSameNode(var ClosestCost: TCost;
  FirstBoundary, ABoundary: TBoundary);
var
  NodeIndex: Integer;
  ANode: TNodeInBoundary;
//  FirstBoundaryMatchedNodes: TNodeInBoundaryList;
//  FirstBoundaryMatchedNodesIndices : TList<Integer>;
//  ABoundaryMatchedNodes: TNodeInBoundaryList;
//  ABoundaryMatchedNodesIndices: TList<Integer>;
  AngleList: TAngleList;
  CompareItem: TAngleCompareItem;
  PriorNode: TNodeInBoundary;
  NextNode: TNodeInBoundary;
  ItemIndex: Integer;
  AnItem: TAngleCompareItem;
  FirstItem: TAngleCompareItem;
  SecondItem: TAngleCompareItem;
//  SegmentIndex: Integer;
//  ASegment: TSegment;
  Comparer: TAngleComparer;
begin
  Assert(ClosestCost <> nil);
  Assert(FirstBoundary <> nil);
  Assert(ABoundary <> nil);
  Assert(ClosestCost.FNode1.FNode = ClosestCost.FNode2.FNode);
  Assert(FirstBoundary.Count > 1);
  Assert(ABoundary.Count > 1);
//  FirstBoundaryMatchedNodes := TNodeInBoundaryList.Create;
//  FirstBoundaryMatchedNodesIndices :=  TList<Integer>.Create;
//  ABoundaryMatchedNodes := TNodeInBoundaryList.Create;
//  ABoundaryMatchedNodesIndices :=  TList<Integer>.Create;
  AngleList := TAngleList.Create;

  try
    for NodeIndex := 0 to FirstBoundary.Count - 2 do
    begin
      ANode := FirstBoundary[NodeIndex];
      if ANode.FNode = ClosestCost.FNode1.FNode then
      begin
        if NodeIndex > 0 then
        begin
          PriorNode := FirstBoundary[NodeIndex-1];
        end
        else
        begin
          PriorNode := FirstBoundary[FirstBoundary.Count-2];
        end;
        CompareItem := TAngleCompareItem.Create;
        AngleList.Add(CompareItem);
        CompareItem.Angle := ArcTan2(PriorNode.Y-ANode.Y, PriorNode.X-ANode.X);
        CompareItem.NodePosition := NodeIndex;
        CompareItem.Boundary := FirstBoundary;
        CompareItem.Direction := dBackward;

        NextNode := FirstBoundary[NodeIndex+1];
        CompareItem := TAngleCompareItem.Create;
        AngleList.Add(CompareItem);
        CompareItem.Angle := ArcTan2(NextNode.Y-ANode.Y, NextNode.X-ANode.X);
        CompareItem.NodePosition := NodeIndex;
        CompareItem.Boundary := FirstBoundary;
        CompareItem.Direction := dForward;
      end;
    end;
    for NodeIndex := 0 to ABoundary.Count - 2 do
    begin
      ANode := ABoundary[NodeIndex];
      if ANode.FNode = ClosestCost.FNode1.FNode then
      begin
        if NodeIndex > 0 then
        begin
          PriorNode := ABoundary[NodeIndex-1];
        end
        else
        begin
          PriorNode := ABoundary[ABoundary.Count-2];
        end;
        CompareItem := TAngleCompareItem.Create;
        AngleList.Add(CompareItem);
        CompareItem.Angle := ArcTan2(PriorNode.Y-ANode.Y, PriorNode.X-ANode.X);
        CompareItem.NodePosition := NodeIndex;
        CompareItem.Boundary := ABoundary;
        CompareItem.Direction := dBackward;

        NextNode := ABoundary[NodeIndex+1];
        CompareItem := TAngleCompareItem.Create;
        AngleList.Add(CompareItem);
        CompareItem.Angle := ArcTan2(NextNode.Y-ANode.Y, NextNode.X-ANode.X);
        CompareItem.NodePosition := NodeIndex;
        CompareItem.Boundary := ABoundary;
        CompareItem.Direction := dForward;
      end;
    end;

    // sort in order of descending angles.
    Comparer := TAngleComparer.Create;
    try
      AngleList.Sort(Comparer) ;
    finally
      Comparer.Free;
    end;

//    AngleList.Reverse;

    FirstItem := nil;
    SecondItem := nil;
    for ItemIndex := 0 to AngleList.Count - 1 do
    begin
      AnItem := AngleList[ItemIndex];
      if AnItem.Boundary = FirstBoundary then
      begin
        FirstItem := AnItem;
      end;
      if (FirstItem <> nil) and (AnItem.Boundary = ABoundary) then
      begin
        SecondItem := AnItem;
        Break;
      end;
    end;

    if (FirstItem <> nil) and (SecondItem = nil) then
    begin
      for ItemIndex := 0 to AngleList.Count - 1 do
      begin
        AnItem := AngleList[ItemIndex];
        if (AnItem.Boundary = ABoundary) then
        begin
          SecondItem := AnItem;
          Break;
        end;
      end;
    end;
    Assert(FirstItem <> nil);
    Assert(SecondItem <> nil);

    ClosestCost.Free;

    ClosestCost := TCost.Create(FirstBoundary[FirstItem.NodePosition],
      ABoundary[SecondItem.NodePosition], self);

//    ABoundary.Reverse;
//    ABoundary.FSegments.Reverse;
//    for SegmentIndex := 0 to ABoundary.FSegments.Count - 1 do
//    begin
//      ASegment := ABoundary.FSegments[SegmentIndex];
//      ASegment.Reverse;
//    end;
//    for NodeIndex := 0 to ABoundary.Count - 2 do
//    begin
//      ABoundary[NodeIndex].FPosition := NodeIndex;
//    end;


  finally
    AngleList.Free;
//    FirstBoundaryMatchedNodes.Free;
//    FirstBoundaryMatchedNodesIndices.Free;
//    ABoundaryMatchedNodes.Free;
//    ABoundaryMatchedNodesIndices.Free;
  end;
end;

procedure TQuadMeshCreator.MakeSingleBoundary;
var
  BoundaryQuadTree: TRbwQuadTree;
  FirstBoundary: TBoundary;
  NodeIndex: Integer;
  ANode: TNodeInBoundary;
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  ClosestNode: TNodeInBoundary;
  ACost: TCost;
  ClosestCost: TCost;
  ADistance: TFloat;
  ClosestDistance: TFloat;
  SubDomainNode: TNodeInBoundary;
  InsertPosition: Integer;
  BoundariesChanged: boolean;
  NewNode: TNodeInBoundary;
  OuterBoundaryNode: TNodeInBoundary;
  Seg1: TSegment;
  Seg2: TSegment;
  SegPosition: Integer;
  SubDomainSegPosition: Integer;
  NewSegment: TSegment;
  NodeDistance: double;
  NumberOfNodesToInsert: Integer;
  ReversedSegment: TSegment;
  AnInnerNode: TNode;
  SegInsertPosition: Integer;
  SegmentIndex: Integer;
  ListIndex: Integer;
  AList: TBoundaryList;
  BoundaryComparer: TBoundaryCountComparer;
  MovedSegment: TSegment;
  Temp: TCost;
  OuterNodeIndex: Integer;
  InnerNodeIndex: Integer;
  OuterNode: TNodeInBoundary;
  InnerNode: TNodeInBoundary;
  InterSectionNodes: TNodeList;
  procedure SwitchIntersectingBoundary;
  var
    NIndex: Integer;
    EdgeNode: TNode;
    FoundIntersectingBoundary: Boolean;
    BoundaryIndex: integer;
    ABoundary: TBoundary;
  begin
    InterSectionNodes.Clear;
    for NIndex := 0 to FirstBoundary.Count - 1 do
    begin
      EdgeNode := FirstBoundary[NIndex].FNode;
      if EdgeNode.FIntersection
        and (InterSectionNodes.IndexOf(EdgeNode) < 0) then
      begin
        InterSectionNodes.Add(EdgeNode);
      end;
    end;
    if InterSectionNodes.Count = 0 then
    begin
      Exit;
    end;

    FoundIntersectingBoundary := False;
    for BoundaryIndex := AList.Count - 1 downto 0 do
    begin
      ABoundary := AList[BoundaryIndex];
      for NIndex := 0 to InterSectionNodes.Count - 1 do
      begin
        EdgeNode := InterSectionNodes[NIndex];
        if ABoundary.IndexOfNode(EdgeNode) >= 0 then
        begin
          FoundIntersectingBoundary := True;
          AList.Delete(BoundaryIndex);
          AList.Add(ABoundary);
          Break;
        end;
      end;
      if FoundIntersectingBoundary then
      begin
        break;
      end;
    end;
  end;
begin
  Assert(FIncludedBoundaries.Count > 0);

  Assert(FBoundaries.Count > 0);
  BoundaryQuadTree := TRbwQuadTree.Create(nil);
  InterSectionNodes := TNodeList.Create;
  try
    BoundaryQuadTree.XMax := FMaxX;
    BoundaryQuadTree.XMin := FMinX;
    BoundaryQuadTree.YMax := FMaxY;
    BoundaryQuadTree.YMin := FMinY;
    for ListIndex := 0 to FIncludedBoundaries.Count - 1 do
    begin
      AList := FIncludedBoundaries[ListIndex];
      if AList.Count = 1 then
      begin
        Continue;
      end;
      BoundaryQuadTree.Clear;
      FirstBoundary := AList[0];

      // Last point should be a duplicate of the first point.
      for NodeIndex := 0 to FirstBoundary.Count - 2 do
      begin
        ANode := FirstBoundary[NodeIndex];
        BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
      end;

      AList.Delete(0);
      BoundaryComparer := TBoundaryCountComparer.Create;
      try
        AList.Sort(BoundaryComparer);
      finally
        BoundaryComparer.Free;
      end;

      BoundariesChanged := True;
      while (AList.Count > 0) and BoundariesChanged do
      begin
        BoundariesChanged := False;



        for BoundaryIndex := AList.Count - 1 downto 0 do
        begin
          SwitchIntersectingBoundary;
          ABoundary := AList[BoundaryIndex];
          ClosestCost := nil;
          try
            ClosestDistance := 0;
            SubDomainNode := nil;
            OuterBoundaryNode := nil;
            for NodeIndex := 0 to ABoundary.Count - 1 do
            begin
              ANode := ABoundary[NodeIndex];
              ClosestNode := BoundaryQuadTree.NearestPointsFirstData(ANode.FNode.X,
                ANode.FNode.Y);
              ACost := TCost.Create(ANode, ClosestNode, self {, ivUnknown});
              try
                if ACost.Visible then
                begin
                  ADistance := Distance(ANode.FNode.Location,
                    ClosestNode.FNode.Location);
                  if (ClosestCost = nil) or (ADistance < ClosestDistance) then
                  begin
                    Temp := ClosestCost;
                    ClosestCost := ACost;
                    ACost := Temp;
                    ClosestDistance := ADistance;
                    if ClosestDistance = 0 then
                    begin
                      break;
                    end;
                  end;
                end;
              finally
                ACost.Free;
              end;
            end;
            if ClosestCost = nil then
            begin
              for OuterNodeIndex := 0 to FirstBoundary.Count - 1 do
              begin
                OuterNode := FirstBoundary[OuterNodeIndex];
                for InnerNodeIndex := 0 to ABoundary.Count - 1 do
                begin
                  InnerNode := ABoundary[InnerNodeIndex];
                  ACost := TCost.Create(OuterNode, InnerNode, self {, ivUnknown});
                  try
                    if ACost.Visible then
                    begin
                      ADistance := Distance(OuterNode.FNode.Location,
                        InnerNode.FNode.Location);
                      if (ClosestCost = nil) or (ADistance < ClosestDistance) then
                      begin
                        Temp := ClosestCost;
                        ClosestCost := ACost;
                        ACost := Temp;
                        ClosestDistance := ADistance;
                      end;
                    end;
                  finally
                    ACost.Free;
                  end;
                end;
              end;
            end;
            if (ClosestCost <> nil) and (ClosestDistance = 0) then
            begin
              GetBestLinkWhenSameNode(ClosestCost, FirstBoundary, ABoundary);
            end;
            if ClosestCost <> nil then
            begin
              if ClosestCost.FNode1.FBoundary = ABoundary then
              begin
                SubDomainNode := ClosestCost.FNode1;
                OuterBoundaryNode := ClosestCost.FNode2;
                Assert(OuterBoundaryNode.FBoundary = FirstBoundary);
              end
              else
              begin
                SubDomainNode := ClosestCost.FNode2;
                OuterBoundaryNode := ClosestCost.FNode1;
                Assert(SubDomainNode.FBoundary = ABoundary);
                Assert(OuterBoundaryNode.FBoundary = FirstBoundary);
              end;
            end;
            if (SubDomainNode <> nil) and (OuterBoundaryNode <> nil) then
            begin
              if ABoundary.Count = 1 then
              begin
                Assert(SubDomainNode.SegmentCount = 1);
                Seg1 := nil;
                Seg2 := SubDomainNode.Segments[0];
              end
              else
              begin
                ABoundary.SplitSegmentAtNode(SubDomainNode);

                Assert(SubDomainNode.SegmentCount = 2);
                Seg1 := SubDomainNode.Segments[0];
                Seg2 := SubDomainNode.Segments[1];
                if Seg1.FNode2 <> Seg2.FNode1 then
                begin
                  SubDomainNode.ReverseSegments;
                  Seg1 := SubDomainNode.Segments[0];
                  Seg2 := SubDomainNode.Segments[1];
                  Assert(Seg1.FNode2 = Seg2.FNode1);
                end;
              end;
              Assert(Seg2.Node1 = SubDomainNode.FNode);
              SubDomainSegPosition := ABoundary.FSegments.IndexOf(Seg2);
              Assert(SubDomainSegPosition >= 0);

              FirstBoundary.SplitSegmentAtNode(OuterBoundaryNode);
              InsertPosition := OuterBoundaryNode.Position + 1;

              Assert(OuterBoundaryNode.SegmentCount = 2);
              Seg1 := OuterBoundaryNode.Segments[0];
              Seg2 := OuterBoundaryNode.Segments[1];
              if Seg1.FNode2 <> Seg2.FNode1 then
              begin
                OuterBoundaryNode.ReverseSegments;
                Seg1 := OuterBoundaryNode.Segments[0];
                Seg2 := OuterBoundaryNode.Segments[1];
                Assert(Seg1.FNode2 = Seg2.FNode1);
              end;
              Assert(Seg2.Node1 = OuterBoundaryNode.FNode);
              SegPosition := FirstBoundary.FSegments.IndexOf(Seg1);
              Assert(SegPosition >= 0);

              NewSegment := nil;
              ReversedSegment := nil;
              NewNode := nil;

              if Distance(OuterBoundaryNode.FNode.Location,
                SubDomainNode.FNode.Location) > 0 then
              begin
                NewSegment := TSegment.Create(OuterBoundaryNode.FNode,
                  SubDomainNode.FNode, stInner, FirstBoundary, self);
                OuterBoundaryNode.Segments[1] := NewSegment;
                SubDomainNode.Segments[0] := NewSegment;
                NodeDistance := NewSegment.Length;
                if (NodeDistance > ABoundary.DesiredSpacing) or
                  (NodeDistance > FirstBoundary.DesiredSpacing) then
                begin
                  NumberOfNodesToInsert := NewSegment.NodesToInsert;
                  if NumberOfNodesToInsert > 0 then
                  begin
                    NewSegment.InsertNodes(NumberOfNodesToInsert);
                  end;
                end;

                ReversedSegment := NewSegment.CreateReversedSegment;
                if SubDomainNode.SegmentCount = 1 then
                begin
                  SubDomainNode.Segments[0] := ReversedSegment;
//                end
//                else
//                begin
//                  SubDomainNode.Segments[1] := ReversedSegment;
                end;

                for NodeIndex := 0 to NewSegment.FInnerNodes.Count - 1 do
                begin
                  AnInnerNode := NewSegment.FInnerNodes[NodeIndex];
                  NewNode := TNodeInBoundary.Create(AnInnerNode, FirstBoundary,
                    NewSegment);
                  FirstBoundary.Insert(InsertPosition, NewNode);
                  Inc(InsertPosition);
                  BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y,
                    NewNode);
                end;
              end;

              for NodeIndex := SubDomainNode.Position to ABoundary.Count - 2 do
              begin
                ANode := ABoundary[NodeIndex];
                if (NewSegment = nil) and (OuterBoundaryNode.FNode = ANode.FNode)
                  and (SubDomainNode.Position = NodeIndex) then
                begin
                  Continue;
                end;
                FirstBoundary.Insert(InsertPosition, ANode);
                ANode.FBoundary := FirstBoundary;
                Inc(InsertPosition);
                BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
              end;
              if (NewSegment = nil) then
              begin
                if (SubDomainNode.Position = 0) then
                begin
                  ANode := ABoundary[ABoundary.Count-1];
                  FirstBoundary.Insert(InsertPosition, ANode);
                  ANode.FBoundary := FirstBoundary;
                  Inc(InsertPosition);
                  BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
                end;
              end;
              for NodeIndex := 0 to SubDomainNode.Position - 1 do
              begin
                ANode := ABoundary[NodeIndex];
//                if (NewSegment = nil) and (OuterBoundaryNode.FNode = ANode.FNode) then
//                begin
//                  Continue;
//                end;
                FirstBoundary.Insert(InsertPosition, ANode);
                ANode.FBoundary := FirstBoundary;
                Inc(InsertPosition);
                BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
              end;

              if (NewSegment = nil) then
              begin
                if (SubDomainNode.Position <> 0) then
                begin
                  ANode := SubDomainNode;
                  FirstBoundary.Insert(InsertPosition, ANode);
                  ANode.FBoundary := FirstBoundary;
                  Inc(InsertPosition);
                  BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
                end;
              end;

              if ReversedSegment <> nil then
              begin
                NewNode := TNodeInBoundary.Create(ReversedSegment.Node1,
                  FirstBoundary, ReversedSegment);
                FirstBoundary.Insert(InsertPosition, NewNode);
                Inc(InsertPosition);
                BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y, NewNode);
                if ABoundary.Count = 1 then
                begin
                  NewNode.InsertSegment(0, NewSegment);
                  if NewNode.FNode.FSegments.IndexOf(NewSegment) < 0 then
                  begin
                    NewNode.FNode.FSegments.Add(NewSegment);
                  end;
                end;

                for NodeIndex := 0 to ReversedSegment.FInnerNodes.Count - 1 do
                begin
                  AnInnerNode := ReversedSegment.FInnerNodes[NodeIndex];
                  NewNode := TNodeInBoundary.Create(AnInnerNode, FirstBoundary,
                    ReversedSegment);
                  FirstBoundary.Insert(InsertPosition, NewNode);
                  Inc(InsertPosition);
                  BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y,
                    NewNode);
                end;

                NewNode := TNodeInBoundary.Create(ReversedSegment.Node2,
                  FirstBoundary, ReversedSegment);
                FirstBoundary.Insert(InsertPosition, NewNode);
                BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y, NewNode);
              end;

              if (NewNode <> nil) and (NewSegment <> nil) then
              begin
                if NewNode.IndexOfSegment(NewSegment) < 0 then
                begin
                  Assert((NewSegment.FNode1 = NewNode.FNode)
                    or (NewSegment.FNode2 = NewNode.FNode));
                  NewNode.AddSegment(NewSegment);
                end;
              end;



              SegInsertPosition := SegPosition + 1;
              if SegInsertPosition = FirstBoundary.FSegments.Count then
              begin
                SegInsertPosition := 0;
              end;
              if NewSegment <> nil then
              begin
                Assert(NewSegment.FBoundary = FirstBoundary);
                FirstBoundary.FSegments.Insert(SegInsertPosition, NewSegment);
                Inc(SegInsertPosition);
              end;

              if ABoundary.Count > 1 then
              begin
                for SegmentIndex := SubDomainSegPosition to ABoundary.FSegments.
                  Count - 1 do
                begin
                  MovedSegment := ABoundary.FSegments[SegmentIndex];
                  MovedSegment.FBoundary := FirstBoundary;
                  FirstBoundary.FSegments.Insert(SegInsertPosition,
                    MovedSegment);
                  Inc(SegInsertPosition);
                end;
                for SegmentIndex := 0 to SubDomainSegPosition - 1 do
                begin
                  MovedSegment := ABoundary.FSegments[SegmentIndex];
                  MovedSegment.FBoundary := FirstBoundary;
                  FirstBoundary.FSegments.Insert(SegInsertPosition,
                    MovedSegment);
                  Inc(SegInsertPosition);
                end;
              end;
              if ReversedSegment <> nil then
              begin
                Assert(ReversedSegment.FBoundary = FirstBoundary);
                FirstBoundary.FSegments.Insert(SegInsertPosition, ReversedSegment);
              end;

              FirstBoundary.FixSegments;

              FirstBoundary.RenumberNodes;

              if ABoundary.FNodes.Count > 1 then
              begin
                ABoundary.FSegments.OwnsObjects := False;
              end;
              ABoundary.RemoveSelfFromAllNodes;

              AList.Delete(BoundaryIndex);
              FBoundaries.Remove(ABoundary);
              BoundariesChanged := True;
            end;
          finally
            ClosestCost.Free;
          end;
        end;
      end;
      Assert(AList.Count = 0);

    end;
  finally
    InterSectionNodes.Free;
    BoundaryQuadTree.Free;
  end;
end;

procedure TQuadMeshCreator.MergeClosedBoundaries;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  ClosedBoundaries: TBoundaryList;
  GpcPoly: TGpcPolygonClass;
  VertexIndex: Integer;
  MergedPolys: Boolean;
  PolyIndex: Integer;
  NodeTree: TRbwQuadTree;
  NodeIndex: Integer;
  ANode: TNode;
  Epsilon: double;
  ContourIndex: Integer;
  UnionGpcPoly: TGpcPolygonClass;
  NewBoundaries: TBoundaryObjectList;
  NewBoundary: TBoundary;
  APoint: TPoint2D;
  NodeList: TNodeList;
  ASegment: TSegment2D;
  Temp: TGpcPolygonClass;
  index: Integer;
  HasEdgeNode: Boolean;
begin
  ClosedBoundaries := TBoundaryList.Create;
  try
    // skip the first boundary.
    for BoundaryIndex := 1 to FBoundaries.Count - 1 do
    begin
      ABoundary:= FBoundaries[BoundaryIndex];
      if (ABoundary.FNodes.Count > 1) and (ABoundary.FNodes[0]
        = ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
      begin
        HasEdgeNode := False;
        for index := 0 to ABoundary.FNodes.Count - 1 do
        begin
          ANode := ABoundary.FNodes[index];
          HasEdgeNode := FBNodes.IndexOf(ANode) >= 0;
          if HasEdgeNode then
          begin
            break;
          end;
        end;
        if not HasEdgeNode then
        begin
          ClosedBoundaries.Add(ABoundary);
        end;
      end;
    end;
    if ClosedBoundaries.Count > 1 then
    begin
      UnionGpcPoly := nil;
      try
        UnionGpcPoly := TGpcPolygonClass.Create;
        UnionGpcPoly.NumberOfContours := 0;
        for BoundaryIndex := 0 to ClosedBoundaries.Count - 1 do
        begin
          GpcPoly := TGpcPolygonClass.Create;
          GpcPoly.NumberOfContours := 1;
          ABoundary:= ClosedBoundaries[BoundaryIndex];
          GpcPoly.VertexCount[0] := ABoundary.FNodes.Count -1;
          for VertexIndex := 0 to ABoundary.FNodes.Count - 2 do
          begin
            GpcPoly.Vertices[0,VertexIndex] :=
              ABoundary.FNodes[VertexIndex].Location;
          end;
          Temp := UnionGpcPoly;

          try
            UnionGpcPoly := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
              GpcPoly, UnionGpcPoly);
          finally
            Temp.Free;
            GpcPoly.Free;
          end;
        end;


        MergedPolys := UnionGpcPoly.NumberOfContours <> ClosedBoundaries.Count;
        if not MergedPolys then
        begin
          for PolyIndex := 0 to UnionGpcPoly.NumberOfContours - 1 do
          begin
            if UnionGpcPoly.Holes[PolyIndex] then
            begin
              MergedPolys := True;
              Break;
            end;
          end;
        end;
        if MergedPolys then
        begin
          NodeTree := TRbwQuadTree.Create(nil);
          NewBoundaries := TBoundaryObjectList.Create;
          NodeList:= TNodeList.Create;
          try
            NodeTree.XMax := FMaxX;
            NodeTree.XMin := FMinX;
            NodeTree.YMax := FMaxY;
            NodeTree.YMin := FMinY;

            for BoundaryIndex := 0 to ClosedBoundaries.Count - 1 do
            begin
              ABoundary := ClosedBoundaries[BoundaryIndex];
              for NodeIndex := 0 to ABoundary.FNodes.Count - 1 do
              begin
                ANode := ABoundary.FNodes[NodeIndex];
                NodeTree.AddPoint(ANode.X, ANode.Y, ANode);
                if NodeList.IndexOf(ANode) < 0 then
                begin
                  NodeList.Add(ANode);
                end;
              end;
            end;

            for ContourIndex := 0 to UnionGpcPoly.NumberOfContours - 1 do
            begin
              if UnionGpcPoly.Holes[ContourIndex] then
              begin
                Continue;
              end;
              NewBoundary := TBoundary.Create(Self, nil, 100);
              NewBoundary.FDesiredSpacing := 0;
              NewBoundaries.Add(NewBoundary);
              NewBoundary.FNodes.Capacity :=
                UnionGpcPoly.VertexCount[ContourIndex]+1;
              for VertexIndex := 0 to UnionGpcPoly.VertexCount[ContourIndex] - 1 do
              begin
                APoint := UnionGpcPoly.Vertices[ContourIndex, VertexIndex];
                ANode := NodeTree.NearestPointsFirstData(APoint.x, APoint.y);
                NodeList.Remove(ANode);
                Assert(NearlyTheSame(ANode.Location, APoint));
                NewBoundary.FNodes.Add(ANode);
              end;
              NewBoundary.FNodes.Add(NewBoundary.FNodes[0]);
              if NewBoundary.NodeOrientation <> Clockwise then
              begin
                NewBoundary.FNodes.Reverse;
              end;
            end;

            if NodeList.Count > 0 then
            begin
              Epsilon := FCharacteristicLength/1e7;
              for NodeIndex := NodeList.Count - 1 downto 0 do
              begin
                ANode := NodeList[NodeIndex];
                for ContourIndex := 0 to NewBoundaries.Count - 1 do
                begin
                  NewBoundary := NewBoundaries[ContourIndex];
                  for VertexIndex := NewBoundary.FNodes.Count - 2 downto 0 do
                  begin
                    ASegment[1] := NewBoundary.FNodes[VertexIndex].Location;
                    ASegment[2] := NewBoundary.FNodes[VertexIndex+1].Location;
                    if Distance(ANode.Location, ASegment) < Epsilon then
                    begin
                      NewBoundary.FNodes.Insert(VertexIndex+1,ANode);
                      NodeList.Remove(ANode);
                    end;
                  end;
                end;
              end;
            end;

            for NodeIndex := NodeList.Count - 1 downto 0 do
            begin
              ANode := NodeList[NodeIndex];
              FNodes.Remove(ANode);
            end;

            for ContourIndex := 0 to NewBoundaries.Count - 1 do
            begin
              NewBoundary := NewBoundaries[ContourIndex];
              for VertexIndex := 0 to NewBoundary.FNodes.Count - 2 do
              begin
                ANode := NewBoundary.FNodes[VertexIndex];
                if ANode.DesiredSpacing > NewBoundary.DesiredSpacing then
                begin
                  NewBoundary.FDesiredSpacing := ANode.DesiredSpacing;
                end;
              end;
            end;

            // need to delete open boundaries inside closed boundaries here.

            for ContourIndex := 0 to ClosedBoundaries.Count - 1 do
            begin
              FBoundaries.Remove(ClosedBoundaries[ContourIndex]);
            end;
            for ContourIndex := 0 to NewBoundaries.Count - 1 do
            begin
              FBoundaries.Add(NewBoundaries[ContourIndex]);
            end;
            NewBoundaries.OwnsObjects := False;

          finally
            NewBoundaries.Free;
            NodeTree.Free;
            NodeList.Free;
          end;
        end;
      finally
        UnionGpcPoly.Free;
      end;
    end;
  finally
    ClosedBoundaries.Free;
  end;

  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    for NodeIndex := ABoundary.FNodes.Count - 1 downto 1 do
    begin
      if ABoundary.FNodes[NodeIndex] = ABoundary.FNodes[NodeIndex-1] then
      begin
        ABoundary.FNodes.Delete(NodeIndex);
      end;
    end;
  end;

end;

//procedure TQuadMeshCreator.MergeOpenNodeBoundaries;
//var
//  IntersectionNodes: TNodeList;
//  LinkedOpenBoundaries: TBoundaryListSqr;
//  LinkedClosedBoundaries: TBoundaryListSqr;
//  ClosedBoundaries: TBoundaryList;
//  NodeIndex: Integer;
//  ANode: TNode;
//  OpenBoundaryList: TBoundaryList;
//  BoundaryIndex: Integer;
//  ABoundary: TBoundary;
//  OtherNode: TNode;
//  CompareItem: TAngleCompareItem;
//  AngleList: TAngleList;
//  BoundaryNodeIndex: Integer;
//  index: Integer;
//  FirstItem: TAngleCompareItem;
//  FirstBoundary: TBoundary;
//  InsertPosition: Integer;
//  InsertNode: TNode;
//  InsertBoundary: TBoundary;
//  Changed: Boolean;
//  Comparer: TAngleComparer;
//begin
//  IntersectionNodes := TNodeList.Create;
//  LinkedOpenBoundaries:= TBoundaryListSqr.Create;
//  LinkedClosedBoundaries:= TBoundaryListSqr.Create;
//  ClosedBoundaries:= TBoundaryList.Create;
//  try
//    for NodeIndex := 0 to FNodes.Count - 1 do
//    begin
//      ANode := FNodes[NodeIndex];
//      if ANode.FIntersection then
//      begin
//        IntersectionNodes.Add(ANode);
//      end;
//    end;
//    repeat
//      Changed := False;
//      for NodeIndex := 0 to IntersectionNodes.Count - 1 do
//      begin
//        InitializeIntersectLists(IntersectionNodes, LinkedClosedBoundaries,
//          LinkedOpenBoundaries, ClosedBoundaries);
//
//        ANode := IntersectionNodes[NodeIndex];
//        OpenBoundaryList := LinkedOpenBoundaries[NodeIndex];
//        if (OpenBoundaryList.Count > 1) then
//        begin
//          begin
//            Changed := True;
//            AngleList := TAngleList.Create;
//            try
//              for BoundaryIndex := 0 to OpenBoundaryList.Count - 1 do
//              begin
//                ABoundary := OpenBoundaryList[BoundaryIndex];
//                Assert(ABoundary.FNodes.Count > 1);
//                CompareItem := TAngleCompareItem.Create;
//                AngleList.Add(CompareItem);
//                CompareItem.Boundary := ABoundary;
//                CompareItem.NodePosition := ABoundary.FNodes.IndexOf(ANode);
//                if CompareItem.NodePosition = 0 then
//                begin
//                  OtherNode := ABoundary.FNodes[1];
//                  CompareItem.Direction := dForward;
//                end
//                else
//                begin
//                  Assert(CompareItem.NodePosition = ABoundary.FNodes.Count-1);
//                  OtherNode := ABoundary.FNodes[CompareItem.NodePosition-1];
//                  CompareItem.Direction := dBackward;
//                end;
//                CompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
//              end;
//
//              // sort in order of descending angles.
//              Comparer := TAngleComparer.Create;
//              try
//                AngleList.Sort(Comparer) ;
//              finally
//                Comparer.Free;
//              end;
//
////              AngleList.Sort(TAngleComparer.Construct(
////                function (const L, R: TAngleCompareItem): integer
////                begin
////                  result := 0;
////                  if R = L then
////                  begin
////                    Exit;
////                  end;
////                  result := Sign(R.Angle - L.Angle);
////                  if result = 0 then
////                  begin
////                    Result := Ord(R.Direction) - Ord(L.Direction);
////                  end;
////                end
////                )) ;
//
//              FirstItem := AngleList[0];
//              InsertBoundary := FirstItem.Boundary;
//              if FirstItem.NodePosition <> 0 then
//              begin
//                InsertBoundary.FNodes.Reverse;
//                FirstItem.NodePosition := 0;
//              end;
//              InsertPosition := FirstItem.Boundary.FNodes.Count;
//              for BoundaryNodeIndex :=
//                FirstItem.Boundary.FNodes.Count - 2 downto 0 do
//              begin
//                InsertNode := FirstItem.Boundary.FNodes[BoundaryNodeIndex];
//                InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
//                Inc(InsertPosition);
//              end;
//              for index := 1 to AngleList.Count - 1 do
//              begin
//                CompareItem := AngleList[index];
//                if CompareItem.NodePosition = 0 then
//                begin
//                  for BoundaryNodeIndex := 1
//                    to CompareItem.Boundary.FNodes.Count - 1 do
//                  begin
//                    InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
//                    InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
//                    Inc(InsertPosition);
//                  end;
//                  for BoundaryNodeIndex :=
//                    CompareItem.Boundary.FNodes.Count - 2 downto 0 do
//                  begin
//                    InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
//                    InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
//                    Inc(InsertPosition);
//                  end;
//                end
//                else
//                begin
//                  for BoundaryNodeIndex :=
//                    CompareItem.Boundary.FNodes.Count - 2 downto 0 do
//                  begin
//                    InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
//                    InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
//                    Inc(InsertPosition);
//                  end;
//                  for BoundaryNodeIndex := 1
//                    to CompareItem.Boundary.FNodes.Count - 1 do
//                  begin
//                    InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
//                    InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
//                    Inc(InsertPosition);
//                  end;
//                end;
//              end;
//              FirstItem := AngleList[0];
//              FirstBoundary := FirstItem.Boundary;
//              for index := 1 to AngleList.Count - 1 do
//              begin
//                CompareItem := AngleList[index];
//                if (CompareItem.Boundary <> FirstBoundary) then
//                begin
//                  FBoundaries.Remove(CompareItem.Boundary);
//                end;
//              end;
//            finally
//               AngleList.Free;
//            end;
//          end;
//        end;
//      end;
//    until not Changed;
//  finally
//    ClosedBoundaries.Free;
//    LinkedOpenBoundaries.Free;
//    LinkedClosedBoundaries.Free;
//    IntersectionNodes.Free;
//  end;
//end;

//procedure TQuadMeshCreator.MergeOpenNodeBoundaries2;
//var
//  OpenBoundaryList: TBoundaryList;
//  BoundaryIndex: Integer;
//  ABoundary: TBoundary;
//  BoundaryLinkList: TBoundaryList;
//  AngleList: TAngleList;
//  BlindEndsBoundaryList: TBoundaryList;
//  StartNode: TNode;
//  EndNode: TNode;
//  AngleItem: TAngleCompareItem;
//  OtherNode: TNode;
//  StartBoundary: TBoundary;
//  index: Integer;
//  StartNodes: TNodeList;
//  EndNodePos: integer;
//  BIndex: Integer;
//  PriorBoundary: TBoundary;
//  FirstBoundary: TBoundary;
//  NodeIndex: Integer;
//  Changed: boolean;
//  LinkFound: boolean;
//  NewBoundary: TBoundary;
//  Comparer: TAngleComparer;
//begin
//  OpenBoundaryList := TBoundaryList.Create;
//  BlindEndsBoundaryList := TBoundaryList.Create;
//  try
//    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
//    begin
//      ABoundary := FBoundaries[BoundaryIndex];
//      if (ABoundary.FNodes.Count > 1) and (ABoundary.FNodes[0]
//        <> ABoundary.FNodes[ABoundary.FNodes.Count-1]) then
//      begin
//        if ABoundary.FNodes[0].FIntersection
//          and ABoundary.FNodes[ABoundary.FNodes.Count-1].FIntersection  then
//        begin
//          OpenBoundaryList.Add(ABoundary);
//        end
//        else if ABoundary.FNodes[0].FIntersection
//          or ABoundary.FNodes[ABoundary.FNodes.Count-1].FIntersection  then
//        begin
//          BlindEndsBoundaryList.Add(ABoundary);
//        end;
//      end;
//    end;
//    if OpenBoundaryList.Count > 0 then
//    begin
//      BoundaryLinkList := TBoundaryList.Create;
//      AngleList := TAngleList.Create;
//      StartNodes := TNodeList.Create;
//      try
//        Changed:= False;
//        repeat
//          if OpenBoundaryList.Count = 0 then
//          begin
//            Break;
//          end;
//          Changed:= False;
//          StartNodes.Clear;
//          ABoundary := OpenBoundaryList[0];
//          OpenBoundaryList.Delete(0);
//          BoundaryLinkList.Add(ABoundary);
//          StartNode := ABoundary.FNodes[0];
//          StartNodes.Add(StartNode);
//          EndNode := ABoundary.FNodes[ABoundary.FNodes.Count-1];
//          OtherNode := ABoundary.FNodes[ABoundary.FNodes.Count-2];
//          AngleList.Clear;
////          AngleItem := TAngleCompareItem.Create;
////          AngleItem.Direction := dForward;
////          AngleItem.Boundary := ABoundary;
////          AngleItem.NodePosition := ABoundary.FNodes.Count-1;
////          AngleItem.Angle := ArcTan2(OtherNode.Y-EndNode.Y, OtherNode.X-EndNode.X);
////          AngleList.Add(AngleItem);
//          repeat
//            LinkFound := False;
//            AngleList.Clear;
//            StartBoundary := BoundaryLinkList[BoundaryLinkList.Count-1];
//
//            AngleItem := TAngleCompareItem.Create;
//            if StartNode = StartBoundary.FNodes[0] then
//            begin
//              AngleItem.Direction := dForward;
//              AngleItem.NodePosition := 0;
//            end
//            else
//            begin
//              Assert(StartBoundary.FNodes[StartBoundary.FNodes.Count-1] = StartNode);
//              AngleItem.Direction := dBackward;
//              AngleItem.NodePosition := StartBoundary.FNodes.Count-1;
//            end;
//            AngleItem.Boundary := StartBoundary;
//            AngleItem.Angle := ArcTan2(OtherNode.Y-EndNode.Y, OtherNode.X-EndNode.X);
//            AngleList.Add(AngleItem);
//
//            for BoundaryIndex := 0 to OpenBoundaryList.Count - 1 do
//            begin
//              ABoundary := OpenBoundaryList[BoundaryIndex];
//              if BoundaryLinkList.IndexOf(ABoundary) >= 0 then
//              begin
//                Continue;
//              end;
//              AngleItem := nil;
//              if ABoundary.FNodes[0] = EndNode then
//              begin
//                AngleItem := TAngleCompareItem.Create;
//                AngleItem.Direction := dBackward;
//                AngleItem.NodePosition := 0;
//                OtherNode := ABoundary.FNodes[1];
//              end
//              else if ABoundary.FNodes[ABoundary.FNodes.Count-1] = EndNode then
//              begin
//                AngleItem := TAngleCompareItem.Create;
//                AngleItem.Direction := dForward;
//                AngleItem.NodePosition := ABoundary.FNodes.Count-1;
//                OtherNode := ABoundary.FNodes[ABoundary.FNodes.Count-2];
//              end;
//              if AngleItem <> nil then
//              begin
//                AngleList.Add(AngleItem);
//                AngleItem.Boundary := ABoundary;
//                AngleItem.Angle := ArcTan2(OtherNode.Y-EndNode.Y, OtherNode.X-EndNode.X);
//              end;
//            end;
//
//            if AngleList.Count > 1 then
//            begin
//              LinkFound := True;
//              Comparer := TAngleComparer.Create;
//              try
//                AngleList.Sort(Comparer) ;
//              finally
//                Comparer.Free;
//              end;
//
////              AngleList.Sort(TAngleComparer.Construct(
////                function (const L, R: TAngleCompareItem): integer
////                begin
////                  result := 0;
////                  if R = L then
////                  begin
////                    Exit;
////                  end;
////                  result := Sign(R.Angle - L.Angle);
////                  if result = 0 then
////                  begin
////                    Result := Ord(R.Direction) - Ord(L.Direction);
////                  end;
////                end
////                )) ;
//              for index := 0 to AngleList.Count - 1 do
//              begin
//                AngleItem := AngleList[0];
//                if AngleItem.Boundary = StartBoundary then
//                begin
//                  break;
//                end
//                else
//                begin
//                  AngleList.Extract(AngleItem);
//                  AngleList.Add(AngleItem);
//                end;
//              end;
//              AngleItem := AngleList[1];
//              ABoundary := AngleItem.Boundary;
//              BoundaryLinkList.Add(ABoundary);
//              if AngleItem.Direction = dForward then
//              begin
//                StartNode := ABoundary.FNodes[ABoundary.FNodes.Count-1];
//                EndNode := ABoundary.FNodes[0];
//                OtherNode := ABoundary.FNodes[1];
//              end
//              else
//              begin
//                Assert(AngleItem.Direction = dBackward);
//                EndNode := ABoundary.FNodes[ABoundary.FNodes.Count-1];
//                StartNode := ABoundary.FNodes[0];
//                OtherNode := ABoundary.FNodes[ABoundary.FNodes.Count-2];
//              end;
//              StartNodes.Add(StartNode);
//              EndNodePos := StartNodes.IndexOf(EndNode);
//              if EndNodePos >= 0 then
//              begin
//                for BIndex := EndNodePos to BoundaryLinkList.Count - 1 do
//                begin
//                  StartNode := StartNodes[BIndex];
//                  ABoundary := BoundaryLinkList[BIndex];
//                  if ABoundary.FNodes[0] <> StartNode then
//                  begin
//                    ABoundary.FNodes.Reverse;
//                  end;
//                end;
//                PriorBoundary := BoundaryLinkList[BoundaryLinkList.Count - 1];
//                for BIndex := EndNodePos to BoundaryLinkList.Count - 1 do
//                begin
//                  ABoundary := BoundaryLinkList[BIndex];
//                  Assert(PriorBoundary.FNodes[PriorBoundary.FNodes.Count-1]
//                    = ABoundary.FNodes[0]);
//                  PriorBoundary := ABoundary;
//                end;
//                FirstBoundary := BoundaryLinkList[EndNodePos];
//                for BIndex := EndNodePos+1 to BoundaryLinkList.Count - 1 do
//                begin
//                  ABoundary := BoundaryLinkList[BIndex];
//                  for NodeIndex := 1 to ABoundary.FNodes.Count - 1 do
//                  begin
//                    FirstBoundary.FNodes.Add(ABoundary.FNodes[NodeIndex]);
//                  end;
//                end;
//                for BIndex := BoundaryLinkList.Count - 1 downto EndNodePos+1 do
//                begin
//                  ABoundary := BoundaryLinkList[BIndex];
//                  BoundaryLinkList.Delete(BIndex);
//                  OpenBoundaryList.Remove(ABoundary);
//                  FBoundaries.Remove(ABoundary);
//                end;
//                if BoundaryLinkList.Count > 1 then
//                begin
//                  OpenBoundaryList.Add(BoundaryLinkList[0]);
//                end;
//                BoundaryLinkList.Clear;
//
//                NewBoundary := TBoundary.Create(self, FirstBoundary.FParent,
//                  FirstBoundary.FDesiredSpacing);
//                FDuplicateBoundaries.Add(NewBoundary);
//                NewBoundary.FNodes.Capacity := FirstBoundary.FNodes.Count;
//                for NodeIndex := FirstBoundary.FNodes.Count - 1 downto 0 do
//                begin
//                  NewBoundary.FNodes.Add(FirstBoundary.FNodes[NodeIndex]);
//                end;
//
//                Changed := True;
//                break;
//              end;
//            end;
//          until not LinkFound;
//        until (not Changed) and (OpenBoundaryList.Count = 0);
//      finally
//        StartNodes.Free;
//        AngleList.Free;
//        BoundaryLinkList.Free;
//      end;
//    end;
//  finally
//    OpenBoundaryList.Free;
//    BlindEndsBoundaryList.Free;
//  end;
//end;

procedure TQuadMeshCreator.GetConnectionsBetweenBoundaries(
  OpenBoundaryList, BlindEndsBoundaryList: TBoundaryList; NodeList: TNodeList;
  ConnectionsList: TNodeConnectionsObjectList);
var
  ABoundary: TBoundary;
  ANode: TNode;
  Changed: Boolean;
  ConnectionIndex: integer;
  NodeLinks: TNodeConnection;
  ALink: TAngleCompareItem;
  OppositeNode: TNode;
  OtherLinks: TNodeConnection;
  NodeIndex: integer;
  BoundaryIndex: Integer;
  procedure AddConnection;
  var
    NodePosition: Integer;
    NodeLinks: TNodeConnection;
  begin
    NodePosition := NodeList.IndexOf(ANode);
    if NodePosition < 0 then
    begin
      NodePosition := NodeList.Add(ANode);
      Assert(NodePosition = ConnectionsList.
        Add(TNodeConnection.Create(ANode)));
    end;
    NodeLinks := ConnectionsList[NodePosition];
    NodeLinks.AddLink(ABoundary);
  end;
begin
  NodeList.Clear;
  ConnectionsList.Clear;
  for BoundaryIndex := 0 to OpenBoundaryList.Count - 1 do
  begin
    ABoundary := OpenBoundaryList[BoundaryIndex];
    ANode := ABoundary.FNodes.First;
    AddConnection;
    ANode := ABoundary.FNodes.Last;
    AddConnection;
  end;
  repeat
    Changed := False;
    for ConnectionIndex := ConnectionsList.Count - 1 downto 0 do
    begin
      NodeLinks := ConnectionsList[ConnectionIndex];
      if NodeLinks.FConnections.Count = 1 then
      begin
        ALink := NodeLinks.FConnections[0];
        if ALink.Direction = dForward then
        begin
          OppositeNode := ALink.Boundary.FNodes.Last;
        end
        else
        begin
          Assert(ALink.Direction = dBackward);
          OppositeNode := ALink.Boundary.FNodes.First;
        end;
        NodeIndex := NodeList.IndexOf(OppositeNode);
        OtherLinks := ConnectionsList[NodeIndex];
        OtherLinks.RemoveLink(ALink.Boundary);
        if BlindEndsBoundaryList.IndexOf(ALink.Boundary) < 0 then
        begin
          BlindEndsBoundaryList.Add(ALink.Boundary);
        end;
      end;
      if NodeLinks.FConnections.Count <= 1 then
      begin
        ConnectionsList.Delete(ConnectionIndex);
        NodeList.Delete(ConnectionIndex);
        Changed := True;
      end;
    end;
  until (not Changed);
  for ConnectionIndex := 0 to ConnectionsList.Count - 1 do
  begin
    ConnectionsList[ConnectionIndex].Sort;
  end;
end;

procedure TQuadMeshCreator.MergeOpenNodeBoundaries3;
var
  OpenBoundaryList: TBoundaryList;
  BlindEndsBoundaryList: TBoundaryList;
  ConnectionsList: TNodeConnectionsObjectList;
  NodeList: TNodeList;
  Changed: Boolean;
  NodeIndex: integer;
  TracedNodes: TNodeList;
  TracedBoundaries: TBoundaryList;
  ANode: TNode;
  NodeConnections: TNodeConnection;
  BoundaryIndex: integer;
  ConnectItem: TAngleCompareItem;
  StartPosition: integer;
  BIndex: integer;
  StartNode: TNode;
  ABoundary: TBoundary;
  PriorBoundary: TBoundary;
  FirstBoundary: TBoundary;
  InnerNodeIndex: integer;
  NewBoundary: TBoundary;
  IntersectionNodes: TNodeList;
  BlindBoundaryIndex: integer;
  AngleList: TAngleList;
  OtherNode: TNode;
  CompareItem: TAngleCompareItem;
  Comparer: TAngleComparer;
  CompareIndex: integer;
  index: integer;
  InsertBoundary: TBoundary;
  InsertPosition: integer;
  SameBoundary: boolean;
  InsertIndex: integer;
  NodePosition: integer;
  function TraceBoundary(TracedNodeList: TNodeList;
    BoundaryList: TBoundaryList): boolean;
  var
    LastNode: TNode;
    NodePosition: Integer;
    NodeConnections: TNodeConnection;
    LastBoundary: TBoundary;
    BoundaryPosition: Integer;
    BoundaryIndex: integer;
    function EvaluateConnection: Boolean;
    var
      AConnection: TAngleCompareItem;
      NextNode: TNode;
    begin
//      result := False;
      AConnection := NodeConnections.FConnections[BoundaryIndex];
      if AConnection.Boundary.FNodes.First = LastNode then
      begin
        NextNode := AConnection.Boundary.FNodes.Last;
      end
      else
      begin
        NextNode := AConnection.Boundary.FNodes.First;
        Assert(LastNode = AConnection.Boundary.FNodes.Last);
      end;
      if TracedNodeList.IndexOf(NextNode) >= 0 then
      begin
        result := True;
        TracedNodeList.Add(LastNode);
        TracedNodeList.Add(NextNode);
        BoundaryList.Add(AConnection.Boundary);
        Exit;
      end;
      TracedNodeList.Add(LastNode);
      BoundaryList.Add(AConnection.Boundary);
      Result := TraceBoundary(TracedNodeList, BoundaryList);
      if result then
      begin
        Exit;
      end;
      TracedNodeList.Delete(TracedNodeList.Count-1);
      BoundaryList.Delete(BoundaryList.Count-1);
    end;
  begin
    result := False;
    Assert(TracedNodeList.Count > 0);
    Assert(TracedNodeList.Count = BoundaryList.Count);
    LastNode := TracedNodeList.Last;
    LastBoundary := BoundaryList.Last;
    if LastBoundary.FNodes.Last = LastNode then
    begin
      LastBoundary.FNodes.Reverse;
    end;
    Assert(LastBoundary.FNodes.First = LastNode);
    LastNode := LastBoundary.FNodes.Last;
    NodePosition := NodeList.IndexOf(LastNode);
    Assert(NodePosition >= 0);
    NodeConnections := ConnectionsList[NodePosition];

    BoundaryPosition := -1;
    for BoundaryIndex := 0 to NodeConnections.FConnections.Count - 1 do
    begin
      if NodeConnections.FConnections[BoundaryIndex].Boundary = LastBoundary then
      begin
        BoundaryPosition := BoundaryIndex;
        break;
      end;
    end;
    Assert(BoundaryPosition >= 0);
    for BoundaryIndex := BoundaryPosition+1 to
      NodeConnections.FConnections.Count - 1 do
    begin
      Result := EvaluateConnection;
      if result then
      begin
        Exit;
      end;
    end;
    for BoundaryIndex := 0 to BoundaryPosition - 1 do
    begin
      Result := EvaluateConnection;
      if result then
      begin
        Exit;
      end;
    end;
  end;
  procedure InsertMoreBlindBoundaries(NodePosition: Integer);
  var
    ANode: TNode;
    AngleList: TAngleList;
    ABoundary: TBoundary;
    SameBoundary: Boolean;
    CompareIndex: integer;
    Comparer: TAngleComparer;
    index: integer;
    BlindBoundaryIndex: integer;
    InsertIndex: Integer;
  begin
    ANode := InsertBoundary.FNodes[NodePosition];
    if not ANode.FIntersection then
    begin
      exit;
    end;
    AngleList := nil;
    try
      AngleList := TAngleList.Create;

      for BlindBoundaryIndex := BlindEndsBoundaryList.Count - 1 downto 0 do
      begin
        if BlindBoundaryIndex >= BlindEndsBoundaryList.Count then
        begin
          Continue;
        end;
        ABoundary := BlindEndsBoundaryList[BlindBoundaryIndex];
        CompareItem := nil;
        if ABoundary.FNodes.Last = ANode then
        begin
          ABoundary.FNodes.Reverse;
        end;
        if ABoundary.FNodes.First = ANode then
        begin
          SameBoundary := False;
          for CompareIndex := 0 to AngleList.Count - 1 do
          begin
            if AngleList[CompareIndex].Boundary = ABoundary then
            begin
              SameBoundary := True;
              break;
            end;
          end;
          if SameBoundary then
          begin
            Continue;
          end;

          BlindEndsBoundaryList.Delete(BlindBoundaryIndex);

          CompareItem := TAngleCompareItem.Create;
          CompareItem.Direction := dForward;
          CompareItem.Boundary := ABoundary;
          CompareItem.NodePosition := 0;
          OtherNode := ABoundary.FNodes[1];
          CompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
          AngleList.Add(CompareItem);
        end
      end;

      if AngleList.Count > 0 then
      begin
        CompareItem := TAngleCompareItem.Create;
        CompareItem.Direction := dBackward;
        CompareItem.Boundary := InsertBoundary;
        CompareItem.NodePosition := NodePosition;
        if NodePosition = 0 then
        begin
          OtherNode := InsertBoundary.FNodes[InsertBoundary.FNodes.Count-2];
        end
        else
        begin
          OtherNode := InsertBoundary.FNodes[NodePosition-1];
        end;

        CompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
        AngleList.Add(CompareItem);

        Comparer := TAngleComparer.Create;
        try
          AngleList.Sort(Comparer);
        finally
          Comparer.Free;
        end;

//        AngleList.Reverse;
        for index := 0 to AngleList.Count - 1 do
        begin
          CompareItem := AngleList[0];
          if (CompareItem.Boundary = InsertBoundary) then
          begin
            break;
          end;
          AngleList.Extract(CompareItem);
          AngleList.Add(CompareItem)
        end;

        for index := 1 to AngleList.Count - 1 do
        begin
          CompareItem := AngleList[index];

          ABoundary := CompareItem.Boundary;

//          Inc(InsertPosition);
          for InsertIndex := 1 to ABoundary.FNodes.Count - 1 do
          begin
            InsertBoundary.FNodes.Insert(InsertPosition, ABoundary.FNodes[InsertIndex]);
            Inc(InsertPosition);
          end;
          InsertMoreBlindBoundaries(InsertPosition-1);
          for InsertIndex := ABoundary.FNodes.Count - 2 downto 0 do
          begin
            InsertBoundary.FNodes.Insert(InsertPosition, ABoundary.FNodes[InsertIndex]);
            Inc(InsertPosition);
          end;

          FBoundaries.Remove(ABoundary);
        end
      end;

    finally
      AngleList.Free;
    end;
  end;
begin
  OpenBoundaryList := TBoundaryList.Create;
  BlindEndsBoundaryList := TBoundaryList.Create;
  ConnectionsList := TNodeConnectionsObjectList.Create;
  NodeList := TNodeList.Create;
  TracedNodes := TNodeList.Create;
  TracedBoundaries := TBoundaryList.Create;
  try
    repeat
      Changed := False;
      GetConnectedOpenBoundaries(OpenBoundaryList, BlindEndsBoundaryList);
      if OpenBoundaryList.Count > 0 then
      begin
        GetConnectionsBetweenBoundaries(OpenBoundaryList, BlindEndsBoundaryList,
          NodeList, ConnectionsList);

        TracedNodes.Clear;
        TracedBoundaries.Clear;
        for NodeIndex := 0 to NodeList.Count - 1 do
        begin
          ANode := NodeList[NodeIndex];
          NodeConnections := ConnectionsList[NodeIndex];
          for BoundaryIndex := 0 to NodeConnections.FConnections.Count - 1 do
          begin
            ConnectItem := NodeConnections.FConnections[BoundaryIndex];
            Assert(TracedNodes.Count = 0);
            Assert(TracedBoundaries.Count = 0);
            TracedNodes.Add(ANode);
            TracedBoundaries.Add(ConnectItem.Boundary);
            if TraceBoundary(TracedNodes, TracedBoundaries) then
            begin
              StartPosition := TracedNodes.IndexOf(TracedNodes.Last);
              Assert(StartPosition < TracedNodes.Count -1);

              for BIndex := StartPosition to TracedBoundaries.Count - 1 do
              begin
                StartNode := TracedNodes[BIndex];
                ABoundary := TracedBoundaries[BIndex];
                if ABoundary.FNodes.First <> StartNode then
                begin
                  ABoundary.FNodes.Reverse;
                end;
              end;
              PriorBoundary := TracedBoundaries[TracedBoundaries.Count - 1];
              for BIndex := StartPosition to TracedBoundaries.Count - 1 do
              begin
                ABoundary := TracedBoundaries[BIndex];
                Assert(PriorBoundary.FNodes.Last = ABoundary.FNodes.First);
                PriorBoundary := ABoundary;
              end;
              FirstBoundary := TracedBoundaries[StartPosition];
              for BIndex := StartPosition+1 to TracedBoundaries.Count - 1 do
              begin
                ABoundary := TracedBoundaries[BIndex];
                for InnerNodeIndex := 1 to ABoundary.FNodes.Count - 1 do
                begin
                  FirstBoundary.FNodes.Add(ABoundary.FNodes[InnerNodeIndex]);
                end;
              end;
              for BIndex := TracedBoundaries.Count - 1 downto StartPosition+1 do
              begin
                ABoundary := TracedBoundaries[BIndex];
                TracedBoundaries.Delete(BIndex);
                OpenBoundaryList.Remove(ABoundary);
                FBoundaries.Remove(ABoundary);
              end;

              if FirstBoundary.NodeOrientation = CounterClockwise then
              begin
                FirstBoundary.FNodes.Reverse;
              end;

              NewBoundary := TBoundary.Create(self, FirstBoundary.FParent,
                FirstBoundary.FDesiredSpacing);
              FDuplicateBoundaries.Add(NewBoundary);
              NewBoundary.FNodes.Capacity := FirstBoundary.FNodes.Count;
              for InnerNodeIndex := FirstBoundary.FNodes.Count - 1 downto 0 do
              begin
                NewBoundary.FNodes.Add(FirstBoundary.FNodes[InnerNodeIndex]);
              end;

              IntersectionNodes := TNodeList.Create;
              AngleList := TAngleList.Create;
              try
                for InnerNodeIndex := 0 to FirstBoundary.FNodes.Count - 2 do
                begin
                  ANode := FirstBoundary.FNodes[InnerNodeIndex];
                  if ANode.FIntersection then
                  begin
                    Assert(IntersectionNodes.IndexOf(ANode) < 0);
                    IntersectionNodes.Add(ANode);
                  end;
                end;

                for InnerNodeIndex := 0 to IntersectionNodes.Count - 1 do
                begin
                  ANode := IntersectionNodes[InnerNodeIndex];
                  AngleList.Clear;
                  for BlindBoundaryIndex := BlindEndsBoundaryList.Count - 1 downto 0 do
                  begin
                    if BlindBoundaryIndex >= BlindEndsBoundaryList.Count then
                    begin
                      Continue;
                    end;
                    ABoundary := BlindEndsBoundaryList[BlindBoundaryIndex];
                    CompareItem := nil;
                    if ABoundary.FNodes.Last = ANode then
                    begin
                      ABoundary.FNodes.Reverse;
                    end;
                    if ABoundary.FNodes.First = ANode then
                    begin
                      SameBoundary := False;
                      for CompareIndex := 0 to AngleList.Count - 1 do
                      begin
                        if AngleList[CompareIndex].Boundary = ABoundary then
                        begin
                          SameBoundary := True;
                          break;
                        end;
                      end;
                      if SameBoundary then
                      begin
                        Continue;
                      end;

                      BlindEndsBoundaryList.Delete(BlindBoundaryIndex);

                      CompareItem := TAngleCompareItem.Create;
                      CompareItem.Direction := dForward;
                      CompareItem.Boundary := ABoundary;
                      CompareItem.NodePosition := 0;
                      OtherNode := ABoundary.FNodes[1];
                      CompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
                      AngleList.Add(CompareItem);
                    end
                  end;

                  if AngleList.Count > 0 then
                  begin
                    NodePosition := FirstBoundary.FNodes.IndexOf(ANode);

                    CompareItem := TAngleCompareItem.Create;
                    CompareItem.Direction := dBackward;
                    CompareItem.Boundary := FirstBoundary;
                    CompareItem.NodePosition := NodePosition;
                    if NodePosition = 0 then
                    begin
                      OtherNode := FirstBoundary.FNodes[FirstBoundary.FNodes.Count-2];
                    end
                    else
                    begin
                      OtherNode := FirstBoundary.FNodes[NodePosition-1];
                    end;
                    CompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
                    AngleList.Add(CompareItem);

                    CompareItem := TAngleCompareItem.Create;
                    CompareItem.Direction := dForward;
                    CompareItem.Boundary := FirstBoundary;
                    CompareItem.NodePosition := NodePosition;
                    OtherNode := FirstBoundary.FNodes[NodePosition+1];
                    CompareItem.Angle := ArcTan2(OtherNode.Y- ANode.Y, OtherNode.X- ANode.X);
                    AngleList.Add(CompareItem);

                    Comparer := TAngleComparer.Create;
                    try
                      AngleList.Sort(Comparer);
                    finally
                      Comparer.Free;
                    end;
//                    AngleList.Reverse;

                    for index := 0 to AngleList.Count - 1 do
                    begin
                      CompareItem := AngleList[0];
                      if (CompareItem.Direction = dBackward) then
                      begin
                        Assert(CompareItem.Boundary = FirstBoundary);
                        break;
                      end;
                      AngleList.Extract(CompareItem);
                      AngleList.Add(CompareItem)
                    end;

                    InsertBoundary := FirstBoundary;
                    InsertPosition := -1;
                    for index := 1 to AngleList.Count - 1 do
                    begin
                      CompareItem := AngleList[index];
                      if CompareItem.Boundary = FirstBoundary then
                      begin
                        InsertBoundary := NewBoundary;
                        InsertPosition := -1;
                        continue;
                      end;

                      ABoundary := CompareItem.Boundary;
                      if InsertPosition < 0 then
                      begin
                        InsertPosition := InsertBoundary.FNodes.
                          IndexOf(ABoundary.FNodes.First);
                        Assert(InsertPosition >= 0);
                        Inc(InsertPosition);
                      end;
                      for InsertIndex := 1 to ABoundary.FNodes.Count - 1 do
                      begin
                        InsertBoundary.FNodes.Insert(InsertPosition, ABoundary.FNodes[InsertIndex]);
                        Inc(InsertPosition);
                      end;
                      InsertMoreBlindBoundaries(InsertPosition-1);
                      for InsertIndex := ABoundary.FNodes.Count - 2 downto 0 do
                      begin
                        InsertBoundary.FNodes.Insert(InsertPosition, ABoundary.FNodes[InsertIndex]);
                        Inc(InsertPosition);
                      end;
                      FBoundaries.Remove(ABoundary);
                    end;
                  end;

                end;

              finally
                IntersectionNodes.Free;
                AngleList.Free;
              end;

              Changed := True;
              break;
            end;
          end;
          if Changed then
          begin
            break;
          end;
        end;

      end;


    until not Changed;
  finally
    TracedNodes.Free;
    TracedBoundaries.Free;
    NodeList.Free;
    ConnectionsList.Free;
    BlindEndsBoundaryList.Free;
    OpenBoundaryList.Free;
  end;
end;

procedure TQuadMeshCreator.MergeOpenWithClosedBoundaries;
var
  IntersectionNodes: TNodeList;
  LinkedOpenBoundaries: TBoundaryListSqr;
  LinkedClosedBoundaries: TBoundaryListSqr;
  ClosedBoundaries: TBoundaryList;
  NodeIndex: Integer;
  ANode: TNode;
  OpenBoundaryList: TBoundaryList;
  ClosedBoundaryList: TBoundaryList;
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  OtherNode: TNode;
  CompareItem: TAngleCompareItem;
  AngleList: TAngleList;
  PolyList: TList<TPolygon2D>;
  APoly: TPolygon2D;
  BoundaryNodeIndex: Integer;
  ForwardCompareItem: TAngleCompareItem;
  BackwardCompareItem: TAngleCompareItem;
  index: Integer;
  AClosedBoundary: TBoundary;
  InnerItemIndex: Integer;
  FirstItem: TAngleCompareItem;
  FirstBoundary: TBoundary;
  InsertPosition: Integer;
  InsertNode: TNode;
  InsertIndexList: TIntegerList;
  InsertBoundary: TBoundary;
  InnerPolyList: TList<TPolygon2D>;
  BIndex: Integer;
  StoredIndex: Integer;
  StartIndex: Integer;
  Changed: Boolean;
begin
  IntersectionNodes := TNodeList.Create;
  LinkedOpenBoundaries:= TBoundaryListSqr.Create;
  LinkedClosedBoundaries:= TBoundaryListSqr.Create;
  ClosedBoundaries:= TBoundaryList.Create;
  PolyList := TList<TPolygon2D>.Create;
  InnerPolyList := TList<TPolygon2D>.Create;
  try
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      if ANode.FIntersection then
      begin
        IntersectionNodes.Add(ANode);
      end;
    end;
    repeat
      Changed := False;
      for NodeIndex := 0 to IntersectionNodes.Count - 1 do
      begin
        InitializeIntersectLists(IntersectionNodes, LinkedClosedBoundaries,
          LinkedOpenBoundaries, ClosedBoundaries);

        if (NodeIndex = 0) then
        begin
          PolyList.Capacity := ClosedBoundaries.Count;
          for BoundaryIndex := 0 to ClosedBoundaries.Count - 1 do
          begin
            ABoundary := ClosedBoundaries[BoundaryIndex];
            SetLength(APoly, ABoundary.FNodes.Count-1);
            for BoundaryNodeIndex := 0 to ABoundary.FNodes.Count - 2 do
            begin
              APoly[BoundaryNodeIndex] := ABoundary.FNodes[BoundaryNodeIndex].Location;
            end;
            PolyList.Add(APoly);
          end;
        end;

        ANode := IntersectionNodes[NodeIndex];
        OpenBoundaryList := LinkedOpenBoundaries[NodeIndex];
        if (OpenBoundaryList.Count > 0) then
        begin
          ClosedBoundaryList := LinkedClosedBoundaries[NodeIndex];
          if (ClosedBoundaryList.Count > 0) then
          begin
            Changed := True;
            AngleList := TAngleList.Create;
            try
              for BoundaryIndex := 0 to OpenBoundaryList.Count - 1 do
              begin
                ABoundary := OpenBoundaryList[BoundaryIndex];
                Assert(ABoundary.FNodes.Count > 1);
                CompareItem := TAngleCompareItem.Create;
                AngleList.Add(CompareItem);
                CompareItem.Boundary := ABoundary;
                CompareItem.NodePosition := ABoundary.FNodes.IndexOf(ANode);
                if CompareItem.NodePosition = 0 then
                begin
                  OtherNode := ABoundary.FNodes[1];
                  CompareItem.Direction := dForward;
                end
                else
                begin
                  Assert(CompareItem.NodePosition = ABoundary.FNodes.Count-1);
                  OtherNode := ABoundary.FNodes[CompareItem.NodePosition-1];
                  CompareItem.Direction := dBackward;
                end;
                CompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
              end;

              for BoundaryIndex := 0 to ClosedBoundaryList.Count - 1 do
              begin
                ABoundary := ClosedBoundaryList[BoundaryIndex];
                if ABoundary.NodeOrientation <> Clockwise then
                begin
                  ABoundary.FNodes.Reverse;
                end;
                Assert(ABoundary.FNodes.Count > 1);
                ForwardCompareItem := TAngleCompareItem.Create;
                AngleList.Add(ForwardCompareItem);
                ForwardCompareItem.Boundary := ABoundary;
                ForwardCompareItem.NodePosition := ABoundary.FNodes.IndexOf(ANode);
                ForwardCompareItem.Direction := dForward;
                OtherNode := ABoundary.FNodes[ForwardCompareItem.NodePosition+1];
                ForwardCompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
                if ANode.Y=OtherNode.Y then
                begin
                  ForwardCompareItem.Angle := -ForwardCompareItem.Angle;
                end;


                BackwardCompareItem := TAngleCompareItem.Create;
                AngleList.Add(BackwardCompareItem);
                BackwardCompareItem.Boundary := ABoundary;
                BackwardCompareItem.NodePosition := ForwardCompareItem.NodePosition;
                BackwardCompareItem.Direction := dBackward;
                if BackwardCompareItem.NodePosition = 0 then
                begin
                  OtherNode := ABoundary.FNodes[ABoundary.FNodes.Count-2];
                end
                else
                begin
                  OtherNode := ABoundary.FNodes[BackwardCompareItem.NodePosition-1];
                end;
                BackwardCompareItem.Angle := ArcTan2(OtherNode.Y-ANode.Y, OtherNode.X-ANode.X);
                if ANode.Y=OtherNode.Y then
                begin
                  BackwardCompareItem.Angle := -BackwardCompareItem.Angle;
                end;
              end;

              // sort in order of descending angles.
              AngleList.Sort(TComparer<TAngleCompareItem>.Construct(
                function (const L, R: TAngleCompareItem): integer
                var
                  LIndex: Integer;
                  RIndex: Integer;
                begin
                  result := 0;
                  if R = L then
                  begin
                    Exit;
                  end;
                  result := Sign(R.Angle - L.Angle);
                  if result = 0 then
                  begin
                    LIndex := ClosedBoundaryList.IndexOf(L.Boundary);
                    RIndex := ClosedBoundaryList.IndexOf(R.Boundary);
                    if (LIndex >= 0) and (RIndex >= 0) then
                    begin
                      result := LIndex - RIndex;
                    end;
                  end;
                  if result = 0 then
                  begin
                    Result := -(Ord(R.Direction) - Ord(L.Direction));
                  end;
                end
                )) ;

              // Reverse AngleList if adding points to the inside of the
              // closed boundaries.
              if ClosedBoundaryList.Count > 1 then
              begin
                AngleList.Reverse;
              end;

              AClosedBoundary := nil;
              if ClosedBoundaryList.Count > 0 then
              begin
                AClosedBoundary := ClosedBoundaryList[0];
                for index := 0 to AngleList.Count - 1 do
                begin
                  CompareItem := AngleList[index];
                  if (CompareItem.Boundary = AClosedBoundary)
                    and (CompareItem.Direction = dBackward) then
                  begin
                    for InnerItemIndex := 0 to index - 1 do
                    begin
                      CompareItem := AngleList[0];
                      AngleList.Extract(CompareItem);
                      AngleList.Add(CompareItem);
                    end;
                    break;
                  end;
                end;
              end;
              InsertIndexList := TIntegerList.Create;
              try
                InnerPolyList.Clear;
                for BoundaryIndex := 0 to ClosedBoundaryList.Count - 1 do
                begin
                  ABoundary := ClosedBoundaryList[BoundaryIndex];
                  InsertIndexList.Add(ABoundary.FNodes.IndexOf(ANode)+1);
                  InnerPolyList.Add(PolyList[ClosedBoundaries.IndexOf(ABoundary)]);
                end;
                StoredIndex := -1;
                if ClosedBoundaryList.Count > 0 then
                begin
                  StartIndex := 0;
                  InsertPosition := -1;
                  InsertBoundary := nil;
                end
                else
                begin
                  StartIndex := 1;
                  FirstItem := AngleList[0];
                  InsertBoundary := FirstItem.Boundary;
                  InsertPosition := FirstItem.NodePosition+1;
                end;
                for index := StartIndex to AngleList.Count - 1 do
                begin
                  CompareItem := AngleList[index];
                  if ClosedBoundaryList.IndexOf(CompareItem.Boundary) >= 0 then
                  begin
                    Continue;
                  end;
                  if ClosedBoundaryList.Count > 0 then
                  begin
                    if ClosedBoundaryList.Count > 1 then
                    begin
                      if CompareItem.NodePosition = 0 then
                      begin
                        OtherNode := CompareItem.Boundary.FNodes[1];
                      end
                      else
                      begin
                        Assert(CompareItem.NodePosition = CompareItem.Boundary.FNodes.Count-1);
                        OtherNode := CompareItem.Boundary.FNodes[CompareItem.NodePosition-1];
                      end;
                      StoredIndex := -1;
                      for BIndex := 0 to ClosedBoundaryList.Count - 1 do
                      begin
                        InsertBoundary := nil;
                        APoly := InnerPolyList[BIndex];
                        if PointInConcavePolygon(OtherNode.Location, APoly) then
                        begin
                          InsertBoundary := ClosedBoundaryList[BIndex];
                          InsertPosition := InsertIndexList[BIndex];
                          StoredIndex := BIndex;
                          break;
                        end;
                      end;
                    end
                    else
                    begin
                      InsertBoundary := ClosedBoundaryList[0];
                      InsertPosition := InsertIndexList[0];
                      StoredIndex := 0;
                    end;
                    Assert(StoredIndex >= 0);
                  end;
                  if CompareItem.NodePosition = 0 then
                  begin
                    for BoundaryNodeIndex := 1
                      to CompareItem.Boundary.FNodes.Count - 1 do
                    begin
                      InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
                      InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
                      Inc(InsertPosition);
                    end;
                    for BoundaryNodeIndex :=
                      CompareItem.Boundary.FNodes.Count - 2 downto 0 do
                    begin
                      InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
                      InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
                      Inc(InsertPosition);
                    end;
                  end
                  else
                  begin
                    for BoundaryNodeIndex :=
                      CompareItem.Boundary.FNodes.Count - 2 downto 0 do
                    begin
                      InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
                      InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
                      Inc(InsertPosition);
                    end;
                    for BoundaryNodeIndex := 1
                      to CompareItem.Boundary.FNodes.Count - 1 do
                    begin
                      InsertNode := CompareItem.Boundary.FNodes[BoundaryNodeIndex];
                      InsertBoundary.FNodes.Insert(InsertPosition, InsertNode);
                      Inc(InsertPosition);
                    end;
                  end;
                  if ClosedBoundaryList.Count > 0 then
                  begin
                    InsertIndexList[StoredIndex] := InsertPosition;
                  end;
                end;
              finally
                InsertIndexList.Free;
              end;
              if ClosedBoundaryList.Count > 0 then
              begin
                for index := 0 to AngleList.Count - 1 do
                begin
                  CompareItem := AngleList[index];
                  if (ClosedBoundaryList.IndexOf(CompareItem.Boundary) < 0) then
                  begin
                    FBoundaries.Remove(CompareItem.Boundary);
                  end;
                end;
              end
              else
              begin
                FirstItem := AngleList[0];
                FirstBoundary := FirstItem.Boundary;
                for index := 1 to AngleList.Count - 1 do
                begin
                  CompareItem := AngleList[index];
                  if (CompareItem.Boundary <> FirstBoundary) then
                  begin
                    FBoundaries.Remove(CompareItem.Boundary);
                  end;
                end;
              end;
            finally
               AngleList.Free;
            end;
          end;
        end;
      end;
    until not Changed;

  finally
    InnerPolyList.Free;
    PolyList.Free;
    ClosedBoundaries.Free;
    LinkedOpenBoundaries.Free;
    LinkedClosedBoundaries.Free;
    IntersectionNodes.Free;
  end;
end;

procedure TQuadMeshCreator.RenumberNodesAndElements;
  procedure AddElements(ABoundary: TBoundary);
  var
    Index: Integer;
  begin
    if ABoundary.SubPartCount > 0 then
    begin
      for Index := 0 to ABoundary.SubPartCount - 1 do
      begin
        AddElements(ABoundary.SubParts[Index]);
      end;
      ABoundary.FSubParts.OwnsObjects := False;
      ABoundary.Free;
    end
    else
    begin
      if ABoundary.NodeCount > 0 then
      begin
        ABoundary.SetCounterClockwiseOrientation;
        FElementList.Add(ABoundary);
      end
      else
      begin
        ABoundary.Free;
      end;
    end;
  end;

var
  NodeIndex: Integer;
  BoundaryIndex: Integer;
  ElementIndex: Integer;
begin
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    AddElements(FBoundaries[BoundaryIndex]);
  end;

  FBoundaries.OwnsObjects := False;

  for NodeIndex := 0 to NodeCount - 1 do
  begin
    FNodeList.Add(NodeObjects[NodeIndex]);
  end;
  FNodes.OwnsObjects := False;

  for ElementIndex := 0 to FElementList.Count - 1 do
  begin
    FElementList[ElementIndex].ElementNumber := ElementIndex;
  end;
  for NodeIndex := 0 to FNodeList.Count - 1 do
  begin
    FNodeList[NodeIndex].NodeNumber := NodeIndex;
  end;

  case RenumberingAlgorithm of
    raNone: ;
    CuthillMcKee: CuthillMcKeeRenumbering.RenumberMesh(self);
    raSloanRandolph: MeshRenumbering.RenumberMesh(self);
    else Assert(False);
  end;


  FElementList.Sort(TIElementComparer.Construct(
    function(const L, R: IElement): Integer
    begin
      result := L.ElementNumber - R.ElementNumber;
    end));

  FNodeList.Sort(TINodeComparer.Construct(
    function(const L, R: INode): Integer
    begin
      result := L.NodeNumber - R.NodeNumber;
    end));

//  frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;

end;

procedure TQuadMeshCreator.SetNodeAdjustmentMethod
  (const Value: TNodeAdjustmentMethod);
begin
  FNodeAdjustmentMethod := Value;
end;

procedure TQuadMeshCreator.SetNodeTypes;
var
  Index: Integer;
begin
  Assert(FBoundaries.Count > 0);
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].SetNodeType(ntSubDomain)
  end;
  for index := 0 to FBNodes.Count - 1 do
  begin
    FBNodes[index].FNodeType := ntEdge;
  end;
end;

procedure TQuadMeshCreator.SetRenumberingAlgorithm(
  const Value: TRenumberingAlgorithm);
begin
  FRenumberingAlgorithm := Value;
end;

procedure TQuadMeshCreator.SetSixNodeClosureMethod
  (const Value: TSixNodeClosureMethod);
begin
  FSixNodeClosureMethod := Value;
end;

procedure TQuadMeshCreator.FixSegments;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
begin
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    ABoundary.FixSegments;
  end;
end;

procedure TQuadMeshCreator.SplitMultiplyConnectedBoundaries;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
begin
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    ABoundary.SplitMultiplyConnectedBoundary;
//    ABoundary.SplitMultiplyConnectedBoundary2;
  end;
end;

function TQuadMeshCreator._AddRef: Integer;
begin
  result := 1;
  // do nothing
end;

function TQuadMeshCreator._Release: Integer;
begin
  result := 1;
  // do nothing
end;

{ TNode }

Type
  TTriangeleCalculationValues = record
    P: double;
    Q: double;
    b: double;
    M: TPoint2D;
    APrime: double;
  end;

function TNode.ValidPosition(ALocation: TPoint2D): boolean;
var
  PolyIndex: Integer;
begin
  Assert(Length(FQuadMeshCreator.FPolygonArray) > 0);
  result := PointInConcavePolygon(ALocation, FQuadMeshCreator.FPolygonArray[0]);
  if result then
  begin
    for PolyIndex := 1 to Length(FQuadMeshCreator.FPolygonArray) - 1 do
    begin
      result := not PointInConcavePolygon(ALocation,
        FQuadMeshCreator.FPolygonArray[PolyIndex]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TNode.WorstAngles(var HighestAngle, LowestAngle: double);
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  FirstAngle: Boolean;
  PriorNode: TNode;
  CurrentNode: TNode;
  NodeIndex: Integer;
  NextNode: TNode;
  AnAngle: TFloat;
begin
  FirstAngle := True;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    AnElement.SetCounterClockwiseOrientation;
    Assert(AnElement.Count = 5);
    PriorNode := AnElement[3].FNode;
    CurrentNode := AnElement[0].FNode;
    for NodeIndex := 0 to AnElement.Count - 2 do
    begin
      NextNode := AnElement[NodeIndex+1].FNode;
      AnAngle := OrientedVertexAngle(PriorNode.Location, CurrentNode.Location,
        NextNode.Location, CounterClockwise);
      if AnAngle < 0 then
      begin
        AnAngle := AnAngle + 360;
      end;
      if FirstAngle then
      begin
        HighestAngle := AnAngle;
        LowestAngle := AnAngle;
        FirstAngle:= False;
      end
      else
      begin
        if HighestAngle < AnAngle then
        begin
          HighestAngle := AnAngle;
        end;
        if LowestAngle > AnAngle then
        begin
          LowestAngle := AnAngle;
        end;
      end;
      PriorNode := CurrentNode;
      CurrentNode := NextNode;
    end;
  end;
end;

procedure TNode.AdjustPositionGiuliani;
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  NodeIndex: Integer;
  NeighborIndexK: Integer;
  NeighborK: TNode;
  NeighborIndexJ: Integer;
  NeighborJ: TNode;
  R: double;
  TotalArea: double;
  TotalBase: double;
  MeanHeight: double;
  MeanBase: double;
  W1: double;
  W2: Extended;
  TriVals: array of TTriangeleCalculationValues;
  S1: double;
  S2: double;
  S3: double;
  S4: double;
  S5: double;
  Index: Integer;
  L: TPoint2D;
  OldLocation: TPoint2D;
  NewLocation: TPoint2D;
  OldHighestAngle: Double;
  OldLowestAngle: Double;
  NewHighestAngle: Double;
  NewLowestAngle: Double;
begin
  if FNodeType <> ntInner then
  begin
    Exit;
  end;
  TotalArea := 0.0;
  TotalBase := 0.0;
  SetLength(TriVals, FElements.Count);
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    NodeIndex := AnElement.IndexOfNode(self);
    Assert(NodeIndex >= 0);
    NeighborIndexK := NodeIndex + 1;
    Assert(NeighborIndexK < AnElement.Count);
    NeighborK := AnElement[NeighborIndexK].FNode;

    NeighborIndexJ := NodeIndex - 1;
    if NeighborIndexJ < 0 then
    begin
      Assert(AnElement[0] = AnElement[AnElement.Count - 1]);
      NeighborIndexJ := AnElement.Count - 2;
    end;
    NeighborJ := AnElement[NeighborIndexJ].FNode;

    // Giuliani Eq. 2.5
    // APrime is the area of the triangle before the node position is changed.
    // b is the length of the base of the triangle.
    TriVals[ElementIndex].P := NeighborJ.Y - NeighborK.Y;
    TriVals[ElementIndex].Q := NeighborK.X - NeighborJ.X;
    R := NeighborJ.X * NeighborK.Y - NeighborK.X * NeighborJ.Y;
    TriVals[ElementIndex].APrime :=
      (TriVals[ElementIndex].P * X + TriVals[ElementIndex].Q * Y + R) / 2;
    TriVals[ElementIndex].b :=
      sqrt(Sqr(TriVals[ElementIndex].P) + Sqr(TriVals[ElementIndex].Q));

    TriVals[ElementIndex].M.X := (NeighborJ.X + NeighborK.X) / 2;
    TriVals[ElementIndex].M.Y := (NeighborJ.Y + NeighborK.Y) / 2;

    // Eq 2.6
    TotalArea := TotalArea + TriVals[ElementIndex].APrime;
    // Eq. 2.7
    TotalBase := TotalBase + TriVals[ElementIndex].b;

  end;
  if TotalBase = 0 then
  begin
    Exit;
  end;
  // Eq. 2.8
  MeanHeight := 2 * TotalArea / TotalBase;
  // Eq. 2.9
  MeanBase := TotalBase / FElements.Count;

  if (MeanHeight = 0) or (MeanBase = 0) then
  begin
    Exit;
  end;

  // Eq. 2.14
  W1 := 1 / Sqr(MeanHeight);
  W2 := 4 / Sqr(MeanBase);

  S1 := 0.0;
  S2 := 0.0;
  S3 := 0.0;
  S4 := 0.0;
  S5 := 0.0;
  for Index := 0 to Length(TriVals) - 1 do
  begin
    if FQuadMeshCreator.NodeAdjustmentMethod = namSarrateHuerta then
    begin
      MeanHeight := TriVals[Index].b / 2;
    end;

    if TriVals[Index].b = 0 then
    begin
      Exit;
    end;

    S1 := S1 + 1 / Sqr(TriVals[Index].b) *
      (W1 * Sqr(TriVals[Index].P) + W2 * Sqr(TriVals[Index].Q));
    S2 := S2 + TriVals[Index].P * TriVals[Index].Q / Sqr(TriVals[Index].b) *
      (W1 - W2);
    S3 := S3 + 1 / Sqr(TriVals[Index].b) *
      (W1 * TriVals[Index].P * (MeanHeight * TriVals[Index].b - 2 *
      TriVals[Index].APrime) - W2 * TriVals[Index].Q *
      ((X - TriVals[Index].M.X) * TriVals[Index].Q - (Y - TriVals[Index].M.Y) *
      TriVals[Index].P));
    S4 := S4 + 1 / Sqr(TriVals[Index].b) *
      (W1 * Sqr(TriVals[Index].Q) + W2 * Sqr(TriVals[Index].P));
    S5 := S5 + 1 / Sqr(TriVals[Index].b) *
      (W1 * TriVals[Index].Q * (MeanHeight * TriVals[Index].b - 2 *
      TriVals[Index].APrime) - W2 * TriVals[Index].P *
      ((Y - TriVals[Index].M.Y) * TriVals[Index].P - (X - TriVals[Index].M.X) *
      TriVals[Index].Q));
  end;

  try
  L.X := (S3 * S4 - S2 * S5) / (S1 * S4 - Sqr(S2));
  L.Y := (S1 * S5 - S2 * S3) / (S1 * S4 - Sqr(S2));

  NewLocation.X := X + L.X;
  NewLocation.Y := Y + L.Y;
  except on EInvalidOp do
    Exit;
  end;

  if ValidPosition(NewLocation) then
  begin
    OldLocation := FLocation;
    WorstAngles(OldHighestAngle, OldLowestAngle);
    Location := NewLocation;
    WorstAngles(NewHighestAngle, NewLowestAngle);
    if (OldHighestAngle >= 180) then
    begin
      if (NewHighestAngle > OldHighestAngle) then
      begin
        FLocation := OldLocation;
      end;
    end
    else  if ((NewHighestAngle > 180) and (NewHighestAngle > OldHighestAngle))
      or ((NewLowestAngle < 20) and (NewLowestAngle < OldLowestAngle))
      then
    begin
      FLocation := OldLocation;
    end;
  end;
end;

procedure TNode.AdjustPositionLagrange;
var
  NodeIndex: Integer;
  APoly: TPolygon2D;
  FoundNodes: TNodeList;
  OldLocation: TPoint2D;
  NewLocation: TPoint2D;
  OldHighestAngle: Double;
  OldLowestAngle: Double;
  NewHighestAngle: Double;
  NewLowestAngle: Double;
begin
  if FNodeType <> ntInner then
  begin
    Exit;
  end;

  FoundNodes := TNodeList.Create;
  try
    GetNeighborNodes(FoundNodes);

    SetLength(APoly, FoundNodes.Count);
    for NodeIndex := 0 to FoundNodes.Count - 1 do
    begin
      APoly[NodeIndex] := FoundNodes[NodeIndex].Location;
    end;
  finally
    FoundNodes.Free;
  end;

  if Area(APoly) <> 0 then
  begin
    NewLocation := Centroid(APoly);

    if ValidPosition(NewLocation) then
    begin
      OldLocation := FLocation;
      WorstAngles(OldHighestAngle, OldLowestAngle);
      Location := NewLocation;
      WorstAngles(NewHighestAngle, NewLowestAngle);
      if (OldHighestAngle >= 180) then
      begin
        if (NewHighestAngle > OldHighestAngle) then
        begin
          FLocation := OldLocation;
        end;
      end
      else  if ((NewHighestAngle > 180) and (NewHighestAngle > OldHighestAngle))
        or ((NewLowestAngle < 20) and (NewLowestAngle < OldLowestAngle))
        then
      begin
        FLocation := OldLocation;
      end;
    end;
  end;
end;

constructor TNode.Create(QuadMeshCreator: TQuadMeshCreator;
DesiredSpacing: double);
begin
  FElements := TBoundaryList.Create;
  FSegments := TSegmentList.Create;
  FQuadMeshCreator := QuadMeshCreator;
  FNodeNumber := FQuadMeshCreator.FNodes.Add(self);
  Assert(DesiredSpacing > 0);
  FDesiredSpacing := DesiredSpacing;
  FDesiredElementCount := 4;
  FConstraintNeighbors:= TNodeList.Create;
end;

destructor TNode.Destroy;
begin
  FConstraintNeighbors.Free;
  FSegments.Free;
  FElements.Free;
  inherited;
end;

procedure TNode.GetNeighborNodes(FoundNodes: TNodeList);
var
  AnElement: TBoundary;
  SubsequentNodeIndex: Integer;
  SubsequentNode: TNodeInBoundary;
  ElementIndex: Integer;
  PriorNodeIndex: Integer;
  NodeComparer: INodeComparer;
  PriorNode: TNodeInBoundary;
  NodeIndex: Integer;
begin
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    // The last point in AnElement is a duplicate of the first point
    // so there are really 4 nodes in the element.
    Assert(AnElement.Count = 5);
    NodeIndex := AnElement.IndexOfNode(self);
    Assert(NodeIndex >= 0);
    SubsequentNodeIndex := NodeIndex + 1;
    // because the last node is a duplicate of the first, there is no need
    // to adjust SubsequentNodeIndex for the possibility of being past the
    // end of  AnElement's nodes.
    SubsequentNode := AnElement[SubsequentNodeIndex];
    PriorNodeIndex := NodeIndex - 1;
    if PriorNodeIndex < 0 then
    begin
      PriorNodeIndex := AnElement.Count - 2;
    end;
    PriorNode := AnElement[PriorNodeIndex];
    if FoundNodes.IndexOf(PriorNode.FNode) < 0 then
    begin
      FoundNodes.Add(PriorNode.FNode);
    end;
    if FoundNodes.IndexOf(SubsequentNode.FNode) < 0 then
    begin
      FoundNodes.Add(SubsequentNode.FNode);
    end;
  end;
  NodeComparer := TNodeAngleComparer.Create(self);
  try
    FoundNodes.Sort(NodeComparer);
  finally
    NodeComparer := nil;
  end;
end;

procedure TNode.GetNeighborNodesAndElements(NeighborNodes: TNodeList;
  ElementList: TBoundaryList);
var
  FoundElement: Boolean;
  NeighborNode: TNode;
  NeighborIndex: Integer;
  AnElement: TBoundary;
  NodeIndex: Integer;
  ElementIndex: Integer;
  UnusedElements: TBoundaryList;
begin
  UnusedElements := TBoundaryList.Create;
  try
    for ElementIndex := 0 to FElements.Count - 1 do
    begin
      Assert(FElements[ElementIndex].Count = 5);
      FElements[ElementIndex].SetCounterClockwiseOrientation;
    end;
    for ElementIndex := 1 to FElements.Count - 1 do
    begin
      UnusedElements.Add(FElements[ElementIndex]);
    end;
    AnElement := FElements[0];
    ElementList.Add(AnElement);
    NodeIndex := AnElement.IndexOfNode(self);
    Assert(NodeIndex >= 0);
    Inc(NodeIndex);
    NeighborNode := AnElement.Items[NodeIndex].FNode;
    NeighborNodes.Add(NeighborNode);
    while ElementList.Count < 4 do
    begin
      FoundElement := False;
      for ElementIndex := 0 to UnusedElements.Count - 1 do
      begin
        AnElement := UnusedElements[ElementIndex];
        NodeIndex := AnElement.IndexOfNode(self);
        Assert(NodeIndex >= 0);
        NeighborIndex := AnElement.IndexOfNode(NeighborNode);
        if NeighborIndex >= 0 then
        begin
          ElementList.Add(AnElement);
          Inc(NeighborIndex, 2);
          if NeighborIndex >= AnElement.Count then
          begin
            Dec(NeighborIndex, 4);
          end;
          NeighborNode := AnElement.Items[NeighborIndex].FNode;
          UnusedElements.Delete(ElementIndex);
          FoundElement := True;
          NeighborNodes.Add(NeighborNode);
          break;
        end;
      end;
      Assert(FoundElement);
    end;
  finally
    UnusedElements.Free;
  end;
end;

function TNode.DiagonalSwapping: boolean;
{
  based on Zhu and others, 1991, International Journal for Numerical
  Methods in Engineering 32: 849-866.

  Original element configuration
      B
     /|\
    / | \
   /  |  \
  E   |   D
  |   |   |
  | 2 | 1 |
  |   |   |
  C   |   F
   \  |  /
    \ | /
     \|/
      A (self)

  Side A-B is eliminated and replaced by either C-D or E-F
}
var
  ElementIndex: Integer;
  Element1: TBoundary;
  NodeAIndex: Integer;
  NodeBIndex: Integer;
  NodeB: TNode;
  AB_Count: Integer;
  InnerElementIndex: Integer;
  Element2: TBoundary;
  NodeDIndex: Integer;
  NodeD: TNode;
  NodeFIndex: Integer;
  NodeF: TNode;
  NodeAIndex2: Integer;
  NodeBIndex2: Integer;
  NodeEIndex: Integer;
  NodeE: TNode;
  NodeCIndex: Integer;
  NodeC: TNode;
  CD_Count: Integer;
  EF_Count: Integer;
  StraightElement: boolean;
  BoundaryNodes: TNodeList;
begin
  result := False;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    Element1 := FElements[ElementIndex];
    NodeAIndex := Element1.IndexOfNode(self);
    Assert(NodeAIndex >= 0);

    NodeBIndex := NodeAIndex + 1;
    if NodeBIndex >= Element1.Count then
    begin
      NodeBIndex := NodeAIndex - 3;
      Assert(NodeBIndex >= 0);
    end;
    NodeB := Element1[NodeBIndex].FNode;

    if (FNodeType <> ntInner) or (NodeB.FNodeType <> ntInner) then
    begin
      Continue;
    end
    else
    begin
      AB_Count := NodeB.FElements.Count + FElements.Count;
      if AB_Count >= FDesiredElementCount + NodeB.FDesiredElementCount + 1 then
      begin
        for InnerElementIndex := 0 to FElements.Count - 1 do
        begin
          if InnerElementIndex = ElementIndex then
          begin
            Continue;
          end;
          Element2 := FElements[InnerElementIndex];
          NodeAIndex2 := Element2.IndexOfNode(self);
          NodeBIndex2 := Element2.IndexOfNode(NodeB);
          if (NodeAIndex2 >= 0) and (NodeBIndex2 >= 0) then
          begin
            NodeDIndex := NodeAIndex + 2;
            if NodeDIndex >= Element1.Count then
            begin
              NodeDIndex := NodeAIndex - 2;
              Assert(NodeDIndex >= 0);
            end;
            NodeD := Element1[NodeDIndex].FNode;

            NodeFIndex := NodeBIndex + 2;
            if NodeFIndex >= Element1.Count then
            begin
              NodeFIndex := NodeBIndex - 2;
              Assert(NodeFIndex >= 0);
            end;
            NodeF := Element1[NodeFIndex].FNode;

            NodeEIndex := NodeAIndex2 + 2;
            if NodeEIndex >= Element2.Count then
            begin
              NodeEIndex := NodeAIndex2 - 2;
              Assert(NodeEIndex >= 0);
            end;
            NodeE := Element2[NodeEIndex].FNode;

            NodeCIndex := NodeBIndex2 + 2;
            if NodeCIndex >= Element2.Count then
            begin
              NodeCIndex := NodeBIndex2 - 2;
              Assert(NodeCIndex >= 0);
            end;
            NodeC := Element2[NodeCIndex].FNode;

            CD_Count := NodeC.FElements.Count + NodeD.FElements.Count;
            EF_Count := NodeE.FElements.Count + NodeF.FElements.Count;
            if EF_Count > CD_Count then
            begin
              if AB_Count - CD_Count >= 3 -
                (NodeC.FDesiredElementCount + NodeD.FDesiredElementCount) +
                (FDesiredElementCount + NodeB.FDesiredElementCount) then
              begin
                StraightElement :=
                  (Collinear(FLocation, NodeF.FLocation, NodeD.FLocation) and
                  Collinear(NodeC.FLocation, NodeF.FLocation, NodeD.FLocation));
                StraightElement := StraightElement or
                  (Collinear(NodeC.FLocation, NodeB.FLocation, NodeD.FLocation)
                  and Collinear(NodeE.FLocation, NodeB.FLocation,
                  NodeD.FLocation));
                if not StraightElement then
                begin
                  // Exit if the new element would be invalid.
                  if not Intersect(EquateSegment(Location, NodeD.Location),
                    EquateSegment(NodeC.Location, NodeF.Location)) then
                  begin
                    Continue;
                  end;
                  if not Intersect(EquateSegment(NodeC.Location,
                    NodeB.Location), EquateSegment(NodeE.Location,
                    NodeD.Location)) then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeC.Location, NodeD.Location,
                    NodeB.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeE.Location, NodeC.Location,
                    NodeD.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeD.Location, NodeC.Location,
                    self.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeF.Location, NodeD.Location,
                    NodeC.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;



                  // Exit if the new element would result in an element
                  // with 3 edge nodes in a straight line.
                  BoundaryNodes := TNodeList.Create;
                  try
                    if NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(self);
                    end;
                    if NodeC.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeC);
                    end;
                    if NodeD.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeD);
                    end;
                    if NodeF.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeF);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if NearlyStraightAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Continue;
                      end;
                    end;
                    BoundaryNodes.Clear;
                    if NodeB.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeB);
                    end;
                    if NodeC.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeC);
                    end;
                    if NodeD.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeD);
                    end;
                    if NodeE.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeE);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if NearlyStraightAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Continue;
                      end;
                    end;
                  finally
                    BoundaryNodes.Free;
                  end;
                  // replace A-B with C-D
                  NodeB.ReplaceNodeInElement(NodeC, Element1);
                  self.ReplaceNodeInElement(NodeD, Element2);
                  self.FElements.Remove(Element2);
                  NodeB.FElements.Remove(Element1);
                  NodeC.FElements.Add(Element1);
                  NodeD.FElements.Add(Element2);
                  result := True;
{$IFDEF TEST}
                  Element1.CheckInvalidElement;
                  Element2.CheckInvalidElement;
{$ENDIF}
                  Exit;
                end;
              end;
            end
            else
            begin
              if AB_Count - EF_Count >= 3 -
                (NodeE.FDesiredElementCount + NodeF.FDesiredElementCount) +
                (FDesiredElementCount + NodeB.FDesiredElementCount) then
              begin
                StraightElement :=
                  (Collinear(FLocation, NodeF.FLocation, NodeE.FLocation) and
                  Collinear(NodeC.FLocation, NodeF.FLocation, NodeE.FLocation));
                StraightElement := StraightElement or
                  (Collinear(NodeF.FLocation, NodeB.FLocation, NodeD.FLocation)
                  and Collinear(NodeE.FLocation, NodeB.FLocation,
                  NodeD.FLocation));
                if not StraightElement then
                begin
                  // Exit if the new element would be invalid.
                  if not Intersect(EquateSegment(Location, NodeE.Location),
                    EquateSegment(NodeC.Location, NodeF.Location)) then
                  begin
                    Continue;
                  end;
                  if not Intersect(EquateSegment(NodeB.Location,
                    NodeF.Location), EquateSegment(NodeE.Location,
                    NodeD.Location)) then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeE.Location, NodeF.Location,
                    NodeD.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeB.Location, NodeE.Location,
                    NodeF.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(NodeF.Location, NodeE.Location,
                    NodeC.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  if OrientedVertexAngle(self.Location, NodeF.Location,
                    NodeE.Location, CounterClockwise) > 180  then
                  begin
                    Continue;
                  end;

                  // Exit if the new element would result in an element
                  // with 3 edge nodes in a straight line.
                  BoundaryNodes := TNodeList.Create;
                  try
                    if NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(self);
                    end;
                    if NodeC.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeC);
                    end;
                    if NodeE.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeE);
                    end;
                    if NodeF.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeF);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if NearlyStraightAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Continue;
                      end;
                    end;
                    BoundaryNodes.Clear;
                    if NodeB.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeB);
                    end;
                    if NodeD.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeD);
                    end;
                    if NodeE.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeE);
                    end;
                    if NodeF.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeF);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if NearlyStraightAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Continue;
                      end;
                    end;
                  finally
                    BoundaryNodes.Free;
                  end;
                  // replace A-B with E-F
                  NodeB.ReplaceNodeInElement(NodeF, Element2);
                  self.ReplaceNodeInElement(NodeE, Element1);
                  self.FElements.Remove(Element1);
                  NodeB.FElements.Remove(Element2);
                  NodeE.FElements.Add(Element1);
                  NodeF.FElements.Add(Element2);
                  result := True;
{$IFDEF TEST}
                  Element1.CheckInvalidElement;
                  Element2.CheckInvalidElement;
{$ENDIF}
                  Exit;
                end;
              end;
            end;

            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TNode.ReplaceNodeInElement(ReplacementNode: TNode;
AnElement: TBoundary);
var
  ASegment: TSegment;
  NodeIndex: Integer;
  SegmentIndex: Integer;
  BoundaryNode: TNodeInBoundary;
begin
  Assert(AnElement.IndexOfNode(ReplacementNode) < 0);
  // After calling this procedure, be sure to remove the element from this
  // node and add it to the replacement node as needed.
  for SegmentIndex := 0 to AnElement.FSegments.Count - 1 do
  begin
    ASegment := AnElement.FSegments[SegmentIndex];
    if ASegment.FNode1 = self then
    begin
      ASegment.FNode1 := ReplacementNode;
    end;
    if ASegment.FNode2 = self then
    begin
      ASegment.FNode2 := ReplacementNode;
    end;
    NodeIndex := ASegment.FInnerNodes.IndexOf(self);
    if NodeIndex >= 0  then
    begin
      ASegment.FInnerNodes[NodeIndex] := ReplacementNode;
    end;
  end;
  NodeIndex := AnElement.FNodes.IndexOf(self);
  if NodeIndex >= 0 then
  begin
    AnElement.FNodes[NodeIndex] := ReplacementNode;
    if NodeIndex = 0 then
    begin
      NodeIndex := AnElement.FNodes.IndexOf(self);
      if NodeIndex >= 0 then
      begin
        AnElement.FNodes[NodeIndex] := ReplacementNode;
      end;
    end;
  end;
  for NodeIndex := 0 to AnElement.Count - 1 do
  begin
    BoundaryNode := AnElement[NodeIndex];
    if BoundaryNode.FNode = self then
    begin
      BoundaryNode.FNode := ReplacementNode;
    end;
  end;
end;

procedure TNode.NodeElimination;
var
  Element1: TBoundary;
  Element2: TBoundary;
  NodePosition2: Integer;
  OppositeNodePosition2: Integer;
  OppositeBoundaryNode2: TNodeInBoundary;
  ReplacementNode: TNode;
begin
  Assert(FElements.Count = 2);
  Element1 := FElements[0];

  Element2 := FElements[1];
  NodePosition2 := Element2.IndexOfNode(self);
  Assert(NodePosition2 >= 0);
  OppositeNodePosition2 := NodePosition2 + 2;
  if OppositeNodePosition2 >= Element2.Count then
  begin
    OppositeNodePosition2 := NodePosition2 - 2;
    Assert(OppositeNodePosition2 >= 0);
  end;
  OppositeBoundaryNode2 := Element2[OppositeNodePosition2];

  ReplacementNode := OppositeBoundaryNode2.FNode;
  ReplaceNodeInElement(ReplacementNode, Element1);

  ReplacementNode.FElements.Add(Element1);
  ReplacementNode.FElements.Remove(Element2);

  Element2.RemoveSelfFromOwnNodes;


  Element2.FParent.FSubParts.Remove(Element2);

{$IFDEF TEST}
  Element1.CheckInvalidElement;
{$ENDIF}
end;

function TNode.ElementElimination: boolean;
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  BoundaryNodeIndex: Integer;
  OppositeNodeIndex: Integer;
  OppositeNode: TNodeInBoundary;
  CornerIndex1: Integer;
  CornerIndex2: Integer;
  CornerNode1: TNodeInBoundary;
  CornerNode2: TNodeInBoundary;
  InnerElementIndex: Integer;
  AnotherElement: TBoundary;
  OldLocation: TPoint2D;
  NewLocation: TPoint2D;
  TestElement: TBoundary;
  Node0: TNode;
  Node1: TNode;
  Node2: TNode;
  Node3: TNode;
begin
  result := False;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    BoundaryNodeIndex := AnElement.IndexOfNode(self);
    Assert(BoundaryNodeIndex >= 0);

    OppositeNodeIndex := BoundaryNodeIndex + 2;
    if OppositeNodeIndex >= AnElement.Count then
    begin
      OppositeNodeIndex := BoundaryNodeIndex - 2;
      Assert(OppositeNodeIndex >= 0);
    end;
    OppositeNode := AnElement[OppositeNodeIndex];
    if (OppositeNode.FNode.FNodeType = ntInner) and
      (OppositeNode.FNode.FElements.Count = 3) then
    begin
      Assert(AnElement[AnElement.Count - 1] = AnElement[0]);

      CornerIndex1 := BoundaryNodeIndex + 1;
      if CornerIndex1 >= AnElement.Count then
      begin
        CornerIndex1 := 1;
      end;
      CornerNode1 := AnElement[CornerIndex1];

      CornerIndex2 := BoundaryNodeIndex - 1;
      if CornerIndex2 < 0 then
      begin
        CornerIndex2 := AnElement.Count - 2;
      end;
      CornerNode2 := AnElement[CornerIndex2];

      NewLocation.X := (CornerNode1.FNode.X + CornerNode2.FNode.X) / 2;
      NewLocation.Y := (CornerNode1.FNode.Y + CornerNode2.FNode.Y) / 2;

      OldLocation := OppositeNode.FNode.FLocation;
      try
        OppositeNode.FNode.FLocation := NewLocation;
        for InnerElementIndex := 0 to OppositeNode.FNode.FElements.Count - 1 do
        begin
          TestElement := OppositeNode.FNode.FElements[InnerElementIndex];
          Node0 := TestElement[0].FNode;
          Node1 := TestElement[1].FNode;
          Node2 := TestElement[2].FNode;
          Node3 := TestElement[3].FNode;
          if not Intersect(EquateSegment(Node0.Location, Node2.Location),
            EquateSegment(Node1.Location, Node3.Location)) then
          begin
            Exit;
          end;
        end;
      finally
        OppositeNode.FNode.FLocation := OldLocation;
      end;

      OldLocation := FLocation;
      try
        FLocation := NewLocation;
        for InnerElementIndex := 0 to FElements.Count - 1 do
        begin
          TestElement := FElements[InnerElementIndex];
          Node0 := TestElement[0].FNode;
          Node1 := TestElement[1].FNode;
          Node2 := TestElement[2].FNode;
          Node3 := TestElement[3].FNode;
          if not Intersect(EquateSegment(Node0.Location, Node2.Location),
            EquateSegment(Node1.Location, Node3.Location)) then
          begin
            Exit;
          end;
        end;
      finally
        FLocation := OldLocation;
      end;

      result := True;
      OppositeNode.FNode.Location := NewLocation;

      for InnerElementIndex := 0 to FElements.Count - 1 do
      begin
        if InnerElementIndex = ElementIndex then
        begin
          Continue;
        end;
        AnotherElement := FElements[InnerElementIndex];
        ReplaceNodeInElement(OppositeNode.FNode, AnotherElement);
        OppositeNode.FNode.FElements.Add(AnotherElement);
{$IFDEF TEST}
        AnotherElement.CheckInvalidElement;
{$ENDIF}
      end;

      AnElement.RemoveSelfFromOwnNodes;
      AnElement.FParent.FSubParts.Remove(AnElement);

      break;
    end;
  end;
end;

{
  N = NeighborNodes
  E = ElementList
  EE1 = ExternalElement1
  EE2 = ExternalElement2
  EN1 = ExternalNode1
  EN2 = ExternalNode2

  EN3       EN4                 EN5         EN6
   *---------*---------*N[2]-----*-----------*
    \        |         |         |          /
     \       |         |         |         /
      \ EE1  |   E[2]  |  E[3]   |  EE2   /
       \     |         |         |       /
     EN1*----*N[1]-----*---------*N[3]--*EN2
           \      E[1] |  E[0]       /
              \        |          /
                 \     |       /
                    \  |    /
                       *N[0]
                     Fan Node




   *---------*---------*---------*---------*
    \                /   \                /
     \           /           \           /
      \       /                 \       /
       \   /                      \    /
        *                           *
           \                       /
              \                 /
                 \           /
                    \     /
                       *




}

function TNode.FanElimination: boolean;
var
  NeighborNodes: TNodeList;
  ElementIndex: Integer;
  AnElement: TBoundary;
  ElementList: TBoundaryList;
  NodeIndex: Integer;
  FanIndex: integer;
  CentralNodeIndex: Integer;
  ExternalElement1: TBoundary;
  ExternalElement2: TBoundary;
  ExternalNode1: TNode;
  ExternalNode2: TNode;
  ExternalNode3: TNode;
  ExternalNode4: TNode;
  ExternalNode5: TNode;
  ExternalNode6: TNode;
begin
  result := False;
  Assert(ElementCount = 4);

  NeighborNodes := TNodeList.Create;
  ElementList := TBoundaryList.Create;
    try
      GetNeighborNodesAndElements(NeighborNodes, ElementList);

      // NeighborNodes contains a list of nodes connected to the central
      // node in clockwise order around the central node.
      // ElementList contains a list of TBoundaries connected to the central
      // node in clockwise order around the central node.
      Assert(NeighborNodes.Count = 4);
      if (NeighborNodes[0].ElementCount = 3)
        and (NeighborNodes[0].NodeType = ntInner)
        and (NeighborNodes[2].ElementCount = 3)
        and (NeighborNodes[2].NodeType = ntInner) then
      begin
        if (NeighborNodes[1].FElements.Count
          - NeighborNodes[1].FDesiredElementCount) >
          (NeighborNodes[3].FElements.Count
          - NeighborNodes[3].FDesiredElementCount) then
        begin
          FanIndex := 1;
        end
        else
        begin
          FanIndex := 3;
        end;
      end
      else if (NeighborNodes[1].ElementCount = 3)
        and (NeighborNodes[1].NodeType = ntInner)
        and (NeighborNodes[3].ElementCount = 3)
        and (NeighborNodes[3].NodeType = ntInner) then
      begin
        if (NeighborNodes[0].FElements.Count
          - NeighborNodes[0].FDesiredElementCount) >
          (NeighborNodes[2].FElements.Count
          - NeighborNodes[2].FDesiredElementCount) then
        begin
          FanIndex := 0;
        end
        else
        begin
          FanIndex := 2;
        end;
      end
      else
      begin
        Exit;
      end;
      if (NeighborNodes[FanIndex].FElements.Count
        > NeighborNodes[FanIndex].FDesiredElementCount) then
      begin
        while FanIndex > 0 do
        begin
          NeighborNodes.Add(NeighborNodes[0]);
          NeighborNodes.Delete(0);
          ElementList.Add(ElementList[0]);
          ElementList.Delete(0);
          Dec(FanIndex);
        end;
  //      FanNode := NeighborNodes[0];

        ExternalElement1 := nil;
        for ElementIndex := 0 to NeighborNodes[1].FElements.Count - 1 do
        begin
          AnElement := NeighborNodes[1].FElements[ElementIndex];
          if AnElement.IndexOfNode(Self) < 0 then
          begin
            ExternalElement1 := AnElement;
            break;
          end;
        end;
        Assert(ExternalElement1 <> nil);
        ExternalElement1.SetCounterClockwiseOrientation;

        ExternalElement2 := nil;
        for ElementIndex := 0 to NeighborNodes[3].FElements.Count - 1 do
        begin
          AnElement := NeighborNodes[3].FElements[ElementIndex];
          if AnElement.IndexOfNode(Self) < 0 then
          begin
            ExternalElement2 := AnElement;
            break;
          end;
        end;
        Assert(ExternalElement2 <> nil);
        ExternalElement2.SetCounterClockwiseOrientation;

        AnElement := ElementList[1];
        NodeIndex := AnElement.IndexOfNode(NeighborNodes[0]);
        CentralNodeIndex := AnElement.IndexOfNode(self);
        Dec(CentralNodeIndex);
        if CentralNodeIndex < 0 then
        begin
          CentralNodeIndex := 3;
        end;
        if (CentralNodeIndex <> NodeIndex) then
        begin
          // straight line element
          Exit;
        end;
        Dec(NodeIndex);
        if NodeIndex < 0 then
        begin
          NodeIndex := 3;
        end;
        ExternalNode1 := AnElement[NodeIndex].FNode;

        NodeIndex := ExternalElement1.IndexOfNode(ExternalNode1);
        Assert(NodeIndex >= 0);
        Dec(NodeIndex);
        if NodeIndex < 0 then
        begin
          NodeIndex := 3;
        end;
        ExternalNode3 := ExternalElement1[NodeIndex].FNode;

        Dec(NodeIndex);
        if NodeIndex < 0 then
        begin
          NodeIndex := 3;
        end;
        ExternalNode4 := ExternalElement1[NodeIndex].FNode;


        AnElement := ElementList[0];
        NodeIndex := AnElement.IndexOfNode(NeighborNodes[0]);
        CentralNodeIndex := AnElement.IndexOfNode(self);
        ExternalNode2 := AnElement[NodeIndex+1].FNode;
        Dec(NodeIndex);
        if NodeIndex < 0 then
        begin
          NodeIndex := 3;
        end;
        if (CentralNodeIndex <> NodeIndex) then
        begin
          // straight line element.
          Exit;
        end;

        NodeIndex := ExternalElement2.IndexOfNode(NeighborNodes[3]);
        Assert(NodeIndex >= 0);
        Dec(NodeIndex);
        if NodeIndex < 0 then
        begin
          NodeIndex := 3;
        end;
        ExternalNode5 := ExternalElement2[NodeIndex].FNode;

        Dec(NodeIndex);
        if NodeIndex < 0 then
        begin
          NodeIndex := 3;
        end;
        ExternalNode6 := ExternalElement2[NodeIndex].FNode;

        if not Intersect(
          EquateSegment(ExternalNode1.Location, ExternalNode2.Location),
          EquateSegment(NeighborNodes[0].Location, NeighborNodes[2].Location)) then
        begin
          Exit;
        end;

        if not Intersect(
          EquateSegment(ExternalNode1.Location, ExternalNode4.Location),
          EquateSegment(ExternalNode3.Location, NeighborNodes[2].Location)) then
        begin
          Exit;
        end;

        if not Intersect(
          EquateSegment(ExternalNode2.Location, ExternalNode5.Location),
          EquateSegment(ExternalNode6.Location, NeighborNodes[2].Location)) then
        begin
          Exit;
        end;

        if (ExternalNode3.NodeType <> ntInner)
          and (ExternalNode4.NodeType <> ntInner)
          and (NeighborNodes[2].NodeType <> ntInner)
          and (OrientedVertexAngle(ExternalNode3.Location, ExternalNode4.Location,
            NeighborNodes[2].Location, Clockwise) > 179) then
        begin
          Exit;
        end;

        if (ExternalNode5.NodeType <> ntInner)
          and (ExternalNode6.NodeType <> ntInner)
          and (NeighborNodes[2].NodeType <> ntInner)
          and (OrientedVertexAngle(NeighborNodes[2].Location,
            ExternalNode5.Location, ExternalNode6.Location, Clockwise) > 179) then
        begin
          Exit;
        end;

        if (ExternalNode1.NodeType <> ntInner)
          and (ExternalNode2.NodeType <> ntInner)
          and (NeighborNodes[0].NodeType <> ntInner)
          and (OrientedVertexAngle(ExternalNode2.Location, NeighborNodes[0].Location,
            ExternalNode1.Location, Clockwise) > 179) then
        begin
          Exit;
        end;

        AnElement := ElementList[0];

        NeighborNodes[1].ReplaceNodeInElement(NeighborNodes[2], ExternalElement1);
        NeighborNodes[3].ReplaceNodeInElement(NeighborNodes[2], ExternalElement2);

        NeighborNodes[3].ReplaceNodeInElement(NeighborNodes[2], AnElement);
        self.ReplaceNodeInElement(ExternalNode1, AnElement);

        NeighborNodes[2].FElements.Add(ExternalElement1);
        NeighborNodes[2].FElements.Add(ExternalElement2);
        NeighborNodes[2].FElements.Add(AnElement);
        ExternalNode1.FElements.Add(AnElement);

        NeighborNodes[1].FElements.Remove(ExternalElement1);
        NeighborNodes[3].FElements.Remove(ExternalElement2);
        NeighborNodes[3].FElements.Remove(AnElement);
        Self.FElements.Remove(AnElement);

        for ElementIndex := 1 to ElementList.Count-1 do
        begin
          AnElement := ElementList[ElementIndex];
          AnElement.RemoveSelfFromAllNodes;
          AnElement.FParent.FSubParts.Remove(AnElement);
        end;

        Assert(NeighborNodes[1].FElements.Count = 0);
        Assert(NeighborNodes[3].FElements.Count = 0);
        Assert(self.FElements.Count = 0);

        ExternalElement1.FixSegments;
        ExternalElement2.FixSegments;
        ElementList[0].FixSegments;

        FQuadMeshCreator.FNodes.Remove(NeighborNodes[1]);
        FQuadMeshCreator.FNodes.Remove(NeighborNodes[3]);
        result := True;
      end;

    finally
      NeighborNodes.Free;
      ElementList.Free;
    end;
end;

function TNode.GetActiveElement(Index: Integer): IElement;
begin
  result := FElements[Index];
end;

function TNode.GetActiveElementCount: Integer;
begin
  result := FElements.Count;
end;

function TNode.GetBypassUpdate: Boolean;
begin
  result := FBypassUpdate;
end;

function TNode.GetLocation: TPoint2D;
begin
  result := FLocation;
end;

function TNode.GetNodeNumber: Integer;
begin
  result := FNodeNumber;
end;

function TNode.GetNodeType: TNodeType;
begin
  result := FNodeType;
end;

function TNode.ImproveTopology1: boolean;
begin
  result := False;
  if FNodeType = ntInner then
  begin
    case FElements.Count of
      2:
        begin
          NodeElimination;
          result := True;
        end;
      3:
        begin
          result := ElementElimination;
          if not result then
          begin
            result := SideElimination;
          end;
          if not result then
          begin
            result := InteriorElementElimination;
          end;
        end;
      4:
        begin
          result := FanElimination;
        end;
    end;
  end;
end;

function TNode.ImproveTopology2: boolean;
begin
  result := DiagonalSwapping;
end;

{
   CE = CentralElement

              *                                     *
             / \                                   / \
            /   \                                 /   \
           /     \                               /     \
         A*       *B                             *       *
         / \     / \                           / \     / \
        /   \   /   \                         /   \   /   \
       /     \ /     \                       /     \ /     \
      /       *I      \                     /       *       \
     /  E1   / \   E3  \                   /        |        \
    /       /   \       \                 /         |         \
   /       /     \       \               /          |          \
 C*-------*F  CE G*-------*D  ------>   *           |           *
   \       \     /       /               \          |          /
    \       \   /       /                 \         |         /
     \   E2  \ /   E4  /                   \        |        /
      \       *H      /                     \       |       /
       \      |      /                       \      |      /
        \     |     /                         \     |     /
         \    |    /                           \    |    /
          \   |   /                             \   |   /
           \  |  /                               \  |  /
            \ | /                                 \ | /
             \|/                                   \|/
              *                                     *
              E

              If B-D-E colinear and on edge   (RightSideColinear)

              *                                        *
             / \                                      / \
            /   \                                    /   \
           /     \                                  /     \
         A*       *B                              A*       *B
         / \     / \                              /|\     / \
        /   \   /   \                            / | \   /   \
       /     \ /     \                          /  \  \ /     \
      /       *I      \                        /    |  *I      \
     /  E1   / \   E3  \                      /  E1 \   \   E3  \
    /       /   \       \                    /       |   \       \
   /       /     \       \                  /        \ CE \       \
 C*-------*F  CE G*-------*D  ------>     C*          |   G*-------*D
   \       \     /       /                  \         \   /       /
    \       \   /       /                    \         | /       /
     \   E2  \ /   E4  /                      \        \/   E4  /
      \       *H      /                        \       *H      /
       \      |      /                          \      |      /
        \     |     /                            \     |     /
         \    |    /                              \    |    /
          \   |   /                                \   |   /
           \  |  /                                  \  |  /
            \ | /                                    \ | /
             \|/                                      \|/
              *                                        *
              E                                        E


              If A-C-E colinear and on edge (LeftSideColinear)

              *                                        *
             / \                                      / \
            /   \                                    /   \
           /     \                                  /     \
         A*       *B                              A*       *B
         / \     / \                              / \     /|\
        /   \   /   \                            /   \   / | \
       /     \ /     \                          /     \ /  |  \
      /       *I      \                        /       *I  |   \
     /  E1   / \   E3  \                      /  E1   /    /    \
    /       /   \       \                    /       /    |  E3  \
   /       /     \       \                  /       /     /       \
 C*-------*F  CE G*-------*D  ------>     C*-------*F CE |        *D
   \       \     /       /                  \       \    /        /
    \       \   /       /                    \       \  |        /
     \   E2  \ /   E4  /                      \    2  \ /       /
      \       *H      /                        \       *H      /
       \      |      /                          \      |      /
        \     |     /                            \     |     /
         \    |    /                              \    |    /
          \   |   /                                \   |   /
           \  |  /                                  \  |  /
            \ | /                                    \ | /
             \|/                                      \|/
              *                                        *
              E                                        E


}

function TNode.InteriorElementElimination: Boolean;
var
  ElementIndex: Integer;
  NodeIndex: Integer;
  AnElement: TBoundary;
  ANode: TNode;
  OutsideElementIndex: Integer;
  NodeF: TNode;
  NodeGIndex: Integer;
  NodeG: TNode;
  NodeHIndex: Integer;
  NodeH: TNode;
  CentralElement: TBoundary;
  NodeFIndex: Integer;
  ThreeElementCount: Integer;
  NodeI: TNode;
  Element1: TBoundary;
  Element2: TBoundary;
  InnerElementIndex: Integer;
  Element3: TBoundary;
  Element4: TBoundary;
  NodeA: TNode;
  NodeB: TNode;
  NodeC: TNode;
  NodeD: TNode;
  NodeE: TNode;
  NodeAIndex: Integer;
  NodeCIndex: Integer;
  NodeEIndex: Integer;
  NodeDIndex: Integer;
  NodeBIndex: integer;
  LeftSideColinear: Boolean;
  RightSideColinear: Boolean;
  Epsilon: double;
begin
  result := False;
  Assert(ElementCount = 3);
  Epsilon := FQuadMeshCreator.FCharacteristicLength/1e7;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    ThreeElementCount := 0;
    OutsideElementIndex := -1;
    AnElement.SetCounterClockwiseOrientation;
    for NodeIndex := 0 to AnElement.Count - 2 do
    begin
      ANode := AnElement.Items[NodeIndex].FNode;
      if (ANode.NodeType = ntInner)  then
      begin
        if (ANode.FElements.Count = 3) then
        begin
          Inc(ThreeElementCount);
        end
        else if (ANode.FElements.Count > 3) then
        begin
          OutsideElementIndex := NodeIndex;
        end;
      end
      else
      begin
        break;
      end;
    end;
    if (ThreeElementCount <> 3) or (OutsideElementIndex < 0) then
    begin
      Continue;
    end;
    CentralElement := AnElement;
    NodeHIndex := OutsideElementIndex-2;
    if NodeHIndex < 0 then
    begin
      NodeHIndex := NodeHIndex + 4;
    end;
    NodeH := CentralElement[NodeHIndex].FNode;

    NodeFIndex := OutsideElementIndex+1;
    NodeF := CentralElement[NodeFIndex].FNode;

    NodeGIndex := OutsideElementIndex-1;
    if NodeGIndex < 0 then
    begin
      NodeGIndex := 3;
    end;
    NodeG := CentralElement[NodeGIndex].FNode;
    NodeI := CentralElement[OutsideElementIndex].FNode;

    Element1 := nil;
    Element2 := nil;
    for InnerElementIndex := 0 to NodeF.FElements.Count - 1 do
    begin
      AnElement := NodeF.FElements[InnerElementIndex];
      if AnElement = CentralElement then
      begin
        Continue;
      end
      else if AnElement.IndexOfNode(NodeI) >= 0 then
      begin
        Element1 := AnElement
      end
      else if AnElement.IndexOfNode(NodeH) >= 0 then
      begin
        Element2 := AnElement
      end
      else
      begin
        Assert(False);
      end;
    end;
    Assert(Element1 <> nil);
    Assert(Element2 <> nil);
    Element1.SetCounterClockwiseOrientation;
    Element2.SetCounterClockwiseOrientation;

    Element3 := nil;
    Element4 := nil;
    for InnerElementIndex := 0 to NodeG.FElements.Count - 1 do
    begin
      AnElement := NodeG.FElements[InnerElementIndex];
      if AnElement = CentralElement then
      begin
        Continue;
      end
      else if AnElement.IndexOfNode(NodeI) >= 0 then
      begin
        Element3 := AnElement
      end
      else if AnElement.IndexOfNode(NodeH) >= 0 then
      begin
        Element4 := AnElement
      end
      else
      begin
        Assert(False);
      end;
    end;
    Assert(Element3 <> nil);
    Assert(Element4 <> nil);
    Element3.SetCounterClockwiseOrientation;
    Element4.SetCounterClockwiseOrientation;

    NodeAIndex := Element1.IndexOfNode(NodeI);
    if NodeAIndex < 0 then
    begin
      // concave element
      Continue;
    end;
    Inc(NodeAIndex);
    NodeA := Element1.Items[NodeAIndex].FNode;

    NodeCIndex := NodeAIndex + 1;
    if NodeCIndex >= Element1.Count then
    begin
      NodeCIndex := NodeCIndex-4;
    end;
    NodeC := Element1.Items[NodeCIndex].FNode;

    NodeEIndex := Element2.IndexOfNode(NodeC);
    if NodeEIndex < 0 then
    begin
      // concave element
      Continue;
    end;
    Inc(NodeEIndex);
    NodeE := Element2.Items[NodeEIndex].FNode;

    NodeDIndex := Element4.IndexOfNode(NodeE);
    if NodeDIndex < 0 then
    begin
      // concave element
      Continue;
    end;
    Inc(NodeDIndex);
    NodeD := Element4.Items[NodeDIndex].FNode;

    NodeBIndex := Element3.IndexOfNode(NodeD);
    if NodeBIndex < 0 then
    begin
      // concave element
      Continue;
    end;
    Inc(NodeBIndex);
    NodeB := Element3.Items[NodeBIndex].FNode;

    LeftSideColinear := False;
    RightSideColinear := False;
    if NodeE.NodeType <> ntInner then
    begin
      if (NodeA.NodeType <> ntInner)
        and (NodeC.NodeType <> ntInner)
        and RobustCollinear(NodeA.Location, NodeC.Location, NodeE.Location, Epsilon)
        then
      begin
        LeftSideColinear := True;
      end;

      if (NodeB.NodeType <> ntInner)
        and (NodeD.NodeType <> ntInner)
        and RobustCollinear(NodeB.Location, NodeD.Location, NodeE.Location, Epsilon)
        then
      begin
        RightSideColinear := True;
      end;
    end;


    if LeftSideColinear then
    begin
      if NodeG <> self then
      begin
        Continue;
      end;

      if not Intersect(EquateSegment(NodeF.Location, NodeB.Location),
        EquateSegment(NodeH.Location, NodeI.Location)) then
      begin
        Continue;
      end;
      if not Intersect(EquateSegment(NodeE.Location, NodeB.Location),
        EquateSegment(NodeH.Location, NodeD.Location)) then
      begin
        Continue;
      end;
      NodeG.ReplaceNodeInElement(NodeB, CentralElement);
      NodeB.FElements.Add(CentralElement);
      NodeG.FElements.Remove(CentralElement);

      NodeG.ReplaceNodeInElement(NodeE, Element3);
      NodeE.FElements.Add(Element3);
      NodeG.FElements.Remove(Element3);

      NodeI.ReplaceNodeInElement(NodeH, Element3);
      NodeH.FElements.Add(Element3);
      NodeI.FElements.Remove(Element3);

      Element4.RemoveSelfFromAllNodes;

      Element4.FParent.FSubParts.Remove(Element4);

      Assert(NodeG.FElements.Count = 0);

      result := True;
      Exit;
    end
    else if RightSideColinear then
    begin
      if NodeF <> self then
      begin
        Continue;
      end;

      if not Intersect(EquateSegment(NodeG.Location, NodeA.Location),
        EquateSegment(NodeH.Location, NodeI.Location)) then
      begin
        Continue;
      end;
      if not Intersect(EquateSegment(NodeA.Location, NodeE.Location),
        EquateSegment(NodeH.Location, NodeC.Location)) then
      begin
        Continue;
      end;
      NodeF.ReplaceNodeInElement(NodeA, CentralElement);
      NodeA.FElements.Add(CentralElement);
      NodeF.FElements.Remove(CentralElement);

      NodeI.ReplaceNodeInElement(NodeH, Element1);
      NodeH.FElements.Add(Element1);
      NodeI.FElements.Remove(Element1);

      NodeF.ReplaceNodeInElement(NodeE, Element1);
      NodeE.FElements.Add(Element1);
      NodeF.FElements.Remove(Element1);



      Element2.RemoveSelfFromAllNodes;

      Element2.FParent.FSubParts.Remove(Element2);

      Assert(NodeF.FElements.Count = 0);

      result := True;
      Exit;

    end
    else
    begin
      if NodeH <> self then
      begin
        Continue;
      end;

      if not Intersect(EquateSegment(NodeA.Location, NodeE.Location),
        EquateSegment(NodeC.Location, NodeI.Location)) then
      begin
        Continue;
      end;
      if not Intersect(EquateSegment(NodeB.Location, NodeE.Location),
        EquateSegment(NodeD.Location, NodeI.Location)) then
      begin
        Continue;
      end;

      NodeF.ReplaceNodeInElement(NodeE, Element1);
      NodeE.FElements.Add(Element1);
      NodeF.FElements.Remove(Element1);

      NodeG.ReplaceNodeInElement(NodeE, Element3);
      NodeE.FElements.Add(Element3);
      NodeG.FElements.Remove(Element3);

      Element2.RemoveSelfFromAllNodes;
      Element4.RemoveSelfFromAllNodes;
      CentralElement.RemoveSelfFromAllNodes;

      Element2.FParent.FSubParts.Remove(Element2);
      Element4.FParent.FSubParts.Remove(Element4);
      CentralElement.FParent.FSubParts.Remove(CentralElement);

      Assert(NodeF.FElements.Count = 0);
      Assert(NodeG.FElements.Count = 0);
      Assert(NodeH.FElements.Count = 0);

      FQuadMeshCreator.FNodes.Remove(NodeG);
      FQuadMeshCreator.FNodes.Remove(NodeH);
      result := True;
      Exit;
    end;
  end;
end;

procedure TNode.SetBypassUpdate(const Value: Boolean);
begin
  FBypassUpdate := Value;
end;

procedure TNode.SetLocation(const Value: TPoint2D);
begin
  FLocation := Value;
end;

procedure TNode.SetNodeNumber(Value: Integer);
begin
  FNodeNumber := Value;
end;

function TNode.SideElimination: boolean;
{
         ElementToKeep2
               NodeG
               /   \
            /        \
         /             \
      NodeC           NodeD
         \             /
           \         /
             \     /
              NodeB
                |
ElementToDel1   |  ElementToDelete2
                |
             A = self
              /  \
            /       \
          /           \
       NodeE         NodeF
         \             /
           \         /
             \     /
              NodeH
         ElementToKeep1

  In ElementToKeep1 replace NodeB with NodeF
  In ElementToKeep2 replace self with NodeC
  OR
  In ElementToKeep1 replace NodeB with NodeE
  In ElementToKeep2 replace self with NodeD

  Delete ElementToDelete1 and ElementToDelete2
  Delete NodeB and self.
}
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  BoundaryNodeIndex: Integer;
  NodeBIndex: Integer;
  NodeB: TNode;
  InnerElementIndex: Integer;
  FirstElementToDelete: TBoundary;
  NextNodeIndex2: Integer;
  NextElementIndex: Integer;
  SecondElementToDelete: TBoundary;
  ElementToKeep1: TBoundary;
  ElementToKeep2: TBoundary;
  NodeCIndex: Integer;
  NodeC: TNode;
  NodeDIndex: Integer;
  NodeD: TNode;
  NodeEIndex: Integer;
  NodeE: TNode;
  NodeFIndex: Integer;
  NodeF: TNode;
  CFCount: Integer;
  DECount: Integer;
  NodeBIndexToDelete: Integer;
  Epsilon: double;
  procedure HandleSideToBeDeleted;
  var
    KeepEleIndex: Integer;
    BoundaryNodes: TNodeList;
    NodeIndex: Integer;
    ANode: TNode;
    CanHandleCF: boolean;
    CanHandleDE: boolean;
    NodeGIndex: Integer;
    NodeG: TNode;
    NodeHIndex: Integer;
    NodeH: TNode;
  begin
    NextElementIndex := 3 - ElementIndex - InnerElementIndex;
    Assert(NextElementIndex < FElements.Count);
    SecondElementToDelete := FElements[ElementIndex];
    Assert((FirstElementToDelete = AnElement) or
      (SecondElementToDelete = AnElement));

    ElementToKeep1 := FElements[NextElementIndex];

    ElementToKeep2 := nil;
    for KeepEleIndex := 0 to NodeB.FElements.Count - 1 do
    begin
      if (NodeB.FElements[KeepEleIndex] <> FirstElementToDelete) and
        (NodeB.FElements[KeepEleIndex] <> SecondElementToDelete) then
      begin
        ElementToKeep2 := NodeB.FElements[KeepEleIndex];
        break;
      end;
    end;
    Assert(ElementToKeep2 <> nil);

    NodeCIndex := FirstElementToDelete.IndexOfNode(self);
    Assert(NodeCIndex >= 0);
    Inc(NodeCIndex, 2);
    if NodeCIndex >= FirstElementToDelete.Count then
    begin
      Dec(NodeCIndex, 4);
      Assert(NodeCIndex >= 0);
    end;
    NodeC := FirstElementToDelete[NodeCIndex].FNode;

    NodeDIndex := SecondElementToDelete.IndexOfNode(self);
    Assert(NodeDIndex >= 0);
    Inc(NodeDIndex, 2);
    if NodeDIndex >= SecondElementToDelete.Count then
    begin
      Dec(NodeDIndex, 4);
      Assert(NodeDIndex >= 0);
    end;
    NodeD := SecondElementToDelete[NodeDIndex].FNode;

    NodeEIndex := FirstElementToDelete.IndexOfNode(NodeB);
    Assert(NodeEIndex >= 0);
    Inc(NodeEIndex, 2);
    if NodeEIndex >= FirstElementToDelete.Count then
    begin
      Dec(NodeEIndex, 4);
      Assert(NodeEIndex >= 0);
    end;
    NodeE := FirstElementToDelete[NodeEIndex].FNode;

    NodeFIndex := SecondElementToDelete.IndexOfNode(NodeB);
    Assert(NodeFIndex >= 0);
    Inc(NodeFIndex, 2);
    if NodeFIndex >= SecondElementToDelete.Count then
    begin
      Dec(NodeFIndex, 4);
      Assert(NodeFIndex >= 0);
    end;
    NodeF := SecondElementToDelete[NodeFIndex].FNode;

    NodeGIndex := ElementToKeep2.IndexOfNode(NodeB);
    Assert(NodeGIndex >= 0);
    Inc(NodeGIndex, 2);
    if NodeGIndex >= ElementToKeep2.Count then
    begin
      Dec(NodeGIndex, 4);
      Assert(NodeGIndex >= 0);
    end;
    NodeG := ElementToKeep2[NodeGIndex].FNode;

    NodeHIndex := ElementToKeep1.IndexOfNode(self);
    Assert(NodeHIndex >= 0);
    Inc(NodeHIndex, 2);
    if NodeHIndex >= ElementToKeep1.Count then
    begin
      Dec(NodeHIndex, 4);
      Assert(NodeHIndex >= 0);
    end;
    NodeH := ElementToKeep1[NodeHIndex].FNode;

    BoundaryNodes := TNodeList.Create;
    try
      CanHandleCF := Intersect(EquateSegment(NodeG.Location, NodeF.Location),
        EquateSegment(NodeC.Location, NodeD.Location));
      CanHandleCF := CanHandleCF and
        Intersect(EquateSegment(NodeC.Location, NodeH.Location),
        EquateSegment(NodeE.Location, NodeF.Location));
      if CanHandleCF then
      begin
        if (NodeG.NodeType <> ntInner)
          and (NodeD.NodeType <> ntInner)
          and (NodeF.NodeType <> ntInner)
          and RobustCollinear(NodeG.Location, NodeD.Location, NodeF.Location,
            Epsilon)
          then
        begin
          CanHandleCF := False;
        end;
        if (NodeG.NodeType <> ntInner)
          and (NodeD.NodeType <> ntInner)
          and (NodeC.NodeType <> ntInner)
          and RobustCollinear(NodeG.Location, NodeD.Location, NodeC.Location,
            Epsilon)
          then
        begin
          CanHandleCF := False;
        end;
        if (NodeG.NodeType <> ntInner)
          and (NodeE.NodeType <> ntInner)
          and (NodeH.NodeType <> ntInner)
          and RobustCollinear(NodeG.Location, NodeE.Location, NodeH.Location,
            Epsilon)
          then
        begin
          CanHandleCF := False;
        end;
        if (NodeG.NodeType <> ntInner)
          and (NodeE.NodeType <> ntInner)
          and (NodeC.NodeType <> ntInner)
          and RobustCollinear(NodeG.Location, NodeE.Location, NodeC.Location,
            Epsilon)
          then
        begin
          CanHandleCF := False;
        end;
      end;
      if CanHandleCF then
      begin
        for NodeIndex := 0 to ElementToKeep1.Count - 2 do
        begin
          ANode := ElementToKeep1[NodeIndex].FNode;
          if (ANode <> self) and (ANode.NodeType in [ntEdge, ntSubDomain]) then
          begin
            BoundaryNodes.Add(ANode);
          end;
        end;
        if NodeC.NodeType in [ntEdge, ntSubDomain] then
        begin
          BoundaryNodes.Add(NodeC);
        end;
        if BoundaryNodes.Count = 3 then
        begin
          CanHandleCF := not NearlyStraightAngle(BoundaryNodes[0].Location,
            BoundaryNodes[1].Location, BoundaryNodes[2].Location)
        end
        else
        begin
          CanHandleCF := True;
        end;
        if CanHandleCF then
        begin
          BoundaryNodes.Clear;
          for NodeIndex := 0 to ElementToKeep2.Count - 2 do
          begin
            ANode := ElementToKeep2[NodeIndex].FNode;
            if (ANode <> NodeB) and (ANode.NodeType in [ntEdge, ntSubDomain])
            then
            begin
              BoundaryNodes.Add(ANode);
            end;
          end;
          if NodeF.NodeType in [ntEdge, ntSubDomain] then
          begin
            BoundaryNodes.Add(NodeF);
          end;
          if BoundaryNodes.Count = 3 then
          begin
            CanHandleCF := not NearlyStraightAngle(BoundaryNodes[0].Location,
              BoundaryNodes[1].Location, BoundaryNodes[2].Location)
          end
          else
          begin
            CanHandleCF := True;
          end;
        end;
      end;

      CanHandleDE := Intersect(EquateSegment(NodeG.Location, NodeE.Location),
        EquateSegment(NodeC.Location, NodeD.Location));
      CanHandleDE := CanHandleDE and
        Intersect(EquateSegment(NodeD.Location, NodeH.Location),
        EquateSegment(NodeE.Location, NodeF.Location));
      if CanHandleDE then
      begin
        if (NodeE.NodeType <> ntInner)
          and (NodeC.NodeType <> ntInner)
          and (NodeG.NodeType <> ntInner)
          and RobustCollinear(NodeE.Location, NodeC.Location, NodeG.Location,
            Epsilon)
          then
        begin
          CanHandleDE := False;
        end;
        if (NodeG.NodeType <> ntInner)
          and (NodeD.NodeType <> ntInner)
          and (NodeC.NodeType <> ntInner)
          and RobustCollinear(NodeG.Location, NodeD.Location, NodeC.Location,
            Epsilon)
          then
        begin
          CanHandleDE := False;
        end;
        if (NodeD.NodeType <> ntInner)
          and (NodeF.NodeType <> ntInner)
          and (NodeH.NodeType <> ntInner)
          and RobustCollinear(NodeD.Location, NodeF.Location, NodeH.Location,
            Epsilon)
          then
        begin
          CanHandleDE := False;
        end;
        if (NodeF.NodeType <> ntInner)
          and (NodeH.NodeType <> ntInner)
          and (NodeE.NodeType <> ntInner)
          and RobustCollinear(NodeF.Location, NodeH.Location, NodeE.Location,
            Epsilon)
          then
        begin
          CanHandleDE := False;
        end;
      end;
      if CanHandleDE then
      begin

        BoundaryNodes.Clear;
        for NodeIndex := 0 to ElementToKeep1.Count - 2 do
        begin
          ANode := ElementToKeep1[NodeIndex].FNode;
          if (ANode <> self) and (ANode.NodeType in [ntEdge, ntSubDomain]) then
          begin
            BoundaryNodes.Add(ANode);
          end;
        end;
        if NodeD.NodeType in [ntEdge, ntSubDomain] then
        begin
          BoundaryNodes.Add(NodeD);
        end;
        if BoundaryNodes.Count = 3 then
        begin
          CanHandleDE := not NearlyStraightAngle(BoundaryNodes[0].Location,
            BoundaryNodes[1].Location, BoundaryNodes[2].Location)
        end
        else
        begin
          CanHandleDE := True;
        end;
        if CanHandleDE then
        begin
          BoundaryNodes.Clear;
          for NodeIndex := 0 to ElementToKeep2.Count - 2 do
          begin
            ANode := ElementToKeep2[NodeIndex].FNode;
            if (ANode <> NodeB) and (ANode.NodeType in [ntEdge, ntSubDomain])
            then
            begin
              BoundaryNodes.Add(ANode);
            end;
          end;
          if NodeE.NodeType in [ntEdge, ntSubDomain] then
          begin
            BoundaryNodes.Add(NodeE);
          end;
          if BoundaryNodes.Count = 3 then
          begin
            CanHandleDE := not NearlyStraightAngle(BoundaryNodes[0].Location,
              BoundaryNodes[1].Location, BoundaryNodes[2].Location)
          end
          else
          begin
            CanHandleDE := True;
          end;
        end;
      end;
    finally
      BoundaryNodes.Free;
    end;

    if CanHandleDE and CanHandleCF then
    begin
      CFCount := NodeC.ElementCount - NodeC.FDesiredElementCount +
        NodeF.ElementCount - NodeF.FDesiredElementCount;
      DECount := NodeD.ElementCount - NodeD.FDesiredElementCount +
        NodeE.ElementCount - NodeE.FDesiredElementCount;

      if CFCount <= DECount then
      begin
        // Make new edge joining nodes C and F
        NodeB.ReplaceNodeInElement(NodeF, ElementToKeep2);
        NodeF.FElements.Add(ElementToKeep2);
        ReplaceNodeInElement(NodeC, ElementToKeep1);
        NodeC.FElements.Add(ElementToKeep1);
      end
      else
      begin
        // Make new edge joining nodes D and E
        NodeB.ReplaceNodeInElement(NodeE, ElementToKeep2);
        NodeE.FElements.Add(ElementToKeep2);
        ReplaceNodeInElement(NodeD, ElementToKeep1);
        NodeD.FElements.Add(ElementToKeep1);
      end;
    end
    else if CanHandleCF then
    begin
      // Make new edge joining nodes C and F
      NodeB.ReplaceNodeInElement(NodeF, ElementToKeep2);
      NodeF.FElements.Add(ElementToKeep2);
      ReplaceNodeInElement(NodeC, ElementToKeep1);
      NodeC.FElements.Add(ElementToKeep1);
    end
    else if CanHandleDE then
    begin
      // Make new edge joining nodes D and E
      NodeB.ReplaceNodeInElement(NodeE, ElementToKeep2);
      NodeE.FElements.Add(ElementToKeep2);
      ReplaceNodeInElement(NodeD, ElementToKeep1);
      NodeD.FElements.Add(ElementToKeep1);
    end
    else
    begin
      Exit;
    end;

    FirstElementToDelete.RemoveSelfFromOwnNodes;
    FirstElementToDelete.FParent.FSubParts.Remove(FirstElementToDelete);

    SecondElementToDelete.RemoveSelfFromOwnNodes;
    SecondElementToDelete.FParent.FSubParts.Remove(SecondElementToDelete);

    NodeBIndexToDelete := FQuadMeshCreator.FNodes.IndexOf(NodeB);
    Assert(NodeBIndexToDelete >= 0);
    FQuadMeshCreator.FNodes.Delete(NodeBIndexToDelete);

    result := True;
{$IFDEF TEST}
    ElementToKeep1.CheckInvalidElement;
    ElementToKeep2.CheckInvalidElement;
{$ENDIF}
  end;

begin
  result := False;
  Assert(FElements.Count = 3);
  if FNodeType <> ntInner then
  begin
    Exit;
  end;
  Epsilon := FQuadMeshCreator.FCharacteristicLength/1e7;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    BoundaryNodeIndex := AnElement.IndexOfNode(self);
    Assert(BoundaryNodeIndex >= 0);

    NodeBIndex := BoundaryNodeIndex + 1;
    Assert(NodeBIndex < AnElement.Count);

    NodeB := AnElement[NodeBIndex].FNode;
    if (NodeB.FNodeType = ntInner) and (NodeB.FElements.Count = 3) then
    begin
      for InnerElementIndex := 0 to FElements.Count - 1 do
      begin
        if InnerElementIndex = ElementIndex then
        begin
          Continue
        end;
        FirstElementToDelete := FElements[InnerElementIndex];
        NextNodeIndex2 := FirstElementToDelete.IndexOfNode(NodeB);
        if NextNodeIndex2 >= 0 then
        begin
          HandleSideToBeDeleted;
          if result then
          begin
            Exit;
          end;
        end;
      end;
    end;

    NodeBIndex := BoundaryNodeIndex - 1;
    if NodeBIndex < 0 then
    begin
      NodeBIndex := AnElement.Count - 2;
      Assert(NodeBIndex >= 0);
    end;
    NodeB := AnElement[NodeBIndex].FNode;
    if (NodeB.FNodeType = ntInner) and (NodeB.FElements.Count = 3) then
    begin
      for InnerElementIndex := 0 to FElements.Count - 1 do
      begin
        if InnerElementIndex = ElementIndex then
        begin
          Continue
        end;
        FirstElementToDelete := FElements[InnerElementIndex];
        NextNodeIndex2 := FirstElementToDelete.IndexOfNode(NodeB);
        if NextNodeIndex2 >= 0 then
        begin
          HandleSideToBeDeleted;
          if result then
          begin
            Exit;
          end;
        end;
      end;
    end;

  end;
end;

{ TNodeInBoundary }

procedure TNodeInBoundary.AddSegment(ASegment: TSegment);
begin
  FSegments.Add(ASegment);
end;

constructor TNodeInBoundary.Create(Node: TNode; Boundary: TBoundary;
Segment: TSegment);
begin
  FNode := Node;
  Assert(Boundary <> nil);
  Assert(Segment <> nil);
  FSegments := TSegmentList.Create;
  if FNode.FElements.IndexOf(Boundary) < 0 then
  begin
    FNode.FElements.Add(Boundary);
    FNode.FSegments.Add(Segment);
  end;
  FBoundary := Boundary;
  AddSegment(Segment);
  FBoundary.FQuadMeshCreator.FBoundaryNodes.Add(self);
end;

procedure TNodeInBoundary.DeleteSegment(Index: Integer);
begin
  FSegments.Delete(index);
end;

destructor TNodeInBoundary.Destroy;
begin
  FSegments.Free;
  inherited;
end;

function TNodeInBoundary.GetDesiredSpacing: double;
begin
  result := FNode.DesiredSpacing;
end;

function TNodeInBoundary.GetElementCount: Integer;
begin
  result := FNode.ElementCount;
end;

function TNodeInBoundary.GetLocation: TPoint2D;
begin
  result := FNode.Location;
end;

function TNodeInBoundary.GetNodeType: TNodeType;
begin
  result := FNode.NodeType;
end;

function TNodeInBoundary.GetSegment(Index: Integer): TSegment;
begin
  result := FSegments[Index];
end;

function TNodeInBoundary.GetSegmentCount: Integer;
begin
  result := FSegments.Count;
end;

function TNodeInBoundary.GetX: double;
begin
  result := FNode.X;
end;

function TNodeInBoundary.GetY: double;
begin
  result := FNode.Y;
end;

function TNodeInBoundary.IndexOfSegment(ASegment: TSegment): integer;
begin
  result := FSegments.IndexOf(ASegment);
end;

procedure TNodeInBoundary.InsertSegment(Index: Integer; ASegment: TSegment);
begin
  FSegments.Insert(Index, ASegment);
end;

function TNodeInBoundary.OnSameSegment(ANode: TNodeInBoundary): boolean;
var
  SegmentIndex: Integer;
  ASegment: TSegment;
  InnerSegmentIndex: Integer;
  AnotherSegment: TSegment;
begin
  result := False;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    if ANode.FSegments.IndexOf(ASegment) >= 0 then
    begin
      result := True;
      Exit;
    end;
    for InnerSegmentIndex := 0 to ANode.FSegments.Count - 1 do
    begin
      AnotherSegment := ANode.FSegments[InnerSegmentIndex];
      if (ASegment.FNode1 = AnotherSegment.FNode2)
        and (ASegment.FNode2 = AnotherSegment.FNode1) then
      begin
        result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TNodeInBoundary.RemoveSegment(ASegment: TSegment);
begin
  FSegments.Remove(ASegment);
end;

procedure TNodeInBoundary.ReverseSegments;
begin
  FSegments.Reverse;
end;

procedure TNodeInBoundary.SetSegment(Index: Integer; const Value: TSegment);
begin
  FSegments[Index] := Value;
end;

{ TSegment }

function TSegment.NodesToInsert: Integer;
var
  NodeDistance: TFloat;
  NStar: Extended;
  NodeSpacing: double;
  Count: Integer;
  LastSpacing: double;
  ElapsedDistance: double;
  NextDistance: double;
begin
  NodeDistance := Distance(Node1.Location, Node2.Location);

  if Node1.DesiredSpacing = Node2.DesiredSpacing then
  begin
    NStar := NodeDistance / Node1.DesiredSpacing;
  end
  else
  begin
    if Node1.DesiredSpacing < Node2.DesiredSpacing then
    begin
      NodeSpacing := Node1.DesiredSpacing;
      LastSpacing := Node2.DesiredSpacing;
    end
    else
    begin
      NodeSpacing := Node2.DesiredSpacing;
      LastSpacing := Node1.DesiredSpacing;
    end;
    Count := 0;
    ElapsedDistance := 0;
    repeat
      NextDistance := ElapsedDistance + NodeSpacing;
      if NextDistance >= NodeDistance then
      begin
        break;
      end;
      ElapsedDistance := NextDistance;
      Inc(Count);
      if NodeSpacing < LastSpacing then
      begin
        NodeSpacing := NodeSpacing * FQuadMeshCreator.GrowthRate;
      end;
    until (NodeSpacing >= LastSpacing);

    NStar := Count + (NodeDistance - ElapsedDistance) / LastSpacing;
  end;

  result := CalcNodesToInsert(NStar);


end;

procedure TSegment.Reverse;
var
  Temp: TNode;
begin
  Temp := FNode1;
  FNode1 := FNode2;
  FNode2 := Temp;
  FInnerNodes.Reverse;
end;

function TSegment.ContainsNode(ANode: TNode): Boolean;
begin
  result := (ANode = FNode1) or (ANode = FNode2)
    or (FInnerNodes.IndexOf(ANode) >= 0);
end;

constructor TSegment.Create(Node1, Node2: TNode; SegmentType: TSegmentType;
Boundary: TBoundary; QuadMeshCreator: TQuadMeshCreator);
begin
  FNode1 := Node1;
  FNode2 := Node2;
  FSegmentType := SegmentType;
  FQuadMeshCreator := QuadMeshCreator;
  FBoundary := Boundary;
  FInnerNodes := TNodeList.Create;
  FNode1.FSegments.Add(self);
  FNode2.FSegments.Add(self);
end;

function TSegment.CreateReversedSegment: TSegment;
var
  NodeIndex: Integer;
  ANode: TNode;
begin
  result := TSegment.Create(Node2, Node1, SegmentType, FBoundary,
    FQuadMeshCreator);
  result.FInnerNodes.Capacity := FInnerNodes.Count;
  for NodeIndex := FInnerNodes.Count - 1 downto 0 do
  begin
    ANode := FInnerNodes[NodeIndex];
    result.FInnerNodes.Add(ANode);
    ANode.FSegments.Add(self);
  end;
end;

destructor TSegment.Destroy;
begin
  FInnerNodes.Free;
  inherited;
end;

procedure TSegment.InsertNodes(NumberToInsert: Integer);
var
  DeltaX: double;
  DeltaY: double;
  NewNodeIndex: Integer;
  NewNode: TNode;
  NodeDistance: TFloat;
  Distances, Spacings: TRealList;
  NodeSpacing: double;
  LastSpacing: double;
  ElapsedDistance: double;
  Factor: double;
  NodeIndex: Integer;
  NewNodeX: Double;
  NewNodeY: Double;
  NodeTree: TRbwQuadTree;
  Epsilon: double;
  procedure InsertNode;
  begin
    case SegmentType of
      stInner:
        NewNode.FNodeType := ntInner;
      stEdge:
        NewNode.FNodeType := ntEdge;
      stSubDomain:
        NewNode.FNodeType := ntSubDomain;
    else
      Assert(False);
    end;
    FInnerNodes.Add(NewNode);
  end;

begin
  Assert(NumberToInsert >= 1);
  Assert(FInnerNodes.Count = 0);
  DeltaX := Node2.X - Node1.X;
  DeltaY := Node2.Y - Node1.Y;
  NodeTree := FQuadMeshCreator.FNodeQuadTree;
  Epsilon := Min((NodeTree.XMax-NodeTree.XMin),(NodeTree.YMax-NodeTree.YMin));
  Epsilon := Epsilon/1e7;
  if Node1.DesiredSpacing = Node2.DesiredSpacing then
  begin
    for NewNodeIndex := 1 to NumberToInsert do
    begin
      Factor := NewNodeIndex / (NumberToInsert + 1);
      NewNodeX := Node1.X + DeltaX * Factor;
      NewNodeY := Node1.Y + DeltaY * Factor;

      NewNode := NodeTree.NearestPointsFirstData(NewNodeX, NewNodeY);
      if (Abs(NewNode.X - NewNodeX)>Epsilon) or (Abs(NewNode.Y - NewNodeY)>Epsilon) then
      begin
        NewNode := TNode.Create(FQuadMeshCreator, Node1.DesiredSpacing);
        case SegmentType of
          stInner: NewNode.FNodeType := ntInner;
          stSubDomain:  NewNode.FNodeType := ntSubDomain;
          stEdge:  NewNode.FNodeType := ntEdge;
        end;
        NewNode.X := NewNodeX;
        NewNode.Y := NewNodeY;
        NodeTree.AddPoint(NewNode.X, NewNode.Y, NewNode);
      end;
      InsertNode;
    end;
  end
  else
  begin
    NodeDistance := Distance(Node1.Location, Node2.Location);
    Distances := TRealList.Create;
    Spacings := TRealList.Create;
    try
      if Node1.DesiredSpacing > Node2.DesiredSpacing then
      begin
        NodeSpacing := Node2.DesiredSpacing;
        LastSpacing := Node1.DesiredSpacing
      end
      else
      begin
        NodeSpacing := Node1.DesiredSpacing;
        LastSpacing := Node2.DesiredSpacing
      end;

      ElapsedDistance := 0;
      while ElapsedDistance < NodeDistance do
      begin
        ElapsedDistance := ElapsedDistance + NodeSpacing;
        Distances.Add(ElapsedDistance);
        Spacings.Add(NodeSpacing);
        if NodeSpacing < LastSpacing then
        begin
          NodeSpacing := NodeSpacing * FQuadMeshCreator.GrowthRate;
          if NodeSpacing > LastSpacing then
          begin
            NodeSpacing := LastSpacing;
          end;
        end;
      end;

      While Distances.Count < NumberToInsert do
      begin
        ElapsedDistance := ElapsedDistance + NodeSpacing;
        Distances.Add(ElapsedDistance);
        Spacings.Add(NodeSpacing);
        if NodeSpacing < LastSpacing then
        begin
          NodeSpacing := NodeSpacing * FQuadMeshCreator.GrowthRate;
          if NodeSpacing > LastSpacing then
          begin
            NodeSpacing := LastSpacing;
          end;
        end;
      end;
      if Distances.Count > NumberToInsert then
      begin
        Distances.Count := NumberToInsert;
        Spacings.Count := NumberToInsert;
      end
      else
      begin
        ElapsedDistance := ElapsedDistance + NodeSpacing;
      end;
      if Node1.DesiredSpacing > Node2.DesiredSpacing then
      begin
        Distances.Reverse;
        Spacings.Reverse;
        for NodeIndex := 0 to Distances.Count - 1 do
        begin
          Distances[NodeIndex] := ElapsedDistance - Distances[NodeIndex];
        end;
      end;

      for NewNodeIndex := 0 to Distances.Count - 1 do
      begin
        Factor := Distances[NewNodeIndex] / ElapsedDistance;
        Assert(Factor > 0);
        Assert(Factor < 1);
        NewNodeX := Node1.X + DeltaX * Factor;
        NewNodeY := Node1.Y + DeltaY * Factor;
        NewNode := NodeTree.NearestPointsFirstData(NewNodeX, NewNodeY);
        if (Abs(NewNode.X - NewNodeX)>Epsilon) or (Abs(NewNode.Y - NewNodeY)>Epsilon) then
        begin
          NewNode := TNode.Create(FQuadMeshCreator, LastSpacing);
          case SegmentType of
            stInner: NewNode.FNodeType := ntInner;
            stSubDomain:  NewNode.FNodeType := ntSubDomain;
            stEdge:  NewNode.FNodeType := ntEdge;
          end;
          NewNode.X := NewNodeX;
          NewNode.Y := NewNodeY;
          NodeTree.AddPoint(NewNode.X, NewNode.Y, NewNode);
        end;
        InsertNode;
      end;
    finally
      Distances.Free;
      Spacings.Free;
    end;
  end;
end;

function TSegment.Length: double;
begin
  if FNode1 = FNode2 then
  begin
    result := 0;
  end
  else
  begin
    result := Distance(FNode1.Location, FNode2.Location);
  end;
end;

function TSegment.Split(ANode: TNode): TSegmentList;
var
  Position: Integer;
  SubSeg: TSegment;
  NodeIndex: Integer;
  AnotherNode: TNode;
begin
  Position := FInnerNodes.IndexOf(ANode);
  Assert(Position >= 0);
  result := TSegmentList.Create;
  SubSeg := TSegment.Create(Node1, ANode, SegmentType, FBoundary,
    FQuadMeshCreator);
  result.Add(SubSeg);
  for NodeIndex := 0 to Position - 1 do
  begin
    AnotherNode := FInnerNodes[NodeIndex];
    AnotherNode.FSegments.Remove(self);
    SubSeg.FInnerNodes.Add(AnotherNode);
    if AnotherNode.FSegments.IndexOf(SubSeg) < 0 then
    begin
      AnotherNode.FSegments.Add(SubSeg);
    end;

  end;
  SubSeg := TSegment.Create(ANode, Node2, SegmentType, FBoundary,
    FQuadMeshCreator);
  result.Add(SubSeg);
  for NodeIndex := Position + 1 to FInnerNodes.Count - 1 do
  begin
    AnotherNode := FInnerNodes[NodeIndex];
    AnotherNode.FSegments.Remove(self);
    SubSeg.FInnerNodes.Add(AnotherNode);
    if AnotherNode.FSegments.IndexOf(SubSeg) < 0 then
    begin
      AnotherNode.FSegments.Add(SubSeg);
    end;
  end;
  Node1.FSegments.Remove(self);
  Node2.FSegments.Remove(self);
  ANode.FSegments.Remove(self);
end;

{ TNodeComparer }

function TNodeAngleComparer.Compare(const Left, Right: TNode): Integer;
var
  Angle1: double;
  Angle2: double;
begin
  Angle1 := ArcTan2(Right.Y - FCenterNode.Y, Right.X - FCenterNode.X);
  Angle2 := ArcTan2(Left.Y - FCenterNode.Y, Left.X - FCenterNode.X);
  result := Sign(Angle2 - Angle1);
end;

constructor TNodeAngleComparer.Create(CenterNode: TNode);
begin
  FCenterNode := CenterNode;
  Assert(FCenterNode <> nil);
end;

{ TNodeSpacingComparer }

function TNodeSpacingComparer.Compare(const Left, Right: TNode): Integer;
begin
  result := Sign(Right.DesiredSpacing - Left.DesiredSpacing);
end;

{ TBoundaryLink }

constructor TBoundaryLink.Create;
begin
  Children := TBoundaryLinkObjectList.Create;
end;

destructor TBoundaryLink.Destroy;
begin
  Children.Free;
  inherited;
end;


{ TBoundaryCountComparer }

function TBoundaryCountComparer.Compare(const Left, Right: TBoundary): Integer;
begin
  Result := Left.Count - Right.Count;
end;

{ TNodeConnection }

procedure TNodeConnection.AddLink(ABoundary: TBoundary);
var
  CompareItem: TAngleCompareItem;
  OtherNode: TNode;
begin
  CompareItem := TAngleCompareItem.Create;
  CompareItem.Boundary := ABoundary;
  if ABoundary.FNodes.First = FNode then
  begin
    CompareItem.Direction := dForward;
    OtherNode := ABoundary.FNodes[1];
  end
  else
  begin
    Assert(ABoundary.FNodes.Last = FNode);
    CompareItem.Direction := dBackward;
    OtherNode := ABoundary.FNodes[ABoundary.FNodes.Count -2];
  end;
  CompareItem.Angle := ArcTan2(OtherNode.Y-FNode.Y, OtherNode.X-FNode.X);
  FConnections.Add(CompareItem);
end;

constructor TNodeConnection.Create(ANode: TNode);
begin
  FNode := ANode;
  FConnections := TAngleList.Create;
end;

destructor TNodeConnection.Destroy;
begin
  FConnections.Free;
  inherited;
end;

procedure TNodeConnection.RemoveLink(ABoundary: TBoundary);
var
  Index: Integer;
begin
  for Index := FConnections.Count - 1 downto 0 do
  begin
    if FConnections[Index].Boundary = ABoundary then
    begin
      FConnections.Delete(Index);
    end;
  end;
end;

procedure TNodeConnection.Sort;
var
  Comparer: TAngleComparer;
begin
  Comparer := TAngleComparer.Create;
  try
    FConnections.Sort(Comparer);
  finally
    Comparer.Free;
  end;
//  FConnections.Reverse;
end;

{ TAngleComparer }

function TAngleComparer.Compare(const Left, Right: TAngleCompareItem): Integer;
begin
  result := 0;
  if Right = Left then
  begin
    Exit;
  end;
  result := Sign(Right.Angle - Left.Angle);
  if result = 0 then
  begin
    Result := -(Ord(Right.Direction) - Ord(Left.Direction));
  end;
end;

{ TBoundaryAreaComparer }

function TBoundaryAreaComparer.Compare(const Left, Right: TBoundary): Integer;
begin
  Result := Sign(Right.Area - Left.Area);
end;

{ TGeompackOptions }

procedure TGeompackOptions.Assign(Source: TPersistent);
var
  SourceOptions: TGeompackOptions;
begin
  if Source is TGeompackOptions then
  begin
    SourceOptions := TGeompackOptions(Source);
    Tolerance := SourceOptions.Tolerance;

    SpacingAngle := SourceOptions.SpacingAngle;
    ToleranceAngle := SourceOptions.ToleranceAngle;
    UniformnessParameter := SourceOptions.UniformnessParameter;
    MeshDistributionVariation := SourceOptions.MeshDistributionVariation;
    QuadSplittingValue := SourceOptions.QuadSplittingValue;
    OptimizationBasedSmoothingCriterion :=
      SourceOptions.OptimizationBasedSmoothingCriterion;

    ElementGenerationParameter := SourceOptions.ElementGenerationParameter;
    DesiredElementCount := SourceOptions.DesiredElementCount;
    AutomaticeElementCount := SourceOptions.AutomaticeElementCount;
    ShapeMeasure := SourceOptions.ShapeMeasure;
    KeepQuadsAlongEdges := SourceOptions.KeepQuadsAlongEdges;
    MaxImprovementIterations := SourceOptions.MaxImprovementIterations;
    MaxSmoothingIterations := SourceOptions.MaxSmoothingIterations;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGeompackOptions.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredTolerance := TRealStorage.Create;
  FStoredTolerance.OnChange := OnChangeEventHander;

  FStoredToleranceAngle := TRealStorage.Create;
  FStoredToleranceAngle.OnChange := OnChangeEventHander;

  FStoredSpacingAngle := TRealStorage.Create;
  FStoredSpacingAngle.OnChange := OnChangeEventHander;

  FStoredMeshDistributionVariation := TRealStorage.Create;
  FStoredMeshDistributionVariation.OnChange := OnChangeEventHander;

  FStoredUniformnessParameter := TRealStorage.Create;
  FStoredUniformnessParameter.OnChange := OnChangeEventHander;

  FStoredOptimizationBasedSmoothingCriterion := TRealStorage.Create;
  FStoredOptimizationBasedSmoothingCriterion.OnChange := OnChangeEventHander;

  FStoredQuadSplittingValue := TRealStorage.Create;
  FStoredQuadSplittingValue.OnChange := OnChangeEventHander;

  Initialize;
end;

destructor TGeompackOptions.Destroy;
begin
  FStoredTolerance.Free;
  FStoredToleranceAngle.Free;
  FStoredSpacingAngle.Free;
  FStoredMeshDistributionVariation.Free;
  FStoredUniformnessParameter.Free;
  FStoredOptimizationBasedSmoothingCriterion.Free;
  FStoredQuadSplittingValue.Free;
  inherited;
end;

function TGeompackOptions.GetMeshDistributionVariation: Double;
begin
  result := StoredMeshDistributionVariation.Value;
end;

function TGeompackOptions.GetOptimizationBasedSmoothingCriterion: double;
begin
  result := StoredOptimizationBasedSmoothingCriterion.Value;
end;

function TGeompackOptions.GetQuadSplittingValue: double;
begin
  result := StoredQuadSplittingValue.Value;
end;

function TGeompackOptions.GetSpacingAngle: Double;
begin
  result := StoredSpacingAngle.Value;
end;

function TGeompackOptions.GetTolerance: Double;
begin
  result := StoredTolerance.Value;
end;

function TGeompackOptions.GetToleranceAngle: Double;
begin
  result := StoredToleranceAngle.Value;
end;

function TGeompackOptions.GetUniformnessParameter: Double;
begin
  result := StoredUniformnessParameter.Value;
end;

procedure TGeompackOptions.Initialize;
begin
  Tolerance := 1e-9;
  SpacingAngle := 30;
  ToleranceAngle := 20;
  UniformnessParameter := 0.25;
  MeshDistributionVariation := 0.4;
  QuadSplittingValue := 0.01;
  OptimizationBasedSmoothingCriterion := 0.35;
  ElementGenerationParameter := 10;
  DesiredElementCount := 0;
  AutomaticeElementCount := True;
  ShapeMeasure := gsmQuadraticMeanRatio;
  KeepQuadsAlongEdges := True;
  MaxImprovementIterations := 3;
  MaxSmoothingIterations := 2;
end;

procedure TGeompackOptions.SetAutomaticeElementCount(const Value: boolean);
begin
  SetBooleanProperty(FAutomaticeElementCount, Value);
end;

procedure TGeompackOptions.SetDesiredElementCount(const Value: integer);
begin
  SetIntegerProperty(FDesiredElementCount, Value);
end;

procedure TGeompackOptions.SetElementGenerationParameter(const Value: integer);
begin
  SetIntegerProperty(FElementGenerationParameter, Value);
end;

procedure TGeompackOptions.SetKeepQuadsAlongEdges(const Value: Boolean);
begin
  SetbooleanProperty(FKeepQuadsAlongEdges, Value);
end;

procedure TGeompackOptions.SetMaxImprovementIterations(const Value: integer);
begin
  SetIntegerProperty(FMaxImprovementIterations, Value);
end;

procedure TGeompackOptions.SetMaxSmoothingIterations(const Value: integer);
begin
  SetIntegerProperty(FMaxSmoothingIterations, Value);
end;

procedure TGeompackOptions.SetMeshDistributionVariation(const Value: Double);
begin
  StoredMeshDistributionVariation.Value := Value;
end;

procedure TGeompackOptions.SetOptimizationBasedSmoothingCriterion(
  const Value: double);
begin
  StoredOptimizationBasedSmoothingCriterion.Value := Value;
end;

procedure TGeompackOptions.SetQuadSplittingValue(const Value: double);
begin
  StoredQuadSplittingValue.Value := Value;
end;

procedure TGeompackOptions.SetShapeMeasure(const Value: TGeompackShapeMeasure);
begin
  if FShapeMeasure <> Value then
  begin
    FShapeMeasure := Value;
    InvalidateModel;
  end;
end;

procedure TGeompackOptions.SetSpacingAngle(const Value: Double);
begin
  StoredSpacingAngle.Value := Value;
end;

procedure TGeompackOptions.SetStoredMeshDistributionVariation(
  const Value: TRealStorage);
begin
  FStoredMeshDistributionVariation.Assign(Value);
end;

procedure TGeompackOptions.SetStoredOptimizationBasedSmoothingCriterion(
  const Value: TRealStorage);
begin
  FStoredOptimizationBasedSmoothingCriterion.Assign(Value);
end;

procedure TGeompackOptions.SetStoredQuadSplittingValue(
  const Value: TRealStorage);
begin
  FStoredQuadSplittingValue.Assign(Value);
end;

procedure TGeompackOptions.SetStoredSpacingAngle(const Value: TRealStorage);
begin
  FStoredSpacingAngle.Assign(Value);
end;

procedure TGeompackOptions.SetStoredTolerance(const Value: TRealStorage);
begin
  FStoredTolerance.Assign(Value);
end;

procedure TGeompackOptions.SetStoredToleranceAngle(const Value: TRealStorage);
begin
  FStoredToleranceAngle.Assign(Value);
end;

procedure TGeompackOptions.SetStoredUniformnessParameter(
  const Value: TRealStorage);
begin
  FStoredUniformnessParameter.Assign(Value);
end;

procedure TGeompackOptions.SetTolerance(const Value: Double);
begin
  StoredTolerance.Value := Value;
end;

procedure TGeompackOptions.SetToleranceAngle(const Value: Double);
begin
  StoredToleranceAngle.Value := Value;
end;

procedure TGeompackOptions.SetUniformnessParameter(const Value: Double);
begin
  StoredUniformnessParameter.Value := Value;
end;

initialization

SetDefaults;

end.
