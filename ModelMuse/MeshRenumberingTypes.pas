unit MeshRenumberingTypes;

interface

uses
  Generics.Collections, Generics.Defaults, FastGEO, //AbstractGridUnit,
  GoPhastTypes, IntervalTree, QuadtreeClass, DataSetUnit, System.Classes;

type
  {
    @name indicates the type of node.
    @value(ntInner The node is in the interior of the mesh and may be moved or
      eliminated to improve the mesh.)

    @value(ntEdge The node is on the external boundary of the mesh.
      It may not be moved or eliminated.)

    @value(ntSubDomain The node is on an internal boundary of the mesh.
      It may not be moved or eliminated.)

  }
  TNodeType = (ntInner, ntEdge, ntSubDomain);

  TRenumberingAlgorithm = (raNone, CuthillMcKee, raSloanRandolph);

  IElement = interface;
  IElement2D = interface;

  INode = interface(IInterface)
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: integer): IElement;
    function GetNodeNumber: integer;
    procedure SetNodeNumber(Value: integer);
    function GetLocation: TPoint2D;
    procedure SetLocation(const Value: TPoint2D);
    function GetNodeType: TNodeType;
    property ActiveElementCount: integer read GetActiveElementCount;
    property ActiveElements[Index: integer]: IElement read GetActiveElement;
    property NodeNumber: integer read GetNodeNumber write SetNodeNumber;
    property Location: TPoint2D read GetLocation write SetLocation;
    property NodeType: TNodeType read GetNodeType;
  end;
  
  ITriangulatable = interface ['{FDDDC4DE-9C35-48A4-B2A1-E8D0956AADB2}']
    function GetLocation: TPoint2D;
    function GetCountTri: Integer;
    function GetItemTri(Index: Integer): ITriangulatable;
    function GetTriangNumber: integer;
    procedure SetTriangNumber(const Value: Integer);
    function IsNeighbor(OtherItem: ITriangulatable): Boolean;
    property TriangNumber: Integer read GetTriangNumber write SetTriangNumber;
    function IndexOf(Item: ITriangulatable): Integer;
    property Location: TPoint2D read GetLocation;
    property ItemCount: Integer read GetCountTri;
    property ItemTri[Index: Integer]: ITriangulatable read GetItemTri ;
  end;

  INode2D = interface (INode) ['{3081F03E-4DCC-47BA-AF62-D6ED35522697}']
    function GetMaxX: Double;
    function GetMaxY: Double;
    function GetMinX: Double;
    function GetMinY: Double;
    function GetLocation: TPoint2D;
    function GetElementI(Index: integer): IElement2D;
    function CellIntersection(const Input: TSegment2D;
      out IntersectingSegments: TSegment2DArray): boolean;
    property MaxX: Double read GetMaxX;
    property MinX: Double read GetMinX;
    property MaxY: Double read GetMaxY;
    property MinY: Double read GetMinY;
    property Location: TPoint2D read GetLocation;
    property ElementsI[Index: Integer]: IElement2D read GetElementI;
  end;

  TINodeList = TList<INode>;
  TINodeComparer = TComparer<INode>;
  TINode2DList = TList<INode2D>;

  IElement = interface(IInterface)
    function GetActiveNode(Index: integer): INode;
    function GetActiveNodeCount: integer;
    function GetElementNumber: integer;
    function GetDisplayNumber: Integer;
    procedure SetElementNumber(Value: integer);
    property NodeCount: integer read GetActiveNodeCount;
    property Nodes[Index: integer]: INode read GetActiveNode;
    property ElementNumber: integer read GetElementNumber
      write SetElementNumber;
    property DisplayNumber: Integer read GetDisplayNumber;
  end;

//  IElement2D = interface;
  TIElement2DList = TList<IElement2D>;

  IElement2D = interface(IElement) ['{B8F1ACFD-7991-4EEE-940B-22B0556EF303}']
    function GetMaxX: Double;
    function GetMaxY: Double;
    function GetMinX: Double;
    function GetMinY: Double;
    function GetActiveNodeCount: Integer;
    function GetNodeI(Index: Integer): INode2D;
    function Intersection(const Input: TSegment2D;
      out IntersectingSegment: TSegment2D): boolean;
    property MinX: Double read GetMinX;
    property MaxX: Double read GetMaxX;
    property MinY: Double read GetMinY;
    property MaxY: Double read GetMaxY;
    function Center: TPoint2D;
    property NodeCount: Integer read GetActiveNodeCount;
    property NodesI[Index: integer]: INode2D read GetNodeI; 
    function IndexOfNode(Node: INode2D): Integer;
    procedure GetNeighborsI(ElementList: TIElement2DList);
  end;

  TIElementList = TList<IElement>;
  TIElementComparer = TComparer<IElement>;

  IMesh = interface(IInterface)
    function GetActiveNode(Index: integer): INode;
    function GetActiveNodeCount: integer;
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: integer): IElement;
    property NodeCount: integer read GetActiveNodeCount;
    property Nodes[Index: integer]: INode read GetActiveNode;
    property ElementCount: integer read GetActiveElementCount;
    property Elements[Index: integer]: IElement read GetActiveElement;
  end;

  INode3D = interface(INode)
    function GetActive: Boolean;
    function GetZ: FastGEO.TFloat;
    function GetTop: double;
    function GetBottom: double;
    property Active: Boolean read GetActive;
    property Z: FastGEO.TFloat read GetZ;
    property Top: double read GetTop;
    property Bottom: Double read GetBottom;
    function NodeLocation: TPoint3D;
    function NodeLocation2D: TPoint2D;
    function NodeNumber2D: Integer;
  end;

  IElement3D = interface(IElement)
    function GetActive: Boolean;
    property Active: Boolean read GetActive;
    function UpperElevation: double;
    function LowerElevation: double;
    function CenterElevation: double;
    function GetCenterLocation: TPoint3d;
    function GetLayer: Integer;
    property CenterLocation: TPoint3d read GetCenterLocation;
    function ElementNumber2D: Integer;
    function GetNodeI(Index: Integer): INode3D;
    function GetNodeLocation(Index: integer): TPoint3D;
    property Layer: Integer read GetLayer;
    property NodesI[Index: integer]: INode3D read GetNodeI;
    property NodeLocation[Index: integer]: TPoint3D read GetNodeLocation;
  end;

  IMesh2D = interface(IMesh)
    function GetActiveNodeI2D(Index: integer): INode2D;
    function GetActiveElementI2D(Index: integer): IElement2D;
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    function GetElementRanges: TRbwRangeTree;
    function GetNodeRanges: TRbwRangeTree;
    function GetElementCenters: TRbwQuadTree;
    function GetNodesLocations: TRbwQuadTree;
    property NodesI2D[Index: integer]: INode2D read GetActiveNodeI2D;
    property ElementsI2D[Index: integer]: IElement2D read GetActiveElementI2D;
    property ElementRanges: TRbwRangeTree read GetElementRanges;
    property NodeRanges: TRbwRangeTree read GetNodeRanges;
    property ElementCenters: TRbwQuadTree read GetElementCenters;
    property NodesLocations: TRbwQuadTree read GetNodesLocations;
    function MeshOutline: TPolygon2D;
    function GetElementOutline(Column: Integer): TElementOutline;
  end;

  TLimits = record
    UpperLimit: Double;
    LowerLimit: Double;
  end;
  TLimitsArray = array of array of TLimits;

  TCellElementPolygons2D = array of array of TPolygon2D;


  IMesh3D = interface(IMesh) ['{37375401-EE59-4E29-9706-0C6CC22A3CCF}']
    function Is3DMesh: Boolean;
    function MeshLimits(ViewDirection: TViewDirection; Angle: double): TGridLimit;

    function GetElementArrayMemberI(Layer, Col: Integer): IElement3D;
    function GetNodeArrayMemberI(Layer, Col: Integer): INode3D;
    function GetLayerCount: Integer;
    function GetMesh2DI: IMesh2D;
    function GetTopDataSet: TDataArray;
    function GetTopContourDataSet: TDataArray;
    function GetThreeDDataSet: TDataArray;
    function GetThreeDContourDataSet: TDataArray;
    function GetSelectedLayer: Integer;
    function GetElementCenters(Angle: Double): TRbwQuadTree;
    function GetIntervalTree(EvalAt: TEvaluatedAt;
      Angle: Double): TRbwIntervalTree;
    function GetRangeTree(EvalAt: TEvaluatedAt; Angle: Double): TRbwRangeTree;
    function GetNeedToRecalculateTopColors: Boolean;
    function GetNeedToRecalculateFrontColors: Boolean;

    procedure CheckUpdateElevations;

    procedure SetTopDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    procedure SetSelectedLayer(Value: Integer);
    procedure SetNeedToRecalculateTopColors(const Value: Boolean);
    procedure SetNeedToRecalculateFrontColors(const Value: Boolean);

    property ElementArrayI[Layer: Integer; Col: Integer]: IElement3D
      read GetElementArrayMemberI;
    property NodeArrayI[Layer: Integer; Col: Integer]: INode3D
      read GetNodeArrayMemberI;
    property LayerCount: Integer read GetLayerCount;
    property Mesh2DI: IMesh2D read GetMesh2DI;
    property SelectedLayer: Integer read GetSelectedLayer write SetSelectedLayer;
    property TopDataSet: TDataArray read GetTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read GetTopContourDataSet write SetTopContourDataSet;
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property ThreeDContourDataSet: TDataArray read GetThreeDContourDataSet write SetThreeDContourDataSet;
    function FrontPolygons(Angle: Double;
      EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray): TCellElementPolygons2D;
    property ElementCenters[Angle: Double]: TRbwQuadTree read GetElementCenters;
    property IntervalTree[EvalAt: TEvaluatedAt; Angle: Double]: TRbwIntervalTree
      read GetIntervalTree;
    property RangeTree[EvalAt: TEvaluatedAt; Angle: Double]: TRbwRangeTree
      read GetRangeTree;
    property NeedToRecalculateTopColors: Boolean
      read GetNeedToRecalculateTopColors write SetNeedToRecalculateTopColors;
    property NeedToRecalculateFrontColors: Boolean
      read GetNeedToRecalculateFrontColors write SetNeedToRecalculateFrontColors;
    function MeshBox(ViewDirection: TViewDirection; ObjectAngle: double): TPolygon2D;
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    procedure GetElementsIntfOnCrossSection(ElementList: TIElement2DList);
    procedure GetNodesIntfOnCrossSection(NodeList: TINode2DList);
    function GetElevationsNeedUpdating: boolean;
    property ElevationsNeedUpdating: Boolean read GetElevationsNeedUpdating;
    function GetActiveNodeCount: integer;
    property ActiveNodeCount: Integer read GetActiveNodeCount;
    property ActiveElementCount: Integer read GetActiveElementCount;
    function GetNodeI(Index: Integer): INode3D;
    property NodesI[Index: Integer]: INode3D read GetNodeI;
    function GetElementI(Index: Integer): IElement3D;
    property ElementsI[Index: Integer]: IElement3D read GetElementI;
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList);
    procedure ApplyLimitToMinMax(DataSet: TDataArray; var MinMax: TMinMax;
      Limits: TColoringLimits);
    end;


implementation

end.
