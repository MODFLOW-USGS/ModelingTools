unit SutraMeshUnit;

interface

uses
  Windows, FastGEO, Classes, GoPhastTypes, GR32, ZoomBox2, MeshRenumbering,
  AbstractGridUnit, Generics.Collections, gpc, Generics.Defaults, Types,
  DataSetUnit, Graphics, SubscriptionUnit, doublePolyhedronUnit, IntervalTree,
  QuadtreeClass, Forms,
  {$IF CompilerVersion >= 23.0}
  // Delphi XE2 and up
  UITypes,
  {$ELSE}
  // Delphi XE and earlier
  Controls,
  {$IFEND}
  OpenGL, MeshRenumberingTypes, GPC_Classes, SubPolygonUnit, QuadMeshGenerator,
  DrawMeshTypesUnit, LineStorage;

Type
  TMeshType = (mt2D, mtProfile, mt3D);
  TMeshGenerationMethod = (mgmFishnet, mgmIrregular, mgmGmsh, mgmGeompack, mgmUnknown);

  TCustomSutraItem = class(TInterfacedPhastCollectionItem)
  private
    FBypassUpdate: Boolean;
    function GetBypassUpdate: Boolean;
    procedure SetBypassUpdate(const Value: Boolean);
  protected
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    property BypassUpdate: Boolean read GetBypassUpdate write SetBypassUpdate;
  end;

  TCustomSutraNode = class(TCustomSutraItem)
  private
    FNumber: Integer;
    procedure SetNumber(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    // @name starts at zero.
    property Number: Integer read FNumber write SetNumber;
  end;

  TVertexList = TList<Tgpc_vertex>;
  TVertexArray = array of Tgpc_vertex;
  TSutraElement2D = class;
  TSutraElement2D_List = TList<TSutraElement2D>;
  TPoint2DComparer = TComparer<TPoint2D>;
  TSutraNode2D = class;
  TSutraNode2D_List = class(TList<TSutraNode2D>)
    procedure Pack;
  end;

  TSutraNode2D = class(TCustomSutraNode, INode, INode2D, ITriangulatable)
  private
    FLocation: TPoint2D;
    FNodeType: TNodeType;
    FElements: TSutraElement2D_List;
    FCellOutline: TVertexArray;
    FMinX: FastGeo.TFloat;
    FMaxX: FastGeo.TFloat;
    FMinY: FastGeo.TFloat;
    FMaxY: FastGeo.TFloat;
    FSelected: Boolean;
    FTriangNumber: Integer;
    // @Name is used in extracting the edge of the mesh in a particular layer.
    // It should not be relied on outside that procedure.
    FNeighborNodes: TSutraNode2D_List;
    procedure SetLocation(const Value: TPoint2D);
    procedure SetX(const Value: FastGeo.TFloat);
    procedure SetY(const Value: FastGEO.TFloat);
    procedure SetNodeType(const Value: TNodeType);
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
      DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
      MinMax: TMinMax{; ActiveDataArray: TDataArray});
    function GetElement(Index: Integer): TSutraElement2D;
    function GetElementCount: Integer;
    procedure InvalidateOutlines;
    function GetMaxX: Double;
    function GetMaxY: Double;
    function GetMinX: Double;
    function GetMinY: Double;
    procedure SetSelected(const Value: Boolean);
    function GetElementI(Index: integer): IElement2D;
  protected
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: Integer): IElement;
    function GetNodeNumber: Integer;
    procedure SetNodeNumber(Value: Integer);
    function GetLocation: TPoint2D;
    function GetNodeType: TNodeType;
    function GetCountTri: Integer;
    function GetItemTri(Index: Integer): ITriangulatable;
    function GetNumber: integer;
    function IsNeighbor(OtherItem: ITriangulatable): Boolean;
    function IndexOf(Item: ITriangulatable): Integer;
    property ElementsI[Index: Integer]: IElement2D read GetElementI;
    function GetTriangNumber: integer;
    procedure SetTriangNumber(const Value: Integer);
  public
    function DisplayNumber: integer;
    property Location: TPoint2D read GetLocation write SetLocation;
    procedure Assign(Source: TPersistent); override;
    procedure AssignINode(Source: INode);
    function IsInsideCell(APoint: TPoint2D): Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CellIntersection(const Input: TSegment2D;
      out IntersectingSegments: TSegment2DArray): boolean;
    function EdgeNode: boolean;
    // @name gets a polygon that defines the cell around the node.
    // The last point in the outline is not the same as the first point
    // so a connecting line is needed between the first and last points.
    procedure GetCellOutline(var CellOutline: TVertexArray);
    function CellArea: double;
    property ElementCount: Integer read GetElementCount;
    property Elements[Index: Integer]: TSutraElement2D read GetElement;
    function IsInside(APoint: TPoint2D): Boolean;
    procedure InvalidateCellOutline;
    property Selected: Boolean read FSelected write SetSelected;
    procedure RemoveElement(AnElement: TSutraElement2D);
    // @name is the sum of all the angles formed by this @classname in
    // all the @link(TSutraElement2D)s in which this @classname is a part.
    // For an interior angle, @name should be 360 degrees plus or minus rounding
    // error.
    function SumOfAngles: double;
    function IdealElementCount: integer;
    procedure ClearElements;
    procedure AddElement(AnElement: TSutraElement2D);
    function ReferenceLength: double;

  published
    property X: FastGEO.TFloat read FLocation.x write SetX;
    property Y: FastGEO.TFloat read FLocation.y write SetY;
    property NodeType: TNodeType read FNodeType write SetNodeType;
    property MaxX: Double read GetMaxX;
    property MinX: Double read GetMinX;
    property MaxY: Double read GetMaxY;
    property MinY: Double read GetMinY;
  end;

  TSutraNode2DArray = array of TSutraNode2D;

  TNode2DLeaf = class(TRangeTreeLeaf)
  private
    FNode: TSutraNode2D;
  public
    constructor Create(ANode: TSutraNode2D);
    // 0 = MinX
    // 1 = MaxX
    // 2 = MinY
    // 3 = MaxY
    function GetCoordinate(Depth: integer): double; override;
    property Node: TSutraNode2D read FNode;
  end;

  TNode2DLeafList = class(TRangeTreeLeafList)
  public
    function CoordinateCount: integer; override;
    Constructor Create;
  end;

  TSutraMesh3D = class;

  TCustomSutraCollection = class(TPhastCollection)
  private
    FMesh: TSutraMesh3D;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    constructor Create(ItemClass: TCollectionItemClass;
      InvalidateModelEvent: TNotifyEvent; Mesh: TSutraMesh3D);
  end;

  TSutraMesh2D = class;

  TSutraNode2D_Collection = class(TCustomSutraCollection)
  private
    FNodeIntervals: TRbwIntervalTree;
    FNodeRanges: TRbwRangeTree;
    FNodesLocations: TRbwQuadTree;
    function GetItem(Index: integer): TSutraNode2D;
    procedure NodeXLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure NodeYLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure InvalidateNodeIntervals;
    function GetNodeRanges: TRbwRangeTree;
    function GetNodesLocations: TRbwQuadTree;
    function GetMesh2D: TSutraMesh2D;
  public
    procedure SortByNodeNumber;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
    destructor Destroy; override;
    property Items[Index: integer]: TSutraNode2D read GetItem; default;
    function Add: TSutraNode2D;
    function TopContainingCell(APoint: TPoint2D): T2DTopCell;
    property Mesh2D: TSutraMesh2D read GetMesh2D;
    property NodeRanges: TRbwRangeTree read GetNodeRanges;
    property NodesLocations: TRbwQuadTree read GetNodesLocations;
  end;

  TCustomSutraNodeNumberItem = class(TCustomSutraItem)
  private
    function GetNodeNumber: integer;
    procedure SetNodeNumber(const Value: integer);
    procedure SetNode(const Value: TCustomSutraNode);
  protected
    FNode: TCustomSutraNode;
    FStoredNodeNumber: integer;
    // Set FNode to the node whose number is FStoredNodeNumber
    procedure UpdateNode; virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    property Node: TCustomSutraNode read FNode write SetNode;
  published
    Property NodeNumber: integer read GetNodeNumber write SetNodeNumber;
  end;

  TSutraNodeNumber2D_Item = class(TCustomSutraNodeNumberItem)
  private
    function GetNode: TSutraNode2D;
    procedure SetNode(const Value: TSutraNode2D);
  protected
    // Set FNode to the node whose number is FStoredNodeNumber
    procedure UpdateNode; override;
  public
    property Node: TSutraNode2D read GetNode write SetNode;
  end;

  TSutraNodeNumber2DList = TList<TSutraNodeNumber2D_Item>;

  TSutraNodeNumber2D_Collection = class(TCustomSutraCollection)
  private
    FElement: TSutraElement2D;
    function GetItem(Index: integer): TSutraNodeNumber2D_Item;
  public
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(InvalidateModelEvent: TNotifyEvent; Element: TSutraElement2D;
      Mesh3D: TSutraMesh3D);
    property Items[Index: integer]: TSutraNodeNumber2D_Item read GetItem; default;
    function Add: TSutraNodeNumber2D_Item;
    function IndexOfNode(Node: TSutraNode2D): Integer;
    // @name reverses the order of the instances of @link(TSutraElement2D)
    // in this @classname
    procedure Reverse;
  end;

  TCustomSutraElement = class(TCustomSutraItem)
  private
    FElementNumber: integer;
    procedure SetElementNumber(Value: integer);
    function GetElementNumber: integer;
    function GetDisplayNumber: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    property DisplayNumber: Integer read GetDisplayNumber;
  published
    property ElementNumber: integer read GetElementNumber
      write SetElementNumber;
  end;

  TSutraElement2D = class(TCustomSutraElement, IElement, IElement2D, ITriangulatable)
  private
    FNodes: TSutraNodeNumber2D_Collection;
    FSelected: Boolean;
    // @Name is used in extracting the edge of the mesh in a particular layer.
    // It should not be relied on outside that procedure.
    FOkElement: boolean;
    FTriangNumber: Integer;
    procedure SetNodes(const Value: TSutraNodeNumber2D_Collection);
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
      DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
      Mesh3D: TSutraMesh3D; MinMax: TMinMax);
    function GetMaxX: Double;
    function GetMaxY: Double;
    function GetMinX: Double;
    function GetMinY: Double;
    procedure SetSelected(const Value: Boolean);
    function GetNodeI(Index: integer): INode2D;
  protected
    function GetActiveNode(Index: Integer): INode;
    function GetActiveNodeCount: integer;
    function GetLocation: TPoint2D;
    function GetCountTri: Integer;
    function GetItemTri(Index: Integer): ITriangulatable;
    function GetNumber: integer;
    function IsNeighbor(OtherItem: ITriangulatable): Boolean;
    function IndexOf(Item: ITriangulatable): Integer;
    function IndexOfNode(Node: INode2D): Integer;
    function GetTriangNumber: integer;
    procedure SetTriangNumber(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignIElement(Source: IElement);
    function IsInside(APoint: TPoint2D): Boolean;
    function OnEdge(APoint: TPoint2D): Boolean;
    function Center: TPoint2D;
    function Intersection(const Input: TSegment2D;
      out IntersectingSegment: TSegment2D): boolean;
    function Edge(Index: integer): TSegment2D;
    function ElementArea: double;
    property MinX: Double read GetMinX;
    property MaxX: Double read GetMaxX;
    property MinY: Double read GetMinY;
    property MaxY: Double read GetMaxY;
    property Selected: Boolean read FSelected write SetSelected;
    function ElementOK: Boolean;
    // Angle in degrees of the corner of the element at ANode.
    function NodeAngle(ANode: TSutraNode2D): double;
    function HasSameNodes(Element: TSutraElement2D): Boolean;
    // @name makes sure that the @link(TSutraNode2D)s in this @classname
    // are oriented in a counterclockwise order.
    procedure SetCorrectOrienatation;
    function AspectRatio(VerticalExaggeration: Double = 1): Double;
    function ReferenceLength: Double;
    property NodeCount: integer read GetActiveNodeCount;
    property NodesI[Index: integer]: INode2D read GetNodeI;
    procedure GetNeighborElements(ElementList: TSutraElement2D_List);
    procedure GetNeighborsI(ElementList: TIElement2DList);
  published
    property Nodes: TSutraNodeNumber2D_Collection read FNodes write SetNodes;
  end;

  TElement2DLeaf = class(TRangeTreeLeaf)
  private
    FElement: TSutraElement2D;
    FMinX: Double;
    FMaxX: Double;
    FMinY: Double;
    FMaxY: Double;
  public
    constructor Create(AnElement: TSutraElement2D);
    // 0 = MinX
    // 1 = MaxX
    // 2 = MinY
    // 3 = MaxY
    function GetCoordinate(Depth: integer): double; override;
    property Element: TSutraElement2D read FElement;
  end;

  TElement2DLeafList = class(TRangeTreeLeafList)
  public
    function CoordinateCount: integer; override;
    Constructor Create;
  end;

  TSutraElement2D_Collection = class(TCustomSutraCollection)
  private
    FElementIntervals: TRbwIntervalTree;
    FElementRanges: TRbwRangeTree;
    FElementCenters: TRbwQuadTree;
    function GetItems(Index: integer): TSutraElement2D;
    procedure ElementXLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure ElementYLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    function GetElementRanges: TRbwRangeTree;
    function GetElementCenters: TRbwQuadTree;
    function GetMesh2D: TSutraMesh2D;
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent; ParentMesh: TSutraMesh3D);
    destructor Destroy; override;
    property Items[Index: integer]: TSutraElement2D read GetItems; default;
    function Add: TSutraElement2D;
    function TopContainingElement(APoint: TPoint2D): T2DTopCell;
    procedure InvalidateElementIntervals;
    property Mesh2D: TSutraMesh2D read GetMesh2D;
    property ElementRanges: TRbwRangeTree read GetElementRanges;
    property ElementCenters: TRbwQuadTree read GetElementCenters;
    procedure SortByElementNumber;
  end;

  ISutraNode2DComparer = IComparer<TSutraNode2D>;

  TSutraNode2DComparer = class(TComparer<TSutraNode2D>)
  private
    FStartPoint: TPoint2D;
    FAngle: Double;
  public
    function Compare(const Left, Right: TSutraNode2D): Integer; override;
    constructor Create(CrossSectionAngle: Double; StartPoint: TPoint2D);
  end;

  TSutraNode2DNumberComparer = class(TComparer<TSutraNode2D>)
  public
    function Compare(const Left, Right: TSutraNode2D): Integer; override;
  end;

  ISutraElement2DComparer = IComparer<TSutraElement2D>;

  TSutraElement2DComparer = class(TComparer<TSutraElement2D>)
  private
    FStartPoint: TPoint2D;
    FAngle: Double;
  public
    function Compare(const Left, Right: TSutraElement2D): Integer; override;
    constructor Create(CrossSectionAngle: Double; StartPoint: TPoint2D);
  end;

  TSutraElement2DNumberComparer = class(TComparer<TSutraElement2D>)
  public
    function Compare(const Left, Right: TSutraElement2D): Integer; override;
  end;


  TMeshGenerationControls = class(TGoPhastPersistent)
  private
    FConcave: TRealStorage;
    FSymmetry: TRealStorage;
    FNodePlacementError: TRealStorage;
    FSplittingAngle: TRealStorage;
    FStructure: TRealStorage;
    FLineLength: TRealStorage;
    FElementGrowthRate: TRealStorage;
    FMeshGenerationMethod: TMeshGenerationMethod;
    FRenumberingAlgorithm: TRenumberingAlgorithm;
    FGeomPackOptions: TGeompackOptions;
    procedure SetConcave(const Value: TRealStorage);
    procedure SetLineLength(const Value: TRealStorage);
    procedure SetNodePlacementError(const Value: TRealStorage);
    procedure SetSplittingAngle(const Value: TRealStorage);
    procedure SetStructure(const Value: TRealStorage);
    procedure SetSymmetry(const Value: TRealStorage);
    procedure ValueChanged(Sender: TObject);
    procedure SetElementGrowthRate(const Value: TRealStorage);
    procedure SetMeshGenerationMethod(const Value: TMeshGenerationMethod);
    procedure SetRenumberingAlgorithm(const Value: TRenumberingAlgorithm);
    procedure SetGeomPackOptions(const Value: TGeompackOptions);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetDefaults;
    procedure Apply;
  published
    property SplittingAngle: TRealStorage read FSplittingAngle
      write SetSplittingAngle;
    property Structure: TRealStorage read FStructure write SetStructure;
    property NodePlacementError: TRealStorage read FNodePlacementError
      write SetNodePlacementError;
    property LineLength: TRealStorage read FLineLength write SetLineLength;
    property Symmetry: TRealStorage read FSymmetry write SetSymmetry;
    property Concave: TRealStorage read FConcave write SetConcave;
    property ElementGrowthRate: TRealStorage read FElementGrowthRate
      write SetElementGrowthRate;
    property MeshGenerationMethod: TMeshGenerationMethod
      read FMeshGenerationMethod write SetMeshGenerationMethod stored True;
    property RenumberingAlgorithm: TRenumberingAlgorithm
      read FRenumberingAlgorithm write SetRenumberingAlgorithm stored True;
    property GeomPackOptions: TGeompackOptions read FGeomPackOptions write SetGeomPackOptions;
  end;

  TSutraMesh2D = class (TCustomMesh, IMesh, IMesh2D)
  private
    FElements: TSutraElement2D_Collection;
    FNodes: TSutraNode2D_Collection;
    FElementDrawingChoice: TDrawingChoice;
    FTopDataSet: TDataArray;
    FThreeDDataSet: TDataArray;
    FSelectedLayer: integer;
    FTopGridObserver: TObserver;
    FDrawNodeNumbers: boolean;
    FDrawElementNumbers: boolean;
    FNodeDrawingChoice: TDrawingChoice;
    FMesh3D: TSutraMesh3D;
    FElementFont: TFont;
    FNodeFont: TFont;
    FMeshGenControls: TMeshGenerationControls;
    FTopContourDataSet: TDataArray;
    FThreeDContourDataSet: TDataArray;
    FDrawElementCenters: boolean;
    procedure SetElements(const Value: TSutraElement2D_Collection);
    procedure SetNodes(const Value: TSutraNode2D_Collection);
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure SetElementDrawingChoice(const Value: TDrawingChoice);
    function MeshLimits: TGridLimit;
    function MeshBox: TPolygon2D;
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    procedure SetSelectedLayer(Value: integer);
    procedure GetNodesOnSegment(Segement: TSegment2D;
      NodesOnSegment: TSutraNode2D_List);
    procedure GetElementsOnSegment(Segment: TSegment2D;
      ElementsOnSegment: TSutraElement2D_List);
    procedure SetTopGridObserver(const Value: TObserver);
    procedure SetDrawNodeNumbers(const Value: boolean);
    procedure SetDrawElementNumbers(const Value: boolean);
    procedure SetNodeDrawingChoice(const Value: TDrawingChoice);
    procedure SetElementFont(const Value: TFont);
    procedure SetNodeFont(const Value: TFont);
    procedure DrawElements(StringValues: TStringList;
      ColorDataArray: TDataArray; const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    procedure DrawNodes(ColorDataArray: TDataArray; const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; StringValues: TStringList);
    function GetSelectedLayer: integer;
    procedure SetMeshGenControls(const Value: TMeshGenerationControls);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure DrawTopContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    function Bandwidth: Integer;
    procedure SetDrawElementCenters(const Value: boolean);
    procedure GetCounts(DataSet: TDataArray; var LayerCount, RowCount,
      ColCount: integer);
    function GetElementRanges: TRbwRangeTree;
    function GetNodeRanges: TRbwRangeTree;
    function GetElementCenters: TRbwQuadTree;
    function GetNodesLocations: TRbwQuadTree;
  protected
    function GetActiveNode(Index: Integer): INode;
    function GetActiveNodeCount: integer;
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: integer): IElement;
    function GetActiveNodeI2D(Index: integer): INode2D;
    function GetActiveElementI2D(Index: integer): IElement2D;
    function GetItemTopLocation(const EvalAt: TEvaluatedAt; const Column,
      Row: integer): TPoint2D; override;
    function GetShortestHorizontalBlockEdge(Layer, Row, Column: Integer): double; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList; out MinMaxInitialized: Boolean); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
    destructor Destroy; override;
    procedure Draw(const BitMap: TPersistent);
    procedure Clear;
    property ElementDrawingChoice: TDrawingChoice read FElementDrawingChoice
      write SetElementDrawingChoice;
    property NodeDrawingChoice: TDrawingChoice read FNodeDrawingChoice
      write SetNodeDrawingChoice;
    property DrawNodeNumbers: boolean read FDrawNodeNumbers
      write SetDrawNodeNumbers;
    property DrawElementNumbers: boolean read FDrawElementNumbers
      write SetDrawElementNumbers;
    property NodeFont: TFont read FNodeFont write SetNodeFont;
    property ElementFont: TFont read FElementFont write SetElementFont;
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    property ThreeDDataSet: TDataArray read FThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read FTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read FTopContourDataSet
      write SetTopContourDataSet;
    property ThreeDContourDataSet: TDataArray read FThreeDContourDataSet
      write SetThreeDContourDataSet;
    property SelectedLayer: integer read GetSelectedLayer write SetSelectedLayer;
    property TopGridObserver: TObserver read FTopGridObserver
      write SetTopGridObserver;
    procedure EndUpdate; override;
    property Mesh3D: TSutraMesh3D read FMesh3D;
    function MeshOutline: TPolygon2D;
    procedure Renumber;
    property DrawElementCenters: boolean read FDrawElementCenters write SetDrawElementCenters;
    procedure DeleteUnconnectedNodes;
    procedure SetCorrectElementOrientation;
    function Is3DMesh: Boolean;
    property ElementRanges: TRbwRangeTree read GetElementRanges;
    property NodeRanges: TRbwRangeTree read GetNodeRanges;
    property ElementCenters: TRbwQuadTree read GetElementCenters;
    property NodesLocations: TRbwQuadTree read GetNodesLocations;
    function GetElementOutline(Column: Integer): TElementOutline;
  published
    property Nodes: TSutraNode2D_Collection read FNodes write SetNodes;
    property Elements: TSutraElement2D_Collection read FElements
      write SetElements;
    property MeshGenControls: TMeshGenerationControls read FMeshGenControls
      write SetMeshGenControls;
  end;

  TSutraElement3D = class;
  TSutraElement3DList = class;

  TSutraNode3D = class(TCustomSutraNode, INode, INode3D)
  private
    FNode2D: TSutraNode2D;
    FZ: FastGEO.TFloat;
    FNode2D_Number: Integer;
    FElements: TSutraElement3DList;
    FActiveElements: TSutraElement3DList;
    FActive: Boolean;
    FVolume: Extended;
    FBottom: Double;
    FTop: Double;
    function GetNode2D_Number: integer;
    procedure SetNode2D_Number(const Value: integer);
    procedure SetZ(const Value: FastGEO.TFloat);
    function GetNode2D: TSutraNode2D;
    procedure UpdateNode2D;
    function GetX: FastGeo.TFloat;
    function GetY: FastGeo.TFloat;
    procedure UpdateActiveElementList;
    function CreatePolyhedron: TPolyhedron;
    function GetVolume: Extended;
    procedure Draw3D;
    function GetActive: Boolean;
    function GetZ: FastGEO.TFloat;
    function GetBottom: Double;
    function GetTop: Double;
    function GetBoundaryNode: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: Integer): IElement;
    function GetNodeNumber: Integer;
    procedure SetNodeNumber(Value: Integer);
    function GetLocation: TPoint2D;
    procedure SetLocation(const Value: TPoint2D);
    function GetNodeType: TNodeType;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignINode(Source: INode);
    property Node2D: TSutraNode2D read GetNode2D;
    property X: FastGeo.TFloat read GetX;
    property Y: FastGeo.TFloat read GetY;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function NodeLocation: TPoint3D;
    property Volume: Extended read GetVolume;
    property Top: Double read GetTop;
    property Bottom: Double read GetBottom;
    function DisplayNumber: integer;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    function Model: TBaseModel;
    function ReferenceLength: double;
    function NodeLocation2D: TPoint2D;
    function NodeNumber2D: Integer;
    property BoundaryNode: Boolean read GetBoundaryNode;
  published
    property Z: FastGEO.TFloat read GetZ write SetZ;
    property Node2D_Number: integer read GetNode2D_Number write SetNode2D_Number;
    property Active: Boolean read GetActive write SetActive;
  end;

  TSutraNode3D_List = TList<TSutraNode3D>;

  TStoredNodeOrElement = class(TObject)
  private
    FX: double;
    FY: double;
    FZ: double;
    FCol: integer;
    FLayer: integer;
    FNodeOrElement: Pointer;
  public
    constructor Create(CenterPoint: TPoint3D; Angle: double;
      ACol, ALayer: Integer; AnObject: TObject);
    property X: double read FX;
    property Y: double read FY;
    property Z: double read FZ;
    property Col: integer read FCol;
    property Layer: integer read FLayer;
    property NodeOrElement: Pointer read FNodeOrElement;
  end;

  TStoredLocations = TObjectList<TStoredNodeOrElement>;

  TSutraNode3D_Collection = class(TCustomSutraCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FRotatedNodeCenters: TRbwQuadTree;
    FAngle: double;
    FStoredRotatedLocations: TStoredLocations;
    function GetItem(Index: integer): TSutraNode3D;
    function GetNodeLocations(Angle: Double): TRbwQuadTree;
  public
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
    destructor Destroy; override;
    property Items[Index: integer]: TSutraNode3D read GetItem; default;
    function Add: TSutraNode3D;
    property NodeLocations[Angle: Double]: TRbwQuadTree read GetNodeLocations;
    procedure InvalidateStoredLocations;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TSutraNodeNumber3D_Item = class(TCustomSutraNodeNumberItem)
  private
    function GetNode: TSutraNode3D;
    procedure SetNode(const Value: TSutraNode3D);
  protected
    // Set FNode to the node whose number is FStoredNodeNumber
    procedure UpdateNode; override;
  public
    property Node: TSutraNode3D read GetNode write SetNode;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    function Model: TBaseModel;
  end;

  TSutraNodeNumber3D_Collection = class(TCustomSutraCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    function GetItem(Index: integer): TSutraNodeNumber3D_Item;
  public
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(Model: TBaseModel; Mesh3D: TSutraMesh3D);
    property Items[Index: integer]: TSutraNodeNumber3D_Item read GetItem; default;
    function Add: TSutraNodeNumber3D_Item;
    function IndexOfNode(Node: TSutraNode3D): Integer;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TQuadPair2D = array[0..1] of TQuadix2D;
  TQuadPair3D = array[0..1] of TQuadix3D;
  TQuadPair3DList = TList<TQuadPair3D>;

  TSutraElement3D = class(TCustomSutraElement, IElement, IElement3D)
  private
    FNodes: TSutraNodeNumber3D_Collection;
    FActiveNodes: TSutraNode3D_List;
    FActive: Boolean;
    FVolume: Extended;
    FElement2D: TSutraElement2D;
    FLayer: integer;
    procedure SetNodes(const Value: TSutraNodeNumber3D_Collection);
    procedure UpdateNodes;
    function GetCenterLocation: TPoint3d;
    procedure UpdateActiveNodeList;
    function GetVolume: extended;
    function CreatePolyhedron: TPolyhedron;
    procedure AddFacesToNodePolyhedron(CornerNode : TSutraNode3D;
      APolyHedron : TPolyhedron; var LastVertex, LastFace : integer);
    procedure Draw3D;
    function GetActive: Boolean;
    function GetLayer: Integer;
    function GetNodeI(Index: integer): INode3D;
    function GetNodeLocation(Index: integer): TPoint3D;
    procedure SetActive(const Value: Boolean);
  protected
    function GetActiveNode(Index: Integer): INode;
    function GetActiveNodeCount: integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignIElement(Source: IElement);
    function UpperElevation: double;
    function LowerElevation: double;
    function CenterElevation: double;
    function CellSection(Node: TSutraNode3D): TQuadPair3D;
    function ElementShape: TQuadPair3D;
    property CenterLocation: TPoint3d read GetCenterLocation;
    property Volume: Extended read GetVolume;
    property Element2D: TSutraElement2D read FElement2D write FElement2D;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    function Model: TBaseModel;
    function ReferenceLength: double;
    function ElementNumber2D: Integer;
    property Layer: Integer read GetLayer;
    property NodesI[Index: integer]: INode3D read GetNodeI;
    property NodeLocation[Index: integer]: TPoint3D read GetNodeLocation;
  published
    property Nodes: TSutraNodeNumber3D_Collection read FNodes write SetNodes;
    property Active: Boolean read GetActive write SetActive;
  end;

  TSutraElement3DList = class(TList<TSutraElement3D>);

  TSutraElement3D_Collection = class(TCustomSutraCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FRotatedElementCenters: TRbwQuadTree;
    FAngle: double;
    FStoredRotatedLocations: TStoredLocations;
//    FMesh: TSutraMesh3D;
    function GetItems(Index: integer): TSutraElement3D;
    function GetElementCenters(Angle: Double): TRbwQuadTree;
  public
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
    destructor Destroy; override;
    property Items[Index: integer]: TSutraElement3D read GetItems; default;
    function Add: TSutraElement3D;
    property ElementCenters[Angle: Double]: TRbwQuadTree read GetElementCenters;
    procedure InvalidateStoredLocations;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TFrontPolygon = class(TRangeTreeLeaf)
  private
    FLayer: Integer;
    FPolygon: TPolygon2D;
    FCol: Integer;
    FMaxX: double;
    FMaxY: double;
    FMinX: double;
    FMinY: double;
  public
    constructor Create(ACol, ALayer: Integer; APolygon: TPolygon2D);
    property Col: Integer read FCol;
    property Layer: Integer read FLayer;
    property Polygon: TPolygon2D read FPolygon;
    property MinX: double read FMinX;
    property MaxX: double read FMaxX;
    property MinY: double read FMinY;
    property MaxY: double read FMaxY;
    function GetCoordinate(Depth: integer): double; override;
  end;

  TFrontPolygonList = class(TRangeTreeLeafList)
  public
    function CoordinateCount: integer; override;
    Constructor Create;
  end;

  TMeshOutline = class(TOutline)
  private
    procedure GetTopPolygons(Mesh: TSutraMesh3D; Layer: Integer;
      var UnionPolygon: TGpcPolygonClass);
    procedure GetFrontPolygons(Mesh: TSutraMesh3D;
      var UnionPolygon: TGpcPolygonClass);
  public
    constructor Create(Mesh: TSutraMesh3D; ViewDirection: TViewDirection;
      Layer: Integer);
  end;

  TSutraMesh3D = class (TCustomMesh, IMesh, IMesh3D, IDrawMesh)
  private
    FColorDataSetObserver: TObserver;
    FElements: TSutraElement3D_Collection;
    FNodes: TSutraNode3D_Collection;
    FMesh2D: TSutraMesh2D;
    FElementDrawingChoice: TDrawingChoice;
    FMeshType: TMeshType;
    FNodeArray: array of array of TSutraNode3D;
    FElementArray: array of array of TSutraElement3D;
    FMeshUpdate: integer;
    FCanDraw: boolean;
    FCrossSection: TMeshCrossSectionLine;
    FThreeDGridObserver: TObserver;
    FElevationsNeedUpdating: boolean;
    FUpdatingElevations: boolean;
    FNodeDrawingChoice: TDrawingChoice;
    FOnSelectedLayerChange: TNotifyEvent;
    FActiveNodes: TSutraNode3D_List;
    FActiveElements: TSutraElement3DList;
    FNeedToRecalculateTopColors: Boolean;
    FNeedToRecalculateFrontColors: Boolean;
    FStoredBlockAngle: double;
    FStoredBlockPolygons: TCellElementPolygons2D;
    FStoredBlockLimits: TLimitsArray;
    FStoredNodeAngle: double;
    FStoredNodePolygons: TCellElementPolygons2D;
    FStoredNodeLimits: TLimitsArray;
    FMaxDist: double;
    FMinDist: double;
    FLoading: Boolean;
    FNodeIntervalTree: TRbwIntervalTree;
    FElementIntervalTree: TRbwIntervalTree;
    FNodeRangeTree: TRbwRangeTree;
    FElementRangeTree: TRbwRangeTree;
    FCanDraw3D: boolean;
    FNeedToRedraw3d: Boolean;
    FListsCreated: Boolean;
    FCrossSectionGLIndex: GLuint;
    FLayerGLIndex: GLuint;
    FNeedToRecordLayer: Boolean;
    FNeedToRecordCrossSection: Boolean;
    FNeedToRecordColoredCells: Boolean;
    FElementNumbers: TIntegerCollection;
    FNodeNumbers: TIntegerCollection;
    FColoredCellsOrElementGLIndex: GLuint;
    FOnMeshTypeChanged: TNotifyEvent;
    FTopMeshOutline: TMeshOutline;
    FFrontMeshOutline: TMeshOutline;
    FSettingSelectedLayer: Boolean;
//    FDrawElementCenters: boolean;
    procedure SetElements(const Value: TSutraElement3D_Collection);
    procedure SetNodes(const Value: TSutraNode3D_Collection);
    procedure SetMesh2D(const Value: TSutraMesh2D);
    procedure SetElementDrawingChoice(const Value: TDrawingChoice);
    procedure SetMeshType(const Value: TMeshType);
    function GetNodeArrayMember(Layer, Col: Integer): TSutraNode3D;
    function GetNodeArrayMemberI(Layer, Col: Integer): INode3D;
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    function GetCanDraw: boolean;
    procedure SetCanDraw(const Value: boolean);
    function GetThreeDDataSet: TDataArray;
    function GetTopDataSet: TDataArray;
    procedure SetSelectedLayer(Value: integer);
    function GetLayerCount: Integer;
    function GetSelectedLayer: integer;
    function GetElementArrayMember(Layer, Col: Integer): TSutraElement3D;
    function GetElementArrayMemberI(Layer, Col: Integer): IElement3D;
    procedure SetCrossSection(const Value: TMeshCrossSectionLine);
    procedure DrawFront(const BitMap: TPersistent);
    procedure DrawPointsOnCrossSection(BitMap: TPersistent);
    function GetTopGridObserver: TObserver;
    procedure SetThreeDGridObserver(const Value: TObserver);
    procedure SetTopGridObserver(const Value: TObserver);
    procedure UpdateElevations;
    procedure SetNodeDrawingChoice(const Value: TDrawingChoice);
    function GetElementFont: TFont;
    function GetNodeFont: TFont;
    procedure SetElementFont(const Value: TFont);
    procedure SetNodeFont(const Value: TFont);
    function GetDrawNodeNumbers: Boolean;
    procedure SetDrawNodeNumbers(const Value: Boolean);
    function GetDrawElementNumbers: Boolean;
    procedure SetDrawElementNumbers(const Value: Boolean);
    procedure SetNeedToRecalculateFrontColors(const Value: Boolean);
    procedure SetNeedToRecalculateTopColors(const Value: Boolean);
    function GetIntervalTree(EvalAt: TEvaluatedAt;
      Angle: Double): TRbwIntervalTree;
    function GetRangeTree(EvalAt: TEvaluatedAt; Angle: Double): TRbwRangeTree;
    procedure GetXIntervalLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure GetYIntervalLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    function GetThreeDContourDataSet: TDataArray;
    function GetTopContourDataSet: TDataArray;
    procedure DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    function GetCanDraw3D: Boolean;
    procedure RecordLayer;
    procedure RecordCrossSection;
    procedure CrossSectionMoved(Sender: TObject);
    procedure SetElementNumbers(const Value: TIntegerCollection);
    procedure SetNodeNumbers(const Value: TIntegerCollection);
    procedure SetElevationsNeedUpdating(const Value: boolean);
    procedure SetDrawElementCenters(const Value: boolean);
    function GetDrawElementCenters: boolean;
    function GetElevationsNeedUpdating: boolean;
    procedure RecordColoredCellsOrElements;
    function FrontMeshBox(ObjectAngle: double): TPolygon2D;
    function GetMesh2D: TSutraMesh2D;
    function GetMesh2DI: IMesh2D;
    function GetElementCenters(Angle: Double): TRbwQuadTree;
    function GetNeedToRecalculateTopColors: Boolean;
    function GetNeedToRecalculateFrontColors: Boolean;
    function GetCrossSection: TMeshCrossSectionLine;
    function GetNodeI(Index: Integer): INode3D;
    function GetElementI(Index: Integer): IElement3D;
  protected
    function GetActiveNodeCount: integer;
    function GetActiveNode(Index: Integer): INode;
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: Integer): IElement;
    procedure SetDefaultCrossSectionLocation;
    function IsFishnetMesh: Boolean;
    function TopOutline(Layer: integer): TOutline;
    function FrontOutline: TOutline;
    function GetItemTopLocation(const EvalAt: TEvaluatedAt; const Column,
      Row: integer): TPoint2D; override;
    function GetShortestHorizontalBlockEdge(Layer, Row, Column: Integer): double; override;
  public
    procedure UpdateNodeArray;
    procedure AssignNodeElevations;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure UpdateNodeNumbers;
    procedure UpdateElementNumbers;
    procedure RestoreNodeNumbers;
    procedure RestoreElementNumbers;
    procedure Assign(Source: TPersistent); override;
    procedure SimpleRenumber;
    function Bandwidth: Integer;
    function OkLocation(const DataSet: TDataArray;
      const Layer, Row, Col: integer): boolean; override;
    procedure ColorDataSetChange(Sender: TObject);
    function TopMeshOutline(Layer: integer): TMeshOutline;
    function FrontMeshOutline: TMeshOutline;
    property ActiveNodeCount: Integer read GetActiveNodeCount;
    property ActiveElementCount: Integer read GetActiveElementCount;
    property ElevationsNeedUpdating: boolean read GetElevationsNeedUpdating
      write SetElevationsNeedUpdating;
    procedure Clear;
    procedure Draw(const BitMap: TPersistent;
      const ViewDirection: TViewDirection);
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    function MeshLimits(ViewDirection: TViewDirection; Angle: double): TGridLimit;
    function MeshBox(ViewDirection: TViewDirection; ObjectAngle: double): TPolygon2D;
    property NodeArray[Layer: Integer; Col: Integer]: TSutraNode3D
      read GetNodeArrayMember;
    property NodeArrayI[Layer: Integer; Col: Integer]: INode3D
      read GetNodeArrayMemberI;
    property ElementArray[Layer: Integer; Col: Integer]: TSutraElement3D
      read GetElementArrayMember;
    property ElementArrayI[Layer: Integer; Col: Integer]: IElement3D
      read GetElementArrayMemberI;
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read GetTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read GetTopContourDataSet write SetTopContourDataSet;
    property ThreeDContourDataSet: TDataArray read GetThreeDContourDataSet write SetThreeDContourDataSet;
    procedure MeshChanged;
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    // @name is the number of layers of elements.
    property LayerCount: Integer read GetLayerCount;
    procedure UpdateElementsInNodes;
    function FrontPolygons(Angle: Double;
      EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray)
      : TCellElementPolygons2D;
    property TopGridObserver: TObserver read GetTopGridObserver
      write SetTopGridObserver;
    property ThreeDGridObserver: TObserver read FThreeDGridObserver
      write SetThreeDGridObserver;
    procedure EndUpdate; override;
    property OnSelectedLayerChange: TNotifyEvent read FOnSelectedLayerChange
      write FOnSelectedLayerChange;
    property NodeFont: TFont read GetNodeFont write SetNodeFont;
    property ElementFont: TFont read GetElementFont write SetElementFont;
    property DrawNodeNumbers: Boolean read GetDrawNodeNumbers
      write SetDrawNodeNumbers;
    property DrawElementNumbers: Boolean read GetDrawElementNumbers
      write SetDrawElementNumbers;
    procedure GetNodesOnCrossSection(NodeList: TSutraNode2D_List);
    function GetCrossSectionStart: TPoint2D;
    procedure GetElementsOnCrossSection(ElementList: TSutraElement2D_List);
    procedure GetElementsIntfOnCrossSection(ElementList: TIElement2DList);
    procedure GetNodesIntfOnCrossSection(NodeList: TINode2DList);
    property NeedToRecalculateTopColors: Boolean
      read GetNeedToRecalculateTopColors write SetNeedToRecalculateTopColors;
    property NeedToRecalculateFrontColors: Boolean
      read GetNeedToRecalculateFrontColors write SetNeedToRecalculateFrontColors;
    // @name is the relative position of the node on the cross section that is
    // furthest from the cross section in the positive direction.
    property MaxDist: Double read FMaxDist;
    // @name is the relative position of the node on the cross section that is
    // furthest from the cross section in the negative direction.
    property MinDist: Double read FMinDist;
    function RotateFromRealWorldCoordinatesToMeshCoordinates
      (const APoint: TPoint2D): TPoint2D;
    function RotateFromMeshCoordinatesToRealWorldCoordinates
      (const APoint: TPoint2D): TPoint2D;  overload;
    procedure CheckUpdateElevations;
    procedure InvalidatePolygons;
    property Loading: Boolean read FLoading write FLoading;
    property IntervalTree[EvalAt: TEvaluatedAt; Angle: Double]: TRbwIntervalTree
      read GetIntervalTree;
    property RangeTree[EvalAt: TEvaluatedAt; Angle: Double]: TRbwRangeTree
      read GetRangeTree;
    property CanDraw3D: Boolean read GetCanDraw3D write FCanDraw3D;
    procedure Draw3D;
    property DrawElementCenters: boolean read GetDrawElementCenters write SetDrawElementCenters;
    function DefaultCrossSectionLocation: TSegment2D;
    property OnMeshTypeChanged: TNotifyEvent read FOnMeshTypeChanged
      write FOnMeshTypeChanged;
    procedure DeleteUnconnectedNodes;
    procedure SetCorrectElementOrientation;
    function Is3DMesh: Boolean;
    property Mesh2DI: IMesh2D read GetMesh2DI;
    property ElementCenters[Angle: Double]: TRbwQuadTree read GetElementCenters;
    property NodesI[Index: Integer]: INode3D read GetNodeI;
    property ElementsI[Index: Integer]: IElement3D read GetElementI;
    property UpdatingElevations: Boolean read FUpdatingElevations;
  published
    property Nodes: TSutraNode3D_Collection read FNodes write SetNodes stored False;
    property Elements: TSutraElement3D_Collection read FElements
      write SetElements stored False;
    property Mesh2D: TSutraMesh2D read GetMesh2D write SetMesh2D;
    property ElementDrawingChoice: TDrawingChoice read FElementDrawingChoice
      write SetElementDrawingChoice Stored True;
    property NodeDrawingChoice: TDrawingChoice read FNodeDrawingChoice
      write SetNodeDrawingChoice Stored True;
    property MeshType: TMeshType read FMeshType write SetMeshType stored True;
    property SelectedLayer: integer read GetSelectedLayer
      write SetSelectedLayer stored True;
    property CrossSection: TMeshCrossSectionLine read GetCrossSection
      write SetCrossSection;
    property NodeNumbers: TIntegerCollection read FNodeNumbers
      write SetNodeNumbers;
    property ElementNumbers: TIntegerCollection read FElementNumbers
      write SetElementNumbers;
  end;

Function ComparePointRightUp(const P1, P2: TPoint2D): integer;
Function ComparePointRightDown(const P1, P2: TPoint2D): integer;
Function ComparePointLeftUp(const P1, P2: TPoint2D): integer;
Function ComparePointLeftDown(const P1, P2: TPoint2D): integer;

procedure UpdateSegmentOrientation(const Input: TSegment2D;
   var IntersectingSegment: TSegment2D);

procedure GetDataSetMeshValue(DataArray: TDataArray;
  Layer, Column: integer; StringValues : TStringList;
  var ShowColor: Boolean; var Fraction: double; MinMax: TMinMax
  {; ActiveDataArray: TDataArray});
function QuadPairsToPolygon(QP: TQuadPair3DList; Angle: Double;
  out PerpendicularLimit: TLimits): TPolygon2D;

implementation

uses
  frmGoPhastUnit, BigCanvasMethods, PhastModelUnit, SysUtils, Math,
  Dialogs, LayerStructureUnit, RbwParser, GR32_Polygons, SolidGeom,
  OctTreeClass, Contnrs,
  ContourUnit, GR32_Backends, ConvexHullUnit, ModelMuseUtilities,
  CuthillMcKeeRenumbering, DataSetNamesUnit;

resourcestring
  StrErrorGeneratingCel = 'Error generating cell for Node %d.';

const
  ThinLine = 0.1;


procedure GetDataSetMeshValue(DataArray: TDataArray;
  Layer, Column: integer; StringValues : TStringList;
  var ShowColor: Boolean; var Fraction: double; MinMax: TMinMax{;
  ActiveDataArray: TDataArray});
var
  AValue: Double;
  AValueInt: integer;
  ValueStr: string;
  SIndex: Integer;
begin

  ShowColor := DataArray.IsValue[Layer, 0, Column]
    and DataArray.ColorGridValueOK(Layer, 0, Column);
  if ShowColor then
  begin
    {if (ActiveDataArray <> nil) and DataArray.Limits.ActiveOnly
      and (DataArray.EvaluatedAt = eaBlocks) then
    begin
      ShowColor := ActiveDataArray.BooleanData[Layer, 0, Column];
      if not ShowColor then
      begin
        Exit;
      end;
    end; }
    case DataArray.DataType of
      rdtDouble:
        begin
          AValue := DataArray.RealData[Layer, 0, Column];
          ShowColor := (AValue >= MinMax.RMin) and (AValue <= MinMax.RMax);
          if ShowColor then
          begin
            if DataArray.Limits.LogTransform  then
            begin
              if AValue > 0 then
              begin
                if MinMax.RMax <> MinMax.RMinPositive then
                begin
                  if AValue = MinMax.RMax then
                  begin
                    Fraction := 0;
                  end
                  else if AValue = MinMax.RMinPositive then
                  begin
                    Fraction := 1;
                  end
                  else
                  begin
                    Fraction := (Log10(MinMax.RMax)-Log10(AValue))
                      /(Log10(MinMax.RMax) - Log10(MinMax.RMinPositive));
                  end;
                end
                else
                begin
                  Fraction := 0.5;
                end;
              end
              else
              begin
                ShowColor := False;
              end;
            end
            else
            begin
              if MinMax.RMax <> MinMax.RMin then
              begin
                Fraction := (MinMax.RMax-AValue)/
                  (MinMax.RMax - MinMax.RMin);
              end
              else
              begin
                Fraction := 0.5;
              end;
            end;
          end
          else
          begin
            Fraction := 0;
          end;
        end;
      rdtInteger:
        begin
          if DataArray.MaxInteger <> DataArray.MinInteger then
          begin
            AValueInt := DataArray.IntegerData[Layer, 0, Column];
            ShowColor := (AValueInt >= MinMax.IMin) and (AValueInt <= MinMax.IMax);
            Fraction := (MinMax.IMax-AValueInt)
              /(MinMax.IMax - MinMax.IMin);
          end
          else
          begin
            Fraction := 0.5;
          end;
        end;
      rdtBoolean:
        begin
          if DataArray.MaxBoolean <> DataArray.MinBoolean then
          begin
            if DataArray.BooleanData[Layer, 0, Column] then
            begin
              Fraction := 1;
            end
            else
            begin
              Fraction := 0;
            end;
          end
          else
          begin
            Fraction := 0.5;
          end;
        end;
      rdtString:
        begin
          if StringValues.Count = 0 then
          begin
            ShowColor := False
          end
          else if StringValues.Count = 1 then
          begin
            ValueStr := DataArray.StringData[
              Layer, 0, Column];
            if StringValues.IndexOf(ValueStr) >= 0 then
            begin
              Fraction := 0.5;
            end
            else
            begin
              ShowColor := False;
            end;
          end
          else
          begin
            ValueStr := DataArray.StringData[Layer, 0, Column];
            SIndex := StringValues.IndexOf(ValueStr);
            if SIndex >= 0 then
            begin
              Fraction := (1-(SIndex / (StringValues.Count - 1)));
            end
            else
            begin
              ShowColor := False;
            end;
          end;
        end;
      else Assert(False);
    end;
  end;

end;

procedure UpdateSegmentOrientation(const Input: TSegment2D;
   var IntersectingSegment: TSegment2D);
begin
  if Input[1].x > Input[2].x then
  begin
    if IntersectingSegment[1].x <= IntersectingSegment[2].x then
    begin
      IntersectingSegment := EquateSegment(IntersectingSegment[2],
        IntersectingSegment[1]);
    end;
  end
  else if Input[1].x < Input[2].x then
  begin
    if IntersectingSegment[1].x >= IntersectingSegment[2].x then
    begin
      IntersectingSegment := EquateSegment(IntersectingSegment[2],
        IntersectingSegment[1]);
    end;
  end
  else
  begin
    if Input[1].y > Input[2].y then
    begin
      if IntersectingSegment[1].y <= IntersectingSegment[2].y then
      begin
        IntersectingSegment := EquateSegment(IntersectingSegment[2],
          IntersectingSegment[1]);
      end;
    end
    else if Input[1].y < Input[2].y then
    begin
      if IntersectingSegment[1].y >= IntersectingSegment[2].y then
      begin
        IntersectingSegment := EquateSegment(IntersectingSegment[2],
          IntersectingSegment[1]);
      end;
    end;
  end;
end;


{ TSutraNode2D }

function TSutraNode2D.CellArea: double;
var
  Outline: TVertexArray;
  OutlinePolygon: TPolygon2D;
  PolyIndex: Integer;
begin
  GetCellOutline(Outline);
  SetLength(OutlinePolygon, Length(Outline));
  for PolyIndex := 0 to Length(Outline) - 1 do
  begin
    OutlinePolygon[PolyIndex] := Outline[PolyIndex];
  end;
  result := Abs(Area(OutlinePolygon));
end;

procedure TSutraNode2D.AddElement(AnElement: TSutraElement2D);
begin
  FElements.Add(AnElement);
end;

procedure TSutraNode2D.Assign(Source: TPersistent);
var
  SourceNode: TSutraNode2D;
begin
  if Source is TSutraNode2D then
  begin
    SourceNode := TSutraNode2D(Source);
    Location := SourceNode.Location;
    NodeType := SourceNode.NodeType;
    Selected := SourceNode.Selected;
  end;
  inherited;
end;

procedure TSutraNode2D.AssignINode(Source: INode);
begin
  Location := Source.Location;
  Number := Source.NodeNumber;
  NodeType := Source.NodeType;
end;

constructor TSutraNode2D.Create(Collection: TCollection);
begin
  inherited;
  FElements := TSutraElement2D_List.Create;
  FNeighborNodes := TSutraNode2D_List.Create;
end;

destructor TSutraNode2D.Destroy;
begin
  FNeighborNodes.Free;
  FElements.Free;
  inherited;
end;

function TSutraNode2D.DisplayNumber: integer;
var
  Mesh: TSutraMesh3D;
  Node3D: TSutraNode3D;
begin
  Mesh := (Collection as  TSutraNode2D_Collection).Mesh2D.Mesh3D;
  if (Mesh.MeshType = mt3D) and (Mesh.Elements.Count > 0) then
  begin
    Node3D := Mesh.NodeArray[Mesh.SelectedLayer, Number];
    result := Node3D.Number + 1;
//    result := ThreeDNodeNumber + 1;
  end
  else
  begin
    result := Number + 1;
  end;
end;

procedure TSutraNode2D.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
  DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
  MinMax: TMinMax{; ActiveDataArray: TDataArray});
const
  NodeRadius = 3;
var
  Outline: TVertexArray;
  NodeIndex: Integer;
  Points: GoPhastTypes.TPointArray;
  Fraction: double;
  AColor: TColor;
  Dummy: TPolygon32;
  ShowColor: Boolean;
  APoint: TPoint;
  ACanvas: TCanvas;
//  APoint: TPoint;
//  NumString: string;
begin
  Dummy := nil;
  GetCellOutline(Outline);
  if (DataArray <> nil) or (DrawingChoice = dcAll) then
  begin
    SetLength(Points, Length(Outline)+1);
    if Length(Outline) = 0 then
    begin
      Points[0] := ConvertTop2D_Point(ZoomBox, Location);
    end
    else
    begin
      for NodeIndex := 0 to Length(Outline) - 1 do
      begin
        Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, Outline[NodeIndex]);
      end;
      Points[Length(Points)-1] := Points[0];
    end;
//    Column := Self.Number;
    if DataArray <> nil then
    begin
      GetDataSetMeshValue(DataArray, SelectedLayer, Self.Number, StringValues,
        ShowColor, Fraction, MinMax);
      if ShowColor then
      begin
        AColor := frmGoPhast.PhastModel.GridColorParameters.FracToColor(Fraction);
        DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
          0, Points, Dummy, False, True);
      end;
    end;

  end;
  case DrawingChoice of
    dcAll:
      begin
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          Points, True, True);
      end;
    dcEdge: ; // do nothing
    else
      Assert(False);
  end;

  if Selected then
  begin
    APoint := ConvertTop2D_Point(ZoomBox, Location);
    if BitMap is TBitmap32 then
    begin
      if (APoint.x >= 0) and (APoint.x < TBitmap32(BitMap).Width)
        and (APoint.y >= 0) and (APoint.y < TBitmap32(BitMap).Height) then
      begin
        ACanvas := TCanvas.Create;
        try
          ACanvas.Handle := TBitmap32(BitMap).Handle;
          ACanvas.Pen.Color := clBlack;
          ACanvas.Brush.Style := bsClear;
          ACanvas.Ellipse(APoint.X-NodeRadius,APoint.Y-NodeRadius,
            APoint.X+NodeRadius+1,APoint.Y+NodeRadius+1);
        finally
          ACanvas.Free;
        end;
      end;
    end
    else
    begin
      (BitMap as TCanvas).Ellipse(APoint.X-NodeRadius,APoint.Y-NodeRadius,
            APoint.X+NodeRadius+1,APoint.Y+NodeRadius+1);
    end;

  end;

//  if DrawNumbers then
//  begin
//    APoint := ConvertTop2D_Point(ZoomBox, Location);
//    NumString := InttoStr(DisplayNumber);
//    BitMap.Textout(APoint.X+1, APoint.Y+1, NumString);
//
//  end;
end;

function TSutraNode2D.EdgeNode: boolean;
begin
  result := NodeType in [ntEdge, ntSubDomain]
end;

Function ComparePointRightUp(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P1.x - P2.x);
  if result = 0 then
  begin
    result := Sign(P1.y - P2.y);
  end;
end;

Function ComparePointsUp(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P1.y - P2.y);
end;

Function ComparePointsDown(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P2.y - P1.y);
end;

Function ComparePointsRight(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P1.x - P2.x);
end;

Function ComparePointsLeft(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P2.x - P1.x);
end;

Function ComparePointRightDown(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P1.x - P2.x);
  if result = 0 then
  begin
    result := Sign(P2.y - P1.y);
  end;
end;

Function ComparePointLeftUp(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P2.x - P1.x);
  if result = 0 then
  begin
    result := Sign(P1.y - P2.y);
  end;
end;

Function ComparePointLeftDown(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P2.x - P1.x);
  if result = 0 then
  begin
    result := Sign(P2.y - P1.y);
  end;
end;

function TSutraNode2D.CellIntersection(const Input: TSegment2D;
  out IntersectingSegments: TSegment2DArray): boolean;
var
  Outline: TVertexArray;
  IntersectionList: TVertexList;
  EdgeSegment: TSegment2D;
  NodeIndex: Integer;
  IntersectIndex: integer;
  APoint: TPoint2D;
  PointIndex: Integer;
  InputLength: double;
  function ClosestPointToInput: TPoint2d;
  var
//    MinIndex: Integer;
    MinDistance: double;
    TestPoint: TPoint2D;
    PIndex: integer;
    TestDistance: double;
    FoundFirst: boolean;
  begin
    FoundFirst := False;
    MinDistance := MaxInt;
    for PIndex := 0 to Length(Outline) - 1 do
    begin
      if (Outline[PIndex].x = Location.x) and (Outline[PIndex].y = Location.y) then
      begin
        Continue;
      end;
      if FoundFirst then
      begin
        TestPoint := ClosestPointOnSegmentFromPoint(Input, Outline[PIndex]);
        TestDistance := Distance(TestPoint, Outline[PIndex]);
        if TestDistance < MinDistance then
        begin
          MinDistance := TestDistance;
          result := TestPoint;
        end;
      end
      else
      begin
        FoundFirst := True;
        TestPoint := ClosestPointOnSegmentFromPoint(Input, Outline[PIndex]);
        result := TestPoint;
        MinDistance := Distance(TestPoint, Outline[PIndex]);
      end;
    end;
  end;
begin
  result := False;
  try
    if (Input[1].x = Input[2].x) and (Input[1].y = Input[2].y) then
    begin
      if IsInsideCell(Input[1]) then
      begin
        SetLength(IntersectingSegments, 1);
        IntersectingSegments[0] := Input;
        result := true;
      end;
      Exit;
    end;
    GetCellOutline(Outline);
    IntersectionList := TVertexList.Create;
    try
      IntersectionList.Capacity := Length(Outline)+2;
      EdgeSegment[2] := Outline[Length(Outline)-1];
      InputLength := Distance(Input[1], Input[2]);
      for NodeIndex := 0 to Length(Outline) - 1 do
      begin
        EdgeSegment[1] := Outline[NodeIndex];
//        if Collinear(EdgeSegment[1], Input[1], Input[2])
//          and Collinear(EdgeSegment[2], Input[1], Input[2]) then
        if (Distance(EdgeSegment[1], Input)/InputLength < 1e-7)
          and (Distance(EdgeSegment[2], Input)/InputLength < 1e-7) then
        begin
          IntersectionList.Add(EdgeSegment[1]);
          IntersectionList.Add(EdgeSegment[2]);
        end
        else
        if Intersect(EdgeSegment, Input, APoint.x, APoint.y) then
        begin
          IntersectionList.Add(APoint);
        end;
        EdgeSegment[2] := EdgeSegment[1];
      end;
      if (IntersectionList.Count = 0) then
      begin
        if IsInsideCell(Input[1]) then
        begin
          SetLength(IntersectingSegments, 1);
          IntersectingSegments[0] := Input;
          IntersectingSegments[0][2] := ClosestPointToInput;
          result := true;
        end
        else if IsInsideCell(Input[2]) then
        begin
          SetLength(IntersectingSegments, 1);
          IntersectingSegments[0] := Input;
          IntersectingSegments[0][1] := ClosestPointToInput;
          result := true;
        end
        else if Distance(Input[1], Location) < 1e-7 then
        begin
          SetLength(IntersectingSegments, 1);
          IntersectingSegments[0][1] := Location;
          IntersectingSegments[0][2] := ClosestPointToInput;
          result := true;
        end
        else if Distance(Input[2], Location) < 1e-7 then
        begin
          SetLength(IntersectingSegments, 1);
          IntersectingSegments[0][2] := Location;
          IntersectingSegments[0][1] := ClosestPointToInput;
          result := true;
        end;

        Exit;
      end;
      IntersectionList.Add(Input[1]);
      IntersectionList.Add(Input[2]);
      if Input[2].x = Input[1].x then
      begin
        if Input[2].y >= Input[1].y then
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointsUp));
        end
        else
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointsDown));
        end;
      end
      else if Input[2].y = Input[1].y then
      begin
        if Input[2].x >= Input[1].x then
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointsRight));
        end
        else
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointsLeft));
        end;
      end
      else if Input[2].x >= Input[1].x then
      begin
        if Input[2].y >= Input[1].y then
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointRightUp));
        end
        else
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointRightDown));
        end;
      end
      else
      begin
        if Input[2].y >= Input[1].y then
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointLeftUp));
        end
        else
        begin
          IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointLeftDown));
        end;
      end;
      for PointIndex := IntersectionList.Count-2 downto 0 do
      begin
        if IsEqual(IntersectionList[PointIndex],
          IntersectionList[PointIndex+1], FastGEO.Epsilon) then
        begin
          IntersectionList.Delete(PointIndex);
        end;
      end;
      if IntersectionList.Count = 1 then
      begin
        IntersectionList.Add(IntersectionList[0]);
      end;
      SetLength(IntersectingSegments, IntersectionList.Count-1);
      IntersectIndex := 0;
      for PointIndex := 0 to IntersectionList.Count - 2 do
      begin
        APoint.x := (IntersectionList[PointIndex].x
          + IntersectionList[PointIndex+1].x)/2;
        APoint.y := (IntersectionList[PointIndex].y
          + IntersectionList[PointIndex+1].y)/2;
        if IsInsideCell(APoint) then
        begin
          IntersectingSegments[IntersectIndex] :=
            EquateSegment(IntersectionList[PointIndex],
            IntersectionList[PointIndex+1]);
          Inc(IntersectIndex);
        end;
      end;
    finally
      IntersectionList.Free;
    end;
    SetLength(IntersectingSegments, IntersectIndex);
    result := IntersectIndex > 0;
  finally
    if result then
    begin
      for IntersectIndex := 0 to Length(IntersectingSegments) - 1 do
      begin
        UpdateSegmentOrientation(Input, IntersectingSegments[IntersectIndex]);
      end;
    end;
  end;
end;

procedure TSutraNode2D.ClearElements;
begin
  FElements.Clear;
end;

function TSutraNode2D.GetActiveElement(Index: Integer): IElement;
begin
  result := Elements[Index];
end;

function TSutraNode2D.GetActiveElementCount: integer;
begin
  result := FElements.Count;
end;

procedure TSutraNode2D.GetCellOutline(var CellOutline: TVertexArray);
var
  PriorUnion: TGpcPolygonClass;
  NodePosition: Integer;
  PriorIndex: Integer;
  VertexList: TVertexList;
  SelfVertex: Tgpc_vertex;
  VertexIndex: Integer;
  NextNode: TSutraNode2D;
  Element: TSutraElement2D;
  UnionPolygon: TGpcPolygonClass;
  NodeList: TSutraNode2D_List;
  NextIndex: Integer;
  ElCenter: TPoint2D;
  EmptyPolygon: TGpcPolygonClass;
  AVertex: Tgpc_vertex;
  ElementPolygons: TGpcPolygonClass;
  ElementIndex: Integer;
  ReduceCount: Integer;
  PriorNode: TSutraNode2D;
  SelfIndex: Integer;
  PointIndex: Integer;
  ContourIndex: Integer;
  AnArea: double;
  TestArea: double;
  CIndex: Integer;
const
  MaxReduceCount = 10;
begin
  if (Length(FCellOutline) = 0) and (FElements.Count > 0) then
  begin
    VertexList := TVertexList.Create;
    ElementPolygons := TGpcPolygonClass.Create;
    NodeList := TSutraNode2D_List.Create;
    try
      SelfVertex.x := X;
      SelfVertex.y := Y;
      Assert(FElements.Count > 0);
      ElementPolygons.NumberOfContours := FElements.Count;
      for ElementIndex := 0 to FElements.Count - 1 do
      begin
        Element := FElements[ElementIndex];
        ElementPolygons.VertexCount[ElementIndex] := 4;
        ElementPolygons.Vertices[ElementIndex, 0] := SelfVertex;
        SelfIndex := Element.Nodes.IndexOfNode(self);
        Assert(SelfIndex >= 0);
        if SelfIndex = Element.Nodes.Count - 1 then
        begin
          NextIndex := 0;
        end
        else
        begin
          NextIndex := SelfIndex + 1;
        end;
        NextNode := Element.Nodes[NextIndex].Node;
        NodePosition := NodeList.IndexOf(NextNode);
        if NodePosition < 0 then
        begin
          AVertex.X := (X + NextNode.X) / 2;
          AVertex.Y := (Y + NextNode.Y) / 2;
          NodeList.Add(NextNode);
          VertexList.Add(AVertex);
        end
        else
        begin
          AVertex := VertexList[NodePosition];
        end;
        ElementPolygons.Vertices[ElementIndex, 1] := AVertex;
        ElCenter := Element.Center;
        AVertex.x := ElCenter.x;
        AVertex.y := ElCenter.y;
        ElementPolygons.Vertices[ElementIndex, 2] := AVertex;
        if SelfIndex = 0 then
        begin
          PriorIndex := Element.Nodes.Count - 1;
        end
        else
        begin
          PriorIndex := SelfIndex - 1;
        end;
        PriorNode := Element.Nodes[PriorIndex].Node;
        NodePosition := NodeList.IndexOf(PriorNode);
        if NodePosition < 0 then
        begin
          AVertex.X := (X + PriorNode.X) / 2;
          AVertex.Y := (Y + PriorNode.Y) / 2;
          NodeList.Add(PriorNode);
          VertexList.Add(AVertex);
        end
        else
        begin
          AVertex := VertexList[NodePosition];
        end;
        ElementPolygons.Vertices[ElementIndex, 3] := AVertex;
      end;
      EmptyPolygon := TGpcPolygonClass.Create;
      try
        EmptyPolygon.NumberOfContours := 0;
        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
          ElementPolygons, EmptyPolygon);
        try
          ReduceCount := 0;
          while UnionPolygon.NumberOfContours > 1 do
          begin
            PriorUnion := UnionPolygon;
            UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
              PriorUnion, EmptyPolygon);
            PriorUnion.Free;
            Inc(ReduceCount);
            if ReduceCount >= MaxReduceCount then
            begin
              Break;
              {$IFDEF DEBUG}
              SetLength(FCellOutline, 0);
              Beep;
              ShowMessage('TSutraNode2D.Number = ' + IntToStr(Number));
              Exit;
              {$ELSE}
              raise Exception.Create(Format(StrErrorGeneratingCel, [Number + 1]));
              {$ENDIF}
            end;
          end;

          PriorUnion := UnionPolygon;
          UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
            PriorUnion, EmptyPolygon);
          PriorUnion.Free;

          ContourIndex := 0;
          if UnionPolygon.NumberOfContours > 1 then
          begin
            AnArea := Abs(UnionPolygon.ContourArea(0));
            for CIndex := 1 to UnionPolygon.NumberOfContours - 1 do
            begin
              TestArea := Abs(UnionPolygon.ContourArea(CIndex));
              if TestArea > AnArea then
              begin
                AnArea := TestArea;
                ContourIndex := CIndex;
              end;
            end;
          end;

//          Assert(UnionPolygon.NumberOfContours = 1);
          if UnionPolygon.NumberOfContours > 0 then
          begin
            SetLength(FCellOutline, UnionPolygon.VertexCount[ContourIndex]);
            for VertexIndex := 0 to UnionPolygon.VertexCount[ContourIndex] - 1 do
            begin
              FCellOutline[VertexIndex] := UnionPolygon.Vertices[ContourIndex, VertexIndex];
            end;
          end
          else
          begin
            FCellOutline := nil;
          end;



        finally
          UnionPolygon.Free;
        end;
      finally
        EmptyPolygon.Free;
      end;
    finally
      NodeList.Free;
      ElementPolygons.Free;
      VertexList.Free;
    end;
    if Length(FCellOutline) > 0 then
    begin
      FMinX := x;
      FMaxX := x;
      FMinY := y;
      FMaxY := y;
      for PointIndex := 0 to Length(FCellOutline) - 1 do
      begin
        if FMaxX < FCellOutline[PointIndex].x then
        begin
          FMaxX := FCellOutline[PointIndex].x;
        end
        else if FMinX > FCellOutline[PointIndex].x then
        begin
          FMinX := FCellOutline[PointIndex].x;
        end;

        if FMaxY < FCellOutline[PointIndex].y then
        begin
          FMaxY := FCellOutline[PointIndex].y;
        end
        else if FMinY > FCellOutline[PointIndex].y then
        begin
          FMinY := FCellOutline[PointIndex].y;
        end;
      end;
    end
    else
    begin
      FMinX := 0;
      FMaxX := 0;
      FMinY := 0;
      FMaxY := 0;
    end;
  end;
  CellOutline := FCellOutline;
  SetLength(CellOutline, Length(CellOutline));
end;

function TSutraNode2D.GetCountTri: Integer;
begin
  result := GetActiveElementCount;
end;

function TSutraNode2D.GetElement(Index: Integer): TSutraElement2D;
begin
  result := FElements[Index];
end;

function TSutraNode2D.GetElementCount: Integer;
begin
  result := FElements.Count;
end;

function TSutraNode2D.GetItemTri(Index: Integer): ITriangulatable;
begin
  result := GetElement(Index);
end;

function TSutraNode2D.GetElementI(Index: integer): IElement2D;
begin
  result :=  Elements[Index];
end;

function TSutraNode2D.GetLocation: TPoint2D;
begin
  result := FLocation;
end;

function TSutraNode2D.GetMaxX: Double;
var
  Dummy: TVertexArray;
begin
  GetCellOutline(Dummy);
  result := FMaxX;
end;

function TSutraNode2D.GetMaxY: Double;
var
  Dummy: TVertexArray;
begin
  GetCellOutline(Dummy);
  result := FMaxY;
end;

function TSutraNode2D.GetMinX: Double;
var
  Dummy: TVertexArray;
begin
  GetCellOutline(Dummy);
  result := FMinX;
end;

function TSutraNode2D.GetMinY: Double;
var
  Dummy: TVertexArray;
begin
  GetCellOutline(Dummy);
  result := FMinY;
end;

function TSutraNode2D.GetNodeNumber: Integer;
begin
  result := Number;
end;

function TSutraNode2D.GetNodeType: TNodeType;
begin
  result := FNodeType;
end;

function TSutraNode2D.GetNumber: integer;
begin
  result := DisplayNumber;
end;

function TSutraNode2D.GetTriangNumber: integer;
begin
  result := FTriangNumber;
end;

function TSutraNode2D.IdealElementCount: integer;
var
  AllAngles: Double;
begin
  AllAngles := SumOfAngles;
  if AllAngles <= 135 then
  begin
    result := 1;
  end
  else if AllAngles <= 225 then
  begin
    result := 2;
  end
  else if AllAngles <= 315 then
  begin
    result := 3;
  end
  else
  begin
    result := 4;
  end;
end;

function TSutraNode2D.IndexOf(Item: ITriangulatable): Integer;
begin
  result := FElements.IndexOf(Item as TSutraElement2D)
end;

procedure TSutraNode2D.InvalidateCellOutline;
begin
  SetLength(FCellOutline, 0)
end;

function TSutraNode2D.IsInside(APoint: TPoint2D): Boolean;
var
  Outline: TVertexArray;
  OutlinePolygon: TPolygon2D;
  PolyIndex: Integer;
begin
  GetCellOutline(Outline);
  SetLength(OutlinePolygon, Length(Outline));
  for PolyIndex := 0 to Length(Outline) - 1 do
  begin
    OutlinePolygon[PolyIndex] := Outline[PolyIndex];
  end;
  result := ModelMuseUtilities.PointInConcavePolygon(APoint, OutlinePolygon);
end;

function TSutraNode2D.IsInsideCell(APoint: TPoint2D): Boolean;
const
  DistEpsilon = 5e-6;
var
  AVertex: Tgpc_vertex;
  CellOutline : TVertexArray;
  VertexIndex: Integer;
  PriorVertex: Tgpc_vertex;
  SegDistance: double;

  function NearlyTheSame(A, B: Single): Boolean;
  const
    Epsilon = 1e-6;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
    end;
  end;
begin
  if NearlyTheSame(APoint.x, FLocation.x)
    and NearlyTheSame(APoint.y, FLocation.y) then
  begin
    result := True;
    Exit;
  end;

  GetCellOutline(CellOutline);

  result := false;
  PriorVertex := CellOutline[Length(CellOutline) - 1];
  for VertexIndex := 0 to Length(CellOutline) - 1 do
  begin
    AVertex := CellOutline[VertexIndex];
    if ((APoint.Y <= AVertex.Y) = (APoint.Y > PriorVertex.Y)) and
      (APoint.X - AVertex.X - (APoint.Y - AVertex.Y) *
      (PriorVertex.X - AVertex.X) /
      (PriorVertex.Y - AVertex.Y) < 0) then
    begin
      result := not result;
    end;
    SegDistance := Distance(AVertex, PriorVertex);
    if SegDistance <> 0 then
    begin
      if Distance(APoint, EquateSegment(AVertex, PriorVertex))
        /SegDistance < DistEpsilon then
      begin
        result := True;
        Exit;
      end;
    end;
//    if Collinear(AVertex, PriorVertex, APoint) then
//    begin
//      result := True;
//      Exit;
//    end;
    PriorVertex := AVertex;
  end;


end;

function TSutraNode2D.IsNeighbor(OtherItem: ITriangulatable): Boolean;
var
  NodeIndex: Integer;
  AnElement: ITriangulatable;
begin
  result := False;
  for NodeIndex := 0 to OtherItem.ItemCount - 1 do
  begin
    AnElement := OtherItem.ItemTri[NodeIndex];
    if FElements.IndexOf(AnElement as TSutraElement2D ) >= 0 then
    begin
      result := True;
      Exit;
    end;
  end;
end;

function TSutraNode2D.ReferenceLength: double;
var
  DiagonalArray: array[0..1] of double;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  TestValue: Double;
  TempValue: Double;
begin
  DiagonalArray[0] := 0;
  DiagonalArray[1] := 0;
  for ElementIndex := 0 to ElementCount - 1 do
  begin
    AnElement := Elements[ElementIndex];
    TestValue := AnElement.ReferenceLength;
    if TestValue > DiagonalArray[0] then
    begin
      TempValue := DiagonalArray[0];
      DiagonalArray[0]  := TestValue;
      TestValue := TempValue;
    end;
    if TestValue > DiagonalArray[1] then
    begin
      DiagonalArray[1]  := TestValue;
    end;
  end;
  result := (DiagonalArray[0] + DiagonalArray[1])/2;

end;

procedure TSutraNode2D.RemoveElement(AnElement: TSutraElement2D);
begin
  FElements.Remove(AnElement);
end;

procedure TSutraNode2D.SetNodeNumber(Value: Integer);
begin
  Number := Value;
end;

procedure TSutraNode2D.SetNodeType(const Value: TNodeType);
begin
  if FNodeType <> Value then
  begin
    BeginUpdate;
    try
      FNodeType := Value;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSutraNode2D.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
end;

procedure TSutraNode2D.SetTriangNumber(const Value: Integer);
begin
  FTriangNumber := Value;
end;

procedure TSutraNode2D.SetLocation(const Value: TPoint2D);
begin
  X := Value.x;
  Y := Value.y;
end;

procedure TSutraNode2D.InvalidateOutlines;
var
  ElIndex: integer;
  Element: TSutraElement2D;
  NodeIndex: integer;
  Node: TSutraNode2D;
  LocalCol:TSutraNode2D_Collection;
begin
  for ElIndex := 0 to ElementCount - 1 do
  begin
    Element := Elements[ElIndex];
    for NodeIndex := 0 to Element.Nodes.Count - 1 do
    begin
      Node := Element.Nodes[NodeIndex].Node;
      Node.InvalidateCellOutline;
    end;
  end;
  LocalCol := Collection as TSutraNode2D_Collection;
  LocalCol.InvalidateNodeIntervals;
  LocalCol.Mesh2D.Elements.InvalidateElementIntervals;
  LocalCol.Mesh2D.Mesh3D.InvalidatePolygons;
end;

procedure TSutraNode2D.SetX(const Value: FastGEO.TFloat);
begin
  if FLocation.x <> Value then
  begin
    InvalidateOutlines;
  end;
  SetRealProperty(FLocation.x, Value);
end;

procedure TSutraNode2D.SetY(const Value: FastGEO.TFloat);
begin
  if FLocation.y <> Value then
  begin
    InvalidateOutlines;
  end;
  SetRealProperty(FLocation.y, Value);
end;

function TSutraNode2D.SumOfAngles: double;
var
  index: integer;
begin
  result := 0;
  for index := 0 to ElementCount - 1 do
  begin
    result := result + Elements[index].NodeAngle(self);
  end;
end;

{ TSutraNode2D_Collection }

function TSutraNode2D_Collection.Add: TSutraNode2D;
begin
  result := inherited Add as TSutraNode2D;
end;

constructor TSutraNode2D_Collection.Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
var
  InvalidateModelEvent: TNotifyEvent;
begin
//  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TSutraNode2D, InvalidateModelEvent, ParentMesh);
//  FMesh2D := ParentMesh.Mesh2D;
end;

destructor TSutraNode2D_Collection.Destroy;
begin
  FNodesLocations.Free;
  FNodeIntervals.Free;
  FNodeRanges.Free;
  inherited;
end;

procedure TCustomSutraNode.Assign(Source: TPersistent);
var
  SourceNode: TCustomSutraNode;
begin
  if Source is TCustomSutraNode then
  begin
    SourceNode := TCustomSutraNode(Source);
    Number := SourceNode.Number;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCustomSutraNode.Create(Collection: TCollection);
begin
  inherited;
  FNumber := -1;
end;

procedure TCustomSutraNode.SetNumber(const Value: Integer);
begin
  if BypassUpdate then
  begin
    FNumber := Value;
  end
  else
  begin
    SetIntegerProperty(FNumber, Value);
  end;
end;

{ TSutraNodeNumberItem }

procedure TCustomSutraNodeNumberItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomSutraNodeNumberItem;
begin
  if Source is TCustomSutraNodeNumberItem then
  begin
    SourceItem := TCustomSutraNodeNumberItem(Source);
    NodeNumber := SourceItem.NodeNumber;
  end
  else
  begin
    inherited;
  end;
end;

function TCustomSutraNodeNumberItem.GetNodeNumber: integer;
begin
  if FNode = nil then
  begin
    result := FStoredNodeNumber;
  end
  else
  begin
    result := FNode.Number;
  end;
end;

procedure TCustomSutraNodeNumberItem.SetNode(const Value: TCustomSutraNode);
begin
  if FNode <> Value then
  begin
    InvalidateModel;
  end;
  FNode := Value;
  if FNode <> nil then
  begin
    FStoredNodeNumber := FNode.Number;
  end;
end;

procedure TCustomSutraNodeNumberItem.SetNodeNumber(const Value: integer);
begin
  SetIntegerProperty(FStoredNodeNumber, Value);
  UpdateNode;
end;

function TSutraNode2D_Collection.GetItem(Index: integer): TSutraNode2D;
begin
  result := inherited Items[Index] as TSutraNode2D;
end;

function TSutraNode2D_Collection.GetMesh2D: TSutraMesh2D;
begin
  result := FMesh.Mesh2D;
end;

function TSutraNode2D_Collection.GetNodeRanges: TRbwRangeTree;
var
  LeafList: TNode2DLeafList;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ALeaf: TNode2DLeaf;
begin
  if FNodeRanges = nil then
  begin
    LeafList := TNode2DLeafList.Create;
    for NodeIndex := 0 to Count - 1 do
    begin
      ANode := Items[NodeIndex];
      ALeaf := TNode2DLeaf.Create(ANode);
      { $IFDEF ErrorOnFree}
//      RegisterExpectedMemoryLeak(ALeaf);
      { $ENDIF}
      LeafList.Add(ALeaf);
    end;
    FNodeRanges := TRbwRangeTree.Create(LeafList);
  end;
  result := FNodeRanges;
end;

function TSutraNode2D_Collection.GetNodesLocations: TRbwQuadTree;
var
  Limits: TGridLimit;
  index: Integer;
  Node: TSutraNode2D;
begin
  if FNodesLocations = nil then
  begin
    FNodesLocations := TRbwQuadTree.Create(nil);
    FNodesLocations.MaxPoints := 20;
    Limits := Mesh2D.MeshLimits;
    FNodesLocations.XMax := Limits.MaxX;
    FNodesLocations.XMin := Limits.MinX;
    FNodesLocations.YMax := Limits.MaxY;
    FNodesLocations.YMin := Limits.MinY;
    for index := 0 to Count - 1 do
    begin
      Node := Items[index];
      FNodesLocations.AddPoint(Node.X, Node.Y, Node);
    end;
  end;
  result := FNodesLocations;
end;

procedure TSutraNode2D_Collection.InvalidateNodeIntervals;
begin
  FreeAndNil(FNodeIntervals);
  FreeAndNil(FNodeRanges);
  FreeAndNil(FNodesLocations);
end;

procedure TSutraNode2D_Collection.NodeXLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  Node2D: TSutraNode2D;
begin
  Node2D := Subject as TSutraNode2D;
  LowerBoundary := Node2D.MinX;
  UpperBoundary := Node2D.MaxX;
end;

procedure TSutraNode2D_Collection.NodeYLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  Node2D: TSutraNode2D;
begin
  Node2D := Subject as TSutraNode2D;
  LowerBoundary := Node2D.MinY;
  UpperBoundary := Node2D.MaxY;
end;

procedure TSutraNode2D_Collection.SortByNodeNumber;
var
  ANodeList: TList<TSutraNode2D>;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  ANodeList := TList<TSutraNode2D>.Create;
  try
    ANodeList.Capacity := Count;
    for NodeIndex := 0 to Count - 1 do
    begin
      ANodeList.Add(nil);
    end;
    for NodeIndex := 0 to Count - 1 do
    begin
      ANode := Items[NodeIndex];
      ANodeList[ANode.FNumber] := ANode;
    end;
    for NodeIndex := 0 to ANodeList.Count - 1 do
    begin
      ANode := ANodeList[NodeIndex];
      ANode.Index := NodeIndex;
    end;
  finally
    ANodeList.Free;
  end;
end;

function TSutraNode2D_Collection.TopContainingCell(
  APoint: TPoint2D): T2DTopCell;
var
  index: Integer;
  Node: TSutraNode2D;
  IntervalDef: TIntDefArray;
  Limits: TGridLimit;
  AList: TList;
  Location: TOneDRealArray;
  EpsilonX: Extended;
  EpsilonY: Extended;
  CellOutline: TVertexArray;
begin
  Result.Row := -1;
  Result.Col := -1;

  if Count = 0 then
  begin
    Exit;
  end;

  if FNodeIntervals = nil then
  begin
    Limits := Mesh2D.MeshLimits;
    SetLength(IntervalDef, 2);
    EpsilonX := (Limits.MaxX-Limits.MinX)/1e7;
    EpsilonY := (Limits.MaxY-Limits.MinY)/1e7;
    IntervalDef[0].LowerBoundary := Limits.MinX-EpsilonX;
    IntervalDef[0].UpperBoundary := Limits.MaxX+EpsilonX;
    IntervalDef[0].OnFindObjectInterval  := NodeXLimits;
    IntervalDef[1].LowerBoundary := Limits.MinY-EpsilonY;
    IntervalDef[1].UpperBoundary := Limits.MaxY+EpsilonY;
    IntervalDef[1].OnFindObjectInterval  := NodeYLimits;

    FNodeIntervals := TRbwIntervalTree.Create(IntervalDef);
    for index := 0 to Count - 1 do
    begin
      Node := Items[index];
      Node.GetCellOutline(CellOutline);
      if CellOutline <> nil then
      begin
        FNodeIntervals.Add(Node);
      end;
    end;
  end;

  AList := TList.Create;
  try
    SetLength(Location, 2);
    Location[0] := APoint.x;
    Location[1] := APoint.y;
    FNodeIntervals.FindContainingObjects(Location, AList);

    for index := 0 to AList.Count - 1 do
    begin
      Node := AList[index];
      if Node.IsInsideCell(APoint) then
      begin
        Result.Row := 0;
        Result.Col := Node.Number;
        Exit;
      end;
    end;
  finally
    AList.Free;
  end;

//  for index := 0 to Count - 1 do
//  begin
//    Node := Items[index];
//    if Node.IsInsideCell(APoint) then
//    begin
//      Result.Row := 0;
//      Result.Col := Node.Number;
//      Exit;
//    end;
//  end;
end;

{ TSutraNodeNumber2D_Item }

function TSutraNodeNumber2D_Item.GetNode: TSutraNode2D;
begin
  result := inherited Node as TSutraNode2D;
end;

procedure TSutraNodeNumber2D_Item.SetNode(const Value: TSutraNode2D);
var
  Element: TSutraElement2D;
begin
  Element := (Collection as TSutraNodeNumber2D_Collection).FElement;
  if (Value <> FNode) and (FNode <> nil) then
  begin
    if (Element <> nil) and ((FNode as TSutraNode2D).FElements.IndexOf(Element) >= 0) then
    begin
      (FNode as TSutraNode2D).FElements.Remove(Element);
    end;
  end;
  inherited SetNode(Value);
  Element := (Collection as TSutraNodeNumber2D_Collection).FElement;
  if (Element <> nil) and (Value.FElements.IndexOf(Element) < 0) then
  begin
    Value.FElements.Add(Element);
  end;
end;

procedure TSutraNodeNumber2D_Item.UpdateNode;
var
//  LocalModel: TCustomModel;
  Mesh: TSutraMesh2D;
  NewNode: TSutraNode2D;
  Element: TSutraElement2D;
begin
//  LocalModel := Model as TCustomModel;
  Mesh := (Collection as TSutraNodeNumber2D_Collection).FMesh.Mesh2D;
  if (FStoredNodeNumber >= 0) and (FStoredNodeNumber < Mesh.Nodes.Count) then
  begin
    NewNode := Mesh.Nodes[FStoredNodeNumber];
  end
  else
  begin
    NewNode := nil;
  end;
  if FNode <> NewNode then
  begin
    Element := (Collection as TSutraNodeNumber2D_Collection).FElement;
    if FNode <> nil then
    begin
      (FNode as TSutraNode2D).FElements.Remove(Element);
    end;
    if (NewNode <> nil) and (NewNode.FElements.IndexOf(Element) < 0) then
    begin
      NewNode.FElements.Add(Element);
    end;
    Mesh.BeginUpdate;
    try
      FNode := NewNode;
    finally
      Mesh.EndUpdate;
    end;
  end;
end;

{ TSutraNodeNumber2D_Collection }

function TSutraNodeNumber2D_Collection.Add: TSutraNodeNumber2D_Item;
begin
  result := inherited Add as TSutraNodeNumber2D_Item;
end;

constructor TSutraNodeNumber2D_Collection.Create(InvalidateModelEvent: TNotifyEvent;
  Element: TSutraElement2D; Mesh3D: TSutraMesh3D);
begin
  FElement := Element;
  inherited Create(TSutraNodeNumber2D_Item, InvalidateModelEvent, Mesh3D);
end;

function TSutraNodeNumber2D_Collection.GetItem(
  Index: integer): TSutraNodeNumber2D_Item;
begin
  result := inherited Items[Index] as TSutraNodeNumber2D_Item;
end;

function TSutraNodeNumber2D_Collection.IndexOfNode(Node: TSutraNode2D): Integer;
var
  NodeIndex: Integer;
begin
  result := -1;
  for NodeIndex := 0 to Count - 1 do
  begin
    if Items[NodeIndex].Node = Node then
    begin
      result := NodeIndex;
      Exit;
    end;
  end;
end;

procedure TSutraNodeNumber2D_Collection.Reverse;
var
  AList: TSutraNodeNumber2DList;
  index: Integer;
begin
  AList := TSutraNodeNumber2DList.Create;
  try
    AList.Capacity := Count;
    for index := Count - 1 downto 0 do
    begin
      AList.Add(Items[index]);
    end;
    for index := 0 to AList.Count - 1 do
    begin
      AList[index].Index := index;
    end;
  finally
    AList.Free;
  end;
end;

{ TCustomSutraElement }

procedure TCustomSutraElement.Assign(Source: TPersistent);
var
  SourceElement: TCustomSutraElement;
begin
  if Source is TCustomSutraElement then
  begin
    SourceElement := TCustomSutraElement(Source);
    ElementNumber := SourceElement.ElementNumber;
  end
  else
  begin
    inherited;
  end;
end;

function TCustomSutraElement.GetDisplayNumber: Integer;
begin
  result := ElementNumber+1;
end;

function TCustomSutraElement.GetElementNumber: integer;
begin
  result := FElementNumber;
end;

procedure TCustomSutraElement.SetElementNumber(Value: integer);
begin
  if BypassUpdate then
  begin
    FElementNumber := Value;
  end
  else
  begin
    SetIntegerProperty(FElementNumber, Value);
  end;
end;

{ TSutraElement2D }

procedure TSutraElement2D.Assign(Source: TPersistent);
var
  SourceElement: TSutraElement2D;
begin
  if Source is TSutraElement2D then
  begin
    SourceElement := TSutraElement2D(Source);
    Nodes := SourceElement.Nodes;
    Selected := SourceElement.Selected;
  end;
  inherited;
end;

procedure TSutraElement2D.AssignIElement(Source: IElement);
var
  NodeIndex: Integer;
  NodeItem: TSutraNodeNumber2D_Item;
  Node: INode;
begin
  Nodes.Clear;
  Nodes.Capacity := Source.NodeCount;
  for NodeIndex := 0 to Source.NodeCount - 1 do
  begin
    NodeItem := Nodes.Add;
    Node := Source.Nodes[NodeIndex];
    NodeItem.NodeNumber := Node.NodeNumber;
  end;
  ElementNumber := Source.ElementNumber;
end;

function TSutraElement2D.Center: TPoint2D;
var
  NodeIndex: Integer;
  Node: TSutraNode2D;
begin
  Result.x := 0;
  Result.y := 0;
  Assert(Nodes.Count = 4);
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    Result.x := Result.x + Node.X;
    Result.y := Result.y + Node.y;
  end;
  Result.x := Result.x/Nodes.Count;
  Result.y := Result.y/Nodes.Count;
end;

constructor TSutraElement2D.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TSutraNodeNumber2D_Collection.Create(OnInvalidateModel, self,
    (Collection as TSutraElement2D_Collection).FMesh);
end;

destructor TSutraElement2D.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TSutraElement2D.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
  DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
  Mesh3D: TSutraMesh3D; MinMax: TMinMax);
var
  Node1: TSutraNode2D;
  NodeIndex: Integer;
  Node2: TSutraNode2D;
  Points: GoPhastTypes.TPointArray;
  SumX: integer;
  SumY: Integer;
  NumString: string;
  Extent: TSize;
  Fraction: Double;
  AColor: TColor;
  Dummy: TPolygon32;
  ShowColor: Boolean;
  Element3D: TSutraElement3D;
//  DisplayNumber: Integer;
  ShowElement: Boolean;
  LocalDisplayNumber: Integer;
begin
  Dummy := nil;
  SumX := 0;
  SumY := 0;
  if (DataArray <> nil) or (DrawingChoice = dcAll)
    or Mesh3D.DrawElementNumbers then
  begin
    SetLength(Points, 5);
    Assert(Nodes.Count = 4);
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      Node2 := Nodes[NodeIndex].Node;
      Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, Node2.Location);
      SumX := SumX + Points[NodeIndex].X;
      SumY := SumY + Points[NodeIndex].Y;
    end;
    Points[4] := Points[0];
    if DataArray <> nil then
    begin
      GetDataSetMeshValue(DataArray, SelectedLayer, ElementNumber,
        StringValues, ShowColor, Fraction, MinMax);
      if ShowColor then
      begin
        AColor := frmGoPhast.PhastModel.GridColorParameters.
          FracToColor(Fraction);
        DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
          0, Points, Dummy, False, True);
      end;
    end;
  end;

  if Selected then
  begin
    DrawBigPolyline32(BitMap, clBlack32, ThickGridLineThickness,
      Points, True);
  end
  else
  begin
    case DrawingChoice of
      dcAll:
        begin
          DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
            Points, True);
        end;
      dcEdge:
        begin
          SetLength(Points, 2);
          Node1 := Nodes[3].Node;
          for NodeIndex := 0 to Nodes.Count - 1 do
          begin
            Node2 := Nodes[NodeIndex].Node;
            if Node1.EdgeNode and Node2.EdgeNode then
            begin
              Points[0] := ConvertTop2D_Point(ZoomBox, Node1.Location);
              Points[1] := ConvertTop2D_Point(ZoomBox, Node2.Location);
              DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                Points, True);
            end;
            Node1 := Node2;
          end;
        end;
      else
        Assert(False);
    end;
  end;

  if Mesh3D.DrawElementNumbers then
  begin
    ShowElement := True;
    if (Mesh3D.MeshType = mt3D) and (Mesh3D.Elements.Count > 0) then
    begin
      Element3D := Mesh3D.ElementArray[SelectedLayer, ElementNumber];
      LocalDisplayNumber := Element3D.DisplayNumber;
      ShowElement := Element3D.Active;
    end
    else
    begin
      LocalDisplayNumber := DisplayNumber;
    end;
    if ShowElement then
    begin
      SumX := SumX div 4;
      SumY := SumY div 4;
      NumString := InttoStr(LocalDisplayNumber);
      if BitMap is TBitmap32 then
      begin
        Extent := TBitmap32(BitMap).TextExtent(NumString);
        TBitmap32(BitMap).Textout(SumX - (Extent.cx div 2),
        SumY {- (Extent.cy div 2)}, NumString);
      end
      else
      begin
        (BitMap as TCanvas).Textout(SumX - (Extent.cx div 2),
        SumY {- (Extent.cy div 2)}, NumString);
      end;
    end;
  end;
end;

function TSutraElement2D.Edge(Index: integer): TSegment2D;
begin
  Assert((Index >= 0) and (Index <= 3));
  Result[1] := Nodes[Index].Node.Location;
  Inc(Index);
  if Index >= 4 then
  begin
    Index := 0;
  end;
  Result[2] := Nodes[Index].Node.Location;
end;

function TSutraElement2D.ElementArea: double;
var
  OutlinePolygon: TPolygon2D;
  PolyIndex: Integer;
begin
  SetLength(OutlinePolygon, Nodes.Count);
  for PolyIndex := 0 to Nodes.Count - 1 do
  begin
    OutlinePolygon[PolyIndex] := Nodes[PolyIndex].Node.Location;
  end;
  result := Abs(Area(OutlinePolygon));
end;

function TSutraElement2D.ElementOK: Boolean;
var
  Node1: TPoint2D;
  Node2: TPoint2D;
  Node3: TPoint2D;
  NodeIndex: Integer;
  Angle: FastGeo.TFloat;
begin
  Node1 := self.Nodes[2].Node.Location;
  Node2 := self.Nodes[3].Node.Location;
  result := True;
  for NodeIndex := 0 to self.Nodes.Count - 1 do
  begin
    Node3 := self.Nodes[NodeIndex].Node.Location;
    Angle := OrientedVertexAngle(Node1, Node2, Node3, CounterClockwise);
    Assert(Angle >= 0);
    if Angle >= 180 then
    begin
      result := False;
      Break;
    end;
    Node1 := Node2;
    Node2 := Node3;
  end;
end;

function TSutraElement2D.GetActiveNode(Index: Integer): INode;
begin
  result := Nodes[Index].Node;
end;

function TSutraElement2D.GetActiveNodeCount: integer;
begin
  result := Nodes.Count;
end;

function TSutraElement2D.GetCountTri: Integer;
begin
  result := NodeCount;
end;

function TSutraElement2D.GetItemTri(Index: Integer): ITriangulatable;
begin
  result := Nodes[Index].Node;
end;

function TSutraElement2D.GetLocation: TPoint2D;
begin
  result := Center;
end;

function TSutraElement2D.AspectRatio(VerticalExaggeration: Double = 1): Double;
var
  Node1: TSutraNode2D;
  Node2: TSutraNode2D;
  Node3: TSutraNode2D;
  Node4: TSutraNode2D;
  Distance13: FastGeo.TFloat;
  Distance24: FastGeo.TFloat;
  TestDistance: FastGeo.TFloat;
  MinDistance: FastGeo.TFloat;
  LongDistance: Extended;
  P1: TPoint2D;
  P2: TPoint2D;
  P3: TPoint2D;
  P4: TPoint2D;
  TempP: TPoint2D;
begin
  Assert(Nodes.Count = 4);
  Node1 := Nodes[0].Node;
  Node2 := Nodes[1].Node;
  Node3 := Nodes[2].Node;
  Node4 := Nodes[3].Node;
  P1 := Node1.Location;
  P2 := Node2.Location;
  P3 := Node3.Location;
  P4 := Node4.Location;
  if (VerticalExaggeration <> 1) and (VerticalExaggeration <> 0) then
  begin
    P1.y := P1.y*VerticalExaggeration;
    P2.y := P2.y*VerticalExaggeration;
    P3.y := P3.y*VerticalExaggeration;
    P4.y := P4.y*VerticalExaggeration;
  end;
  Distance13 := Distance(P1, P3);
  Distance24 := Distance(P2, P4);
  if Distance13 > Distance24 then
  begin
    LongDistance := Distance13;
  end
  else
  begin
    LongDistance := Distance24;
    TempP := P1;
    P1 := P2;
    P2 := P3;
    P3 := P4;
    P4 := TempP
  end;
  if LongDistance <= 0 then
  begin
    result := 0;
    Exit;
  end;
  TestDistance := MinimumDistanceFromPointToSegment(P2,
    EquateSegment(P1, P4));
  MinDistance := TestDistance;
  TestDistance := MinimumDistanceFromPointToSegment(P2,
    EquateSegment(P3, P4));
  if TestDistance < MinDistance then
  begin
    MinDistance := TestDistance;
  end;
  TestDistance := MinimumDistanceFromPointToSegment(P4,
    EquateSegment(P1, P2));
  if TestDistance < MinDistance then
  begin
    MinDistance := TestDistance;
  end;
  TestDistance := MinimumDistanceFromPointToSegment(P4,
    EquateSegment(P3, P2));
  if TestDistance < MinDistance then
  begin
    MinDistance := TestDistance;
  end;
  result := MinDistance/LongDistance;

end;

function TSutraElement2D.GetMaxX: Double;
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  if Nodes.Count > 0 then
  begin
    result := Nodes[0].Node.x;
    for NodeIndex := 1 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex].Node;
      if ANode.x > result then
      begin
        result := ANode.x;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TSutraElement2D.GetMaxY: Double;
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  if Nodes.Count > 0 then
  begin
    result := Nodes[0].Node.y;
    for NodeIndex := 1 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex].Node;
      if ANode.y > result then
      begin
        result := ANode.y;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TSutraElement2D.GetMinX: Double;
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  if Nodes.Count > 0 then
  begin
    result := Nodes[0].Node.x;
    for NodeIndex := 1 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex].Node;
      if ANode.x < result then
      begin
        result := ANode.x;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TSutraElement2D.GetMinY: Double;
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  if Nodes.Count > 0 then
  begin
    result := Nodes[0].Node.y;
    for NodeIndex := 1 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex].Node;
      if ANode.y < result then
      begin
        result := ANode.y;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TSutraElement2D.GetNeighborElements(
  ElementList: TSutraElement2D_List);
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
begin
  ElementList.Clear;
  for NodeIndex := 0 to NodeCount - 1 do
  begin
    ANode := Nodes[NodeIndex].Node;
    for ElementIndex := 0 to ANode.ElementCount - 1 do
    begin
      AnElement := ANode.Elements[ElementIndex];
      if (AnElement <> self) and (ElementList.IndexOf(AnElement) < 0)
        and AnElement.IsNeighbor(self) then
      begin
        ElementList.Add(AnElement);
      end;
    end;
  end;
end;

procedure TSutraElement2D.GetNeighborsI(ElementList: TIElement2DList);
var
  ElementList2D: TSutraElement2D_List;
  ElementIndex: Integer;
begin
  ElementList.Clear;
  ElementList2D := TSutraElement2D_List.Create;
  try
    GetNeighborElements(ElementList2D);
    ElementList.Capacity := ElementList2D.Count;
    for ElementIndex := 0 to ElementList2D.Count - 1 do
    begin
      ElementList.Add(ElementList2D[ElementIndex]);
    end;
  finally
    ElementList2D.Free;
  end;
end;

function TSutraElement2D.GetNodeI(Index: integer): INode2D;
begin
  result := Nodes[Index].Node;
end;

function TSutraElement2D.GetNumber: integer;
begin
  result := GetDisplayNumber;
end;

function TSutraElement2D.GetTriangNumber: integer;
begin
  result := FTriangNumber;
end;

function TSutraElement2D.HasSameNodes(Element: TSutraElement2D): Boolean;
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  result := True;
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[NodeIndex].Node;
    result := Element.FNodes.IndexOfNode(ANode) >= 0;
    if not result then
    begin
      Exit;
    end;
  end;
end;

function TSutraElement2D.IndexOf(Item: ITriangulatable): Integer;
begin
  result := FNodes.IndexOfNode(Item as TSutraNode2D);
end;

function TSutraElement2D.IndexOfNode(Node: INode2D): Integer;
var
  NodeIndex: Integer;
begin
  result := -1;
  for NodeIndex := 0 to NodeCount - 1 do
  begin
    if NodesI[NodeIndex] = Node then
    begin
      result := NodeIndex;
      break;
    end;
  end;
end;

function TSutraElement2D.Intersection(const Input: TSegment2D;
  out IntersectingSegment: TSegment2D): boolean;
var
  Quad: TQuadix2D;
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Quad[NodeIndex+1] := Nodes[NodeIndex].Node.Location;
  end;
  result := Clip(Input, Quad, IntersectingSegment);
  if result then
  begin
    UpdateSegmentOrientation(Input, IntersectingSegment);
  end;
end;

function TSutraElement2D.IsInside(APoint: TPoint2D): Boolean;
var
  NodeIndex: Integer;
  PriorNode: TSutraNode2D;
  Node: TSutraNode2D;
begin
  result := false;
  PriorNode := Nodes[Nodes.Count - 1].Node;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    if ((APoint.Y <= Node.Y) = (APoint.Y > PriorNode.Y)) and
      (APoint.X - Node.X - (APoint.Y - Node.Y) *
      (PriorNode.X - Node.X) /
      (PriorNode.Y - Node.Y) < 0) then
    begin
      result := not result;
    end;
    PriorNode := Node;
  end;
end;

function TSutraElement2D.IsNeighbor(OtherItem: ITriangulatable): Boolean;
var
  Count: Integer;
  NodeIndex: Integer;
  AnItem: ITriangulatable;
begin
  Count := 0;
  for NodeIndex := 0 to OtherItem.ItemCount - 1 do
  begin
    AnItem := OtherItem.ItemTri[NodeIndex];
    if FNodes.IndexOfNode(AnItem as TSutraNode2D ) >= 0 then
    begin
      Inc(Count);
    end;
  end;
  Assert(Count <= 2);
  result := Count = 2;
end;

function TSutraElement2D.ReferenceLength: Double;
var
  Node1: TSutraNode2D;
  Node2: TSutraNode2D;
  Node3: TSutraNode2D;
  Node4: TSutraNode2D;
begin
  Assert(Nodes.Count = 4);
  Node1 := Nodes[0].Node;
  Node2 := Nodes[1].Node;
  Node3 := Nodes[2].Node;
  Node4 := Nodes[3].Node;
  result := Max(Distance(Node1.Location, Node3.Location),
    Distance(Node2.Location, Node4.Location));
end;

function TSutraElement2D.OnEdge(APoint: TPoint2D): Boolean;
var
  NodeIndex: Integer;
  PriorNode: TSutraNode2D;
  Node: TSutraNode2D;
  ASegment: TSegment2D;
begin
  result := false;
  PriorNode := Nodes[Nodes.Count - 1].Node;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    ASegment[1] := PriorNode.Location;
    ASegment[2] := Node.Location;
    if ((APoint.Y <= Node.Y) = (APoint.Y >+ PriorNode.Y)) and
      (Collinear(PriorNode.Location, APoint, Node.Location)
      or (Distance(APoint, ASegment) < Distance(ASegment[1], ASegment[2])/10000)) then
    begin
      result := True;
      break;
    end;
    PriorNode := Node;
  end;
end;

function TSutraElement2D.NodeAngle(ANode: TSutraNode2D): double;
var
  NodePosition: Integer;
  PriorNode: TSutraNode2D;
  NextNode: TSutraNode2D;
begin
  NodePosition := FNodes.IndexOfNode(ANode);
  Assert(NodePosition >= 0);
  if NodePosition = 0 then
  begin
    PriorNode := FNodes[FNodes.Count-1].Node;
  end
  else
  begin
    PriorNode := FNodes[NodePosition-1].Node;
  end;
  if NodePosition = FNodes.Count-1 then
  begin
    NextNode := FNodes[0].Node;
  end
  else
  begin
    NextNode := FNodes[NodePosition+1].Node;
  end;
  result := OrientedVertexAngle(PriorNode.Location, ANode.Location,
    NextNode.Location, CounterClockwise);
end;

procedure TSutraElement2D.SetCorrectOrienatation;
var
  InputPoints: TPolygon2D;
  OutputPoints: TPolygon2D;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ElementOrienatation: integer;
begin
  Assert(Nodes.Count > 0);
  SetLength(InputPoints, Nodes.Count);
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    ANode := Nodes[NodeIndex].Node;
    InputPoints[NodeIndex] := ANode.Location;
  end;
  ConvexHull2(InputPoints, ElementOrienatation, OutputPoints);
  if (ElementOrienatation <> CounterClockwise) then
  begin
    Nodes.Reverse;
  end;

end;

procedure TSutraElement2D.SetNodes(
  const Value: TSutraNodeNumber2D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraElement2D.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
end;

procedure TSutraElement2D.SetTriangNumber(const Value: Integer);
begin
  FTriangNumber := Value;
end;

{ TSutraElement2D_Collection }

function TSutraElement2D_Collection.Add: TSutraElement2D;
begin
  result := inherited Add as TSutraElement2D;
end;

constructor TSutraElement2D_Collection.Create(InvalidateModelEvent: TNotifyEvent;
  ParentMesh: TSutraMesh3D);
begin
  inherited Create(TSutraElement2D, InvalidateModelEvent, ParentMesh);
end;

destructor TSutraElement2D_Collection.Destroy;
begin
  FElementCenters.Free;
  FElementIntervals.Free;
  FElementRanges.Free;
  inherited;
end;

function TSutraElement2D_Collection.GetElementCenters: TRbwQuadTree;
var
  Limits: TGridLimit;
  index: Integer;
  Element: TSutraElement2D;
  Center: TPoint2D;
begin
  if FElementCenters = nil then
  begin
    FElementCenters := TRbwQuadTree.Create(nil);
    FElementCenters.MaxPoints := 20;
    Limits := Mesh2D.MeshLimits;
    FElementCenters.XMax := Limits.MaxX;
    FElementCenters.XMin := Limits.MinX;
    FElementCenters.YMax := Limits.MaxY;
    FElementCenters.YMin := Limits.MinY;
    for index := 0 to Count - 1 do
    begin
      Element := Items[index];
      Center := Element.Center;
      FElementCenters.AddPoint(Center.X, Center.Y, Element);
    end;
  end;
  result := FElementCenters;
end;

function TSutraElement2D_Collection.GetElementRanges: TRbwRangeTree;
var
  LeafList: TElement2DLeafList;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  Leaf: TElement2DLeaf;
begin
  if FElementRanges = nil then
  begin
    LeafList := TElement2DLeafList.Create;
    for ElementIndex := 0 to Count - 1 do
    begin
      AnElement := Items[ElementIndex];
      Leaf := TElement2DLeaf.Create(AnElement);
      { $IFDEF ErrorOnFree}
//      RegisterExpectedMemoryLeak(Leaf);
      { $ENDIF}

      LeafList.Add(Leaf);
    end;
    FElementRanges := TRbwRangeTree.Create(LeafList);
  end;
  result := FElementRanges
end;

function TSutraElement2D_Collection.GetItems(Index: integer): TSutraElement2D;
begin
  result := inherited Items[Index] as TSutraElement2D
end;

function TSutraElement2D_Collection.GetMesh2D: TSutraMesh2D;
begin
  result := FMesh.Mesh2D;
end;

procedure TSutraElement2D_Collection.InvalidateElementIntervals;
begin
  FreeAndNil(FElementIntervals);
  FreeAndNil(FElementRanges);
  FreeAndNil(FElementCenters);
end;

procedure TSutraElement2D_Collection.SortByElementNumber;
var
  AnElementList: TList<TSutraElement2D>;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
begin
  AnElementList := TList<TSutraElement2D>.Create;
  try
    AnElementList.Capacity := Count;
    for ElementIndex := 0 to Count - 1 do
    begin
      AnElementList.Add(nil);
    end;
    for ElementIndex := 0 to Count - 1 do
    begin
      AnElement := Items[ElementIndex];
      AnElementList[AnElement.ElementNumber] := AnElement;
    end;
    for ElementIndex := 0 to AnElementList.Count - 1 do
    begin
      AnElement := AnElementList[ElementIndex];
      AnElement.Index := ElementIndex;
    end;
  finally
    AnElementList.Free;
  end;
end;

procedure TSutraElement2D_Collection.ElementXLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  Element: TSutraElement2D;
  NodeIndex: integer;
  ANode: TSutraNode2D;
begin
  Element := Subject as TSutraElement2D;

  if Element.Nodes.Count > 0 then
  begin
    ANode := Element.Nodes[0].Node;
    LowerBoundary := ANode.x;
    UpperBoundary := ANode.x;
    for NodeIndex := 1 to Element.Nodes.Count - 1 do
    begin
      ANode := Element.Nodes[NodeIndex].Node;
      if UpperBoundary < ANode.x then
      begin
        UpperBoundary := ANode.x;
      end
      else if LowerBoundary > ANode.x then
      begin
        LowerBoundary := ANode.x;
      end;
    end;
  end
  else
  begin
    LowerBoundary := 0;
    UpperBoundary := 0;
  end;

end;

procedure TSutraElement2D_Collection.ElementYLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  Element: TSutraElement2D;
  NodeIndex: integer;
  ANode: TSutraNode2D;
begin
  Element := Subject as TSutraElement2D;

  if Element.Nodes.Count > 0 then
  begin
    ANode := Element.Nodes[0].Node;
    LowerBoundary := ANode.y;
    UpperBoundary := ANode.y;
    for NodeIndex := 1 to Element.Nodes.Count - 1 do
    begin
      ANode := Element.Nodes[NodeIndex].Node;
      if UpperBoundary < ANode.y then
      begin
        UpperBoundary := ANode.y;
      end
      else if LowerBoundary > ANode.y then
      begin
        LowerBoundary := ANode.y;
      end;
    end;
  end
  else
  begin
    LowerBoundary := 0;
    UpperBoundary := 0;
  end;

end;

function TSutraElement2D_Collection.TopContainingElement(
  APoint: TPoint2D): T2DTopCell;
var
  index: Integer;
  Element: TSutraElement2D;
  IntervalDef: TIntDefArray;
  Limits: TGridLimit;
  AList: TList;
  Location: TOneDRealArray;
  EpsilonX: Extended;
  EpsilonY: Extended;
begin
  Result.Row := -1;
  Result.Col := -1;
  if Count = 0 then
  begin
    Exit;
  end;

  if FElementIntervals = nil then
  begin
    Limits := Mesh2D.MeshLimits;
    SetLength(IntervalDef, 2);
    EpsilonX := (Limits.MaxX-Limits.MinX)/1e8;
    EpsilonY := (Limits.MaxY-Limits.MinY)/1e8;
    IntervalDef[0].LowerBoundary := Limits.MinX-EpsilonX;
    IntervalDef[0].UpperBoundary := Limits.MaxX+EpsilonX;
    IntervalDef[0].OnFindObjectInterval  := ElementXLimits;
    IntervalDef[1].LowerBoundary := Limits.MinY-EpsilonY;
    IntervalDef[1].UpperBoundary := Limits.MaxY+EpsilonY;
    IntervalDef[1].OnFindObjectInterval  := ElementYLimits;

    FElementIntervals := TRbwIntervalTree.Create(IntervalDef);
    for index := 0 to Count - 1 do
    begin
      Element := Items[index];
      FElementIntervals.Add(Element);
    end;
  end;

  AList := TList.Create;
  try
    SetLength(Location, 2);
    Location[0] := APoint.x;
    Location[1] := APoint.y;
    FElementIntervals.FindContainingObjects(Location, AList);

    for index := 0 to AList.Count - 1 do
    begin
      Element := AList[index];
      if Element.IsInside(APoint) then
      begin
        Result.Row := 0;
        Result.Col := Element.ElementNumber;
        Exit;
      end;
    end;
    for index := 0 to AList.Count - 1 do
    begin
      Element := AList[index];
      if Element.OnEdge(APoint) then
      begin
        Result.Row := 0;
        Result.Col := Element.ElementNumber;
        Exit;
      end;
    end;
  finally
    AList.Free;
  end;

//  for index := 0 to Count - 1 do
//  begin
//    Element := Items[index];
//    if Element.IsInside(APoint) then
//    begin
//      Result.Row := 0;
//      Result.Col := Element.ElementNumber;
//      Exit;
//    end;
//  end;
end;

{ TSutraMesh2D }

procedure TSutraMesh2D.Assign(Source: TPersistent);
var
  SourceMesh: TSutraMesh2D;
begin
  if Source is TSutraMesh2D then
  begin
    SourceMesh := TSutraMesh2D(Source);
    Nodes := SourceMesh.Nodes;
    Elements := SourceMesh.Elements;
    MeshGenControls := SourceMesh.MeshGenControls;
  end
  else
  begin
    inherited;
  end;
end;

function TSutraMesh2D.Bandwidth: Integer;
var
  index: Integer;
  LowNode: TSutraNode2D;
  AnElement: TSutraElement2D;
  NodeIndex: Integer;
  HighNode: TSutraNode2D;
  ANode: TSutraNode2D;
  Delta: Integer;
begin
  Result := 0;
  for index := 0 to Elements.Count -1 do
  begin
    AnElement := Elements[index];
//    if AnElement.Active then
    begin
      LowNode := AnElement.Nodes[0].Node;
      HighNode := LowNode;
      for NodeIndex := 1 to AnElement.Nodes.Count - 1 do
      begin
        ANode := AnElement.Nodes[NodeIndex].Node;
        if ANode.Number < LowNode.Number then
        begin
          LowNode := ANode;
        end;
        if ANode.Number > HighNode.Number then
        begin
          HighNode := ANode;
        end;
      end;
      Delta := HighNode.Number - LowNode.Number;
      if Delta > Result then
      begin
        Result := Delta
      end;
    end;
  end;
  result := result*2 + 1;
end;

procedure TSutraMesh2D.GetCounts(DataSet: TDataArray; var LayerCount, RowCount,
  ColCount: integer);
begin
  LayerCount := DataSet.LayerCount;
  RowCount := DataSet.RowCount;
  ColCount := DataSet.ColumnCount;
end;

procedure TSutraMesh2D.Clear;
begin
  Nodes.Clear;
  Elements.Clear;
end;

constructor TSutraMesh2D.Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
var
  LocalModel: TCustomModel;
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(Model);
//  FShouldUpdateMinMax := True;
  FMeshGenControls := TMeshGenerationControls.Create(OnInvalidateModelEvent);
  FMesh3D := ParentMesh;
  FNodeFont := TFont.Create;
  FElementFont := TFont.Create;
  FNodes := TSutraNode2D_Collection.Create(Model, ParentMesh);
  FElements := TSutraElement2D_Collection.Create(OnInvalidateModelEvent, ParentMesh);
  LocalModel := Model as TCustomModel;
  if LocalModel <> nil then
  begin
    FTopGridObserver := LocalModel.TopGridObserver;
    if LocalModel.ModelSelection in SutraSelection then
    begin
      FTopGridObserver.OnUpToDateSet := LocalModel.OnTopSutraMeshChanged;
    end;
  end
  else
  begin
    FTopGridObserver := nil;
  end;
  FDrawNodeNumbers := False;
  FDrawElementNumbers := False;
end;

procedure TSutraMesh2D.DeleteUnconnectedNodes;
var
  NodeIndex: Integer;
  RenumberingNeeded: boolean;
begin
  RenumberingNeeded := False;
  for NodeIndex := FNodes.Count - 1 downto 0 do
  begin
    if FNodes[NodeIndex].ElementCount = 0 then
    begin
      FNodes.Delete(NodeIndex);
      RenumberingNeeded := True;
    end;
  end;
  if RenumberingNeeded then
  begin
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      FNodes[NodeIndex].Number := NodeIndex;
    end;
  end;
end;

destructor TSutraMesh2D.Destroy;
begin
  FElements.Free;
  FNodes.Free;
  FElementFont.Free;
  FNodeFont.Free;
  FMeshGenControls.Free;
  inherited;
end;

procedure TSutraMesh2D.Draw(const BitMap: TPersistent);
begin
  DrawTop(BitMap, frmGoPhast.frameTopView.ZoomBox);
end;

procedure TSutraMesh2D.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  APoint: TPoint;
  ColorDataArray: TDataArray;
  StringValues : TStringList;
  Node3D: TSutraNode3D;
  ActiveNode: Boolean;
begin
  StringValues := TStringList.Create;
  try
    ColorDataArray := nil;
    if ThreeDDataSet <> nil then
    begin
      ColorDataArray := ThreeDDataSet;
    end
    else if TopDataSet <> nil then
    begin
      ColorDataArray := TopDataSet;
    end;
    if (ColorDataArray = nil) or (ColorDataArray.EvaluatedAt = eaBlocks) then
    begin
      DrawElements(StringValues, ColorDataArray, ZoomBox, BitMap);
      DrawNodes(ColorDataArray, BitMap, ZoomBox, StringValues);
    end
    else
    begin
      DrawNodes(ColorDataArray, BitMap, ZoomBox, StringValues);
      DrawElements(StringValues, ColorDataArray, ZoomBox, BitMap);
    end;

    if DrawNodeNumbers then
    begin
      for NodeIndex := 0 to Nodes.Count - 1 do
      begin
        ANode := Nodes[NodeIndex];
        ActiveNode := True;
        if (Mesh3D.MeshType = mt3D) and (Mesh3D.Elements.Count > 0) then
        begin
          Node3D := Mesh3D.NodeArray[Mesh3D.SelectedLayer, ANode.Number];
          ActiveNode := Node3D.Active;
        end;
        if ActiveNode then
        begin
          APoint := ConvertTop2D_Point(ZoomBox, ANode.Location);
          if BitMap is TBitmap32 then
          begin
            TBitmap32(BitMap).Textout(APoint.X, APoint.Y, IntToStr(ANode.DisplayNumber));
          end
          else
          begin
            (BitMap as TCanvas).Textout(APoint.X, APoint.Y, IntToStr(ANode.DisplayNumber));
          end;
        end;
      end;
    end;
  finally
    StringValues.free;
  end;

  DrawTopContours(ZoomBox, BitMap);
end;

procedure TSutraMesh2D.DrawTopContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  Contourer: TMultipleContourCreator;
begin
  if (Nodes.Count > 0) and (TopContourDataSet <> nil)
    and (TopContourDataSet.Orientation in [dsoTop, dso3D]) then
  begin
      Contourer := TMultipleContourCreator.Create;
      try
        PlotList := FTopContourPlotList;
        Contourer.DataSet := TopContourDataSet;
        Contourer.ActiveDataSet := nil;
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdTop;
        Contourer.Mesh := Self.Mesh3D;
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedLayer,
          frmGoPhast.PhastModel.ContourColorParameters, vdTop,
          frmGoPhast.PhastModel.ContourLabelSpacing,
          frmGoPhast.PhastModel.TopContoursUpToDate);
        frmGoPhast.PhastModel.TopContoursUpToDate := True;
      finally
        Contourer.Free;
      end;
  end;
end;

procedure TSutraMesh2D.EndUpdate;
begin
  inherited;
  if (FUpdateCount = 0) then
  begin
    frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;
    if Assigned(TopGridObserver) then
    begin
      TopGridObserver.UpToDate := False;
      TopGridObserver.UpToDate := True;
    end;
  end;
end;

procedure TSutraMesh2D.DrawNodes(ColorDataArray: TDataArray;
  const BitMap: TPersistent; const ZoomBox: TQRbwZoomBox2;
  StringValues: TStringList);
var
  ANode: TSutraNode2D;
  NodeIndex: Integer;
  Node3D: TSutraNode3D;
  DrawNode: Boolean;
  Layer: Integer;
  MinMax: TMinMax;
  MinMaxInitialized: Boolean;
begin
  if ColorDataArray <> nil then
  begin
    Layer := -1;
    case ColorDataArray.Orientation of
      dsoTop: Layer := 0;
      dso3D: Layer := SelectedLayer;
      else Assert(False);
    end;
  end
  else
  begin
    Layer := SelectedLayer;
  end;

  if BitMap is TBitmap32 then
  begin
    TBitmap32(BitMap).Font := NodeFont;
  end
  else
  begin
    (BitMap as TCanvas).Font := NodeFont;
  end;
  if (ColorDataArray <> nil) and (ColorDataArray.EvaluatedAt = eaNodes) then
  begin
    GetMinMax(MinMax, ColorDataArray, StringValues, MinMaxInitialized);
    ApplyLimittoMinMax(ColorDataArray, MinMax, ColorDataArray.Limits);
  end;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    ANode := Nodes[NodeIndex];
    DrawNode := True;
    if (FMesh3D.MeshType = mt3d) and (FMesh3D.Elements.Count > 0) then
    begin
      if (ColorDataArray = nil) or (ColorDataArray.Orientation = dso3D) then
      begin
        Node3D := FMesh3D.NodeArray[SelectedLayer, ANode.Number];
        DrawNode := Node3D.Active;
      end;
    end;
    if DrawNode then
    begin
      if (ColorDataArray <> nil) and (ColorDataArray.EvaluatedAt = eaNodes) then
      begin
        ANode.DrawTop(BitMap, ZoomBox, NodeDrawingChoice, ColorDataArray,
          Layer, StringValues, MinMax);
      end
      else
      begin
        ANode.DrawTop(BitMap, ZoomBox, NodeDrawingChoice, nil,
          Layer, StringValues, MinMax);
      end;
    end;
  end;
end;

procedure TSutraMesh2D.DrawElements(StringValues: TStringList;
  ColorDataArray: TDataArray; const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  ElementIndex: Integer;
  DrawElement: Boolean;
  Element3D: TSutraElement3D;
  Element2D: TSutraElement2D;
  ElementLayer: Integer;
  MinMax: TMinMax;
  MinMaxInitialized: Boolean;
begin
  if BitMap is TBitmap32 then
  begin
    TBitmap32(BitMap).Font := NodeFont;
  end
  else
  begin
    (BitMap as TCanvas).Font := NodeFont;
  end;
  ElementLayer := -1;
  if ColorDataArray <> nil then
  begin
    case ColorDataArray.Orientation of
      dsoTop: ElementLayer := 0;
      dso3D: ElementLayer := SelectedLayer;
      else Assert(False);
    end;
  end
  else
  begin
    ElementLayer := SelectedLayer;
  end;
  if ElementLayer >= FMesh3D.LayerCount then
  begin
    ElementLayer := FMesh3D.LayerCount - 1;
  end;
  if (ColorDataArray <> nil) and (ColorDataArray.EvaluatedAt = eaBlocks) then
  begin
    GetMinMax(MinMax, ColorDataArray, StringValues, MinMaxInitialized);
    ApplyLimittoMinMax(ColorDataArray, MinMax, ColorDataArray.Limits);
  end;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
  {$IFDEF DEBUG}
    try
  {$ENDIF}
    Element2D := Elements[ElementIndex];
    DrawElement := True;
    if FMesh3D.MeshType = mt3d then
    begin
      if FMesh3D.Elements.Count > 0 then
      begin
        if (ColorDataArray = nil) or (ColorDataArray.Orientation = dso3D)  then
        begin
          Element3D := FMesh3D.elementArray[ElementLayer, Element2D.ElementNumber];
          DrawElement := Element3D.Active;
        end;
      end;
    end;
    if DrawElement then
    begin
      if (ColorDataArray <> nil) and (ColorDataArray.EvaluatedAt = eaBlocks) then
      begin
        Element2D.DrawTop(BitMap, ZoomBox, ElementDrawingChoice,
          ColorDataArray, ElementLayer, StringValues, Mesh3D, MinMax);
      end
      else
      begin
        Element2D.DrawTop(BitMap, ZoomBox, ElementDrawingChoice,
          nil, ElementLayer, StringValues, Mesh3D, MinMax);
      end;
    end;
  {$IFDEF DEBUG}
    except
      ShowMessage('ElementIndex = ' + IntToStr(ElementIndex));
    end;
  {$ENDIF}
  end;
end;

function TSutraMesh2D.GetActiveElement(Index: integer): IElement;
begin
  result := Elements[Index];
end;

function TSutraMesh2D.GetActiveElementCount: integer;
begin
  result := Elements.Count;
end;

function TSutraMesh2D.GetActiveElementI2D(Index: integer): IElement2D;
begin
  result := Elements[Index];
end;

function TSutraMesh2D.GetActiveNode(Index: Integer): INode;
begin
  result := Nodes[Index];
end;

function TSutraMesh2D.GetActiveNodeCount: integer;
begin
  result := Nodes.Count;
end;

function TSutraMesh2D.GetActiveNodeI2D(Index: integer): INode2D;
begin
  result := Nodes[Index];
end;

function TSutraMesh2D.GetElementCenters: TRbwQuadTree;
begin
  result := Elements.ElementCenters;
end;

function TSutraMesh2D.GetElementOutline(Column: Integer): TElementOutline;
var
//  ACell: TModflowIrregularCell2D;
  NodeIndex: Integer;
  AnElement: TSutraElement2D;
begin
  AnElement := Elements[Column];
  result.Count := AnElement.Nodes.Count;
  for NodeIndex := 0 to AnElement.Nodes.Count -1 do
  begin
    result.Points[NodeIndex] := AnElement.Nodes[NodeIndex].Node.Location;
  end;
end;

function TSutraMesh2D.GetElementRanges: TRbwRangeTree;
begin
  result := Elements.ElementRanges;
end;

procedure TSutraMesh2D.GetElementsOnSegment(Segment: TSegment2D;
  ElementsOnSegment: TSutraElement2D_List);
var
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  EdgeIndex: Integer;
  ElementComparer: ISutraElement2DComparer;
begin
  ElementsOnSegment.Clear;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    AnElement := Elements[ElementIndex];
    for EdgeIndex := 0 to 3 do
    begin
      if Intersect(AnElement.Edge(EdgeIndex), Segment) then
      begin
        if ElementsOnSegment.IndexOf(AnElement) < 0 then
        begin
          ElementsOnSegment.Add(AnElement);
        end;
        break;
      end;
    end;
  end;

  ElementComparer := TSutraElement2DComparer.Create(
    ArcTan2(Segment[2].y - Segment[1].y, Segment[2].x - Segment[1].x),
    Segment[1]);
  try
    ElementsOnSegment.Sort(ElementComparer);
  finally
    ElementComparer := nil;
  end;
end;

function TSutraMesh2D.GetItemTopLocation(const EvalAt: TEvaluatedAt; const Column,
  Row: integer): TPoint2D;
begin
  case EvalAt of
    eaBlocks:
      begin
        result := Elements[Column].Center;
      end;
    eaNodes:
      begin
        result := Nodes[Column].Location;
      end;
    else
      Assert(False);
  end;
end;

procedure TSutraMesh2D.GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
  StringValues: TStringList; out MinMaxInitialized: Boolean);
var
  LayerCount, RowCount, ColCount: integer;
begin
  Assert(DataSet <> nil);
  Assert(Model = (DataSet.Model as TCustomModel));
  DataSet.Initialize;
  GetCounts(DataSet, LayerCount, RowCount, ColCount);
  if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
  begin
    Exit;
  end;

  MinMax.RMinPositive := 0;
  MinMaxInitialized := False;
  StringValues.Sorted := True;
  StringValues.Duplicates := dupIgnore;
  StringValues.CaseSensitive := True;
  StringValues.Capacity := LayerCount*RowCount*ColCount;
  CalculateMinMax(DataSet, MinMaxInitialized, MinMax, StringValues);
end;

function TSutraMesh2D.GetNodeRanges: TRbwRangeTree;
begin
  result := Nodes.NodeRanges;
end;

function TSutraMesh2D.GetNodesLocations: TRbwQuadTree;
begin
  result := Nodes.GetNodesLocations;
end;

procedure TSutraMesh2D.GetNodesOnSegment(Segement: TSegment2D;
  NodesOnSegment: TSutraNode2D_List);
var
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  EdgeIndex: Integer;
  IntersectPoint: TPoint2D;
  NodeIndex: Integer;
  Node1: TSutraNode2D;
  Node2: TSutraNode2D;
  NodeComparer: ISutraNode2DComparer;
  function NodeOK(Node: TSutraNode2D): boolean;
  var
    LayerIndex: Integer;
    Node3D: TSutraNode3D;
  begin
    result := NodesOnSegment.IndexOf(Node) < 0;
    if result then
    begin
      result := False;
      for LayerIndex := 0 to FMesh3D.LayerCount - 1 do
      begin
        Node3D := FMesh3D.NodeArray[LayerIndex, Node.Number];
        result := (Node3D <> nil) and Node3D.Active;
        if result then
        begin
          Exit;
        end;
      end;
    end;
  end;
begin
  NodesOnSegment.Clear;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    AnElement := Elements[ElementIndex];
    for EdgeIndex := 0 to 3 do
    begin
      if Intersect(AnElement.Edge(EdgeIndex), Segement,
        IntersectPoint.x, IntersectPoint.y) then
      begin

        NodeIndex := EdgeIndex;
        Node1 := AnElement.Nodes[NodeIndex].Node;
        Inc(NodeIndex);
        if NodeIndex >= 4 then
        begin
          NodeIndex := 0;
        end;
        Node2 := AnElement.Nodes[NodeIndex].Node;

        if Distance(Node1.Location, IntersectPoint) <
          Distance(Node2.Location, IntersectPoint) then
        begin
          if NodeOK(Node1) then
          begin
            NodesOnSegment.Add(Node1);
          end;
        end
        else
        begin
          if NodeOK(Node2) then
          begin
            NodesOnSegment.Add(Node2);
          end;
        end;
      end;
    end;
  end;

  NodeComparer := TSutraNode2DComparer.Create(
    ArcTan2(Segement[2].y - Segement[1].y, Segement[2].x - Segement[1].x),
    Segement[1]);
  try
    NodesOnSegment.Sort(NodeComparer);
  finally
    NodeComparer := nil;
  end;

end;

function TSutraMesh2D.GetSelectedLayer: integer;
begin
  result := -1;
  case Mesh3D.MeshType of
    mt2D, mtProfile: result := 0;
    mt3D:
      begin
        result := FSelectedLayer;
        if result >= Mesh3D.LayerCount then
        begin
          result := Mesh3D.LayerCount;
        end;
      end;
    else Assert(False);
  end;
end;

function TSutraMesh2D.GetShortestHorizontalBlockEdge(Layer, Row,
  Column: Integer): double;
var
  Element: TSutraElement2D;
  Node1: TPoint2D;
  NodeIndex: Integer;
  TestDistance: double;
  Node2: TPoint2D;
begin
  Element := Elements[Column];
  Node1 := Element.Nodes[0].Node.Location;
  Node2 := Element.Nodes[1].Node.Location;
  result := Distance(Node1,Node2);
  for NodeIndex := 2 to Element.Nodes.Count - 1 do
  begin
    Node1 := Node2;
    Node2 := Element.Nodes[NodeIndex].Node.Location;
    TestDistance := Distance(Node1,Node2);
    if TestDistance < result then
    begin
      result := TestDistance;
    end;
  end;
end;

function TSutraMesh2D.Is3DMesh: Boolean;
begin
  result := False;
end;

function TSutraMesh2D.MeshBox: TPolygon2D;
var
  Limits: TGridLimit;
begin
  Limits := MeshLimits;
  SetLength(result, 4);
  result[0].x := Limits.MinX;
  result[0].y := Limits.MinY;

  result[1].x := Limits.MinX;
  result[1].y := Limits.MaxY;

  result[2].x := Limits.MaxX;
  result[2].y := Limits.MaxY;

  result[3].x := Limits.MaxX;
  result[3].y := Limits.MinY;
end;

function TSutraMesh2D.MeshLimits: TGridLimit;
var
  Node: TSutraNode2D;
  NodeIndex: Integer;
begin
  result.MinX := 0;
  result.MaxX := 0;
  result.MinY := 0;
  result.MaxY := 0;
  result.MinZ := 0;
  result.MaxZ := 0;
  if Nodes.Count > 0 then
  begin
    Node := Nodes[0];
    result.MinX := Node.X;
    result.MinY := Node.Y;
    result.MaxX := Node.X;
    result.MaxY := Node.Y;
    for NodeIndex := 1 to Nodes.Count - 1 do
    begin
      Node := Nodes[NodeIndex];
//      if Node.NodeType = ntEdge then
      begin
        if Node.X > result.MaxX then
        begin
          result.MaxX := Node.X
        end;
        if Node.X < result.MinX then
        begin
          result.MinX := Node.X
        end;
        if Node.Y > result.MaxY then
        begin
          result.MaxY := Node.Y
        end;
        if Node.Y < result.MinY then
        begin
          result.MinY := Node.Y
        end;
      end;
    end;
  end;

end;

function TSutraMesh2D.MeshOutline: TPolygon2D;
var
  FirstNode: TSutraNode2D;
  NodeIndex: Integer;
  OutlineCount: Integer;
  NextNode: TSutraNode2D;
  PriorNodes: TSutraNode2D_List;
  AllInnerNodes: Boolean;
  function GetNextOutlineNode(ANode: TSutraNode2D): TSutraNode2D;
  var
    ElementIndex: Integer;
    AnElement: TSutraElement2D;
    AnotherNode: TSutraNode2D;
    NodePosition: Integer;
    function IsNextEdgeNode: boolean;
    begin
      if AllInnerNodes then
      begin
        result := (AnotherNode.SumOfAngles < 359.9)
          and (AnotherNode <> ANode)
          and (AnotherNode <> FirstNode)
          and (PriorNodes.IndexOf(AnotherNode) < 0);
      end
      else
      begin
        result := (AnotherNode.NodeType <> ntInner)
          and (AnotherNode.SumOfAngles < 359.9)
          and (AnotherNode <> ANode)
          and (AnotherNode <> FirstNode)
          and (PriorNodes.IndexOf(AnotherNode) < 0);
      end;
    end;
  begin
    result := nil;
    for ElementIndex := 0 to ANode.FElements.Count - 1 do
    begin
      AnElement := ANode.FElements[ElementIndex];
      NodePosition := AnElement.Nodes.IndexOfNode(ANode);
      if NodePosition = AnElement.Nodes.Count -1 then
      begin
        AnotherNode := AnElement.Nodes[0].Node;
      end
      else
      begin
        AnotherNode := AnElement.Nodes[NodePosition+1].Node;
      end;
      if IsNextEdgeNode then
      begin
        result := AnotherNode;
        Exit;
      end;

      if NodePosition = 0 then
      begin
        AnotherNode := AnElement.Nodes[AnElement.Nodes.Count -1].Node;
      end
      else
      begin
        AnotherNode := AnElement.Nodes[NodePosition-1].Node;
      end;
      if IsNextEdgeNode then
      begin
        result := AnotherNode;
        Exit;
      end;
    end;
  end;
begin
  SetLength(result, Nodes.Count);
  if Nodes.Count > 0 then
  begin
    PriorNodes := TSutraNode2D_List.Create;
    try

      AllInnerNodes := True;
      FirstNode := nil;
      for NodeIndex := 0 to Nodes.Count - 1 do
      begin
        if (Nodes[NodeIndex].NodeType <> ntInner)
          and (Nodes[NodeIndex].SumOfAngles < 359.9) then
        begin
          FirstNode := Nodes[NodeIndex];
          AllInnerNodes := False;
          break;
        end;
      end;
      if FirstNode = nil then
      begin

        for NodeIndex := 0 to Nodes.Count - 1 do
        begin
          if (Nodes[NodeIndex].SumOfAngles < 359.9) then
          begin
            FirstNode := Nodes[NodeIndex];
            break;
          end;
        end;
      end;
      Assert(FirstNode <> nil);
      result[0] := FirstNode.Location;
      OutlineCount := 1;
      NextNode := GetNextOutlineNode(FirstNode);
      Assert(NextNode <> nil);
      PriorNodes.Add(NextNode);
      while NextNode <> nil do
      begin
        Assert(OutlineCount < Nodes.Count);
        result[OutlineCount] := NextNode.Location;
        NextNode := GetNextOutlineNode(NextNode);
        PriorNodes.Add(NextNode);
//        if PriorNodes.Count > 4 then
//        begin
//          PriorNodes.Delete(0);
//        end;
        Inc(OutlineCount);
      end;
      SetLength(result, OutlineCount);
    finally
      PriorNodes.Free;
    end;
  end;
end;

procedure TSutraMesh2D.Renumber;
var
  NodeList: TSutraNode2D_List;
  index: Integer;
  ElementList: TSutraElement2D_List;
  AnElement: TSutraElement2D;
  Node: TSutraNodeNumber2D_Item;
  NodeIndex: integer;
begin
  Elements.InvalidateElementIntervals;
  Nodes.InvalidateNodeIntervals;

  case MeshGenControls.RenumberingAlgorithm of
    raNone: ;
    CuthillMcKee: CuthillMcKeeRenumbering.RenumberMesh(self);
    raSloanRandolph: MeshRenumbering.RenumberMesh(self);
    else Assert(False);
  end;

  NodeList := TSutraNode2D_List.Create;
  try
    NodeList.Capacity := FNodes.Count;
    for index := 0 to FNodes.Count - 1 do
    begin
      NodeList.Add(FNodes[index]);
    end;
//    NodeList.Sort(TSutraNode2DNumberComparer.Default);
    NodeList.Sort(TSutraNode2DNumberComparer.Construct(
      function(const L, R: TSutraNode2D): Integer
      begin
        result := L.Number - R.Number;
      end));
    for Index := 0 to NodeList.Count - 1 do
    begin
      NodeList.Items[index].Index := index;
      NodeList.Items[index].Number := Index;
    end;
  finally
    NodeList.Free;
  end;

  ElementList := TSutraElement2D_List.Create;
  try
    ElementList.Capacity := FElements.Count;
    for index := 0 to FElements.Count - 1 do
    begin
      ElementList.Add(FElements[index]);
    end;
//    ElementList.Sort(TSutraElement2DNumberComparer.Default);
    ElementList.Sort(TSutraElement2DNumberComparer.Construct(
      function(const L, R: TSutraElement2D): Integer
      begin
        result := L.ElementNumber - R.ElementNumber;
      end));
    for Index := 0 to ElementList.Count - 1 do
    begin
      AnElement := ElementList.Items[index];
      AnElement.Index := index;
      AnElement.ElementNumber := Index;
      for NodeIndex := 0 to AnElement.FNodes.Count - 1 do
      begin
        Node := AnElement.FNodes[NodeIndex];
        // Update stored node number.
        Node.Node := Node.Node;
      end;
    end;
  finally
    ElementList.Free;
  end;

//  FElements.Sort(TIElementComparer.Construct(
//    function(const L, R: IElement): Integer
//    begin
//      result := L.ElementNumber - R.ElementNumber;
//    end));

//  frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;

end;

procedure TSutraMesh2D.SetCorrectElementOrientation;
var
  ElementIndex: Integer;
begin
  for ElementIndex := 0 to Elements.Count -1 do
  begin
    Elements[ElementIndex].SetCorrectOrienatation;
  end;
end;

procedure TSutraMesh2D.SetDrawElementCenters(const Value: boolean);
begin
  FDrawElementCenters := Value;
end;

procedure TSutraMesh2D.SetDrawElementNumbers(const Value: boolean);
begin
  FDrawElementNumbers := Value;
end;

procedure TSutraMesh2D.SetElementDrawingChoice(const Value: TDrawingChoice);
begin
  if FElementDrawingChoice <> Value then
  begin
    FElementDrawingChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSutraMesh2D.SetElementFont(const Value: TFont);
begin
  FElementFont.Assign(Value);
end;

procedure TSutraMesh2D.SetDrawNodeNumbers(const Value: boolean);
begin
  FDrawNodeNumbers := Value;
end;

procedure TSutraMesh2D.SetElements(const Value: TSutraElement2D_Collection);
begin
  FElements.Assign(Value);
end;

procedure TSutraMesh2D.SetMeshGenControls(const Value: TMeshGenerationControls);
begin
  FMeshGenControls.Assign(Value);
end;

procedure TSutraMesh2D.SetNodeDrawingChoice(const Value: TDrawingChoice);
begin
  FNodeDrawingChoice := Value;
end;

procedure TSutraMesh2D.SetNodeFont(const Value: TFont);
begin
  FNodeFont.Assign(Value);
end;

procedure TSutraMesh2D.SetNodes(const Value: TSutraNode2D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraMesh2D.SetSelectedLayer(Value: integer);
begin
  FSelectedLayer := Value;
end;

function TSutraMesh2D.TopContainingCellOrElement(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  case EvaluatedAt of
    eaBlocks:
      begin
        result := Elements.TopContainingElement(APoint)
      end;
    eaNodes:
      begin
        result := Nodes.TopContainingCell(APoint)
      end;
    else Assert(False);
  end;
end;

{ TSutraNode3D }

procedure TSutraNode3D.Assign(Source: TPersistent);
var
  SourceNode: TSutraNode3D;
begin
  if Source is TSutraNode3D then
  begin
    SourceNode := TSutraNode3D(Source);
    Z := SourceNode.Z;
    Node2D_Number := SourceNode.Node2D_Number;
  end;
  inherited;
end;

procedure TSutraNode3D.AssignINode(Source: INode);
begin
  Number := Source.NodeNumber;
end;

constructor TSutraNode3D.Create(Collection: TCollection);
begin
  inherited;
  FActive := False;
  FElements:= TSutraElement3DList.Create;
  FActiveElements:= TSutraElement3DList.Create;
end;

function TSutraNode3D.CreatePolyhedron: TPolyhedron;
var
  LastFace, LastVertex : integer;
  AnElement : TSutraElement3D;
  Index : integer;
//  FaceCount, VertexCount : integer;
//  AllocatedMemory : integer;
begin
//  AllocatedMemory := GetHeapStatus.TotalAddrSpace;
  result := TPolyhedron.Create(FElements.Count*8,FElements.Count*12);
  result.OctTree.MaxPoints := 5;
  if Length(VI_array) < FElements.Count*8 then
  begin
    setLength(VI_array,FElements.Count*8);
    for Index := 0 to FElements.Count*8 -1 do
    begin
      VI_array[Index] := Index;
    end;
  end;

  result.OctTree.XMax := X;
  result.OctTree.XMin := X;
  result.OctTree.YMax := Y;
  result.OctTree.YMin := Y;
  result.OctTree.ZMax := Z;
  result.OctTree.ZMin := Z;

  LastFace := -1;
  LastVertex := -1;
  for Index := 0 to FElements.Count -1 do
  begin
    AnElement := FElements[Index];
    if AnElement.Active then
    begin
      AnElement.AddFacesToNodePolyhedron(self, result,LastVertex,LastFace);
    end;
  end;
  Assert(LastVertex<FElements.Count*8);
  Assert(LastFace<FElements.Count*12);
  result.SetArrayLengths(LastFace,LastVertex);
{  result.EliminateInternalFaces(LastFace, LastVertex);

  FaceCount := result.FaceCount;
  VertexCount := result.VertexCount;
  Assert((VertexCount = 8)
    or (VertexCount = 12)
    or (VertexCount = 18)
    or (VertexCount = 26));
  Assert((FaceCount = 12)
    or (FaceCount = 20)
    or (FaceCount = 32)
    or (FaceCount = 48));  }
//  AllocatedMemory := GetHeapStatus.TotalAddrSpace - AllocatedMemory;
//  ShowMessage(IntToStr(AllocatedMemory));
end;

destructor TSutraNode3D.Destroy;
begin
  FActiveElements.Free;
  FElements.Free;
  inherited;
end;

function TSutraNode3D.DisplayNumber: integer;
begin
  result := Number + 1;
end;

procedure TSutraNode3D.Draw3D;
const
  X = 0;
  Y = 1;
  Z = 2;
var
  Polyhedron: TPolyhedron;
  FaceIndex: Integer;
  AFace: TPointi;
  VertexIndex: Integer;
  Avertex: TPointd;
begin
  Polyhedron := CreatePolyhedron;
  try
    for FaceIndex := 0 to Polyhedron.FaceCount - 1 do
    begin
      AFace := Polyhedron.Faces[FaceIndex];
      glBegin(GL_POLYGON);
      try
        for VertexIndex := 0 to Length(AFace) - 1 do
        begin
          Avertex := Polyhedron.Vertices[AFace[VertexIndex]];
          glVertex3d(Avertex[X], Avertex[Y], Avertex[Z]);
        end;
      finally
        glEnd;
      end;
    end;
  finally
    Polyhedron.Free;
  end;
end;

function TSutraNode3D.GetActive: Boolean;
begin
  Result := FActive;
end;

function TSutraNode3D.GetActiveElement(Index: Integer): IElement;
begin
  result := FActiveElements[Index];
end;

function TSutraNode3D.GetActiveElementCount: integer;
begin
  result := FActiveElements.Count;
end;

function TSutraNode3D.GetBottom: Double;
begin
  result := FBottom;
end;

function TSutraNode3D.GetBoundaryNode: Boolean;
var
  ElementIndex: Integer;
  AnElement: TSutraElement3D;
begin
  result := False;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    if not AnElement.Active then
    begin
      result := True;
      Break;
    end;
  end;
end;

function TSutraNode3D.GetLocation: TPoint2D;
begin
  result.x := X;
  result.y := Y;
end;

function TSutraNode3D.GetNode2D: TSutraNode2D;
begin
  if FNode2D = nil then
  begin
    UpdateNode2D
  end;
  Result := FNode2D;
end;

function TSutraNode3D.GetNode2D_Number: integer;
begin
  if FNode2D <> nil then
  begin
    result := FNode2D.Number;
  end
  else
  begin
    result := FNode2D_Number;
  end;
end;

function TSutraNode3D.GetNodeNumber: Integer;
begin
  result := Number;
end;

function TSutraNode3D.GetNodeType: TNodeType;
begin
  result := ntInner;
end;

function TSutraNode3D.GetTop: Double;
begin
  result := FTop;
end;

function TSutraNode3D.GetVolume: Extended;
var
  Polyhedron: TPolyhedron;
  AnArea: Extended;
begin
  if FVolume = 0 then
  begin
    Polyhedron := CreatePolyhedron;
    try
      Polyhedron.GetProps(FVolume, AnArea);
    finally
      Polyhedron.Free;
    end;
  end;
  result := FVolume
end;

function TSutraNode3D.GetX: FastGeo.TFloat;
begin
  result := Node2D.X
end;

function TSutraNode3D.GetY: FastGeo.TFloat;
begin
  result := Node2D.Y
end;

function TSutraNode3D.GetZ: FastGEO.TFloat;
begin
  result := FZ;
end;

function TSutraNode3D.Model: TBaseModel;
begin
  result := (Collection as TSutraNode3D_Collection).Model;
end;

function TSutraNode3D.NodeLocation: TPoint3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function TSutraNode3D.NodeLocation2D: TPoint2D;
begin
  result := Node2D.Location;
end;

function TSutraNode3D.NodeNumber2D: Integer;
begin
  result := Node2D.Number;
end;

function TSutraNode3D.ReferenceLength: double;
begin
  result := Sqrt(Sqr(FNode2D.ReferenceLength) + Sqr(Top-Bottom));
end;

procedure TSutraNode3D.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TSutraNode3D.SetLocation(const Value: TPoint2D);
begin
  Node2D.X := Value.x;
  Node2D.Y := Value.y;
end;

procedure TSutraNode3D.SetNode2D_Number(const Value: integer);
begin
  SetIntegerProperty(FNode2D_Number, Value);
  UpdateNode2D;
end;

procedure TSutraNode3D.SetNodeNumber(Value: Integer);
begin
  Number := Value;
end;

procedure TSutraNode3D.SetZ(const Value: FastGEO.TFloat);
begin
  SetRealProperty(FZ, Value);
end;

procedure TSutraNode3D.UpdateActiveElementList;
var
  ElementIndex: Integer;
  AnElement: TSutraElement3D;
begin
  FActiveElements.Clear;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    if AnElement.Active then
    begin
      FActiveElements.Add(AnElement);
    end;
  end;
end;

procedure TSutraNode3D.UpdateNode2D;
var
  LocalModel: TCustomModel;
  Mesh: TSutraMesh2D;
begin
  LocalModel := Model as TCustomModel;
  Mesh := LocalModel.SutraMesh.Mesh2D;
  if (FNode2D_Number >= 0) and (FNode2D_Number < Mesh.Nodes.Count) then
  begin
    FNode2D := Mesh.Nodes[FNode2D_Number];
  end
  else
  begin
    FNode2D := nil;
  end;
end;
{ TSutraNode3D_Collection }

function TSutraNode3D_Collection.Add: TSutraNode3D;
begin
  result := inherited Add as TSutraNode3D
end;

constructor TSutraNode3D_Collection.Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
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
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TSutraNode3D, InvalidateModelEvent, ParentMesh);
  FStoredRotatedLocations:= TStoredLocations.Create;
end;

destructor TSutraNode3D_Collection.Destroy;
begin
  FRotatedNodeCenters.Free;
  FStoredRotatedLocations.Free;
  inherited;
end;

function TSutraNode3D_Collection.GetItem(Index: integer): TSutraNode3D;
begin
  result := inherited Items[index] as TSutraNode3D
end;

function TSutraNode3D_Collection.GetNodeLocations(Angle: Double): TRbwQuadTree;
var
  index: integer;
  ANode: TSutraNode3D;
  CenterPoint: TPoint3D;
  StoredPoint: TStoredNodeOrElement;
  LayerIndex: Integer;
  ColIndex: Integer;
  RandomIndex: integer;
begin
  if Angle <> FAngle then
  begin
    FreeAndNil(FRotatedNodeCenters);
    FAngle := Angle
  end;
  if FRotatedNodeCenters = nil then
  begin
    FStoredRotatedLocations.Clear;
    FRotatedNodeCenters := TRbwQuadTree.Create(nil);

    for LayerIndex := 0 to FMesh.LayerCount do
    begin
      for ColIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
      begin
        ANode := FMesh.NodeArray[LayerIndex,ColIndex];
        if ANode.Active then
        begin
          CenterPoint := ANode.NodeLocation;
          StoredPoint := TStoredNodeOrElement.Create(
            CenterPoint, FAngle, ColIndex, LayerIndex, ANode);
          FStoredRotatedLocations.Add(StoredPoint);
        end;
      end;
    end;

    if FStoredRotatedLocations.Count > 0 then
    begin
      StoredPoint := FStoredRotatedLocations[0];
      FRotatedNodeCenters.XMin := StoredPoint.X;
      FRotatedNodeCenters.XMax := StoredPoint.X;
      FRotatedNodeCenters.YMin := StoredPoint.Z;
      FRotatedNodeCenters.YMax := StoredPoint.Z;
      for Index := 1 to Min(100, FStoredRotatedLocations.Count - 1) do
      begin
        RandomIndex := Random(FStoredRotatedLocations.Count);
        StoredPoint := FStoredRotatedLocations[RandomIndex];
        if FRotatedNodeCenters.XMin > StoredPoint.X then
        begin
          FRotatedNodeCenters.XMin := StoredPoint.X;
        end
        else if FRotatedNodeCenters.XMax < StoredPoint.X then
        begin
          FRotatedNodeCenters.XMax := StoredPoint.X;
        end;

        if FRotatedNodeCenters.YMin > StoredPoint.Z then
        begin
          FRotatedNodeCenters.YMin := StoredPoint.Z;
        end
        else if FRotatedNodeCenters.YMax < StoredPoint.Z then
        begin
          FRotatedNodeCenters.YMax := StoredPoint.Z;
        end;
      end;
      for Index := 0 to FStoredRotatedLocations.Count - 1 do
      begin
        StoredPoint := FStoredRotatedLocations[Index];
        FRotatedNodeCenters.AddPoint(StoredPoint.X, StoredPoint.Z, StoredPoint);
      end;
    end;
  end;
  result := FRotatedNodeCenters;
end;

procedure TSutraNode3D_Collection.InvalidateStoredLocations;
begin
  FreeAndNil(FRotatedNodeCenters);
end;

{ TSutraNodeNumber3D_Collection }

function TSutraNodeNumber3D_Collection.Add: TSutraNodeNumber3D_Item;
begin
  result := inherited Add as TSutraNodeNumber3D_Item
end;

constructor TSutraNodeNumber3D_Collection.Create(Model: TBaseModel;
  Mesh3D: TSutraMesh3D);
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
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TSutraNodeNumber3D_Item, InvalidateModelEvent, Mesh3D);
end;

function TSutraNodeNumber3D_Collection.GetItem(
  Index: integer): TSutraNodeNumber3D_Item;
begin
  result := inherited Items[Index] as TSutraNodeNumber3D_Item
end;

function TSutraNodeNumber3D_Collection.IndexOfNode(Node: TSutraNode3D): Integer;
var
  NodeIndex: Integer;
begin
  result := -1;
  for NodeIndex := 0 to Count - 1 do
  begin
    if Items[NodeIndex].Node = Node then
    begin
      result := NodeIndex;
      Exit;
    end;
  end;
end;

{ TSutraElement3D }

procedure TSutraElement3D.AddFacesToNodePolyhedron(CornerNode: TSutraNode3D;
  APolyHedron: TPolyhedron; var LastVertex, LastFace: integer);
var
  AFace : TList;
  Anode : TSutraNode3D;
  function GetFaceCenterArray : TPointd;
  var
    Index : integer;
  begin
    result[X] := 0;
    result[Y] := 0;
    result[Z] := 0;
    for Index := 0 to AFace.Count -1 do
    begin
      Anode := AFace[Index];
      result[X] := result[X] + Anode.X;
      result[Y] := result[Y] + Anode.Y;
      result[Z] := result[Z] + Anode.Z;
    end;
    result[X] := result[X]/AFace.Count;
    result[Y] := result[Y]/AFace.Count;
    result[Z] := result[Z]/AFace.Count;
  end;
var
  Node0, Node1, Node2, Node3, Node4, Node5, Node6, Node7 : TSutraNode3D;
  FirstNode, SecondNode : TSutraNode3D;
  FaceList : TList;
  Index : integer;
  NodeIndex : integer;
  ElementCenter, FaceCenter, EdgeCenter, Corner : TPointd;
  ElementCenterIndex, FaceCenterIndex, EdgeCenterIndex1, EdgeCenterIndex2, CornerIndex : integer;
  temp : double;
  EpsilonX, EpsilonY, EpsilonZ : double;
  MinX, MinY, MinZ, MaxX, MaxY, MaxZ : double;
  CenterPoint : TPoint3d;
begin
  ANode := Nodes[0].Node;
  MinX := ANode.X;
  MaxX := MinX;
  MinY := ANode.Y;
  MaxY := MinY;
  MinZ := ANode.Z;
  MaxZ := MinZ;
  for Index := 1 to Nodes.Count -1 do
  begin
    ANode := Nodes[Index].Node;
    temp := ANode.X;
    if temp < MinX then
    begin
      MinX := temp
    end
    else if temp > MaxX then
    begin
      MaxX := temp;
    end;
    temp := ANode.Y;
    if temp < MinY then
    begin
      MinY := temp
    end
    else if temp > MaxY then
    begin
      MaxY := temp;
    end;
    temp := ANode.Z;
    if temp < MinZ then
    begin
      MinZ := temp
    end
    else if temp > MaxZ then
    begin
      MaxZ := temp;
    end;
  end;
  EpsilonX := (MaxX-MinX)/200000000;
  EpsilonY := (MaxY-MinY)/200000000;
  EpsilonZ := (MaxZ-MinZ)/2000000000;

  Assert(CornerNode <> nil);
  Corner[X] := CornerNode.X;
  Corner[Y] := CornerNode.Y;
  Corner[Z] := CornerNode.Z;

  CornerIndex := APolyHedron.VertexIndex(Corner,LastVertex,EpsilonX,EpsilonY,EpsilonZ);
  if CornerIndex = -1 then
  begin
    Inc(LastVertex);
    CornerIndex := LastVertex;
    APolyHedron.Vertices[CornerIndex] := Corner;
    APolyHedron.OctTree.AddPoint(Corner[X], Corner[Y], Corner[Z],
      @VI_array[LastVertex]);
  end;
  // take care of the special case where the corner is at (0,0,0);
{  else if (CornerIndex = LastVertex + 1) then
  begin
//    LastVertex := 0;
    Inc(LastVertex);
    CornerIndex := LastVertex;
    APolyHedron.Vertices[CornerIndex] := Corner;
  end;  }

  CenterPoint := CenterLocation;
  ElementCenter[X] := CenterPoint.X;
  ElementCenter[Y] := CenterPoint.Y;
  ElementCenter[Z] := CenterPoint.Z;

  ElementCenterIndex := APolyHedron.VertexIndex(ElementCenter,LastVertex,EpsilonX,EpsilonY,EpsilonZ);
  if ElementCenterIndex = -1 then
  begin
    Inc(LastVertex);
    ElementCenterIndex := LastVertex;
    APolyHedron.Vertices[ElementCenterIndex] := ElementCenter;
    APolyHedron.OctTree.AddPoint(ElementCenter[X], ElementCenter[Y], ElementCenter[Z],
      @VI_array[LastVertex]);
  end;
{  else if (ElementCenterIndex = LastVertex + 1) then
  begin
//    LastVertex := 0;
    Inc(LastVertex);
    ElementCenterIndex := LastVertex;
    APolyHedron.Vertices[ElementCenterIndex] := ElementCenter;
  end; }

  Node0 := Nodes[0].Node;
  Node1 := Nodes[1].Node;
  Node2 := Nodes[2].Node;
  Node3 := Nodes[3].Node;
  Node4 := Nodes[4].Node;
  Node5 := Nodes[5].Node;
  Node6 := Nodes[6].Node;
  Node7 := Nodes[7].Node;
  FaceList := TList.Create;
  try
    FaceList.Capacity := 6;
    for Index := 1 to 6 do
    begin
      AFace := TList.Create;
      AFace.Capacity := 4;
      FaceList.Add(AFace);
    end;

    AFace := FaceList[0];
    AFace.Add(Node0);
    AFace.Add(Node1);
    AFace.Add(Node2);
    AFace.Add(Node3);

    AFace := FaceList[1];
    AFace.Add(Node1);
    AFace.Add(Node5);
    AFace.Add(Node6);
    AFace.Add(Node2);

    AFace := FaceList[2];
    AFace.Add(Node0);
    AFace.Add(Node4);
    AFace.Add(Node5);
    AFace.Add(Node1);

    AFace := FaceList[3];
    AFace.Add(Node3);
    AFace.Add(Node7);
    AFace.Add(Node4);
    AFace.Add(Node0);

    AFace := FaceList[4];
    AFace.Add(Node2);
    AFace.Add(Node6);
    AFace.Add(Node7);
    AFace.Add(Node3);

    AFace := FaceList[5];
    AFace.Add(Node7);
    AFace.Add(Node6);
    AFace.Add(Node5);
    AFace.Add(Node4);

    for Index := 0 to FaceList.Count -1 do
    begin
      AFace := FaceList[Index];
      NodeIndex := AFace.IndexOf(CornerNode);
      if NodeIndex > -1 then
      begin
        FaceCenter := GetFaceCenterArray;

        FaceCenterIndex := APolyHedron.VertexIndex(FaceCenter,LastVertex,EpsilonX,EpsilonY,EpsilonZ);
        if FaceCenterIndex = -1 then
        begin
          Inc(LastVertex);
          FaceCenterIndex := LastVertex;
          APolyHedron.Vertices[FaceCenterIndex] := FaceCenter;
          APolyHedron.OctTree.AddPoint(FaceCenter[X], FaceCenter[Y], FaceCenter[Z],
            @VI_array[LastVertex]);
        end;
{        else if (FaceCenterIndex = LastVertex + 1) then
        begin
          Inc(LastVertex);
          FaceCenterIndex := LastVertex;
          APolyHedron.Vertices[FaceCenterIndex] := FaceCenter;
        end;  }

        case NodeIndex of
          0,1,2:
            begin
              FirstNode := AFace[NodeIndex];
              SecondNode := AFace[NodeIndex+1];
              EdgeCenter[X] := (FirstNode.X + SecondNode.X)/2;
              EdgeCenter[Y] := (FirstNode.Y + SecondNode.Y)/2;
              EdgeCenter[Z] := (FirstNode.Z + SecondNode.Z)/2;
            end;
          3:
            begin
              FirstNode := AFace[0];
              SecondNode := AFace[3];
              EdgeCenter[X] := (FirstNode.X + SecondNode.X)/2;
              EdgeCenter[Y] := (FirstNode.Y + SecondNode.Y)/2;
              EdgeCenter[Z] := (FirstNode.Z + SecondNode.Z)/2;
            end;
          else
            begin
              Assert(False);
            end;
        end;

        EdgeCenterIndex1 := APolyHedron.VertexIndex(EdgeCenter,LastVertex,EpsilonX,EpsilonY,EpsilonZ);
        if EdgeCenterIndex1 = -1 then
        begin
          Inc(LastVertex);
          EdgeCenterIndex1 := LastVertex;
          APolyHedron.Vertices[EdgeCenterIndex1] := EdgeCenter;
          APolyHedron.OctTree.AddPoint(EdgeCenter[X], EdgeCenter[Y], EdgeCenter[Z],
            @VI_array[LastVertex]);
        end;
{        else if (EdgeCenterIndex1 = LastVertex + 1) then
        begin
          Inc(LastVertex);
          EdgeCenterIndex1 := LastVertex;
          APolyHedron.Vertices[EdgeCenterIndex1] := EdgeCenter;
        end; }

        Inc(LastFace);
        APolyHedron.FaceValue[LastFace,0] := EdgeCenterIndex1;
        APolyHedron.FaceValue[LastFace,1] := ElementCenterIndex;
        APolyHedron.FaceValue[LastFace,2] := FaceCenterIndex;

        case NodeIndex of
          0:
            begin
              FirstNode := AFace[0];
              SecondNode := AFace[3];
              EdgeCenter[X] := (FirstNode.X + SecondNode.X)/2;
              EdgeCenter[Y] := (FirstNode.Y + SecondNode.Y)/2;
              EdgeCenter[Z] := (FirstNode.Z + SecondNode.Z)/2;
            end;
          1,2,3:
            begin
              FirstNode := AFace[NodeIndex];
              SecondNode := AFace[NodeIndex-1];
              EdgeCenter[X] := (FirstNode.X + SecondNode.X)/2;
              EdgeCenter[Y] := (FirstNode.Y + SecondNode.Y)/2;
              EdgeCenter[Z] := (FirstNode.Z + SecondNode.Z)/2;
            end;
          else
            begin
              Assert(False);
            end;
        end;

        EdgeCenterIndex2 := APolyHedron.VertexIndex(EdgeCenter,LastVertex,EpsilonX,EpsilonY,EpsilonZ);
        if EdgeCenterIndex2 = -1 then
        begin
          Inc(LastVertex);
          EdgeCenterIndex2 := LastVertex;
          APolyHedron.Vertices[EdgeCenterIndex2] := EdgeCenter;
          APolyHedron.OctTree.AddPoint(EdgeCenter[X], EdgeCenter[Y], EdgeCenter[Z],
            @VI_array[LastVertex]);
        end;
{        else if (EdgeCenterIndex2 = LastVertex + 1) then
        begin
          Inc(LastVertex);
          EdgeCenterIndex2 := LastVertex;
          APolyHedron.Vertices[EdgeCenterIndex2] := EdgeCenter;
        end; }

        Inc(LastFace);
        APolyHedron.FaceValue[LastFace,0] := EdgeCenterIndex2;
        APolyHedron.FaceValue[LastFace,1] := FaceCenterIndex;
        APolyHedron.FaceValue[LastFace,2] := ElementCenterIndex;

        Inc(LastFace);
        APolyHedron.FaceValue[LastFace,0] := EdgeCenterIndex2;
        APolyHedron.FaceValue[LastFace,1] := EdgeCenterIndex1;
        APolyHedron.FaceValue[LastFace,2] := FaceCenterIndex;

        Inc(LastFace);
        APolyHedron.FaceValue[LastFace,0] := EdgeCenterIndex1;
        APolyHedron.FaceValue[LastFace,1] := EdgeCenterIndex2;
        APolyHedron.FaceValue[LastFace,2] := CornerIndex;
      end;
    end;
  finally
    for Index := 0 to FaceList.Count -1 do
    begin
      TList(FaceList[Index]).Free;
    end;
    FaceList.Free;
  end;

end;

procedure TSutraElement3D.Assign(Source: TPersistent);
var
  SourceElement: TSutraElement3D;
begin
  if Source is TSutraElement3D then
  begin
    SourceElement := TSutraElement3D(Source);
    Nodes := SourceElement.Nodes;
  end;
  inherited;
end;

procedure TSutraElement3D.AssignIElement(Source: IElement);
var
  NodeIndex: Integer;
  NodeItem: TSutraNodeNumber3D_Item;
  Node: INode;
begin
  Nodes.Clear;
  Nodes.Capacity := Source.NodeCount;
  for NodeIndex := 0 to Source.NodeCount - 1 do
  begin
    NodeItem := Nodes.Add;
    Node := Source.Nodes[NodeIndex];
    NodeItem.NodeNumber := Node.NodeNumber;
  end;
  ElementNumber := Source.ElementNumber;
end;

function Average2Point3D(const Point1, Point2: TPoint3D): TPoint3D;
begin
  Result.x := (Point1.x + Point2.x)/2;
  Result.y := (Point1.y + Point2.y)/2;
  Result.z := (Point1.z + Point2.z)/2;
end;

function Average4Point3D(const Point1, Point2, Point3, Point4: TPoint3D): TPoint3D;
begin
  Result.x := (Point1.x + Point2.x + Point3.x + Point4.x)/4;
  Result.y := (Point1.y + Point2.y + Point3.y + Point4.y)/4;
  Result.z := (Point1.z + Point2.z + Point3.z + Point4.z)/4;
end;

function QuadPairsToPolygon(QP: TQuadPair3DList; Angle: Double;
  out PerpendicularLimit: TLimits): TPolygon2D;
var
  Projection: array of TQuadPair2D;
  P1, Old_resultP, resultP: TGpcPolygonClass;
  QuadIndex: Integer;
  PointIndex: Integer;
  PointDistance: Extended;
  PointAngle: Extended;
  ListIndex: Integer;
  APoint: TPoint3D;
  QuadPair: TQuadPair3D;
  NodeIndex: Integer;
  Count: Integer;
  PerpendicularDistance: Extended;
  FoundFirst: Boolean;
//  MinX, MaxX, MinY, MaxY: double;
  Epsilon: double;
  Index: Integer;
  ContourIndex: Integer;
  Area, NewArea: double;
  PriorArea: double;
  PriorAreaFound: Boolean;
  PriorContourCount: Integer;
  MinX, MinY, MaxX, MaxY: double;
//  VertexIndex: integer;
//  AVertex: TPoint2d;
begin
  Assert(QP.Count > 0);
  Old_resultP := nil;
  P1 := TGpcPolygonClass.Create;
//  P2 := TGpcPolygonClass.Create;
  try
    // Putting all the quadrilaterals into one polygon doesn't work
    // for some SUTRA models.
//    P1.NumberOfContours := 6;
    P1.NumberOfContours := 1;
    P1.VertexCount[0] := 4;
//    P1.VertexCount[1] := 4;
//    P1.VertexCount[2] := 4;
//    P1.VertexCount[3] := 4;
//    P1.VertexCount[4] := 4;
//    P1.VertexCount[5] := 4;
    Old_resultP := TGpcPolygonClass.Create;
    resultP := Old_resultP;

    MinX := 0;
    MaxX := 0;
    MinY := 0;
    MaxY := 0;
    FoundFirst := False;
    SetLength(Projection, QP.Count);
    for ListIndex := 0 to QP.Count - 1 do
    begin
      QuadPair := QP[ListIndex];
      for QuadIndex := 0 to 1 do
      begin
        for PointIndex := 1 to 4 do
        begin
          Projection[ListIndex,QuadIndex,PointIndex].y :=
            QuadPair[QuadIndex,PointIndex].z;
          APoint := QuadPair[QuadIndex,PointIndex];
          PointDistance := Sqrt(Sqr(APoint.x) + Sqr(APoint.y));
          PointAngle := ArcTan2(APoint.y, APoint.x);
          Projection[ListIndex,QuadIndex,PointIndex].x :=
            Cos(PointAngle-Angle)*PointDistance;
          PerpendicularDistance := Sin(PointAngle-Angle)*PointDistance;
          if FoundFirst then
          begin
            if PerpendicularDistance > PerpendicularLimit.UpperLimit then
            begin
              PerpendicularLimit.UpperLimit := PerpendicularDistance;
            end;
            if PerpendicularDistance < PerpendicularLimit.LowerLimit then
            begin
              PerpendicularLimit.LowerLimit := PerpendicularDistance;
            end;
            if MinX > Projection[ListIndex,QuadIndex,PointIndex].x then
            begin
              MinX := Projection[ListIndex,QuadIndex,PointIndex].x;
            end;
            if MaxX < Projection[ListIndex,QuadIndex,PointIndex].x then
            begin
              MaxX := Projection[ListIndex,QuadIndex,PointIndex].x;
            end;
            if MinY > Projection[ListIndex,QuadIndex,PointIndex].y then
            begin
              MinY := Projection[ListIndex,QuadIndex,PointIndex].y;
            end;
            if MaxY < Projection[ListIndex,QuadIndex,PointIndex].y then
            begin
              MaxY := Projection[ListIndex,QuadIndex,PointIndex].y;
            end;
          end
          else
          begin
            FoundFirst := True;
            PerpendicularLimit.UpperLimit := PerpendicularDistance;
            PerpendicularLimit.LowerLimit := PerpendicularDistance;
            MinX := Projection[ListIndex,QuadIndex,PointIndex].x;
            MaxX := Projection[ListIndex,QuadIndex,PointIndex].x;
            MinY := Projection[ListIndex,QuadIndex,PointIndex].y;
            MaxY := Projection[ListIndex,QuadIndex,PointIndex].y;
          end;
        end;
      end;
    end;

//    Epsilon := Sqr(Max(MaxX-MinX, MaxY-MinY)/10000000);
//    Epsilon := (MaxX-MinX)*(MaxY-MinY)/10000;
//    Epsilon := (MaxX-MinX)*(MaxY-MinY)/50000;
    Epsilon := (MaxX-MinX)*(MaxY-MinY)/100000;
//    P2.NumberOfContours := 1;
//    P2.VertexCount[0] := 4;

//    AVertex.X := MinX;
//    AVertex.Y := MinY;
//    P2.Vertices[0,0] := AVertex;
//    AVertex.Y := MaxY;
//    P2.Vertices[0,1] := AVertex;
//    AVertex.X := MaxX;
//    P2.Vertices[0,2] := AVertex;
//    AVertex.Y := MinY;
//    P2.Vertices[0,3] := AVertex;

//    Epsilon := 0;

    for ListIndex := 0 to QP.Count - 1 do
    begin

      P1.Vertices[0,0] := Projection[ListIndex,0,1];
      P1.Vertices[0,1] := Projection[ListIndex,0,2];
      P1.Vertices[0,2] := Projection[ListIndex,0,3];
      P1.Vertices[0,3] := Projection[ListIndex,0,4];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

//      P1.Vertices[1,0] := Projection[ListIndex,1,1];
//      P1.Vertices[1,1] := Projection[ListIndex,1,2];
//      P1.Vertices[1,2] := Projection[ListIndex,1,3];
//      P1.Vertices[1,3] := Projection[ListIndex,1,4];
      P1.Vertices[0,0] := Projection[ListIndex,1,1];
      P1.Vertices[0,1] := Projection[ListIndex,1,2];
      P1.Vertices[0,2] := Projection[ListIndex,1,3];
      P1.Vertices[0,3] := Projection[ListIndex,1,4];
//
      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

      P1.Vertices[0,0] := Projection[ListIndex,0,1];
      P1.Vertices[0,1] := Projection[ListIndex,0,2];
      P1.Vertices[0,2] := Projection[ListIndex,1,2];
      P1.Vertices[0,3] := Projection[ListIndex,1,1];
//      P1.Vertices[2,0] := Projection[ListIndex,0,1];
//      P1.Vertices[2,1] := Projection[ListIndex,0,2];
//      P1.Vertices[2,2] := Projection[ListIndex,1,2];
//      P1.Vertices[2,3] := Projection[ListIndex,1,1];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;
//
      P1.Vertices[0,0] := Projection[ListIndex,0,2];
      P1.Vertices[0,1] := Projection[ListIndex,0,3];
      P1.Vertices[0,2] := Projection[ListIndex,1,3];
      P1.Vertices[0,3] := Projection[ListIndex,1,2];
//      P1.Vertices[3,0] := Projection[ListIndex,0,2];
//      P1.Vertices[3,1] := Projection[ListIndex,0,3];
//      P1.Vertices[3,2] := Projection[ListIndex,1,3];
//      P1.Vertices[3,3] := Projection[ListIndex,1,2];

      if (Abs(P1.ContourArea(0)) > Epsilon) then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

//      P1.Vertices[4,0] := Projection[ListIndex,0,3];
//      P1.Vertices[4,1] := Projection[ListIndex,0,4];
//      P1.Vertices[4,2] := Projection[ListIndex,1,4];
//      P1.Vertices[4,3] := Projection[ListIndex,1,3];
      P1.Vertices[0,0] := Projection[ListIndex,0,3];
      P1.Vertices[0,1] := Projection[ListIndex,0,4];
      P1.Vertices[0,2] := Projection[ListIndex,1,4];
      P1.Vertices[0,3] := Projection[ListIndex,1,3];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

//      P1.Vertices[5,0] := Projection[ListIndex,0,4];
//      P1.Vertices[5,1] := Projection[ListIndex,0,1];
//      P1.Vertices[5,2] := Projection[ListIndex,1,1];
//      P1.Vertices[5,3] := Projection[ListIndex,1,4];
      P1.Vertices[0,0] := Projection[ListIndex,0,4];
      P1.Vertices[0,1] := Projection[ListIndex,0,1];
      P1.Vertices[0,2] := Projection[ListIndex,1,1];
      P1.Vertices[0,3] := Projection[ListIndex,1,4];

      if (Abs(P1.ContourArea(0)) > Epsilon)
//        or (Abs(P1.ContourArea(1)) > Epsilon)
//        or (Abs(P1.ContourArea(2)) > Epsilon)
//        or (Abs(P1.ContourArea(3)) > Epsilon)
//        or (Abs(P1.ContourArea(4)) > Epsilon)
//        or (Abs(P1.ContourArea(5)) > Epsilon)
        then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;
    end;

    P1.NumberOfContours := 0;

    Count := 0;
    PriorContourCount := Old_resultP.NumberOfContours;
    while Old_resultP.NumberOfContours > 1 do
    begin
      resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
      if resultP.NumberOfContours > Old_resultP.NumberOfContours  then
      begin
        resultP.Free;
        resultP := Old_resultP;
        break;
      end;
      Old_resultP.Free;
      Old_resultP := resultP;
      Inc(Count);
      if (Count > 100) then
      begin
        Break;
      end;
      if Old_resultP.NumberOfContours = PriorContourCount then
      begin
        break;
      end;
      PriorContourCount := Old_resultP.NumberOfContours;
    end;

    Area := 0;
    PriorArea := 0;
    ContourIndex := 0;
    PriorAreaFound := False;
    for Index := 0 to resultP.NumberOfContours - 1 do
    begin
      if not resultP.Holes[Index] then
      begin
        if (Not PriorAreaFound) then
        begin
          Area := Abs(resultP.ContourArea(Index));
          if Area > 0 then
          begin
            PriorAreaFound := True;
            ContourIndex := Index
          end;
        end
        else
        begin
          NewArea := Abs(resultP.ContourArea(Index));
          if Area < NewArea then
          begin
            PriorArea := Area;
            Area := NewArea;
            ContourIndex := Index;
          end;
        end;
      end;
    end;

    if PriorArea > 0 then
    begin
      Assert(PriorArea/Area < 1e-8, 'Area = ' + FloatToStr(Area));
    end;

    if resultP.NumberOfContours > 0 then
    begin
      SetLength(Result, resultP.VertexCount[ContourIndex]);
      for NodeIndex := 0 to resultP.VertexCount[ContourIndex] - 1 do
      begin
        Result[NodeIndex] := resultP.Vertices[ContourIndex,NodeIndex];
      end;
    end
    else
    begin
      SetLength(Result, 0);
    end;
  finally
    P1.Free;
//    P2.Free;
    Old_resultP.Free;
  end;



end;

function TSutraElement3D.CellSection(Node: TSutraNode3D): TQuadPair3D;
var
  NodePostion: Integer;
  LayerAdd: Integer;
  NextNode: Integer;
  PriorNode: Integer;
begin
  NodePostion := Nodes.IndexOfNode(Node);
  Assert(NodePostion >= 0);
  Assert(NodePostion <= 7);
  if NodePostion >= 4 then
  begin
    LayerAdd := 4;
    Dec(NodePostion, 4);
  end
  else
  begin
    LayerAdd := 0;
  end;

  if NodePostion = 3 then
  begin
    NextNode := 0;
  end
  else
  begin
    NextNode := NodePostion + 1;
  end;

  if NodePostion = 0 then
  begin
    PriorNode := 3;
  end
  else
  begin
    PriorNode := NodePostion - 1;
  end;

  Result[0][1] := Node.NodeLocation;
  Result[0][2] := Average2Point3D(
    Node.NodeLocation,
    Nodes[LayerAdd+NextNode].Node.NodeLocation);
  Result[0][3] := Average4Point3D(
    Nodes[LayerAdd].Node.NodeLocation,
    Nodes[LayerAdd+1].Node.NodeLocation,
    Nodes[LayerAdd+2].Node.NodeLocation,
    Nodes[LayerAdd+3].Node.NodeLocation);
  Result[0][4] := Average2Point3D(
    Node.NodeLocation,
    Nodes[LayerAdd+PriorNode].Node.NodeLocation);

  Result[1][1] := Nodes[4-LayerAdd+NodePostion].Node.NodeLocation;
  Result[1][2] := Average2Point3D(
    Nodes[4-LayerAdd+NodePostion].Node.NodeLocation,
    Nodes[4-LayerAdd+NextNode].Node.NodeLocation);
  Result[1][3] := Average4Point3D(
    Nodes[4-LayerAdd].Node.NodeLocation,
    Nodes[4-LayerAdd+1].Node.NodeLocation,
    Nodes[4-LayerAdd+2].Node.NodeLocation,
    Nodes[4-LayerAdd+3].Node.NodeLocation);
  Result[1][4] := Average2Point3D(
    Nodes[4-LayerAdd+NodePostion].Node.NodeLocation,
    Nodes[4-LayerAdd+PriorNode].Node.NodeLocation);

  Result[1][1] := Average2Point3D(Result[0][1], Result[1][1]);
  Result[1][2] := Average2Point3D(Result[0][2], Result[1][2]);
  Result[1][3] := Average2Point3D(Result[0][3], Result[1][3]);
  Result[1][4] := Average2Point3D(Result[0][4], Result[1][4]);
end;

function TSutraElement3D.CenterElevation: double;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  result := 0;
  for NodeIndex := 0 to 7 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result := result + ANode.Z;
  end;
  result := result/8;
end;

constructor TSutraElement3D.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TSutraNodeNumber3D_Collection.Create(Model,
    (Collection as TSutraElement3D_Collection).FMesh);
  FActiveNodes := TSutraNode3D_List.Create;
end;

destructor TSutraElement3D.Destroy;
begin
  FActiveNodes.Free;
  FNodes.Free;
  inherited;
end;

procedure TSutraElement3D.Draw3D;
var
  EShape: TQuadPair3D;
begin
  EShape := ElementShape;
  glBegin(GL_POLYGON);
  try
    glVertex3d(EShape[0,1].X, EShape[0,1].Y, EShape[0,1].Z);
    glVertex3d(EShape[0,2].X, EShape[0,2].Y, EShape[0,2].Z);
    glVertex3d(EShape[0,3].X, EShape[0,3].Y, EShape[0,3].Z);
    glVertex3d(EShape[0,4].X, EShape[0,4].Y, EShape[0,4].Z);
  finally
    glEnd;
  end;

  glBegin(GL_POLYGON);
  try
    glVertex3d(EShape[1,1].X, EShape[1,1].Y, EShape[1,1].Z);
    glVertex3d(EShape[1,2].X, EShape[1,2].Y, EShape[1,2].Z);
    glVertex3d(EShape[1,3].X, EShape[1,3].Y, EShape[1,3].Z);
    glVertex3d(EShape[1,4].X, EShape[1,4].Y, EShape[1,4].Z);
  finally
    glEnd;
  end;

  glBegin(GL_POLYGON);
  try
    glVertex3d(EShape[0,1].X, EShape[0,1].Y, EShape[0,1].Z);
    glVertex3d(EShape[1,1].X, EShape[1,1].Y, EShape[1,1].Z);
    glVertex3d(EShape[1,2].X, EShape[1,2].Y, EShape[1,2].Z);
    glVertex3d(EShape[0,2].X, EShape[0,2].Y, EShape[0,2].Z);
  finally
    glEnd;
  end;

  glBegin(GL_POLYGON);
  try
    glVertex3d(EShape[0,2].X, EShape[0,2].Y, EShape[0,2].Z);
    glVertex3d(EShape[1,2].X, EShape[1,2].Y, EShape[1,2].Z);
    glVertex3d(EShape[1,3].X, EShape[1,3].Y, EShape[1,3].Z);
    glVertex3d(EShape[0,3].X, EShape[0,3].Y, EShape[0,3].Z);
  finally
    glEnd;
  end;

  glBegin(GL_POLYGON);
  try
    glVertex3d(EShape[0,3].X, EShape[0,3].Y, EShape[0,3].Z);
    glVertex3d(EShape[1,3].X, EShape[1,3].Y, EShape[1,3].Z);
    glVertex3d(EShape[1,4].X, EShape[1,4].Y, EShape[1,4].Z);
    glVertex3d(EShape[0,4].X, EShape[0,4].Y, EShape[0,4].Z);
  finally
    glEnd;
  end;

  glBegin(GL_POLYGON);
  try
    glVertex3d(EShape[0,4].X, EShape[0,4].Y, EShape[0,4].Z);
    glVertex3d(EShape[1,4].X, EShape[1,4].Y, EShape[1,4].Z);
    glVertex3d(EShape[1,1].X, EShape[1,1].Y, EShape[1,1].Z);
    glVertex3d(EShape[0,1].X, EShape[0,1].Y, EShape[0,1].Z);
  finally
    glEnd;
  end;
end;

function TSutraElement3D.ElementNumber2D: Integer;
begin
  result := Element2D.ElementNumber;
end;

function TSutraElement3D.ElementShape: TQuadPair3D;
begin
  Result[0][1] := Nodes[0].Node.NodeLocation;
  Result[0][2] := Nodes[1].Node.NodeLocation;
  Result[0][3] := Nodes[2].Node.NodeLocation;
  Result[0][4] := Nodes[3].Node.NodeLocation;
  Result[1][1] := Nodes[4].Node.NodeLocation;
  Result[1][2] := Nodes[5].Node.NodeLocation;
  Result[1][3] := Nodes[6].Node.NodeLocation;
  Result[1][4] := Nodes[7].Node.NodeLocation;
end;

function TSutraElement3D.GetActive: Boolean;
begin
  Result := FActive;
end;

function TSutraElement3D.GetActiveNode(Index: Integer): INode;
begin
  result := FActiveNodes[Index];
end;

function TSutraElement3D.GetActiveNodeCount: integer;
begin
  result := FActiveNodes.Count;
end;

function TSutraElement3D.GetCenterLocation: TPoint3d;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  Result.x := 0;
  Result.y := 0;
  Result.z := 0;
  for NodeIndex := 0 to 7 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result.x := result.x + ANode.X;
    result.y := result.y + ANode.y;
    result.z := result.z + ANode.z;
  end;
  result.x := result.x/8;
  result.y := result.y/8;
  result.z := result.z/8;
end;

function TSutraElement3D.GetLayer: Integer;
begin
  result := FLayer;
end;

function TSutraElement3D.GetNodeI(Index: integer): INode3D;
begin
  result := FActiveNodes[Index];
end;

function TSutraElement3D.GetNodeLocation(Index: integer): TPoint3D;
begin
  result := FActiveNodes[Index].NodeLocation;
end;

function TSutraElement3D.CreatePolyhedron: TPolyhedron;
var
  Index: integer;
  Node1: TSutraNode3D;
  Node0: TSutraNode3D;
  Node2: TSutraNode3D;
  Node3: TSutraNode3D;
  Node4: TSutraNode3D;
  Node5: TSutraNode3D;
  Node6: TSutraNode3D;
  Node7: TSutraNode3D;
begin
  result := TPolyhedron.Create(14, 24);

  for Index := 0 to 7 do
  begin
    Node1 := Nodes[Index].Node;
    result.VertexValue[Index,X] := Node1.X;
    result.VertexValue[Index,Y] := Node1.Y;
    result.VertexValue[Index,Z] := Node1.Z;
  end;

  Node0 := Nodes[0].Node;
  Node1 := Nodes[1].Node;
  Node2 := Nodes[2].Node;
  Node3 := Nodes[3].Node;
  Node4 := Nodes[4].Node;
  Node5 := Nodes[5].Node;
  Node6 := Nodes[6].Node;
  Node7 := Nodes[7].Node;

  result.VertexValue[8,X] := (Node0.X + Node1.X + Node2.X + Node3.X)/4;
  result.VertexValue[8,Y] := (Node0.Y + Node1.Y + Node2.Y + Node3.Y)/4;
  result.VertexValue[8,Z] := (Node0.Z + Node1.Z + Node2.Z + Node3.Z)/4;

  result.VertexValue[9,X] := (Node0.X + Node1.X + Node4.X + Node5.X)/4;
  result.VertexValue[9,Y] := (Node0.Y + Node1.Y + Node4.Y + Node5.Y)/4;
  result.VertexValue[9,Z] := (Node0.Z + Node1.Z + Node4.Z + Node5.Z)/4;

  result.VertexValue[10,X] := (Node1.X + Node2.X + Node5.X + Node6.X)/4;
  result.VertexValue[10,Y] := (Node1.Y + Node2.Y + Node5.Y + Node6.Y)/4;
  result.VertexValue[10,Z] := (Node1.Z + Node2.Z + Node5.Z + Node6.Z)/4;

  result.VertexValue[11,X] := (Node4.X + Node5.X + Node6.X + Node7.X)/4;
  result.VertexValue[11,Y] := (Node4.Y + Node5.Y + Node6.Y + Node7.Y)/4;
  result.VertexValue[11,Z] := (Node4.Z + Node5.Z + Node6.Z + Node7.Z)/4;

  result.VertexValue[12,X] := (Node0.X + Node3.X + Node4.X + Node7.X)/4;
  result.VertexValue[12,Y] := (Node0.Y + Node3.Y + Node4.Y + Node7.Y)/4;
  result.VertexValue[12,Z] := (Node0.Z + Node3.Z + Node4.Z + Node7.Z)/4;

  result.VertexValue[13,X] := (Node2.X + Node3.X + Node6.X + Node7.X)/4;
  result.VertexValue[13,Y] := (Node2.Y + Node3.Y + Node6.Y + Node7.Y)/4;
  result.VertexValue[13,Z] := (Node2.Z + Node3.Z + Node6.Z + Node7.Z)/4;

  result.FaceValue[0,0] := 0;
  result.FaceValue[0,1] := 8;
  result.FaceValue[0,2] := 1;

  result.FaceValue[1,0] := 1;
  result.FaceValue[1,1] := 8;
  result.FaceValue[1,2] := 2;

  result.FaceValue[2,0] := 2;
  result.FaceValue[2,1] := 8;
  result.FaceValue[2,2] := 3;

  result.FaceValue[3,0] := 3;
  result.FaceValue[3,1] := 8;
  result.FaceValue[3,2] := 0;


  result.FaceValue[4,0] := 1;
  result.FaceValue[4,1] := 9;
  result.FaceValue[4,2] := 0;

  result.FaceValue[5,0] := 0;
  result.FaceValue[5,1] := 9;
  result.FaceValue[5,2] := 4;

  result.FaceValue[6,0] := 4;
  result.FaceValue[6,1] := 9;
  result.FaceValue[6,2] := 5;

  result.FaceValue[7,0] := 5;
  result.FaceValue[7,1] := 9;
  result.FaceValue[7,2] := 1;


  result.FaceValue[8,0] := 2;
  result.FaceValue[8,1] := 10;
  result.FaceValue[8,2] := 1;

  result.FaceValue[9,0] := 6;
  result.FaceValue[9,1] := 10;
  result.FaceValue[9,2] := 2;

  result.FaceValue[10,0] := 5;
  result.FaceValue[10,1] := 10;
  result.FaceValue[10,2] := 6;

  result.FaceValue[11,0] := 1;
  result.FaceValue[11,1] := 10;
  result.FaceValue[11,2] := 5;


  result.FaceValue[12,0] := 7;
  result.FaceValue[12,1] := 11;
  result.FaceValue[12,2] := 6;

  result.FaceValue[13,0] := 4;
  result.FaceValue[13,1] := 11;
  result.FaceValue[13,2] := 7;

  result.FaceValue[14,0] := 5;
  result.FaceValue[14,1] := 11;
  result.FaceValue[14,2] := 4;

  result.FaceValue[15,0] := 6;
  result.FaceValue[15,1] := 11;
  result.FaceValue[15,2] := 5;


  result.FaceValue[16,0] := 3;
  result.FaceValue[16,1] := 12;
  result.FaceValue[16,2] := 2;

  result.FaceValue[17,0] := 2;
  result.FaceValue[17,1] := 12;
  result.FaceValue[17,2] := 6;

  result.FaceValue[18,0] := 6;
  result.FaceValue[18,1] := 12;
  result.FaceValue[18,2] := 7;

  result.FaceValue[19,0] := 7;
  result.FaceValue[19,1] := 12;
  result.FaceValue[19,2] := 3;


  result.FaceValue[20,0] := 0;
  result.FaceValue[20,1] := 13;
  result.FaceValue[20,2] := 3;

  result.FaceValue[21,0] := 3;
  result.FaceValue[21,1] := 13;
  result.FaceValue[21,2] := 7;

  result.FaceValue[22,0] := 7;
  result.FaceValue[22,1] := 13;
  result.FaceValue[22,2] := 4;

  result.FaceValue[23,0] := 4;
  result.FaceValue[23,1] := 13;
  result.FaceValue[23,2] := 0;

end;

function TSutraElement3D.GetVolume: Extended;
var
  APolyhdron: TPolyhedron;
  AnArea: Extended;
begin
  if FVolume = 0 then
  begin
    APolyhdron := CreatePolyhedron;
    try
      APolyhdron.GetProps(FVolume, AnArea);
    finally
      APolyhdron.Free;
    end;
  end;
  result := FVolume;
end;

function TSutraElement3D.LowerElevation: double;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  result := 0;
  for NodeIndex := 4 to 7 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result := result + ANode.Z;
  end;
  result := result/4;
end;

function TSutraElement3D.Model: TBaseModel;
begin
  result := (Collection as TSutraElement3D_Collection).Model;
end;

function TSutraElement3D.ReferenceLength: double;
begin
  result := Sqrt(Sqr(FElement2D.ReferenceLength)
    + Sqr(UpperElevation-LowerElevation));
end;

procedure TSutraElement3D.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TSutraElement3D.SetNodes(const Value: TSutraNodeNumber3D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraElement3D.UpdateActiveNodeList;
var
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  FActiveNodes.Clear;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    if Node.Active then
    begin
      FActiveNodes.Add(Node);
    end;
  end;
end;

procedure TSutraElement3D.UpdateNodes;
var
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    Node.FElements.Add(Self);
  end;
end;

function TSutraElement3D.UpperElevation: double;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  result := 0;
  for NodeIndex := 0 to 3 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result := result + ANode.Z;
  end;
  result := result/4;
end;
{ TSutraMesh3D }

procedure TSutraMesh3D.Assign(Source: TPersistent);
var
  SouceMesh: TSutraMesh3D;
begin
  if Source is TSutraMesh3D then
  begin
    SouceMesh := TSutraMesh3D(Source);
//    Nodes := SouceMesh.Nodes;
//    Elements := SouceMesh.Elements;
    Mesh2D := SouceMesh.Mesh2D;
    ElementDrawingChoice := SouceMesh.ElementDrawingChoice;
    NodeDrawingChoice := SouceMesh.NodeDrawingChoice;
    SelectedLayer := SouceMesh.SelectedLayer;
    CrossSection := SouceMesh.CrossSection;
  end
  else
  begin
    inherited;
  end;
end;

function TSutraMesh3D.Bandwidth: Integer;
var
  index: Integer;
  LowNode: TSutraNode3D;
  AnElement: TSutraElement3D;
  NodeIndex: Integer;
  HighNode: TSutraNode3D;
  ANode: TSutraNode3D;
  Delta: Integer;
begin
  if MeshType <> mt3D then
  begin
    result := Mesh2D.Bandwidth;
    Exit;
  end;
  Result := 0;
  for index := 0 to Elements.Count -1 do
  begin
    AnElement := Elements[index];
    if AnElement.Active then
    begin
      LowNode := AnElement.Nodes[0].Node;
      HighNode := LowNode;
      for NodeIndex := 1 to AnElement.Nodes.Count - 1 do
      begin
        ANode := AnElement.Nodes[NodeIndex].Node;
        if ANode.Number < LowNode.Number then
        begin
          LowNode := ANode;
        end;
        if ANode.Number > HighNode.Number then
        begin
          HighNode := ANode;
        end;
      end;
      Delta := HighNode.Number - LowNode.Number;
      if Delta > Result then
      begin
        Result := Delta
      end;
    end;
  end;
  result := result*2 + 1;
end;

procedure TSutraMesh3D.CheckUpdateElevations;
begin
  if ElevationsNeedUpdating then
  begin
    ElevationsNeedUpdating := False;
    ThreeDGridObserver.UpToDate := True;
    UpdateElevations;
  end;
end;

procedure TSutraMesh3D.InvalidatePolygons;
begin
  FStoredNodePolygons := nil;
  FStoredBlockPolygons := nil;
  Nodes.InvalidateStoredLocations;
  Elements.InvalidateStoredLocations;
end;

function TSutraMesh3D.Is3DMesh: Boolean;
begin
  result := MeshType = mt3D;
end;

function TSutraMesh3D.IsFishnetMesh: Boolean;
begin
  result := Mesh2D.MeshGenControls.MeshGenerationMethod = mgmFishnet;
end;

procedure TSutraMesh3D.Clear;
begin
  BeginUpdate;
  try
    ThreeDDataSet := nil;
    TopDataSet := nil;
    ThreeDContourDataSet := nil;
    TopContourDataSet := nil;
    Elements.Clear;
    Nodes.Clear;
    Mesh2D.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TSutraMesh3D.CrossSectionMoved(Sender: TObject);
begin
  FNeedToRedraw3d := True;
  FNeedToRecordCrossSection := True;
  if Model <> nil then
  begin
    (Model as TCustomModel).FrontContoursUpToDate := False;
  end;

//  frmGoPhast.Invalidate3DView;
end;

procedure TSutraMesh3D.ColorDataSetChange(Sender: TObject);
begin
  if not FColorDataSetObserver.UpToDate then
  begin
    FNeedToRecordColoredCells := True;
    frmGoPhast.frame3DView.glWidModelView.Invalidate;
  end;
end;

constructor TSutraMesh3D.Create(Model: TBaseModel);
var
  LocalModel: TCustomModel;
var
  InvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(Model);

  FColorDataSetObserver := TObserver.Create(nil);
  FColorDataSetObserver.OnUpToDateSet := ColorDataSetChange;
  FElementNumbers := TIntegerCollection.Create(InvalidateModelEvent);
  FNodeNumbers := TIntegerCollection.Create(InvalidateModelEvent);
  FCanDraw3D := True;
  FLoading := True;
  FStoredBlockAngle := -1000;
  FStoredNodeAngle := -1000;
  FElevationsNeedUpdating := True;
  FCrossSection := TMeshCrossSectionLine.Create(InvalidateModelEvent);
  FCrossSection.OnMoved := CrossSectionMoved;
  FElements := TSutraElement3D_Collection.Create(Model, self);
  FNodes := TSutraNode3D_Collection.Create(Model, self);
  FMesh2D := TSutraMesh2D.Create(Model, self);
  FActiveNodes := TSutraNode3D_List.Create;
  FActiveElements := TSutraElement3DList.Create;


  FMeshType := mt3D;
  LocalModel := Model as TCustomModel;
  if LocalModel <> nil then
  begin
    FThreeDGridObserver := LocalModel.ThreeDGridObserver;
  end
  else
  begin
    FThreeDGridObserver := nil;
  end;
//  {$IFDEF SUTRA}
//  if LocalModel.ModelSelection = msSutra22 then
//  begin
//    FThreeDGridObserver.OnUpToDateSet := LocalModel.OnTopSutraMeshChanged;
//  end;
//  {$ENDIF}
  NodeDrawingChoice := dcEdge;
  ElementDrawingChoice := dcAll;

  FListsCreated := False;
  FNeedToRedraw3d := True;
  FNeedToRecordCrossSection := True;
  FNeedToRecordLayer := True;
  FCanDraw := True;
  FLoading := False;

end;

destructor TSutraMesh3D.Destroy;
begin
  FTopMeshOutline.Free;
  FFrontMeshOutline.Free;
  FNodeIntervalTree.Free;
  FElementIntervalTree.Free;
  FNodeRangeTree.Free;
  FElementRangeTree.Free;
  FActiveElements.Free;
  FActiveNodes.Free;
  FMesh2D.Free;
  FNodes.Free;
  FElements.Free;
  FCrossSection.Free;
  FElementNumbers.Free;
  FNodeNumbers.Free;
  FColorDataSetObserver.Free;
  inherited;
end;

function Average2Point(const Point1, Point2: TPoint2D): TPoint2D;
begin
  Result.x := (Point1.x + Point2.x)/2;
  Result.y := (Point1.y + Point2.y)/2;
end;

function Average4Point(const Point1, Point2, Point3, Point4: TPoint2D): TPoint2D;
begin
  Result.x := (Point1.x + Point2.x + Point3.x + Point4.x)/4;
  Result.y := (Point1.y + Point2.y + Point3.y + Point4.y)/4;
end;

procedure TSutraMesh3D.GetNodesIntfOnCrossSection(NodeList: TINode2DList);
var  
  SutraNodeList: TSutraNode2D_List;
  NodeIndex: Integer;
begin
  SutraNodeList := TSutraNode2D_List.Create;
  try
    GetNodesOnCrossSection(SutraNodeList);
    for NodeIndex := 0 to SutraNodeList.Count -1 do
    begin
      NodeList.Add(SutraNodeList[NodeIndex]);
    end;
  finally    
    SutraNodeList.Free;
  end;
end;

procedure TSutraMesh3D.GetNodesOnCrossSection(NodeList: TSutraNode2D_List);
begin
  Mesh2D.GetNodesOnSegment(CrossSection.Segment, NodeList);
end;

function TSutraMesh3D.GetRangeTree(EvalAt: TEvaluatedAt;
  Angle: Double): TRbwRangeTree;
var
  Limits: TLimitsArray;
  Polygons: TCellElementPolygons2D;
  procedure CreateRangeTree(var RangeTree: TRbwRangeTree);
  var
    LayerIndex: integer;
    ColIndex: integer;
    PolyObject: TFrontPolygon;
    PolyList: TFrontPolygonList;
  begin
    PolyList := TFrontPolygonList.Create;
    for LayerIndex := 0 to Length(Polygons) - 1 do
    begin
      for ColIndex := 0 to Length(Polygons[0]) - 1 do
      begin
        if Length(Polygons[LayerIndex,ColIndex]) > 0 then
        begin
          PolyObject := TFrontPolygon.Create(ColIndex,LayerIndex,
            Polygons[LayerIndex,ColIndex]);
          PolyList.Add(PolyObject)
        end;
      end;
    end;
    RangeTree := TRbwRangeTree.Create(PolyList);
  end;
begin
  result := nil;
  case EvalAt of
    eaBlocks:
      begin
        if (Angle <> FStoredBlockAngle) then
        begin
          FreeAndNil(FElementIntervalTree);
          FreeAndNil(FElementRangeTree);
          FStoredBlockAngle := Angle;
        end;
        if FElementRangeTree = nil then
        begin
          Polygons := FrontPolygons(Angle, EvalAt, Limits);
          CreateRangeTree(FElementRangeTree);
        end;
        result := FElementRangeTree;
      end;
    eaNodes:
      begin
        if (Angle <> FStoredNodeAngle) then
        begin
          FreeAndNil(FNodeIntervalTree);
          FreeAndNil(FNodeRangeTree);
          FStoredNodeAngle := Angle;
        end;
        if FNodeRangeTree = nil then
        begin
          Polygons := FrontPolygons(Angle, EvalAt, Limits);
          CreateRangeTree(FNodeRangeTree);
        end;
        result := FNodeRangeTree;
      end;
    else Assert(False);
  end;
end;

function TSutraMesh3D.GetCrossSection: TMeshCrossSectionLine;
begin
  result := FCrossSection;
end;

function TSutraMesh3D.GetCrossSectionStart: TPoint2D;
var
  Origin: TPoint2D;
  CrossSectionLine: TLine2D;
begin
  Origin.x := 0;
  Origin.y := 0;
  CrossSectionLine[1] := CrossSection.StartPoint;
  CrossSectionLine[2] := CrossSection.EndPoint;
  result := ClosestPointOnLineFromPoint(CrossSectionLine, Origin);
end;

procedure TSutraMesh3D.GetElementsIntfOnCrossSection(
  ElementList: TIElement2DList);
var
  SutraElementList: TSutraElement2D_List;
  CellIndex: Integer;
begin
  SutraElementList:= TSutraElement2D_List.Create;
  try
    GetElementsOnCrossSection(SutraElementList);
    for CellIndex := 0 to SutraElementList.Count -1 do
    begin
      ElementList.Add(SutraElementList[CellIndex]);
    end;
  finally
    SutraElementList.Free;
  end;
end;

procedure TSutraMesh3D.GetElementsOnCrossSection(ElementList: TSutraElement2D_List);
begin
  Mesh2D.GetElementsOnSegment(CrossSection.Segment, ElementList);
end;

function TSutraMesh3D.GetElevationsNeedUpdating: boolean;
begin
  result := FElevationsNeedUpdating and (Mesh2D.Nodes.Count > 0)
    and (MeshType = mt3D);
//  if Mesh2D.Nodes.Count = 0 then
//  begin
//    result := False;
//    Exit;
//  end;
//  if MeshType in [mt2D, mtProfile] then
//  begin
//    result := False;
//    Exit;
//  end

end;

procedure TSutraMesh3D.GetXIntervalLimits(Subject: TObject;
    out LowerBoundary, UpperBoundary: double);
var
  PolyObject: TFrontPolygon;
begin
  PolyObject := TFrontPolygon(Subject);
  LowerBoundary := PolyObject.MinX;
  UpperBoundary := PolyObject.MaxX;
end;

procedure TSutraMesh3D.GetYIntervalLimits(Subject: TObject;
    out LowerBoundary, UpperBoundary: double);
var
  PolyObject: TFrontPolygon;
begin
  PolyObject := TFrontPolygon(Subject);
  LowerBoundary := PolyObject.MinY;
  UpperBoundary := PolyObject.MaxY;
end;

function TSutraMesh3D.GetIntervalTree(EvalAt: TEvaluatedAt;
  Angle: Double): TRbwIntervalTree;
var
  Limits: TLimitsArray;
  Polygons: TCellElementPolygons2D;
  procedure CreateIntevalTree(var IntervalTree: TRbwIntervalTree);
  var
    LayerIndex: integer;
    ColIndex: integer;
    AList: TObjectList;
    PolyObject: TFrontPolygon;
    IntDefs: TIntDefArray;
    Index: Integer;
  begin
    AList := TObjectList.Create;
    try
      AList.OwnsObjects := False;
      AList.Capacity := Length(Polygons)*Length(Polygons[0]);
      SetLength(IntDefs, 2);
      IntDefs[0].OnFindObjectInterval := GetXIntervalLimits;
      IntDefs[1].OnFindObjectInterval := GetYIntervalLimits;
      for LayerIndex := 0 to Length(Polygons) - 1 do
      begin
        for ColIndex := 0 to Length(Polygons[0]) - 1 do
        begin
          if Length(Polygons[LayerIndex,ColIndex]) > 0 then
          begin
            PolyObject := TFrontPolygon.Create(ColIndex,LayerIndex,
              Polygons[LayerIndex,ColIndex]);
            if AList.Count = 0 then
            begin
              IntDefs[0].LowerBoundary := PolyObject.MinX;
              IntDefs[0].UpperBoundary := PolyObject.MaxX;
              IntDefs[1].LowerBoundary := PolyObject.MinY;
              IntDefs[1].UpperBoundary := PolyObject.MaxY;
            end
            else
            begin
              if IntDefs[0].LowerBoundary > PolyObject.MinX then
              begin
                IntDefs[0].LowerBoundary := PolyObject.MinX;
              end;
              if IntDefs[0].UpperBoundary < PolyObject.MaxX then
              begin
                IntDefs[0].UpperBoundary := PolyObject.MaxX;
              end;
              if IntDefs[1].LowerBoundary > PolyObject.MinY then
              begin
                IntDefs[1].LowerBoundary := PolyObject.MinY;
              end;
              if IntDefs[1].UpperBoundary < PolyObject.MaxY then
              begin
                IntDefs[1].UpperBoundary := PolyObject.MaxY;
              end;
            end;
            AList.Add(PolyObject)
          end;
        end;
      end;
      IntervalTree := TRbwIntervalTree.Create(IntDefs);
      IntervalTree.OwnsObjects := True;
      for Index := 0 to AList.Count - 1 do
      begin
        IntervalTree.Add(AList[Index]);
      end;
    finally
      AList.Free;
    end;
  end;
begin
  result := nil;
  case EvalAt of
    eaBlocks:
      begin
        if (Angle <> FStoredBlockAngle) then
        begin
          FreeAndNil(FElementIntervalTree);
          FreeAndNil(FElementRangeTree);
          FStoredBlockAngle := Angle;
        end;
        if FElementIntervalTree = nil then
        begin
          Polygons := FrontPolygons(Angle, EvalAt, Limits);
          CreateIntevalTree(FElementIntervalTree);
        end;
        result := FElementIntervalTree;
      end;
    eaNodes:
      begin
        if (Angle <> FStoredNodeAngle) then
        begin
          FreeAndNil(FNodeIntervalTree);
          FreeAndNil(FNodeRangeTree);
        end;
        if FNodeIntervalTree = nil then
        begin
          Polygons := FrontPolygons(Angle, EvalAt, Limits);
          CreateIntevalTree(FNodeIntervalTree);
        end;
        result := FNodeIntervalTree;
      end;
    else Assert(False);
  end;
end;

function TSutraMesh3D.GetItemTopLocation(const EvalAt: TEvaluatedAt; const Column,
  Row: integer): TPoint2D;
begin
  result := Mesh2D. ItemTopLocation[EvalAt, Column, Row];
end;

procedure TSutraMesh3D.DrawFront(const BitMap: TPersistent);
var
  NodeList: TSutraNode2D_List;
  Node2D_Index: Integer;
  Node2D: TSutraNode2D;
  LayerIndex: Integer;
  Node3D: TSutraNode3D;
  SegmentAngle: double;
  Angle: double;
  X_Float: double;
  Points: GoPhastTypes.TPointArray;
  Points2: GoPhastTypes.TPointArray;
  X_Int: Integer;
  ZoomBox: TQRbwZoomBox2;
  ElementList: TSutraElement2D_List;
  Element2D: TSutraElement2D;
  Element3D: TSutraElement3D;
  ARect: TRect;
  Element2D_Index: integer;
  ElCenter: TPoint2D;
  ZInt: Integer;
  ElementOutlines: array of array of TPoint;
  ElementOutlinesFloat: array of array of TPoint2D;
  TwoDElements: array of TSutraElement2D;
  YInt: Integer;
  ElementIndex: Integer;
  PriorNode2D: TSutraNode2D;
  Index1: Integer;
  Index2: Integer;
  Delta: Integer;
  CellOutlines: array of array of TPoint;
  StringValues: TStringlist;
//  RowIndex: Integer;
//  ColIndex: Integer;
  ShowColor: Boolean;
  Fraction: Double;
  AColor: TColor;
  Dummy: TPolygon32;
//  Origin: TPoint2D;
  StartPoint: TPoint2D;
//  CrossSectionLine: TLine2D;
  NumString: string;
  ALine: TLine2D;
  ClosestPoint: TPoint2D;
  ADistance: double;
  OffSet: FastGeo.TFloat;
  DrawEdge: Boolean;
  MinMax: TMinMax;
  Extent: TSize;
  ACanvas: TCanvas;
  MinMaxInitialized: Boolean;
  function Point2DtoPoint(const APoint: TPoint2D): TPoint;
  begin
    Result.X := ZoomBox.XCoord(APoint.x);
    Result.Y := ZoomBox.YCoord(APoint.y);
  end;
  procedure ComputeNodeCrossSectionDistance;
  begin
    ClosestPoint := ClosestPointOnLineFromPoint(
      ALine, Node2D.Location);
    ADistance := -Distance(Node2D.Location, ClosestPoint);
    if ADistance <> 0 then
    begin
      if FastGEO.Orientation(Node2D.Location,
        CrossSection.StartPoint, CrossSection.EndPoint) =
        LeftHandSide then
      begin
        ADistance := -ADistance;
      end;
    end;
    ADistance := ADistance + OffSet;
  end;
begin
  if FUpdatingElevations then
  begin
    Exit;
  end;
  Dummy := nil;
  NodeList := TSutraNode2D_List.Create;
  ElementList := TSutraElement2D_List.Create;
  try
    GetNodesOnCrossSection(NodeList);
    if NodeList.Count = 0 then
    begin
      FMaxDist := 0;
      FMinDist := 0;
      Exit;
    end;

    ALine := EquateLine(CrossSection.StartPoint, CrossSection.EndPoint);

    ClosestPoint := ClosestPointOnLineFromPoint(
      ALine, EquatePoint(0.0, 0.0));
    OffSet := Distance(EquatePoint(0.0, 0.0), ClosestPoint);
    if OffSet <> 0 then
    begin
      if FastGEO.Orientation(EquatePoint(0.0, 0.0),
        CrossSection.StartPoint, CrossSection.EndPoint) =
        LeftHandSide then
      begin
        OffSet := -OffSet;
      end;
    end;

    Node2D := NodeList[0];
    ComputeNodeCrossSectionDistance;
    FMaxDist := ADistance;
    FMinDist := ADistance;
    for Node2D_Index := 1 to NodeList.Count - 1 do
    begin
      Node2D := NodeList[Node2D_Index];
      ComputeNodeCrossSectionDistance;
      FMaxDist := Max(ADistance, FMaxDist);
      FMinDist := Min(ADistance, FMinDist);
    end;

    SetLength(ElementOutlines, LayerCount+1, NodeList.Count);
    SetLength(ElementOutlinesFloat, LayerCount+1, NodeList.Count);

    SegmentAngle := CrossSection.Angle;
    SetLength(Points, 5);

    ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
//    StartPoint := GetCrossSectionStart;
    StartPoint := EquatePoint(0.0, 0.0);

    // Determine the locations of the outlines of the elements in cross section.
    for Node2D_Index := 0 to NodeList.Count - 1 do
    begin
      Node2D := NodeList[Node2D_Index];
      Angle := ArcTan2(Node2D.y - StartPoint.y,
        Node2D.x - StartPoint.x) - SegmentAngle;
      X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
        + StartPoint.x;
      X_Int := ZoomBox.XCoord(X_Float);
      for LayerIndex := 0 to LayerCount do
      begin
        Node3D := NodeArray[LayerIndex, Node2D.Number];
        YInt := ZoomBox.YCoord(Node3D.Z);
        ElementOutlines[LayerIndex, Node2D_Index].X := X_Int;
        ElementOutlines[LayerIndex, Node2D_Index].y := YInt;

        ElementOutlinesFloat[LayerIndex, Node2D_Index].X := X_Float;
        ElementOutlinesFloat[LayerIndex, Node2D_Index].y := Node3D.Z;
      end;
    end;
    GetElementsOnCrossSection(ElementList);

    // Determine the 2D elements to use for drawing the mesh.
    SetLength(TwoDElements, NodeList.Count-1);
    for ElementIndex := 0 to Length(TwoDElements) - 1 do
    begin
      TwoDElements[ElementIndex] := nil;
    end;

    PriorNode2D := NodeList[0];
    for Node2D_Index := 1 to NodeList.Count - 1 do
    begin
      Node2D := NodeList[Node2D_Index];
      for Element2D_Index := 0 to ElementList.Count - 1 do
      begin
        Element2D := ElementList[Element2D_Index];
        Index1 := Element2D.Nodes.IndexOfNode(PriorNode2D);
        if (Index1 >= 0) then
        begin
          Index2 := Element2D.Nodes.IndexOfNode(Node2D);
          if  (Index2 >= 0) then
          begin
            if TwoDElements[Node2D_Index-1] = nil then
            begin
              TwoDElements[Node2D_Index-1] := Element2D;
            end
            else
            begin
              // Use the element that is further back
              Delta := Index2 - Index1;
              if (Delta = 1) or (Delta = -3) then
              begin
                TwoDElements[Node2D_Index-1] := Element2D;
              end;
            end;
          end;
        end;
      end;
      PriorNode2D := Node2D;
    end;

    if ((ThreeDDataSet <> nil) and (ThreeDDataSet.Orientation = dso3D))
      or (NodeDrawingChoice = dcAll) then
    begin

      StringValues := TStringlist.Create;
      try

        if ThreeDDataSet <> nil then
        begin
          GetMinMax(MinMax, ThreeDDataSet, StringValues, MinMaxInitialized);
          ApplyLimittoMinMax(ThreeDDataSet, MinMax, ThreeDDataSet.Limits);
        end;

        if (ThreeDDataSet <> nil) and (ThreeDDataSet.EvaluatedAt = eaBlocks) then
        begin
//          UpdateMinMax
          // color the elements.
          for Node2D_Index := 0 to NodeList.Count - 2 do
          begin
            Element2D := TwoDElements[Node2D_Index];
            if Element2D = nil then
            begin
              Continue;
            end;

            for LayerIndex := 0 to LayerCount - 1 do
            begin
              if not ElementArray[LayerIndex,
                Element2D.ElementNumber].Active then
              begin
                Continue;
              end;

              GetDataSetMeshValue(ThreeDDataSet, LayerIndex,
                Element2D.ElementNumber, StringValues, ShowColor, Fraction,
                MinMax);
              if ShowColor then
              begin
                Points[0] := ElementOutlines[LayerIndex, Node2D_Index];
                Points[1] := ElementOutlines[LayerIndex+1, Node2D_Index];
                Points[2] := ElementOutlines[LayerIndex+1, Node2D_Index+1];
                Points[3] := ElementOutlines[LayerIndex, Node2D_Index+1];
                Points[4] := Points[0];

                AColor := frmGoPhast.PhastModel.GridColorParameters.
                  FracToColor(Fraction);
                DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                  0, Points, Dummy, False, True);
              end;
            end;
          end;

        end;

        if ((ThreeDDataSet <> nil) and (ThreeDDataSet.EvaluatedAt = eaNodes))
          or (NodeDrawingChoice = dcAll) then
        begin
          // color the cell around the nodes.

          // Determine the outlines of the cells.
          SetLength(CellOutlines, LayerCount+2, NodeList.Count*2-1);
          // Set the corners at the top right and lower right.
          CellOutlines[0,NodeList.Count*2-2] :=
            Point2DtoPoint(ElementOutlinesFloat[0,NodeList.Count-1]);
          CellOutlines[LayerCount+1,NodeList.Count*2-2] :=
            Point2DtoPoint(ElementOutlinesFloat[LayerCount,NodeList.Count-1]);

          // Set the locations of the cell edge at the top and bottom edges
          for Index2 := 0 to NodeList.Count - 2 do
          begin
            CellOutlines[0,Index2*2] := Point2DtoPoint(ElementOutlinesFloat[0, Index2]);
            CellOutlines[0,Index2*2+1] := Point2DtoPoint(Average2Point(
              ElementOutlinesFloat[0,Index2+1], ElementOutlinesFloat[0, Index2]));

            CellOutlines[LayerCount+1,Index2*2] := Point2DtoPoint(ElementOutlinesFloat[LayerCount, Index2]);
            CellOutlines[LayerCount+1,Index2*2+1] := Point2DtoPoint(Average2Point(
              ElementOutlinesFloat[LayerCount,Index2+1], ElementOutlinesFloat[LayerCount, Index2]));
          end;

          // Set the locations of the cell corners at the right edge.
          for Index1 := 1 to LayerCount do
          begin
            CellOutlines[Index1,NodeList.Count*2-2] := Point2DtoPoint(Average2Point(
              ElementOutlinesFloat[Index1,NodeList.Count-1],
              ElementOutlinesFloat[Index1-1,NodeList.Count-1]));
          end;

          // Set the location of the cell corners in the interior
          for Index1 := 1 to LayerCount do
          begin
            for Index2 := 0 to NodeList.Count - 2 do
            begin
              CellOutlines[Index1,Index2*2] := Point2DtoPoint(Average2Point(
                ElementOutlinesFloat[Index1-1,Index2],
                ElementOutlinesFloat[Index1, Index2]));

              CellOutlines[Index1,Index2*2+1] := Point2DtoPoint(Average4Point(
                ElementOutlinesFloat[Index1-1,Index2+1],
                ElementOutlinesFloat[Index1, Index2+1],
                ElementOutlinesFloat[Index1-1,Index2],
                ElementOutlinesFloat[Index1, Index2]));
            end;
          end;

          SetLength(Points,5);
          SetLength(Points2,7);
          for Node2D_Index := 0 to NodeList.Count-1 do
          begin
            Node2D := NodeList[Node2D_Index];
            for LayerIndex := 0 to LayerCount do
            begin
              if not NodeArray[LayerIndex, Node2D.Number].Active then
              begin
                Continue;
              end;

              if ((ThreeDDataSet <> nil)
                and (ThreeDDataSet.EvaluatedAt = eaNodes)) then
              begin
                GetDataSetMeshValue(ThreeDDataSet, LayerIndex,
                  Node2D.Number, StringValues, ShowColor, Fraction, MinMax);
              end
              else
              begin
                ShowColor := False;
              end;
              if ShowColor or (NodeDrawingChoice = dcAll) then
              begin

                if ShowColor then
                begin
                  AColor := frmGoPhast.PhastModel.GridColorParameters.
                    FracToColor(Fraction);
                end
                else
                begin
                  AColor := clBlack;
                end;

                if Length(CellOutlines[LayerIndex]) > Node2D_Index*2 then
                begin
                  if (Node2D_Index = 0) or (TwoDElements[Node2D_Index-1] = nil) then
                  begin
                    if Length(CellOutlines[LayerIndex]) > Node2D_Index*2+1 then
                    begin
                      Points[0] := CellOutlines[LayerIndex, Node2D_Index*2];
                      Points[1] := CellOutlines[LayerIndex, Node2D_Index*2+1];
                      Points[2] := CellOutlines[LayerIndex+1, Node2D_Index*2+1];
                      Points[3] := CellOutlines[LayerIndex+1, Node2D_Index*2];
                      Points[4] := Points[0];

                      if ShowColor then
                      begin
                        DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                          0, Points, Dummy, False, True);
                      end;
                      if (NodeDrawingChoice = dcAll) then
                      begin
                        DrawBigPolyline32(BitMap, clBlack32,
                          OrdinaryGridLineThickness, Points, True, True);
                      end;
                    end;
                  end
                  else if (Node2D_Index = NodeList.Count-1)
                    or (TwoDElements[Node2D_Index] = nil) then
                  begin
                    Points[0] := CellOutlines[LayerIndex, Node2D_Index*2-1];
                    Points[1] := CellOutlines[LayerIndex, Node2D_Index*2];
                    Points[2] := CellOutlines[LayerIndex+1, Node2D_Index*2];
                    Points[3] := CellOutlines[LayerIndex+1, Node2D_Index*2-1];
                    Points[4] := Points[0];

                    if ShowColor then
                    begin
                      DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                        0, Points, Dummy, False, True);
                    end;
                    if (NodeDrawingChoice = dcAll) then
                    begin
                      DrawBigPolyline32(BitMap, clBlack32,
                        OrdinaryGridLineThickness, Points, True, True);
                    end;
                  end
                  else
                  begin
                    Points2[0] := CellOutlines[LayerIndex, Node2D_Index*2-1];
                    Points2[1] := CellOutlines[LayerIndex, Node2D_Index*2];
                    Points2[2] := CellOutlines[LayerIndex, Node2D_Index*2+1];
                    Points2[3] := CellOutlines[LayerIndex+1, Node2D_Index*2+1];
                    Points2[4] := CellOutlines[LayerIndex+1, Node2D_Index*2];
                    Points2[5] := CellOutlines[LayerIndex+1, Node2D_Index*2-1];
                    Points2[6] := Points2[0];

                    if ShowColor then
                    begin
                      DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                        0, Points2, Dummy, False, True);
                    end;
                    if (NodeDrawingChoice = dcAll) then
                    begin
                      DrawBigPolyline32(BitMap, clBlack32,
                        OrdinaryGridLineThickness, Points2, True, True);
                    end;
                  end;
                end;
              end;
            end;

          end;
        end
      finally
        StringValues.Free;
      end;
    end;

    SetLength(Points,5);
    // Draw the outlines of the elements.
    for Node2D_Index := 0 to NodeList.Count - 2 do
    begin
      Element2D := TwoDElements[Node2D_Index];
      if Element2D = nil then
      begin
        Continue;
      end;


      // draw all but the selected layer.
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        if LayerIndex = SelectedLayer then
        begin
          Continue;
        end;
        if not ElementArray[LayerIndex, Element2D.ElementNumber].Active then
        begin
          Continue;
        end;

        Points[0] := ElementOutlines[LayerIndex, Node2D_Index];
        Points[1] := ElementOutlines[LayerIndex+1, Node2D_Index];
        Points[2] := ElementOutlines[LayerIndex+1, Node2D_Index+1];
        Points[3] := ElementOutlines[LayerIndex, Node2D_Index+1];
        Points[4] := Points[0];


        case ElementDrawingChoice of
          dcEdge:
            begin
              // draw left edge
              if (Node2D_Index = 0) then
              begin
                DrawEdge := True;
              end
              else
              begin
                Node2D := NodeList[Node2D_Index-1];
                DrawEdge :=
                  (not NodeArray[LayerIndex, Node2D.Number].Active)
                  or (not NodeArray[LayerIndex+1, Node2D.Number].Active)
              end;
              if DrawEdge then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 0, 2);
              end;

              // draw right edge
              if (Node2D_Index = NodeList.Count - 2) then
              begin
                DrawEdge := True;
              end
              else
              begin
                Node2D := NodeList[Node2D_Index+1];
                DrawEdge :=
                  (not NodeArray[LayerIndex, Node2D.Number].Active)
                  or (not NodeArray[LayerIndex+1, Node2D.Number].Active)
              end;
              if DrawEdge then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 2, 2);
              end;

              // draw top edge
              if (LayerIndex = 0) then
              begin
                DrawEdge := True;
              end
              else
              begin
                Node2D := NodeList[Node2D_Index];
                DrawEdge :=
                  (not NodeArray[LayerIndex-1, Node2D.Number].Active);
                if not DrawEdge then
                begin
                  Node2D := NodeList[Node2D_Index+1];
                  DrawEdge :=
                    (not NodeArray[LayerIndex-1, Node2D.Number].Active);
                end;
              end;
              if DrawEdge then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 3, 2);
              end;

              // draw bottom edge
              if (LayerIndex = LayerCount - 1) then
              begin
                DrawEdge := True;
              end
              else
              begin
                Node2D := NodeList[Node2D_Index];
                DrawEdge :=
                  (not NodeArray[LayerIndex+1, Node2D.Number].Active);
                if not DrawEdge then
                begin
                  Node2D := NodeList[Node2D_Index+1];
                  DrawEdge :=
                    (not NodeArray[LayerIndex+1, Node2D.Number].Active);
                end;
              end;
              if DrawEdge then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 1, 2);
              end;
            end;
          dcAll:
            begin
              DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                Points, True);
            end;
          else Assert(False);
        end;
      end;

      // draw the selected layer last using a different color.
      if SelectedLayer < LayerCount then
      begin
        if not ElementArray[SelectedLayer, Element2D.ElementNumber].Active then
        begin
          Continue;
        end;
        LayerIndex := SelectedLayer;
        Points[0] := ElementOutlines[SelectedLayer, Node2D_Index];
        Points[1] := ElementOutlines[SelectedLayer+1, Node2D_Index];
        Points[2] := ElementOutlines[SelectedLayer+1, Node2D_Index+1];
        Points[3] := ElementOutlines[SelectedLayer, Node2D_Index+1];
        Points[4] := Points[0];
      end
      else
      begin
        if not ElementArray[SelectedLayer-1, Element2D.ElementNumber].Active then
        begin
          Continue;
        end;
        LayerIndex := SelectedLayer-1;
        Points[0] := ElementOutlines[SelectedLayer-1, Node2D_Index];
        Points[1] := ElementOutlines[SelectedLayer, Node2D_Index];
        Points[2] := ElementOutlines[SelectedLayer, Node2D_Index+1];
        Points[3] := ElementOutlines[SelectedLayer-1, Node2D_Index+1];
        Points[4] := Points[0];
      end;

      case ElementDrawingChoice of
        dcEdge:
          begin
            // draw left edge
            if (Node2D_Index = 0) then
            begin
              DrawEdge := (SelectedLayer <> LayerCount);
            end
            else
            begin
              Node2D := NodeList[Node2D_Index-1];
              DrawEdge :=
                (not NodeArray[LayerIndex, Node2D.Number].Active)
                or (not NodeArray[LayerIndex+1, Node2D.Number].Active)
            end;
            if DrawEdge then
            begin
              DrawBigPolyline32(BitMap, Color32(CrossSection.Color),
                OrdinaryGridLineThickness, Points, True, False, 0, 2);
            end;

            // draw right edge
            if (Node2D_Index = NodeList.Count - 2) then
            begin
              DrawEdge := (SelectedLayer <> LayerCount);
            end
            else
            begin
              Node2D := NodeList[Node2D_Index+1];
              DrawEdge :=
                (not NodeArray[LayerIndex, Node2D.Number].Active)
                or (not NodeArray[LayerIndex+1, Node2D.Number].Active)
            end;
            if DrawEdge then
            begin
              DrawBigPolyline32(BitMap, Color32(CrossSection.Color),
                OrdinaryGridLineThickness, Points, True, False, 2, 2);
            end;

            // draw top edge
//            if (LayerIndex = 0) then
//            begin
              DrawEdge := (SelectedLayer <> LayerCount);
//            end
//            else
//            begin
//              Node2D := NodeList[Node2D_Index];
//              DrawEdge :=
//                (not NodeArray[LayerIndex-1, Node2D.Number].Active);
//              if not DrawEdge then
//              begin
//                Node2D := NodeList[Node2D_Index+1];
//                DrawEdge :=
//                  (not NodeArray[LayerIndex-1, Node2D.Number].Active);
//              end;
//            end;
            if DrawEdge then
            begin
              DrawBigPolyline32(BitMap, Color32(CrossSection.Color),
                OrdinaryGridLineThickness, Points, True, False, 3, 2);
            end;

            // draw bottom edge
//            if (LayerIndex = LayerCount - 1) then
//            begin
              DrawEdge := (ThreeDDataSet = nil)
                or (ThreeDDataSet.EvaluatedAt = eaBlocks);
//            end
//            else
//            begin
//              Node2D := NodeList[Node2D_Index];
//              DrawEdge :=
//                (not NodeArray[LayerIndex+1, Node2D.Number].Active);
//              if not DrawEdge then
//              begin
//                Node2D := NodeList[Node2D_Index+1];
//                DrawEdge :=
//                  (not NodeArray[LayerIndex+1, Node2D.Number].Active);
//              end;
//            end;
            if DrawEdge then
            begin
              DrawBigPolyline32(BitMap, Color32(CrossSection.Color),
                OrdinaryGridLineThickness, Points, True, False, 1, 2);
            end;
          end;
        dcAll:
          begin
            DrawBigPolyline32(BitMap, Color32(CrossSection.Color),
              OrdinaryGridLineThickness, Points, True);
          end;
        else Assert(False);
      end;
    end;

    if DrawNodeNumbers then
    begin
      if BitMap is TBitmap32 then
      begin
        TBitmap32(BitMap).Font := NodeFont;
      end
      else
      begin
        (BitMap as TCanvas).Font := NodeFont;
      end;
      for Node2D_Index := 0 to NodeList.Count - 1 do
      begin
        Node2D := NodeList[Node2D_Index];
        Angle := ArcTan2(Node2D.y - StartPoint.y,
          Node2D.x - StartPoint.x) - SegmentAngle;
        X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
          + StartPoint.x;
        X_Int := ZoomBox.XCoord(X_Float);
        for LayerIndex := 0 to LayerCount do
        begin
          Node3D := NodeArray[LayerIndex, Node2D.Number];
          if Node3D.Active then
          begin
            YInt := ZoomBox.YCoord(Node3D.Z);

            NumString := InttoStr(Node3D.Number+1);
//            Extent := BitMap.TextExtent(NumString);
            if BitMap is TBitmap32 then
            begin
              TBitmap32(BitMap).Textout(X_Int+1 {- (Extent.cx div 2)}, YInt+1, NumString);
            end
            else
            begin
              (BitMap as TCanvas).Textout(X_Int+1 {- (Extent.cx div 2)}, YInt+1, NumString);
            end;
          end;

        end;
      end;
    end;


    // Draw dots at the center of each element intersected by the cross section
    // and element numbers.
    if DrawElementCenters or DrawElementNumbers then
    begin
      if BitMap is TBitmap32 then
      begin
        TBitmap32(BitMap).Font := ElementFont;
      end
      else
      begin
        (BitMap as TCanvas).Font := ElementFont;
      end;
      SetLength(Points, 5);
      for Element2D_Index := 0 to ElementList.Count - 1 do
      begin
        Element2D := ElementList[Element2D_Index];


        ElCenter := Element2D.Center;
        Angle := ArcTan2(ElCenter.y - StartPoint.y,
          ElCenter.x - StartPoint.x) - SegmentAngle;
        X_Float := Distance(StartPoint, ElCenter)*Cos(Angle)
          + StartPoint.x;
        X_Int := ZoomBox.XCoord(X_Float);
        ARect.Left := X_Int-1;
        ARect.Right := X_Int+1;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          Element3D := ElementArray[LayerIndex, Element2D.ElementNumber];
          if Element3D.active then
          begin
            ZInt := ZoomBox.YCoord(Element3D.CenterElevation);
            ARect.Top := ZInt-1;
            ARect.Bottom := ZInt+1;
            if DrawElementCenters then
            begin
              if LayerIndex = SelectedLayer then
              begin
                DrawBigRectangle32(BitMap, Color32(CrossSection.Color),
                  Color32(CrossSection.Color), OrdinaryGridLineThickness, ARect);
              end
              else
              begin
                DrawBigRectangle32(BitMap, clBlack32, clBlack32,
                  OrdinaryGridLineThickness, ARect);
              end;
            end;
            if DrawElementNumbers then
            begin
              NumString := InttoStr(Element3D.ElementNumber+1);
              if BitMap is TBitmap32 then
              begin
                Extent := TBitmap32(BitMap).TextExtent(NumString);
                TBitmap32(BitMap).Textout(X_Int - (Extent.cx div 2),
                  ZInt {- (Extent.cy div 2)}, NumString);
              end
              else
              begin
                ACanvas := BitMap as TCanvas;
                Extent := ACanvas.TextExtent(NumString);
                ACanvas.Textout(X_Int - (Extent.cx div 2),
                  ZInt {- (Extent.cy div 2)}, NumString);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    ElementList.Free;
    NodeList.Free;
  end;

  DrawFrontContours(ZoomBox, BitMap);
end;

procedure TSutraMesh3D.DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  Contourer: TMultipleContourCreator;
begin
  if (Nodes.Count > 0) and (ThreeDContourDataSet <> nil)
    and (ThreeDContourDataSet.Orientation in [dsoFront, dso3d]) then
  begin
      Contourer := TMultipleContourCreator.Create;
      try
        PlotList := FFrontContourPlotList;
        Contourer.DataSet := ThreeDContourDataSet;
        Contourer.ActiveDataSet := nil;
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdTop;
        Contourer.Mesh := Self;
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedLayer,
          frmGoPhast.PhastModel.ContourColorParameters, vdFront,
          frmGoPhast.PhastModel.ContourLabelSpacing,
          frmGoPhast.PhastModel.FrontContoursUpToDate);
        frmGoPhast.PhastModel.FrontContoursUpToDate := True;
      finally
        Contourer.Free;
      end;
  end;
end;

procedure TSutraMesh3D.DrawPointsOnCrossSection(BitMap: TPersistent);
var
  NodesOnSegment: TSutraNode2D_List;
  Points: GoPhastTypes.TPointArray;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ZoomBox: TQRbwZoomBox2;
  ARect: TRect;
  ElList: TSutraElement2D_List;
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  ElCenter: TPoint2D;
  ElementLayer: Integer;
  AnElement3D: TSutraElement3D;
begin
  if not DrawElementCenters then
  begin
    Exit;
  end;
  if Elements.Count = 0 then
  begin
    Exit;
  end;
  ZoomBox := frmGoPhast.frameTopView.ZoomBox;
  NodesOnSegment := TSutraNode2D_List.Create;
  try
    Mesh2D.GetNodesOnSegment(CrossSection.Segment, NodesOnSegment);
    SetLength(Points, NodesOnSegment.Count);
    for NodeIndex := 0 to NodesOnSegment.Count - 1 do
    begin
      ANode := NodesOnSegment[NodeIndex];
      Points[NodeIndex].X := ZoomBox.XCoord(ANode.X);
      Points[NodeIndex].Y := ZoomBox.YCoord(ANode.Y);
    end;
    DrawBigPolyline32(BitMap, Color32(CrossSection.Color), ThickGridLineThickness,
      Points, True);
//    for NodeIndex := 0 to Length(Points) - 1 do
//    begin
//      ARect.Left := Points[NodeIndex].X -2;
//      ARect.Right := Points[NodeIndex].X +2;
//      ARect.Top := Points[NodeIndex].Y -2;
//      ARect.Bottom := Points[NodeIndex].Y +2;
//      DrawBigRectangle32(BitMap, Color32(CrossSection.Color), Color32(CrossSection.Color),
//        OrdinaryGridLineThickness, ARect);
//    end;
  finally
    NodesOnSegment.Free;
  end;

  ElementLayer := SelectedLayer;
  if ElementLayer >= LayerCount then
  begin
    ElementLayer := LayerCount-1;
  end;
  ElList := TSutraElement2D_List.Create;
  try
    Mesh2D.GetElementsOnSegment(CrossSection.Segment, ElList);
    for ElementIndex := 0 to ElList.Count - 1 do
    begin
      AnElement2D := ElList[ElementIndex];
      AnElement3D := ElementArray[ElementLayer, AnElement2D.ElementNumber];
      if AnElement3D.Active then
      begin
        ElCenter := AnElement2D.Center;
        ARect.Left := ZoomBox.XCoord(ElCenter.x)-2;
        ARect.Top := ZoomBox.YCoord(ElCenter.y)-2;
        ARect.Right := ARect.Left + 4 ;
        ARect.Bottom := ARect.Top + 4;
        DrawBigRectangle32(BitMap, Color32(CrossSection.Color), Color32(CrossSection.Color),
          OrdinaryGridLineThickness, ARect);
      end;
    end;
  finally
    ElList.Free;
  end;
end;

procedure TSutraMesh3D.EndUpdate;
begin
  inherited;
  if (FUpdateCount = 0) then
  begin
    frmGoPhast.PhastModel.DataArrayManager.InvalidateAll3DDataSets;
  end;
end;

procedure TSutraMesh3D.Draw(const BitMap: TPersistent;
  const ViewDirection: TViewDirection);
begin
  if (FUpdateCount > 0) or FUpdatingElevations then
  begin
    Exit;
  end;
  CheckUpdateElevations;
  case ViewDirection of
    vdTop:
      begin
        Mesh2D.Draw(BitMap);
        if MeshType = mt3D then
        begin
          CrossSection.Draw(BitMap);
          DrawPointsOnCrossSection(BitMap);
        end;
        FNeedToRecalculateTopColors := False;
      end;
    vdFront:
      begin
        if (MeshType = mt3D) and (LayerCount > 0) then
        begin
          DrawFront(BitMap);
        end;
        FNeedToRecalculateFrontColors := False;
      end;
    vdSide: ;
    else Assert(False);
  end;
end;

procedure TSutraMesh3D.RecordLayer;
var
  Layer: Integer;
  ColIndex: Integer;
  AnElement3D: TSutraElement3D;
  NodeIndex: Integer;
  Element2D: TSutraElement2D;
  ANode2D: TSutraNode2D;
  ANode3D: TSutraNode3D;
  NodeLayer: Integer;
  X, Y, Z: single;
begin
  try
    glNewList(FLayerGLIndex, GL_COMPILE);
    try
      glLineWidth(ThinLine);
      glColor3f(0.0, 0.0, 0.0);

      Layer := SelectedLayer;
      NodeLayer := Layer;
      if Layer >= LayerCount then
      begin
        Layer := LayerCount -1;
      end;

      for ColIndex := 0 to Mesh2D.Elements.Count - 1 do
      begin
        AnElement3D := ElementArray[Layer,ColIndex];
        if AnElement3D.Active then
        begin
          glBegin(GL_LINE_LOOP);
          try
            Element2D := Mesh2D.Elements[ColIndex];
            for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
            begin
              ANode2D := Element2D.Nodes[NodeIndex].Node;
              ANode3D := NodeArray[NodeLayer, ANode2D.Number];
              X := ANode2D.X;
              Y := ANode2D.Y;
              Z := ANode3D.Z;
              glVertex3f(X, Y, Z);
            end;
          finally
            glEnd;
          end;
        end;
      end;
    finally
      glEndList;
    end;
  finally
    FNeedToRecordLayer := False;
  end;
end;

procedure TSutraMesh3D.RestoreElementNumbers;
var
  Index: Integer;
begin
  if FElementNumbers.Count = Elements.Count then
  begin
    for Index := 0 to Elements.Count - 1 do
    begin
      Elements[Index].ElementNumber := FElementNumbers[Index].Value
    end;
    FElementNumbers.Clear;
  end;
end;

procedure TSutraMesh3D.RestoreNodeNumbers;
var
  Index: Integer;
begin
  if FNodeNumbers.Count = Nodes.Count then
  begin
    for Index := 0 to Nodes.Count - 1 do
    begin
      Nodes[Index].Number := FNodeNumbers[Index].Value
    end;
    FNodeNumbers.Clear;
  end;
end;

procedure TSutraMesh3D.RecordCrossSection;
var
  NodeList: TSutraNode2D_List;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  LineStarted: Boolean;
  LayerIndex: Integer;
  X, Y, Z: single;
  ANode3D: TSutraNode3D;
  PriorNode2D: TSutraNode2D;
  SharedElement: Boolean;
  ElementIndex: Integer;
  Element2D: TSutraElement2D;
begin
  if FUpdatingElevations then
  begin
    Exit;
  end;
  NodeList := TSutraNode2D_List.Create;
  try
    GetNodesOnCrossSection(NodeList);
    if NodeList.Count = 0 then
    begin
      Exit;
    end;

    try
      glNewList(FCrossSectionGLIndex, GL_COMPILE);
      try
        glLineWidth(ThinLine);
        glColor3f(0.0, 0.0, 0.0);
        for NodeIndex := 0 to NodeList.Count - 1 do
        begin
          ANode2D := NodeList[NodeIndex];
          LineStarted := False;
          for LayerIndex := 0 to LayerCount do
          begin
            ANode3D := NodeArray[LayerIndex, ANode2D.Number];
            if ANode3D.Active then
            begin
              if not LineStarted then
              begin
                glBegin(GL_LINE_STRIP);
                LineStarted := True;
              end;
              X := ANode2D.X;
              Y := ANode2D.Y;
              Z := ANode3D.Z;
              glVertex3f(X, Y, Z);
            end
            else
            begin
              if LineStarted then
              begin
                glEnd;
                LineStarted := False;
              end;
            end;
          end;
          if LineStarted then
          begin
            glEnd;
//            LineStarted := False;
          end;
        end;

        for LayerIndex := 0 to LayerCount do
        begin
          LineStarted := False;
          PriorNode2D := nil;
          for NodeIndex := 0 to NodeList.Count - 1 do
          begin
            ANode2D := NodeList[NodeIndex];
            ANode3D := NodeArray[LayerIndex, ANode2D.Number];
            if ANode3D.Active then
            begin
              if not LineStarted then
              begin
                glBegin(GL_LINE_STRIP);
                LineStarted := True;
              end
              else if PriorNode2D <> nil then
              begin
                SharedElement := False;
                for ElementIndex := 0 to PriorNode2D.ElementCount - 1 do
                begin
                  Element2D := PriorNode2D.Elements[ElementIndex];
                  if ANode2D.FElements.IndexOf(Element2D) >= 0 then
                  begin
                    SharedElement := True;
                    break;
                  end;
                end;
                if not SharedElement then
                begin
                  glEnd;
                  glBegin(GL_LINE_STRIP);
                end;
              end;
              X := ANode2D.X;
              Y := ANode2D.Y;
              Z := ANode3D.Z;
              glVertex3f(X, Y, Z);
            end
            else
            begin
              if LineStarted then
              begin
                glEnd;
                LineStarted := False;
              end;
            end;
            PriorNode2D := ANode2D;
          end;
          if LineStarted then
          begin
            glEnd;
//            LineStarted := False;
          end;
        end;

      finally
        glEndList;
      end;
    finally
      FNeedToRecordCrossSection := False;
    end;
  finally
    NodeList.Free;
  end;
end;

procedure TSutraMesh3D.RecordColoredCellsOrElements;
var
  Red: GLubyte;
  Green: GLubyte;
  Blue: GLubyte;
  ElementIndex: Integer;
  LayerIndex: Integer;
  AnElement3D: TSutraElement3D;
  StringValues: TStringList;
  ShowColor: Boolean;
  Fraction: Double;
  AColor: TColor;
  NodeIndex: Integer;
  ANode3D: TSutraNode3D;
  MinMax: TMinMax;
  LayerMax: integer;
  MinMaxInitialized: Boolean;
begin
  Assert(ThreeDDataSet <> nil);
  StringValues := TStringList.Create;
  try
    GetMinMax(MinMax, ThreeDDataSet, StringValues, MinMaxInitialized);
    ApplyLimittoMinMax(ThreeDDataSet, MinMax, ThreeDDataSet.Limits);

    LayerMax := -1;
    if ThreeDDataSet.Orientation = dso3D then
    begin
      LayerMax := LayerCount;
    end
    else
    begin
      case ThreeDDataSet.EvaluatedAt of
        eaBlocks: LayerMax := 1;
        eaNodes: LayerMax := 0;
        else Assert(False);
      end;
    end;
    glNewList(FColoredCellsOrElementGLIndex, GL_COMPILE);
    try
      case ThreeDDataSet.EvaluatedAt of
        eaBlocks:
          begin
            for ElementIndex := 0 to Mesh2D.Elements.Count - 1 do
            begin
              for LayerIndex := 0 to LayerMax - 1 do
              begin
                AnElement3D := ElementArray[LayerIndex, ElementIndex];
                if AnElement3D.Active then
                begin
                  GetDataSetMeshValue(ThreeDDataSet, LayerIndex,
                    ElementIndex, StringValues, ShowColor, Fraction, MinMax);
                  if ShowColor then
                  begin
                    AColor := frmGoPhast.PhastModel.GridColorParameters.
                      FracToColor(Fraction);
                    ExtractColorComponents(AColor, Red, Green, Blue);
                    glColor3ub(Red, Green, Blue);
                    AnElement3D.Draw3D;
                  end;
                end;
              end;
            end;
          end;
        eaNodes:
          begin
            for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
            begin
              for LayerIndex := 0 to LayerMax do
              begin
                ANode3D := NodeArray[LayerIndex, NodeIndex];
                if ANode3D.Active then
                begin
                GetDataSetMeshValue(ThreeDDataSet, LayerIndex,
                  NodeIndex, StringValues, ShowColor, Fraction, MinMax);
                if ShowColor then
                begin
                  AColor := frmGoPhast.PhastModel.GridColorParameters.
                    FracToColor(Fraction);
                  ExtractColorComponents(AColor, Red, Green, Blue);
                  glColor3ub(Red, Green, Blue);
                  ANode3D.Draw3D;
                end;
                end;
              end;
            end;
          end;
        else Assert(False);

      end;
    finally
      glEndList
    end;
  finally
    StringValues.Free;
  end;
  FColorDataSetObserver.UpToDate := True;
  FNeedToRecordColoredCells := False;
end;

procedure TSutraMesh3D.Draw3D;
const
  NumberOfLists = 3;
var
  Colors: array[0..2] of GLint;
begin

  glDisable(GL_LIGHTING);
  glDisable(GL_LIGHT0);
  glMatrixMode(GL_MODELVIEW);

  glPushMatrix;
  try

    glEnable(GL_LINE_SMOOTH);
    Colors[0] := 0;
    Colors[1] := 0;
    Colors[2] := 0;
    glMaterialiv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @Colors);


    if FNeedToRedraw3d or FNeedToRecordLayer or FNeedToRecordCrossSection
      or (FNeedToRecordColoredCells and (ThreeDDataSet <> nil)) then
    begin
      if not FListsCreated then
      begin
        FLayerGLIndex := glGenLists(NumberOfLists);
        FCrossSectionGLIndex := Succ(FLayerGLIndex);
        FColoredCellsOrElementGLIndex := Succ(FCrossSectionGlIndex);
        FListsCreated := True;
      end;

      if FNeedToRecordLayer and frmGoPhast.acShowTopMesh.Checked then
      begin
        RecordLayer;
      end;

      if FNeedToRecordCrossSection and frmGoPhast.acShowFrontMesh.Checked then
      begin
        RecordCrossSection
      end;

      if FNeedToRecordColoredCells and (ThreeDDataSet <> nil) then
      begin
        RecordColoredCellsOrElements
      end;

      FNeedToRedraw3d := False;
    end;

    if not FNeedToRecordLayer and frmGoPhast.acShowTopMesh.Checked then
    begin
      glCallList(FLayerGLIndex);
    end;

    if not FNeedToRecordCrossSection and frmGoPhast.acShowFrontMesh.Checked then
    begin
      glCallList(FCrossSectionGLIndex);
    end;

    if ThreeDDataSet <> nil then
    begin
      glCallList(FColoredCellsOrElementGLIndex);
    end;

  finally
    glPopMatrix;
  end;

end;

procedure TSutraMesh3D.SetDefaultCrossSectionLocation;
begin
  CrossSection.Segment := DefaultCrossSectionLocation;
end;

function TSutraMesh3D.DefaultCrossSectionLocation: TSegment2D;
var
  MidY: Extended;
  Width: Double;
  Limits: TGridLimit;
begin
  Limits := MeshLimits(vdTop, 0);
  Width := Limits.MaxX - Limits.MinX;
  MidY := (Limits.MaxY + Limits.MinY) / 2;
  result[1].x := Limits.MinX - Width / 10;
  result[2].x := Limits.MaxX + Width / 10;
  result[1].y := MidY;
  result[2].y := MidY;
end;

procedure TSutraMesh3D.DeleteUnconnectedNodes;
begin
  Mesh2D.DeleteUnconnectedNodes;
end;

procedure TSutraMesh3D.UpdateNodeNumbers;
var
  index: Integer;
begin
  FNodeNumbers.Clear;
  for index := 0 to Nodes.Count - 1 do
  begin
    FNodeNumbers.Add.Value := Nodes[index].Number;
  end;
end;

function TSutraMesh3D.GetActiveElement(Index: Integer): IElement;
begin
  result := FActiveElements[Index];
end;

function TSutraMesh3D.GetActiveElementCount: integer;
begin
  result := FActiveElements.Count;
end;

function TSutraMesh3D.GetActiveNode(Index: Integer): INode;
begin
  result := FActiveNodes[Index];
end;

function TSutraMesh3D.GetActiveNodeCount: integer;
begin
  result := FActiveNodes.Count;
end;

function TSutraMesh3D.GetCanDraw: boolean;
begin
  result := FCanDraw and (frmGoPhast.PhastModel.DataSetUpdateCount = 0)
    and not FUpdatingElevations;
end;

function TSutraMesh3D.GetCanDraw3D: Boolean;
begin
  Result := FCanDraw3D and CanDraw
    and (MeshType = mt3D) and (Elements.Count > 0);
end;

function TSutraMesh3D.GetDrawElementCenters: boolean;
begin
  result := Mesh2D.DrawElementCenters;
end;

function TSutraMesh3D.GetDrawElementNumbers: Boolean;
begin
  result := Mesh2D.DrawElementNumbers;
end;

function TSutraMesh3D.GetDrawNodeNumbers: Boolean;
begin
  result := Mesh2D.DrawNodeNumbers;
end;

function TSutraMesh3D.GetElementArrayMember(Layer,
  Col: Integer): TSutraElement3D;
begin
  CheckUpdateElevations;
  result := FElementArray[Layer, Col];
  result.FLayer := Layer;
end;

function TSutraMesh3D.GetElementArrayMemberI(Layer, Col: Integer): IElement3D;
begin
  result := GetElementArrayMember(Layer, Col);
end;

function TSutraMesh3D.GetElementCenters(Angle: Double): TRbwQuadTree;
begin
  result := Elements.ElementCenters[Angle];
end;

function TSutraMesh3D.GetElementFont: TFont;
begin
  result := Mesh2D.ElementFont;
end;

function TSutraMesh3D.GetElementI(Index: Integer): IElement3D;
begin
  result := Elements[Index];
end;

//function TSutraMesh3D.GetElementNumbers: TIntegerCollection;
//var
//  index: Integer;
//begin
//end;

function TSutraMesh3D.FrontPolygons(Angle: Double;
  EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray): TCellElementPolygons2D;
var
  LayerIndex: Integer;
  ElementIndex: Integer;
  QuadPairList: TQuadPair3DList;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  result := nil;
  Limits := nil;
  CheckUpdateElevations;
  case EvaluatedAt of
    eaBlocks:
      begin
        if (Abs(Angle - FStoredBlockAngle) < 1e-10)
          and (FStoredBlockPolygons <> nil) then
        begin
          result := FStoredBlockPolygons;
          Limits := FStoredBlockLimits;
          Exit;
        end;
      end;
    eaNodes:
      begin
        if (Abs(Angle - FStoredNodeAngle) < 1e-10)
          and (FStoredNodePolygons <> nil) then
        begin
          result := FStoredNodePolygons;
          Limits := FStoredNodeLimits;
          Exit;
        end;
      end;
    else
      Assert(False);
  end;
  Screen.Cursor := crHourGlass;
  QuadPairList := TQuadPair3DList.Create;
  try
    case EvaluatedAt of
      eaBlocks:
        begin
          FreeAndNil(FElementIntervalTree);
          FreeAndNil(FElementRangeTree);
          SetLength(result, Length(FElementArray), Length(FElementArray[0]));
          SetLength(Limits, Length(FElementArray), Length(FElementArray[0]));
          for LayerIndex := 0 to Length(FElementArray) - 1 do
          begin
            for ElementIndex := 0 to Length(FElementArray[0]) - 1 do
            begin
              Element := ElementArray[LayerIndex, ElementIndex];
              if Element.Active then
              begin
                QuadPairList.Clear;
                QuadPairList.Add(Element.ElementShape);
                try
                  Result[LayerIndex,ElementIndex] :=
                    QuadPairsToPolygon(QuadPairList, Angle, Limits[LayerIndex,ElementIndex]);
                except
//                  ShowMessage(IntToStr(ElementIndex) + ' ' + IntToStr(LayerIndex));
                  raise;
                end;
              end
              else
              begin
                SetLength(Result[LayerIndex,ElementIndex], 0);
              end;
            end;
          end;
        end;
      eaNodes:
        begin
          FreeAndNil(FNodeIntervalTree);
          FreeAndNil(FNodeRangeTree);
          SetLength(result, Length(FNodeArray), Length(FNodeArray[0]));
          SetLength(Limits, Length(FNodeArray), Length(FNodeArray[0]));
          for LayerIndex := 0 to Length(FNodeArray) - 1 do
          begin
            for NodeIndex := 0 to Length(FNodeArray[0]) - 1 do
            begin
              Node := NodeArray[LayerIndex, NodeIndex];
              if Node.Active then
              begin
                QuadPairList.Clear;
                QuadPairList.Capacity := Node.FElements.Count;
                for ElementIndex := 0 to Node.FElements.Count - 1 do
                begin
                  if Node.FElements[ElementIndex].Active then
                  begin
                    QuadPairList.Add(Node.FElements[ElementIndex].CellSection(Node));
                  end;
                end;
                try
                  Result[LayerIndex,NodeIndex] :=
                    QuadPairsToPolygon(QuadPairList, Angle,
                    Limits[LayerIndex,NodeIndex]);
                except
                  begin
                  {$IFDEF DEBUG}
                    showMessage('TSutraNode3D.Number =' + IntToStr(Node.Number));
                  {$ENDIF}
                    raise;
                  end;
                end;
              end
              else
              begin
                SetLength(Result[LayerIndex,NodeIndex], 0);
              end;
            end;
          end;
        end;
      else Assert(False);
    end;
  finally
    case EvaluatedAt of
      eaBlocks:
        begin
          FStoredBlockAngle := Angle;
          FStoredBlockPolygons := result;
          FStoredBlockLimits := Limits;
        end;
      eaNodes:
        begin
          FStoredNodeAngle := Angle;
          FStoredNodePolygons := result;
          FStoredNodeLimits := Limits;
        end;
      else
        Assert(False);
    end;
    QuadPairList.Free;
    Screen.Cursor := crDefault;
  end;
end;

function TSutraMesh3D.GetLayerCount: Integer;
begin
  if frmGoPhast.PhastModel.SutraLayerStructure <> nil then
  begin
    result := frmGoPhast.PhastModel.SutraLayerStructure.LayerCount;
  end
  else
  begin
    result := 0;
  end;

end;

function TSutraMesh3D.GetMesh2D: TSutraMesh2D;
begin
  result := FMesh2D;
end;

function TSutraMesh3D.GetMesh2DI: IMesh2D;
begin
  result := GetMesh2D;
end;

function TSutraMesh3D.GetNeedToRecalculateFrontColors: Boolean;
begin
  result := FNeedToRecalculateFrontColors;
end;

function TSutraMesh3D.GetNeedToRecalculateTopColors: Boolean;
begin
  result := FNeedToRecalculateTopColors;
end;

function TSutraMesh3D.GetNodeArrayMember(Layer, Col: Integer): TSutraNode3D;
begin
  CheckUpdateElevations;
  if Length(FNodeArray) = 0 then
  begin
    result := nil;
  end
  else
  begin
    result := FNodeArray[Layer, Col];
  end;
end;

function TSutraMesh3D.GetNodeArrayMemberI(Layer, Col: Integer): INode3D;
begin
  result := GetNodeArrayMember(Layer, Col);
end;

function TSutraMesh3D.GetNodeFont: TFont;
begin
  result := Mesh2D.NodeFont;
end;

function TSutraMesh3D.GetNodeI(Index: Integer): INode3D;
begin
  result := Nodes[Index];
end;

function TSutraMesh3D.GetSelectedLayer: integer;
begin
  result := Mesh2D.SelectedLayer;
end;

function TSutraMesh3D.GetShortestHorizontalBlockEdge(Layer, Row,
  Column: Integer): double;
begin
  result := Mesh2D.ShortestHorizontalBlockEdge[Layer, Row, Column]
end;

function TSutraMesh3D.GetThreeDContourDataSet: TDataArray;
begin
  result := Mesh2D.ThreeDContourDataSet;
end;

function TSutraMesh3D.GetThreeDDataSet: TDataArray;
begin
  result := Mesh2D.ThreeDDataSet;
end;

function TSutraMesh3D.GetTopContourDataSet: TDataArray;
begin
  result := Mesh2D.TopContourDataSet;
end;

function TSutraMesh3D.GetTopDataSet: TDataArray;
begin
  result := Mesh2D.TopDataSet;
end;

function TSutraMesh3D.GetTopGridObserver: TObserver;
begin
  result := Mesh2D.TopGridObserver;
end;

function TSutraMesh3D.MeshLimits(ViewDirection: TViewDirection; Angle: double): TGridLimit;
var
//  Node: TSutraNode3D;
  NodeIndex: Integer;
  function RotatePoint(APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
  begin
    result := APoint;
    if (Angle <> 0) then
    begin
      temp.X := Cos(-Angle) * result.X - Sin(-Angle) * result.Y;
      temp.Y := Sin(-Angle) * result.X + Cos(-Angle) * result.Y;
      result := temp;
    end;
  end;
var
  ANode2D : TSutraNode2D;
  APoint: TPoint2D;
//  MinX: double;
//  MaxX: double;
//  NodeIndex: Integer;
  ANode3D: TSutraNode3D;
//  MinZ: Double;
//  MaxZ: Double;
  StartIndex: integer;
begin
  CheckUpdateElevations;
  if Mesh2D.Nodes.Count = 0 then
  begin
    result.MinX := 0;
    result.MaxX := 0;
    result.MinY := 0;
    result.MaxY := 0;
    result.MinZ := 0;
    result.MaxZ := 0;
    Exit;
  end;
  case ViewDirection of
    vdTop: result := Mesh2D.MeshLimits;
    vdFront:
      begin
        ANode2D := Mesh2D.Nodes[0];
        APoint := RotatePoint(ANode2D.Location);
        result.MinX := APoint.X;
        result.MaxX := APoint.X;
        for NodeIndex := 1 to Mesh2D.Nodes.Count - 1 do
        begin
          ANode2D := Mesh2D.Nodes[NodeIndex];
          APoint := RotatePoint(ANode2D.Location);
          if result.MinX > APoint.X then
          begin
            result.MinX := APoint.X;
          end
          else if result.MaxX < APoint.X then
          begin
            result.MaxX := APoint.X;
          end;
        end;
        StartIndex := Nodes.Count;
        for NodeIndex := 0 to Nodes.Count - 1 do
        begin
          ANode3D := Nodes[NodeIndex];
          if ANode3D.Active then
          begin
            result.MinZ := ANode3D.Z;
            result.MaxZ := ANode3D.Z;
            StartIndex := NodeIndex+ 1;
            break;
          end;
        end;
        for NodeIndex := StartIndex to Nodes.Count - 1 do
        begin
          ANode3D := Nodes[NodeIndex];
          if ANode3D.Active then
          begin
            if result.MinZ > ANode3D.Z then
            begin
              result.MinZ := ANode3D.Z;
            end
            else if result.MaxZ < ANode3D.Z then
            begin
              result.MaxZ := ANode3D.Z;
            end;
          end;
        end;
      end;
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

function TSutraMesh3D.OkLocation(const DataSet: TDataArray; const Layer, Row,
  Col: integer): boolean;
var
  Element: TSutraElement3D;
  Node: TSutraNode3D;
begin
  result := inherited;
  if result then
  begin
    if (MeshType in [mt2D, mtProfile]) or (DataSet.Orientation <> dso3D) then
    begin
      Exit;
    end;
    CheckUpdateElevations;
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          Element := ElementArray[Layer, Col];
          result := (Element <> nil) and Element.Active;
        end;
      eaNodes:
        begin
          Node := NodeArray[Layer, Col];
          result := (Node <> nil) and Node.Active;
        end;
      else Assert(False);
    end;
  end;
end;

function TSutraMesh3D.RotateFromMeshCoordinatesToRealWorldCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
  MeshAngle: Double;
begin
  result := APoint;
  MeshAngle := CrossSection.Angle;
  if (MeshType in [mt2D, mtProfile]) or (MeshAngle <> 0) then
  begin
    temp.X := Cos(MeshAngle) * result.X - Sin(MeshAngle) * result.Y;
    temp.Y := Sin(MeshAngle) * result.X + Cos(MeshAngle) * result.Y;
    result := temp;
  end;
end;

function TSutraMesh3D.RotateFromRealWorldCoordinatesToMeshCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
  MeshAngle: Double;
begin
  result := APoint;
  MeshAngle := CrossSection.Angle;
  if (MeshType in [mt2D, mtProfile]) or (MeshAngle <> 0) then
  begin
    temp.X := Cos(-MeshAngle) * result.X - Sin(-MeshAngle) * result.Y;
    temp.Y := Sin(-MeshAngle) * result.X + Cos(-MeshAngle) * result.Y;
    result := temp;
  end;

end;

function TSutraMesh3D.FrontMeshBox(ObjectAngle: double): TPolygon2D;
var
  Limits: TGridLimit;
begin
  result := nil;
  if (Nodes.Count = 0) or (Mesh2D.Nodes.Count = 0) then
  begin
    Exit;
  end;
  Limits := MeshLimits(vdFront, ObjectAngle);
  SetLength(Result, 4);
  result[0].x := Limits.MinX;
  result[0].y := Limits.MinZ;

  result[1].x := Limits.MinX;
  result[1].y := Limits.MaxZ;

  result[2].x := Limits.MaxX;
  result[2].y := Limits.MaxZ;

  result[3].x := Limits.MaxX;
  result[3].y := Limits.MinZ;
end;

function TSutraMesh3D.FrontMeshOutline: TMeshOutline;
begin
  { TODO -cSUTRA : Consider caching FFrontMeshOutline and only updating it when needed. }
  FFrontMeshOutline.Free;
  FFrontMeshOutline := TMeshOutline.Create(self, vdFront, 0);
  result := FFrontMeshOutline
end;

function TSutraMesh3D.FrontOutline: TOutline;
begin
  result := FrontMeshOutline
end;

function TSutraMesh3D.MeshBox(ViewDirection: TViewDirection; ObjectAngle: double): TPolygon2D;
begin
  case ViewDirection of
    vdTop: result := Mesh2D.MeshBox;
    vdFront: result := FrontMeshBox(ObjectAngle);
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

procedure TSutraMesh3D.MeshChanged;
begin
  if FMeshUpdate > 0 then Exit;

  CanDraw := True;

  if frmGoPhast.frame3DView.glWidModelView <> nil then
  begin
    FNeedToRedraw3d := True;
    FNeedToRecordLayer := True;
    FNeedToRecordCrossSection := True;
  end;
  frmGoPhast.EnableVisualization;
//  ViewsChanged;
end;

procedure TSutraMesh3D.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
end;

procedure TSutraMesh3D.SetCorrectElementOrientation;
begin
  Mesh2D.SetCorrectElementOrientation;
end;

procedure TSutraMesh3D.UpdateNodeArray;
var
  LayerIndex: Integer;
  NodeList: TSutraNode3D_List;
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  NodeList := TSutraNode3D_List.Create;
  try
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      NodeList.Clear;
      NodeList.Capacity := Mesh2D.Nodes.Count;
      for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
      begin
        ANode := FNodeArray[LayerIndex, NodeIndex];
        NodeList.Add(ANode);
      end;
      for NodeIndex := 0 to NodeList.Count - 1 do
      begin
        ANode := NodeList[NodeIndex];
        FNodeArray[LayerIndex, ANode.Node2D.Number] := ANode;
      end;
  //    LayerGroup := LocalModel.SutraLayerStructure[LayerGroupIndex];
    end;
  finally
    NodeList.Free;
  end;

end;

procedure TSutraMesh3D.AssignNodeElevations;
var
  LocalModel: TPhastModel;
  ANode: TSutraNode3D;
  IntermediateLayerIndex: Integer;
  MinimumThickness: Double;
  LayerGroup: TSutraLayerGroup;
  MiddleNode: TSutraNode3D;
  LayerGroupIndex: Integer;
  PriorLayerIndex: Integer;
  UnitHeight: TFloat;
  IntIndex: Integer;
  HigherNode: TSutraNode3D;
  DataArray: TDataArray;
  Fraction: Double;
  LayerFractions: TDoubleDynArray;
  Local_ColIndex: Integer;
  LayerIndex: Integer;
begin
  LocalModel := Model as TPhastModel;
  LayerIndex := -1;
  for LayerGroupIndex := 0 to LocalModel.SutraLayerStructure.Count - 1 do
  begin
    LayerGroup := LocalModel.SutraLayerStructure[LayerGroupIndex];
    MinimumThickness := LayerGroup.MinThickness;
    LayerFractions := LocalModel.LayerFractions(LayerGroup);
    DataArray := LocalModel.DataArrayManager.GetDataSetByName(LayerGroup.DataArrayName);
    Assert(DataArray <> nil);
    Assert(DataArray.Orientation = dsoTop);
    DataArray.Initialize;
    PriorLayerIndex := LayerIndex;
    LayerIndex := LayerIndex + LayerGroup.LayerCount;
    Assert(Length(LayerFractions) = LayerGroup.LayerCount - 1);
    for Local_ColIndex := 0 to Mesh2D.Nodes.Count - 1 do
    begin
      ANode := FNodeArray[LayerIndex, Local_ColIndex];
      ANode.Z := DataArray.RealData[0, 0, Local_ColIndex];
      if PriorLayerIndex >= 0 then
      begin
        HigherNode := FNodeArray[PriorLayerIndex, Local_ColIndex];
        UnitHeight := HigherNode.Z - ANode.Z;
        if UnitHeight < 0 then
        begin
          ANode.Z := HigherNode.Z;
          UnitHeight := 0;
        end;
        if UnitHeight < MinimumThickness then
        begin
          ANode.Z := HigherNode.Z - MinimumThickness;
          UnitHeight := MinimumThickness;
        end;
        for IntermediateLayerIndex := 0 to Length(LayerFractions) - 1 do
        begin
          IntIndex := PriorLayerIndex + IntermediateLayerIndex + 1;
          Assert(IntIndex < LayerIndex);
          MiddleNode := FNodeArray[IntIndex, Local_ColIndex];
          Fraction := LayerFractions[IntermediateLayerIndex];
          MiddleNode.Z := ANode.Z + UnitHeight * Fraction;
        end;
      end;
    end;
  end;
end;

procedure TSutraMesh3D.SetCrossSection(const Value: TMeshCrossSectionLine);
begin
  FCrossSection.Assign(Value);
end;

procedure TSutraMesh3D.SetDrawElementCenters(const Value: boolean);
begin
  Mesh2D.DrawElementCenters := Value;
end;

procedure TSutraMesh3D.SetDrawElementNumbers(const Value: Boolean);
begin
  Mesh2D.DrawElementNumbers := Value;
end;

procedure TSutraMesh3D.SetDrawNodeNumbers(const Value: Boolean);
begin
  Mesh2D.DrawNodeNumbers := Value;
end;

procedure TSutraMesh3D.SetElementDrawingChoice(const Value: TDrawingChoice);
begin
  FElementDrawingChoice := Value;
  Mesh2D.ElementDrawingChoice := Value;
end;

procedure TSutraMesh3D.SetElementFont(const Value: TFont);
begin
  Mesh2D.ElementFont := Value;
end;

procedure TSutraMesh3D.SetElementNumbers(const Value: TIntegerCollection);
begin
  FElementNumbers.Assign(Value);
end;

procedure TSutraMesh3D.SetElements(const Value: TSutraElement3D_Collection);
begin
  FElements.Assign(Value);
end;

procedure TSutraMesh3D.SetElevationsNeedUpdating(const Value: boolean);
begin
  FElevationsNeedUpdating := Value;
end;

procedure TSutraMesh3D.SetMesh2D(const Value: TSutraMesh2D);
begin
  FMesh2D.Assign(Value);
end;

procedure TSutraMesh3D.SetMeshType(const Value: TMeshType);
begin
  if FMeshType <> Value then
  begin
    FMeshType := Value;
    ElevationsNeedUpdating := True;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
    frmGoPhast.PhastModel.InvalidateSegments;
    InvalidateModel;
  end;

  if Assigned(OnMeshTypeChanged) then
  begin
    OnMeshTypeChanged(self);
  end;
end;

procedure TSutraMesh3D.SetNeedToRecalculateFrontColors(const Value: Boolean);
begin
  if Value then
  begin
    FNeedToRecalculateFrontColors := Value;
  end;
end;

procedure TSutraMesh3D.SetNeedToRecalculateTopColors(const Value: Boolean);
begin
  if Value then
  begin
    FNeedToRecalculateTopColors := Value;
  end;
end;

procedure TSutraMesh3D.SetNodeDrawingChoice(const Value: TDrawingChoice);
begin
  FNodeDrawingChoice := Value;
  Mesh2D.NodeDrawingChoice := Value;
end;

procedure TSutraMesh3D.SetNodeFont(const Value: TFont);
begin
  Mesh2D.NodeFont := Value;
end;

procedure TSutraMesh3D.SetNodeNumbers(const Value: TIntegerCollection);
begin
  FNodeNumbers.Assign(Value);
end;

procedure TSutraMesh3D.SetNodes(const Value: TSutraNode3D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraMesh3D.SetSelectedLayer(Value: integer);
begin
  if FSettingSelectedLayer then
  begin
    Exit;
  end;
  FSettingSelectedLayer := True;
  try
    if Value < 0 then
    begin
      Value := 0;
    end
    else
    begin
      if LayerCount > 0 then
      begin
        case frmGoPhast.frameTopView.EvaluatedAt of
          eaBlocks:
            begin
              if Value > LayerCount - 1 then
              begin
                Value := LayerCount - 1;
              end;
            end;
          eaNodes:
            begin
              if Value > LayerCount then
              begin
                Value := LayerCount;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    if Mesh2D.SelectedLayer <> Value then
    begin
      Mesh2D.SelectedLayer := Value;
      if Model <> nil then
      begin
        (Model as TPhastModel).CombinedDisplayLayer := Value;
        FNeedToRecordLayer := True;
        FNeedToRedraw3d := True;
        (Model as TPhastModel).TopContoursUpToDate := False;
    //    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    //    begin
    //      FDisplayLayer := Value;
    //    end;
    //    FRecordedTopGrid := False;
        if Assigned(FOnSelectedLayerChange) then
        begin
          FOnSelectedLayerChange(self);
        end;
      end;
    end;
  finally
    FSettingSelectedLayer := False;
  end;
end;

procedure TSutraMesh3D.SetThreeDContourDataSet(const Value: TDataArray);
begin
  Mesh2D.ThreeDContourDataSet := Value;
end;

procedure TSutraMesh3D.SetThreeDDataSet(const Value: TDataArray);
begin
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.FrontDiscretizationChanged := True;
  frmGoPhast.SideDiscretizationChanged := True;
  if Mesh2D.ThreeDDataSet <> Value then
  begin
    if Mesh2D.ThreeDDataSet <> nil then
    begin
      Mesh2D.ThreeDDataSet.StopsTalkingTo(FColorDataSetObserver);
    end;
    Mesh2D.ThreeDDataSet := Value;
    if Mesh2D.ThreeDDataSet <> nil then
    begin
      Mesh2D.ThreeDDataSet.TalksTo(FColorDataSetObserver);
    end;
    FNeedToRecordColoredCells := True;
  end;

end;

procedure TSutraMesh3D.SetThreeDGridObserver(const Value: TObserver);
begin
  FThreeDGridObserver := Value;
  if Assigned(FThreeDGridObserver) then
  begin
    FThreeDGridObserver.UpToDate := False;
    FThreeDGridObserver.UpToDate := True;
  end;
end;

procedure TSutraMesh3D.SetTopContourDataSet(const Value: TDataArray);
begin
  Mesh2D.TopContourDataSet := Value;
end;

procedure TSutraMesh3D.SetTopDataSet(const Value: TDataArray);
begin
  Mesh2D.TopDataSet := Value;
end;

procedure TSutraMesh3D.SetTopGridObserver(const Value: TObserver);
begin
  Mesh2D.TopGridObserver := Value;
end;

procedure TSutraMesh2D.SetThreeDContourDataSet(const Value: TDataArray);
begin
  if FThreeDContourDataSet <> Value then
  begin
    FThreeDContourDataSet := Value;
  end;
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.FrontDiscretizationChanged := True;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TSutraMesh2D.SetThreeDDataSet(const Value: TDataArray);
begin
  if FThreeDDataSet <> Value then
  begin
    FThreeDDataSet := Value;
    frmGoPhast.InvalidateImage32AllViews;
  end;
end;

procedure TSutraMesh2D.SetTopContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopContourDataSet <> Value then
  begin
    FTopContourDataSet := Value;
  end;
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TSutraMesh2D.SetTopDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopDataSet <> Value then
  begin
    FTopDataSet := Value;
    frmGoPhast.TopDiscretizationChanged := True;
    frmGoPhast.InvalidateImage32AllViews;
  end;
end;

procedure TSutraMesh2D.SetTopGridObserver(const Value: TObserver);
begin
  FTopGridObserver := Value;
  if Assigned(FTopGridObserver) then
  begin
    FTopGridObserver.UpToDate := False;
    FTopGridObserver.UpToDate := True;
  end;
end;

function TSutraMesh3D.TopContainingCellOrElement(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  result := Mesh2D.TopContainingCellOrElement(APoint, EvaluatedAt)
end;

function TSutraMesh3D.TopMeshOutline(Layer: integer): TMeshOutline;
begin
  { TODO -cSUTRA : Consider caching FTopMeshOutline and only updating it when needed. }
  FTopMeshOutline.Free;
  FTopMeshOutline := TMeshOutline.Create(self, vdTop, Layer);
  result := FTopMeshOutline
end;

function TSutraMesh3D.TopOutline(Layer: integer): TOutline;
begin
  result := TopMeshOutline(Layer);
end;

procedure TSutraMesh3D.UpdateElementNumbers;
var
  index: Integer;
begin
  FElementNumbers.Clear;
  for index := 0 to Elements.Count - 1 do
  begin
    FElementNumbers.Add.Value := Elements[index].ElementNumber;
  end;
end;

procedure TSutraMesh3D.UpdateElementsInNodes;
var
  NodeIndex: Integer;
  ElementIndex: Integer;
begin
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Nodes[NodeIndex].FElements.Clear;
  end;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    Elements[ElementIndex].UpdateNodes;
  end;
end;

procedure TSutraMesh3D.UpdateElevations;
var
  LocalModel: TPhastModel;
  LayerIndex: Integer;
  ColIndex: Integer;
  NodeLayerCount: Integer;
  ANode: TSutraNode3D;
  ElementLayerCount: integer;
  AnElement: TSutraElement3D;
  Element2D: TSutraElement2D;
  NodeItem: TSutraNodeNumber3D_Item;
  NodeIndex: Integer;
  ANode2D: TSutraNodeNumber2D_Item;
  Node3D: TSutraNode3D;
  Active: Boolean;
  Node3D_1: TSutraNodeNumber3D_Item;
  Node3D_2: TSutraNodeNumber3D_Item;
  ElementIndex: Integer;
  NodeActive: Boolean;
  TempActive: Boolean;
  NodeAbove: TSutraNode3D;
  ActiveNodeDataArray: TDataArray;
  NodeShouldBeActive: Boolean;
begin
  if FUpdatingElevations or not (frmGoPhast.ModelSelection in SutraSelection) then
  begin
    Exit;
  end;

  FUpdatingElevations := True;
  try
    InvalidatePolygons;

    frmGoPhast.PhastModel.UpdateDataSetDimensions;
    frmGoPhast.PhastModel.InvalidateScreenObjects;

    LocalModel := Model as TPhastModel;
    Elements.Clear;
    Nodes.Clear;
    BeginUpdate;
    try
      if Mesh2D.Nodes.Count = 0 then
      begin
        Exit;
      end;
      if MeshType in [mt2D, mtProfile] then
      begin
        Exit;
      end
      else
      begin
    //    try
        if LocalModel.SutraLayerStructure.Count <= 1 then
        begin
          Exit;
        end;
        NodeLayerCount := LocalModel.SutraLayerStructure.NodeLayerCount;
        SetLength(FNodeArray, NodeLayerCount, Mesh2D.Nodes.Count);
        for LayerIndex := 0 to NodeLayerCount - 1 do
        begin
          for ColIndex := 0 to Mesh2D.Nodes.Count - 1 do
          begin
            FNodeArray[LayerIndex, ColIndex] := Nodes.Add;
            FNodeArray[LayerIndex, ColIndex].Node2D_Number := ColIndex;
          end;
        end;
        AssignNodeElevations;
      end;

      for NodeIndex := 0 to Nodes.Count - 1 do
      begin
        Node3D := Nodes[NodeIndex];
        Node3D.Active := False;
      end;

      ElementLayerCount := LocalModel.SutraLayerStructure.ElementLayerCount;
      SetLength(FElementArray, ElementLayerCount, Mesh2D.Elements.Count);
      for LayerIndex := 0 to ElementLayerCount - 1 do
      begin
        for ColIndex := 0 to Mesh2D.Elements.Count - 1 do
        begin
          Element2D := Mesh2D.Elements[ColIndex];
          AnElement := Elements.Add;
          AnElement.Element2D := Element2D;
          FElementArray[LayerIndex, ColIndex] := AnElement;
          for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
          begin
            ANode2D := Element2D.Nodes[NodeIndex];
            Node3D := FNodeArray[LayerIndex, ANode2D.NodeNumber];
            NodeItem := AnElement.Nodes.Add;
            NodeItem.Node := Node3D;
          end;
          for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
          begin
            ANode2D := Element2D.Nodes[NodeIndex];
            Node3D := FNodeArray[LayerIndex+1, ANode2D.NodeNumber];
            NodeItem := AnElement.Nodes.Add;
            NodeItem.Node := Node3D;
          end;
          Active := True;
          for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
          begin
//            ANode2D := Element2D.Nodes[NodeIndex];
            Node3D_1 := AnElement.Nodes[NodeIndex];
            Node3D_2 := AnElement.Nodes[NodeIndex + Element2D.Nodes.Count];
            NodeActive := (Node3D_1.Node.Z > Node3D_2.Node.Z);
            Node3D_1.Node.Active := (Node3D_1.Node.Active or NodeActive);
            Node3D_2.Node.Active := (Node3D_2.Node.Active or NodeActive);
            if not NodeActive then
            begin
              Active := False;
            end;
          end;
          AnElement.Active := Active;
//          if Active then
//          begin
//            for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
//            begin
//              AnElement.Nodes[NodeIndex].Node.Active := True;
//            end;
//          end;
        end;
      end;




      if not Loading
        and (CrossSection.StartX = CrossSection.EndX)
        and (CrossSection.StartY = CrossSection.EndY)
        then
      begin
        SetDefaultCrossSectionLocation;
      end;
    finally
      EndUpdate;
    end;
    UpdateElementsInNodes;

    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      Node3D := Nodes[NodeIndex];
      if Node3D.Active then
      begin
        TempActive := False;
        for ElementIndex := 0 to Node3D.FElements.Count - 1 do
        begin
          AnElement := Node3D.FElements[ElementIndex];
          if AnElement.Active then
          begin
            TempActive := True;
            break;
          end;
        end;
        Node3D.Active := TempActive;
      end;
    end;

    ActiveNodeDataArray := LocalModel.DataArrayManager.GetDataSetByName(KNodeActive);
    ActiveNodeDataArray.Initialize;


    for ColIndex := 0 to Mesh2D.Nodes .Count - 1 do
    begin
      NodeAbove := nil;
      for LayerIndex := 0 to LayerCount do
      begin
        NodeShouldBeActive := ActiveNodeDataArray.BooleanData[LayerIndex, 0, ColIndex];
        Node3D := NodeArray[LayerIndex, ColIndex];
        Node3D.FTop := Node3D.Z;
        Node3D.FBottom := Node3D.Z;
        if not NodeShouldBeActive  then
        begin
          Node3D.Active := False;
        end;
        if Node3D.Active and (NodeAbove <> nil) and NodeAbove.Active then
        begin
          Node3D.FTop := (NodeAbove.Z + Node3D.Z)/2;
          NodeAbove.FBottom := Node3D.FTop;
        end;

        if not NodeShouldBeActive then
        begin

          for ElementIndex := 0 to Node3D.FElements.Count - 1 do
          begin
            AnElement := Node3D.FElements[ElementIndex];
            AnElement.Active := False;
//            if AnElement.Active then
//            begin
//              TempActive := True;
//              break;
//            end;
          end;
        end;

        NodeAbove := Node3D
      end;
    end;

    FActiveNodes.Clear;
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex];
      ANode.UpdateActiveElementList;
      if ANode.FActiveElements.Count = 0 then
      begin
        ANode.Active := False;
      end;
      if ANode.Active then
      begin
        FActiveNodes.Add(ANode);
      end;
    end;

    FActiveElements.Clear;
    for ElementIndex := 0 to Elements.Count - 1 do
    begin
      AnElement := Elements[ElementIndex];
      AnElement.UpdateActiveNodeList;
      if AnElement.Active then
      begin
        FActiveElements.Add(AnElement);
      end;
    end;

    SimpleRenumber;
    RestoreNodeNumbers;
    RestoreElementNumbers;

//    RenumberMesh(self);

  finally
    FUpdatingElevations := False;
    FNeedToRecordLayer := True;
    FNeedToRecordCrossSection := True;
    FNeedToRedraw3d := True;
  end;
end;

procedure TSutraMesh3D.SimpleRenumber;
var
  Count: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  ANode: TSutraNode3D;
  AnElement: TSutraElement3D;
begin
  CheckUpdateElevations;
  if MeshType = mt3D then
  begin
    Count := 0;
    if Mesh2D.Nodes.Count > LayerCount then
    begin
      for ColIndex := 0 to Mesh2D.Nodes.Count - 1 do
      begin
        for LayerIndex := 0 to LayerCount do
        begin
          ANode := NodeArray[LayerIndex, ColIndex];
          if (ANode <> nil) and ANode.Active then
          begin
            ANode.Number := Count;
            ANode.Index := ANode.Number;
            Inc(Count);
          end;
        end;
      end;
    end
    else
    begin
      for LayerIndex := 0 to LayerCount do
      begin
        for ColIndex := 0 to Mesh2D.Nodes.Count - 1 do
        begin
          ANode := NodeArray[LayerIndex, ColIndex];
          if (ANode <> nil) and ANode.Active then
          begin
            ANode.Number := Count;
            ANode.Index := ANode.Number;
            Inc(Count);
          end;
        end;
      end;
    end;
    Count := 0;
    if Mesh2D.Elements.Count > LayerCount-1 then
    begin
      for ColIndex := 0 to Mesh2D.Elements.Count - 1 do
      begin
        for LayerIndex := 0 to LayerCount-1 do
        begin
          AnElement := ElementArray[LayerIndex, ColIndex];
          if (AnElement <> nil) and AnElement.Active then
          begin
            AnElement.ElementNumber := Count;
            AnElement.Index := AnElement.ElementNumber;
            Inc(Count);
          end;
        end;
      end;
    end
    else
    begin
      for LayerIndex := 0 to LayerCount-1 do
      begin
        for ColIndex := 0 to Mesh2D.Elements.Count - 1 do
        begin
          AnElement := ElementArray[LayerIndex, ColIndex];
          if (AnElement <> nil) and AnElement.Active then
          begin
            AnElement.ElementNumber := Count;
            AnElement.Index := AnElement.ElementNumber;
            Inc(Count);
          end;
        end;
      end;
    end;
  end;
end;
{ TSutraNodeNumber3D_Item }

function TSutraNodeNumber3D_Item.GetNode: TSutraNode3D;
begin
  result := inherited Node as TSutraNode3D;
end;

function TSutraNodeNumber3D_Item.Model: TBaseModel;
begin
  result := (Collection as TSutraNodeNumber3D_Collection).Model;
end;

procedure TSutraNodeNumber3D_Item.SetNode(const Value: TSutraNode3D);
begin
  inherited SetNode(Value);
end;

procedure TSutraNodeNumber3D_Item.UpdateNode;
var
  LocalModel: TCustomModel;
  Mesh: TSutraMesh3D;
begin
  LocalModel := Model as TCustomModel;
  Mesh := LocalModel.SutraMesh;
  if (FStoredNodeNumber >= 0) and (FStoredNodeNumber < Mesh.Nodes.Count) then
  begin
    FNode := Mesh.Nodes[FStoredNodeNumber];
  end
  else
  begin
    FNode := nil;
  end;
end;

{ TSutraElement3D_Collection }

function TSutraElement3D_Collection.Add: TSutraElement3D;
begin
  result := inherited Add as TSutraElement3D;
end;

constructor TSutraElement3D_Collection.Create(Model: TBaseModel;
  ParentMesh: TSutraMesh3D);
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
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TSutraElement3D, InvalidateModelEvent, ParentMesh);
  FAngle := -1000;
  FStoredRotatedLocations:= TStoredLocations.Create;
end;

destructor TSutraElement3D_Collection.Destroy;
begin
  FStoredRotatedLocations.Free;
  FRotatedElementCenters.Free;
  inherited;
end;

function TSutraElement3D_Collection.GetElementCenters(
  Angle: Double): TRbwQuadTree;
var
  index: integer;
  AnElement: TSutraElement3D;
  CenterPoint: TPoint3D;
  StoredPoint: TStoredNodeOrElement;
  LayerIndex: Integer;
  ColIndex: Integer;
  RandomIndex: integer;
begin
  if Angle <> FAngle then
  begin
    FreeAndNil(FRotatedElementCenters);
    FAngle := Angle
  end;
  if FRotatedElementCenters = nil then
  begin
    FStoredRotatedLocations.Clear;
    FRotatedElementCenters := TRbwQuadTree.Create(nil);

    for LayerIndex := 0 to FMesh.LayerCount - 1 do
    begin
      for ColIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
      begin
        AnElement := FMesh.ElementArray[LayerIndex,ColIndex];
        if AnElement.Active then
        begin
          CenterPoint := AnElement.CenterLocation;
          StoredPoint := TStoredNodeOrElement.Create(
            CenterPoint, FAngle, ColIndex, LayerIndex, AnElement);
          FStoredRotatedLocations.Add(StoredPoint);
        end;
      end;
    end;

    if FStoredRotatedLocations.Count > 0 then
    begin
      StoredPoint := FStoredRotatedLocations[0];
      FRotatedElementCenters.XMin := StoredPoint.X;
      FRotatedElementCenters.XMax := StoredPoint.X;
      FRotatedElementCenters.YMin := StoredPoint.Z;
      FRotatedElementCenters.YMax := StoredPoint.Z;
      for Index := 1 to Min(100, FStoredRotatedLocations.Count - 1) do
      begin
        RandomIndex := Random(FStoredRotatedLocations.Count);
        StoredPoint := FStoredRotatedLocations[RandomIndex];
        if FRotatedElementCenters.XMin > StoredPoint.X then
        begin
          FRotatedElementCenters.XMin := StoredPoint.X;
        end
        else if FRotatedElementCenters.XMax < StoredPoint.X then
        begin
          FRotatedElementCenters.XMax := StoredPoint.X;
        end;

        if FRotatedElementCenters.YMin > StoredPoint.Z then
        begin
          FRotatedElementCenters.YMin := StoredPoint.Z;
        end
        else if FRotatedElementCenters.YMax < StoredPoint.Z then
        begin
          FRotatedElementCenters.YMax := StoredPoint.Z;
        end;
      end;
      for Index := 0 to FStoredRotatedLocations.Count - 1 do
      begin
        StoredPoint := FStoredRotatedLocations[Index];
        FRotatedElementCenters.AddPoint(StoredPoint.X, StoredPoint.Z, StoredPoint);
      end;
    end;
  end;
  result := FRotatedElementCenters;
end;

function TSutraElement3D_Collection.GetItems(Index: integer): TSutraElement3D;
begin
  result := inherited Items[index] as TSutraElement3D;
end;

procedure TSutraElement3D_Collection.InvalidateStoredLocations;
begin
  FreeAndNil(FRotatedElementCenters);
end;

{ TCustomSutraItem }

procedure TCustomSutraItem.BeginUpdate;
begin
  inherited;
  Collection.BeginUpdate;
end;

procedure TCustomSutraItem.EndUpdate;
begin
  Collection.EndUpdate;
  inherited;
end;

function TCustomSutraItem.GetBypassUpdate: Boolean;
begin
  result := FBypassUpdate;
end;

procedure TCustomSutraItem.SetBypassUpdate(const Value: Boolean);
begin
  FBypassUpdate := Value;
end;

{ TCustomSutraCollection }

procedure TCustomSutraCollection.BeginUpdate;
begin
  inherited;
  if FMesh = nil then
  begin
    raise Exception.Create('Error in ' + ClassName);
//    showMessage(ClassName);
  end;
  FMesh.BeginUpdate;
end;

constructor TCustomSutraCollection.Create(ItemClass: TCollectionItemClass;
  InvalidateModelEvent: TNotifyEvent; Mesh: TSutraMesh3D);
begin
  inherited Create(ItemClass, InvalidateModelEvent);
  FMesh := Mesh;
end;

procedure TCustomSutraCollection.EndUpdate;
begin
  inherited;
  FMesh.EndUpdate;
end;

{ TSutraNode2Comparer }

function TSutraNode2DComparer.Compare(const Left, Right: TSutraNode2D): Integer;
var
  Angle: double;
  LeftX: FastGeo.TFloat;
  RightX: FastGeo.TFloat;
begin
  Angle := ArcTan2(Left.y - FStartPoint.y, Left.x - FStartPoint.x) - FAngle;
  LeftX := Distance(FStartPoint, Left.Location)*Cos(Angle);
  Angle := ArcTan2(Right.y - FStartPoint.y, Right.x - FStartPoint.x) - FAngle;
  RightX := Distance(FStartPoint, Right.Location)*Cos(Angle);
  result := Sign(LeftX - RightX);
end;

constructor TSutraNode2DComparer.Create(CrossSectionAngle: Double;
  StartPoint: TPoint2D);
begin
  FStartPoint := StartPoint;
  FAngle := CrossSectionAngle;
end;

{ TSutraElement2DComparer }

function TSutraElement2DComparer.Compare(const Left,
  Right: TSutraElement2D): Integer;
var
  Angle: double;
  LeftX: FastGeo.TFloat;
  RightX: FastGeo.TFloat;
  CenterPoint: TPoint2D;
begin
  CenterPoint := Left.Center;
  Angle := ArcTan2(CenterPoint.y - FStartPoint.y, CenterPoint.x - FStartPoint.x) - FAngle;
  LeftX := Distance(FStartPoint, CenterPoint)*Cos(Angle);

  CenterPoint := Right.Center;
  Angle := ArcTan2(CenterPoint.y - FStartPoint.y, CenterPoint.x - FStartPoint.x) - FAngle;
  RightX := Distance(FStartPoint, CenterPoint)*Cos(Angle);

  result := Sign(LeftX - RightX);
end;

constructor TSutraElement2DComparer.Create(CrossSectionAngle: Double;
  StartPoint: TPoint2D);
begin
  FStartPoint := StartPoint;
  FAngle := CrossSectionAngle;
end;

{ TSutraNode2DLeaf }

constructor TNode2DLeaf.Create(ANode: TSutraNode2D);
begin
  FNode := ANode;
end;

function TNode2DLeaf.GetCoordinate(Depth: integer): double;
begin
  result := 0;
  case Depth of
    0: result := FNode.MinX;
    1: result := FNode.MaxX;
    2: result := FNode.MinY;
    3: result := FNode.MaxY;
    else Assert(False);
  end;
end;

{ TSutraNode2DLeafList }

function TNode2DLeafList.CoordinateCount: integer;
begin
  Result := 4;
end;

constructor TNode2DLeafList.Create;
begin
  inherited;
{ $IFNDEF ErrorOnFree}
  OwnsObjects := True;
{ $ENDIF}
end;

{ TSutraElement2DLeaf }

constructor TElement2DLeaf.Create(AnElement: TSutraElement2D);
begin
  FElement := AnElement;
  FMinX := FElement.MinX;
  FMaxX := FElement.MaxX;
  FMinY := FElement.MinY;
  FMaxY := FElement.MaxY;
end;

function TElement2DLeaf.GetCoordinate(Depth: integer): double;
begin
  result := 0;
  case Depth of
    0: result := FMinX;
    1: result := FMaxX;
    2: result := FMinY;
    3: result := FMaxY;
    else Assert(False);
  end;
end;

{ TSutraElement2DLeafList }

function TElement2DLeafList.CoordinateCount: integer;
begin
  result := 4;
end;

constructor TElement2DLeafList.Create;
begin
  inherited;
  // previously, freeing the elements of the list causes an access violation in 32-bit
  // but not 64 bit
{ $IFNDEF ErrorOnFree}
  OwnsObjects := True;
{ $ENDIF}
end;

{ TFrontPolygon }

constructor TFrontPolygon.Create(ACol, ALayer: Integer; APolygon: TPolygon2D);
var
  PointIndex: integer;
begin
  FCol := ACol;
  FLayer := ALayer;
  FPolygon := APolygon;
  Assert(Length(FPolygon) > 0);
  FMinX := FPolygon[0].x;
  FMaxX := FPolygon[0].x;
  FMinY := FPolygon[0].y;
  FMaxY := FPolygon[0].y;
  for PointIndex := 1 to Length(FPolygon) - 1 do
  begin
    if FPolygon[PointIndex].x < FMinX then
    begin
      FMinX := FPolygon[PointIndex].x;
    end
    else if FPolygon[PointIndex].x > FMaxX then
    begin
      FMaxX := FPolygon[PointIndex].x;
    end;

    if FPolygon[PointIndex].y < FMinY then
    begin
      FMinY := FPolygon[PointIndex].y;
    end
    else if FPolygon[PointIndex].y > FMaxY then
    begin
      FMaxY := FPolygon[PointIndex].y;
    end;
  end;
end;

function TFrontPolygon.GetCoordinate(Depth: integer): double;
begin
  result := 0;
  case Depth of
    0: result := MinX;
    1: result := MaxX;
    2: result := MinY;
    3: result := MaxY;
    else Assert(False);
  end;
end;

{ TFrontPolygonList }

function TFrontPolygonList.CoordinateCount: integer;
begin
  result := 4;
end;

constructor TFrontPolygonList.Create;
begin
  inherited;
  OwnsObjects := True;
end;

{ TStoredNodeOrElement }

constructor TStoredNodeOrElement.Create(CenterPoint: TPoint3D; Angle: double;
  ACol, ALayer: Integer; AnObject: TObject);
var
  PointDistance: Extended;
  PointAngle: Extended;
begin
  PointDistance := sqrt(Sqr(CenterPoint.x) + Sqr(CenterPoint.y));
  PointAngle := ArcTan2(CenterPoint.y,CenterPoint.x);
  FX := Cos(PointAngle-Angle)*PointDistance;
  FY := Sin(PointAngle-Angle)*PointDistance;
  FZ := CenterPoint.Z;
  FCol := ACol;
  FLayer := ALayer;
  FNodeOrElement := AnObject;
end;

{ TMeshGenerationControls }

procedure TMeshGenerationControls.Apply;
begin
  C1 := SplittingAngle.Value;
  C2 := Structure.Value;
  C3 := NodePlacementError.Value;
  C4 := LineLength.Value;
  C5 := Symmetry.Value;
  C6 := Concave.Value;
//  AltC1 := AltSplittingAngle.Value;
//  AltC2 := AltStructure.Value;
//  AltC3 := AltNodePlacementError.Value;
//  AltC4 := AltLineLength.Value;
//  AltC5 := AltSymmetry.Value;
//  AltC6 := AltConcave.Value;
  QuadMeshGenerator.ElementGrowthRate := ElementGrowthRate.Value;

end;

procedure TMeshGenerationControls.Assign(Source: TPersistent);
var
  SourceControls: TMeshGenerationControls;
begin

  if Source is TMeshGenerationControls then
  begin
    SourceControls := TMeshGenerationControls(Source);
    SplittingAngle := SourceControls.SplittingAngle;
    Structure := SourceControls.Structure;
    NodePlacementError := SourceControls.NodePlacementError;
    LineLength := SourceControls.LineLength;
    Symmetry := SourceControls.Symmetry;
    Concave := SourceControls.Concave;
    NodePlacementError := SourceControls.NodePlacementError;
    ElementGrowthRate := SourceControls.ElementGrowthRate;
    MeshGenerationMethod := SourceControls.MeshGenerationMethod;
    RenumberingAlgorithm := SourceControls.RenumberingAlgorithm;
    GeomPackOptions := SourceControls.GeomPackOptions;
  end
  else
  begin
    inherited;
  end;
end;

constructor TMeshGenerationControls.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
//  if Model = nil then
//  begin
//    inherited Create(nil);
//  end
//  else
//  begin
//    inherited Create(Model.Invalidate);
//  end;

//  FAltConcave := TRealStorage.Create;
//  FAltLineLength := TRealStorage.Create;
//  FAltNodePlacementError := TRealStorage.Create;
//  FAltSplittingAngle := TRealStorage.Create;
//  FAltStructure := TRealStorage.Create;
//  FAltSymmetry := TRealStorage.Create;
  FConcave := TRealStorage.Create;
  FLineLength := TRealStorage.Create;
  FNodePlacementError := TRealStorage.Create;
  FSplittingAngle := TRealStorage.Create;
  FStructure := TRealStorage.Create;
  FSymmetry := TRealStorage.Create;
  FElementGrowthRate := TRealStorage.Create;
  FGeomPackOptions := TGeompackOptions.Create(ValueChanged);

  SetDefaults;

//  FAltConcave.OnChange := ValueChanged;
//  FAltLineLength.OnChange := ValueChanged;
//  FAltNodePlacementError.OnChange := ValueChanged;
//  FAltSplittingAngle.OnChange := ValueChanged;
//  FAltStructure.OnChange := ValueChanged;
//  FAltSymmetry.OnChange := ValueChanged;
  FConcave.OnChange := ValueChanged;
  FLineLength.OnChange := ValueChanged;
  FNodePlacementError.OnChange := ValueChanged;
  FSplittingAngle.OnChange := ValueChanged;
  FStructure.OnChange := ValueChanged;
  FSymmetry.OnChange := ValueChanged;
  FElementGrowthRate.OnChange := ValueChanged;

end;

destructor TMeshGenerationControls.Destroy;
begin
  FGeomPackOptions.Free;
  FElementGrowthRate.Free;
//  FAltLineLength.Free;
  FConcave.Free;
  FSymmetry.Free;
  FNodePlacementError.Free;
  FSplittingAngle.Free;
  FStructure.Free;
//  FAltConcave.Free;
//  FAltSymmetry.Free;
//  FAltNodePlacementError.Free;
  FLineLength.Free;
//  FAltSplittingAngle.Free;
//  FAltStructure.Free;
  inherited;
end;

//procedure TMeshGenerationControls.SetAltConcave(const Value: TRealStorage);
//begin
//  FAltConcave.Assign(Value);
//end;
//
//procedure TMeshGenerationControls.SetAltLineLength(const Value: TRealStorage);
//begin
//  FAltLineLength.Assign(Value);
//end;
//
//procedure TMeshGenerationControls.SetAltNodePlacementError(
//  const Value: TRealStorage);
//begin
//  FAltNodePlacementError.Assign(Value);
//end;
//
//procedure TMeshGenerationControls.SetAltSplittingAngle(
//  const Value: TRealStorage);
//begin
//  FAltSplittingAngle.Assign(Value);
//end;
//
//procedure TMeshGenerationControls.SetAltStructure(const Value: TRealStorage);
//begin
//  FAltStructure.Assign(Value);
//end;
//
//procedure TMeshGenerationControls.SetAltSymmetry(const Value: TRealStorage);
//begin
//  FAltSymmetry.Assign(Value);
//end;
//
procedure TMeshGenerationControls.SetConcave(const Value: TRealStorage);
begin
  FConcave.Assign(Value);
end;

procedure TMeshGenerationControls.SetDefaults;
begin
  SplittingAngle.Value := 0.52;
  Structure.Value := 0.17;
  NodePlacementError.Value := 0;
  LineLength.Value := 0.17;
  Symmetry.Value := 0.14;
  Concave.Value := 0.36;
//  AltSplittingAngle.Value := 0.56;
//  AltStructure.Value := 0.33;
//  AltNodePlacementError.Value := 0.11;
//  AltLineLength.Value := 0.0;
//  AltSymmetry.Value := 0.0;
//  AltConcave.Value := 1.2;
  ElementGrowthRate.Value := 1.05;
  MeshGenerationMethod := mgmUnknown;
  GeomPackOptions.Initialize;
end;

procedure TMeshGenerationControls.SetElementGrowthRate(
  const Value: TRealStorage);
begin
  FElementGrowthRate.Assign(Value);
end;

procedure TMeshGenerationControls.SetGeomPackOptions(
  const Value: TGeompackOptions);
begin
  FGeomPackOptions.Assign(Value);
end;

procedure TMeshGenerationControls.SetLineLength(const Value: TRealStorage);
begin
  FLineLength.Assign(Value);
end;

procedure TMeshGenerationControls.SetMeshGenerationMethod(
  const Value: TMeshGenerationMethod);
begin
  if FMeshGenerationMethod <> Value then
  begin
    FMeshGenerationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMeshGenerationControls.SetNodePlacementError(
  const Value: TRealStorage);
begin
  FNodePlacementError.Assign(Value);
end;

procedure TMeshGenerationControls.SetRenumberingAlgorithm(
  const Value: TRenumberingAlgorithm);
begin
  if FRenumberingAlgorithm <> Value then
  begin
    FRenumberingAlgorithm := Value;
    InvalidateModel;
  end;
end;

procedure TMeshGenerationControls.SetSplittingAngle(const Value: TRealStorage);
begin
  FSplittingAngle.Assign(Value);
end;

procedure TMeshGenerationControls.SetStructure(const Value: TRealStorage);
begin
  FStructure.Assign(Value);
end;

procedure TMeshGenerationControls.SetSymmetry(const Value: TRealStorage);
begin
  FSymmetry.Assign(Value);
end;

procedure TMeshGenerationControls.ValueChanged(Sender: TObject);
begin
  InvalidateModel;
end;

{ TSutraNode2DNumberComparer }

function TSutraNode2DNumberComparer.Compare(const Left,
  Right: TSutraNode2D): Integer;
begin
  result := Left.Number - Right.Number;
end;

{ TSutraElement2DNumberComparer }

function TSutraElement2DNumberComparer.Compare(const Left,
  Right: TSutraElement2D): Integer;
begin
  result := Left.ElementNumber - Right.ElementNumber;
end;

{ TMeshOutline }

//procedure TMeshOutline.GetTopPolygons(Mesh: TSutraMesh3D;
//  Layer: Integer; var UnionPolygon: TGpcPolygonClass);
//var
//  ElementIndex: Integer;
//  AnElement2D: TSutraElement2D;
//  ElementOK: Boolean;
//  NodeIndex: Integer;
//  ANode2D: TSutraNode2D;
//  ANode3D: TSutraNode3D;
//  ElementList: TSutraElement2D_List;
//  MeshElements: TGpcPolygonClass;
//  EmptyPolygon: TGpcPolygonClass;
//  PriorUnionPolygon: TGpcPolygonClass;
//begin
//  if Mesh.MeshType = mt3D then
//  begin
//    Assert(Layer >= 0);
//    Assert(Layer <= Mesh.LayerCount);
//  end;
//
//  ElementList := TSutraElement2D_List.Create;
//  try
//    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
//    begin
//      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
//      ElementOK := True;
//      if Mesh.MeshType = mt3D then
//      begin
//        for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
//        begin
//          ANode2D := AnElement2D.Nodes[NodeIndex].Node;
//          ANode3D := Mesh.NodeArray[Layer, ANode2D.Number];
//          ElementOK := ANode3D.Active;
//          if not ElementOK then
//          begin
//            break;
//          end;
//        end;
//      end;
//      if ElementOK then
//      begin
//        ElementList.Add(AnElement2D);
//      end;
//    end;
//    MeshElements := TGpcPolygonClass.Create;
//    EmptyPolygon := TGpcPolygonClass.Create;
//    PriorUnionPolygon := TGpcPolygonClass.Create;
//    try
//      EmptyPolygon.NumberOfContours := 0;
//      PriorUnionPolygon.NumberOfContours := 0;
//      MeshElements.NumberOfContours := 1;
//      for ElementIndex := 0 to ElementList.Count - 1 do
//      begin
//        AnElement2D := ElementList[ElementIndex];
//        MeshElements.VertexCount[0] := AnElement2D.Nodes.Count;
//        for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
//        begin
//          ANode2D := AnElement2D.Nodes[NodeIndex].Node;
//          MeshElements.Vertices[0, NodeIndex]
//            := ANode2D.Location;
//        end;
//        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
//          MeshElements, PriorUnionPolygon);
//        FreeAndNil(PriorUnionPolygon);
//        PriorUnionPolygon := UnionPolygon;
//      end;
////      UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
////        MeshElements, EmptyPolygon);
//      PriorUnionPolygon := nil;
//      While (UnionPolygon.NumberOfContours > 1) and ((PriorUnionPolygon = nil)
//        or (PriorUnionPolygon.NumberOfContours > UnionPolygon.NumberOfContours)) do
//      begin
//        PriorUnionPolygon.Free;
//        PriorUnionPolygon := UnionPolygon;
//        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
//          PriorUnionPolygon, EmptyPolygon);
//      end;
//    finally
//      PriorUnionPolygon.Free;
//      MeshElements.Free;
//      EmptyPolygon.Free;
//    end;
//  finally
//    ElementList.Free;
//  end;
//end;

procedure TMeshOutline.GetFrontPolygons(Mesh: TSutraMesh3D;
  var UnionPolygon: TGpcPolygonClass);
var
  NodeList: TSutraNode2D_List;
  ALine: TLine2D;
  ClosestPoint: TPoint2D;
//  OffSet: TFloat;
  StartPoint: TPoint2D;
  Node2D_Index: Integer;
  Node2D: TSutraNode2D;
  Angle: Extended;
  X_Float: FastGeo.TFloat;
  XDistances: TList<FastGeo.TFloat>;
  PolygonList: TList<TPolygon2D>;
  Node2D_1: TSutraNode2D;
  Node2D_2: TSutraNode2D;
  Node3D_1: TSutraNode3D;
  Node3D_2: TSutraNode3D;
  Node3D_3: TSutraNode3D;
  Node3D_4: TSutraNode3D;
  SegmentAngle: Double;
  LayerIndex: Integer;
  ElementOutLine: TPolygon2D;
  NodeIndex: Integer;
  MeshElements: TGpcPolygonClass;
  EmptyPolygon: TGpcPolygonClass;
  ElementIndex: Integer;
  APolygon: TPolygon2D;
begin
  Assert(Mesh.MeshType = mt3D);
  NodeList := TSutraNode2D_List.Create;
  try
    Mesh.GetNodesOnCrossSection(NodeList);

    ALine := EquateLine(Mesh.CrossSection.StartPoint, Mesh.CrossSection.EndPoint);

    ClosestPoint := ClosestPointOnLineFromPoint(
      ALine, EquatePoint(0.0, 0.0));

    SegmentAngle := Mesh.CrossSection.Angle;
    StartPoint := EquatePoint(0.0, 0.0);


    // Determine the locations of the outlines of the elements in cross section.
    XDistances := TList<FastGeo.TFloat>.Create;
    PolygonList := TList<TPolygon2D>.Create;
    try
      XDistances.Capacity := NodeList.Count;
      for Node2D_Index := 0 to NodeList.Count - 1 do
      begin
        Node2D := NodeList[Node2D_Index];
        Angle := ArcTan2(Node2D.y - StartPoint.y,
          Node2D.x - StartPoint.x) - SegmentAngle;
        X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
          + StartPoint.x;
        XDistances.Add(X_Float);
      end;

      PolygonList.Capacity := (NodeList.Count-1)*Mesh.LayerCount;
      for Node2D_Index := 0 to NodeList.Count - 2 do
      begin
        Node2D_1 := NodeList[Node2D_Index];
        Node2D_2 := NodeList[Node2D_Index+1];
        for LayerIndex := 0 to Mesh.LayerCount-1 do
        begin
          Node3D_1 := Mesh.NodeArray[LayerIndex,Node2D_1.Number];
          Node3D_2 := Mesh.NodeArray[LayerIndex+1,Node2D_1.Number];
          Node3D_3 := Mesh.NodeArray[LayerIndex+1,Node2D_2.Number];
          Node3D_4 := Mesh.NodeArray[LayerIndex,Node2D_2.Number];
          if Node3D_1.Active and Node3D_2.Active
            and Node3D_3.Active and Node3D_4.Active then
          begin
            // Make each polygon a separate array by calling SetLength.
            SetLength(ElementOutLine,4);

            ElementOutLine[0].x := XDistances[Node2D_Index];
            ElementOutLine[0].Y := Node3D_1.Z;
            ElementOutLine[1].x := XDistances[Node2D_Index];
            ElementOutLine[1].Y := Node3D_2.Z;
            ElementOutLine[2].x := XDistances[Node2D_Index+1];
            ElementOutLine[2].Y := Node3D_3.Z;
            ElementOutLine[3].x := XDistances[Node2D_Index+1];
            ElementOutLine[3].Y := Node3D_4.Z;
            PolygonList.Add(ElementOutLine);
          end;
        end;
      end;

      MeshElements := TGpcPolygonClass.Create;
      EmptyPolygon := TGpcPolygonClass.Create;
      try
        EmptyPolygon.NumberOfContours := 0;
        MeshElements.NumberOfContours := PolygonList.Count;
        for ElementIndex := 0 to PolygonList.Count - 1 do
        begin
          APolygon := PolygonList[ElementIndex];
          MeshElements.VertexCount[ElementIndex] := Length(APolygon);
          for NodeIndex := 0 to Length(APolygon) -1 do
          begin
            MeshElements.Vertices[ElementIndex, NodeIndex]
              := APolygon[NodeIndex];
          end;
        end;
        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
          MeshElements, EmptyPolygon);
      finally
        MeshElements.Free;
        EmptyPolygon.Free;
      end;

    finally
      XDistances.Free;
      PolygonList.Free;
    end;


  finally
    NodeList.Free;
  end
end;

procedure TMeshOutline.GetTopPolygons(Mesh: TSutraMesh3D;
  Layer: Integer; var UnionPolygon: TGpcPolygonClass);
var
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  ElementOK: Boolean;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  ANode3D: TSutraNode3D;
  OuterNodeIndex: integer;
  NodePosition: integer;
  AnotherNode: TSutraNode2D;
  NeighborNodePosition: integer;
  EdgeNodes: TSutraNode2D_List;
  Polygons: TObjectList<TSutraNode2D_List>;
  APolyList: TSutraNode2D_List;
  PriorNode: TSutraNode2D;
  FirstNode: TSutraNode2D;
  NeighborIndex: Integer;
  PolyIndex: Integer;
  Exaggeration: Double;
  function ConvertYLocation(APoint: TPoint2d): TPoint2d;
  begin
    result.x := APoint.x;
    result.y := APoint.y * Exaggeration;
  end;
begin
  if Mesh.MeshType = mt3D then
  begin
    Assert(Layer >= 0);
    Assert(Layer <= Mesh.LayerCount);
  end;
  Exaggeration := 1;
  if Mesh.MeshType = mtProfile then
  begin
    Exaggeration := (Mesh.Model as TPhastModel).Exaggeration;
    if Exaggeration = 0 then
    begin
      Exaggeration := 1;
    end;
  end;

  try
    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
    begin
      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
      ElementOK := True;
      if Mesh.MeshType = mt3D then
      begin
        for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
        begin
          ANode2D := AnElement2D.Nodes[NodeIndex].Node;
          ANode3D := Mesh.NodeArray[Layer, ANode2D.Number];
          ElementOK := ANode3D.Active;
          if not ElementOK then
          begin
            break;
          end;
        end;
      end;
      AnElement2D.FOkElement := ElementOK;
    end;

    EdgeNodes := TSutraNode2D_List.Create;
    try
      for OuterNodeIndex := 0 to Mesh.Mesh2D.Nodes.Count - 1 do
      begin
        ANode2D := Mesh.Mesh2D.Nodes[OuterNodeIndex];
        ANode2D.FNeighborNodes.Clear;
        for ElementIndex := 0 to ANode2D.ElementCount - 1 do
        begin
          AnElement2D := ANode2D.Elements[ElementIndex];
          NodePosition := AnElement2D.FNodes.IndexOfNode(ANode2D);

          if NodePosition > 0 then
          begin
            AnotherNode := AnElement2D.FNodes[NodePosition-1].Node;
          end
          else
          begin
            AnotherNode := AnElement2D.FNodes[3].Node;
          end;
          NeighborNodePosition := ANode2D.FNeighborNodes.IndexOf(AnotherNode);
          if NeighborNodePosition >= 0 then
          begin
            ANode2D.FNeighborNodes.Delete(NeighborNodePosition);
          end
          else
          begin
            ANode2D.FNeighborNodes.Add(AnotherNode);
          end;

          if NodePosition < 3 then
          begin
            AnotherNode := AnElement2D.FNodes[NodePosition+1].Node;
          end
          else
          begin
            AnotherNode := AnElement2D.FNodes[0].Node;
          end;
          NeighborNodePosition := ANode2D.FNeighborNodes.IndexOf(AnotherNode);
          if NeighborNodePosition >= 0 then
          begin
            ANode2D.FNeighborNodes.Delete(NeighborNodePosition);
          end
          else
          begin
            ANode2D.FNeighborNodes.Add(AnotherNode);
          end;

        end;

        if ANode2D.FNeighborNodes.Count > 0 then
        begin
          EdgeNodes.Add(ANode2D);
        end;
      end;

      Polygons := TObjectList<TSutraNode2D_List>.Create;
      try
        while EdgeNodes.Count > 0 do
        begin
          APolyList := TSutraNode2D_List.Create;
          Polygons.Add(APolyList);
          ANode2D := EdgeNodes[0];
          EdgeNodes[0] := nil;
          APolyList.Add(ANode2D);
          PriorNode := nil;
          FirstNode := ANode2D;
          repeat
            AnotherNode := nil;
            for NeighborIndex := 0 to ANode2D.FNeighborNodes.Count - 1 do
            begin
              AnotherNode := ANode2D.FNeighborNodes[NeighborIndex];
              if AnotherNode = PriorNode then
              begin
                Continue;
              end;
              if (APolyList.Count > 1) and (AnotherNode = FirstNode) then
              begin
                Break;
              end;
              NodePosition := EdgeNodes.IndexOf(AnotherNode);
              Assert(NodePosition >= 0);
              EdgeNodes[NodePosition] := nil;
              APolyList.Add(AnotherNode);
              PriorNode := ANode2D;
              ANode2D := AnotherNode;
              break;
            end;
            Assert(AnotherNode <> nil);
            if (APolyList.Count > 1) and (AnotherNode = FirstNode) then
            begin
              EdgeNodes.Pack;
              break;
            end;
          until (False);
        end;
        UnionPolygon := TGpcPolygonClass.Create;
        UnionPolygon.NumberOfContours := Polygons.Count;
        for PolyIndex := 0 to Polygons.Count - 1 do
        begin
          APolyList := Polygons[PolyIndex];
          UnionPolygon.VertexCount[PolyIndex] := APolyList.Count;
          for NodeIndex := 0 to APolyList.Count - 1 do
          begin
            ANode2D := APolyList[NodeIndex];
            UnionPolygon.Vertices[PolyIndex, NodeIndex]
              := ConvertYLocation(ANode2D.Location);
          end;
        end;
      finally
        Polygons.Free;
      end;

    finally
      EdgeNodes.Free;
    end;
//    MeshElements := TGpcPolygonClass.Create;
//    EmptyPolygon := TGpcPolygonClass.Create;
//    PriorUnionPolygon := TGpcPolygonClass.Create;
//    try
//      EmptyPolygon.NumberOfContours := 0;
//      PriorUnionPolygon.NumberOfContours := 0;
//      MeshElements.NumberOfContours := 1;
//      for ElementIndex := 0 to ElementList.Count - 1 do
//      begin
//        AnElement2D := ElementList[ElementIndex];
//        MeshElements.VertexCount[0] := AnElement2D.Nodes.Count;
//        for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
//        begin
//          ANode2D := AnElement2D.Nodes[NodeIndex].Node;
//          MeshElements.Vertices[0, NodeIndex]
//            := ANode2D.Location;
//        end;
//        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
//          MeshElements, PriorUnionPolygon);
//        FreeAndNil(PriorUnionPolygon);
//        PriorUnionPolygon := UnionPolygon;
//      end;
////      UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
////        MeshElements, EmptyPolygon);
//      PriorUnionPolygon := nil;
//      While (UnionPolygon.NumberOfContours > 1) and ((PriorUnionPolygon = nil)
//        or (PriorUnionPolygon.NumberOfContours > UnionPolygon.NumberOfContours)) do
//      begin
//        PriorUnionPolygon.Free;
//        PriorUnionPolygon := UnionPolygon;
//        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
//          PriorUnionPolygon, EmptyPolygon);
//      end;
//    finally
//      PriorUnionPolygon.Free;
//      MeshElements.Free;
//      EmptyPolygon.Free;
//    end;
  finally
//    ElementList.Free;
  end;
end;

constructor TMeshOutline.Create(Mesh: TSutraMesh3D;
  ViewDirection: TViewDirection; Layer: Integer);
var
  UnionPolygon: TGpcPolygonClass;
  APolygon: TPolygon2D;
  PolyIndex: integer;
  VertexIndex: integer;
begin
  inherited Create;
  UnionPolygon := nil;
  try
    case ViewDirection of
      vdTop:
        begin
          GetTopPolygons(Mesh, Layer, UnionPolygon);
//          GetTopPolygons(Mesh, Layer, UnionPolygon);
        end;
      vdFront:
        begin
          GetFrontPolygons(Mesh, UnionPolygon);
        end;
      else
        Assert(False);
    end;
    for PolyIndex := 0 to UnionPolygon.NumberOfContours - 1 do
    begin
      SetLength(APolygon, UnionPolygon.VertexCount[PolyIndex]+1);
      for VertexIndex := 0 to UnionPolygon.VertexCount[PolyIndex] - 1 do
      begin
        APolygon[VertexIndex] := UnionPolygon.Vertices[PolyIndex, VertexIndex];
      end;
      APolygon[Length(APolygon)-1] := APolygon[0];
      FPolygons.Add(TSubPolygon.Create(APolygon, Length(APolygon), 0, 0));
    end;
  finally
    UnionPolygon.Free;
  end;
end;

{ TSutraNode2D_List }

procedure TSutraNode2D_List.Pack;
var
  LocalCount: Integer;
  index: Integer;
begin
  LocalCount := 0;
  for index := 0 to Count - 1 do
  begin
    if Items[index] <> nil then
    begin
      if index <> LocalCount then
      begin
        Items[LocalCount] := Items[index];
      end;
      Inc(LocalCount);
    end;
  end;
  Count := LocalCount;
end;

end.
