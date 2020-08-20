unit ModflowIrregularMeshUnit;

interface

uses
  FastGEO, System.Classes, System.Generics.Collections, GoPhastTypes,
  System.SysUtils, MeshRenumberingTypes, AbstractGridUnit, DataSetUnit,
  ZoomBox2, Vcl.Dialogs, GR32, Vcl.Graphics, GR32_Polygons, DrawMeshTypesUnit,
  System.Generics.Defaults, IntervalTree, QuadTreeClass, SutraMeshUnit,
  SubscriptionUnit, System.Types, Vcl.Controls, Vcl.Forms, ModflowGncUnit,
  Winapi.OpenGL, SubPolygonUnit, GPC_Classes, LineStorage, ModflowCellUnit;

type

  TMf6GridType = (mgtStructured, mgtLayered, mgtUnstructured);
  TModflowIrregularCell2D = class;
  TMFIrregularCell2D_List = TList<TModflowIrregularCell2D>;

  TOverlapInterval = class(TObject)
    Max: double;
    Min: double;
    function IsInside(OtherInterval: TOverlapInterval): Boolean;
    function OverLaps(OtherInterval: TOverlapInterval): Boolean;
  end;

  TOverlapList = TObjectList<TOverlapInterval>;

  TCellEdge = class(TObject)
    Node1: Integer;
    Node2: Integer;
    function IsSame(OtherEdge: TCellEdge): Boolean;
  end;

  TLocationItem = class(TInterfacedPhastCollectionItem)
  private
    FLocation: TPoint2D;
    procedure SetX(const Value: double);
    procedure ReadX(Reader: TReader);
    procedure WriteX(Writer: TWriter);
    procedure ReadStringX(Reader: TReader);
    procedure WriteStringX(Writer: TWriter);
    procedure SetY(const Value: double);
    procedure ReadY(Reader: TReader);
    procedure WriteY(Writer: TWriter);
    procedure ReadStringY(Reader: TReader);
    procedure WriteStringY(Writer: TWriter);
    function GetLocation: TPoint2D;
    procedure SetLocation(const Value: TPoint2D);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
    property Location: TPoint2D read GetLocation write SetLocation;
  published
    property X: double read FLocation.X write SetX;
    property Y: double read FLocation.Y write SetY;
  end;

  TModflowNode = class(TLocationItem, INode, INode2D, ITriangulatable)
  private
    FCells: TMFIrregularCell2D_List;
    FNumber: Integer;
    // @name is set in @link(TModflowDisvGrid.SetNodeElevations).
    FTopElevations: array of double;
    // @name is set in @link(TModflowDisvGrid.SetNodeElevations).
    FBottomElevations: array of double;
    // @name is set in @link(TModflowDisvGrid.SetNodeElevations).
    FActive: array of Boolean;
    FTriangNumber: Integer;
    function GetActiveElement(Index: integer): IElement;
    function GetActiveElementCount: integer;
    function GetNodeNumber: integer;
    function GetNodeType: TNodeType;
    procedure SetNodeNumber(Value: integer);
//    procedure SetNumber(const Value: Integer);
    { TODO -cMODFLOW 6 : These need to be finished. }
    function GetMaxX: Double;
    function GetMaxY: Double;
    function GetMinX: Double;
    function GetMinY: Double;
    function GetMaxZ: Double;
    function GetMinZ: Double;
    function GetActive: Boolean;
    function GetCell(Index: Integer): TModflowIrregularCell2D;
  protected
    function GetCountTri: Integer;
    function GetItemTri(Index: Integer): ITriangulatable;
    function GetNumber: integer;
    function IsNeighbor(OtherItem: ITriangulatable): Boolean;
    function IndexOf(Item: ITriangulatable): Integer;
    function GetElementI(Index: integer): IElement2D;
    function GetTriangNumber: integer;
    procedure SetTriangNumber(const Value: Integer);
  protected
    property ElementsI[Index: Integer]: IElement2D read GetElementI;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ActiveElementCount: integer read GetActiveElementCount;
    property ActiveElements[Index: integer]: IElement read GetActiveElement;
    property NodeNumber: integer read GetNodeNumber write SetNodeNumber;
    property NodeType: TNodeType read GetNodeType;
    procedure AddCell(ACell: TModflowIrregularCell2D);
    { TODO -cMODFLOW 6 : These need to be finished. }
    function CellIntersection(const Input: TSegment2D;
      out IntersectingSegments: TSegment2DArray): boolean;
    property MaxX: Double read GetMaxX;
    property MinX: Double read GetMinX;
    property MaxY: Double read GetMaxY;
    property MinY: Double read GetMinY;
    property Active: Boolean read GetActive;
    property MaxZ: Double read GetMaxZ;
    property MinZ: Double read GetMinZ;
    property Cells[Index: Integer]: TModflowIrregularCell2D read GetCell;
  published

    // @name starts at zero.
    property Number: Integer read GetNodeNumber write SetNodeNumber;
  end;

  TModflowNodes = class(TPhastCollection)
  private
    function GetItem(Index: Integer): TModflowNode;
    procedure SetItem(Index: Integer; const Value: TModflowNode);
    function TopContainingCell(APoint: TPoint2D): T2DTopCell;
  public
    function Add: TModflowNode;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TModflowNode read GetItem write SetItem; default;
  end;

  TModflowNodeList = TList<TModflowNode>;

  TNodeArray2D = array of array of TModflowNode;
  TNodeArray2DList = TList<TNodeArray2D>;

  TModflowIrregularCell2D = class(TLocationItem, IElement, IElement2D, ITriangulatable)
  private
    FElementNumber: Integer;
    FNodeNumbers: TIntegerCollection;
    FElementCorners: TModflowNodeList;
    FTriangNumber: integer;
    procedure SetNodeNumbers(const Value: TIntegerCollection);
    function GetActiveNode(Index: integer): INode;
    function GetActiveNodeCount: integer;
    function GetElementNumber: integer;
    procedure SetElementNumber(Value: integer);
    procedure SetElementCorners(const Value: TModflowNodeList);
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
      DataArray, IDomainDataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
      MinMax: TMinMax; DrawCellEdge, DrawCellNumber: Boolean;
      NumberFont: TFont);
    function GetEdge(Index: Integer): TSegment2D;
    function Center: TPoint2D;
    function GetDisplayNumber: Integer;
    function GetMinWidth: double;
    function GetNodeI(Index: Integer): INode2D;
    function IsInside(APoint: TPoint2D): Boolean;
    function OnEdge(APoint: TPoint2D): Boolean;
  protected
    function GetMaxX: Double;
    function GetMaxY: Double;
    function GetMinX: Double;
    function GetMinY: Double;
    function GetLocation: TPoint2D;
    function GetCountTri: Integer;
    function GetItemTri(Index: Integer): ITriangulatable;
    function GetNumber: integer;
    function IsNeighbor(OtherItem: ITriangulatable): Boolean; overload;
    function IndexOf(Item: ITriangulatable): Integer;
    function IndexOfNode(Node: INode2D): Integer;
    function GetTriangNumber: integer;
    procedure SetTriangNumber(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property NodeCount: integer read GetActiveNodeCount;
    property Nodes[Index: integer]: INode read GetActiveNode;
    property ElementCorners: TModflowNodeList read FElementCorners write SetElementCorners;
    property Edges[Index: Integer]: TSegment2D read GetEdge;
    function Intersection(const Input: TSegment2D;
      out IntersectingSegment: TSegment2D): boolean;
    property MinX: Double read GetMinX;
    property MaxX: Double read GetMaxX;
    property MinY: Double read GetMinY;
    property MaxY: Double read GetMaxY;
    property DisplayNumber: Integer read GetDisplayNumber;
    property MinWidth: double read GetMinWidth;
    function Area: double;
    property NodesI[Index: integer]: INode2D read GetNodeI;
    function IsNeighbor(OtherCell: TModflowIrregularCell2D): Boolean; overload;
    procedure GetNeighbors(CellList: TMFIrregularCell2D_List);
    // Get the neighbors of the current cell in order around the edge
    // of the cell.
    procedure GetNeighborsInOrder(CellList: TMFIrregularCell2D_List);
    procedure GetNeighborsI(ElementList: TIElement2DList);
    function SharedWidth(OtherCell: TModflowIrregularCell2D): double;
    function ReferenceLength: Double;
    function PointInside(APoint: TPoint2D): Boolean;
    function ShareANode(ACell: TModflowIrregularCell2D): Boolean;
  published
    // @name starts at zero.
    property ElementNumber: integer read GetElementNumber
      write SetElementNumber;
    property NodeNumbers: TIntegerCollection read FNodeNumbers
      write SetNodeNumbers;
  end;

  TIrregular2DCellList = TList<TModflowIrregularCell2D>;

  IMF_IrregCellComparer = IComparer<TModflowIrregularCell2D>;

  // Compare by Y values.
  TMF_IrregCellComparer = class(TComparer<TModflowIrregularCell2D>)
  private
    FStartPoint: TPoint2D;
    FAngle: Double;
//    procedure GetMinMaxX(ACell: TModflowIrregularCell2D; out MinX, MaxX: double);
  public
    function CompareX(const Left, Right: TModflowIrregularCell2D): Integer;
    constructor Create(CrossSectionAngle: Double; StartPoint: TPoint2D);
    function Compare(const Left, Right: TModflowIrregularCell2D): Integer; override;
//    function CompleteOverlap(const Left, Right: TModflowIrregularCell2D): Boolean;
  end;

  TModflowIrregularGrid2D = class;
  TModflowDisvGrid = class;

  TModflowIrregularCell2DCollection = class(TPhastCollection)
  private
    FElementIntervals: TRbwIntervalTree;
    FElementCenters: TRbwQuadTree;
    FStoredLocations: TStoredLocations;
    FElementRanges: TRbwRangeTree;
    FMesh: TModflowDisvGrid;
    function GetItem(Index: Integer): TModflowIrregularCell2D;
    procedure SetItem(Index: Integer; const Value: TModflowIrregularCell2D);
    function TopContainingElement(APoint: TPoint2D): T2DTopCell;
    { TODO -cMODFLOW 6 : These need to be finished. }
    function GetMesh2D: TModflowIrregularGrid2D;
    procedure ElementXLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure ElementYLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    function GetElementRanges: TRbwRangeTree;
    function GetElementCenters: TRbwQuadTree;
    procedure FreeInternalObjects;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(InvalidateModelEvent: TNotifyEvent;
      ParentMesh: TModflowDisvGrid);
    destructor Destroy; override;
    function Add: TModflowIrregularCell2D;
    procedure Clear;
    property Mesh2D: TModflowIrregularGrid2D read GetMesh2D;
    property Items[Index: Integer]: TModflowIrregularCell2D read GetItem
      write SetItem; default;
  end;

  TModflowIrregularGrid2D = class(TCustomMesh, IMesh, IMesh2D)
  private
    FCellCorners: TModflowNodes;
    FCells: TModflowIrregularCell2DCollection;
    FTopContourDataSet: TDataArray;
    FThreeDContourDataSet: TDataArray;
    FTopDataSet: TDataArray;
    FThreeDDataSet: TDataArray;
    FDrawingChoice: TDrawingChoice;
    FMesh: TModflowDisvGrid;
    // @name[RowIndex, ColIndex] indicates the level of quad tree refinement
    // to apply when assigning a structured grid to a DISV grid.
    FQuadRefinement: TTwoDIntegerArray;
    FCellStartCounts: TTwoDIntegerArray;
    FGhostNodes: TGhostNodes;
    FLocalGrid: TCustomModelGrid;
    FStoredOutline: TPolygon2D;
    FGridLineDrawingChoice: TGridLineDrawingChoice;
    FDrawCellNumbers: Boolean;
    FNumberFont: TFont;
    procedure SetCellCorners(const Value: TModflowNodes);
    procedure SetCells(const Value: TModflowIrregularCell2DCollection);
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    procedure DrawCells(StringValues: TStringList;
      ColorDataArray: TDataArray; const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    function GetSelectedLayer: integer;
    procedure SetSelectedLayer(const Value: integer);
    procedure SetDrawingChoice(const Value: TDrawingChoice);
    procedure GetElementsOnSegment(Segment: TSegment2D;
      CellsOnSegment: TMFIrregularCell2D_List);
    { TODO -cMODFLOW 6 : These need to be completed }
//    function GetElementNumber: integer;
//    procedure SetElementNumber(const Value: integer);
    function MeshLimits: TGridLimit;
    function GetElementCenters: TRbwQuadTree;
    function GetNodesLocations: TRbwQuadTree;
    procedure UpdateTimeDataSet;
    function GetColumnCount: Integer;
    procedure SetGhostNodes(const Value: TGhostNodes);
    procedure CreateGhostNodes;
    function CellsPerEdge(RowIndex, ColIndex: Integer): Integer;
    function NodesAbove(ColIndex, RowIndex,
      InnerColIndex, InnerRowIndex: Integer
    {$IFDEF GridGen}
      ; NeighborColumnHigher: Boolean
    {$ENDIF}
      ): TCellIdCollection;
    function NodesBelow(ColIndex, RowIndex,
      InnerColIndex, InnerRowIndex: Integer
    {$IFDEF GridGen}
      ; NeighborColumnHigher: Boolean
    {$ENDIF}
      ): TCellIdCollection;
    function NodesLeft(ColIndex, RowIndex,
      InnerColIndex, InnerRowIndex: Integer
    {$IFDEF GridGen}
      ; NeighborRowHigher: Boolean
    {$ENDIF}
      ): TCellIdCollection;
    function NodesRight(ColIndex, RowIndex,
      InnerColIndex, InnerRowIndex: Integer
    {$IFDEF GridGen}
      ; NeighborRowHigher: Boolean
    {$ENDIF}
      ): TCellIdCollection;
    function GetMinWidth(CellIndex: Integer): double;
    procedure DrawTopContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    procedure SetGridLineDrawingChoice(const Value: TGridLineDrawingChoice);
    procedure SetDrawCellNumbers(const Value: Boolean);
    procedure SetNumberFont(const Value: TFont);
    // If @name is @true, the grid will be displayed with (1) all it's grid
    // lines in 2D views of the model (2) only
    // the first, last and selected grid lines, or (3) only grid lines
    // next to active elements.
    property GridLineDrawingChoice: TGridLineDrawingChoice
      read FGridLineDrawingChoice write SetGridLineDrawingChoice;
    property DrawCellNumbers: Boolean read FDrawCellNumbers write SetDrawCellNumbers;
    Property NumberFont: TFont read FNumberFont write SetNumberFont;
  protected
    function GetActiveNode(Index: integer): INode;
    function GetActiveNodeCount: integer;
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: integer): IElement;
    procedure UpdateNodesAndCells;
    { TODO -cMODFLOW 6 : These need to be completed }
    function GetElementRanges: TRbwRangeTree;
    function GetNodeRanges: TRbwRangeTree;
//    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetActiveNodeI2D(Index: integer): INode2D;
    function GetActiveElementI2D(Index: integer): IElement2D;
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    Constructor Create(Model: TBaseModel; ParentMesh: TModflowDisvGrid);
    destructor Destroy; override;
    procedure Draw(const BitMap: TPersistent);
    property NodeCount: integer read GetActiveNodeCount;
    property Nodes[Index: integer]: INode read GetActiveNode;
    property ElementCount: integer read GetActiveElementCount;
    property Elements[Index: integer]: IElement read GetActiveElement;
    property ThreeDDataSet: TDataArray read FThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read FTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read FTopContourDataSet
      write SetTopContourDataSet;
    property ThreeDContourDataSet: TDataArray read FThreeDContourDataSet
      write SetThreeDContourDataSet;
    property SelectedLayer: integer read GetSelectedLayer write SetSelectedLayer;
    property DrawingChoice: TDrawingChoice read FDrawingChoice
      write SetDrawingChoice;
    function DefaultCrossSectionLocation: TSegment2D;
    property ElementRanges: TRbwRangeTree read GetElementRanges;
    property NodeRanges: TRbwRangeTree read GetNodeRanges;
    property ElementCenters: TRbwQuadTree read GetElementCenters;
    property NodesLocations: TRbwQuadTree read GetNodesLocations;
    property ColumnCount: Integer read GetColumnCount;
//    property NodeCount: integer read GetActiveNodeCount;
//    property Nodes[Index: integer]: INode read GetActiveNode;
//    property ElementNumber: integer read GetElementNumber
//      write SetElementNumber;
    function MeshBox: TPolygon2D;
    function MeshOutline: TPolygon2D;
    property MinWidth[CellIndex: Integer]: double read GetMinWidth;
    function CellArea(CellIndex: Integer): double;
    function GetElementOutline(Column: Integer): TElementOutline;
    function OkLocation(const DataSet: TDataArray;
      const Layer, Row, Col: integer): boolean; override;
  published
    procedure Loaded;
    property Cells: TModflowIrregularCell2DCollection read FCells write SetCells;
    property CellCorners: TModflowNodes read FCellCorners write SetCellCorners;
    property GhostNodes: TGhostNodes read FGhostNodes write SetGhostNodes;
  end;

  TModflowDisVCell = class;

  TModflowDisVCellList = TList<TModflowDisVCell>;

  TModflowDisVCell = class(TInterfacedPhastCollectionItem, IElement, IElement3D)
  private
    FCell2D: TModflowIrregularCell2D;
    FBottom: Double;
    FTop: Double;
    FActive: Boolean;
//    FElementNumber: Integer;
    FActiveElements: TModflowDisVCellList;
    function GetActive: Boolean;
    procedure SetBottom(const Value: Double);
    procedure SetTop(const Value: Double);
    procedure SetCell2D(const Value: TModflowIrregularCell2D);
    procedure SetActive(const Value: Boolean);
    function GetDisplayNumber: Integer;
    procedure Draw3D(Layer: Integer);
    function GetCenterLocation: TPoint3D;
    function GetLayer: Integer;
    function GetNodeI(Index: integer): INode3D;
    function GetNodeLocation(Index: integer): TPoint3D;
  protected
    function GetActiveNode(Index: integer): INode;
    function GetActiveNodeCount: integer;
    function GetElementNumber: integer;
    procedure SetElementNumber(Value: integer);
    property DisplayNumber: Integer read GetDisplayNumber;
    function ElementNumber2D: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    property Top: Double read FTop write SetTop;
    property Bottom: Double read FBottom write SetBottom;
    property Cell2D: TModflowIrregularCell2D read FCell2D write SetCell2D;
    property Active: Boolean read GetActive write SetActive;
    function UpperElevation: double;
    function LowerElevation: double;
    function CenterElevation: double;
    function Thickness: double;
    property CenterLocation: TPoint3d read GetCenterLocation;
    property Layer: Integer read GetLayer;
    property NodesI[Index: integer]: INode3D read GetNodeI;
    property NodeLocation[Index: integer]: TPoint3D read GetNodeLocation;
    function ReferenceLength: double;
  end;

  TModflowIrregLayerItem = class;

  TModflowIrregularLayer = class(TPhastCollection)
  private
    FLayerItem: TModflowIrregLayerItem;
    function GetItem(Index: Integer): TModflowDisVCell;
    procedure SetItem(Index: Integer; const Value: TModflowDisVCell);
    function GetLayer: integer;
  public
    function Add: TModflowDisVCell;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TModflowDisVCell read GetItem
      write SetItem; default;
    property Layer: Integer read GetLayer;
  end;

  TModflowIrregLayerItem = class(TPhastCollectionItem)
  private
    FLayer: TModflowIrregularLayer;
    procedure SetLayer(const Value: TModflowIrregularLayer);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  public
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Layer: TModflowIrregularLayer read FLayer write SetLayer stored False;
    property Count: Integer read GetCount write SetCount;
  end;

  TModflowIrregularLayers = class(TPhastCollection)
  private
    function GetItem(Index: Integer): TModflowIrregLayerItem;
    procedure SetItem(Index: Integer; const Value: TModflowIrregLayerItem);
  public
    function Add: TModflowIrregLayerItem;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TModflowIrregLayerItem read GetItem
      write SetItem; default;
  end;

  TPolygon2Darray = array of TPolygon2D;

  TIrregularGridOutline = class(TOutline)
  private
    procedure GetTopPolygons(Mesh: TModflowDisvGrid; Layer: Integer;
      var UnionPolygon: TGpcPolygonClass);
    procedure GetFrontPolygons(Mesh: TModflowDisvGrid;
      var UnionPolygon: TGpcPolygonClass);
  public
    constructor Create(Mesh: TModflowDisvGrid; ViewDirection: TViewDirection;
      Layer: Integer);
  end;

  TModflowDisvGrid = class(TCustomMesh, IDrawMesh, IMesh3D)
  private
    FTwoDGrid: TModflowIrregularGrid2D;
    FLayers: TModflowIrregularLayers;
    FCanDraw: Boolean;
    FSelectedLayer: Integer;
    FCrossSection: TMeshCrossSectionLine;
    FNeedToRedraw3d: Boolean;
    FNeedToRecordCrossSection: Boolean;
    FActiveElements: TModflowDisVCellList;
    FThreeDGridObserver: TObserver;
    FUpdatingElevations: Boolean;
    FElevationsNeedUpdating: Boolean;
    FLoading: Boolean;
    FMeshUpdate: Integer;
    FNeedToRecordLayer: Boolean;
    FFrontDataSet: TDataArray;
    FMaxDist: Double;
    FMinDist: Double;
    FStoredBlockAngle: double;
    FStoredBlockPolygons: TCellElementPolygons2D;
    FStoredBlockLimits: TLimitsArray;
    FElementIntervalTree: TRbwIntervalTree;
    FElementRangeTree: TRbwRangeTree;
    FAssigningQuadRefinement: Boolean;
    FNeedToRecalculateFrontColors: Boolean;
    FNeedToRecalculateTopColors: Boolean;
    FNeedToRecordColoredCells: Boolean;
    FListsCreated: Boolean;
    FLayerGLIndex: GLuint;
    FCrossSectionGLIndex: GLuint;
    FColoredCellsOrElementGLIndex: GLuint;
    FCanDraw3D: Boolean;
    FColorDataSetObserver: TObserver;
    FRotatedElementCenters: TRbwQuadTree;
    FStoredRotatedLocations: TStoredLocations;
    FFrontOutline: TOutline;
    FTopOutline: TOutline;
    FGridLineDrawingChoice: TGridLineDrawingChoice;
    FEdgesGLIndex: Integer;
    FDrawCellNumbers: Boolean;
    FNumberFont: TFont;
    procedure SetLayers(const Value: TModflowIrregularLayers);
    procedure SetTwoDGrid(const Value: TModflowIrregularGrid2D);
    function GetCell(Layer, Col: Integer): TModflowDisVCell;
    function GetCanDraw: boolean;
    procedure SetCanDraw(const Value: boolean);
    function GetSelectedLayer: Integer;
    procedure SetSelectedLayer(Value: Integer);
    procedure SetCrossSection(const Value: TMeshCrossSectionLine);
    procedure CrossSectionMoved(Sender: TObject);
    procedure DrawFront(const BitMap: TPersistent);
    procedure GetCellsOnCrossSection(CellList: TMFIrregularCell2D_List);
    procedure GetNodesIntfOnCrossSection(NodeList: TINode2DList);
    function GetElevationsNeedUpdating: boolean;
    procedure SetElevationsNeedUpdating(const Value: boolean);
    procedure SetThreeDGridObserver(const Value: TObserver);
    procedure UpdateElevations;
    function GetTopContourDataSet: TDataArray;
    function GetThreeDContourDataSet: TDataArray;
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    function GetThreeDDataSet: TDataArray;
    function GetTopDataSet: TDataArray;
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    procedure SetFrontDataSet(const Value: TDataArray);
    function GetFrontDataSet: TDataArray;
    function GetFrontContourDataSet: TDataArray;
    procedure SetFrontContourDataSet(const Value: TDataArray);
    function GetElementCenters(Angle: Double): TRbwQuadTree;
    function GetIntervalTree(EvalAt: TEvaluatedAt;
      Angle: Double): TRbwIntervalTree;
    function GetRangeTree(EvalAt: TEvaluatedAt; Angle: Double): TRbwRangeTree;
    procedure GetXIntervalLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure GetYIntervalLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    // Get the elevations of the cells that have this node as their
    // corner. Use Inverse Distance Weighted Interpolation to get
    // Node elevation.
    procedure SetNodeElevations;
    procedure AssignQuadRefinementArray;
    function GetNeedToRecalculateFrontColors: Boolean;
    function GetNeedToRecalculateTopColors: Boolean;
    procedure SetNeedToRecalculateFrontColors(const Value: Boolean);
    procedure SetNeedToRecalculateTopColors(const Value: Boolean);
    function GetColumnCount: Integer;
    function GetRowCount: Integer;
    function FrontMeshBox(ObjectAngle: double): TPolygon2D;
    function GetCrossSection: TMeshCrossSectionLine;
    procedure RecordLayer;
    procedure RecordCrossSection;
    procedure RecordColoredCells;
    function GetCanDraw3D: Boolean;
    procedure SetLayerCount(const Value: Integer);
    function GetNodeI(Index: Integer): INode3D;
    function GetElementI(Index: Integer): IElement3D;
    procedure DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    procedure SetGridLineDrawingChoice(const Value: TGridLineDrawingChoice);
    procedure RecordColoredGridEdges;
    procedure SetDrawCellNumbers(const Value: Boolean);
    procedure SetNumberFont(const Value: TFont);
    procedure OnChangeNumberFont(Sender: TObject);
  protected
    { TODO -cMODFLOW 6 : These needs to be completed }
    function GetActiveNode(Index: integer): INode;
    function GetActiveNodeCount: integer;
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: integer): IElement;
    function GetElementArrayMemberI(Layer, Col: Integer): IElement3D;
    function GetNodeArrayMemberI(Layer, Col: Integer): INode3D;
    function GetMesh2DI: IMesh2D;
    function GetLayerCount: Integer;
    function IsFishnetMesh: Boolean;
    function TopOutline(Layer: integer): TOutline;
    function FrontOutline: TOutline;
  public
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Cells[Layer: Integer; Col: Integer]: TModflowDisVCell
      read GetCell;
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    procedure Draw(const BitMap: TPersistent;
      const ViewDirection: TViewDirection);
    property SelectedLayer: Integer read GetSelectedLayer write SetSelectedLayer;
    procedure Draw3D;
    function DefaultCrossSectionLocation: TSegment2D;
    procedure SetDefaultCrossSectionLocation;
    function IsLayerUniform(const LayerIndex: integer): boolean;
    function Is3DMesh: Boolean;
    property ElementArrayI[Layer: Integer; Col: Integer]: IElement3D
      read GetElementArrayMemberI;
    property NodeArrayI[Layer: Integer; Col: Integer]: INode3D
      read GetNodeArrayMemberI;
    property LayerCount: Integer read GetLayerCount write SetLayerCount;
    property Mesh2DI: IMesh2D read GetMesh2DI;
    function MeshLimits(ViewDirection: TViewDirection; Angle: double): TGridLimit;
    procedure CheckUpdateElevations;
    property ElevationsNeedUpdating: boolean read GetElevationsNeedUpdating
      write SetElevationsNeedUpdating;
    property ThreeDGridObserver: TObserver read FThreeDGridObserver
      write SetThreeDGridObserver;
    property Loading: Boolean read FLoading write FLoading;
    procedure MeshChanged;
    property TopDataSet: TDataArray read GetTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read GetTopContourDataSet
      write SetTopContourDataSet;
    property FrontDataSet: TDataArray read GetFrontDataSet write SetFrontDataSet;
    property FrontContourDataSet: TDataArray read GetFrontContourDataSet
      write SetFrontContourDataSet;
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property ThreeDContourDataSet: TDataArray read GetThreeDContourDataSet
      write SetThreeDContourDataSet;
    property ActiveCellCount: Integer read GetActiveElementCount;
    // @name is the relative position of the cell corner on the cross section that is
    // furthest from the cross section in the positive direction.
    property MaxDist: Double read FMaxDist;
    // @name is the relative position of the cell corner on the cross section that is
    // furthest from the cross section in the negative direction.
    property MinDist: Double read FMinDist;
    //  @name returns a two-dimensional array of polygons for all 
    // active cells indexed by 
    // layer number and cell number in that order.
    function FrontPolygons(Angle: Double;
      EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray)
      : TCellElementPolygons2D;
    property ElementCenters[Angle: Double]: TRbwQuadTree read GetElementCenters;
    property IntervalTree[EvalAt: TEvaluatedAt; Angle: Double]: TRbwIntervalTree
      read GetIntervalTree;
    property RangeTree[EvalAt: TEvaluatedAt; Angle: Double]: TRbwRangeTree
      read GetRangeTree;
    property AssigningQuadRefinement: Boolean read FAssigningQuadRefinement;
    procedure NotifyMeshChanged(Sender: TObject);
    property NeedToRecalculateTopColors: Boolean
      read GetNeedToRecalculateTopColors write SetNeedToRecalculateTopColors;
    property NeedToRecalculateFrontColors: Boolean
      read GetNeedToRecalculateFrontColors write SetNeedToRecalculateFrontColors;
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    property ColumnCount: Integer read GetColumnCount;
    property RowCount: Integer read GetRowCount;
    function MeshBox(ViewDirection: TViewDirection; ObjectAngle: double): TPolygon2D;
    function TwoDElementCenter(const Column, Row: integer): TPoint2D;
    function RotateFromRealWorldCoordinatesToMeshCoordinates
      (const APoint: TPoint2D): TPoint2D;
    function RotateFromMeshCoordinatesToRealWorldCoordinates
      (const APoint: TPoint2D): TPoint2D;  overload;
    property CanDraw3D: Boolean read GetCanDraw3D write FCanDraw3D;
    procedure ColorDataSetChange(Sender: TObject);
    function GetContainingLayer(CellIndex: integer; const AZPosition: real): integer;
    function CellArea(CellIndex: Integer): double;
    function CellVolume(LayerIndex, CellIndex: Integer): double;
    function LayerPosition(LayerIndex, CellIndex: Integer): double;
    function LayerCenter(LayerIndex, CellIndex: Integer): double;
    property ActiveNodeCount: Integer read GetActiveNodeCount;
    property ActiveElementCount: Integer read GetActiveElementCount;
    property NodesI[Index: Integer]: INode3D read GetNodeI;
    property ElementsI[Index: Integer]: IElement3D read GetElementI;
    procedure GetActiveOutlineTop(Layer: Integer; out Outline: TPolygon2Darray;
      CheckAllLayers: Boolean = False);
    procedure GetInactiveOutlineTop(out Outline: TPolygon2Darray);
    procedure GetModelOutlineTop(out Outline: TPolygon2Darray);
    procedure GetCellsTop(Layer: Integer; out Cells: TPolygon2Darray;
      CheckAllLayers: Boolean = False);
    procedure Clear;
    // If @name is @true, the grid will be displayed with (1) all it's grid
    // lines in 2D views of the model (2) only
    // the first, last and selected grid lines, or (3) only grid lines
    // next to active elements.
    property GridLineDrawingChoice: TGridLineDrawingChoice
      read FGridLineDrawingChoice write SetGridLineDrawingChoice;
    function OkLocation(const DataSet: TDataArray;
      const Layer, Row, Col: integer): boolean; override;
    // Layer, Row, Col are the indicies of an element.
    // CellList will be filled with the horizontal neighbors of that element.
    procedure GetHorizontalNeighbors(const Layer, Row, Col: integer;
      CellList: TCellLocationList);
    function CellThickness(const Layer, Row, Col: integer): double;
    property DrawCellNumbers: Boolean read FDrawCellNumbers write SetDrawCellNumbers;
    Property NumberFont: TFont read FNumberFont write SetNumberFont;
    procedure GetElementsIntfOnCrossSection(ElementList: TIElement2DList);
  published
    procedure Loaded;
    property TwoDGrid: TModflowIrregularGrid2D read FTwoDGrid write SetTwoDGrid;
    property Layers: TModflowIrregularLayers read FLayers write SetLayers;
    property CrossSection: TMeshCrossSectionLine read GetCrossSection
      write SetCrossSection;
  end;

  TNeighborConnection = (ncVertical, ncHorizontal, ncVerticallyStaggered);

  TNeighborItem = class(TPhastCollectionItem)
  private
    FConnection: TNeighborConnection;
    FNumber: integer;
    procedure SetConnection(const Value: TNeighborConnection);
    procedure SetNumber(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Number: integer read FNumber write SetNumber;
    property Connection: TNeighborConnection read FConnection write SetConnection;
  end;

  TNeighbors = class(TPhastCollection)
  private
    function GetItem(Index: Integer): TNeighborItem;
    procedure SetItem(Index: Integer; const Value: TNeighborItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TNeighborItem read GetItem
      write SetItem; default;
  end;

  TModflowDisUCell = class(TModflowDisVCell)
  private
    FNeighbors: TNeighbors;
    procedure SetNeighbors(const Value: TNeighbors);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Neighbors: TNeighbors read FNeighbors write SetNeighbors;
  end;

  TMf6Element2DLeaf = class(TRangeTreeLeaf)
  private
    FElement: TModflowIrregularCell2D;
    FMinX: Double;
    FMaxX: Double;
    FMinY: Double;
    FMaxY: Double;
  public
    constructor Create(AnElement: TModflowIrregularCell2D);
    // 0 = MinX
    // 1 = MaxX
    // 2 = MinY
    // 3 = MaxY
    function GetCoordinate(Depth: integer): double; override;
    property Element: TModflowIrregularCell2D read FElement;
  end;


function Clip(const Segment:TSegment2D; const Polygon : TPolygon2D;
  out CSegment:TSegment2D):Boolean; overload;

implementation

uses
  frmGoPhastUnit, BigCanvasMethods, System.Math, PhastModelUnit,
  LayerStructureUnit, ConvexHullUnit, System.Contnrs,
  ModflowGridUnit, ModelMuseUtilities, gpc, ContourUnit, EdgeDisplayUnit,
  ScreenObjectUnit;

const
  ThinLine = 0.1;

function PointsSame(Point1, Point2: TPoint2D): boolean;
begin
  result := (Point1.x = Point2.x) and (Point1.y = Point2.y);
end;

function SegmentsSame(Segment1, Segment2: TSegment2D): boolean;
begin
  result := (PointsSame(Segment1[1], Segment2[1]) and PointsSame(Segment1[2], Segment2[2]))
    or (PointsSame(Segment1[1], Segment2[2]) and PointsSame(Segment1[2], Segment2[1]));
end;

function SegmentLength(ASegment: TSegment2D): double;
begin
  result := Distance(ASegment[1], ASegment[2])
end;

function Clip(const Segment:TSegment2D; const Polygon : TPolygon2D;
  out CSegment:TSegment2D):Boolean; overload;
var
  Pos : Integer;
  APoint: TPoint2D;
  SideSegment: TSegment2D;
  SideIndex: Integer;
  SegLength: TFloat;
begin
//  Result := False;
  Pos := 0;
  CSegment := Segment;

  if ModelMuseUtilities.PointInConcavePolygon(Segment[1], Polygon) then
  begin
    Inc(Pos);
  end;
  if ModelMuseUtilities.PointInConcavePolygon(Segment[2], Polygon) then
  begin
    Inc(Pos);
    CSegment[Pos] := Segment[2];
  end;

  SideSegment[2] := Polygon[Length(Polygon) - 1];
  for SideIndex := 0 to Length(Polygon) - 1 do
  begin
    SideSegment[1] := Polygon[SideIndex];
    if Intersect(Segment, SideSegment, APoint.X, APoint.Y) then
    begin
      if Pos < 2 then
      begin
        if Pos = 0 then
        begin
          Inc(Pos);
          CSegment[Pos] := APoint;
        end
        else if not Coincident(APoint, CSegment[1]) then
        begin
          Inc(Pos);
          CSegment[Pos] := APoint;
        end;
      end
      else
      begin
        if not Coincident(APoint, CSegment[1]) and not Coincident(APoint, CSegment[2]) then
        begin
          SegLength := Distance(CSegment[1], CSegment[2]);
          if Distance(CSegment[1], APoint) > SegLength then
          begin
            CSegment[2] := APoint;
          end
          else if Distance(CSegment[2], APoint) > SegLength then
          begin
            CSegment[1] := APoint;
          end
        end;
      end;
    end;
    SideSegment[2] := SideSegment[1];
  end;

  result := Pos = 2;
end;


{ TModflowNode }

procedure TModflowNode.AddCell(ACell: TModflowIrregularCell2D);
begin
  FCells.Add(ACell);
end;

procedure TModflowNode.Assign(Source: TPersistent);
var
  SourceNode: TModflowNode;
begin
  if Source is TModflowNode then
  begin
    SourceNode := TModflowNode(Source);
    Location := SourceNode.Location;
  end;
  inherited;
end;

function TModflowNode.CellIntersection(const Input: TSegment2D;
  out IntersectingSegments: TSegment2DArray): boolean;
begin
  result := False;
  IntersectingSegments := nil;
end;

constructor TModflowNode.Create(Collection: TCollection);
begin
  inherited;
  FCells := TMFIrregularCell2D_List.Create;
  FNumber := Collection.Count-1;
end;

destructor TModflowNode.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TModflowNode.GetActive: Boolean;
var
  LayerIndex: Integer;
begin
  result := False;
  for LayerIndex := 0 to Length(FActive) - 1 do
  begin
    if FActive[LayerIndex] then
    begin
      result := True;
      Exit;
    end;
  end;
end;

function TModflowNode.GetActiveElement(Index: integer): IElement;
begin
  result := FCells[Index];
end;

function TModflowNode.GetActiveElementCount: integer;
begin
  result := FCells.Count;
end;

function TModflowNode.GetCell(Index: Integer): TModflowIrregularCell2D;
begin
  result := FCells[Index];
end;

function TModflowNode.GetCountTri: Integer;
begin
  result := ActiveElementCount;
end;

function TModflowNode.GetItemTri(Index: Integer): ITriangulatable;
begin
  result := FCells[Index];
end;

function TModflowNode.GetElementI(Index: integer): IElement2D;
begin
  result := FCells[index];
end;

function TModflowNode.GetMaxX: Double;
begin
  result := X;
end;

function TModflowNode.GetMaxY: Double;
begin
  result := Y;
end;

function TModflowNode.GetMaxZ: Double;
var
  FoundFirst: Boolean;
  LayerIndex: Integer;
  Value: double;
begin
  result := 0;
  if Length(FTopElevations) > 0 then
  begin
    FoundFirst := False;
    for LayerIndex := 0 to Length(FTopElevations) - 1 do
    begin
      if FActive[LayerIndex] then
      begin
        Value := FTopElevations[LayerIndex];
        if FoundFirst then
        begin
          if Value > result then
          begin
             result := Value;
          end;
        end
        else
        begin
          result := Value;
          FoundFirst := True;
        end;
      end;
    end;
  end;
end;

function TModflowNode.GetMinX: Double;
begin
  result := X;
end;

function TModflowNode.GetMinY: Double;
begin
  result := Y;
end;

function TModflowNode.GetMinZ: Double;
var
  FoundFirst: Boolean;
  LayerIndex: Integer;
  Value: double;
begin
  result := 0;
  if Length(FTopElevations) > 0 then
  begin
    FoundFirst := False;
    for LayerIndex := 0 to Length(FBottomElevations) - 1 do
    begin
      if FActive[LayerIndex] then
      begin
        Value := FBottomElevations[LayerIndex];
        if FoundFirst then
        begin
          if Value < result then
          begin
             result := Value;
          end;
        end
        else
        begin
          result := Value;
          FoundFirst := True;
        end;
      end;
    end;
  end;
end;

function TLocationItem.GetLocation: TPoint2D;
begin
  result := FLocation;
end;

function TModflowNode.GetNodeNumber: integer;
begin
  result := FNumber;
end;

function TModflowNode.GetNodeType: TNodeType;
begin
  result := ntInner;
end;

function TModflowNode.GetNumber: integer;
begin
  result := GetNodeNumber+1;
end;

function TModflowNode.GetTriangNumber: integer;
begin
  result := FTriangNumber;
end;

function TModflowNode.IndexOf(Item: ITriangulatable): Integer;
begin
  result := FCells.IndexOf(Item as TModflowIrregularCell2D)
end;

function TModflowNode.IsNeighbor(OtherItem: ITriangulatable): Boolean;
var
  NodeIndex: Integer;
  AnElement: ITriangulatable;
begin
  result := False;
  for NodeIndex := 0 to OtherItem.ItemCount - 1 do
  begin
    AnElement := OtherItem.ItemTri[NodeIndex];
    if FCells.IndexOf(AnElement as TModflowIrregularCell2D ) >= 0 then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TLocationItem.SetLocation(const Value: TPoint2D);
begin
  X := Value.X;
  Y := Value.Y;
end;

procedure TModflowNode.SetNodeNumber(Value: integer);
begin
  if FNumber <> Value then
  begin
    Index := Value;
    FNumber := Value;
  end;
end;

procedure TModflowNode.SetTriangNumber(const Value: Integer);
begin
  FTriangNumber := Value;
end;

//procedure TModflowNode.SetNumber(const Value: Integer);
//begin
//  SetIntegerProperty(FNumber, Value);
//end;

{ TModflowNodes }

function TModflowNodes.Add: TModflowNode;
begin
  result := TModflowNode(inherited Add);
//  result.NodeNumber := Count -1;
end;

constructor TModflowNodes.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TModflowNode, InvalidateModelEvent);
end;

function TModflowNodes.GetItem(Index: Integer): TModflowNode;
begin
  result := inherited Items[index] as TModflowNode;
end;

procedure TModflowNodes.SetItem(Index: Integer; const Value: TModflowNode);
begin
  inherited Items[index] := Value;
end;

function TModflowNodes.TopContainingCell(APoint: TPoint2D): T2DTopCell;
begin
  Result.Row := -1;
  Result.Col := -1;
end;

{ TModflowIrregularCell2D }

function TModflowIrregularCell2D.Area: double;
var
  Polygon: TPolygon2D;
  NodeIndex: Integer;
  ANode: TModflowNode;
begin
  SetLength(Polygon, ElementCorners.Count);
  for NodeIndex := 0 to ElementCorners.Count - 1 do
  begin
    ANode := ElementCorners[NodeIndex];
    Polygon[NodeIndex] := ANode.Location;
  end;
  Result := Abs(FastGeo.Area(Polygon));
end;

procedure TModflowIrregularCell2D.Assign(Source: TPersistent);
var
  MFIregCell: TModflowIrregularCell2D;
begin
//  inherited;
  if Source is TModflowIrregularCell2D then
  begin
    MFIregCell := TModflowIrregularCell2D(Source);
    NodeNumbers := MFIregCell.NodeNumbers;
    ElementNumber := MFIregCell.ElementNumber;
  end;
  inherited;
end;

function TModflowIrregularCell2D.Center: TPoint2D;
//var
//  NodeIndex: Integer;
//  Node: TPoint2D;
//  APolygon: TPolygon2D;
begin
  result := Location;
//  SetLength(APolygon, NodeCount);
//  for NodeIndex := 0 to NodeCount - 1 do
//  begin
//    APolygon[NodeIndex] := Nodes[NodeIndex].Location;
//  end;
//  result := Centroid(APolygon);
end;

constructor TModflowIrregularCell2D.Create(Collection: TCollection);
var
  NotifyEvent: TNotifyEvent;
begin
  inherited;
  NotifyEvent := (Collection as TPhastCollection).OnInvalidateModel;
  FNodeNumbers := TIntegerCollection.Create(NotifyEvent);
  FElementCorners := TModflowNodeList.Create;
  FElementNumber := Collection.Count -1;
end;

destructor TModflowIrregularCell2D.Destroy;
begin
  FElementCorners.Free;
  FNodeNumbers.Free;
  inherited;
end;

procedure TModflowIrregularCell2D.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
  DataArray, IDomainDataArray: TDataArray; SelectedLayer: integer; StringValues: TStringList;
  MinMax: TMinMax; DrawCellEdge, DrawCellNumber: Boolean; NumberFont: TFont);
var
  Points: GoPhastTypes.TPointArray;
  NodeIndex: Integer;
  APoint2D: TPoint2D;
  ShowColor: Boolean;
  Fraction: Double;
  AColor: TColor;
  Dummy: TPolygon32;
//  X: Integer;
//  Y: Integer;
  APoint: TPoint;
  NumberStr: String;
  ASize: TSize;
  ExistingFont: TFont;
begin
  Dummy := nil;
  SetLength(Points, NodeCount+1);
  for NodeIndex := 0 to NodeCount - 1 do
  begin
    APoint2D := Nodes[NodeIndex].Location;
    Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, APoint2D);
  end;
  Points[NodeCount] := Points[0];
  if DataArray <> nil then
  begin
    GetDataSetMeshValue(DataArray, SelectedLayer, ElementNumber,
      StringValues, ShowColor, Fraction, MinMax);
    if DataArray.Limits.ActiveOnly and
      (IDomainDataArray.IntegerData[SelectedLayer,0,ElementNumber] <= 0) then
    begin
      AColor := InactiveGridColor;
      DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
        0, Points, Dummy, False, True);
    end
    else
    begin
      if ShowColor then
      begin
        AColor := frmGoPhast.PhastModel.GridColorParameters.
          FracToColor(Fraction);
        DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
          0, Points, Dummy, False, True);
      end;
    end;
  end;
  if DrawCellEdge then
  begin
    DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
      Points, True);
  end;
  if DrawCellNumber then
  begin
    APoint.X := ZoomBox.XCoord(Location.x);
    if (APoint.X > 0) and (APoint.X < ZoomBox.Width) then
    begin
      APoint.Y := ZoomBox.YCoord(Location.y);
      if (APoint.Y > 0) and (APoint.Y < ZoomBox.Height) then
      begin
        ExistingFont := TFont.Create;
        try
          NumberStr := IntToStr(DisplayNumber);
          if BitMap is TBitmap32 then
          begin
            ExistingFont.Assign(TBitmap32(BitMap).Font);
            TBitmap32(BitMap).Font := NumberFont;
            ASize := TBitmap32(BitMap).TextExtent(NumberStr);
          end
          else
          begin
            ExistingFont.Assign((BitMap as TCanvas).Font);
            TCanvas(BitMap).Font := NumberFont;
            ASize := TCanvas(BitMap).TextExtent(NumberStr);
          end;
          APoint.X := APoint.X - (ASize.Width div 2);
          APoint.Y := APoint.Y - (ASize.Height div 2);
          DrawBigText(BitMap, APoint, NumberStr, NumberFont);
          if BitMap is TBitmap32 then
          begin
            TBitmap32(BitMap).Font.Assign(ExistingFont);
          end
          else
          begin
            TCanvas(BitMap).Font.Assign(ExistingFont);
          end;
        finally
          ExistingFont.Free;
        end;
      end;
    end;
  end;
end;

function TModflowIrregularCell2D.GetActiveNode(Index: integer): INode;
begin
  result := FElementCorners[Index];
end;

function TModflowIrregularCell2D.GetActiveNodeCount: integer;
begin
  result := FElementCorners.Count;
end;

function TModflowIrregularCell2D.GetCountTri: Integer;
begin
  result := NodeCount;
end;

function TModflowIrregularCell2D.GetDisplayNumber: Integer;
begin
  result := ElementNumber + 1;
end;

function TModflowIrregularCell2D.GetEdge(Index: Integer): TSegment2D;
begin
  result[1] := Nodes[Index].Location;
  Inc(Index);
  if Index >= NodeCount then
  begin
    Index := 0;
  end;
  result[2] := Nodes[Index].Location;
end;

function TModflowIrregularCell2D.GetElementNumber: integer;
begin
  result := FElementNumber;
end;

function TModflowIrregularCell2D.GetItemTri(Index: Integer): ITriangulatable;
begin
  result := FElementCorners[Index];
end;

function TModflowIrregularCell2D.GetLocation: TPoint2D;
begin
  result := Center;
end;

function TModflowIrregularCell2D.GetMaxX: Double;
var
  NodeIndex: Integer;
  ANode: INode;
begin
  if NodeCount > 0 then
  begin
    result := Nodes[0].Location.x;
    for NodeIndex := 1 to NodeCount - 1 do
    begin
      ANode := Nodes[NodeIndex];
      if ANode.Location.x > result then
      begin
        result := ANode.Location.x;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TModflowIrregularCell2D.GetMaxY: Double;
var
  NodeIndex: Integer;
  ANode: INode;
begin
  if NodeCount > 0 then
  begin
    result := Nodes[0].Location.y;
    for NodeIndex := 1 to NodeCount - 1 do
    begin
      ANode := Nodes[NodeIndex];
      if ANode.Location.y > result then
      begin
        result := ANode.Location.y;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TModflowIrregularCell2D.GetMinWidth: double;
begin
  result := Min( MaxX - MinX, MaxY - MinY);
end;

function TModflowIrregularCell2D.GetMinX: Double;
var
  NodeIndex: Integer;
  ANode: INode;
begin
  if NodeCount > 0 then
  begin
    result := Nodes[0].Location.x;
    for NodeIndex := 1 to NodeCount - 1 do
    begin
      ANode := Nodes[NodeIndex];
      if ANode.Location.x < result then
      begin
        result := ANode.Location.x;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TModflowIrregularCell2D.GetMinY: Double;
var
  NodeIndex: Integer;
  ANode: INode;
begin
  if NodeCount > 0 then
  begin
    result := Nodes[0].Location.y;
    for NodeIndex := 1 to NodeCount - 1 do
    begin
      ANode := Nodes[NodeIndex];
      if ANode.Location.y < result then
      begin
        result := ANode.Location.y;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TModflowIrregularCell2D.GetNeighbors(CellList: TMFIrregularCell2D_List);
var
  NodeIndex: Integer;
  ANode: TModflowNode;
  CellIndex: Integer;
  OtherCell: TModflowIrregularCell2D;
begin
  CellList.Clear;
  for NodeIndex := 0 to NodeNumbers.Count - 1 do
  begin
    ANode := ElementCorners[NodeIndex];
    for CellIndex := 0 to ANode.FCells.Count -1 do
    begin
      OtherCell := ANode.FCells[CellIndex];
      if (CellList.IndexOf(OtherCell) < 0) and IsNeighbor(OtherCell) then
      begin
        CellList.Add(OtherCell);
      end;
    end;
  end;
end;

procedure TModflowIrregularCell2D.GetNeighborsI(ElementList: TIElement2DList);
var
  CellList: TMFIrregularCell2D_List;
  CellIndex: Integer;
begin
  ElementList.Clear;
  CellList  := TMFIrregularCell2D_List.Create;
  try
    GetNeighbors(CellList);
    ElementList.Capacity := CellList.Count;
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      ElementList.Add(CellList[CellIndex]);
    end;
  finally
    CellList.Free;
  end;
end;

procedure TModflowIrregularCell2D.GetNeighborsInOrder(
  CellList: TMFIrregularCell2D_List);
//var
//  UnOrderedCellList: TMFIrregularCell2D_List;
//  NodeIndex: Integer;
//  Node1: TModflowNode;
//  Node2: TModflowNode;
//  CellIndex: Integer;
//  ACell: TModflowIrregularCell2D;
begin
//  CellList.Clear;
//  UnOrderedCellList := TMFIrregularCell2D_List.Create;
//  try
//    GetNeighbors(UnOrderedCellList);
//    for NodeIndex := 0 to ElementCorners.Count - 1 do
//    begin
//      Node1 := ElementCorners[NodeIndex];
//      if NodeIndex = ElementCorners.Count - 1 then
//      begin
//        Node2 := ElementCorners[0];
//      end
//      else
//      begin
//        Node2 := ElementCorners[NodeIndex+1];
//      end;
//
//      for CellIndex := 0 to UnOrderedCellList.Count - 1 do
//      begin
//        ACell := UnOrderedCellList[CellIndex];
//        if (ACell.ElementCorners.IndexOf(Node1) >= 0)
//          and (ACell.ElementCorners.IndexOf(Node2) >= 0) then
//        begin
//          CellList.Add(ACell);
//          UnOrderedCellList.Delete(CellIndex);
//          break;
//        end;
//      end;
//    end;
//  finally
//    UnOrderedCellList.Free;
//  end;

  GetNeighbors(CellList);
  CellList.Sort(TComparer<TModflowIrregularCell2D>.Construct(
    function (const L, R: TModflowIrregularCell2D): integer
    var
      AngleL: double;
      AngleR: double;
    begin
      AngleL := ArcTan2(L.Center.y - Center.y, L.Center.x - Center.x);
      AngleR := ArcTan2(R.Center.y - Center.y, R.Center.x - Center.x);
      Result := Sign(AngleL - AngleR);
    end
  )) ;
end;

function TModflowIrregularCell2D.GetNodeI(Index: Integer): INode2D;
begin
  result := FElementCorners[Index];
end;

function TModflowIrregularCell2D.GetNumber: integer;
begin
  result := DisplayNumber;
end;

function TModflowIrregularCell2D.GetTriangNumber: integer;
begin
  result := FTriangNumber;
end;

function TModflowIrregularCell2D.IndexOf(Item: ITriangulatable): Integer;
begin
  result := FElementCorners.IndexOf(Item as TModflowNode);
end;

function TModflowIrregularCell2D.IndexOfNode(Node: INode2D): Integer;
begin
  result := FElementCorners.IndexOf(Node as TModflowNode);
end;

function TModflowIrregularCell2D.Intersection(const Input: TSegment2D;
  out IntersectingSegment: TSegment2D): boolean;
var
  Polygon: TPolygon2D;
  NodeIndex: Integer;
begin
  SetLength(Polygon, NodeCount);
  for NodeIndex := 0 to NodeCount - 1 do
  begin
    Polygon[NodeIndex] := Nodes[NodeIndex].Location;
  end;
  result := Clip(Input, Polygon, IntersectingSegment);
  if result then
  begin
    UpdateSegmentOrientation(Input, IntersectingSegment);
  end;
end;

function TModflowIrregularCell2D.IsInside(APoint: TPoint2D): Boolean;
var
  NodeIndex: Integer;
  PriorNode: INode;
  Node: INode;
begin
  result := false;
  PriorNode := Nodes[NodeCount - 1];
  for NodeIndex := 0 to NodeCount - 1 do
  begin
    Node := Nodes[NodeIndex];
    if ((APoint.Y <= Node.Location.Y) = (APoint.Y > PriorNode.Location.Y)) and
      (APoint.X - Node.Location.X - (APoint.Y - Node.Location.Y) *
      (PriorNode.Location.X - Node.Location.X) /
      (PriorNode.Location.Y - Node.Location.Y) < 0) then
    begin
      result := not result;
    end;
    PriorNode := Node;
  end;
end;

function TModflowIrregularCell2D.IsNeighbor(
  OtherItem: ITriangulatable): Boolean;
var
  Count: Integer;
  NodeIndex: Integer;
  AnItem: ITriangulatable;
begin
  if OtherItem = (self as ITriangulatable) then
  begin
    result := False;
    Exit;
  end;
  Count := 0;
  for NodeIndex := 0 to OtherItem.ItemCount - 1 do
  begin
    AnItem := OtherItem.ItemTri[NodeIndex];
    if FElementCorners.IndexOf(AnItem as TModflowNode) >= 0 then
    begin
      Inc(Count);
    end;
  end;
  Assert(Count <= 2);
  result := Count = 2;
end;

function TModflowIrregularCell2D.IsNeighbor(
  OtherCell: TModflowIrregularCell2D): Boolean;
var
  SharedNodeCount: Integer;
  NodeIndex: Integer;
  ANode: TModflowNode;
begin
  result := False;
  if OtherCell = self then
  begin
    Exit;
  end;
  SharedNodeCount := 0;
  for NodeIndex := 0 to NodeNumbers.Count - 1 do
  begin
    ANode := ElementCorners[NodeIndex];
    if ANode.FCells.IndexOf(OtherCell) >= 0 then
    begin
      Inc(SharedNodeCount);
      if SharedNodeCount = 2 then
      begin
        result := True;
        Exit;
      end;
    end;
  end;
end;

function TModflowIrregularCell2D.OnEdge(APoint: TPoint2D): Boolean;
var
  NodeIndex: Integer;
  PriorNode: INode;
  Node: INode;
  ASegment: TSegment2D;
begin
  result := false;
  PriorNode := Nodes[NodeCount - 1];
  for NodeIndex := 0 to NodeCount - 1 do
  begin
    Node := Nodes[NodeIndex];
    ASegment[1] := PriorNode.Location;
    ASegment[2] := Node.Location;
    if ((APoint.Y <= Node.Location.Y) = (APoint.Y >+ PriorNode.Location.Y)) and
      (Collinear(PriorNode.Location, APoint, Node.Location)
      or (Distance(APoint, ASegment) < Distance(ASegment[1], ASegment[2])/10000)) then
    begin
      result := True;
      break;
    end;
    PriorNode := Node;
  end;
end;

function TModflowIrregularCell2D.PointInside(APoint: TPoint2D): Boolean;
var
  Points: TRealPointArray;
  PointIndex: Integer;
  Polygon: TSubPolygon;
begin
  SetLength(Points, NodeCount+1);
  for PointIndex := 0 to NodeCount - 1 do
  begin
    Points[PointIndex] := FElementCorners[PointIndex].Location;
  end;
  Points[NodeCount] := Points[0];

  Polygon := TSubPolygon.Create(Points, NodeCount+1, 0, 0);
  try
    result := Polygon.IsPointInside(APoint);
  finally
    Polygon.Free;
  end;

end;

function TModflowIrregularCell2D.ReferenceLength: Double;
var
  OuterNodeIndex: Integer;
  Node1: TModflowNode;
  InnerNodeIndex: Integer;
  Node2: TModflowNode;
begin
  result := 0;
  for OuterNodeIndex := 0 to FElementCorners.Count - 2 do
  begin
    Node1 := FElementCorners[OuterNodeIndex];
    for InnerNodeIndex := OuterNodeIndex +1 to FElementCorners.Count - 1 do
    begin
      Node2 := FElementCorners[InnerNodeIndex];
      result := Max(result, Distance(Node1.Location, Node2.Location));
    end;
  end;
end;

procedure TModflowIrregularCell2D.SetElementCorners(
  const Value: TModflowNodeList);
begin
  FElementCorners.Clear;
  FElementCorners.AddRange(Value);
end;

procedure TModflowIrregularCell2D.SetElementNumber(Value: integer);
begin
  if FElementNumber <> Value then
  begin
    FElementNumber := Value;
    Index := Value;
    InvalidateModel;
  end;
end;

procedure TModflowIrregularCell2D.SetNodeNumbers(const Value: TIntegerCollection);
begin
  FNodeNumbers.Assign(Value);
end;

procedure TModflowIrregularCell2D.SetTriangNumber(const Value: Integer);
begin
  FTriangNumber := Value;
end;

function TModflowIrregularCell2D.ShareANode(
  ACell: TModflowIrregularCell2D): Boolean;
var
  NodeIndex: Integer;
  ANode: TModflowNode;
begin
  result := False;
  for NodeIndex := 0 to ElementCorners.Count - 1 do
  begin
    ANode := ElementCorners[NodeIndex];
    result := ACell.ElementCorners.IndexOf(ANode) >= 0;
    if result then
    begin
      Exit;
    end;
  end;
end;

function TModflowIrregularCell2D.SharedWidth(
  OtherCell: TModflowIrregularCell2D): double;
var
  NodeIndex: Integer;
  ANode: TModflowNode;
  Node1: TModflowNode;
  Node2: TModflowNode;
begin
  Node1 := nil;
  Node2 := nil;
  for NodeIndex := 0 to NodeNumbers.Count - 1 do
  begin
    ANode := ElementCorners[NodeIndex];
    if ANode.FCells.IndexOf(OtherCell) >= 0 then
    begin
      if Node1 = nil then
      begin
        Node1 := ANode;
      end
      else
      begin
        Node2 := ANode;
        break;
      end;
    end;
  end;
  if Node2 <> nil then
  begin
    result := Distance(Node1.Location, Node2.Location);
  end
  else
  begin
    result := 0
  end;
end;

{ TModflowIrregularCell2DCollection }

function TModflowIrregularCell2DCollection.Add: TModflowIrregularCell2D;
begin
  result := inherited Add as TModflowIrregularCell2D;
end;

procedure TModflowIrregularCell2DCollection.Assign(Source: TPersistent);
begin
  FreeInternalObjects;
  inherited;
end;

procedure TModflowIrregularCell2DCollection.Clear;
begin
  FreeInternalObjects;
  inherited;
end;

procedure TModflowIrregularCell2DCollection.FreeInternalObjects;
begin
  FreeAndNil(FElementRanges);
  FreeAndNil(FElementIntervals);
  FreeAndNil(FElementCenters);
end;

constructor TModflowIrregularCell2DCollection.Create(
  InvalidateModelEvent: TNotifyEvent; ParentMesh: TModflowDisvGrid);
begin
  inherited Create(TModflowIrregularCell2D, InvalidateModelEvent);
  FStoredLocations := TStoredLocations.Create;
  FMesh := ParentMesh;
end;

destructor TModflowIrregularCell2DCollection.Destroy;
begin
  FElementCenters.Free;
  FElementIntervals.Free;
  FStoredLocations.Free;
  FElementRanges.Free;
  inherited;
end;

procedure TModflowIrregularCell2DCollection.ElementXLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  Element: TModflowIrregularCell2D;
  NodeIndex: integer;
  ANode: INode;
begin
  Element := Subject as TModflowIrregularCell2D;

  if Element.NodeCount > 0 then
  begin
    ANode := Element.Nodes[0];
    LowerBoundary := ANode.Location.x;
    UpperBoundary := ANode.Location.x;
    for NodeIndex := 1 to Element.NodeCount - 1 do
    begin
      ANode := Element.Nodes[NodeIndex];
      if UpperBoundary < ANode.Location.x then
      begin
        UpperBoundary := ANode.Location.x;
      end
      else if LowerBoundary > ANode.Location.x then
      begin
        LowerBoundary := ANode.Location.x;
      end;
    end;
  end
  else
  begin
    LowerBoundary := 0;
    UpperBoundary := 0;
  end;

end;

procedure TModflowIrregularCell2DCollection.ElementYLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  Element: TModflowIrregularCell2D;
  NodeIndex: integer;
  ANode: INode;
begin
  Element := Subject as TModflowIrregularCell2D;

  if Element.NodeCount > 0 then
  begin
    ANode := Element.Nodes[0];
    LowerBoundary := ANode.Location.y;
    UpperBoundary := ANode.Location.y;
    for NodeIndex := 1 to Element.NodeCount - 1 do
    begin
      ANode := Element.Nodes[NodeIndex];
      if UpperBoundary < ANode.Location.y then
      begin
        UpperBoundary := ANode.Location.y;
      end
      else if LowerBoundary > ANode.Location.y then
      begin
        LowerBoundary := ANode.Location.y;
      end;
    end;
  end
  else
  begin
    LowerBoundary := 0;
    UpperBoundary := 0;
  end;

end;

function TModflowIrregularCell2DCollection.GetElementCenters: TRbwQuadTree;
var
  Limits: TGridLimit;
  index: Integer;
  Element: TModflowIrregularCell2D;
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
      Center := Element.Location;
      FElementCenters.AddPoint(Center.X, Center.Y, Element);
    end;
  end;
  result := FElementCenters;
end;

function TModflowIrregularCell2DCollection.GetElementRanges: TRbwRangeTree;
var
  LeafList: TElement2DLeafList;
  ElementIndex: Integer;
  AnElement: TModflowIrregularCell2D;
//  AnElement: IElement2D;
  Leaf: TMf6Element2DLeaf;
begin
  if FElementRanges = nil then
  begin
    LeafList := TElement2DLeafList.Create;
    for ElementIndex := 0 to Count - 1 do
    begin
      AnElement := Items[ElementIndex];
      Leaf := TMf6Element2DLeaf.Create(AnElement);
      { $IFDEF ErrorOnFree}
//      RegisterExpectedMemoryLeak(Leaf);
      { $ENDIF}

      LeafList.Add(Leaf);
    end;
    FElementRanges := TRbwRangeTree.Create(LeafList);
  end;
  result := FElementRanges
end;

function TModflowIrregularCell2DCollection.GetItem(
  Index: Integer): TModflowIrregularCell2D;
begin
  result := inherited Items[index] as TModflowIrregularCell2D;
end;

function TModflowIrregularCell2DCollection.GetMesh2D: TModflowIrregularGrid2D;
begin
  result := FMesh.TwoDGrid;
end;

procedure TModflowIrregularCell2DCollection.SetItem(Index: Integer;
  const Value: TModflowIrregularCell2D);
begin
  inherited Items[index] := Value;
end;

function TModflowIrregularCell2DCollection.TopContainingElement(
  APoint: TPoint2D): T2DTopCell;
var
  index: Integer;
  Element: TModflowIrregularCell2D;
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

end;

{ TModflowIrregularGrid2D }

procedure TModflowIrregularGrid2D.Assign(Source: TPersistent);
var
  SourceGrid: TModflowIrregularGrid2D;
  RowIndex: Integer;
  ColIndex: Integer;
  APoint2D: TPoint2D;
  ModflowNode: TModflowNode;
  ACell: TModflowIrregularCell2D;
//  MinEdgeDistance: double;
//  TestDistance: double;
//  Epsilon: Double;
//  NodeQuadTree: TRbwQuadTree;
  InnerRowIndex: Integer;
  InnerColIndex: Integer;
  CornerPoint1: TPoint2D;
  Distance1: double;
  Distance2: double;
  Angle1: Double;
  Angle2: Double;
  CellSubdivider: Integer;
  CellFirstCorner: TPoint2D;
  CellSecondCorner: TPoint2D;
  CellThirdCorner: TPoint2D;
  CellForthCorner: TPoint2D;
  EdgePoint: TPoint2D;
  FirstCellCreated: Boolean;
  ParentNodeArray: TNodeArray2D;
  ChildNodeArray: TNodeArray2D; 
  NeighborNodeArray: TNodeArray2D; 
  PriorRow: TNodeArray2DList;
  CurrentRow: TNodeArray2DList;
  NextRow: TNodeArray2DList;
  AllRows: TObjectList<TNodeArray2DList>;
  NeighborCount: Integer;
  CellCorner: TPoint2D;
  NewNodeNumber: Integer;
  CellCountNeeded: Integer;
//  procedure AddNodeToCell(APoint: TPoint2D);
//  var
//    ModflowNode: TModflowNode;
//    NewNodeNumber: Integer;
//  begin
//    ModflowNode := NodeQuadTree.NearestPointsFirstData(APoint.X, APoint.Y);
//    if Distance(APoint, ModflowNode.Location) >  Epsilon then
//    begin
//      ModflowNode := CellCorners.Add;
//      ModflowNode.Location := APoint;
//      NodeQuadTree.AddPoint(APoint.X, APoint.y, ModflowNode);
//    end;
//    NewNodeNumber := ModflowNode.NodeNumber;
//    ACell.NodeNumbers.Add.Value := NewNodeNumber;
//    ACell.ElementCorners.Add(ModflowNode);
//    ModflowNode.AddCell(ACell);
//  end;
//  procedure AddCenterPointoCell(Point1, Point2: TPoint2D);
//  var
//    APoint2D: TPoint2D;
//  begin
//    APoint2D.X := (Point1.X + Point2.X)/2;
//    APoint2D.Y := (Point1.Y + Point2.Y)/2;
//    AddNodeToCell(APoint2D);
//  end;
begin
  if Source is TModflowIrregularGrid2D then
  begin
    SourceGrid := TModflowIrregularGrid2D(Source);
    Cells := SourceGrid.Cells;
    CellCorners := SourceGrid.CellCorners;
    GhostNodes := SourceGrid.GhostNodes;
  end
  else if Source is TCustomModelGrid then
  begin
//    NodeQuadTree := TRbwQuadTree.Create(nil);
//    PriorRow := nil;
//    PriorRow := TNodeArray2DList.Create;
//    CurrentRow := TNodeArray2DList.Create;
    CellCountNeeded := 0;
    AllRows := TObjectList<TNodeArray2DList>.Create;
    try
      Clear;
      FLocalGrid := TCustomModelGrid(Source);
      if (FLocalGrid.ColumnCount = 0) or (FLocalGrid.RowCount = 0)
        or (FLocalGrid.LayerCount = 0) then
      begin
        Exit;
      end;
      AllRows.Capacity := FLocalGrid.RowCount;
      SetLength(ParentNodeArray, FLocalGrid.RowCount+1, FLocalGrid.ColumnCount+1);
      CellCorners.Capacity := (FLocalGrid.RowCount+1) * (FLocalGrid.ColumnCount+1);
      for RowIndex := 0 to FLocalGrid.RowCount do
      begin
        for ColIndex := 0 to FLocalGrid.ColumnCount do
        begin
          APoint2D := FLocalGrid.TwoDElementCorner(ColIndex, RowIndex);
          ModflowNode := CellCorners.Add;
          ModflowNode.Location := APoint2D;
//          NodeQuadTree.AddPoint(APoint2D.X, APoint2D.y, ModflowNode);
          ParentNodeArray[RowIndex, ColIndex] := ModflowNode;
        end;
      end;
//      MinEdgeDistance := Abs(FLocalGrid.RowPosition[1] - FLocalGrid.RowPosition[0]);
//      for RowIndex := 1 to FLocalGrid.RowCount - 1 do
//      begin
//        TestDistance := Abs(FLocalGrid.RowPosition[RowIndex+1]
//          - FLocalGrid.RowPosition[RowIndex]);
//        if TestDistance < MinEdgeDistance then
//        begin
//          MinEdgeDistance := TestDistance;
//        end;
//      end;
//      for ColIndex := 0 to FLocalGrid.ColumnCount - 1 do
//      begin
//        TestDistance := Abs(FLocalGrid.ColumnPosition[ColIndex+1]
//          - FLocalGrid.ColumnPosition[ColIndex]);
//        if TestDistance < MinEdgeDistance then
//        begin
//          MinEdgeDistance := TestDistance;
//        end;
//      end;
//      Epsilon := MinEdgeDistance/1e6;
      Angle1 := FLocalGrid.GridAngleDegrees;
      Angle2 := Angle1+270;
      while (Angle1 < 0) do
      begin
        Angle1 := Angle1 + 360
      end;

      while (Angle2 > 360) do
      begin
        Angle2 := Angle2 - 360
      end;
      SetLength(FCellStartCounts, FLocalGrid.RowCount, FLocalGrid.ColumnCount);
      PriorRow := nil;
      for RowIndex := 0 to FLocalGrid.RowCount-1 do
      begin
        CurrentRow := TNodeArray2DList.Create;
        CurrentRow.Capacity := FLocalGrid.ColumnCount;
        AllRows.Add(CurrentRow);

        Distance2 := Abs(FLocalGrid.RowPosition[RowIndex+1]
          - FLocalGrid.RowPosition[RowIndex]);
        for ColIndex := 0 to FLocalGrid.ColumnCount-1 do
        begin
          Distance1 := Abs(FLocalGrid.ColumnPosition[ColIndex+1]
            - FLocalGrid.ColumnPosition[ColIndex]);
          begin
            CellSubdivider := Round(IntPower(2,FQuadRefinement[RowIndex, ColIndex]));
//            FirstCellCreated := False;
            SetLength(ChildNodeArray, CellSubdivider+1, CellSubdivider+1);
             
            CornerPoint1 := FLocalGrid.TwoDElementCorner(ColIndex, RowIndex);
            CellCountNeeded := CellCountNeeded + Sqr(CellSubdivider);

            for InnerRowIndex := 0 to CellSubdivider do
            begin
              if InnerRowIndex = 0 then
              begin
                EdgePoint := CornerPoint1;
              end
              else
              begin
                EdgePoint := ProjectPoint(CornerPoint1, Angle2,
                  Distance2/CellSubdivider*InnerRowIndex);
              end;
              for InnerColIndex := 0 to CellSubdivider do
              begin
                ModflowNode := nil;
                if InnerRowIndex = 0 then
                begin
                  if InnerColIndex = 0 then
                  begin
                    ModflowNode := ParentNodeArray[RowIndex, ColIndex];
                  end
                  else if InnerColIndex = CellSubdivider then
                  begin
                    ModflowNode := ParentNodeArray[RowIndex, ColIndex+1];
                  end;
                end
                else if InnerRowIndex = CellSubdivider then
                begin
                  if InnerColIndex = 0 then
                  begin
                    ModflowNode := ParentNodeArray[RowIndex+1, ColIndex];
                  end
                  else if InnerColIndex = CellSubdivider then
                  begin
                    ModflowNode := ParentNodeArray[RowIndex+1, ColIndex+1];
                  end;
                end;
                if ModflowNode = nil then
                begin
                  if (InnerRowIndex = 0) then
                  begin
                    if PriorRow <> nil then
                    begin
                      NeighborNodeArray := PriorRow[ColIndex];
                      NeighborCount := Length(NeighborNodeArray)-1;
                      if CellSubdivider > NeighborCount then
                      begin
                        if not Odd(InnerColIndex) then
                        begin
                          ModflowNode := NeighborNodeArray[NeighborCount, InnerColIndex div 2];
                        end;
                      end
                      else if CellSubdivider < NeighborCount then
                      begin
                        ModflowNode := NeighborNodeArray[NeighborCount, InnerColIndex * 2];
                      end
                      else
                      begin
                        ModflowNode := NeighborNodeArray[NeighborCount, InnerColIndex];
                      end;
                    end;
                  end
                  else if InnerColIndex = 0 then
                  begin
                    if ColIndex > 0 then
                    begin
                      NeighborNodeArray := CurrentRow[ColIndex-1];
                      NeighborCount := Length(NeighborNodeArray)-1;
                      if CellSubdivider > NeighborCount then
                      begin
                        if not Odd(InnerRowIndex) then
                        begin
                          ModflowNode := NeighborNodeArray[InnerRowIndex div 2, NeighborCount];
                        end;
                      end
                      else if CellSubdivider < NeighborCount then
                      begin
                        ModflowNode := NeighborNodeArray[InnerRowIndex * 2, NeighborCount];
                      end
                      else
                      begin
                        ModflowNode := NeighborNodeArray[InnerRowIndex, NeighborCount];
                      end;
                    end;

                  end;
                end;


                if ModflowNode = nil then
                begin
                  if InnerColIndex = 0 then
                  begin
                    CellCorner := EdgePoint;
                  end
                  else
                  begin
                    CellCorner := ProjectPoint(EdgePoint, Angle1,
                      Distance1/CellSubdivider*InnerColIndex);
                  end;
                  ModflowNode := CellCorners.Add;
                  ModflowNode.Location := CellCorner;
                end;

                ChildNodeArray[InnerRowIndex, InnerColIndex] := ModflowNode;
              end;
            end;
            CurrentRow.Add(ChildNodeArray);



          end;
        end;
        PriorRow := CurrentRow;
      end;
      Cells.Capacity := CellCountNeeded;
      for RowIndex := 0 to FLocalGrid.RowCount-1 do
      begin
        CurrentRow := AllRows[RowIndex];
        if RowIndex > 0 then
        begin
          PriorRow := AllRows[RowIndex-1]
        end
        else
        begin
          PriorRow := nil;
        end;
        if RowIndex < FLocalGrid.RowCount-1 then
        begin
          NextRow := AllRows[RowIndex+1]
        end
        else
        begin
          NextRow := nil;
        end;
        for ColIndex := 0 to FLocalGrid.ColumnCount-1 do
        begin
          ChildNodeArray := CurrentRow[ColIndex];
          CellSubdivider := Round(IntPower(2,FQuadRefinement[RowIndex, ColIndex]));
          Assert(Length(ChildNodeArray) = CellSubdivider+1);
          FirstCellCreated := False;

          for InnerRowIndex := 0 to CellSubdivider-1 do
          begin
//              if InnerRowIndex = 0 then
//              begin
//                EdgePoint := CornerPoint1;
//              end
//              else
//              begin
//                EdgePoint := ProjectPoint(CornerPoint1, Angle2,
//                  Distance2/CellSubdivider*InnerRowIndex);
//              end;
            for InnerColIndex := 0 to CellSubdivider-1 do
            begin
              if not FirstCellCreated then
              begin
                FCellStartCounts[RowIndex, ColIndex] := Cells.Count;
                FirstCellCreated := True;
              end;
              ACell := Cells.Add;
              ACell.ElementCorners.Capacity := 4;
              ACell.NodeNumbers.Capacity := 4;

//                if InnerColIndex = 0 then
//                begin
//                  CellFirstCorner := EdgePoint;
//                end
//                else
//                begin
//                  CellFirstCorner := ProjectPoint(EdgePoint, Angle1,
//                    Distance1/CellSubdivider*InnerColIndex);
//                end;
//                CellSecondCorner := ProjectPoint(EdgePoint, Angle1,
//                    Distance1/CellSubdivider*(InnerColIndex+1));
//                CellThirdCorner := ProjectPoint(CellSecondCorner, Angle2,
//                    Distance2/CellSubdivider);
//                CellForthCorner := ProjectPoint(CellFirstCorner, Angle2,
//                    Distance2/CellSubdivider);


              ModflowNode := ChildNodeArray[InnerRowIndex,InnerColIndex];
              // nodes must be in clockwise order.

              NewNodeNumber := ModflowNode.NodeNumber;
              ACell.NodeNumbers.Add.Value := NewNodeNumber;
              ACell.ElementCorners.Add(ModflowNode);
              ModflowNode.AddCell(ACell);
              CellFirstCorner := ModflowNode.Location;

//                AddNodeToCell(CellFirstCorner);

              if (InnerRowIndex = 0) and (RowIndex > 0) and
                (FQuadRefinement[RowIndex-1, ColIndex]
                > FQuadRefinement[RowIndex, ColIndex]) then
              begin
                Assert(PriorRow <> nil);
                NeighborNodeArray := PriorRow[ColIndex];
                ModflowNode := NeighborNodeArray[Length(NeighborNodeArray)-1,
                  InnerColIndex*2 + 1];
                NewNodeNumber := ModflowNode.NodeNumber;
                ACell.NodeNumbers.Add.Value := NewNodeNumber;
                ACell.ElementCorners.Add(ModflowNode);
                ModflowNode.AddCell(ACell);
//                AddCenterPointoCell(CellFirstCorner, CellSecondCorner)
              end;

              ModflowNode := ChildNodeArray[InnerRowIndex,InnerColIndex+1];
              NewNodeNumber := ModflowNode.NodeNumber;
              ACell.NodeNumbers.Add.Value := NewNodeNumber;
              ACell.ElementCorners.Add(ModflowNode);
              ModflowNode.AddCell(ACell);
              CellSecondCorner := ModflowNode.Location;

//              AddNodeToCell(CellSecondCorner);

              if (InnerColIndex = CellSubdivider-1) and
                (ColIndex < FLocalGrid.ColumnCount -1) and
                (FQuadRefinement[RowIndex, ColIndex+1]
                > FQuadRefinement[RowIndex, ColIndex]) then
              begin
                NeighborNodeArray := CurrentRow[ColIndex+1];
                ModflowNode := NeighborNodeArray[InnerRowIndex*2 + 1, 0];
                NewNodeNumber := ModflowNode.NodeNumber;
                ACell.NodeNumbers.Add.Value := NewNodeNumber;
                ACell.ElementCorners.Add(ModflowNode);
                ModflowNode.AddCell(ACell);
//                AddCenterPointoCell(CellSecondCorner, CellThirdCorner)
              end;

//              AddNodeToCell(CellThirdCorner);
              ModflowNode := ChildNodeArray[InnerRowIndex+1,InnerColIndex+1];
              NewNodeNumber := ModflowNode.NodeNumber;
              ACell.NodeNumbers.Add.Value := NewNodeNumber;
              ACell.ElementCorners.Add(ModflowNode);
              ModflowNode.AddCell(ACell);
              CellThirdCorner := ModflowNode.Location;

              if (InnerRowIndex = CellSubdivider-1)
                and (RowIndex < FLocalGrid.RowCount -1) and
                (FQuadRefinement[RowIndex+1, ColIndex]
                > FQuadRefinement[RowIndex, ColIndex]) then
              begin
                Assert(NextRow <> nil);
                NeighborNodeArray := NextRow[ColIndex];
                ModflowNode := NeighborNodeArray[0, InnerColIndex*2 + 1];
                NewNodeNumber := ModflowNode.NodeNumber;
                ACell.NodeNumbers.Add.Value := NewNodeNumber;
                ACell.ElementCorners.Add(ModflowNode);
                ModflowNode.AddCell(ACell);

//                AddCenterPointoCell(CellThirdCorner, CellForthCorner)
              end;

//              AddNodeToCell(CellForthCorner);
              ModflowNode := ChildNodeArray[InnerRowIndex+1,InnerColIndex];
              NewNodeNumber := ModflowNode.NodeNumber;
              ACell.NodeNumbers.Add.Value := NewNodeNumber;
              ACell.ElementCorners.Add(ModflowNode);
              ModflowNode.AddCell(ACell);
              CellForthCorner := ModflowNode.Location;

              if (InnerColIndex = 0) and (ColIndex > 0) and
                (FQuadRefinement[RowIndex, ColIndex-1]
                > FQuadRefinement[RowIndex, ColIndex]) then
              begin
                NeighborNodeArray := CurrentRow[ColIndex-1];
                ModflowNode := NeighborNodeArray[InnerRowIndex*2 + 1,
                  Length(NeighborNodeArray)-1];
                NewNodeNumber := ModflowNode.NodeNumber;
                ACell.NodeNumbers.Add.Value := NewNodeNumber;
                ACell.ElementCorners.Add(ModflowNode);
                ModflowNode.AddCell(ACell);
//                AddCenterPointoCell(CellForthCorner, CellFirstCorner)
              end;

              APoint2D.x := (CellFirstCorner.x + CellSecondCorner.x
                + CellThirdCorner.x + CellForthCorner.x)/4;
              APoint2D.y := (CellFirstCorner.y + CellSecondCorner.y
                + CellThirdCorner.y + CellForthCorner.y)/4;
              ACell.Location := APoint2D;
            end;
          end;

        end;

      end;
      CreateGhostNodes;


    finally
//      NodeQuadTree.Free;
//      PriorRow.Free;
      AllRows.Free;
      FLocalGrid := nil;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflowIrregularGrid2D.Clear;
begin
  Cells.Clear;
  CellCorners.Clear;
  GhostNodes.Clear;
  FStoredOutline := nil;


  FTopContourDataSet := nil;
  FThreeDContourDataSet := nil;
  FTopDataSet := nil;
  FThreeDDataSet := nil;
//  FQuadRefinement := nil;
  FCellStartCounts := nil;
  FStoredOutline := nil;

{    FCellCorners: TModflowNodes;
    FCells: TModflowIrregularCell2DCollection;
    FTopContourDataSet: TDataArray;
    FThreeDContourDataSet: TDataArray;
    FTopDataSet: TDataArray;
    FThreeDDataSet: TDataArray;
    FDrawingChoice: TDrawingChoice;
    FMesh: TModflowDisvGrid;
    // @name[RowIndex, ColIndex] indicates the level of quad tree refinement
    // to apply when assigning a structured grid to a DISV grid.
    FQuadRefinement: TTwoDIntegerArray;
    FCellStartCounts: TTwoDIntegerArray;
    FGhostNodes: TGhostNodes;
    FLocalGrid: TCustomModelGrid;
    FStoredOutline: TPolygon2D;
  }
end;

constructor TModflowIrregularGrid2D.Create(Model: TBaseModel; ParentMesh: TModflowDisvGrid);
var
  InvalidatEvent: TNotifyEvent;
begin
  inherited Create(Model);
  FNumberFont := TFont.Create;
  FMesh := ParentMesh;
  if Model <> nil then
  begin
    InvalidatEvent := Model.Invalidate;
  end
  else
  begin
    InvalidatEvent := nil;
  end;
  FCells := TModflowIrregularCell2DCollection.Create(InvalidatEvent, ParentMesh);
  FCellCorners := TModflowNodes.Create(InvalidatEvent);
  FGhostNodes := TGhostNodes.Create;
  FThreeDDataSet := nil;
  FTopDataSet := nil;
  FTopContourDataSet := nil;
  FThreeDContourDataSet := nil;
end;

function TModflowIrregularGrid2D.DefaultCrossSectionLocation: TSegment2D;
var
  MinX: Double;
  MaxX: Double;
  MinY: Double;
  MaxY: Double;
  NodeIndex: Integer;
  ANode: TModflowNode;
  DeltaX: Double;
begin
  if NodeCount = 0 then
  begin
    result[1].X := 0;
    result[1].Y := 0;
    result[2].X := 0;
    result[2].Y := 0;
  end
  else
  begin
    MinX := CellCorners[0].X;
    MaxX := MinX;
    MinY := CellCorners[0].Y;
    MaxY := MinY;
    for NodeIndex := 0 to NodeCount - 1 do
    begin
      ANode := CellCorners[NodeIndex];
      if ANode.X < MinX then
      begin
        MinX := ANode.X;
      end;
      if ANode.X > MaxX then
      begin
        MaxX := ANode.X;
      end;
      if ANode.Y < MinY then
      begin
        MinY := ANode.Y;
      end;
      if ANode.Y > MaxY then
      begin
        MaxY := ANode.Y;
      end;
    end;
    DeltaX := MaxX - MinX;
    result[1].X := MinX - DeltaX/10;
    result[1].Y := (MinY + MaxY)/2;
    result[2].X := MaxX + DeltaX/10;
    result[2].Y := result[1].Y;
  end;
end;

procedure TModflowIrregularGrid2D.UpdateTimeDataSet;
begin
  if frmGoPhast.PhastModel.ThreeDTimeList <> nil then
  begin
    frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(frmGoPhast.PhastModel.ThreeDTimeList, frmGoPhast.PhastModel.ThreeDDisplayTime);
    if frmGoPhast.PhastModel.TopTimeList <> nil then
    begin
      frmGoPhast.PhastModel.UpdateTopTimeDataSet(frmGoPhast.PhastModel.TopTimeList, frmGoPhast.PhastModel.TopDisplayTime);
    end;
    if frmGoPhast.PhastModel.FrontTimeList <> nil then
    begin
      frmGoPhast.PhastModel.UpdateFrontTimeDataSet(frmGoPhast.PhastModel.FrontTimeList, frmGoPhast.PhastModel.FrontDisplayTime);
    end;
    if frmGoPhast.PhastModel.SideTimeList <> nil then
    begin
      frmGoPhast.PhastModel.UpdateSideTimeDataSet(frmGoPhast.PhastModel.SideTimeList, frmGoPhast.PhastModel.SideDisplayTime);
    end;
  end;
end;

destructor TModflowIrregularGrid2D.Destroy;
begin
  FGhostNodes.Free;
  FCellCorners.Free;
  FCells.Free;
  FNumberFont.Free;
  inherited;
end;

procedure TModflowIrregularGrid2D.Draw(const BitMap: TPersistent);
begin
  DrawTop(BitMap, frmGoPhast.frameTopView.ZoomBox);
end;

procedure TModflowIrregularGrid2D.DrawCells(StringValues: TStringList;
  ColorDataArray: TDataArray; const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  CellLayer: Integer;
  MinMax: TMinMax;
  CellIndex: Integer;
  ACell: TModflowIrregularCell2D;
  DrawCellEdge: Boolean;
  IDomainDataArray: TDataArray;
  Outline: TPolygon2Darray;
  DrawCellLabel: Boolean;
  MinMaxInitialized: Boolean;
  procedure DrawMeshOutline;
  var
    Points: GoPhastTypes.TPointArray;
    PolygonIndex: Integer;
    APolygon: TPolygon2D;
    PointIndex: Integer;
  begin
    for PolygonIndex := 0 to Length(Outline) - 1 do
    begin
      APolygon := Outline[PolygonIndex];
      SetLength(Points, Length(APolygon)+1);
      for PointIndex := 0 to Length(APolygon) - 1 do
      begin
        Points[PointIndex] := ConvertTop2D_Point(ZoomBox, APolygon[PointIndex]);
      end;
      Points[Length(Points)-1] := Points[0];
      DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
        Points, True);
    end;
  end;
begin
  CellLayer := -1;

  if ColorDataArray <> nil then
  begin
    case ColorDataArray.Orientation of
      dsoTop: CellLayer := 0;
      dso3D: CellLayer := SelectedLayer;
      else Assert(False);
    end;
  end
  else
  begin
    CellLayer := SelectedLayer;
  end;
  if (ColorDataArray <> nil) then
  begin
    GetMinMax(MinMax, ColorDataArray, StringValues, MinMaxInitialized);
    ApplyLimittoMinMax(ColorDataArray, MinMax, ColorDataArray.Limits);
  end;
  IDomainDataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(K_IDOMAIN);
  if GridLineDrawingChoice in [gldcActive, gldcActiveEdge] then
  begin
    IDomainDataArray.Initialize;
  end;
  for CellIndex := 0 to Cells.Count - 1 do
  begin
    ACell := Cells[CellIndex];
    DrawCellEdge := True;
    DrawCellLabel := False;
    case GridLineDrawingChoice of
      gldcAll:
        begin
          DrawCellEdge := True;
          DrawCellLabel := DrawCellNumbers;
        end;
      gldcExterior:
        begin
          DrawCellEdge := False;
          DrawCellLabel := DrawCellNumbers;
        end;
      gldcActive:
        begin
          DrawCellEdge := IDomainDataArray.IntegerData[SelectedLayer, 0, CellIndex] > 0;
          DrawCellLabel := DrawCellNumbers and (IDomainDataArray.IntegerData[SelectedLayer, 0, CellIndex] > 0);
        end;
      gldcActiveEdge:
        begin
          DrawCellEdge := False;
          DrawCellLabel := DrawCellNumbers and (IDomainDataArray.IntegerData[SelectedLayer, 0, CellIndex] > 0);
        end;
      else Assert(False)
    end;
    ACell.DrawTop(BitMap, ZoomBox, DrawingChoice,
      ColorDataArray, IDomainDataArray, CellLayer, StringValues, MinMax,
      DrawCellEdge, DrawCellLabel, NumberFont);
  end;
  case GridLineDrawingChoice of
    gldcAll: ;
    gldcExterior:
      begin
//        FMesh.GetModelOutlineTop(Outline);
        FMesh.GetActiveOutlineTop(-1, Outline, False);
        DrawMeshOutline;
      end;
    gldcActive:
      begin
        FMesh.GetActiveOutlineTop(CellLayer, Outline);
        DrawMeshOutline;
      end;
    gldcActiveEdge:
      begin
        FMesh.GetActiveOutlineTop(CellLayer, Outline);
        DrawMeshOutline;
      end;
    else Assert(False)
  end;

end;

procedure TModflowIrregularGrid2D.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
//  NodeIndex: Integer;
  ColorDataArray: TDataArray;
  StringValues : TStringList;
  LocalModel: TCustomModel;
//  ActiveNode: Boolean;
begin
  UpdateTimeDataSet;

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
    if (ColorDataArray <> nil) and (ColorDataArray.EvaluatedAt <> eaBlocks) then
    begin
      ColorDataArray := nil;
    end;
    DrawCells(StringValues, ColorDataArray, ZoomBox, BitMap);
  finally
    StringValues.free;
  end;

  DrawTopContours(ZoomBox, BitMap);

  LocalModel := Model as TCustomModel;
  if LocalModel.EdgeDisplay <> nil then
  begin
    LocalModel.EdgeDisplay.Draw(SelectedLayer, BitMap);
  end;
end;

procedure TModflowIrregularGrid2D.DrawTopContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  TopContourer: TMultipleContourCreator;
  LocalModel: TCustomModel;
begin
  if (NodeCount > 0) and (TopContourDataSet <> nil) then
  begin
      TopContourer := TMultipleContourCreator.Create;
      try
        PlotList := FTopContourPlotList;
        TopContourer.DataSet := TopContourDataSet;
        LocalModel := Model as TCustomModel;
        TopContourer.ActiveDataSet :=
          LocalModel.DataArrayManager.GetDataSetByName(K_IDomain);
        TopContourer.BitMap := BitMap;
        TopContourer.ViewDirection := vdTop;
        TopContourer.Mesh := Self.FMesh;
        TopContourer.ZoomBox := ZoomBox;
        TopContourer.DrawContours(SelectedLayer,
          frmGoPhast.PhastModel.ContourColorParameters, vdTop,
          frmGoPhast.PhastModel.ContourLabelSpacing,
          frmGoPhast.PhastModel.TopContoursUpToDate);
        frmGoPhast.PhastModel.TopContoursUpToDate := True;
      finally
        TopContourer.Free;
      end;
  end;
end;

function TModflowIrregularGrid2D.GetActiveElement(Index: integer): IElement;
begin
  result := Cells[Index];
end;

function TModflowIrregularGrid2D.GetActiveElementCount: integer;
begin
  result := Cells.Count;
end;

function TModflowIrregularGrid2D.GetActiveElementI2D(
  Index: integer): IElement2D;
begin
  result := Cells[Index];
end;

function TModflowIrregularGrid2D.GetActiveNode(Index: integer): INode;
begin
  result := CellCorners[Index];
end;

function TModflowIrregularGrid2D.GetActiveNodeCount: integer;
begin
  result := CellCorners.Count;
end;

function TModflowIrregularGrid2D.GetActiveNodeI2D(Index: integer): INode2D;
begin
  result := CellCorners[Index];
end;

function TModflowIrregularGrid2D.GetColumnCount: Integer;
begin
  result := ElementCount;
end;

function TModflowIrregularGrid2D.GetElementCenters: TRbwQuadTree;
begin
  result := Cells.GetElementCenters;
end;

function TModflowIrregularGrid2D.GetElementOutline(
  Column: Integer): TElementOutline;
var
  ACell: TModflowIrregularCell2D;
  NodeIndex: Integer;
begin
  ACell := Cells[Column];
  result.Count := ACell.ElementCorners.Count;
  for NodeIndex := 0 to ACell.ElementCorners.Count -1 do
  begin
    result.Points[NodeIndex] := ACell.ElementCorners[NodeIndex].Location;
  end;
end;

function TModflowIrregularGrid2D.GetElementRanges: TRbwRangeTree;
begin
  result := Cells.GetElementRanges;
end;

procedure TModflowIrregularGrid2D.GetElementsOnSegment(Segment: TSegment2D;
  CellsOnSegment: TMFIrregularCell2D_List);
var
  CellIndex: Integer;
  ACell: TModflowIrregularCell2D;
  CellComparerI: IMF_IrregCellComparer;
//  CellComparer: TMF_IrregCellComparer;
  Cell1: TModflowIrregularCell2D;
  OverlapList: TOverlapList;
  MinX: double;
  MaxX: Double;
  OtherInterval: TOverlapInterval;
  OverlapIndex: Integer;
  OverLapInterval: TOverlapInterval;
  NewInterval: TOverlapInterval;
  OIndex: Integer;
  IntersectSegment: TSegment2D;
  D1: TFloat;
  D2: TFloat;
begin
  CellsOnSegment.Clear;
  for CellIndex := 0 to Cells.Count - 1 do
  begin
    ACell := Cells[CellIndex];
    if ACell.Intersection(Segment, IntersectSegment) then
    begin
      CellsOnSegment.Add(ACell);
    end;
  end;

  OverlapList := TOverlapList.Create;
  CellComparerI := TMF_IrregCellComparer.Create(
    ArcTan2(Segment[2].y - Segment[1].y, Segment[2].x - Segment[1].x),
    Segment[1]);
  try
    CellsOnSegment.Sort(CellComparerI);

//    CellComparer := CellComparerI as TMF_IrregCellComparer;
    for CellIndex := CellsOnSegment.Count - 1 downto 0 do
    begin
      Cell1 := CellsOnSegment[CellIndex];
      Assert(Cell1.Intersection(Segment, IntersectSegment));
      D1 := Distance(IntersectSegment[1], Segment[1]);
      D2 := Distance(IntersectSegment[2], Segment[1]);
      if D1 < D2 then
      begin
        MinX := D1;
        MaxX := D2;
      end
      else
      begin
        MinX := D2;
        MaxX := D1;
      end;

//      CellComparer.GetMinMaxX(Cell1, MinX, MaxX);
      OverLapInterval := TOverlapInterval.Create;
      OverLapInterval.Max := MaxX;
      OverLapInterval.Min := MinX;
      NewInterval := OverLapInterval;
      for OverlapIndex := OverlapList.Count -1 downto 0 do
      begin
        OtherInterval := OverlapList[OverlapIndex];
        if OverLapInterval.IsInside(OtherInterval) then
        begin
          FreeAndNil(OverLapInterval);
          CellsOnSegment.Delete(CellIndex);
          NewInterval := nil;
          break;
        end
        else if OverLapInterval.Overlaps(OtherInterval) then
        begin
          OtherInterval.Max := Max(OtherInterval.Max, OverLapInterval.Max);
          OtherInterval.Min := Min(OtherInterval.Min, OverLapInterval.Min);
          OIndex := OverlapList.IndexOf(OverLapInterval);
          if OIndex >= 0 then
          begin
            OverlapList.Delete(OIndex);
          end
          else
          begin
            FreeAndNil(OverLapInterval);
          end;
          NewInterval := nil;
          OverLapInterval := OtherInterval;
        end;
      end;
      if NewInterval <> nil then
      begin
        OverlapList.Add(NewInterval)
      end;
//      Cell2 := CellsOnSegment[CellIndex+1];
//      if CellComparer.CompleteOverlap(Cell1, Cell2) then
//      begin
//        if CellComparer.YCompare(Cell1,Cell2) > 0 then
//        begin
//          CellsOnSegment.Delete(CellIndex);
//        end
//        else
//        begin
//          CellsOnSegment.Delete(CellIndex+1);
//        end;
//      end;
    end;
  finally
    CellComparerI := nil;
    OverlapList.Free;
  end;



end;

function TModflowIrregularGrid2D.GetMinWidth(CellIndex: Integer): double;
begin
  result := Cells[CellIndex].MinWidth
end;

function TModflowIrregularGrid2D.GetNodeRanges: TRbwRangeTree;
begin
  result := nil;
end;

function TModflowIrregularGrid2D.GetNodesLocations: TRbwQuadTree;
begin
  result := nil;
end;

function TModflowIrregularGrid2D.GetSelectedLayer: integer;
begin
  Result := FMesh.SelectedLayer;
end;

procedure TModflowIrregularGrid2D.Loaded;
var
  CellIndex: Integer;
  ACell: TModflowIrregularCell2D;
  NodeIndex: Integer;
  ANode: TModflowNode;
begin
  for CellIndex := 0 to ElementCount - 1 do
  begin
    ACell := Cells[CellIndex];
    for NodeIndex := 0 to ACell.NodeNumbers.Count -1 do
    begin
      ANode := CellCorners[ACell.NodeNumbers[NodeIndex].Value];
      ACell.ElementCorners.Add(ANode);
      ANode.FCells.Add(ACell);
    end;
  end;
end;

function TModflowIrregularGrid2D.MeshBox: TPolygon2D;
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

function TModflowIrregularGrid2D.CellArea(CellIndex: Integer): double;
begin
  result := Cells[CellIndex].Area
end;

function TModflowIrregularGrid2D.CellsPerEdge(RowIndex, ColIndex: Integer): Integer;
var
  Refinement: Integer;
begin
  Refinement := FQuadRefinement[RowIndex, ColIndex];
  result := Round(IntPower(2, Refinement));
end;

function TModflowIrregularGrid2D.NodesAbove(ColIndex, RowIndex,
  InnerColIndex, InnerRowIndex: Integer
{$IFDEF GridGen}
  ; NeighborColumnHigher: Boolean
{$ENDIF}
  ): TCellIdCollection;
var
  CellSubdivider: Integer;
  CellSubdividerAbove: Integer;
  AboveCell: TWeightedCellId;
  RowStart: Integer;
  ColumnWidth: double;
  InnerNodes: TCellIdCollection;
  NodeIndex: Integer;
  OtherCellSubdivider: Integer;
  OtherColWidth: Double;
  ACellWeight: TWeightedCellId;
  OtherWeight: double;
begin
  result := nil;
  if (RowIndex > 0) or (InnerRowIndex > 0) then
  begin
    result := TCellIdCollection.Create;
    CellSubdivider := CellsPerEdge(RowIndex, ColIndex);
    if InnerRowIndex > 0 then
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex]
        + CellSubdivider * (InnerRowIndex-1);

      AboveCell := result.Add;
      AboveCell.Cell := RowStart + InnerColIndex;
      AboveCell.Weight := 1;

      Exit;
    end;

    CellSubdividerAbove := CellsPerEdge(RowIndex-1, ColIndex);
    RowStart := FCellStartCounts[RowIndex - 1, ColIndex]
      + CellSubdividerAbove * (CellSubdividerAbove - 1);
    if CellSubdividerAbove > CellSubdivider then
    begin
    {$IFDEF GridGen}
      if NeighborColumnHigher then
      begin
        AboveCell := result.Add;
        AboveCell.Cell := RowStart + InnerColIndex*2 + 1;
        AboveCell.Weight := 1;
      end
      else
      begin
        AboveCell := result.Add;
        AboveCell.Cell := RowStart + InnerColIndex*2;
        AboveCell.Weight := 1;
      end;
    {$ELSE}
      AboveCell := result.Add;
      AboveCell.Cell := RowStart + InnerColIndex*2;
      AboveCell.Weight := 0.5;
      AboveCell := result.Add;
      AboveCell.Cell := RowStart + InnerColIndex*2 + 1;
      AboveCell.Weight := 0.5;
    {$ENDIF}
    end
  {$IFNDEF GridGen}
    else if CellSubdividerAbove < CellSubdivider then
    begin
      InnerNodes := nil;
      try
        if Odd(InnerColIndex) then
        begin
          InnerNodes := NodesRight(ColIndex, RowIndex-1, InnerColIndex div 2, CellSubdividerAbove-1);
        end
        else
        begin
          InnerNodes := NodesLeft(ColIndex, RowIndex-1, InnerColIndex div 2, CellSubdividerAbove-1);
        end;
        if InnerNodes <> nil then
        begin
          ColumnWidth := FLocalGrid.ColumnWidth[ColIndex] / CellSubdividerAbove;
          if Odd(InnerColIndex) then
          begin
            if InnerColIndex < CellSubdivider - 1 then
            begin
              OtherColWidth := ColumnWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex-1, ColIndex+1);
              OtherColWidth := FLocalGrid.ColumnWidth[ColIndex+1] / OtherCellSubdivider;
            end;
          end
          else
          begin
            if InnerColIndex > 0 then
            begin
              OtherColWidth := ColumnWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex-1, ColIndex-1);
              OtherColWidth := FLocalGrid.ColumnWidth[ColIndex-1] / OtherCellSubdivider;
            end;
          end;

          AboveCell := result.Add;
          AboveCell.Cell := RowStart + InnerColIndex div 2;
          AboveCell.Weight := ColumnWidth/2/(ColumnWidth + OtherColWidth);
          OtherWeight := (ColumnWidth/2 + OtherColWidth)/(ColumnWidth + OtherColWidth);

          for NodeIndex := InnerNodes.Count - 1 downto 0 do
          begin
            ACellWeight := InnerNodes[NodeIndex];
            ACellWeight.Collection := result;
            ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
          end;
        end
        else
        begin
          AboveCell := result.Add;
          AboveCell.Cell := RowStart + InnerColIndex div 2;
          AboveCell.Weight := 1;
        end;
      finally
        InnerNodes.Free;
      end;
    end
  {$ENDIF}
    else
    begin
      AboveCell := result.Add;
      AboveCell.Cell := RowStart + InnerColIndex;
      AboveCell.Weight := 1;
    end;
  end;
end;

function TModflowIrregularGrid2D.NodesBelow(ColIndex, RowIndex,
  InnerColIndex, InnerRowIndex: Integer
{$IFDEF GridGen}
  ; NeighborColumnHigher: Boolean
{$ENDIF}
  ): TCellIdCollection;
var
  CellSubdivider: Integer;
  CellSubdividerBelow: Integer;
  BelowCell: TWeightedCellId;
  RowStart: Integer;
  ColumnWidth: double;
  InnerNodes: TCellIdCollection;
  NodeIndex: Integer;
  ACellWeight: TWeightedCellId;
  OtherColWidth: Double;
  OtherWeight: double;
  OtherCellSubdivider: double;
begin
  result := nil;
  CellSubdivider := CellsPerEdge(RowIndex, ColIndex);
  if (RowIndex < FLocalGrid.RowCount -1) or (InnerRowIndex < CellSubdivider -1) then
  begin
    result := TCellIdCollection.Create;
    if (InnerRowIndex < CellSubdivider -1) then
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex]
        + CellSubdivider * (InnerRowIndex+1);

      BelowCell := result.Add;
      BelowCell.Cell := RowStart + InnerColIndex;
      BelowCell.Weight := 1;

      Exit;
    end;
    CellSubdividerBelow := CellsPerEdge(RowIndex+1, ColIndex);

    RowStart := FCellStartCounts[RowIndex + 1, ColIndex];
    if CellSubdividerBelow > CellSubdivider then
    begin
    {$IFDEF GridGen}
      if NeighborColumnHigher then
      begin
        BelowCell := result.Add;
        BelowCell.Cell := RowStart + InnerColIndex*2 + 1;
        BelowCell.Weight := 1;
      end
      else
      begin
        BelowCell := result.Add;
        BelowCell.Cell := RowStart + InnerColIndex*2;
        BelowCell.Weight := 1;
      end;
    {$ELSE}
      BelowCell := result.Add;
      BelowCell.Cell := RowStart + InnerColIndex*2;
      BelowCell.Weight := 0.5;
      BelowCell := result.Add;
      BelowCell.Cell := RowStart + InnerColIndex*2 + 1;
      BelowCell.Weight := 0.5;
    {$ENDIF}
    end
  {$IFNDEF GridGen}
    else if CellSubdividerBelow < CellSubdivider then
    begin
      InnerNodes := nil;
      try
        if Odd(InnerColIndex) then
        begin
          InnerNodes := NodesRight(ColIndex, RowIndex+1, InnerColIndex div 2, 0);
        end
        else
        begin
          InnerNodes := NodesLeft(ColIndex, RowIndex+1, InnerColIndex div 2, 0);
        end;
        if InnerNodes <> nil then
        begin
          ColumnWidth := FLocalGrid.ColumnWidth[ColIndex] / CellSubdividerBelow;
          if Odd(InnerColIndex) then
          begin
            if InnerColIndex < CellSubdivider - 1 then
            begin
              OtherColWidth := ColumnWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex+1, ColIndex+1);
              OtherColWidth := FLocalGrid.ColumnWidth[ColIndex+1] / OtherCellSubdivider;
            end;
          end
          else
          begin
            if InnerColIndex > 0 then
            begin
              OtherColWidth := ColumnWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex+1, ColIndex-1);
              OtherColWidth := FLocalGrid.ColumnWidth[ColIndex-1] / OtherCellSubdivider;
            end;
          end;

          BelowCell := result.Add;
          BelowCell.Cell := RowStart + InnerColIndex div 2;
          BelowCell.Weight := ColumnWidth/2/(ColumnWidth + OtherColWidth);
          OtherWeight := (ColumnWidth/2 + OtherColWidth)/(ColumnWidth + OtherColWidth);

          for NodeIndex := InnerNodes.Count - 1 downto 0 do
          begin
            ACellWeight := InnerNodes[NodeIndex];
            ACellWeight.Collection := result;
            ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
          end;
        end
        else
        begin
          BelowCell := result.Add;
          BelowCell.Cell := RowStart + InnerColIndex div 2;
          BelowCell.Weight := 1;
        end;
      finally
        InnerNodes.Free;
      end;
    end
  {$ENDIF}
    else
    begin
      BelowCell := result.Add;
      BelowCell.Cell := RowStart + InnerColIndex;
      BelowCell.Weight := 1;
    end;
  end;
end;

function TModflowIrregularGrid2D.NodesLeft(ColIndex, RowIndex,
  InnerColIndex, InnerRowIndex: Integer
  {$IFDEF GridGen}
    ; NeighborRowHigher: Boolean
  {$ENDIF}
  ): TCellIdCollection;
var
  CellSubdivider: Integer;
  CellSubdividerLeft: Integer;
  LeftCell: TWeightedCellId;
  RowStart: Integer;
  RowWidth: double;
  InnerNodes: TCellIdCollection;
  NodeIndex: Integer;
  OtherRowWidth: Double;
  OtherCellSubdivider: Integer;
  OtherWeight: double;
  ACellWeight: TWeightedCellId;
begin
  result := nil;
  if (ColIndex > 0) or (InnerColIndex > 0) then
  begin
    result := TCellIdCollection.Create;
    CellSubdivider := CellsPerEdge(RowIndex, ColIndex);
    if InnerColIndex > 0 then
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex]
        + CellSubdivider * InnerRowIndex;

      LeftCell := result.Add;
      LeftCell.Cell := RowStart + InnerColIndex -1;
      LeftCell.Weight := 1;

      Exit;
    end;

    CellSubdividerLeft := CellsPerEdge(RowIndex, ColIndex-1);

    if CellSubdividerLeft > CellSubdivider then
    begin
    {$IFDEF GridGen}
      if NeighborRowHigher then
      begin
        RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
          + CellSubdividerLeft * InnerRowIndex * 2;
        LeftCell := result.Add;
        LeftCell.Cell := RowStart + 2*CellSubdividerLeft -1;
        LeftCell.Weight := 1;
      end
      else
      begin
        RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
          + CellSubdividerLeft * InnerRowIndex * 2;
        LeftCell := result.Add;
        LeftCell.Cell := RowStart + CellSubdividerLeft -1;
        LeftCell.Weight := 1;
      end;
    {$ELSE}
      RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
        + CellSubdividerLeft * InnerRowIndex * 2;
      LeftCell := result.Add;
      LeftCell.Cell := RowStart + CellSubdividerLeft -1;
      LeftCell.Weight := 0.5;
      LeftCell := result.Add;
      LeftCell.Cell := RowStart + 2*CellSubdividerLeft -1;
      LeftCell.Weight := 0.5;
    {$ENDIF}
    end
  {$IFNDEF GridGen}
    else if CellSubdividerLeft < CellSubdivider then
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
        + CellSubdividerLeft * (InnerRowIndex div 2);

      InnerNodes := nil;
      try
        if Odd(InnerRowIndex) then
        begin
          InnerNodes := NodesBelow(ColIndex-1, RowIndex, CellSubdividerLeft-1, InnerRowIndex div 2);
        end
        else
        begin
          InnerNodes := NodesAbove(ColIndex-1, RowIndex, CellSubdividerLeft-1, InnerRowIndex div 2);
        end;
        if InnerNodes <> nil then
        begin
          RowWidth := FLocalGrid.RowWidth[RowIndex] / CellSubdividerLeft;
          if Odd(InnerRowIndex) then
          begin
            if InnerRowIndex < CellSubdivider - 1 then
            begin
              OtherRowWidth := RowWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex+1, ColIndex-1);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex+1] / OtherCellSubdivider;
            end;
          end
          else
          begin
            if InnerRowIndex > 0 then
            begin
              OtherRowWidth := RowWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex-1, ColIndex-1);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex-1] / OtherCellSubdivider;
            end;
          end;

          LeftCell := result.Add;
          LeftCell.Cell := RowStart + InnerColIndex div 2;
          LeftCell.Weight := RowWidth/2/(RowWidth + OtherRowWidth);
          OtherWeight := (RowWidth/2 + OtherRowWidth)/(RowWidth + OtherRowWidth);

          for NodeIndex := InnerNodes.Count - 1 downto 0 do
          begin
            ACellWeight := InnerNodes[NodeIndex];
            ACellWeight.Collection := result;
            ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
          end;
        end
        else
        begin
          LeftCell := result.Add;
          LeftCell.Cell := RowStart + CellSubdivider -1;
          LeftCell.Weight := 1;
        end;
      finally
        InnerNodes.Free;
      end;
    end
  {$ENDIF}
    else
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
        + CellSubdividerLeft * (InnerRowIndex div 2);
//      RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
//        + CellSubdividerLeft * InnerRowIndex;
      LeftCell := result.Add;
      LeftCell.Cell := RowStart + CellSubdividerLeft -1;
      LeftCell.Weight := 1;
    end;
  end;
end;

function TModflowIrregularGrid2D.NodesRight(ColIndex, RowIndex,
  InnerColIndex, InnerRowIndex: Integer
{$IFDEF GridGen}
  ; NeighborRowHigher: Boolean
{$ENDIF}
  ): TCellIdCollection;
var
  CellSubdivider: Integer;
  CellSubdividerRight: Integer;
  RightCell: TWeightedCellId;
  RowStart: Integer;
  RowWidth: double;
  InnerNodes: TCellIdCollection;
  NodeIndex: Integer;
  OtherRowWidth: Double;
  OtherCellSubdivider: Integer;
  OtherWeight: double;
  ACellWeight: TWeightedCellId;
begin
  result := nil;
  CellSubdivider := CellsPerEdge(RowIndex, ColIndex);
  if (ColIndex < FLocalGrid.ColumnCount-1) or (InnerColIndex < CellSubdivider-1) then
  begin
    result := TCellIdCollection.Create;
    if (InnerColIndex < CellSubdivider-1) then
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex]
        + CellSubdivider * InnerRowIndex;

      RightCell := result.Add;
      RightCell.Cell := RowStart + InnerColIndex + 1;
      RightCell.Weight := 1;

      Exit;
    end;
    CellSubdividerRight := CellsPerEdge(RowIndex, ColIndex+1);

    if CellSubdividerRight > CellSubdivider then
    begin
    {$IFDEF GridGen}
      if NeighborRowHigher then
      begin
        RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
          + CellSubdividerRight * InnerRowIndex * 2
          + CellSubdividerRight;
        RightCell := result.Add;
        RightCell.Cell := RowStart + CellSubdividerRight;
        RightCell.Weight := 1;
      end
      else
      begin
        RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
          + CellSubdividerRight * InnerRowIndex * 2;
        RightCell := result.Add;
        RightCell.Cell := RowStart;
        RightCell.Weight := 1;
      end;
    {$ELSE}
      RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
        + CellSubdividerRight * InnerRowIndex * 2;
      RightCell := result.Add;
      RightCell.Cell := RowStart;
      RightCell.Weight := 0.5;
      RightCell := result.Add;
      RightCell.Cell := RowStart + CellSubdividerRight;
      RightCell.Weight := 0.5;
    {$ENDIF}
    end
  {$IFNDEF GridGen}
    else if CellSubdividerRight < CellSubdivider then
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
        + CellSubdividerRight * (InnerRowIndex div 2);
      InnerNodes := nil;
      try
        if Odd(InnerRowIndex) then
        begin
          InnerNodes := NodesBelow(ColIndex+1, RowIndex, 0, InnerRowIndex div 2);
        end
        else
        begin
          InnerNodes := NodesAbove(ColIndex+1, RowIndex, 0, InnerRowIndex div 2);
        end;

        if InnerNodes <> nil then
        begin
          RowWidth := FLocalGrid.RowWidth[RowIndex] / CellSubdividerRight;
          if Odd(InnerRowIndex) then
          begin
            if InnerRowIndex < CellSubdivider - 1 then
            begin
              OtherRowWidth := RowWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex+1, ColIndex+1);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex+1] / OtherCellSubdivider;
            end;
          end
          else
          begin
            if InnerRowIndex > 0 then
            begin
              OtherRowWidth := RowWidth;
            end
            else
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex-1, ColIndex+1);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex-1] / OtherCellSubdivider;
            end;
          end;

          RightCell := result.Add;
          RightCell.Cell := RowStart;
          RightCell.Weight := RowWidth/2/(RowWidth + OtherRowWidth);
          OtherWeight := (RowWidth/2 + OtherRowWidth)/(RowWidth + OtherRowWidth);

          for NodeIndex := InnerNodes.Count - 1 downto 0 do
          begin
            ACellWeight := InnerNodes[NodeIndex];
            ACellWeight.Collection := result;
            ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
          end;
        end
        else
        begin
          RightCell := result.Add;
          RightCell.Cell := RowStart;
          RightCell.Weight := 1;
        end;

      finally
        InnerNodes.Free;
      end;
    end
  {$ENDIF}
    else
    begin
      RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
        + CellSubdividerRight * InnerRowIndex;
      RightCell := result.Add;
      RightCell.Cell := RowStart;
      RightCell.Weight := 1;
    end;
  end;
end;

function TModflowIrregularGrid2D.OkLocation(const DataSet: TDataArray;
  const Layer, Row, Col: integer): boolean;
var
  IDomain: TDataArray;
begin
  result := inherited;
  if result and DataSet.Limits.ActiveOnly then
  begin
    IDomain := (Model as TCustomModel).
      DataArrayManager.GetDataSetByName(K_IDOMAIN);
    result := IDomain.IntegerData[Layer, Row, Col] > 0;
  end;
end;

procedure TModflowIrregularGrid2D.CreateGhostNodes;
var
//  BelowWeightCells: TIrregular2DCellList;
  Refinement: Integer;
  RowStart: Integer;
  ContainingCell: TModflowIrregularCell2D;
  LeftLinkedCell: TModflowIrregularCell2D;
  CellSubdividerBelow: Integer;
  RightLinkedCell: TModflowIrregularCell2D;
  RowIndex: Integer;
  ColIndex: Integer;
  InnerColIndex: Integer;
  CellSubdivider: Integer;
//  BelowDistance: double;
  CellSubdividerAbove: Integer;
  LeftLinkedCells: TCellIdCollection;
  RightLinkedCells: TCellIdCollection;
  ColumnWidth: double;
  RowWidth: double;
  AGhostNode: TGhostNode;
  ACellWeight: TWeightedCellId;
  OtherColumnWidth: double;
  Weight: double;
  NodeIndex: Integer;
  OtherWeight: double;
  CellSubdividerLeft: Integer;
  CellSubdividerRight: Integer;
//  AStringList: TStringList;
//  GhostNodeIndex: Integer;
//  AStringBuilder: TStringBuilder;
  InnerRowIndex: Integer;
  OtherRowWidth: Double;
  AboveLinkedCells: TCellIdCollection;
  BelowLinkedCell: TModflowIrregularCell2D;
  BelowLinkedCells: TCellIdCollection;
  AboveLinkedCell: TModflowIrregularCell2D;
  OtherCellSubdivider: Integer;
begin
  for RowIndex := 0 to FLocalGrid.RowCount - 1 do
  begin
    for ColIndex := 0 to FLocalGrid.ColumnCount - 1 do
    begin
      Refinement := FQuadRefinement[RowIndex, ColIndex];
      CellSubdivider := CellsPerEdge(RowIndex, ColIndex);
      ColumnWidth := FLocalGrid.ColumnWidth[ColIndex] / CellSubdivider;
      RowWidth := FLocalGrid.RowWidth[RowIndex] / CellSubdivider;

      if (RowIndex > 0)
        and (FQuadRefinement[RowIndex - 1, ColIndex] > Refinement) then
      begin
        CellSubdividerAbove := CellsPerEdge(RowIndex - 1, ColIndex);

        RowStart := FCellStartCounts[RowIndex - 1, ColIndex]
          + CellSubdividerAbove * (CellSubdividerAbove - 1);
        for InnerColIndex := 0 to CellSubdivider - 1 do
        begin
          if InnerColIndex > 0 then
          begin
            OtherColumnWidth := ColumnWidth
          end
          else
          begin
            if ColIndex > 0 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex, ColIndex-1);
              OtherColumnWidth := FLocalGrid.ColumnWidth[ColIndex-1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (ColumnWidth/2 + OtherColumnWidth)/(ColumnWidth + OtherColumnWidth);
          OtherWeight := ColumnWidth/2/(ColumnWidth + OtherColumnWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + InnerColIndex];
          LeftLinkedCell := Cells[RowStart + InnerColIndex * 2];

          LeftLinkedCells := nil;
          try
            LeftLinkedCells := NodesLeft(ColIndex, RowIndex,
              InnerColIndex, 0
            {$IFDEF GridGen}
              , False
            {$ENDIF}
              );
            if LeftLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := LeftLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := LeftLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := LeftLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            LeftLinkedCells.Free;
          end;
        end;

        for InnerColIndex := 0 to CellSubdivider - 1 do
        begin
          if InnerColIndex < CellSubdivider - 1 then
          begin
            OtherColumnWidth := ColumnWidth
          end
          else
          begin
            if ColIndex < FLocalGrid.ColumnCount -1 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex, ColIndex+1);
              OtherColumnWidth := FLocalGrid.ColumnWidth[ColIndex+1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (ColumnWidth/2 + OtherColumnWidth)/(ColumnWidth + OtherColumnWidth);
          OtherWeight := ColumnWidth/2/(ColumnWidth + OtherColumnWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + InnerColIndex];
          RightLinkedCell := Cells[RowStart + InnerColIndex * 2 + 1];
          RightLinkedCells := nil;
          try
            RightLinkedCells := NodesRight(ColIndex, RowIndex,
              InnerColIndex, 0
            {$IFDEF GridGen}
              , False
            {$ENDIF}
              );
            if RightLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := RightLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := RightLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := RightLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            RightLinkedCells.Free;
          end;
        end;
      end;

      if (RowIndex < FLocalGrid.RowCount -1)
        and (FQuadRefinement[RowIndex + 1, ColIndex] > Refinement) then
      begin
        CellSubdividerBelow := CellsPerEdge(RowIndex + 1, ColIndex);

        RowStart := FCellStartCounts[RowIndex + 1, ColIndex];
//          + CellSubdividerBelow * (CellSubdividerBelow - 1);
        for InnerColIndex := 0 to CellSubdivider - 1 do
        begin
          if InnerColIndex > 0 then
          begin
            OtherColumnWidth := ColumnWidth
          end
          else
          begin
            if ColIndex > 0 then
            begin
              CellSubdividerLeft := CellsPerEdge(RowIndex, ColIndex-1);
              OtherColumnWidth := FLocalGrid.ColumnWidth[ColIndex-1] / CellSubdividerLeft;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (ColumnWidth/2 + OtherColumnWidth)/(ColumnWidth + OtherColumnWidth);
          OtherWeight := ColumnWidth/2/(ColumnWidth + OtherColumnWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + (CellSubdivider -1) * CellSubdivider
            + InnerColIndex];
          LeftLinkedCell := Cells[RowStart + InnerColIndex * 2];

          LeftLinkedCells := nil;
          try
            LeftLinkedCells := NodesLeft(ColIndex, RowIndex,
              InnerColIndex, CellSubdivider-1
            {$IFDEF GridGen}
              , True
            {$ENDIF}
              );
            if LeftLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := LeftLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := LeftLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := LeftLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            LeftLinkedCells.Free;
          end;
        end;

        for InnerColIndex := 0 to CellSubdivider - 1 do
        begin
          if InnerColIndex < CellSubdivider - 1 then
          begin
            OtherColumnWidth := ColumnWidth
          end
          else
          begin
            if ColIndex < FLocalGrid.ColumnCount -1 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex, ColIndex+1);
              OtherColumnWidth := FLocalGrid.ColumnWidth[ColIndex+1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (ColumnWidth/2 + OtherColumnWidth)/(ColumnWidth + OtherColumnWidth);
          OtherWeight := ColumnWidth/2/(ColumnWidth + OtherColumnWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + (CellSubdivider -1) * CellSubdivider
            + InnerColIndex];
          RightLinkedCell := Cells[RowStart + InnerColIndex * 2 + 1];
          RightLinkedCells := nil;
          try
            RightLinkedCells := NodesRight(ColIndex, RowIndex,
              InnerColIndex, CellSubdivider-1
            {$IFDEF GridGen}
              , True
            {$ENDIF}
              );
            if RightLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := RightLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := RightLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := RightLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            RightLinkedCells.Free;
          end;
        end;
      end;

      if (ColIndex > 0)
        and (FQuadRefinement[RowIndex, ColIndex - 1] > Refinement) then
      begin
        CellSubdividerLeft := CellsPerEdge(RowIndex, ColIndex - 1);

        for InnerRowIndex := 0 to CellSubdivider - 1 do
        begin
          RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
            + CellSubdividerLeft * InnerRowIndex *2;
          if InnerRowIndex > 0 then
          begin
            OtherRowWidth := RowWidth
          end
          else
          begin
            if RowIndex > 0 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex-1, ColIndex);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex-1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (RowWidth/2 + OtherRowWidth)/(RowWidth + OtherRowWidth);
          OtherWeight := RowWidth/2/(RowWidth + OtherRowWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + InnerRowIndex*CellSubdivider];
          AboveLinkedCell := Cells[RowStart + CellSubdividerLeft -1];

          AboveLinkedCells := nil;
          try
            AboveLinkedCells := NodesAbove(ColIndex, RowIndex,
              0, InnerRowIndex
            {$IFDEF GridGen}
              , False
            {$ENDIF}
              );
            if AboveLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := AboveLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := AboveLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := AboveLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            AboveLinkedCells.Free;
          end;
        end;

        for InnerRowIndex := 0 to CellSubdivider - 1 do
        begin
          RowStart := FCellStartCounts[RowIndex, ColIndex - 1]
            + CellSubdividerLeft * (InnerRowIndex* 2 +1);
          if InnerRowIndex < CellSubdivider - 1 then
          begin
            OtherRowWidth := RowWidth
          end
          else
          begin
            if RowIndex < FLocalGrid.RowCount -1 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex+1, ColIndex);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex+1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (RowWidth/2 + OtherRowWidth)/(RowWidth + OtherRowWidth);
          OtherWeight := RowWidth/2/(RowWidth + OtherRowWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + InnerRowIndex * CellSubdivider];
          BelowLinkedCell := Cells[RowStart + CellSubdividerLeft -1];
          BelowLinkedCells := nil;
          try
            BelowLinkedCells := NodesBelow(ColIndex, RowIndex,
              0, InnerRowIndex
            {$IFDEF GridGen}
              , False
            {$ENDIF}
            );
            if BelowLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := BelowLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := BelowLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := BelowLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            BelowLinkedCells.Free;
          end;
        end;
      end;

      if (ColIndex < FLocalGrid.ColumnCount -1)
        and (FQuadRefinement[RowIndex, ColIndex + 1] > Refinement) then
      begin
        CellSubdividerRight := CellsPerEdge(RowIndex, ColIndex + 1);

        for InnerRowIndex := 0 to CellSubdivider - 1 do
        begin
          RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
            + CellSubdividerRight * InnerRowIndex *2;
          if InnerRowIndex > 0 then
          begin
            OtherRowWidth := RowWidth
          end
          else
          begin
            if RowIndex > 0 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex-1, ColIndex);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex-1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (RowWidth/2 + OtherRowWidth)/(RowWidth + OtherRowWidth);
          OtherWeight := RowWidth/2/(RowWidth + OtherRowWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + (InnerRowIndex+1)*CellSubdivider -1];
          AboveLinkedCell := Cells[RowStart];

          AboveLinkedCells := nil;
          try
            AboveLinkedCells := NodesAbove(ColIndex, RowIndex,
              CellSubdivider-1, InnerRowIndex
            {$IFDEF GridGen}
              , True
            {$ENDIF}
              );
            if AboveLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := AboveLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := AboveLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := AboveLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            AboveLinkedCells.Free;
          end;
        end;

        for InnerRowIndex := 0 to CellSubdivider - 1 do
        begin
          RowStart := FCellStartCounts[RowIndex, ColIndex + 1]
            + CellSubdividerRight * (InnerRowIndex* 2 +1);
          if InnerRowIndex < CellSubdivider - 1 then
          begin
            OtherRowWidth := RowWidth
          end
          else
          begin
            if RowIndex < FLocalGrid.RowCount -1 then
            begin
              OtherCellSubdivider := CellsPerEdge(RowIndex+1, ColIndex);
              OtherRowWidth := FLocalGrid.RowWidth[RowIndex+1] / OtherCellSubdivider;
            end
            else
            begin
              Continue
            end;
          end;
          Weight := (RowWidth/2 + OtherRowWidth)/(RowWidth + OtherRowWidth);
          OtherWeight := RowWidth/2/(RowWidth + OtherRowWidth);

          ContainingCell := Cells[FCellStartCounts[RowIndex, ColIndex]
            + (InnerRowIndex + 1) * CellSubdivider - 1];
          BelowLinkedCell := Cells[RowStart];
          BelowLinkedCells := nil;
          try
            BelowLinkedCells := NodesBelow(ColIndex, RowIndex,
              CellSubdivider-1, InnerRowIndex
            {$IFDEF GridGen}
              , True
            {$ENDIF}
              );
            if BelowLinkedCells <> nil then
            begin
              AGhostNode := GhostNodes.Add;
              AGhostNode.ContainingCell.Cell := ContainingCell.ElementNumber;
              AGhostNode.LinkedCell.Cell := BelowLinkedCell.ElementNumber;

              ACellWeight := AGhostNode.CellWeights.Add;
              ACellWeight.Cell := ContainingCell.ElementNumber;
              ACellWeight.Weight := Weight;

              for NodeIndex := BelowLinkedCells.Count -1 downto 0 do
              begin
                ACellWeight := BelowLinkedCells[NodeIndex];
                ACellWeight.Collection := AGhostNode.CellWeights;
                ACellWeight.Weight := ACellWeight.Weight * OtherWeight;
              end;
            end;
          finally
            BelowLinkedCells.Free;
          end;
        end;
      end;
    end;
  end;

//  AStringList := TStringList.Create;
//  AStringBuilder := TStringBuilder.Create;
//  try
//    for GhostNodeIndex := 0 to GhostNodes.Count - 1 do
//    begin
//      AGhostNode := GhostNodes[GhostNodeIndex];
//      AStringBuilder.Append(AGhostNode.ContainingCell.Cell+1);
//      AStringBuilder.Append(' ');
//      AStringBuilder.Append(AGhostNode.LinkedCell.Cell+1);
//      AStringBuilder.Append(' ');
//      for NodeIndex := 0 to AGhostNode.CellWeights.Count - 1 do
//      begin
//        ACellWeight := AGhostNode.CellWeights[NodeIndex];
//        AStringBuilder.Append(ACellWeight.Cell+1);
//        AStringBuilder.Append(' ');
//        AStringBuilder.Append(ACellWeight.Weight);
//        AStringBuilder.Append(' ');
//      end;
//      AStringList.Add(AStringBuilder.ToString);
//      AStringBuilder.Clear;
//    end;
//    AStringList.SaveToFile('C:\Colab\GWModelTools\ModelMuse\GhostNodes.txt');
//  finally
//    AStringBuilder.Free;
//    AStringList.Free;
//  end;


end;

function TModflowIrregularGrid2D.MeshLimits: TGridLimit;
var
  Node: INode;
  NodeIndex: Integer;
begin
  result.MinX := 0;
  result.MaxX := 0;
  result.MinY := 0;
  result.MaxY := 0;
  result.MinZ := 0;
  result.MaxZ := 0;
  if NodeCount > 0 then
  begin
    Node := Nodes[0];
    result.MinX := Node.Location.X;
    result.MinY := Node.Location.Y;
    result.MaxX := Node.Location.X;
    result.MaxY := Node.Location.Y;
    for NodeIndex := 1 to NodeCount - 1 do
    begin
      Node := Nodes[NodeIndex];
//      if Node.NodeType = ntEdge then
      begin
        if Node.Location.X > result.MaxX then
        begin
          result.MaxX := Node.Location.X
        end;
        if Node.Location.X < result.MinX then
        begin
          result.MinX := Node.Location.X
        end;
        if Node.Location.Y > result.MaxY then
        begin
          result.MaxY := Node.Location.Y
        end;
        if Node.Location.Y < result.MinY then
        begin
          result.MinY := Node.Location.Y
        end;
      end;
    end;
  end;

end;

function TModflowIrregularGrid2D.MeshOutline: TPolygon2D;
var
//  CellPolygon1: TGpcPolygonClass;
//  CellPolygon2: TGpcPolygonClass;
//  UnionPolygon: TGpcPolygonClass;
//  ACell: TModflowIrregularCell2D;
//  AVertex: Tgpc_vertex;
//  CellIndex: Integer;
//  NodeIndex: Integer;
  Outline: TPolygon2Darray;
  PolyArea: double;
  PolyIndex: Integer;
  TestArea: double;
  procedure AssignCellToPolygon(ACell: TModflowIrregularCell2D;
    APolygon: TGpcPolygonClass);
  var  
    NodeIndex: Integer;
    AVertex: Tgpc_vertex;
    ANode: TModflowNode;
  begin
    APolygon.NumberOfContours := 1;
    APolygon.VertexCount[0] := ACell.ElementCorners.Count;
    for NodeIndex := 0 to ACell.ElementCorners.Count -1 do
    begin
      ANode := ACell.ElementCorners[NodeIndex];
      AVertex.X := ANode.X;
      AVertex.Y := ANode.Y;
      APolygon.Vertices[0,NodeIndex] := AVertex;
    end;
  end;
begin
  if FStoredOutline <> nil then
  begin
    result := FStoredOutline;
    Exit;
  end;
  if ElementCount = 0 then
  begin
    result := nil;
  end
  else
  begin
    FMesh.GetActiveOutlineTop(-1, Outline);
    result := Outline[0];
    if Length(Outline) > 1 then
    begin
      PolyArea := Area(result);
      for PolyIndex := 1 to Length(Outline) -1 do   
      begin
        TestArea := Area(Outline[PolyIndex]);
        if TestArea > PolyArea then
        begin
          result := Outline[PolyIndex];
          PolyArea := TestArea;
        end; 
      end;
    end;


    {UnionPolygon := TGpcPolygonClass.Create;
    CellPolygon2 := TGpcPolygonClass.Create;
    try
      ACell := Cells[0];
      AssignCellToPolygon(ACell, UnionPolygon);
      CellPolygon1 := UnionPolygon;
      for CellIndex := 1 to ElementCount - 1 do
      begin
        ACell := Cells[CellIndex];
        AssignCellToPolygon(ACell, CellPolygon2);
        
        UnionPolygon := TGpcPolygonClass.CreateFromOperation(
          GPC_Union, CellPolygon1, CellPolygon2);
        CellPolygon1.Free;
        CellPolygon1 := UnionPolygon;
      end;
      
      if CellPolygon1.NumberOfContours > 1 then
      begin
        CellPolygon2.NumberOfContours := 0;
        UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_Union, CellPolygon1, CellPolygon2);
        CellPolygon1.Free;
//        CellPolygon1 := UnionPolygon;
      end;

      SetLength(result, UnionPolygon.VertexCount[0] + 1);
      for NodeIndex := 0 to UnionPolygon.VertexCount[0] -1 do
      begin
        AVertex := UnionPolygon.Vertices[0,NodeIndex];
        result[NodeIndex].X := AVertex.X;
        result[NodeIndex].Y := AVertex.Y;
      end;
      result[Length(result)-1] := result[0];
    
    finally
      CellPolygon2.Free;
      UnionPolygon.Free;
    end;}
  end;

  FStoredOutline := result;


end;

procedure TModflowIrregularGrid2D.SetCellCorners(const Value: TModflowNodes);
begin
  FCellCorners.Assign(Value);
end;

procedure TModflowIrregularGrid2D.SetCells(
  const Value: TModflowIrregularCell2DCollection);
begin
  FCells.Assign(Value);
end;

procedure TModflowIrregularGrid2D.SetDrawCellNumbers(const Value: Boolean);
begin
  FDrawCellNumbers := Value;
end;

procedure TModflowIrregularGrid2D.SetDrawingChoice(const Value: TDrawingChoice);
begin
  if FDrawingChoice <> Value then
  begin
    FDrawingChoice := Value;
    InvalidateModel;
  end;
end;

procedure TModflowIrregularGrid2D.SetGhostNodes(const Value: TGhostNodes);
begin
  FGhostNodes.Assign(Value);
end;

procedure TModflowIrregularGrid2D.SetGridLineDrawingChoice(
  const Value: TGridLineDrawingChoice);
begin
  FGridLineDrawingChoice := Value;
end;

procedure TModflowIrregularGrid2D.SetNumberFont(const Value: TFont);
begin
  FNumberFont.Assign(Value);
end;

procedure TModflowIrregularGrid2D.SetSelectedLayer(const Value: integer);
begin
  FMesh.SelectedLayer := Value;
end;

procedure TModflowIrregularGrid2D.SetThreeDContourDataSet(
  const Value: TDataArray);
begin
  if FThreeDContourDataSet <> Value then
  begin
    FThreeDContourDataSet := Value;
  end;
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.FrontDiscretizationChanged := True;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TModflowIrregularGrid2D.SetThreeDDataSet(const Value: TDataArray);
begin
  if FThreeDDataSet <> Value then
  begin
    FThreeDDataSet := Value;
    frmGoPhast.InvalidateImage32AllViews;
  end;
end;

procedure TModflowIrregularGrid2D.SetTopContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopContourDataSet <> Value then
  begin
    FTopContourDataSet := Value;
  end;
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TModflowIrregularGrid2D.SetTopDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopDataSet <> Value then
  begin
    FTopDataSet := Value;
    frmGoPhast.TopDiscretizationChanged := True;
    frmGoPhast.InvalidateImage32AllViews;
  end;
end;

function TModflowIrregularGrid2D.TopContainingCellOrElement(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  case EvaluatedAt of
    eaBlocks:
      begin
        result := Cells.TopContainingElement(APoint)
      end;
    eaNodes:
      begin
        result := CellCorners.TopContainingCell(APoint)
      end;
    else Assert(False);
  end;
end;

procedure TModflowIrregularGrid2D.UpdateNodesAndCells;
var
  NodeIndex: Integer;
  ANode: TModflowNode;
  CellIndex: Integer;
  ACell: TModflowIrregularCell2D;
  NodeNumber: Integer;
begin
  for NodeIndex := 0 to CellCorners.Count - 1 do
  begin
    ANode := CellCorners[NodeIndex];
    ANode.FCells.Clear;
  end;
  for CellIndex := 0 to Cells.Count - 1 do
  begin
    ACell := Cells[CellIndex];
    ACell.ElementCorners.Clear;
    for NodeIndex := 0 to ACell.NodeNumbers.Count - 1 do
    begin
      NodeNumber := ACell.NodeNumbers[NodeIndex].Value;
      Assert(NodeNumber >= 0);
      Assert(NodeNumber < CellCorners.Count);
      ANode := CellCorners[NodeNumber];
      Assert(ACell.ElementCorners.IndexOf(ANode) < 0);
      ACell.ElementCorners.Add(ANode);
      Assert(ANode.FCells.IndexOf(ACell)< 0);
      ANode.FCells.Add(ACell);
    end;
  end;
end;

function TModflowIrregularGrid2D._AddRef: Integer;
begin
  result := -1;
end;

function TModflowIrregularGrid2D._Release: Integer;
begin
  result := -1;
end;

procedure TLocationItem.SetX(const Value: double);
begin
  SetRealProperty(FLocation.X, Value);
end;

procedure TLocationItem.ReadX(Reader: TReader);
begin
  X := Reader.ReadFloat;
end;

procedure TLocationItem.WriteX(Writer: TWriter);
begin
  Writer.WriteFloat(X);
end;

procedure TLocationItem.ReadStringX(Reader: TReader);
begin
  X := StrToFloat(Reader.ReadString)
end;

procedure TLocationItem.WriteStringX(Writer: TWriter);
begin
  Writer.WriteString(FloatToStrF(X, ffGeneral, 16, 18, GetLFormatSettings));
end;

procedure TLocationItem.SetY(const Value: double);
begin
  SetRealProperty(FLocation.Y, Value);
end;

procedure TLocationItem.ReadY(Reader: TReader);
begin
  Y := Reader.ReadFloat;
end;

procedure TLocationItem.WriteY(Writer: TWriter);
begin
  Writer.WriteFloat(Y);
end;

procedure TLocationItem.ReadStringY(Reader: TReader);
begin
  Y := StrToFloat(Reader.ReadString)
end;

procedure TLocationItem.WriteStringY(Writer: TWriter);
begin
  Writer.WriteString(FloatToStrF(Y, ffGeneral, 16, 18, GetLFormatSettings));
end;

procedure TLocationItem.Assign(Source: TPersistent);
var
  SourceLocation: TLocationItem;
begin
  if Source is TLocationItem then
  begin
    SourceLocation := TLocationItem(Source);
    Location := SourceLocation.Location
  end
  else
  begin
    inherited;
  end;

end;

procedure TLocationItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('X', ReadX, WriteX, X = 0);
  Filer.DefineProperty('StringX', ReadStringX, WriteStringX,
    (X <> 0) and (Abs(X) < 1E-10));
  Filer.DefineProperty('Y', ReadY, WriteY, Y = 0);
  Filer.DefineProperty('StringY', ReadStringY, WriteStringY,
    (Y <> 0) and (Abs(Y) < 1E-10));
end;

{ TModflowDisVCell }

procedure TModflowDisVCell.Assign(Source: TPersistent);
var
  SourceCell: TModflowDisVCell;
begin
  if Source is TModflowDisVCell then
  begin
    SourceCell := TModflowDisVCell(Source);
    Top := SourceCell.Top;
    Bottom := SourceCell.Bottom;
    Cell2D := SourceCell.Cell2D;
  end
  else
  begin
    inherited;
  end;
end;

function TModflowDisVCell.CenterElevation: double;
begin
 result := (UpperElevation + LowerElevation)/2;
end;

function TModflowDisVCell.GetCenterLocation: TPoint3D;
begin
  result.x := FCell2D.X;
  result.y := FCell2D.Y;
  result.Z := CenterElevation;
end;

procedure TModflowDisVCell.Draw3D(Layer: Integer);
var
  X, Y, Z: single;
  NodeIndex: Integer;
  ANode: TModflowNode;
  PriorNode: TModflowNode;
begin
  glBegin(GL_POLYGON);
  try
    for NodeIndex := 0 to Cell2D.ElementCorners.Count - 1 do
    begin
      ANode := Cell2D.ElementCorners[NodeIndex];
      X := ANode.X;
      Y := ANode.Y;
      Z := ANode.FTopElevations[Layer];
      glVertex3f(X, Y, Z);
    end;
  finally
    glEnd;
  end;

  glBegin(GL_POLYGON);
  try
    for NodeIndex := 0 to Cell2D.ElementCorners.Count - 1 do
    begin
      ANode := Cell2D.ElementCorners[NodeIndex];
      X := ANode.X;
      Y := ANode.Y;
      Z := ANode.FBottomElevations[Layer];
      glVertex3f(X, Y, Z);
    end;
  finally
    glEnd;
  end;

  PriorNode := Cell2D.ElementCorners.Last;
  for NodeIndex := 0 to Cell2D.ElementCorners.Count - 1 do
  begin
    glBegin(GL_POLYGON);
    try
      ANode := Cell2D.ElementCorners[NodeIndex];
      X := ANode.X;
      Y := ANode.Y;
      Z := ANode.FTopElevations[Layer];
      glVertex3f(X, Y, Z);


      Z := ANode.FBottomElevations[Layer];
      glVertex3f(X, Y, Z);

      X := PriorNode.X;
      Y := PriorNode.Y;
      Z := PriorNode.FBottomElevations[Layer];
      glVertex3f(X, Y, Z);


      Z := PriorNode.FTopElevations[Layer];
      glVertex3f(X, Y, Z);

      PriorNode := ANode;
    finally
      glEnd;
    end;
  end;

end;

function TModflowDisVCell.ElementNumber2D: Integer;
begin
  result := Cell2D.ElementNumber;
end;

function TModflowDisVCell.GetActive: Boolean;
begin
  result := FActive;
end;

function TModflowDisVCell.GetActiveNode(Index: integer): INode;
begin
  result := nil;
end;

function TModflowDisVCell.GetActiveNodeCount: integer;
begin
  result := Cell2D.NodeCount*2;
end;

function TModflowDisVCell.GetDisplayNumber: Integer;
begin
  result := GetElementNumber + 1;
end;

function TModflowDisVCell.GetElementNumber: integer;
begin
  result := Cell2D.ElementNumber;
end;

function TModflowDisVCell.GetLayer: Integer;
begin
  Result := (Collection as TModflowIrregularLayer).Layer;
end;

function TModflowDisVCell.GetNodeI(Index: integer): INode3D;
begin
  result := nil;
end;

function TModflowDisVCell.GetNodeLocation(Index: integer): TPoint3D;
var
  ANode: TModflowNode;
  APoint2D: TPoint2D;
begin
  ANode := Cell2D.ElementCorners[Index mod Cell2D.ElementCorners.Count];
  APoint2D := ANode.Location;
  result.X := APoint2D.X;
  result.Y := APoint2D.Y;
  if Index >= Cell2D.ElementCorners.Count then
  begin
    result.Z := ANode.FBottomElevations[Layer];
  end
  else
  begin
    result.Z := ANode.FTopElevations[Layer];
  end;
end;

function TModflowDisVCell.LowerElevation: double;
begin
  result := Bottom;
end;

function TModflowDisVCell.ReferenceLength: double;
begin
  result := Sqrt(Sqr(FCell2D.ReferenceLength)
    + Sqr(Top-Bottom));
end;

procedure TModflowDisVCell.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      FActiveElements.Add(self);
    end
    else
    begin
      FActiveElements.Remove(self);
    end;
  end;
end;

procedure TModflowDisVCell.SetBottom(const Value: Double);
begin
  SetRealProperty(FBottom, Value);
end;

procedure TModflowDisVCell.SetCell2D(const Value: TModflowIrregularCell2D);
begin
  FCell2D := Value;
end;

procedure TModflowDisVCell.SetElementNumber(Value: integer);
begin
  Cell2D.ElementNumber := Value;
end;

procedure TModflowDisVCell.SetTop(const Value: Double);
begin
  SetRealProperty(FTop, Value);
end;

function TModflowDisVCell.Thickness: double;
begin
  result := Top - Bottom;
end;

function TModflowDisVCell.UpperElevation: double;
begin
  result := Top;
end;

{ TModflowIrregularLayer }

function TModflowIrregularLayer.Add: TModflowDisVCell;
begin
  result := inherited Add as TModflowDisVCell;
end;

constructor TModflowIrregularLayer.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TModflowDisVCell, InvalidateModelEvent);
end;

function TModflowIrregularLayer.GetItem(Index: Integer): TModflowDisVCell;
begin
  result := inherited Items[Index] as TModflowDisVCell;
end;

function TModflowIrregularLayer.GetLayer: integer;
begin
  result := FLayerItem.Index;
end;

procedure TModflowIrregularLayer.SetItem(Index: Integer;
  const Value: TModflowDisVCell);
begin
  inherited Items[Index] := Value;
end;

{ TModflowIrregLayerItemd }

procedure TModflowIrregLayerItem.Assign(Source: TPersistent);
begin
  if Source is TModflowIrregLayerItem then
  begin
    Layer := TModflowIrregLayerItem(Source).Layer;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowIrregLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FLayer := TModflowIrregularLayer.Create(OnInvalidateModel);
  FLayer.FLayerItem := self;
end;

destructor TModflowIrregLayerItem.Destroy;
begin
  FLayer.Free;
  inherited;
end;

function TModflowIrregLayerItem.GetCount: Integer;
begin
  result := Layer.Count;
end;

procedure TModflowIrregLayerItem.SetCount(const Value: Integer);
begin
  Layer.Count := Value;
end;

procedure TModflowIrregLayerItem.SetLayer(const Value: TModflowIrregularLayer);
begin
  FLayer.Assign(Value);
end;

{ TModflowIrregularLayers }

function TModflowIrregularLayers.Add: TModflowIrregLayerItem;
begin
  result := inherited Add as TModflowIrregLayerItem;
end;

constructor TModflowIrregularLayers.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TModflowIrregLayerItem, InvalidateModelEvent);
end;

function TModflowIrregularLayers.GetItem(
  Index: Integer): TModflowIrregLayerItem;
begin
  result := inherited Items[Index] as TModflowIrregLayerItem;
end;

procedure TModflowIrregularLayers.SetItem(Index: Integer;
  const Value: TModflowIrregLayerItem);
begin
  inherited Items[Index] := Value;
end;

{ TModflowDisvGrid }

procedure TModflowDisvGrid.Assign(Source: TPersistent);
var
  SourceDisv: TModflowDisvGrid;
  RowIndex: Integer;
  LocalGrid: TCustomModelGrid;
  ColIndex: Integer;
  LayerIndex: Integer;
  ALayer: TModflowIrregularLayer;
  ACell2D: TModflowIrregularCell2D;
  ACell3D: TModflowDisVCell;
  CellIndex2D: Integer;
  ActiveDataArray: TDataArray;
  Refinement: Integer;
  CellIndex: Integer;
  procedure ClearVariables;
  begin
    FreeAndNil(FElementIntervalTree);
    FStoredBlockLimits := nil;
    FStoredBlockPolygons := nil;
  end;
begin
  if Source is TModflowDisvGrid then
  begin
    ClearVariables;
    SourceDisv := TModflowDisvGrid(Source);
    TwoDGrid := SourceDisv.TwoDGrid;
    Layers := SourceDisv.Layers;
    CrossSection := SourceDisv.CrossSection;
    Loaded;
  end
  else if Source is TCustomModelGrid then
  begin
    ClearVariables;
    LocalGrid := TCustomModelGrid(Source);
    (LocalGrid as TModflowGrid).UpdateCellElevations;
    if (LocalGrid.ColumnCount <= 0) or (LocalGrid.RowCount <= 0)
      or (LocalGrid.LayerCount <= 0) then
    begin
      Exit;
    end;

    AssignQuadRefinementArray;

    TwoDGrid.Assign(Source);
    Layers.Clear;
    ActiveDataArray := (LocalGrid.Model as TCustomModel).
      DataArrayManager.GetDataSetByName(rsActive);
    for LayerIndex := 0 to LocalGrid.LayerCount - 1 do
    begin
      ALayer := Layers.Add.Layer;
      CellIndex2D := 0;
      for RowIndex := 0 to LocalGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to LocalGrid.ColumnCount - 1 do
        begin
          Refinement := Sqr(Round(IntPower(2, TwoDGrid.FQuadRefinement[RowIndex,ColIndex])));
          for CellIndex := 0 to Refinement - 1 do
          begin
            ACell2D := TwoDGrid.Cells[CellIndex2D];
            ACell3D := ALayer.Add;
            ACell3D.FActiveElements := FActiveElements;
            ACell3D.Cell2D := ACell2D;
            ACell3D.Top := LocalGrid.CellElevation[ColIndex, RowIndex, LayerIndex];
            ACell3D.Bottom := LocalGrid.CellElevation[ColIndex, RowIndex, LayerIndex+1];
            ACell3D.Active := ActiveDataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
            Inc(CellIndex2D);
          end;
        end;
      end;
    end;
    SetDefaultCrossSectionLocation;
    SelectedLayer := LocalGrid.SelectedLayer;
//    SetNodeElevations;
    frmGoPhast.InvalidateScreenObjects;
    (Model as TCustomModel).InvalidateContours;
  end
  else
  begin
    inherited;
  end;
  frmGoPhast.InvalidateDataSets;
end;

procedure TModflowDisvGrid.AssignQuadRefinementArray;
var
  Grid: TModflowGrid;
  RowIndex: Integer;
  ColIndex: Integer;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  LocalModel: TCustomModel;
  CellIndex: Integer;
  ACell: TCellAssignment;
  InnerRowStart: Integer;
  InnerRowEnd: Integer;
  InnerRowIndex: Integer;
  InnerColStart: Integer;
  InnerColEnd: Integer;
  InnerColIndex: Integer;
  Changed: Boolean;
  QuadRefinement: TTwoDIntegerArray;
begin
  FAssigningQuadRefinement := True;
  try
    LocalModel := Model as TCustomModel;
    Grid := LocalModel.ModflowGrid;
    SetLength(QuadRefinement, Grid.RowCount, Grid.ColumnCount);
    for RowIndex := 0 to Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to Grid.ColumnCount - 1 do
      begin
        QuadRefinement[RowIndex,ColIndex] := 0;
      end;
    end;

    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Deleted or (AScreenObject.QuadtreeRefinementLevel <= 0) then
      begin
        Continue;
      end;
      if AScreenObject.ViewDirection <> vdTop then
      begin
        Continue;
      end;
      CellList := TCellAssignmentList.Create;
      try
        AScreenObject.GetCellsToAssign({Grid,} '0', nil, nil, CellList, alAll,
          LocalModel);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          if AScreenObject.QuadtreeRefinementLevel >
            QuadRefinement[ACell.Row, ACell.Column] then
          begin
            QuadRefinement[ACell.Row, ACell.Column] :=
              AScreenObject.QuadtreeRefinementLevel
          end;
        end;
      finally
        CellList.Free;
      end;
    end;

    repeat
      Changed := False;
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        InnerRowStart := Max(0, RowIndex-1);
        InnerRowEnd := Min(Grid.RowCount - 1, RowIndex+1);
        for ColIndex := 0 to Grid.ColumnCount - 1 do
        begin
          InnerColStart := Max(0, ColIndex-1);
          InnerColEnd := Min(Grid.ColumnCount - 1, ColIndex+1);
          for InnerRowIndex := InnerRowStart to InnerRowEnd do
          begin
            if QuadRefinement[InnerRowIndex,ColIndex]
              - QuadRefinement[RowIndex,ColIndex] > 1 then
            begin
              QuadRefinement[RowIndex,ColIndex] :=
                QuadRefinement[InnerRowIndex,ColIndex] -1;
              Changed := True;
            end;
          end;
          for InnerColIndex := InnerColStart to InnerColEnd do
          begin
            if QuadRefinement[RowIndex,InnerColIndex]
              - QuadRefinement[RowIndex,ColIndex] > 1 then
            begin
              QuadRefinement[RowIndex,ColIndex] :=
                QuadRefinement[RowIndex,InnerColIndex] -1;
              Changed := True;
            end;
          end;
        end;
      end;
    until not Changed;
    TwoDGrid.FQuadRefinement := QuadRefinement;
  finally
    FAssigningQuadRefinement := False;
  end;
end;

function TModflowDisvGrid.CellArea(CellIndex: Integer): double;
begin
  result := TwoDGrid.CellArea(CellIndex);
end;

function TModflowDisvGrid.CellThickness(const Layer, Row, Col: integer): double;
var
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
begin
  ALayer := Layers[Layer].Layer;
  ACell := ALayer[Col];
  result := ACell.Thickness;
end;

function TModflowDisvGrid.CellVolume(LayerIndex, CellIndex: Integer): double;
var
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
begin
  ALayer := Layers[LayerIndex].Layer;
  ACell := ALayer[CellIndex];
  result := ACell.Thickness * ACell.Cell2D.Area;
end;

procedure TModflowDisvGrid.CheckUpdateElevations;
begin
  if ElevationsNeedUpdating then
  begin
    ElevationsNeedUpdating := False;
    ThreeDGridObserver.UpToDate := True;
    UpdateElevations;
  end;
end;

procedure TModflowDisvGrid.Clear;
begin

  FTwoDGrid.Clear;
  FLayers.Clear;
  FSelectedLayer := 0;
  FNeedToRedraw3d := True;
  FNeedToRecordCrossSection := True;
  FActiveElements.Clear;
  FElevationsNeedUpdating := True;
  FNeedToRecordLayer := True;
  FFrontDataSet := nil;
  FStoredBlockPolygons := nil;
  FStoredBlockLimits := nil;
  FreeAndNil(FElementIntervalTree);
  FreeAndNil(FElementRangeTree);
  FreeAndNil(FRotatedElementCenters);
  FNeedToRecalculateFrontColors := True;
  FNeedToRecalculateTopColors := True;
  FNeedToRecordColoredCells := True;
  FStoredRotatedLocations.Clear;

{    FTwoDGrid: TModflowIrregularGrid2D;
    FLayers: TModflowIrregularLayers;
    FCanDraw: Boolean;
    FSelectedLayer: Integer;
    FCrossSection: TMeshCrossSectionLine;
    FNeedToRedraw3d: Boolean;
    FNeedToRecordCrossSection: Boolean;
    FActiveElements: TModflowDisVCellList;
    FThreeDGridObserver: TObserver;
    FUpdatingElevations: Boolean;
    FElevationsNeedUpdating: Boolean;
    FLoading: Boolean;
    FMeshUpdate: Integer;
    FNeedToRecordLayer: Boolean;
    FFrontDataSet: TDataArray;
    FMaxDist: Double;
    FMinDist: Double;
    FStoredBlockAngle: double;
    FStoredBlockPolygons: TCellElementPolygons2D;
    FStoredBlockLimits: TLimitsArray;
    FElementIntervalTree: TRbwIntervalTree;
    FElementRangeTree: TRbwRangeTree;
    FAssigningQuadRefinement: Boolean;
    FNeedToRecalculateFrontColors: Boolean;
    FNeedToRecalculateTopColors: Boolean;
    FNeedToRecordColoredCells: Boolean;
    FListsCreated: Boolean;
    FLayerGLIndex: GLuint;
    FCrossSectionGLIndex: GLuint;
    FColoredCellsOrElementGLIndex: GLuint;
    FCanDraw3D: Boolean;
    FColorDataSetObserver: TObserver;
    FRotatedElementCenters: TRbwQuadTree;
    FStoredRotatedLocations: TStoredLocations;
}
end;

procedure TModflowDisvGrid.ColorDataSetChange(Sender: TObject);
begin
  if not FColorDataSetObserver.UpToDate then
  begin
    FNeedToRecordColoredCells := True;
    frmGoPhast.frame3DView.glWidModelView.Invalidate;
  end;
end;

constructor TModflowDisvGrid.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FNumberFont := TFont.Create;
  FNumberFont.Name := 'Helvetica';
  FNumberFont.Size := 6;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
    FThreeDGridObserver := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
    FThreeDGridObserver := (Model as TCustomModel).ThreeDGridObserver;
  end;
  inherited;
  FColorDataSetObserver := TObserver.Create(nil);
  FColorDataSetObserver.OnUpToDateSet := ColorDataSetChange;


  FTwoDGrid := TModflowIrregularGrid2D.Create(Model, self);
  FLayers := TModflowIrregularLayers.Create(InvalidateModelEvent);
  FActiveElements := TModflowDisVCellList.Create;
  FStoredRotatedLocations := TStoredLocations.Create;
  FTwoDGrid.NumberFont := NumberFont;
  NumberFont.OnChange := OnChangeNumberFont;
//  FGhostNodes := TGhostNodes.Create;
  FCanDraw := True;
  FSelectedLayer := 0;
  FCrossSection := TMeshCrossSectionLine.Create(InvalidateModelEvent);
  FCrossSection.OnMoved := CrossSectionMoved;
  FNeedToRedraw3d := True;
  FNeedToRecordCrossSection := True;
  FCanDraw3D := True;
end;

procedure TModflowDisvGrid.OnChangeNumberFont(Sender: TObject);
begin
  FTwoDGrid.NumberFont := NumberFont;
end;

procedure TModflowDisvGrid.CrossSectionMoved(Sender: TObject);
begin
  FNeedToRedraw3d := True;
  FNeedToRecordCrossSection := True;
  if Model <> nil then
  begin
    (Model as TCustomModel).FrontContoursUpToDate := False;
  end;
end;

function TModflowDisvGrid.DefaultCrossSectionLocation: TSegment2D;
begin
  result := TwoDGrid.DefaultCrossSectionLocation;
end;

destructor TModflowDisvGrid.Destroy;
begin
  FTopOutline.Free;
  FFrontOutline.Free;
  FRotatedElementCenters.Free;
  FElementIntervalTree.Free;
  FElementRangeTree.Free;
//  FGhostNodes.Free;
  FCrossSection.Free;
  FLayers.Free;
  FTwoDGrid.Free;
  FActiveElements.Free;
  FColorDataSetObserver.Free;
  FStoredRotatedLocations.Free;
  FNumberFont.Free;
  inherited;
end;

procedure TModflowDisvGrid.Draw(const BitMap: TPersistent;
  const ViewDirection: TViewDirection);
begin
  CheckUpdateElevations;
  case ViewDirection of
    vdTop:
      begin
        TwoDGrid.Draw(BitMap);
        CrossSection.Draw(BitMap);
      end;
    vdFront:
      begin
        DrawFront(BitMap);
      end;
    vdSide:
      begin

      end;
    else
      Assert(False);
  end;
end;

procedure TModflowDisvGrid.RecordColoredGridEdges;
var
  LocalEdgeDisplay: TCustomModflowGridEdgeDisplay;
  MinValue, MaxValue, MinPositive: double;
  Value: double;
  Color: TColor;
  Red: GLubyte;
  Green: GLubyte;
  Blue: GLubyte;
  ColoringLimits: TColoringLimits;
  ActiveDataArray: TDataArray;
  LogMax, LogMin: double;
  Index: Integer;
  Edge: TCustomModflowGridEdgeFeature;
  procedure DrawFace;
  var
    X, Y, Z: single;
    ANodeNode: TModflowNode;
  begin
    glBegin(GL_POLYGON);
    try
      ANodeNode := Edge.SharedNodes[0];
      X := ANodeNode.Location.X;
      Y := ANodeNode.Location.Y;
      Z := ANodeNode.FTopElevations[Edge.Layer];
      glVertex3f(X, Y, Z);

      Z := ANodeNode.FBottomElevations[Edge.Layer];
      glVertex3f(X, Y, Z);

      ANodeNode := Edge.SharedNodes[1];
      X := ANodeNode.Location.X;
      Y := ANodeNode.Location.Y;
      Z := ANodeNode.FBottomElevations[Edge.Layer];
      glVertex3f(X, Y, Z);

      Z := ANodeNode.FTopElevations[Edge.Layer];
      glVertex3f(X, Y, Z);
    finally
      glEnd;
    end;
  end;
begin
  LocalEdgeDisplay := frmGoPhast.PhastModel.EdgeDisplay;
  if (LocalEdgeDisplay = nil) then
  begin
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    LocalEdgeDisplay.UpdateData;

    glNewList(FEdgesGLIndex, GL_COMPILE);
    try

      LocalEdgeDisplay.GetValueRangeToDisplay(MinValue, MaxValue, MinPositive);

      ColoringLimits := LocalEdgeDisplay.Limits[LocalEdgeDisplay.DataToPlot];

      LogMax := 0;
      LogMin := 0;
      if ColoringLimits.LogTransform then
      begin
        if MinValue <= 0 then
        begin
          MinValue := MinPositive;
        end;
        if MaxValue <= 0 then
        begin
          MaxValue := MinPositive;
        end;
        if MinValue > 0 then
        begin
          LogMin := Log10(MinValue);
          LogMax := Log10(MaxValue);
        end;
      end;

      ActiveDataArray := nil;
      if ColoringLimits.ActiveOnly then
      begin
        ActiveDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsActive);
        ActiveDataArray.Initialize;
      end;

      for Index := 0 to LocalEdgeDisplay.Count - 1 do
      begin
        Edge := LocalEdgeDisplay[Index];
        if not Edge.HasData then
        begin
          Continue;
        end;
        Value := Edge.RealValue[LocalEdgeDisplay.DataToPlot];
        if (Value < MinValue) or (Value > MaxValue) then
        begin
          Continue;
        end;

        if not LocalEdgeDisplay.UseEdge(ActiveDataArray, Edge) then
        begin
          Continue;
        end;

        if not ColoringLimits.ValueOk(Value) then
        begin
          Continue;
        end;

        if MinValue = MaxValue then
        begin
          Value := 0.5;
        end
        else
        begin
          if ColoringLimits.LogTransform then
          begin
            Assert(Value > 0);
            Value := Log10(Value);
            Value := (LogMax - Value)/(LogMax - LogMin);
          end
          else
          begin
            Value := (MaxValue - Value)/(MaxValue - MinValue);
          end;
        end;
        Color := GridFracToRainbow(Value);
        ExtractColorComponents(Color, Red, Green, Blue);
        glColor3ub(Red, Green, Blue);

        Edge.UpdateDisvSharedNodes;
        DrawFace;
      end
    finally
      glEndList
    end;

  finally
    Screen.Cursor := crDefault;
  end;

end;


procedure TModflowDisvGrid.Draw3D;
const
  NumberOfLists = 4;
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
      or (FNeedToRecordColoredCells and (ThreeDDataSet <> nil))
      or not (Model as TCustomModel).HfbDisplayer.UpToDate then
    begin
      if not FListsCreated then
      begin
        FLayerGLIndex := glGenLists(NumberOfLists);
        FCrossSectionGLIndex := Succ(FLayerGLIndex);
        FColoredCellsOrElementGLIndex := Succ(FCrossSectionGlIndex);
        FEdgesGLIndex := Succ(FColoredCellsOrElementGLIndex);
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
        RecordColoredCells
      end;

      if not (Model as TCustomModel).HfbDisplayer.UpToDate then
      begin
        RecordColoredGridEdges;
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

    if frmGoPhast.tb3DColors.Down
      and (frmGoPhast.PhastModel.EdgeDisplay <> nil) then
    begin
      glCallList(FEdgesGLIndex);
    end;


  finally
    glPopMatrix;
  end;

end;

procedure TModflowDisvGrid.GetCellsTop(Layer: Integer; out Cells: TPolygon2Darray;
  CheckAllLayers: Boolean);
var
  CellIndex: Integer;
  APolygon: TPolygon2D;
  ACell2D: TModflowIrregularCell2D;
  Active: TDataArray;
  ShouldUse: Boolean;
  LIndex: Integer;
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
  NodeIndex: Integer;
  ANode: TModflowNode;
  PolyIndex: Integer;
begin
  SetLength(Cells, TwoDGrid.Cells.Count);
  PolyIndex := 0;
  Active := (Model as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
  for CellIndex := 0 to TwoDGrid.Cells.Count - 1 do
  begin
    if CheckAllLayers then
    begin
      ShouldUse := False;
      for LIndex := 0 to Layers.Count - 1 do
      begin
        ALayer := Layers[LIndex].Layer;
        ACell := ALayer[CellIndex];

        if Active.BooleanData[LIndex, 0, ACell.Cell2D.ElementNumber]  then
        begin
          ShouldUse := True;
          break;
        end;
      end;
    end
    else
    begin
      ShouldUse := True;
      if Layer >= 0 then
      begin
        ALayer := Layers[Layer].Layer;
        ACell := ALayer[CellIndex];

        ShouldUse := Active.BooleanData[Layer, 0, ACell.Cell2D.ElementNumber];
      end;
    end;
    if not ShouldUse then
    begin
      Continue;
    end;
    ACell2D := TwoDGrid.Cells[CellIndex];
    SetLength(APolygon, ACell2D.ElementCorners.Count);
    for NodeIndex := 0 to ACell2D.ElementCorners.Count - 1 do
    begin
      ANode := ACell2D.ElementCorners[NodeIndex];
      APolygon[NodeIndex] := ANode.Location;
    end;

    Cells[PolyIndex] := APolygon;
    Inc(PolyIndex);
  end;

end;

procedure TModflowDisvGrid.GetCellsOnCrossSection(
  CellList: TMFIrregularCell2D_List);
var
  ALine: TLine2D;  
begin
  TwoDGrid.GetElementsOnSegment(CrossSection.Segment, CellList);
  ALine := EquateLine(CrossSection.Segment[1], CrossSection.Segment[2]);
  CellList.Sort(TComparer<TModflowIrregularCell2D>.Construct(
    function (const L, R: TModflowIrregularCell2D): integer
    var
      PointL: TPoint2D;
      PointR: TPoint2D;
    begin
      PointL := ClosestPointOnLineFromPoint(ALine, L.Center); 
      PointR := ClosestPointOnLineFromPoint(ALine, R.Center); 
      result := Sign(Distance(CrossSection.Segment[1], PointL)
        - Distance(CrossSection.Segment[1], PointR));
    end
    ));
end;

function TModflowDisvGrid.GetColumnCount: Integer;
begin
  result := TwoDGrid.ColumnCount;
end;

function TModflowDisvGrid.GetContainingLayer(CellIndex: integer;
  const AZPosition: real): integer;
var
  LayerIndex: Integer;
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
begin
  result := -1;
  for LayerIndex := 0 to Layers.Count - 1 do
  begin
    ALayer := Layers[LayerIndex].Layer;
    ACell := ALayer[CellIndex];
    if LayerIndex = 0 then
    begin
      if AZPosition > ACell.Top then
      begin
        Exit;
      end;

      if (ACell.Top >= AZPosition) and (ACell.Bottom <= AZPosition) then
      begin
        result := LayerIndex;
        Exit;
      end;
    end
    else
    begin
      if (ACell.Top > AZPosition) and (ACell.Bottom <= AZPosition) then
      begin
        result := LayerIndex;
        Exit;
      end;
    end;
  end;
  result := Layers.Count;
end;

function TModflowDisvGrid.GetCrossSection: TMeshCrossSectionLine;
begin
  result := FCrossSection;
end;

function TModflowDisvGrid.GetElementArrayMemberI(Layer,
  Col: Integer): IElement3D;
begin
  result := Layers[Layer].Layer[Col];
end;

function TModflowDisvGrid.GetElementCenters(Angle: Double): TRbwQuadTree;
var
  LayerIndex: Integer;
  ALayer: TModflowIrregularLayer;
  CellIndex: Integer;
  ACell: TModflowDisVCell;
  CenterPoint: TPoint3D;
  StoredPoint: TStoredNodeOrElement;
  Index: Integer;
  RandomIndex: Integer;
begin
  if (Angle <> FStoredBlockAngle) then
  begin
    FreeAndNil(FElementIntervalTree);
    FreeAndNil(FElementRangeTree);
    FreeAndNil(FRotatedElementCenters);
    FStoredBlockAngle := Angle;
  end;
  if FRotatedElementCenters = nil then
  begin
    FStoredRotatedLocations.Clear;
    FRotatedElementCenters := TRbwQuadTree.Create(nil);

    for LayerIndex := 0 to Layers.Count - 1 do
    begin
      ALayer := Layers[LayerIndex].Layer;
      for CellIndex := 0 to ALayer.Count - 1 do
      begin
        ACell := ALayer[CellIndex];
        CenterPoint := ACell.CenterLocation;
        StoredPoint := TStoredNodeOrElement.Create(
          CenterPoint, FStoredBlockAngle, CellIndex, LayerIndex, ACell);
        FStoredRotatedLocations.Add(StoredPoint);
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

function TModflowDisvGrid.GetElementI(Index: Integer): IElement3D;
begin
  result := FActiveElements[Index];
end;

procedure TModflowDisvGrid.GetElementsIntfOnCrossSection(
  ElementList: TIElement2DList);
var
  CellList: TMFIrregularCell2D_List;
  CellIndex: Integer;
begin
  CellList:= TMFIrregularCell2D_List.Create;
  try
    GetCellsOnCrossSection(CellList);
    for CellIndex := 0 to CellList.Count -1 do
    begin
      ElementList.Add(CellList[CellIndex]);
    end;
  finally
    CellList.Free;
  end;
end;

function TModflowDisvGrid.GetElevationsNeedUpdating: boolean;
begin
  result := FElevationsNeedUpdating and (TwoDGrid.CellCorners.Count > 0)
end;

function TModflowDisvGrid.GetFrontContourDataSet: TDataArray;
begin
  result := nil;
end;

function TModflowDisvGrid.GetFrontDataSet: TDataArray;
begin
  result := FFrontDataSet
end;

procedure TModflowDisvGrid.GetHorizontalNeighbors(const Layer, Row,
  Col: integer; CellList: TCellLocationList);
var
  NeighborList: TMFIrregularCell2D_List;
  NeighborIndex: Integer;
  ANeighbor: TModflowIrregularCell2D;
  ACell: TCellLocation;
begin
  NeighborList := TMFIrregularCell2D_List.Create;
  try
    ACell.Layer := Layer;
    ACell.Row := 0;
    TwoDGrid.Cells[Col].GetNeighbors(NeighborList);
    for NeighborIndex := 0 to NeighborList.Count - 1 do
    begin
      ANeighbor := NeighborList[NeighborIndex];
      ACell.Column := ANeighbor.ElementNumber;
      CellList.Add(ACell);
    end;
  finally
    NeighborList.Free;
  end;
end;

procedure TModflowDisvGrid.GetInactiveOutlineTop(out Outline: TPolygon2Darray);
var
  Edges: TRbwQuadTree;
  CellIndex: Integer;
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
  ACell2D: TModflowIrregularCell2D;
  NodeIndex: Integer;
  PriorNode: Integer;
  ANode: Integer;
  X: double;
  Y: double;
  ACellEdge: TCellEdge;
  OtherEdge: TCellEdge;
  SegmentEdges: TRbwQuadTree;
  GridLimits: TGridLimit;
  EdgeIndex: Integer;
  AnArray: TPointerArray;
  ItemIndex: Integer;
  ASegment: TSegment2D;
  AllEdges: array of TSegment2D;
  EIndex: Integer;
  APolygon: TPolygon2D;
  FirstLocation: TQuadPoint;
  SegPointer: TSegment2DPtr;
  OutlineIndex: Integer;
  PriorPoint: TPoint2D;
//  Data: Pointer;
  SegData: Pointer;
  PointIndex: Integer;
  DataIndex: Integer;
  Data: TPointerArray;
  IDomainDataArray: TDataArray;
  IsActive: boolean;
  LIndex: Integer;
  function ReverseSegment(ASegment: TSegment2D): TSegment2D;
  begin
    result[1] := ASegment[2];
    result[2] := ASegment[1];
  end;
begin
  Edges := TRbwQuadtree.Create(nil);
  try
    Edges.XMin := 0;
    Edges.YMin := 0;
    Edges.XMax := TwoDGrid.CellCorners.Count;
    Edges.YMax := TwoDGrid.CellCorners.Count;

    IDomainDataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(K_IDOMAIN);
//    ALayer := nil;
    for CellIndex := 0 to TwoDGrid.Cells.Count - 1 do
    begin
//      if CheckAllLayers then
      begin
        IsActive := False;
        for LIndex := 0 to Layers.Count - 1 do
        begin
          ALayer := Layers[LIndex].Layer;
          ACell := ALayer[CellIndex];

          if IDomainDataArray.IntegerData[LIndex, 0, ACell.Cell2D.ElementNumber] > 0  then
          begin
            IsActive := True;
            break;
          end;
        end;
        if IsActive then
        begin
          Continue;
        end;
      end;

      ACell2D := TwoDGrid.Cells[CellIndex];
      PriorNode := ACell2D.NodeNumbers.Last.Value;
      for NodeIndex := 0 to ACell2D.NodeCount - 1 do
      begin
        ANode := ACell2D.NodeNumbers[NodeIndex].Value;
        ACellEdge := TCellEdge.Create;
        ACellEdge.Node1 := PriorNode;
        ACellEdge.Node2 := ANode;
        X := ANode;
        Y := PriorNode;
        if Edges.Count > 0 then
        begin
          OtherEdge := Edges.NearestPointsFirstData(X,Y);
          if OtherEdge.IsSame(ACellEdge) then
          begin
            Edges.RemovePoint(X, Y, OtherEdge);
            OtherEdge.Free;
            ACellEdge.Free;
          end
          else
          begin
            X := PriorNode;
            Y := ANode;
            OtherEdge := Edges.NearestPointsFirstData(X,Y);
            if OtherEdge.IsSame(ACellEdge) then
            begin
              Edges.RemovePoint(X, Y, OtherEdge);
              OtherEdge.Free;
              ACellEdge.Free;
            end
            else
            begin
              Edges.AddPoint(X, Y, ACellEdge);
            end;
          end;
        end
        else
        begin
          Edges.AddPoint(X, Y, ACellEdge);
        end;
        PriorNode := ANode;
      end;
    end;

    SetLength(AllEdges, Edges.Count);
    EIndex := 0;
    SegmentEdges := TRbwQuadtree.Create(nil);
    try
      GridLimits := TwoDGrid.MeshLimits;
      SegmentEdges.XMin := GridLimits.MinX;
      SegmentEdges.XMax := GridLimits.MaxX;
      SegmentEdges.YMin := GridLimits.MinY;
      SegmentEdges.YMax := GridLimits.MaxY;

      for EdgeIndex := 0 to Edges.Count - 1 do
      begin
        AnArray := Edges.Points[EdgeIndex].Data;
        for ItemIndex := 0 to Length(AnArray) - 1 do
        begin
          ACellEdge := AnArray[ItemIndex];
          ASegment[1] := TwoDGrid.CellCorners[ACellEdge.Node1].Location;
          ASegment[2] := TwoDGrid.CellCorners[ACellEdge.Node2].Location;
          AllEdges[EIndex] := ASegment;
          SegmentEdges.AddPoint(ASegment[1].X, ASegment[1].Y, Addr(AllEdges[EIndex]));
          SegmentEdges.AddPoint(ASegment[2].X, ASegment[2].Y, Addr(AllEdges[EIndex]));
          Inc(EIndex);
          ACellEdge.Free;
        end;
      end;

      OutlineIndex := 0;
      while SegmentEdges.Count > 0 do
      begin
        Inc(OutlineIndex);
        SetLength(Outline, OutlineIndex);
        SetLength(APolygon, SegmentEdges.Count);
        FirstLocation := SegmentEdges.Points[0];
        Assert(Length(FirstLocation.Data) > 0);
        SegPointer := TSegment2DPtr(FirstLocation.Data[0]);
        SegmentEdges.RemovePoint(FirstLocation.X, FirstLocation.Y, FirstLocation.Data[0]);
        PriorPoint.X := FirstLocation.X;
        PriorPoint.Y := FirstLocation.Y;
        ASegment := SegPointer^;
        if PointsSame(ASegment[2], PriorPoint) then
        begin
          ASegment := ReverseSegment(ASegment);
        end;
        PointIndex := 0;
        repeat
          APolygon[PointIndex] := ASegment[2];
          X := ASegment[2].X;
          Y := ASegment[2].Y;
          if SegmentEdges.Count = 0 then
          begin
            break;
          end;
          SegmentEdges.FindClosestPointsData(X, Y, Data);
          SegData := nil;
//          SegmentEdges.FirstNearestPoint(X, Y, SegData);
          if (X = ASegment[2].X) and (Y = ASegment[2].Y) and (Data <> nil) then
          begin
            for DataIndex := 0 to Length(Data) -1 do
            begin
              SegPointer := TSegment2DPtr(Data[DataIndex]);
              if SegmentsSame(ASegment, SegPointer^) then
              begin
                SegmentEdges.RemovePoint(X, Y, Data[DataIndex]);
                break;
              end;
            end;
            if SegmentEdges.Count > 0 then
            begin
              SegmentEdges.FirstNearestPoint(X, Y, SegData);
              if (X = ASegment[2].X) and (Y = ASegment[2].Y) and (SegData <> nil) then
              begin
                SegPointer := TSegment2DPtr(SegData);
              end
              else
              begin
                break;
              end;
            end
            else
            begin
              break;
            end;
          end
          else
          begin
            break;
          end;
          Inc(PointIndex);
          PriorPoint := ASegment[2];
          SegmentEdges.RemovePoint(X, Y, SegData);
          ASegment := SegPointer^;
          if PointsSame(ASegment[2], PriorPoint) then
          begin
            ASegment := ReverseSegment(ASegment);
          end;
        until (PriorPoint.x = FirstLocation.X)
          and (PriorPoint.y = FirstLocation.y);
        PriorPoint.X := FirstLocation.X;
        PriorPoint.Y := FirstLocation.Y;
        APolygon[PointIndex] := PriorPoint;
        SetLength(APolygon, PointIndex+1);
        Outline[OutlineIndex-1] := APolygon;
      end;

    finally
      SegmentEdges.Free;
    end;

  finally
    Edges.Free;
  end;
end;

function TModflowDisvGrid.GetIntervalTree(EvalAt: TEvaluatedAt;
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
//  result := nil;
  Assert(EvalAt = eaBlocks);
  if (Angle <> FStoredBlockAngle) then
  begin
    FreeAndNil(FElementIntervalTree);
    FreeAndNil(FElementRangeTree);
    FreeAndNil(FRotatedElementCenters);
    FStoredBlockAngle := Angle;
  end;
  if FElementIntervalTree = nil then
  begin
    Polygons := FrontPolygons(Angle, EvalAt, Limits);
    CreateIntevalTree(FElementIntervalTree);
  end;
  result := FElementIntervalTree;
end;

function TModflowDisvGrid.GetLayerCount: Integer;
begin
  result := Layers.Count;
end;

function TModflowDisvGrid.GetMesh2DI: IMesh2D;
begin
  result := TwoDGrid;
end;

procedure TModflowDisvGrid.GetModelOutlineTop(out Outline: TPolygon2Darray);
begin
  GetActiveOutlineTop(-1, Outline, True);
end;

function TModflowDisvGrid.GetNeedToRecalculateFrontColors: Boolean;
begin
  result := FNeedToRecalculateFrontColors
end;

function TModflowDisvGrid.GetNeedToRecalculateTopColors: Boolean;
begin
  result := FNeedToRecalculateTopColors;
end;

function TModflowDisvGrid.GetNodeArrayMemberI(Layer, Col: Integer): INode3D;
begin
  result := nil;
end;

function TModflowDisvGrid.GetNodeI(Index: Integer): INode3D;
begin
  result := nil;
end;

procedure TModflowDisvGrid.GetNodesIntfOnCrossSection(NodeList: TINode2DList);
begin
  // do nothing
end;

function TModflowDisvGrid.GetRangeTree(EvalAt: TEvaluatedAt;
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
//  result := nil;
  Assert(EvalAt = eaBlocks);
  if (Angle <> FStoredBlockAngle) then
  begin
    FreeAndNil(FElementRangeTree);
    FreeAndNil(FElementIntervalTree);
    FreeAndNil(FRotatedElementCenters);
    FStoredBlockAngle := Angle;
  end;
  if FElementRangeTree = nil then
  begin
    Polygons := FrontPolygons(Angle, EvalAt, Limits);
    CreateRangeTree(FElementRangeTree);
  end;
  result := FElementRangeTree;
end;

function TModflowDisvGrid.GetRowCount: Integer;
begin
  result := 1;
end;

procedure TModflowDisvGrid.DrawFront(const BitMap: TPersistent);
var
  CellList: TMFIrregularCell2D_List;
  PointList: TList<TPoint2D>;
  CellIndex: Integer;
  EdgeIndex: Integer;
  AnEdge: TSegment2D;
  CrossSectionSegment: TSegment2D;
  ACell2D: TModflowIrregularCell2D;
  Epsilon: Double;
  SegmentList: TList<TSegment2D>;
  PriorCell: TModflowIrregularCell2D;
  NextCell: TModflowIrregularCell2D;
  ASegment: TSegment2D;
  LayerIndex: Integer;
  CellOutline: TPolygon2D;
  PolygonSize: Integer;
  PointIndex: Integer;
  ACell3D: TModflowDisVCell;
  ALayer: TModflowIrregularLayer;
  PriorCell3D: TModflowDisVCell;
  NextCell3D: TModflowDisVCell;
  NextSegment: TSegment2D;
  PriorSegment: TSegment2D;
  SegLength: double;
  ZoomBox: TQRbwZoomBox2;
  Points: GoPhastTypes.TPointArray;
  ClosestPoint: TPoint2D;
  OffSet: double;
  ADistance: double;
  SegmentLine: TLine2D;
  FoundFirst: Boolean;
  IntersectedCell: Boolean;
  NodeIndex: Integer;
  ANode: TPoint2D;
  OriginOffset: double;
  StringValues: TStringList;
  MinMax: TMinMax;
  ShowColor: Boolean;
  Fraction: Double;
  AColor: TColor;
  Dummy: TPolygon32;
  SegmentAngle: double;
  OriginAngle: Double;
  ActiveDataArray: TDataArray;
  MinMaxInitialized: Boolean;
//  ANode: INode;
  function Interpolate(Length1, Length2, Value1, Value2: double): double;
  var
    Fraction: Double;
    DeltaZ: Double;
  begin
    Fraction := Length1/(Length1 + Length2);
    DeltaZ := Value2 - Value1;
    result := Value1 + Fraction*DeltaZ;
  end;
  procedure ComputeCrossSectionDistance(ALine: TLine2D; ALocation: TPoint2D);
  var
    ClosestPoint: TPoint2D;
    PointAngle: Double;
  begin
    ClosestPoint := ClosestPointOnLineFromPoint(
      ALine, ALocation);
    ADistance := -Distance(ALocation, ClosestPoint);
    if ADistance <> 0 then
    begin
      PointAngle := ArcTan2(ALine[1].Y - ClosestPoint.y, ALine[1].x - ClosestPoint.x);
      if Abs(SegmentAngle - PointAngle) > 0.001 then
      {if FastGEO.Orientation(ALocation,
        CrossSection.StartPoint, CrossSection.EndPoint) =
        LeftHandSide then}
      begin
        ADistance := -ADistance;
      end;
    end;
//    ADistance := ADistance + OriginOffset;
    ADistance := ADistance + Offset;
  end;
begin
  if TwoDGrid.ElementCount = 0 then
  begin
    Exit;
  end;

  CrossSectionSegment := CrossSection.Segment;
  if (CrossSectionSegment[1].x = CrossSectionSegment[2].x) 
    and (CrossSectionSegment[1].y = CrossSectionSegment[2].y) then
  begin
    Exit;
  end;

  TwoDGrid.UpdateTimeDataSet;
  Dummy := nil;

  FMaxDist := 0;
  FMinDist := 0;

  ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
  PolygonSize := 6;
  SetLength(CellOutline, PolygonSize);
  SetLength(Points, PolygonSize+1);
  CellList := TMFIrregularCell2D_List.Create;
  PointList := TList<TPoint2D>.Create;
  SegmentList := TList<TSegment2D>.Create;
  StringValues := TStringList.Create;
  try
    if (ThreeDDataSet <> nil) then
    begin
      GetMinMax(MinMax, ThreeDDataSet, StringValues, MinMaxInitialized);
      ApplyLimittoMinMax(ThreeDDataSet, MinMax, ThreeDDataSet.Limits);
    end;


    SegmentLine := EquateLine(CrossSectionSegment[1],CrossSectionSegment[2]);
    SegmentAngle := ArcTan2(SegmentLine[1].Y - SegmentLine[2].y, SegmentLine[1].x - SegmentLine[2].x);

    ClosestPoint := ClosestPointOnLineFromPoint(
      SegmentLine, EquatePoint(0.0, 0.0));
    OriginOffset := Distance(ClosestPoint, CrossSectionSegment[1]);
    if OriginOffset <> 0 then
    begin
      OriginAngle := ArcTan2(SegmentLine[1].Y - ClosestPoint.y, SegmentLine[1].x - ClosestPoint.x);
      if Abs(SegmentAngle - OriginAngle) > 0.001 then
      {if FastGEO.Orientation(EquatePoint(0.0, 0.0),
        CrossSection.StartPoint, CrossSection.EndPoint) =
        LeftHandSide then}
      begin
        OriginOffset := -OriginOffset;
      end;
    end;
    OffSet := Distance(CrossSectionSegment[1], ClosestPoint);
    if OffSet <> 0 then
    begin
      OriginAngle := ArcTan2(SegmentLine[1].Y - ClosestPoint.y, SegmentLine[1].x - ClosestPoint.x);
      if Abs(SegmentAngle - OriginAngle) > 0.001 then
      {if FastGEO.Orientation(EquatePoint(0.0, 0.0),
        CrossSection.StartPoint, CrossSection.EndPoint) =
        LeftHandSide then}
      begin
        OffSet := -OffSet;
      end;
    end;

    Epsilon := Distance(CrossSectionSegment[1], CrossSectionSegment[2])/10E6;
    GetCellsOnCrossSection(CellList);
    FoundFirst := False;
    ActiveDataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
    for CellIndex := CellList.count -1 downto 0 do
    begin
      ACell2D := CellList[CellIndex];
      PointList.Clear;
      IntersectedCell := False;
      for EdgeIndex := 0 to ACell2D.NodeCount -1 do
      begin
        AnEdge := ACell2D.Edges[EdgeIndex];
        if Intersect(AnEdge, CrossSectionSegment) then
        begin
          if Collinear(CrossSectionSegment[1], CrossSectionSegment[2],
            AnEdge[1])
            and Collinear(CrossSectionSegment[1], CrossSectionSegment[2],
            AnEdge[2]) then
          begin
            PointList.Add(AnEdge[1]);
            PointList.Add(AnEdge[2]);
          end
          else
          begin
            PointList.Add(IntersectionPoint(AnEdge, CrossSectionSegment));
          end;
          IntersectedCell := True;
        end;
      end;
      if IntersectedCell then
      begin
        FoundFirst := False;
        for NodeIndex := 0 to ACell2D.NodeCount -1 - 1 do
        begin
          ANode := ACell2D.Nodes[NodeIndex].Location;
          ComputeCrossSectionDistance(SegmentLine, ANode);
          if FoundFirst then
          begin
            FMaxDist := Max(ADistance, FMaxDist);
            FMinDist := Min(ADistance, FMinDist);
          end
          else
          begin
            FMaxDist := ADistance;
            FMinDist := ADistance;
            FoundFirst := True;
          end;
        end;
      end;
      PointList.Sort(TComparer<TPoint2D>.Construct(
        function (const L, R: TPoint2D): integer
        begin
          result := Sign(Distance(CrossSectionSegment[1], L)
            - Distance(CrossSectionSegment[1], R));
        end
        ));

      for PointIndex := PointList.Count -1 downto 1 do
      begin
        If Distance(PointList[PointIndex], PointList[PointIndex-1]) < Epsilon then
        begin
          PointList.Delete(PointIndex);
        end;
      end;
      While PointList.Count > 2 do
      begin
        PointList.Delete(1)
      end;

      If PointList.Count < 2 then
      begin
        CellList.Delete(CellIndex);
      end
      else
      begin
        SegmentList.Add(EquateSegment(PointList[0], PointList[1]));
      end;
    end;
    SegmentList.Reverse;
    PriorCell := nil;
    for CellIndex := 0 to CellList.count -1 do
    begin
      ACell2D := CellList[CellIndex];
      ASegment := SegmentList[CellIndex];
      SegLength := SegmentLength(ASegment);
      if CellIndex < CellList.count -1 then
      begin
        NextCell := CellList[CellIndex+1];
        NextSegment := SegmentList[CellIndex+1];
        if Distance(ASegment[2], NextSegment[1]) > Epsilon then
        begin
          NextCell := nil;
        end;
      end
      else
      begin
        NextCell := nil;
      end;

      if PriorCell <> nil then
      begin
        PriorSegment := SegmentList[CellIndex-1];
        if Distance(PriorSegment[2], ASegment[1]) > Epsilon then
        begin
          PriorCell := nil;
        end;
      end;

      CellOutline[0].X := Distance(CrossSectionSegment[1], ASegment[1]) - OriginOffset;
      CellOutline[2].X := Distance(CrossSectionSegment[1], ASegment[2]) - OriginOffset;
      CellOutline[1].X := (CellOutline[0].X + CellOutline[2].X)/2;
      CellOutline[3].X := CellOutline[2].X;
      CellOutline[4].X := CellOutline[1].X;
      CellOutline[5].X := CellOutline[0].X;

      for LayerIndex := 0 to Layers.Count -1 do
      begin
        ALayer := Layers[LayerIndex].Layer;
        ACell3D := ALayer[ACell2D.Index];
        CellOutline[1].Y := ACell3D.Top;
        CellOutline[4].Y := ACell3D.Bottom;
        if PriorCell = nil then
        begin
          CellOutline[0].Y := ACell3D.Top;
          CellOutline[5].Y := ACell3D.Bottom;
        end
        else
        begin
          PriorCell3D := ALayer[PriorCell.Index];
          PriorSegment := SegmentList[CellIndex-1];
          CellOutline[0].Y := Interpolate(SegmentLength(PriorSegment),
            SegLength, PriorCell3D.Top, ACell3D.Top);
          CellOutline[5].Y := Interpolate(SegmentLength(PriorSegment),
            SegLength, PriorCell3D.Bottom, ACell3D.Bottom);
        end;
        if NextCell = nil then
        begin
          CellOutline[2].Y := ACell3D.Top;
          CellOutline[3].Y := ACell3D.Bottom;
        end
        else
        begin
          NextCell3D := ALayer[NextCell.Index];
          NextSegment := SegmentList[CellIndex+1];
          CellOutline[2].Y := Interpolate(SegLength,
            SegmentLength(NextSegment), ACell3D.Top, NextCell3D.Top);
          CellOutline[3].Y := Interpolate(SegLength,
            SegmentLength(NextSegment), ACell3D.Bottom, NextCell3D.Bottom);
        end;

        for PointIndex := 0 to PolygonSize -1 do
        begin
          Points[PointIndex].X := ZoomBox.XCoord(CellOutline[PointIndex].X);
          Points[PointIndex].Y := ZoomBox.YCoord(CellOutline[PointIndex].Y);
        end;
        Points[PolygonSize] := Points[0];

        if ThreeDDataSet <> nil then
        begin
          GetDataSetMeshValue(ThreeDDataSet, LayerIndex, ACell2D.ElementNumber,
            StringValues, ShowColor, Fraction, MinMax);
          if ShowColor then
          begin
            if ThreeDDataSet.Limits.ActiveOnly and not
              ActiveDataArray.BooleanData[LayerIndex,0,ACell2D.ElementNumber] then
            begin
              AColor := InactiveGridColor;
            end
            else
            begin
              AColor := frmGoPhast.PhastModel.GridColorParameters.
                FracToColor(Fraction);
            end;
            DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
              0, Points, Dummy, False, True);
          end;
        end;

        case GridLineDrawingChoice of
          gldcAll:
            begin
              DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                Points, True);
            end;
          gldcExterior:
            begin
              if LayerIndex = 0 then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 0 , 3);
              end;
              if LayerIndex = Layers.Count -1 then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 3 , 3);
              end;
              if NextCell = nil then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 2 , 2);
              end;
              if PriorCell = nil then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True, False, 5 , 2);
              end;
            end;
          gldcActive:
            begin
              if ActiveDataArray.BooleanData[LayerIndex, 0, ACell2D.ElementNumber] then
              begin
                DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                  Points, True);
              end;
            end;
          gldcActiveEdge:
            begin
              if ActiveDataArray.BooleanData[LayerIndex, 0, ACell2D.ElementNumber] then
              begin
                if (LayerIndex = 0) or
                  not ActiveDataArray.BooleanData[LayerIndex-1, 0, ACell2D.ElementNumber] then
                begin
                  DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                    Points, True, False, 0 , 3);
                end;
                if (LayerIndex = Layers.Count -1) or
                  not ActiveDataArray.BooleanData[LayerIndex+1, 0, ACell2D.ElementNumber] then
                begin
                  DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                    Points, True, False, 3 , 3);
                end;
                if (NextCell = nil) or
                  not ActiveDataArray.BooleanData[LayerIndex, 0, NextCell.ElementNumber] then
                begin
                  DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                    Points, True, False, 2 , 2);
                end;
                if (PriorCell = nil) or
                  not ActiveDataArray.BooleanData[LayerIndex, 0, PriorCell.ElementNumber] then
                begin
                  DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
                    Points, True, False, 5 , 2);
                end;
              end;
            end;
        end;

      end;
      PriorCell := ACell2D;
    end;

  finally
    CellList.Free;
    PointList.Free;
    SegmentList.Free;
    StringValues.Free;
  end;

  DrawFrontContours(ZoomBox, BitMap);
end;

procedure TModflowDisvGrid.DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  Contourer: TMultipleContourCreator;
  LocalModel: TCustomModel;
begin

  if (LayerCount > 1) and (Mesh2DI.NodeCount > 0) and (ThreeDContourDataSet <> nil) then
  begin
      Contourer := TMultipleContourCreator.Create;
      try
        PlotList := FFrontContourPlotList;
        Contourer.DataSet := ThreeDContourDataSet;
        LocalModel := Model as TCustomModel;
        Contourer.ActiveDataSet :=
          LocalModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
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

procedure TModflowDisvGrid.SetNeedToRecalculateFrontColors(
  const Value: Boolean);
begin
  if Value then
  begin
    FNeedToRecalculateFrontColors := Value;
    NeedToRecalculateTopColors := Value;
  end;
end;

procedure TModflowDisvGrid.SetNeedToRecalculateTopColors(const Value: Boolean);
begin
  if Value then
  begin
    FNeedToRecalculateTopColors := Value;
  end;
end;

procedure TModflowDisvGrid.SetNodeElevations;
var
  NodeIndex: Integer;
  ANode: TModflowNode;
  LayerIndex: Integer;
  ALayer: TModflowIrregularLayer;
  NodeCells: TMFIrregularCell2D_List;
  UpperWeightedSum: Double;
  LowerWeightedSum: Double;
  WeightSum: Double;
  NeighborIndex: Integer;
  ANeighbor2D: TModflowIrregularCell2D;
  NeighborDistance: double;
  ANeighbor3D: TModflowDisVCell;
  Weight: double;
  NodeTop: double;
  NodeBottom: double;
begin
  for LayerIndex := 0 to Layers.Count - 1 do
  begin
    ALayer := Layers[LayerIndex].Layer;
    ALayer.Count := TwoDGrid.ElementCount;
  end;
  for NodeIndex := 0 to TwoDGrid.GetActiveNodeCount -1 do
  begin
    ANode := TwoDGrid.CellCorners[NodeIndex];
    SetLength(ANode.FTopElevations, Layers.Count);
    SetLength(ANode.FBottomElevations, Layers.Count);
    SetLength(ANode.FActive, Layers.Count);

    NodeCells := ANode.FCells;
    for LayerIndex := 0 to Layers.Count - 1 do
    begin
      ALayer := Layers[LayerIndex].Layer;
      UpperWeightedSum := 0;
      LowerWeightedSum := 0;
      WeightSum := 0;
      for NeighborIndex := 0 to NodeCells.Count - 1 do
      begin
        ANeighbor2D := NodeCells[NeighborIndex];
        NeighborDistance := Distance(ANode.Location, ANeighbor2D.Location);
        ANeighbor3D := ALayer[ANeighbor2D.ElementNumber];
//        if ANeighbor3D.Active then
        begin
          if NeighborDistance = 0 then
          begin
            UpperWeightedSum := ANeighbor3D.Top;
            LowerWeightedSum := ANeighbor3D.Bottom;
            WeightSum := 1;
            break;
          end
          else
          begin
            Weight := 1/NeighborDistance;
            UpperWeightedSum := UpperWeightedSum + Weight*ANeighbor3D.Top;
            LowerWeightedSum := LowerWeightedSum + Weight*ANeighbor3D.Bottom;
            WeightSum := WeightSum + Weight;
          end;
        end;
      end;
      if WeightSum = 0 then
      begin
        NodeTop := 0;
        NodeBottom := 0;
        ANode.FActive[LayerIndex] := False;
//        Assert(False);
      end
      else
      begin
        NodeTop := UpperWeightedSum/WeightSum;
        NodeBottom := LowerWeightedSum/WeightSum;
        ANode.FActive[LayerIndex] := True;
      end;
      ANode.FTopElevations[LayerIndex] := NodeTop;
      ANode.FBottomElevations[LayerIndex] := NodeBottom;
    end;
  end;
end;

procedure TModflowDisvGrid.SetNumberFont(const Value: TFont);
begin
  FNumberFont.Assign(Value);
  TwoDGrid.NumberFont := Value;
end;

function TModflowDisvGrid.FrontMeshBox(ObjectAngle: double): TPolygon2D;
var
  Limits: TGridLimit;
begin
  result := nil;
  if (TwoDGrid.CellCorners.Count = 0) then
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

function TModflowDisvGrid.FrontOutline: TOutline;
begin
  FFrontOutline.Free;
  result := TIrregularGridOutline.Create(self, vdFront, 0);
  FFrontOutline := result;
end;

function TModflowDisvGrid.FrontPolygons(Angle: Double;
  EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray): TCellElementPolygons2D;
var
//  QuadPairList: TQuadPair3DList;
  LayerIndex: Integer;
  ElementIndex: Integer;
  ALayer: TModflowIrregularLayer;
  Cell3D: TModflowDisVCell;
  PerpendicularLimit: TLimits;
  PointList: TList<TPoint2D>;
  function CellToPolygon(Cell3D: TModflowDisVCell; Angle: Double;
    out PerpendicularLimit: TLimits): TPolygon2D;
  var
    Cell2D: TModflowIrregularCell2D;
    NodeIndex: Integer;
    ANode: TModflowNode;
    NodeTop: double;
    NodeBottom: double;
    PointDistance: Double;
    PointAngle: Double;
    TopPoint: TPoint2D;
    BottomPoint: TPoint2D;
    FoundFirst: Boolean;
    PointIndex: Integer;
    APolygon: TPolygon2D;
    PerpendicularDistance: Double;
  begin
    Cell2D := TwoDGrid.Cells[ElementIndex];
    PointList.Clear;
    FoundFirst := False;
    for NodeIndex := 0 to Cell2D.NodeCount - 1 do
    begin
      // Find top and bottom elevation for this node
      ANode := Cell2D.ElementCorners[NodeIndex];
      NodeTop := ANode.FTopElevations[LayerIndex];
      NodeBottom := ANode.FBottomElevations[LayerIndex];
      TopPoint.Y := NodeTop;
      BottomPoint.Y := NodeBottom;

      PointDistance := Sqrt(Sqr(ANode.x) + Sqr(ANode.y));
      PointAngle := ArcTan2(ANode.y, ANode.x);
      PerpendicularDistance := Sin(PointAngle-Angle)*PointDistance;
      TopPoint.X := Cos(PointAngle-Angle)*PointDistance;
      BottomPoint.X := TopPoint.X;
      PointList.Add(TopPoint);
      PointList.Add(BottomPoint);

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
      end
      else
      begin
        FoundFirst := True;
        PerpendicularLimit.UpperLimit := PerpendicularDistance;
        PerpendicularLimit.LowerLimit := PerpendicularDistance;
      end;

    end;
    SetLength(APolygon,PointList.count);
    for PointIndex := 0 to PointList.Count - 1 do
    begin
      APolygon[PointIndex] := PointList[PointIndex];
    end;
    ConvexHull(APolygon,result);
  end;
begin
  result := nil;
  Limits := nil;
  CheckUpdateElevations;
  Assert(EvaluatedAt = eaBlocks);
  if (Abs(Angle - FStoredBlockAngle) < 1e-10)
    and (FStoredBlockPolygons <> nil) then
  begin
    result := FStoredBlockPolygons;
    Limits := FStoredBlockLimits;
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  PointList := TList<TPoint2D>.Create;
  try
    FreeAndNil(FElementIntervalTree);
    FreeAndNil(FElementRangeTree);
    FreeAndNil(FRotatedElementCenters);
    if Layers.Count = 0 then
    begin
      Exit;
    end;
//    SetNodeElevations;
    ALayer := Layers[0].Layer;
    SetLength(result, Layers.Count, ALayer.Count);
    SetLength(Limits, Layers.Count, ALayer.Count);
    for LayerIndex := 0 to Layers.Count - 1 do
    begin
      ALayer := Layers[LayerIndex].Layer;
      for ElementIndex := 0 to ALayer.Count - 1 do
      begin
        Cell3D := ALayer[ElementIndex];
        if Cell3D.Active then
        begin
          Result[LayerIndex,ElementIndex] :=
            CellToPolygon(Cell3D, Angle, PerpendicularLimit);
          Limits[LayerIndex,ElementIndex] := PerpendicularLimit;
        end
        else
        begin
          SetLength(Result[LayerIndex,ElementIndex], 0);
        end;
      end;
    end
  finally
    FStoredBlockAngle := Angle;
    FStoredBlockPolygons := result;
    FStoredBlockLimits := Limits;
    PointList.Free;
    Screen.Cursor := crDefault;
  end;

end;

function TModflowDisvGrid.GetActiveElement(Index: integer): IElement;
begin
  result := FActiveElements[Index];
end;

function TModflowDisvGrid.GetActiveElementCount: integer;
begin
  result := FActiveElements.Count;
end;

function TModflowDisvGrid.GetActiveNode(Index: integer): INode;
begin
  Assert(False);
end;

function TModflowDisvGrid.GetActiveNodeCount: integer;
begin
  result := 0;
end;

procedure TModflowDisvGrid.GetActiveOutlineTop(Layer: Integer;
  out Outline: TPolygon2Darray; CheckAllLayers: Boolean = False);
var
  Edges: TRbwQuadTree;
  CellIndex: Integer;
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
  ACell2D: TModflowIrregularCell2D;
  NodeIndex: Integer;
  PriorNode: Integer;
  ANode: Integer;
  X: double;
  Y: double;
  ACellEdge: TCellEdge;
  OtherEdge: TCellEdge;
  SegmentEdges: TRbwQuadTree;
  GridLimits: TGridLimit;
  EdgeIndex: Integer;
  AnArray: TPointerArray;
  ItemIndex: Integer;
  ASegment: TSegment2D;
  AllEdges: array of TSegment2D;
  EIndex: Integer;
  APolygon: TPolygon2D;
  FirstLocation: TQuadPoint;
  SegPointer: TSegment2DPtr;
  OutlineIndex: Integer;
  PriorPoint: TPoint2D;
//  Data: Pointer;
  SegData: Pointer;
  PointIndex: Integer;
  DataIndex: Integer;
  Data: TPointerArray;
  IDomainDataArray: TDataArray;
  IsActive: boolean;
  LIndex: Integer;
  function ReverseSegment(ASegment: TSegment2D): TSegment2D;
  begin
    result[1] := ASegment[2];
    result[2] := ASegment[1];
  end;
begin
  Edges := TRbwQuadtree.Create(nil);
  try
    Edges.XMin := 0;
    Edges.YMin := 0;
    Edges.XMax := TwoDGrid.CellCorners.Count;
    Edges.YMax := TwoDGrid.CellCorners.Count;

    IDomainDataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(K_IDOMAIN);
    if Layer >= 0 then
    begin
      ALayer := Layers[Layer].Layer;
    end
    else
    begin
      ALayer := nil
    end;
    for CellIndex := 0 to TwoDGrid.Cells.Count - 1 do
    begin
      if CheckAllLayers then
      begin
        IsActive := False;
        for LIndex := 0 to Layers.Count - 1 do
        begin
          ALayer := Layers[LIndex].Layer;
          ACell := ALayer[CellIndex];

          if IDomainDataArray.IntegerData[LIndex, 0, ACell.Cell2D.ElementNumber] > 0  then
          begin
            IsActive := True;
            break;
          end;
        end;
        if not IsActive then
        begin
          Continue;
        end;
      end
      else
      begin
        if ALayer <> nil then
        begin
          ACell := ALayer[CellIndex];

          if IDomainDataArray.IntegerData[Layer, 0, ACell.Cell2D.ElementNumber] <= 0  then
          begin
            Continue;
          end;
        end;
      end;

      ACell2D := TwoDGrid.Cells[CellIndex];
      PriorNode := ACell2D.NodeNumbers.Last.Value;
      for NodeIndex := 0 to ACell2D.NodeCount - 1 do
      begin
        ANode := ACell2D.NodeNumbers[NodeIndex].Value;
        ACellEdge := TCellEdge.Create;
        ACellEdge.Node1 := PriorNode;
        ACellEdge.Node2 := ANode;
        X := ANode;
        Y := PriorNode;
        if Edges.Count > 0 then
        begin
          OtherEdge := Edges.NearestPointsFirstData(X,Y);
          if OtherEdge.IsSame(ACellEdge) then
          begin
            Edges.RemovePoint(X, Y, OtherEdge);
            OtherEdge.Free;
            ACellEdge.Free;
          end
          else
          begin
            X := PriorNode;
            Y := ANode;
            OtherEdge := Edges.NearestPointsFirstData(X,Y);
            if OtherEdge.IsSame(ACellEdge) then
            begin
              Edges.RemovePoint(X, Y, OtherEdge);
              OtherEdge.Free;
              ACellEdge.Free;
            end
            else
            begin
              Edges.AddPoint(X, Y, ACellEdge);
            end;
          end;
        end
        else
        begin
          Edges.AddPoint(X, Y, ACellEdge);
        end;
        PriorNode := ANode;
      end;
    end;

    SetLength(AllEdges, Edges.Count);
    EIndex := 0;
    SegmentEdges := TRbwQuadtree.Create(nil);
    try
      GridLimits := TwoDGrid.MeshLimits;
      SegmentEdges.XMin := GridLimits.MinX;
      SegmentEdges.XMax := GridLimits.MaxX;
      SegmentEdges.YMin := GridLimits.MinY;
      SegmentEdges.YMax := GridLimits.MaxY;

      for EdgeIndex := 0 to Edges.Count - 1 do
      begin
        AnArray := Edges.Points[EdgeIndex].Data;
        for ItemIndex := 0 to Length(AnArray) - 1 do
        begin
          ACellEdge := AnArray[ItemIndex];
          ASegment[1] := TwoDGrid.CellCorners[ACellEdge.Node1].Location;
          ASegment[2] := TwoDGrid.CellCorners[ACellEdge.Node2].Location;
          Assert(Distance(ASegment[1], ASegment[2]) >0);
          AllEdges[EIndex] := ASegment;
          SegmentEdges.AddPoint(ASegment[1].X, ASegment[1].Y, Addr(AllEdges[EIndex]));
          SegmentEdges.AddPoint(ASegment[2].X, ASegment[2].Y, Addr(AllEdges[EIndex]));
          Inc(EIndex);
          ACellEdge.Free;
        end;
      end;

      OutlineIndex := 0;
      while SegmentEdges.Count > 0 do
      begin
        Inc(OutlineIndex);
        SetLength(Outline, OutlineIndex);
        SetLength(APolygon, SegmentEdges.Count+1);
        FirstLocation := SegmentEdges.Points[0];
        Assert(Length(FirstLocation.Data) > 0);
        SegPointer := TSegment2DPtr(FirstLocation.Data[0]);
        SegmentEdges.RemovePoint(FirstLocation.X, FirstLocation.Y, FirstLocation.Data[0]);
        PriorPoint.X := FirstLocation.X;
        PriorPoint.Y := FirstLocation.Y;
        ASegment := SegPointer^;
        if PointsSame(ASegment[2], PriorPoint) then
        begin
          ASegment := ReverseSegment(ASegment);
        end;
        PointIndex := 0;
        repeat
          if PointIndex >= Length(APolygon) then
          begin
            SetLength(APolygon, PointIndex+1);
          end;
          APolygon[PointIndex] := ASegment[2];
          X := ASegment[2].X;
          Y := ASegment[2].Y;
          if SegmentEdges.Count = 0 then
          begin 
            break;
          end;
          SegmentEdges.FindClosestPointsData(X, Y, Data);
          SegData := nil;
//          SegmentEdges.FirstNearestPoint(X, Y, SegData);
          if (X = ASegment[2].X) and (Y = ASegment[2].Y) and (Data <> nil) then
          begin
            for DataIndex := 0 to Length(Data) -1 do
            begin
              SegPointer := TSegment2DPtr(Data[DataIndex]);
              if SegmentsSame(ASegment, SegPointer^) then
              begin
                SegmentEdges.RemovePoint(X, Y, Data[DataIndex]);
                break;
              end;
            end;
            if SegmentEdges.Count > 0 then
            begin
              SegmentEdges.FirstNearestPoint(X, Y, SegData);
              if (X = ASegment[2].X) and (Y = ASegment[2].Y) and (SegData <> nil) then
              begin
                SegPointer := TSegment2DPtr(SegData);
              end
              else
              begin
                break;
              end;
            end
            else
            begin
              break;
            end;
          end
          else
          begin  
            break;
          end;
          Inc(PointIndex);
          PriorPoint := ASegment[2];
          SegmentEdges.RemovePoint(X, Y, SegData);
          ASegment := SegPointer^;
          if PointsSame(ASegment[2], PriorPoint) then
          begin
            ASegment := ReverseSegment(ASegment);
          end;
        until (PriorPoint.x = FirstLocation.X)
          and (PriorPoint.y = FirstLocation.y);
        PriorPoint.X := FirstLocation.X;
        PriorPoint.Y := FirstLocation.Y;
        APolygon[PointIndex] := PriorPoint;
        SetLength(APolygon, PointIndex+1);
        Outline[OutlineIndex-1] := APolygon;
      end;
     
    finally
      SegmentEdges.Free;
    end;

  finally
    Edges.Free;
  end;
end;

function TModflowDisvGrid.GetCanDraw: boolean;
begin
  result := FCanDraw;
end;

function TModflowDisvGrid.GetCanDraw3D: Boolean;
begin
  Result := FCanDraw3D and CanDraw
    and (TwoDGrid.Cells.Count > 0);
end;

function TModflowDisvGrid.GetCell(Layer, Col: Integer): TModflowDisVCell;
begin
  Result := FLayers[Layer].Layer[Col];
end;

function TModflowDisvGrid.GetSelectedLayer: Integer;
begin
  result := FSelectedLayer;
end;

function TModflowDisvGrid.GetThreeDContourDataSet: TDataArray;
begin
  result := TwoDGrid.ThreeDContourDataSet;
end;

function TModflowDisvGrid.GetThreeDDataSet: TDataArray;
begin
  result := TwoDGrid.ThreeDDataSet;
end;

function TModflowDisvGrid.GetTopContourDataSet: TDataArray;
begin
  result := TwoDGrid.TopContourDataSet;
end;

function TModflowDisvGrid.GetTopDataSet: TDataArray;
begin
  result := TwoDGrid.TopDataSet;
end;

procedure TModflowDisvGrid.GetXIntervalLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  PolyObject: TFrontPolygon;
begin
  PolyObject := TFrontPolygon(Subject);
  LowerBoundary := PolyObject.MinX;
  UpperBoundary := PolyObject.MaxX;
end;

procedure TModflowDisvGrid.GetYIntervalLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  PolyObject: TFrontPolygon;
begin
  PolyObject := TFrontPolygon(Subject);
  LowerBoundary := PolyObject.MinY;
  UpperBoundary := PolyObject.MaxY;
end;

function TModflowDisvGrid.Is3DMesh: Boolean;
begin
  result := True;
end;

function TModflowDisvGrid.IsFishnetMesh: Boolean;
begin 
  result := False;
end;

function TModflowDisvGrid.IsLayerUniform(const LayerIndex: integer): boolean;
var
  ALayer: TModflowIrregularLayer;
  Elevation: double;
  CellIndex: Integer;
begin
  result := True;
  Assert(LayerIndex >= 0);
  Assert(LayerIndex <= Layers.Count);
  if LayerIndex < Layers.Count then
  begin
    ALayer := Layers[LayerIndex].Layer;
    Assert(ALayer.Count > 0);
    Elevation := ALayer[0].Top;
    for CellIndex := 1 to ALayer.Count -1 do
    begin
      if Elevation <> ALayer[CellIndex].Top then
      begin
        result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    ALayer := Layers[LayerIndex-1].Layer;
    Assert(ALayer.Count > 0);
    Elevation := ALayer[0].Bottom;
    for CellIndex := 1 to ALayer.Count -1 do
    begin
      if Elevation <> ALayer[CellIndex].Bottom then
      begin
        result := False;
        Exit;
      end;
    end;
  end;
end;

function TModflowDisvGrid.LayerCenter(LayerIndex, CellIndex: Integer): double;
begin
  result := (LayerPosition(LayerIndex, CellIndex) + LayerPosition(LayerIndex+1, CellIndex))/2;
end;

function TModflowDisvGrid.LayerPosition(LayerIndex, CellIndex: Integer): double;
var
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
begin
  if LayerIndex = 0 then
  begin
    ALayer := Layers[0].Layer;
    ACell := ALayer[CellIndex];
    result := ACell.Top;
  end
  else
  begin
    ALayer := Layers[LayerIndex-1].Layer;
    ACell := ALayer[CellIndex];
    result := ACell.Bottom;
  end;
end;

procedure TModflowDisvGrid.Loaded;
begin
  TwoDGrid.Loaded;
  SetNodeElevations;
  if Model <> nil then
  begin
    (Model as TCustomModel).InvalidateContours;
  end;
end;

function TModflowDisvGrid.MeshBox(ViewDirection: TViewDirection;
  ObjectAngle: double): TPolygon2D;
begin
  case ViewDirection of
    vdTop: result := TwoDGrid.MeshBox;
    vdFront: result := FrontMeshBox(ObjectAngle);
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

procedure TModflowDisvGrid.MeshChanged;
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
  ViewsChanged;
end;

function TModflowDisvGrid.MeshLimits(ViewDirection: TViewDirection;
  Angle: double): TGridLimit;
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
  ANode2D : TModflowNode;
  APoint: TPoint2D;
  NodeIndex: Integer;
  FoundFirst: Boolean;
begin
  if not CanDraw3D then
  begin
    Exit;
  end;
  CheckUpdateElevations;
  if TwoDGrid.Cells.Count = 0 then
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
    vdTop: result := TwoDGrid.MeshLimits;
    vdFront:
      begin
        FoundFirst := False;
        for NodeIndex := 1 to TwoDGrid.NodeCount - 1 do
        begin
          ANode2D := TwoDGrid.CellCorners[NodeIndex];
//          if ANode2D.Active then
          begin
            APoint := RotatePoint(ANode2D.Location);
            if FoundFirst then
            begin
              if APoint.X < result.MinX then
              begin
                result.MinX := APoint.X;
              end;
              if APoint.X > result.MaxX then
              begin
                result.MaxX := APoint.X;
              end;
              if ANode2D.MinZ < result.MinZ then
              begin
                result.MinZ := ANode2D.MinZ;
              end;
              if ANode2D.MaxZ > result.MaxZ then
              begin
                result.MaxZ := ANode2D.MaxZ;
              end;
            end
            else
            begin
              result.MinX := APoint.X;
              result.MaxX := APoint.X;
              result.MinZ := ANode2D.MinZ;
              result.MaxZ := ANode2D.MaxZ;
              FoundFirst := True;
            end;
          end;
        end;
      end;
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

procedure TModflowDisvGrid.NotifyMeshChanged(Sender: TObject);
begin
  ElevationsNeedUpdating := True;
end;

function TModflowDisvGrid.OkLocation(const DataSet: TDataArray; const Layer,
  Row, Col: integer): boolean;
var
  ActiveArray: TDataArray;
begin
  result := inherited;
  if result and DataSet.Limits.ActiveOnly then
  begin
    ActiveArray := (Model as TCustomModel).
      DataArrayManager.GetDataSetByName(rsActive);
    result := ActiveArray.BooleanData[Layer, Row, Col];
  end;
end;

procedure TModflowDisvGrid.RecordColoredCells;
var
  Red: GLubyte;
  Green: GLubyte;
  Blue: GLubyte;
  ElementIndex: Integer;
  LayerIndex: Integer;
//  AnElement3D: TSutraElement3D;
  StringValues: TStringList;
  ShowColor: Boolean;
  Fraction: Double;
  AColor: TColor;
//  NodeIndex: Integer;
//  ANode3D: TSutraNode3D;
  MinMax: TMinMax;
  LayerMax: integer;
  ACell2D: TModflowIrregularCell2D;
  AnElement3D: TModflowDisVCell;
  MinMaxInitialized: Boolean;
begin
  Assert(ThreeDDataSet <> nil);
  StringValues := TStringList.Create;
  try
    GetMinMax(MinMax, ThreeDDataSet, StringValues, MinMaxInitialized);
    ApplyLimittoMinMax(ThreeDDataSet, MinMax, ThreeDDataSet.Limits);

//    LayerMax := -1;
    if ThreeDDataSet.Orientation = dso3D then
    begin
      LayerMax := LayerCount;
    end
    else
    begin
      LayerMax := 1;
    end;
    glNewList(FColoredCellsOrElementGLIndex, GL_COMPILE);
    try
      case ThreeDDataSet.EvaluatedAt of
        eaBlocks:
          begin
            for ElementIndex := 0 to TwoDGrid.Cells.Count - 1 do
            begin
              ACell2D := TwoDGrid.Cells[ElementIndex];
              for LayerIndex := 0 to LayerMax - 1 do
              begin
                AnElement3D := Layers[LayerIndex].Layer[ACell2D.ElementNumber];
                if AnElement3D.Active then
                begin
                  GetDataSetMeshValue(ThreeDDataSet, LayerIndex,
                    ACell2D.ElementNumber, StringValues, ShowColor, Fraction, MinMax);
                  if ShowColor then
                  begin
                    AColor := frmGoPhast.PhastModel.GridColorParameters.
                      FracToColor(Fraction);
                    ExtractColorComponents(AColor, Red, Green, Blue);
                    glColor3ub(Red, Green, Blue);
                    AnElement3D.Draw3D(LayerIndex);
                  end;
                end;
              end;
            end;
          end;
        eaNodes:
//          begin
//            for NodeIndex := 0 to TwoDGrid.Nodes.Count - 1 do
//            begin
//              for LayerIndex := 0 to LayerMax do
//              begin
//                ANode3D := NodeArray[LayerIndex, NodeIndex];
//                if ANode3D.Active then
//                begin
//                GetDataSetMeshValue(ThreeDDataSet, LayerIndex,
//                  NodeIndex, StringValues, ShowColor, Fraction, MinMax);
//                if ShowColor then
//                begin
//                  AColor := frmGoPhast.PhastModel.GridColorParameters.
//                    FracToColor(Fraction);
//                  ExtractColorComponents(AColor, Red, Green, Blue);
//                  glColor3ub(Red, Green, Blue);
//                  ANode3D.Draw3D;
//                end;
//                end;
//              end;
//            end;
//          end;
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

procedure TModflowDisvGrid.RecordCrossSection;
var
  CellList: TMFIrregularCell2D_List;
  NodeIndex: Integer;
  LayerIndex: Integer;
  X, Y, Z: single;
  ACell: TModflowIrregularCell2D;
  ACell3D: TModflowDisVCell;
  ANode: TModflowNode;
  LayerAboveActive: Boolean;
  CellIndex: Integer;
begin
  if FUpdatingElevations then
  begin
    Exit;
  end;
  CellList := TMFIrregularCell2D_List.Create;
  try
    GetCellsOnCrossSection(CellList);
    if CellList.Count = 0 then
    begin
      Exit;
    end;

    try
      glNewList(FCrossSectionGLIndex, GL_COMPILE);
      try
        glLineWidth(ThinLine);
        glColor3f(0.0, 0.0, 0.0);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          LayerAboveActive := False;
          for LayerIndex := 0 to LayerCount-1 do
          begin
            ACell3D := Layers[LayerIndex].Layer[ACell.ElementNumber];
            if ACell3D.Active then
            begin
              if not LayerAboveActive then
              begin
                glBegin(GL_LINE_STRIP);
                try
                  for NodeIndex := 0 to ACell.ElementCorners.Count - 1 do
                  begin
                    ANode := ACell.ElementCorners[NodeIndex];
                    X := ANode.X;
                    Y := ANode.Y;
                    Z := ANode.FTopElevations[LayerIndex];
                    glVertex3f(X, Y, Z);
                  end;
                  ANode := ACell.ElementCorners[0];
                  X := ANode.X;
                  Y := ANode.Y;
                  Z := ANode.FTopElevations[LayerIndex];
                  glVertex3f(X, Y, Z);
                finally
                  glEnd;
                end;
              end;

              glBegin(GL_LINE_STRIP);
              try
                for NodeIndex := 0 to ACell.ElementCorners.Count - 1 do
                begin
                  ANode := ACell.ElementCorners[NodeIndex];
                  X := ANode.X;
                  Y := ANode.Y;
                  Z := ANode.FBottomElevations[LayerIndex];
                  glVertex3f(X, Y, Z);
                end;
                ANode := ACell.ElementCorners[0];
                X := ANode.X;
                Y := ANode.Y;
                Z := ANode.FBottomElevations[LayerIndex];
                glVertex3f(X, Y, Z);
              finally
                glEnd;
              end;

              glBegin(GL_LINES);
              try
                for NodeIndex := 0 to ACell.ElementCorners.Count - 1 do
                begin
                  ANode := ACell.ElementCorners[NodeIndex];
                  X := ANode.X;
                  Y := ANode.Y;
                  Z := ANode.FTopElevations[LayerIndex];
                  glVertex3f(X, Y, Z);

                  Z := ANode.FBottomElevations[LayerIndex];
                  glVertex3f(X, Y, Z);
                end;
              finally
                glEnd;
              end;


              LayerAboveActive := True;
            end
            else
            begin
              LayerAboveActive := False;
            end;
          end;
        end;

      finally
        glEndList;
      end;
    finally
      FNeedToRecordCrossSection := False;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TModflowDisvGrid.RecordLayer;
var
  Layer: Integer;
  ColIndex: Integer;
  NodeIndex: Integer;
  X, Y, Z: single;
  AnElement3D: TModflowDisVCell;
  Element2D: TModflowIrregularCell2D;
  ANode2D: TModflowNode;
begin
  try
    glNewList(FLayerGLIndex, GL_COMPILE);
    try
      glLineWidth(ThinLine);
      glColor3f(0.0, 0.0, 0.0);

      Layer := SelectedLayer;
      if Layer >= LayerCount then
      begin
        Layer := LayerCount -1;
      end;

      for ColIndex := 0 to TwoDGrid.Cells.Count - 1 do
      begin
        AnElement3D := Layers[Layer].Layer[ColIndex];
        if AnElement3D.Active then
        begin
          glBegin(GL_LINE_LOOP);
          try
            Element2D := TwoDGrid.Cells[ColIndex];
            for NodeIndex := 0 to Element2D.NodeCount - 1 do
            begin
              ANode2D := Element2D.ElementCorners[NodeIndex];
              X := ANode2D.X;
              Y := ANode2D.Y;
              Z := ANode2D.FTopElevations[Layer];
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

function TModflowDisvGrid.RotateFromMeshCoordinatesToRealWorldCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
  MeshAngle: Double;
begin
  result := APoint;
  MeshAngle := CrossSection.Angle;
  if (MeshAngle <> 0) then
  begin
    temp.X := Cos(MeshAngle) * result.X - Sin(MeshAngle) * result.Y;
    temp.Y := Sin(MeshAngle) * result.X + Cos(MeshAngle) * result.Y;
    result := temp;
  end;
end;

function TModflowDisvGrid.RotateFromRealWorldCoordinatesToMeshCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
  MeshAngle: Double;
begin
  result := APoint;
  MeshAngle := CrossSection.Angle;
  if (MeshAngle <> 0) then
  begin
    temp.X := Cos(-MeshAngle) * result.X - Sin(-MeshAngle) * result.Y;
    temp.Y := Sin(-MeshAngle) * result.X + Cos(-MeshAngle) * result.Y;
    result := temp;
  end;
end;

procedure TModflowDisvGrid.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
end;

procedure TModflowDisvGrid.SetCrossSection(const Value: TMeshCrossSectionLine);
begin
  FCrossSection.Assign(Value);
end;

procedure TModflowDisvGrid.SetDefaultCrossSectionLocation;
begin
  CrossSection.Segment := DefaultCrossSectionLocation;
end;

procedure TModflowDisvGrid.SetDrawCellNumbers(const Value: Boolean);
begin
  TwoDGrid.DrawCellNumbers := Value;
  FDrawCellNumbers := Value;
end;

procedure TModflowDisvGrid.SetElevationsNeedUpdating(const Value: boolean);
begin
  FElevationsNeedUpdating := Value;
  if FElevationsNeedUpdating then
  begin
    FStoredBlockPolygons := nil;
    FStoredBlockLimits := nil;
    FreeAndNil(FElementIntervalTree);
    FreeAndNil(FElementRangeTree);
    FreeAndNil(FRotatedElementCenters);

  end;
end;

procedure TModflowDisvGrid.SetFrontContourDataSet(const Value: TDataArray);
begin

end;

procedure TModflowDisvGrid.SetFrontDataSet(const Value: TDataArray);
begin
  if FFrontDataSet <> Value then
  begin
    FFrontDataSet := Value;
  end;
//  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.FrontDiscretizationChanged := True;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TModflowDisvGrid.SetGridLineDrawingChoice(
  const Value: TGridLineDrawingChoice);
begin
  FGridLineDrawingChoice := Value;
  TwoDGrid.GridLineDrawingChoice := Value;
end;

procedure TModflowDisvGrid.SetLayerCount(const Value: Integer);
//var
//  LayerItem: TModflowIrregLayerItem;
//  ALayer: TModflowIrregularLayer;
//  CellIndex: Integer;
//  ACell2D: TModflowIrregularCell2D;
//  ACell3D: TModflowDisVCell;
begin
  if Value <> LayerCount then
  begin
    while Value < LayerCount do
    begin
      Layers.Last.Free;
    end;
    While Value > LayerCount do
    begin
      {LayerItem :=} Layers.Add;
//      ALayer := LayerItem.Layer;
//      for CellIndex := 0 to TwoDGrid.Cells.Count - 1 do
//      begin
//        ACell2D := TwoDGrid.Cells[CellIndex];
//        ACell3D := ALayer.Add;
//        ACell3D.Cell2D := ACell2D;
//      end;
    end;
    ElevationsNeedUpdating := True;
//    MeshChanged;
  end;
end;

procedure TModflowDisvGrid.SetLayers(const Value: TModflowIrregularLayers);
begin
  FLayers.Assign(Value);
end;

procedure TModflowDisvGrid.SetSelectedLayer(Value: Integer);
begin
  if Value >= LayerCount then
  begin
    Value := LayerCount -1;
  end
  else if Value < 0 then
  begin
    Value := 0;
  end;
  if FSelectedLayer <> Value then
  begin
    FSelectedLayer := Value;
    FNeedToRecordLayer := True;
    (Model as TPhastModel).TopContoursUpToDate := False;
    ViewsChanged;
  end;
end;

procedure TModflowDisvGrid.SetThreeDContourDataSet(const Value: TDataArray);
begin
  TwoDGrid.ThreeDContourDataSet := Value;
end;

procedure TModflowDisvGrid.SetThreeDDataSet(const Value: TDataArray);
begin
//  TwoDGrid.ThreeDDataSet := Value;
  if TwoDGrid.ThreeDDataSet <> Value then
  begin
    if TwoDGrid.ThreeDDataSet <> nil then
    begin
      TwoDGrid.ThreeDDataSet.StopsTalkingTo(FColorDataSetObserver);
    end;
    TwoDGrid.ThreeDDataSet := Value;
    if TwoDGrid.ThreeDDataSet <> nil then
    begin
      TwoDGrid.ThreeDDataSet.TalksTo(FColorDataSetObserver);
    end;
    FNeedToRecordColoredCells := True;
  end;
end;

procedure TModflowDisvGrid.SetThreeDGridObserver(const Value: TObserver);
begin
  FThreeDGridObserver := Value;
  if Assigned(FThreeDGridObserver) then
  begin
    FThreeDGridObserver.UpToDate := False;
    FThreeDGridObserver.UpToDate := True;
  end;
end;

procedure TModflowDisvGrid.SetTopContourDataSet(const Value: TDataArray);
begin
  TwoDGrid.TopContourDataSet := Value;
end;

procedure TModflowDisvGrid.SetTopDataSet(const Value: TDataArray);
begin
  TwoDGrid.TopDataSet := Value;
end;

procedure TModflowDisvGrid.SetTwoDGrid(const Value: TModflowIrregularGrid2D);
begin
  FTwoDGrid.Assign(Value);
end;

function TModflowDisvGrid.TopContainingCellOrElement(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  result := TwoDGrid.TopContainingCellOrElement(APoint, EvaluatedAt);
end;

function TModflowDisvGrid.TopOutline(Layer: integer): TOutline;
begin
  FTopOutline.Free;
  result := TIrregularGridOutline.Create(self, vdTop, Layer);
  FTopOutline := result;
end;

function TModflowDisvGrid.TwoDElementCenter(const Column,
  Row: integer): TPoint2D;
begin
  Assert(Row = 0);
  Result := TwoDGrid.Cells[Column].Location;
end;

procedure TModflowDisvGrid.UpdateElevations;
var
  LocalModel: TCustomModel;
  LayerIndex: Integer;
  ColIndex: Integer;
  ElementLayerCount: integer;
  AnElement: TModflowDisVCell;
  Element2D: TModflowIrregularCell2D;
  Active: Boolean;
  ElementIndex: Integer;
  ALayer: TModflowIrregularLayer;
//  FNeedToRecordLayer: Boolean;
  TopDataArray: TDataArray;
  LayerGroupIndex: integer;
  ALayerGroup: TLayerGroup;
  BottomDataArray: TDataArray;
  TopIndex: Integer;
  BottomIndex: Integer;
  Top: double;
  Bottom: double;
  UnitHeight: Double;
  ActiveDataArray: TDataArray;
  LayerFractions: TDoubleDynArray;
  LIndex: Integer;
//  NodeAbove: TSutraNode3D;
begin
  LocalModel := Model as TCustomModel;
  if FUpdatingElevations or (frmGoPhast.ModelSelection <> msModflow2015)
    or not LocalModel.DisvUsed then
  begin
    Exit;
  end;

  FUpdatingElevations := True;
  try
    frmGoPhast.PhastModel.UpdateDataSetDimensions;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    if Layers.Count <> frmGoPhast.PhastModel.LayerStructure.LayerCount then
    begin
      Layers.Count := frmGoPhast.PhastModel.LayerStructure.LayerCount;
      frmGoPhast.UpdateDataSetDimensions;
    end;
    for LayerIndex := 0 to Layers.Count -1 do
    begin
      Layers[LayerIndex].Layer.Clear;
    end;
    BeginUpdate;
    try
      if TwoDGrid.CellCorners.Count = 0 then
      begin
        Exit;
      end;
      begin
        LocalModel := Model as TPhastModel;
        if LocalModel.LayerStructure.Count <= 1 then
        begin
          Exit;
        end;
      end;

      ElementLayerCount := LocalModel.LayerStructure.LayerCount;
      for LayerIndex := 0 to ElementLayerCount - 1 do
      begin
        ALayer := Layers[LayerIndex].Layer;
        for ColIndex := 0 to TwoDGrid.Cells.Count - 1 do
        begin
          Element2D := TwoDGrid.Cells[ColIndex];
          AnElement := ALayer.Add;
          AnElement.FActiveElements := FActiveElements;
          AnElement.Cell2D := Element2D;
          Active := True;
          AnElement.Active := Active;
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

    TopDataArray := LocalModel.DataArrayManager.GetDataSetByName(kModelTop);
    TopDataArray.Initialize;
    TopIndex := 0;
    LIndex := 0;
    for LayerGroupIndex := 1 to LocalModel.LayerStructure.Count -1 do
    begin
      ALayerGroup := LocalModel.LayerStructure[LayerGroupIndex];
      LayerFractions := LocalModel.LayerFractions(ALayerGroup);
      Assert(Length(LayerFractions)+1 = ALayerGroup.LayerCount);
      BottomDataArray := LocalModel.DataArrayManager.GetDataSetByName(ALayerGroup.DataArrayName);
      BottomDataArray.Initialize;
      BottomIndex := TopIndex + ALayerGroup.LayerCount;
      for ColIndex := 0 to TwoDGrid.Cells.Count -1 do
      begin
        Top := TopDataArray.RealData[0, 0, ColIndex];
        Bottom := BottomDataArray.RealData[0, 0, ColIndex];
        UnitHeight := Top - Bottom;
        for LayerIndex := TopIndex to BottomIndex -1 do
        begin
          ALayer := Layers[LayerIndex+LIndex].Layer;
          AnElement := ALayer[ColIndex];
          if LayerIndex = TopIndex then
          begin
            AnElement.Top := Top;
          end
          else
          begin
            AnElement.Top := Top - UnitHeight*(1-LayerFractions[LayerIndex-1]);
          end;

          if LayerIndex = BottomIndex -1 then
          begin
            AnElement.Bottom := Bottom;
          end
          else
          begin
            AnElement.Bottom := Top - UnitHeight*(1-LayerFractions[LayerIndex]);
          end;
        end;
      end;
      TopDataArray := BottomDataArray;
      LIndex := LIndex + ALayerGroup.LayerCount;
    end;

  finally
    FUpdatingElevations := False;
    FNeedToRecordLayer := True;
    FNeedToRecordCrossSection := True;
    FNeedToRedraw3d := True;
    SetNodeElevations;
    ElevationsNeedUpdating := False;
  end;

  ActiveDataArray := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataArray.Initialize;
  FActiveElements.Clear;
  For LayerIndex := 0 to Layers.Count -1 do
  begin
    ALayer := Layers[LayerIndex].Layer;
    for ElementIndex := 0 to ALayer.Count - 1 do
    begin
      AnElement := ALayer[ElementIndex];
      AnElement.Active := ActiveDataArray.BooleanData[LayerIndex, 0, ElementIndex];
      if AnElement.Active then
      begin
        FActiveElements.Add(AnElement);
      end;
    end;
  end;

end;

{ TNeighborItem }

procedure TNeighborItem.Assign(Source: TPersistent);
var
  NeighborSource: TNeighborItem;
begin
  if Source is TNeighborItem then
  begin
    NeighborSource := TNeighborItem(Source);
    Number := NeighborSource.Number;
    Connection := NeighborSource.Connection;
  end
  else
  begin
    inherited;
  end;
end;

procedure TNeighborItem.SetConnection(const Value: TNeighborConnection);
begin
  if FConnection <> Value then
  begin
    FConnection := Value;
    InvalidateModel;
  end;
end;

procedure TNeighborItem.SetNumber(const Value: integer);
begin
  SetIntegerProperty(FNumber, Value);
end;

{ TNeighbors }

constructor TNeighbors.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TNeighborItem, InvalidateModelEvent);
end;

function TNeighbors.GetItem(Index: Integer): TNeighborItem;
begin
  Result := inherited Items[Index] as TNeighborItem;
end;

procedure TNeighbors.SetItem(Index: Integer; const Value: TNeighborItem);
begin
  inherited Items[Index] := Value;
end;

{ TModflowDisUCell }

procedure TModflowDisUCell.Assign(Source: TPersistent);
begin
  if Source is TModflowDisUCell then
  begin
    Neighbors := TModflowDisUCell(Source).Neighbors;
  end;
  inherited;

end;

constructor TModflowDisUCell.Create(Collection: TCollection);
begin
  inherited;
  FNeighbors := TNeighbors.Create(OnInvalidateModel);
end;

destructor TModflowDisUCell.Destroy;
begin
  FNeighbors.Free;
  inherited;
end;

procedure TModflowDisUCell.SetNeighbors(const Value: TNeighbors);
begin
  FNeighbors.Assign(Value);
end;

{ TMF_IrregCellComparer }

function TMF_IrregCellComparer.CompareX(const Left,
  Right: TModflowIrregularCell2D): Integer;
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

//function TMF_IrregCellComparer.CompleteOverlap(const Left,
//  Right: TModflowIrregularCell2D): Boolean;
//var
//  LeftMinX: Double;
//  LeftMaxX: Double;
//  RightMinX: Double;
//  RightMaxX: Double;
//  Epsilon: Double;
//begin
//  GetMinMaxX(Left, LeftMinX, LeftMaxX);
//  GetMinMaxX(Right, RightMinX, RightMaxX);
//  Epsilon := (Max(LeftMaxX,RightMaxX) - Min(LeftMinX,RightMinX))/1e6;
//  result := ((LeftMinX <= RightMinX+Epsilon) and (LeftMaxX >= RightMaxX-Epsilon))
//    or ((LeftMinX+Epsilon >= RightMinX) and (LeftMaxX-Epsilon <= RightMaxX))
//end;

constructor TMF_IrregCellComparer.Create(CrossSectionAngle: Double;
  StartPoint: TPoint2D);
begin
  FStartPoint := StartPoint;
  FAngle := CrossSectionAngle;
end;

//procedure TMF_IrregCellComparer.GetMinMaxX(ACell: TModflowIrregularCell2D;
//  out MinX, MaxX: double);
//var
//  NodeIndex: Integer;
//  Angle: double;
//  APoint: TPoint2D;
//  Value: double;
//begin
//  APoint := ACell.Center;
//  Angle := ArcTan2(APoint.y - FStartPoint.y, APoint.x - FStartPoint.x) - FAngle;
//  MaxX := Distance(FStartPoint, APoint)*Cos(Angle);
//  MinX := MaxX;
//  for NodeIndex := 0 to ACell.ElementCorners.Count - 1 do
//  begin
//    APoint := ACell.ElementCorners[NodeIndex].Location;
//    Angle := ArcTan2(APoint.y - FStartPoint.y, APoint.x - FStartPoint.x) - FAngle;
//    Value := Distance(FStartPoint, APoint)*Cos(Angle);
//    if Value < MinX then
//    begin
//      MinX := Value;
//    end;
//    if Value > MaxX then
//    begin
//      MaxX := Value;
//    end;
//  end;
//
//end;

function TMF_IrregCellComparer.Compare(const Left,
  Right: TModflowIrregularCell2D): Integer;
var
  Angle: double;
  LeftY: FastGeo.TFloat;
  RightY: FastGeo.TFloat;
  CenterPoint: TPoint2D;
begin
  CenterPoint := Left.Center;
  Angle := ArcTan2(CenterPoint.y - FStartPoint.y, CenterPoint.x - FStartPoint.x) - FAngle;
  LeftY := Distance(FStartPoint, CenterPoint)*Sin(Angle);

  CenterPoint := Right.Center;
  Angle := ArcTan2(CenterPoint.y - FStartPoint.y, CenterPoint.x - FStartPoint.x) - FAngle;
  RightY := Distance(FStartPoint, CenterPoint)*Sin(Angle);

//  result := Sign(LeftY - RightY);
  result := Sign(RightY - LeftY);
end;

{ TOverlapInterval }

function TOverlapInterval.IsInside(OtherInterval: TOverlapInterval): Boolean;
begin
  result := (Max <= OtherInterval.Max) and (Min >= OtherInterval.Min);
end;

function TOverlapInterval.OverLaps(OtherInterval: TOverlapInterval): Boolean;
begin
  result := ((Min <= OtherInterval.Max) and (Max >= OtherInterval.Min))
    or ((Max >= OtherInterval.Min) and (Min <= OtherInterval.Max))
end;

{ TCellEdge }

function TCellEdge.IsSame(OtherEdge: TCellEdge): Boolean;
begin
  result := ((Node1 = OtherEdge.Node1) and (Node2 = OtherEdge.Node2))
    or ((Node1 = OtherEdge.Node2) and (Node2 = OtherEdge.Node1));
end;

{ TIrregularGridOutline }

constructor TIrregularGridOutline.Create(Mesh: TModflowDisvGrid;
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
    if UnionPolygon = nil then
    begin
      Exit;
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

procedure TIrregularGridOutline.GetFrontPolygons(Mesh: TModflowDisvGrid;
  var UnionPolygon: TGpcPolygonClass);
var  
  ElementPolygons: TCellElementPolygons2D; 
  Limits: TLimitsArray;
  ElementList: TIElement2DList;
  APolygon: TGpcPolygonClass;
  EmptyPolygon: TGpcPolygonClass;
  TempPolygon: TGpcPolygonClass;
  NumberOfContours: Integer;
  CellIndex: Integer;
  LayerIndex: Integer;
  ACell: IElement2D;
  PointIndex: Integer;
begin
  UnionPolygon := nil;
  ElementPolygons := Mesh.FrontPolygons(Mesh.CrossSection.Angle,
    eaBlocks, Limits);
  ElementList := TIElement2DList.Create;
  EmptyPolygon := TGpcPolygonClass.Create;
  try
    EmptyPolygon.NumberOfContours := 0;

    Mesh.GetElementsIntfOnCrossSection(ElementList);
//    NumberOfContours := 0;
//    for CellIndex := 0 to ElementList.Count -1 do
//    begin
//      ACell := ElementList[CellIndex];
//      for LayerIndex := 0 to Mesh.LayerCount -1 do
//      begin
//        if Length(ElementPolygons[LayerIndex,
//          ACell.ElementNumber]) > 0 then
//        begin
//          Inc(NumberOfContours)
//        end;
//      end;
//    end;

    NumberOfContours := 0;
    for CellIndex := 0 to ElementList.Count -1 do
    begin
      ACell := ElementList[CellIndex];
      for LayerIndex := 0 to Mesh.LayerCount -1 do
      begin
        if Length(ElementPolygons[LayerIndex, ACell.ElementNumber]) > 0 then
        begin
          APolygon := TGpcPolygonClass.Create;
          try
            APolygon.NumberOfContours := 1;
            APolygon.VertexCount[0] :=
              Length(ElementPolygons[LayerIndex, ACell.ElementNumber]);
            APolygon.Holes[0] := False;
            for PointIndex := 0 to Length(ElementPolygons[LayerIndex,
              ACell.ElementNumber]) -1 do
            begin
              APolygon.Vertices[NumberOfContours, PointIndex] :=
                ElementPolygons[LayerIndex, ACell.ElementNumber, PointIndex];
            end;
            if UnionPolygon = nil then
            begin
              UnionPolygon := APolygon;
              APolygon := nil;
            end
            else 
            begin
              TempPolygon := UnionPolygon;
              UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION, APolygon, TempPolygon);
              TempPolygon.Free;
            end;
//            Inc(NumberOfContours);
          finally
            APolygon.Free;
          end;
        end;
      end;
    end;

    {NumberOfContours := 0;
    for CellIndex := 0 to ElementList.Count -1 do
    begin
      ACell := ElementList[CellIndex];
      for LayerIndex := 0 to Mesh.LayerCount -1 do
      begin
        if Length(ElementPolygons[LayerIndex, ACell.ElementNumber]) > 0 then
        begin
          for PointIndex := 0 to Length(ElementPolygons[LayerIndex,
            ACell.ElementNumber]) -1 do
          begin
            APolygon.Vertices[NumberOfContours, PointIndex] :=
              ElementPolygons[LayerIndex, ACell.ElementNumber, PointIndex];
          end;
          Inc(NumberOfContours);
        end;
      end;
    end;}
//    EmptyPolygon.NumberOfContours := 0;
//    UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION, APolygon, EmptyPolygon);
//    while (UnionPolygon.NumberOfContours <> APolygon.NumberOfContours) do
//    begin
//      APolygon.Free;
//      APolygon := UnionPolygon;
//      UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION, APolygon, EmptyPolygon);
//    end;
  finally
//    APolygon.Free;
    EmptyPolygon.Free;
    ElementList.Free;
  end;
  
end;

procedure TIrregularGridOutline.GetTopPolygons(Mesh: TModflowDisvGrid;
  Layer: Integer; var UnionPolygon: TGpcPolygonClass);
var 
  Outline: TPolygon2Darray;
  APolygon: TGpcPolygonClass;
  EmptyPolygon: TGpcPolygonClass;
  ContourIndex: Integer;
  NodeIndex: Integer;
begin
  Mesh.GetActiveOutlineTop(Layer, Outline);
  APolygon := TGpcPolygonClass.Create;
  EmptyPolygon := TGpcPolygonClass.Create;
  try
    APolygon.NumberOfContours := Length(Outline);
    for ContourIndex := 0 to APolygon.NumberOfContours -1 do
    begin
      APolygon.VertexCount[ContourIndex] := Length(Outline[ContourIndex]);
      APolygon.Holes[ContourIndex] := False;
    end;
    for ContourIndex := 0 to APolygon.NumberOfContours -1 do
    begin
      for NodeIndex := 0 to APolygon.VertexCount[ContourIndex] -1 do
      begin
        APolygon.Vertices[ContourIndex, NodeIndex] := Outline[ContourIndex, NodeIndex];
      end;
    end;
    EmptyPolygon.NumberOfContours := 0;
    UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION, APolygon, EmptyPolygon);
  finally
    APolygon.Free;
    EmptyPolygon.Free;
  end;
end;

{ TMf6Element2DLeaf }

constructor TMf6Element2DLeaf.Create(AnElement: TModflowIrregularCell2D);
begin
  FElement := AnElement;
  FMinX := FElement.MinX;
  FMaxX := FElement.MaxX;
  FMinY := FElement.MinY;
  FMaxY := FElement.MaxY;
end;

function TMf6Element2DLeaf.GetCoordinate(Depth: integer): double;
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

end.
