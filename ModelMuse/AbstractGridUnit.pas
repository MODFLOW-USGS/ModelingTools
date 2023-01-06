{
  @abstract(The main item in @name is @link(TCustomModelGrid) which is an abstract
  class that defines the interface for a grid.
  It implements storage for columns and rows but not for layers.)


@author(Richard B. Winston <rbwinst@usgs.gov>)
  }
unit AbstractGridUnit;

interface

uses System.UITypes, Winapi.Windows, Types, SysUtils, Classes, Graphics, Controls,
  Forms,
  OpenGL,
  GR32, // TBitmap32 and TFloatRect are declared in GR32.
  FastGEO, GoPhastTypes, RbwParser, ZoomBox2, SubscriptionUnit,
  IsosurfaceUnit, ContourUnit, DataSetUnit, LineStorage, ModflowCellUnit;

type
  // @abstract(@name is used for grid related errors.)
  EInvalidGrid = class(Exception);

  T3DCellCoordinates = record
    Col1_Row1_Lay1: T3DRealPoint;
    Col2_Row1_Lay1: T3DRealPoint;
    Col1_Row2_Lay1: T3DRealPoint;
    Col2_Row2_Lay1: T3DRealPoint;
    Col1_Row1_Lay2: T3DRealPoint;
    Col2_Row1_Lay2: T3DRealPoint;
    Col1_Row2_Lay2: T3DRealPoint;
    Col2_Row2_Lay2: T3DRealPoint;
  end;

  T3DElementCoordinates = record
    TopCenter: T3DRealPoint;
    TopEdge: array[0..7] of T3DRealPoint;
    BottomCenter: T3DRealPoint;
    BottomEdge: array[0..7] of T3DRealPoint;
  end;

  // @name is used to indicate the direction in which columns are numbered.
  TColumnDirection = (cdWestToEast, cdEastToWest);
  // @name is used to indicate the direction in which rows are numbered.
  TRowDirection = (rdSouthToNorth, rdNorthToSouth);
  // @name is used to indicate the direction in which layers are numbered.
  TLayerDirection = (ldBottomToTop, ldTopToBottom);

  { @name is used to store a color for each cell in @link(TCustomModelGrid).
  @longcode(#TCellColors = array of array of array of TColor;#)
  }
  TCellColors = array of array of array of TColor;

  TGridLineDrawingChoice = (gldcAll, gldcExterior, gldcActive, gldcActiveEdge);

  TCustomDiscretization = class(TGoPhastPersistent)
  private
    function GetCanDraw: boolean;
    procedure SetCanDraw(const Value: boolean);
  strict private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
  protected
    // See @link(ThreeDDataSet).
    FThreeDDataSet: TDataArray;
    FCanDraw: boolean;
    FTopContourPlotList: TPlotList;
    FFrontContourPlotList: TPlotList;
    FSideContourPlotList: TPlotList;
    function ValueOK(DataSet: TDataArray;
      const Layer, Row, Col: integer): boolean;
    procedure InitializeMinMax(const Layer, Row, Col: integer;
      DataSet: TDataArray; var MinMaxInitialized: boolean; var MinMax: TMinMax;
      StringValues: TStringList);
    procedure UpdateMinMax(const Layer, Row, Col: integer; DataSet: TDataArray;
      var MinMaxInitialized: boolean; var MinMax: TMinMax;
      StringValues: TStringList); virtual;
    procedure SetMinMax(DataSet: TDataArray; var MinMaxInitialized: boolean;
      var MinMax: TMinMax; StringValues: TStringList;
      LayerCount, RowCount, ColCount: integer); virtual;
    procedure CalculateMinMax(DataSet: TDataArray;
      var MinMaxInitialized: boolean; var MinMax: TMinMax;
      StringValues: TStringList); virtual; abstract;
    procedure InvalidateContours;
    function GetItemTopLocation(const EvalAt: TEvaluatedAt; const Column,
      Row: integer): TPoint2D; virtual; abstract;
    function GetShortestHorizontalBlockEdge(Layer, Row, Column: Integer): double;
      virtual; abstract;
  public
    // notify the views that the need to redraw;
    procedure ViewsChanged;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    function OkLocation(const DataSet: TDataArray;
      const Layer, Row, Col: integer): boolean; virtual;
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList; out MinMaxInitialized: Boolean); virtual; abstract;
   { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
   //
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure ApplyLimitToMinMax(DataSet: TDataArray; var MinMax: TMinMax;
      Limits: TColoringLimits);
    property ItemTopLocation[const EvalAt: TEvaluatedAt; const Column,
      Row: integer]: TPoint2D read GetItemTopLocation;
    property ShortestHorizontalBlockEdge[Layer, Row, Column: Integer]: double
      read GetShortestHorizontalBlockEdge;
  end;
  { TODO : ThreeDDataSet, TPhastModel.ThreeDTimeList, and
TPhastModel.ThreeDDisplayTime are all related.  Maybe they should be
put together somehow.  There are similar groupings for the top, front, and
side views of the model.}

  {@abstract(@name is an abstract class that defines the interface for a grid.
   It implements storage for columns and rows but not for layers.)  It also
   implements the display of the top view of the grid but not the front or side
   views.  The columns and rows boundaries are stored in one-dimensional
   arrays.  For some finite-difference models such as PHAST, the layers can
   also be stored in a one-dimensional arrays.  Others, such as MODFLOW,
   require a more complex access method.  The interface defined
   in @name defines the more complex access method required by a
   MODFLOW-type grid.
   )
   @seealso(TPhastGrid)
   @seealso(TModflowGrid)
   }
  TCustomModelGrid = class(TCustomDiscretization)
  private
    // See @link(EdgesGLIndex).
    FEdgesGLIndex: GLuint;
    // See @link(CellsGLIndex).
    FCellsGLIndex: GLuint;
    // See @link(ColumnCount).
    FColumnCount: integer;
    // See @link(ColumnDirection).
    FColumnDirection: TColumnDirection;
    // See @link(ColumnPositions).
    FColumnPositions: TOneDRealArray;
    // See @link(FrontCellColors).
    FFrontCellColors: array of array of TColor;
    // See @link(FrontDataSet).
    FFrontDataSet: TDataArray;
    // See @link(FrontGridGLIndex).
    FFrontGridGLIndex: GLuint;
    // See @link(GridAngle).
    FGridAngle: real;
    // See @link(GridShellGLIndex).
    FGridShellGLIndex: GLuint;
    // See @link(LayerCount).
    FLayerCount: integer;
    // See @link(LayerDirection).
    FLayerDirection: TLayerDirection;
    // See @link(NeedToRecalculate3DCellColors).
    FNeedToRecalculate3DCellColors: boolean;
    // See @link(OnSelectedColumnChange).
    FOnSelectedColumnChange: TNotifyEvent;
    // @name: TNotifyEvent;
    // See @link(OnSelectedLayerChange).
    FOnSelectedLayerChange: TNotifyEvent;
    // See @link(OnSelectedRowChange).
    FOnSelectedRowChange: TNotifyEvent;
    // See @link(RowCount).
    FRowCount: integer;
    // See @link(RowDirection).
    FRowDirection: TRowDirection;
    // See @link(RowPositions).
    FRowPositions: TOneDRealArray;
    // See @link(SelectedColumn).
    FSelectedColumn: integer;
    // See @link(SelectedLayer).
    FSelectedLayer: integer;
    // See @link(SelectedRow).
    FSelectedRow: integer;
    // See @link(SideCellColors).
    FSideCellColors: array of array of TColor;
    // See @link(SideDataSet).
    FSideDataSet: TDataArray;
    // See @link(SideGridGLIndex)
    FSideGridGLIndex: GLuint;

    // See @link(TopCellColors).
    FTopCellColors: array of array of TColor;
    // See @link(TopDataSet).
    FTopDataSet: TDataArray;
    // See @link(TopGridGLIndex).
    FTopGridGLIndex: GLuint;
    // See @link(TopGridObserver).
    FTopGridObserver: TObserver;
    // See @link(ThreeDGridObserver).
    FThreeDGridObserver: TObserver;
    // See @link(DisplayColumn).
    FDisplayColumn: integer;
    // See @link(DisplayLayer).
    FDisplayLayer: integer;
    // See @link(DisplayRow).
    FDisplayRow: integer;
    // @name is used in @link(LayersChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @seealso(BeginLayerChange)
    // @seealso(EndLayerChange)
    FLayerUpdate: Integer;
    // @name is used in @link(ColumnsChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @SeeAlso(BeginColumnChange)
    // @SeeAlso(EndColumnChange)
    FColumnUpdate: Integer;
    // @name is used in @link(GridChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @SeeAlso(BeginGridChange)
    // @SeeAlso(EndGridChange)
    FGridUpdate: Integer;
    // @name is used in @link(RowsChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @SeeAlso(BeginRowChange)
    // @SeeAlso(EndRowChange)
    FRowUpdate: Integer;
    // See @link(CreateNodeGlGrid).
    FNodeGlGrid: TGrid;
    // See @link(CreateBlockGlGrid).
    FBlockGlGrid: TGrid;
    // @name is used for drawing contours on the top view of the
    // model for data evaluated at nodes.
    // See @link(ContourGrid).
    FTopNodeContourGrid: T2DGrid;
    // @name is used for drawing contours on the front view of the
    // model for data evaluated at nodes.
    // See @link(ContourGrid).
    FFrontNodeContourGrid: T2DGrid;
    // @name is used for drawing contours on the side view of the
    // model for data evaluated at nodes.
    // See @link(ContourGrid).
    FSideNodeContourGrid: T2DGrid;
    // @name is used for drawing contours on the top view of the
    // model for data evaluated at elements.
    // See @link(ContourGrid).
    FTopElementContourGrid: T2DGrid;
    // @name is used for drawing contours on the front view of the
    // model for data evaluated at elements.
    // See @link(ContourGrid).
    FFrontElementContourGrid: T2DGrid;
    // @name is used for drawing contours on the side view of the
    // model for data evaluated at elements.
    // See @link(ContourGrid).
    FSideElementContourGrid: T2DGrid;
    // @name is used to store whether or not the
    // grid shell display
    // in 3D has been recorded or not.
    FRecordedShell: Boolean;
    // @name is used to store whether or not the
    // grid in side view
    // in 3D has been recorded or not.
    FRecordedSideGrid: Boolean;
    // @name is used to store whether or not the
    // grid in front view
    // in 3D has been recorded or not.
    FRecordedFrontGrid: Boolean;
    // @name is used to store whether or not the
    // grid in top view
    // in 3D has been recorded or not.
    FRecordedTopGrid: Boolean;
    // @name is the number of elements in a grid.
    // (In MODFLOW, @name is the number of cells in the grid.)
    FElementCount: integer;
    FGridLineDrawingChoice: TGridLineDrawingChoice;
    FTopContourDataSet: TDataArray;
    FThreeDContourDataSet: TDataArray;
    FFrontContourDataSet: TDataArray;
    FSideContourDataSet: TDataArray;
    FDrawing3DGrid: Boolean;
    FDraw3DAllowed: boolean;
    FBlockGridCache: TMemoryStream;
    FNodeGridCache: TMemoryStream;
    FDrawColoredGridLines: boolean;
    // See @link(ColWidths).
    function GetColWidths: TOneDRealArray;
    // See @link(RowWidths).
    function GetRowWidths: TOneDRealArray;
    // @name returns an array that has a length equal to
    // Length(PositionArray)-1.  The result has the difference between
    // each adjacent member of PositionArray.  @name is used in
    // @link(GetColWidths) and @link(GetRowWidths).
    function GetWidths(const PositionArray: TOneDRealArray): TOneDRealArray;
    // @name is used to read @link(ColumnPositions) from a stream.
    // It calls @link(ReadRealArray) to do this.
    procedure ReadColumnPositions(Reader: TReader);
    // @name is used to read @link(RowPositions) from a stream.
    // It calls @link(ReadRealArray) to do this.
    procedure ReadRowPositions(Reader: TReader);
    // See @link(NeedToRecalculate3DCellColors).
    procedure SetNeedToRecalculate3DCellColors(const Value: boolean);
    // See @link(NeedToRecalculateFrontCellColors).
    procedure SetNeedToRecalculateFrontCellColors(const Value: boolean);
    // See @link(NeedToRecalculateSideCellColors).
    procedure SetNeedToRecalculateSideCellColors(const Value: boolean);
    // See @link(NeedToRecalculateTopCellColors).
    procedure SetNeedToRecalculateTopCellColors(const Value: boolean);
    // See @link(SelectedColumn).
    procedure SetSelectedColumn(Value: integer);
    // See @link(SelectedLayer).
    procedure SetSelectedLayer(Value: integer);
    // See @link(SelectedRow).
    procedure SetSelectedRow(Value: integer);
    // See @1ink(ThreeDGridObserver).
    procedure SetThreeDGridObserver(const Value: TObserver);
    // See @1ink(TopGridObserver).
    procedure SetTopGridObserver(const Value: TObserver);
    // @name writes @link(ColumnPositions) to
    // a stream using @link(WriteRealArray).
    procedure WriteColumnPositions(Writer: TWriter);
    // @name writes @link(RowPositions) to
    // a stream using @link(WriteRealArray).
    procedure WriteRowPositions(Writer: TWriter);
    // See @link(DisplayColumn).
    procedure SetDisplayColumn(Value: integer);
    // See @link(DisplayLayer).
    procedure SetDisplayLayer(Value: integer);
    // See @link(DisplayRow).
    procedure SetDisplayRow(Value: integer);
    procedure RecordColoredGridEdges;
    procedure DrawLeftCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawRightCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawBackCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawFrontCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawBottomCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawTopCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    // @name creates @link(FBlockGlGrid) which can be used for
    // drawing isosurfaces of data at nodes. @link(FBlockGlGrid)
    // has dimensions (@link(ColumnCount) + 2, @link(RowCount) + 2,
    // @link(LayerCount) + 2.  Thus there is an extra point in each
    // direction outside the element centers.  These points are used in drawing
    // @link(TScreenObject)s in 3D.
    //
    // @name is called by @link(PrivateGlGrid).
    procedure CreateBlockGlGrid;
    // @name creates @link(FNodeGlGrid) which can be used for
    // drawing isosurfaces of data at nodes. @link(FNodeGlGrid)
    // has dimensions (@link(ColumnCount) + 3, @link(RowCount) + 3,
    // @link(LayerCount) + 3.  Thus there is an extra point in each
    // direction outside the grid.  These points are used in drawing
    // @link(TScreenObject)s in 3D.
    //
    // @name is called by @link(PrivateGlGrid).
    procedure CreateNodeGlGrid;
    // @name creates and caches a @link(TGrid) which can then
    // be used for drawing isosurfaces or contour lines.
    // @name is called from @link(ContourGrid) and @link(GlGrid).
    function PrivateGlGrid(EvaluatedAt: TEvaluatedAt;
      ModelSelection: TModelSelection):TGrid;
    procedure SetGridLineDrawingChoice(const Value: TGridLineDrawingChoice);
    procedure DrawTopContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    procedure SetOnSelectedLayerChange(const Value: TNotifyEvent);
    procedure CacheBlockGlGrid;
    procedure CacheGlGrid(GridCacheStream: TMemoryStream; var AGlGrid: TGrid);
    procedure RestoreBlockGlGrid;
    procedure RestoreGlGrid(var AGlGrid: TGrid; GridCacheStream: TMemoryStream);
    procedure CacheNodeGlGrid;
    procedure RestoreNodeGlGrid;
    function IsActiveOk(const DataSet: TDataArray; const Layer, Row,
      Col: integer): boolean;
    procedure DrawOrdinaryTopRows(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure DrawOrdinaryTopColumns(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure GetCounts(DataSet: TDataArray; var LayerCount, RowCount,
      ColCount: integer);
    function GetCellCoordinates(Col, Row, Layer: integer): T3DCellCoordinates;
    function GetElementCoordinates(Col, Row, Layer: integer): T3DElementCoordinates;
    procedure InvalidateScreenObjects;
    procedure SetOnSelectedColumnChange(const Value: TNotifyEvent);
    procedure SetOnSelectedRowChange(const Value: TNotifyEvent);
    function GetThreeDGridObserver: TObserver;
    function GetFrontDataSet: TDataArray;
    function GetFrontContourDataSet: TDataArray;
    function GetThreeDDataSet: TDataArray;
    function GetGridAngleDegrees: double;
    function GetTopElementOutline(Row, Column: Integer): TElementOutline;
  protected
    procedure UpdateMinMax(const Layer, Row, Col: integer; DataSet: TDataArray;
      var MinMaxInitialized: boolean;  var MinMax: TMinMax;
      StringValues: TStringList); override;
    procedure SetMinMax(DataSet: TDataArray; var MinMaxInitialized: boolean;
      var MinMax: TMinMax; StringValues: TStringList;
      LayerCount, RowCount, ColCount: integer); override;
    procedure CalculateMinMax(DataSet: TDataArray;
      var MinMaxInitialized: Boolean;
      var MinMax: TMinMax; StringValues: TStringList); override;
    function GetElevationsNeedUpdating: Boolean; virtual;
  protected
    // @name is used to indicate whether or not the display lists
    // @link(FGridShellGLIndex), @link(FTopGridGLIndex),
    // @link(FFrontGridGLIndex), @link(FSideGridGLIndex), and
    // @link(FCellsGLIndex) have been created.
    FListsCreated: boolean;
    { @name indicates that the colors of the cells
      in the front view of the grid
      need to be recalculated.}
    FNeedToRecalculateFrontCellColors: boolean;
    { @name indicates that the colors of the cells
      in the side view of the grid
      need to be recalculated.}
    FNeedToRecalculateSideCellColors: boolean;
    { @name indicates that the colors of the cells
      in the top view of the grid
      need to be recalculated.}
    FNeedToRecalculateTopCellColors: boolean;
    { @name indicates that the 3D colored cells in the grid
      or the grid shell need to be redrawn.}
    FNeedToRedraw3d: boolean;
    // @name is used to store an OpenGL display list for display
    // of properties of the edges of cells. @name is used to display
    // Horizontal Flow Barriers in MODFLOW.
    property EdgesGLIndex: GLuint read FEdgesGLIndex;
    // @name is used to store an OpenGL display list for the colored
    // cells or elements of the grid.
    property CellsGLIndex: GLuint read FCellsGLIndex;
    // @name causes @link(TopGridObserver) to have @link(TObserver.UpToDate)
    // set to false and then to true. That will invalidate everything that
    // depends on @link(TopGridObserver).
    procedure ColumnsChanged;
    { @name is used to define how @link(ColumnPositions) and
      @link(RowPositions) are written to and read from a stream.  This
      allows those properties to be saved to a file and read from a file.
      @SeeAlso(ReadRealArray) @SeeAlso(ReadColumnPositions)
      @SeeAlso(ReadRowPositions) @SeeAlso(WriteRealArray)
      @SeeAlso(WriteColumnPositions) @SeeAlso(WriteRowPositions)}
    procedure DefineProperties(Filer: TFiler); override;
    {@name draws a front view of the grid on BitMap.}
    procedure DrawFront(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); virtual; abstract;
    {@name draws a side view of the grid on BitMap.}
    procedure DrawSide(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); virtual; abstract;
    {@name draws a top view of the grid on BitMap.}
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); virtual;
    // @name is used to store a OpenGL display list for the front
    // view of the grid.
    property FrontGridGLIndex: GLuint read FFrontGridGLIndex;
    // @name sets Elevations to a 3D array of real numbers.  Each
    // number in the array is the elevation of one corner of a cell or
    // element.
    procedure GetCellCornerElevations(const EvalAt: TEvaluatedAt;
      out Elevations: TThreeDRealArray); virtual; abstract;
    // See @link(CellElevation).
    function GetCellElevation(const Column, Row, Layer: integer): real;
      virtual; abstract;
    // See @link(CellThickness).
    function GetCellThickness(const Column, Row, Layer: integer): real;
      virtual; abstract;
    // See @link(ColumnPosition).
    function GetColumnPosition(const Column: integer): real; virtual;
    // See @link(ColumnWidth).
    function GetColumnWidth(const Column: integer): real; virtual;
    { Positions is an array of column or row boundaries.
     @name gets the column or row (not column boundary
     or row boundary) that contains APosition.
     If APosition < Positions[0] then result := -1.
     If APosition > Positions[Length(Positions)-1] then
       result := Length(Positions)-1. }
    function GetContainingColumnOrRow(const Positions: TOneDRealArray;
      const APosition: real): integer;
    {See @link(FrontCellColors).}
    function GetFrontCellColors(const Column, Layer: integer): TColor; virtual;
    // See @link(RowPosition).
    function GetRowPosition(const Row: integer): real; virtual;
    // See @link(RowWidth).
    function GetRowWidth(const Row: integer): real; virtual;
    {See @link(SideCellColors).}
    function GetSideCellColors(const Row, Layer: integer): TColor; virtual;
    {See @link(TopCellColors).}
    function GetTopCellColors(const Column, Row: integer): TColor; virtual;
    // See @link(TwoDCellElevations)
    function GetTwoDCellElevations(const Col, Row: integer): TOneDRealArray;
      virtual; abstract;
    // @name is used to store a OpenGL display list for the grid shell.
    property GridShellGLIndex: GLuint read FGridShellGLIndex;
    // @name causes @link(ThreeDGridObserver) to have @link(TObserver.UpToDate)
    // set to false and then to true. That will invalidate everything that
    // depends on @link(ThreeDGridObserver).
    procedure LayersChanged;
    {@name finds the index of the element in Positions that is
     closest to APosition.  If the index is known to be within a certain range,
     NearestColumnOrRow can be made faster by specifying First and Last.
     If APosition is less than the first member of Positions, @name returns -1.
     If APosition is greater than the last member of Positions, @name returns
     Length(Positions).
     @param(Positions is an array of real numbers sorted in ascending order.)
     @param(APosition, is the value that is being searched for.)
     @param(First is the first member of Positions to be included in the
     search. If First = -1, use the first member of Positions as the
     beginning of the search.)
     @param(Last is the last member of Positions to be included in the
     search. If Last = -1, use the last member of Positions as the
     beginning of the search.)
     }
    function NearestColumnOrRow(const Positions: TOneDRealArray;
      const APosition: real; const First: integer = -1;
      const Last: integer = -1): integer;
    // @name creates an OpenGL display list using @link(FCellsGLIndex)
    // to show colored grid cells or elements.
    procedure RecordColoredGrid; virtual;
    // @name creates an OpenGL display list using @link(FFrontGridGLIndex)
    // to show the grid on the front view of the model.
    procedure RecordFront; virtual;
    // @name creates an OpenGL display list using @link(GridShellGLIndex)
    // to show the grid shell.
    procedure RecordShell; virtual;
    // @name creates an OpenGL display list using @link(FSideGridGLIndex)
    // to show the grid on the side view of the model.
    procedure RecordSide; virtual;
    // @name creates an OpenGL display list using @link(FTopGridGLIndex)
    // to show the grid on the top view of the model.
    procedure RecordTop; virtual;
    // @name causes @link(TopGridObserver) to have @link(TObserver.UpToDate)
    // set to false and then to true. That will invalidate everything that
    // depends on @link(TopGridObserver).
    procedure RowsChanged;
    // See @link(CellElevation).
    procedure SetCellElevation(const Column, Row, Layer: integer;
      const Value: real); virtual; abstract;
    // See @link(CellThickness).
    procedure SetCellThickness(const Column, Row, Layer: integer;
      const Value: real); virtual; abstract;
    // See @link(ColumnCount).
    procedure SetColumnCount(const Value: integer); virtual;
    // See @link(ColumnDirection).
    procedure SetColumnDirection(const Value: TColumnDirection); virtual;
    // @name sets LocalLineColor to an appropriate value
    // for the column indicated by ColIndex.
    procedure SetColumnLineColor(ColIndex: Integer; LocalEvalAt: TEvaluatedAt;
      var LocalLineColor: TColor32; var LineWidth: single);
    // See @link(ColumnPosition).
    procedure SetColumnPosition(const Column: integer; const Value: real);
      virtual;
    // See @link(ColumnPositions).
    procedure SetColumnPositions(const Value: TOneDRealArray);
    // See @link(ColumnWidth).
    procedure SetColumnWidth(const Column: integer; const Value: real);
    {See @link(FrontCellColors).}
    procedure SetFrontCellColors(const Column, Layer: integer;
      const Value: TColor); virtual;
    // See @link(GridAngle).
    procedure SetGridAngle(Value: real); virtual;
    // see @link(LayerCount).
    procedure SetLayerCount(const Value: integer); virtual;
    // See @link(LayerDirection).
    procedure SetLayerDirection(const Value: TLayerDirection); virtual;
    // @name sets LocalLineColor to an appropriate value
    // for the layer indicated by LayerIndex.
    procedure SetLayerLineColor(LayerIndex: Integer; LocalEvalAt: TEvaluatedAt;
      var LocalLineColor: TColor32; var LineWidth: single);
    // @name sets LocalEvalAt to an appropriate value based on ViewDirection
    // and the data set used to color the grid.
    procedure SetLocalEvalAt(ViewDirection: TViewDirection;
      var LocalEvalAt: TEvaluatedAt);
    // See @link(RowCount).
    procedure SetRowCount(const Value: integer); virtual;
    // See @link(RowDirection).
    procedure SetRowDirection(const Value: TRowDirection); virtual;
    // @name sets LocalLineColor to an appropriate value for the row indicated
    // by ColIndex.
    procedure SetRowLineColor(RowIndex: Integer; LocalEvalAt: TEvaluatedAt;
      var LocalLineColor: TColor32; var LineWidth: single);
    // See @link(RowPosition).
    procedure SetRowPosition(const Row: integer; const Value: real); virtual;
    // See @link(RowPositions).
    procedure SetRowPositions(const Value: TOneDRealArray);
    // See @link(RowWidth).
    procedure SetRowWidth(const Row: integer; const Value: real); virtual;
    {See @link(SideCellColors).}
    procedure SetSideCellColors(const Row, Layer: integer;
      const Value: TColor); virtual;
    {See @link(TopCellColors).}
    procedure SetTopCellColors(const Column, Row: integer;
      const Value: TColor); virtual;
    // @name is used to store a OpenGL display list for the side
    // view of the grid.
    property SideGridGLIndex: GLuint read FSideGridGLIndex;
    // @name is used to store a OpenGL display list for the horizontal
    // view of the grid.
    property TopGridGLIndex: GLuint read FTopGridGLIndex;
    // @name modifies CellColors to reflect the values of @link(ThreeDDataSet).
    procedure Update3DCellColors(var CellColors: TCellColors);
    procedure ElementCountChanged;
    procedure DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    procedure DrawSideContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
    function IsElementActive(const Layer, Row, Col: integer): boolean;
    procedure SetFrontContourDataSet(const Value: TDataArray); virtual;
    procedure SetSideContourDataSet(const Value: TDataArray); virtual;
    procedure SetThreeDContourDataSet(const Value: TDataArray); virtual;
    procedure SetTopContourDataSet(const Value: TDataArray); virtual;
    // See @link(SideDataSet).
    procedure SetSideDataSet(const Value: TDataArray); virtual;
    // See @link(ThreeDDataSet).
    procedure SetThreeDDataSet(const Value: TDataArray); virtual;
    // See @link(TopDataSet).
    procedure SetTopDataSet(const Value: TDataArray); virtual;
    // See @link(FrontDataSet).
    procedure SetFrontDataSet(const Value: TDataArray); virtual;
    { TODO -cRefactor : Consider replacing ChildModel with an interface. }
    //
    function GetChildDataArray(const Value: TDataArray;
      ChildModel: TBaseModel): TDataArray;
    function GetItemTopLocation(const EvalAt: TEvaluatedAt; const Column,
      Row: integer): TPoint2D; override;
    function GetShortestHorizontalBlockEdge(Layer, Row, Column: Integer): double; override;
  public
    function OkLocation(const DataSet: TDataArray;
      const Layer, Row, Col: integer): boolean; override;
    procedure GetRealMinMax(DataSet: TDataArray; var MinMax: TMinMax);
    procedure GetIntegerMinMax(DataSet: TDataArray; var MinMax: TMinMax);
    procedure GetBooleanMinMax(DataSet: TDataArray; var MinMax: TMinMax);
    procedure GetStringMinMax(DataSet: TDataArray; var MinMax: TMinMax);
    property Drawing3DGrid: boolean read FDrawing3DGrid;
    // @name creates and caches a @link(T2DGrid) which can then be used
    // in drawing contour lines in @link(TContourCreator.DrawContour).
    // The dimensions are based on the dimensions of @link(GlGrid).
    function ContourGrid(EvaluatedAt: TEvaluatedAt;
      ModelSelection: TModelSelection; ViewDirection: TViewDirection;
      ColRowOrLayer: integer): T2DGrid;
    procedure BeginLayerChange;
    procedure EndLayerChange;
    procedure BeginGridChange;
    procedure EndGridChange;
    procedure BeginColumnChange;
    procedure EndColumnChange;
    procedure BeginRowChange;
    procedure EndRowChange;

    { Add a new column boundary at position "Value".}
    procedure AddColumn(const Value: real);
    { Add a new row boundary at position "Value".}
    procedure AddRow(const Value: real);
    {Copies the properties of Source into self.  Only those properties that
     normally would be saved to file are copied.}
    procedure Assign(Source: TPersistent); override;

    // @name is used to determine whether or not a grid can be drawn in 3D.
    function CanDraw3D: boolean;
    property Draw3DAllowed: boolean read FDraw3DAllowed write FDraw3DAllowed;
    { @name is an abstract property that represents the elevation of
      the boundary between the cell at Row, Column between Layer and Layer-1.
      There are LayerCount + 1 layers of elevations in the grid.  }
    property CellElevation[const Column, Row, Layer: integer]: real
      read GetCellElevation write SetCellElevation;
    { @name is an abstract property that represents the thickness of
      a cell.  Thicknesses must be greater than or equal to 0.}
    property CellThickness[const Column, Row, Layer: integer]: real
      read GetCellThickness write SetCellThickness;
    {@name returns the position of the center of a column}
    function ColumnCenter(const Column: integer): real;
    { @name defines either the boundary between one column and its
      neighbor or the position of one edge of the grid.  There are
      @link(ColumnCount)+1 members in ColumnPosition.
      For direct access to the array
      use @link(ColumnPositions) instead. The origin of the grid is at
     (ColumnPosition[0], RowPosition[0]).  }
    property ColumnPosition[const Column: integer]: real
      read GetColumnPosition write SetColumnPosition;
    { @name is the array of boundaries between adjacent columns.
      After editing ColumnPositions, call UpdateColumnPositions to update the
      number of columns and to make sure the columns are sorted in ascending
      order.}
    property ColumnPositions: TOneDRealArray read FColumnPositions
      write SetColumnPositions;
    { @name is the width of a column. ColumnWidth must be greater than
      or equal to 0. }
    property ColumnWidth[const Column: integer]: real read GetColumnWidth
      write SetColumnWidth;
    // @name represents an array of column widths.
    property ColWidths: TOneDRealArray read GetColWidths;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates a grid with 0 rows, columns and layers.
    constructor Create(Model: TBaseModel);
    // @name deletes the column boundary at the position AColumn.
    procedure DeleteColumn(const AColumn: integer);
    // @name deletes the row boundary at the position ARow.
    procedure DeleteRow(const ARow: integer);
    // @name gets rid of OpenGL display lists.
    destructor Destroy; override;
    {@name draws a top, front or side view of the grid on BitMap.
    ViewDirection determines which one is drawn.  @SeeAlso(DrawTop)
    @SeeAlso(DrawFront)  @SeeAlso(DrawSide)}
    procedure Draw(const BitMap: TPersistent;
      const ViewDirection: TViewDirection);
    { @name draws the grid in the current OpenGL rendering context.}
    procedure Draw3D; virtual;
    // If @name is @true, the grid will be displayed with (1) all it's grid
    // lines in 2D views of the model (2) only
    // the first, last and selected grid lines, or (3) only grid lines
    // next to active elements.
    property GridLineDrawingChoice: TGridLineDrawingChoice
      read FGridLineDrawingChoice write SetGridLineDrawingChoice;
    {@name is used to retrieve or set the color of a element or
     node in a front view of the grid.  Whether the color is the
     color of a cell
     or a node depends on whether the @link(FrontDataSet) is evaluated at
     elements or nodes.}
    property FrontCellColors[const Column, Layer: integer]: TColor
      read GetFrontCellColors write SetFrontCellColors;
    {@name is the  @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a front view of the grid.  See: @link(FrontCellColors),
     TPhastModel.@link(TCustomModel.FrontDisplayTime), and
     TPhastModel.@link(TCustomModel.FrontTimeList).}
    property FrontDataSet: TDataArray read GetFrontDataSet write SetFrontDataSet;
    property FrontContourDataSet: TDataArray read GetFrontContourDataSet
      write SetFrontContourDataSet;
    // @name returns the column that contains AnXPosition.
    // See @link(GetContainingColumnOrRow).
    function GetContainingColumn(const AnXPosition: real): integer;
    // @name returns the layer at (ACol, ARow) that contains AZPosition.
    function GetContainingLayer(ACol, ARow: integer;
      const AZPosition: real): integer; virtual; abstract;
    // @name returns the row that contains AYPosition.
    // See @link(GetContainingColumnOrRow).
    function GetContainingRow(const AYPosition: real): integer;
    // @name returns the limits of one of the sparse 2D elevation
    // arrays in a @link(TScreenObject) in Limit1 and Limit2.
    // Depending on ViewDirection. Limit1 and Limit2 will be
    // @unorderedList(
    //   @item(Column and Row limits for top view)
    //   @item(Column and Layer limits for front view)
    //   @item(Row and Layer limits for side view)
    // )
    procedure GetLimits(const EvaluatedAt: TEvaluatedAt;
      const ViewDirection: TViewDirection; out Limit1, Limit2: integer);
    // @name is called when the grid has changed in a way that means it needs
    // to be redrawn.  It notifies the
    // appropriate controls to redraw themselves.
    procedure GridChanged;
    // @name returns the highest elevation in the grid.
    // @seealso(LowestElevation)
    function HighestElevation: real; virtual; abstract;
    // @name sets display @link(TDataArray)s
    // such as @link(TopDataSet), @link(FrontDataSet) to nil.
    // Resets @link(DisplayLayer) @link(DisplayRow) and @link(DisplayColumn)
    // to 0 if they are less than zero.
    procedure Initialize;
    // @name returns the lowest elevation in the grid.
    // @seealso(HighestElevation)
    function LowestElevation: real; virtual; abstract;
    // @name returns the index of the layer boundary in (Col, Row)
    // that is closest to APosition.
    function Nearest2DCellElevation(const Col, Row: integer;
      const APosition: real; const First: integer = -1;
      const Last: integer = -1): integer;
    { @name returns the index of the column center closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestColumnCenter(const APosition: real;
      First: integer = -1; Last: integer = -1): integer;
    { @name returns the index of the column boundary closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestColumnPosition(const APosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    { @name returns the index of the row center closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestRowCenter(const APosition: real;
      First: integer = -1; Last: integer = -1): integer;
    { @name returns the index of the row boundary closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestRowPosition(const APosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    { @name indicates that the colors of the cells
      in the 3D view of the grid need to be recalculated.
      @link(FNeedToRecalculate3DCellColors) is a protected field.}
    property NeedToRecalculate3DCellColors: boolean read
      FNeedToRecalculate3DCellColors write SetNeedToRecalculate3DCellColors;
    { @name sets @link(NeedToRecalculateFrontCellColors),
      @link(NeedToRecalculateSideCellColors),
      @link(NeedToRecalculateTopCellColors), and
      @link(NeedToRecalculate3DCellColors) to false.}
    procedure NeedToRecalculateCellColors;
    { @name indicates that the colors of the cells
      in the front view of the grid need to be recalculated.
      NeedToRecalculateFrontCellColors can be used to set
      @link(FNeedToRecalculateFrontCellColors) to false but not to true.
      @link(FNeedToRecalculateFrontCellColors) is a protected field.}
    property NeedToRecalculateFrontCellColors: boolean
      read FNeedToRecalculateFrontCellColors write
      SetNeedToRecalculateFrontCellColors;
    { @name indicates that the colors of the cells
      in the side view of the grid need to be recalculated.
      NeedToRecalculateSideCellColors can be used to set
      @link(FNeedToRecalculateSideCellColors) to false but not to true.
      @link(FNeedToRecalculateSideCellColors) is a protected field.}
    property NeedToRecalculateSideCellColors: boolean
      read FNeedToRecalculateSideCellColors write
      SetNeedToRecalculateSideCellColors;
    { @name indicates that the colors of the cells
      in the top view of the grid need to be recalculated.
      NeedToRecalculateTopCellColors can be used to set
      @link(FNeedToRecalculateTopCellColors) to false but not to true.
      @link(FNeedToRecalculateTopCellColors) is a protected field.}
    property NeedToRecalculateTopCellColors: boolean
      read FNeedToRecalculateTopCellColors write
      SetNeedToRecalculateTopCellColors;
    { @name is an event that can be used to respond to changes
      in which column is selected.}
    property OnSelectedColumnChange: TNotifyEvent read FOnSelectedColumnChange
      write SetOnSelectedColumnChange;
    { @name is an event that can be used to respond to changes
      in which layer is selected.}
    property OnSelectedLayerChange: TNotifyEvent read FOnSelectedLayerChange
      write SetOnSelectedLayerChange;
    { @name is an event that can be used to respond to changes
      in which row is selected.}
    property OnSelectedRowChange: TNotifyEvent read FOnSelectedRowChange write
      SetOnSelectedRowChange;
    {@name sets the colors of the cells in a front view of the grid to white.
     See @link(UpdateCellColors).}
    procedure ResetFrontCellColors;
    {Sets the colors of the cells in a side view of the grid to white.
     See @link(UpdateCellColors).}
    procedure ResetSideCellColors;
    {Sets the colors of the cells in a top view of the grid to white.
     See @link(UpdateCellColors).}
    procedure ResetTopCellColors;
     { TODO : Change "RealWorld" to "Global" to match the user documentation. }
    { @name converts coordinates
      expressed in terms of Column and Row coordinates to ones based on real
      world coordinates.  }
    function RotateFromGridCoordinatesToRealWorldCoordinates
      (const APoint: TPoint2D): TPoint2D;  overload;
    procedure RotateFromGridCoordinatesToRealWorldCoordinates(var X, Y: TFloat); overload;
    { @name is the inverse of
      @link(RotateFromGridCoordinatesToRealWorldCoordinates).}
    function RotateFromRealWorldCoordinatesToGridCoordinates
      (const APoint: TPoint2D): TPoint2D; overload;
    procedure RotateFromRealWorldCoordinatesToGridCoordinates
      (var X, Y: TFloat); overload;
    {@name returns the position of the center of a row}
    function RowCenter(const Row: integer): real;
    { @name defines either the boundary between one row and its
      neighbor or the position of one edge of the grid.  There are
      RowCount+1 members in @name.  For direct access to the array
      use @link(RowPositions) instead. The origin of the grid is at
      (ColumnPosition[0], RowPosition[0]).}
    property RowPosition[const Row: integer]: real read GetRowPosition
      write SetRowPosition;
    { @name is the array of boundaries between adjacent rows.
      After editing @name, call @link(UpdateRowPositions) to update the
      number of rows and to make sure the rows are sorted in ascending
      order.}
    property RowPositions: TOneDRealArray read FRowPositions
      write SetRowPositions;
    { @name is the width of a row. RowWidth must be greater than
      or equal to 0. }
    property RowWidth[const Row: integer]: real read GetRowWidth
      write SetRowWidth;
    // @name represents an array of rows widths.
    property RowWidths: TOneDRealArray read GetRowWidths;
    {@name is used to retrieve or set the color of a element or
     node in a side view of the grid.  Whether the color is the color of a cell
     or a node depends on whether the @link(SideDataSet) is evaluated at
     elements or nodes.}
    property SideCellColors[const Row, Layer: integer]: TColor
      read GetSideCellColors write SetSideCellColors;
    {@name is the  @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a side view of the grid.  See: @link(SideCellColors),
     TPhastModel.@link(TCustomModel.SideDisplayTime), and
     TPhastModel.@link(TCustomModel.SideTimeList).}
    property SideDataSet: TDataArray read FSideDataSet write SetSideDataSet;
    property SideContourDataSet: TDataArray read FSideContourDataSet
      write SetSideContourDataSet;
    { @name returns the X, Y, and Z coordinates of the
      center of a column boundary in the coordinate system of the grid.
      @SeeAlso(ThreeDRowEdgeCenter) @SeeAlso(ThreeDLayerEdgeCenter)}
    function ThreeDColumnEdgeCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    {@name is the @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a 3D view of the grid.  See: @link(TCellColors),
     TPhastModel.@link(TCustomModel.ThreeDDisplayTime), and
     TPhastModel.@link(TCustomModel.ThreeDTimeList).}
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property ThreeDContourDataSet: TDataArray read FThreeDContourDataSet
      write SetThreeDContourDataSet;
    { @name returns the X, Y, and Z coordinates of the center of
      a grid element in the coordinate system of the grid.}
    function ThreeDElementCenter(const Column, Row, Layer: integer):
      T3DRealPoint; virtual;
    function RotatedThreeDElementCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    { @name returns the X, Y, and Z coordinates of a corner of
      a grid element in the coordinate system of the grid.}
    function ThreeDElementCorner(const Column, Row, Layer: integer):
      T3DRealPoint; virtual;
    function RotatedThreeDElementCorner(const Column, Row, Layer: integer):
      T3DRealPoint;
    function ThreeDCellCorner(Column, Row, Layer: integer):
      T3DRealPoint; virtual;
    // @name is used to notify @link(TScreenObject)s and @link(TDataArray)s
    // That they need to redraw themselves due to a change in the columns,
    //  rows. or layers.
    property ThreeDGridObserver: TObserver read GetThreeDGridObserver
      write SetThreeDGridObserver;
    { @name returns the X, Y, and Z coordinates of the
      center of a layer boundary in the coordinate system of the grid.
      @SeeAlso(ThreeDColumnEdgeCenter) @SeeAlso(ThreeDRowEdgeCenter)}
    function ThreeDLayerEdgeCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    { @name returns the X, Y, and Z coordinates of the
      center of a row boundary in the coordinate system of the grid.
      @SeeAlso(ThreeDColumnEdgeCenter) @SeeAlso(ThreeDLayerEdgeCenter)}
    function ThreeDRowEdgeCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    {@name is used to retrieve or set the color of a element or
     node in a top view of the grid.  Whether the color is the color of a cell
     or a node depends on whether the @link(TopDataSet) is evaluated at
     elements or nodes.}
    property TopCellColors[const Column, Row: integer]: TColor
      read GetTopCellColors write SetTopCellColors;
    // @name represents a one-dimensional array of values representing the
    // top and bottom elevations of the elements at (Col,Row).
    property  TwoDCellElevations[const Col, Row: integer]: TOneDRealArray
      read GetTwoDCellElevations;
    { @name retrieves the indices of the element or node that
      contains APoint.  Whether the indices of an element or node are
      retrieved depends on EvaluatedAt.  If APoint is in grid coordinates,
      NeedToRotatePointToGridCoordinates should be set to False.  If the
      result is known to be within a certain range, the function can be
      speeded up by specifying BelowCol, AboveCol, BelowRow, and AboveRow.
      The T2DTopCell that is returned should never have values outside the
      valid range as long as there are at least one column and row in the
      grid. @name will return a cell inside the grid even if the point is
      outside the grid.}
    function TopContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt;
      const NeedToRotatePointToGridCoordinates: boolean = True;
      const BelowCol: integer = -1; const AboveCol: integer = -1;
      const BelowRow: integer = -1; const AboveRow: integer = -1): T2DTopCell;
    // @name is like @link(TopContainingCell) except that the cell may be
    // outside the grid
    function UnconstrainedTopContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt;
      const NeedToRotatePointToGridCoordinates: boolean = True;
      const BelowCol: integer = -1; const AboveCol: integer = -1;
      const BelowRow: integer = -1; const AboveRow: integer = -1): T2DTopCell;
    {@name is the  @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a top view of the grid.  See: @link(TopCellColors),
     TPhastModel.@link(TCustomModel.TopDisplayTime), and
     TPhastModel.@link(TCustomModel.TopTimeList).}
    property TopDataSet: TDataArray read FTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read FTopContourDataSet
      write SetTopContourDataSet;
    // @name is used to notify @link(TScreenObject)s and @link(TDataArray)s
    // That they need to redraw themselves due to a change in the columns
    // or rows.
    property TopGridObserver: TObserver read FTopGridObserver
      write SetTopGridObserver;
    { TwoDCellCorner returns the X ,and Y coordinates of a corner of
      a grid cell in the real-world coordinates.}
    function TwoDCellCorner(const Column, Row: integer): TPoint2D;
    { @name returns the X ,and Y coordinates of the center
      of the edge of a column in the real-world coordinates.}
    function TwoDColumnEdgeCenter(const Column, Row: integer): TPoint2D;
    { @name returns the X, and Y coordinates of the center of
      a grid element in the real-world coordinates.}
    function TwoDElementCenter(const Column, Row: integer): TPoint2D;
    function UnrotatedTwoDElementCenter(const Column, Row: integer): TPoint2D;
    { TwoDCellCorner returns the X ,and Y coordinates of a corner of
      a grid element in the real-world coordinates.}
    function TwoDElementCorner(const Column, Row: integer): TPoint2D;
    function UnrotatedTwoDElementCorner(const Column, Row: integer): TPoint2D;
    { @name returns the X ,and Y coordinates of the center
      of the edge of a row in the real-world coordinates.}
    function TwoDRowEdgeCenter(const Column, Row: integer): TPoint2D;
    {When the values in the displayed @link(TopDataSet), @link(FrontDataSet),
     or @link(SideDataSet) are up-to-date, call @name to update
     colors of the grid cells.}
    procedure UpdateCellColors(const ViewDirection: TViewDirection);
    { After editing @link(ColumnPositions), call @name to update the
      number of columns and to make sure the columns are sorted in ascending
      order.}
    procedure UpdateColumnPositions; virtual;
    { After editing @link(RowPositions), call @name to update the
      number of rows and to make sure the rows are sorted in ascending
      order.}
    procedure UpdateRowPositions; virtual; // update this.
    // The result of @name is used in drawing @link(TScreenObject)s in
    // the 3D view. The dimensions are
    // ColumnCount + 2, RowCount + 2, LayerCount + 2
    // if EvaluatedAt = eaBlocks and
    // ColumnCount + 3, RowCount + 3, LayerCount + 3
    // if EvaluatedAt = eaNodes.
    function GlGrid(EvaluatedAt: TEvaluatedAt;
      ModelSelection: TModelSelection): TGrid;
    {@name returns the elevation of the center of an element.}
    function LayerCenter(Column, Row, Layer: integer): real;
    // @name returns and elevation 5% of the layer thickness below the layer
    // top.
    function NearLayerTop(Column, Row, Layer: integer): real;
    // @name returns the extent of the grid in grid coordinates;
    function GridLimits(ViewDirection: TViewDirection): TGridLimit;
    function GridOutline(ViewDirection: TViewDirection): TPolygon2D;
    property DrawColoredGridLines: boolean read FDrawColoredGridLines
      write FDrawColoredGridLines;
    // @name returns the coordinates of a cell surrounding a node
    property CellCoordinates[Col, Row, Layer: integer]: T3DCellCoordinates
      read GetCellCoordinates;
    property ElementCoordinates[Col, Row, Layer: integer]: T3DElementCoordinates
      read GetElementCoordinates;
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList; out MinMaxInitialized: Boolean); override;
    property GridAngleDegrees: double read GetGridAngleDegrees;
    property ElevationsNeedUpdating: Boolean read GetElevationsNeedUpdating;
    property TopElementOutline[Row: Integer; Column: Integer]: TElementOutline
      read GetTopElementOutline;
    // NewAngle is in radians
    procedure RotateAroundGridOrigin(NewAngle: Double);
    // Layer, Row, Col are the indicies of an element.
    // CellList will be filled with the horizontal neighbors of that element.
    procedure GetHorizontalNeighbors(const Layer, Row, Col: integer;
      CellList: TCellLocationList);
    function UniformColumns(out MaxWidth, MinWidth: Double): boolean;
    function UniformRows(out MaxWidth, MinWidth: Double): boolean;
    function InsideGrid(APoint: TPoint2D; NeedToRotatePointToGridCoordinates: Boolean = True): Boolean;
  published
    { @name is the number of columns in the grid.
      Each column has a width that is greater than or equal to 0.}
    property ColumnCount: integer read FColumnCount write SetColumnCount;
    { @name determines whether columns are numbered
      from west to east or east to west.}
    property ColumnDirection: TColumnDirection read FColumnDirection
      write SetColumnDirection;
    { @name is the angle made by the grid relative to the East direction.
      it is measured counterclockwise.  If @name is 0, columns run
      north/south and rows run east/west. @name is measured in radians.}
    property GridAngle: real read FGridAngle write SetGridAngle;
    { @name is the number of layers in the grid.  Each element in the grid
      has a thickness that is greater than or equal to 0.}
    property LayerCount: integer read FLayerCount write SetLayerCount;
    { @name determines whether layers are numbered from top to bottom
      or bottom to top.}
    property LayerDirection: TLayerDirection read FLayerDirection
      write SetLayerDirection;
    { @name is the number of rows in the grid.  Each row has a
      width that is greater than or equal to 0.}
    property RowCount: integer read FRowCount write SetRowCount;
    { @name determines whether rows are numbered from north to south
      or south to north.}
    property RowDirection: TRowDirection read FRowDirection
      write SetRowDirection;
    {@name is the column that is displayed in the side view
     of the grid.  @SeeAlso(SelectedRow) @SeeAlso(SelectedLayer)}
    property SelectedColumn: integer read FSelectedColumn write
      SetSelectedColumn stored True;
    {@name is the layer that is displayed in the top view
     of the grid.  @SeeAlso(SelectedColumn) @SeeAlso(SelectedRow)}
    property SelectedLayer: integer read FSelectedLayer write SetSelectedLayer stored True;
    {@name is the row that is displayed in the front view
     of the grid.  @SeeAlso(SelectedColumn) @SeeAlso(SelectedLayer)}
    property SelectedRow: integer read FSelectedRow write SetSelectedRow stored True;
    // @name is the grid column that should be displayed on the 3D view.
    property DisplayColumn: integer read FDisplayColumn write SetDisplayColumn stored True;
    // @name is the grid row that should be displayed on the 3D view.
    property DisplayRow: integer read FDisplayRow write SetDisplayRow stored True;
    // @name is the grid layer that should be displayed on the 3D view.
    property DisplayLayer: integer read FDisplayLayer write SetDisplayLayer stored True;
  end;

  TCustomMesh = class abstract(TCustomDiscretization)
  protected
    FUpdateCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CalculateMinMax(DataSet: TDataArray;
      var MinMaxInitialized: Boolean; var MinMax: TMinMax;
      StringValues: TStringList); override;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
      virtual; stdcall;
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList; out MinMaxInitialized: Boolean); override;
  end;

{
   @name calls @link(TColorParameters.FracToColor
   frmGoPhast.PhastModel.GridColors.FracToColor(Fraction);)
}
function GridFracToRainbow(const Fraction: real): TColor;

{@name is used to read a one-dimensional array of real numbers
 from a stream. See @link(TCustomModelGrid.DefineProperties)}
procedure ReadRealArray(const Reader: TReader;
  var Positions: TOneDRealArray; const Count: integer);

{@name is used to write a one-dimensional array of real numbers
 to a stream. See @link(TCustomModelGrid.DefineProperties)}
procedure WriteRealArray(const Writer: TWriter;
  const Positions: TOneDRealArray);

{@name is used to read a one-dimensional array of integers
 from a stream. See @link(TCustomModelGrid.DefineProperties)}
procedure ReadIntegerArray(const Reader: TReader;
  var Positions: TOneDIntegerArray; const Count: integer);

{@name is used to write a one-dimensional array of integers
 to a stream. See @link(TCustomModelGrid.DefineProperties)}
procedure WriteIntegerArray(const Writer: TWriter;
  const Positions: TOneDIntegerArray);

// @name returns @True if all members of AnArray are the same.
function IsUniform(const AnArray: TOneDRealArray): boolean;

function ConvertTop2D_Point(const ZoomBox: TQRbwZoomBox2;
  const APoint: TPoint2D): TPoint;

function Convert2D_FrontPoint(const ZoomBox: TQRbwZoomBox2;
  const APoint: TPoint2D): TPoint;

function Convert2D_SidePoint(const ZoomBox: TQRbwZoomBox2;
  const APoint: TPoint2D): TPoint;

const
  // @name is the thickness of thin grid lines when drawn in the top, front,
  // or side views.
  OrdinaryGridLineThickness = 1.0;
  // @name is the thickness of thick grid lines when drawn in the top, front,
  // or side views.
  ThickGridLineThickness = 2.85;

const
  // @name is the thickness of thick grid lines when drawn in the 3D view.
  ThickLine = 2.0;
  // @name is the thickness of thin grid lines when drawn in the 3D view.
  ThinLine = 1.0;

var
  // @name is the color used for the selected column
  // (@link(TCustomModelGrid.SelectedColumn)).
  ExistingColumnSelectionCellColor: TColor;
  // @name is the color used for the selected row
  // (@link(TCustomModelGrid.SelectedRow)).
  ExistingRowSelectionCellColor: TColor;
  // @name is the color used for the selected layer
  // (@link(TCustomModelGrid.SelectedLayer)).
  ExistingLayerSelectionCellColor: TColor;

resourcestring
  StrInvalidLayerNumber = 'Invalid layer number: %d';

const
  InactiveGridColor = clSilver;

implementation

uses GR32_Polygons, Math, RealListUnit, frmGoPhastUnit, BigCanvasMethods,
  ModelMuseUtilities, PhastDataSets, PhastModelUnit, InteractiveTools,
  EdgeDisplayUnit, ZLib, IntListUnit, PathlineReader, ScreenObjectUnit,
  Vcl.Dialogs;

resourcestring
  StrInvalidColumnNumbe = 'Invalid column number: %d';
  StrInvalidRowNumber = 'Invalid row number: %d';
  StrColumnWidthsMustB = 'Column widths must be greater than or equal to 0.';
  StrRowWidthsMustBeG = 'Row widths must be greater than or equal to 0.';
  StrTheNumberOfCells = 'The number of cells in the grid must not exceed %d.';

const
  // @name is the number of OpenGL display lists used to display the grid.
  NumberOfLists = 6;

function IsUniform(const AnArray: TOneDRealArray): boolean;
var
  Size: double;
  Index: integer;
begin
  Assert(Length(AnArray) >= 1);
  Size := AnArray[0];
  result := True;
  for Index := 1 to Length(AnArray) - 1 do
  begin
    result := AnArray[Index] = Size;
    if not result then
    begin
      Exit;
    end;
  end;
end;


procedure ReadRealArray(const Reader: TReader;
  var Positions: TOneDRealArray; const Count: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
//  with Reader do
  begin
    if Count <= 1 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      Reader.ReadListBegin;
      Index := 0;
      while not Reader.EndOfList do
      begin
        Positions[Index] := Reader.ReadFloat;
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      Reader.ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count);
      Reader.ReadListBegin;
      for Index := 0 to Count - 1 do
      begin
        Positions[Index] := Reader.ReadFloat;
      end;
      Reader.ReadListEnd;
    end;
  end;
end;

procedure WriteRealArray(const Writer: TWriter;
  const Positions: TOneDRealArray);
var
  Count: integer;
  Index: integer;
begin
//  with Writer do
  begin
    Count := Length(Positions);
    Writer.WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      Writer.WriteFloat(Positions[Index]);
    end;
    Writer.WriteListEnd;
  end;
end;

procedure ReadIntegerArray(const Reader: TReader;
  var Positions: TOneDIntegerArray; const Count: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
//  with Reader do
  begin
    if Count = 0 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      Reader.ReadListBegin;
      Index := 0;
      while not Reader.EndOfList do
      begin
        Positions[Index] := Reader.ReadInteger;
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      Reader.ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count);
      Reader.ReadListBegin;
      for Index := 0 to Count - 1 do
      begin
        if Reader.EndOfList then
        begin
          SetLength(Positions, Index);
          break;
        end;
        Positions[Index] := Reader.ReadInteger;
      end;
      Reader.ReadListEnd;
    end;
  end;
end;

procedure WriteIntegerArray(const Writer: TWriter;
  const Positions: TOneDIntegerArray);
var
  Count: integer;
  Index: integer;
begin
//  with Writer do
  begin
    Count := Length(Positions);
    Writer.WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      Writer.WriteInteger(Positions[Index]);
    end;
    Writer.WriteListEnd;
  end;
end;

function GridFracToRainbow(const Fraction: real): TColor;
begin
  result := frmGoPhast.PhastModel.GridColorParameters.FracToColor(Fraction);
end;

{ TCustomModelGrid }

procedure TCustomModelGrid.AddColumn(const Value: real);
begin
  SetLength(FColumnPositions, Length(FColumnPositions) + 1);
  FColumnPositions[Length(FColumnPositions) - 1] := Value;
  UpdateColumnPositions;
  InvalidateScreenObjects;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomModelGrid.AddRow(const Value: real);
begin
  SetLength(FRowPositions, Length(FRowPositions) + 1);
  FRowPositions[Length(FRowPositions) - 1] := Value;
  UpdateRowPositions;
  InvalidateScreenObjects;
  GridChanged;
  RowsChanged;
end;

constructor TCustomModelGrid.Create(Model: TBaseModel);
begin
  inherited Create(Model);
//  FModel := Model;
  FDrawColoredGridLines := True;
  FDraw3DAllowed := True;
  FGridLineDrawingChoice := gldcAll;
  FElementCount := 0;
  FNeedToRedraw3d := True;
  FRecordedSideGrid := False;
  FRecordedFrontGrid := False;
  FRecordedTopGrid := False;
  FRecordedShell := False;
  FRecordedSideGrid := False;
  FRecordedFrontGrid := False;
  FRecordedTopGrid := False;
  FGridAngle := 0;
  FColumnCount := -1;
  FLayerCount := -1;
  FRowCount := -1;
  SetLength(FColumnPositions, 0);
  SetLength(FRowPositions, 0);
  FCanDraw := True;
end;

procedure TCustomModelGrid.DeleteColumn(const AColumn: integer);
var
  Index: integer;
begin
  if (AColumn < 0) or (AColumn >= Length(FColumnPositions)) then
  begin
    Exit;
  end;
  for Index := AColumn + 1 to Length(FColumnPositions) - 1 do
  begin
    FColumnPositions[Index - 1] := FColumnPositions[Index];
  end;
  ColumnCount := ColumnCount - 1;
  frmGoPhast.PhastModel.InvalidateSegments;
  InvalidateScreenObjects;
  frmGoPhast.InvalidateModel;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomModelGrid.DeleteRow(const ARow: integer);
var
  Index: integer;
begin
  if (ARow < 0) or (ARow >= Length(FRowPositions)) then
  begin
    Exit;
  end;
  for Index := ARow + 1 to Length(FRowPositions) - 1 do
  begin
    FRowPositions[Index - 1] := FRowPositions[Index];
  end;
  RowCount := RowCount - 1;
  frmGoPhast.PhastModel.InvalidateSegments;
  InvalidateScreenObjects;
  frmGoPhast.InvalidateModel;
  GridChanged;
  RowsChanged;
end;

procedure TCustomModelGrid.Draw(const BitMap: TPersistent;
  const ViewDirection: TViewDirection);
begin
  if (not CanDraw) or ((Model as TCustomModel).LayerCount = 0)
    or (BitMap = nil) then
  begin
    Exit;
  end;

  // Force update of cell elevations.
//  HighestElevation;
  case ViewDirection of
    vdTop:
      begin
        DrawTop(BitMap, frmGoPhast.frameTopView.ZoomBox);
      end;
    vdFront:
      begin
        DrawFront(BitMap, frmGoPhast.frameFrontView.ZoomBox);
      end;
    vdSide:
      begin
        DrawSide(BitMap, frmGoPhast.frameSideView.ZoomBox);
      end;
  else
    Assert(False);
  end;
end;

function ConvertTop2D_Point(const ZoomBox: TQRbwZoomBox2;
  const APoint: TPoint2D): TPoint;
begin
  result.X := ZoomBox.XCoord(APoint.X);
  result.Y := ZoomBox.YCoord(APoint.Y);
end;

function Convert2D_FrontPoint(const ZoomBox: TQRbwZoomBox2;
  const APoint: TPoint2D): TPoint;
begin
  result.X := ZoomBox.XCoord(APoint.X);
  result.Y := ZoomBox.YCoord(APoint.Y);
end;

function Convert2D_SidePoint(const ZoomBox: TQRbwZoomBox2;
  const APoint: TPoint2D): TPoint;
begin
  result.X := ZoomBox.XCoord(APoint.Y);
  result.Y := ZoomBox.YCoord(APoint.X);
end;

procedure TCustomModelGrid.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  RowIndex, ColumnIndex: integer;
  Point1, Point2, Point3, Point4: TPoint2D;
  AColor, NewColor: TColor;
  PriorRow: integer;
  BoxCoordinates: TPointArray;
  TempValue: double;
  PointCount: integer;
  ColumnLimit, RowLimit: integer;
  LineWidth: single;
  Polygon: TPolygon32;
  MultiplePolygons: boolean;
  LocalModel: TCustomModel;
  procedure CalculatePoint1;
  var
    X1: double;
    Y1: double;
    X2: double;
    Y2: double;
    Angle: double;
  begin
    Angle := GridAngle;
    X1 := Cos(Angle) * ColumnPosition[ColumnIndex];
    Y1 := Sin(Angle) * ColumnPosition[ColumnIndex];
    Y2 := TempValue - Y1;
    try
      if (Angle = Pi / 2) or (Angle = -Pi / 2) then
      begin
        X2 := 0;
      end
      else
      begin
        X2 := Tan(Angle) * Y2;
      end;
    except
      X2 := 0;
    end;
    Point1.X := X1 - X2;
    Point1.Y := TempValue;
  end;
  procedure CalculatePoint3;
  var
    X1: double;
    Y1: double;
    X2: double;
    Y2: double;
    Angle: double;
  begin
    Angle := GridAngle;
    X1 := Sin(Angle) * RowPosition[RowIndex];
    Y1 := Cos(Angle) * RowPosition[RowIndex];
    X2 := X1 + TempValue;
    try
      if (Angle = Pi / 2) or (Angle = -Pi / 2) then
      begin
        Y2 := 0;
      end
      else
      begin
        Y2 := Tan(Angle) * X2;
      end;
    except
      Y2 := 0;
    end;
    Point1.X := TempValue;
    Point1.Y := Y1 + Y2;
  end;
  function GetNodeBasedCorner(Col, Row: integer): TPoint2D;
  begin
    if (Col = 0) or (Col > ColumnLimit) then
    begin
      if Col > ColumnLimit then
      begin
        Dec(Col);
      end;
      // column center or cell center
      if (Row = 0) or (Row > RowLimit) then
      begin
        if (Row > RowLimit) then
        begin
          Dec(Row);
        end;
        // cell center
        result := TwoDElementCorner(Col, Row);
      end
      else
      begin
        // column center
        result := TwoDColumnEdgeCenter(Col, Row - 1);
      end;
    end
    else
    begin
      // row center or cell center
      if (Row = 0) or (Row > RowLimit) then
      begin
        // row center
        if (Row > RowLimit) then
        begin
          Dec(Row);
        end;
        result := TwoDRowEdgeCenter(Col - 1, Row);
      end
      else
      begin
        // cell center
        result := TwoDElementCenter(Col - 1, Row - 1);
      end;
    end;
  end;
begin
  Polygon := nil;
  MultiplePolygons := False;
  SetLength(BoxCoordinates, 4);
//  with ZoomBox do
  begin
    if (ColumnCount <= 0)
      or (RowCount <= 0)
      or (LayerCount <= 0) then
    begin
      LineWidth := OrdinaryGridLineThickness;
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (GridLineDrawingChoice = gldcExterior) and (ColumnIndex <> 0)
          and (ColumnIndex <> ColumnCount) then
        begin
          Continue;
        end;

        PointCount := 0;

        TempValue := ZoomBox.Y(ZoomBox.Image32.Height);
        CalculatePoint1;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(ZoomBox, Point1);
        Inc(PointCount);

        TempValue := ZoomBox.Y(0);
        CalculatePoint1;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(ZoomBox, Point1);
        //Inc(PointCount);

        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;

      for RowIndex := 0 to RowCount do
      begin
        if (GridLineDrawingChoice = gldcExterior) and (RowIndex <> 0)
          and (RowIndex <> RowCount) then
        begin
          Continue;
        end;
        PointCount := 0;

        TempValue := ZoomBox.X(ZoomBox.Image32.Width);
        CalculatePoint3;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(ZoomBox, Point1);
        Inc(PointCount);

        TempValue := ZoomBox.X(0);
        CalculatePoint3;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(ZoomBox, Point1);
        //Inc(PointCount);

        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;
    end
    else
    begin
      if TopDataSet <> nil then
      begin
        case TopDataSet.EvaluatedAt of
          eaBlocks:
            begin
              ColumnLimit := ColumnCount - 1;
              RowLimit := RowCount - 1;
            end;
          eaNodes:
            begin
              ColumnLimit := ColumnCount;
              RowLimit := RowCount;
            end;
        else
          begin
            Assert(False);
            ColumnLimit := -1;
            RowLimit := -1;
          end;
        end;

        for ColumnIndex := 0 to ColumnLimit do
        begin
            PriorRow := 0;
            AColor := TopCellColors[ColumnIndex, 0];
            NewColor := AColor;
            for RowIndex := 1 to RowLimit do
            begin
              NewColor := TopCellColors[ColumnIndex, RowIndex];
              if (NewColor <> AColor) then
              begin
                if AColor <> clWhite then
                begin
                  case TopDataSet.EvaluatedAt of
                    eaBlocks:
                      begin
                        Point1 := TwoDElementCorner(ColumnIndex, PriorRow);
                        Point2 := TwoDElementCorner(ColumnIndex + 1, PriorRow);
                        Point3 := TwoDElementCorner(ColumnIndex + 1, RowIndex);
                        Point4 := TwoDElementCorner(ColumnIndex, RowIndex);
                      end;
                    eaNodes:
                      begin
                        Point1 := GetNodeBasedCorner(ColumnIndex, PriorRow);
                        Point2 := GetNodeBasedCorner(ColumnIndex + 1, PriorRow);
                        Point3 := GetNodeBasedCorner(ColumnIndex + 1, RowIndex);
                        Point4 := GetNodeBasedCorner(ColumnIndex, RowIndex);
                      end;
                  else
                    Assert(False);
                  end;
                  BoxCoordinates[0] := ConvertTop2D_Point(ZoomBox, Point1);
                  BoxCoordinates[1] := ConvertTop2D_Point(ZoomBox, Point2);
                  BoxCoordinates[2] := ConvertTop2D_Point(ZoomBox, Point3);
                  BoxCoordinates[3] := ConvertTop2D_Point(ZoomBox, Point4);
                  DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                    0, BoxCoordinates, Polygon, MultiplePolygons, True);
                end;
                AColor := NewColor;
                PriorRow := RowIndex;

              end;
            end;

            if NewColor <> clWhite then
            begin
              case TopDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    Point1 := TwoDElementCorner(ColumnIndex, PriorRow);
                    Point2 := TwoDElementCorner(ColumnIndex + 1, PriorRow);
                    Point3 := TwoDElementCorner(ColumnIndex + 1, RowCount);
                    Point4 := TwoDElementCorner(ColumnIndex, RowCount);
                  end;
                eaNodes:
                  begin
                    Point1 := GetNodeBasedCorner(ColumnIndex, PriorRow);
                    Point2 := GetNodeBasedCorner(ColumnIndex + 1, PriorRow);
                    Point3 := GetNodeBasedCorner(ColumnIndex + 1, RowCount + 1);
                    Point4 := GetNodeBasedCorner(ColumnIndex, RowCount + 1);
                  end;
              else
                Assert(False);
              end;
              BoxCoordinates[0] := ConvertTop2D_Point(ZoomBox, Point1);
              BoxCoordinates[1] := ConvertTop2D_Point(ZoomBox, Point2);
              BoxCoordinates[2] := ConvertTop2D_Point(ZoomBox, Point3);
              BoxCoordinates[3] := ConvertTop2D_Point(ZoomBox, Point4);
              DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
                0, BoxCoordinates, Polygon, MultiplePolygons, True);
            end;
//          end;
        end;
      end;
      DrawOrdinaryTopColumns(BitMap, ZoomBox);
      DrawOrdinaryTopRows(BitMap, ZoomBox);

    end;
  end;
  LocalModel := Model as TCustomModel;
  if LocalModel.EdgeDisplay <> nil then
  begin
    LocalModel.EdgeDisplay.Draw(SelectedLayer, BitMap);
  end;
  DrawTopContours(ZoomBox, BitMap);
end;

procedure TCustomModelGrid.ElementCountChanged;
const
  BigGrid = 100000;
var
  NewElementCount: integer;
begin
  try
    NewElementCount := Max(FColumnCount,0)*Max(FRowCount,0)*Max(FLayerCount,0);
  except
    NewElementCount := MaxInt;
    Beep;
    MessageDlg(Format(StrTheNumberOfCells, [MaxInt]), mtError, [mbOK], 0);
  end;
  if (NewElementCount >= BigGrid) and (FElementCount < BigGrid) then
  begin
    if frmGoPhast.acShow3DObjects.Checked then
    begin
      frmGoPhast.acShow3DObjects.OnExecute(frmGoPhast.acShow3DObjects);
    end;
  end;
  FElementCount := NewElementCount;
end;

procedure TCustomModelGrid.EndColumnChange;
begin
  Dec(FColumnUpdate);
  ColumnsChanged;
end;

procedure TCustomModelGrid.EndGridChange;
begin
  Dec(FGridUpdate);
  GridChanged;
end;

procedure TCustomModelGrid.EndLayerChange;
begin
  Dec(FLayerUpdate);
  LayersChanged;
end;

procedure TCustomModelGrid.EndRowChange;
begin
  Dec(FRowUpdate);
  RowsChanged;
end;

function TCustomModelGrid.GetCellCoordinates(Col, Row, Layer: integer): T3DCellCoordinates;
var
  X1: Real;
  X2: Real;
  Y1: Real;
  Y2: Real;
  Z1: Real;
  Z2: Real;
  TempPoint: TPoint2D;
begin
  if Col = 0 then
  begin
    X1 := ColumnPosition[0];
  end
  else
  begin
    X1 := (ColumnPosition[Col] + ColumnPosition[Col-1])/2;
  end;
  if Col = ColumnCount then
  begin
    X2 := ColumnPosition[Col];
  end
  else
  begin
    X2 := (ColumnPosition[Col] + ColumnPosition[Col+1])/2;
  end;

  if Row = 0 then
  begin
    Y1 := RowPosition[0];
  end
  else
  begin
    Y1 := (RowPosition[Row] + RowPosition[Row-1])/2;
  end;
  if Row = RowCount then
  begin
    Y2 := RowPosition[Row];
  end
  else
  begin
    Y2 := (RowPosition[Row] + RowPosition[Row+1])/2;
  end;

  if Layer = 0 then
  begin
    Z1 := CellElevation[Col, Row, 0];
  end
  else
  begin
    Z1 := (CellElevation[Col, Row, Layer] + CellElevation[Col, Row, Layer-1])/2;
  end;
  if Layer = LayerCount then
  begin
    Z2 := CellElevation[Col, Row, Layer];
  end
  else
  begin
    Z2 := (CellElevation[Col, Row, Layer] + CellElevation[Col, Row, Layer+1])/2;
  end;

  TempPoint.x := X1;
  TempPoint.y := Y1;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col1_Row1_Lay1.x := TempPoint.x;
  result.Col1_Row1_Lay1.y := TempPoint.y;
  result.Col1_Row1_Lay1.z := Z1;

  TempPoint.x := X2;
  TempPoint.y := Y1;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col2_Row1_Lay1.x := TempPoint.x;
  result.Col2_Row1_Lay1.y := TempPoint.y;
  result.Col2_Row1_Lay1.z := Z1;


  TempPoint.x := X1;
  TempPoint.y := Y2;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col1_Row2_Lay1.x := TempPoint.x;
  result.Col1_Row2_Lay1.y := TempPoint.y;
  result.Col1_Row2_Lay1.z := Z1;

  TempPoint.x := X2;
  TempPoint.y := Y2;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col2_Row2_Lay1.x := TempPoint.x;
  result.Col2_Row2_Lay1.y := TempPoint.y;
  result.Col2_Row2_Lay1.z := Z1;

  TempPoint.x := X1;
  TempPoint.y := Y1;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col1_Row1_Lay2.x := TempPoint.x;
  result.Col1_Row1_Lay2.y := TempPoint.y;
  result.Col1_Row1_Lay2.z := Z2;

  TempPoint.x := X2;
  TempPoint.y := Y1;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col2_Row1_Lay2.x := TempPoint.x;
  result.Col2_Row1_Lay2.y := TempPoint.y;
  result.Col2_Row1_Lay2.z := Z2;


  TempPoint.x := X1;
  TempPoint.y := Y2;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col1_Row2_Lay2.x := TempPoint.x;
  result.Col1_Row2_Lay2.y := TempPoint.y;
  result.Col1_Row2_Lay2.z := Z2;

  TempPoint.x := X2;
  TempPoint.y := Y2;
  TempPoint := RotateFromGridCoordinatesToRealWorldCoordinates(TempPoint);
  result.Col2_Row2_Lay2.x := TempPoint.x;
  result.Col2_Row2_Lay2.y := TempPoint.y;
  result.Col2_Row2_Lay2.z := Z2;
end;

function TCustomModelGrid.GetColumnPosition(const Column: integer): real;
begin
  if (Column < 0) or (Column > ColumnCount) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidColumnNumbe, [Column]));
  end;
  result := FColumnPositions[Column];
end;

function TCustomModelGrid.GetColumnWidth(const Column: integer): real;
begin
  result := ColumnPosition[Column + 1] - ColumnPosition[Column];
  if ColumnDirection = cdEastToWest then
  begin
    result := -result;
  end;
end;

function TCustomModelGrid.GetWidths(
  const PositionArray: TOneDRealArray): TOneDRealArray;
var
  Index: Integer;
begin
  if Length(PositionArray) >= 2 then
  begin
    SetLength(result, Length(PositionArray)-1);
    for Index := 1 to Length(PositionArray) - 1 do
    begin
      result[Index-1] := PositionArray[Index] - PositionArray[Index-1]
    end;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TCustomModelGrid.CacheBlockGlGrid;
begin
  if FBlockGridCache = nil then
  begin
    FBlockGridCache := TMemoryStream.Create;
    FBlockGridCache.Position := 0;
    CacheGlGrid(FBlockGridCache, FBlockGlGrid);
  end;
end;

procedure TCustomModelGrid.RestoreBlockGlGrid;
begin
  RestoreGlGrid(FBlockGlGrid, FBlockGridCache);
end;

procedure TCustomModelGrid.CacheNodeGlGrid;
begin
  if FNodeGridCache = nil then
  begin
    FNodeGridCache := TMemoryStream.Create;
    FNodeGridCache.Position := 0;
    CacheGlGrid(FNodeGridCache, FNodeGlGrid);
  end;
end;

procedure TCustomModelGrid.RestoreNodeGlGrid;
begin
  RestoreGlGrid(FNodeGlGrid, FNodeGridCache);
end;


procedure TCustomModelGrid.CreateBlockGlGrid;
var
  ColumnIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  APoint3D: T3DRealPoint;
  APoint2D: TPoint2D;
begin
  if FBlockGridCache <> nil then
  begin
    RestoreBlockGlGrid;
    Exit;
  end;

  SetLength(FBlockGlGrid, ColumnCount + 2, RowCount + 2, LayerCount + 2);

  APoint2D := TwoDElementCorner (0, 0);
  FBlockGlGrid[0,0,0].P.X := APoint2D.X;
  FBlockGlGrid[0,0,0].P.Y := APoint2D.Y;
  FBlockGlGrid[0,0,0].P.Z := CellElevation[0,0,0];

  FBlockGlGrid[0,0,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[0,0,LayerCount+1].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[0,0,LayerCount+1].P.Z := CellElevation[0,0,LayerCount];
  except on EOverflow do
    FBlockGlGrid[0,0,LayerCount+1].P.Z := 0;
  end;


  APoint2D := TwoDElementCorner (ColumnCount, 0);
  FBlockGlGrid[ColumnCount + 1,0,0].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,0,0].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[ColumnCount + 1,0,0].P.Z := CellElevation[ColumnCount-1,0,0];
  except on EOverflow do
    FBlockGlGrid[ColumnCount + 1,0,0].P.Z := 0;
  end;

  FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.Z :=
      CellElevation[ColumnCount-1,0,LayerCount];
  except on EOverflow do
    FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.Z := 0;
  end;


  APoint2D := TwoDElementCorner (0, RowCount);
  FBlockGlGrid[0,RowCount + 1,0].P.X := APoint2D.X;
  FBlockGlGrid[0,RowCount + 1,0].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[0,RowCount + 1,0].P.Z := CellElevation[0,RowCount-1,0];
  except on EOverflow do
    FBlockGlGrid[0,RowCount + 1,0].P.Z := 0;
  end;

  APoint2D := TwoDElementCorner (0, RowCount);
  FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.Z :=
      CellElevation[0,RowCount-1,LayerCount];
  except on EOverflow do
    FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.Z := 0;
  end;


  APoint2D := TwoDElementCorner (ColumnCount, RowCount);
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.Z :=
      CellElevation[ColumnCount-1,RowCount-1,0];
  except on EOverflow do
    FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.Z := 0;
  end;

  FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.Y := APoint2D.Y;
  try
    FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.Z :=
      CellElevation[ColumnCount-1,RowCount-1,LayerCount];
  except on EOverflow do
    FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.Z := 0;
  end;


  for ColumnIndex := 0 to ColumnCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        APoint2D := TwoDElementCenter(ColumnIndex, RowIndex);
        APoint3D.X := APoint2D.x;
        APoint3D.Y := APoint2D.y;
        try
          APoint3D.Z := (CellElevation[ColumnIndex,RowIndex,LayerIndex]
            + CellElevation[ColumnIndex,RowIndex,LayerIndex+1])/2;
        except on EOverflow do
          APoint3D.Z := 0;
        end;

        FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.X := APoint3D.X;
        FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Y := APoint3D.Y;
        try
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        except on EOverflow do
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Z := 0;
        end;

        if LayerIndex = 0 then
        begin
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.X := APoint3D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.Y := APoint3D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.Z := 0;
          end;
        end;
        if LayerIndex = LayerCount - 1 then
        begin
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.X := APoint3D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.Y := APoint3D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.Z := 0;
          end;
        end;

        if RowIndex = 0 then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Z := 0
          end;
        end;
        if RowIndex = RowCount - 1 then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Z := 0;
          end;
        end;

        if ColumnIndex = 0 then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Z := 0;
          end;
        end;
        if ColumnIndex = ColumnCount - 1 then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Z := 0;
          end;
        end;

        if (ColumnIndex = 0) and (RowIndex = 0) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Z := 0;
          end;
        end;
        if (ColumnIndex = ColumnCount - 1) and (RowIndex = 0) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Z := 0;
          end;
        end;
        if (ColumnIndex = 0) and (RowIndex = RowCount - 1) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Z := 0;
          end;
        end;
        if (ColumnIndex = ColumnCount - 1) and (RowIndex = RowCount - 1) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex+1, RowIndex+1);
          FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Z := 0;
          end;
        end;

        if (ColumnIndex = 0) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.Z := 0;
          end;
        end;
        if (ColumnIndex = ColumnCount - 1) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.Z := 0;
          end;
        end;
        if (ColumnIndex = 0) and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.Z := 0;
          end;
        end;
        if (ColumnIndex = ColumnCount - 1)
          and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.Z := 0;
          end;
        end;

        if (RowIndex = 0) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.Z := 0;
          end;
        end;
        if (RowIndex = RowCount - 1) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.Z := 0;
          end;
        end;
        if (RowIndex = 0) and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.Z := 0;
          end;
        end;
        if (RowIndex = RowCount - 1) and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.Y := APoint2D.Y;
          try
            FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.Z :=
              CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
          except on EOverflow do
            FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.Z := 0;
          end;
        end;
      end;
    end;
  end;
  for ColumnIndex := 0 to ColumnCount + 1 do
  begin
    for RowIndex := 0 to RowCount + 1 do
    begin
      for LayerIndex := 0 to LayerCount + 1 do
      begin
        FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex].Value := 0;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.CreateNodeGlGrid;
var
  ColumnIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  APoint2D: TPoint2D;
  APoint3D: T3DRealPoint;
  ElevAbove, ElevBelow: double;
begin
  if FNodeGridCache <> nil then
  begin
    RestoreNodeGlGrid;
    Exit;
  end;
  SetLength(FNodeGlGrid, ColumnCount + 3, RowCount + 3, LayerCount + 3);

  ElevAbove := CellElevation[0,0,0]
    + (CellElevation[0,0,0] - CellElevation[0,0,1])/2;
  ElevBelow := CellElevation[0,0,LayerCount]
    + (CellElevation[0,0,LayerCount] - CellElevation[0,0,LayerCount-1])/2;

  APoint2D.x := ColumnPosition[0] + (ColumnPosition[0] - ColumnPosition[1])/2;
  APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[0,0,0].P.X := APoint2D.X;
  FNodeGlGrid[0,0,0].P.Y := APoint2D.Y;
  FNodeGlGrid[0,0,0].P.Z := ElevAbove;

  FNodeGlGrid[0,0,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[0,0,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[0,0,LayerCount + 2].P.Z := ElevBelow;

  APoint2D.x := ColumnPosition[ColumnCount]
    + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1]);
  APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[ColumnCount + 2,0,0].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,0,0].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,0,0].P.Z := ElevAbove;

  FNodeGlGrid[ColumnCount + 2,0,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,0,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,0,LayerCount + 2].P.Z := ElevBelow;

  APoint2D.x := ColumnPosition[0] + (ColumnPosition[0] - ColumnPosition[1])/2;
  APoint2D.y := RowPosition[RowCount]
    + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[0,RowCount + 2,0].P.X := APoint2D.X;
  FNodeGlGrid[0,RowCount + 2,0].P.Y := APoint2D.Y;
  FNodeGlGrid[0,RowCount + 2,0].P.Z := ElevAbove;

  FNodeGlGrid[0,RowCount + 2,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[0,RowCount + 2,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[0,RowCount + 2,LayerCount + 2].P.Z := ElevBelow;

  APoint2D.x := ColumnPosition[ColumnCount]
    + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
  APoint2D.y := RowPosition[RowCount]
    + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[ColumnCount + 2,RowCount + 2,0].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,0].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,0].P.Z := ElevAbove;

  FNodeGlGrid[ColumnCount + 2,RowCount + 2,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,LayerCount + 2].P.Z := ElevBelow;

  for ColumnIndex := 0 to ColumnCount do
  begin
    for RowIndex := 0 to RowCount do
    begin
      APoint2D := TwoDElementCorner(ColumnIndex, RowIndex);
      APoint3D.X := APoint2D.x;
      APoint3D.Y := APoint2D.y;
      APoint3D.Z := ElevAbove;

      FNodeGlGrid[ColumnIndex+1,RowIndex+1,0].P.X := APoint3D.X;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,0].P.Y := APoint3D.Y;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,0].P.Z := APoint3D.Z;

      APoint3D.Z := ElevBelow;

      FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerCount + 2].P.X := APoint3D.X;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerCount + 2].P.Y := APoint3D.Y;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerCount + 2].P.Z := APoint3D.Z;

      for LayerIndex := 0 to LayerCount do
      begin
        APoint2D := TwoDElementCorner(ColumnIndex, RowIndex);
        APoint3D.X := APoint2D.x;
        APoint3D.Y := APoint2D.y;
        APoint3D.Z := CellElevation[ColumnIndex,RowIndex,LayerIndex];

        FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.X := APoint3D.X;
        FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Y := APoint3D.Y;
        FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;

        if ColumnIndex = 0 then
        begin
          APoint2D.x := ColumnPosition[0]
            + (ColumnPosition[0] - ColumnPosition[1])/2;
          APoint2D.y := RowPosition[RowIndex];
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if ColumnIndex = ColumnCount then
        begin
          APoint2D.x := ColumnPosition[ColumnCount]
            + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
          APoint2D.y := RowPosition[RowIndex];
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if RowIndex = 0 then
        begin
          APoint2D.x := ColumnPosition[ColumnIndex];
          APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if RowIndex = RowCount then
        begin
          APoint2D.x := ColumnPosition[ColumnIndex];
          APoint2D.y := RowPosition[RowCount]
            + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if (ColumnIndex = 0) and (RowIndex = 0) then
        begin
          APoint2D.x := ColumnPosition[0]
            + (ColumnPosition[0] - ColumnPosition[1])/2;
          APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = ColumnCount) and (RowIndex = 0) then
        begin
          APoint2D.x := ColumnPosition[ColumnCount]
            + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
          APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = 0) and (RowIndex = RowCount) then
        begin
          APoint2D.x := ColumnPosition[0]
            + (ColumnPosition[0] - ColumnPosition[1])/2;
          APoint2D.y := RowPosition[RowCount]
            + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = ColumnCount) and (RowIndex = RowCount) then
        begin
          APoint2D.x := ColumnPosition[ColumnCount]
            + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
          APoint2D.y := RowPosition[RowCount]
            + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;
      end;
    end;
  end;
  for ColumnIndex := 0 to ColumnCount do
  begin
    for RowIndex := 0 to RowCount do
    begin
      for LayerIndex := 0 to LayerCount do
      begin
        FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex].Value := 0;
      end;
    end;
  end;
end;

function TCustomModelGrid.ContourGrid(EvaluatedAt: TEvaluatedAt;
  ModelSelection: TModelSelection; ViewDirection: TViewDirection;
  ColRowOrLayer: integer): T2DGrid;
var
  Local3DGridPoints: TGrid;
  ColIndex, RowIndex, LayIndex: integer;
  P: TGlCoord;
  APoint: TPoint2D;
begin
  result := nil;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  case ViewDirection of
    vdTop:
      begin
        case EvaluatedAt of
          eaBlocks:
            result := FTopElementContourGrid;
          eaNodes:
            result := FTopNodeContourGrid;
          else Assert(False);
        end;
      end;
    vdFront:
      begin
        case EvaluatedAt of
          eaBlocks:
            result := FFrontElementContourGrid;
          eaNodes:
            result := FFrontNodeContourGrid;
          else Assert(False);
        end;
      end;
    vdSide:
      begin
        case EvaluatedAt of
          eaBlocks:
            result := FSideElementContourGrid;
          eaNodes:
            result := FSideNodeContourGrid;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;
  if result = nil then
  begin
    Local3DGridPoints := PrivateGlGrid(EvaluatedAt, ModelSelection);
    case ViewDirection of
      vdTop:
        begin
          LayIndex := ColRowOrLayer+1;
          SetLength(result,Length(Local3DGridPoints),
            Length(Local3DGridPoints[0]));
          for ColIndex := 0 to Length(Local3DGridPoints)-1 do
          begin
            for RowIndex := 0 to Length(Local3DGridPoints[0])-1 do
            begin
              result[ColIndex,RowIndex].P.x :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.X;
              result[ColIndex,RowIndex].P.y :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.Y;
            end;
          end;
        case EvaluatedAt of
          eaBlocks:
            FTopElementContourGrid := result;
          eaNodes:
            FTopNodeContourGrid := result;
          else Assert(False);
        end;
        end;
      vdFront:
        begin
          RowIndex := ColRowOrLayer+1;
          SetLength(result,Length(Local3DGridPoints),
            Length(Local3DGridPoints[0,0]));
          for ColIndex := 0 to Length(Local3DGridPoints)-1 do
          begin
            for LayIndex := 0 to Length(Local3DGridPoints[0,0])-1 do
            begin
              P := Local3DGridPoints[ColIndex,RowIndex,LayIndex].P;
              APoint.X := P.X;
              APoint.Y := P.Y;
              APoint:= RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
              result[ColIndex,LayIndex].P.x := APoint.X;
              result[ColIndex,LayIndex].P.y := P.Z;
            end;
          end;
          case EvaluatedAt of
            eaBlocks:
              FFrontElementContourGrid := result;
            eaNodes:
              FFrontNodeContourGrid := result;
            else Assert(False);
          end;
        end;
      vdSide:
        begin
          ColIndex := ColRowOrLayer+1;
          SetLength(result,Length(Local3DGridPoints[0]),
            Length(Local3DGridPoints[0,0]));
          for RowIndex := 0 to Length(Local3DGridPoints[0])-1 do
          begin
            for LayIndex := 0 to Length(Local3DGridPoints[0,0])-1 do
            begin
              P := Local3DGridPoints[ColIndex,RowIndex,LayIndex].P;
              APoint.X := P.X;
              APoint.Y := P.Y;
              APoint:= RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
              result[RowIndex,LayIndex].P.x := P.Z;
              result[RowIndex,LayIndex].P.y := APoint.Y;
            end;
          end;
          case EvaluatedAt of
            eaBlocks:
              FSideElementContourGrid := result;
            eaNodes:
              FSideNodeContourGrid := result;
            else Assert(False);
          end;
        end;
      else Assert(False);
    end;
    case EvaluatedAt of
      eaBlocks: CacheBlockGlGrid;
      eaNodes: CacheNodeGlGrid;
      else Assert(False);
    end;
  end;
end;

function TCustomModelGrid.GlGrid(EvaluatedAt: TEvaluatedAt;
  ModelSelection: TModelSelection): TGrid;
begin
  result := nil;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  Result := PrivateGlGrid(EvaluatedAt, ModelSelection);
  if Result <> nil then
  begin
    // make a copy of the cached result.
    SetLength(result, Length(result), Length(result[0]), Length(result[0,0]));
    case EvaluatedAt of
      eaBlocks: CacheBlockGlGrid;
      eaNodes: CacheNodeGlGrid;
      else Assert(False);
    end;
  end;
end;

function TCustomModelGrid.PrivateGlGrid(EvaluatedAt: TEvaluatedAt;
  ModelSelection: TModelSelection): TGrid;
begin
  result := nil;
  case EvaluatedAt of
    eaBlocks:
      begin
        if FBlockGlGrid = nil then
        begin
          CreateBlockGlGrid;
        end;
        result := FBlockGlGrid;
      end;
    eaNodes:
      begin
        if ModelSelection = msPhast then
        begin
          if FNodeGlGrid = nil then
          begin
            CreateNodeGlGrid;
          end;
          result := FNodeGlGrid;
        end;
      end;
    else
      Assert(False);
  end;
end;

function TCustomModelGrid.GetColWidths: TOneDRealArray;
begin
  result := GetWidths(FColumnPositions);
end;

function TCustomModelGrid.GetRowPosition(const Row: integer): real;
begin
  if (Row < 0) or (Row > RowCount) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidRowNumber, [Row]));
  end;
  result := FRowPositions[Row];
end;

function TCustomModelGrid.GetRowWidth(const Row: integer): real;
begin
  result := RowPosition[Row + 1] - RowPosition[Row];
  if RowDirection = rdNorthToSouth then
  begin
    result := -result;
  end;
end;

function TCustomModelGrid.GetRowWidths: TOneDRealArray;
begin
  result := GetWidths(FRowPositions);
end;

function TCustomModelGrid.GetThreeDDataSet: TDataArray;
begin
  result := FThreeDDataSet;
end;

function TCustomModelGrid.GetThreeDGridObserver: TObserver;
begin
  result := FThreeDGridObserver;
end;

function TCustomModelGrid.GetTopCellColors(const Column, Row: integer): TColor;
var
  ColumnLimit, RowLimit: integer;
begin
  ColumnLimit := ColumnCount;
  RowLimit := RowCount;
  if TopDataSet <> nil then
  begin
    if NeedToRecalculateTopCellColors then
    begin
      UpdateCellColors(vdTop);
    end;
    if TopDataSet <> nil then
    begin
      case TopDataSet.EvaluatedAt of
        eaBlocks:
          begin
            ColumnLimit := ColumnCount;
            RowLimit := RowCount;
          end;
        eaNodes:
          begin
            ColumnLimit := ColumnCount + 1;
            RowLimit := RowCount + 1;
          end;
      else
        begin
          ColumnLimit := -1;
          RowLimit := -1;
          Assert(False);
        end;
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidColumnNumbe, [Column]));
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidRowNumber, [Row]));
  end;
  Result := FTopCellColors[Column, Row];

end;

function TCustomModelGrid.GetTopElementOutline(Row,
  Column: Integer): TElementOutline;
begin
  result.Count := 4;
  result.Points[0] := TwoDElementCorner(Column, Row);
  result.Points[1] := TwoDElementCorner(Column, Row+1);
  result.Points[2] := TwoDElementCorner(Column+1, Row+1);
  result.Points[3] := TwoDElementCorner(Column+1, Row);
end;

function TCustomModelGrid.NearestColumnPosition(const APosition: real;
  const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(FColumnPositions, APosition, First, Last);
end;

function TCustomModelGrid.NearestColumnOrRow(const Positions: TOneDRealArray;
  const APosition: real; const First: integer = -1;
  const Last: integer = -1): integer;
var
  Below, Above, Middle: integer;
  Reversed: boolean;
begin
  if Length(Positions) <= 0 then
  begin
    result := -1;
  end
  else if Length(Positions) = 1 then
  begin
    result := 0;
  end
  else
  begin
    Above := Length(Positions) - 1;
    Reversed := Positions[0] > Positions[Above];
    if Reversed then
    begin
      if APosition < Positions[Above] then
      begin
        result := Above + 1;
        Exit;
      end
      else if APosition > Positions[0] then
      begin
        result := -1;
        Exit;
      end;
      if (Last >= 0) and (Above > Last) then
      begin
        Above := Last;
      end;

      Below := 0;
      if (First >= 0) and (Below < First) then
      begin
        Below := First;
      end;
      if Above < Below then
      begin
        result := -1;
        Exit;
      end;
      while Above - Below > 1 do
      begin
        Middle := (Above + Below) div 2;
        if Positions[Middle] < APosition then
        begin
          Above := Middle;
        end
        else
        begin
          Below := Middle;
        end;
      end;
      if Abs(Positions[Below] - APosition)
        < Abs(Positions[Above] - APosition) then
      begin
        result := Below;
      end
      else
      begin
        result := Above;
      end;
    end
    else
    begin
      if APosition < Positions[0] then
      begin
        result := -1;
        Exit;
      end
      else if APosition > Positions[Above] then
      begin
        result := Above + 1;
        Exit;
      end;
      if (Last >= 0) and (Above > Last) then
      begin
        Above := Last;
      end;

      Below := 0;
      if (First >= 0) and (Below < First) then
      begin
        Below := First;
      end;
      if Above < Below then
      begin
        result := -1;
        Exit;
      end;
      while Above - Below > 1 do
      begin
        Middle := (Above + Below) div 2;
        if Positions[Middle] > APosition then
        begin
          Above := Middle;
        end
        else
        begin
          Below := Middle;
        end;
      end;
      if Abs(Positions[Below] - APosition)
        < Abs(Positions[Above] - APosition) then
      begin
        result := Below;
      end
      else
      begin
        result := Above;
      end;
    end;

  end;
end;

function TCustomModelGrid.NearestRowPosition(const APosition: real;
  const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(FRowPositions, APosition, First, Last);
end;

procedure TCustomModelGrid.RotateAroundGridOrigin(NewAngle: Double);
var
  Index: integer;
  GridCenter: TPoint2D;
  TrueCenter: TPoint2D;
begin
  while NewAngle > Pi do
  begin
    NewAngle := NewAngle - 2 * Pi;
  end;
  while NewAngle < -Pi do
  begin
    NewAngle := NewAngle + 2 * Pi;
  end;
  if FGridAngle <> NewAngle then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculatesideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    //  Find coordinates of center in grid coordinates.
    if ColumnCount >= 0 then
    begin
      GridCenter.X := FColumnPositions[0];
    end
    else
    begin
      GridCenter.X := 0;
    end;

    if RowCount >= 0 then
    begin
      GridCenter.Y := FRowPositions[0];
    end
    else
    begin
      GridCenter.Y := 0;
    end;

    // convert to true coordinates.
    TrueCenter := RotateFromGridCoordinatesToRealWorldCoordinates(GridCenter);

    // Off set all positions so that center is at (0,0).
    for Index := 0 to Length(FColumnPositions) - 1 do
    begin
      FColumnPositions[Index] := FColumnPositions[Index] - GridCenter.X;
    end;

    for Index := 0 to Length(FRowPositions) - 1 do
    begin
      FRowPositions[Index] := FRowPositions[Index] - GridCenter.Y;
    end;

    // change grid angle
    FGridAngle := NewAngle;

    // find coordinates of (old) center in new grid coordinates
    GridCenter := RotateFromRealWorldCoordinatesToGridCoordinates(TrueCenter);

    // Offset all positions to grid center;
    for Index := 0 to Length(FColumnPositions) - 1 do
    begin
      FColumnPositions[Index] := FColumnPositions[Index] + GridCenter.X;
    end;

    for Index := 0 to Length(FRowPositions) - 1 do
    begin
      FRowPositions[Index] := FRowPositions[Index] + GridCenter.Y;
    end;

    frmGoPhast.dcMoveColCursor.RedrawCursor;
    frmGoPhast.dcMoveRowCursor.RedrawCursor;
    frmGoPhast.dcAddColCursor.RedrawCursor;
    frmGoPhast.dcAddRowCursor.RedrawCursor;
    frmGoPhast.dcSubdivide.RedrawCursor;
    frmGoPhast.dcSetSpacing.RedrawCursor;
    InvalidateScreenObjects;
    GridChanged;
    ColumnsChanged;
    RowsChanged;
  end;
end;

function TCustomModelGrid.RotatedThreeDElementCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  APoint: TPoint2D;
begin
  result := ThreeDElementCenter(Column, Row, Layer);
  APoint.X := Result.X;
  APoint.Y := Result.Y;
  APoint:= RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function TCustomModelGrid.RotatedThreeDElementCorner(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  APoint: TPoint2D;
begin
  result := ThreeDElementCorner(Column, Row, Layer);
  APoint.X := Result.X;
  APoint.Y := Result.Y;
  RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function TCustomModelGrid.RotateFromGridCoordinatesToRealWorldCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
begin
  result := APoint;
  if GridAngle <> 0 then
  begin
    temp.X := Cos(GridAngle) * result.X - Sin(GridAngle) * result.Y;
    temp.Y := Sin(GridAngle) * result.X + Cos(GridAngle) * result.Y;
    result := temp;
  end;
end;

procedure TCustomModelGrid.RotateFromGridCoordinatesToRealWorldCoordinates(
  var X, Y: TFloat);
var
  APoint: TPoint2D;
begin
  APoint.x := X;
  APoint.y := Y;
  APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
  X := APoint.x;
  Y := APoint.y;
end;

procedure TCustomModelGrid.RotateFromRealWorldCoordinatesToGridCoordinates(
  var X, Y: TFloat);
var
  APoint: TPoint2D;
begin
  APoint.x := X;
  APoint.y := Y;
  APoint := RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  X := APoint.x;
  Y := APoint.y;
end;

function TCustomModelGrid.RotateFromRealWorldCoordinatesToGridCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
begin
  result := APoint;
  if GridAngle <> 0 then
  begin
    temp.X := Cos(-GridAngle) * result.X - Sin(-GridAngle) * result.Y;
    temp.Y := Sin(-GridAngle) * result.X + Cos(-GridAngle) * result.Y;
    result := temp;
  end;
end;

procedure TCustomModelGrid.SetColumnCount(const Value: integer);
var
  Index: integer;
  LastValue: real;
begin
  if FColumnCount <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;

    SetLength(FColumnPositions, Value + 1);
    if Value > FColumnCount then
    begin
      if FColumnCount >= 0 then
      begin
        LastValue := FColumnPositions[FColumnCount];
      end
      else
      begin
        LastValue := 0;
      end;

      for Index := FColumnCount + 1 to Value do
      begin
        FColumnPositions[Index] := LastValue;
      end;
    end;
    FColumnCount := Value;
    if SelectedColumn >= FColumnCount then
    begin
      SelectedColumn := FColumnCount - 1;
    end;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.UpdateDataSetDimensions;
    InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    ColumnsChanged;
    ElementCountChanged;
  end;
end;

procedure TCustomModelGrid.SetColumnDirection(const Value: TColumnDirection);
var
  Index: Integer;
  Temp: real;
begin
  if FColumnDirection <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FColumnDirection := Value;
    if FColumnPositions <> nil then
    begin
      for Index := 0 to ColumnCount div 2 do
      begin
        Temp := FColumnPositions[Index];
        FColumnPositions[Index] := FColumnPositions[ColumnCount - Index];
        FColumnPositions[ColumnCount - Index] := Temp;
      end;
    end;
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.InvalidateSegments;
      InvalidateScreenObjects;
    end;
    frmGoPhast.InvalidateModel;
    GridChanged;
    ColumnsChanged;
  end;
end;

procedure TCustomModelGrid.SetColumnPosition(const Column: integer;
  const Value: real);
begin
  if (Column < 0) or (Column > ColumnCount) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidColumnNumbe, [Column]));
  end;
  if FColumnPositions[Column] <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FColumnPositions[Column] := Value;
    frmGoPhast.PhastModel.InvalidateSegments;
    InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    ColumnsChanged;
  end;
  if ColumnDirection = cdWestToEast then
  begin
    if ((Column > 0) and (Value < FColumnPositions[Column - 1]))
      or ((Column < ColumnCount) and (Value > FColumnPositions[Column + 1]))
        then
    begin
      UpdateColumnPositions;
    end;
  end
  else
  begin
    if ((Column > 0) and (Value > FColumnPositions[Column - 1]))
      or ((Column < ColumnCount) and (Value < FColumnPositions[Column + 1]))
        then
    begin
      UpdateColumnPositions;
    end;
  end;
end;

procedure TCustomModelGrid.SetColumnPositions(const Value: TOneDRealArray);
var
  Index: integer;
begin
  if SelectedColumn >  Length(Value) - 2 then
  begin
    SelectedColumn := Length(Value) - 2;
  end;

  SetLength(FColumnPositions, Length(Value));
  for Index := 0 to Length(Value) - 1 do
  begin
    FColumnPositions[Index] := Value[Index];
  end;

  UpdateColumnPositions;
  InvalidateScreenObjects;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomModelGrid.SetColumnWidth(const Column: integer;
  const Value: real);
var
  Delta: real;
  Index: integer;
begin
  if Value < 0 then
  begin
    raise EInvalidGrid.Create(StrColumnWidthsMustB);
  end;
  Delta := ColumnWidth[Column] - Value;
  for Index := Column + 1 to ColumnCount do
  begin
    if ColumnDirection = cdWestToEast then
    begin
      FColumnPositions[Index] := FColumnPositions[Index] - Delta;
    end
    else
    begin
      FColumnPositions[Index] := FColumnPositions[Index] + Delta;
    end;
  end;
  frmGoPhast.PhastModel.InvalidateSegments;
  InvalidateScreenObjects;
  frmGoPhast.InvalidateModel;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateFrontCellColors := True;
  NeedToRecalculate3DCellColors := True;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomModelGrid.SetDisplayColumn(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if (Value > ColumnCount) and (ColumnCount > 0) then
  begin
    Value := ColumnCount;
  end;
  if FDisplayColumn <> Value then
  begin
    SelectedColumn := Value;
    FDisplayColumn := Value;
    FRecordedSideGrid := False;
    if Assigned(FOnSelectedColumnChange) then
    begin
      FOnSelectedColumnChange(self);
    end;
  end;
end;

procedure TCustomModelGrid.SetDisplayLayer(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if (Value > LayerCount) and (LayerCount > 0) then
  begin
    Value := LayerCount;
  end;
  if FDisplayLayer <> Value then
  begin
    SelectedLayer := Value;
    FDisplayLayer := Value;
    FRecordedTopGrid := False;
    if Assigned(FOnSelectedLayerChange) then
    begin
      FOnSelectedLayerChange(self);
    end;
  end;
end;

procedure TCustomModelGrid.SetDisplayRow(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if (Value > RowCount) and (RowCount > 0) then
  begin
    Value := RowCount;
  end;
  if FDisplayRow <> Value then
  begin
    SelectedRow := Value;
    FDisplayRow := Value;
    FRecordedFrontGrid := False;
    if Assigned(FOnSelectedRowChange) then
    begin
      FOnSelectedRowChange(self);
    end;
  end;
end;

procedure TCustomModelGrid.SetGridLineDrawingChoice(const Value: TGridLineDrawingChoice);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  FGridLineDrawingChoice := Value;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel. ModflowGrid.GridLineDrawingChoice := Value;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.SetGridAngle(Value: real);
var
  Index: integer;
  GridCenter: TPoint2D;
  TrueCenter: TPoint2D;
begin
  while Value > Pi do
  begin
    Value := Value - 2 * Pi;
  end;
  while Value < -Pi do
  begin
    Value := Value + 2 * Pi;
  end;
  if FGridAngle <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculatesideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    //  Find coordinates of center in grid coordinates.
    if ColumnCount >= 0 then
    begin
      GridCenter.X := (FColumnPositions[0] +
        FColumnPositions[Length(FColumnPositions) - 1]) / 2;
    end
    else
    begin
      GridCenter.X := 0;
    end;

    if RowCount >= 0 then
    begin
      GridCenter.Y := (FRowPositions[0] +
        FRowPositions[Length(FRowPositions) - 1]) / 2;
    end
    else
    begin
      GridCenter.Y := 0;
    end;

    // convert to true coordinates.
    TrueCenter := RotateFromGridCoordinatesToRealWorldCoordinates(GridCenter);

    // Off set all positions so that center is at (0,0).
    for Index := 0 to Length(FColumnPositions) - 1 do
    begin
      FColumnPositions[Index] := FColumnPositions[Index] - GridCenter.X;
    end;

    for Index := 0 to Length(FRowPositions) - 1 do
    begin
      FRowPositions[Index] := FRowPositions[Index] - GridCenter.Y;
    end;

    // change grid angle
    FGridAngle := Value;

    // find coordinates of (old) center in new grid coordinates
    GridCenter := RotateFromRealWorldCoordinatesToGridCoordinates(TrueCenter);

    // Offset all positions to grid center;
    for Index := 0 to Length(FColumnPositions) - 1 do
    begin
      FColumnPositions[Index] := FColumnPositions[Index] + GridCenter.X;
    end;

    for Index := 0 to Length(FRowPositions) - 1 do
    begin
      FRowPositions[Index] := FRowPositions[Index] + GridCenter.Y;
    end;

    frmGoPhast.dcMoveColCursor.RedrawCursor;
    frmGoPhast.dcMoveRowCursor.RedrawCursor;
    frmGoPhast.dcAddColCursor.RedrawCursor;
    frmGoPhast.dcAddRowCursor.RedrawCursor;
    frmGoPhast.dcSubdivide.RedrawCursor;
    frmGoPhast.dcSetSpacing.RedrawCursor;
    InvalidateScreenObjects;
    GridChanged;
    ColumnsChanged;
    RowsChanged;
  end;
end;

procedure TCustomModelGrid.SetLayerCount(const Value: integer);
begin
  if FLayerCount <> Value then
  begin
    FLayerCount := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
//    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateScreenObjects;
    InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    if SelectedLayer >= FLayerCount then
    begin
      SelectedLayer := FLayerCount - 1;
    end;
    frmGoPhast.UpdateDataSetDimensions;
    GridChanged;
    LayersChanged;
    ElementCountChanged;
  end;
end;

procedure TCustomModelGrid.SetLayerDirection(const Value: TLayerDirection);
begin
  if FLayerDirection <> Value then
  begin
    FLayerDirection := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.InvalidateSegments;
      InvalidateScreenObjects;
    end;
    frmGoPhast.InvalidateModel;
    GridChanged;
    LayersChanged;
  end;
end;

procedure TCustomModelGrid.ResetTopCellColors;
var
  ColIndex, RowIndex: integer;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if (FColumnCount <= 0) or (FRowCount <= 0) then
  begin
    SetLength(FTopCellColors, 0, 0);
    FNeedToRecalculateTopCellColors := False;
    Exit;
  end;
  if TopDataSet <> nil then
  begin
    case TopDataSet.EvaluatedAt of
      eaBlocks:
        begin
          if (Length(FTopCellColors) <> FColumnCount)
            or (Length(FTopCellColors[0]) <> FRowCount) then
          begin
            SetLength(FTopCellColors, FColumnCount, FRowCount);
          end;
          for ColIndex := 0 to FColumnCount - 1 do
          begin
            for RowIndex := 0 to FRowCount - 1 do
            begin
              TopCellColors[ColIndex, RowIndex] := clWhite;
            end;
          end;
        end;
      eaNodes:
        begin
          if (Length(FTopCellColors) <> FColumnCount + 1)
            or (Length(FTopCellColors[0]) <> FRowCount + 1) then
          begin
            SetLength(FTopCellColors, FColumnCount + 1, FRowCount + 1);
          end;
          for ColIndex := 0 to FColumnCount do
          begin
            for RowIndex := 0 to FRowCount do
            begin
              TopCellColors[ColIndex, RowIndex] := clWhite;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  end;

  FNeedToRecalculateTopCellColors := False;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildModel.Grid.ResetTopCellColors;
        end;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.SetRowCount(const Value: integer);
var
  Index: integer;
  LastValue: real;
begin
  if FRowCount <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    InvalidateScreenObjects;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    SetLength(FRowPositions, Value + 1);
    if Value > FRowCount then
    begin
      if FRowCount >= 0 then
      begin
        LastValue := FRowPositions[FRowCount];
      end
      else
      begin
        LastValue := 0;
      end;

      for Index := FRowCount + 1 to Value do
      begin
        FRowPositions[Index] := LastValue;
      end;
    end;
    FRowCount := Value;
    if SelectedRow >= FRowCount then
    begin
      SelectedRow := FRowCount - 1;
    end;
    frmGoPhast.UpdateDataSetDimensions;
    GridChanged;
    RowsChanged;
    ElementCountChanged;
  end;
end;

procedure TCustomModelGrid.SetRowDirection(const Value: TRowDirection);
var
  Index: integer;
  Temp: real;
begin
  if FRowDirection <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FRowDirection := Value;
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.InvalidateSegments;
      InvalidateScreenObjects;
    end;
    frmGoPhast.InvalidateModel;
    if FRowPositions <> nil then
    begin
      for Index := 0 to RowCount div 2 do
      begin
        Temp := FRowPositions[Index];
        FRowPositions[Index] := FRowPositions[RowCount - Index];
        FRowPositions[RowCount - Index] := Temp;
      end;
    end;
    GridChanged;
    RowsChanged;
  end;
end;

procedure TCustomModelGrid.SetRowPosition(const Row: integer;
  const Value: real);
begin
  if (Row < 0) or (Row > RowCount) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidRowNumber, [Row]));
  end;
  if FRowPositions[Row] <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FRowPositions[Row] := Value;
    frmGoPhast.PhastModel.InvalidateSegments;
    InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    RowsChanged;
  end;
  if (RowDirection = rdSouthToNorth) then
  begin
    if ((Row > 0) and (Value < FRowPositions[Row - 1]))
      or ((Row < RowCount) and (Value > FRowPositions[Row + 1])) then
    begin
      UpdateRowPositions;
    end;
  end
  else
  begin
    if ((Row > 0) and (Value > FRowPositions[Row - 1]))
      or ((Row < RowCount) and (Value < FRowPositions[Row + 1])) then
    begin
      UpdateRowPositions;
    end;
  end;
end;

procedure TCustomModelGrid.SetRowPositions(const Value: TOneDRealArray);
var
  Index: integer;
begin
  if SelectedRow > Length(Value) - 2 then
  begin
    SelectedRow := Length(Value) - 2;
  end;

  SetLength(FRowPositions, Length(Value));
  for Index := 0 to Length(Value) - 1 do
  begin
    FRowPositions[Index] := Value[Index];
  end;
  UpdateRowPositions;
  InvalidateScreenObjects;
  GridChanged;
  RowsChanged;
end;

procedure TCustomModelGrid.SetRowWidth(const Row: integer; const Value: real);
var
  Delta: real;
  Index: integer;
begin
  if Value < 0 then
  begin
    raise EInvalidGrid.Create(StrRowWidthsMustBeG);
  end;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateSideCellColors := True;
  NeedToRecalculate3DCellColors := True;
  InvalidateScreenObjects;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  Delta := RowWidth[Row] - Value;
  for Index := Row + 1 to RowCount do
  begin
    if RowDirection = rdSouthToNorth then
    begin
      FRowPositions[Index] := FRowPositions[Index] - Delta;
    end
    else
    begin
      FRowPositions[Index] := FRowPositions[Index] + Delta;
    end;
  end;
  GridChanged;
  RowsChanged;
end;

procedure TCustomModelGrid.SetTopCellColors(const Column, Row: integer;
  const Value: TColor);
var
  ColumnLimit, RowLimit: integer;
begin
  if TopDataSet = nil then
  begin
    ColumnLimit := ColumnCount;
    RowLimit := RowCount;
  end
  else
  begin
    case TopDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount;
          RowLimit := RowCount;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount + 1;
          RowLimit := RowCount + 1;
        end;
    else
      begin
        ColumnLimit := -1;
        RowLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidColumnNumbe, [Column]));
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidRowNumber, [Row]));
  end;
  FTopCellColors[Column, Row] := Value;
end;

procedure TCustomModelGrid.SetTopContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopContourDataSet <> Value then
  begin
    FTopContourDataSet := Value;
  end;
end;

function TCustomModelGrid.ThreeDElementCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
begin
  result.X := (ColumnPosition[Column] + ColumnPosition[Column + 1]) / 2;
  result.Y := (RowPosition[Row] + RowPosition[Row + 1]) / 2;
  result.Z := (CellElevation[Column, Row, Layer]
    + CellElevation[Column, Row, Layer + 1]) / 2;
end;

function TCustomModelGrid.ThreeDElementCorner(const Column, Row,
  Layer: integer): T3DRealPoint;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];
  result.Z := CellElevation[Column, Row, Layer];
end;

function TCustomModelGrid.UnconstrainedTopContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt;
  const NeedToRotatePointToGridCoordinates: boolean; const BelowCol, AboveCol,
  BelowRow, AboveRow: integer): T2DTopCell;
begin
  if NeedToRotatePointToGridCoordinates then
  begin
    APoint := RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  end;
  result.Col := NearestColumnPosition(APoint.X);
  result.Row := NearestRowPosition(APoint.Y);
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        Exit;
      end;
  else
    Assert(False);
  end;

  if ColumnDirection = cdWestToEast then
  begin
    if (result.Col > 0) and (result.Col <= ColumnCount)
      and (ColumnPosition[result.Col] > APoint.X) then
    begin
      Dec(result.Col);
    end;
    if (result.Col = ColumnCount)
      and (ColumnPosition[result.Col] < APoint.X) then
    begin
      result.Col := -1;
    end;
    if  (result.Col > ColumnCount) then
    begin
      result.Col := -1;
    end;
  end
  else
  begin
    if (result.Col > 0) and (result.Col <= ColumnCount)
      and (ColumnPosition[result.Col] < APoint.X) then
    begin
      Dec(result.Col);
    end;
    if (result.Col = ColumnCount)
      and (ColumnPosition[result.Col] > APoint.X) then
    begin
      result.Col := -1;
    end;
    if (result.Col >= ColumnCount) then
    begin
      result.Col := -1;
    end;
  end;

  if RowDirection = rdSouthToNorth then
  begin
    if (result.Row > 0) and (result.Row <= RowCount)
      and (RowPosition[result.Row] > APoint.Y) then
    begin
      Dec(result.Row);
    end;
    if (result.Row = RowCount)
      and (RowPosition[result.Row] < APoint.Y) then
    begin
      result.Row := -1
    end;
    if (result.Row > RowCount) then
    begin
      result.Row := -1
    end;
  end
  else
  begin
    if (result.Row > 0) and (result.Row <= RowCount)
      and (RowPosition[result.Row] < APoint.Y) then
    begin
      Dec(result.Row);
    end;
    if (result.Row = RowCount)
      and (RowPosition[result.Row] > APoint.Y) then
    begin
      result.Row := -1;
    end;
    if (result.Row > RowCount) then
    begin
      result.Row := -1
    end;
  end;

end;

function TCustomModelGrid.UniformColumns(out MaxWidth,
  MinWidth: Double): boolean;
var
  ColIndex: Integer;
  AColWidth: double;
begin
  if ColumnCount >= 1 then
  begin
    MaxWidth := ColumnWidth[0];
    MinWidth := MaxWidth;
    for ColIndex := 1 to ColumnCount-1 do
    begin
      AColWidth := ColumnWidth[ColIndex];
      if AColWidth < MinWidth then
      begin
        MinWidth := AColWidth;
      end
      else if AColWidth > MaxWidth then
      begin
        MaxWidth := AColWidth;
      end;
    end;
    result := MaxWidth = MinWidth;
  end
  else
  begin
    MaxWidth := 0;
    MinWidth := 0;
    result := False;
  end;
end;

function TCustomModelGrid.UniformRows(out MaxWidth, MinWidth: Double): boolean;
var 
  RowIndex: Integer;
  ARowWidth: double;
begin
  if RowCount >= 1 then
  begin
    MaxWidth := RowWidth[0];
    MinWidth := MaxWidth;
    for RowIndex := 1 to RowCount-1 do
    begin
      ARowWidth := RowWidth[RowIndex];
      if ARowWidth < MinWidth then
      begin
        MinWidth := ARowWidth;
      end
      else if ARowWidth > MaxWidth then
      begin
        MaxWidth := ARowWidth;
      end;
    end;
    result := MaxWidth = MinWidth;
  end
  else
  begin
    MaxWidth := 0;
    MinWidth := 0;
    result := False;
  end;
end;

function TCustomModelGrid.UnrotatedTwoDElementCenter(const Column,
  Row: integer): TPoint2D;
begin
  result.X := (ColumnPosition[Column] + ColumnPosition[Column + 1]) / 2;
  result.Y := (RowPosition[Row] + RowPosition[Row + 1]) / 2;
end;

function TCustomModelGrid.TwoDElementCenter(const Column,
  Row: integer): TPoint2D;
begin
  result.X := (ColumnPosition[Column] + ColumnPosition[Column + 1]) / 2;
  result.Y := (RowPosition[Row] + RowPosition[Row + 1]) / 2;
  result := RotateFromGridCoordinatesToRealWorldCoordinates(result);
end;

function TCustomModelGrid.UnrotatedTwoDElementCorner(const Column,
  Row: integer): TPoint2D;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];
end;

function TCustomModelGrid.TwoDElementCorner(const Column,
  Row: integer): TPoint2D;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];
  result := RotateFromGridCoordinatesToRealWorldCoordinates(result);
end;

procedure TCustomModelGrid.UpdateColumnPositions;
var
  ARealList: TRealList;
  Index: integer;
begin
  FColumnCount := Length(FColumnPositions) - 1;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateFrontCellColors := True;
  NeedToRecalculate3DCellColors := True;
  frmGoPhast.UpdateDataSetDimensions;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  ARealList := TRealList.Create;
  try
    ARealList.Capacity := ColumnCount + 1;
    if ColumnDirection = cdWestToEast then
    begin
      for Index := 0 to ColumnCount do
      begin
        ARealList.Add(FColumnPositions[Index]);
      end;
    end
    else
    begin
      for Index := ColumnCount downto 0 do
      begin
        ARealList.Add(FColumnPositions[Index]);
      end;
    end;

    ARealList.Sort;

    for Index := ARealList.Count - 1 downto 1 do
    begin
      if ARealList[Index] = ARealList[Index - 1] then
      begin
        ARealList.Delete(Index);
      end;
    end;

    ColumnCount := ARealList.Count - 1;

    if ColumnDirection = cdWestToEast then
    begin
      for Index := 0 to ColumnCount do
      begin
        FColumnPositions[Index] := ARealList[Index];
      end;
    end
    else
    begin
      for Index := ColumnCount downto 0 do
      begin
        FColumnPositions[ColumnCount-Index] := ARealList[Index];
      end;
    end;
    if SelectedColumn < 0 then
    begin
      SelectedColumn := ColumnCount - 1;
    end;
  finally
    ARealList.Free;
  end;
  ElementCountChanged;
end;

procedure TCustomModelGrid.UpdateRowPositions;
var
  ARealList: TRealList;
  Index: integer;
begin
  FRowCount := Length(FRowPositions) - 1;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateSideCellColors := True;
  NeedToRecalculate3DCellColors := True;
  frmGoPhast.UpdateDataSetDimensions;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  ARealList := TRealList.Create;
  try
    ARealList.Capacity := RowCount + 1;
    if RowDirection = rdSouthToNorth then
    begin
      for Index := 0 to RowCount do
      begin
        ARealList.Add(FRowPositions[Index]);
      end;
    end
    else
    begin
      for Index := RowCount downto 0 do
      begin
        ARealList.Add(FRowPositions[Index]);
      end;
    end;

    ARealList.Sort;

    for Index := ARealList.Count - 1 downto 1 do
    begin
      if ARealList[Index] = ARealList[Index - 1] then
      begin
        ARealList.Delete(Index);
      end;
    end;

    RowCount := ARealList.Count - 1;

    if RowDirection = rdSouthToNorth then
    begin
      for Index := 0 to RowCount do
      begin
        FRowPositions[Index] := ARealList[Index];
      end;
    end
    else
    begin
      for Index := RowCount downto 0 do
      begin
        FRowPositions[RowCount-Index] := ARealList[Index];
      end;
    end;
    if SelectedRow < 0 then
    begin
      SelectedRow := RowCount - 1;
    end;

  finally
    ARealList.Free;
  end;
  ElementCountChanged;
end;

function TCustomModelGrid.TopContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt;
  const NeedToRotatePointToGridCoordinates: boolean = True;
  const BelowCol: integer = -1; const AboveCol: integer = -1;
  const BelowRow: integer = -1; const AboveRow: integer = -1): T2DTopCell;
begin
  if NeedToRotatePointToGridCoordinates then
  begin
    APoint := RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  end;
  result.Col := NearestColumnPosition(APoint.X);
  result.Row := NearestRowPosition(APoint.Y);
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        Exit;
      end;
  else
    Assert(False);
  end;

  if result.Col < 0 then
  begin
    result.Col := 0;
  end;
  if result.Row < 0 then
  begin
    result.Row := 0;
  end;

//  if (result.Col > -1) and (result.Row > -1) then
//  begin
    if result.Col >= ColumnCount then
    begin
      if EvaluatedAt = eaBlocks then
      begin
        result.Col := ColumnCount -1;
      end
      else
      begin
        Dec(result.Col);
      end;
    end;
    if result.Row >= RowCount then
    begin
      if EvaluatedAt = eaBlocks then
      begin
        result.Row := RowCount -1;
      end
      else
      begin
        Dec(result.Row);
      end;
    end;
    if ColumnDirection = cdWestToEast then
    begin
      if (result.Col > 0) and (ColumnPosition[result.Col] > APoint.X) then
      begin
        Dec(result.Col);
      end;
    end
    else
    begin
      if (result.Col > 0) and (ColumnPosition[result.Col] < APoint.X) then
      begin
        Dec(result.Col);
      end;
    end;
    if RowDirection = rdSouthToNorth then
    begin
      if (result.Row > 0) and (RowPosition[result.Row] > APoint.Y) then
      begin
        Dec(result.Row);
      end;
    end
    else
    begin
      if (result.Row > 0) and (RowPosition[result.Row] < APoint.Y) then
      begin
        Dec(result.Row);
      end;
    end;
//  end;
end;

procedure TCustomModelGrid.SetNeedToRecalculateTopCellColors(
  const Value: boolean);
begin
  if Value then
  begin
    FNeedToRecalculateTopCellColors := True;
  end;
end;

procedure TCustomModelGrid.SetOnSelectedColumnChange(const Value: TNotifyEvent);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  FOnSelectedColumnChange := Value;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.ModflowGrid.OnSelectedColumnChange := Value;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.SetOnSelectedLayerChange(const Value: TNotifyEvent);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  FOnSelectedLayerChange := Value;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.ModflowGrid.OnSelectedLayerChange := Value;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.SetOnSelectedRowChange(const Value: TNotifyEvent);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  FOnSelectedRowChange := Value;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.ModflowGrid.OnSelectedRowChange := Value;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.SetSelectedColumn(Value: integer);
var
  PathLines: TPathLineReader;
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else
  begin
    if ColumnCount > 0 then
    begin
      case frmGoPhast.frameSideView.EvaluatedAt of
        eaBlocks:
          begin
            if Value > ColumnCount - 1 then
            begin
              Value := ColumnCount - 1;
            end;
          end;
        eaNodes:
          begin
            if Value > ColumnCount then
            begin
              Value := ColumnCount;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
  if FSelectedColumn <> Value then
  begin
    FSideElementContourGrid := nil;
    FSideNodeContourGrid := nil;
    FSelectedColumn := Value;
    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FDisplayColumn := Value;
    end;
    FRecordedSideGrid := False;
    if Assigned(FOnSelectedColumnChange) then
    begin
      FOnSelectedColumnChange(self);
    end;
    PathLines := frmGoPhast.PhastModel.PathLines;
    if PathLines.DisplayPathLines
      and PathLines.DisplayLimits.LimitToCurrentIn2D then
    begin
      PathLines.SideQuadTree.Clear;
    end;
  end;
end;

procedure TCustomModelGrid.SetSelectedLayer(Value: integer);
var
  PathLines: TPathLineReader;
begin
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
  if FSelectedLayer <> Value then
  begin
    FSelectedLayer := Value;
    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FDisplayLayer := Value;
    end;
    FRecordedTopGrid := False;
    if Assigned(FOnSelectedLayerChange) then
    begin
      FOnSelectedLayerChange(self);
    end;
    PathLines := frmGoPhast.PhastModel.PathLines;
    if PathLines.DisplayPathLines
      and PathLines.DisplayLimits.LimitToCurrentIn2D then
    begin
      PathLines.TopQuadTree.Clear;
    end;
  end;
end;

procedure TCustomModelGrid.SetSelectedRow(Value: integer);
var
  PathLines: TPathLineReader;
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else
  begin
    if RowCount > 0 then
    begin
      case frmGoPhast.frameFrontView.EvaluatedAt of
        eaBlocks:
          begin
            if Value > RowCount - 1 then
            begin
              Value := RowCount - 1;
            end;
          end;
        eaNodes:
          begin
            if Value > RowCount then
            begin
              Value := RowCount;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
  if FSelectedRow <> Value then
  begin
    FFrontElementContourGrid := nil;
    FFrontNodeContourGrid := nil;
    FSelectedRow := Value;
    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FDisplayRow := Value;
    end;
    FRecordedFrontGrid := False;
    if Assigned(FOnSelectedRowChange) then
    begin
      FOnSelectedRowChange(self);
    end;
    PathLines := frmGoPhast.PhastModel.PathLines;
    if PathLines.DisplayPathLines
      and PathLines.DisplayLimits.LimitToCurrentIn2D then
    begin
      PathLines.FrontQuadTree.Clear;
    end;
  end;
end;

procedure TCustomModelGrid.SetNeedToRecalculateFrontCellColors(
  const Value: boolean);
begin
  if Value then
  begin
    FNeedToRecalculateFrontCellColors := True;
  end;
end;

function TCustomModelGrid.GetFrontCellColors(const Column,
  Layer: integer): TColor;
var
  ColumnLimit, LayerLimit: integer;
begin
  ColumnLimit := ColumnCount;
  LayerLimit := LayerCount;
  if FrontDataSet <> nil then
  begin
    if NeedToRecalculateFrontCellColors then
    begin
      UpdateCellColors(vdFront);
    end;
    if FrontDataSet <> nil then
    begin
      case FrontDataSet.EvaluatedAt of
        eaBlocks:
          begin
            ColumnLimit := ColumnCount;
            LayerLimit := LayerCount;
          end;
        eaNodes:
          begin
            ColumnLimit := ColumnCount + 1;
            LayerLimit := LayerCount + 1;
          end;
      else
        begin
          ColumnLimit := -1;
          LayerLimit := -1;
          Assert(False);
        end;
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidColumnNumbe, [Column]));
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidLayerNumber, [Layer]));
  end;
  Result := FFrontCellColors[Column, Layer];
end;

function TCustomModelGrid.GetFrontContourDataSet: TDataArray;
begin
  result := FFrontContourDataSet;
end;

function TCustomModelGrid.GetFrontDataSet: TDataArray;
begin
  result := FFrontDataSet;
end;

function TCustomModelGrid.GetGridAngleDegrees: double;
begin
  result := GridAngle/Pi*180;
end;

function TCustomModelGrid.GridLimits(ViewDirection: TViewDirection): TGridLimit;
var
  ACorner: TPoint2D;
begin
  if ViewDirection = vdTop then
  begin
    ACorner := TwoDElementCorner(0,0);
    result.MinX := ACorner.x;
    result.MaxX := ACorner.x;
    result.MinY := ACorner.y;
    result.MaxY := ACorner.y;
    ACorner := TwoDElementCorner(ColumnCount,0);
    result.MinX := Min(result.MinX, ACorner.x);
    result.MaxX := Max(result.MaxX, ACorner.x);
    result.MinY := Min(result.MinY, ACorner.y);
    result.MaxY := Max(result.MaxY, ACorner.y);
    ACorner := TwoDElementCorner(ColumnCount,RowCount);
    result.MinX := Min(result.MinX, ACorner.x);
    result.MaxX := Max(result.MaxX, ACorner.x);
    result.MinY := Min(result.MinY, ACorner.y);
    result.MaxY := Max(result.MaxY, ACorner.y);
    ACorner := TwoDElementCorner(0,RowCount);
    result.MinX := Min(result.MinX, ACorner.x);
    result.MaxX := Max(result.MaxX, ACorner.x);
    result.MinY := Min(result.MinY, ACorner.y);
    result.MaxY := Max(result.MaxY, ACorner.y);
  end
  else
  begin
    if ViewDirection = vdFront then
    begin
      case ColumnDirection of
        cdWestToEast:
          begin
            result.MinX := ColumnPosition[0];
            result.MaxX := ColumnPosition[ColumnCount];
          end;
        cdEastToWest:
          begin
            result.MinX := ColumnPosition[ColumnCount];
            result.MaxX := ColumnPosition[0];
          end;
      end;
    end
    else
    begin
      result.MinX := 0;
      result.MaxX := 0;
    end;
    if ViewDirection = vdSide then
    begin
      case RowDirection of
        rdSouthToNorth:
          begin
            result.MinY := RowPosition[0];
            result.MaxY := RowPosition[RowCount];
          end;
        rdNorthToSouth:
          begin
            result.MinY := RowPosition[RowCount];
            result.MaxY := RowPosition[0];
          end;
      end;
    end
    else
    begin
      result.MinY := 0;
      result.MaxY := 0;
    end;
    result.MinZ := LowestElevation;
    result.MaxZ := HighestElevation;
  end;
end;

function TCustomModelGrid.GridOutline(ViewDirection: TViewDirection): TPolygon2D;
begin
  SetLength(result, 4);
  case ViewDirection of
    vdTop:
      begin
        result[0] := TwoDElementCorner(0,0);
        result[1] := TwoDElementCorner(ColumnCount,0);
        result[2] := TwoDElementCorner(ColumnCount,RowCount);
        result[3] := TwoDElementCorner(0,RowCount);
      end;
    vdFront:
      begin
        result[0].x := ColumnPosition[0];
        result[0].y := HighestElevation;
        result[1].x := ColumnPosition[ColumnCount];
        result[1].y := HighestElevation;
        result[2].x := ColumnPosition[ColumnCount];
        result[2].y := LowestElevation;
        result[3].x := ColumnPosition[0];
        result[3].y := LowestElevation;
      end;
    vdSide:
      begin
        result[0].y := HighestElevation;
        result[0].x := RowPosition[0];
        result[1].y := HighestElevation;
        result[1].x := RowPosition[RowCount];
        result[2].y := LowestElevation;
        result[2].x := RowPosition[RowCount];
        result[3].y := LowestElevation;
        result[3].x := RowPosition[0];
      end;
  end;
end;

procedure TCustomModelGrid.GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
  StringValues: TStringList; out MinMaxInitialized: Boolean);
var
  LayerCount, RowCount, ColCount: integer;
begin
  Assert(DataSet <> nil);
  Assert(Model = DataSet.Model);
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

procedure TCustomModelGrid.GetHorizontalNeighbors(const Layer, Row, Col: integer;
  CellList: TCellLocationList);
var
  ACell: TCellLocation;
begin
  CellList.Clear;
  if Row > 0 then
  begin
    ACell.Layer := Layer;
    ACell.Row := Row-1;
    ACell.Column := Col;
    CellList.Add(ACell);
  end;
  if Col > 0 then
  begin
    ACell.Layer := Layer;
    ACell.Row := Row;
    ACell.Column := Col-1;
    CellList.Add(ACell);
  end;
  if Row < RowCount-1 then
  begin
    ACell.Layer := Layer;
    ACell.Row := Row+1;
    ACell.Column := Col;
    CellList.Add(ACell);
  end;
  if Col < ColumnCount-1 then
  begin
    ACell.Layer := Layer;
    ACell.Row := Row;
    ACell.Column := Col+1;
    CellList.Add(ACell);
  end;
end;

function TCustomModelGrid.OkLocation(const DataSet: TDataArray;
  const Layer, Row, Col: Integer): Boolean;
begin
  result := inherited;
  if result and not IsActiveOK(DataSet, Layer, Row, Col) then
  begin
    result := False;
  end;
end;

procedure TCustomModelGrid.CalculateMinMax(DataSet: TDataArray;
  var MinMaxInitialized: Boolean;
  var MinMax: TMinMax; StringValues: TStringList);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LocalPhastModel: TPhastModel;
  LocalDataArray: TDataArray;
  LayerCount, RowCount, ColCount: Integer;
begin
  if Model is TChildModel then
  begin
    LocalPhastModel := TChildModel(Model).ParentModel as TPhastModel;
  end
  else
  begin
    LocalPhastModel := Model as TPhastModel;
  end;
  if LocalPhastModel.LgrUsed then
  begin
    if LocalPhastModel.ThreeDTimeList <> nil then
    begin
      LocalPhastModel.ThreeDTimeList.Initialize;
    end;
    LocalDataArray := LocalPhastModel.Grid.ThreeDDataSet;
    if (LocalDataArray = nil)
      or ((DataSet.Model = LocalDataArray.Model) and (DataSet <> LocalDataArray)) then
    begin
      Exit;
    end;
    LocalDataArray.Initialize;
    LocalPhastModel.Grid.GetCounts(LocalDataArray, LayerCount, RowCount, ColCount);
    if (LayerCount > 0) and (RowCount > 0) and (ColCount > 0) then
    begin
      LocalPhastModel.Grid.SetMinMax(LocalDataArray, MinMaxInitialized,
        MinMax, StringValues, LayerCount, RowCount, ColCount);
    end;
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        if ChildModel.ThreeDTimeList <> nil then
        begin
          ChildModel.ThreeDTimeList.Initialize;
        end;
        LocalDataArray := ChildModel.Grid.ThreeDDataSet;
        if LocalDataArray <> nil then
        begin
          LocalDataArray.Initialize;
          ChildModel.Grid.GetCounts(LocalDataArray, LayerCount, RowCount, ColCount);
          if (LayerCount > 0) and (RowCount > 0) and (ColCount > 0) then
          begin
            ChildModel.Grid.SetMinMax(LocalDataArray, MinMaxInitialized,
              MinMax, StringValues, LayerCount, RowCount, ColCount);
          end;
        end;
      end;
    end;
  end
  else
  begin
    GetCounts(DataSet, LayerCount, RowCount, ColCount);
    SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
      LayerCount, RowCount, ColCount);
  end;
  if DataSet.Limits.LogTransform and (MinMax.RMinPositive > 0)
    and (MinMax.RMax > 0) then
  begin
    MinMax.LogRMin := Log10(MinMax.RMinPositive);
    MinMax.LogRMax := Log10(MinMax.RMax);
  end
  else
  begin
    MinMax.LogRMin := 0;
    MinMax.LogRMax := 0;
  end;
  if DataSet.Datatype = rdtString then
  begin
    if StringValues.Count > 0 then
    begin
      MinMax.SMin := StringValues[0];
      MinMax.SMax := StringValues[StringValues.Count-1];
    end;
  end;
end;

function TCustomModelGrid.GetChildDataArray(const Value: TDataArray;
  ChildModel: TBaseModel): TDataArray;
begin
  if Value = nil then
  begin
    result := nil;
  end
  else if Value.Name = '' then
  begin
    result := nil;
  end
  else
  begin
    result := (ChildModel as TChildModel).
      DataArrayManager.GetDataSetByName(Value.Name);
  end;
end;

procedure TCustomModelGrid.InvalidateScreenObjects;
begin
  if Model is TPhastModel then
  begin
    TPhastModel(Model).InvalidateScreenObjects;
  end;
end;

procedure TCustomModelGrid.DrawOrdinaryTopColumns(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  ColumnIndex: Integer;
  Point1: TPoint2D;
  Point2: TPoint2D;
  LineColor: TColor32;
  LineWidth: Single;
  EvalAt: TEvaluatedAt;
  RowIndex: Integer;
  Layer: Integer;
//  IsEdge: Boolean;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((ColumnIndex < ColumnCount) and
      IsElementActive(Layer,RowIndex, ColumnIndex))
      or ((ColumnIndex > 0)
      and IsElementActive(Layer,RowIndex, ColumnIndex-1))
  end;
  function IsEdge: boolean;
  begin
    result := ((ColumnIndex < ColumnCount) and
      IsElementActive(Layer,RowIndex, ColumnIndex))
      <> ((ColumnIndex > 0)
      and IsElementActive(Layer,RowIndex, ColumnIndex-1));
  end;
begin
  SetLocalEvalAt(vdTop, EvalAt);
  for ColumnIndex := 0 to ColumnCount do
  begin
    if (ColumnIndex mod 10 = 0) or (ColumnIndex = ColumnCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetColumnLineColor(ColumnIndex, EvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          Point1 := TwoDElementCorner(ColumnIndex, 0);
          Point2 := TwoDElementCorner(ColumnIndex, RowCount);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [ConvertTop2D_Point(ZoomBox, Point1),
            ConvertTop2D_Point(ZoomBox, Point2)], True);
        end;
      gldcExterior:
        begin
          if (ColumnIndex <> 0) and (ColumnIndex <> ColumnCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          Point1 := TwoDElementCorner(ColumnIndex, 0);
          Point2 := TwoDElementCorner(ColumnIndex, RowCount);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [ConvertTop2D_Point(ZoomBox, Point1),
            ConvertTop2D_Point(ZoomBox, Point2)], True);
        end;
      gldcActive:
        begin
          Layer := SelectedLayer;
          if Layer >= LayerCount then
          begin
            Dec(Layer);
          end;
          for RowIndex := 0 to RowCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              Point1 := TwoDElementCorner(ColumnIndex, RowIndex);
              Point2 := TwoDElementCorner(ColumnIndex, RowIndex+1);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth,
                [ConvertTop2D_Point(ZoomBox, Point1),
                ConvertTop2D_Point(ZoomBox, Point2)], True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Layer := SelectedLayer;
          if Layer >= LayerCount then
          begin
            Dec(Layer);
          end;
          for RowIndex := 0 to RowCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              Point1 := TwoDElementCorner(ColumnIndex, RowIndex);
              Point2 := TwoDElementCorner(ColumnIndex, RowIndex+1);
              DrawBigPolyline32(BitMap, LineColor, LineWidth,
                [ConvertTop2D_Point(ZoomBox, Point1),
                ConvertTop2D_Point(ZoomBox, Point2)], True);
            end;
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TCustomModelGrid.DrawOrdinaryTopRows(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  RowIndex: Integer;
  Point1: TPoint2D;
  Point2: TPoint2D;
  LineWidth: single;
  LineColor: TColor32;
  LocalEvalAt: TEvaluatedAt;
  ColumnIndex: Integer;
  Layer: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((RowIndex < RowCount) and
      IsElementActive(Layer,RowIndex, ColumnIndex))
      or ((RowIndex > 0)
      and IsElementActive(Layer,RowIndex-1, ColumnIndex))
  end;
  function IsEdge: boolean;
  begin
    result := ((RowIndex < RowCount) and
      IsElementActive(Layer,RowIndex, ColumnIndex))
      <> ((RowIndex > 0)
      and IsElementActive(Layer,RowIndex-1, ColumnIndex));
  end;
begin
  SetLocalEvalAt(vdTop, LocalEvalAt);
  for RowIndex := 0 to RowCount do
  begin
    if (RowIndex mod 10 = 0) or (RowIndex = RowCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetRowLineColor(RowIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          Point1 := TwoDElementCorner(0, RowIndex);
          Point2 := TwoDElementCorner(ColumnCount, RowIndex);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [ConvertTop2D_Point(ZoomBox, Point1),
            ConvertTop2D_Point(ZoomBox, Point2)], True);
        end;
      gldcExterior:
        begin
          if (RowIndex <> 0) and (RowIndex <> RowCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          Point1 := TwoDElementCorner(0, RowIndex);
          Point2 := TwoDElementCorner(ColumnCount, RowIndex);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [ConvertTop2D_Point(ZoomBox, Point1),
            ConvertTop2D_Point(ZoomBox, Point2)], True);
        end;
      gldcActive:
        begin
          Layer := SelectedLayer;
          if Layer >= LayerCount then
          begin
            Layer := Layer-1;
          end;
          for ColumnIndex := 0 to ColumnCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              Point1 := TwoDElementCorner(ColumnIndex, RowIndex);
              Point2 := TwoDElementCorner(ColumnIndex+1, RowIndex);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth,
                [ConvertTop2D_Point(ZoomBox, Point1),
                  ConvertTop2D_Point(ZoomBox, Point2)], True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Layer := SelectedLayer;
          if Layer >= LayerCount then
          begin
            Layer := Layer-1;
          end;
          for ColumnIndex := 0 to ColumnCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              Point1 := TwoDElementCorner(ColumnIndex, RowIndex);
              Point2 := TwoDElementCorner(ColumnIndex+1, RowIndex);
              DrawBigPolyline32(BitMap, LineColor, LineWidth,
                [ConvertTop2D_Point(ZoomBox, Point1),
                ConvertTop2D_Point(ZoomBox, Point2)], True);
            end;
          end;
        end
      else
        Assert(False);
    end;
  end;
end;

procedure TCustomModelGrid.RestoreGlGrid(var AGlGrid: TGrid;
  GridCacheStream: TMemoryStream);
var
  RowIndex: Integer;
  ColIndex: Integer;
  LayerCount: Integer;
  RowCount: Integer;
  ColCount: Integer;
  DecompressionStream: TDecompressionStream;
begin
  Assert(GridCacheStream <> nil);
  GridCacheStream.Position := 0;
  DecompressionStream := TDecompressionStream.Create(GridCacheStream);
  try
    DecompressionStream.Read(ColCount, SizeOf(ColCount));
    DecompressionStream.Read(RowCount, SizeOf(RowCount));
    DecompressionStream.Read(LayerCount, SizeOf(LayerCount));
    SetLength(AGlGrid, ColCount, RowCount, LayerCount);
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        DecompressionStream.Read(AGlGrid[ColIndex, RowIndex, 0], LayerCount * SizeOf(TGridPoint));
      end;
    end;
  finally
    DecompressionStream.Free;
  end;
end;

procedure TCustomModelGrid.CacheGlGrid(GridCacheStream: TMemoryStream;
  var AGlGrid: TGrid);
var
  RowIndex: Integer;
  ColIndex: Integer;
  Compressor: TCompressionStream;
  LayerCount: Integer;
  RowCount: Integer;
  ColCount: Integer;
  TempStream: TMemoryStream;
begin
  Assert(AGlGrid <> nil);
  ColCount := Length(AGlGrid);
  RowCount := Length(AGlGrid[0]);
  LayerCount := Length(AGlGrid[0, 0]);
  TempStream := TMemoryStream.Create;
  Compressor := TCompressionStream.Create(clDefault, GridCacheStream);
  try
    TempStream.Write(ColCount, SizeOf(ColCount));
    TempStream.Write(RowCount, SizeOf(RowCount));
    TempStream.Write(LayerCount, SizeOf(LayerCount));
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        TempStream.Write(AGlGrid[ColIndex, RowIndex, 0], LayerCount * SizeOf(TGridPoint));
      end;
    end;
    TempStream.SaveToStream(Compressor);
  finally
    Compressor.Free;
    TempStream.Free;
  end;
  AGlGrid := nil;
end;

procedure TCustomModelGrid.GetLimits(const EvaluatedAt: TEvaluatedAt;
  const ViewDirection: TViewDirection; out Limit1, Limit2: integer);
begin
  // Get the limits of one of the sparse 2D
  // elevation arrays in a TScreenObject.
  // Col, Row limits for top view
  // Col, Lay limits for front view
  // Row, Lay limits for side view
  case ViewDirection of
    vdTop:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Limit1 := ColumnCount-1;
              Limit2 := RowCount-1;
            end;
          eaNodes:
            begin
              Limit1 := ColumnCount;
              Limit2 := RowCount;
            end;
        else
          Assert(False);
        end;
      end;
    vdFront:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Limit1 := ColumnCount-1;
              Limit2 := LayerCount-1;
            end;
          eaNodes:
            begin
              Limit1 := ColumnCount;
              Limit2 := LayerCount;
            end;
        else
          Assert(False);
        end;
      end;
    vdSide:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Limit1 := RowCount-1;
              Limit2 := LayerCount-1;
            end;
          eaNodes:
            begin
              Limit1 := RowCount;
              Limit2 := LayerCount;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomModelGrid.SetFrontCellColors(const Column, Layer: integer;
  const Value: TColor);
var
  ColumnLimit, LayerLimit: integer;
begin
  if FrontDataSet = nil then
  begin
    ColumnLimit := ColumnCount;
    LayerLimit := LayerCount;
  end
  else
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount;
          LayerLimit := LayerCount;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount + 1;
          LayerLimit := LayerCount + 1;
        end;
    else
      begin
        ColumnLimit := -1;
        LayerLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidColumnNumbe, [Column]));
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidLayerNumber, [Layer]));
  end;
  FFrontCellColors[Column, Layer] := Value;
end;

procedure TCustomModelGrid.SetFrontContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoFront, dso3D]));
  if FFrontContourDataSet <> Value then
  begin
    FFrontContourDataSet := Value;
  end;
end;

procedure TCustomModelGrid.ResetFrontCellColors;
var
  ColIndex, LayerIndex: integer;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if (FColumnCount <= 0) or (FLayerCount <= 0) then
  begin
    SetLength(FFrontCellColors, 0, 0);
    FNeedToRecalculateFrontCellColors := False;
    Exit;
  end;

  if FrontDataSet <> nil then
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          if (Length(FFrontCellColors) <> FColumnCount)
            or (Length(FFrontCellColors[0]) <> FLayerCount) then
          begin
            SetLength(FFrontCellColors, FColumnCount, FLayerCount);
          end;
          for ColIndex := 0 to FColumnCount - 1 do
          begin
            for LayerIndex := 0 to FLayerCount - 1 do
            begin
              FrontCellColors[ColIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
      eaNodes:
        begin
          if (Length(FFrontCellColors) <> FColumnCount + 1)
            or (Length(FFrontCellColors[0]) <> FLayerCount + 1) then
          begin
            SetLength(FFrontCellColors, FColumnCount + 1, FLayerCount + 1);
          end;
          for ColIndex := 0 to FColumnCount do
          begin
            for LayerIndex := 0 to FLayerCount do
            begin
              FrontCellColors[ColIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  end;

  FNeedToRecalculateFrontCellColors := False;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildModel.Grid.ResetFrontCellColors;
        end;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.SetNeedToRecalculateSideCellColors(
  const Value: boolean);
begin
  if Value then
  begin
    FNeedToRecalculateSideCellColors := True;
  end;
end;

procedure TCustomModelGrid.ResetSideCellColors;
var
  RowIndex, LayerIndex: integer;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if (FRowCount <= 0) or (FLayerCount <= 0) then
  begin
    SetLength(FSideCellColors, 0, 0);
    FNeedToRecalculateSideCellColors := False;
    Exit;
  end;
  if SideDataSet <> nil then
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          if (Length(FSideCellColors) <> FRowCount)
            or (Length(FSideCellColors[0]) <> FLayerCount) then
          begin
            SetLength(FSideCellColors, FRowCount, FLayerCount);
          end;
          for RowIndex := 0 to FRowCount - 1 do
          begin
            for LayerIndex := 0 to FLayerCount - 1 do
            begin
              FSideCellColors[RowIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
      eaNodes:
        begin
          if (Length(FSideCellColors) <> FRowCount + 1)
            or (Length(FSideCellColors[0]) <> FLayerCount + 1) then
          begin
            SetLength(FSideCellColors, FRowCount + 1, FLayerCount + 1);
          end;
          for RowIndex := 0 to FRowCount do
          begin
            for LayerIndex := 0 to FLayerCount do
            begin
              FSideCellColors[RowIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  end;

  FNeedToRecalculateSideCellColors := False;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildModel.Grid.ResetSideCellColors;
        end;
      end;
    end;
  end;
end;

function TCustomModelGrid.GetShortestHorizontalBlockEdge(Layer, Row,
  Column: Integer): double;
begin
  result := Min(ColumnWidth[Column], RowWidth[Row]);
end;

function TCustomModelGrid.GetSideCellColors(const Row, Layer: integer): TColor;
var
  RowLimit, LayerLimit: integer;
begin
  RowLimit := RowCount;
  LayerLimit := LayerCount;
  if SideDataSet <> nil then
  begin
    if NeedToRecalculateSideCellColors then
    begin
      UpdateCellColors(vdSide);
    end;
    if SideDataSet <> nil then
    begin
      case SideDataSet.EvaluatedAt of
        eaBlocks:
          begin
            RowLimit := RowCount;
            LayerLimit := LayerCount;
          end;
        eaNodes:
          begin
            RowLimit := RowCount + 1;
            LayerLimit := LayerCount + 1;
          end;
      else
        begin
          RowLimit := -1;
          LayerLimit := -1;
          Assert(False);
        end;
      end;
    end;
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidRowNumber, [Row]));
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidLayerNumber, [Layer]));
  end;
  Result := FSideCellColors[Row, Layer];
end;

procedure TCustomModelGrid.SetSideCellColors(const Row, Layer: integer;
  const Value: TColor);
var
  RowLimit, LayerLimit: integer;
begin
  if SideDataSet = nil then
  begin
    RowLimit := RowCount;
    LayerLimit := LayerCount;
  end
  else
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          RowLimit := RowCount;
          LayerLimit := LayerCount;
        end;
      eaNodes:
        begin
          RowLimit := RowCount + 1;
          LayerLimit := LayerCount + 1;
        end;
    else
      begin
        RowLimit := -1;
        LayerLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidRowNumber, [Row]));
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidLayerNumber, [Layer]));
  end;
  FSideCellColors[Row, Layer] := Value;
end;

procedure TCustomModelGrid.SetSideContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoSide, dso3D]));
  if FSideContourDataSet <> Value then
  begin
    FSideContourDataSet := Value;
  end;
end;

procedure TCustomModelGrid.SetFrontDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoFront, dso3D]));
  if FFrontDataSet <> Value then
  begin
    FFrontDataSet := Value;
    NeedToRecalculateFrontCellColors := True;

    // This ensures that the selected row is still valid for the new data set.
    if frmGoPhast.Grid <> nil then
    begin
      frmGoPhast.Grid.SelectedRow := frmGoPhast.Grid.SelectedRow;
    end;

    frmGoPhast.frameFrontView.ItemChange(nil);
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TCustomModelGrid.SetSideDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoSide, dso3D]));
  if FSideDataSet <> Value then
  begin
    FSideDataSet := Value;
    NeedToRecalculateSideCellColors := True;

    // This ensures that the selected column
    // is still valid for the new data set.
    if frmGoPhast.Grid <> nil then
    begin
      frmGoPhast.Grid.SelectedColumn := frmGoPhast.Grid.SelectedColumn;
    end;

    frmGoPhast.frameSideView.ItemChange(nil);
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TCustomModelGrid.SetTopDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopDataSet <> Value then
  begin
    FTopDataSet := Value;
    NeedToRecalculateTopCellColors := True;

    // This ensures that the selected layer
    // is still valid for the new data set.
    if frmGoPhast.Grid <> nil then
    begin
      frmGoPhast.Grid.SelectedLayer := frmGoPhast.Grid.SelectedLayer;
    end;

    frmGoPhast.frameTopView.ItemChange(nil);
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TCustomModelGrid.SetTopGridObserver(const Value: TObserver);
begin
  FTopGridObserver := Value;
  if Assigned(FTopGridObserver) then
  begin
    FTopGridObserver.UpToDate := False;
    FTopGridObserver.UpToDate := True;
  end;
end;

//procedure TCustomModelGrid.InitializeMinMax(const Layer, Row, Col: integer;
//  DataSet: TDataArray; var MinMaxInitialized: boolean; var MinMax: TMinMax;
//  StringValues: TStringList);
//var
//  UseString: Boolean;
//  TempString: string;
//begin
//  if not OkLocation(DataSet, Layer, Row, Col) then
//  begin
//    Exit;
//  end;
//  MinMaxInitialized := True;
//  case DataSet.Datatype of
//    rdtDouble:
//      begin
//        MinMax.RMin := DataSet.RealData[Layer, Row, Col];
//        MinMax.RMax := MinMax.RMin;
//        if DataSet.Limits.LowerLimit.UseLimit then
//        begin
//          MinMax.RMin := DataSet.Limits.LowerLimit.RealLimitValue;
//        end;
//        if DataSet.Limits.UpperLimit.UseLimit then
//        begin
//          MinMax.RMax := DataSet.Limits.UpperLimit.RealLimitValue;
//        end;
//        if MinMax.RMin > 0 then
//        begin
//          MinMax.RMinPositive := MinMax.RMin;
//        end;
//      end;
//    rdtInteger:
//      begin
//        if DataSet.DisplayRealValue then
//        begin
//          MinMax.RMin := DataSet.RealData[Layer, Row, Col];
//          MinMax.RMax := MinMax.RMin;
//          if DataSet.Limits.LowerLimit.UseLimit then
//          begin
//            MinMax.RMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
//          end;
//          if DataSet.Limits.UpperLimit.UseLimit then
//          begin
//            MinMax.RMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
//          end;
//        end
//        else
//        begin
//          MinMax.IMin := DataSet.IntegerData[Layer, Row, Col];
//          MinMax.IMax := MinMax.IMin;
//          if DataSet.Limits.LowerLimit.UseLimit then
//          begin
//            MinMax.IMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
//          end;
//          if DataSet.Limits.UpperLimit.UseLimit then
//          begin
//            MinMax.IMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
//          end;
//        end;
//      end;
//    rdtBoolean:
//      begin
//        MinMax.BMin := False;
//        MinMax.BMax := True;
//        if DataSet.Limits.LowerLimit.UseLimit then
//        begin
//          MinMax.BMin := DataSet.Limits.LowerLimit.BooleanLimitValue;
//        end;
//        if DataSet.Limits.UpperLimit.UseLimit then
//        begin
//          MinMax.BMax := DataSet.Limits.UpperLimit.BooleanLimitValue;
//        end;
//      end;
//    rdtString:
//      begin
//        UseString := True;
//        TempString := DataSet.StringData[Layer, Row, Col];
//        if DataSet.Limits.UpperLimit.UseLimit then
//        begin
//          if TempString > DataSet.Limits.UpperLimit.StringLimitValue then
//          begin
//            UseString := False;
//          end;
//        end;
//        if DataSet.Limits.LowerLimit.UseLimit then
//        begin
//          if TempString < DataSet.Limits.LowerLimit.StringLimitValue then
//          begin
//            UseString := False;
//          end;
//        end;
//        if UseString then
//        begin
//          StringValues.Add(TempString);
//        end;
//        if DataSet.Limits.LowerLimit.UseLimit then
//        begin
//          MinMax.SMin := DataSet.Limits.LowerLimit.StringLimitValue;
//        end
//        else
//        begin
//          MinMax.SMin := TempString;
//        end;
//        if DataSet.Limits.UpperLimit.UseLimit then
//        begin
//          MinMax.SMax := DataSet.Limits.UpperLimit.StringLimitValue;
//        end
//        else
//        begin
//          MinMax.SMax := TempString;
//        end;
//      end;
//  else
//    Assert(False);
//  end;
//end;

procedure TCustomModelGrid.UpdateMinMax(const Layer, Row, Col: integer;
  DataSet: TDataArray; var MinMaxInitialized: boolean; var MinMax: TMinMax;
  StringValues: TStringList);
begin
  Assert(Layer <= LayerCount);
  Assert(Row <= RowCount);
  Assert(Col <= ColumnCount);
  inherited;
end;

procedure TCustomModelGrid.SetMinMax(DataSet: TDataArray;
  var MinMaxInitialized: boolean; var MinMax: TMinMax;
  StringValues: TStringList; LayerCount, RowCount, ColCount: integer);
begin
  Assert(LayerCount <= Self.LayerCount + 1);
  Assert(RowCount <= Self.RowCount + 1);
  Assert(ColCount <= Self.ColumnCount + 1);
  inherited;
end;

procedure TCustomModelGrid.GetRealMinMax(DataSet: TDataArray; var MinMax: TMinMax);
var
  LayerCount, RowCount, ColCount: integer;
  MinMaxInitialized: boolean;
  StringValues: TStringList;
begin
  Assert(DataSet.DataType = rdtDouble);
  GetCounts(DataSet, LayerCount, RowCount, ColCount);
  MinMaxInitialized := False;
  StringValues := nil;
  SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
    LayerCount, RowCount, ColCount);
end;

procedure TCustomModelGrid.GetIntegerMinMax(DataSet: TDataArray;
  var MinMax: TMinMax);
var
  LayerCount, RowCount, ColCount: integer;
  MinMaxInitialized: boolean;
  StringValues: TStringList;
begin
  Assert(DataSet.DataType = rdtInteger);
  GetCounts(DataSet, LayerCount, RowCount, ColCount);
  MinMaxInitialized := False;
  StringValues := nil;
  SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
    LayerCount, RowCount, ColCount);
end;

function TCustomModelGrid.GetItemTopLocation(const EvalAt: TEvaluatedAt;
  const Column, Row: integer): TPoint2D;
begin
  case EvalAt of
    eaBlocks:
      begin
        result := TwoDElementCenter(Column, Row);
      end;
    eaNodes:
      begin
        result := TwoDElementCorner(Column, Row);
      end;
    else
      Assert(False);
  end;
end;

procedure TCustomModelGrid.GetBooleanMinMax(DataSet: TDataArray;
  var MinMax: TMinMax);
var
  LayerCount, RowCount, ColCount: integer;
  MinMaxInitialized: boolean;
  StringValues: TStringList;
begin
  Assert(DataSet.DataType = rdtBoolean);
  GetCounts(DataSet, LayerCount, RowCount, ColCount);
  MinMaxInitialized := False;
  StringValues := nil;
  SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
    LayerCount, RowCount, ColCount);
end;

procedure TCustomModelGrid.GetStringMinMax(DataSet: TDataArray;
  var MinMax: TMinMax);
var
  LayerCount, RowCount, ColCount: integer;
  MinMaxInitialized: boolean;
  StringValues: TStringList;
begin
  Assert(DataSet.DataType = rdtString);
  GetCounts(DataSet, LayerCount, RowCount, ColCount);
  MinMaxInitialized := False;

  StringValues := TStringList.Create;
  try
    SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
      LayerCount, RowCount, ColCount);

  finally
    StringValues.Free;
  end;
end;

procedure TCustomModelGrid.GetCounts(DataSet: TDataArray; var LayerCount, RowCount, ColCount: integer);
begin
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        LayerCount := DataSet.LayerCount;
        RowCount := DataSet.RowCount;
        ColCount := DataSet.ColumnCount;
      end;
    eaNodes:
      begin
        case DataSet.Orientation of
          dsoTop:
            begin
              {if (DataSet = frmGoPhast.PhastModel.DataArrayManager.
                GetDataSetByName(rsInitial_Water_Table)) then
              begin
//             This is a clumsy hack. It should be updated.
                LayerCount := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
//                LayerCount := ThreeDDataSet.LayerCount;
              end
              else} if
                ((DataSet is TSparseArrayPhastInterpolationDataSet) and
                ((frmGoPhast.PhastModel.RiverHead.
                IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                DataSet)) >= 0)
                or (frmGoPhast.PhastModel.RiverAssociatedSolution.
                IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                DataSet)) >= 0)))
                or (frmGoPhast.PhastModel.DataArrayManager.RiverDataSets.IndexOf(
                DataSet) >= 0) then
              begin
                LayerCount := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
              end
              else
              begin
                LayerCount := DataSet.LayerCount;
              end;
              RowCount := DataSet.RowCount + 1;
              ColCount := DataSet.ColumnCount + 1;
            end;
          dsoFront:
            begin
              LayerCount := DataSet.LayerCount + 1;
              RowCount := DataSet.RowCount;
              ColCount := DataSet.ColumnCount + 1;
            end;
          dsoSide:
            begin
              LayerCount := DataSet.LayerCount + 1;
              RowCount := DataSet.RowCount + 1;
              ColCount := DataSet.ColumnCount;
            end;
          dso3D:
            begin
              LayerCount := DataSet.LayerCount + 1;
              RowCount := DataSet.RowCount + 1;
              ColCount := DataSet.ColumnCount + 1;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;


function TCustomModelGrid.GetElementCoordinates(Col, Row,
  Layer: integer): T3DElementCoordinates;
var
  Z1: Real;
  Z2: Real;
  TempPoint: TPoint2D;
  function AveragePoint(Point1, Point2: T3DRealPoint): T3DRealPoint;
  begin
    result.x := (Point1.x + Point2.x)/2;
    result.y := (Point1.y + Point2.y)/2;
  end;
begin
  Z1 := CellElevation[Col, Row, Layer];
  Z2 := CellElevation[Col, Row, Layer+1];
  TempPoint := TwoDElementCenter(Col, Row);
  result.TopCenter.x := TempPoint.x;
  result.TopCenter.y := TempPoint.y;
  result.TopCenter.z := Z1;

  result.BottomCenter.x := TempPoint.x;
  result.BottomCenter.y := TempPoint.y;
  result.BottomCenter.z := Z2;

  result.TopEdge[0] := RotatedThreeDElementCorner(Col, Row, Layer);
  result.TopEdge[2] := RotatedThreeDElementCorner(Col+1, Row, Layer);
  result.TopEdge[4] := RotatedThreeDElementCorner(Col+1, Row+1, Layer);
  result.TopEdge[6] := RotatedThreeDElementCorner(Col, Row+1, Layer);

  result.TopEdge[1] := AveragePoint(result.TopEdge[0], result.TopEdge[2]);
  if Row = 0 then
  begin
    result.TopEdge[1].Z := Z1;
  end
  else
  begin
    result.TopEdge[1].Z := (Z1 + CellElevation[Col, Row-1, Layer])/2;
  end;

  result.TopEdge[3] := AveragePoint(result.TopEdge[2], result.TopEdge[4]);
  if Col = ColumnCount -1 then
  begin
    result.TopEdge[3].Z := Z1;
  end
  else
  begin
    result.TopEdge[3].Z := (Z1 + CellElevation[Col+1, Row, Layer])/2;
  end;


  result.TopEdge[5] := AveragePoint(result.TopEdge[4], result.TopEdge[6]);
  if Row = RowCount -1 then
  begin
    result.TopEdge[5].Z := Z1;
  end
  else
  begin
    result.TopEdge[5].Z := (Z1 + CellElevation[Col, Row+1, Layer])/2;
  end;

  result.TopEdge[7] := AveragePoint(result.TopEdge[6], result.TopEdge[0]);
  if Col = 0 then
  begin
    result.TopEdge[7].Z := Z1;
  end
  else
  begin
    result.TopEdge[7].Z := (Z1 + CellElevation[Col-1, Row, Layer])/2;
  end;

  result.BottomEdge[0] := RotatedThreeDElementCorner(Col, Row, Layer+1);
  result.BottomEdge[2] := RotatedThreeDElementCorner(Col+1, Row, Layer+1);
  result.BottomEdge[4] := RotatedThreeDElementCorner(Col+1, Row+1, Layer+1);
  result.BottomEdge[6] := RotatedThreeDElementCorner(Col, Row+1, Layer+1);

  result.BottomEdge[1] := AveragePoint(result.BottomEdge[0], result.BottomEdge[2]);
  if Row = 0 then
  begin
    result.BottomEdge[1].Z := Z2;
  end
  else
  begin
    result.BottomEdge[1].Z := (Z2 + CellElevation[Col, Row-1, Layer+1])/2;
  end;

  result.BottomEdge[3] := AveragePoint(result.BottomEdge[2], result.BottomEdge[4]);
  if Col = ColumnCount -1 then
  begin
    result.BottomEdge[3].Z := Z2;
  end
  else
  begin
    result.BottomEdge[3].Z := (Z2 + CellElevation[Col+1, Row, Layer+1])/2;
  end;

  result.BottomEdge[5] := AveragePoint(result.BottomEdge[4], result.BottomEdge[6]);
  if Row = RowCount -1 then
  begin
    result.BottomEdge[5].Z := Z2;
  end
  else
  begin
    result.BottomEdge[5].Z := (Z2 + CellElevation[Col, Row+1, Layer+1])/2;
  end;

  result.BottomEdge[7] := AveragePoint(result.BottomEdge[6], result.BottomEdge[0]);
  if Col = 0 then
  begin
    result.BottomEdge[7].Z := Z2;
  end
  else
  begin
    result.BottomEdge[7].Z := (Z2 + CellElevation[Col-1, Row, Layer+1])/2;
  end;

end;

function TCustomModelGrid.GetElevationsNeedUpdating: Boolean;
begin
  result := False;
end;

procedure TCustomModelGrid.UpdateCellColors(
  const ViewDirection: TViewDirection);
var
  DataSet: TDataArray;
  LayerCount, RowCount, ColCount: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  RTemp: real;
  ITemp: integer;
  BTemp: boolean;
  STemp: string;
  SIndex: integer;
  StringValues: TStringList;
  LogRTemp: double;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LocalPhastModel: TPhastModel;
  MinMax: TMinMax;
  MinMaxInitialized: Boolean;
begin
  StringValues := TStringList.Create;
  try
    DataSet := nil;
    case ViewDirection of
      vdTop: DataSet := TopDataSet;
      vdFront: DataSet := FrontDataSet;
      vdSide: DataSet := SideDataSet;
      else Assert(False);
    end;
    if DataSet = nil then
      Exit;

    Assert(Model = DataSet.Model);
    DataSet.Initialize;
    GetCounts(DataSet, LayerCount, RowCount, ColCount);
    if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
    begin
      Exit;
    end;

    GetMinMax(MinMax, DataSet, StringValues, MinMaxInitialized);
    ApplyLimittoMinMax(DataSet, MinMax, DataSet.Limits);

    case ViewDirection of
      vdTop:
        begin
          if LayerCount > 1 then
          begin
            LayerIndex := SelectedLayer;
          end
          else
          begin
            LayerIndex := 0;
          end;
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColCount - 1 do
            begin
              case DataSet.Datatype of
                rdtDouble:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet,
                      LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := clWhite;
                    end
                    else if (MinMax.RMax = MinMax.RMin) then
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if RTemp = MinMax.RMin then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                      begin
                        if DataSet.Limits.LogTransform  then
                        begin
                          Assert(MinMax.RMinPositive > 0);
                          if RTemp > 0 then
                          begin
                            Assert(RTemp > 0);
                            LogRTemp := Log10(RTemp);
                            if MinMax.LogRMax = MinMax.LogRMin then
                            begin
                              TopCellColors[ColIndex, RowIndex] :=
                                GridFracToRainbow(0.5);
                            end
                            else
                            begin
                              TopCellColors[ColIndex, RowIndex] :=
                                GridFracToRainbow((MinMax.LogRMax - LogRTemp)
                                / (MinMax.LogRMax - MinMax.LogRMin));
                            end;
                          end
                          else
                          begin
                            TopCellColors[ColIndex, RowIndex] := clWhite;
                          end;
                        end
                        else
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((MinMax.RMax - RTemp)
                            / (MinMax.RMax - MinMax.RMin));
                        end;
                      end;
                    end;
                  end;
                rdtInteger:
                  begin
                    if DataSet.DisplayRealValue then
                    begin
                      if DataSet.Limits.ActiveOnly
                        and DataSet.Limits.ShadeInactiveArea
                        and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        TopCellColors[ColIndex, RowIndex] := InactiveGridColor;
                      end
                      else if not OkLocation(DataSet,
                        LayerIndex, RowIndex, ColIndex) then
                      begin
                        TopCellColors[ColIndex, RowIndex] := clWhite;
                      end
                      else if (MinMax.RMax = MinMax.RMin) then
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if RTemp = MinMax.RMin then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                        end;
                      end;
                    end
                    else
                    begin
                      if DataSet.Limits.ActiveOnly
                        and DataSet.Limits.ShadeInactiveArea
                        and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        TopCellColors[ColIndex, RowIndex] := InactiveGridColor;
                      end
                      else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        TopCellColors[ColIndex, RowIndex] := clWhite;
                      end
                      else if (MinMax.IMax = MinMax.IMin) then
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if ITemp = MinMax.IMin then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if (ITemp >= MinMax.IMin) and (ITemp <= MinMax.IMax) then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((MinMax.IMax - ITemp) / (MinMax.IMax - MinMax.IMin));
                        end;
                      end;
                    end;
                  end;
                rdtBoolean:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := clWhite;
                    end
                    else if MinMax.BMax = MinMax.BMin then
                    begin
                      BTemp := DataSet.BooleanData
                        [LayerIndex, RowIndex, ColIndex];
                      if BTemp = MinMax.BMin then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex]
                        then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(1);
                      end
                      else
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0);
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex)
                      or (StringValues.Count = 0) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := clWhite;
                    end
                    else if StringValues.Count = 1 then
                    begin
                      STemp := DataSet.StringData[
                        LayerIndex, RowIndex, ColIndex];
                      if StringValues.IndexOf(STemp) >= 0 then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      STemp := DataSet.StringData[LayerIndex, RowIndex,
                        ColIndex];
                      SIndex := StringValues.IndexOf(STemp);
                      if SIndex >= 0 then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(1-(SIndex / (StringValues.Count - 1)));
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          FNeedToRecalculateTopCellColors := False;
        end;
      vdFront:
        begin
          if RowCount > 1 then
          begin
            RowIndex := SelectedRow;
          end
          else
          begin
            RowIndex := 0;
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            for ColIndex := 0 to ColCount - 1 do
            begin
              case DataSet.Datatype of
                rdtDouble:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := clWhite;
                    end
                    else if (MinMax.RMax = MinMax.RMin) then
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if RTemp = MinMax.RMin then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                      begin
                        if DataSet.Limits.LogTransform  then
                        begin
                          Assert(MinMax.RMinPositive > 0);
                          if RTemp > 0 then
                          begin
                            Assert(RTemp > 0);
                            LogRTemp := Log10(RTemp);
                            if MinMax.LogRMax = MinMax.LogRMin then
                            begin
                              FrontCellColors[ColIndex, LayerIndex] :=
                                GridFracToRainbow(0.5);
                            end
                            else
                            begin
                              FrontCellColors[ColIndex, LayerIndex] :=
                                GridFracToRainbow((MinMax.LogRMax - LogRTemp) / (MinMax.LogRMax - MinMax.LogRMin));
                            end;
                          end
                          else
                          begin
                            FrontCellColors[ColIndex, LayerIndex] := clWhite;
                          end;
                        end
                        else
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                        end;
                      end;
                    end;
                  end;
                rdtInteger:
                  begin
                    if DataSet.DisplayRealValue then
                    begin
                      if DataSet.Limits.ActiveOnly
                        and DataSet.Limits.ShadeInactiveArea
                        and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] := InactiveGridColor;
                      end
                      else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] := clWhite;
                      end
                      else if (MinMax.RMax = MinMax.RMin) then
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if RTemp = MinMax.RMin then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                        end;
                      end;
                    end
                    else
                    begin
                      if DataSet.Limits.ActiveOnly
                        and DataSet.Limits.ShadeInactiveArea
                        and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] := InactiveGridColor;
                      end
                      else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] := clWhite;
                      end
                      else if (MinMax.IMax = MinMax.IMin) then
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if ITemp = MinMax.IMin then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if (ITemp >= MinMax.IMin) and (ITemp <= MinMax.IMax) then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((MinMax.IMax - ITemp) / (MinMax.IMax - MinMax.IMin));
                        end;
                      end;
                    end;
                  end;
                rdtBoolean:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := clWhite;
                    end
                    else if MinMax.BMax = MinMax.BMin then
                    begin
                      BTemp := DataSet.BooleanData
                        [LayerIndex, RowIndex, ColIndex];
                      if BTemp = MinMax.BMin then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex]
                        then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(1);
                      end
                      else
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0);
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex)
                      or (StringValues.Count = 0) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := clWhite;
                    end
                    else if StringValues.Count = 1 then
                    begin
                      STemp := DataSet.StringData[
                        LayerIndex, RowIndex, ColIndex];
                      if StringValues.IndexOf(STemp) >= 0 then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      STemp := DataSet.StringData[LayerIndex, RowIndex,
                        ColIndex];
                      SIndex := StringValues.IndexOf(STemp);
                      if SIndex >= 0 then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(1-(SIndex / (StringValues.Count - 1)));
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          FNeedToRecalculateFrontCellColors := False;
        end;
      vdSide:
        begin
          if ColCount > 1 then
          begin
            ColIndex := SelectedColumn;
          end
          else
          begin
            ColIndex := 0;
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            for RowIndex := 0 to RowCount - 1 do
            begin
              case DataSet.Datatype of
                rdtDouble:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := clWhite;
                    end
                    else if (MinMax.RMax = MinMax.RMin) then
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if RTemp = MinMax.RMin then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                      begin
                       if DataSet.Limits.LogTransform  then
                        begin
                          Assert(MinMax.RMinPositive > 0);
                          if RTemp > 0 then
                          begin
                            Assert(RTemp > 0);
                            LogRTemp := Log10(RTemp);
                            if MinMax.LogRMax = MinMax.LogRMin then
                            begin
                              SideCellColors[RowIndex, LayerIndex]  :=
                                GridFracToRainbow(0.5);
                            end
                            else
                            begin
                              SideCellColors[RowIndex, LayerIndex]  :=
                                GridFracToRainbow((MinMax.LogRMax - LogRTemp) / (MinMax.LogRMax - MinMax.LogRMin));
                            end;
                          end
                          else
                          begin
                            SideCellColors[RowIndex, LayerIndex] := clWhite;
                          end;
                        end
                        else
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                        end;
                      end;
                    end;
                  end;
                rdtInteger:
                  begin
                    if DataSet.DisplayRealValue then
                    begin
                      if DataSet.Limits.ActiveOnly
                        and DataSet.Limits.ShadeInactiveArea
                        and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        SideCellColors[RowIndex, LayerIndex] := InactiveGridColor;
                      end
                      else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        SideCellColors[RowIndex, LayerIndex] := clWhite;
                      end
                      else if (MinMax.RMax = MinMax.RMin) then
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if RTemp = MinMax.RMin then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                        end;
                      end;
                    end
                    else
                    begin
                      if DataSet.Limits.ActiveOnly
                        and DataSet.Limits.ShadeInactiveArea
                        and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        SideCellColors[RowIndex, LayerIndex] := InactiveGridColor;
                      end
                      else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        SideCellColors[RowIndex, LayerIndex] := clWhite;
                      end
                      else if (MinMax.IMax = MinMax.IMin) then
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if ITemp = MinMax.IMin then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if (ITemp >= MinMax.IMin) and (ITemp <= MinMax.IMax) then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow((MinMax.IMax - ITemp) / (MinMax.IMax - MinMax.IMin));
                        end;
                      end;
                    end;
                  end;
                rdtBoolean:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := clWhite;
                    end
                    else if MinMax.BMax = MinMax.BMin then
                    begin
                      BTemp := DataSet.BooleanData
                        [LayerIndex, RowIndex, ColIndex];
                      if BTemp = MinMax.BMin then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex]
                        then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(1);
                      end
                      else
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0);
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if DataSet.Limits.ActiveOnly
                      and DataSet.Limits.ShadeInactiveArea
                      and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := InactiveGridColor;
                    end
                    else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex)
                      or (StringValues.Count = 0) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := clWhite;
                    end
                    else if StringValues.Count = 1 then
                    begin
                      STemp := DataSet.StringData[
                        LayerIndex, RowIndex, ColIndex];
                      if StringValues.IndexOf(STemp) >= 0 then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      STemp := DataSet.StringData[LayerIndex, RowIndex,
                        ColIndex];
                      SIndex := StringValues.IndexOf(STemp);
                      if SIndex >= 0 then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(1-(SIndex / (StringValues.Count - 1)));
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          FNeedToRecalculateSideCellColors := False;
        end;
    else
      Assert(False);
    end;
  finally
    StringValues.Free;
  end;
  if Model is TPhastModel then
  begin
    LocalPhastModel := TPhastModel(Model);
    if LocalPhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildModel.Grid.UpdateCellColors(ViewDirection);
        end;
      end;
    end;
  end;
end;

function TCustomModelGrid.IsElementActive(const Layer, Row, Col: integer): boolean;
var
  ActiveDataSet: TDataArray;
  LocalModel: TCustomModel;
begin
  result := True;
  LocalModel := Model as TCustomModel;
  if LocalModel.ModelSelection = msModflow2015 then
  begin
    ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    if ActiveDataSet <> nil then
    begin
      ActiveDataSet.Initialize;
      result := ActiveDataSet.IntegerData[Layer, Row, Col] >= 1;
    end;
  end
  else
  begin
    ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
    if ActiveDataSet <> nil then
    begin
      ActiveDataSet.Initialize;
      result := ActiveDataSet.BooleanData[Layer, Row, Col];
    end;
  end;
end;

function TCustomModelGrid.IsActiveOk(const DataSet: TDataArray;
  const Layer, Row, Col: integer): boolean;
var
  ActiveDataSet: TDataArray;
  LayerIndicies: TIntegerList;
  RowIndicies: TIntegerList;
  ColumnIndicies: TIntegerList;
  LayToCheck: Integer;
  RowToCheck: Integer;
  ColToCheck: Integer;
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
  IDomainDataArray: TDataArray;
begin
  result := True;
  if DataSet.Limits.ActiveOnly then
  begin
    ActiveDataSet := (Model as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
    if ActiveDataSet <> nil then
    begin
      ActiveDataSet.Initialize;
      if Model.ModelSelection = msModflow2015 then
      begin
        IDomainDataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(K_IDOMAIN);
        IDomainDataArray.Initialize;
      end
      else
      begin
        IDomainDataArray := nil;
      end;
      result := False;
      LayerIndicies := TIntegerList.Create;
      RowIndicies := TIntegerList.Create;
      ColumnIndicies := TIntegerList.Create;
      try
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              LayerIndicies.Add(Layer);
              RowIndicies.Add(Row);
              ColumnIndicies.Add(Col);
              case DataSet.Orientation of
                dsoTop:
                  begin
                    LayerIndicies.Clear;
                    LayerIndicies.Add(SelectedLayer);
                  end;
                dsoFront:
                  begin
                    RowIndicies.Clear;
                    RowIndicies.Add(SelectedRow)
                  end;
                dsoSide:
                  begin
                    ColumnIndicies.Clear;
                    ColumnIndicies.Add(SelectedColumn);
                  end;
                dso3D:
                  begin
                    // do nothing
                  end
                else Assert(False);
              end;
            end;
          eaNodes:
            begin
              if Layer < LayerCount then
              begin
                LayerIndicies.Add(Layer);
              end;
              if Layer > 0 then
              begin
                LayerIndicies.Add(Layer-1);
              end;
              if Row < RowCount then
              begin
                RowIndicies.Add(Row);
              end;
              if Row > 0 then
              begin
                RowIndicies.Add(Row-1);
              end;
              if Col < ColumnCount then
              begin
                ColumnIndicies.Add(Col);
              end;
              if Col > 0 then
              begin
                ColumnIndicies.Add(Col-1);
              end;
              case DataSet.Orientation of
                dsoTop:
                  begin
                    LayerIndicies.Clear;
                    LayerIndicies.Add(SelectedLayer);
                  end;
                dsoFront:
                  begin
                    RowIndicies.Clear;
                    RowIndicies.Add(SelectedRow)
                  end;
                dsoSide:
                  begin
                    ColumnIndicies.Clear;
                    ColumnIndicies.Add(SelectedColumn);
                  end;
                dso3D:
                  begin
                    // do nothing
                  end
                else Assert(False);
              end;
            end;
          else Assert(False);
        end;
        for LayerIndex := 0 to LayerIndicies.Count - 1 do
        begin
          LayToCheck := LayerIndicies[LayerIndex];
          for RowIndex := 0 to RowIndicies.Count - 1 do
          begin
            RowToCheck := RowIndicies[RowIndex];
            for ColIndex := 0 to ColumnIndicies.Count - 1 do
            begin
              ColToCheck := ColumnIndicies[ColIndex];
              result := ActiveDataSet.BooleanData[
                LayToCheck, RowToCheck, ColToCheck];
              if result and (IDomainDataArray <> nil) then
              begin
                result := IDomainDataArray.IntegerData[LayToCheck, RowToCheck, ColToCheck] > 0;
              end;
              if result then Exit;
            end;
          end;
        end;
      finally
        LayerIndicies.Free;
        ColumnIndicies.Free;
        RowIndicies.Free;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.Update3DCellColors(var CellColors: TCellColors);
var
  DataSet: TDataArray;
  LayerCount, RowCount, ColCount: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  RTemp: real;
  ITemp: integer;
  BTemp: boolean;
  STemp: string;
  SIndex: integer;
  StringValues: TStringList;
  MinMaxInitialized: boolean;
  LogRTemp: double;
  MinMax: TMinMax;
begin
  MinMax.RMinPositive := 0;
  MinMaxInitialized := False;
  StringValues := TStringList.Create;
  try
    StringValues.Sorted := True;
    StringValues.Duplicates := dupIgnore;
    StringValues.CaseSensitive := True;

    DataSet := ThreeDDataSet;
    if DataSet = nil then
      Exit;

    GetCounts(DataSet, LayerCount, RowCount, ColCount);
    if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
    begin
      Exit;
    end;

    SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
      LayerCount, RowCount, ColCount);

    ApplyLimitToMinMax(DataSet, MinMax, DataSet.Limits);

    if DataSet.Limits.LogTransform and (MinMax.RMinPositive > 0)
      and (MinMax.RMax > 0) then
    begin
      MinMax.LogRMin := Log10(MinMax.RMinPositive);
      MinMax.LogRMax := Log10(MinMax.RMax);
    end
    else
    begin
      MinMax.LogRMin := 0;
      MinMax.LogRMax := 0;
    end;
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColCount - 1 do
        begin
          case DataSet.Datatype of
            rdtDouble:
              begin
                if DataSet.Limits.ActiveOnly
                  and DataSet.Limits.ShadeInactiveArea
                  and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := InactiveGridColor;
                end
                else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                end
                else if (MinMax.RMax = MinMax.RMin) then
                begin
                  RTemp := DataSet.RealData
                    [LayerIndex, RowIndex, ColIndex];
                  if RTemp = MinMax.RMin then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0.5);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end
                else
                begin
                  RTemp := DataSet.RealData
                    [LayerIndex, RowIndex, ColIndex];
                  if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                  begin
                    if DataSet.Limits.LogTransform then
                    begin
                      Assert(MinMax.RMinPositive > 0);
                      if RTemp > 0 then
                      begin
                        Assert(RTemp > 0);
                        LogRTemp := Log10(RTemp);
                        if MinMax.LogRMax = MinMax.LogRMin then
                        begin
                          CellColors[LayerIndex, RowIndex, ColIndex] :=
                            GridFracToRainbow(0.5);
                        end
                        else
                        begin
                          CellColors[LayerIndex, RowIndex, ColIndex] :=
                            GridFracToRainbow((MinMax.LogRMax - LogRTemp) / (MinMax.LogRMax - MinMax.LogRMin));
                        end;
                      end
                      else
                      begin
                        CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                      end;
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                    end;
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end;
              end;
            rdtInteger:
              begin
                if DataSet.DisplayRealValue then
                begin
                  if DataSet.Limits.ActiveOnly
                    and DataSet.Limits.ShadeInactiveArea
                    and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := InactiveGridColor;
                  end
                  else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end
                  else if (MinMax.RMax = MinMax.RMin) then
                  begin
                    RTemp := DataSet.RealData
                      [LayerIndex, RowIndex, ColIndex];
                    if RTemp = MinMax.RMin then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow(0.5);
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                    end;
                  end
                  else
                  begin
                    RTemp := DataSet.RealData
                      [LayerIndex, RowIndex, ColIndex];
                    if (RTemp >= MinMax.RMin) and (RTemp <= MinMax.RMax) then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((MinMax.RMax - RTemp) / (MinMax.RMax - MinMax.RMin));
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                    end;
                  end;
                end
                else
                begin
                  if DataSet.Limits.ActiveOnly
                    and DataSet.Limits.ShadeInactiveArea
                    and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := InactiveGridColor;
                  end
                  else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end
                  else if (MinMax.IMax = MinMax.IMin) then
                  begin
                    ITemp := DataSet.IntegerData
                      [LayerIndex, RowIndex, ColIndex];
                    if ITemp = MinMax.IMin then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow(0.5);
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                    end;
                  end
                  else
                  begin
                    ITemp := DataSet.IntegerData
                      [LayerIndex, RowIndex, ColIndex];
                    if (ITemp >= MinMax.IMin) and (ITemp <= MinMax.IMax) then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((MinMax.IMax - ITemp) / (MinMax.IMax - MinMax.IMin));
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                    end;
                  end;
                end;
              end;
            rdtBoolean:
              begin
                if DataSet.Limits.ActiveOnly
                  and DataSet.Limits.ShadeInactiveArea
                  and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := InactiveGridColor;
                end
                else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                end
                else if MinMax.BMax = MinMax.BMin then
                begin
                  BTemp := DataSet.BooleanData
                    [LayerIndex, RowIndex, ColIndex];
                  if BTemp = MinMax.BMin then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0.5);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end
                else
                begin
                  if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(1);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0);
                  end;
                end;
              end;
            rdtString:
              begin
                if DataSet.Limits.ActiveOnly
                  and DataSet.Limits.ShadeInactiveArea
                  and not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := InactiveGridColor;
                end
                else if not OkLocation(DataSet, LayerIndex, RowIndex, ColIndex)
                  or (StringValues.Count = 0) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                end
                else if StringValues.Count = 1 then
                begin
                  STemp := DataSet.StringData[
                    LayerIndex, RowIndex, ColIndex];
                  if StringValues.IndexOf(STemp) >= 0 then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0.5);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end
                else
                begin
                  STemp := DataSet.StringData[LayerIndex, RowIndex, ColIndex];
                  SIndex := StringValues.IndexOf(STemp);
                  if SIndex >= 0 then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(1-(SIndex / (StringValues.Count - 1)));
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end;
              end;
          else
            Assert(False);
          end;
        end;
      end;
    end;
    FNeedToRecalculate3DCellColors := False;
  finally
    StringValues.Free;
  end;
end;

function TCustomModelGrid.GetContainingColumnOrRow(
  const Positions: TOneDRealArray; const APosition: real): integer;
var
  NearestPositionIndex: integer;
  PositionLength: integer;
  Reversed: boolean;
begin
  PositionLength := Length(Positions);
  if PositionLength <= 1 then
  begin
    result := -1;
    Exit;
  end;
  NearestPositionIndex := NearestColumnOrRow(Positions, APosition);
  if (NearestPositionIndex < 0) then
  begin
    result := -1;
  end
  else if (NearestPositionIndex >= PositionLength) then
  begin
    result := PositionLength - 1;
  end
  else
  begin
    Reversed := Positions[PositionLength-1] < Positions[0];
    if Reversed then
    begin
      if APosition < Positions[NearestPositionIndex] then
      begin
        result := NearestPositionIndex;
      end
      else
      begin
        if (NearestPositionIndex = 0)
          and (APosition = Positions[0]) then
        begin
          result := NearestPositionIndex;
        end
        else
        begin
          result := NearestPositionIndex - 1;
        end;
      end;
    end
    else
    begin
      if APosition > Positions[NearestPositionIndex] then
      begin
        result := NearestPositionIndex;
      end
      else
      begin
        if (NearestPositionIndex = 0)
          and (APosition = Positions[0]) then
        begin
          result := NearestPositionIndex;
        end
        else
        begin
          result := NearestPositionIndex - 1;
        end;
      end;
    end;
  end;
end;

function TCustomModelGrid.GetContainingColumn(
  const AnXPosition: real): integer;
begin
  result := GetContainingColumnOrRow(FColumnPositions, AnXPosition);
end;

function TCustomModelGrid.GetContainingRow(const AYPosition: real): integer;
begin
  result := GetContainingColumnOrRow(FRowPositions, AYPosition);
end;

procedure TCustomModelGrid.NeedToRecalculateCellColors;
begin
  NeedToRecalculateFrontCellColors := True;
  NeedToRecalculateSideCellColors := True;
  NeedToRecalculateTopCellColors := True;
  NeedToRecalculate3DCellColors := True;
end;

procedure TCustomModelGrid.Assign(Source: TPersistent);
var
  SourceGrid: TCustomModelGrid;
begin
  if Source is TCustomModelGrid then
  begin
    SourceGrid := TCustomModelGrid(Source);
    ColumnPositions := SourceGrid.ColumnPositions;
    RowPositions := SourceGrid.RowPositions;
    ColumnDirection := SourceGrid.ColumnDirection;
    GridAngle := SourceGrid.GridAngle;
    LayerCount := SourceGrid.LayerCount;
    SelectedLayer := SourceGrid.SelectedLayer;
    SelectedColumn := SourceGrid.SelectedColumn;
    SelectedRow := SourceGrid.SelectedRow;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TCustomModelGrid.BeginColumnChange;
begin
  Inc(FColumnUpdate);
end;

procedure TCustomModelGrid.BeginGridChange;
begin
  Inc(FGridUpdate);
end;

procedure TCustomModelGrid.BeginLayerChange;
begin
  Inc(FLayerUpdate);
  CanDraw := False;
end;

procedure TCustomModelGrid.BeginRowChange;
begin
  Inc(FRowUpdate);
end;

procedure TCustomModelGrid.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ColumnPositions', ReadColumnPositions,
    WriteColumnPositions, True);
  Filer.DefineProperty('RowPositions', ReadRowPositions,
    WriteRowPositions, True);
end;

procedure TCustomModelGrid.ReadColumnPositions(Reader: TReader);
var
  Positions: TOneDRealArray;
begin
  ReadRealArray(Reader, Positions, ColumnCount + 1);
  ColumnPositions := Positions;
end;

procedure TCustomModelGrid.WriteColumnPositions(Writer: TWriter);
begin
  WriteRealArray(Writer, ColumnPositions);
end;

procedure TCustomModelGrid.ReadRowPositions(Reader: TReader);
var
  Positions: TOneDRealArray;
begin
  ReadRealArray(Reader, Positions, RowCount + 1);
  RowPositions := Positions;
end;

procedure TCustomModelGrid.WriteRowPositions(Writer: TWriter);
begin
  WriteRealArray(Writer, RowPositions);
end;

function TCustomModelGrid.TwoDCellCorner(const Column, Row: integer): TPoint2D;
begin
  if (Column = 0) and (Row = 0) then
  begin
    result := TwoDElementCorner(Column, Row);
  end
  else if (Column = 0) then
  begin
    if Row = RowCount+1 then
    begin
      result := TwoDElementCorner(Column, Row-1);
    end
    else
    begin
      result := TwoDColumnEdgeCenter(Column, Row-1);
    end;
  end
  else if (Row = 0) then
  begin
    if Column = ColumnCount+1 then
    begin
      result := TwoDElementCorner(Column-1, Row);
    end
    else
    begin
      result := TwoDRowEdgeCenter(Column-1, Row);
    end;
  end
  else if Column = ColumnCount+1 then
  begin
    if Row = RowCount+1 then
    begin
      result := TwoDElementCorner(Column-1, Row-1);
    end
    else
    begin
      result := TwoDColumnEdgeCenter(Column-1, Row-1);
    end;
  end
  else if Row = RowCount+1 then
  begin
    result := TwoDRowEdgeCenter(Column-1, Row-1);
  end
  else
  begin
    result := TwoDElementCenter(Column-1, Row-1);
  end;
end;

function TCustomModelGrid.TwoDColumnEdgeCenter(const Column,
  Row: integer): TPoint2D;
var
  Point1, Point2: TPoint2D;
begin
  Point1 := TwoDElementCorner(Column, Row + 1);
  Point2 := TwoDElementCorner(Column, Row);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
end;

function TCustomModelGrid.TwoDRowEdgeCenter(const Column,
  Row: integer): TPoint2D;
var
  Point1, Point2: TPoint2D;
begin
  Point1 := TwoDElementCorner(Column + 1, Row);
  Point2 := TwoDElementCorner(Column, Row);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
end;

function TCustomModelGrid.ThreeDCellCorner(Column, Row,
  Layer: integer): T3DRealPoint;
begin
//  if Column > ColumnCount then
//  begin
//    Column := ColumnCount
//  end;
//  if Row > RowCount then
//  begin
//    Row := RowCount
//  end;
//  if Layer > LayerCount then
//  begin
//    Layer := LayerCount
//  end;
  if (Column = 0) then
  begin
    result.X := ColumnPosition[Column];
  end
  else if (Column = ColumnCount+1) then
  begin
    result.X := ColumnPosition[Column-1];
  end
  else
  begin
    result.X := (ColumnPosition[Column] + ColumnPosition[Column-1])/2;
  end;
  if (Row = 0)  then
  begin
    result.Y := RowPosition[Row];
  end
  else if (Row = RowCount+1) then
  begin
    result.Y := RowPosition[Row-1];
  end
  else
  begin
    result.Y := (RowPosition[Row] + RowPosition[Row-1])/2;
  end;
  if (Layer = 0) then
  begin
    result.Z := CellElevation[Column, Row, Layer];
  end
  else if (Layer = LayerCount+1) then
  begin
    result.Z := CellElevation[Column, Row, Layer-1];
  end
  else
  begin
    result.Z := (CellElevation[Column, Row, Layer]
      + CellElevation[Column, Row, Layer-1])/ 2;
  end;
end;

function TCustomModelGrid.ThreeDColumnEdgeCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Point1, Point2: T3DRealPoint;
begin
  Point1 := ThreeDElementCorner(Column, Row, Layer);
  Point2 := ThreeDElementCorner(Column, Row + 1, Layer);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
  result.Z := (Point1.Z + Point2.Z) / 2;
end;

function TCustomModelGrid.ThreeDLayerEdgeCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Point1, Point2: T3DRealPoint;
begin
  Point1 := ThreeDElementCorner(Column, Row, Layer);
  Point2 := ThreeDElementCorner(Column, Row, Layer + 1);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
  result.Z := (Point1.Z + Point2.Z) / 2;
end;

function TCustomModelGrid.ThreeDRowEdgeCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Point1, Point2: T3DRealPoint;
begin
  Point1 := ThreeDElementCorner(Column, Row, Layer);
  Point2 := ThreeDElementCorner(Column + 1, Row, Layer);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
  result.Z := (Point1.Z + Point2.Z) / 2;
end;

procedure TCustomModelGrid.RecordShell;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FGridShellGLIndex, GL_COMPILE);
    glLineWidth(ThinLine);
    // Draw left side;
    glColor3f(0.0, 0.0, 0.0);
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    ColumnIndex := 0;
    X := ColumnPosition[ColumnIndex];
    for RowIndex := 0 to RowCount do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for RowIndex := RowCount downto 0 do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw right side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    ColumnIndex := ColumnCount;
    X := ColumnPosition[ColumnIndex];
    for RowIndex := 0 to RowCount do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for RowIndex := RowCount downto 0 do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw front side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := 0;
    Y := RowPosition[RowIndex];
    for ColumnIndex := 0 to ColumnCount do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for ColumnIndex := ColumnCount downto 0 do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw back side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := RowCount;
    Y := RowPosition[RowIndex];
    for ColumnIndex := 0 to ColumnCount do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for ColumnIndex := ColumnCount downto 0 do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;
    glEndList;
  finally
    FRecordedShell := True;
  end;
end;

procedure TCustomModelGrid.RecordSide;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FSideGridGLIndex, GL_COMPILE);
    // Draw grid lines on selected column.

    ColumnIndex := DisplayColumn;
    if (ColumnIndex >= 0) and (ColumnIndex <= ColumnCount) then
    begin
      X := ColumnPosition[ColumnIndex];
      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);

        LayerIndex := 0;
        Y := RowPosition[RowIndex];
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        glEnd;
      end;
      for LayerIndex := 0 to LayerCount do
      begin
        if (LayerIndex = LayerCount) or (LayerIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for RowIndex := 0 to RowCount do
        begin
          Y := RowPosition[RowIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;
    end;
    glEndList;
  finally
    FRecordedSideGrid := True;
  end;
end;

procedure TCustomModelGrid.RecordFront;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FFrontGridGLIndex, GL_COMPILE);
    // Draw grid lines on selected Row.

    RowIndex := DisplayRow;
    if (RowIndex >= 0) and (RowIndex <= RowCount) then
    begin
      Y := RowPosition[RowIndex];
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);
        LayerIndex := 0;
        X := ColumnPosition[ColumnIndex];
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        glEnd;
      end;

      for LayerIndex := 0 to LayerCount do
      begin
        if (LayerIndex = LayerCount) or (LayerIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          X := ColumnPosition[ColumnIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;
    end;
    glEndList;
  finally
    FRecordedFrontGrid := True;
  end;
end;

procedure TCustomModelGrid.RecordTop;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FTopGridGLIndex, GL_COMPILE);
    // Draw grid lines on selected layer.

    LayerIndex := DisplayLayer;
    if (LayerIndex >= 0) and (LayerIndex <= LayerCount) then
    begin
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for RowIndex := 0 to RowCount do
        begin
          X := ColumnPosition[ColumnIndex];
          Y := RowPosition[RowIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;

      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          X := ColumnPosition[ColumnIndex];
          Y := RowPosition[RowIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;
    end;
    glEndList;
  finally
    FRecordedTopGrid := True;
  end;
end;

procedure TCustomModelGrid.Draw3D;
var
  Colors: array[0..2] of GLint;
begin
  if not CanDraw3D then
    Exit;

  if FDrawing3DGrid then
  begin
    Exit;
  end;

  // Draw outer box

  glDisable(GL_LIGHTING);
  glDisable(GL_LIGHT0);
  glMatrixMode(GL_MODELVIEW);

  glPushMatrix;

  glEnable(GL_LINE_SMOOTH);
  Colors[0] := 0;
  Colors[1] := 0;
  Colors[2] := 0;
  glMaterialiv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @Colors);

  // If the grid has changed, record display lists of the grid.
  if FNeedToRedraw3d then
  begin
    // If the display lists have not been created, create them.
    if not FListsCreated then
    begin
      FGridShellGLIndex := glGenLists(NumberOfLists);
      FTopGridGLIndex := Succ(FGridShellGLIndex);
      FFrontGridGLIndex := Succ(FTopGridGLIndex);
      FSideGridGLIndex := Succ(FFrontGridGLIndex);
      FCellsGLIndex := Succ(FSideGridGLIndex);
      FEdgesGLIndex := Succ(FCellsGLIndex);
      FListsCreated := True;
    end;
  end;

//  // Force update of cell elevations.
//  HighestElevation;

  if frmGoPhast.tbShell.Down and
    (FNeedToRedraw3d or not FRecordedShell) then
  begin
    // Record display lists of the
    // grid shell, Side, Front, and Top.

    RecordShell;
    FRecordedShell := True;
  end;

  if frmGoPhast.tbSideGrid.Down and
    (not FRecordedSideGrid) then
  begin
    RecordSide;
    // FRecordedSideGrid is private and won't be set
    // by overridden versions of RecordSide.
    FRecordedSideGrid := True;
  end;

  if frmGoPhast.tbFrontGrid.Down and
    (not FRecordedFrontGrid) then
  begin
    RecordFront;
    // FRecordedFrontGrid is private and won't be set
    // by overridden versions of RecordFront.
    FRecordedFrontGrid := True;
  end;

  if frmGoPhast.tbTopGrid.Down and
    (not FRecordedTopGrid) then
  begin
    RecordTop;
    // FRecordedTopGrid is private and won't be set
    // by overridden versions of RecordTop.
    FRecordedTopGrid := True;
  end;

  if FNeedToRedraw3d then
  begin
    if frmGoPhast.tb3DColors.Down and (ThreeDDataSet <> nil) then
    begin
      FDrawing3DGrid := True;
      try
        RecordColoredGrid;
      finally
        FDrawing3DGrid := False;
      end;
    end;
    if frmGoPhast.tb3DColors.Down
      and (frmGoPhast.PhastModel.EdgeDisplay <> nil) then
    begin
      RecordColoredGridEdges;
    end;

    // indicate that the display lists are up to date.
    FNeedToRedraw3d := False;
  end;

  // Show the display lists.
  if frmGoPhast.tbShell.Down and FRecordedShell then
  begin
    glCallList(FGridShellGLIndex);
  end;
  if frmGoPhast.tbTopGrid.Down and FRecordedTopGrid then
  begin
    glCallList(FTopGridGLIndex);
  end;
  if frmGoPhast.tbFrontGrid.Down and FRecordedFrontGrid then
  begin
    glCallList(FFrontGridGLIndex);
  end;
  if frmGoPhast.tbSideGrid.Down and FRecordedSideGrid then
  begin
    glCallList(FSideGridGLIndex);
  end;
  if frmGoPhast.tb3DColors.Down and (ThreeDDataSet <> nil) then
  begin
    glCallList(FCellsGLIndex);
  end;
  if frmGoPhast.tb3DColors.Down
    and (frmGoPhast.PhastModel.EdgeDisplay <> nil) then
  begin
    glCallList(FEdgesGLIndex);
  end;

  glPopMatrix;
end;

function TCustomModelGrid.CanDraw3D: boolean;
begin
  result := FDraw3DAllowed and (ColumnCount >= 1) and (RowCount >= 1)
    and (LayerCount >= 1)
    and (FLayerUpdate = 0) and (FColumnUpdate = 0) and (FGridUpdate = 0)
    and (FRowUpdate = 0) and (frmGoPhast <> nil) and frmGoPhast.CanDraw;
end;

procedure TCustomDiscretization.ViewsChanged;
begin
  frmGoPhast.frameTopView.ItemChange(nil);
  frmGoPhast.frameFrontView.ItemChange(nil);
  frmGoPhast.frameSideView.ItemChange(nil);
  if frmGoPhast.frame3DView.glWidModelView <> nil then
  begin
    frmGoPhast.frame3DView.glWidModelView.Invalidate;
  end;
end;

procedure TCustomModelGrid.GridChanged;
begin
  if FGridUpdate > 0 then Exit;

  CanDraw := True;
  if (LayerCount <= 0) or (RowCount <= 0) or (ColumnCount <= 0) then
  begin
    TopDataSet := nil;
    FrontDataSet := nil;
    SideDataSet := nil;
    ThreeDDataSet := nil;

    TopContourDataSet := nil;
    FrontContourDataSet := nil;
    SideContourDataSet := nil;
    ThreeDContourDataSet := nil;
  end;

  FBlockGlGrid := nil;
  FreeAndNil(FBlockGridCache);
  FNodeGlGrid := nil;
  FreeAndNil(FNodeGridCache);

  FTopElementContourGrid := nil;
  FTopNodeContourGrid := nil;
  FFrontElementContourGrid := nil;
  FFrontNodeContourGrid := nil;
  FSideElementContourGrid := nil;
  FSideNodeContourGrid := nil;
  FRecordedShell := False;
  FRecordedSideGrid := False;
  FRecordedFrontGrid := False;
  FRecordedTopGrid := False;

  if Model is TPhastModel then
  begin
    TPhastModel(Model).InvalidateMapping;
  end;

  if frmGoPhast.frame3DView.glWidModelView <> nil then
  begin
    FNeedToRedraw3d := True;
  end;
  frmGoPhast.EnableVisualization;
  ViewsChanged;
end;

destructor TCustomModelGrid.Destroy;
begin
  if FListsCreated then
  begin
    try
      glDeleteLists(FGridShellGLIndex, NumberOfLists);
    except on E: EExternalException do
      begin
        //
      end;

    end;
  end;
  FreeAndNil(FBlockGridCache);
  FreeAndNil(FNodeGridCache);
  inherited;
end;

procedure TCustomModelGrid.SetThreeDContourDataSet(const Value: TDataArray);
begin
  if FThreeDContourDataSet <> Value then
  begin
    FThreeDContourDataSet := Value;
  end;
end;

procedure TCustomModelGrid.SetThreeDDataSet(const Value: TDataArray);
begin
//  FThreeDDataSet := Value;
  if FThreeDDataSet <> Value then
  begin
    FThreeDDataSet := Value;
    NeedToRecalculate3DCellColors := True;
    GridChanged;
  end;
end;

procedure TCustomModelGrid.SetThreeDGridObserver(const Value: TObserver);
begin
  FThreeDGridObserver := Value;
  if Assigned(FThreeDGridObserver) then
  begin
    FThreeDGridObserver.UpToDate := False;
    FThreeDGridObserver.UpToDate := True;
  end;
end;

procedure TCustomModelGrid.SetNeedToRecalculate3DCellColors(
  const Value: boolean);
begin
  if FNeedToRecalculate3DCellColors <> Value then
  begin
    FNeedToRecalculate3DCellColors := Value;
    if not FNeedToRecalculate3DCellColors then
    begin
      GridChanged;
    end;
  end;
end;

procedure TCustomModelGrid.RecordColoredGridEdges;
var
  Index: Integer;
  Edge: TCustomModflowGridEdgeFeature;
  ZPositions: TThreeDRealArray;
  XPositions: TOneDRealArray;
  YPositions: TOneDRealArray;
  MinValue, MaxValue, MinPositive: double;
  Value: double;
  Color: TColor;
  Red: GLubyte;
  Green: GLubyte;
  Blue: GLubyte;
  LocalEdgeDisplay: TCustomModflowGridEdgeDisplay;
  ColoringLimits: TColoringLimits;
  ActiveDataArray: TDataArray;
  LogMax, LogMin: double;
begin
  Screen.Cursor := crHourGlass;
  try
    LocalEdgeDisplay := frmGoPhast.PhastModel.EdgeDisplay;
    Assert(LocalEdgeDisplay <> nil);
    LocalEdgeDisplay.UpdateData;

    XPositions := FColumnPositions;
    YPositions := FRowPositions;
    GetCellCornerElevations(eaBlocks, ZPositions);

    glNewList(FEdgesGLIndex, GL_COMPILE);

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

      if Edge.Col1 <> Edge.Col2 then
      begin
        if Edge.Col2 > Edge.Col1 then
        begin
          DrawRightCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end
        else
        begin
          DrawLeftCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end;
      end
      else
      begin
        Assert(Edge.Row1 <> Edge.Row2);
        if Edge.Row2 > Edge.Row1 then
        begin
          DrawBackCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end
        else
        begin
          DrawFrontCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end;
      end;
    end;

    glEndList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCustomModelGrid.DrawLeftCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  X := XPositions[ColIndex];
  glBegin(GL_POLYGON);
  // back bottom point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // front bottom point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // front top point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // back top point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);
  glEnd;
end;

procedure TCustomModelGrid.DrawRightCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  X := XPositions[ColIndex + 1];
  glBegin(GL_POLYGON);
  // back top point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // front top point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // front bottom point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // back bottom point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomModelGrid.DrawSideContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  Contourer: TMultipleContourCreator;
  LocalModel: TCustomModel;
begin
  LocalModel := Model as TCustomModel;
  if not LocalModel.CanDrawContours then
  begin
    Exit;
  end;
  if (ColumnCount >= 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    if (SideContourDataSet <> nil)
      and (SideContourDataSet.Orientation in [dsoSide, dso3D]) then
    begin
      Contourer := TMultipleContourCreator.Create;
      try
        PlotList := FSideContourPlotList;
        Contourer.DataSet := SideContourDataSet;
        Contourer.ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdSide;
        Contourer.Grid := ContourGrid(SideContourDataSet.EvaluatedAt,
          LocalModel.ModelSelection, vdSide, SelectedColumn);
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedColumn,
          frmGoPhast.PhastModel.ContourColorParameters, vdSide,
          frmGoPhast.PhastModel.ContourLabelSpacing,
          False);
//        LocalModel.SideContoursUpToDate := True;
      finally
        Contourer.Free;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.DrawBackCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  // right bottom point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // left bottom point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // left top point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // right top point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomModelGrid.DrawFrontCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  // right top point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // left top point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // left bottom point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // right bottom point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomModelGrid.DrawTopCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);

  // north east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // south east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // south west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // north west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomModelGrid.DrawBottomCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  // north west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // south west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // south east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // north east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomModelGrid.RecordColoredGrid;
var
  CellColors: TCellColors;
  LayerLength, RowLength, ColLength: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  Red, Green, Blue: GLubyte;
  XPositions, YPositions: TOneDRealArray;
  ZPositions: TThreeDRealArray;
  Index: integer;
var
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Assert(ThreeDDataSet <> nil);
    if frmGoPhast.PhastModel.ThreeDTimeList <> nil then
    begin
      SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer;
      SelectedRow := frmGoPhast.PhastGrid.SelectedRow;
      SelectedColumn := frmGoPhast.PhastGrid.SelectedColumn;
      try
        frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(
          frmGoPhast.PhastModel.ThreeDTimeList,
          frmGoPhast.PhastModel.ThreeDDisplayTime);
        if frmGoPhast.PhastModel.TopTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateTopTimeDataSet(
            frmGoPhast.PhastModel.TopTimeList,
            frmGoPhast.PhastModel.TopDisplayTime);
        end;
        if frmGoPhast.PhastModel.FrontTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateFrontTimeDataSet(
            frmGoPhast.PhastModel.FrontTimeList,
            frmGoPhast.PhastModel.FrontDisplayTime);
        end;
        if frmGoPhast.PhastModel.SideTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateSideTimeDataSet(
            frmGoPhast.PhastModel.SideTimeList,
            frmGoPhast.PhastModel.SideDisplayTime);
        end;
      finally
        frmGoPhast.PhastGrid.SelectedLayer := SelectedLayer;
        frmGoPhast.PhastGrid.SelectedRow := SelectedRow;
        frmGoPhast.PhastGrid.SelectedColumn := SelectedColumn;
      end;
    end
    else
    begin
      ThreeDDataSet.Initialize;
    end;

    if ThreeDDataSet = nil then
    begin
      Exit;
    end;

    glNewList(FCellsGLIndex, GL_COMPILE);
    LayerLength := -1;
    RowLength := -1;
    ColLength := -1;
    case ThreeDDataSet.EvaluatedAt of
      eaBlocks:
        begin
          LayerLength := ThreeDDataSet.LayerCount;
          RowLength := ThreeDDataSet.RowCount;
          ColLength := ThreeDDataSet.ColumnCount;
        end;
      eaNodes:
        begin
          { TODO : This is a clumsy hack. It should be updated. }
          if (ThreeDDataSet = frmGoPhast.PhastModel.DataArrayManager.
            GetDataSetByName(rsInitial_Water_Table)) then
          begin
            LayerLength := ThreeDDataSet.LayerCount;
          end
          else if
            ((ThreeDDataSet is TSparseArrayPhastInterpolationDataSet) and
            ((frmGoPhast.PhastModel.RiverHead.
            IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
            ThreeDDataSet)) >= 0)
            or (frmGoPhast.PhastModel.RiverAssociatedSolution.
            IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
            ThreeDDataSet)) >= 0)))
            or (frmGoPhast.PhastModel.DataArrayManager.RiverDataSets.IndexOf(
            ThreeDDataSet) >= 0) then
          begin
            LayerLength := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
          end
          else
          begin
            LayerLength := ThreeDDataSet.LayerCount + 1;
          end;
          //LayerLength := ThreeDDataSet.LayerCount+1;
          RowLength := ThreeDDataSet.RowCount + 1;
          ColLength := ThreeDDataSet.ColumnCount + 1;
          case ThreeDDataSet.Orientation of
            dsoTop:
              begin
                { TODO : This is a clumsy hack. It should be updated. }
                if (ThreeDDataSet = frmGoPhast.PhastModel.DataArrayManager.
                  GetDataSetByName(rsInitial_Water_Table)) then
                begin
                  LayerLength := ThreeDDataSet.LayerCount;
                end
                else if
                  ((ThreeDDataSet is TSparseArrayPhastInterpolationDataSet) and
                  ((frmGoPhast.PhastModel.RiverHead.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  ThreeDDataSet)) >= 0)
                  or (frmGoPhast.PhastModel.RiverAssociatedSolution.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  ThreeDDataSet)) >= 0)))
                  or (frmGoPhast.PhastModel.DataArrayManager.RiverDataSets.IndexOf(
                  ThreeDDataSet) >= 0)
                    then
                begin
                  LayerLength := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
                end
                else
                begin
                  LayerLength := ThreeDDataSet.LayerCount;
                end;
                //LayerLength := 1;
              end;
            dsoFront:
              begin
                RowLength := 1;
              end;
            dsoSide:
              begin
                ColLength := 1;
              end;
            dso3D:
              begin
                // do nothing
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;

    SetLength(CellColors, LayerLength, RowLength, ColLength);

    Update3DCellColors(CellColors);

    case ThreeDDataSet.EvaluatedAt of
      eaBlocks:
        begin
          XPositions := FColumnPositions;
          YPositions := FRowPositions;
        end;
      eaNodes:
        begin
          SetLength(XPositions, Length(FColumnPositions) + 1);
          XPositions[0] := FColumnPositions[0];
          for Index := 1 to Length(FColumnPositions) - 1 do
          begin
            XPositions[Index] := (FColumnPositions[Index]
              + FColumnPositions[Index - 1]) / 2;
          end;
          XPositions[Length(FColumnPositions)]
            := FColumnPositions[Length(FColumnPositions) - 1];

          SetLength(YPositions, Length(FRowPositions) + 1);
          YPositions[0] := FRowPositions[0];
          for Index := 1 to Length(FRowPositions) - 1 do
          begin
            YPositions[Index] := (FRowPositions[Index]
              + FRowPositions[Index - 1]) / 2;
          end;
          YPositions[Length(FRowPositions)]
            := FRowPositions[Length(FRowPositions) - 1];
        end;
    else
      Assert(False);
    end;

    GetCellCornerElevations(ThreeDDataSet.EvaluatedAt, ZPositions);

    for LayerIndex := 0 to LayerLength - 1 do
    begin
      for RowIndex := 0 to RowLength - 1 do
      begin
        for ColIndex := 0 to ColLength - 1 do
        begin
          if CellColors[LayerIndex, RowIndex, ColIndex] = clWhite then
            Continue;

          ExtractColorComponents(CellColors[LayerIndex, RowIndex, ColIndex],
            Red, Green, Blue);

          glColor3ub(Red, Green, Blue);

          if (ColIndex = ColLength - 1) or
            (CellColors[LayerIndex, RowIndex, ColIndex + 1] = clWhite) then
          begin
            DrawRightCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (ColIndex = 0) or
            (CellColors[LayerIndex, RowIndex, ColIndex - 1] = clWhite) then
          begin
            DrawLeftCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (RowIndex = RowLength - 1) or
            (CellColors[LayerIndex, RowIndex + 1, ColIndex] = clWhite) then
          begin
            DrawBackCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (RowIndex = 0) or
            (CellColors[LayerIndex, RowIndex - 1, ColIndex] = clWhite) then
          begin
            DrawFrontCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (LayerIndex = LayerLength - 1) or
            (CellColors[LayerIndex + 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawTopCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (LayerIndex = 0) or
            (CellColors[LayerIndex - 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawBottomCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
        end;
      end;
    end;
    glEndList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCustomModelGrid.Initialize;
begin
  TopDataSet := nil;
  FrontDataSet := nil;
  SideDataSet := nil;
  ThreeDDataSet := nil;
  TopContourDataSet := nil;
  FrontContourDataSet := nil;
  SideContourDataSet := nil;
  ThreeDContourDataSet := nil;
  if (FDisplayColumn < 0) and (ColumnCount > 0) then
  begin
    FDisplayColumn := 0;
  end;
  if (FDisplayRow < 0) and (RowCount > 0) then
  begin
    FDisplayRow := 0;
  end;
  if (FDisplayLayer < 0) and (LayerCount > 0) then
  begin
    FDisplayLayer := 0;
  end;
end;

function TCustomModelGrid.InsideGrid(APoint: TPoint2D;
  NeedToRotatePointToGridCoordinates: Boolean): Boolean;
var
  TestValue: Double;
begin
  result := False;
  if NeedToRotatePointToGridCoordinates then
  begin
    APoint := RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  end;
  if (ColumnCount <= 0) or (RowCount <= 0) then
  begin
    Exit;
  end;
  TestValue := Min(FColumnPositions[0],
    FColumnPositions[Length(FColumnPositions)-1]);
  if (APoint.x < TestValue) then
  begin
    Exit;
  end;
  TestValue := Max(FColumnPositions[0],
    FColumnPositions[Length(FColumnPositions)-1]);
  if (APoint.x > TestValue) then
  begin
    Exit;
  end;
  TestValue := Min(FRowPositions[0],
    FRowPositions[Length(FRowPositions)-1]);
  if (APoint.y < TestValue) then
  begin
    Exit;
  end;
  TestValue := Max(FRowPositions[0],
    FRowPositions[Length(FRowPositions)-1]);
  if (APoint.y > TestValue) then
  begin
    Exit;
  end;

  result := True;
end;

function TCustomModelGrid.LayerCenter(Column, Row, Layer: integer): real;
begin
  result := (CellElevation[Column, Row, Layer]
    + CellElevation[Column, Row, Layer+1])/2;
end;

function TCustomModelGrid.NearLayerTop(Column, Row, Layer: integer): real;
begin
  result := ((CellElevation[Column, Row, Layer] * 19)
    + CellElevation[Column, Row, Layer+1])/20;
end;

procedure TCustomModelGrid.DrawTopContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  Contourer: TMultipleContourCreator;
  LocalModel: TCustomModel;
begin
  LocalModel := Model as TCustomModel;
  if not LocalModel.CanDrawContours then
  begin
    Exit;
  end;
  if (ColumnCount >= 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    if (TopContourDataSet <> nil)
      and (TopContourDataSet.Orientation in [dsoTop, dso3D]) then
    begin
      Contourer := TMultipleContourCreator.Create;
      try
        PlotList := FTopContourPlotList;
        Contourer.DataSet := TopContourDataSet;
        Contourer.ActiveDataSet :=
          LocalModel.DataArrayManager.GetDataSetByName(rsActive);
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdTop;
        Contourer.Grid := ContourGrid(TopContourDataSet.EvaluatedAt,
          LocalModel.ModelSelection, vdTop, SelectedLayer);
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedLayer,
          frmGoPhast.PhastModel.ContourColorParameters, vdTop,
          frmGoPhast.PhastModel.ContourLabelSpacing,
          False);
//        LocalModel.TopContoursUpToDate := True;
      finally
        Contourer.Free;
      end;
    end;
  end;
end;

procedure TCustomModelGrid.LayersChanged;
begin
  if FLayerUpdate > 0 then Exit;

  GridChanged;
  if Assigned(ThreeDGridObserver) then
  begin
    ThreeDGridObserver.UpToDate := False;
    ThreeDGridObserver.UpToDate := True;
  end;
  InvalidateContours;
end;

function TCustomModelGrid.Nearest2DCellElevation(const Col, Row: integer;
  const APosition: real; const First, Last: integer): integer;
begin
  result := NearestColumnOrRow(TwoDCellElevations[Col, Row],
    APosition, First, Last);
end;

function TCustomModelGrid.NearestColumnCenter(const APosition: real;
  First, Last: integer): integer;
begin
  if First <> -1 then
  begin
    Dec(First);
  end;
  if Last <> -1 then
  begin
    Inc(Last);
  end;
  result := NearestColumnPosition(APosition, First, Last);
  if result > 0 then
  begin
    if (result > ColumnCount) or (ColumnPosition[result] > APosition) then
    begin
      Dec(result);
    end;
    if (result >= ColumnCount)  then
    begin
      result := ColumnCount -1
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomModelGrid.NearestRowCenter(const APosition: real; First,
  Last: integer): integer;
begin
  if First <> -1 then
  begin
    Dec(First);
  end;
  if Last <> -1 then
  begin
    Inc(Last);
  end;
  result := NearestRowPosition(APosition, First, Last);
  if result > 0 then
  begin
    if (result > RowCount) or(RowPosition[result] > APosition) then
    begin
      Dec(result);
    end;
    if (result >= RowCount) then
    begin
      result := RowCount-1;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomModelGrid.ColumnCenter(const Column: integer): real;
begin
  result := (ColumnPosition[Column] + ColumnPosition[Column + 1])/2
end;

procedure TCustomModelGrid.ColumnsChanged;
begin
  if FColumnUpdate > 0 then Exit;

  GridChanged;
  if Assigned(TopGridObserver) then
  begin
    TopGridObserver.UpToDate := False;
    TopGridObserver.UpToDate := True;
  end;
  InvalidateContours;
end;

function TCustomModelGrid.RowCenter(const Row: integer): real;
begin
  result := (RowPosition[Row] + RowPosition[Row + 1])/2
end;

procedure TCustomModelGrid.RowsChanged;
begin
  if FRowUpdate > 0 then Exit;

  GridChanged;
  if Assigned(TopGridObserver) then
  begin
    TopGridObserver.UpToDate := False;
    TopGridObserver.UpToDate := True;
  end;
  InvalidateContours;
end;

procedure TCustomModelGrid.SetLocalEvalAt(ViewDirection: TViewDirection;
  var LocalEvalAt: TEvaluatedAt);
begin
  case ViewDirection of
    vdTop:
      begin
        if SideDataSet = nil then
        begin
          LocalEvalAt := eaBlocks;
        end
        else
        begin
          LocalEvalAt := SideDataSet.EvaluatedAt;
        end;
      end;
    vdFront:
      begin
        if FrontDataSet = nil then
        begin
          LocalEvalAt := eaBlocks;
        end
        else
        begin
          LocalEvalAt := FrontDataSet.EvaluatedAt;
        end;
      end;
    vdSide:
      begin
        if TopDataSet = nil then
        begin
          LocalEvalAt := eaBlocks;
        end
        else
        begin
          LocalEvalAt := TopDataSet.EvaluatedAt;
        end;
      end;
  end;
end;

procedure TCustomModelGrid.SetLayerLineColor(LayerIndex: Integer;
  LocalEvalAt: TEvaluatedAt; var LocalLineColor: TColor32;
  var LineWidth: single);
begin
  case LocalEvalAt of
    eaBlocks:
      begin
        if DrawColoredGridLines and ((LayerIndex = SelectedLayer)
          or (LayerIndex = SelectedLayer + 1)) then
        begin
          LocalLineColor := Color32(ExistingLayerSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
    eaNodes:
      begin
        if DrawColoredGridLines and (LayerIndex = SelectedLayer) then
        begin
          LocalLineColor := Color32(ExistingLayerSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomModelGrid.SetColumnLineColor(ColIndex: Integer;
  LocalEvalAt: TEvaluatedAt; var LocalLineColor: TColor32;
  var LineWidth: single);
begin
  case LocalEvalAt of
    eaBlocks:
      begin
        if DrawColoredGridLines and ((ColIndex = SelectedColumn)
          or (ColIndex = SelectedColumn + 1))
          and (Model.ModelSelection <> msFootprint) then
        begin
          LocalLineColor := Color32(ExistingColumnSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
    eaNodes:
      begin
        if DrawColoredGridLines and (ColIndex = SelectedColumn) then
        begin
          LocalLineColor := Color32(ExistingColumnSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomModelGrid.SetRowLineColor(RowIndex: Integer;
  LocalEvalAt: TEvaluatedAt; var LocalLineColor: TColor32;
  var LineWidth: single);
begin
  case LocalEvalAt of
    eaBlocks:
      begin
        if DrawColoredGridLines and ((RowIndex = SelectedRow)
          or (RowIndex = SelectedRow + 1))
          and (Model.ModelSelection <> msFootprint) then
        begin
          LocalLineColor := Color32(ExistingRowSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
    eaNodes:
      begin
        if DrawColoredGridLines and (RowIndex = SelectedRow) then
        begin
          LocalLineColor := Color32(ExistingRowSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomModelGrid.DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
var
  Contourer: TMultipleContourCreator;
  LocalModel: TCustomModel;
begin
  LocalModel := Model as TCustomModel;
  if not LocalModel.CanDrawContours then
  begin
    Exit;
  end;
  if (ColumnCount >= 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    if (FrontContourDataSet <> nil)
      and (FrontContourDataSet.Orientation in [dsoFront, dso3d]) then
    begin
      Contourer := TMultipleContourCreator.Create;
      try
        PlotList := FFrontContourPlotList;
        Contourer.DataSet := FrontContourDataSet;
        Contourer.ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdFront;
        Contourer.Grid := ContourGrid(FrontContourDataSet.EvaluatedAt,
          LocalModel.ModelSelection, vdFront, SelectedRow);
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedRow,
          frmGoPhast.PhastModel.ContourColorParameters, vdFront,
          frmGoPhast.PhastModel.ContourLabelSpacing,
          False);
//        LocalModel.FrontContoursUpToDate := True;
      finally
        Contourer.Free;
      end;
    end;
  end;
end;

function TCustomDiscretization.OkLocation(const DataSet: TDataArray;
  const Layer, Row, Col: integer): boolean;
begin
  result := True;
  if not DataSet.IsValue[Layer, Row, Col] then
  begin
    result := False;
  end
  else if not ValueOK(DataSet, Layer, Row, Col) then
  begin
    result := False;
  end
end;

function TCustomDiscretization.ValueOK(DataSet: TDataArray;
  const Layer, Row, Col: integer): boolean;
begin
  result := DataSet.ColorGridValueOK(Layer, Row, Col);
end;

constructor TCustomDiscretization.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
  end
  else
  begin
    inherited Create(Model.Invalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;

  FTopContourPlotList := TPlotList.Create;
  FFrontContourPlotList := TPlotList.Create;
  FSideContourPlotList := TPlotList.Create;

end;

procedure TCustomDiscretization.InitializeMinMax(const Layer, Row, Col: integer;
  DataSet: TDataArray; var MinMaxInitialized: boolean; var MinMax: TMinMax;
  StringValues: TStringList);
var
  UseString: boolean;
  TempString: string;
begin
  if not OkLocation(DataSet, Layer, Row, Col) then
  begin
    Exit;
  end;
  MinMaxInitialized := True;
  case DataSet.Datatype of
    rdtDouble:
      begin
        MinMax.RMin := DataSet.RealData[Layer, Row, Col];
        MinMax.RMax := MinMax.RMin;
        if MinMax.RMin > 0 then
        begin
          MinMax.RMinPositive := MinMax.RMin;
        end;
        if DataSet.Limits.LowerLimit.UseLimit then
        begin
          MinMax.RMin := DataSet.Limits.LowerLimit.RealLimitValue;
        end;
        if DataSet.Limits.UpperLimit.UseLimit then
        begin
          MinMax.RMax := DataSet.Limits.UpperLimit.RealLimitValue;
        end;
        if MinMax.RMin > 0 then
        begin
          MinMax.RMinPositive := MinMax.RMin;
        end;
      end;
    rdtInteger:
      begin
        MinMax.IMin := DataSet.IntegerData[Layer, Row, Col];
        MinMax.IMax := MinMax.IMin;
        if DataSet.DisplayRealValue then
        begin
          MinMax.RMin := DataSet.RealData[Layer, Row, Col];
          MinMax.RMax := MinMax.RMin;
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            MinMax.RMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            MinMax.RMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
          end;
        end
        else
        begin
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            MinMax.IMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            MinMax.IMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
          end;
        end;
      end;
    rdtBoolean:
      begin
        MinMax.BMin := False;
        MinMax.BMax := True;
        if DataSet.Limits.LowerLimit.UseLimit then
        begin
          MinMax.BMin := DataSet.Limits.LowerLimit.BooleanLimitValue;
        end;
        if DataSet.Limits.UpperLimit.UseLimit then
        begin
          MinMax.BMax := DataSet.Limits.UpperLimit.BooleanLimitValue;
        end;
      end;
    rdtString:
      begin
        UseString := True;
        TempString := DataSet.StringData[Layer, Row, Col];
        if DataSet.Limits.UpperLimit.UseLimit then
        begin
          if TempString > DataSet.Limits.UpperLimit.StringLimitValue then
          begin
            UseString := False;
          end;
        end;
        if DataSet.Limits.LowerLimit.UseLimit then
        begin
          if TempString < DataSet.Limits.LowerLimit.StringLimitValue then
          begin
            UseString := False;
          end;
        end;
        if UseString then
        begin
          StringValues.Add(TempString);
        end;
        if DataSet.Limits.LowerLimit.UseLimit then
        begin
          MinMax.SMin := DataSet.Limits.LowerLimit.StringLimitValue;
        end
        else
        begin
          MinMax.SMin := TempString;
        end;
        if DataSet.Limits.UpperLimit.UseLimit then
        begin
          MinMax.SMax := DataSet.Limits.UpperLimit.StringLimitValue;
        end
        else
        begin
          MinMax.SMax := TempString;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomDiscretization.InvalidateContours;
begin
  if (Model <> nil) and not (Model as TCustomModel).Clearing then
  begin
    (Model as TCustomModel).InvalidateContours
  end;
end;

procedure TCustomDiscretization.UpdateMinMax(const Layer, Row, Col: integer;
  DataSet: TDataArray; var MinMaxInitialized: boolean;
  var MinMax: TMinMax; StringValues: TStringList);
var
  RTemp: double;
  ITemp: integer;
  UseString: boolean;
  TempString: string;
begin
  Assert(Layer <= DataSet.LayerCount);
  Assert(Row <= DataSet.RowCount);
  Assert(Col <= DataSet.ColumnCount);
  if not OkLocation(DataSet, Layer, Row, Col) then
    Exit;
  // if not IsActiveOK(DataSet, Layer, Row, Col) then
  // Exit;
  if not MinMaxInitialized then
  begin
    InitializeMinMax(Layer, Row, Col, DataSet, MinMaxInitialized, MinMax,
      StringValues);
  end
  else
  begin
    case DataSet.Datatype of
      rdtDouble:
        begin
          RTemp := DataSet.RealData[Layer, Row, Col];
          if (RTemp > MinMax.RMax) and
            not DataSet.Limits.UpperLimit.UseLimit then
          begin
            MinMax.RMax := RTemp;
          end
          else if (RTemp < MinMax.RMin) and
            not DataSet.Limits.LowerLimit.UseLimit then
          begin
            MinMax.RMin := RTemp;
          end;
          if (RTemp > 0) then
          begin
            if (MinMax.RMinPositive = 0) or (RTemp < MinMax.RMinPositive) then
            begin
              MinMax.RMinPositive := RTemp;
            end;
          end;
        end;
      rdtInteger:
        begin
          if DataSet.DisplayRealValue then
          begin
            RTemp := DataSet.RealData[Layer, Row, Col];
            if (RTemp > MinMax.RMax) and
              not DataSet.Limits.UpperLimit.UseLimit then
            begin
              MinMax.RMax := RTemp;
            end
            else if (RTemp < MinMax.RMin) and
              not DataSet.Limits.LowerLimit.UseLimit then
            begin
              MinMax.RMin := RTemp;
            end;
          end;

          ITemp := DataSet.IntegerData[Layer, Row, Col];
          if (ITemp > MinMax.IMax) and
            not DataSet.Limits.UpperLimit.UseLimit then
          begin
            MinMax.IMax := ITemp;
          end
          else if (ITemp < MinMax.IMin) and
            not DataSet.Limits.LowerLimit.UseLimit then
          begin
            MinMax.IMin := ITemp;
          end;
        end;
      rdtBoolean:
        begin
          Exit;
        end;
      rdtString:
        begin
          UseString := True;
          TempString := DataSet.StringData[Layer, Row, Col];
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            if TempString > DataSet.Limits.UpperLimit.StringLimitValue then
            begin
              UseString := False;
            end;
          end;
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            if TempString < DataSet.Limits.LowerLimit.StringLimitValue then
            begin
              UseString := False;
            end;
          end;
          if UseString then
          begin
            StringValues.Add(TempString);
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TCustomDiscretization.SetMinMax(DataSet: TDataArray;
  var MinMaxInitialized: boolean; var MinMax: TMinMax;
  StringValues: TStringList; LayerCount, RowCount, ColCount: integer);
var
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
begin
  DataSet.Initialize;
  for LayerIndex := 0 to LayerCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColCount - 1 do
      begin
        UpdateMinMax(LayerIndex, RowIndex, ColIndex, DataSet, MinMaxInitialized,
          MinMax, StringValues);
      end;
    end;
  end;
  if not MinMaxInitialized then
  begin
    MinMax.RMin := 0;
    MinMax.RMax := 0;
    MinMax.RMinPositive := 1E-20;
    MinMax.LogRMin := 1E-20;
    MinMax.LogRMax := 1E-20;
    MinMax.IMin := 0;
    MinMax.IMax := 0;
    MinMax.BMin := False;
    MinMax.BMax := False;
    MinMax.SMin := '';
    MinMax.SMax := '';
  end;
end;

{ TCustomSutraMesh }

procedure TCustomMesh.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomMesh.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.EnableVisualization;
  end;
end;

function TCustomMesh.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

function TCustomMesh._AddRef: Integer;
begin
  result := -1;
end;

function TCustomMesh._Release: Integer;
begin
  result := -1;
end;

procedure TCustomMesh.CalculateMinMax(DataSet: TDataArray;
var MinMaxInitialized: Boolean; var MinMax: TMinMax; StringValues: TStringList);
begin
  SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
    DataSet.LayerCount, DataSet.RowCount, DataSet.ColumnCount);
  if DataSet.Limits.LogTransform and (MinMax.RMinPositive > 0) and
    (MinMax.RMax > 0) then
  begin
    MinMax.LogRMin := Log10(MinMax.RMinPositive);
    MinMax.LogRMax := Log10(MinMax.RMax);
  end
  else
  begin
    MinMax.LogRMin := 0;
    MinMax.LogRMax := 0;
  end;
  if DataSet.DataType = rdtString then
  begin
    if StringValues.Count > 0 then
    begin
      MinMax.SMin := StringValues[0];
      MinMax.SMax := StringValues[StringValues.Count - 1];
    end;
  end;
end;

procedure TCustomMesh.GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
  StringValues: TStringList; out MinMaxInitialized: Boolean);
var
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
begin
  Assert(DataSet <> nil);
  Assert(Model = DataSet.Model);
  DataSet.Initialize;
  if DataSet.Datatype = rdtString then
  begin
    StringValues.Sorted := True;
    StringValues.Duplicates := dupIgnore;
    StringValues.CaseSensitive := True;
    StringValues.Capacity := DataSet.LayerCount * DataSet.RowCount *
      DataSet.ColumnCount;
    for LayerIndex := 0 to DataSet.LayerCount - 1 do
    begin
      for RowIndex := 0 to DataSet.RowCount - 1 do
      begin
        for ColIndex := 0 to DataSet.ColumnCount - 1 do
        begin
          if DataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            StringValues.Add(DataSet.StringData[LayerIndex, RowIndex,
              ColIndex]);
          end;
        end;
      end;
    end;
    StringValues.Capacity := StringValues.Count;
  end;
  MinMax.RMinPositive := 0;
  MinMaxInitialized := False;
  // StringValues.Sorted := True;
  // StringValues.Duplicates := dupIgnore;
  // StringValues.CaseSensitive := True;
  // StringValues.Capacity := DataSet.LayerCount*DataSet.RowCount*DataSet.ColumnCount;
  CalculateMinMax(DataSet, MinMaxInitialized, MinMax, StringValues);
end;

destructor TCustomDiscretization.Destroy;
begin
  FTopContourPlotList.Free;
  FFrontContourPlotList.Free;
  FSideContourPlotList.Free;
  inherited;
end;

procedure TCustomDiscretization.ApplyLimitToMinMax(DataSet: TDataArray;
  var MinMax: TMinMax; Limits: TColoringLimits);
begin
  if Limits.LowerLimit.UseLimit then
  begin
    case DataSet.DataType of
      rdtDouble:
        begin
          if Limits.LowerLimit.RealLimitValue > 0 then
          begin
            MinMax.RMinPositive := Limits.LowerLimit.RealLimitValue;
            MinMax.LogRMin := Log10(MinMax.RMinPositive);
          end;
          MinMax.RMin := Limits.LowerLimit.RealLimitValue;
        end;
      rdtInteger:
        begin
          MinMax.IMin := Limits.LowerLimit.IntegerLimitValue;
        end;
      rdtBoolean:
        begin
          MinMax.BMin := Limits.LowerLimit.BooleanLimitValue;
        end;
      rdtString:
        begin
          MinMax.SMin := Limits.LowerLimit.StringLimitValue;
        end;
    else
      Assert(False);
    end;
  end;
  if Limits.UpperLimit.UseLimit then
  begin
    case DataSet.DataType of
      rdtDouble:
        begin
          MinMax.RMax := Limits.UpperLimit.RealLimitValue;
          if MinMax.RMax > 0 then
          begin
            MinMax.LogRMax := Log10(MinMax.RMax);
          end;
        end;
      rdtInteger:
        begin
          MinMax.IMax := Limits.UpperLimit.IntegerLimitValue;
        end;
      rdtBoolean:
        begin
          MinMax.BMax := Limits.UpperLimit.BooleanLimitValue;
        end;
      rdtString:
        begin
          MinMax.SMax := Limits.UpperLimit.StringLimitValue;
        end;
    else
      Assert(False);
    end;
  end;
end;

function TCustomDiscretization.GetCanDraw: boolean;
begin
  result := FCanDraw and (frmGoPhast.PhastModel.DataSetUpdateCount = 0);
end;

procedure TCustomDiscretization.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
end;

end.
