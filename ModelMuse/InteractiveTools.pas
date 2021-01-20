{
May 23, 2006: Added code to prevent frmScreenObjects from being displayed
  when it shouldn't be.
  Removed code for creating frmScreenObjectProperties in FinishScreenObjects
  and replaced with an assertion that it isn't nil because
  frmScreenObjectProperties is created in frmGoPhast.FormCreate.
  Fixed destruction of CurrentScreenObject and also destroyed
  CurrentScreenObject if the number of nodes it has is zero.
May 24, 2006: implemented setting hint of the top front and side view
  @link(TQRbwZoomBox2 zoomboxes) with the current tool.
}

{@abstract(@name defines descendants of @link(TCustomInteractiveTool) that
  manage the interaction between the user and the model.)}

unit InteractiveTools;

interface

uses
  ModflowIrregularMeshUnit, PhastModelUnit, System.UITypes, Windows,
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  Types, SysUtils, Classes, Controls, Forms, Graphics, FastGEO, GoPhastTypes,
  AbstractGridUnit, frameViewUnit, SelectUnit, ScreenObjectUnit, UndoItems,
  UndoItemsScreenObjects, QuadTreeClass, SutraMeshUnit, FishnetMeshGenerator,
  ArgusDataEntry, Generics.Collections, ZoomBox2, DrawMeshTypesUnit,
  JvBalloonHint, PointCollectionUnit;

const
  // @name is the color (silver) used to draw selected cells or elements.
  SelectedCellsColor = clSilver;

var
  SelectingObjectsWithLine: boolean = False;

type
  TPoint64 = record
    X: Int64;
    Y: Int64;
    class operator Implicit(APoint: TPoint): TPoint64;
  end;

  {@abstract(@name defines the behavior when the user wants to zoom in
    on a particular area that has been outlined.)}
  TZoomTool = class(TCustomInteractiveTool)
  protected
    // @name changes the cursor to the appropriate value when
    // the user wants to zoom in on an area.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name starts the zoom process.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name completes the zooming in process.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to zoom in by a factor of 2 at the location
    where the user clicks the mouse.)}
  TZoomInTool = class(TCustomInteractiveTool)
  protected
    // @name returns the cursor to the class.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name causes the magnification to increase by a factor of two.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to zoom out by a factor of 2 at the location
    where the user clicks the mouse.)}
  TZoomOutTool = class(TCustomInteractiveTool)
  protected
    // @name returns the cursor to the class.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name causes the magnification to decrease by a factor of two.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to move the view of the model.)}
  TPanTool = class(TCustomInteractiveTool)
  protected
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name starts to move the view of the model.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name finishes moving the view of the model.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name cancels the panning operation.
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is an abstract base class for
    @link(TCustomInteractiveTool)s that interact with the grid.)}
  TCustomGridTool = class(TCustomInteractiveTool)
  private
    procedure DrawColumnRowOrLayer(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    function IsInsideSelectionWidth(RealPoint: TPoint2D;
      const X, Y: Integer): boolean;
  protected
    // @name returns @true if a column boundary on the front view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnFrontColumn(const X, Y: integer): boolean;
    // @name returns @true if a layer boundary on the front view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnFrontLayer(const X, Y: integer): boolean;
    // @name returns @true if a layer boundary on the side view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnSideLayer(const X, Y: integer): boolean;
    // @name returns @true if a row boundary on the side view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnSideRow(const X, Y: integer): boolean;
    // @name returns @true if a column boundary on the top view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnTopColumn(const X, Y: integer): boolean;
    // @name returns @true if a row boundary on the top view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnTopRow(const X, Y: integer): boolean;
    // @name shows the position of a column or layer boundary in the front
    // view of the model when a column or
    // layer boundary is being added or moved.
    procedure ShowNewColumnOrLayer(const BitMap: TBitmap32);
    // @name shows the position of a column or row boundary in the top
    // view of the model when a column or
    // row boundary is being added or moved.
    procedure ShowNewColumnOrRow(const BitMap: TBitmap32);
    // @name shows the position of a row or layer boundary in the side
    // view of the model when a row or
    // layer boundary is being added or moved.
    procedure ShowNewRowOrLayer(const BitMap: TBitmap32);
  end;

  {@abstract(@name is an abstract base class for @link(TCustomGridTool)s
    that have a different cursor depending on whether the mouse is or is not
    over a column, row, or layer boundary.)}
  TCustomGridCursorTool = class(TCustomGridTool)
  private
    // See @link(UseSelectedCursor).
    FUseSelectedCursor: boolean;
  protected
    // @name returns the cursor to use when the mouse is
    // not over a column, row, or layer boundary.
    function GetNonSelectedCursor: TCursor; virtual;
    // @name returns the cursor to use when the mouse is
    // over a column, row, or layer boundary.
    function GetSelectedCursor: TCursor; virtual; abstract;
    // @name determines whether (X,Y) (in screen coordinates) is over a column
    // or layer boundary on the front view of the model.
    function IsOverFrontColumnOrLayer(X, Y: integer): boolean;
    // @name determines whether the point Y) (in screen coordinates) is over a
    // row or layer boundary on the side view of the model.
    function IsOverSideRowOrLayer(X, Y: integer): boolean;
    // @name determines whether the point X,Y (in screen coordinates) is over
    // a column or row on the top view of the model.
    function IsOverTopColumnOrRow(X, Y: integer): boolean;
    // @name sets the cursor to @link(GetNonSelectedCursor) or
    // @link(GetSelectedCursor) depending on whether X,Y is
    // over a grid boundary on the front view of the model.
    procedure SetFrontCursor(X, Y: integer);
    // @name sets the cursor to @link(GetNonSelectedCursor) or
    // @link(GetSelectedCursor) depending on whether X,Y is
    // over a grid boundary on the side view of the model.
    procedure SetSideCursor(X, Y: integer);
    // @name sets the cursor to @link(GetNonSelectedCursor) or
    // @link(GetSelectedCursor) depending on whether X,Y is
    // over a grid boundary on the top view of the model.
    procedure SetTopCursor(X, Y: integer);
    // If @name is true then @link(SetTopCursor), @link(SetFrontCursor)
    // and @link(SetSideCursor) will always set the cursor to
    // the value returned by @link(GetSelectedCursor).
    property UseSelectedCursor: boolean read FUseSelectedCursor
      write FUseSelectedCursor;
  public
    // @name sets the cursor depending on whether X,Y is
    // over a grid boundary.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  end;

{ TODO :
Consider creating descendants that each only handle one view of the model. }

  {@abstract(@name is used to add column, row, or layer boundaries to the grid
    at a position where the user clicks the mouse.)}
  TAddGridBoundaryTool = class(TCustomGridTool)
  protected
    // @name adds a column or layer boundary on the front view of the model.
    procedure AddColumnOrLayer(X, Y: Integer);
    // @name adds a column or row boundary on the top view of the model.
    procedure AddColumnOrRow(X, Y: Integer);
    // @name adds a row or layer boundary on the side view of the model.
    procedure AddRowOrLayer(X, Y: Integer);
    // @name defines the cursor to use with this @classname.
    function GetCursor: TCursor; override;
    procedure DrawOnBitMap32(Sender: TObject;
      Buffer: TBitmap32); override;
  public
    // @name sets the Hint of TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    // TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    // TfrmGoPhast.@link(TfrmGoPhast.frameSideView). Each is set to a
    // different value.
    procedure Activate; override;
    // @name causes @link(TCustomInteractiveTool.ZoomBox).Image32
    // to be redrawn.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name adds a column, row, or layer boundary.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to move a column, row, or layer boundary.)}
  TMovingGridBoundaryTool = class(TCustomGridCursorTool)
  private
    // See @link(MovingColumn).
    FMovingColumn: boolean;
    // See @link(MovingLayer).
    FMovingLayer: boolean;
    // See @link(MovingRow).
    FMovingRow: boolean;
    // See @link(MovingColumn).
    procedure SetMovingColumn(const Value: boolean);
    // See @link(MovingLayer).
    procedure SetMovingLayer(const Value: boolean);
    // See @link(MovingRow).
    procedure SetMovingRow(const Value: boolean);
  protected
    // @name: integer;
    // @name is the index of the column boundary being moved.
    FColumnBeingMoved: integer;
    // Name is the X-coordinate of the cursor.  It is set in
    // @link(MouseMove) and used in @link(GetSelectedCursor).
    FCurrentX: integer;
    // Name is the Y-coordinate of the cursor.  It is set in
    // @link(MouseMove) and used in @link(GetSelectedCursor).
    FCurrentY: integer;
    // @name: integer;
    // @name is the index of the layer boundary being moved.
    FLayerBeingMoved: integer;
    // @name: integer;
    // @name is the index of the row boundary being moved.
    FRowBeingMoved: integer;
    // @name starts to move a column or layer on the front view of the model.
    procedure BeginFrontMove(X, Y: Integer);
    // @name starts to move a row or layer on the side view of the model.
    procedure BeginSideMove(X, Y: Integer);
    // @name starts to move a column or row on the top view of the model.
    procedure BeginTopMove(X, Y: Integer);
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name returns the correct cursor for the top, front, or side
    // view of the model when the cursor is over a column, row, or layer
    // boundary is is moving a column, row, or layer boundary.
    function GetSelectedCursor: TCursor; override;
    // @name moves a column or layer on the front view of the model.
    procedure MoveColumnOrLayer(X, Y: Integer);
    // @name moves a column or row on the top view of the model.
    procedure MoveColumnOrRow(X, Y: Integer);
    // @name moves a row or layer on the side view of the model.
    procedure MoveRowOrLayer(X, Y: Integer);
    // @name indicates that a column boundary is being moved.
    property MovingColumn: boolean read FMovingColumn write SetMovingColumn;
    // @name indicates that a layer boundary is being moved.
    property MovingLayer: boolean read FMovingLayer write SetMovingLayer;
    // @name indicates that a row boundary is being moved.
    property MovingRow: boolean read FMovingRow write SetMovingRow;
    procedure DrawOnBitMap32(Sender: TObject;
      Buffer: TBitmap32); override;
  public
    procedure Activate; override;
    // @name starts to move a column, row, or layer boundary.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name sets the cursor.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name moves the column, row, or layer boundary.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name aborts movement.
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name deletes a column, row, or layer boundary.)}
  TDeleteGridBoundaryTool = class(TCustomGridCursorTool)
  protected
    // @name deletes the column or layer (or both) boundary at X,Y
    // on the front view of the model.
    procedure DeleteColumnOrLayer(X, Y: Integer);
    // @name deletes the column or row (or both) boundary at X,Y
    // on the top view of the model.
    procedure DeleteColumnOrRow(X, Y: Integer);
    // @name deletes the row or layer (or both) boundary at X,Y
    // on the side view of the model.
    procedure DeleteRowOrLayer(X, Y: Integer);
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name specifies the cursor to use when deleting a column, row, or layer
    // boundary.
    function GetSelectedCursor: TCursor; override;
  public
    // @name deletes a column, row, or layer boundary at X,Y.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is an abstract base class. Its descendants are
    used for cases where the user selects a range of cells on which
    to perform some action.)
    @SeeAlso(frmSetSpacingUnit.TSpacingGridTool)
    @SeeAlso(frmSubdivideUnit.TSubdivideGridTool)
    @SeeAlso(TColRowLayerSelectorTool)}
  TCustomCellSelectionTool = class(TCustomInteractiveTool)
  private
    function GetEvalAt: TEvaluatedAt;
  protected
    // @name draws the selected cells on the front view of the model.
    procedure DrawSelectedFrontCells(FirstCol, LastCol, FirstLayer,
      LastLayer: integer; const BitMap: TBitmap32; Direction: TViewDirection;
      Color: TColor = SelectedCellsColor);
    // @name draws the selected cells on the side view of the model.
    procedure DrawSelectedSideCells(FirstRow, LastRow, FirstLayer,
      LastLayer: integer; const BitMap: TBitmap32; Direction: TViewDirection;
      Color: TColor = SelectedCellsColor);
    // @name draws the selected cells on the top view of the model.
    procedure DrawSelectedTopCells(FirstCol, LastCol, FirstRow,
      LastRow: integer; const BitMap: TBitmap32; Direction: TViewDirection;
      Color: TColor = SelectedCellsColor);
  public
    procedure Activate;override;
  end;

  TColRowLayerSelectorTool = class (TCustomCellSelectionTool)
  private
    FNewRow: integer;
    FNewColumn: integer;
    FNewLayer: integer;
    FShouldDraw: boolean;
    procedure DrawNewColRowLayerSelection(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    procedure DrawExistingColRowLayerSelection(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    procedure GetCellUnderMouse(X, Y: integer);
    procedure SetNewSelection;
    procedure DrawASelection(Col, Row, Lay: Integer; Color1, Color2: TColor;
      const BitMap: TBitmap32; const Direction: TViewDirection);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    function GetHint: string; override;
  public
    property NewColumn: integer read FNewColumn;
    property NewRow: integer read FNewRow;
    property NewLayer: integer read FNewLayer;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Activate; override;
  end;

  {@abstract(@name is used to rotate the grid on the top view of the model.)}
  TRotateGridTool = class(TCustomInteractiveTool)
  private
    // @name: boolean;
    // See @link(Rotating).
    FRotating: boolean;
    // @name: double;
    // See @link(StartAngle).
    FStartAngle: double;
    // @name draws the rotated grid.
    procedure DrawRotatedGrid(Bitmap: TBitmap32);
    // @name returns the point at the center of the grid
    // on the top view of the model.
    function GridCenter: TPoint2D;
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    // @name sets the Hint of TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    // TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    // TfrmGoPhast.@link(TfrmGoPhast.frameSideView). Each is set to a
    // different value.
    procedure Activate; override;
    // @name starts rotating the grid.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name causes ZoomBox.Image32 to be redrawn.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  // @name rotates the grid.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // Abort grid rotation
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name indicates whether the grid is being rotated.
    property Rotating: boolean read FRotating;
    // @name indicates the grid angle when rotating was started.
    property StartAngle: double read FStartAngle;
  end;

  TCustomCrossSectionTool = class(TCustomInteractiveTool)
  protected
    function GetMeshOrDisvAvailable: boolean;
  end;

  TEditCrossSectionTool = class(TCustomCrossSectionTool)
  private
    FStartX: integer;
    FStartY: integer;
    FPoint1Selected: Boolean;
    FPoint2Selected: Boolean;
    FCurrentX: Integer;
    FCurrentY: Integer;
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name causes ZoomBox.Image32 to be redrawn.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TRotateCrossSectionTool = class(TCustomCrossSectionTool)
  private
    FCenterPoint: TPoint2D;
    FStart: TPoint2D;
    FStarted: Boolean;
    FCurrentX: Integer;
    FCurrentY: Integer;
    procedure GetNewLocation(X, Y: Integer; out NewLocation: TSegment2D);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TCustomStoreVerticesTool = class(TCustomInteractiveTool)
  private
    function FindPointInNearbyScreenObject(const APoint: TPoint;
      out NearbyPoint: TPoint2D): boolean;
    procedure ClearPoints;
  protected
    // @name: @link(TScreenObject);
    // See @link(CurrentScreenObject).
    FCurrentScreenObject: TScreenObject;
    FShift: TShiftState;
    FViewDirection: TViewDirection;
    FVisibleVertices: TRbwQuadTree;
    // @name contains instances of TScreenPointStorage.
    // @name is created and filled in @link(StorePointsOfOtherObjects).
    FStoredPoints: TList;
    // Store the screen coordinates of visible objects.
    procedure StorePointsOfOtherObjects(ScreenObject: TScreenObject);
    function GetSnapPoint(var FoundNearbyPoint: Boolean; var NearbyPoint: TPoint2D): TPoint;
  public
    // @name is the @link(TScreenObject) that is being created.
    property CurrentScreenObject: TScreenObject read FCurrentScreenObject
      write FCurrentScreenObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate; override;
  end;

  {@abstract(@name is an abstract base class
    for @link(TCustomInteractiveTool)s for creating @link(TScreenObject)s.)}
  TCustomCreateScreenObjectTool = class(TCustomStoreVerticesTool)
  private
    function CanAddPoint32: boolean;
  protected
    // @name: @link(TCustomUndo);
    // See @link(CurrentUndo).
    FCurrentUndo: TCustomUndo;
    // @name: boolean;
    // @name is set to true in @link(DoubleClick) and is used in @link(MouseUp)
    // to indicate when the @link(TScreenObject) has been completed.
    FDoubleClicked: boolean;
    // @name: integer;
    // @name is set to the X-coordinate of the cursor in @link(MouseUp).
    // In some descendants it is tested for identity with the current
    // X-coordinate of the cursor before being set and action is taken
    // if the location is different.
    FPriorCursorX: integer;
    // @name: integer;
    // @name is set to the Y-coordinate of the cursor in @link(MouseUp).
    // In some descendants it is tested for identity with the current
    // X-coordinate of the cursor before being set and action is taken
    // if the location is different.
    FPriorCursorY: integer;
    // @name returns true if the mouse is over the correct view to add a
    // point to @link(CurrentScreenObject).
    function CanAddPoint: boolean;
    // @name is used to undo or redo the creation of the @link(TScreenObject).
    property CurrentUndo: TCustomUndo read FCurrentUndo;
    // @name is used to set the default values for the elevation formulas
    // when creating a @link(TScreenObject).
    procedure SetDefaultElevationFormulas;
    function ShouldClosePolygon(X, Y: integer): boolean; virtual;
  public
    procedure RemoveScreenObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // @name responds to Double-Click events to set @link(FDoubleClicked)
    // to @true.  @link(FDoubleClicked) is used in @link(MouseUp)
    // to indicate when the @link(TScreenObject) has been completed.
    procedure DoubleClick(Sender: TObject); override;
    // @name shows the form that allows the user to specify
    // the properties of the new @link(TScreenObject).
    procedure FinishScreenObjects;
    // @name checks if @link(FDoubleClicked) is @true.  if so, it calls
    // @link(FinishScreenObjects).  It also sets @link(FPriorCursorX)
    // and @link(FPriorCursorY).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TCustomCreateScreenObjectTool32 = class(TCustomCreateScreenObjectTool)
  public
    procedure Activate; override;
  end;

  TCustomEditScreenObjectTool = class(TCustomStoreVerticesTool)
  private
    // @name returns @true if any @link(TScreenObject)s are selected
    // in the view to which this @classname applies.
    function AreScreenObjectsSelected: boolean;
    // @name is used to select (or deselect) the @link(TScreenObject) at X,Y.
    // @param(X is the X mouse coordinate.)
    // @param(Y is the Y mouse coordinate.)
    // @param(SelectLowerScreenObjects If SelectLowerScreenObjects is true,
    //   @name will look for a @link(TScreenObject) that is below the
    //   selected one and try to select it. )
    // @param(CanChangeSelection If CanChangeSelection is true, the
    //   @link(TScreenObject) at X,Y will be selected. otherwise, there will
    //   be no change in what is selected. )
    // @param(ReturnScreenObjectPresent ReturnScreenObjectPresent is only
    //   used if CanChangeSelection is false.  If ReturnScreenObjectPresent
    //   is true, @name returns true if there is a @link(TScreenObject) at
    //   X,Y that can be selected.  If ReturnScreenObjectPresent, @name
    //   returns true if there already is a selected @link(TScreenObject) at
    //   X,Y.)
    // @returns(@name returns @true if a @link(TScreenObject)
    // can be selected at X,Y. Otherwise it returns @false.)
    function SelectScreenObjects(const X, Y: integer;
      const SelectLowerScreenObjects, ToggleSelectedItem: boolean;
      const CanChangeSelection: boolean = True;
      const ReturnScreenObjectPresent: boolean = False): boolean;
  end;

  TScreenObjectTool = class (TCustomEditScreenObjectTool)
  private
    // @name is used to select @link(TScreenObject)s by enclosing them
    // in a polygon.
    // @name is the @link(TLine) used to select the @link(TScreenObject)s
    //  by enclosing them in a polygon.
    FSelectLine: TLine;
    function SelectScreenObjectsInGui(const ToggleSelection: boolean): boolean;
  protected
    function ScreenObjectInside(AScreenObject: TScreenObject): boolean; virtual;
  public
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to select @link(TScreenObject)s by enclosing them
    with a polygon.)}
  TLassoTool = class(TScreenObjectTool)
  protected
    function ScreenObjectInside(AScreenObject: TScreenObject): boolean;override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name returns @true if an entire object is selected by
    // being inside @link(FSelectLine). If ToggleSelection is true,
    // @link(TScreenObject)s inside
    // @link(FSelectLine) are toggled from Selected
    // to not Selected and vice-versa.
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    procedure Activate; override;
    // @name creates a new @link(FSelectLine) (and frees the old one)
    // and adds a point to it.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name adds points to @link(FSelectLine)
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name selects @link(TScreenObject)s with @link(FSelectLine).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TRulerTool = class(TCustomInteractiveTool)
  private
    FLine: TSimpleLine;
    FDoubleClicked: Boolean;
//    FRect: TRect;
//    FTimer: TTimer;
    FX: Integer;
    FY: Integer;
    FControl: TControl;
    procedure ShowHintAtAPoint(X: Integer; Y: Integer; Sender: TObject;
      ForceShow: Boolean = False);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    function GetHint: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Activate; override;
    procedure Deactivate; override;
    Destructor Destroy; override;
    procedure DoubleClick(Sender: TObject); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure DeleteLastSavedPoint;
    property Control: TControl read FControl;
  end;

  {@abstract(@name is used to create a point @link(TScreenObject).)}
  TCreatePointScreenObjectTool = class(TCustomCreateScreenObjectTool)
  private
    // @name creates the point @link(TScreenObject).
    procedure CreatePointScreenObject(X, Y: Integer; Shift: TShiftState);
  protected
    function GetCursor: TCursor; override;
  public
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to Mouse-Up events by creating a @link(TScreenObject).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to create a line or polygon @link(TScreenObject).)}
  TCreateLineScreenObjectTool = class(TCustomCreateScreenObjectTool32)
  private
    FStartX: Integer;
    FStartY: Integer;
    // @name creates TCustomCreateScreenObjectTool.@link(
    // TCustomStoreVerticesTool.CurrentScreenObject) if it does not
    // exist.  It adds a point at X,Y to @link(
    // TCustomStoreVerticesTool.CurrentScreenObject).
    procedure ContinueLineScreenObject(X, Y: Integer; Shift: TShiftState);
  protected
    function ShouldClosePolygon(X, Y: integer): boolean; override;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    function GetCursor: TCursor; override;
  public
    // @name sets the cursor and shows what the @link(TScreenObject)
    // would look like if the mouse button was clicked.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name checks that the cursor has moved from its previous position.
    // If it has, it calls @link(ContinueLineScreenObject).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to create a line @link(TScreenObject)
    in which the segments are aligned with the grid.)}
  TCreateStraightLineScreenObjectTool = class(TCustomCreateScreenObjectTool32)
  private
    // @name creates TCustomCreateScreenObjectTool.@link(
    // TCustomStoreVerticesTool.CurrentScreenObject) if it does not
    // exist.  It adds a point based on X,Y to @link(
    // TCustomStoreVerticesTool.CurrentScreenObject).  The point is added
    // so that the last segment is aligned with the grid.
    procedure ContinueStraightLineScreenObject(X, Y: Integer; Shift: TShiftState);
    procedure GetPointFromCursorPosition(var APoint: TPoint2D;
      X, Y: Integer; PreviousPoint: TPoint2D);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    function GetCursor: TCursor; override;
  public
    // @name sets the cursor and shows what the @link(TScreenObject)
    // would look like if the mouse button was clicked.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name checks that the cursor has moved from its previous position.
    // If it has, it calls @link(ContinueStraightLineScreenObject).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name creates a @link(TScreenObject) that is shaped like
    a rectangle aligned with the grid.)}
  TCreateRectangleScreenObjectTool = class(TCustomCreateScreenObjectTool32)
  private
    // If TCustomCreateScreenObjectTool.@link(
    // TCustomStoreVerticesTool.CurrentScreenObject) does not exist,
    // @name creates it and adds a point at X,Y.
    // If it does already exist, @name adds additional points to create
    // a @link(TScreenObject) shaped like a rectangle with the
    // point at X,Y at the opposite corner from the first point and
    // with the sides aligned with the grid.
    procedure ContinueRectangle(X, Y: Integer; Shift: TShiftState);
    procedure GetRemaingPointsOnRectangleCorners
      (FirstCorner, ThirdCorner: TPoint2D;
         var SecondCorner, FourthCorner: TPoint2D);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    function GetCursor: TCursor; override;
  public
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name sets the cursor and shows what the rectangle would look
    // if the mouse button were clicked.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name calls @link(ContinueRectangle).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is meant to be the abstract ancestor of tools
    that need to edit @link(TScreenObject)s .)}
  TCustomModifyGeometryTool = class(TCustomEditScreenObjectTool)
  private
    // @name returns an index to the edge of the selected
    // @link(TScreenObject) that is at X,Y.  If there isn't
    // an edge at X,Y, @name returns -1.
    function GetEdge(const X, Y: integer): integer;
  protected
    // In descendants, @name is used to edit a @link(TScreenObject).
    procedure DoEdit(const X,Y: integer); virtual; abstract;
    // In descendants, @name is used to set a different cursor at
    // different locations.
    procedure SetCursorAtLocation(const X, Y: Integer); virtual; abstract;
  public
    // @name responds to OnMouseDown events by selecting the
    // @link(TScreenObject) under the cursor.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events by changing the
    // cursor depending on whether or not the mouse
    //  is over a @link(TScreenObject) or not.
    // See @link(SetCursorAtLocation).
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to OnMouseUp events editing
    // the @link(TScreenObject) under the cursor.
    // See @link(DoEdit).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to delete a segment in a @link(TScreenObject).)}
  TDeleteSegmentTool = class(TCustomModifyGeometryTool)
  protected
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    {@name changes the cursor depending on whether or not the mouse
      is over a @link(TScreenObject) or not.)}
    procedure SetCursorAtLocation(const X, Y: Integer); override;
    // @name deletes a segment in a @link(TScreenObject).
    procedure DoEdit(const X, Y: integer); override;
  end;

  {@abstract(@name is used to insert a vertex into a @link(TScreenObject).)}
  TInsertPointTool = class(TCustomModifyGeometryTool)
  protected
    // @name insert a vertex into a @link(TScreenObject) at the position
    // closest to X,Y.
    procedure DoEdit(const X, Y: integer); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name change the cursor depending on whether or not the cursor is
    // over a @link(TScreenObject).
    procedure SetCursorAtLocation(const X, Y: Integer); override;
  end;

  TCustomAddPartTool = class(TCustomEditScreenObjectTool)
  protected
//    FScreenObject: TScreenObject;
  protected
    FUndoAddPart: TUndoAddPart;
    procedure SubmitUndo;
    procedure EnsureUndo;
  public
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Activate; override;
    procedure Deactivate; override;
    Destructor Destroy; override;
  end;

  TAddLinePartTool = class(TCustomAddPartTool)
  protected
    FNewPart: boolean;
    FDoubleClicked: boolean;
    function GetCursor: TCursor; override;
    function ShouldClosePolygon(X, Y: integer): boolean; virtual;
  public
    procedure FinishSection; virtual;
    procedure Activate; override;
    procedure DoubleClick(Sender: TObject); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TAddPolygonPartTool = class(TAddLinePartTool)
  private
    FStartX: Integer;
    FStartY: Integer;
  protected
    function GetCursor: TCursor; override;
    function ShouldClosePolygon(X, Y: integer): boolean; override;
  public
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure FinishSection; override;
  end;

  TAddPointPartTool = class(TCustomAddPartTool)
  protected
    function GetCursor: TCursor; override;
  public
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  { @abstract(@name is the abstract ancestor of tools used
    to select and move @link(TScreenObject)s mainly by clicking on them.)
    @SeeAlso(TLassoTool)}
  TCustomSelectScreenObjectTool = class abstract (TScreenObjectTool)
  private
    // @name is set to true while the mouse button is down.
    FMouseButtonIsDown: boolean;
    // @name is set to true when moving @link(TScreenObject)s.
    FMovingScreenObjects: boolean;
    // @name is the starting X-coordinate of the mouse.  It is used
    // when moving the selected @link(TScreenObject)s.
    FStartX: integer;
    // @name is the starting Y-coordinate of the mouse.  It is used
    // when moving the selected @link(TScreenObject)s.
    FStartY: integer;
    // @name moves a @link(TScreenObject) based on X and Y.
    procedure MoveScreenObjects(const X, Y: integer; Shift: TShiftState;
      SelectedNode: integer);
    // @name draws a rectangle outline the area that will be used to
    // select @link(TScreenObject)s when draging with the mouse.
    procedure DrawSelectionRectangle32(BitMap: TBitmap32);
  protected
    procedure GetOffset(const APoint: TPoint2D; out XOffset, YOffset: real); virtual;
  public
    // @name responds to OnMouseDown events by initializing @link(FStartX),
    // @link(FStartY), and @link(FMouseButtonIsDown).
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events. by updating
    // @link(FMovingScreenObjects).
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); override;
    // @name responds to OnMouseUp events. by setting @link(FMouseButtonIsDown)
    // to @false.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to select and move one or more individual vertices
    in a @link(TScreenObject).)
    Points can be selected in either of two ways.
    1. The user clicks on a node of the selected @link(TScreenObject).
       If no @link(TScreenObject) is selected, the one under the curson will
       be selected.
    2. The user drags with the mouse button down to select a group
       of nodes inside the selection rectangle.}
  TSelectPointTool = class abstract (TCustomSelectScreenObjectTool)
  private
    // @name: boolean;
    // @name is set to true if one or more vertices on the selected
    // @link(TScreenObject) is selected.
    FPointIsSelected: boolean;
    FSelectedNode: Integer;
    FSelectedNodeLocation: TPoint2D;
    // @name shows how the selected @link(TScreenObject) would look if the
    // mouse was released at its current position.
    procedure ShowMovedPoints(const BitMap: TBitmap32);
    // @name returns the @link(TScreenObject) that is already selected
    // and which has a node at or near X,Y.
    function FindSelectedScreenObject(const X, Y: integer): TScreenObject;
    // This procedure selects the node of a selected screen object
    // that is at or near (X,Y).
    function SelectPointsOfASelectedScreenObject(const X, Y: integer;
      const AddToSelection: boolean): boolean;
    // @name selects the nodes in
    // @name uses a "lasso" to select nodes in one selected object.
    // All other selected objects are de-selected.
    function SelectPointsOfAllSelectedScreenObjectsWithLine(
      const AddToSelection: boolean): boolean;
    // @name selects nodes that are inside TScreenObjectTool.@link(
    // TScreenObjectTool.FSelectLine).
    // if AddToSelection is not True, it deselects nodes that are not
    // inside  @link(TScreenObjectTool.FSelectLine).
    // Changed is set to True if any nodes have been
    // changed from unselected to selected or vice versa.
    function SelectPointsWithLine(const AScreenObject: TScreenObject;
      const AddToSelection: boolean; out Changed: boolean): boolean;
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    procedure GetOffset(const APoint: TPoint2D; out XOffset, YOffset: real); override;
    function GetCursor: TCursor; override;
  public
    procedure Activate; override;
    // @name responds to OnMouseDown events by seeing if a node
    // in a @link(TScreenObject) could be
    // selected at this position.  If so, it is selected.
    // It also creates an outline that is used in dragging to select points.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events by showing how the
    // @link(TScreenObject) would be changed if the mouse were released
    // at X,Y. See @link(ShowMovedPoints).
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to OnMouseUp events by either moving the selected
    // points or selecting the points inside a rectangle created by dragging
    // with the mouse button down.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to select and move one
    or more  @link(TScreenObject)s.)
    Points can be selected in either of two ways.
    1. The user clicks on a @link(TScreenObject).
    2. The user drags with the mouse button down to select a group
       of @link(TScreenObject) that are completely inside the selection
       rectangle.}
  TSelectScreenObjectTool = class(TCustomSelectScreenObjectTool)
  private
    // @name is set to @true when the user double-clicks.
    FDoubleClicked: boolean;
    // @name indicates that the selection rectangle should be drawn.
    FShouldDrawSelectionRectangle: boolean;
    FMaxX: double;
    FMinX: double;
    FMaxY: double;
    FMinY: double;
  protected
    function ScreenObjectInside(AScreenObject: TScreenObject): boolean;
      override;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name sets @link(FShouldDrawSelectionRectangle) to @True.
    procedure Activate; override;
    // @name responds to OnDoubleClick events by setting @link(FDoubleClicked)
    // to true.
    procedure DoubleClick(Sender: TObject); override;
    // @name responds to OnMouseDown events by initializing
    // @link(TCustomSelectScreenObjectTool.FMovingScreenObjects) and
    // @link(TCustomSelectScreenObjectTool).
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events by
    // redrawing the selection rectangle.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to OnMouseUp events by either selecting or moving
    // @link(TScreenObject)s.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name indicates that the selection rectangle should be drawn.
    property ShouldDrawSelectionRectangle: boolean
      read FShouldDrawSelectionRectangle write FShouldDrawSelectionRectangle;
  end;

  TEditVertexValueTool = class(TCustomEditScreenObjectTool)
  private
    FDoubleClicked: boolean;
    procedure EditPoint(const X, Y: integer);
  public
    procedure DoubleClick(Sender: TObject); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  // @name is used to select nodes and elements in @link(TSutraMesh2D) and
  // move or delete them.
  TMoveSutraNodesTool = class(TCustomInteractiveTool)
  private
    FSelectedNodes: TSutraNode2D_List;
    FSelectedElements: TSutraElement2D_List;
    FNodeQuadTree: TRbwQuadTree;
    // @name records the X-coordinate of the cursor when the @link(MouseDown)
    // is called.
    FStartX: Integer;
    // @name records the Y-coordinate of the cursor when the @link(MouseDown)
    // is called.
    FStartY: Integer;
    // @name updates @link(FNodeQuadTree) with all the nodes in the
    // @link(TSutraMesh2D). It also updates @link(FSelectedNodes) and
    // @link(FSelectedElements).
    procedure UpdateQuadTree;
  protected
    function GetCursor: TCursor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // @name deletes the selected nodes and elements in the SUTRA mesh.
    // It is called when the user clicks the delete key on the keyboard when
    // the @classname is active.
    procedure DeleteSelectedNodesAndElements;
    procedure Activate; override;
    // @name updates the selected nodes and elements in the SUTRA mesh.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name moves the selected nodes and the nodes of the selected elements
    // in the SUTRA mesh.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name displays @link(TfrmNodeLocation) which allows the user to edit
    // the node locations of the selected nodes and the nodes of the selected
    // elements numerically.
    procedure DoubleClick(Sender: TObject); override;
  end;

  // @name is used to define fishnet mesh elements which are used to
  // generate a mesh.
  TFishnetMeshGenerationTool = class(TCustomInteractiveTool)
  private
    FNodes: TRbwQuadTree;
    FFishnetGenerator: TFishnetMeshGenerator;
    FElement: TFishnetMeshElement;
    FEdit1: TRbwDataEntry;
    FEdit2: TRbwDataEntry;
    FSelectedElement: TFishnetMeshElement;
    FPanning: Boolean;
    FSelectedNode: TFishnetMeshNode;
    FUndoValues: TUndoFishnetMeshValues;
    FUndoGeometry: TUndoFishnetMesh;
    procedure EditChanged(Sender: TObject);
    procedure UpdateEdits(AnElement: TFishnetMeshElement);
    procedure SubmitUndo;
    procedure SubmitUndoValues;
  protected
    function GetCursor: TCursor; override;
    procedure CreateLayers; override;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure DoubleClick(Sender: TObject); override;
    procedure DeleteSelectedElement;
    procedure DeleteLastNode;
    procedure HideEdits;
    procedure UpdateQuadTree;
    procedure NilElement;
  end;

  TNewElement = class;
  TNewElementList = TList<TNewElement>;

  TNewNode = class(TObject)
  private
    FNode: TSutraNode2D;
    FLocation: TPoint2D;
    FNewElements: TNewElementList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNewNodeList = TList<TNewNode>;
  TNewNodeObjectList = TObjectList<TNewNode>;

  TNewElement = class(TObject)
  private
    FNodes: TNewNodeList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DrawEdges(const BitMap: TBitmap32; const ZoomBox: TQRbwZoomBox2;
      DrawPoints: boolean);
  end;

  TNewElementObjectList = TObjectList<TNewElement>;

  // @name is used to draw new SUTRA elements
  TDrawElementTool = class(TCustomInteractiveTool)
  private
    FNewNodes: TNewNodeObjectList;
    FNewElements: TNewElementObjectList;
    FNodeQuadTree: TRbwQuadTree;
    FNewNodeQuadTree: TRbwQuadTree;
    FMesh: TSutraMesh3D;
    FCurrentElement: TNewElement;
  protected
    procedure CreateLayers; override;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // @name either finds a node at or near where the mouse was clicked or
    // creates one there. The node is added to the current element.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Activate; override;
    // @name finalizes changes to the mesh.
    procedure Deactivate; override;
    // @name deletes the last node created. It is called when the user
    // clicks the Escape key on the keyboard while this @classname is active.
    procedure DeleteLastNode;
  end;

  TCustomModifyPilotPointTool = class(TCustomInteractiveTool)
  private
    FNewPoints: TSimplePointCollection;
    FQuadTree: TRbwQuadTree;
  protected
    procedure FillQuadTree; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate; override;
  end;

  TAddPilotPoint = class(TCustomModifyPilotPointTool)
  public
    procedure Activate; override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TDeletePilotPoint = class(TCustomModifyPilotPointTool)
  private
    FX: Integer;
    FY: Integer;
    FNewBetweeenObsPoints: TSimplePointCollection;
    // @name is used to select pilot points by enclosing them
    // in a polygon.
    // @name is the @link(TLine) used to select the @link(TPointItem)s
    //  by enclosing them in a polygon.
    FSelectLine: TLine;
  protected
    procedure CreateLayers; override;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    procedure FillQuadTree; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name adds points to @link(FSelectLine)
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

procedure CalculateCenterPoint(out FCenterPoint: TPoint2D);
procedure SetNewCrossSectionAngle(NewAngle:
  Double; out NewLocation: TSegment2D);

{$REGION 'Global_Variables'}
  var
    // @name is the instance of @link(TZoomTool) used in ModelMuse.
    ZoomTool: TZoomTool;
    // @name is the instance of @link(TZoomInTool) used in ModelMuse.
    ZoomInTool: TZoomInTool;
    // @name is the instance of @link(TZoomOutTool) used in ModelMuse.
    ZoomOutTool: TZoomOutTool;
    // @name is the instance of @link(TPanTool) used in ModelMuse.
    PanTool: TPanTool;
    // @name is the instance of @link(TAddGridBoundaryTool) used in ModelMuse.
    AddGridBoundaryTool: TAddGridBoundaryTool;
    // @name is the instance of @link(TMovingGridBoundaryTool) used in ModelMuse.
    MovingGridBoundaryTool: TMovingGridBoundaryTool;
    // @name is the instance of @link(TDeleteGridBoundaryTool) used in ModelMuse.
    DeleteGridBoundaryTool: TDeleteGridBoundaryTool;
    // @name is the instance of @link(TRotateGridTool) used in ModelMuse.
    RotateGridTool: TRotateGridTool;
    // @name is the instance of @link(TLassoTool) used in ModelMuse.
    LassoTool: TLassoTool;
    // @name is the instance of @link(TCreatePointScreenObjectTool)
    // used in ModelMuse.
    CreatePointScreenObjectTool: TCreatePointScreenObjectTool;
    // @name is the instance of @link(TCreateLineScreenObjectTool)
    // used in ModelMuse.
    CreateLineScreenObjectTool: TCreateLineScreenObjectTool;
    // @name is the instance of @link(TCreateStraightLineScreenObjectTool)
    // used in ModelMuse.
    CreateStraightLineScreenObjectTool: TCreateStraightLineScreenObjectTool;
    // @name is the instance of @link(TCreateRectangleScreenObjectTool)
    // used in ModelMuse.
    CreateRectangleScreenObjectTool: TCreateRectangleScreenObjectTool;
    // @name is the instance of @link(TDeleteSegmentTool) used in ModelMuse.
    DeleteSegmentTool: TDeleteSegmentTool;
    // @name is the instance of @link(TInsertPointTool) used in ModelMuse.
    InsertPointTool: TInsertPointTool;
    // @name is the instance of @link(TSelectPointTool) used in ModelMuse.
    SelectPointTool: TSelectPointTool;
    // @name is the instance of @link(TSelectScreenObjectTool) used in ModelMuse.
    SelectScreenObjectTool: TSelectScreenObjectTool;
    ColRowLayerSelectorTool: TColRowLayerSelectorTool;
    AddLinePartTool: TAddLinePartTool;
    AddPolygonPartTool: TAddPolygonPartTool;
    AddPointPartTool: TAddPointPartTool;
    EditVertexValueTool: TEditVertexValueTool;
    RulerTool: TRulerTool;
    EditCrossSectionTool: TEditCrossSectionTool;
    RotateCrossSectionTool: TRotateCrossSectionTool;
    MoveSutraNodesTool: TMoveSutraNodesTool;
    FishnetTool: TFishnetMeshGenerationTool;
    DrawElementTool: TDrawElementTool;
    AddPilotPointTool: TAddPilotPoint;
    DeletePilotPointTool: TDeletePilotPoint;

{$ENDREGION}

const
  // When selecting objects or nodes, the cursor may not be exactly over the
  // object to be selected.  @name defines how far off the cursor
  // can be and still select the object.
  // @name also controls how far you have to drag an object before
  // it will actually move.
  SelectionWidth = 5;

implementation

uses Math, CursorsFoiledAgain, GR32_Polygons, frmGoPhastUnit, frmSubdivideUnit,
  frmSetSpacingUnit, frmScreenObjectPropertiesUnit, BigCanvasMethods,
  LayerStructureUnit, DataSetUnit, Contnrs, frmPointValuesUnit,
  Dialogs, frmFishnetElementPropertiesUnit, MeshRenumbering, GR32_Backends,
  frmNodeLocationUnit, MeshRenumberingTypes;

resourcestring
  StrClickAndDragToZo = 'Click and drag to zoom in';
  StrClickToZoomIn = 'Click to zoom in';
  StrClickToZoomOut = 'Click to zoom out';
  StrClickAndDragToMo = 'Click and drag to move view';
  StrClickOnGridLineA = 'Click on grid line and drag to move it.';
  StrClickOnColumnLine = 'Click on column line and drag to move it.';
  StrClickOnRowLineAn = 'Click on row line and drag to move it.';
  StrClickOnGridBounda = 'Click on grid boundary to delete it';
  StrClickOnColumnBoun = 'Click on column boundary to delete it';
  StrClickOnRowBoundar = 'Click on row boundary to delete it';
  StrClickAndDragToSe = 'Click and drag to select objects with lasso';
  StrClickToCreatePoin = 'Click to create point object';
  StrClickToCreatePoly = 'Click to create polyline object';
  StrClickToCreatePolygon = 'Click to create polygon object';
  StrClickToCreateStra = 'Click to create straight line object';
  StrClickToCreateRect = 'Click to create rectangle object';
  StrClickToDeleteSegm = 'Click to delete segment';
  StrClickOnASegmentT = 'Click on a segment to create a new node there.';
  StrClickOnAVertexTo = 'Click on a vertex to select it';
  StrClickOnObjectToS = 'Click on object to select it or click and drag';
  StrClickToChangeSele = 'Click to change selected column, row, and layer.';
  StrClickToStartMeasu = 'Click to start measuring. Double-click to stop.';
  StrYouCanNotMoveThe = 'Moving the node located at (%0g, %1g) ' +
  'would cause one or more elements to be invalid. Are you sure you'
  + ' want to do this?';
  StrTheSelectedFishnet = 'The selected fishnet mesh element only has %d nod' +
  'es. If you created it accidentally, you can delete it now. Do you ' +
  'want to delete it?';
  StrLineLengthG = 'Line length = %g';

function GetCrossSection: TMeshCrossSectionLine;
begin
  if frmGoPhast.DisvUsed then
  begin
    result := frmGoPhast.PhastModel.DisvGrid.CrossSection;
  end
  else
  begin
    result := frmGoPhast.PhastModel.SutraMesh.CrossSection;
  end;
end;

function ConvertSidePoint(APoint: TPoint2D): TPoint2D;
begin
  result.x := APoint.y;
  result.y := APoint.x;
end;

// Adapted from FastGeo.
function VertexAngleRadians(x1,y1,x2,y2,x3,y3:TFloat):TFloat; overload;
var
  Dist      : TFloat;
  InputTerm : TFloat;
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
  x1   := x1 - x2;
  x3   := x3 - x2;
  y1   := y1 - y2;
  y3   := y3 - y2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1) * (x3 * x3 + y3 * y3);

  if IsEqual(Dist,Zero) then
    Result := Zero
  else
  begin
    InputTerm := (x1 * x3 + y1 * y3) / sqrt(Dist);
    if IsEqual(InputTerm,1.0) then
      Result := Zero
    else if IsEqual(InputTerm,-1.0) then
      Result := Pi
    else
      Result := ArcCos(InputTerm);
  end;
end;
(* End of Vertex Angle *)


function VertexAngleRadians(const Point1,Point2,Point3:TPoint2D):TFloat;
  overload;
begin
  Result := VertexAngleRadians(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;


type
  TScreenPointStorage = class(TObject)
    FScreenObject: TScreenObject;
    FVertexIndex: integer;
  end;

{$REGION 'TZoomTool'}
{ TZoomTool }

function TZoomTool.GetCursor: TCursor;
begin
  if ZoomBox.CanZoomIn then
  begin
    result := crZoom;
  end
  else
  begin
    result := crArrow;
  end;
end;

function TZoomTool.GetHint: string;
begin
  result := StrClickAndDragToZo;
end;

procedure TZoomTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    ZoomBox.BeginZoom(X, Y);
  end;
end;

procedure TZoomTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    UpdateCursors;
    with View do
    begin
      AdjustScales;
      ShowMagnification;
      MagnificationChanged := True;
      frmGoPhast.SynchronizeViews(ViewDirection);
    end;
    UpdateCursors;
  end;
  inherited;
end;
{ TZoomInTool }

function TZoomInTool.GetCursor: TCursor;
begin
  if ZoomBox.CanZoomIn then
  begin
    result := crZoomIn;
  end
  else
  begin
    result := crArrow;
  end;
end;

function TZoomInTool.GetHint: string;
begin
  result := StrClickToZoomIn;
end;

procedure TZoomInTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ZoomBox.ZoomByAt(2, X, Y);
    // If further zooming in is impossible, change the cursor.
    If not ZoomBox.CanZoomIn then
    begin
      UpdateCursors;
    end;
    with View do
    begin
      AdjustScales;
      ShowMagnification;
      MagnificationChanged := True;
      frmGoPhast.SynchronizeViews(ViewDirection);
    end;
    UpdateCursors;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'TZoomOutTool'}
{ TZoomOutTool }

function TZoomOutTool.GetCursor: TCursor;
begin
  if ZoomBox.CanZoomOut then
  begin
    result := crZoomOut;
  end
  else
  begin
    result := crArrow;
  end;
end;

function TZoomOutTool.GetHint: string;
begin
  result := StrClickToZoomOut;
end;

procedure TZoomOutTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ZoomBox.ZoomByAt(0.5, X, Y);
    with View do
    begin
      AdjustScales;
      ShowMagnification;
      MagnificationChanged := True;
      frmGoPhast.SynchronizeViews(ViewDirection);
    end;
    UpdateCursors;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'TPanTool'}
{ TPanTool }

function TPanTool.GetHint: string;
begin
  result := StrClickAndDragToMo;
end;

procedure TPanTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to pan to a different position.
  // Start panning and change the cursor to indicate that panning has
  // begun.
  if Button <> mbLeft then
  begin
    Exit;
  end;
  ZoomBox.Panning := True;
  Cursor := crHandGrab;
  Screen.Cursor := crHandGrab;
  frmGoPhast.CanDraw := False;
end;

procedure TPanTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // The user has finished panning to a different position.
  // Stop panning and change the cursor to indicate that panning has
  // stopped.
  inherited;
  frmGoPhast.CanDraw := True;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  ZoomBox.Panning := False;
  Cursor := crHandFlat;
  Screen.Cursor := crDefault;
  with View do
  begin
    MagnificationChanged := True;
    AdjustScales;
    frmGoPhast.SynchronizeViews(ViewDirection);
  end;
end;

procedure TPanTool.RightClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseUp(Sender, Button, Shift, X, Y);
end;
{$ENDREGION}

{$REGION 'TCustomGridTool'}
{ TCustomGridTool }

function TCustomGridTool.IsOnTopColumn(const X, Y: integer): boolean;
var
  RealPoint: TPoint2D;
  GridPoint: TPoint2D;
  Column: integer;
  Temp1, Temp2: real;
  MaxRow, MinRow: real;
begin
  // This function returns true if X,Y is over a column boundary
  // as seen from above.

  // Find the real world coordinates of the current location
  result := false;
  RealPoint.X := ZoomBox.X(X);
  RealPoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  GridPoint := frmGoPhast.
    Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealPoint);
  // Find the nearest column to that position..
  Column := frmGoPhast.Grid.NearestColumnPosition(GridPoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount) then
  begin
    // offset the point to the position of the nearest column
    GridPoint.X := frmGoPhast.Grid.ColumnPosition[Column];
    // rotate back to screen coordinates
    RealPoint := frmGoPhast.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(GridPoint);
    if IsInsideSelectionWidth(RealPoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        Temp1 := frmGoPhast.Grid.RowPosition[0];
        Temp2 := frmGoPhast.Grid.RowPosition[frmGoPhast.Grid.RowCount];
        MaxRow := Max(Temp1, Temp2);
        MinRow := Min(Temp1, Temp2);

        result := (GridPoint.Y >= MinRow) and (GridPoint.Y <= MaxRow);
      end;
    end;
  end;
end;

function TCustomGridTool.IsOnTopRow(const X, Y: integer): boolean;
var
  RealPoint: TPoint2D;
  GridPoint: TPoint2D;
  Row: integer;
begin
  // This function returns true if X,Y is over a row boundary
  // as seen from above.

  // Find the real world coordinates of the current location
  result := false;
  RealPoint.X := ZoomBox.X(X);
  RealPoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  GridPoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(RealPoint);
  // find the nearest row to that position.
  Row := frmGoPhast.Grid.NearestRowPosition(GridPoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.Grid.RowCount) then
  begin
    // offset the point to the position of the nearest row
    GridPoint.Y := frmGoPhast.Grid.RowPosition[Row];
    // rotate back to screen coordinates
    RealPoint := frmGoPhast.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(GridPoint);
    if IsInsideSelectionWidth(RealPoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        result := (GridPoint.X >= frmGoPhast.Grid.ColumnPosition[0])
          and (GridPoint.X <= frmGoPhast.Grid.ColumnPosition[
          frmGoPhast.Grid.ColumnCount]);
      end;
    end;
  end;
end;

function TCustomGridTool.IsOnFrontColumn(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Column: integer;
  Top, Bottom: real;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount) then
  begin
    APoint.X := frmGoPhast.Grid.ColumnPosition[Column];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        if Column = frmGoPhast.Grid.ColumnCount then
        begin
          Column := frmGoPhast.Grid.ColumnCount-1
        end;
        Top := frmGoPhast.Grid.CellElevation[
          Column,frmGoPhast.PhastModel.SelectedRow ,0];
        Bottom := frmGoPhast.Grid.CellElevation[
          Column,frmGoPhast.PhastModel.SelectedRow,frmGoPhast.Grid.LayerCount];
        if Column < frmGoPhast.Grid.ColumnCount-1 then
        begin
          Top :=
            Max(Top,frmGoPhast.Grid.CellElevation[
            Column+1,frmGoPhast.PhastModel.SelectedRow,0]);
          Bottom :=
            Min(Bottom,frmGoPhast.Grid.CellElevation[Column+1,
            frmGoPhast.PhastModel.SelectedRow,frmGoPhast.Grid.LayerCount]);
        end;

        result := (APoint.Y >= Bottom) and (APoint.Y <= Top);
      end;
    end;
  end;
end;

function TCustomGridTool.IsOnFrontLayer(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Layer: integer;
begin
  result := false;
  if frmGoPhast.PhastModel.ModelSelection <> msPhast then
  begin
    Exit;
  end;

  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.PhastGrid.ColumnCount <= 0)
        or (frmGoPhast.PhastGrid.RowCount <= 0)
        or (frmGoPhast.PhastGrid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        result := (APoint.X >= frmGoPhast.PhastGrid.ColumnPosition[0])
          and (APoint.X <= frmGoPhast.PhastGrid.ColumnPosition[
          frmGoPhast.PhastGrid.ColumnCount]);
      end;
    end;
  end;
end;

function TCustomGridTool.IsOnSideLayer(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Layer: integer;
begin
  result := false;
  if frmGoPhast.PhastModel.ModelSelection <> msPhast then
  begin
    Exit;
  end;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.PhastGrid.ColumnCount <= 0)
        or (frmGoPhast.PhastGrid.RowCount <= 0)
        or (frmGoPhast.PhastGrid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        result := (APoint.Y >= frmGoPhast.PhastGrid.RowPosition[0])
          and (APoint.Y <= frmGoPhast.PhastGrid.RowPosition[
          frmGoPhast.PhastGrid.RowCount]);
      end;
    end;
  end;
end;

function TCustomGridTool.IsOnSideRow(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Row: integer;
  Top, Bottom: real;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.Grid.RowCount) then
  begin
    APoint.Y := frmGoPhast.Grid.RowPosition[Row];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        if Row = frmGoPhast.Grid.RowCount then
        begin
          Row := frmGoPhast.Grid.RowCount-1;
        end;
        Top := frmGoPhast.Grid.CellElevation[frmGoPhast.PhastModel.SelectedColumn,
          Row, 0];
        Bottom := frmGoPhast.Grid.CellElevation[frmGoPhast.PhastModel.SelectedColumn,
          Row,frmGoPhast.Grid.LayerCount];
        if Row < frmGoPhast.Grid.RowCount-1 then
        begin
          Top :=
            Max(Top,frmGoPhast.Grid.CellElevation[frmGoPhast.PhastModel.SelectedColumn,
          Row+1, 0]);
          Bottom :=
            Min(Bottom,frmGoPhast.Grid.CellElevation[frmGoPhast.PhastModel.SelectedColumn,
            Row+1,frmGoPhast.Grid.LayerCount]);
        end;

        result := (APoint.X >= Bottom) and (APoint.X <= Top);
      end;
    end;
  end;
end;

procedure TCustomGridTool.ShowNewColumnOrLayer(const BitMap: TBitmap32);
var
  P1, P2: T3DRealPoint;
  CursorPoint: TPoint2D;
  Column: integer;
begin
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);

  if (frmGoPhast.Grid.ColumnCount > 0)
    and (frmGoPhast.Grid.RowCount > 0)
    and (frmGoPhast.Grid.LayerCount > 0) then
  begin
    if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingColumn) or
      frmGoPhast.tbAddVerticalBoundary.Down then
    begin
      if frmGoPhast.PhastModel.SelectedRow >= 0 then
      begin
        P1.X := CursorPoint.X;
        Column := frmGoPhast.Grid.NearestColumnCenter(P1.X);
        P1.Y := frmGoPhast.Grid.CellElevation[Column,
          frmGoPhast.PhastModel.SelectedRow,0];

        P2.X := CursorPoint.X;
        P2.Y := frmGoPhast.Grid.CellElevation[Column,frmGoPhast.
          PhastModel.SelectedRow,frmGoPhast.Grid.LayerCount];

        DrawBigPolyline32(BitMap, clBlack32, 1,
          [Point(ZoomBox.XCoord(P1.X),
            ZoomBox.YCoord(P1.Y)),
          Point(ZoomBox.XCoord(P2.X),
            ZoomBox.YCoord(P2.Y))], True);
      end;
    end
    else if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingLayer) or
      frmGoPhast.tbAddHorizontalBoundary.Down then
    begin
      if frmGoPhast.PhastModel.ModelSelection <> msPhast then
      begin
        Exit;
      end;
      P1.X := frmGoPhast.Grid.ColumnPosition[0];
      P1.Y := CursorPoint.Y;

      P2.X :=
        frmGoPhast.Grid.ColumnPosition[frmGoPhast.Grid.ColumnCount];
      P2.Y := CursorPoint.Y;

      DrawBigPolyline32(BitMap, clBlack32, 1,
        [Point(ZoomBox.XCoord(P1.X),
          ZoomBox.YCoord(P1.Y)),
        Point(ZoomBox.XCoord(P2.X),
          ZoomBox.YCoord(P2.Y))], True);
    end;
  end;
end;

procedure TCustomGridTool.ShowNewColumnOrRow(const BitMap: TBitmap32);
var
  P1, P2: TPoint2D;
  CursorPoint: TPoint2D;
begin
  // If you are editing the grid, show that.
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  CursorPoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(CursorPoint);

  if (frmGoPhast.Grid.ColumnCount > 0)
    and (frmGoPhast.Grid.RowCount > 0)
    and (frmGoPhast.Grid.LayerCount > 0) then
  begin
    if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingColumn)
      or frmGoPhast.tbAddVerticalBoundary.Down then
    begin
      // moving or adding a column
      P1.X := CursorPoint.X;
      P1.Y := frmGoPhast.Grid.RowPosition[0];
      P1 := frmGoPhast. Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P1);

      P2.X := CursorPoint.X;
      P2.Y := frmGoPhast.Grid.RowPosition[
        frmGoPhast.Grid.RowCount];
      P2 := frmGoPhast.Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P2);

      DrawBigPolyline32(BitMap, clBlack32, 1,
        [View.ConvertPoint(P1),View.ConvertPoint(P2)], True);
    end
    else if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingRow)
      or frmGoPhast.tbAddHorizontalBoundary.Down then
    begin
      // moving or adding a row
      P1.X := frmGoPhast.Grid.ColumnPosition[0];
      P1.Y := CursorPoint.Y;
      P1 := frmGoPhast.Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P1);

      P2.X := frmGoPhast.Grid.ColumnPosition[
        frmGoPhast.Grid.ColumnCount];
      P2.Y := CursorPoint.Y;
      P2 := frmGoPhast.Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P2);

      DrawBigPolyline32(BitMap, clBlack32, 1,
        [View.ConvertPoint(P1),View.ConvertPoint(P2)], True);
    end;
  end;
end;

procedure TCustomGridTool.ShowNewRowOrLayer(const BitMap: TBitmap32);
var
  P1, P2: T3DRealPoint;
  CursorPoint: TPoint2D;
  Row: integer;
begin
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);

  if (frmGoPhast.Grid.ColumnCount > 0)
    and (frmGoPhast.Grid.RowCount > 0)
    and (frmGoPhast.Grid.LayerCount > 0) then
  begin
    if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingLayer)
      or frmGoPhast.tbAddVerticalBoundary.Down then
    begin
      if frmGoPhast.PhastModel.ModelSelection <> msPhast then
      begin
        Exit;
      end;
      P1.Y := frmGoPhast.PhastGrid.RowPosition[0];
      P1.X := CursorPoint.X;

      P2.Y := frmGoPhast.PhastGrid.RowPosition
        [frmGoPhast.PhastGrid.RowCount];
      P2.X := CursorPoint.X;

      DrawBigPolyline32(BitMap, clBlack32, 1,
        [Point(ZoomBox.XCoord(P1.X),
          ZoomBox.YCoord(P1.Y)),
        Point(ZoomBox.XCoord(P2.X),
          ZoomBox.YCoord(P2.Y))], True);
    end
    else if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingRow)
      or frmGoPhast.tbAddHorizontalBoundary.Down then
    begin
      if frmGoPhast.PhastModel.SelectedColumn >= 0 then
      begin
        P1.Y := CursorPoint.Y;
        Row := frmGoPhast.Grid.NearestRowCenter(P1.X);
        P1.X := frmGoPhast.Grid.CellElevation[frmGoPhast.PhastModel.SelectedColumn,
          Row,0];

        P2.Y := CursorPoint.Y;
        P2.X := frmGoPhast.Grid.CellElevation[frmGoPhast.PhastModel.SelectedColumn,
          Row,frmGoPhast.Grid.LayerCount];

        DrawBigPolyline32(BitMap, clBlack32, 1,
          [Point(ZoomBox.XCoord(P1.X),
            ZoomBox.YCoord(P1.Y)),
          Point(ZoomBox.XCoord(P2.X),
            ZoomBox.YCoord(P2.Y))], True);

      end;
    end;
  end;
end;

function TCustomGridTool.IsInsideSelectionWidth(RealPoint: TPoint2D;
  const X, Y: Integer): boolean;
begin
  result := (Abs(ZoomBox.XCoord(RealPoint.X) - X) <= SelectionWidth)
    and (Abs(ZoomBox.YCoord(RealPoint.Y) - Y) <= SelectionWidth);
end;

procedure TCustomGridTool.DrawColumnRowOrLayer(const Direction: TViewDirection;
  const BitMap: TBitmap32);
begin
  case Direction of
    vdTop:
      ShowNewColumnOrRow(BitMap);
    vdFront:
      ShowNewColumnOrLayer(BitMap);
    vdSide:
      ShowNewRowOrLayer(BitMap);
  else
    Assert(False);
  end;
end;
{$ENDREGION}

{$REGION 'TAddGridBoundaryTool'}
{ TAddGridBoundaryTool }

procedure TAddGridBoundaryTool.Activate;
begin
  inherited;
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    frmGoPhast.frameTopView.ZoomBox.Hint := 'Click to add a column.'
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    frmGoPhast.frameTopView.ZoomBox.Hint := 'Click to add a row.'
  end
  else
  begin
    Assert(False);
  end;

  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    frmGoPhast.frameFrontView.ZoomBox.Hint := 'Click to add a column.'
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.frameFrontView.ZoomBox.Hint := 'Click to add a layer.'
    end
    else
    begin
      frmGoPhast.frameFrontView.ZoomBox.Hint := 'Layers can not be added this way.';
    end;
  end
  else
  begin
    Assert(False);
  end;

  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.frameSideView.ZoomBox.Hint := 'Click to add a layer.'
    end
    else
    begin
      frmGoPhast.frameSideView.ZoomBox.Hint := 'Layers can not be added this way.';
    end;
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    frmGoPhast.frameSideView.ZoomBox.Hint := 'Click to add a row.'
  end
  else
  begin
    Assert(False);
  end;
  CreateLayers;
end;

procedure TAddGridBoundaryTool.AddColumnOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(ZoomBox.X(X)));
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(ZoomBox.Y(Y)));
    end;
  end;
end;

procedure TAddGridBoundaryTool.AddColumnOrRow(X, Y: Integer);
var
  APoint: TPoint2D;
begin
  // transform the mouse coordinates to grid coordinates.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    // Add a column.
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(APoint.X));
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    // Add a row.
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(APoint.Y));
  end;
end;

procedure TAddGridBoundaryTool.AddRowOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(ZoomBox.X(X)));
    end;
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(ZoomBox.Y(Y)));
  end;
end;

procedure TAddGridBoundaryTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGophast.CurrentTool <> self then Exit;
  if Sender <> Layer32 then Exit;
  Layer32.BringToFront;
  Buffer.BeginUpdate;
  try
    DrawColumnRowOrLayer(View.ViewDirection, Buffer);
  finally
    Buffer.EndUpdate;
  end;
end;

function TAddGridBoundaryTool.GetCursor: TCursor;
begin
  result := crArrow;
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.dcAddColCursor.Cursor;
        end;
      vdFront, vdSide:
        begin
          result := crVertical;
        end;
    else
      Assert(False);
    end;
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.dcAddRowCursor.Cursor;
        end;
      vdFront, vdSide:
        begin
          result := crHorizontal;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TAddGridBoundaryTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    ZoomBox.InvalidateImage32;
  end;
end;

procedure TAddGridBoundaryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Add a column or row at the mouse position.
  if Button = mbLeft then
  begin
    case ViewDirection of
      vdTop: AddColumnOrRow(X, Y);
      vdFront: AddColumnOrLayer(X, Y);
      vdSide: AddRowOrLayer(X, Y);
    else
      Assert(False);
    end;
  end;
  inherited;
end;

{$ENDREGION}

{$REGION 'TMovingGridBoundaryTool'}
{ TMovingGridBoundaryTool }

procedure TMovingGridBoundaryTool.Activate;
begin
  inherited;
  CreateLayers;
end;

procedure TMovingGridBoundaryTool.BeginFrontMove(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Layer: integer;
begin
  // start to move a column or layer.

  if IsOverFrontColumnOrLayer(X,Y) then
  begin
    // The point is over the grid.

    // First find the nearest column or layer boundaries.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);

    if IsOnFrontColumn(X, Y) then
    begin
      Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
      // If the point is over a column, set the cursor.
      // and set MovingColumn to true.
      // Store the column that is being moved.
      Cursor := crMoveColumn;
      MovingLayer := False;
      MovingColumn := True;
      FColumnBeingMoved := Column;
    end
    else if IsOnFrontLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
      // If the point is over a layer, set the cursor.
      // and set MovingLayer to true.
      // Store the row that is being moved.
      Cursor := crMoveRow;
      MovingColumn := False;
      MovingLayer := True;
      FLayerBeingMoved := Layer;
    end;
  end;
end;

procedure TMovingGridBoundaryTool.BeginSideMove(X, Y: Integer);
var
  APoint: TPoint2D;
  Row, Layer: integer;
begin
  if IsOverSideRowOrLayer(X,Y) then
  begin
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);

    if IsOnSideLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
      Cursor := crMoveColumn;
      MovingRow := False;
      MovingLayer := True;
      FLayerBeingMoved := Layer;
    end
    else if IsOnSideRow(X, Y) then
    begin
      Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
      Cursor := crMoveRow;
      MovingLayer := False;
      MovingRow := True;
      FRowBeingMoved := Row;
    end;
  end;
end;

procedure TMovingGridBoundaryTool.BeginTopMove(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  // This procedure is used to start moving a column or row as seen from
  // the top.

  if IsOverTopColumnOrRow(X,Y) then
  begin
    // First find the nearest column or row boundaries.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    APoint := frmGoPhast.Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(APoint);

    // The point is over the grid.
    if IsOnTopColumn(X, Y) then
    begin
      Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
      // If the point is over a column, set the cursor.
      // and set MovingColumn to true.
      // Store the column that is being moved.
      Cursor := frmGoPhast.dcMoveColCursor.Cursor;

      MovingRow := False;
      MovingColumn := True;
      FColumnBeingMoved := Column;
    end
    else if IsOnTopRow(X, Y) then
    begin
      Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
      // If the point is over a row, set the cursor.
      // and set MovingRow to true.
      // Store the row that is being moved.
      Cursor := frmGoPhast.dcMoveRowCursor.Cursor;
      MovingColumn := False;
      MovingRow := True;
      FRowBeingMoved := Row;
    end;
  end;
end;

procedure TMovingGridBoundaryTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGophast.CurrentTool <> self then Exit;
  if MouseIsDown then
  begin
    Layer32.BringToFront;
    Buffer.BeginUpdate;
    try
      DrawColumnRowOrLayer(View.ViewDirection, Buffer);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;

function TMovingGridBoundaryTool.GetHint: string;
begin
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined: Assert(False);
    msPhast:
      begin
        result := StrClickOnGridLineA;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        case ViewDirection of
          vdTop: result := StrClickOnGridLineA;
          vdFront: result := StrClickOnColumnLine;
          vdSide: result := StrClickOnRowLineAn;
          else Assert(False);
        end;
      end
    else Assert(False);
  end;

end;

function TMovingGridBoundaryTool.GetSelectedCursor: TCursor;
begin
  result := crArrow;
  case ViewDirection of
    vdTop:
      begin
        if MovingColumn then
        begin
          result := frmGoPhast.dcMoveColCursor.Cursor;
        end
        else if MovingRow then
        begin
          result := frmGoPhast.dcMoveRowCursor.Cursor;
        end
        else if IsOnTopColumn(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving columns
          result := frmGoPhast.dcMoveColCursor.Cursor;
        end
        else if IsOnTopRow(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving rows
          result := frmGoPhast.dcMoveRowCursor.Cursor;
        end
        else
        begin
          result := crArrow;
        end;
      end;
    vdFront:
      begin
        if MovingColumn then
        begin
          result := crMoveColumn;
        end
        else if MovingLayer then
        begin
          result := crMoveRow;
        end
        else if IsOnFrontColumn(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving columns
          result := crMoveColumn;
        end
        else if IsOnFrontLayer(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving layers
          result := crMoveRow;
        end
        else
        begin
          result := crArrow;
        end;
      end;
    vdSide:
      begin
        if MovingLayer then
        begin
          result := crMoveColumn;
        end
        else if MovingRow then
        begin
          result := crMoveRow;
        end
        else if IsOnSideLayer(FCurrentX, FCurrentY) then
        begin
          result := crMoveColumn;
        end
        else if IsOnSideRow(FCurrentX, FCurrentY) then
        begin
          result := crMoveRow;
        end
        else
        begin
          result := crArrow;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TMovingGridBoundaryTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if Button <> mbLeft then Exit;
  case ViewDirection of
    vdTop: BeginTopMove(X, Y);
    vdFront: BeginFrontMove(X, Y);
    vdSide: BeginSideMove(X, Y);
  else
    Assert(False);
  end;
end;

procedure TMovingGridBoundaryTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    FCurrentX := X;
    FCurrentY := Y;
    inherited;
    if UseSelectedCursor then
    begin
      ZoomBox.InvalidateImage32;
    end;
  end;

end;

procedure TMovingGridBoundaryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    case ViewDirection of
      vdTop: MoveColumnOrRow(X, Y);
      vdFront: MoveColumnOrLayer(X, Y);
      vdSide: MoveRowOrLayer(X, Y);
    else
      Assert(False);
    end;
  end;
  inherited;
end;

procedure TMovingGridBoundaryTool.MoveColumnOrLayer(X, Y: Integer);
begin
  if not (MovingColumn or MovingLayer) then
  begin
    Exit;
  end;
  if MovingColumn then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create
      (FColumnBeingMoved, ZoomBox.X(X)));
  end
  else if MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create
      (FLayerBeingMoved, ZoomBox.Y(Y)));
  end;
  MovingColumn := False;
  MovingLayer := False;
end;

procedure TMovingGridBoundaryTool.MoveColumnOrRow(X, Y: Integer);
var
  APoint: TPoint2D;
begin
  // If the user isn't moving a column or row, quit.
  if not (MovingColumn or MovingRow) then
  begin
    Exit;
  end;
  // transform the mouse coordinates to grid coordinates.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);

  if MovingColumn then
  begin
    // move a column
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create(
      FColumnBeingMoved, APoint.X));
  end
  else if MovingRow then
  begin
    // move a row.
    frmGoPhast.UndoStack.Submit(TUndoMoveRow.Create(
      FRowBeingMoved, APoint.Y));
  end;
  // Stop moving columns and rows.
  MovingColumn := False;
  MovingRow := False;
end;

procedure TMovingGridBoundaryTool.MoveRowOrLayer(X, Y: Integer);
var
  APoint: TPoint2D;
begin
  if not (MovingRow or MovingLayer) then
  begin
    Exit;
  end;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  if MovingRow then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveRow.Create(FRowBeingMoved,
      APoint.Y));
  end
  else if MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create(FLayerBeingMoved,
      APoint.X));
  end;
  MovingRow := False;
  MovingLayer := False;
end;

procedure TMovingGridBoundaryTool.RightClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MovingColumn := False;
  MovingRow := False;
  MovingLayer := False;
end;

procedure TMovingGridBoundaryTool.SetMovingColumn(const Value: boolean);
begin
  FMovingColumn := Value;
  if Value then
  begin
    UseSelectedCursor := True;
  end
  else
  begin
    UseSelectedCursor := False;
  end;
end;

procedure TMovingGridBoundaryTool.SetMovingLayer(const Value: boolean);
begin
  FMovingLayer := Value;
  if Value then
  begin
    UseSelectedCursor := True;
  end
  else
  begin
    UseSelectedCursor := False;
  end;
end;

procedure TMovingGridBoundaryTool.SetMovingRow(const Value: boolean);
begin
  FMovingRow := Value;
  if Value then
  begin
    UseSelectedCursor := True;
  end
  else
  begin
    UseSelectedCursor := False;
  end;
end;
{$ENDREGION}

{$REGION 'TDeleteGridBoundaryTool'}
{ TDeleteGridBoundaryTool }

procedure TDeleteGridBoundaryTool.DeleteColumnOrLayer(X, Y: Integer);
var
  Column, Layer: integer;
  UndoDeleteColumn: TUndoDeleteColumn;
  UndoDeleteLayer: TUndoDeleteLayer;
begin
  if IsOverFrontColumnOrLayer(X,Y) then
  begin
    if IsOnFrontColumn(X, Y) then
    begin
      Column := frmGoPhast.Grid.NearestColumnPosition(ZoomBox.X(X));
      UndoDeleteColumn := TUndoDeleteColumn.Create(Column);
      frmGoPhast.UndoStack.Submit(UndoDeleteColumn);
    end;
    if IsOnFrontLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(ZoomBox.Y(Y));
      UndoDeleteLayer := TUndoDeleteLayer.Create(Layer);
      frmGoPhast.UndoStack.Submit(UndoDeleteLayer);
    end;
  end;
end;

procedure TDeleteGridBoundaryTool.DeleteColumnOrRow(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  if IsOverTopColumnOrRow(X,Y) then
  begin
    // The cursor is over the grid.

    // Convert to grid coordinates.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    APoint := frmGoPhast.Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
    if IsOnTopColumn(X, Y) then
    begin
      // the cursor is over a column so delete it.
      Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
      frmGoPhast.UndoStack.Submit(TUndoDeleteColumn.Create(Column));
    end;
    if IsOnTopRow(X, Y) then
    begin
      // the cursor is over a row so delete it.
      Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
      frmGoPhast.UndoStack.Submit(TUndoDeleteRow.Create(Row));
    end;
  end;
end;

procedure TDeleteGridBoundaryTool.DeleteRowOrLayer(X, Y: Integer);
var
  Row, Layer: integer;
  UndoDeleteRow: TUndoDeleteRow;
  UndoDeleteLayer: TUndoDeleteLayer;
begin
  if IsOverSideRowOrLayer(X,Y) then
  begin
    if IsOnSideLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(ZoomBox.X(X));
      UndoDeleteLayer := TUndoDeleteLayer.Create(Layer);
      frmGoPhast.UndoStack.Submit(UndoDeleteLayer);
    end;
    if IsOnSideRow(X, Y) then
    begin
      Row := frmGoPhast.Grid.NearestRowPosition(ZoomBox.Y(Y));
      UndoDeleteRow := TUndoDeleteRow.Create(Row);
      frmGoPhast.UndoStack.Submit(UndoDeleteRow);
    end;
  end;
end;

function TDeleteGridBoundaryTool.GetHint: string;
begin
  case frmGoPhast.PhastModel.ModelSelection of
    msPhast:
      begin
        result := StrClickOnGridBounda;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        case ViewDirection of
          vdTop: result := StrClickOnGridBounda;
          vdFront: result := StrClickOnColumnBoun;
          vdSide: result := StrClickOnRowBoundar;
        else
          Assert(False);
        end;
      end;
    else
      Assert(False);
  end;
end;

function TDeleteGridBoundaryTool.GetSelectedCursor: TCursor;
begin
  result := crDelete;
end;

procedure TDeleteGridBoundaryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    case ViewDirection of
      vdTop: DeleteColumnOrRow(X, Y);
      vdFront: DeleteColumnOrLayer(X, Y);
      vdSide: DeleteRowOrLayer(X, Y);
    else
      Assert(False);
    end;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'TCustomGridCursorTool'}
{TCustomGridCursorTool}

function TCustomGridCursorTool.GetNonSelectedCursor: TCursor;
begin
  result := crArrow;
end;

procedure TCustomGridCursorTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    case ViewDirection of
      vdTop: SetTopCursor(X, Y);
      vdFront: SetFrontCursor(X, Y);
      vdSide: SetSideCursor(X, Y);
    else
      Assert(False);
    end;
  end;
end;

function TCustomGridCursorTool.IsOverFrontColumnOrLayer(X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Column, Layer: integer;
begin
  // Determine whether (X,Y) (in screen coordinates) is over a column
  // or layer boundary on the front view of the model.

  // get the location in real-world coordinates of the current cursor location
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  // Get the column and layer boundaries closest to the current cursor.
  Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
  if ((Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount)) then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
      if (Layer <= 0) and (Layer > frmGoPhast.PhastGrid.LayerCount) then
      begin
        Result := False;
        Exit;
      end;
    end;


    // The location is over the grid
    if IsOnFrontColumn(X, Y) or IsOnFrontLayer(X, Y) then
    begin
      // The location is over a column or layer boundary
      result := True;
    end
    else
    begin
      result := False;
    end;
  end
  else
  begin
    // The location is not over the grid
    result := False;
  end;
end;

procedure TCustomGridCursorTool.SetFrontCursor(X, Y: integer);
begin
  if UseSelectedCursor then
  begin
    Cursor := GetSelectedCursor;
    Exit;
  end;

  if IsOverFrontColumnOrLayer(X, Y) then
  begin
    Cursor := GetSelectedCursor;
  end
  else
  begin
    Cursor := GetNonSelectedCursor;
  end;
end;

function TCustomGridCursorTool.IsOverSideRowOrLayer(X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Row, Layer: integer;
begin
  // Determine whether the point Y) (in screen coordinates) is over a
  // row or layer boundary on the side view of the model.

  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
  if ((Row >= 0) and (Row <= frmGoPhast.Grid.RowCount)) then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
      if (Layer <= 0) and (Layer > frmGoPhast.PhastGrid.LayerCount) then
      begin
        Result := False;
        Exit;
      end;
    end;
    // The location is over the grid.
    if IsOnSideRow(X, Y) or IsOnSideLayer(X, Y) then
    begin
      // The location is over a row or layer boundary
      result := True;
    end
    else
    begin
      result := False;
    end;
  end
  else
  begin
    // The location is not over the grid.
    result := False;
  end;
end;

procedure TCustomGridCursorTool.SetSideCursor(X, Y: integer);
begin
  if UseSelectedCursor then
  begin
    Cursor := GetSelectedCursor;
    Exit;
  end;

  if IsOverSideRowOrLayer(X, Y) then
  begin
    Cursor := GetSelectedCursor;
  end
  else
  begin
    Cursor := GetNonSelectedCursor;
  end;
end;

function TCustomGridCursorTool.IsOverTopColumnOrRow(X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  // Determine whether the point X,Y (in screen coordinates) is over
  // a column or row on the top view of the model.

  // Find the real world coordinates of the point
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  APoint :=
    frmGoPhast.Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  // find the nearest column and row to that position.
  Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
  Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
  if ((Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount))
    or ((Row >= 0) and (Row <= frmGoPhast.Grid.RowCount)) then
  begin
    // The point is over the grid
    if IsOnTopColumn(X, Y) or IsOnTopRow(X, Y) then
    begin
      // The point is over a column or row boundary
      result := True;
    end
    else
    begin
      result := False;
    end;
  end
  else
  begin
    // The point is not over the grid
    result := False;
  end;
end;

procedure TCustomGridCursorTool.SetTopCursor(X, Y: integer);
begin
  if UseSelectedCursor then
  begin
    Cursor := GetSelectedCursor;
    Exit;
  end;

  if IsOverTopColumnOrRow(X, Y) then
  begin
    Cursor := GetSelectedCursor;
  end
  else
  begin
    Cursor := GetNonSelectedCursor;
  end;
end;
{$ENDREGION}

{$REGION 'TRotateGridTool'}
{ TRotateGridTool }

procedure TRotateGridTool.Activate;
begin
  frmGoPhast.frameTopView.ZoomBox.Hint := frmGoPhast.tbGridAngle.Hint;
  frmGoPhast.frameFrontView.ZoomBox.Hint := 'Only the top view can be rotated.';
  frmGoPhast.frameSideView.ZoomBox.Hint := 'Only the top view can be rotated.';
  Layer32;
end;

procedure TRotateGridTool.DrawRotatedGrid(Bitmap: TBitmap32);
var
  ARealPoint: TPoint2D;
  Center: TPoint2D;
  NewAngle: double;
begin
  ARealPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  ARealPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  Center := GridCenter;
  NewAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
  Layer32.BringToFront;
  View.DrawRotatedGrid(NewAngle - RotateGridTool.StartAngle, Bitmap);
end;

function TRotateGridTool.GridCenter: TPoint2D;
begin
  result := View.GridCenter;
end;

procedure TRotateGridTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGophast.CurrentTool <> self then Exit;
  if Rotating then
  begin
    DrawRotatedGrid(Buffer);
  end;
end;

procedure TRotateGridTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ARealPoint: TPoint2D;
  Center: TPoint2D;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if ViewDirection = vdTop then
  begin
    ARealPoint.X := ZoomBox.X(X);
    ARealPoint.Y := ZoomBox.Y(Y);
    Center := GridCenter;
    FStartAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
    FRotating := True;
  end;
end;

procedure TRotateGridTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    if (ViewDirection = vdTop) and (ssLeft in Shift) then
    begin
      ZoomBox.InvalidateImage32;
    end;
  end;
end;

procedure TRotateGridTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ARealPoint: TPoint2D;
  Center: TPoint2D;
  NewAngle: double;
begin
  if Button = mbLeft then
  begin
    if ViewDirection = vdTop then
    begin
      ARealPoint.X := ZoomBox.X(X);
      ARealPoint.Y := ZoomBox.Y(Y);
      Center := GridCenter;
      NewAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
      FRotating := False;
      if NewAngle <> StartAngle then
      begin
        frmGoPhast.UndoStack.Submit(
          TUndoSetAngle.Create(frmGoPhast.Grid.GridAngle
          + NewAngle - StartAngle));
        frmGoPhast.SynchronizeViews(vdTop);
      end;
    end;
  end;
  inherited;
end;

procedure TRotateGridTool.RightClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRotating := false;
end;
{$ENDREGION}

{$REGION 'TLassoTool'}
{ TLassoTool }

procedure TLassoTool.Activate;
begin
  inherited;
  CreateLayers;
end;

procedure TLassoTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.CurrentTool <> self then Exit;
  if FSelectLine <> nil then
  begin
    Buffer.BeginUpdate;
    try
      FSelectLine.Draw(Buffer);
    finally
      Buffer.EndUpdate
    end;
  end;
end;

function TLassoTool.GetHint: string;
begin
  result := StrClickAndDragToSe;
end;

procedure TLassoTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  FSelectLine.Free;
  FSelectLine := TLine.Create(1000);
  FSelectLine.AddPoint(Point(X, Y));
  ZoomBox.InvalidateImage32;
end;

procedure TLassoTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    if (FSelectLine <> nil) then
    begin
      // set the correct cursor
      Cursor := crArrow;
      // If the cursor has moved far enough, add another point to the
      // lasso.
      if (FSelectLine.Count = 0)
        or (Abs(FSelectLine.Points[FSelectLine.Count - 1].X - X) > 10)
        or (Abs(FSelectLine.Points[FSelectLine.Count - 1].Y - Y) > 10) then
      begin
        FSelectLine.AddPoint(Point(X, Y));
        ZoomBox.InvalidateImage32;
      end;
    end;
  end;
end;

procedure TLassoTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if (FSelectLine <> nil) then
    begin
      // finish FSelectLine
      FSelectLine.AddPoint(Point(X, Y));
      FSelectLine.AddPoint(FSelectLine.Points[0]);
      // Select screen objects with SelectLine.
      SelectScreenObjectsInGui(ssShift in Shift);
      // Get rid of SelectLine
      FreeAndNil(FSelectLine);
      // redraw
      ZoomBox.InvalidateImage32;
      frmGoPhast.tbSelect.Down := true;
      frmGoPhast.tbSelect.OnMouseDown(frmGoPhast.tbSelect, mbLeft, [], 0, 0);
      frmGoPhast.tbSelectClick(frmGoPhast.tbSelect);
    end;
  end;
  inherited;
end;

function TLassoTool.ScreenObjectInside(AScreenObject: TScreenObject): boolean;
var
  APoint: TPoint;
  PointIsInside: Boolean;
  SectionIndex: Integer;
begin
  // If at least one point of the screen object is inside SelectLine
  // and SelectLine does not intersect the screen object then
  // the entire object is inside SelectLine.

  // PointIsInside  will be set to true if the first point in
  // AScreenObject is inside SelectLine.

  Assert(FSelectLine <> nil);
  PointIsInside := false;
  if AScreenObject.Count > 0 then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      APoint := AScreenObject.CanvasCoordinates[
        AScreenObject.SectionStart[SectionIndex]];
      PointIsInside := FSelectLine.Inside(APoint);
      if not PointIsInside then
      begin
        break;
      end;
    end;
  end;
  result := PointIsInside and not AScreenObject.SelectLines.Intersect(FSelectLine);
end;

function TScreenObjectTool.SelectScreenObjectsInGui(
  const ToggleSelection: boolean): boolean;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  Update: boolean;
  UndoChangeSelection: TUndoChangeSelection;
begin
  result := false;
  SelectingObjectsWithLine := True;
  try
    // SelectScreenObjectsWithLine returns true if an entire object is selected by
    // being inside SelectLine. If ToggleSelection is true, screen objects inside
    // SelectLine are toggled from Selected to not Selected and vice-versa.
    UndoChangeSelection := TUndoChangeSelection.Create;
    try
      Update := False;
      try
        for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
          // Do some simple checks to make sure the screen object can be selected.
          if (AScreenObject.ViewDirection = ViewDirection)
            and not AScreenObject.Deleted and AScreenObject.Visible then
          begin
            if ScreenObjectInside(AScreenObject) then
            begin
              if ToggleSelection then
              begin
                AScreenObject.Selected := not AScreenObject.Selected;
                if AScreenObject.Selected then
                begin
                  result := True;
                end;
                Update := True;
              end
              else
              begin
                if not AScreenObject.Selected then
                begin
                  AScreenObject.Selected := True;
                  Update := True;
                end;
                result := True;
              end;
            end
            else if not ToggleSelection then
            begin
              if AScreenObject.Selected then
              begin
                AScreenObject.Selected := False;
                Update := True;
              end;
            end;
          end
          else
          begin
            if AScreenObject.Selected then
            begin
              AScreenObject.Selected := False;
              Update := True;
            end;
          end;
        end;
      finally
        View.UpdateSelectRectangle;
        if Update then
        begin
          View.ScreenObjectsHaveChanged := True;
        end;
      end;
      UndoChangeSelection.SetPostSelection;
      // If the selection needs to be changed, add UndoChangeSelection
      // to the UndoStack. This will cause the selection to be changed.
      // The UndoStack will take over ownership of UndoChangeSelection.
      // If there is no change in what's selected.
      // get rid of UndoChangeSelection.
      if UndoChangeSelection.SelectionChanged then
      begin
        frmGoPhast.UndoStack.Submit(UndoChangeSelection);
      end
      else
      begin
        UndoChangeSelection.Free;
      end;
    except
      UndoChangeSelection.Free;
      raise;
    end;
  finally
    SelectingObjectsWithLine := False;
  end;
end;

{$ENDREGION}

{$REGION 'TCustomCellSelectionTool'}
{ TCustomCellSelectionTool }

procedure TCustomCellSelectionTool.Activate;
begin
  inherited;
  CreateLayers;
end;

function TCustomCellSelectionTool.GetEvalAt: TEvaluatedAt;
begin
  result := eaBlocks;
  if frmGoPhast.PhastModel.TopDataSet <> nil then
  begin
    result := frmGoPhast.PhastModel.TopDataSet.EvaluatedAt;
  end;
  if frmGoPhast.PhastModel.FrontDataSet <> nil then
  begin
    result := frmGoPhast.PhastModel.FrontDataSet.EvaluatedAt;
  end;
  if frmGoPhast.PhastModel.SideDataSet <> nil then
  begin
    result := frmGoPhast.PhastModel.SideDataSet.EvaluatedAt;
  end;
  if frmGoPhast.PhastModel.ThreeDDataSet <> nil then
  begin
    result := frmGoPhast.PhastModel.ThreeDDataSet.EvaluatedAt;
  end;
end;

procedure TCustomCellSelectionTool.DrawSelectedFrontCells(FirstCol,
  LastCol, FirstLayer, LastLayer: integer; const BitMap: TBitmap32;
  Direction: TViewDirection; Color: TColor = SelectedCellsColor);
const
  Opacity = 125;
var
  APoint: T3DRealPoint;
  L1, L2, C1, C2: integer;
  Polygon: TPointArray;
  SelectColor32: TColor32;
  CIndex, LIndex: integer;
  FrontPoints: T2DRealPointArray;
  APoint2D: TPoint2D;
  P: TPolygon32;
  MultiplePolygons: boolean;
begin
  P := nil;
  MultiplePolygons := False;
  Assert((FirstCol >= 0) and (LastCol >= 0)
    and (FirstLayer >= 0) and (LastLayer >= 0));
  if FirstCol < LastCol then
  begin
    C1 := FirstCol;
    C2 := LastCol + 1;
  end
  else
  begin
    C1 := LastCol;
    C2 := FirstCol + 1;
  end;
  if FirstLayer < LastLayer then
  begin
    L1 := FirstLayer;
    L2 := LastLayer + 1;
  end
  else
  begin
    L1 := LastLayer;
    L2 := FirstLayer + 1;
  end;

  case frmGoPhast.PhastModel.ModelSelection of
    msPhast:
      begin
        SetLength(Polygon, 4);
        if GetEvalAt = eaBlocks then
        begin
          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C1, 0, L1);
          Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C2, 0, L1);
          Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C2, 0, L2);
          Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C1, 0, L2);
          Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Z);
        end
        else
        begin
          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L1);
          Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2, 0, L1);
          Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2, 0, L2);
          Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L2);
          Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.X);
          Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Z);
        end;
        SelectColor32 := Color32(Color);
        SelectColor32 := SetAlpha(SelectColor32, Opacity);
        DrawBigPolygon32(BitMap, SelectColor32,
          SelectColor32, 0, Polygon, P, MultiplePolygons, True);
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        FrontPoints := frmGoPhast.PhastModel.SelectedModel.ModflowGrid.FrontCellPoints(
          frmGoPhast.PhastModel.SelectedModel.ModflowGrid.SelectedRow);
        if FrontPoints = nil then
        begin
          Exit;
        end;
        SetLength(Polygon, 6);

        for CIndex := C1 to C2 - 1 do
        begin
          for LIndex := L1 to L2 - 1 do
          begin
            APoint2D := FrontPoints[CIndex*2, LIndex];
            Polygon[0] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := FrontPoints[CIndex*2+1, LIndex];
            Polygon[1] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := FrontPoints[CIndex*2+2, LIndex];
            Polygon[2] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := FrontPoints[CIndex*2+2, LIndex+1];
            Polygon[3] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := FrontPoints[CIndex*2+1, LIndex+1];
            Polygon[4] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := FrontPoints[CIndex*2, LIndex+1];
            Polygon[5] := View(Direction).ConvertPoint(APoint2D);

            SelectColor32 := Color32(Color);
            SelectColor32 := SetAlpha(SelectColor32, Opacity);
            try
              // This is a work-around rather than a real fix.
              DrawBigPolygon32(BitMap, SelectColor32,
                SelectColor32, 0, Polygon, P, MultiplePolygons, True);
            except on EAccessViolation do
              begin
                frmGoPhast.AdjustDrawingWidth;
                Exit;
              end;
            end;
          end;
        end;
      end;
    msFootPrint:
      begin
        Assert(False);
        { TODO -cFootprint : Need to draw selected col or row. }
      end
    else
      Assert(False);
  end;

end;

procedure TCustomCellSelectionTool.DrawSelectedSideCells(FirstRow, LastRow,
  FirstLayer, LastLayer: integer; const BitMap: TBitmap32;
  Direction: TViewDirection; Color: TColor = SelectedCellsColor);
const
  Opacity = 125;
var
  APoint: T3DRealPoint;
  L1, L2, R1, R2: integer;
  Polygon: TPointArray;
  SelectColor32: TColor32;
  RIndex, LIndex: integer;
  SidePoints: T2DRealPointArray;
  APoint2D: TPoint2D;
  P: TPolygon32;
  MultiplePolygons: boolean;
begin
  P := nil;
  MultiplePolygons := False;
  Assert((FirstRow >= 0) and (LastRow >= 0)
    and (FirstLayer >= 0) and (LastLayer >= 0));
  if FirstRow < LastRow then
  begin
    R1 := FirstRow;
    R2 := LastRow + 1;
  end
  else
  begin
    R1 := LastRow;
    R2 := FirstRow + 1;
  end;

  if FirstLayer < LastLayer then
  begin
    L1 := FirstLayer;
    L2 := LastLayer + 1;
  end
  else
  begin
    L1 := LastLayer;
    L2 := FirstLayer + 1;
  end;

  case frmGoPhast.PhastModel.ModelSelection of
    msPhast:
      begin
        SetLength(Polygon, 4);
        if GetEvalAt = eaBlocks then
        begin
          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R1, L1);
          Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R2, L1);
          Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R2, L2);
          Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

          APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R1, L2);
          Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Y);
        end
        else
        begin
          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L1);
          Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R2, L1);
          Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R2, L2);
          Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

          APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L2);
          Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.Z);
          Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Y);
        end;

        SelectColor32 := Color32(Color);
        SelectColor32 := SetAlpha(SelectColor32, Opacity);

        DrawBigPolygon32(BitMap, SelectColor32,
          SelectColor32, 0, Polygon, P, MultiplePolygons, True);
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        SidePoints := frmGoPhast.PhastModel.SelectedModel.ModflowGrid.SideCellPoints(
          frmGoPhast.PhastModel.SelectedModel.ModflowGrid.SelectedColumn);
        if SidePoints = nil then
        begin
          Exit;
        end;
        SetLength(Polygon, 6);

        for RIndex := R1 to R2 - 1 do
        begin
          for LIndex := L1 to L2 - 1 do
          begin
            { TODO : Use conversion routines like ConvertPoint wherever possible. }
            APoint2D := SidePoints[RIndex*2, LIndex];
            Polygon[0] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := SidePoints[RIndex*2+1, LIndex];
            Polygon[1] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := SidePoints[RIndex*2+2, LIndex];
            Polygon[2] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := SidePoints[RIndex*2+2, LIndex+1];
            Polygon[3] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := SidePoints[RIndex*2+1, LIndex+1];
            Polygon[4] := View(Direction).ConvertPoint(APoint2D);

            APoint2D := SidePoints[RIndex*2, LIndex+1];
            Polygon[5] := View(Direction).ConvertPoint(APoint2D);

            SelectColor32 := Color32(Color);
            SelectColor32 := SetAlpha(SelectColor32, Opacity);
            try
              DrawBigPolygon32(BitMap, SelectColor32,
                SelectColor32, 0, Polygon, P, MultiplePolygons, True);
            except on EAccessViolation do
              begin
                // This is a work-around rather than a real fix.
                frmGoPhast.AdjustDrawingWidth;
                Exit;
              end;
            end;
          end;
        end;
      end;
    msFootPrint:
      begin
        Assert(False);
        { TODO -cFootprint : Need to draw selected col or row. }
      end
    else
      Assert(False);
  end;
end;

procedure TCustomCellSelectionTool.DrawSelectedTopCells(FirstCol, LastCol,
  FirstRow, LastRow: integer; const BitMap: TBitmap32;
  Direction: TViewDirection; Color: TColor = SelectedCellsColor);
const
  Opacity = 125;
var
  APoint: TPoint2D;
  R1, R2, C1, C2: integer;
  Polygon: TPointArray;
  SelectColor32: TColor32;
  P: TPolygon32;
  MultiplePolygons: boolean;
  Grid: TCustomModelGrid;
begin
  Grid := frmGoPhast.PhastModel.SelectedModel.Grid;
  P := nil;
  MultiplePolygons := False;
  Assert((FirstRow >= 0) and (LastRow >= 0)
    and (FirstCol >= 0) and (LastCol >= 0));

  if FirstRow >= Grid.RowCount then
  begin
    FirstRow := Grid.RowCount-1;
  end;
  if LastRow >= Grid.RowCount then
  begin
    LastRow := Grid.RowCount-1;
  end;
  if FirstCol >= Grid.ColumnCount then
  begin
    FirstCol := Grid.ColumnCount-1;
  end;
  if LastCol >= Grid.ColumnCount then
  begin
    LastCol := Grid.ColumnCount-1;
  end;

  SetLength(Polygon, 4);
  if FirstCol < LastCol then
  begin
    C1 := FirstCol;
    C2 := LastCol + 1;
  end
  else
  begin
    C1 := LastCol;
    C2 := FirstCol + 1;
  end;
  if FirstRow < LastRow then
  begin
    R1 := FirstRow;
    R2 := LastRow + 1;
  end
  else
  begin
    R1 := LastRow;
    R2 := FirstRow + 1;
  end;

  if GetEvalAt = eaBlocks then
  begin
    APoint := Grid.TwoDElementCorner(C1, R1);
    Polygon[0] := View(Direction).ConvertPoint(APoint);

    APoint := Grid.TwoDElementCorner(C1, R2);
    Polygon[1] := View(Direction).ConvertPoint(APoint);

    APoint := Grid.TwoDElementCorner(C2, R2);
    Polygon[2] := View(Direction).ConvertPoint(APoint);

    APoint := Grid.TwoDElementCorner(C2, R1);
    Polygon[3] := View(Direction).ConvertPoint(APoint);
  end
  else
  begin
    APoint := Grid.TwoDCellCorner(C1, R1);
    Polygon[0] := View(Direction).ConvertPoint(APoint);

    APoint := Grid.TwoDCellCorner(C1, R2);
    Polygon[1] := View(Direction).ConvertPoint(APoint);

    APoint := Grid.TwoDCellCorner(C2, R2);
    Polygon[2] := View(Direction).ConvertPoint(APoint);

    APoint := Grid.TwoDCellCorner(C2, R1);
    Polygon[3] := View(Direction).ConvertPoint(APoint);
  end;

  SelectColor32 := Color32(Color);
  SelectColor32 := SetAlpha(SelectColor32, Opacity);
  DrawBigPolygon32(BitMap, SelectColor32,
    SelectColor32, 0, Polygon, P, MultiplePolygons, True);
end;
{$ENDREGION}

{$REGION 'TCustomCreateScreenObjectTool'}
{ TCustomCreateScreenObjectTool }
procedure TCustomCreateScreenObjectTool32.Activate;
begin
  inherited;
  CreateLayers;
end;

function TCustomCreateScreenObjectTool.CanAddPoint: boolean;
  function CompatibleCursorGrid: boolean;
  begin
    result := false;
    case CurrentScreenObject.ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.CursorGrid = cgTop;
        end;
      vdFront:
        begin
          result := frmGoPhast.CursorGrid = cgFront;
        end;
      vdSide:
        begin
          result := frmGoPhast.CursorGrid = cgSide;
        end;
    else Assert(False);
    end;
  end;
begin
  result := (CurrentScreenObject = nil) or CompatibleCursorGrid;
end;

function TCustomCreateScreenObjectTool.CanAddPoint32: boolean;
  function CompatibleCursorGrid: boolean;
  begin
    result := false;
    case CurrentScreenObject.ViewDirection of
      vdTop:
        begin
          result := (Layer32 = FTopLayer);
        end;
      vdFront:
        begin
          result := (Layer32 = FFrontLayer);
        end;
      vdSide:
        begin
          result := (Layer32 = FSideLayer);
        end;
    else Assert(False);
    end;
  end;
begin
  result := (CurrentScreenObject = nil) or CompatibleCursorGrid;
end;

constructor TCustomCreateScreenObjectTool.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentUndo := nil;
end;

destructor TCustomCreateScreenObjectTool.Destroy;
begin
  FCurrentUndo.Free;
//  if (FCurrentUndo <> nil) and (FCurrentUndo is TUndoCreateScreenObject)
//    and not (TUndoCreateScreenObject(FCurrentUndo).HasBeenUsed) then
//  begin
//    FreeAndNil(FCurrentUndo)
//  end;
  inherited;
end;

procedure TCustomCreateScreenObjectTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FDoubleClicked := True;
end;

procedure TCustomCreateScreenObjectTool.FinishScreenObjects;
var
  UndoCreateScreenObject: TUndoCreateScreenObject;
begin
  if not frmGoPhast.CanEdit then
  begin
    //CurrentScreenObject.Free;
    frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
    CurrentScreenObject := nil;
    ClearPoints;
    FreeAndNil(FCurrentUndo);
    Exit;
  end;
  frmGoPhast.CanEdit := False;
  try
    if CurrentScreenObject <> nil then
    begin
      if CurrentScreenObject.Count <= 0 then
      begin
        frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
        CurrentScreenObject := nil;
        ClearPoints;
        FreeAndNil(FCurrentUndo);
        Exit;
      end;

      { TODO : This step should be replaced by a call to a virtual function. }
      if frmGoPhast.tbPolygon.Down then
      begin
        try
          CurrentScreenObject.AddPoint(CurrentScreenObject.Points[0], False);
        except on E: EScreenObjectError do
          begin
            Beep;
          end;
        end;
      end;

      if CurrentScreenObject.Closed then
      begin
        CurrentScreenObject.SetValuesOfEnclosedCells := True;
      end
      else
      begin
        CurrentScreenObject.SetValuesOfIntersectedCells := True;
      end;

      SetDefaultElevationFormulas;

      UndoCreateScreenObject := CurrentUndo as TUndoCreateScreenObject;
      try
        try
          Assert(frmScreenObjectProperties <> nil);

          frmScreenObjectProperties.GetData(CurrentScreenObject);
          if frmScreenObjectProperties.ShowModal <> mrOK then
          begin
            RemoveScreenObject;
          end
          else
          begin
            UndoCreateScreenObject.SetPostSelection;
            UndoCreateScreenObject.UpdateObservations;
            frmGoPhast.UndoStack.Submit(UndoCreateScreenObject);
            FCurrentUndo := nil;
            // FPreviousScreenObjects allows the screen object
            // to still be visible
            // in ZoomBox.Image32 while the grid is being
            // redrawn.
            View.PreviousScreenObjects.Add(CurrentScreenObject);
          end;
          CurrentScreenObject := nil;
          ClearPoints;
        finally
          if View.CanFocus then
          begin
            View.SetFocus;
          end;
        end;
      except
        frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
        CurrentScreenObject := nil;
        ClearPoints;
        FreeAndNil(FCurrentUndo);
        raise;
      end;
      View.ScreenObjectsHaveChanged := True;
    end;
  finally
    frmGoPhast.CanEdit := True;
  end;
end;

procedure TCustomCreateScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    if (FCurrentScreenObject = nil)
      and (FViewDirection <> ViewDirection)
      and (FStoredPoints <> nil) then
    begin
      ClearPoints
    end;
    FViewDirection := ViewDirection;
  end;
end;

procedure TCustomCreateScreenObjectTool.RemoveScreenObject;
begin
  frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
  FreeAndNil(FCurrentUndo);
end;

procedure TCustomCreateScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    if FDoubleClicked or ShouldClosePolygon(X, Y) then
    begin
      FinishScreenObjects;
      FDoubleClicked := False;
    end;
    FPriorCursorX := X;
    FPriorCursorY := Y;
    ZoomBox.InvalidateImage32;
  end;
end;

procedure TCustomCreateScreenObjectTool.SetDefaultElevationFormulas;
begin
  Assert(CurrentScreenObject <> nil);
  CurrentScreenObject.HigherElevationFormula :=
    frmGoPhast.PhastModel.DefaultHigherElevationFormula(
    CurrentScreenObject.ViewDirection, CurrentScreenObject.EvaluatedAt);
  CurrentScreenObject.LowerElevationFormula :=
    frmGoPhast.PhastModel.DefaultLowerElevationFormula(
    CurrentScreenObject.ViewDirection, CurrentScreenObject.EvaluatedAt);
  CurrentScreenObject.ElevationFormula :=
    frmGoPhast.PhastModel.DefaultElevationFormula(
    CurrentScreenObject.ViewDirection, CurrentScreenObject.EvaluatedAt);
end;

function TCustomCreateScreenObjectTool.ShouldClosePolygon(X,
  Y: integer): boolean;
begin
  result := False;
end;

procedure TCustomStoreVerticesTool.StorePointsOfOtherObjects(
  ScreenObject: TScreenObject);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  PointIndex: Integer;
  APoint: TPoint2D;
  AZoomBox: TQRbwZoomBox2;
  IntPoint: TPoint;
  PointStorage: TScreenPointStorage;
  LocalViewDirection: TViewDirection;
begin
  if FStoredPoints = nil then
  begin
    if ScreenObject <> nil then
    begin
      LocalViewDirection := ScreenObject.ViewDirection;
    end
    else
    begin
      LocalViewDirection := ViewDirection;
    end;
    AZoomBox := ZoomBox;
    FStoredPoints := TObjectList.Create;
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Visible and not AScreenObject.Deleted
        and (AScreenObject.ViewDirection = LocalViewDirection)
        and (AScreenObject <> ScreenObject) then
      begin
        for PointIndex := 0 to AScreenObject.Count - 1 do
        begin
          APoint := AScreenObject.Points[PointIndex];
          IntPoint.X := AZoomBox.XCoord(APoint.X);
          IntPoint.Y := AZoomBox.YCoord(APoint.Y);
          PointStorage := TScreenPointStorage.Create;
          FStoredPoints.Add(PointStorage);
          PointStorage.FScreenObject := AScreenObject;
          PointStorage.FVertexIndex := PointIndex;
          FVisibleVertices.AddPoint(IntPoint.X, IntPoint.Y, PointStorage);
        end;
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TCreatePointScreenObjectTool'}
{ TCreatePointScreenObjectTool }

procedure TCreatePointScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    FShift := Shift;
    UpdateCursors;
  end;
end;

procedure TCreatePointScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  if Button = mbLeft then
  begin
    CreatePointScreenObject(X, Y, Shift);
  end;
  inherited;
end;

procedure TCreatePointScreenObjectTool.CreatePointScreenObject(X, Y: Integer;
  Shift: TShiftState);
var
  APoint: TPoint2D;
begin
  // create the screen object.
  if frmGoPhast.tbPoint.Down then
  begin
    CurrentScreenObject :=
      frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
      frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
    CurrentScreenObject.ElevationCount :=
      TElevationCount(frmGoPhast.comboZCount.ItemIndex);
    ClearPoints;
  end
  else
  begin
    Assert(False);
  end;

  frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
  StorePointsOfOtherObjects(CurrentScreenObject);

  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
  end;

  // Add a point at the current mouse position.
  try
    CurrentScreenObject.AddPoint(APoint, False);
  except on E: EScreenObjectError do
    begin
      Beep;
    end
  end;
  // Get the object properties from the user.
  FinishScreenObjects;
end;

function TCreatePointScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapPointArrow;
  end
  else
  begin
    result := crPointArrow;
  end;
end;

function TCreatePointScreenObjectTool.GetHint: string;
begin
  result := StrClickToCreatePoin;
end;
{$ENDREGION}

{$REGION 'TCreateLineScreenObjectTool'}
{ TCreateLineScreenObjectTool }

procedure TCreateLineScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if Button = mbRight then Exit;
  if Button = mbLeft then
  begin
    if ((FPriorCursorX <> X) or (FPriorCursorY <> Y)) and CanAddPoint then
    begin
      ContinueLineScreenObject(X, Y, Shift);
    end;
  end;
  inherited;
end;

function TCreateLineScreenObjectTool.ShouldClosePolygon(X, Y: integer): boolean;
begin
  result := frmGoPhast.tbPolygon.Down
    and (CurrentScreenObject <> nil)
    and (CurrentScreenObject.Count > 3)
    and (Abs(X- FStartX) < SelectionWidth)
    and (Abs(Y- FStartY) < SelectionWidth);
  if result then
  begin
    CurrentScreenObject.Count := CurrentScreenObject.Count -1;
  end;
end;

procedure TCreateLineScreenObjectTool.ContinueLineScreenObject(X, Y: Integer;
  Shift: TShiftState);
var
  APoint: TPoint2D;
begin
  // If the screen object hasn't been started yet, start it.
  if CurrentScreenObject = nil then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else if frmGoPhast.tbPolygon.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
      FStartX := X;
      FStartY := Y;
    end
    else
    begin
      Exit;
//      Assert(False);
    end;
    CurrentScreenObject.ElevationCount := TElevationCount(frmGoPhast.comboZCount.ItemIndex);
    frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
    ClearPoints;
  end;
  StorePointsOfOtherObjects(CurrentScreenObject);
  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
  end;
  // Add a point at the current mouse position.
  try
    CurrentScreenObject.AddPoint(APoint, False);
  except on E: EScreenObjectError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TCreateLineScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: array of TPoint;
  FoundNearbyPoint: Boolean;
  NearbyPoint: TPoint2D;
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if (frmGoPhast.CurrentTool <> self) then Exit;
  if (CurrentScreenObject <> nil) and CanAddPoint32
    and (CurrentScreenObject.Count > 0)
    and (Sender = Layer32) then
  begin
    Buffer.BeginUpdate;
    try
      if frmGoPhast.tbLine.Down then
      begin
        SetLength(Points, 2);
        Points[0] := FCurrentScreenObject.
          CanvasCoordinates[FCurrentScreenObject.Count-1];
        Points[1] := GetSnapPoint(FoundNearbyPoint, NearbyPoint);
      end
      else if frmGoPhast.tbPolygon.Down then
      begin
        if (ssShift in FShift) then
        begin
          StorePointsOfOtherObjects(FCurrentScreenObject);
        end;
        SetLength(Points, 3);
        Points[0] := FCurrentScreenObject.
          CanvasCoordinates[FCurrentScreenObject.Count-1];
        Points[1] := GetSnapPoint(FoundNearbyPoint, NearbyPoint);
        Points[2] := FCurrentScreenObject.
          CanvasCoordinates[0];
      end
      else
      begin
        Exit;
        Assert(False);
      end;

      Layer32.BringToFront;
      DrawBigPolyline32(Buffer, clBlack32, 1, Points, True);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;

procedure TCreateLineScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    FShift := Shift;
    UpdateCursors;
    // redraw to indicate where the screen object would be if you clicked now.
    if (CurrentScreenObject <> nil) and CanAddPoint then
    begin
      if ssShift in Shift then
      begin
        StorePointsOfOtherObjects(CurrentScreenObject);
      end;
      Layer32.Changed;
      View.ZoomBox.InvalidateImage32;
    end;
  end;

end;

function TCreateLineScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);
  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      result := crSnapLineArrow;
    end
    else
    begin
      Assert(frmGoPhast.tbPolygon.Down);
      result := crSnapPolygonArrow;
    end;
  end
  else
  begin
    if frmGoPhast.tbLine.Down then
    begin
      result := crLineArrow;
    end
    else
    begin
      Assert(frmGoPhast.tbPolygon.Down);
      result := crPolygonArrow;
    end;
  end;
end;

function TCreateLineScreenObjectTool.GetHint: string;
begin
  if frmGoPhast.tbLine.Down then
  begin
    result := StrClickToCreatePoly;
  end
  else if frmGoPhast.tbPolygon.Down then
  begin
    result := StrClickToCreatePolygon;
  end
  else
  begin
    Assert(False);
  end;
end;
{$ENDREGION}

{$REGION 'TCreateStraightLineScreenObjectTool'}
{ TCreateStraightLineScreenObjectTool }

procedure TCreateStraightLineScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    FShift := Shift;
    UpdateCursors;

    // redraw to indicate where the screen object would be if you clicked now.
    if (CurrentScreenObject <> nil) and CanAddPoint then
    begin
      View.ZoomBox.InvalidateImage32;
    end;
  end;

  inherited;
end;

procedure TCreateStraightLineScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  if Button = mbLeft then
  begin
    if ((FPriorCursorX <> X) or (FPriorCursorY <> Y)) and not FDoubleClicked
      and CanAddPoint then
    begin
      ContinueStraightLineScreenObject(X, Y, Shift);
    end;
  end;
  inherited;
end;

procedure TCreateStraightLineScreenObjectTool.GetPointFromCursorPosition
  (var APoint: TPoint2D; X, Y: Integer; PreviousPoint: TPoint2D);
var
  YR: Real;
  XR: Real;
  Angle: Real;
  RotatedAngle: Real;
  R: Real;
  XPrimeD: Real;
  YPrimeD: Real;
  XPrimeI: Integer;
  YPrimeI: Integer;
  GridAngle: real;
begin
  case View.ViewDirection of
    vdTop:
      begin
        YR := APoint.Y - PreviousPoint.Y;
        XR := APoint.X - PreviousPoint.X;
        Angle := ArcTan2(YR, XR);
        if frmGoPhast.Grid <> nil then
        begin
          GridAngle :=  frmGoPhast.Grid.GridAngle
        end
        else if frmGoPhast.DisvUsed then
        begin
          GridAngle :=  frmGoPhast.ModflowGrid.GridAngle
        end
        else
        begin
          GridAngle := 0;
        end;
        RotatedAngle := Angle - GridAngle;
        R := Sqrt(Sqr(XR) + Sqr(YR));
        XPrimeD := Cos(RotatedAngle) * R;
        YPrimeD := Sin(RotatedAngle) * R;
        if Abs(XPrimeD) > Abs(YPrimeD) then
        begin
          APoint.X := PreviousPoint.X +
            Cos(GridAngle) * XPrimeD;
          APoint.Y := PreviousPoint.Y +
            Sin(GridAngle) * XPrimeD;
        end
        else
        begin
          APoint.X := PreviousPoint.X -
            Sin(GridAngle) * YPrimeD;
          APoint.Y := PreviousPoint.Y +
            Cos(GridAngle) * YPrimeD;
        end;
      end;
    vdFront, vdSide:
      begin
        XPrimeI := X - ZoomBox.XCoord(PreviousPoint.X);
        YPrimeI := Y - ZoomBox.YCoord(PreviousPoint.Y);
        if Abs(XPrimeI) > Abs(YPrimeI) then
        begin
          APoint.X := ZoomBox.X(X);
          APoint.Y := PreviousPoint.Y;
        end
        else
        begin
          APoint.X := PreviousPoint.X;
          APoint.Y := ZoomBox.Y(Y);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TCreateStraightLineScreenObjectTool.
  ContinueStraightLineScreenObject(X, Y: Integer; Shift: TShiftState);
var
  APoint, PreviousPoint: TPoint2D;
  FoundNearbyPoint: Boolean;
  NearbyPoint: TPoint2D;
begin
  // if the straight line object hasn't been started yet, begin it.
  if CurrentScreenObject = nil then
  begin
    if frmGoPhast.tbStraightLine.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else
    begin
      Exit;
//      Assert(False);
    end;
    CurrentScreenObject.ElevationCount := TElevationCount(frmGoPhast.comboZCount.ItemIndex);

    frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
    ClearPoints;
  end;
  StorePointsOfOtherObjects(CurrentScreenObject);
  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    FoundNearbyPoint := True;
  end
  else
  begin
    FoundNearbyPoint := True;
  end;
  NearbyPoint := APoint;
  // Check and see whether this is a new box or not.
  if CurrentScreenObject.count = 0 then
  begin
    // If it is a new box, add one point at the mouse position.
    try
      CurrentScreenObject.AddPoint(APoint, False);
    except on E: EScreenObjectError do
      begin
        Beep;
      end
    end;
  end
  else
  begin
    // If it is not a new box, add another point in such a way that the
    // new edge is parallel to one of the edges of the grid.
    PreviousPoint := CurrentScreenObject.Points[CurrentScreenObject.Count -
      1];
    GetPointFromCursorPosition(APoint, X, Y, PreviousPoint);

    try
      CurrentScreenObject.AddPoint(APoint, False);
    except on E: EScreenObjectError do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end
    end;

    if FoundNearbyPoint and
      ((NearbyPoint.x <> APoint.x) or (NearbyPoint.y <> APoint.y)) then
    begin
      try
        CurrentScreenObject.AddPoint(NearbyPoint, False);
      except on E: EScreenObjectError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
        end
      end;
    end;
  end;
end;

procedure TCreateStraightLineScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: array of TPoint;
  APoint, PreviousPoint: TPoint2D;
  SnapPoint: TPoint;
  FoundNearbyPoint: Boolean;
  NearbyPoint: TPoint2D;
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if (frmGoPhast.CurrentTool <> self) then Exit;
  if (CurrentScreenObject <> nil) and CanAddPoint32
    and (CurrentScreenObject.Count > 0)
    and (Sender = Layer32) then
  begin
    Buffer.BeginUpdate;
    try
      SnapPoint := GetSnapPoint(FoundNearbyPoint, NearbyPoint);
      if FoundNearbyPoint then
      begin
        APoint := NearbyPoint;
      end
      else
      begin
        APoint.X := ZoomBox.X(SnapPoint.X);
        APoint.Y := ZoomBox.Y(SnapPoint.Y);
      end;
      SetLength(Points, 2);
      PreviousPoint := CurrentScreenObject.Points[CurrentScreenObject.Count -
        1];

      GetPointFromCursorPosition(APoint, SnapPoint.X,
        SnapPoint.Y, PreviousPoint);

      if View.ViewDirection = vdSide then
      begin
        PreviousPoint := ConvertSidePoint(PreviousPoint);
        APoint := ConvertSidePoint(APoint);
      end;


      Points[0] := View.ConvertPoint(PreviousPoint);
      Points[1] := View.ConvertPoint(APoint);

      Layer32.BringToFront;
      DrawBigPolyline32(Buffer, clBlack32, 1, Points, True);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;

function TCreateStraightLineScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapStraightLineArrow;
  end
  else
  begin
    result := crStraightLineArrow;
  end;
end;

function TCreateStraightLineScreenObjectTool.GetHint: string;
begin
  result := StrClickToCreateStra;
end;
{$ENDREGION}

{$REGION 'TCreateRectangleScreenObjectTool'}
{ TCreateRectangleScreenObjectTool }

procedure TCreateRectangleScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    FShift := Shift;
    UpdateCursors;

    // redraw to indicate where the screen object would be if you clicked now.
    if (CurrentScreenObject <> nil) and CanAddPoint then
    begin
      View.ZoomBox.InvalidateImage32;
    end;
  end;

  inherited;
end;

procedure TCreateRectangleScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  if Button = mbLeft then
  begin
    if CanAddPoint then
    begin
      ContinueRectangle(X, Y, Shift);
    end;
  end;
  inherited;
end;

procedure TCreateRectangleScreenObjectTool.GetRemaingPointsOnRectangleCorners
  (FirstCorner, ThirdCorner: TPoint2D;
  var SecondCorner, FourthCorner: TPoint2D);
var
  YPrime: Real;
  XPrime: Real;
  R: Real;
  RotatedAngle: Real;
  Angle: Real;
  XR: Real;
  YR: Real;
  GridAngle: Real;
begin
  case ViewDirection of
    vdTop:
      begin
        YR := ThirdCorner.Y - FirstCorner.Y;
        XR := ThirdCorner.X - FirstCorner.X;
        Angle := ArcTan2(YR, XR);
        if frmGoPhast.Grid <> nil then
        begin
          GridAngle := frmGoPhast.Grid.GridAngle;
        end
        else if frmGoPhast.DisvUsed then
        begin
          GridAngle := frmGoPhast.ModflowGrid.GridAngle;
        end
        else
        begin
          GridAngle := 0;
        end;
        RotatedAngle := Angle - GridAngle;
        R := SqrT(Sqr(XR) + Sqr(YR));
        XPrime := Cos(RotatedAngle) * R;
        YPrime := Sin(RotatedAngle) * R;
        SecondCorner.X := FirstCorner.X +
          Cos(GridAngle) * XPrime;
        SecondCorner.Y := FirstCorner.Y +
          Sin(GridAngle) * XPrime;
        FourthCorner.X := FirstCorner.X -
          Sin(GridAngle) * YPrime;
        FourthCorner.Y := FirstCorner.Y +
          Cos(GridAngle) * YPrime;
      end;
    vdFront, vdSide:
      begin
        SecondCorner.X := FirstCorner.X;
        SecondCorner.Y := ThirdCorner.Y;
        FourthCorner.X := ThirdCorner.X;
        FourthCorner.Y := FirstCorner.Y;
      end;
  end;
end;

procedure TCreateRectangleScreenObjectTool.ContinueRectangle(X, Y: Integer;
  Shift: TShiftState);
var
  ThirdCorner, SecondCorner, FourthCorner, FirstCorner: TPoint2D;
begin
  // if the box hasn't been started yet, begin it.
  if CurrentScreenObject = nil then
  begin
    if frmGoPhast.tbRectangle.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else
    begin
      Exit;
//      Assert(False);
    end;
    CurrentScreenObject.ElevationCount := TElevationCount(
      frmGoPhast.comboZCount.ItemIndex);

    frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
    ClearPoints;
  end;
  StorePointsOfOtherObjects(CurrentScreenObject);
  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), ThirdCorner) then
  begin
    // Get the real-world coordinates of the mouse.
    ThirdCorner.X := ZoomBox.X(X);
    ThirdCorner.Y := ZoomBox.Y(Y);
  end;
  // Check and see whether this is a new box or not.
  if CurrentScreenObject.count = 0 then
  begin
    // If it is a new box, add one point at the mouse position.
    try
      CurrentScreenObject.AddPoint(ThirdCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;
  end
  else
  begin
    // If it is not a new box, add 4 points to finish the box.

    // The edges of box should be parallel to the grid.
    FirstCorner := CurrentScreenObject.Points[CurrentScreenObject.Count - 1];

    GetRemaingPointsOnRectangleCorners (FirstCorner, ThirdCorner,
      SecondCorner, FourthCorner);

    try
      CurrentScreenObject.AddPoint(SecondCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;

    try
      CurrentScreenObject.AddPoint(ThirdCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;

    try
      CurrentScreenObject.AddPoint(FourthCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;

    try
      CurrentScreenObject.AddPoint(CurrentScreenObject.Points[0], False);
    except on E: EScreenObjectError do
      begin
        Beep;
      end
    end;

    // Get the object properties from the user.
    FinishScreenObjects;
  end;
end;

procedure TCreateRectangleScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: array of TPoint;
  ThirdCorner, SecondCorner, FourthCorner, FirstCorner: TPoint2D;
  SnapPoint: TPoint;
  FoundNearbyPoint: Boolean;
  NearbyPoint: TPoint2D;
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if (frmGoPhast.CurrentTool <> self) then Exit;
  if (CurrentScreenObject <> nil) and CanAddPoint32
    and (CurrentScreenObject.Count > 0)
    and (Sender = Layer32) then
  begin
    Buffer.BeginUpdate;
    try
      SetLength(Points, 5);
      FirstCorner := CurrentScreenObject.Points[CurrentScreenObject.Count - 1];

      SnapPoint := GetSnapPoint(FoundNearbyPoint, NearbyPoint);
      if FoundNearbyPoint then
      begin
        ThirdCorner := NearbyPoint;
      end
      else
      begin
        ThirdCorner.X := ZoomBox.X(SnapPoint.X);
        ThirdCorner.Y := ZoomBox.Y(SnapPoint.Y);
      end;

      GetRemaingPointsOnRectangleCorners (FirstCorner, ThirdCorner,
        SecondCorner, FourthCorner);

      if View.ViewDirection = vdSide then
      begin
        FirstCorner := ConvertSidePoint(FirstCorner);
        SecondCorner := ConvertSidePoint(SecondCorner);
        FourthCorner := ConvertSidePoint(FourthCorner);
      end;

      Points[0] := View.ConvertPoint(FirstCorner);
      Points[1] := View.ConvertPoint(SecondCorner);
      Points[2] := SnapPoint;
      Points[3] := View.ConvertPoint(FourthCorner);
      Points[4] := Points[0];

      Layer32.BringToFront;
      DrawBigPolyline32(Buffer, clBlack32, 1, Points, True);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;

function TCreateRectangleScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapRectangleArrow;
  end
  else
  begin
    result := crRectangleArrow;
  end;
end;

function TCreateRectangleScreenObjectTool.GetHint: string;
begin
  result := StrClickToCreateRect;
end;
{$ENDREGION}

{$REGION 'TCustomEditScreenObjectTool'}
{ TCustomEditScreenObjectTool }

function TCustomEditScreenObjectTool.AreScreenObjectsSelected: boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  // This procedure returns True if any screen object
  // that has a TScreenObject.ViewDirection
  // that matches ViewDirection
  // is selected.
  result := False;
  if frmGoPhast.PhastModel.SelectedScreenObjectCount > 0 then
  begin
    for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      if AScreenObject.Selected
        and not AScreenObject.Deleted
        and (AScreenObject.ViewDirection = ViewDirection)
        then
      begin
        result := True;
        Exit;
      end;
    end;
  end;
end;

function TCustomEditScreenObjectTool.SelectScreenObjects(const X, Y: integer;
  const SelectLowerScreenObjects, ToggleSelectedItem: boolean;
  const CanChangeSelection: boolean = True;
  const ReturnScreenObjectPresent: boolean = False): boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
  Update: boolean;
  Start: integer;
  UndoChangeSelection: TUndoChangeSelection;
begin
  if not SelectLowerScreenObjects and not ToggleSelectedItem and
    CanChangeSelection and not ReturnScreenObjectPresent then
  begin
    // This may be the first click of a double-click.
    // If so, exit without doing anything.
    with frmGoPhast.PhastModel do
    begin
      for Index := 0 to ScreenObjectCount - 1 do
      begin
        AScreenObject := ScreenObjects[Index];
        if AScreenObject.Selected and (AScreenObject.ViewDirection =
          ViewDirection) then
        begin
          if AScreenObject.Select(X, Y) then
          begin
            result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;

  // This procedure selects screen objects that are at or near (X,Y).
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    if SelectLowerScreenObjects then
    begin
      // Start before the first selected TScreenObject at X,Y.
      Start := frmGoPhast.PhastModel.ScreenObjectCount - 1;
      for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if (AScreenObject.ViewDirection = ViewDirection) then
        begin
          if AScreenObject.Select(X, Y) then
          begin
            if AScreenObject.Selected then
            begin
              Start := Index - 1;
              if CanChangeSelection and not ToggleSelectedItem then
              begin
                AScreenObject.Selected := False;
              end;
              if Start = -1 then
              begin
                Start := frmGoPhast.PhastModel.ScreenObjectCount - 1
              end;
            end
            else
            begin
              // break;
            end;
          end;
        end;
      end;
    end
    else
    begin
      // Start at the last TScreenObject.
      Start := frmGoPhast.PhastModel.ScreenObjectCount - 1;
    end;

    try
      for Index := Start downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if result then
        begin
          // generally speaking, the TScreenObject that
          // is already selected should be deselected.
          // if another TScreenObject is being selected.
          if CanChangeSelection and AScreenObject.Selected
            and not ToggleSelectedItem then
          begin
            AScreenObject.Selected := False;
            Update := True;
          end;
          if (AScreenObject.ViewDirection <> ViewDirection) then
          begin
            // Deselect TScreenObjects that are on a diffent view of the model.
            if CanChangeSelection and AScreenObject.Selected then
            begin
              AScreenObject.Selected := False;
              Update := True;
            end;
          end;
          continue;
        end;
        if (AScreenObject.ViewDirection <> ViewDirection) then
        begin
          // Deselect TScreenObjects that are on a diffent view of the model.
          if CanChangeSelection and AScreenObject.Selected then
          begin
            AScreenObject.Selected := False;
            Update := True;
          end;
        end
        else
        begin
          if AScreenObject.Select(X, Y) then
          begin
            if not CanChangeSelection then
            begin
              if ReturnScreenObjectPresent then
              begin
                result := True;
              end
              else
              begin
                result := AScreenObject.Selected;
              end;
              if result then
              begin
                UndoChangeSelection.Free;
                Exit;
              end
              else
              begin
                Continue;
              end;
            end;
            if ToggleSelectedItem then
            begin
              AScreenObject.Selected := not AScreenObject.Selected;
              Update := True;
              if AScreenObject.Selected then
              begin
                result := True;
              end;
            end
            else
            begin
              if not AScreenObject.Selected then
              begin
                AScreenObject.Selected := True;
                Update := True;
              end;
              result := True;
            end;
          end
          else
          begin
            if not CanChangeSelection then
            begin
              Continue;
            end;
            if not ToggleSelectedItem then
            begin
              if AScreenObject.Selected then
              begin
                AScreenObject.Selected := False;
                Update := True;
              end;
            end;
          end;
        end;
      end;
    finally
      View.UpdateSelectRectangle;
      if Update then
      begin
        //        ContoursChanged := True;
      end;
    end;
    UndoChangeSelection.SetPostSelection;
    if UndoChangeSelection.SelectionChanged then
    begin
      frmGoPhast.UndoStack.Submit(UndoChangeSelection);
    end
    else
    begin
      UndoChangeSelection.Free;
    end;
  except
    UndoChangeSelection.Free;
    raise;
  end;
end;
{$ENDREGION}

{$REGION 'TDeleteSegmentTool'}
{ TDeleteSegmentTool }

procedure TDeleteSegmentTool.SetCursorAtLocation(const X, Y: Integer);
var
  Edge: integer;
begin
  // The user wants to delete on segment of an object.  Check if the
  // mouse is over the edge of an object.
  Edge := GetEdge(X,Y);

  // set the Cursor based on whether or not the mouse is over an
  // edge of an object.
  if Edge >= 0 then
  begin
    Cursor := crDeleteSegment;
  end
  else
  begin
    Cursor := crDisabledDeleteSegment;
  end;
end;

procedure TDeleteSegmentTool.DoEdit(const X, Y: integer);
var
  Index: integer;
  AScreenObject: TScreenObject;
  Edge: integer;
  UndoDeleteSegment: TUndoDeleteSegment;
begin
  // This procedure allows the user to delete one edge of a selected object.
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      Edge := AScreenObject.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        UndoDeleteSegment := TUndoDeleteSegment.Create(AScreenObject, Edge);
        frmGoPhast.UndoStack.Submit(UndoDeleteSegment);
        UndoDeleteSegment.SetPostSelection;
        Exit;
      end;
    end;
  end;
end;

function TDeleteSegmentTool.GetHint: string;
begin
  result := StrClickToDeleteSegm;
end;
{$ENDREGION}

{$REGION 'TInsertPointTool'}
{ TInsertPointTool }

procedure TInsertPointTool.DoEdit(const X, Y: integer);
var
  Index: integer;
  AScreenObject: TScreenObject;
  Edge: integer;
  APoint: TPoint2D;
  UndoInsertPoint: TUndoInsertPoint;
begin
  // This procedure allows the user to insert a node into a selected object.
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      Edge := AScreenObject.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        APoint.X := ZoomBox.X(X);
        APoint.Y := ZoomBox.Y(Y);
        UndoInsertPoint := TUndoInsertPoint.Create(AScreenObject, Edge + 1,
          APoint);
        frmGoPhast.UndoStack.Submit(UndoInsertPoint);
        UndoInsertPoint.SetPostSelection;
        Exit;
      end;
    end;
  end;
end;

function TInsertPointTool.GetHint: string;
begin
  result := StrClickOnASegmentT;
end;

procedure TInsertPointTool.SetCursorAtLocation(const X, Y: Integer);
var
  Edge: integer;
begin
  // The user wants to insert a point.  Check if the mouse is over the
  // edge of a selected object.
  Edge := GetEdge(X,Y);
  // set the Cursor based on whether or not the mouse is over an
  // edge of a selected object.
  if Edge >= 0 then
  begin
    Cursor := crInsertPoint;
  end
  else
  begin
    Cursor := crDisabledInsertPoint;
  end;
end;
{$ENDREGION}

{$REGION 'TSelectPointTool'}
{ TSelectPointTool }

procedure TSelectPointTool.Activate;
begin
  inherited;
  CreateLayers;
end;

procedure TSelectPointTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.CurrentTool <> self then Exit;
  Layer32.BringToFront;
  Buffer.BeginUpdate;
  try
    ShowMovedPoints(Buffer);
  finally
    Buffer.EndUpdate;
  end;
end;

procedure TSelectPointTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ScreenObjectAlreadySelected: boolean;
  NodeToSelect: integer;
  SelectedScreenObject: TScreenObject;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  // The user wants to select a node on a screen object.
  // See if the user clicked on a node.
  ScreenObjectAlreadySelected := AreScreenObjectsSelected;
  FMovingScreenObjects := ScreenObjectAlreadySelected or SelectScreenObjects
    (X, Y, (ssCtrl in Shift), (ssShift in Shift), False, True);
  if FMovingScreenObjects then
  begin
    if not ScreenObjectAlreadySelected then
    begin
      SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift), True,
        True);
    end;

    SelectedScreenObject := FindSelectedScreenObject(X, Y);
    CurrentScreenObject := SelectedScreenObject;
    ClearPoints;

    if SelectedScreenObject = nil then
    begin
      FMovingScreenObjects := False;
      NodeToSelect := -1;
    end
    else
    begin
      NodeToSelect := FindNodeInSelectedScreenObjects(X, Y,
        SelectedScreenObject);

      FMovingScreenObjects := (NodeToSelect >= 0) and
        (SelectedScreenObject.SelectedVertices[NodeToSelect] or (ssShift in
          Shift));
    end;

    if FMovingScreenObjects then
    begin
      FPointIsSelected := True;
    end
    else
    begin
      FPointIsSelected := (NodeToSelect >= 0); //False;
    end;
  end;
  // define a box that will outline the nodes to be selected.
  FSelectLine.Free;
  FSelectLine := TLine.Create(5);
  FSelectLine.AddPoint(Point(X, Y));
end;

procedure TSelectPointTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    if (FCurrentScreenObject = nil)
      and (FViewDirection <> ViewDirection)
      and (FStoredPoints <> nil) then
    begin
      ClearPoints
    end;
    FViewDirection := ViewDirection;
    FShift := Shift;
    // If something is happening, redraw.
    if (FSelectLine <> nil) and not FMovingScreenObjects then
    begin
      ZoomBox.InvalidateImage32;
    end
    else if (FCurrentScreenObject <> nil)
      and FCurrentScreenObject.Selected and FMovingScreenObjects
      and (ssLeft in Shift) then
    begin
      ZoomBox.InvalidateImage32;
    end;
    UpdateCursors;
  end;
end;

procedure TSelectPointTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if (FSelectLine <> nil) then
  begin
    // Don't do anything if the mouse hasn't moved much.
    if ((Abs(X - FStartX) > SelectionWidth)
      or (Abs(Y - FStartY) > SelectionWidth)) then
    begin
      // if you are moving nodes, move them
      if FMovingScreenObjects then
      begin
        Assert(FCurrentScreenObject <> nil);
        StorePointsOfOtherObjects(FCurrentScreenObject);
        MoveScreenObjects(X, Y, Shift, FSelectedNode);
      end
      else
      begin
        // otherwise select screen objects with a rectangle.
        // finish SelectLine
        APoint := FSelectLine.Points[0];
        FSelectLine.AddPoint(Point(APoint.X, Y));
        FSelectLine.AddPoint(Point(X, Y));
        FSelectLine.AddPoint(Point(X, APoint.Y));
        FSelectLine.AddPoint(APoint);
        // Select screen objects with SelectLine.
        SelectPointsOfAllSelectedScreenObjectsWithLine(ssShift in Shift);
      end;
    end
    else
    begin
      if FPointIsSelected then
      begin
        SelectPointsOfASelectedScreenObject(X, Y,
          (ssShift in Shift));
      end;
    end;
    // Get rid of SelectLine
    FreeAndNil(FSelectLine);
    // redraw
    ZoomBox.InvalidateImage32;
  end;
end;

procedure TSelectPointTool.ShowMovedPoints(const BitMap: TBitmap32);
var
  PointIndex: integer;
  PointArray: array of TPoint;
  IsSegment: boolean;
  Function IsStartPoint(Out IsSegment: boolean): boolean;
  var
    SectionIndex: integer;
  begin
    result := False;
    IsSegment := False;
    for SectionIndex := 0 to FCurrentScreenObject.SectionCount - 1 do
    begin
      if PointIndex = FCurrentScreenObject.SectionStart[SectionIndex] then
      begin
        result := true;
        IsSegment := FCurrentScreenObject.SectionLength[SectionIndex] > 1;
        break;
      end
    end;
  end;
  Function IsEndPoint(Out IsSegment: boolean): boolean;
  var
    SectionIndex: integer;
  begin
    result := False;
    IsSegment := False;
    for SectionIndex := 0 to FCurrentScreenObject.SectionCount - 1 do
    begin
      if PointIndex = FCurrentScreenObject.SectionEnd[SectionIndex] then
      begin
        result := true;
        IsSegment := FCurrentScreenObject.SectionLength[SectionIndex] > 1;
        break;
      end;
    end;
  end;
begin
  if FMouseButtonIsDown and FMovingScreenObjects
    and frmGoPhast.tbSelectPoint.Down
    and (FCurrentScreenObject <> nil)
    and FCurrentScreenObject.Selected then
  begin
    // show the new locations of the selected points.
    for PointIndex := 0 to FCurrentScreenObject.Count - 1 do
    begin
      if FCurrentScreenObject.SelectedVertices[PointIndex] then
      begin
        if FCurrentScreenObject.Count = 1 then
        begin
          DrawBigRectangle32(BitMap, clBlack32, clTransparent32, 1,
            frmGoPhast.CursorX - 3,
            frmGoPhast.CursorY - 3,
            frmGoPhast.CursorX + 4,
            frmGoPhast.CursorY + 4);
        end
        else
        begin
//          if PointIndex = 0 then
          if IsStartPoint(IsSegment) then
          begin
            if IsSegment then
            begin
              SetLength(PointArray, 2);
            end
            else
            begin
              SetLength(PointArray, 1);
            end;
//            SetLength(PointArray, 2);
            PointArray[0] := FCurrentScreenObject.CanvasCoordinates[PointIndex];
            PointArray[0].X := PointArray[0].X
              + frmGoPhast.CursorX - FStartX;
            PointArray[0].Y := PointArray[0].Y
              + frmGoPhast.CursorY - FStartY;

            if IsSegment then
            begin

              PointArray[1] := FCurrentScreenObject.CanvasCoordinates[PointIndex+1];
              if FCurrentScreenObject.SelectedVertices[PointIndex+1] then
              begin
                PointArray[1].X := PointArray[1].X
                  + frmGoPhast.CursorX - FStartX;
                PointArray[1].Y := PointArray[1].Y
                  + frmGoPhast.CursorY - FStartY;
              end;
            end;
          end
//          else if PointIndex = FSelectedPointScreenObject.Count - 1 then
          else if IsEndPoint(IsSegment) then
          begin
            if IsSegment then
            begin
              SetLength(PointArray, 2);
            end
            else
            begin
              SetLength(PointArray, 1);
            end;
//            SetLength(PointArray, 2);
              PointArray[0] := FCurrentScreenObject.
                CanvasCoordinates[PointIndex];
              PointArray[0].X := PointArray[0].X
                + frmGoPhast.CursorX - FStartX;
              PointArray[0].Y := PointArray[0].Y
                + frmGoPhast.CursorY - FStartY;

            if IsSegment then
            begin
              PointArray[1] := FCurrentScreenObject.
                CanvasCoordinates[PointIndex - 1];
              if FCurrentScreenObject.SelectedVertices[PointIndex - 1] then
              begin
                PointArray[1].X := PointArray[1].X
                  + frmGoPhast.CursorX - FStartX;
                PointArray[1].Y := PointArray[1].Y
                  + frmGoPhast.CursorY - FStartY;
              end;
            end;
          end
          else
          begin
            SetLength(PointArray, 3);
            PointArray[0] := FCurrentScreenObject.
              CanvasCoordinates[PointIndex - 1];
            if FCurrentScreenObject.SelectedVertices[PointIndex - 1] then
            begin
              PointArray[0].X := PointArray[0].X
                + frmGoPhast.CursorX - FStartX;
              PointArray[0].Y := PointArray[0].Y
                + frmGoPhast.CursorY - FStartY;
            end;

            PointArray[1] := FCurrentScreenObject.
              CanvasCoordinates[PointIndex];
            PointArray[1].X := PointArray[1].X
              + frmGoPhast.CursorX - FStartX;
            PointArray[1].Y := PointArray[1].Y
              + frmGoPhast.CursorY - FStartY;

            PointArray[2] := FCurrentScreenObject.
              CanvasCoordinates[PointIndex + 1];
            if FCurrentScreenObject.SelectedVertices[PointIndex + 1] then
            begin
              PointArray[2].X := PointArray[2].X
                + frmGoPhast.CursorX - FStartX;
              PointArray[2].Y := PointArray[2].Y
                + frmGoPhast.CursorY - FStartY;
            end;
          end;

          DrawBigPolyline32(BitMap, clBlack32, 1, PointArray, True);
        end;
      end;
    end;
  end
  else
  begin
    DrawSelectionRectangle32(BitMap);
  end;
end;

function TSelectPointTool.FindSelectedScreenObject(const X, Y: integer):
  TScreenObject;
var
  Index: integer;
  AScreenObject: TScreenObject;
  SelectedNode: Integer;
begin
  result := nil;
  FSelectedNode := -1;
  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      SelectedNode := FindNodeInSelectedScreenObjects(X, Y, AScreenObject);
      if SelectedNode >= 0 then
      begin
        FSelectedNode := SelectedNode;
        FSelectedNodeLocation := AScreenObject.Points[FSelectedNode];
        result := AScreenObject;
        Exit;
      end;
    end;
  end;
end;

function TSelectPointTool.SelectPointsOfASelectedScreenObject(
  const X, Y: integer; const AddToSelection: boolean): boolean;
var
  Index: integer;
  AScreenObject, SelectedScreenObject: TScreenObject;
  UndoChangeSelection: TUndoChangeSelection;
  SelectedPoint: integer;
  PointIndex: integer;
  SectionIndex: Integer;
begin
  // This procedure selects the node of a selected screen object
  // that is at or near (X,Y).
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    SelectedScreenObject := nil;
    try
      for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];

        if (AScreenObject.ViewDirection = ViewDirection) and
          AScreenObject.Selected then
        begin
          SelectedPoint := FindNodeInSelectedScreenObjects(X, Y, AScreenObject);
          if SelectedPoint >= 0 then
          begin
            result := True;
            SelectedScreenObject := AScreenObject;
            FCurrentScreenObject := AScreenObject;
            ClearPoints;
            if AddToSelection then
            begin
              SelectedScreenObject.SelectedVertices[SelectedPoint] :=
                not SelectedScreenObject.SelectedVertices[SelectedPoint];
            end
            else
            begin
              for PointIndex := 0 to SelectedScreenObject.Count - 1 do
              begin
                SelectedScreenObject.SelectedVertices[PointIndex] :=
                  PointIndex = SelectedPoint;
              end;
            end;
            if SelectedScreenObject.Closed then
            begin
              for SectionIndex := 0 to SelectedScreenObject.SectionCount - 1 do
              begin
                if SelectedScreenObject.SectionClosed[SectionIndex] and
                  (SelectedPoint =
                  SelectedScreenObject.SectionStart[SectionIndex]) then
                begin
                  SelectedScreenObject.SelectedVertices[
                    SelectedScreenObject.SectionEnd[SectionIndex]] :=
                    SelectedScreenObject.SelectedVertices[
                    SelectedScreenObject.SectionStart[SectionIndex]];
                end;
              end;
            end;
            break;
          end;
        end;
      end;
    finally
      if result then
      begin
        for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
          AScreenObject.Selected := AScreenObject = SelectedScreenObject;
        end;
      end;
    end;
    UndoChangeSelection.SetPostSelection;
    if UndoChangeSelection.SelectionChanged then
    begin
      frmGoPhast.UndoStack.Submit(UndoChangeSelection);
    end
    else
    begin
      UndoChangeSelection.Free;
    end;
  except
    UndoChangeSelection.Free;
    raise;
  end;
end;

function TSelectPointTool.SelectPointsOfAllSelectedScreenObjectsWithLine
  (const AddToSelection: boolean): boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
  Update: boolean;
  Changed: boolean;
  UndoChangeSelection: TUndoChangeSelection;
begin
  // This procedure uses a "lasso" to select nodes in one selected object.
  // All other selected objects are deselected.
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    try
      for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if result then
        begin
          if AScreenObject.Selected then
          begin
            AScreenObject.Selected := False;
            Update := True;
          end;
          Continue;
        end;
        if (AScreenObject.ViewDirection = ViewDirection) and
          AScreenObject.Selected then
        begin
          Changed := False;
          if SelectPointsWithLine(AScreenObject, AddToSelection, Changed) then
          begin
            result := True;
            FCurrentScreenObject := AScreenObject;
            ClearPoints;
          end
          else
          begin
            AScreenObject.Selected := false;
            Update := True;
          end;
          if Changed then
          begin
            Update := True;
          end;
        end
        else if AScreenObject.Selected then
        begin
          AScreenObject.Selected := False;
          Update := True;
        end;
      end;
    finally
      if Update then
      begin
        View.ScreenObjectsHaveChanged := True;
      end;
    end;
    UndoChangeSelection.SetPostSelection;
    if UndoChangeSelection.SelectionChanged then
    begin
      frmGoPhast.UndoStack.Submit(UndoChangeSelection);
    end
    else
    begin
      UndoChangeSelection.Free;
    end;
  except
    UndoChangeSelection.Free;
    raise;
  end;
end;

function TSelectPointTool.SelectPointsWithLine(
  const AScreenObject: TScreenObject;
  const AddToSelection: boolean; out Changed: boolean): boolean;
var
  PointIndex: integer;
  APoint: TPoint;
begin
  // This procedure selects nodes that are inside SelectLine.
  // if AddToSelection is not True, it deselects nodes that are not
  // inside SelectLine.  Changed is set to True if any nodes have been
  // changed from unselected to selected or vice versa.
  Changed := False;
  result := False;
  for PointIndex := 0 to AScreenObject.Count - 1 do
  begin
    APoint := AScreenObject.CanvasCoordinates[PointIndex];
    if FSelectLine.Inside(APoint) then
    begin
      result := True;
      if AddToSelection then
      begin
        AScreenObject.SelectedVertices[PointIndex] :=
          not AScreenObject.SelectedVertices[PointIndex];
        Changed := True;
      end
      else
      begin
        if not AScreenObject.SelectedVertices[PointIndex] then
        begin
          AScreenObject.SelectedVertices[PointIndex] := True;
          Changed := True;
        end;
      end;
    end
    else
    begin
      if not AddToSelection then
      begin
        if AScreenObject.SelectedVertices[PointIndex] then
        begin
          Changed := True;
          AScreenObject.SelectedVertices[PointIndex] := False;
        end;
      end;
    end;
  end;
end;

function TSelectPointTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);


  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
    Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapSelectPoint;
  end
  else
  begin
    result := crSelectPoint;
  end;
end;

function TSelectPointTool.GetHint: string;
begin
  result := StrClickOnAVertexTo;
end;

procedure TSelectPointTool.GetOffset(const APoint: TPoint2D; out XOffset,
  YOffset: real);
var
  StartPoint: TPoint2D;
  ClosestLocation: TPoint2D;
  PointDistance: real;
  SectionIndex: integer;
begin
  StartPoint.x := ZoomBox.X(FStartX);
  StartPoint.Y := ZoomBox.Y(FStartY);

  ClosestLocation := FCurrentScreenObject.Points[0];
  PointDistance := Distance(ClosestLocation, StartPoint);
  FCurrentScreenObject.IsAnyPointCloser(StartPoint, PointDistance,
    ClosestLocation, SectionIndex, 1);

  XOffset := APoint.X - ClosestLocation.X;
  YOffset := APoint.Y - ClosestLocation.Y;
end;

{$ENDREGION}

{$REGION 'TSelectScreenObjectTool'}
{ TSelectScreenObjectTool }

procedure TSelectScreenObjectTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FDoubleClicked := True;
end;

procedure TSelectScreenObjectTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  FShouldDrawSelectionRectangle := True;
  // The user wants to select screen objects.
  if ssDouble in Shift then
  begin
    FMovingScreenObjects := False;
    // Don't change the selected screen objects when double-clicking.
    Exit;
  end;
  // The user wants to select screen objects and may want to move them too.
  FMovingScreenObjects := SelectScreenObjects
    (X, Y, (ssCtrl in Shift), (ssShift in Shift),
    not (ssShift in Shift) and ((ssCtrl in Shift) or not
    AreScreenObjectsSelected));
  if not FMovingScreenObjects then
  begin
    // if the user didn't click on anything, the user must want to
    // draq a box around the objects to be selected.

    // SelectLine is used to define the box.  It will have 5 nodes because
    // it is rectangular and the first node will be at the same position as
    // the last one.
    FSelectLine.Free;
    FSelectLine := TLine.Create(5);
    FSelectLine.AddPoint(Point(X, Y));
    ZoomBox.InvalidateImage32;
  end;
end;

procedure TSelectScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // Set the correct cursor.
  if not (ssMiddle in Shift) then
  begin
    Cursor := crArrow;
    // If something is happening, redraw.
    if (ssLeft in Shift) or FMovingScreenObjects
      or (FSelectLine <> nil) then
    begin
      ZoomBox.InvalidateImage32;
    end;
  end;
end;

procedure TSelectScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Val1: double;
  Val2: double;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if not FDoubleClicked then
  begin
    if ((Abs(X - FStartX) > SelectionWidth)
      or (Abs(Y - FStartY) > SelectionWidth)) then
    begin
      // Don't do anything if the mouse hasn't moved much.
      if FMovingScreenObjects then
      begin
        // If you are moving screen objects, move them.
        MoveScreenObjects(X, Y, Shift, -1);
      end
      else
      begin
        // Otherwise select screen objects with a rectangle.
        // finish SelectLine
        if FSelectLine <> nil then
        begin
          Val1 := ZoomBox.X(FStartX);
          Val2 := ZoomBox.X(X);
          FMaxX := Max(Val1, Val2);
          FMinX := Min(Val1, Val2);

          Val1 := ZoomBox.Y(FStartY);
          Val2 := ZoomBox.Y(Y);
          FMaxY := Max(Val1, Val2);
          FMinY := Min(Val1, Val2);


          // Select screen objects with a rectangle.
          SelectScreenObjectsInGui(ssShift in Shift);
          // redraw
          ZoomBox.InvalidateImage32;
        end;

      end;
    end
    else if not FMovingScreenObjects or (ssShift in Shift) then
    begin
      SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift),
        True);
    end;
  end;
  // Get rid of SelectLine
  FreeAndNil(FSelectLine);

  if FDoubleClicked then
  begin
    // Edit the object properties.
    try
      frmGoPhast.EditScreenObjects;
    finally
      FDoubleClicked := False;
    end;

  end;
end;

function TSelectScreenObjectTool.ScreenObjectInside(AScreenObject: TScreenObject): boolean;
begin
  // If at least one point of the screen object is inside SelectLine
  // and SelectLine does not intersect SelectLine then
  // the entire object is inside SelectLine.
  result := false;
  if AScreenObject.Count > 0 then
  begin
    result := (AScreenObject.MaxX <= FMaxX)
      and (AScreenObject.MinX >= FMinX)
      and (AScreenObject.MaxY <= FMaxY)
      and (AScreenObject.MinY >= FMinY);
  end;
end;

procedure TSelectScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  TL, BR: TPoint64;
  BitMap: TBitmap32;
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if (FSelectLine = nil) and FShouldDrawSelectionRectangle
    and (frmGoPhast.CurrentTool = self)
    and (Sender = Layer32)
    and (frmGoPhast.CursorGrid = View.CursorGrid) then
  begin
    Layer32.BringToFront;
    Buffer.BeginUpdate;
    try
      try
        // draw a rectangle around the selected screen objects.
        TL := View.FSelectTopLeft;
        BR := View.FSelectBottomRight;
        if FMovingScreenObjects then
        begin
          TL.X := TL.X + frmGoPhast.CursorX - FStartX;
          BR.X := BR.X + frmGoPhast.CursorX - FStartX;
          TL.Y := TL.Y + frmGoPhast.CursorY - FStartY;
          BR.Y := BR.Y + frmGoPhast.CursorY - FStartY;
        end;

        BitMap := TBitmap32.Create;
        try
          BitMap.Width := Min(BR.X - TL.X + 1, MAXINT);
          BitMap.Height := Min(BR.Y - TL.Y + 1, MAXINT);
          BitMap.DrawMode := dmBlend;
          BitMap.SetStipple([0, 0, 0, 0, 0,
            clBlack32, clBlack32, clBlack32, clBlack32, clBlack32]);
          BitMap.MoveTo(0,0);
          BitMap.LineToFSP(0,BitMap.Height-1);
          BitMap.LineToFSP(BitMap.Width-1,BitMap.Height-1);
          BitMap.LineToFSP(BitMap.Width-1,0);
          BitMap.LineToFSP(0,0);
          Buffer.Draw(TL.X, TL.Y, BitMap);
        finally
          BitMap.Free;
        end;
      except
        ZoomBox.ZoomBy(0.5);
        with View do
        begin
          AdjustScales;
          ShowMagnification;
          MagnificationChanged := True;
          UpdateSelectRectangle;
        end;
      end;
    finally
      Buffer.EndUpdate;
    end;
  end;
  DrawSelectionRectangle32(Buffer);
end;

procedure TSelectScreenObjectTool.Activate;
begin
  inherited;
  CreateLayers;
  FShouldDrawSelectionRectangle := True;
end;

function TSelectScreenObjectTool.GetHint: string;
begin
  result := StrClickOnObjectToS;
end;
{$ENDREGION}

{$REGION 'TCustomSelectScreenObjectTool'}
{ TCustomSelectScreenObjectTool }
procedure TCustomSelectScreenObjectTool.DrawSelectionRectangle32(
  BitMap: TBitmap32);
var
  TopLeft: TPoint;
begin
  if (FSelectLine <> nil) and (frmGoPhast.CurrentTool = self) and
    (frmGoPhast.CursorGrid = View.CursorGrid) then
  begin
    if FSelectLine.Count = 1 then
    begin
      Layer32.BringToFront;
      BitMap.BeginUpdate;
      try
        TopLeft := FSelectLine.Points[0];
        DrawBigRectangle32(BitMap, clBlack32, clTransparent32, 1,
          TopLeft.X, TopLeft.Y, frmGoPhast.CursorX, frmGoPhast.CursorY, True);
      finally
        BitMap.EndUpdate;
      end;
    end;
  end;
end;

//destructor TCustomSelectScreenObjectTool.Destroy;
//begin
//  FSelectLine.Free;
//  inherited;
//end;

procedure TCustomSelectScreenObjectTool.GetOffset(const APoint: TPoint2D;
  out XOffset, YOffset: real);
begin
  Assert(False);
end;

procedure TCustomSelectScreenObjectTool.MoveScreenObjects(const X, Y: integer;
  Shift: TShiftState; SelectedNode: integer);
var
  XOffset, YOffset: real;
  UndoMoveScreenObject: TUndoMoveScreenObject;
  APoint: TPoint2D;
//  SelectedNodeNewLocation: TPoint2D;
begin
  // Move the screen objects a distance specified by where the mouse was clicked
  // down and where it was clicked up.
  if  (ssShift in Shift)
    and FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    GetOffset(APoint, XOffset, YOffset);
  end
  else
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    XOffset := APoint.X - ZoomBox.X(FStartX);
    YOffset := APoint.Y - ZoomBox.Y(FStartY);
  end;

  if (XOffset <> 0) or (YOffset <> 0) then
  begin
    // if the cursor has moved, move the selected screen objects.
    UndoMoveScreenObject := TUndoMoveScreenObject.Create(XOffset, YOffset,
      ViewDirection, SelectedNode, APoint);
    frmGoPhast.UndoStack.Submit(UndoMoveScreenObject);
    UndoMoveScreenObject.SetPostSelection;
    FStartX := X;
    FStartY := Y;
    FMovingScreenObjects := False;
  end;
end;

procedure TCustomSelectScreenObjectTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FStartX := X;
    FStartY := Y;
    FMouseButtonIsDown := True;
  end;
end;

procedure TCustomSelectScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FMouseButtonIsDown := False;
  end;
end;

procedure TCustomSelectScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    FMovingScreenObjects := FMovingScreenObjects and (ssLeft in Shift);
  end;
end;

{$ENDREGION}

{$REGION 'TCustomModifyGeometryTool'}
{ TCustomModifyGeometryTool }

procedure TCustomModifyGeometryTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to insert a node in an existing object.
  // You need to select an object.
  if Button = mbLeft then
  begin
    if not AreScreenObjectsSelected then
    begin
      SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift));
    end;
  end;
end;

procedure TCustomModifyGeometryTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to insert a point.
  // Check if the mouse is over the
  // edge of a selected object and change the cursor appropriately.
  if not (ssMiddle in Shift) then
  begin
    SetCursorAtLocation(X, Y);
  end;
end;

procedure TCustomModifyGeometryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to insert a point
  if Button = mbLeft then
  begin
    if not AreScreenObjectsSelected then
    begin
      SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift));
    end;
    DoEdit(X, Y);
  end;
end;

function TCustomModifyGeometryTool.GetEdge(const X, Y: integer): integer;
var
  Index: integer;
  AScreenObject: TScreenObject;
  SelectedObjects: boolean;
begin
  // The user wants to insert a point.  Check if the mouse is over the
  // edge of a selected object.
  result := -1;
  SelectedObjects := AreScreenObjectsSelected;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if (AScreenObject.Selected or not SelectedObjects)
      and (AScreenObject.ViewDirection = ViewDirection) then
    begin
      result := AScreenObject.SelectEdge(X, Y);
      if result >= 0 then
      begin
        Exit;
      end;
    end
  end;
end;

{$ENDREGION}

{$REGION 'TColRowLayerSelectorTool'}
{ TColRowLayerSelectorTool }

procedure TColRowLayerSelectorTool.DrawExistingColRowLayerSelection(
  const Direction: TViewDirection; const BitMap: TBitmap32);
var
  Color1, Color2: TColor;
begin
  Color1 := clBlack;
  Color2 := clBlack;
  case Direction of
    vdTop:
      begin
        Color1 := ExistingColumnSelectionCellColor;
        Color2 := ExistingRowSelectionCellColor;
      end;
    vdFront:
      begin
        Color1 := ExistingColumnSelectionCellColor;
        Color2 := ExistingLayerSelectionCellColor;
      end;
    vdSide:
      begin
        Color1 := ExistingRowSelectionCellColor;
        Color2 := ExistingLayerSelectionCellColor;
      end;
    else Assert(False);
  end;
  DrawASelection(frmGoPhast.PhastModel.SelectedModel.SelectedColumn,
    frmGoPhast.PhastModel.SelectedModel.SelectedRow,
    frmGoPhast.PhastModel.SelectedModel.SelectedLayer,
    Color1, Color2, BitMap, Direction);
end;

procedure TColRowLayerSelectorTool.DrawNewColRowLayerSelection(
  const Direction: TViewDirection; const BitMap: TBitmap32);
begin
  DrawASelection(NewColumn, NewRow, NewLayer,
    SelectedCellsColor, SelectedCellsColor, BitMap, Direction);
end;

procedure TColRowLayerSelectorTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.SelectedModel.Grid = nil then
  begin
    Exit;
  end;

  if (frmGoPhast.PhastModel.SelectedModel.Grid.ColumnCount <= 0)
    or (frmGoPhast.PhastModel.SelectedModel.Grid.RowCount <= 0)
    or (frmGoPhast.PhastModel.SelectedModel.Grid.LayerCount <= 0) then
  begin
    Exit
  end;

  if (frmGoPhast.CurrentTool = self)
    and ((Sender = FTopLayer) or (Sender = FFrontLayer)
    or (Sender = FSideLayer))  then
  begin
    if Sender = FTopLayer then
    begin
      DrawExistingColRowLayerSelection(vdTop, Buffer);
    end
    else if Sender = FFrontLayer then
    begin
      DrawExistingColRowLayerSelection(vdFront, Buffer);
    end
    else if Sender = FSideLayer then
    begin
      DrawExistingColRowLayerSelection(vdSide, Buffer);
    end;
  end;
  if FShouldDraw and (frmGoPhast.CurrentTool = self)
    and (Sender = Layer32)
    and (frmGoPhast.CursorGrid = View.CursorGrid) then
  begin
    DrawNewColRowLayerSelection(View.ViewDirection, Buffer);
  end;
end;

procedure TColRowLayerSelectorTool.GetCellUnderMouse(X, Y: integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
begin
  // This procedure is used to update the view on the screen when
  // setting the spacing of rows, columns or layers.

  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          FNewColumn := Column;
          FNewRow := Row;
          FShouldDraw := True;
          ZoomBox.InvalidateImage32;
        end
        else
        begin
          FShouldDraw := False;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          FNewLayer := Layer;
          FNewColumn := Column;
          FShouldDraw := True;
          ZoomBox.InvalidateImage32;
        end
        else
        begin
          FShouldDraw := False;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          FNewLayer := Layer;
          FNewRow := Row;
          FShouldDraw := True;
          ZoomBox.InvalidateImage32;
        end
        else
        begin
          FShouldDraw := False;
        end;
      end;
  else
    Assert(False);
  end;
end;

function TColRowLayerSelectorTool.GetHint: string;
begin
  result := StrClickToChangeSele;
end;

procedure TColRowLayerSelectorTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (ssMiddle in Shift) then
  begin
    GetCellUnderMouse(X,Y)
  end;
end;

procedure TColRowLayerSelectorTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    GetCellUnderMouse(X,Y);
    SetNewSelection;
  end;
  inherited;
end;

procedure TColRowLayerSelectorTool.Activate;
begin
  inherited;
  UpdateAllViews;
//  FTopLayer.Changed;
//  FSideLayer.Changed;
//  FFrontLayer.Changed;
//  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
//  frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
//  frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
end;

procedure TColRowLayerSelectorTool.DrawASelection(Col, Row, Lay: Integer;
  Color1, Color2: TColor; const BitMap: TBitmap32;
  const Direction: TViewDirection);
var
  EvaluatedAt: TEvaluatedAt;
  Limit2: Integer;
  Limit1: Integer;
begin
  EvaluatedAt := GetEvaluatedAt(Direction);
  frmGoPhast.PhastModel.SelectedModel.Grid.GetLimits(EvaluatedAt, Direction, Limit1, Limit2);
  case Direction of
    vdTop:
      begin
        DrawSelectedTopCells(Col, Col, 0, Limit2, BitMap, Direction, Color1);
        DrawSelectedTopCells(0, Limit1, Row, Row, BitMap, Direction, Color2);
      end;
    vdFront:
      begin
        DrawSelectedFrontCells(Col, Col, 0, Limit2, BitMap, Direction, Color1);
        DrawSelectedFrontCells(0, Limit1, Lay, Lay, BitMap, Direction, Color2);
      end;
    vdSide:
      begin
        DrawSelectedSideCells(Row, Row, 0, Limit2, BitMap, Direction, Color1);
        DrawSelectedSideCells(0, Limit1, Lay, Lay, BitMap, Direction, Color2);
      end;
  else
    Assert(False);
  end;
end;

procedure TColRowLayerSelectorTool.SetNewSelection;
begin
  if not FShouldDraw then Exit;
  case ViewDirection of
    vdTop:
      begin
        frmGoPhast.PhastModel.SelectedModel.SelectedColumn := NewColumn;
        frmGoPhast.PhastModel.SelectedModel.SelectedRow := NewRow;
        frmGoPhast.PhastModel.UpdateCombinedDisplayColumn;
        frmGoPhast.frameSideView.ItemChange(nil);
        frmGoPhast.PhastModel.UpdateCombinedDisplayRow;
        frmGoPhast.frameFrontView.ItemChange(nil);
      end;
    vdFront:
      begin
        frmGoPhast.PhastModel.SelectedModel.SelectedLayer := NewLayer;
        frmGoPhast.PhastModel.SelectedModel.SelectedColumn := NewColumn;
        frmGoPhast.PhastModel.UpdateCombinedDisplayColumn;
        frmGoPhast.frameSideView.ItemChange(nil);
        frmGoPhast.PhastModel.UpdateCombinedDisplayLayer;
        frmGoPhast.frameTopView.ItemChange(nil);
      end;
    vdSide:
      begin
        frmGoPhast.PhastModel.SelectedModel.SelectedLayer := NewLayer;
        frmGoPhast.PhastModel.SelectedModel.SelectedRow := NewRow;
        frmGoPhast.PhastModel.UpdateCombinedDisplayRow;
        frmGoPhast.frameFrontView.ItemChange(nil);
        frmGoPhast.PhastModel.UpdateCombinedDisplayLayer;
        frmGoPhast.frameTopView.ItemChange(nil);
      end;
  else
    Assert(False);
  end;
end;

{$ENDREGION}

{ TAddPartTool }

procedure TAddLinePartTool.Activate;
begin
  inherited;
  FNewPart := True;
  FDoubleClicked := False;
end;

function TAddLinePartTool.ShouldClosePolygon(X, Y: integer): boolean;
begin
  result := False;
end;

procedure TAddLinePartTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FinishSection;
  SubmitUndo;
end;

procedure TAddLinePartTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
begin
  try
    inherited;
    if Button <> mbLeft then
    begin
      Exit;
    end;
    if FDoubleClicked then
    begin
      FDoubleClicked := False;
    end
    else
    begin
      if (FCurrentScreenObject <> nil)
        and (ViewDirection = FCurrentScreenObject.ViewDirection) then
      begin
        if ssShift in Shift then
        begin
          StorePointsOfOtherObjects(FCurrentScreenObject);
        end;
        if not (ssShift in Shift)
          or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
        begin
          // Get the real-world coordinates of the mouse.
          APoint.X := ZoomBox.X(X);
          APoint.Y := ZoomBox.Y(Y);
        end;
        FCurrentScreenObject.AddPoint(APoint, FNewPart);
        EnsureUndo;
        FUndoAddPart.AddPoint(APoint);
        FNewPart := False;
        if ShouldClosePolygon(X, Y) then
        begin
          DoubleClick(Sender);
          FDoubleClicked := False;
        end;
      end;
    end;
  except on E: EScreenObjectError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TAddLinePartTool.FinishSection;
begin
  FDoubleClicked := True;
  FNewPart := True;
end;

function TAddLinePartTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapMultiPartLine;
  end
  else
  begin
    result := crMultiPartLine;
  end;
end;

{ TAddClosedPartTool }

function TAddPolygonPartTool.ShouldClosePolygon(X, Y: integer): boolean;
begin
  EnsureUndo;
  result := FUndoAddPart.Count > 3;
  if result then
  begin
    result := (Abs(X - FStartX) < SelectionWidth)
      and (Abs(Y - FStartY) < SelectionWidth)
  end;
  if result then
  begin
    FUndoAddPart.DeleteLastPoint;
    FCurrentScreenObject.Count := FCurrentScreenObject.Count -1;
  end;
end;

procedure TAddPolygonPartTool.FinishSection;
var
  APoint: TPoint2D;
begin
  inherited;
  if FDoubleClicked then
  begin
    if (FCurrentScreenObject <> nil)
      and (ViewDirection = FCurrentScreenObject.ViewDirection) then
    begin
      APoint := FCurrentScreenObject.Points[FCurrentScreenObject.
        SectionStart[FCurrentScreenObject.SectionCount -1]];
      try
        FCurrentScreenObject.AddPoint(APoint, False);
        EnsureUndo;
        FUndoAddPart.AddPoint(APoint);
      except on E: EScreenObjectError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
        end;
      end;
    end;
  end;
end;

{ TCustomAddPartTool }

procedure TCustomAddPartTool.Activate;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  inherited;
  FCurrentScreenObject := nil;
  ClearPoints;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      FCurrentScreenObject := AScreenObject;
      break;
    end;
  end;
  Assert(FCurrentScreenObject <> nil);
  StorePointsOfOtherObjects(FCurrentScreenObject);
  FreeAndNil(FUndoAddPart);
end;

procedure TCustomAddPartTool.DeActivate;
begin
  inherited;
  SubmitUndo;
end;

destructor TCustomAddPartTool.Destroy;
begin
  FUndoAddPart.Free;
  inherited;
end;

procedure TCustomAddPartTool.EnsureUndo;
begin
  if FUndoAddPart = nil then
  begin
    FUndoAddPart := TUndoAddPart.Create(FCurrentScreenObject);
  end;
end;

procedure TCustomAddPartTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    if (FCurrentScreenObject = nil)
      and (FViewDirection <> ViewDirection)
      and (FStoredPoints <> nil) then
    begin
      ClearPoints
    end;
    FViewDirection := ViewDirection;
    FShift := Shift;
    UpdateCursors;
  end;
end;

procedure TCustomAddPartTool.SubmitUndo;
begin
  try
    if FUndoAddPart <> nil then
    begin
      FUndoAddPart.SetPostSelection;
      frmGoPhast.UndoStack.Submit(FUndoAddPart);
      FUndoAddPart := nil;
    end;
  except
    FreeAndNil(FUndoAddPart);
    raise;
  end;
end;

{ TAddPointPartTool }

function TAddPointPartTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapMultiPartPoint;
  end
  else
  begin
    result := crMultiPartPoint;
  end;
end;

procedure TAddPointPartTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if (FCurrentScreenObject <> nil)
    and (ViewDirection = FCurrentScreenObject.ViewDirection) then
  begin
    if ssShift in Shift then
    begin
      StorePointsOfOtherObjects(FCurrentScreenObject);
    end;
    if not (ssShift in Shift)
      or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
    begin
      // Get the real-world coordinates of the mouse.
      APoint.X := ZoomBox.X(X);
      APoint.Y := ZoomBox.Y(Y);
    end;
    FCurrentScreenObject.AddPoint(APoint, True);
    EnsureUndo;
    FUndoAddPart.AddPoint(APoint);
    SubmitUndo;
  end;
end;

{ TScreenObjectTool }

destructor TScreenObjectTool.Destroy;
begin
  FSelectLine.Free;
  inherited;
end;

function TScreenObjectTool.ScreenObjectInside(
  AScreenObject: TScreenObject): boolean;
begin
  result := False;
end;

{ TCustomStoreVerticesTool }

procedure TCustomStoreVerticesTool.Activate;
begin
  inherited;
  ClearPoints;
end;

procedure TCustomStoreVerticesTool.ClearPoints;
begin
  FVisibleVertices.Clear;
  FreeAndNil(FStoredPoints);
end;

constructor TCustomStoreVerticesTool.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleVertices := TRbwQuadTree.Create(self);
  FStoredPoints := nil;
end;

destructor TCustomStoreVerticesTool.Destroy;
begin
  FStoredPoints.Free;
  inherited;
end;

function TCustomStoreVerticesTool.FindPointInNearbyScreenObject(const APoint: TPoint;
  out NearbyPoint: TPoint2D): boolean;
const
  SelectRange = 3;
var
  Block: T2DBlock;
  Points: TQuadPointInRegionArray;
  PointStorage: TScreenPointStorage;
  DeltaX: double;
  DeltaY: double;
  DistSqr: Extended;
  SelectIndex: Integer;
  Index: Integer;
  TestDistSqr: double;
begin
  result := False;
  if (FStoredPoints <> nil) and (FStoredPoints.Count > 0) then
  begin
    Block.XMin := APoint.X - SelectRange;
    Block.XMax := APoint.X + SelectRange;
    Block.YMin := APoint.Y - SelectRange;
    Block.YMax := APoint.Y + SelectRange;
    FVisibleVertices.FindPointsInBlock(Block, Points);
    if Length(Points) > 0 then
    begin
      if Length(Points) = 1 then
      begin
        SelectIndex := 0;
      end
      else
      begin
        DeltaX := Points[0].X - APoint.X;
        DeltaY := Points[0].Y - APoint.Y;
        DistSqr := Sqr(DeltaX) + Sqr(DeltaY);
        SelectIndex := 0;
        for Index := 1 to Length(Points) - 1 do
        begin
          DeltaX := Points[Index].X - APoint.X;
          DeltaY := Points[Index].Y - APoint.Y;
          TestDistSqr := Sqr(DeltaX) + Sqr(DeltaY);
          if TestDistSqr < DistSqr then
          begin
            DistSqr := TestDistSqr;
            SelectIndex:= Index;
          end;
        end;
      end;
      PointStorage := Points[SelectIndex].Data[0];
      NearbyPoint := PointStorage.FScreenObject.Points[
        PointStorage.FVertexIndex];
      result := True;
    end;
  end;
end;

function TCustomStoreVerticesTool.GetSnapPoint(var FoundNearbyPoint: Boolean; var NearbyPoint: TPoint2D): TPoint;
var
  APoint: TPoint2D;
begin
  FoundNearbyPoint := False;
  if (ssShift in FShift) then
  begin
    StorePointsOfOtherObjects(FCurrentScreenObject);
  end;
  result := Point(frmGoPhast.CursorX, frmGoPhast.CursorY);
  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(Point(result.X, result.Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    result.X := ZoomBox.XCoord(APoint.X);
    result.Y := ZoomBox.YCoord(APoint.Y);
    FoundNearbyPoint := True;
    NearbyPoint := APoint;
  end;
end;

function TAddPolygonPartTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapMultiPartPolygon;
  end
  else
  begin
    result := crMultiPartPolygon;
  end;
end;

procedure TAddPolygonPartTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FNewPart then
    begin
      FStartX := X;
      FStartY := Y;
    end;
  end;
  inherited;
end;

{ TEditVertexValueTool }

procedure TEditVertexValueTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FDoubleClicked := True;
end;

procedure TEditVertexValueTool.EditPoint(const X, Y: integer);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  SelectedPoint: Integer;
begin
  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];

    if (AScreenObject.ViewDirection = ViewDirection) and
      AScreenObject.Selected then
    begin
      SelectedPoint := FindNodeInSelectedScreenObjects(X, Y, AScreenObject);
      if SelectedPoint >= 0 then
      begin
        with TfrmPointValues.Create(nil) do
        begin
          try
            GetData(AScreenObject,SelectedPoint);
            ShowModal;
          finally
            Free;
          end;
        end;
        Exit;
      end;
    end;
  end;
end;

procedure TEditVertexValueTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    Cursor := crVertexValue;
  end;
end;

procedure TEditVertexValueTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if FDoubleClicked then
  begin
    EditPoint(X, Y);
  end
  else if not AreScreenObjectsSelected then
  begin
    SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift),
      True);
  end;
  FDoubleClicked := False;
//  FX := X;
//  FY := Y;
end;

{ TPoint64 }

class operator TPoint64.Implicit(APoint: TPoint): TPoint64;
begin
  result.X := APoint.X;
  result.Y := APoint.Y;
end;

{ TRulerTool }

procedure TRulerTool.Activate;
begin
  inherited;
  Cursor := crMeasure;
  CreateLayers;
end;

constructor TRulerTool.Create(AOwner: TComponent);
begin
  inherited;
  FX := -1;
  FY := -1;
end;

procedure TRulerTool.Deactivate;
begin
  inherited;
  FreeAndNil(FLine);
//  frmGoPhast.hntMeasure.CancelHint;
  frmGoPhast.bhntMeasureRuler.CancelHint;
end;

procedure TRulerTool.DeleteLastSavedPoint;
begin
  if FLine <> nil then
  begin
    if FLine.Count > 2 then
    begin
      FLine.DeleteNextToLastPoint;
      ShowHintAtAPoint(FX, FY, FControl, True);
//      ShowHint;
    end
    else
    begin
      FreeAndNil(FLine);
//      frmGoPhast.hntMeasure.CancelHint;
      frmGoPhast.bhntMeasureRuler.CancelHint;
    end;
    ZoomBox.InvalidateImage32;
  end;
end;

procedure TRulerTool.ShowHintAtAPoint(X: Integer; Y: Integer; Sender: TObject; ForceShow: Boolean = False);
var
  APoint: TPoint;
  Hint: string;
begin
  if (((FX <> X) or (FY <> Y)) or ForceShow) and (FLine <> nil) then
  begin
    FX := X;
    FY := Y;
    FControl := Sender as TControl;
    APoint := FControl.ClientToScreen(Point(X, Y));
    APoint := frmGoPhast.ScreenToClient(APoint);
    Hint := Format(StrLineLengthG, [FLine.LineLength]);
    frmGoPhast.bhntMeasureRuler.ActivateHintPos(frmGoPhast, APoint, '', Hint,
      cJvBallonHintVisibleTimeDefault, ikNone);
  end;
end;

//procedure TRulerTool.ShowHint;
//var
//  Hint: string;
//begin
//  if FLine = nil then
//  begin
//    Exit;
//  end;
//  FRect.TopLeft := FLine.ZoomBox.ClientToScreen(Point(0, 0));
//  FRect.BottomRight := FLine.ZoomBox.ClientToScreen(
//    Point(FLine.ZoomBox.Width, FLine.ZoomBox.Height));
//  Hint := Format(StrLineLengthG, [FLine.LineLength]);
//  frmGoPhast.hntMeasure.ActivateHint(FRect, Hint);
//  frmGoPhast.sbMain.Panels[1].Text := Hint;
//end;

//procedure TRulerTool.ShowHintAtPoint(APoint: TPoint);
//var
//  Hint: String;
//begin
//  if FLine = nil then
//  begin
//    Exit;
//  end;
////  APoint := AControl.ClientToScreen(APoint);
////  APoint := frmGoPhast.ScreenToClient(APoint);
//  Hint := Format(StrLineLengthG, [FLine.LineLength]);
//  frmGoPhast.jvblnhnt1.ActivateHintPos(frmGoPhast, APoint, '', Hint,
//    cJvBallonHintVisibleTimeDefault, ikNone);
//
//end;

//procedure TRulerTool.TimerHintTimer(Sender: TObject);
//begin
//  ShowHint;
//end;

destructor TRulerTool.Destroy;
begin
  FLine.Free;
  inherited;
end;

procedure TRulerTool.DoubleClick(Sender: TObject);
var
  TempZB: TQRbwZoomBox2;
begin
  inherited;
  TempZB := nil;
  if FLine <> nil then
  begin
    TempZB := FLine.ZoomBox;
  end;
  FreeAndNil(FLine);
  if TempZB <> nil then
  begin
    TempZB.InvalidateImage32;
  end;
//  frmGoPhast.hntMeasure.CancelHint;
  frmGoPhast.bhntMeasureRuler.CancelHint;
  FDoubleClicked := True;
end;

procedure TRulerTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.CurrentTool <> self then Exit;
  if (FLine <> nil) and (FLine.ZoomBox = ZoomBox) then
  begin
    Buffer.BeginUpdate;
    try
      FLine.Draw(Buffer);
    finally
      Buffer.EndUpdate
    end;
  end;
end;

function TRulerTool.GetHint: string;
begin
  result := StrClickToStartMeasu
end;

procedure TRulerTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  APoint2D: TPoint2D;
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    if (FLine <> nil) and (FLine.ZoomBox = ZoomBox) then
    begin
      APoint2D.x := ZoomBox.X(X);
      APoint2D.y := ZoomBox.Y(Y);
      FLine.Points[FLine.Count-1] := APoint2D;
      // If the cursor has moved, draw the rest of the measuring line
      ZoomBox.InvalidateImage32;
//      FTimer.Enabled := True;
      // and show the hint.
//      ShowHint;
      ShowHintAtAPoint(X, Y, Sender);

    end;
  end;
end;

procedure TRulerTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
  TempZB: TQRbwZoomBox2;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if FDoubleClicked then
  begin
    FDoubleClicked := False;
  end
  else
  begin
    APoint.x := ZoomBox.X(X);
    APoint.y := ZoomBox.Y(Y);
    if (FLine <> nil) and (FLine.ZoomBox <> ZoomBox) then
    begin
      TempZB := FLine.ZoomBox;
      FreeAndNil(FLine);
      TempZB.InvalidateImage32;
//      frmGoPhast.hntMeasure.CancelHint;
      frmGoPhast.bhntMeasureRuler.CancelHint;
    end;
    if FLine = nil then
    begin
//      frmGoPhast.hntMeasure.CancelHint;
      frmGoPhast.bhntMeasureRuler.CancelHint;
      FLine := TSimpleLine.Create(ZoomBox);
      FLine.AddPoint(APoint);
    end;
    if FLine.ZoomBox = ZoomBox then
    begin
      FLine.AddPoint(APoint);
    end
  end;
  ZoomBox.InvalidateImage32;
end;

{ TEditCrossSectionTool }

procedure TEditCrossSectionTool.Activate;
var
  CrossSection: TMeshCrossSectionLine;
begin
  inherited;
  CreateLayers;
  CrossSection := GetCrossSection;
  CrossSection.Editing := True;
  View.ModelChanged := True;
  Cursor := crMoveCrossSection;
  UpdateAllViews;
end;

procedure TEditCrossSectionTool.Deactivate;
var
  CrossSection: TMeshCrossSectionLine;
begin
  inherited;
  CrossSection := GetCrossSection;
  CrossSection.Editing := False;
  View.ModelChanged := True;
  UpdateAllViews;
end;

procedure TEditCrossSectionTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: GoPhastTypes.TPointArray;
  ZoomBox: TQRbwZoomBox2;
  CrossSection: TMeshCrossSectionLine;
  NewSegment: TSegment2D;
  DeltaX: Extended;
  DeltaY: Extended;
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if (FPoint1Selected or FPoint2Selected) and (ViewDirection = vdTop) then
  begin
    SetLength(Points, 2);
    ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    CrossSection := GetCrossSection;
    NewSegment := CrossSection.Segment;

    if FPoint1Selected and FPoint2Selected then
    begin
      DeltaX := ZoomBox.X(FCurrentX)-ZoomBox.X(FStartX);
      DeltaY := ZoomBox.Y(FCurrentY)-ZoomBox.Y(FStartY);
      NewSegment[1].x := NewSegment[1].x + DeltaX;
      NewSegment[1].y := NewSegment[1].y + DeltaY;
      NewSegment[2].x := NewSegment[2].x + DeltaX;
      NewSegment[2].y := NewSegment[2].y + DeltaY;
    end
    else if FPoint1Selected then
    begin
      NewSegment[1].x := ZoomBox.X(FCurrentX);
      NewSegment[1].y := ZoomBox.Y(FCurrentY);
    end
    else if FPoint2Selected then
    begin
      NewSegment[2].x := ZoomBox.X(FCurrentX);
      NewSegment[2].y := ZoomBox.Y(FCurrentY);
    end;


    Points[0] := ConvertTop2D_Point(ZoomBox, NewSegment[1]);
    Points[1] := ConvertTop2D_Point(ZoomBox, NewSegment[2]);
    DrawBigPolyline32(Buffer, Color32(CrossSection.Color),
      OrdinaryGridLineThickness, Points, True, True);
  end;
end;

procedure TEditCrossSectionTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LocalZoomBox: TQRbwZoomBox2;
  CrossSection: TMeshCrossSectionLine;
  X1: Integer;
  X2: Integer;
  Y1: Integer;
  Y2: Integer;
  MinX: Int64;
  MaxX: Int64;
  MinY: Int64;
  MaxY: Int64;
  Selected: Boolean;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;

  FPoint1Selected := False;
  FPoint2Selected := False;
  if (ViewDirection = vdTop)
    and GetMeshOrDisvAvailable then
  begin
    FStartX := X;
    FStartY := Y;
    LocalZoomBox := ZoomBox;
    CrossSection := GetCrossSection;
    Selected := False;

    X1 := LocalZoomBox.XCoord(CrossSection.StartX);
    X2 := LocalZoomBox.XCoord(CrossSection.EndX);
    Y1 := LocalZoomBox.YCoord(CrossSection.StartY);
    Y2 := LocalZoomBox.YCoord(CrossSection.EndY);
    MinX := Int64(Min(X1, X2)) - SelectEpsilon;
    MaxX := Int64(Max(X1, X2)) + SelectEpsilon;
    MinY := Int64(Min(Y1, Y2)) - SelectEpsilon;
    MaxY := Int64(Max(Y1, Y2)) + SelectEpsilon;

    if IsValueInside(MinX, X, MaxX) and IsValueInside(MinY, Y, MaxY) then
    begin
      if (X1 = X2) or (Y1 = Y2) then
      begin
        Selected := True;
      end
      else
      begin
        if Abs(X2 - X1) > Abs(Y2 - Y1) then
        begin
          if Abs((X - X1) / (X2 - X1) * (Y2 - Y1) + Y1 - Y) < SelectEpsilon then
          begin
            Selected := True;
          end
        end
        else
        begin
          if Abs((Y - Y1) / (Y2 - Y1) * (X2 - X1) + X1 - X) < SelectEpsilon then
          begin
            Selected := True;
          end
        end;
      end;
    end;
    if Selected then
    begin
      if (Abs(X - X1) < SelectEpsilon) and (Abs(Y - Y1) < SelectEpsilon)  then
      begin
        FPoint1Selected := True;
        FPoint2Selected := False;
      end
      else if (Abs(X - X2) < SelectEpsilon) and (Abs(Y - Y2) < SelectEpsilon)  then
      begin
        FPoint2Selected := True;
        FPoint1Selected := False;
      end
      else
      begin
        FPoint1Selected := True;
        FPoint2Selected := True;
      end;
    end;
  end;
end;

procedure TEditCrossSectionTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    FCurrentX := X;
    FCurrentY := Y;
    if (ViewDirection = vdTop) and (ssLeft in Shift) then
    begin
      ZoomBox.InvalidateImage32;
    end;
  end;
end;

procedure TEditCrossSectionTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LocalZoomBox: TQRbwZoomBox2;
  DeltaX: double;
  DeltaY: Extended;
  NewSegment: TSegment2D;
  Undo: TUndoMoveCrossSection;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;

  LocalZoomBox := ZoomBox;

  if FPoint1Selected or FPoint2Selected then
  begin
    NewSegment := GetCrossSection.Segment;
    if FPoint1Selected and FPoint2Selected then
    begin
      DeltaX := LocalZoomBox.X(X)-LocalZoomBox.X(FStartX);
      DeltaY := LocalZoomBox.Y(Y)-LocalZoomBox.Y(FStartY);
      NewSegment[1].x := NewSegment[1].x + DeltaX;
      NewSegment[1].y := NewSegment[1].y + DeltaY;
      NewSegment[2].x := NewSegment[2].x + DeltaX;
      NewSegment[2].y := NewSegment[2].y + DeltaY;
    end
    else if FPoint1Selected then
    begin
      NewSegment[1].x := LocalZoomBox.X(X);
      NewSegment[1].y := LocalZoomBox.Y(Y);
    end
    else if FPoint2Selected then
    begin
      NewSegment[2].x := LocalZoomBox.X(X);
      NewSegment[2].y := LocalZoomBox.Y(Y);
    end;
    Undo := TUndoMoveCrossSection.Create(NewSegment);
    FPoint1Selected := False;
    FPoint2Selected := False;
    frmGoPhast.UndoStack.Submit(Undo);
  end;
end;
{ TRotateCrossSectionTool }

procedure TRotateCrossSectionTool.Activate;
var
  CrossSection: TMeshCrossSectionLine;
begin
  inherited;
  CreateLayers;
  FStarted := False;
  CrossSection := GetCrossSection;
  CrossSection.Editing := True;
  CalculateCenterPoint(FCenterPoint);
end;

procedure TRotateCrossSectionTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;

  if (ViewDirection = vdTop)
    and GetMeshOrDisvAvailable then
  begin
    FStart.x := ZoomBox.X(X);
    FStart.y := ZoomBox.Y(Y);
    FStarted := True;
  end;
end;

procedure TRotateCrossSectionTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    FCurrentX := X;
    FCurrentY := Y;
    if (ViewDirection = vdTop)
      and GetMeshOrDisvAvailable
      and FStarted then
    begin
      ZoomBox.InvalidateImage32;
    end;
  end;
end;

procedure TRotateCrossSectionTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewLocation: TSegment2D;
  Undo: TUndoRotateCrossSection;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if (ViewDirection = vdTop)
    and GetMeshOrDisvAvailable
    and FStarted then
  begin
    GetNewLocation(X, Y, NewLocation);
    Undo := TUndoRotateCrossSection.Create(NewLocation);
    frmGoPhast.UndoStack.Submit(Undo);
    FStarted := False;
  end;
end;

procedure CalculateCenterPoint(out FCenterPoint: TPoint2D);
var
  APoint: TPoint2D;
  Bottom: Double;
  InterSectPoint: TPoint2D;
  CrossSection: TMeshCrossSectionLine;
  CrossSegSegment: TSegment2D;
  Left: Double;
  Right: Double;
  ZoomBox: TQRbwZoomBox2;
  Top: Double;
  PointList: Generics.Collections.TList<TPoint2D>;
  EdgeSegment: TSegment2D;
begin
  PointList := TList<TPoint2D>.Create;
  try
    ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    Left := ZoomBox.X(0);
    Right := ZoomBox.X(ZoomBox.Width);
    Top := ZoomBox.Y(0);
    Bottom := ZoomBox.Y(ZoomBox.Height);
    CrossSection := GetCrossSection;
    APoint := CrossSection.StartPoint;
    if (APoint.x >= Left) and (APoint.x <= Right)
      and (APoint.y >= Bottom) and (APoint.y <= Top) then
    begin
      PointList.Add(APoint);
    end;
    APoint := CrossSection.EndPoint;
    if (APoint.x >= Left) and (APoint.x <= Right)
      and (APoint.y >= Bottom) and (APoint.y <= Top) then
    begin
      PointList.Add(APoint);
    end;
    CrossSegSegment := CrossSection.Segment;
    if PointList.Count < 2 then
    begin
      EdgeSegment[1].x := Left;
      EdgeSegment[2].x := Left;
      EdgeSegment[1].y := Top;
      EdgeSegment[2].y := Bottom;
      if Intersect(CrossSegSegment, EdgeSegment) then
      begin
        InterSectPoint := IntersectionPoint(CrossSegSegment, EdgeSegment);
        PointList.Add(InterSectPoint);
      end;
    end;
    if PointList.Count < 2 then
    begin
      EdgeSegment[1].x := Left;
      EdgeSegment[2].x := Right;
      EdgeSegment[1].y := Top;
      EdgeSegment[2].y := Top;
      if Intersect(CrossSegSegment, EdgeSegment) then
      begin
        InterSectPoint := IntersectionPoint(CrossSegSegment, EdgeSegment);
        PointList.Add(InterSectPoint);
      end;
    end;
    if PointList.Count < 2 then
    begin
      EdgeSegment[1].x := Right;
      EdgeSegment[2].x := Right;
      EdgeSegment[1].y := Top;
      EdgeSegment[2].y := Bottom;
      if Intersect(CrossSegSegment, EdgeSegment) then
      begin
        InterSectPoint := IntersectionPoint(CrossSegSegment, EdgeSegment);
        PointList.Add(InterSectPoint);
      end;
    end;
    if PointList.Count < 2 then
    begin
      EdgeSegment[1].x := Left;
      EdgeSegment[2].x := Right;
      EdgeSegment[1].y := Bottom;
      EdgeSegment[2].y := Bottom;
      if Intersect(CrossSegSegment, EdgeSegment) then
      begin
        InterSectPoint := IntersectionPoint(CrossSegSegment, EdgeSegment);
        PointList.Add(InterSectPoint);
      end;
    end;
    if PointList.Count = 0 then
    begin
      PointList.Add(CrossSection.StartPoint);
      PointList.Add(CrossSection.EndPoint);
    end;
    Assert(PointList.Count = 2);
    FCenterPoint.x := (PointList[0].x + PointList[1].x) / 2;
    FCenterPoint.y := (PointList[0].y + PointList[1].y) / 2;
  finally
    PointList.Free;
  end;
end;

procedure SetNewCrossSectionAngle(NewAngle: Double;
  out NewLocation: TSegment2D);
var
  DeltaX: Double;
  DeltaY: Double;
  SectionHalfLength: Double;
  CrossSection: TMeshCrossSectionLine;
  CenterPoint: TPoint2D;
begin
  CrossSection := GetCrossSection;
  CalculateCenterPoint(CenterPoint);
  SectionHalfLength := Distance(CrossSection.StartPoint,
    CrossSection.EndPoint) / 2;
  DeltaX := SectionHalfLength * Cos(NewAngle);
  DeltaY := SectionHalfLength * Sin(NewAngle);
  NewLocation[1].x := CenterPoint.x - DeltaX;
  NewLocation[1].y := CenterPoint.y - DeltaY;
  NewLocation[2].x := CenterPoint.x + DeltaX;
  NewLocation[2].y := CenterPoint.y + DeltaY;
end;

procedure TRotateCrossSectionTool.Deactivate;
var
  CrossSection: TMeshCrossSectionLine;
begin
  inherited;
  CrossSection := GetCrossSection;
  CrossSection.Editing := False;
  View.ModelChanged := True;
  UpdateAllViews;
end;

procedure TRotateCrossSectionTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: GoPhastTypes.TPointArray;
  NewLocation: TSegment2D;
  CrossSection: TMeshCrossSectionLine;
//  MeshAvailable: Boolean;
//  DisvGridAvailable: Boolean;
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if (ViewDirection = vdTop)
    and GetMeshOrDisvAvailable
    and FStarted then
  begin
    SetLength(Points, 2);
    GetNewLocation(FCurrentX, FCurrentY, NewLocation);
    Points[0] := ConvertTop2D_Point(ZoomBox, NewLocation[1]);
    Points[1] := ConvertTop2D_Point(ZoomBox, NewLocation[2]);
    CrossSection := GetCrossSection;
    DrawBigPolyline32(Buffer, Color32(CrossSection.Color),
      OrdinaryGridLineThickness, Points, True, True);
  end;
end;

procedure TRotateCrossSectionTool.GetNewLocation(X, Y: Integer;
  out NewLocation: TSegment2D);
var
  DeltaAngle: Double;
  NewAngle: Double;
  EndPoint: TPoint2D;
  CrossSection: TMeshCrossSectionLine;
//  CrossSection: TCrossSection;
begin
  EndPoint.x := ZoomBox.X(X);
  EndPoint.y := ZoomBox.Y(Y);
  DeltaAngle := VertexAngleRadians(FStart, FCenterPoint, EndPoint);
  if Orientation(FStart, FCenterPoint, EndPoint) <> Clockwise then
  begin
    DeltaAngle := 2 * pi - DeltaAngle;
  end;
  CrossSection := GetCrossSection;
  NewAngle := CrossSection.Angle + DeltaAngle;
  while NewAngle > Pi/2 do
  begin
    NewAngle := NewAngle-Pi;
  end;
  while NewAngle < -Pi/2 do
  begin
    NewAngle := NewAngle+Pi;
  end;
  SetNewCrossSectionAngle(NewAngle, NewLocation);
end;

{ TMoveSutraNodesTool }

procedure TMoveSutraNodesTool.Activate;
var
  FMesh: TSutraMesh3D;
  Limits: TGridLimit;
begin
  inherited;
  FSelectedNodes.Clear;
  FSelectedElements.Clear;
  FNodeQuadTree.Clear;
  FMesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  Limits := FMesh.MeshLimits(vdTop, 0);
  FNodeQuadTree.XMin := Limits.MinX;
  FNodeQuadTree.XMax := Limits.MaxX;
  FNodeQuadTree.YMin := Limits.MinY;
  FNodeQuadTree.YMax := Limits.MaxY;
  UpdateQuadTree;
end;

constructor TMoveSutraNodesTool.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedNodes:= TSutraNode2D_List.Create;
  FSelectedElements:= TSutraElement2D_List.Create;
  FNodeQuadTree := TRbwQuadTree.Create(self);
end;

procedure TMoveSutraNodesTool.DeleteSelectedNodesAndElements;
var
  NodeIndex: Integer;
  Mesh: TSutraMesh3D;
  ElementIndex: Integer;
  ANode: TSutraNode2D;
  AnElement: TSutraElement2D;
  NodePosition: Integer;
  Undo: TUndoChangeMesh;
begin
  Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  if (Mesh <> nil) and ((FSelectedNodes.Count > 0)
    or (FSelectedElements.Count > 0)) then
  begin
    Undo := TUndoChangeMesh.Create;
    try
      Mesh.BeginUpdate;
      Mesh.Mesh2D.BeginUpdate;
      try
        for ElementIndex := 0 to FSelectedElements.Count - 1 do
        begin
          AnElement := FSelectedElements[ElementIndex];
          for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
          begin
            ANode := AnElement.Nodes[NodeIndex].Node;
            ANode.RemoveElement(AnElement);
            if ANode.ElementCount = 0 then
            begin
              ANode.Free;
            end;
          end;
          AnElement.Free;
        end;

        for NodeIndex := 0 to FSelectedNodes.Count - 1 do
        begin
          ANode := FSelectedNodes[NodeIndex];
          for ElementIndex := 0 to ANode.ElementCount - 1 do
          begin
            AnElement := ANode.Elements[ElementIndex];
            NodePosition := AnElement.Nodes.IndexOfNode(ANode);
            AnElement.Nodes.Delete(NodePosition);
            if AnElement.Nodes.Count = 0 then
            begin
              AnElement.Free;
            end;
          end;
          ANode.Free;
        end;

        for ElementIndex := Mesh.Mesh2D.Elements.Count - 1 downto 0 do
        begin
          if Mesh.Mesh2D.Elements[ElementIndex].Nodes.Count < 4 then
          begin
            AnElement := Mesh.Mesh2D.Elements[ElementIndex];
            for NodeIndex := AnElement.Nodes.Count - 1 downto 0 do
            begin
              ANode := AnElement.Nodes[NodeIndex].Node;
              ANode.RemoveElement(AnElement);

//              AnElement.Nodes.Delete(NodeIndex);
              if ANode.ElementCount = 0 then
              begin
                ANode.Free;
              end;
            end;
            Mesh.Mesh2D.Elements.Delete(ElementIndex);
          end;
        end;
      finally
        Mesh.Mesh2D.Renumber;
        Mesh.Mesh2D.EndUpdate;
        Mesh.EndUpdate;
      end;
      Undo.UpdateNewMesh(frmGoPhast.PhastModel.SutraMesh);
      frmGoPhast.UndoStack.Submit(Undo);
      Activate;
    except
      Undo.Free;
    end;
  end;
end;

destructor TMoveSutraNodesTool.Destroy;
begin
  FSelectedNodes.Free;
  FSelectedElements.Free;
  inherited;
end;

procedure TMoveSutraNodesTool.DoubleClick(Sender: TObject);
var
  NodeList: TSutraNode2D_List;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  Comparer: TSutraNode2DNumberComparer;
begin
  inherited;
  if (FSelectedNodes.Count >= 1) or (FSelectedElements.Count >= 1) then
  begin
    NodeList := TSutraNode2D_List.Create;
    try
      NodeList.AddRange(FSelectedNodes);
      for ElementIndex := 0 to FSelectedElements.Count - 1 do
      begin
        AnElement := FSelectedElements[ElementIndex];
        for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
        begin
          ANode := AnElement.Nodes[NodeIndex].Node;
          if not ANode.Selected and (NodeList.IndexOf(ANode) < 0) then
          begin
            NodeList.Add(ANode);
          end;
        end;
      end;
      Comparer := TSutraNode2DNumberComparer.Create;
      try
        NodeList.Sort(Comparer);
      finally
        Comparer.Free;
      end;
      With TfrmNodeLocation.Create(nil) do
      begin
        GetData(NodeList);
        ShowModal;
        Free;
      end;
    finally
      NodeList.Free;
    end;
  end;
end;

function TMoveSutraNodesTool.GetCursor: TCursor;
begin
  result := crMoveNode;
end;

procedure TMoveSutraNodesTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  SearchRadius = 5;
var
  APoint: TPoint2D;
  SearchX: double;
  SearchY: double;
  Data: Pointer;
  NewX: Integer;
  NewY: Integer;
  ANode: TSutraNode2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
//  AllSelected: Boolean;
  AnotherNode: TSutraNode2D;
  NodeIndex: Integer;
  UpdateView: boolean;
  Undo: TUndoSelectNodes;
  Mesh2D: TSutraMesh2D;
  index: Integer;
  FoundElement: Boolean;
  function HandleElement: boolean;
  var
    index: Integer;
  begin
    Result := False;
    if AnElement.IsInside(APoint) then
    begin
      result := True;
//      FoundElement := True;
      if ssShift in Shift then
      begin
        AnElement.Selected := not AnElement.Selected;
        if AnElement.Selected then
        begin
          if FSelectedElements.IndexOf(AnElement) < 0 then
          begin
            FSelectedElements.Add(AnElement);
          end;
        end
        else
        begin
          FSelectedElements.Remove(AnElement);
        end;
        UpdateView := True;
      end
      else
      begin
        if (FSelectedElements.Count <> 1) or not AnElement.Selected then
        begin
          UpdateView := True;
          for index := 0 to FSelectedElements.Count - 1 do
          begin
            FSelectedElements[index].Selected := False;
          end;
          FSelectedElements.Clear;
          AnElement.Selected := True;
          FSelectedElements.Add(AnElement);
        end;
      end;

//      Break;
    end
  end;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if ViewDirection <> vdTop then
  begin
    Exit;
  end;
  UpdateView := False;
  if FNodeQuadTree.Count = 0 then
  begin
    Exit;
  end;
  Undo := TUndoSelectNodes.Create;
  FStartX := X;
  FStartY := Y;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  SearchX := APoint.x;
  SearchY := APoint.y;
  UpdateQuadTree;
  FNodeQuadTree.FirstNearestPoint(SearchX, SearchY, Data);
  NewX := ZoomBox.XCoord(SearchX);
  NewY := ZoomBox.YCoord(SearchY);
  ANode := Data;
  Mesh2D := frmGoPhast.PhastModel.SutraMesh.Mesh2D;
  if not (ssShift in Shift) then
  begin
    for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
    begin
      AnotherNode := Mesh2D.Nodes[NodeIndex];
      if AnotherNode <> ANode then
      begin
        AnotherNode.Selected := False;
        UpdateView := True;
      end;
    end;
    FSelectedNodes.Clear;
    for index := 0 to FSelectedElements.Count - 1 do
    begin
      FSelectedElements[index].Selected := False;
    end;
    FSelectedElements.Clear;
  end;

  if (Abs(X-NewX) < SearchRadius) and (Abs(Y-NewY) < SearchRadius) then
  begin
    if ssShift in Shift then
    begin
      ANode.Selected := not ANode.Selected;
      if ANode.Selected then
      begin
        if FSelectedNodes.IndexOf(ANode) < 0 then
        begin
          FSelectedNodes.Add(ANode);
          UpdateView := True;
        end;
      end
      else
      begin
        FSelectedNodes.Remove(ANode);
        UpdateView := True;
      end;
    end
    else
    begin
      ANode.Selected := True;
      if FSelectedNodes.IndexOf(ANode) < 0 then
      begin
        FSelectedNodes.Add(ANode);
        UpdateView := True;
      end;
    end;
  end
  else
  begin
    FoundElement := False;
    for ElementIndex := 0 to ANode.ElementCount - 1 do
    begin
      AnElement := ANode.Elements[ElementIndex];
      if HandleElement then
      begin
        FoundElement := True;
        Break;
      end;
//      if AnElement.IsInside(APoint) then
//      begin
//        FoundElement := True;
//        if ssShift in Shift then
//        begin
//          AnElement.Selected := not AnElement.Selected;
//          if AnElement.Selected then
//          begin
//            FSelectedElements.Add(AnElement);
//          end
//          else
//          begin
//            FSelectedElements.Remove(AnElement);
//          end;
//          UpdateView := True;
//        end
//        else
//        begin
//          if (FSelectedElements.Count <> 1) or not AnElement.Selected then
//          begin
//            UpdateView := True;
//            for index := 0 to FSelectedElements.Count - 1 do
//            begin
//              FSelectedElements[index].Selected := False;
//            end;
//            FSelectedElements.Clear;
//            AnElement.Selected := True;
//            FSelectedElements.Add(AnElement);
//          end;
//        end;
//
//        Break;
//      end;
    end;
    if not FoundElement then
    begin
      for ElementIndex := 0 to Mesh2D.Elements.Count - 1 do
      begin
        AnElement := Mesh2D.Elements[ElementIndex];
        if HandleElement then
        begin
          Break;
        end;
      end;
    end;
  end;
  for NodeIndex := FSelectedNodes.Count - 1 downto 0 do
  begin
    if not FSelectedNodes[NodeIndex].Selected then
    begin
      FSelectedNodes.Delete(NodeIndex);
    end;
  end;
  for ElementIndex := FSelectedElements.Count - 1 downto 0 do
  begin
    if not FSelectedElements[ElementIndex].Selected then
    begin
      FSelectedElements.Delete(ElementIndex);
    end;
  end;

  if UpdateView then
  begin
    Undo.UpdateSelectedNodes(FSelectedNodes);
    Undo.UpdateSelectedElements(FSelectedElements);
    frmGoPhast.UndoStack.Submit(Undo);
    Activate;
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TMoveSutraNodesTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    UpdateCursors;
  end;
end;

procedure TMoveSutraNodesTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  MinDistance = 3;
var
  StartX: Extended;
  StartY: Extended;
  EndX: Extended;
  EndY: Extended;
  DeltaX: Extended;
  DeltaY: Extended;
  ANode: TSutraNode2D;
  index: Integer;
  MoveOk: Boolean;
  NodePoint: TPoint2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  NodePosition: Integer;
  Neighbor1Position: Integer;
  Neighbor2Position: Integer;
  OppositePosition: Integer;
  Neighbor1: TSutraNode2D;
  Neighbor2: TSutraNode2D;
  Opposite: TSutraNode2D;
  Neighbor1Point: TPoint2D;
  Neighbor2Point: TPoint2D;
  OppositePoint: TPoint2D;
  ErrorMessage: string;
  Nodes: TSutraNode2D_Collection;
  Undo: TUndoMoveSutraNodes;
  NodeIndex: integer;
  Mesh2D: TSutraMesh2D;
  Elements: TSutraElement2D_Collection;
  procedure SetErrorMessage;
  var
    NodeLocation: TPoint2D;
  begin
    NodeLocation := ANode.Location;
    ErrorMessage := Format(StrYouCanNotMoveThe,
      [NodeLocation.x, NodeLocation.y]);
    Beep;
  end;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  if ((Abs(X-FStartX)>MinDistance) or (Abs(Y-FStartY)>MinDistance))
    and ((FSelectedNodes.Count > 0) or (FSelectedElements.Count > 0)) then
  begin
    FSelectedNodes.Clear;
    FSelectedElements.Clear;
    Mesh2D := frmGoPhast.PhastModel.SutraMesh.Mesh2D;
    for index := 0 to Mesh2D.Nodes.Count - 1 do
    begin
      ANode := Mesh2D.Nodes[index];
      if ANode.Selected then
      begin
        FSelectedNodes.Add(ANode);
      end;
    end;
    for Index := 0 to Mesh2D.Elements.Count - 1 do
    begin
      AnElement := Mesh2D.Elements[Index];
      if AnElement.Selected then
      begin
        for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
        begin
          ANode := AnElement.Nodes[NodeIndex].Node;
          if not ANode.Selected then
          begin
            FSelectedNodes.Add(ANode);
          end;
        end;
      end;
    end;
    StartX := ZoomBox.X(FStartX);
    StartY := ZoomBox.Y(FStartY);
    EndX := ZoomBox.X(X);
    EndY := ZoomBox.Y(Y);
    DeltaX := EndX-StartX;
    DeltaY := EndY-StartY;
    MoveOk := True;
    ErrorMessage := '';
    for index := 0 to FSelectedNodes.Count - 1 do
    begin
      ANode := FSelectedNodes[index];
      NodePoint := ANode.Location;
      NodePoint.x := NodePoint.x + DeltaX;
      NodePoint.y := NodePoint.y + DeltaY;
      for ElementIndex := 0 to ANode.ElementCount - 1 do
      begin
        AnElement := ANode.Elements[ElementIndex];
        Assert(AnElement.Nodes.Count = 4);
        NodePosition := AnElement.Nodes.IndexOfNode(ANode);
        Assert(NodePosition >= 0);
        if NodePosition > 0 then
        begin
          Neighbor1Position := NodePosition - 1;
        end
        else
        begin
          Neighbor1Position := 3;
        end;
        if NodePosition < 3 then
        begin
          Neighbor2Position := NodePosition + 1;
        end
        else
        begin
          Neighbor2Position := 0;
        end;
        OppositePosition := NodePosition - 2;
        if OppositePosition < 0 then
        begin
          Inc(OppositePosition, 4);
        end;
        Neighbor1 := AnElement.Nodes[Neighbor1Position].Node;
        Neighbor2 := AnElement.Nodes[Neighbor2Position].Node;
        Opposite := AnElement.Nodes[OppositePosition].Node;
        Neighbor1Point := Neighbor1.Location;
        if Neighbor1.Selected then
        begin
          Neighbor1Point.x := Neighbor1Point.x + DeltaX;
          Neighbor1Point.y := Neighbor1Point.y + DeltaY;
        end;
        Neighbor2Point := Neighbor2.Location;
        if Neighbor2.Selected then
        begin
          Neighbor2Point.x := Neighbor2Point.x + DeltaX;
          Neighbor2Point.y := Neighbor2Point.y + DeltaY;
        end;
        OppositePoint := Opposite.Location;
        if Opposite.Selected then
        begin
          OppositePoint.x := OppositePoint.x + DeltaX;
          OppositePoint.y := OppositePoint.y + DeltaY;
        end;
        if Intersect(EquateSegment(NodePoint, Neighbor1Point),
          EquateSegment(Neighbor2Point, OppositePoint)) then
        begin
          MoveOk := False;
          SetErrorMessage;
          break;
        end;
        if Intersect(EquateSegment(NodePoint, Neighbor2Point),
          EquateSegment(Neighbor1Point, OppositePoint)) then
        begin
          MoveOk := False;
          SetErrorMessage;
          break;
        end;
      end;
    end;
    if MoveOk
      or (MessageDlg(ErrorMessage, mtWarning, [mbYes, mbNo], 0, mbNo) = mrYes)
      then
    begin

      Undo := TUndoMoveSutraNodes.Create;
      try
        Nodes := (frmGoPhast.PhastModel.Mesh as TSutraMesh3D).Mesh2D.Nodes;
        for NodeIndex := 0 to Nodes.Count - 1 do
        begin
          ANode := Nodes[NodeIndex];
          if ANode.Selected then
          begin
            ANode.X := ANode.X + DeltaX;
            ANode.Y := ANode.Y + DeltaY;
            for index := 0 to ANode.ElementCount - 1 do
            begin
              AnElement := ANode.Elements[Index];
              AnElement.SetCorrectOrienatation;
            end;
          end;
        end;
        Elements := (frmGoPhast.PhastModel.Mesh as TSutraMesh3D).Mesh2D.Elements;
        for Index := 0 to Elements.Count - 1 do
        begin
          AnElement := Elements[Index];
          if AnElement.Selected then
          begin
            for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
            begin
              ANode := AnElement.Nodes[NodeIndex].Node;
              if not ANode.Selected then
              begin
                ANode.X := ANode.X + DeltaX;
                ANode.Y := ANode.Y + DeltaY;
              end;
            end;
            AnElement.SetCorrectOrienatation;
          end;
        end;
        Undo.UpdateNewMesh(frmGoPhast.PhastModel.SutraMesh);
      except
        Undo.Free;
        raise;
      end;

      frmGoPhast.UndoStack.Submit(Undo);

//      frmGoPhast.UndoStack.Submit(TUndoMoveSutraNodes.Create(DeltaX, DeltaY));
      Activate;
    end;
  end;
end;

procedure TMoveSutraNodesTool.UpdateQuadTree;
var
  FMesh: TSutraMesh3D;
  APoint: TPoint2D;
  Node2DIndex: Integer;
  ANode: TSutraNode2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
begin
  FMesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  FNodeQuadTree.Clear;
  for Node2DIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
  begin
    ANode := FMesh.Mesh2D.Nodes[Node2DIndex];
    APoint := ANode.Location;
    FNodeQuadTree.AddPoint(APoint.x, APoint.y, ANode);
    if ANode.Selected then
    begin
      FSelectedNodes.Add(ANode);
    end;
  end;
  FSelectedElements.Clear;
  for ElementIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
  begin
    AnElement := FMesh.Mesh2D.Elements[ElementIndex];
    if AnElement.Selected then
    begin
      FSelectedElements.Add(AnElement);
    end;
  end;
end;

{ TFishnetMeshGenerationTool }

procedure TFishnetMeshGenerationTool.Activate;
  procedure SetEditProperties(Edit: TRbwDataEntry);
  begin
    Edit.Visible := False;
    Edit.Parent := ZoomBox.Image32;
    Edit.Text := '1';
    Edit.DataType := dtInteger;
    Edit.Min := 1;
    Edit.CheckMin := True;
    Edit.OnChange := EditChanged;
    Edit.Width := 60;
  end;
var
  LastElement: TFishnetMeshElement;
begin
  inherited;
  if FUndoValues = nil then
  begin
    FUndoValues := TUndoFishnetMeshValues.Create;
  end;
  if FUndoGeometry = nil then
  begin
    FUndoGeometry := TUndoFishnetMesh.Create;
  end;
  if FEdit1 = nil then
  begin
    Assert(FEdit2 = nil);

    FEdit1 := TRbwDataEntry.Create(self);
    SetEditProperties(FEdit1);

    FEdit2 := TRbwDataEntry.Create(self);
    SetEditProperties(FEdit2);
  end;


  CreateLayers;
  FFishnetGenerator := frmGoPhast.PhastModel.FishnetMeshGenerator;
  UpdateQuadTree;

  FSelectedElement:= nil;
  if FFishnetGenerator.Elements.Count > 0 then
  begin
    LastElement := FFishnetGenerator.Elements[
      FFishnetGenerator.Elements.Count-1];
    if LastElement.Nodes.Count < 4 then
    begin
      FElement := LastElement;
      FSelectedElement := LastElement;
      LastElement.Selected := True;
    end;
  end;
end;

constructor TFishnetMeshGenerationTool.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedElement := nil;
  FFishnetGenerator := nil;
  FNodes := TRbwQuadTree.Create(self);

end;

procedure TFishnetMeshGenerationTool.CreateLayers;
  function CreateLayer(ZoomBox: TQRbwZoomBox2): TPositionedLayer;
  begin
    result := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
      TPositionedLayer;
    result.OnPaint := DrawOnBitMap32;
  end;
begin
  if FTopLayer = nil then
  begin
    FTopLayer := CreateLayer(frmGoPhast.frameTopView.ZoomBox);
  end;
end;

procedure TFishnetMeshGenerationTool.DeActivate;
begin
  inherited;
  FElement := nil;
  HideEdits;
end;

procedure TFishnetMeshGenerationTool.DeleteLastNode;
var
  LastElement: TFishnetMeshElement;
  LastNode: TFishnetMeshNode;
begin
  if FFishnetGenerator.Elements.Count = 0 then
  begin
    Exit;
  end;
  LastElement := FFishnetGenerator.Elements[
    FFishnetGenerator.Elements.Count-1];
  if LastElement.Nodes.Count = 0 then
  begin
    if FElement = LastElement then
    begin
      FElement := nil;
    end;
    LastElement.Free;
    SubmitUndo;
    Exit;
  end;
  LastNode := LastElement.Nodes.Items[LastElement.Nodes.Count-1];
  LastElement.Nodes.Delete(LastElement.Nodes.Count-1);
  LastNode.Elements.Remove(LastElement);
  if LastNode.Elements.Count = 0 then
  begin
    LastNode.Free;
  end;
  if LastElement.Nodes.Count = 0 then
  begin
    if FElement = LastElement then
    begin
      FElement := nil;
    end;
    LastElement.Free;
    SubmitUndo;
  end
  else
  begin
    FElement := LastElement;
    FElement.Selected := True;
  end;
  FSelectedElement := nil;
  HideEdits;
  UpdateQuadTree;
  frmGoPhast.InvalidateTop;
end;

procedure TFishnetMeshGenerationTool.SubmitUndoValues;
begin
  FUndoValues.UpdateNewFishnetMesh;
  FUndoGeometry.UpdateOldFishnetMesh;
  frmGoPhast.UndoStack.Submit(FUndoValues);
  FUndoValues := TUndoFishnetMeshValues.Create;
end;

procedure TFishnetMeshGenerationTool.SubmitUndo;
begin
  FUndoValues.UpdateOldFishnetMesh;
  FUndoGeometry.UpdateNewFishnetMesh;
  frmGoPhast.UndoStack.Submit(FUndoGeometry);
  FUndoGeometry := TUndoFishnetMesh.Create;
end;

procedure TFishnetMeshGenerationTool.DeleteSelectedElement;
var
  NodeIndex: Integer;
  ANode: TFishnetMeshNode;
begin
  if FSelectedElement <> nil then
  begin
    for NodeIndex := 0 to FSelectedElement.Nodes.Count - 1 do
    begin
      ANode := FSelectedElement.Nodes.Items[NodeIndex];
      ANode.Elements.Remove(FSelectedElement);
      if ANode.Elements.Count = 0 then
      begin
        if FSelectedNode = ANode then
        begin
          FSelectedNode := nil;
        end;
        ANode.Free;
      end;
    end;
    if FSelectedElement = FElement then
    begin
      FElement := nil;
    end;
    FSelectedElement.Free;
    FSelectedElement := nil;
    SubmitUndo;
    HideEdits;
    frmGoPhast.InvalidateTop;
    UpdateQuadTree;
  end;
end;

destructor TFishnetMeshGenerationTool.Destroy;
begin
  FUndoValues.Free;
  FUndoGeometry.Free;
  inherited;
end;

procedure TFishnetMeshGenerationTool.DoubleClick(Sender: TObject);
var
  AForm: TfrmFishnetElementProperties;
begin
  inherited;
  if (FSelectedElement <> nil) then
  begin
    if (FSelectedElement.Nodes.Count = 4) then
    begin
      AForm := TfrmFishnetElementProperties.Create(nil);
      try
        AForm.GetData(FSelectedElement);
        if AForm.ShowModal = mrOK then
        begin
          UpdateEdits(FSelectedElement);
          FFishnetGenerator.EliminateDuplicateNodes;
          SubmitUndoValues;
        end;
      finally
        AForm.Free
      end;
    end
    else
    begin
      if (MessageDlg(Format(StrTheSelectedFishnet,
        [FSelectedElement.Nodes.Count]), mtWarning, [mbYes, mbNo], 0) = mrYes)
        then
      begin
        DeleteSelectedElement;
      end;
    end;
  end;
end;

procedure TFishnetMeshGenerationTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.CurrentTool <> self then Exit;
  if ViewDirection <> vdTop then
  begin
    Exit;
  end;
  FFishnetGenerator.DrawTop(Buffer, ZoomBox);
end;

procedure TFishnetMeshGenerationTool.EditChanged(Sender: TObject);
var
  Value: Integer;
begin
  if FSelectedElement = nil then
  begin
    Exit;
  end;
  if Sender = FEdit1 then
  begin
    if not FEdit1.Visible then
    begin
      Exit;
    end;
    if TryStrToInt(FEdit1.Text, Value) then
    begin
      if Value >= 1 then
      begin
        FSelectedElement.FirstControl.Count := Value;
        FFishnetGenerator.UpdateCount1(FSelectedElement);
        SubmitUndoValues;
      end;
    end;
  end
  else if Sender = FEdit2 then
  begin
    if not FEdit2.Visible then
    begin
      Exit;
    end;
    if TryStrToInt(FEdit2.Text, Value) then
    begin
      if Value >= 1 then
      begin
        FSelectedElement.SecondControl.Count := Value;
        FFishnetGenerator.UpdateCount2(FSelectedElement);
        SubmitUndoValues;
      end;
    end;
  end
  else
  begin
    Exit;
  end;

end;

function TFishnetMeshGenerationTool.GetCursor: TCursor;
begin
  if FPanning then
  begin
    Result := PanTool.Cursor;
  end
  else
  begin
    result := crFishnet;
  end;

end;

procedure TFishnetMeshGenerationTool.HideEdits;
begin
  if FEdit1 <> nil then
  begin
    FEdit1.Visible := False;
    FEdit2.Visible := False;
  end;
end;

procedure TFishnetMeshGenerationTool.UpdateQuadTree;
var
  MinX: double;
  MaxX: double;
  MinY: double;
  MaxY: double;
  Local_index: Integer;
  Local_index1: Integer;
  AZoomBox: TQRbwZoomBox2;
  ANode: TFishnetMeshNode;
begin
  FNodes.Clear;
  MinX := 0;
  MaxX := 0;
  MinY := 0;
  MaxY := 0;
  if FFishnetGenerator = nil then
  begin
    Exit;
  end;
  for Local_index := FFishnetGenerator.Nodes.Count - 1 downto 0 do
  begin
    ANode := FFishnetGenerator.Nodes[Local_index];
    if ANode.Elements.Count = 0 then
    begin
      ANode.Free;
    end;
  end;
  if FFishnetGenerator.Nodes.Count > 0 then
  begin
    ANode := FFishnetGenerator.Nodes[0];
    MinX := ANode.X;
    MinY := ANode.Y;
    MaxX := MinX;
    MaxY := MinY;
    for Local_index := 1 to FFishnetGenerator.Nodes.Count - 1 do
    begin
      ANode := FFishnetGenerator.Nodes[Local_index];
      if ANode.X < MinX then
      begin
        MinX := ANode.X;
      end
      else if ANode.X > MaxX then
      begin
        MaxX := ANode.X;
      end;
      if ANode.Y < MinY then
      begin
        MinY := ANode.Y;
      end
      else if ANode.Y > MaxY then
      begin
        MaxY := ANode.Y;
      end;
    end;
  end;
  AZoomBox := ZoomBox;
  FNodes.XMax := AZoomBox.XCoord(MaxX);
  FNodes.XMin := AZoomBox.XCoord(MinX);
  FNodes.YMin := AZoomBox.YCoord(MaxY);
  FNodes.YMax := AZoomBox.YCoord(MinY);
  for Local_index1 := 0 to FFishnetGenerator.Nodes.Count - 1 do
  begin
    ANode := FFishnetGenerator.Nodes[Local_index1];
    FNodes.AddPoint(AZoomBox.XCoord(ANode.X), AZoomBox.YCoord(ANode.Y), ANode);
  end;
end;

procedure TFishnetMeshGenerationTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SelectionWidth = 5;
var
  ANode: TFishnetMeshNode;
  LocalX: double;
  LocalY: double;
  Data: TPointerArray;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  FPanning := False;
  FSelectedNode := nil;
  if ssShift in Shift then
  begin
    ANode := nil;
    if FFishnetGenerator.Nodes.Count > 0 then
    begin
      LocalX := X;
      LocalY := Y;
      FNodes.FindClosestPointsData(LocalX, LocalY, Data);
      if (Abs(X-LocalX) <= SelectionWidth)
        and  (Abs(Y-LocalY) <= SelectionWidth) then
      begin
        ANode := Data[0];
      end;
    end;
    if ANode <> nil then
    begin
      FSelectedNode := ANode;
      FSelectedNode.Selected := True;
      frmGoPhast.InvalidateTop;
    end
    else
    begin
      FPanning := True;
      UpdateCursors;
    end;

  end;
  if FPanning then
  begin
    PanTool.MouseDown(Sender, Button, Shift, X, Y);
  end;
end;

procedure TFishnetMeshGenerationTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (ssMiddle in Shift) then
  begin
    UpdateCursors;
  end;
end;

procedure TFishnetMeshGenerationTool.UpdateEdits(AnElement: TFishnetMeshElement);
begin
  AnElement.ComputeNumberCenterLocations(ZoomBox);

  FEdit1.Left := AnElement.CenterLocation1.X - FEdit1.Width div 2;
  FEdit1.Top := AnElement.CenterLocation1.Y - FEdit1.Height div 2;
  FEdit1.Text := IntToStr(AnElement.FirstControl.Count);
  FEdit1.Visible := True;

  FEdit2.Left := AnElement.CenterLocation2.X - FEdit2.Width div 2;
  FEdit2.Top := AnElement.CenterLocation2.Y - FEdit2.Height div 2;
  FEdit2.Text := IntToStr(AnElement.SecondControl.Count);
  FEdit2.Visible := True;
end;

procedure TFishnetMeshGenerationTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SelectionWidth = 5;
var
  ANode: TFishnetMeshNode;
  LocalX: double;
  LocalY: double;
  Data: TPointerArray;
  AZoomBox: TQRbwZoomBox2;
  APoint: TPoint2D;
  OkLocation: Boolean;
  ElementIndex: Integer;
  AnElement: TFishnetMeshElement;
  Segment1: TSegment2D;
  Segment2: TSegment2D;
  InsideElement: TFishnetMeshElement;
  AnotherNode: TFishnetMeshNode;
  NodeIndex: Integer;
  LastElement: TFishnetMeshElement;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  AZoomBox := ZoomBox;
  if FSelectedNode <> nil then
  begin
    FSelectedNode.X := AZoomBox.X(X);
    FSelectedNode.Y := AZoomBox.Y(Y);
    FFishnetGenerator.EliminateDuplicateNodes;
    HideEdits;
    FSelectedNode.Selected := False;
    frmGoPhast.InvalidateTop;
    SubmitUndo;
    Exit;
  end;
  if FPanning then
  begin
    PanTool.MouseUp(Sender, Button, Shift, X, Y);
    FPanning := False;
    HideEdits;
    UpdateCursors;
    Exit;
  end;
  if ViewDirection <> vdTop then
  begin
    Exit;
  end;
  if FEdit1.Visible then
  begin
    if FSelectedElement <> nil then
    begin
      FFishnetGenerator.UpdateCount1(FSelectedElement);
      FFishnetGenerator.UpdateCount2(FSelectedElement);
    end;
    HideEdits;
    ZoomBox.InvalidateImage32;
    Exit;
  end;
  ANode := nil;
  if FFishnetGenerator.Nodes.Count > 0 then
  begin
    LocalX := X;
    LocalY := Y;
    FNodes.FindClosestPointsData(LocalX, LocalY, Data);
    if (Abs(X-LocalX) <= SelectionWidth)
      and  (Abs(Y-LocalY) <= SelectionWidth) then
    begin
      ANode := Data[0];
      LastElement := FFishnetGenerator.Elements[FFishnetGenerator.Elements.Count-1];
      if ((FSelectedElement = nil) or (FSelectedElement = LastElement))
        and (LastElement.Nodes.IndexOf(ANode) >= 0) then
      begin
        Beep;
        Exit;
      end;
    end;
  end;
  InsideElement := nil;

  APoint.X := AZoomBox.X(X);
  APoint.Y := AZoomBox.Y(Y);
  OkLocation :=  True;
  if (FElement <> nil)  then
  begin
    for NodeIndex := 0 to FElement.Nodes.Count - 1 do
    begin
      OkLocation := not Coincident(APoint, FElement.Nodes[NodeIndex].Location);
      if not OkLocation then
      begin
        break;
      end;
    end;
    if OkLocation and (FElement.Nodes.Count = 3) then
    begin
      Segment1 := EquateSegment(
        FElement.Nodes[0].Location, FElement.Nodes[2].Location);
      Segment2 := EquateSegment(
        FElement.Nodes[1].Location, APoint);
      OkLocation := Intersect(Segment1, Segment2);
    end;
  end;
  if not OkLocation then
  begin
    Beep;
    Exit;
  end;

  if ANode = nil then
  begin
    if OkLocation then
    begin
      for ElementIndex := 0 to FFishnetGenerator.Elements.Count - 1 do
      begin
        AnElement := FFishnetGenerator.Elements[ElementIndex];
        if AnElement = FElement then
        begin
          Continue;
        end;
        if AnElement.IsInside(APoint) then
        begin
          InsideElement := AnElement;
          OkLocation := False;
          break;
        end;
      end;
    end;
    if OkLocation and (FElement <> nil) and (FElement.Nodes.Count >= 1) then
    begin
      if FElement.Nodes.Count = 3 then
      begin
        Segment1 := EquateSegment(
          FElement.Nodes[0].Location, APoint);
        Segment2 := EquateSegment(
          FElement.Nodes[2].Location, APoint);
        for ElementIndex := 0 to FFishnetGenerator.Elements.Count - 1 do
        begin
          AnElement := FFishnetGenerator.Elements[ElementIndex];
          if AnElement = FElement then
          begin
            Continue;
          end;
          OkLocation := not AnElement.Intersect(Segment1, FElement.Nodes)
            and not AnElement.Intersect(Segment2, FElement.Nodes);
          if not OkLocation then
          begin
            break;
          end;
        end;
      end
      else
      begin
        Segment1 := EquateSegment(
          FElement.Nodes[FElement.Nodes.Count-1].Location, APoint);
        for ElementIndex := 0 to FFishnetGenerator.Elements.Count - 1 do
        begin
          AnElement := FFishnetGenerator.Elements[ElementIndex];
          if AnElement = FElement then
          begin
            Continue;
          end;
          OkLocation := not AnElement.Intersect(Segment1, FElement.Nodes);
          if not OkLocation then
          begin
            break;
          end;
        end;
      end;
    end;
    if OkLocation then
    begin
      ANode := FFishnetGenerator.Nodes.Add;
      ANode.Location := APoint;
      FNodes.AddPoint(X, Y, ANode);
    end;
  end;
  if ANode <> nil then
  begin
    for ElementIndex := 0 to FFishnetGenerator.Elements.Count - 1 do
    begin
      AnElement := FFishnetGenerator.Elements[ElementIndex];
      if AnElement <> FElement then
      begin
        AnElement.Selected := False;
      end;
    end;
    if FElement = nil then
    begin
      HideEdits;
      FElement := FFishnetGenerator.Elements.Add;
      FElement.Selected := True;
      FSelectedElement := nil;
    end;
    FElement.Nodes.Add(ANode);
    if ANode.Elements.IndexOf(FElement) < 0 then
    begin
      ANode.Elements.Add(FElement);
    end;


    if FElement.Nodes.Count >= 3 then
    begin
      for NodeIndex := 0 to FFishnetGenerator.Nodes.Count - 1 do
      begin
        AnotherNode := FFishnetGenerator.Nodes[NodeIndex];
        if FElement.Nodes.IndexOf(AnotherNode) >= 0 then
        begin
          Continue;
        end;
        if FElement.IsInside(AnotherNode.Location) then
        begin
          FElement.Nodes.Remove(ANode);
          ANode.Elements.Remove(FElement);
          if ANode.Elements.Count = 0 then
          begin
            ANode.Free;
          end;
          Beep;
          Exit;
        end;
      end;
    end;

    if FElement.Nodes.Count = 4 then
    begin
      FSelectedElement := FElement;
      FFishnetGenerator.GetCountsFromNeighbors(FElement);

      UpdateEdits(FSelectedElement);
      FElement.Selected := False;
      FElement := nil;
      SubmitUndo;
      FEdit1.Visible := True;
      FEdit2.Visible := True;
    end;
  end
  else
  begin
    // prevent 5th node from being added to an element during double-click.
    if (FElement = nil) and (InsideElement <> nil) then
    begin
      InsideElement.Selected := True;
      FSelectedElement := InsideElement;
//      FElement := InsideElement;
      UpdateEdits(FSelectedElement);
    end;
  end;
  ZoomBox.InvalidateImage32;
end;

procedure TFishnetMeshGenerationTool.NilElement;
begin
  FElement := nil;
end;

{ TNewElement }

constructor TNewElement.Create;
begin
  FNodes:= TNewNodeList.Create;
end;

destructor TNewElement.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TNewElement.DrawEdges(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2; DrawPoints: boolean);
const
  NodeRadius = 3;
var
  Points: GoPhastTypes.TPointArray;
  ANode: TNewNode;
  NodeIndex: Integer;
  APoint: TPoint;
  ACanvas: TCanvas;
begin
  if FNodes.Count > 0 then
  begin
    SetLength(Points, FNodes.Count+1);
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, ANode.FLocation);
    end;
    Points[FNodes.Count] := Points[0];
    DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
      Points, True, True);
    if DrawPoints and (FNodes.Count <> 4) then
    begin
      ACanvas := TCanvas.Create;
      try
        ACanvas.Handle := BitMap.Handle;
        ACanvas.Pen.Color := clBlack;
        ACanvas.Brush.Style := bsClear;
        for NodeIndex := 0 to FNodes.Count - 1 do
        begin
          APoint := Points[NodeIndex];
          if (APoint.x >= 0) and (APoint.x < BitMap.Width)
            and (APoint.y >= 0) and (APoint.y < BitMap.Height) then
          begin
            ACanvas.Ellipse(APoint.X-NodeRadius,APoint.Y-NodeRadius,
              APoint.X+NodeRadius+1,APoint.Y+NodeRadius+1);
          end;
        end;
      finally
        ACanvas.Free;
      end;
    end;
  end;
end;

{ TDrawElementTool }

procedure TDrawElementTool.Activate;
var
  Node2DIndex: Integer;
  ANode: TSutraNode2D;
  APoint: TPoint2D;
  Limits: TGridLimit;
begin
  inherited;
  FMesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  Limits := FMesh.MeshLimits(vdTop, 0);
  FNodeQuadTree.Clear;
  FNodeQuadTree.XMin := Limits.MinX;
  FNodeQuadTree.XMax := Limits.MaxX;
  FNodeQuadTree.YMin := Limits.MinY;
  FNodeQuadTree.YMax := Limits.MaxY;

  FNewNodeQuadTree.Clear;
  FNewNodeQuadTree.XMin := Limits.MinX;
  FNewNodeQuadTree.XMax := Limits.MaxX;
  FNewNodeQuadTree.YMin := Limits.MinY;
  FNewNodeQuadTree.YMax := Limits.MaxY;

  for Node2DIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
  begin
    ANode := FMesh.Mesh2D.Nodes[Node2DIndex];
    APoint := ANode.Location;
    FNodeQuadTree.AddPoint(APoint.x, APoint.y, ANode);
  end;
  CreateLayers;
  FCurrentElement := nil;

end;

constructor TDrawElementTool.Create(AOwner: TComponent);
begin
  inherited;
  FNewNodeQuadTree := TRbwQuadTree.Create(self);
  FNodeQuadTree := TRbwQuadTree.Create(self);
  FNewNodes := TNewNodeObjectList.Create;
  FNewElements := TNewElementObjectList.Create;
end;

procedure TDrawElementTool.CreateLayers;
  function CreateLayer(ZoomBox: TQRbwZoomBox2): TPositionedLayer;
  begin
    result := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
      TPositionedLayer;
    result.OnPaint := DrawOnBitMap32;
  end;
begin
  if FTopLayer = nil then
  begin
    FTopLayer := CreateLayer(frmGoPhast.frameTopView.ZoomBox);
  end;
end;

procedure TDrawElementTool.DeActivate;
var
  index: Integer;
  LastElement: TNewElement;
  Undo: TUndoDrawElements;
  NodeIndex: Integer;
  NewNode: TNewNode;
  Mesh2D: TSutraMesh2D;
  ElementIndex: Integer;
  NewElement: TNewElement;
  AnElement: TSutraElement2D;
  NodeNumberItem: TSutraNodeNumber2D_Item;
begin
  inherited;
  if FNewElements.Count > 0 then
  begin
    LastElement := FNewElements.Last;
    if LastElement.FNodes.Count <> 4 then
    begin
      for index := 0 to LastElement.FNodes.Count - 1 do
      begin
        LastElement.FNodes[index].FNewElements.Remove(LastElement);
      end;
      FNewElements.Delete(FNewElements.Count-1);
      FCurrentElement := nil;
    end;
  end;
  if FNewElements.Count > 0 then
  begin
    Undo := TUndoDrawElements.Create;
    try
      Mesh2D := frmGoPhast.PhastModel.SutraMesh.Mesh2D;
      for NodeIndex := 0 to FNewNodes.Count - 1 do
      begin
        NewNode := FNewNodes[NodeIndex];
        if (NewNode.FNewElements.Count > 0) and (NewNode.FNode = nil) then
        begin
          NewNode.FNode := Mesh2D.Nodes.Add;
          NewNode.FNode.Location := NewNode.FLocation;
          NewNode.FNode.NodeType := ntEdge
        end;
      end;
      for ElementIndex := 0 to FNewElements.Count - 1 do
      begin
        NewElement := FNewElements[ElementIndex];
        AnElement := Mesh2D.Elements.Add;
        if Orientation(NewElement.FNodes[0].FNode.Location,
          NewElement.FNodes[1].FNode.Location,
          NewElement.FNodes[2].FNode.Location) = Clockwise then
        begin
          for NodeIndex := NewElement.FNodes.Count - 1 downto 0 do
          begin
            NewNode := NewElement.FNodes[NodeIndex];
            NodeNumberItem := AnElement.Nodes.Add;
            NodeNumberItem.Node := NewNode.FNode;
          end;
        end
        else
        begin
          for NodeIndex := 0 to NewElement.FNodes.Count - 1 do
          begin
            NewNode := NewElement.FNodes[NodeIndex];
            NodeNumberItem := AnElement.Nodes.Add;
            NodeNumberItem.Node := NewNode.FNode;
          end;
        end;
      end;
      Mesh2D.Renumber;
      FNewElements.Clear;
      FNewNodes.Clear;
      Undo.UpdateNewMesh(frmGoPhast.PhastModel.SutraMesh);
    except on E: Exception do
      begin
        Undo.Free;
        raise
      end;
    end;
    frmGoPhast.UndoStack.Submit(Undo);
  end;
end;

procedure TDrawElementTool.DeleteLastNode;
var
  LastElement: TNewElement;
  LastNode: TNewNode;
begin
  if FNewElements.Count > 0 then
  begin
    LastElement := FNewElements.Last;
    if LastElement.FNodes.Count > 0 then
    begin
      LastNode := LastElement.FNodes.Last;
      LastElement.FNodes.Delete(LastElement.FNodes.Count-1);
      LastNode.FNewElements.Remove(LastElement);
      if LastNode.FNewElements.Count = 0 then
      begin
        FNewNodeQuadTree.RemovePoint(
          LastNode.FLocation.x, LastNode.FLocation.y, LastNode);
        FNewNodes.Remove(LastNode);
      end;
      if LastElement.FNodes.Count = 0 then
      begin
        FNewElements.Delete(FNewElements.Count-1);
        FCurrentElement := nil;
      end
      else
      begin
        FCurrentElement := LastElement;
      end;
      ZoomBox.InvalidateImage32;
    end;
  end;
end;

destructor TDrawElementTool.Destroy;
begin
  FNewElements.Free;
  FNewNodes.Free;
  inherited;
end;

procedure TDrawElementTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
var
  index: Integer;
  NewElement: TNewElement;
  AZoomBox: TQRbwZoomBox2;
begin
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.CurrentTool <> self then Exit;
  if ViewDirection <> vdTop then
  begin
    Exit;
  end;
  AZoomBox := ZoomBox;
  for index := 0 to FNewElements.Count - 1 do
  begin
    NewElement := FNewElements[index];
    NewElement.DrawEdges(Buffer, AZoomBox, index = FNewElements.Count - 1);
  end;
//  FFishnetGenerator.DrawTop(Buffer, ZoomBox);
end;

procedure TDrawElementTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  SelectionWidth = 5;
var
  AZoomBox: TQRbwZoomBox2;
  APoint: TPoint2D;
  NewNode: TNewNode;
  NodeX: Integer;
  NodeY: Integer;
  Node2D: TSutraNode2D;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  AZoomBox := ZoomBox;
  APoint.X := AZoomBox.X(X);
  APoint.Y := AZoomBox.Y(Y);
  NewNode := nil;
  if FNewNodeQuadTree.Count > 0 then
  begin
    NewNode := FNewNodeQuadTree.NearestPointsFirstData(APoint.X, APoint.Y);
    NodeX := AZoomBox.XCoord(NewNode.FLocation.x);
    NodeY := AZoomBox.YCoord(NewNode.FLocation.y);
    if (Abs(NodeX-X) > SelectionWidth) or (Abs(NodeY-Y) > SelectionWidth) then
    begin
      NewNode := nil;
    end;
  end;
  if NewNode = nil then
  begin
    if FNodeQuadTree.Count > 0 then
    begin
      Node2D := FNodeQuadTree.NearestPointsFirstData(APoint.X, APoint.Y);
      NodeX := AZoomBox.XCoord(Node2D.x);
      NodeY := AZoomBox.YCoord(Node2D.y);
      if (Abs(NodeX-X) <= SelectionWidth) and (Abs(NodeY-Y) <= SelectionWidth) then
      begin
        NewNode := TNewNode.Create;
        FNewNodes.Add(NewNode);
        NewNode.FNode := Node2D;
        NewNode.FLocation := Node2D.Location;
        FNewNodeQuadTree.AddPoint(NewNode.FLocation.x,
          NewNode.FLocation.y, NewNode);
      end;
    end;
  end;
  if NewNode = nil then
  begin
    NewNode := TNewNode.Create;
    FNewNodes.Add(NewNode);
    NewNode.FNode := nil;
    NewNode.FLocation := APoint;
    FNewNodeQuadTree.AddPoint(NewNode.FLocation.x,
      NewNode.FLocation.y, NewNode);
  end;

  if FCurrentElement = nil then
  begin
    FCurrentElement := TNewElement.Create;
    FNewElements.Add(FCurrentElement);
  end;

  FCurrentElement.FNodes.Add(NewNode);
  NewNode.FNewElements.Add(FCurrentElement);

  if FCurrentElement.FNodes.Count = 4 then
  begin
    FCurrentElement := nil;
  end;
  AZoomBox.InvalidateImage32;
end;

{ TNewNode }

constructor TNewNode.Create;
begin
  FNewElements := TNewElementList.Create;
end;

destructor TNewNode.Destroy;
begin
  FNewElements.Free;
  inherited;
end;

function TCustomCrossSectionTool.GetMeshOrDisvAvailable: boolean;
var
  MeshAvailable: boolean;
  DisvGridAvailable: boolean;
begin
  MeshAvailable := (frmGoPhast.PhastModel.ModelSelection in SutraSelection) and
    (frmGoPhast.PhastModel.SutraMesh <> nil);
  DisvGridAvailable := frmGoPhast.DisvUsed and
    (frmGoPhast.PhastModel.DisvGrid <> nil);
  result := MeshAvailable or DisvGridAvailable;
end;

{ TAddPilotPoint }

procedure TAddPilotPoint.Activate;
begin
  inherited;
  Cursor := crAddPilotPoint;
//  FillQuadTree;
end;

procedure TAddPilotPoint.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
  AZoomBox: TQRbwZoomBox2;
  NewPoints: TSimplePointCollection;
  LastPoint: TPointItem;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  AZoomBox := ZoomBox;
  if AZoomBox <> frmGoPhast.frameTopView.ZoomBox then
  begin
    Exit;
  end;
  APoint.X := AZoomBox.X(X);
  APoint.Y := AZoomBox.Y(Y);

  if FNewPoints.Count > 0 then
  begin
    LastPoint := FNewPoints.Last;
    if (LastPoint.Point2D.x = APoint.x) and (LastPoint.Point2D.y = APoint.y) then
    begin
      Exit;
    end;
    LastPoint := FQuadTree.NearestPointsFirstData(APoint.x, APoint.y);
    if (LastPoint.Point2D.x = APoint.x) and (LastPoint.Point2D.y = APoint.y) then
    begin
      Exit;
    end;
  end;

  NewPoints.Add.Point2D := APoint;
  frmGoPhast.UndoStack.Submit(TUndoAddPilotPoint.Create(NewPoints));

  AZoomBox.InvalidateImage32;
end;

{ TDeletePilotPoint }

procedure TDeletePilotPoint.Activate;
begin
  inherited;
  Cursor := crDeletePilotPoint;
  CreateLayers;
//  FillQuadTree;
end;

constructor TDeletePilotPoint.Create(AOwner: TComponent);
begin
  inherited;
  FNewBetweeenObsPoints := TSimplePointCollection.Create;
end;

procedure TDeletePilotPoint.CreateLayers;
  function CreateLayer(ZoomBox: TQRbwZoomBox2): TPositionedLayer;
  begin
    result := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
      TPositionedLayer;
    result.OnPaint := DrawOnBitMap32;
  end;
begin
  if FTopLayer = nil then
  begin
    FTopLayer := CreateLayer(frmGoPhast.frameTopView.ZoomBox);
  end;
end;

destructor TDeletePilotPoint.Destroy;
begin
  FNewBetweeenObsPoints.Free;
  inherited;
end;

procedure TDeletePilotPoint.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if FButton = mbMiddle then
  begin
    Exit;
  end;
  if frmGoPhast.CurrentTool <> self then Exit;
  if FSelectLine <> nil then
  begin
    Buffer.BeginUpdate;
    try
      FSelectLine.Draw(Buffer);
    finally
      Buffer.EndUpdate
    end;
  end;
end;

procedure TDeletePilotPoint.FillQuadTree;
var
  index: Integer;
  PointItem: TPointItem;
begin
  inherited;
  FNewBetweeenObsPoints.Assign(
    frmGoPhast.PhastModel.PestProperties.BetweenObservationsPilotPoints);
  for index := 0 to FNewBetweeenObsPoints.Count - 1 do
  begin
    PointItem := FNewBetweeenObsPoints[index];
    FQuadTree.AddPoint(PointItem.Point2D.x, PointItem.Point2D.y, PointItem);
  end;
end;

procedure TDeletePilotPoint.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FX := X;
  FY := Y;

  if Button <> mbLeft then
  begin
    Exit;
  end;
  FSelectLine.Free;
  FSelectLine := TLine.Create(2);
  FSelectLine.AddPoint(Point(X, Y));
  ZoomBox.InvalidateImage32;

end;

procedure TDeletePilotPoint.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  APoint: TPoint;
begin
  if not (ssMiddle in Shift) then
  begin
    if (FSelectLine <> nil) then
    begin
      // set the correct cursor
//      Cursor := crArrow;
      // If the cursor has moved far enough, add another point to the
      // lasso.
      FSelectLine.Clear;
      APoint := Point(FX,FY);
      FSelectLine.AddPoint(APoint);
      FSelectLine.AddPoint(Point(APoint.X, Y));
      FSelectLine.AddPoint(Point(X, Y));
      FSelectLine.AddPoint(Point(X, APoint.Y));
      FSelectLine.AddPoint(APoint);

      ZoomBox.InvalidateImage32;
    end;
  end;
end;

procedure TDeletePilotPoint.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AZoomBox: TQRbwZoomBox2;
//  NewPoints: TSimplePointCollection;
//  QuadTree: TRbwQuadTree;
  PointItem: TPointItem;
  Block: T2DBlock;
  Points: TQuadPointInRegionArray;
  PointIndex: Integer;
  ItemIndex: Integer;
  XDouble: double;
  YDouble: double;
  DataPointer: Pointer;
begin
  inherited;
  if Button <> mbLeft then
  begin
    Exit;
  end;
  FreeAndNil(FSelectLine);
  AZoomBox := ZoomBox;
  if AZoomBox <> frmGoPhast.frameTopView.ZoomBox then
  begin
    Exit;
  end;

//  FNewPoints := TSimplePointCollection.Create;
//  FQuadTree := TRbwQuadTree.Create(nil);
//  try
//    FillQuadTree;

  if (Abs(X - FX) > 3) or (Abs(Y - FY) > 3) then
  begin
    Block.XMax := Max(AZoomBox.X(X), AZoomBox.X(FX));
    Block.XMin := Min(AZoomBox.X(X), AZoomBox.X(FX));
    Block.YMax := Max(AZoomBox.Y(Y), AZoomBox.Y(FY));
    Block.YMin := Min(AZoomBox.Y(Y), AZoomBox.Y(FY));
    FQuadTree.FindPointsInBlock(Block, Points);
    if Length(Points) = 0 then
    begin
      Exit;
    end;
    for PointIndex := 0 to Length(Points) - 1 do
    begin
      for ItemIndex := 0 to Length(Points[PointIndex].Data) - 1 do
      begin
        PointItem := Points[PointIndex].Data[ItemIndex];
        FQuadTree.RemovePoint(PointItem.Point2D.X, PointItem.Point2D.Y, PointItem);
        PointItem.Free;
      end;
    end;
  end
  else
  begin
    XDouble := AZoomBox.X(X);
    YDouble := AZoomBox.Y(Y);
    FQuadTree.FirstNearestPoint(XDouble, YDouble, DataPointer);
    if (Abs(X-AZoomBox.XCoord(XDouble)) > 3)
      or (Abs(Y-AZoomBox.YCoord(YDouble)) > 3) then
    begin
      Exit;
    end;
    PointItem := DataPointer;
    FQuadTree.RemovePoint(PointItem.Point2D.X, PointItem.Point2D.Y, PointItem);
    PointItem.Free;
  end;

  frmGoPhast.UndoStack.Submit(
    TUndoDeletePilotPoint.Create(FNewPoints, FNewBetweeenObsPoints));


    //    FQuadTree.Free;
//    FNewPoints.Free;
    AZoomBox.InvalidateImage32;
end;

{ TCustomModifyPilotPointTool }

procedure TCustomModifyPilotPointTool.Activate;
begin
  inherited;
  FillQuadTree;
end;

constructor TCustomModifyPilotPointTool.Create(AOwner: TComponent);
begin
  inherited;
  FNewPoints := TSimplePointCollection.Create;
//  FNewBetweenObsPoints := TSimplePointCollection.Create;
  FQuadTree := TRbwQuadTree.Create(nil);
end;

destructor TCustomModifyPilotPointTool.Destroy;
begin
  FQuadTree.Free;
  FNewPoints.Free;
  inherited;
end;

procedure TCustomModifyPilotPointTool.FillQuadTree;
var
  DisLimits: TGridLimit;
  index: Integer;
  AZoomBox: TQRbwZoomBox2;
  PointItem: TPointItem;
begin
  AZoomBox := ZoomBox;
  DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
  FQuadTree.Clear;
  FQuadTree.XMax := AZoomBox.XCoord(DisLimits.MaxX);
  FQuadTree.XMin := AZoomBox.XCoord(DisLimits.MinX);
  FQuadTree.YMax := AZoomBox.YCoord(DisLimits.MaxY);
  FQuadTree.YMin := AZoomBox.YCoord(DisLimits.MinY);
  FNewPoints.Assign(frmGoPhast.PhastModel.PestProperties.SpecifiedPilotPoints);
  for index := 0 to FNewPoints.Count - 1 do
  begin
    PointItem := FNewPoints[index];
    FQuadTree.AddPoint(PointItem.Point2D.x, PointItem.Point2D.y, PointItem);
  end;
end;

initialization
  ZoomTool := TZoomTool.Create(nil);
  ZoomInTool := TZoomInTool.Create(nil);
  ZoomOutTool := TZoomOutTool.Create(nil);
  PanTool := TPanTool.Create(nil);
  AddGridBoundaryTool := TAddGridBoundaryTool.Create(nil);
  MovingGridBoundaryTool := TMovingGridBoundaryTool.Create(nil);
  DeleteGridBoundaryTool := TDeleteGridBoundaryTool.Create(nil);
  RotateGridTool := TRotateGridTool.Create(nil);
  LassoTool := TLassoTool.Create(nil);
  CreatePointScreenObjectTool := TCreatePointScreenObjectTool.Create(nil);
  CreateLineScreenObjectTool := TCreateLineScreenObjectTool.Create(nil);
  CreateStraightLineScreenObjectTool :=
    TCreateStraightLineScreenObjectTool.Create(nil);
  CreateRectangleScreenObjectTool :=
    TCreateRectangleScreenObjectTool.Create(nil);
  DeleteSegmentTool := TDeleteSegmentTool.Create(nil);
  InsertPointTool := TInsertPointTool.Create(nil);
  SelectPointTool := TSelectPointTool.Create(nil);
  SelectScreenObjectTool := TSelectScreenObjectTool.Create(nil);
  ColRowLayerSelectorTool:= TColRowLayerSelectorTool.Create(nil);
  AddLinePartTool:= TAddLinePartTool.Create(nil);
  AddPolygonPartTool:= TAddPolygonPartTool.Create(nil);
  AddPointPartTool := TAddPointPartTool.Create(nil);
  EditVertexValueTool := TEditVertexValueTool.Create(nil);
  RulerTool := TRulerTool.Create(nil);
  EditCrossSectionTool:= TEditCrossSectionTool.Create(nil);
  RotateCrossSectionTool:= TRotateCrossSectionTool.Create(nil);
  MoveSutraNodesTool := TMoveSutraNodesTool.Create(nil);
  FishnetTool := TFishnetMeshGenerationTool.Create(nil);
  DrawElementTool := TDrawElementTool.Create(nil);
  AddPilotPointTool := TAddPilotPoint.Create(nil);
  DeletePilotPointTool := TDeletePilotPoint.Create(nil);

finalization
  ZoomTool.Free;
  ZoomInTool.Free;
  ZoomOutTool.Free;
  PanTool.Free;
  AddGridBoundaryTool.Free;
  MovingGridBoundaryTool.Free;
  DeleteGridBoundaryTool.Free;
  RotateGridTool.Free;
  LassoTool.Free;

  CreatePointScreenObjectTool.Free;
  CreateLineScreenObjectTool.Free;
  CreateStraightLineScreenObjectTool.Free;
  CreateRectangleScreenObjectTool.Free;
  DeleteSegmentTool.Free;
  InsertPointTool.Free;
  SelectPointTool.Free;
  SelectScreenObjectTool.Free;
  ColRowLayerSelectorTool.Free;
  AddLinePartTool.Free;
  AddPolygonPartTool.Free;
  AddPointPartTool.Free;
  EditVertexValueTool.Free;
  RulerTool.Free;
  EditCrossSectionTool.Free;
  RotateCrossSectionTool.Free;
  MoveSutraNodesTool.Free;
  FishnetTool.Free;
  DrawElementTool.Free;
  AddPilotPointTool.Free;
  DeletePilotPointTool.Free;
end.
