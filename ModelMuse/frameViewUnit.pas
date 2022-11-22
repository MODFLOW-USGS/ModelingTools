{@abstract(@name is used to define @link(TframeView) which
  is used to display one view of the model and to
  handle user-interaction with the @link(TPhastGrid) and
  @link(TScreenObject)s. Much of the user interaction is delegated to
  descendants of @link(TCustomInteractiveTool).  @link(TCustomInteractiveTool)
  is also defined in @name.)

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frameViewUnit;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, RbwRuler, StdCtrls, SyncObjs,
  AbstractGridUnit,
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  ScreenObjectUnit, SelectUnit, UndoItems, Menus, RbwModelCube,
  DataSetUnit, FastGEO, GoPhastTypes, ZoomBox2, PhastDataSets,
  CompressedImageUnit, ColRowLayerChangeUnit;

type

  TCustomInteractiveTool = class;

  { TODO : Define a graphical method of specifying control
  points on a TScreenObject.  Use the control points to specify river
  properties. }

  { TODO : Allow the user to specify custom colors for coloring the grid. }

  {@abstract(@name is used to display one view of the model and to
    handle user-interaction with the @link(TPhastGrid) and
    @link(TScreenObject)s. Much of the user interaction is delegated to
    descendants of @link(TCustomInteractiveTool).)}
  TframeView = class(TFrame)
    // See @link(BackOneClick).
    BackOne: TMenuItem;
    // See @link(ForwardOneClick).
    ForwardOne: TMenuItem;
    // See @link(miHideClick).
    miHide: TMenuItem;
    // @name allows the user to change the selected column, row, or layer
    // by clicking on the model cube.
    // See TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.MouseDown),
    // TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.MouseMove),
    // TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.MouseUp), @link(ModelCubePaint),
    // TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.ChangeColRowLayer), and
    // @link(ColRowLayerSelector).
    ModelCube: TRbwModelCube;
    // @name is a pop-up menu that holds the TMenuItems declared as part of
    // @classname.
    // See @link(ToFront), @link(ToBack), @link(ForwardOne), @link(BackOne),
    // and @link(miHide).  Its OnPopup event is handled by @link(OrderMenuPopup).
    OrderMenu: TPopupMenu;
    pnlTop: TPanel;
    // @name is the horizontal ruler.
    // See @link(rulerDblClick) and @link(rulerMouseMove).
    rulHorizontal: TRbwRuler;
    // @name is the vertical ruler.
    // See @link(rulerDblClick) and @link(rulerMouseMove).
    rulVertical: TRbwRuler;
    // See @link(ToBackClick).
    ToBack: TMenuItem;
    // See @link(ToFrontClick).
    ToFront: TMenuItem;
    // @name is responsible for displaying one view of the model.
    // @seealso(ZoomBoxExit),
    // @seealso(ZoomBoxPan)
    // @seealso(ZoomBoxResize)
    // @seealso(ZoomBoxImage32DblClick)
    // @seealso(ZoomBoxImage32MouseDown)
    // @seealso(ZoomBoxImage32MouseMove)
    // @seealso(ZoomBoxImage32MouseUp)
    // @seealso(Paint)
    ZoomBox: TQRbwZoomBox2;
    miSelectAll: TMenuItem;
    HideAllOthers: TMenuItem;
    ShowAll1: TMenuItem;
    miMergeObjects: TMenuItem;
    miEditSelectedObjects: TMenuItem;
    miInvertSelectedVertices: TMenuItem;
    miLockSelectedObjects: TMenuItem;
    miUnlockSelectedObjects: TMenuItem;
    miUnselectAll: TMenuItem;
    // @name allows the user to move selected objects back one
    // in the list of objects so that they move behind one other object.
    procedure BackOneClick(Sender: TObject);
    // @name allows the user to move selected objects forward one
    // in the list of objects so that they move forward of one other object.
    procedure ForwardOneClick(Sender: TObject);
    // @name hides the selected @link(TScreenObject)s.
    procedure miHideClick(Sender: TObject);
    // @name draws a black rectangle outlining the @link(ModelCube) component.
    procedure ModelCubePaint(Sender: TObject);
    // @name enables or disables @link(ToFront), @link(ToBack),
    // @link(ForwardOne), @link(BackOne), and @link(miHide) depending on the
    // context.
    procedure OrderMenuPopup(Sender: TObject);
    // @name is OnDblClick event handler for @link(rulHorizontal), and
    // @link(rulVertical).  It shows the @link(TfrmRulerOptions) form.
    procedure rulerDblClick(Sender: TObject);
    // @name is OnMouseMove event handler for @link(rulHorizontal), and
    // @link(rulVertical).
    // @name sets TfrmGoPhast.@link(TfrmGoPhast.CursorGrid) to cgNone;
    procedure rulerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // @name allows the user to move the selected @link(TScreenObject)s behind
    // all other @link(TScreenObject)s.
    procedure ToBackClick(Sender: TObject);
    // @name allows the user to move the selected @link(TScreenObject)s
    // in front of all other @link(TScreenObject)s.
    procedure ToFrontClick(Sender: TObject);
    // If the user clicks on a button or somewhere else while creating a
    // @link(TScreenObject), the @link(TScreenObject) is terminated.
    // @name is the @link(ZoomBox).OnExit event handler.
    procedure ZoomBoxExit(Sender: TObject);
    // @name is the event handler for @link(ZoomBox).OnPan.
    // It updates @link(rulHorizontal) and @link(rulVertical) with
    // its current position.
    procedure ZoomBoxPan(Sender: TObject; DeltaX, DeltaY: real);
    // @name is the event handler for @link(ZoomBox).OnResize.
    // @name sets frmGoPhast.@link(TfrmGoPhast.timTimer).OnTimer to
    // frmGoPhast.@link(TfrmGoPhast.ResizeZoomBoxes) with a delay of 100 ms.
    // The effect is to prevent the control from redrawing until
    // the user has finished resizing it.
    procedure ZoomBoxResize(Sender: TObject);
    // @name is the event handler for @link(ZoomBox).Image32.OnMouseDown
    // If the right mouse button is depressed, it exits and allows
    // @link(OrderMenu) to take over.
    // Otherwise it updates frmGoPhast.@link(TfrmGoPhast.CursorGrid),
    // frmGoPhast.@link(TfrmGoPhast.CursorX), and
    // frmGoPhast.@link(TfrmGoPhast.CursorY).
    // Then it delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.MouseDown).
    procedure ZoomBoxImage32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    // @name is the event handler for @link(ZoomBox).Image32.OnMouseMove
    // Otherwise it updates frmGoPhast.@link(TfrmGoPhast.CursorGrid),
    // frmGoPhast.@link(TfrmGoPhast.CursorX), and
    // frmGoPhast.@link(TfrmGoPhast.CursorY).
    // It then calls @link(UpdateStatusBar).
    // Then it delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.MouseMove).
    procedure ZoomBoxImage32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    // @name is the event handler for @link(ZoomBox).Image32.OnMouseUp.
    // If the right mouse button is depressed, it exits and allows
    // @link(OrderMenu) to take over.
    // Otherwise it updates frmGoPhast.@link(TfrmGoPhast.CursorGrid),
    // frmGoPhast.@link(TfrmGoPhast.CursorX), and
    // frmGoPhast.@link(TfrmGoPhast.CursorY).
    // Then it delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.MouseUp).
    procedure ZoomBoxImage32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    // @name is the @link(ZoomBox).Image32.OnDblClick event handler
    // It delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.DoubleClick).
    procedure ZoomBoxImage32DblClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    // @name hides the @link(TScreenObject)s that are not selected.
    procedure HideAllOthersClick(Sender: TObject);
    procedure ShowAll1Click(Sender: TObject);
    procedure miMergeObjectsClick(Sender: TObject);
    procedure ZoomBoxMagnificationChanged(Sender: TObject);
    procedure ModelCubeMouseEnter(Sender: TObject);
    procedure ModelCubeMouseLeave(Sender: TObject);
    procedure miInvertSelectedVerticesClick(Sender: TObject);
    procedure miLockSelectedObjectsClick(Sender: TObject);
    procedure miUnlockSelectedObjectsClick(Sender: TObject);
    procedure miUnselectAllClick(Sender: TObject);
  private
    MouseStartX: integer;
    MouseStartY: integer;
    // @name is the TBitmap32 on which the @link(TfrmGoPhast.Grid) and
    // @link(TScreenObject)s are drawn.
    FBitMap32: TBitmap32;
{$IFDEF ExtraDrawingNeeded}
// It isn't clear that the code in which FPaintingLayer and FNeedToRedraw
// are used is actually required.  Until it is clear, I am enclosing the code
// in IFDEFs.
//
// If ExtraDrawingNeeded is not defined, sometimes screen objects
// are not redisplayed when creating a screen object has been cancelled.

// It looks like these variables only needs to be used if
// Application.ProcessMessages is called by
// PaintLayer or something called by PaintLayer.
    FPaintingLayer: Boolean;
    FNeedToRedraw: Boolean;
{$ENDIF}
    FPreviousMagnification: double;
    FPaintingNeeded: Boolean;
    procedure UpdateStatusBarCoordinates(APoint: TPoint2D);
    procedure UpdateStatusBarForTopBlockDataSet(Column, Row, X, Y: Integer; const Location: TPoint2D);
    procedure UpdateStatusBarForTopNodeDataSet(Column:
      Integer; Row: Integer; const Location: TPoint2D);
    procedure UpdateStatusBarFrontBlockDataSet(Column, Layer: Integer;
      const Location: TPoint2D);
    procedure UpdateStatusBarFrontNodeDataSet(Column: Integer; Layer: Integer;
      const Location: TPoint2D);
    procedure UpdateStatusBarSideBlockDataSet(Row, Layer: Integer;
      const Location: TPoint2D);
    procedure UpdateStatusBarSideNodeDataSet(Row, Layer: Integer; const Location: TPoint2D);
    procedure ShowCurrentValue(DataSet: TDataArray; const NameToDisplay: string;
      Column, Row, Layer, GlobalColumn, GlobalRow, GlobalLayer: Integer;
      const Location: TPoint2D);
    procedure HideScreenObjects(HideSelected: Boolean);
    procedure AllowBitmapsToBeDisplayed;
    function GetTopDisplayDataSet: TDataArray;
  public
    FHasDrawn: boolean;
    FPositionedLayer: TPositionedLayer;
    procedure SelectAll;
    property PaintingNeeded: boolean read FPaintingNeeded;
  private
    // @name: boolean;
    // @name is used inside @link(ZoomBoxImage32MouseUp) to prevent it
    // from being called again until it has finished whatever it has to do.
    FBusy: boolean;
    // @name: @link(TCustomLayerRowColumnSelector);
    // See @link(ColRowLayerSelector).
    FColRowLayerSelector: TCustomLayerRowColumnSelector;
    // @name: real;
    // See @link(DeltaGridAngle).
    FDeltaGridAngle: real;
    // @name: boolean;
    // @name is used inside @link(Paint) to prevent it
    // from being called again until it has finished whatever it has to do.
    FDrawing: boolean;
    // @name: boolean;
    // See @link(MagnificationChanged).
    FMagnificationChanged: boolean;
    // @name: TList;
    // See @link(PreviousScreenObjects).
    FPreviousScreenObjects: TList;
    // @name: boolean;
    // See @link(IsResizing).
    FResizing: boolean;
    // @name: @link(TViewDirection);
    // See @link(ViewDirection).
    FViewDirection: TViewDirection;
    FModelChanged: boolean;
    FPilotPointsChanged: Boolean;
    // @name is used to redraw the horizontal scale.
    // @param(NewX NewX indicates how far the image in @link(ZoomBox)
    // has moved in the X direction in pixels.)
    procedure AdjustHorizontalScale(const NewX: integer);
    // @name is used to redraw the vertical scale.
    // @param(NewY NewY indicates how far the image in @link(ZoomBox)
    // has moved in the Y direction in pixels.)
    procedure AdjustVerticalScale(const NewY: integer);
    // @name returns the @link(TDataArray) the is used to color the grid
    // in this view of the model.
    function ColorDataSet: TDataArray;
    // If @link(ColorDataSet) is a transient @link(TDataArray), @name is
    // the @link(TPhastTimeList) that owns it.
    function ColorTimeList: TCustomTimeList;
    // @name is the main routine for drawing the
    // @link(TfrmGoPhast.Grid) and @link(TScreenObject)s.
    // It will also draw any imported images.
    procedure DrawGridAndScreenObjects;
    // @name will draw an imported image (Source) on a bitmap (Dest)
    // at its proper location.
    procedure DrawImage(const Source: TCompressedBitmapItem);
    // @name will draw the @link(TScreenObject)s on @link(FBitMap32).
    procedure DrawScreenObjects;
    // @name gets the column and layer numbers of the element containing APoint;
    // APoint must be on a front view of the model.
    procedure GetColLayer(APoint: TPoint2D; out Col, Layer: integer);
    // See @link(ColRowLayerSelector).
    function GetColRowLayerSelector: TCustomLayerRowColumnSelector;
    // See @link(CurrentScreenObject).
    function GetCurrentScreenObject: TScreenObject;
    // See @link(GridChanged).
    function GetGridChanged: boolean;
    // See @link(NeedToRecalculateCellColors).
    function GetNeedToRecalculateCellColors: boolean;
    // @name returns the column and layer numbers of the cell containing APoint;
    // APoint must be on a side view of the model.
    // The frmGoPhast.PhastGrid must be in use.
    procedure GetNodeColLayer(APoint: TPoint2D; out Col,
      Layer: integer);
    // @name returns the column and row numbers of the cell containing APoint;
    // APoint must be on a top view of the model.
    // APoint must be in real-world coordinates not grid coordinates.
    procedure GetNodeRowCol(APoint: TPoint2D; out Row,
      Column: integer);
    // @name returns the row and layer numbers of the cell containing APoint;
    // APoint must be on a front view of the model.
    // The frmGoPhast.PhastGrid must be in use.
    procedure GetNodeRowLayer(APoint: TPoint2D; out Row,
      Layer: integer);
    // @name gets the column and row numbers of the element containing APoint;
    // APoint must be on a top view of the model.
    // APoint must be in real-world coordinates not grid coordinates.
    procedure GetRowCol(APoint: TPoint2D; out Row, Column: integer);
    // @name gets the row and layer numbers of the element containing APoint;
    // APoint must be on a side view of the model.
    procedure GetRowLayer(APoint: TPoint2D; out Row, Layer: integer);
    // See @link(ScreenObjectsHaveChanged).
    function GetScreenObjectsChanged: boolean;
    // @name initializes the data set used to color the grid on this
    // view of the model.
    procedure InitializeDataSet;
    // @name checks if the data set used to color the grid on this view of the
    // model is up to date.  If not, it initializes it.
    // Once it is up to data, @name will store the correct color for each cell
    // or element in the grid.
    function RecalculateCellColors: boolean;
    // @name sets to the colors of each cell or element in the grid to White.
    procedure ResetCellColors;
    // @name sets PointArray to the positions of the corner of the grid
    // after having been rotated by Angle.
    procedure RotatedGridCorners(const Angle: real;
      out PointArray: array of TPoint);
    // See @link(DeltaGridAngle).
    procedure SetDeltaGridAngle(const Value: real);
    // See @link(GridChanged).
    procedure SetGridChanged(const Value: boolean);
    // See @link(NeedToRecalculateCellColors).
    procedure SetNeedToRecalculateCellColors(const Value: boolean);
    // See @link(ScreenObjectsHaveChanged).
    procedure SetScreenObjectsChanged(const Value: boolean);
    // @name changes ColRowLayerSelector based on @link(ViewDirection).
    procedure SetUpColRowLayerSelector;
    // See @link(ViewDirection).
    procedure SetViewDirection(const Value: TViewDirection);
    // @name shows appropriate text on the status bar.
    procedure UpdateStatusBar(const X, Y: integer);
    // When the data set used to color the grid
    // is a transient @link(TDataArray),
    // @name is used to make sure that an up to date version of the
    // @link(TDataArray) is used.
    procedure UpdateTimeDataSet;
    // If the magnification is too big, zoom out a bit and warn the user.
    procedure WarnTooBig;
    procedure ZoomBoxHitTest(Sender: TObject; X, Y: Integer;
      var Passed: Boolean);
    // Paint is called from PaintLayer.
    procedure Paint(Sender: TObject);
    // @name draws the current @link(TScreenObject) and previous
    // @link(TScreenObject) to the @link(TQRbwZoomBox2.Image32) bitmap
    procedure PaintOldScreenObjects(const BitMap: TBitmap32);
    procedure DrawSelectedAndOldScreenObjects(const BitMap: TBitmap32);
    procedure PaintSelectedScreenObjects(Sender: TObject; Buffer: TBitmap32);
    function ConvertTop2D_Point(const APoint: TPoint2D): TPoint;
    function ConvertFront2D_Point(const APoint: TPoint2D): TPoint;
    function ConvertSide2D_Point(const APoint: TPoint2D): TPoint;
    procedure DrawPathLines;
    procedure DrawEndPoints;
    procedure DrawTimeSeries;
    procedure UpdateStatusBarTopElementNode(const APoint: TPoint2D);
    procedure GetMeshColLayer(APoint: TPoint2D; out NodeCol, NodeLayer,
      ElCol, ElLayer: integer);
    procedure DrawVectors;
    procedure DrawCrossSection;
    procedure UnSelectAll;
    { Private declarations }
  protected
    // @name is used to indicate that a change has been made to the grid
    // so that the view of the model needs to be redrawn.
    property GridChanged: boolean read GetGridChanged write SetGridChanged;
    // @name is used to respond to the OnMouseDown, OnMouseMove, and
    // OnMouseUp events of @link(ModelCube).  It allows the user to change
    // the selected column, row, or layer to be changed by clicking on
    // @link(ModelCube).
    property ColRowLayerSelector: TCustomLayerRowColumnSelector
      read GetColRowLayerSelector;
  public

    // @name is the bottom right corner of the rectangle outlining the
    // selected @link(TScreenObject)s.
    FSelectBottomRight: TPoint;
    // @name is the top left corner of the rectangle outlining the
    // selected @link(TScreenObject)s.
    FSelectTopLeft: TPoint;
    property Drawing: boolean read FDrawing;
    property ModelChanged: boolean read FModelChanged write FModelChanged;
    // @name calls TScreenObject.@link(TScreenObject.InvalidateCoordinates)
    // for every @link(TScreenObject) on this view of the model.
    procedure InvalidateScreenObjectCoordinates;
    // @name adjusts the scales on @link(rulHorizontal) and @link(rulVertical)
    // to match the coordinates in @link(ZoomBox).
    procedure AdjustScales;
    // @name creates an instance of @classname.
    constructor Create(AOwner: TComponent); override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    // @name is the @link(TScreenObject) that is being created.
    property CurrentScreenObject: TScreenObject read GetCurrentScreenObject;
    // @name is used to indicate which view of the model
    // (if any) the cursor is over.
    function CursorGrid: TCursorGrid;
    // When rotating the grid with @link(TRotateGridTool), @name is
    // used to store the amount by which the grid is being rotated.
    property DeltaGridAngle: real read FDeltaGridAngle write
      SetDeltaGridAngle;
    // @name destroys the current instance of @classname.
    // Do not call @classname directly.  Call Free instead.
    destructor Destroy; override;
    // @name displays the column, row, or layer that is selected
    // on the status bar.
    procedure DisplayItem;
    // @name draws an outline of the grid after it is rotated by DeltaAngle.
    procedure DrawRotatedGrid(const DeltaAngle: real; BitMap: TBitmap32);
    // See TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.EvaluatedAt).
    function EvaluatedAt: TEvaluatedAt;
    // See TCustomCreateScreenObjectTool.@link(
    // TCustomCreateScreenObjectTool.FinishScreenObjects);
    procedure FinishScreenObjects;
    // See TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.ItemChange);
    procedure ItemChange(Sender: TObject);
    // When zooming in or out, @name is set to true to indicate that
    // redrawing of the model must occur.
    property MagnificationChanged: boolean read FMagnificationChanged
      write FMagnificationChanged;
    // When the @link(TDataArray) used to color the grid is changed
    // @name is set to true to indicate that the cell colors need to be
    // updated.
    property NeedToRecalculateCellColors: boolean
      read GetNeedToRecalculateCellColors
      write SetNeedToRecalculateCellColors;
    property PilotPointsChanged: Boolean read FPilotPointsChanged
      write FPilotPointsChanged;
    // @name is used in @link(PaintOldScreenObjects) to draw
    // @link(TScreenObject)s with relatively little flickering.
    property PreviousScreenObjects: TList read FPreviousScreenObjects;
    // @name is used in @link(ZoomBoxResize) and
    // @link(TfrmGoPhast.ResizeZoomBoxes) to introduce a small delay
    // before the Image32 redraws.
    property IsResizing: boolean read FResizing write FResizing;
    // @name is used to indicate that there has been a change to the
    // @link(TScreenObject)s so the view of the model needs to be
    // redrawn.
    property ScreenObjectsHaveChanged: boolean read GetScreenObjectsChanged
      write SetScreenObjectsChanged;
    // @name shows the magnification on the status bar.
    procedure ShowMagnification;
    // @name calculates the coordinates of a rectangle that surrounds
    // the selected objects.
    procedure UpdateSelectRectangle;
    // @name indicates the direction from which the model is viewed in
    // this @classname.
    property ViewDirection: TViewDirection read FViewDirection
      write SetViewDirection;
    function ConvertPoint(const APoint: TPoint2D): TPoint;
    // @name returns a TPoint2D for the center of the grid.
    function GridCenter: TPoint2D;
    { Public declarations }
  end;

  {@abstract(@name is an abstract base class.  Its descendants (mainly in
    @link(InteractiveTools)) handle
    on-screen interaction between the user and the grid or
    @link(TScreenObject)s.)
    The main procedures in which the interaction is handled are
    @link(MouseDown), @link(MouseMove) and @link(MouseUp).
    These are called in TframeView.@link(TframeView.ZoomBoxImage32MouseDown),
    TframeView.@link(TframeView.ZoomBoxImage32MouseMove),
    and TframeView.@link(TframeView.ZoomBoxImage32MouseUp).

    The hints in TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    TfrmGoPhast.@link(TfrmGoPhast.frameSideView) are set by the current tool
    via @link(GetHint), @link(Activate), and @link(Deactivate).

    @SeeAlso(TfrmGoPhast.CurrentTool)
    }
  TCustomInteractiveTool = class(TComponent)
  private
    // See @link(Cursor).
    FCursor: TCursor;
    // See @link(ViewDirection);
    FViewDirection: TViewDirection;
    FStoredCursor: TCursor;
    // See @link(ViewDirection).
    procedure SetViewDirection(const Value: TViewDirection);
    // Getter for @link(MouseIsDown)
    function GetMouseIsDown: boolean;

  protected
    FTopLayer: TPositionedLayer;
    FSideLayer: TPositionedLayer;
    FFrontLayer: TPositionedLayer;
    FButton: TMouseButton;
    procedure CreateLayers; virtual;
    function Layer32: TPositionedLayer;
    // See @link(Cursor).
    function GetCursor: TCursor; virtual;
    // @name determines the column and layer of the cell in the grid
    // containing APoint.
    procedure GetColLayer(APoint: TPoint2D; out Col, Layer: integer);
    // See @link(Hint)
    function GetHint: string; virtual;
    // @name determines the row and column of the cell in the grid
    // containing APoint.
    procedure GetRowCol(APoint: TPoint2D; out Row, Column: integer);
    // @name determines the row and layer of the cell in the grid
    // containing APoint.
    procedure GetRowLayer(APoint: TPoint2D; out Row, Layer: integer);
    // See @link(Cursor).
    procedure SetCursor(const Value: TCursor); virtual;
    // @name sets the cursors of
    // TQRbwZoomBox2.Image32
    // and TframeView.@link(TframeView.ZoomBox).
    procedure UpdateCursors; virtual;
    // @name is the @link(TframeView) to which this @classname applies.
    function View: TframeView; overload;
    function View(Direction: TViewDirection): TframeView; overload;
    // @name is the TQRbwZoomBox2 of @link(View).
    function ZoomBox: TQRbwZoomBox2;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); virtual;
    function GetEvaluatedAt(const Direction: TViewDirection): TEvaluatedAt;
    // @name returns the index of the first vertex in AScreenObject
    // that is within @link(SelectionWidth) of X,Y.
    function FindNodeInSelectedScreenObjects(const X, Y: integer;
      const AScreenObject: TScreenObject): integer;
  public
    procedure UpdateAllViews;
    // @name is called when a @classname is made the
    // @link(TfrmGoPhast.CurrentTool).
    // @name sets the Hint of TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    // TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    // TfrmGoPhast.@link(TfrmGoPhast.frameSideView).
    procedure Activate; virtual;
    // @name is called when another @classname replaces the current instance of
    // @classname as the @link(TfrmGoPhast.CurrentTool).
    procedure Deactivate; virtual;
    // @name defines the cursor that is displayed to the user when the
    // mouse is over a @link(TframeView).
    //
    // (Note: in some cases, setting the cursor has no effect because
    // @link(GetCursor) does not refer to the value that was set.
    property Cursor: TCursor read GetCursor write SetCursor;
    // Descendants of @classname override @name to respond to
    // OnDoubleClick events.
    procedure DoubleClick(Sender: TObject); virtual;
    // @name is used to set the hint of
    // TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    // TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    // TfrmGoPhast.@link(TfrmGoPhast.frameSideView).
    property Hint: string read GetHint;
    // Descendants of @classname override @name to respond to
    // OnMouseDown events.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    // @name is @true if the mouse is down.  It reads a global variable
    // in the implementation section.
    property MouseIsDown: boolean read GetMouseIsDown;
    // Descendants of @classname override @name to respond to
    // OnMouseMove events.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); virtual;
    // Descendants of @classname override @name to respond to
    // OnMouseUp events.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    // handle a right click
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    // @name sets the cursors in all views.
    procedure SetAllCursors(const Value: TCursor); virtual;
    // @name is the @link(TViewDirection) from which this tool is viewed.
    property ViewDirection: TViewDirection read FViewDirection write
      SetViewDirection;
  end;

implementation

{$R *.dfm}

uses GR32_Polygons, frmGoPhastUnit, CursorsFoiledAgain, Math, RbwParser,
  frmScreenObjectPropertiesUnit, UndoItemsScreenObjects, frmGridAngleUnit,
  InteractiveTools, frmSetSpacingUnit, frmSubdivideUnit, BigCanvasMethods,
  frmRulerOptionsUnit, PhastModelUnit, frmGridValueUnit, EdgeDisplayUnit,
  CustomModflowWriterUnit, frmProgressUnit, SutraMeshUnit, frmDisplayDataUnit,
  frmCustomGoPhastUnit, VectorDisplayUnit, Generics.Collections,
  DrawMeshTypesUnit, MeshRenumberingTypes, ModelMuseUtilities,
  PestPropertiesUnit;

resourcestring
  StrTheSImageCanNo = 'The %s  image can not be shown at this magnification.' +
  ' When the magnification is reduced, you can display it again.';
  StrSorryTheViewWas = 'Sorry; the view was zoomed in too far.  I''ve had to' +
  ' zoom out a bit';
  StrMagnificationG = 'Magnification = %g';
  Str0sEndMemberValInt = '%0:s; End member values = (%1:d, %2:d)';
  StrTrue = 'True';
  StrFalse = 'False';
  StrXY0s1s = '(X,Y): (%0:s, %1:s)';
  StrXY0s1sRotated = '(X,Y): (%0:s, %1:s); (X''Y''): (%2:s, %3:s)';
  StrXZ0s1s = '(X'',Z): (%0:s, %1:s)';
  StrYZ0s1s = '(Y'',Z): (%0:s, %1:s)';
  StrChooseAToolAndCl = 'Choose a tool and click to edit model';
  StrCol0dRow1d = 'Col: %0:d; Row: %1:d';
  StrCol0dLay1d = 'Col: %0:d; Lay: %1:d';
  StrRow0dLay1d = 'Row: %0:d; Lay: %1:d';
  StrElement0dNode = 'Element: %0:d; Node: %1:d';
  StrCell0d = 'Cell: %0:d';
  StrCell0dLayer = 'Cell: %0:d; Layer: %1:d';

const
  SelectedCellsColor = clSilver;
  MaxGridCorners = 4;
var
  GlobalMouseIsDown: boolean;

  { TframeTop }

procedure TframeView.AdjustHorizontalScale(const NewX: integer);
begin
    // @name is used to redraw the horizontal scale.
    // @param(NewX NewX indicates how far the image in @link(ZoomBox)
    // has moved in the X direction in pixels.)

  // set the ends of the horizontal ruler so that the scale
  // is displayed properly.
  rulHorizontal.RulerEnds.Lower := 11;

  // This accounts for the width of the vertical scroll bar (width = 18)
  // plus the space between the end of the ruler at the end
  // of the empty space in ZoomBox ( = 12)
  // The value of 30 comes from 18+12

  // The ends must be adjusted by 1 to account for Panel1.BevelWidth = 1;
  rulHorizontal.RulerEnds.Upper := rulHorizontal.Width - 29;
  if rulHorizontal.RulerStart = sTopLeft then
  begin
    rulHorizontal.RulerValues.Lower := ZoomBox.X(10 + NewX);
    rulHorizontal.RulerValues.Upper := ZoomBox.X(ZoomBox.Width - 30
      + NewX);
  end
  else
  begin
    rulHorizontal.RulerValues.Upper := ZoomBox.X(10 + NewX);
    rulHorizontal.RulerValues.Lower := ZoomBox.X(ZoomBox.Width - 30
      + NewX);
  end;
  rulHorizontal.Invalidate;
end;

procedure TframeView.AdjustVerticalScale(const NewY: integer);
begin
    // @name is used to redraw the vertical scale.
    // @param(NewY NewY indicates how far the image in @link(ZoomBox)
    // has moved in the Y direction in pixels.)

  // set the ends of the horizontal ruler so that the scale
  // is displayed properly.
  rulVertical.RulerEnds.Lower := 11;

  // This accounts for the width of the vertical scroll bar (width = 18)
  // plus the space between the end of the ruler at the end
  // of the empty space in ZoomBox ( = 12)
  // The value of 30 comes from 18+12

  // The ends must be adjusted by 1 to account for Panel1.BevelWidth = 1;
  rulVertical.RulerEnds.Upper := rulVertical.Height - 29;
  if rulVertical.RulerStart = sTopLeft then
  begin
    rulVertical.RulerValues.Lower := ZoomBox.Y(10 + NewY);
    rulVertical.RulerValues.Upper := ZoomBox.Y(ZoomBox.Height - 30
      + NewY);
  end
  else
  begin
    rulVertical.RulerValues.Upper := ZoomBox.Y(10 + NewY);
    rulVertical.RulerValues.Lower := ZoomBox.Y(ZoomBox.Height - 30
      + NewY);
  end;
  rulVertical.Invalidate;
end;

procedure TframeView.UpdateStatusBarTopElementNode(const APoint: TPoint2D);
var
  Row: Integer;
  Column: Integer;
  ElementNumber: Integer;
//  Mesh: TSutraMesh3D;
  ElementLayer: Integer;
  NodeNumber: Integer;
  NodeLayer: Integer;
  Mesh: IMesh3D;
begin
  Mesh := frmGoPhast.PhastModel.SelectedModel.Mesh3D;
  if Mesh = nil then
  begin
    Exit;
  end;
  GetRowCol(APoint, Row, Column);
  ElementNumber := -1;
  if Column >= 0 then
  begin
    if not Mesh.Is3DMesh then
    begin
      ElementNumber := Mesh.Mesh2DI.Elements[Column].DisplayNumber;
    end
    else
    begin
      ElementLayer := Mesh.SelectedLayer;
      if ElementLayer >= Mesh.LayerCount then
      begin
        ElementLayer := Mesh.LayerCount-1;
      end;
      if Mesh.ElementCount > 0 then
      begin
        ElementNumber := Mesh.ElementArrayI[ElementLayer,Column].DisplayNumber;
      end
      else
      begin
        ElementNumber := Mesh.Mesh2DI.Elements[Column].DisplayNumber;
      end;
    end;
  end;
  GetNodeRowCol(APoint, Row, Column);
  NodeNumber := -1;
  if Column >= 0 then
  begin
    if not Mesh.Is3DMesh then
    begin
      NodeNumber := Mesh.Mesh2DI.Nodes[Column].NodeNumber+1;
    end
    else
    begin
      NodeLayer := Mesh.SelectedLayer;
      if Mesh.ElementCount > 0 then
      begin
        NodeNumber := Mesh.NodeArrayI[NodeLayer,Column].NodeNumber+1;
      end
      else
      begin
        NodeNumber := Mesh.Mesh2DI.NodesI2D[Column].NodeNumber+1;
      end;
    end;
  end;
  if frmGoPhast.PhastModel.SelectedModel.DisvUsed then
  begin
    frmGoPhast.sbMain.Panels[1].Text :=
      Format(StrCell0d, [ElementNumber]);
  end
  else
  begin
    frmGoPhast.sbMain.Panels[1].Text :=
      Format(StrElement0dNode, [ElementNumber, NodeNumber]);
  end;
end;


procedure TframeView.UpdateStatusBar(const X, Y: integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
  DataSet: TDataArray;
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  FillPanels1And2: Boolean;
  NodeLayer: Integer;
  NodeCol: Integer;
  ElCol: Integer;
  ElLayer: Integer;
//  Node3D: TSutraNode3D;
//  Element3D: TSutraElement3D;
  Mesh: IMesh3D;
  Node3D: INode3D;
  Element3D: IElement3D;
begin
  if (frmGoPhast.PhastModel.ComponentState * [csLoading, csReading]) <> [] then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.SelectedModel = nil then
  begin
    Exit;
  end;
    // @name shows appropriate text on the status bar.

  // Display information on the status bar.

  // get the location in real-world coordinates of the current cursor location
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  UpdateStatusBarCoordinates(APoint);

  Grid := frmGoPhast.PhastModel.SelectedModel.Grid;
  if (Grid <> nil) and Grid.ElevationsNeedUpdating then
  begin
    frmGoPhast.sbMain.Repaint;
    Exit;
  end;
  Mesh := frmGoPhast.PhastModel.Mesh3D;
  if (Mesh <> nil) and Mesh.ElevationsNeedUpdating then
  begin
    frmGoPhast.sbMain.Repaint;
    Exit;
  end;
  case ViewDirection of
    vdTop:
      begin
        DataSet := GetTopDisplayDataSet;
        if (DataSet = nil) or (DataSet.EvaluatedAt = eaBlocks) then
        begin
          // Get the column and row containing the current cursor position.
          GetRowCol(APoint, Row, Column);
          FillPanels1And2 := False;
          if (Column >= 0) and (Row >= 0) then
          begin
            if Grid <> nil then
            begin
              if (Column < Grid.ColumnCount) and (Row < Grid.RowCount) then
              begin
                FillPanels1And2 := True;
              end;
            end
            else if (Mesh <> nil) then
            begin
              if (Column < Mesh.Mesh2DI.ElementCount) and (Row < 1) then
              begin
                FillPanels1And2 := True;
              end;
            end;
          end;
          if FillPanels1And2 then
          begin
            if Mesh = nil then
            begin
              // If the cursor is over the grid,
              // display the column and row number

              frmGoPhast.sbMain.Panels[1].Text :=
                Format(StrCol0dRow1d, [Column + 1, Row + 1]);
            end
            else
            begin
              // display the cell number
              UpdateStatusBarTopElementNode(APoint);
            end;
            UpdateStatusBarForTopBlockDataSet(Column, Row, X, Y, APoint);
          end
          else
          begin
            // If the cursor is not over the grid or mesh, don't display
            // the column and row number or data set value.
            frmGoPhast.sbMain.Panels[1].Text := '';
            frmGoPhast.sbMain.Panels[2].Text := '';
          end;
        end
        else
        begin
          // Get the column and row containing the current cursor position.
          GetNodeRowCol(APoint, Row, Column);
          FillPanels1And2 := False;
          if (Column >= 0) and (Row >= 0) then
          begin
            if Grid <> nil then
            begin
              if (Column <= Grid.ColumnCount) and (Row <= Grid.RowCount) then
              begin
                FillPanels1And2 := True;
              end;
            end
            else if (Mesh <> nil) then
            begin
              if (Column < Mesh.Mesh2DI.NodeCount) and (Row < 1) then
              begin
                FillPanels1And2 := True;
              end;
            end;
          end;
          if FillPanels1And2 then
          begin
            if Mesh = nil then
            begin
              // If the cursor is over the grid,
              // display the column and row number
              frmGoPhast.sbMain.Panels[1].Text :=
                Format(StrCol0dRow1d, [Column + 1, Row + 1]);
            end
            else
            begin
              UpdateStatusBarTopElementNode(APoint);
            end;
            UpdateStatusBarForTopNodeDataSet(Column, Row, APoint);
          end
          else
          begin
            // If the cursor is not over the grid, don't display
            // the column and row number or data set value.
            frmGoPhast.sbMain.Panels[1].Text := '';
            frmGoPhast.sbMain.Panels[2].Text := '';
          end;
        end;
      end;
    vdFront:
      begin
        if Mesh <> nil then
        begin
          DataSet := Mesh.ThreeDDataSet;
          if DataSet = nil then
          begin
            DataSet := Mesh.ThreeDContourDataSet;
          end;
          GetMeshColLayer(APoint, NodeCol, NodeLayer, ElCol, ElLayer);
          if Mesh is TSutraMesh3D then
          begin
            if (NodeCol >= 0) and  (NodeLayer >= 0)
              and (ElCol >= 0) and  (ElLayer >= 0) then
            begin
              Node3D := Mesh.NodeArrayI[NodeLayer, NodeCol];
              Element3D := Mesh.ElementArrayI[ElLayer, ElCol];
              frmGoPhast.sbMain.Panels[1].Text :=
                Format(StrElement0dNode, [Element3D.ElementNumber+1, Node3D.NodeNumber+1]);
              if (DataSet = nil) or (DataSet.EvaluatedAt = eaBlocks) then
              begin
                UpdateStatusBarFrontBlockDataSet(ElCol, ElLayer, APoint);
              end
              else
              begin
                UpdateStatusBarFrontNodeDataSet(NodeCol, NodeLayer, APoint);
              end;
            end
            else
            begin
              frmGoPhast.sbMain.Panels[1].Text := '';
              frmGoPhast.sbMain.Panels[2].Text := '';
            end;
          end
          else
          begin
            if (ElCol >= 0) and  (ElLayer >= 0) then
            begin
              Element3D := Mesh.ElementArrayI[ElLayer, ElCol];
              frmGoPhast.sbMain.Panels[1].Text :=
                Format(StrCell0dLayer, [Element3D.ElementNumber+1, ElLayer+1]);
              UpdateStatusBarFrontBlockDataSet(ElCol, ElLayer, APoint);
            end
            else
            begin
              frmGoPhast.sbMain.Panels[1].Text := '';
              frmGoPhast.sbMain.Panels[2].Text := '';
            end;
          end;
        end
        else
        begin
          if Grid = nil then
          begin
            DataSet := nil;
          end
          else
          begin
            DataSet := Grid.FrontDataSet;
            if DataSet = nil then
            begin
              DataSet := Grid.FrontContourDataSet;
            end;
          end;
          if (DataSet = nil) or (DataSet.EvaluatedAt = eaBlocks) then
          begin
            // Get the column and layer containing the current cursor position.
            GetColLayer(APoint, Column, Layer);
            if (Column >= 0) and (Layer >= 0)
              and (Column < Grid.ColumnCount)
              and (Layer < Grid.LayerCount) then
            begin
              // If the cursor is over the grid,
              // display the column and layer number
              frmGoPhast.sbMain.Panels[1].Text :=
                Format(StrCol0dLay1d, [Column + 1, Layer + 1]);
              UpdateStatusBarFrontBlockDataSet(Column, Layer, APoint);
            end
            else
            begin
              // If the cursor is not over the grid, don't display
              // the column and layer number or the data set value.
              frmGoPhast.sbMain.Panels[1].Text := '';
              frmGoPhast.sbMain.Panels[2].Text := '';
            end;
          end
          else
          begin
            // Get the column and layer containing the current cursor position.
            GetNodeColLayer(APoint, Column, Layer);
            if (Column >= 0) and (Layer >= 0)
              and (Column <= Grid.ColumnCount)
              and (Layer <= Grid.LayerCount) then
            begin
              // If the cursor is over the grid,
              // display the column and layer number
              frmGoPhast.sbMain.Panels[1].Text :=
                Format(StrCol0dLay1d, [Column + 1, Layer + 1]);
              UpdateStatusBarFrontNodeDataSet(Column, Layer, APoint);
            end
            else
            begin
              // If the cursor is not over the grid, don't display
              // the column and layer number or the data set value.
              frmGoPhast.sbMain.Panels[1].Text := '';
              frmGoPhast.sbMain.Panels[2].Text := '';
            end;
          end;
        end;
      end;
    vdSide:
      begin
        if Grid = nil then
        begin
          DataSet := nil;
        end
        else
        begin
          DataSet := Grid.SideDataSet;
          if DataSet = nil then
          begin
            DataSet := Grid.SideContourDataSet;
          end;
        end;
        if (DataSet = nil) or (DataSet.EvaluatedAt = eaBlocks) then
        begin
          // Get the row and layer containing the current cursor position.
          GetRowLayer(APoint, Row, Layer);
          if (Row >= 0) and (Layer >= 0)
            and (Row < Grid.RowCount)
            and (Layer < Grid.LayerCount) then
          begin
            frmGoPhast.sbMain.Panels[1].Text :=
              Format(StrRow0dLay1d, [Row + 1, Layer + 1]);
            UpdateStatusBarSideBlockDataSet(Row, Layer, APoint);
          end
          else
          begin
            // If the cursor is not over the grid, don't display
            // the row and layer number or the data set value.
            frmGoPhast.sbMain.Panels[1].Text := '';
            frmGoPhast.sbMain.Panels[2].Text := '';
          end;
        end
        else
        begin
          // Get the row and layer containing the current cursor position.
          GetNodeRowLayer(APoint, Row, Layer);
          if (Row >= 0) and (Layer >= 0)
            and (Row <= Grid.RowCount)
            and (Layer <= Grid.LayerCount) then
          begin
            frmGoPhast.sbMain.Panels[1].Text :=
              Format(StrRow0dLay1d, [Row + 1, Layer + 1]);
            UpdateStatusBarSideNodeDataSet(Row, Layer, APoint);
          end
          else
          begin
            // If the cursor is not over the grid, don't display
            // the row and layer number or the data set value.
            frmGoPhast.sbMain.Panels[1].Text := '';
            frmGoPhast.sbMain.Panels[2].Text := '';
          end;
        end;
      end;
  else
    Assert(False);
  end;
  frmGoPhast.sbMain.Repaint;
end;

procedure TframeView.ShowMagnification;
begin
    // @name shows the magnification on the status bar.
  frmGoPhast.sbMain.Panels[0].Text :=
    Format(StrMagnificationG, [ZoomBox.Magnification]);
end;

function TframeView.GetScreenObjectsChanged: boolean;
begin
    // See @link(ScreenObjectsHaveChanged).
  case ViewDirection of
    vdTop: result := frmGoPhast.TopScreenObjectsChanged;
    vdFront: result := frmGoPhast.FrontScreenObjectsChanged;
    vdSide: result := frmGoPhast.SideScreenObjectsChanged;
  else
    begin
      Assert(False);
      result := False;
    end;
  end;
end;

procedure TframeView.SetScreenObjectsChanged(const Value: boolean);
begin
    // See @link(ScreenObjectsHaveChanged).
  case ViewDirection of
    vdTop: frmGoPhast.TopScreenObjectsChanged := Value;
    vdFront: frmGoPhast.FrontScreenObjectsChanged := Value;
    vdSide: frmGoPhast.SideScreenObjectsChanged := Value;
  else
    begin
      Assert(False);
    end;
  end;
end;

function TframeView.GetNeedToRecalculateCellColors: boolean;
begin
  if frmGoPhast.PhastModel <> nil then
  begin
    case ViewDirection of
      vdTop: result := frmGoPhast.PhastModel.NeedToRecalculateTopCellColors;
      vdFront: result := frmGoPhast.PhastModel.NeedToRecalculateFrontCellColors;
      vdSide: result := frmGoPhast.PhastModel.NeedToRecalculateSideCellColors;
    else
      begin
        Assert(False);
        result := False;
      end;
    end;
  end
  else
  begin
    result := False;
  end;
end;

procedure TframeView.SetNeedToRecalculateCellColors(const Value: boolean);
begin
  // See @link(NeedToRecalculateCellColors).
  if frmGoPhast.PhastModel <> nil then
  begin
    case ViewDirection of
      vdTop: frmGoPhast.PhastModel.NeedToRecalculateTopCellColors := Value;
      vdFront: frmGoPhast.PhastModel.NeedToRecalculateFrontCellColors := Value;
      vdSide: frmGoPhast.PhastModel.NeedToRecalculateSideCellColors := Value;
    else
      begin
        Assert(False);
      end;
    end;
  end;
  if Value then
  begin
    frmGoPhast.InvalidateImage32AllViews;
  end;
end;

function TframeView.ColorTimeList: TCustomTimeList;
begin
    // If @link(ColorDataSet) is a transient @link(TDataArray), @name is
    // the @link(TPhastTimeList) that owns it.
  case ViewDirection of
    vdTop:
      begin
        result := frmGoPhast.PhastModel.TopTimeList;
      end;
    vdFront:
      begin
        result := frmGoPhast.PhastModel.FrontTimeList;
      end;
    vdSide:
      begin
        result := frmGoPhast.PhastModel.SideTimeList;
      end;
  else
    begin
      Assert(False);
      result := nil;
    end;
  end;
end;

function TframeView.ConvertFront2D_Point(const APoint: TPoint2D): TPoint;
begin
  result.X := ZoomBox.XCoord(APoint.X);
  result.Y := ZoomBox.YCoord(APoint.Y);
end;

function TframeView.ConvertPoint(const APoint: TPoint2D): TPoint;
begin
  case ViewDirection of
    vdTop: result := ConvertTop2D_Point(APoint);
    vdFront: result := ConvertFront2D_Point(APoint);
    vdSide: result := ConvertSide2D_Point(APoint);
    else Assert(False);
  end;
end;

function TframeView.ConvertSide2D_Point(const APoint: TPoint2D): TPoint;
begin
  result.X := ZoomBox.XCoord(APoint.Y);
  result.Y := ZoomBox.YCoord(APoint.X);
end;

function TframeView.ConvertTop2D_Point(const APoint: TPoint2D): TPoint;
begin
  result.X := ZoomBox.XCoord(APoint.X);
  result.Y := ZoomBox.YCoord(APoint.Y);
end;

function TframeView.ColorDataSet: TDataArray;
begin
    // @name returns the @link(TDataArray) the is used to color the grid
    // in this view of the model.
  if frmGoPhast.Grid <> nil then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.Grid.TopDataSet;
        end;
      vdFront:
        begin
          result := frmGoPhast.Grid.FrontDataSet;
        end;
      vdSide:
        begin
          result := frmGoPhast.Grid.SideDataSet;
        end;
    else
      begin
        Assert(False);
        result := nil;
      end;
    end;
  end
  else if frmGoPhast.PhastModel.Mesh3D <> nil then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.PhastModel.Mesh3D.TopDataSet;
        end;
      vdFront:
        begin
          result := frmGoPhast.PhastModel.Mesh3D.ThreeDDataSet;
        end;
      vdSide:
        begin
          result := nil;
        end;
    else
      begin
        Assert(False);
        result := nil;
      end;
    end;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TframeView.UpdateTimeDataSet;
var
  Time: double;
  TimeList: TCustomTimeList;
begin
    // When the data set used to color the grid
    // is a transient @link(TDataArray),
    // @name is used to make sure that an up to date version of the
    // @link(TDataArray) is used.
  TimeList := ColorTimeList;
  case ViewDirection of
    vdTop:
      begin
        Time := frmGoPhast.PhastModel.TopDisplayTime;
        frmGoPhast.PhastModel.UpdateTopTimeDataSet(TimeList, Time);
      end;
    vdFront:
      begin
        Time := frmGoPhast.PhastModel.FrontDisplayTime;
        frmGoPhast.PhastModel.UpdateFrontTimeDataSet(TimeList, Time);
      end;
    vdSide:
      begin
        Time := frmGoPhast.PhastModel.SideDisplayTime;
        frmGoPhast.PhastModel.UpdateSideTimeDataSet(TimeList, Time);
      end;
  else
    begin
      Assert(False);
    end;
  end;
  Time := frmGoPhast.PhastModel.ThreeDDisplayTime;
  frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(TimeList, Time);
end;

procedure TframeView.InitializeDataSet;
var
  DataSet: TDataArray;
  TimeList: TCustomTimeList;
begin
    // @name initializes the data set used to color the grid on this
    // view of the model.
  TimeList := ColorTimeList;
  if TimeList = nil then
  begin
    DataSet := ColorDataSet;
    if DataSet = nil then
      exit;
    if not DataSet.UpToDate then
    begin
      DataSet.Initialize;
//      ScreenObjectsHaveChanged := True;
    end;
  end
  else
  begin
    if not TimeList.UpToDate then
    begin
      frmGoPhast.PhastModel.ThreeDDataSet := nil;
      TimeList.Initialize;
//      ScreenObjectsHaveChanged := True;
    end;
    UpdateTimeDataSet;
  end;
end;

function TframeView.RecalculateCellColors: boolean;
var
  ACursor: TCursor;
  Mesh: IMesh3D;
//  Mesh: TSutraMesh3D;
begin
    // @name checks if the data set used to color the grid on this view of the
    // model is up to date.  If not, it initializes it.
    // Once it is up to data, @name will store the correct color for each cell
    // or element in the grid.
  result := False;
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ResetCellColors;
    if ScreenObjectsHaveChanged then
    begin
      ScreenObjectsHaveChanged := True;
      NeedToRecalculateCellColors := True;
      Exit;
    end;

    Mesh := frmGoPhast.PhastModel.Mesh3D;
    if Mesh <> nil then
    begin
      Mesh.CheckUpdateElevations;
    end;

    InitializeDataSet;

    ResetCellColors;
    if frmGoPhast.Grid <> nil then
    begin
      frmGoPhast.Grid.UpdateCellColors(ViewDirection);
    end;
  finally
    Screen.Cursor := ACursor;
//    Screen.Cursor := crDefault;
  end;

  result := True;
end;

function TframeView.GetGridChanged: boolean;
begin
    // See @link(GridChanged).
  case ViewDirection of
    vdTop: result := frmGoPhast.TopDiscretizationChanged;
    vdFront: result := frmGoPhast.FrontDiscretizationChanged;
    vdSide: result := frmGoPhast.SideDiscretizationChanged;
  else
    begin
      Assert(False);
      result := False;
    end;
  end;
end;

procedure TframeView.SetGridChanged(const Value: boolean);
begin
    // See @link(GridChanged).
  case ViewDirection of
    vdTop: frmGoPhast.TopDiscretizationChanged := Value;
    vdFront: frmGoPhast.FrontDiscretizationChanged := Value;
    vdSide: frmGoPhast.SideDiscretizationChanged := Value;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TframeView.DrawCrossSection;
var
  ChildIndex: Integer;
begin
  frmGoPhast.PhastModel.CrossSection.Draw(FBitMap32, ViewDirection);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel.
        CrossSection.Draw(FBitMap32, ViewDirection);
    end;
  end;

end;

procedure TframeView.DrawGridAndScreenObjects;
var
  BitmapIndex: integer;
  Item: TCompressedBitmapItem;
//  Mesh: TSutraMesh3D;
  Grid: TCustomModelGrid;
  Mesh: IDrawMesh;
//  PestProperties: TPestProperties;
begin
  FPaintingNeeded := True;
  if not frmGoPhast.CanDraw then
  begin
    Exit;
  end;
  PilotPointsChanged := False;
    // @name is the main routine for drawing the
    // @link(TfrmGoPhast.Grid) and @link(TScreenObject)s.
    // It will also draw any imported images.
  if frmGoPhast.Grid <> nil then
  begin
    frmGoPhast.Grid.Draw3DAllowed := False;
  end;
  try
    if ViewDirection = vdSide then
    begin
      if frmGoPhast.DisvUsed or (frmGoPhast.ModelSelection in SutraSelection) then
      begin
        Exit;
      end;
    end;

    Mesh := frmGoPhast.PhastModel.DrawMesh;
    if (Mesh <> nil) and not Mesh.CanDraw then
    begin
      Exit;
    end;
    try
      FreeAndNil(FBitMap32);
//      FBitMap32.Free;
//      FBitMap32 := nil;
      FBitMap32 := TBitmap32.Create;

      FBitMap32.Height := ZoomBox.Image32.Height;
      FBitMap32.Width := ZoomBox.Image32.Width;
      FBitMap32.Font := frmGoPhast.PhastModel.ContourFont;

      // If the colors of the grid cells are out of date,
      // recalculate them.
      if NeedToRecalculateCellColors then
      begin
        // Make sure the grid elevations are updated first.
        Grid := frmGoPhast.Grid;
        if (Grid <> nil) and (Grid.LayerCount >= 1)
          and (Grid.RowCount >= 1)  and (Grid.ColumnCount >= 1)  then
        begin
          Grid.ThreeDElementCenter(0,0,0);
        end;

        if not RecalculateCellColors then
          Exit;
      end;

      // draw a box around the drawing area
      DrawBigRectangle32(FBitMap32, clBlack32, clWhite32, 1.0, 0, 0,
        ZoomBox.Image32.ClientWidth-1, ZoomBox.Image32.ClientHeight-1);

      for BitmapIndex := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
      begin
        Item := frmGoPhast.PhastModel.Bitmaps.Items[BitmapIndex] as
          TCompressedBitmapItem;
        if Item.Visible and Item.CanShow and (Item.ViewDirection = ViewDirection) then
        begin
          try
            DrawImage(Item);
          except on EOutOfResources do
            begin
              Item.CanShow := False;
              if Item.DisplayMessage then
              begin
                MessageDlg(Format(StrTheSImageCanNo, [Item.Name]),
                  mtInformation, [mbOK], 0);
                Item.DisplayMessage := False;
              end;
            end;
          end;
        end;
      end;

      // draw the grid.
      if frmGoPhast.Grid <> nil then
      begin
        frmGoPhast.Grid.Draw(FBitMap32, ViewDirection);
        if (frmGoPhast.PhastModel.ThreeDDataSet <> nil)
          or (frmGoPhast.Grid.ThreeDContourDataSet <> nil) then
        begin
          if (frmDisplayData <> nil) and frmDisplayData.Visible then
          begin
            UpdateFrmDisplayData(True);
          end;
//          if frmDisplayData = nil then
//          begin
//            Application.CreateForm(TfrmDisplayData, frmDisplayData);
//          end;
        end;
      end
      else
      begin
        if (frmGoPhast.PhastModel.ThreeDDataSet <> nil)
          {or (frmGoPhast.Grid.ThreeDContourDataSet <> nil)} then
        begin
          if frmDisplayData = nil then
          begin
            Application.CreateForm(TfrmDisplayData, frmDisplayData);
          end;
          UpdateFrmDisplayData(True);
        end;
      end;


      if (Mesh <> nil) then
      begin
        Mesh.Draw(FBitMap32, ViewDirection);
      end;

      if ViewDirection = vdTop then
      begin
        frmGoPhast.PhastModel.DrawPilotPoints(FBitMap32);
      end;

      // Do not call Application.ProcessMessages.
      // Calling Application.ProcessMessages causes all the views to
      // be redrawn.  If that is done, PaintLayer has to deal with the
      // case where it is called within PaintLayer from another view.
      // In addition, this may be the cause of some access violations
      // within Graphics32.
      // See the compiler definition ExtraDrawingNeeded in the TFrameView
      // declaration.

//      Application.ProcessMessages;

      // If the objects have been changed while drawing the grid
      // stop and start over.
      if ScreenObjectsHaveChanged then
      begin
        ScreenObjectsHaveChanged := True;
        ZoomBox.InvalidateImage32;
        ZoomBox.Invalidate;
        Exit;
      end;

      // If there are no objects, finish up and quit.
      if (frmGoPhast.PhastModel.ScreenObjectCount = 0)
        and not frmGoPhast.PhastModel.LgrUsed
        and (not frmGoPhast.PhastModel.PathLines.HasData)
        and (not frmGoPhast.PhastModel.Endpoints.HasData)
        and (not frmGoPhast.PhastModel.TimeSeries.HasData)
        and (frmGoPhast.PhastModel.CrossSection.DataArrays.Count = 0)
        and not (frmGoPhast.ModelSelection in SutraSelection)
        then
      begin
        GridChanged := False;
        Exit;
      end;

      // Draw the screen objects.
      InvalidateScreenObjectCoordinates;
      DrawScreenObjects;

      if ViewDirection = vdTop then
      begin
        frmGoPhast.PhastModel.DrawHeadObservations(FBitMap32, ZoomBox);
        frmGoPhast.PhastModel.DrawPestPointObservations(FBitMap32, ZoomBox);
        frmGoPhast.PhastModel.DrawSfrStreamLinkages(FBitMap32, ZoomBox);
        frmGoPhast.PhastModel.DrawStrStreamLinkages(FBitMap32, ZoomBox);
        frmGoPhast.PhastModel.DrawSfrMf6StreamLinkages(FBitMap32, ZoomBox);
        frmGoPhast.PhastModel.DrawSwrReachConnections(FBitMap32, ZoomBox);
      end;

      if frmGoPhast.ModelSelection in ModflowSelection then
      begin
        DrawPathLines;
        DrawTimeSeries;
        DrawEndPoints;
        DrawCrossSection;
      end;

      if frmGoPhast.ModelSelection in SutraSelection + [msModflow2015] then
      begin
        DrawVectors;
      end;

      if frmGoPhast.ModelSelection in SutraSelection then
      begin
        DrawCrossSection;
      end;
    except
      on EInvalidGraphicOperation do
      begin
        // If the size is too big, make it smaller and start over.
        WarnTooBig;
      end;
      on EIntOverflow do
      begin
        // If the size is too big, make it smaller and start over.
        ZoomBox.ZoomBy(0.5);
        ZoomBox.InvalidateImage32;
        Beep;
        MessageDlg(StrSorryTheViewWas, mtInformation, [mbOK], 0);
      end;
      on EOverflow do
      begin
        // If the size is too big, make it smaller and start over.
        ZoomBox.ZoomBy(0.5);
        ZoomBox.InvalidateImage32;
        Beep;
        MessageDlg(StrSorryTheViewWas, mtInformation, [mbOK], 0);
      end;
    end;
  finally
    ModelChanged := False;
    if frmGoPhast.Grid <> nil then
    begin
      frmGoPhast.Grid.Draw3DAllowed := True;
      frmGoPhast.frame3DView.glWidModelView.Invalidate;
    end;
    FHasDrawn := True;
  end;
end;

procedure TframeView.Paint(Sender: TObject);
var
  ShouldUpdate: boolean;
  PreviousCursor: TCursor;
begin
  // calling Application.ProcessMessages here causes the Zoombox to redraw
  // properly but also can be a bottleneck.
//  Application.ProcessMessages;
  // @name is draws the grid @link(TfrmGoPhast.Grid) and
  // @link(TScreenObject)s.
  if FDrawing or not frmGoPhast.CanDraw then
    Exit;
  FDrawing := True;
  ShouldUpdate := ScreenObjectsHaveChanged or NeedToRecalculateCellColors
    or PilotPointsChanged;
  ScreenObjectsHaveChanged := False;
  try
    if (FBitMap32 <> nil) then
    begin
      // if you need to redraw the FBitMap32,
      // change the cursor to an hourglass,
      // redraw, and change the cursor back.
      if GridChanged or ModelChanged
        or (FBitMap32.Height <> ZoomBox.Image32.Height)
        or (FBitMap32.Width <> ZoomBox.Image32.Width)
        or ShouldUpdate or MagnificationChanged then
      begin
        MagnificationChanged := False;
        PreviousCursor := ZoomBox.Cursor;
        try
          ZoomBox.Cursor := crHourGlass;
          try
            DrawGridAndScreenObjects;
          except on EInvalidOp do
            begin
              ZoomBox.ZoomBy(0.01);
              GridChanged := True;
              ZoomBox.InvalidateImage32;
              Exit;
            end;
          end;
        finally
          ZoomBox.Cursor := PreviousCursor;
        end;
      end;
    end;
  finally
    ShouldUpdate := ScreenObjectsHaveChanged or NeedToRecalculateCellColors;
    ScreenObjectsHaveChanged := False;
    if ShouldUpdate then
    begin
      ZoomBox.InvalidateImage32;
    end;
    FDrawing := False;
  end;
end;

constructor TframeView.Create(AOwner: TComponent);
var
  TempLayer: TPositionedLayer;
begin
    // @name creates an instance of @classname.
  inherited;
  DoubleBuffered := True;
  pnlTop.ParentColor := True;
  FBitMap32 := TBitmap32.Create;
  FPreviousScreenObjects := TList.Create;
  SetUpColRowLayerSelector;

  // Image32.Layers is a TCollection.
  // You can have it hold different sorts of TCustomLayer
  // by passing it a descendant of TCustomLayer.
  // Here we use TPositionedLayer.
  FPositionedLayer := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
    TPositionedLayer;
  // Assign an event handler for the OnPaint event.
  FPositionedLayer.OnPaint := PaintLayer;
  // Set the location of the TPositionedLayer

  // Tell the layer to respond to mouse events
  FPositionedLayer.LayerOptions := FPositionedLayer.LayerOptions or
    LOB_MOUSE_EVENTS;

  // ZoomBoxHitTest means that the hit test always says the hit test
  // succeeds.
  FPositionedLayer.OnHitTest := ZoomBoxHitTest;

  // TempLayer is used for painting the selected and old screen objects.
  TempLayer := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
    TPositionedLayer;
  TempLayer.OnPaint := PaintSelectedScreenObjects;
end;

procedure TframeView.ZoomBoxHitTest(Sender: TObject; X, Y: Integer; var
  Passed: Boolean);
begin
  Passed := True;
end;

procedure TframeView.ZoomBoxImage32DblClick(Sender: TObject);
begin
    // @name is the @link(ZoomBox).Image32.OnDblClick event handler
    // It delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.DoubleClick).
//  with frmGoPhast do
  begin
    if (frmGoPhast.CurrentTool <> nil) then
    begin
      frmGoPhast.CurrentTool.DoubleClick(Sender);
    end;
  end;
end;

procedure TframeView.ZoomBoxImage32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer:
  TCustomLayer);
begin
    // @name is the event handler for @link(ZoomBox).Image32.OnMouseDown
    // If the right mouse button is depressed, it exits and allows
    // @link(OrderMenu) to take over.
    // Otherwise it updates frmGoPhast.@link(TfrmGoPhast.CursorGrid),
    // frmGoPhast.@link(TfrmGoPhast.CursorX), and
    // frmGoPhast.@link(TfrmGoPhast.CursorY).
    // Then it delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.MouseDown).

  // This is the main procedure for responding to OnMouseDown events
  // on the screen.
  MouseStartX := X;
  MouseStartY := Y;

  if ssRight in Shift then
  begin
    // for a right click, don't respond.  Instead OrderMenu will appear.
    Exit;
  end;
  frmGoPhast.CursorGrid := CursorGrid;

  // Record the current cursor position.
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;

  if frmGoPhast.CurrentTool <> nil then
  begin
    frmGoPhast.CurrentTool.ViewDirection := ViewDirection;
    frmGoPhast.CurrentTool.MouseDown(Sender, Button, Shift, X, Y);
  end;
end;

procedure TframeView.ZoomBoxImage32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
    // @name is the event handler for @link(ZoomBox).Image32.OnMouseMove
    // Otherwise it updates frmGoPhast.@link(TfrmGoPhast.CursorGrid),
    // frmGoPhast.@link(TfrmGoPhast.CursorX), and
    // frmGoPhast.@link(TfrmGoPhast.CursorY).
    // It then calls @link(UpdateStatusBar).
    // Then it delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.MouseMove).

  // This procedure is the main one for responding to MouseMove events.
  if ZoomBox.Panning then
  begin
    FPositionedLayer.Location := FloatRect(X, Y, X, Y);
  end;

  // Record which grid the cursor is over
  frmGoPhast.CursorGrid := CursorGrid;
  // Record the current cursor position
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;

  // display the current position and other related data.
  UpdateStatusBar(X, Y);

  if frmGoPhast.CurrentTool <> nil then
  begin
    if not frmGoPhast.CurrentTool.MouseIsDown then
    begin
      frmGoPhast.CurrentTool.ViewDirection := ViewDirection;
    end;

    frmGoPhast.CurrentTool.MouseMove(Sender, Shift, X, Y);
    Exit;
  end;
end;

procedure TframeView.ZoomBoxImage32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer:
  TCustomLayer);
begin
    // @name is the event handler for @link(ZoomBox).Image32.OnMouseUp.
    // If the right mouse button is depressed, it exits and allows
    // @link(OrderMenu) to take over.
    // Otherwise it updates frmGoPhast.@link(TfrmGoPhast.CursorGrid),
    // frmGoPhast.@link(TfrmGoPhast.CursorX), and
    // frmGoPhast.@link(TfrmGoPhast.CursorY).
    // Then it delegates the event to
    // frmGoPhast.@link(TfrmGoPhast.CurrentTool).@link(
    // TCustomInteractiveTool.MouseUp).
  if FBusy then
    Exit;
  FBusy := True;
  try
    if ssRight in Shift then
    begin
      if frmGoPhast.CurrentTool <> nil then
      begin
        frmGoPhast.CurrentTool.RightClick(Sender, Button, Shift, X, Y);
      end;
      // for a right click, don't create objects
      Exit;
    end;
    // Record the current cursor position.
    frmGoPhast.CursorGrid := CursorGrid;
    frmGoPhast.CursorX := X;
    frmGoPhast.CursorY := Y;

    if frmGoPhast.CurrentTool <> nil then
    begin
      frmGoPhast.CurrentTool.MouseUp(Sender, Button, Shift, X, Y);
    end;

  finally
    FBusy := False;
  end;
end;

procedure TframeView.ZoomBoxMagnificationChanged(Sender: TObject);
begin
  AllowBitmapsToBeDisplayed;
end;

procedure TframeView.PaintLayer(Sender: TObject; Buffer: TBitmap32);
begin
  if frmGoPhast.PhastModel.DrawMesh = nil then
//  if not (frmGoPhast.PhastModel.ModelSelection in SutraSelection) then
  begin
    if frmGoPhast.Grid = nil then
    begin
      Exit;
    end;
    if
  {$IFDEF ExtraDrawingNeeded}
      FPaintingLayer or
  {$ENDIF}
      frmGoPhast.Grid.Drawing3DGrid then
    begin
      Exit;
    end;
  end;
{$IFDEF ExtraDrawingNeeded}
  if frmGoPhast.frameTopView.FPaintingLayer then
  begin
    FNeedToRedraw := True;
    Exit;
  end;
  if frmGoPhast.frameFrontView.FPaintingLayer then
  begin
    FNeedToRedraw := True;
    Exit;
  end;
  if frmGoPhast.frameSideView.FPaintingLayer then
  begin
    FNeedToRedraw := True;
    Exit;
  end;
  FPaintingLayer := True;
{$ENDIF}
  try
    try
      Buffer.BeginUpdate;
      try
        Paint(Sender);
        if ZoomBox.Panning then
        begin
          Buffer.Draw(
            Round(FPositionedLayer.Location.Left) - MouseStartX,
            Round(FPositionedLayer.Location.Top) - MouseStartY,
            FBitMap32);
        end
        else
        begin
          Buffer.Draw(0, 0, FBitMap32);
        end;
      finally
        Buffer.EndUpdate;
      end;
    finally
  {$IFDEF ExtraDrawingNeeded}
      FNeedToRedraw := False;
      FPaintingLayer := False;
      if frmGoPhast.frameTopView.FNeedToRedraw then
      begin
        frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
      end;
      if frmGoPhast.frameFrontView.FNeedToRedraw then
      begin
        frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
      end;
      if frmGoPhast.frameSideView.FNeedToRedraw then
      begin
        frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
      end;
  {$ENDIF}
    end;
  finally
    FPaintingNeeded := False;
  end;
end;

procedure TframeView.PaintSelectedScreenObjects(Sender: TObject; Buffer:
  TBitmap32);
var
  ABitMap: TBitmap32;
begin

  While SelectingObjectsWithLine do
  begin
    Sleep(20);
  end;
  if (frmGoPhast.PhastModel = nil) or
    (frmGoPhast.PhastModel.SelectedScreenObjectCount = 0)
    or not frmGoPhast.CanDraw then
  begin
    Exit;
  end;

  Buffer.BeginUpdate;
  try
    ABitMap := TBitmap32.Create;
    try
      ABitMap.Height := Buffer.Height;
      ABitMap.Width := Buffer.Width;
      ABitMap.DrawMode := dmBlend;
      DrawSelectedAndOldScreenObjects(ABitMap);
      if ZoomBox.Panning then
      begin
        Buffer.Draw(
          Round(FPositionedLayer.Location.Left) - MouseStartX,
          Round(FPositionedLayer.Location.Top) - MouseStartY,
          ABitMap);
      end
      else
      begin
        Buffer.Draw(0, 0, ABitMap);
      end;
    finally
      ABitMap.Free;
    end;
  finally
    Buffer.EndUpdate;
  end;
end;

destructor TframeView.Destroy;
begin
    // @name destroys the current instance of @classname.
    // Do not call @classname directly.  Call Free instead.
  FBitMap32.Free;
  FPreviousScreenObjects.Free;
  inherited;
end;

procedure TframeView.rulerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
    // @name is OnMouseMove event handler for @link(rulHorizontal), and
    // @link(rulVertical).
    // @name sets TfrmGoPhast.@link(TfrmGoPhast.CursorGrid) to cgNone;

  // This is called whenever the mouse is over one of the rulers.
  frmGoPhast.CursorGrid := cgNone;
end;

procedure TframeView.GetNodeRowCol(APoint: TPoint2D; out Row, Column:
  integer);
var
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  TopCell: T2DTopCell;
  Mesh: IMesh3D;
begin
  Grid := frmGoPhast.PhastModel.SelectedModel.Grid;
  if Grid = nil then
  begin
    Mesh := frmGoPhast.PhastModel.Mesh3D;
    if Mesh = nil then
    begin
      Column := -1;
      Row := -1;
    end
    else
    begin
      TopCell := Mesh.TopContainingCellOrElement(APoint, eaNodes);
      Column := TopCell.Col;
      Row := TopCell.Row;
    end;
  end
  else
  begin
    // @name returns the column and row numbers of the cell containing APoint;
    // APoint must be on a top view of the model.
    // APoint must be in real-world coordinates not PhastModel.SelectedModel.Grid coordinates.

    APoint :=
      Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
    Column := Grid.NearestColumnPosition(APoint.X);
    Row := Grid.NearestRowPosition(APoint.Y);
    if (Column < 0) or (Row < 0)
      or (Column > Grid.ColumnCount)
      or (Row > Grid.RowCount) then
    begin
      Column := -1;
      Row := -1;
    end;
  end;
end;

procedure TframeView.GetRowCol(APoint: TPoint2D; out Row, Column:
  integer);
var
  NeighborColumn, NeighborRow: integer;
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  TopCell: T2DTopCell;
  Mesh: IMesh3D;
begin
  Grid := frmGoPhast.PhastModel.SelectedModel.Grid;
  if Grid = nil then
  begin
    Mesh := frmGoPhast.PhastModel.Mesh3D;
    if Mesh = nil then
    begin
      Column := -1;
      Row := -1;
    end
    else
    begin
      TopCell := Mesh.TopContainingCellOrElement(APoint, eaBlocks);
      Column := TopCell.Col;
      Row := TopCell.Row;
    end;
  end
  else
  begin
    // @name gets the column and row numbers of the element containing APoint;
    // APoint must be on a top view of the model.
    // APoint must be in real-world coordinates not grid coordinates.
    if (Grid.ColumnCount <= 0)
      or (Grid.RowCount <= 0) then
    begin
      Column := -1;
      Row := -1;
      Exit;
    end;

    // Get the row and column numbers of the cell containing APoint;
    APoint :=
      Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
    Column := Grid.NearestColumnPosition(APoint.X);
    Row := Grid.NearestRowPosition(APoint.Y);

    case EvaluatedAt of
      eaBlocks:
        begin
          if (Column >= 0) and (Row >= 0)
            and (Column <= Grid.ColumnCount)
            and (Row <= Grid.RowCount) then
          begin
            if APoint.X > Grid.ColumnPosition[Column] then
            begin
              if Grid.ColumnDirection = cdWestToEast then
              begin
                NeighborColumn := Column + 1;
              end
              else
              begin
                NeighborColumn := Column - 1;
              end;
            end
            else
            begin
              if Grid.ColumnDirection = cdWestToEast then
              begin
                NeighborColumn := Column - 1;
              end
              else
              begin
                NeighborColumn := Column + 1;
              end;
            end;
            if APoint.Y > Grid.RowPosition[Row] then
            begin
              if Grid.RowDirection = rdSouthToNorth then
              begin
                NeighborRow := Row + 1;
              end
              else
              begin
                NeighborRow := Row - 1;
              end;
            end
            else
            begin
              if Grid.RowDirection = rdSouthToNorth then
              begin
                NeighborRow := Row - 1;
              end
              else
              begin
                NeighborRow := Row + 1;
              end;
            end;
            if (NeighborColumn >= 0) and (NeighborRow >= 0)
              and (NeighborColumn <= Grid.ColumnCount)
              and (NeighborRow <= Grid.RowCount) then
            begin
              if NeighborColumn < Column then
              begin
                Column := NeighborColumn;
              end;
              if NeighborRow < Row then
              begin
                Row := NeighborRow;
              end;
            end
            else
            begin
              Column := -1;
              Row := -1;
            end;
          end
          else
          begin
            Column := -1;
            Row := -1;
          end;
        end;
      eaNodes:
        begin
          if (APoint.X < Grid.ColumnPosition[0])
            or (APoint.X > Grid.ColumnPosition[Grid.ColumnCount])
            or (APoint.Y < Grid.RowPosition[0])
            or (APoint.Y > Grid.RowPosition[Grid.RowCount])
            then
          begin
            Column := -1;
            Row := -1;
          end
        end;
      else Assert(False);
    end;
  end;
end;

procedure TframeView.WarnTooBig;
begin
    // If the magnification is too big, zoom out a bit and warn the user.
  // zoom out a bit.
  if (ZoomBox.Image32.Width > ZoomBox.Width) or
    (ZoomBox.Image32.Height > ZoomBox.Height) then
  begin
    ZoomBox.ZoomBy(0.8);
    ZoomBox.InvalidateImage32;
    Beep;
  end;
  MessageDlg(StrSorryTheViewWas, mtInformation, [mbOK], 0);
end;

procedure TframeView.InvalidateScreenObjectCoordinates;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
    // @name calls TScreenObject.@link(TScreenObject.InvalidateCoordinates)
    // for every @link(TScreenObject) on this view of the model.
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1
    do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.ViewDirection = ViewDirection then
    begin
      AScreenObject.InvalidateCoordinates;
    end;
  end;
end;

procedure TframeView.miInvertSelectedVerticesClick(Sender: TObject);
begin
  frmGoPhast.miInvertSelectedVerticesClick(Sender);
end;

procedure TframeView.miLockSelectedObjectsClick(Sender: TObject);
begin
  frmGoPhast.miLockSelectedObjectsClick(Sender);
end;

procedure TframeView.DrawVectors;
var
  LocalModel: TPhastModel;
  VelocityVectors: TVectorCollection;
  VItem: TVectorItem;
begin
  LocalModel := frmGoPhast.PhastModel;
  LocalModel.MaxVectors.PlotVectors2D(ViewDirection, FBitmap32);
  if (LocalModel.ModelSelection in SutraSelection)
    and (LocalModel.SutraMesh <> nil)
    and (LocalModel.SutraMesh.MeshType = mt3D) then
  begin
    LocalModel.MidVectors.PlotVectors2D(ViewDirection, FBitmap32);
    LocalModel.MinVectors.PlotVectors2D(ViewDirection, FBitmap32);
  end;
  VelocityVectors := LocalModel.VelocityVectors;
  if VelocityVectors.SelectedItem >= 0 then
  begin
    VItem := VelocityVectors.Items[VelocityVectors.SelectedItem] as TVectorItem;
    VItem.Vectors.PlotVectors2D(ViewDirection, FBitmap32);
  end;
end;

procedure TframeView.DrawPathLines;
var
  Orientation: TDataSetOrientation;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Orientation := dsoTop;
  case ViewDirection of
    vdTop: Orientation := dsoTop;
    vdFront: Orientation := dsoFront;
    vdSide: Orientation := dsoSide;
    else Assert(False);
  end;
  if (frmGoPhast.PhastModel.Pathlines.HasData) then
  begin
    frmGoPhast.PhastModel.Pathlines.Draw(Orientation, FBitmap32);
  end;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if (ChildModel.Pathlines.HasData)then
      begin
        ChildModel.Pathlines.Draw(Orientation, FBitmap32);
      end;
    end;
  end;
end;

procedure TframeView.DrawEndPoints;
var
  Orientation: TDataSetOrientation;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Orientation := dsoTop;
  case ViewDirection of
    vdTop: Orientation := dsoTop;
    vdFront: Orientation := dsoFront;
    vdSide: Orientation := dsoSide;
    else Assert(False);
  end;
  if (frmGoPhast.PhastModel.EndPoints.HasData) then
  begin
    frmGoPhast.PhastModel.EndPoints.Draw(Orientation, FBitmap32);
  end;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel.EndPoints.HasData then
      begin
        ChildModel.EndPoints.Draw(Orientation, FBitmap32);
      end;
    end;
  end;
end;

procedure TframeView.DrawTimeSeries;
var
  Orientation: TDataSetOrientation;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Orientation := dsoTop;
  case ViewDirection of
    vdTop: Orientation := dsoTop;
    vdFront: Orientation := dsoFront;
    vdSide: Orientation := dsoSide;
    else Assert(False);
  end;
  if (frmGoPhast.PhastModel.TimeSeries.HasData) then
  begin
    frmGoPhast.PhastModel.TimeSeries.Draw(Orientation, FBitmap32);
  end;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if (ChildModel.TimeSeries.HasData) then
      begin
        ChildModel.TimeSeries.Draw(Orientation, FBitmap32);
      end;
    end;
  end;
end;

procedure TframeView.DrawScreenObjects;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
    // @name will draw the @link(TScreenObject)s on @link(FBitMap32).
  Assert(FBitmap32 <> nil);
  try
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1
      do
    begin
      if ScreenObjectsHaveChanged then
      begin
        ScreenObjectsHaveChanged := True;
        Exit;
      end;
      AScreenObject :=
        frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
//      if (AScreenObject <> CurrentScreenObject) then
      begin
        AScreenObject.Draw(FBitmap32, ViewDirection);
      end;
    end;
    GridChanged := False;

  except on EInvalidGraphicOperation do
    begin
      WarnTooBig;
    end;
  end;
  FPreviousScreenObjects.Clear;
end;

function TframeView.EvaluatedAt: TEvaluatedAt;
begin
    // See TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.EvaluatedAt).
  result := ColRowLayerSelector.EvaluatedAt;
end;

procedure TframeView.ZoomBoxExit(Sender: TObject);
begin
    // If the user clicks on a button or somewhere else while creating a
    // @link(TScreenObject), the @link(TScreenObject) is terminated.
    // @name is the @link(ZoomBox).OnExit event handler.
  // If the user clicks on a button or somewhere else, the screen object
  // is terminated.
  FinishScreenObjects;
end;

procedure TframeView.miUnselectAllClick(Sender: TObject);
begin
  UnSelectAll;
end;

procedure TframeView.UpdateSelectRectangle;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  First: boolean;
  Temp, TempL, TempH: integer;
begin
    // @name calculates the coordinates of a rectangle that surrounds
    // the selected objects.

  // This procedure calculates the coordinates of a rectangle that surrounds
  // the selected objects.
  First := True;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if (AScreenObject.ViewDirection = ViewDirection) and
      AScreenObject.Selected
      then
    begin
      if First then
      begin
        First := False;
        FSelectTopLeft.X := ZoomBox.XCoord(AScreenObject.MaxX);
        FSelectTopLeft.Y := ZoomBox.YCoord(AScreenObject.MaxY);
        FSelectBottomRight.X := ZoomBox.XCoord(AScreenObject.MinX);
        FSelectBottomRight.Y := ZoomBox.YCoord(AScreenObject.MinY);
        if FSelectTopLeft.X > FSelectBottomRight.X then
        begin
          Temp := FSelectTopLeft.X;
          FSelectTopLeft.X := FSelectBottomRight.X;
          FSelectBottomRight.X := Temp;
        end;
        if FSelectTopLeft.Y > FSelectBottomRight.Y then
        begin
          Temp := FSelectTopLeft.Y;
          FSelectTopLeft.Y := FSelectBottomRight.Y;
          FSelectBottomRight.Y := Temp;
        end;
      end
      else
      begin
        TempL := ZoomBox.XCoord(AScreenObject.MinX);
        TempH := ZoomBox.XCoord(AScreenObject.MaxX);
        if TempH < TempL then
        begin
          Temp := TempH;
          TempH := TempL;
          TempL := Temp;
        end;
        if FSelectTopLeft.X > TempL then
        begin
          FSelectTopLeft.X := TempL
        end;
        if FSelectBottomRight.X < TempH then
        begin
          FSelectBottomRight.X := TempH
        end;
        TempL := ZoomBox.YCoord(AScreenObject.MinY);
        TempH := ZoomBox.YCoord(AScreenObject.MaxY);
        if TempH < TempL then
        begin
          Temp := TempH;
          TempH := TempL;
          TempL := Temp;
        end;
        if FSelectTopLeft.Y > TempL then
        begin
          FSelectTopLeft.Y := TempL
        end;
        if FSelectBottomRight.Y < TempH then
        begin
          FSelectBottomRight.Y := TempH
        end;
      end;
    end;
  end;
  if First then
  begin
    FSelectTopLeft.X := -1;
    FSelectTopLeft.Y := -1;
    FSelectBottomRight.X := -1;
    FSelectBottomRight.Y := -1;
  end;
end;

procedure TframeView.DrawSelectedAndOldScreenObjects(const BitMap:
  TBitmap32);
var
  AScreenObject: TScreenObject;
  Index: Integer;
  ErrorMessages: TStringList;
begin
  // Nothing here should take long to draw.

  ErrorMessages := TStringList.Create;
  try
    for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      try
        AScreenObject.DrawSelected(BitMap, ViewDirection);
      except on E: ECircularRefScreenObjectError do
        ErrorMessages.Add(E.message);
      end;
    end;
    PaintOldScreenObjects(BitMap);
    if ErrorMessages.Count > 0 then
    begin
      Beep;
      MessageDlg(ErrorMessages.Text, mtError, [mbOK], 0);
    end;
  finally
    ErrorMessages.Free;
  end;
end;

procedure TframeView.PaintOldScreenObjects(const BitMap: TBitmap32);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  ErrorMessages: TStringList;
begin
  // @name draws the current @link(TScreenObject) and previous
  // @link(TScreenObject) to the @link(ZoomBox.Image32) bitmap
  ErrorMessages := TStringList.Create;
  try
    for Index := 0 to FPreviousScreenObjects.Count - 1 do
    begin
      AScreenObject := FPreviousScreenObjects[Index];
      try
        AScreenObject.Draw(BitMap, ViewDirection);
      except on E: ECircularRefScreenObjectError do
        ErrorMessages.Add(E.message);
      end;
    end;
    if (BitMap <> nil) and (CurrentScreenObject <> nil) then
    begin
      try
        CurrentScreenObject.Draw(BitMap, ViewDirection);
      except on E: ECircularRefScreenObjectError do
        ErrorMessages.Add(E.message);
      end;
    end;
    if ErrorMessages.Count > 0 then
    begin
      Beep;
      MessageDlg(ErrorMessages.Text, mtError, [mbOK], 0);
    end;
  finally
    ErrorMessages.Free;
  end;
end;

procedure TframeView.ToFrontClick(Sender: TObject);
var
  UndoToFront: TUndoToFront;
begin
    // @name allows the user to move the selected @link(TScreenObject)s
    // in front of all other @link(TScreenObject)s.
  // This procedure allows the user to move selected objects in front
  // of all other objects.
  UndoToFront := TUndoToFront.Create(ViewDirection);
  UndoToFront.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoToFront);
end;

procedure TframeView.ToBackClick(Sender: TObject);
var
  UndoToBack: TUndoToBack;
begin
    // @name allows the user to move the selected @link(TScreenObject)s behind
    // all other @link(TScreenObject)s.
  // This procedure allows the user to move the selected objects behind
  // all other objects.
  UndoToBack := TUndoToBack.Create(ViewDirection);
  UndoToBack.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoToBack);
end;

procedure TframeView.ForwardOneClick(Sender: TObject);
var
  UndoMoveUp: TUndoMoveUp;
begin
    // @name allows the user to move selected objects forward one
    // in the list of objects so that they move forward of one other object.

  // This procedure allows the user to move selected objects forward one
  // in the list of objects so that they move forward of one other object.
  UndoMoveUp := TUndoMoveUp.Create(ViewDirection);
  UndoMoveUp.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoMoveUp);
end;

procedure TframeView.BackOneClick(Sender: TObject);
var
  UndoMoveDown: TUndoMoveDown;
begin
    // @name allows the user to move selected objects back one
    // in the list of objects so that they move behind one other object.

  // This procedure allows the user to move selected objects back one
  // in the list of objects so that they move behind one other object.
  UndoMoveDown := TUndoMoveDown.Create(ViewDirection);
  UndoMoveDown.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoMoveDown);
end;

procedure TframeView.ModelCubeMouseEnter(Sender: TObject);
begin
  frmGoPhast.FCubeControl := ModelCube;
end;

procedure TframeView.ModelCubeMouseLeave(Sender: TObject);
begin
  frmGoPhast.FCubeControl := nil;
end;

procedure TframeView.ModelCubePaint(Sender: TObject);
begin
    // @name draws a black rectangle outlining the @link(ModelCube) component.
  ModelCube.Canvas.Rectangle(0, 0, ModelCube.Width, ModelCube.Height);
end;

procedure TframeView.OrderMenuPopup(Sender: TObject);
var
  Index: integer;
  AScreenObject: TScreenObject;
  notSelectedFound: boolean;
begin
    // @name enables or disables @link(ToFront), @link(ToBack),
    // @link(ForwardOne), @link(BackOne), and @link(miHide) depending on the
    // context.
  ToBack.Enabled := False;
  BackOne.Enabled := False;
  ToFront.Enabled := False;
  ForwardOne.Enabled := False;
  miHide.Enabled := False;
  notSelectedFound := False;
  miUnselectAll.Enabled := False;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      miHide.Enabled := True;
      miUnselectAll.Enabled := True;
      if notSelectedFound then
      begin
        ToBack.Enabled := True;
        BackOne.Enabled := True;
        break;
      end;
    end
    else
    begin
      notSelectedFound := True;
    end;
  end;
  miInvertSelectedVertices.Enabled := miHide.Enabled;
  notSelectedFound := False;
  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      if notSelectedFound then
      begin
        ToFront.Enabled := True;
        ForwardOne.Enabled := True;
        break;
      end;
    end
    else
    begin
      notSelectedFound := True;
    end;
  end;
end;

procedure TframeView.GetNodeColLayer(APoint: TPoint2D; out Col, Layer:
  integer);
begin
    // @name returns the column and layer numbers of the cell containing APoint;
    // APoint must be on a side view of the model.

  // Get the column and layer numbers of the cell containing APoint;
  Layer := frmGoPhast.PhastModel.SelectedModel.PhastGrid.NearestLayerPosition(APoint.Y);
  Col := frmGoPhast.PhastModel.SelectedModel.Grid.NearestColumnPosition(APoint.X);
  if (Col < 0) or (Layer < 0)
    or (Col > frmGoPhast.PhastModel.SelectedModel.Grid.ColumnCount)
    or (Layer > frmGoPhast.PhastModel.SelectedModel.Grid.LayerCount) then
  begin
    Layer := -1;
    Col := -1;
  end;
end;

procedure TframeView.GetMeshColLayer(APoint: TPoint2D; out NodeCol, NodeLayer,
  ElCol, ElLayer: integer);
var
//  Mesh: TSutraMesh3D;
  NodeList: TSutraNode2D_List;
  SegmentAngle: Double;
  StartPoint: TPoint2D;
  Node2D_Index: Integer;
  Node2D: TSutraNode2D;
  Angle: Extended;
  X_Float: TFloat;
  TestValue: TFloat;
  LayerIndex: Integer;
  ClosestNodeIndex: Integer;
  ClosestLayerIndex: Integer;
  FirstFound: Boolean;
  Limits: TLimitsArray;
  NodePolygons: TCellElementPolygons2D;
  ElementList: TIElement2DList;
  ElementIndex: Integer;
//  AnElement2D: TSutraElement2D;
  ElementPolygons: TCellElementPolygons2D;
  Closest2DElement: IElement2D;
//  Element3D: TSutraElement3D;
  ElCenter: TPoint2D;
  CenterDistance: TFloat;
  Node3D: TSutraNode3D;
  ElCenter2D: TPoint2D;
  Mesh: IMesh3D;
  AnElement2D: IElement2D;
  Element3D: IElement3D;
  DrawMesh: IDrawMesh;
//  ElementList3D: TSutraElement3DList;
begin
  NodeCol := -1;
  NodeLayer := -1;
  ElCol := -1;
  ElLayer := -1;
  Mesh := frmGoPhast.PhastModel.SelectedModel.Mesh3D;
  DrawMesh := frmGoPhast.PhastModel.SelectedModel.DrawMesh;

  if Mesh = nil then
  begin
    Exit;
  end;
  if not Mesh.Is3DMesh then
  begin
    Exit;
  end;
  NodeList := TSutraNode2D_List.Create;
  ElementList := TIElement2DList.Create;
  try
    if Mesh is TSutraMesh3D then
    begin
      (Mesh as TSutraMesh3D).GetNodesOnCrossSection(NodeList);
      if NodeList.Count = 0 then
      begin
        Exit;
      end;
    end;
    Mesh.GetElementsIntfOnCrossSection(ElementList);
    if ElementList.Count = 0 then
    begin
      Exit;
    end;

    SegmentAngle := DrawMesh.CrossSection.Angle;
//    StartPoint := Mesh.GetCrossSectionStart;
    StartPoint := EquatePoint(0,0);
    ClosestNodeIndex := -1;
    TestValue := 0;
//    Node2D := nil;
    if NodeList.Count > 0 then
    begin
      for Node2D_Index := 0 to NodeList.Count - 1 do
      begin
        Node2D := NodeList[Node2D_Index];
        Angle := ArcTan2(Node2D.y - StartPoint.y,
          Node2D.x - StartPoint.x) - SegmentAngle;
        X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
          + StartPoint.x;
        X_Float := Abs(APoint.x - X_Float);
        if Node2D_Index = 0 then
        begin
          ClosestNodeIndex := Node2D_Index;
          TestValue := X_Float;
        end
        else if X_Float < TestValue then
        begin
          ClosestNodeIndex := Node2D_Index;
          TestValue := X_Float;
        end;
      end;
      Node2D := NodeList[ClosestNodeIndex];
      Angle := ArcTan2(Node2D.y - StartPoint.y,
        Node2D.x - StartPoint.x) - SegmentAngle;
      X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
        + StartPoint.x;
      if (ClosestNodeIndex = 0) and (X_Float < APoint.x) then
      begin
        Exit;
      end;
      if (ClosestNodeIndex = NodeList.Count -1) and (X_Float > APoint.x) then
      begin
        Exit;
      end;

      Assert( Mesh.Is3DMesh);
      ClosestLayerIndex := -1;
  //    FirstFound := False;
      NodePolygons := Mesh.FrontPolygons(SegmentAngle, eaNodes, Limits);
      for LayerIndex := 0 to Mesh.LayerCount do
      begin
        Node3D := (Mesh as TSutraMesh3D).NodeArray[LayerIndex, Node2D.Number];
        if Node3D.Active then
        begin
          if ModelMuseUtilities.PointInConcavePolygon(APoint,
            NodePolygons[LayerIndex, Node2D.Number]) then
          begin
            ClosestLayerIndex := LayerIndex;
            Break;
          end;
        end;
      end;
      if ClosestLayerIndex < 0 then
      begin
        Exit;
      end;
      NodeLayer := ClosestLayerIndex;
      NodeCol := Node2D.Number;

      for ElementIndex := ElementList.Count - 1 downto 0 do
      begin
        AnElement2D := ElementList[ElementIndex];
        if (AnElement2D as TSutraElement2D).Nodes.IndexOfNode(Node2D) < 0 then
        begin
          ElementList.Delete(ElementIndex);
        end;
      end;
    end
    else
    begin
      NodeLayer := -1;
      NodeCol := -1;
    end;

    ElementPolygons := Mesh.FrontPolygons(SegmentAngle, eaBlocks, Limits);

    FirstFound := False;
    Closest2DElement := nil;
    TestValue := 0;
    ClosestLayerIndex := -1;
    for ElementIndex := 0 to ElementList.Count - 1 do
    begin
      AnElement2D := ElementList[ElementIndex];
      ElCenter2D := AnElement2D.Center;
      Angle := ArcTan2(ElCenter2D.y - StartPoint.y,
        ElCenter2D.x - StartPoint.x) - SegmentAngle;
      ElCenter.X := Distance(StartPoint, ElCenter2D)*Cos(Angle)
            + StartPoint.x;
      for LayerIndex := 0 to Mesh.LayerCount - 1 do
      begin
        if ModelMuseUtilities.PointInConcavePolygon(APoint,
          ElementPolygons[LayerIndex, AnElement2D.ElementNumber]) then
        begin
          Element3D := Mesh.ElementArrayI[LayerIndex,AnElement2D.ElementNumber];
          if Element3D.Active then
          begin
            ElCenter.y := Element3D.CenterElevation;
            CenterDistance := Distance(APoint, ElCenter);
            if not FirstFound then
            begin
              ClosestLayerIndex := LayerIndex;
              Closest2DElement := AnElement2D;
              TestValue := CenterDistance;
              FirstFound := True;
            end
            else if CenterDistance < TestValue then
            begin
              ClosestLayerIndex := LayerIndex;
              Closest2DElement := AnElement2D;
              TestValue := CenterDistance;
            end;
          end;
        end;
      end;
    end;
    if Closest2DElement = nil then
    begin
      ElCol := -1;
      ElLayer := -1;
    end
    else
    begin
      ElCol := Closest2DElement.ElementNumber;
      ElLayer := ClosestLayerIndex;
    end;
  finally
    NodeList.Free;
    ElementList.Free;
  end;
end;

procedure TframeView.GetColLayer(APoint: TPoint2D; out Col, Layer:
  integer);
var
  NeighborCol, NeighborLayer: integer;
  Grid: TCustomModelGrid;
begin
    // @name gets the column and layer numbers
    // of the element containing APoint;
    // APoint must be on a side view of the model.
  Grid := frmGoPhast.PhastModel.SelectedModel.Grid;
  if (Grid = nil) or (Grid.ColumnCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.LayerCount <= 0) then
  begin
    Layer := -1;
    Col := -1;
    Exit;
  end;


  // Get the column and layer numbers of the cell containing APoint;
  Col := Grid.NearestColumnPosition(APoint.X);
  case EvaluatedAt of
    eaBlocks:
      begin
        if (Col < 0) or (Col > Grid.ColumnCount) then
        begin
          Layer := -1;
          Col := -1;
          Exit;
        end;

        if Grid.ColumnDirection = cdWestToEast then
        begin
          if APoint.X > Grid.ColumnPosition[Col] then
          begin
            NeighborCol := Col + 1;
          end
          else
          begin
            NeighborCol := Col - 1;
          end;
        end
        else
        begin
          if APoint.X > Grid.ColumnPosition[Col] then
          begin
            NeighborCol := Col - 1;
          end
          else
          begin
            NeighborCol := Col + 1;
          end;
        end;

        if (NeighborCol >= 0) and
          (NeighborCol <= Grid.ColumnCount) then
        begin
          if NeighborCol < Col then
          begin
            Col := NeighborCol;
          end;
        end
        else
        begin
          Layer := -1;
          Col := -1;
          Exit;
        end;
      end;
    eaNodes:
      begin
        if (APoint.X < Grid.ColumnPosition[0])
          or (APoint.X > Grid.ColumnPosition[
            Grid.ColumnCount])
          or (APoint.Y < frmGoPhast.PhastGrid.LayerElevation[0])
          or (APoint.Y > frmGoPhast.PhastGrid.LayerElevation[
            frmGoPhast.PhastGrid.LayerCount])
          then
        begin
          Layer := -1;
          Col := -1;
          Exit;
        end;
      end;
    else Assert(False)
  end;

  Layer := Grid.Nearest2DCellElevation(Col,
    frmGoPhast.PhastModel.SelectedModel.SelectedRow, APoint.Y);
  case EvaluatedAt of
    eaBlocks:
      begin
        if (Layer < 0)
          or (Layer > Grid.LayerCount) then
        begin
          Layer := -1;
          Col := -1;
          Exit;
        end;

        if Grid.LayerDirection = ldBottomToTop then
        begin
          if APoint.Y > Grid.CellElevation[Col,
            frmGoPhast.PhastModel.SelectedModel.SelectedRow, Layer] then
          begin
            NeighborLayer := Layer + 1;
          end
          else
          begin
            NeighborLayer := Layer - 1;
          end;
        end
        else
        begin
          if APoint.Y > Grid.CellElevation[Col,
            frmGoPhast.PhastModel.SelectedModel.SelectedRow, Layer] then
          begin
            NeighborLayer := Layer - 1;
          end
          else
          begin
            NeighborLayer := Layer + 1;
          end;
        end;
        if (NeighborLayer >= 0)
          and (NeighborLayer <= Grid.LayerCount) then
        begin
          if NeighborLayer < Layer then
          begin
            Layer := NeighborLayer;
          end;
        end
        else
        begin
          Layer := -1;
          Col := -1;
        end;
      end;
    eaNodes:
      begin
        if (APoint.X < Grid.ColumnPosition[0])
          or (APoint.X > Grid.ColumnPosition[
            frmGoPhast.Grid.ColumnCount])
          or (APoint.Y < frmGoPhast.PhastGrid.LayerElevation[0])
          or (APoint.Y > frmGoPhast.PhastGrid.LayerElevation[
            frmGoPhast.PhastGrid.LayerCount])
          then
        begin
          Layer := -1;
          Col := -1;
          Exit;
        end;
      end;
    else Assert(False)
  end;
end;

procedure TframeView.GetNodeRowLayer(APoint: TPoint2D; out Row, Layer:
  integer);
begin
    // @name returns the row and layer numbers of the cell containing APoint;
    // APoint must be on a front view of the model.
  Layer := frmGoPhast.PhastModel.SelectedModel.PhastGrid.NearestLayerPosition(APoint.X);
  Row := frmGoPhast.PhastModel.SelectedModel.Grid.NearestRowPosition(APoint.Y);
  if (Row < 0) and (Layer < 0)
    and (Row > frmGoPhast.PhastModel.SelectedModel.Grid.RowCount)
    and (Layer > frmGoPhast.PhastModel.SelectedModel.Grid.LayerCount) then
  begin
    Layer := -1;
    Row := -1;
  end;
end;

procedure TframeView.GetRowLayer(APoint: TPoint2D; out Row, Layer:
  integer);
var
  NeighborRow, NeighborLayer: integer;
  Grid: TCustomModelGrid;
begin
    // @name gets the row and layer numbers of the element containing APoint;
    // APoint must be on a side view of the model.
  Grid := frmGoPhast.PhastModel.SelectedModel.Grid;
  if (Grid = nil) or (Grid.RowCount <= 0) or (Grid.ColumnCount <= 0)
    or (Grid.LayerCount <= 0) then
  begin
    Layer := -1;
    Row := -1;
    Exit;
  end;


  // Get the row and layer numbers of the cell containing APoint;
  Row := Grid.NearestRowPosition(APoint.Y);

  case EvaluatedAt of
    eaBlocks:
      begin
        if (Row < 0)
          or (Row > Grid.RowCount) then
        begin
          Layer := -1;
          Row := -1;
          Exit;
        end;
        if Grid.RowDirection = rdSouthToNorth then
        begin
          if APoint.Y > Grid.RowPosition[Row] then
          begin
            NeighborRow := Row + 1;
          end
          else
          begin
            NeighborRow := Row - 1;
          end;
        end
        else
        begin
          if APoint.Y > Grid.RowPosition[Row] then
          begin
            NeighborRow := Row - 1;
          end
          else
          begin
            NeighborRow := Row + 1;
          end;
        end;

        if (NeighborRow >= 0)
          and (NeighborRow <= Grid.RowCount) then
        begin
          if NeighborRow < Row then
          begin
            Row := NeighborRow;
          end;
        end
        else
        begin
          Layer := -1;
          Row := -1;
          Exit;
        end;
      end;
    eaNodes:
      begin
        if (APoint.Y < frmGoPhast.PhastGrid.RowPosition[0])
          or (APoint.Y > frmGoPhast.PhastGrid.RowPosition[
            frmGoPhast.PhastGrid.RowCount])
          or (APoint.X < frmGoPhast.PhastGrid.LayerElevation[0])
          or (APoint.X > frmGoPhast.PhastGrid.LayerElevation[
            frmGoPhast.PhastGrid.LayerCount])
          then
        begin
          Layer := -1;
          Row := -1;
          Exit;
        end;
      end;
    else Assert(False);
  end;

  Layer := Grid.Nearest2DCellElevation(
    frmGoPhast.PhastModel.SelectedModel.SelectedColumn, Row, APoint.X);

  case EvaluatedAt of
    eaBlocks:
      begin
        if (Layer < 0)
          or (Layer > Grid.LayerCount) then
        begin
          Layer := -1;
          Row := -1;
          Exit;
        end;
        if Grid.LayerDirection = ldBottomToTop then
        begin
          if APoint.X > Grid.CellElevation[
            frmGoPhast.PhastModel.SelectedModel.SelectedColumn, Row, Layer] then
          begin
            NeighborLayer := Layer + 1;
          end
          else
          begin
            NeighborLayer := Layer - 1;
          end;
        end
        else
        begin
          if APoint.X > Grid.CellElevation[
            frmGoPhast.PhastModel.SelectedModel.SelectedColumn, Row, Layer] then
          begin
            NeighborLayer := Layer - 1;
          end
          else
          begin
            NeighborLayer := Layer + 1;
          end;
        end;

        if (NeighborLayer < 0)
          or (NeighborLayer > Grid.LayerCount) then
        begin
          Layer := -1;
          Row := -1;
          Exit;
        end;

        if NeighborLayer < Layer then
        begin
          Layer := NeighborLayer;
        end;
      end;
    eaNodes:
      begin
        if (APoint.Y < frmGoPhast.PhastGrid.RowPosition[0])
          or (APoint.Y > frmGoPhast.PhastGrid.RowPosition[
            frmGoPhast.PhastGrid.RowCount])
          or (APoint.X < frmGoPhast.PhastGrid.LayerElevation[0])
          or (APoint.X > frmGoPhast.PhastGrid.LayerElevation[
            frmGoPhast.PhastGrid.LayerCount])
          then
        begin
          Layer := -1;
          Row := -1;
          Exit;
        end;
      end;
    else Assert(False);
  end;

end;

function TframeView.CursorGrid: TCursorGrid;
begin
    // @name is used to indicate which view of the model
    // (if any) the cursor is over.
  case ViewDirection of
    vdTop: result := cgTop;
    vdFront: result := cgFront;
    vdSide: result := cgSide
  else
    begin
      result := cgNone;
    end;
  end;
end;

procedure TframeView.DisplayItem;
begin
    // @name displays the column, row, or layer that is selected
    // on the status bar.
  ColRowLayerSelector.DisplayItem;
end;

procedure TframeView.SetDeltaGridAngle(const Value: real);
begin
    // See @link(DeltaGridAngle).
  if FDeltaGridAngle <> Value then
  begin
    FDeltaGridAngle := Value;
    ZoomBox.InvalidateImage32;
  end;
end;

procedure TframeView.ResetCellColors;
begin
    // @name sets to the colors of each cell or element in the grid to White.
  if frmGoPhast.Grid <> nil then
  begin
    case ViewDirection of
      vdTop:
        begin
          frmGoPhast.Grid.ResetTopCellColors
        end;
      vdFront:
        begin
          frmGoPhast.Grid.ResetFrontCellColors
        end;
      vdSide:
        begin
          frmGoPhast.Grid.ResetSideCellColors
        end;
    else
      Assert(False);
    end;
  end;

end;

procedure TframeView.AdjustScales;
begin
    // @name adjusts the scales on @link(rulHorizontal) and @link(rulVertical)
    // to match the coordinates in @link(ZoomBox).
  AdjustHorizontalScale(0);
  AdjustVerticalScale(0);
  if (frmGoPhast.PhastModel <> nil)
    and (frmGoPhast.PhastModel.Bitmaps <> nil)
    and not (csDestroying in ComponentState) then
  begin
    frmGoPhast.PhastModel.Bitmaps.InvalidateView(ViewDirection);
  end;
end;

procedure TframeView.DrawRotatedGrid(const DeltaAngle: real; BitMap:
  TBitmap32);
var
  GridOutline: TPointArray;
  P: TPolygon32;
  MultiplePolygons: boolean;
begin
  P := nil;
  MultiplePolygons := False;
    // @name draws an outline of the grid after it is rotated by DeltaAngle.
  SetLength(GridOutline, 5);

  RotatedGridCorners(DeltaAngle, GridOutline);

  DrawBigPolygon32(BitMap, clBlack32, clTransparent32, 1,
    GridOutline, P, MultiplePolygons, True);
end;

function TframeView.GridCenter: TPoint2D;
var
  Corners: array[0..MaxGridCorners - 1] of TPoint2D;
  Index: integer;
begin
    // @name returns a @link(TPoint2D) for the center of the grid.
  result.X := 0;
  result.Y := 0;
  if (frmGoPhast.Grid.RowCount > 0) and
    (frmGoPhast.Grid.ColumnCount > 0) then
  begin
    Corners[0] := frmGoPhast.Grid.TwoDElementCorner(0, 0);
    Corners[1] := frmGoPhast.Grid.TwoDElementCorner(0,
      frmGoPhast.Grid.RowCount);
    Corners[2] :=
      frmGoPhast.Grid.TwoDElementCorner(frmGoPhast.Grid.ColumnCount,
      frmGoPhast.Grid.RowCount);
    Corners[3] :=
      frmGoPhast.Grid.TwoDElementCorner(
      frmGoPhast.Grid.ColumnCount, 0);
  end
  else
  begin
    Exit;
  end;

  for Index := 0 to MaxGridCorners - 1 do
  begin
    result.X := Corners[Index].X + result.X;
    result.Y := Corners[Index].Y + result.Y;
  end;
  result.X := result.X / MaxGridCorners;
  result.Y := result.Y / MaxGridCorners;
end;

function TframeView.GetTopDisplayDataSet: TDataArray;
//var
//  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
begin
//  result := nil;
//  case frmGoPhast.PhastModel.ModelSelection of
//    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
//      msModflowFmp, msModflowCfp, msModflow2015, msFootPrint:
//      begin
        result := frmGoPhast.PhastModel.SelectedModel.TopDataSet;
//        if Grid <> nil then
//        begin
//          result := Grid.TopDataSet;
          if result = nil then
          begin
            result := frmGoPhast.PhastModel.SelectedModel.TopContourDataSet;
//          end;
          end;
//      end;
//    msSutra22, msSutra30:
//      begin
//        Mesh := frmGoPhast.PhastModel.SutraMesh;
//        if Mesh <> nil then
//        begin
//          result := Mesh.TopDataSet;
//          if result = nil then
//          begin
//            result := Mesh.TopContourDataSet;
//          end;
//        end;
//      end;
//  else
//    result := nil;
//    Assert(False);
//  end;
end;

procedure TframeView.AllowBitmapsToBeDisplayed;
var
  BitmapIndex: Integer;
  Item: TCompressedBitmapItem;
begin
  If (FPreviousMagnification = 0) or (FPreviousMagnification >= ZoomBox.Magnification) then
  begin
    for BitmapIndex := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
    begin
      Item := frmGoPhast.PhastModel.Bitmaps.Items[BitmapIndex] as TCompressedBitmapItem;
      Item.CanShow := True;
    end;
    FPreviousMagnification := ZoomBox.Magnification;
  end;
end;

procedure TframeView.HideScreenObjects(HideSelected: Boolean);
var
  Count: Integer;
  UndoHide: TUndoShowHideScreenObject;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  Count := -1;
  UndoHide := TUndoShowHideScreenObject.Create;
  try
    for ScreenObjectIndex := 0 to
      frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted and AScreenObject.Visible and
        (AScreenObject.Selected = HideSelected) then
      begin
        Count := UndoHide.AddScreenObjectToChange(AScreenObject);
      end;
    end;
    UndoHide.SetPostSelection;
    if Count >= 0 then
    begin
      frmGoPhast.UndoStack.Submit(UndoHide);
    end
    else
    begin
      UndoHide.Free;
    end;
  except
    UndoHide.Free;
    raise ;
  end;
end;

procedure TframeView.ShowAll1Click(Sender: TObject);
var
  Count: Integer;
  UndoHide: TUndoShowHideScreenObject;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  Count := -1;
  UndoHide := TUndoShowHideScreenObject.Create;
  try
    for ScreenObjectIndex := 0 to
      frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted and not AScreenObject.Visible then
      begin
        Count := UndoHide.AddScreenObjectToChange(AScreenObject);
      end;
    end;
    UndoHide.SetPostSelection;
    if Count >= 0 then
    begin
      frmGoPhast.UndoStack.Submit(UndoHide);
    end
    else
    begin
      UndoHide.Free;
    end;
  except
    UndoHide.Free;
    raise ;
  end;
end;

procedure TframeView.ShowCurrentValue(DataSet: TDataArray;
  const NameToDisplay: string; Column, Row, Layer, GlobalColumn, GlobalRow,
  GlobalLayer: Integer; const Location: TPoint2D);
var
  ValueToDisplay: string;
  Explanation: string;
begin
  if not DataSet.UpToDate then
  begin
    Exit;
  end;
  case DataSet.Datatype of
    rdtDouble:
      begin
        ValueToDisplay := FloatToStr(DataSet.RealData[Layer, Row, Column]);
        Explanation := DataSet.Annotation[Layer, Row, Column];
      end;
    rdtInteger:
      begin
        if (DataSet is TIntegerPhastDataSet)
          and TIntegerPhastDataSet(DataSet).
          IsInterpolatedCell[Layer, Row, Column] then
        begin
          ValueToDisplay := FloatToStr(TIntegerPhastDataSet(DataSet).
            RealValue[Layer, Row, Column]);
          Explanation := Format(Str0sEndMemberValInt,
            [DataSet.Annotation[Layer, Row, Column],
            TIntegerPhastDataSet(DataSet).CellValue1[Layer, Row, Column],
            TIntegerPhastDataSet(DataSet).CellValue2[Layer, Row, Column]
            ]);
        end
        else if DataSet is TSparseIntegerPhastDataSet
          and TSparseIntegerPhastDataSet(DataSet).
          IsInterpolatedCell[Layer, Row, Column] then
        begin
          ValueToDisplay := FloatToStr(TSparseIntegerPhastDataSet(DataSet).
            RealValue[Layer, Row, Column]);
          Explanation := Format(Str0sEndMemberValInt,
            [DataSet.Annotation[Layer, Row, Column],
            TSparseIntegerPhastDataSet(DataSet).CellValue1[Layer, Row, Column],
            TSparseIntegerPhastDataSet(DataSet).CellValue2[Layer, Row, Column]
            ]);
        end
        else
        begin
          ValueToDisplay := IntToStr(DataSet.IntegerData[Layer, Row, Column]);
          Explanation := DataSet.Annotation[Layer, Row, Column];
        end;
      end;
    rdtBoolean:
      begin
        if DataSet.BooleanData[Layer, Row, Column] then
        begin
          ValueToDisplay := StrTrue;
        end
        else
        begin
          ValueToDisplay :=  StrFalse;
        end;
        Explanation := DataSet.Annotation[Layer, Row, Column];
      end;
    rdtString:
      begin
        ValueToDisplay := DataSet.StringData[Layer, Row, Column];
        Explanation := DataSet.Annotation[Layer, Row, Column];
      end;
  else
    Assert(False);
  end;

  if (Explanation <> StrNoValueAssigned)
    and (Pos(StrNoValueAssigned, Explanation) > 0) then
  begin
    Explanation := StringReplace(Explanation, StrNoValueAssigned, '', []);
  end;

  frmGoPhast.sbMain.Panels[2].Text := ValueToDisplay + ': ' + NameToDisplay
    + '; ' + StringReplace(Explanation, sLineBreak, ' ', [rfReplaceAll]);
  if frmGridValue <> nil then
  begin
    frmGridValue.UpdateValue(GlobalLayer, GlobalRow, GlobalColumn,
      NameToDisplay, ValueToDisplay, Explanation, Location, ViewDirection,
      DataSet.EvaluatedAt);
  end;
end;

procedure TframeView.UnSelectAll;
var
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Undo: TUndoChangeSelection;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Undo := TUndoChangeSelection.Create;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Selected
      and (ScreenObject.ViewDirection = ViewDirection) then
    begin
      ScreenObject.Selected := False;
    end;
  end;
  Undo.SetPostSelection;
  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TframeView.SelectAll;
var
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Undo: TUndoChangeSelection;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Undo := TUndoChangeSelection.Create;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Visible
      and (ScreenObject.ViewDirection = ViewDirection) then
    begin
      ScreenObject.Selected := True;
    end
    else
    begin
      ScreenObject.Selected := False;
    end;
  end;
  Undo.SetPostSelection;
  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TframeView.UpdateStatusBarSideNodeDataSet(Row, Layer: Integer; const Location: TPoint2D);
var
  NameToDisplay: string;
  Column: Integer;
  DataSet: TDataArray;
  TrueColumn: Integer;
  Grid: TCustomModelGrid;
begin
  // Make sure the grid is updated.
  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount > 0)
    and (Grid.RowCount > 0) and (Grid.ColumnCount > 0) then
  begin
    Grid.ThreeDElementCenter(0,0,0);
  end;

  Column := frmGoPhast.PhastModel.SelectedColumn;
  TrueColumn := Column;
  // Display the value of the current data set (if any).
  DataSet := frmGoPhast.Grid.SideDataSet;
  if DataSet = nil then
  begin
    DataSet := frmGoPhast.Grid.SideContourDataSet;
  end;
  if (DataSet <> nil) then
  begin
    if DataSet.Orientation = dso3D then
    begin
      Column := frmGoPhast.PhastModel.SelectedColumn;
    end
    else
    begin
      Column := 0;
    end;
    if frmGoPhast.PhastModel.SideTimeList = nil then
    begin
      NameToDisplay := DataSet.Name;
    end
    else
    begin
      NameToDisplay := frmGoPhast.PhastModel.SideTimeList.Name;
    end;
    if DataSet.IsValue[Layer, Row, Column] and (Column >= 0)
      and (Column <= frmGoPhast.Grid.ColumnCount) then
    begin
      ShowCurrentValue(DataSet, NameToDisplay, Column, Row, Layer,
        TrueColumn, Row, Layer, Location);
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, Row, TrueColumn, NameToDisplay, '', '',
          Location, ViewDirection, DataSet.EvaluatedAt);
      end;
    end;
  end
  else
  begin
    frmGoPhast.sbMain.Panels[2].Text := '';
    if frmGridValue <> nil then
    begin
      frmGridValue.UpdateValue(Layer, Row, TrueColumn, '', '', '', Location,
        ViewDirection, eaBlocks);
    end;
  end;
end;

procedure TframeView.UpdateStatusBarSideBlockDataSet(Row, Layer: Integer;
  const Location: TPoint2D);
var
  NameToDisplay: string;
  Column: Integer;
  DataSet: TDataArray;
  TrueColumn: Integer;
  Grid: TCustomModelGrid;
begin
  // Make sure the grid is updated.
  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount > 0)
    and (Grid.RowCount > 0) and (Grid.ColumnCount > 0) then
  begin
    Grid.ThreeDElementCenter(0,0,0);
  end;

  // Display the value of the current data set (if any).
  Column := frmGoPhast.PhastModel.SelectedModel.SelectedColumn;
  TrueColumn := Column;
  DataSet := frmGoPhast.PhastModel.SelectedModel.Grid.SideDataSet;
  if DataSet = nil then
  begin
    DataSet := frmGoPhast.PhastModel.SelectedModel.Grid.SideContourDataSet;
  end;
  if (DataSet <> nil) then
  begin
    if DataSet.Orientation = dso3D then
    begin
      Column := frmGoPhast.PhastModel.SelectedModel.SelectedColumn;
    end
    else
    begin
      Column := 0;
    end;
    if frmGoPhast.PhastModel.SideTimeList = nil then
    begin
      NameToDisplay := DataSet.Name;
    end
    else
    begin
      NameToDisplay := frmGoPhast.PhastModel.SideTimeList.Name;
    end;
    if DataSet.IsValue[Layer, Row, Column] and (Column >= 0)
      and (Column < frmGoPhast.PhastModel.SelectedModel.Grid.ColumnCount) then
    begin
      ShowCurrentValue(DataSet, NameToDisplay, Column, Row, Layer,
        TrueColumn, Row, Layer, Location);
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, Row, TrueColumn, NameToDisplay, '', '',
          Location, ViewDirection, DataSet.EvaluatedAt);
      end;
    end;
  end
  else
  begin
    frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, Row, TrueColumn, '', '', '',
          Location, ViewDirection, eaBlocks);
      end;
  end;
end;

procedure TframeView.UpdateStatusBarFrontNodeDataSet(
  Column: Integer; Layer: Integer; const Location: TPoint2D);
var
  NameToDisplay: string;
  Row: Integer;
  DataSet: TDataArray;
  TrueRow: Integer;
  Mesh: TSutraMesh3D;
  Grid: TCustomModelGrid;
begin
  // Make sure the grid is updated.
  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount > 0)
    and (Grid.RowCount > 0) and (Grid.ColumnCount > 0) then
  begin
    Grid.ThreeDElementCenter(0,0,0);
  end;

  // Display the value of the current data set (if any).
  Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  if Mesh <> nil then
  begin
    DataSet := Mesh.ThreeDDataSet;
    if DataSet = nil then
    begin
      DataSet := Mesh.ThreeDContourDataSet;
    end;
    if (DataSet <> nil) then
    begin
      if frmGoPhast.PhastModel.TopTimeList = nil then
      begin
        NameToDisplay := DataSet.Name;
      end
      else
      begin
        NameToDisplay := frmGoPhast.PhastModel.TopTimeList.Name;
      end;
      if DataSet.Orientation = dsoTop then
      begin
        Layer := 0;
      end;
      if DataSet.IsValue[Layer, 0, Column]  then
      begin
        ShowCurrentValue(DataSet, NameToDisplay, Column, 0, Layer,
          Column, 0, Layer, Location);
      end
      else
      begin
        frmGoPhast.sbMain.Panels[2].Text := '';
        if frmGridValue <> nil then
        begin
          frmGridValue.UpdateValue(Layer, 0, Column, NameToDisplay, '', '',
            Location, ViewDirection, DataSet.EvaluatedAt);
        end;
      end;
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, 0, Column, '', '', '',
          Location, ViewDirection, eaBlocks);
      end;
    end;
  end
  else
  begin
    Row := frmGoPhast.PhastModel.SelectedRow;
    TrueRow := Row;
    DataSet := frmGoPhast.Grid.FrontDataSet;
    if DataSet = nil then
    begin
      DataSet := frmGoPhast.Grid.FrontContourDataSet;
    end;
    if (DataSet <> nil) then
    begin
      if DataSet.Orientation = dso3D then
      begin
        Row := frmGoPhast.PhastModel.SelectedRow;
      end
      else
      begin
        Row := 0;
      end;
      if frmGoPhast.PhastModel.FrontTimeList = nil then
      begin
        NameToDisplay := DataSet.Name;
      end
      else
      begin
        NameToDisplay := frmGoPhast.PhastModel.FrontTimeList.Name;
      end;
      if DataSet.IsValue[Layer, Row, Column] and (Row >= 0) and (Row <=
        frmGoPhast.Grid.RowCount) then
      begin
        ShowCurrentValue(DataSet, NameToDisplay, Column, Row, Layer,
          Column, TrueRow, Layer, Location);
      end
      else
      begin
        frmGoPhast.sbMain.Panels[2].Text := '';
        if frmGridValue <> nil then
        begin
          frmGridValue.UpdateValue(Layer, TrueRow, Column, NameToDisplay, '', '',
            Location, ViewDirection, DataSet.EvaluatedAt);
        end;
      end;
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, TrueRow, Column, '', '', '',
          Location, ViewDirection, eaBlocks);
      end;
    end;
  end;
end;

procedure TframeView.UpdateStatusBarFrontBlockDataSet(Column, Layer: Integer;
  const Location: TPoint2D);
var
  NameToDisplay: string;
  Row: Integer;
  DataSet: TDataArray;
  TrueRow: Integer;
//  Mesh: TSutraMesh3D;
  Grid: TCustomModelGrid;
  Mesh: IMesh3D;
begin
  // Make sure the grid is updated.
  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount > 0)
    and (Grid.RowCount > 0) and (Grid.ColumnCount > 0) then
  begin
    Grid.ThreeDElementCenter(0,0,0);
  end;
  // Display the value of the current data set (if any).
  Mesh := frmGoPhast.PhastModel.Mesh3D;
  if Mesh <> nil then
  begin
    DataSet := Mesh.ThreeDDataSet;
    if DataSet = nil then
    begin
      DataSet := Mesh.ThreeDContourDataSet;
    end;
    if (DataSet <> nil) then
    begin
      if frmGoPhast.PhastModel.FrontTimeList = nil then
      begin
        NameToDisplay := DataSet.Name;
      end
      else
      begin
        NameToDisplay := frmGoPhast.PhastModel.FrontTimeList.Name;
      end;
      if DataSet.Orientation = dsoTop then
      begin
        Layer := 0;
      end;
      if DataSet.IsValue[Layer, 0, Column]  then
      begin
        ShowCurrentValue(DataSet, NameToDisplay, Column, 0, Layer,
          Column, 0, Layer, Location);
      end
      else
      begin
        frmGoPhast.sbMain.Panels[2].Text := '';
        if frmGridValue <> nil then
        begin
          frmGridValue.UpdateValue(Layer, 0, Column, NameToDisplay, '', '',
            Location, ViewDirection, DataSet.EvaluatedAt);
        end;
      end;
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, 0, Column, '', '', '',
          Location, ViewDirection, eaBlocks);
      end;
    end;
  end
  else
  begin
    Row := frmGoPhast.PhastModel.SelectedModel.SelectedRow;
    TrueRow := Row;
    DataSet := frmGoPhast.PhastModel.SelectedModel.FrontDataSet;
    if DataSet = nil then
    begin
      DataSet := frmGoPhast.PhastModel.SelectedModel.FrontContourDataSet;
    end;
    if (DataSet <> nil) then
    begin
      if DataSet.Orientation = dso3D then
      begin
        Row := frmGoPhast.PhastModel.SelectedModel.SelectedRow;
      end
      else
      begin
        Row := 0;
      end;
      if frmGoPhast.PhastModel.FrontTimeList = nil then
      begin
        NameToDisplay := DataSet.Name;
      end
      else
      begin
        NameToDisplay := frmGoPhast.PhastModel.FrontTimeList.Name;
      end;
      if DataSet.IsValue[Layer, Row, Column] and (Row >= 0) and (Row <
        frmGoPhast.PhastModel.SelectedModel.Grid.RowCount) then
      begin
        ShowCurrentValue(DataSet, NameToDisplay, Column, Row, Layer,
          Column, TrueRow, Layer, Location);
      end
      else
      begin
        frmGoPhast.sbMain.Panels[2].Text := '';
        if frmGridValue <> nil then
        begin
          frmGridValue.UpdateValue(Layer, TrueRow, Column, NameToDisplay, '', '',
            Location, ViewDirection, DataSet.EvaluatedAt);
        end;
      end;
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(Layer, TrueRow, Column, '', '', '',
          Location, ViewDirection, eaBlocks);
      end;
    end;
  end;
end;

procedure TframeView.UpdateStatusBarForTopNodeDataSet(
  Column: Integer; Row: Integer; const Location: TPoint2D);
var
  NameToDisplay: string;
  Layer: Integer;
  DataSet: TDataArray;
  TrueLayer: Integer;
  ShowValue: Boolean;
  Mesh: TSutraMesh3D;
  Grid: TCustomModelGrid;
begin
  // Make sure the grid is updated.
  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount > 0)
    and (Grid.RowCount > 0) and (Grid.ColumnCount > 0) then
  begin
    Grid.ThreeDElementCenter(0,0,0);
  end;

  // Display the value of the current data set (if any).
  Layer := frmGoPhast.PhastModel.SelectedLayer;
  TrueLayer := Layer;
  DataSet := GetTopDisplayDataSet;


  if (DataSet <> nil) then
  begin
    if DataSet.Orientation = dso3D then
    begin
      Layer := frmGoPhast.PhastModel.SelectedLayer;
    end
    else
    begin
      Layer := 0;
    end;
    if frmGoPhast.PhastModel.TopTimeList = nil then
    begin
      NameToDisplay := DataSet.Name;
    end
    else
    begin
      NameToDisplay := frmGoPhast.PhastModel.TopTimeList.Name;
    end;

    ShowValue := False;
    if DataSet.IsValue[Layer, Row, Column] and (Layer >= 0) then
    begin
      case frmGoPhast.PhastModel.ModelSelection of
        msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
          msModflowFmp, msModflowCfp, msFootPrint, msModflow2015
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
          :
        begin
          ShowValue := (Layer <= frmGoPhast.PhastModel.SelectedModel.Grid.LayerCount);
        end;
        msSutra22, msSutra30, msSutra40:
        begin
          Mesh := frmGoPhast.PhastModel.SutraMesh;
          ShowValue := (Mesh <> nil) and (Layer <= Mesh.LayerCount);
        end;
      else
        Assert(False);
      end;
    end;

    if ShowValue then
    begin
      ShowCurrentValue(DataSet, NameToDisplay, Column, Row, Layer,
        Column, Row, TrueLayer, Location);
    end
    else
    begin
      frmGoPhast.sbMain.Panels[2].Text := '';
      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(TrueLayer, Row, Column, NameToDisplay, '', '',
          Location, ViewDirection, DataSet.EvaluatedAt);
      end;
    end;
  end
  else
  begin
    frmGoPhast.sbMain.Panels[2].Text := '';
    if frmGridValue <> nil then
    begin
      frmGridValue.UpdateValue(TrueLayer, Row, Column, '', '', '',
        Location, ViewDirection, eaBlocks);
    end;
  end;
end;

procedure TframeView.UpdateStatusBarForTopBlockDataSet(
  Column, Row, X, Y: Integer; const Location: TPoint2D);
var
  NameToDisplay: string;
  Layer: Integer;
  DataSet: TDataArray;
  EdgeDisplay: TCustomModflowGridEdgeDisplay;
  HasUpdated: boolean;
  Value: double;
  Explanation: string;
  TrueLayer: Integer;
  Mesh: TSutraMesh3D;
  ShowValue: boolean;
  Grid: TCustomModelGrid;
begin
  HasUpdated := False;
  // Display the value of the current data set (if any).

  // Make sure the grid is updated.
  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount > 0)
    and (Grid.RowCount > 0) and (Grid.ColumnCount > 0) then
  begin
    Grid.ThreeDElementCenter(0,0,0);
  end;

  Layer := frmGoPhast.PhastModel.SelectedModel.SelectedLayer;
  TrueLayer := Layer;

  DataSet := GetTopDisplayDataSet;

  if (DataSet <> nil) then
  begin
    if DataSet.Orientation = dso3D then
    begin
      case frmGoPhast.PhastModel.ModelSelection of
        msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015, msFootPrint
        {$IFDEF OWHMV2}
        , msModflowOwhm2
        {$ENDIF}
        :
        begin
          Layer := frmGoPhast.PhastModel.SelectedModel.SelectedLayer;
        end;
        msSutra22, msSutra30, msSutra40:
        begin
//          Layer := frmGoPhast.PhastModel.SutraMesh.SelectedLayer;
        end;
      else
        Assert(False);
      end;
    end
    else
    begin
      Layer := 0;
    end;
    if frmGoPhast.PhastModel.TopTimeList = nil then
    begin
      NameToDisplay := DataSet.Name;
    end
    else
    begin
      NameToDisplay := frmGoPhast.PhastModel.TopTimeList.Name;
    end;

    if (Layer >= 0) and DataSet.IsValue[Layer, Row, Column] then
    begin
      ShowValue := False;
      case frmGoPhast.PhastModel.ModelSelection of
        msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015, msFootPrint
        {$IFDEF OWHMV2}
        , msModflowOwhm2
        {$ENDIF}
        :
        begin
          ShowValue := (Layer < frmGoPhast.PhastModel.SelectedModel.LayerCount);
        end;
        msSutra22, msSutra30, msSutra40:
        begin
          Mesh := frmGoPhast.PhastModel.SutraMesh;
          ShowValue := (Mesh <> nil) and (Layer < Mesh.LayerCount);
        end;
      else
        Assert(False);
      end;
      if ShowValue then
      begin
        ShowCurrentValue(DataSet, NameToDisplay, Column, Row, Layer, Column, Row, TrueLayer, Location);
        HasUpdated := True;
      end;
    end;
  end
  else if not (frmGoPhast.PhastModel.ModelSelection in SutraSelection) and
    (frmGoPhast.PhastModel.SelectedModel.EdgeDisplay <> nil) then
  begin
    EdgeDisplay := frmGoPhast.PhastModel.SelectedModel.EdgeDisplay;
    if EdgeDisplay.Select(X, Y, Layer) then
    begin
      NameToDisplay := EdgeDisplay.RealDescription[EdgeDisplay.DataToPlot];
      Value := EdgeDisplay.RealValue[X, Y, Layer];
      Explanation := EdgeDisplay.RealAnnotation[X, Y, Layer];

      frmGoPhast.sbMain.Panels[2].Text := FloatToStr(Value) + ': '
        + NameToDisplay + '; '
        + StringReplace(Explanation, sLineBreak, ' ', [rfReplaceAll]);

      if frmGridValue <> nil then
      begin
        frmGridValue.UpdateValue(TrueLayer, Row, Column, NameToDisplay,
          FloatToStr(Value), Explanation, Location, ViewDirection, eaBlocks);
      end;
      HasUpdated := True;
    end;
  end;
  if not HasUpdated then
  begin
    NameToDisplay := '';
    frmGoPhast.sbMain.Panels[2].Text := '';
    if frmGridValue <> nil then
    begin
      frmGridValue.UpdateValue(TrueLayer, Row, Column, NameToDisplay, '', '',
        Location, ViewDirection, eaBlocks);
    end;
  end;
end;

procedure TframeView.UpdateStatusBarCoordinates(APoint: TPoint2D);
var
  RotatedPoint: TPoint2D;
  RotationAngle: Real;
begin
  // Display the current coordinates.
  if (frmGoPhast.phastModel.ComponentState * [csLoading, csReading]) <> [] then
  begin
    Exit;
  end;

  case ViewDirection of
    vdTop:
      begin
        if frmGoPhast.Grid = nil then
        begin
          if (frmGoPhast.PhastModel.SelectedModel.DrawMesh = nil) then Exit;
          RotationAngle := frmGoPhast.PhastModel.SelectedModel.DrawMesh.CrossSection.Angle;
          if RotationAngle = 0 then
          begin
            frmGoPhast.sbMain.Panels[0].Text := Format(StrXY0s1s,
              [FloatToStrF(APoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
              FloatToStrF(APoint.Y, ffGeneral, rulVertical.RulerPrecision, 1)]);
          end
          else
          begin
            RotatedPoint := frmGoPhast.PhastModel.DrawMesh.
              RotateFromRealWorldCoordinatesToMeshCoordinates(APoint);
            frmGoPhast.sbMain.Panels[0].Text := Format(StrXY0s1sRotated,
              [FloatToStrF(APoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
              FloatToStrF(APoint.Y, ffGeneral, rulVertical.RulerPrecision, 1),
              FloatToStrF(RotatedPoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
              FloatToStrF(RotatedPoint.Y, ffGeneral, rulVertical.RulerPrecision, 1)]);
          end;
        end
        else
        begin
          RotationAngle := frmGoPhast.Grid.GridAngle;
          if RotationAngle = 0 then
          begin
            frmGoPhast.sbMain.Panels[0].Text := Format(StrXY0s1s,
              [FloatToStrF(APoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
              FloatToStrF(APoint.Y, ffGeneral, rulVertical.RulerPrecision, 1)]);
          end
          else
          begin
            RotatedPoint := frmGoPhast.Grid.
              RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
            frmGoPhast.sbMain.Panels[0].Text := Format(StrXY0s1sRotated,
              [FloatToStrF(APoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
              FloatToStrF(APoint.Y, ffGeneral, rulVertical.RulerPrecision, 1),
              FloatToStrF(RotatedPoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
              FloatToStrF(RotatedPoint.Y, ffGeneral, rulVertical.RulerPrecision, 1)]);
          end;
        end;
      end;
    vdFront:
      begin
        frmGoPhast.sbMain.Panels[0].Text := Format(StrXZ0s1s,
          [FloatToStrF(APoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1),
          FloatToStrF(APoint.Y, ffGeneral, rulVertical.RulerPrecision, 1)]);
      end;
    vdSide:
      begin
        frmGoPhast.sbMain.Panels[0].Text := Format(StrYZ0s1s,
          [FloatToStrF(APoint.Y, ffGeneral, rulVertical.RulerPrecision, 1),
          FloatToStrF(APoint.X, ffGeneral, rulHorizontal.RulerPrecision, 1)]);
      end;
  else
    Assert(False);
  end;
end;

procedure TframeView.RotatedGridCorners(const Angle: real;
  out PointArray: array of TPoint);
var
  Center: TPoint2D;
  Corners: array[0..MaxGridCorners - 1] of TPoint2D;
  Index: integer;
  PointAngle, PointDistance: real;
  X, Y: real;
begin
    // @name sets PointArray to the positions of the corner of the grid
    // after having been rotated by Angle.
  Assert(Length(PointArray) = 5);
  if (frmGoPhast.Grid.RowCount > 0) and
    (frmGoPhast.Grid.ColumnCount > 0) then
  begin
    Corners[0] := frmGoPhast.Grid.TwoDElementCorner(0, 0);
    Corners[1] := frmGoPhast.Grid.TwoDElementCorner(0,
      frmGoPhast.Grid.RowCount);
    Corners[2] :=
      frmGoPhast.Grid.TwoDElementCorner(frmGoPhast.Grid.ColumnCount,
      frmGoPhast.Grid.RowCount);
    Corners[3] :=
      frmGoPhast.Grid.TwoDElementCorner(
      frmGoPhast.Grid.ColumnCount, 0);
  end;
  Center := GridCenter;

  for Index := 0 to MaxGridCorners - 1 do
  begin
    PointAngle := ArcTan2(Corners[Index].Y - Center.Y,
      Corners[Index].X - Center.X);
    PointDistance := Sqrt(Sqr(Corners[Index].X - Center.X)
      + Sqr(Corners[Index].Y - Center.Y));
    PointAngle := PointAngle + Angle;
    X := Center.X + PointDistance * Cos(PointAngle);
    Y := Center.Y + PointDistance * Sin(PointAngle);
    PointArray[Index].X := ZoomBox.XCoord(X);
    PointArray[Index].Y := ZoomBox.YCoord(Y);
  end;

  PointArray[4] := PointArray[0];
end;

procedure TframeView.ZoomBoxPan(Sender: TObject; DeltaX, DeltaY: real);
var
  X, Y: integer;
begin
    // @name is the event handler for @link(ZoomBox).OnPan.
    // It updates @link(rulHorizontal) and @link(rulVertical) with
    // its current position.
  X := ZoomBox.XCoord(ZoomBox.OriginX) - ZoomBox.XCoord(ZoomBox.OriginX +
    DeltaX);
  Y := ZoomBox.YCoord(ZoomBox.OriginY) - ZoomBox.YCoord(ZoomBox.OriginY +
    DeltaY);
  AdjustHorizontalScale(X);
  AdjustVerticalScale(Y);
end;

procedure TframeView.ZoomBoxResize(Sender: TObject);
begin
    // @name is the event handler for @link(ZoomBox).OnResize.
    // @name sets frmGoPhast.@link(TfrmGoPhast.Timer).OnTimer to
    // frmGoPhast.@link(TfrmGoPhast.ResizeZoomBoxes) with a delay of 100 ms.
    // The effect is to prevent the control from redrawing until
    // the user has finished resizing it.
  if (frmGoPhast.timTimer <> nil) and not IsResizing then
  begin
    IsResizing := True;
    ZoomBox.ImmediateResize := False;
    frmGoPhast.timTimer.Enabled := False;
    frmGoPhast.timTimer.OnTimer := frmGoPhast.ResizeZoomBoxes;
    frmGoPhast.timTimer.Interval := 100;
    frmGoPhast.timTimer.Enabled := True;
  end;
end;

procedure TframeView.rulerDblClick(Sender: TObject);
begin
    // @name is OnDblClick event handler for @link(rulHorizontal), and
    // @link(rulVertical).  It shows the @link(TfrmRulerOptions) form.
  frmGoPhast.ClickedRuler := Sender;
  ShowAForm(TfrmRulerOptions);
end;

procedure TframeView.DrawImage(const Source: TCompressedBitmapItem);
begin
    // @name will draw an imported image (Source) on a bitmap (Dest)
    // at its proper location.
  Assert((Source <> nil) {and (Dest <> nil)});
  Source.DrawCompressedImage(FBitMap32, FBitMap32.Width, FBitMap32.Height);


  FPreviousMagnification := ZoomBox.Magnification;
end;

procedure TframeView.HideAllOthersClick(Sender: TObject);
begin
    // @name hides the @link(TScreenObject)s that are not selected.
  // This procedure hides the TScreenObjects that are not selected
  HideScreenObjects(False);
end;

procedure TframeView.miHideClick(Sender: TObject);
begin
    // @name hides the selected @link(TScreenObject)s.
  // This procedure hides the selected TScreenObjects
  HideScreenObjects(True);
end;

procedure TframeView.SetUpColRowLayerSelector;
begin
    // @name changes ColRowLayerSelector based on @link(ViewDirection).
  FColRowLayerSelector.Free;
  FColRowLayerSelector := TCustomLayerRowColumnSelector.New(self,
    ViewDirection);
end;

procedure TframeView.SetViewDirection(const Value: TViewDirection);
begin
    // See @link(ViewDirection).
  if FViewDirection <> Value then
  begin
    FViewDirection := Value;
    SetUpColRowLayerSelector;
  end;
end;

function TframeView.GetColRowLayerSelector: TCustomLayerRowColumnSelector;
begin
    // See @link(ColRowLayerSelector).
  if FColRowLayerSelector = nil then
  begin
    SetUpColRowLayerSelector;
  end;
  result := FColRowLayerSelector;
end;

procedure TframeView.ItemChange(Sender: TObject);
begin
    // See TCustomLayerRowColumnSelector.@link(
    // TCustomLayerRowColumnSelector.ItemChange);
  ColRowLayerSelector.ItemChange(Sender);
end;

procedure TframeView.miMergeObjectsClick(Sender: TObject);
begin
  frmGoPhast.miMergeObjectsClick(Sender);
end;

procedure TframeView.miSelectAllClick(Sender: TObject);
begin
  SelectAll;
end;

procedure TframeView.miUnlockSelectedObjectsClick(Sender: TObject);
begin
  frmGoPhast.miUnlockSelectedObjectsClick(Sender);
end;

procedure TframeView.FinishScreenObjects;
begin
    // See TCustomCreateScreenObjectTool.@link(
    // TCustomCreateScreenObjectTool.FinishScreenObjects);
  if (frmGoPhast.CurrentTool <> nil)
    and (frmGoPhast.CurrentTool is TCustomCreateScreenObjectTool) then
  begin
    TCustomCreateScreenObjectTool(frmGoPhast.CurrentTool).FinishScreenObjects;
  end;
end;

function TframeView.GetCurrentScreenObject: TScreenObject;
begin
    // See @link(CurrentScreenObject).
  result := nil;
  if (frmGoPhast.CurrentTool <> nil)
    and (frmGoPhast.CurrentTool is TCustomCreateScreenObjectTool) then
  begin
    result :=
      TCustomCreateScreenObjectTool(frmGoPhast.CurrentTool).CurrentScreenObject
  end;
end;

{ TCustomInteractiveTool }

procedure TCustomInteractiveTool.Activate;
var
  StoredViewDirection: TViewDirection;
begin
  StoredViewDirection := FViewDirection;
  try
    FViewDirection := vdTop;
    if Hint = '' then
    begin
      frmGoPhast.frameTopView.ZoomBox.Hint := StrChooseAToolAndCl;
    end
    else
    begin
      frmGoPhast.frameTopView.ZoomBox.Hint := Hint;
    end;
    FViewDirection := vdFront;
    if Hint = '' then
    begin
      frmGoPhast.frameFrontView.ZoomBox.Hint := StrChooseAToolAndCl;
    end
    else
    begin
      frmGoPhast.frameFrontView.ZoomBox.Hint := Hint;
    end;
    FViewDirection := vdSide;
    if Hint = '' then
    begin
      frmGoPhast.frameSideView.ZoomBox.Hint := StrChooseAToolAndCl;
    end
    else
    begin
      frmGoPhast.frameSideView.ZoomBox.Hint := Hint;
    end;
  finally
    FViewDirection := StoredViewDirection;
  end;
  UpdateAllViews;
end;

procedure TCustomInteractiveTool.Deactivate;
begin
  frmGoPhast.frameTopView.ZoomBox.Hint := StrChooseAToolAndCl;
  frmGoPhast.frameFrontView.ZoomBox.Hint := StrChooseAToolAndCl;
  frmGoPhast.frameSideView.ZoomBox.Hint := StrChooseAToolAndCl;
  UpdateAllViews;
end;

procedure TCustomInteractiveTool.DoubleClick(Sender: TObject);
begin
  // do nothing
end;

procedure TCustomInteractiveTool.GetColLayer(APoint: TPoint2D; out Col,
  Layer: integer);
begin
  View.GetColLayer(APoint, Col, Layer);
end;

function TCustomInteractiveTool.GetCursor: TCursor;
begin
  result := FCursor;
end;

function TCustomInteractiveTool.GetHint: string;
begin
  result := '';
end;

function TCustomInteractiveTool.GetMouseIsDown: boolean;
begin
  result := GlobalMouseIsDown;
end;

procedure TCustomInteractiveTool.GetRowCol(APoint: TPoint2D; out Row,
  Column: integer);
begin
  View.GetRowCol(APoint, Row, Column);
end;

procedure TCustomInteractiveTool.GetRowLayer(APoint: TPoint2D; out Row,
  Layer: integer);
begin
  View.GetRowLayer(APoint, Row, Layer);
end;

procedure TCustomInteractiveTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GlobalMouseIsDown := True;
  FButton := Button;
  if Button = mbMiddle then
  begin
    FStoredCursor := Cursor;
    PanTool.MouseDown(Sender, mbLeft, Shift, X, Y);
  end;
end;

procedure TCustomInteractiveTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // do nothing.
end;

procedure TCustomInteractiveTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GlobalMouseIsDown := False;
  if FButton = mbMiddle then
  begin
    PanTool.MouseUp(Sender, mbLeft, Shift, X, Y);
    FButton := mbLeft;
    Cursor := FStoredCursor;
    ZoomBox.Panning := False;
  end;
end;

procedure TCustomInteractiveTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  // do nothing
end;

function TCustomInteractiveTool.FindNodeInSelectedScreenObjects(const X,
  Y: integer; const AScreenObject: TScreenObject): integer;
var
  PointIndex: integer;
  APoint: TPoint;
  DeltaX: Int64;
  DeltaY: Int64;
  X64: Int64;
  Y64: Int64;
  PointX: Int64;
  PointY: Int64;
begin
  result := -1;
  Assert(AScreenObject.Selected);
  X64 := X;
  Y64 := Y;
  for PointIndex := 0 to AScreenObject.Count - 1 do
  begin
    APoint := AScreenObject.CanvasCoordinates[PointIndex];
    PointX := APoint.X;
    PointY := APoint.Y;
    DeltaX := Abs(X64 - PointX); 
    DeltaY := Abs(Y64 - PointY); 
    if (DeltaX < SelectionWidth)
      and (DeltaY < SelectionWidth) then
    begin
      result := PointIndex;
      Exit;
    end;
  end;
end;

procedure TCustomInteractiveTool.RightClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // do nothing.
end;

procedure TCustomInteractiveTool.SetAllCursors(const Value: TCursor);
begin
  frmGoPhast.frameTopView.ZoomBox.Cursor := Value;
  frmGoPhast.frameTopView.ZoomBox.Image32.Cursor := Value;
  frmGoPhast.frameFrontView.ZoomBox.Cursor := Value;
  frmGoPhast.frameFrontView.ZoomBox.Image32.Cursor := Value;
  frmGoPhast.frameSideView.ZoomBox.Cursor := Value;
  frmGoPhast.frameSideView.ZoomBox.Image32.Cursor := Value;
end;

procedure TCustomInteractiveTool.UpdateAllViews;
begin
  if FTopLayer <> nil then
  begin
    FTopLayer.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  end;
  if FFrontLayer <> nil then
  begin
    FFrontLayer.Changed;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  end;
  if FSideLayer <> nil then
  begin
    FSideLayer.Changed;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TCustomInteractiveTool.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  UpdateCursors;
end;

procedure TCustomInteractiveTool.SetViewDirection(
  const Value: TViewDirection);
begin
  FViewDirection := Value;
end;

procedure TCustomInteractiveTool.UpdateCursors;
begin
  View.ZoomBox.Cursor := GetCursor;
  View.ZoomBox.Image32.Cursor := GetCursor;
  // This is needed to update the cursor for TSelectPointTool when dragging
  // a vertex.
  Screen.Cursor := GetCursor;
end;

function TCustomInteractiveTool.View(Direction: TViewDirection): TframeView;
begin
  result := nil;
  case Direction of
    vdTop:
      begin
        result := frmGoPhast.frameTopView;
      end;
    vdFront:
      begin
        result := frmGoPhast.frameFrontView;
      end;
    vdSide:
      begin
        result := frmGoPhast.frameSideView;
      end;
  else
    Assert(False);
  end;
end;

function TCustomInteractiveTool.View: TframeView;
begin
  result := View(ViewDirection);
end;

function TCustomInteractiveTool.ZoomBox: TQRbwZoomBox2;
begin
  result := View.ZoomBox;
end;

procedure TCustomInteractiveTool.CreateLayers;
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
  if FFrontLayer = nil then
  begin
    FFrontLayer := CreateLayer(frmGoPhast.frameFrontView.ZoomBox);
  end;
  if FSideLayer = nil then
  begin
    FSideLayer := CreateLayer(frmGoPhast.frameSideView.ZoomBox);
  end;
end;

function TCustomInteractiveTool.Layer32: TPositionedLayer;
  function CreateLayer: TPositionedLayer;
  begin
    result := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
      TPositionedLayer;
    result.OnPaint := DrawOnBitMap32;
  end;
begin
  result := nil;
  case ViewDirection of
    vdTop:
      begin
        if FTopLayer = nil then
        begin
          FTopLayer := CreateLayer;
        end;
        result := FTopLayer;
      end;
    vdFront:
      begin
        if FFrontLayer = nil then
        begin
          FFrontLayer := CreateLayer;
        end;
        result := FFrontLayer;
      end;
    vdSide:
      begin
        if FSideLayer = nil then
        begin
          FSideLayer := CreateLayer;
        end;
        result := FSideLayer;
      end;
  else Assert(False);
  end;
  result.BringToFront;
end;

function TCustomInteractiveTool.GetEvaluatedAt(
  const Direction: TViewDirection): TEvaluatedAt;
var
  ColoredDataSet: TDataArray;
begin
  ColoredDataSet := nil;
  case Direction of
    vdTop:
      begin
        ColoredDataSet := frmGoPhast.Grid.TopDataSet;
      end;
    vdFront:
      begin
        ColoredDataSet := frmGoPhast.Grid.FrontDataSet;
      end;
    vdSide:
      begin
        ColoredDataSet := frmGoPhast.Grid.SideDataSet;
      end;
  else
    Assert(False);
  end;
  if ColoredDataSet = nil then
  begin
    result := eaBlocks;
  end
  else
  begin
    result := ColoredDataSet.EvaluatedAt;
  end;
end;

end.



