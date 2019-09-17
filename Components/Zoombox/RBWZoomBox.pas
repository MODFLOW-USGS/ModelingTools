unit RbwZoomBox;

    {
    TRbwZoomBox provides methods for converting real-number coordinates to
    screen coordinates so that they may be easily displayed.

    By default, the positive directions for the real-number coordinate system
    are to the right (X-axis) and upward (Y-axis).  However, either coordinate
    system may be reversed.  By default, the vertical exaggeration is 1 but
    the vertical exaggeration may be set to any positive real number.  However,
    if the vertical exaggeration is set to too high or too low a value,
    EInvalidOp may be raised when calculating the screen coordinates.

    TRbwZoomBox has two imbedded components, a TPaintBox and a TShape;  The
    TPaintBox provides the drawing surface. The TShape is only visible
    during zooming operations.  Methods or properties that merely surface
    methods or properties of the TPaintBox and TShape have the same name as the
    methods in the TPaintBox and TShape with a prefix of "PB" or "S".

    Zooming methods:
      AbortZoom
      BeginZoom
      ContinueZoom
      FinishZoom
      ZoomBy
      ZoomByAt
      ZoomOut
      SetZoom

    Panning methods:
      BeginPan
      EndPan

    Coordinate Conversion methods
      MouseToCoordinates
      X
      XCoord
      Y
      YCoord

    TRbwZoomPoint is a helper class that store the real-number coordinates.
    Many of the zooming operations require that TRbwZoomPoint be used to store
    the coordinates.
    }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  extctrls, menus;

type
  EInvalidZoomLevel = Class(Exception);
  {EInvalidZoomLevel is raised if there is an attempt to set a zoom level <= 0
  in TRbwZoomBox.SetZoom}

  TRbwZoomPoint = class;

  TZBArray = array of TRbwZoomPoint;

  TRbwZoomBox = class(TScrollBox)
  private
    FMultiplier, FDefaultMultiplier : Extended;
    StartX, StartY, EndX, EndY : Integer;
    ZoomBegun : Boolean;
    FMinX: extended;
    FMaxY: extended;
    FMaxX: extended;
    FMinY: extended;
    FPaintBox: TPaintBox;
    FZoomWindow: TShape;
    PointsList : TList;
    FOnClick: TNotifyEvent;
    FOnDblClick : TNotifyEvent;
    FOnDragDrop : TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnEndDrag: TEndDragEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnScrollHorizontal: TNotifyEvent;
    FOnScrollVertical: TNotifyEvent;
    FOnStartDrag: TStartDragEvent;
    Panning : boolean;
    StartXScrollBarPosition, StartYScrollBarPosition : integer;
    StartXPosition, StartYPosition : integer;
    FTopMargin: integer;
    FLeftMargin: integer;
    FRightMargin: integer;
    FBottomMargin: integer;
    FYPositive: boolean;
    FXPositive: boolean;
    FVerticalExaggeration: double;
    FSelectionWidth : integer;
    procedure SetMultiplier( Value: extended);
    procedure SetMaxX(const Value: extended);
    procedure SetMaxY(const Value: extended);
    procedure SetMinX(const Value: extended);
    procedure SetMinY(const Value: extended);
    function ReadPBCanvas : TCanvas;
    function ReadPBClientHeight: Integer;
    function ReadPBClientWidth: Integer;
    function GetPBClientOrigin : TPoint;
    function GetPBClientRect : TRect;
    function GetPBColor : TColor;
    procedure SetPBColor(const Value: TColor);
    function GetPBComponentCount : integer;
    function GetPBComponentIndex : integer;
    procedure SetPBComponentIndex(const Value: integer);
    function GetPBComponents (Index : integer) : TComponent;
    function GetPBControlState : TControlState;
    procedure SetPBControlState (const Controlstate: TControlState);
    function GetPBControlStyle : TControlStyle;
    procedure SetPBControlStyle (const ControlStyle: TControlStyle);
    function GetPBCursor : TCursor;
    procedure SetPBCursor (const Cursor: TCursor);
    function GetPBDragKind : TDragKind;
    procedure SetPBDragKind(const Value : TDragKind);
    function GetPBDragCursor: TCursor;
    procedure SetPBDragCursor(const Value: TCursor);
    function GetPBDragMode: TDragMode;
    procedure SetPBDragMode(const Value: TDragMode);
    function GetPBEnabled: Boolean;
    procedure SetPBEnabled(const Value: Boolean);
    function GetPBFont: TFont;
    procedure SetPBFont(const Value: TFont);
    function GetPBHeight: Integer;
    procedure SetPBHeight(const Value: Integer);
    function GetPBHint: string;
    procedure SetPBHint(const Value: string);
    function GetPBLeft: Integer;
    procedure SetPBLeft(const Value: Integer);
    function GetPBPopupMenu: TPopupMenu;
    procedure SetPBPopupMenu(const Value: TPopupMenu);
    function GetPBShowHint: Boolean;
    procedure SetPBShowHint(const Value: Boolean);
    function GetPBTop: Integer;
    procedure SetPBTop(const Value: Integer);
    function GetPBVisible: Boolean;
    procedure SetPBVisible(const Value: Boolean);
    function GetPBWidth: Integer;
    procedure SetPBWidth(const Value: Integer);
    function GetPBWindowProc: TWndMethod;
    procedure SetPBWindowProc(const Value: TWndMethod);
    function GetPBComObject : IUnknown;
    function GetPBTag: Longint;
    procedure SetPBTag(const Value: Longint);
    function GetSBrush: TBrush;
    procedure SetSBrush(const Value: TBrush);
    function GetSPen: TPen;
    procedure SetSPen(const Value: TPen);
    function GetSBoundsRect: TRect;
    procedure SetSBoundsRect(const Value: TRect);
    function GetSCursor: TCursor;
    procedure SetSCursor(const Value: TCursor);
    function GetSDragCursor: TCursor;
    procedure SetSDragCursor(const Value: TCursor);
    function GetOnPBPaint: TNotifyEvent;
    procedure SetOnPBPaint(const Value: TNotifyEvent);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnDblClick(const Value: TNotifyEvent);
    procedure SetOnDragDrop(const Value: TDragDropEvent);
    procedure SetOnDragOver(const Value: TDragOverEvent);
    procedure SetOnEndDrag(const Value: TEndDragEvent);
    procedure SetOnMouseDown(const Value: TMouseEvent);
    procedure SetOnMouseMove(const Value: TMouseMoveEvent);
    procedure SetOnMouseUp(const Value: TMouseEvent);
    procedure SetOnScrollHorizontal(const Value: TNotifyEvent);
    procedure SetOnScrollVertical(const Value: TNotifyEvent);
    procedure SetOnStartDrag(const Value: TStartDragEvent);
    procedure PanMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PanUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       X, Y: Integer);
    procedure SetBottomMargin(const Value: integer);
    procedure SetLeftMargin(const Value: integer);
    procedure SetRightMargin(const Value: integer);
    procedure SetTopMargin(const Value: integer);
    procedure ZoomBy1(ZoomFactor: Extended);
    procedure SetVerticalExaggeration(Value: double);
    { Private declarations }
  protected
    procedure CreateWnd; override ;
    {Creates a Windows control corresponding to the TZoomBox object.
    Description
    Use CreateWnd to create a Windows control corresponding to the
    ZoomBox object.
    CreateWnd initializes the paintbox size
    and then calls TScrollingWinControl.CreateWnd.}
    procedure DoExit; override;
    {DoExit  calls AbortZoom and then calls TWinControl.DoExit}
    { Protected declarations }
    Procedure Pan; virtual;
    {When Panning is true, Pan moves the scrollbars to follow the
    mouse.  This keeps the same location underneath the mouse at all times.  }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {MouseMove calls the inherited MouseMove and then calls Pan.}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    {MouseUp calls the inherited MouseUp and then calls Pan.}
    procedure Loaded; override;
    {Loaded calls the inherited Loaded and then calls ZoomOut.}
    procedure ScrollH(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure Scrollv(var Msg: TWMHScroll); message WM_VSCROLL;
  public
    procedure AbortZoom;
    {If for some reason you want to terminate a zoom operation without doing
    the zoom, call AbortZoom instead of FinishZoom.}
    procedure BeginPan;
    {BeginPan stores the current mouse and scrollbar positions and sets Panning
    to True.  This causes scrollbars to change position to keep the current
    position under the mouse until EndPan is called.}
    procedure BeginZoom( X, Y: Integer);
    {Begin Zoom starts a zooming operation. X, and Y are the X and Y screen
    coordinates of the Paintbox in the TRbwZoomBox. You can get those in
    OnMouseDown.}
    procedure ContinueZoom ( X, Y: Integer);
    procedure CancelZoom;
    {If BeginZoom has been called, and neither AbortZoom nor FinishZoom have
    yet been called, ContinueZoom will draw a shape on the paintbox.
    One of the corners of the shape will be at the location where the zoom
    began. The other will be at X, and Y where are the X and Y screen
    coordinates of the Paintbox in the TRbwZoomBox. You can get those in
    OnMouseMove.}
    Constructor Create(AOwner: TComponent); Override;
    {Creates and initializes a new TZoomBox object.
    Description
    Create performs the following tasks:
    Calls the TScrollBox Create method, passing it AOwner.
    Creates a Paintbox and shape passing itself as AOwner and Parent.
    It Creates a list of TRbwZoomPoint.
    Sets the Margins to 20.
    Sets Multiplier to 1.
    It sets the vertical exageration to 1.
    It sets XPositive and YPositive to True.}
    Property DefaultMultiplier : Extended read FDefaultMultiplier;
    {DefaultMultiplier is the zoom level that would display all the
    TRbwZoomPoint's with a blank area surrounding them Margin pixels wide
    that were in existence when the last GetMinMax occurred.}
    procedure DeletePoints(Points: array of TRbwZoomPoint);
    Destructor Destroy; override;
    {Removes the TScrollingWinControl object from memory.
    Description
    Do not call Destroy. Instead, call Free, which calls Destroy if the
    TZoomBox object is not nil.
    Destroy frees the Paintbox and shape and the list of TZoomPoints and then
    calls TScrollBox.Destroy.
    It does not destroy the TZoomPoints in the list.}
    Procedure EndPan;
    {This sets Panning to False so that the scrollbars cease to change
    position to keep the current position under the mouse}
    procedure FinishZoom ( X, Y: Integer);
    {FinishZoom terminates a zoom operation. X, and Y are the X and Y screen
    coordinates of the Paintbox in the TRbwZoomBox. You can get those in
    OnMouseUp. FinishZoom sets the zoom level so that
    the area outlined by the X, and Y parameters in BeginZoom and the
    X, and Y parameters in FinishZoom are displayed in the TRbwZoomBox. }
    procedure GetMinMax;
    {GetMinMax determines the minimum and maximum coordinates of the currently
    existing TRbwZoomPoints associated with the TRbwZoomBox for which
    UseForZoomOut is true and sets the DefaultMultiplier to a value sufficient
    to display all the TRbwZoomPoints and still leave a blank area equal to
    the TopMargin, BottomMargin, LeftMargin and RightMargin pixels on their
    respective sides.}
    procedure Invalidate; override;
    {Invalidate calls the inherited Invalidate and then calls Invalidate of
    the embedded TPaintBox.}
    function IsPointInside(const X, Y  : extended;
      const ZoomPointArray : TZBArray) : boolean;
    {IsPointInside tests whether a point with real-number coordinates X and Y
    is inside a polygon defined by a series of TRbwZoomPoint in ZoomPointArray.
    The first and last TRbwZoomPoints in ZoomPointArray must be at the same
    location for IsPointInside to return true.}
    procedure MouseToCoordinates(const AnXCoord, AYCoord: integer;
      var AnX, AY : extended);
    {MouseToCoordinates changes the screen coordinates AnXCoord and AYCoord to the
    floating point coordinates X and Y.}
    procedure PBBeginDrag(Immediate: Boolean; Threshold: Integer = -1);
    {See TPaintBox.BeginDrag.}
    function SetZoom(AZoomLevel: Extended) : Extended;
    {SetZoom changes the degree of zooming. AZoomLevel should be the degree
    of zooming that you wish to set. If AZoomLevel  is set equal to
    DefaultMultiplier, this is equivalent to ZoomOut. The zoom level that
    is actually set will generally differ slightly from that specified by
    AZoomLevel. However, the displayed region will differ from that requested
    by one pixel or less. The actual zoom level that is set is the result of
    the SetZoom function. }
    procedure PBBringToFront;
    {See TPointBox.BringToFront.}
    property PBCanvas : TCanvas read ReadPBCanvas ;
    {See TPaintBox.Canvas.}
    property PBClientHeight :Integer read ReadPBClientHeight ;
    {See TPaintBox.ClientHeight.}
    property PBClientOrigin : TPoint read GetPBClientOrigin;
    {See TPaintBox.ClientOrigin.}
    property PBClientRect : TRect read GetPBClientRect;
    {See TPaintBox.ClientRect.}
    function PBClientToScreen(const Point: TPoint): TPoint;
    {See TPaintBox.ClientToScreen.}
    property PBClientWidth :Integer read ReadPBClientWidth ;
    {See TPaintBox.ClientWidth.}
    property PBComponentCount : integer read GetPBComponentCount;
    {See TPaintBox.ComponentCount.}
    property PBComponentIndex : integer read GetPBComponentIndex
       write SetPBComponentIndex;
    {See TPaintBox.ComponentIndex.}
    property PBComponents[Index: Integer]: TComponent read GetPBComponents;
    {See TPaintBox.Components.}
    property PBComObject : IUnknown read GetPBComObject;
    {See TPaintBox.ComObject.}
    property PBControlState : TControlState read GetPBControlState
       write SetPBControlState;
    {See TPaintBox.ControlState.}
    property PBControlStyle: TControlStyle read GetPBControlStyle
       write SetPBControlStyle;
    {See TPaintBox.ControlStyle.}
    procedure PBDragDrop(Source: TObject; X, Y: Integer);
    {See TPaintBox.DragDrop.}
    function PBDragging: Boolean;
    {See TPaintBox.Dragging.}
    procedure PBEndDrag(Drop: Boolean);
    {See TPaintBox.EndDrag.}
    function PBGetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    {See TPaintBox.GetTextBuf.}
    function PBGetTextLen: Integer;
    {See TPaintBox.GetTextLen.}
    procedure PBHide;
    {See TPaintBox.Hide.}
    function PBPerform(Msg: Cardinal; WParam, LParam: Longint): Longint;
    {See TPaintBox.Perform.}
    procedure PbRefresh;
    {See TPaintBox.Refresh.}
    procedure PBRepaint;
    {See TPaintBox.Repaint.}
    function PBScreenToClient(const Point: TPoint): TPoint;
    {See TPaintBox.ScreenToClient.}
    procedure PBSendToBack;
    {See TPaintBox.SendToBack.}
    procedure PBSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    {See TPaintBox.SetBounds.}
    procedure PBSetTextBuf(Buffer: PChar);
    {See TPaintBox.SetTextBuf.}
    procedure PBShow;
    {See TPaintBox.Show.}
    procedure PBUpdate;
    {See TPaintBox.Update.}
    property PBWindowProc: TWndMethod read GetPBWindowProc
       write SetPBWindowProc;
    {See TPaintBox.WindowProc.}
    property SBoundsRect: TRect read GetSBoundsRect write SetSBoundsRect;
    {See TShape.BoundsRect.}
    {SelectionWidth is used in SelectPoint, SelectSegment, SelectPolyLine,
    and SelectPolygon to determine how far away from the object a location
    can be and still allow the object to be selected.}
    function SelectPoint(const X, Y: integer;
      const AZoomPoint : TRbwZoomPoint) : boolean; virtual;
    {SelectPoint returns true only if the screen coordinates of AZoomPoint are
    within SelectionWidth in both the X and Y directions.}
    function SelectPolygon(const X, Y: integer;
      const ZoomPointArray : TZBArray): boolean; virtual;
    {SelectPolygon returns true if the screen coordinates X and Y are close
    to or inside the polygon defined by the series of points in ZoomPointArray.
    The last point in ZoomPointArray should have the same coordinates as the
    first point in ZoomPointArray to properly define a polygon.  If the
    coordinates are not identical, SelectPolygon returns the same result as
    SelectPolyLine.
    SelectPolygon first calls SelectPolyLine and returns True if
    SelectPolyLine returns true.  Otherwise it then calls IsPointInside using
    the real-number coordinates corresponding to X and Y and returns the
    result of IsPointInside.}
    function SelectPolyLine(const X, Y: integer;
      const ZoomPointArray : TZBArray): boolean; virtual;
    {SelectPolyLine returns true if the screen coordinates X and Y are close
    to the line defined by the series of points in ZoomPointArray.
    SelectPolyLine calls SelectPoint for each TRbwZoomPoint in TZBArray and
    then SelectSegment for each adjacent pair of TRbwZoomPoint's in TZBArray
    until a value of True has been returned or all members of TZBArray have
    been tested.}
    function SelectSegment(const X, Y: integer; const ZoomPoint1,
      ZoomPoint2 :TRbwZoomPoint): boolean; virtual;
    {SelectSegment will return true if the point with screen coordinates X, Y
    is within SelectionWidth of the line segment defined by ZoomPoint1 and
    ZoomPoint2.  The distance is measured either horizontally or vertically
    depending on whether or not the slope of the line connecting ZoomPoint1
    and ZoomPoint2 is closer to the vertical or the horizontal in the screen
    coordinates.

    SelectSegment can be used to decide whether to select a particular
    line segment with the mouse. }
    procedure SetRange(MinimumX, MaximumX, MinimumY, MaximumY: Extended);
    {SetRange sets the minimum and maximum real-number coordinates of the
    TRbwZoomBox and sets the Multiplier to a value sufficient all values
    within that range and still leave a blank area equal to the TopMargin,
    BottomMargin, LeftMargin and RightMargin pixels on their respective sides.}
    procedure SetXRange(MinimumX, MaximumX: Extended);
    {SetRange sets the minimum and maximum real-number coordinates of the X-axis of the
    zoombox and sets the Multiplier to a value sufficient all values within
    that range and still leave a blank area equal to the TopMargin,
    BottomMargin, LeftMargin and RightMargin pixels on their respective sides.}
    procedure SetYRange(MinimumY, MaximumY: Extended);
    {SetRange sets the minimum and maximum real-number coordinates of the Y-axis of the
    zoombox and sets the Multiplier to a value sufficient all values within
    that range and still leave a blank area equal to the TopMargin,
    BottomMargin, LeftMargin and RightMargin pixels on their respective sides.}
    function X(const XCoord : integer) : extended;
    {X converts a screen coordinate into a real-number X coordinate.}
    function XCoord(const X : extended) : integer;
    {XCoord converts a real-number X coordinate into a screen coordinate.}
    property VerticalExaggeration : double read FVerticalExaggeration
      write SetVerticalExaggeration;
    function Y(const YCoord : integer) : extended;
    {Y converts a screen coordinate into a real-number Y coordinate.}
    function YCoord(const Y : extended) : integer;
    {YCoord converts a real-number Y coordinate into a screen coordinate.}
    Procedure ZoomBy(ZoomFactor : Extended);
    {ZoomBy changes the level of zoom by a factor specified by ZoomFactor. The
    real-number coordinates are not changed by ZoomBy. See also ZoomByAt}
    Procedure ZoomByAt(ZoomFactor, X, Y : Extended);
    {ZoomByAt calls ZoomBy and passes it ZoomFactor. It then centers the
    position indicated by the floating point coordinates X and Y.}
    procedure ZoomOut;
    {ZoomOut sets the zoom level so that all the TRbwZoomPoint's that had
    been created at the time of the previous call to GetMinMax and for which
    UseForZoomOut was true will be displayed with blank areas at the edges
    with a width equal to LeftMargin, RightMargin, TopMargin, or BottomMargin.}
    { Public declarations }
  published
    {The TZRbwoomBox maintains a blank area at least Margin pixels wide on all
    sides of the TZoomPoints that were in existence the last time GetMinMax was
    called.}
    Property BottomMargin : integer read FBottomMargin write SetBottomMargin;
    {BottomMargin is the width of the space in pixels between the bottom
    edge of the embedded TPaintBox client area and the TRbwZoomPoint whose
    screen Y-coordinate was the highest (furthest down on the screen) at the
    time of the last call to GetMinMax.}
    Property LeftMargin : integer read FLeftMargin write SetLeftMargin;
    {LeftMargin is the width of the space in pixels between the left
    edge of the embedded TPaintBox client area and the TRbwZoomPoint whose
    screen X-coordinate was the lowest (furthest to the left on the screen)
    at the time of the last call to GetMinMax.}
    Property RightMargin : integer read FRightMargin write SetRightMargin;
    {RightMargin is the width of the space in pixels between the right
    edge of the embedded TPaintBox client area and the TRbwZoomPoint whose
    screen X-coordinate was the highest (furthest to the right on the screen)
    at the time of the last call to GetMinMax.}
    Property TopMargin : integer read FTopMargin write SetTopMargin;
    {TopMargin is the width of the space in pixels between the top
    edge of the embedded TPaintBox client area and the TRbwZoomPoint whose
    screen Y-coordinate was the lowest (highest up on the screen) at the
    time of the last call to GetMinMax.}
    Property MinX : extended read FMinX write SetMinX;
    {MinX is the minimum X value of all the TRbwZoomPoints that were associated
    with the TRbwZoomBox and for which UseForZoomOut was true at the the last
    time GetMinMax was called. If no TRbwZoomPoints were in existence, it is
    set to a default value.}
    Property MaxX : extended read FMaxX write SetMaxX;
    {MaxX is the maximum X value of all the TRbwZoomPoints that were associated
    with the TRbwZoomBox and for which UseForZoomOut was true at the the last
    time GetMinMax was called. If no TRbwZoomPoints were in existence, it is
    set to a default value.}
    Property MinY : extended read FMinY write SetMinY;
    {MinY is the minimum Y value of all the TRbwZoomPoints that were associated
    with the TRbwZoomBox and for which UseForZoomOut was true at the the last
    time GetMinMax was called. If no TRbwZoomPoints were in existence, it is
    set to a default value.}
    Property MaxY : extended read FMaxY write SetMaxY;
    {MaxY is the maximum Y value of all the TRbwZoomPoints that were associated
    with the TRbwZoomBox and for which UseForZoomOut was true at the the last
    time GetMinMax was called. If no TRbwZoomPoints were in existence, it is
    set to a default value.}
    Property Multiplier : Extended read FMultiplier write SetMultiplier;
    {Multiplier is the current zoom level.}
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    {See TPaintBox.OnClick.}
    property OnDblClick: TNotifyEvent read FOnDblClick write SetOnDblClick;
    {See TPaintBox.OnDblClick.}
    property OnDragDrop: TDragDropEvent read FOnDragDrop write SetOnDragDrop;
    {See TPaintBox.OnDragDrop.}
    property OnDragOver: TDragOverEvent read FOnDragOver write SetOnDragOver;
    {See TPaintBox.OnDragOver.}
    property OnEndDrag: TEndDragEvent read FOnEndDrag write SetOnEndDrag;
    {See TPaintBox.OnEndDrag.}
    property OnKeyDown;
    {See TWinControl.OnKeyDown.}
    property OnKeyPress;
    {See TWinControl.OnKeyPress.}
    property OnKeyUp;
    {See TWinControl.OnKeyUp.}
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    {See TPaintBox.OnMouseDown.}
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove
      write SetOnMouseMove;
    {See TPaintBox.OnMouseMove.}
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    {See TPaintBox.OnMouseUp.}
    property OnPaint: TNotifyEvent read GetOnPBPaint write SetOnPBPaint;
    {See TPaintBox.OnPaint.}
    property OnScrollHorizontal: TNotifyEvent read FOnScrollHorizontal write SetOnScrollHorizontal;
    property OnScrollVertical: TNotifyEvent read FOnScrollVertical write SetOnScrollVertical;
    property OnStartDrag: TStartDragEvent read FOnStartDrag
      write SetOnStartDrag;
    {See TPaintBox.OnStartDrag.}
    property PBColor : TColor read GetPBColor Write SetPBColor;
    {See TPaintBox.Color.}
    property PBCursor : TCursor read GetPBCursor Write SetPBCursor;
    {See TPaintBox.Cursor.}
    property PBDragCursor : TCursor read GetPBDragCursor Write SetPBDragCursor;
    {See TPaintBox.DragCursor.}
    property PBDragKind: TDragKind read GetPBDragKind Write SetPBDragKind;
    {See TPaintBox.DragKind.}
    property PBDragMode: TDragMode read GetPBDragMode Write SetPBDragMode;
    {See TPaintBox.DragMode.}
    property PBEnabled: Boolean read GetPBEnabled Write SetPBEnabled;
    {See TPaintBox.Enabled.}
    property PBFont : TFont read GetPBFont write SetPBFont;
    {See TPaintBox.Font.}
    property PBHeight : Integer read GetPBHeight write SetPBHeight;
    {See TPaintBox.Height.}
    property PBHint : string read GetPBHint write SetPBHint;
    {See TPaintBox.Hint.}
    property PBLeft : integer read GetPBLeft write SetPBLeft;
    {See TPaintBox.Left.}
    property PBPopupMenu : TPopupMenu read GetPBPopupMenu write SetPBPopupMenu;
    {See TPaintBox.PopupMenu.}
    property PBShowHint: Boolean read GetPBShowHint write SetPBShowHint;
    {See TPaintBox.ShowHint.}
    property PBTag : Longint read GetPBTag write SetPBTag;
    {See TPaintBox.Tag.}
    property PBTop: Integer read GetPBTop write SetPBTop;
    {See TPaintBox.Top.}
    property PBVisible: Boolean read GetPBVisible write SetPBVisible;
    {See TPaintBox.Visible.}
    property PBWidth : Integer read GetPBWidth write SetPBWidth;
    {See TPaintBox.Width.}
    property SBrush: TBrush read GetSBrush write SetSBrush;
    {See TShape.Brush.}
    property SCursor: TCursor read GetSCursor write SetSCursor;
    {See TShape.Cursor.}
    property SDragCursor: TCursor read GetSDragCursor write SetSDragCursor;
    {See TShape.DragCursor.}
    property SelectionWidth : integer read FSelectionWidth
      write FSelectionWidth;
    property SPen:  TPen read GetSPen write SetSPen;
    {See TShape.Pen.}
    property XPositive : boolean read FXPositive write FXPositive default True;
    {XPositive is true if the direction of the real-number X-coordinate axis is
    positive to the right.}
    property YPositive : boolean read FYPositive write FYPositive default True;
    {YPositive is true if the direction of the real-number Y-coordinate axis is
    positive upward.}
    {VerticalExaggeration represents the degree of vertical exaggeration that
    will be used in converting real-number Y-coordinates to the screen
    Y-coordinates.}
    { Published declarations }
  end;

  TRbwZoomPoint = class(TObject)
  private
    FY: Extended;
    FX: Extended;
    Parent : TRbwZoomBox;
    FUseForZoomOut: boolean;
    procedure SetUseForZoomOut(const Value: boolean);
  protected
    function GetXCoord : Integer; virtual;
    function GetYCoord : Integer; virtual;
    Procedure SetXCoord(const AnX : Integer); virtual;
    Procedure SetYCoord(const AY  : Integer); virtual;
    procedure SetX(const Value: Extended); virtual;
    procedure SetY(const Value: Extended); virtual;
  public
    Constructor Create (const An_RbwZoomBox : TRbwZoomBox); virtual;
    Destructor Destroy ; override;
    Property UseForZoomOut : boolean read FUseForZoomOut write SetUseForZoomOut;
    {If UseForZoomOut is true, when GetMinMax is performed on the TRbwZoomBox,
    the TRbwZoomPoint will be used to determine the range of points that should
    be displayed.}
    Property X : Extended read FX write SetX;
    {X is the real number X-coordinate of the TRbwZoomPoint}
    Property Y : Extended read FY write SetY;
    {Y is the real number Y-coordinate of the TRbwZoomPoint}
    Property XCoord : Integer read GetXCoord write SetXCoord;
    {XCoord is the screen number X-coordinate of the TRbwZoomPoint}
    property YCoord : Integer read GetYCoord write SetYCoord;
    {YCoord is the screen number Y-coordinate of the TRbwZoomPoint}
    property ZoomBox : TRbwZoomBox read Parent;
    {ZoomBox is the TRbwZoomBox with which the TRbwZoomPoint is associated.}
  end;

procedure Register;

implementation

{$R *.RES}

procedure Register;
begin
  RegisterComponents('RBW', [TRbwZoomBox]);
end;

{ TRbwZoomBox }

procedure TRbwZoomBox.BeginZoom(X, Y: Integer);
begin
  ZoomBegun := True;
  StartX := X - HorzScrollBar.Position;
  StartY := Y - VertScrollBar.Position;
end;

procedure TRbwZoomBox.CancelZoom;
begin
  ZoomBegun := False;
end;

procedure TRbwZoomBox.ContinueZoom(X, Y: Integer);
begin

  if ZoomBegun then
  begin
    EndX := X - HorzScrollBar.Position;
    EndY := Y - VertScrollBar.Position;
    if StartX < EndX then
    begin
      FZoomWindow.Left := StartX;
      FZoomWindow.Width := EndX - StartX;
    end
    else
    begin
      FZoomWindow.Left := EndX;
      FZoomWindow.Width := StartX - EndX;
    end;
    if StartY < EndY then
    begin
      FZoomWindow.Top := StartY;
      FZoomWindow.Height := EndY - StartY;
    end
    else
    begin
      FZoomWindow.Top := EndY;
      FZoomWindow.Height := StartY - EndY;
    end;
  end;
end;

constructor TRbwZoomBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVerticalExaggeration := 1;
  FSelectionWidth := 3;
  XPositive := True;
  YPositive := True;
  Panning := False;
  PointsList := TList.Create;
  FMaxX := 1;
  FMaxY := 1;
  FTopMargin := 20;
  FLeftMargin := 20;
  FRightMargin := 20;
  FBottomMargin := 20;
  FPaintBox := TPaintBox.Create(self);
  FPaintBox.Parent := self;
  FZoomWindow := TShape.Create(self);
  FZoomWindow.Parent := self;
  FZoomWindow.Width := 0;
  FZoomWindow.Height := 0;
  FZoomWindow.Brush.Style := bsClear;
  FMultiplier := 1;
  FPaintBox.OnMouseMove := PanMove;
  FZoomWindow.OnMouseMove := PanMove;
  FPaintBox.OnMouseUp := PanUp;
  FZoomWindow.OnMouseUp := PanUp;
end;

procedure TRbwZoomBox.CreateWnd;
begin
  inherited CreateWnd;
  FPaintBox.Width := Round(ClientWidth*FMultiplier);
  FPaintBox.Height := Round(ClientHeight*FMultiplier);
  FMinX := LeftMargin;
  FMaxX := ClientWidth - RightMargin;
  FMinY := BottomMargin;
  FMaxY := ClientHeight - TopMargin;
end;

procedure TRbwZoomBox.FinishZoom(X, Y: Integer);
var
  XMult, YMult, LocalMult : Extended;
  LeftX, RightX, TopY, BottomY : integer;
  APoint : TRbwZoomPoint;
  Width, Height : Integer;
begin
  ZoomBegun := False;
  FZoomWindow.Width := 0;
  FZoomWindow.Height := 0;
  FPaintbox.Cursor := crDefault;
  EndX := X - HorzScrollBar.Position;
  EndY := Y - VertScrollBar.Position;

  if StartX < EndX then
  begin
    LeftX := StartX + HorzScrollBar.Position;
    RightX := EndX + HorzScrollBar.Position ;
  end
  else
  begin
    LeftX := EndX + HorzScrollBar.Position;
    RightX := StartX + HorzScrollBar.Position;
  end;
  if StartY < EndY then
  begin
    TopY := StartY + VertScrollBar.Position;
    BottomY := EndY + VertScrollBar.Position;
  end
  else
  begin
    TopY := EndY + VertScrollBar.Position;
    BottomY := StartY + VertScrollBar.Position;
  end;

  if (RightX > LeftX) and (BottomY > TopY) then
  begin
    APoint := TRbwZoomPoint.Create(self);
    try
      begin
        APoint.UseForZoomOut := False;

        XMult := (ClientWidth*FMultiplier )/(RightX-LeftX);
        YMult := (ClientHeight*FMultiplier)/(BottomY-TopY);

        APoint.XCoord := LeftX ;
        APoint.YCoord := TopY;

        if XMult < YMult then
        begin
          LocalMult := XMult;
        end
        else
        begin
          LocalMult := YMult;
        end;
        Width := LeftMargin + RightMargin
          + Round((MaxX-MinX)*LocalMult);
        if Width > High(Smallint) then
        begin
          LocalMult := (High(Smallint) -1
            - (LeftMargin + RightMargin))/(MaxX-MinX);
          Width := (LeftMargin + RightMargin)
            + Round((MaxX-MinX)*LocalMult);
        end;
        Height := TopMargin + BottomMargin
          + Round((MaxY-MinY)*LocalMult*VerticalExaggeration);
        if Height > High(Smallint) then
        begin
          LocalMult := (High(Smallint) -1 -
            (TopMargin + BottomMargin))/(MaxY-MinY)/VerticalExaggeration;
          Width := (LeftMargin + RightMargin)
            + Round((MaxX-MinX)*LocalMult);
          Height := (TopMargin + BottomMargin)
            + Round((MaxY-MinY)*LocalMult*VerticalExaggeration);
        end;

        FPaintbox.Width := Width;
        FPaintbox.Height := Height;
        FMultiplier := LocalMult;
        HorzScrollBar.Range := FPaintbox.Width;
        VertScrollBar.Range := FPaintbox.Height;
        HorzScrollBar.Position := APoint.XCoord;
        VertScrollBar.Position := APoint.YCoord;
        FPaintbox.Invalidate;
      end;
    finally
      begin
        APoint.Free;
      end;
    end;
  end;
end;

procedure TRbwZoomBox.SetMaxX(const Value: extended);
begin
  FMaxX := Value;
  if FMinX >= FMaxX then
  begin
    FMinX := FMaxX -1;
  end;
end;

procedure TRbwZoomBox.SetMaxY(const Value: extended);
begin
  FMaxY := Value;
  if FMinY >= FMaxY then
  begin
    FMinY := FMaxY -1;
  end;
end;

procedure TRbwZoomBox.SetMinX(const Value: extended);
begin
  FMinX := Value;
  if FMinX >= FMaxX then
  begin
    FMaxX := FMinX +1;
  end;
end;

procedure TRbwZoomBox.SetMinY(const Value: extended);
begin
  FMinY := Value;
  if FMinY >= FMaxY then
  begin
    FMaxY := FMinY +1;
  end;
end;



function TRbwZoomBox.ReadPBCanvas : TCanvas;
begin
  result := FPaintBox.Canvas ;
end;

function TRbwZoomBox.ReadPBClientHeight: Integer;
begin
  result := FPaintBox.ClientHeight;
end;

function TRbwZoomBox.ReadPBClientWidth: Integer;
begin
  result := FPaintBox.ClientWidth;
end;

function TRbwZoomBox.GetPBClientOrigin: TPoint;
begin
  result := FPaintBox.ClientOrigin;
end;

function TRbwZoomBox.GetPBClientRect: TRect;
begin
  result := FPaintBox.ClientRect;
end;

procedure TRbwZoomBox.SetPBColor(const Value: TColor);
begin
  FPaintbox.Color := Value;
end;

function TRbwZoomBox.GetPBColor: TColor;
begin
  result := FPaintbox.Color;
end;

function TRbwZoomBox.GetPBComponentCount: integer;
begin
  result := FPaintbox.ComponentCount;
end;

function TRbwZoomBox.GetPBComponentIndex: integer;
begin
  result := FPaintbox.ComponentIndex;
end;

procedure TRbwZoomBox.SetPBComponentIndex(const Value: integer);
begin
  FPaintbox.ComponentIndex := Value;
end;

function TRbwZoomBox.GetPBComponents(Index: integer): TComponent;
begin
  result :=  FPaintbox.Components[Index];
end;

function TRbwZoomBox.GetPBControlState: TControlState;
begin
  result :=  FPaintbox.ControlState;
end;

procedure TRbwZoomBox.SetPBControlState(const Controlstate: TControlState);
begin
  FPaintbox.ControlState := Controlstate;
end;

function TRbwZoomBox.GetPBControlStyle: TControlStyle;
begin
  result :=  FPaintbox.ControlStyle;
end;

procedure TRbwZoomBox.SetPBControlStyle(const ControlStyle: TControlStyle);
begin
  FPaintbox.ControlStyle := ControlStyle;
end;

function TRbwZoomBox.GetPBCursor: TCursor;
begin
  result :=  FPaintbox.Cursor;
end;

procedure TRbwZoomBox.SetPBCursor(const Cursor: TCursor);
begin
  FPaintbox.Cursor := Cursor;
end;

function TRbwZoomBox.GetPBDragCursor: TCursor;
begin
  result := FPaintBox.DragCursor
end;

function TRbwZoomBox.GetPBDragKind: TDragKind;
begin
  result := FPaintBox.DragKind
end;

procedure TRbwZoomBox.SetPBDragCursor(const Value: TCursor);
begin
  FPaintBox.DragCursor := Value
end;

procedure TRbwZoomBox.SetPBDragKind(const Value: TDragKind);
begin
  FPaintBox.DragKind := Value
end;

function TRbwZoomBox.GetPBDragMode: TDragMode;
begin
  result := FPaintBox.DragMode;
end;

procedure TRbwZoomBox.SetPBDragMode(const Value: TDragMode);
begin
  FPaintBox.DragMode := Value;
end;

function TRbwZoomBox.GetPBEnabled: Boolean;
begin
  result := FPaintBox.Enabled;
end;

procedure TRbwZoomBox.SetPBEnabled(const Value: Boolean);
begin
  FPaintBox.Enabled := Value;
end;

function TRbwZoomBox.GetPBFont: TFont;
begin
  result := FPaintBox.Font;
end;

procedure TRbwZoomBox.SetPBFont(const Value: TFont);
begin
  FPaintBox.Font := Value;
end;

function TRbwZoomBox.GetPBHeight: Integer;
begin
  result := FPaintBox.Height;
end;

procedure TRbwZoomBox.SetPBHeight(const Value: Integer);
begin
  FPaintBox.Height := Value;
end;

function TRbwZoomBox.GetPBHint: string;
begin
  result := FPaintBox.Hint;
end;

procedure TRbwZoomBox.SetPBHint(const Value: string);
begin
  FPaintBox.Hint := Value;
end;

function TRbwZoomBox.GetPBLeft: Integer;
begin
  result := FPaintBox.Left;
end;

procedure TRbwZoomBox.SetPBLeft(const Value: Integer);
begin
  FPaintBox.Left := Value;
end;

function TRbwZoomBox.GetPBPopupMenu: TPopupMenu;
begin
  result := FPaintBox.PopUpMenu;
end;

procedure TRbwZoomBox.SetPBPopupMenu(const Value: TPopupMenu);
begin
  FPaintBox.PopUpMenu := Value;
end;

function TRbwZoomBox.GetPBShowHint: Boolean;
begin
  result := FPaintBox.ShowHint;
end;

procedure TRbwZoomBox.SetPBShowHint(const Value: Boolean);
begin
  FPaintBox.ShowHint := Value;
end;

function TRbwZoomBox.GetPBTop: Integer;
begin
  result := FPaintbox.Top
end;

procedure TRbwZoomBox.SetPBTop(const Value: Integer);
begin
  FPaintbox.Top := Value;
end;

function TRbwZoomBox.GetPBVisible: Boolean;
begin
  result := FPaintbox.Visible
end;

procedure TRbwZoomBox.SetPBVisible(const Value: Boolean);
begin
  FPaintbox.Visible := Value;
end;

function TRbwZoomBox.GetPBWidth: Integer;
begin
  result := FPaintbox.Width
end;

procedure TRbwZoomBox.SetPBWidth(const Value: Integer);
begin
  FPaintbox.Width := Value;
end;

function TRbwZoomBox.GetPBWindowProc: TWndMethod;
begin
  result := FPaintbox.WindowProc
end;

procedure TRbwZoomBox.SetPBWindowProc(const Value: TWndMethod);
begin
  FPaintbox.WindowProc := Value;
end;

function TRbwZoomBox.GetPBComObject: IUnknown;
begin
  result := FPaintbox.ComObject;
end;

function TRbwZoomBox.GetPBTag: Longint;
begin
  result := FPaintBox.Tag;
end;

procedure TRbwZoomBox.SetPBTag(const Value: Integer);
begin
  FPaintBox.Tag := Value;
end;

function TRbwZoomBox.GetSBrush: TBrush;
begin
  result := FZoomWindow.Brush
end;

procedure TRbwZoomBox.SetSBrush(const Value: TBrush);
begin
  FZoomWindow.Brush := Value;
end;

function TRbwZoomBox.GetSPen: TPen;
begin
  result := FZoomWindow.Pen;
end;

procedure TRbwZoomBox.SetSPen(const Value: TPen);
begin
  FZoomWindow.Pen := Value;
end;

function TRbwZoomBox.GetSBoundsRect: TRect;
begin
  result := FZoomWindow.BoundsRect;
end;

procedure TRbwZoomBox.SetSBoundsRect(const Value: TRect);
begin
  FZoomWindow.BoundsRect := Value;
end;

function TRbwZoomBox.GetSCursor: TCursor;
begin
  result := FZoomWindow.Cursor;
end;

procedure TRbwZoomBox.SetSCursor(const Value: TCursor);
begin
  FZoomWindow.Cursor := Value;
end;

function TRbwZoomBox.GetSDragCursor: TCursor;
begin
  result := FZoomWindow.DragCursor;
end;

procedure TRbwZoomBox.SetSDragCursor(const Value: TCursor);
begin
  FZoomWindow.DragCursor := Value;
end;

function TRbwZoomBox.GetOnPBPaint: TNotifyEvent;
begin
  result := FPaintBox.OnPaint
end;

procedure TRbwZoomBox.SetOnPBPaint(const Value: TNotifyEvent);
begin
  FPaintBox.OnPaint := Value;
end;

procedure TRbwZoomBox.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
  FPaintBox.OnClick := Value;
end;

procedure TRbwZoomBox.SetOnDblClick(const Value: TNotifyEvent);
begin
  FOnDblClick := Value;
  FPaintBox.OnDblClick := Value;
end;

procedure TRbwZoomBox.SetOnDragDrop(const Value: TDragDropEvent);
begin
  FOnDragDrop := Value;
  FPaintBox.onDragDrop := Value;
  FZoomWindow.OnDragDrop := Value;
end;

procedure TRbwZoomBox.SetOnDragOver(const Value: TDragOverEvent);
begin
  FOnDragOver := Value;
  FPaintBox.OnDragOver := Value;
  FZoomWindow.OnDragOver := Value;
end;

procedure TRbwZoomBox.SetOnEndDrag(const Value: TEndDragEvent);
begin
  FOnEndDrag := Value;
  FPaintBox.OnEndDrag := Value;
  FZoomWindow.OnEndDrag := Value;
end;

procedure TRbwZoomBox.SetOnMouseDown(const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
  FPaintBox.OnMouseDown := Value;
  FZoomWindow.OnMouseDown := Value;
end;

procedure TRbwZoomBox.PanMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  If Assigned(FOnMouseMove) then
  begin
    FOnMouseMove(Sender, Shift, X, Y);
  end;
  Pan;
end;

procedure TRbwZoomBox.PanUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If Assigned(FOnMouseUp) then
  begin
    FOnMouseUp(Sender, Button, Shift, X, Y);
  end;
  Pan;
end;

procedure TRbwZoomBox.SetOnMouseMove(const Value: TMouseMoveEvent);
begin
  FOnMouseMove := Value;
end;

procedure TRbwZoomBox.SetOnMouseUp(const Value: TMouseEvent);
begin
  FOnMouseUp := Value;
end;

procedure TRbwZoomBox.SetOnScrollHorizontal(const Value: TNotifyEvent);
begin
  FOnScrollHorizontal := Value;
end;

procedure TRbwZoomBox.SetOnScrollVertical(const Value: TNotifyEvent);
begin
  FOnScrollVertical := Value;
end;

procedure TRbwZoomBox.SetOnStartDrag(const Value: TStartDragEvent);
begin
  FOnStartDrag := Value;
  FPaintBox.OnStartDrag := Value;
  FZoomWindow.OnStartDrag := Value;
end;

procedure TRbwZoomBox.ZoomOut;
begin
  HorzScrollBar.Position := 0;
  VertScrollBar.Position := 0;
  HorzScrollBar.Visible := False;
  VertScrollBar.Visible := False;
  FPaintBox.Width := ClientWidth;
  FPaintBox.Height:= ClientHeight;
  HorzScrollBar.Range := ClientWidth;
  VertScrollBar.Range := ClientHeight;
  HorzScrollBar.Visible := True;
  VertScrollBar.Visible := True;
  GetMinMax;
  FMultiplier := FDefaultMultiplier;
  FPaintBox.Invalidate;
end;

procedure TRbwZoomBox.GetMinMax;
var
  PointIndex : integer;
  AValue : Extended;
  XMultiplier, YMultiplier : Extended;
  APoint : TRbwZoomPoint;
  Increment: double;
begin
  if PointsList.Count > 0 then
  begin
    APoint := PointsList.Items[0];
    FMinX := APoint.X;
    FMinY := APoint.Y;
    FMaxX := FMinX;
    FMaxY := FMinY;
    for PointIndex := 1 to PointsList.Count -1 do
    begin
      APoint := PointsList.Items[PointIndex];
      AValue := APoint.X;
      if AValue < MinX then MinX := AValue;
      if AValue > MaxX then MaxX := AValue;
      AValue := APoint.Y;
      if AValue < MinY then MinY := AValue;
      if AValue > MaxY then MaxY := AValue;
    end;
  end
  else
  begin
    FMinX := LeftMargin;
    FMaxX := ClientWidth - RightMargin;
    FMinY := BottomMargin;
    FMaxY := ClientHeight - TopMargin;
  end;
  Increment := 1e-323;
  While MaxX <= MinX do
  begin
    MaxX := MinX + Increment;
    Increment := Increment * 2;
  end;
  Increment := 1e-323;
  While MaxY <= MinY do
  begin
    MaxY := MinY + Increment;
    Increment := Increment * 2;
  end;
  XMultiplier := (FPaintBox.Width-(LeftMargin + RightMargin))
    /(MaxX-MinX);
  YMultiplier := (FPaintBox.Height-(TopMargin + BottomMargin))
    /(MaxY-MinY)/VerticalExaggeration;
  if XMultiplier < YMultiplier then
  begin
    FDefaultMultiplier := XMultiplier;
  end
  else
  begin
    FDefaultMultiplier := YMultiplier;
  end;
end;

procedure TRbwZoomBox.SetRange(MinimumX, MaximumX, MinimumY,
  MaximumY : Extended);
var
  XMultiplier, YMultiplier : Extended;
begin
  FMinX := MinimumX;
  FMaxX := MaximumX;
  FMinY := MinimumY;
  FMaxY := MaximumY;
  if MaxX = MinX then
  begin
    MaxX := MaxX + 1;
  end;
  if MaxY = MinY then
  begin
    MaxY := MaxY + 1;
  end;
  XMultiplier := (FPaintBox.Width -(LeftMargin + RightMargin))/(MaxX-MinX);
  YMultiplier := (FPaintBox.Height-(TopMargin + BottomMargin))/(MaxY-MinY)
    /VerticalExaggeration;
  if XMultiplier < YMultiplier then
  begin
    FMultiplier := XMultiplier;
  end
  else
  begin
    FMultiplier := YMultiplier;
  end;
end;

procedure TRbwZoomBox.SetXRange(MinimumX, MaximumX : Extended);
begin

  FMinX := MinimumX;
  FMaxX := MaximumX;
  if MaxX = MinX then
  begin
    MaxX := MaxX + 1;
  end;
  FMultiplier := (FPaintBox.Width-(LeftMargin + RightMargin))/(MaxX-MinX);
end;

procedure TRbwZoomBox.SetYRange(MinimumY, MaximumY : Extended);
begin

  FMinY := MinimumY;
  FMaxY := MaximumY;
  if MaxY = MinY then
  begin
    MaxY := MaxY + 1;
  end;
  FMultiplier := (FPaintBox.Height-(TopMargin + BottomMargin))/(MaxY-MinY)
    /VerticalExaggeration;
end;

procedure TRbwZoomBox.DeletePoints(Points: array of TRbwZoomPoint);
var
  APoint: TRbwZoomPoint;
  Index: integer;
  PointIndex: integer;
begin
  for Index := 0 to Length(Points) - 1 do
  begin
    APoint := Points[Index];
    Assert(APoint.Parent = Self);
    PointIndex := PointsList.IndexOf(APoint);
    Assert(PointIndex >= 0);
    PointsList[PointIndex] := nil;
    APoint.Parent := nil;
  end;
  PointsList.Pack;
end;

destructor TRbwZoomBox.Destroy;
begin
  PointsList.Free;
  Inherited Destroy;
end;

procedure TRbwZoomBox.AbortZoom;
begin
  ZoomBegun := False;
  FZoomWindow.Width := 0;
  FZoomWindow.Height := 0;
  FPaintbox.Cursor := crDefault;
end;

procedure TRbwZoomBox.DoExit;
begin
  self.AbortZoom;
  inherited;
end;

function TRbwZoomBox.SetZoom(AZoomLevel: Extended) : Extended;
var
  TopLeft, BottomRight : TRbwZoomPoint;
begin
  If AZoomLevel <= 0 then raise EInvalidZoomLevel.Create(
    'Error: ZoomLevels must be greater than 0.');
  TopLeft := TRbwZoomPoint.Create(self);
  BottomRight := TRbwZoomPoint.Create(self);
  try
    begin
      TopLeft.UseForZoomOut := False;
      BottomRight.UseForZoomOut := False;
      TopLeft.XCoord := HorzScrollBar.Position;
      TopLeft.YCoord := VertScrollBar.Position;
      BottomRight.XCoord := HorzScrollBar.Position + ClientWidth;
      BottomRight.YCoord := VertScrollBar.Position + ClientHeight;
      BottomRight.X := (BottomRight.X - TopLeft.X)/AZoomLevel*FMultiplier
        + TopLeft.X;
      BottomRight.Y := (BottomRight.Y - TopLeft.Y)/AZoomLevel*FMultiplier
        + TopLeft.Y;
      BeginZoom(TopLeft.XCoord,TopLeft.YCoord);
      FinishZoom(BottomRight.XCoord,BottomRight.YCoord);
      result := FMultiplier;
    end;
  finally
    begin
      TopLeft.Free;
      BottomRight.Free;
    end;
  end;
end;

procedure TRbwZoomBox.ZoomBy(ZoomFactor: Extended);
begin
  ZoomByAt(ZoomFactor,
    X(HorzScrollBar.Position+ClientWidth div 2),
    Y(VertScrollBar.Position+ClientHeight div 2)
    );
end;


procedure TRbwZoomBox.ZoomBy1(ZoomFactor: Extended);
begin
  SetZoom(ZoomFactor * FMultiplier);
end;

procedure TRbwZoomBox.ZoomByAt(ZoomFactor, X, Y: Extended);
var
  AZoomPoint : TRbwZoomPoint;
begin
  AZoomPoint := TRbwZoomPoint.Create(self);
  try
    begin
      AZoomPoint.UseForZoomOut := False;
      AZoomPoint.X := X;
      AZoomPoint.Y := Y;
      ZoomBy1(ZoomFactor);
      HorzScrollBar.Position := AZoomPoint.XCoord - Round(Width/2);
      VertScrollBar.Position := AZoomPoint.YCoord - Round(Height/2);
    end;
  finally
    begin
      AZoomPoint.Free;
    end;
  end;

end;

procedure TRbwZoomBox.MouseToCoordinates(const AnXCoord, AYCoord: integer;
  var AnX, AY: extended);
begin
  AnX := X(AnXCoord);
  AY  := Y(AYCoord);
end;

procedure TRbwZoomBox.Invalidate;
begin
  inherited;
  FPaintBox.Invalidate;
end;

procedure TRbwZoomBox.PBBeginDrag(Immediate: Boolean; Threshold: Integer);
begin
  FPaintBox.BeginDrag(Immediate, Threshold);
end;

procedure TRbwZoomBox.PBBringToFront;
begin
  FPaintBox.BringToFront;
end;

function TRbwZoomBox.PBClientToScreen(const Point: TPoint): TPoint;
begin
  result :=  FPaintBox.ClientToScreen( Point);
end;

procedure TRbwZoomBox.PBDragDrop(Source: TObject; X, Y: Integer);
begin
  FPaintBox.DragDrop(Source, X, Y);
end;

function TRbwZoomBox.PBDragging: Boolean;
begin
  result := FPaintBox.Dragging;
end;

procedure TRbwZoomBox.PBEndDrag(Drop: Boolean);
begin
  FPaintBox.EndDrag(Drop);
end;

function TRbwZoomBox.PBGetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  result := FPaintBox.GetTextBuf(Buffer, BufSize);
end;

function TRbwZoomBox.PBGetTextLen: Integer;
begin
  result := FPaintBox.GetTextLen;
end;

procedure TRbwZoomBox.PBHide;
begin
  FPaintBox.Hide;
end;

function TRbwZoomBox.PBPerform(Msg: Cardinal; WParam,
  LParam: Integer): Longint;
begin
  result := FPaintBox.Perform(Msg, WParam, LParam);
end;

procedure TRbwZoomBox.PbRefresh;
begin
  FPaintBox.Refresh;
end;

procedure TRbwZoomBox.PBRepaint;
begin
  FPaintBox.Repaint;
end;

function TRbwZoomBox.PBScreenToClient(const Point: TPoint): TPoint;
begin
  result := FPaintBox.ScreenToClient(Point);
end;

procedure TRbwZoomBox.PBSendToBack;
begin
  FPaintBox.SendToBack;
end;

procedure TRbwZoomBox.PBSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FPaintBox.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TRbwZoomBox.PBSetTextBuf(Buffer: PChar);
begin
  FPaintBox.SetTextBuf(Buffer);
end;

procedure TRbwZoomBox.PBShow;
begin
  FPaintBox.Show;
end;

procedure TRbwZoomBox.PBUpdate;
begin
  FPaintBox.Update;
end;

procedure TRbwZoomBox.SetMultiplier( Value: extended);
var
  NewWidth, NewHeight : Integer;
begin
  if Value <> FMultiplier then
  begin
    NewWidth :=  Trunc((FPaintBox.Width - (LeftMargin + RightMargin))
      * Value / FMultiplier) + (LeftMargin + RightMargin);
    If NewWidth > High(Smallint) then
    begin
      Value := Value * High(Smallint) / NewWidth
    end;
    NewHeight :=  Trunc((FPaintBox.Height - (TopMargin + BottomMargin))
      * Value / FMultiplier)+ (TopMargin + BottomMargin);
    If NewHeight > High(Smallint) then
    begin
      Value := Value * High(Smallint) / NewHeight
    end;
    PBWidth :=  Trunc((FPaintBox.Width - (LeftMargin + RightMargin))
      * Value / FMultiplier) + (LeftMargin + RightMargin);
    PBHeight :=  Trunc((FPaintBox.Height - (TopMargin + BottomMargin))
      * Value / FMultiplier)+ (TopMargin + BottomMargin);

    HorzScrollBar.Range := PBWidth;
    VertScrollBar.Range := PBHeight;
    FMultiplier := Value;
  end;
end;

procedure TRbwZoomBox.BeginPan;
begin
  StartXScrollBarPosition := HorzScrollBar.Position;
  StartYScrollBarPosition := VertScrollBar.Position;
  StartXPosition := Mouse.CursorPos.X;
  StartYPosition := Mouse.CursorPos.Y;
  Panning := True;
end;

procedure TRbwZoomBox.EndPan;
begin
  Panning := False;
end;

procedure TRbwZoomBox.Pan;
begin
  if Panning then
  begin
    HorzScrollBar.Position := StartXScrollBarPosition
      - (Mouse.CursorPos.x - StartXPosition);
    VertScrollBar.Position := StartYScrollBarPosition
      - (Mouse.CursorPos.y - StartYPosition);
  end;
end;

procedure TRbwZoomBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Pan;
end;

procedure TRbwZoomBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  Pan;
end;

procedure TRbwZoomBox.SetBottomMargin(const Value: integer);
begin
  FBottomMargin := Value;
  ZoomOut;
end;

procedure TRbwZoomBox.SetLeftMargin(const Value: integer);
begin
  FLeftMargin := Value;
  ZoomOut;
end;

procedure TRbwZoomBox.SetRightMargin(const Value: integer);
begin
  FRightMargin := Value;
  ZoomOut;
end;

procedure TRbwZoomBox.SetTopMargin(const Value: integer);
begin
  FTopMargin := Value;
  ZoomOut;
end;

function TRbwZoomBox.X(const XCoord: integer): extended;
begin
  if XPositive then
  begin
    result :=  (XCoord -  LeftMargin)/FMultiplier + MinX
  end
  else
  begin
   result := (FPaintBox.Width - XCoord -
     RightMargin )/FMultiplier + MinX ;
  end;
end;

function TRbwZoomBox.XCoord(const X: extended): integer;
begin
  if XPositive then
  begin
    result := Round((X - MinX)*FMultiplier + LeftMargin);
  end
  else
  begin
    result := FPaintBox.Width -
      Round((X - MinX)*FMultiplier + TopMargin);
  end;
end;

function TRbwZoomBox.Y(const YCoord: integer): extended;
begin
  if YPositive then
  begin
    result := (FPaintBox.Height - YCoord -
      BottomMargin )/FMultiplier/VerticalExaggeration + MinY ;
  end
  else
  begin
    result :=  (YCoord -  TopMargin)/FMultiplier/VerticalExaggeration + MinY;
  end;
end;

function TRbwZoomBox.YCoord(const Y: extended): integer;
begin
  if YPositive then
  begin
    result := FPaintBox.Height -
      Round((Y - MinY)*FMultiplier*VerticalExaggeration + BottomMargin);
  end
  else
  begin
    result := Round((Y - MinY)*FMultiplier*VerticalExaggeration + TopMargin);
  end;
end;

procedure TRbwZoomBox.Loaded;
begin
  inherited;
  ZoomOut;
end;

procedure TRbwZoomBox.SetVerticalExaggeration(Value: double);
var
  AZoomPoint : TRbwZoomPoint;
  Height : integer;
  LocalMult : Extended;
begin
  if Value <= 0 then
  begin
    Value := 1;
  end;
  if Value <> FVerticalExaggeration then
  begin
    if not (csDesigning in ComponentState) then
    begin
      AZoomPoint := TRbwZoomPoint.Create(self);
      try
        AZoomPoint.UseForZoomOut := False;
        AZoomPoint.XCoord := HorzScrollBar.Position;
        AZoomPoint.YCoord := VertScrollBar.Position;

        LocalMult := Value/FVerticalExaggeration;

        Height := Round((FPaintBox.Height - TopMargin - BottomMargin)*LocalMult)
          + TopMargin + BottomMargin;
        if Height > High(Smallint) then
        begin
          Value := (High(Smallint) -1 -
            (TopMargin + BottomMargin))/Height;
          Height := High(Smallint) -1;
          LocalMult := Value/FVerticalExaggeration;
        end;

        FPaintbox.Height := Height;
        FMultiplier := FMultiplier*LocalMult;
        VertScrollBar.Range := FPaintBox.Height;

        FVerticalExaggeration := Value;
        HorzScrollBar.Position := AZoomPoint.XCoord;
        VertScrollBar.Position := AZoomPoint.YCoord;
      finally
        AZoomPoint.Free;
      end;
      Invalidate;
    end
    else
    begin
        FVerticalExaggeration := Value;
    end;

  end;
end;

function TRbwZoomBox.SelectPoint(const X, Y: integer;
  const AZoomPoint: TRbwZoomPoint): boolean;
begin
  Result := (Abs(X - AZoomPoint.XCoord) <= SelectionWidth)
    and (Abs(Y - AZoomPoint.YCoord) <= SelectionWidth);
end;

function TRbwZoomBox.SelectSegment(const X, Y: integer; const ZoomPoint1,
  ZoomPoint2: TRbwZoomPoint): boolean;
var
  MinX, MinY, MaxX, MaxY : integer;
  XPos, YPos : extended;
  Slope : extended;
  Intercept : extended;
begin
  result := False;
  if ZoomPoint1.Parent <> ZoomPoint2.Parent then
  begin
    Exit;
  end;

  if ZoomPoint1.XCoord < ZoomPoint2.XCoord then
  begin
    MinX := ZoomPoint1.XCoord - SelectionWidth;
    MaxX := ZoomPoint2.XCoord + SelectionWidth;
  end
  else
  begin
    MaxX := ZoomPoint1.XCoord + SelectionWidth;
    MinX := ZoomPoint2.XCoord - SelectionWidth;
  end;
  if (X > MaxX) or (X < MinX) then
  begin
    Exit;
  end;
  if ZoomPoint1.YCoord < ZoomPoint2.YCoord then
  begin
    MinY := ZoomPoint1.YCoord - SelectionWidth;
    MaxY := ZoomPoint2.YCoord + SelectionWidth;
  end
  else
  begin
    MaxY := ZoomPoint1.YCoord + SelectionWidth;
    MinY := ZoomPoint2.YCoord - SelectionWidth;
  end;
  if (Y > MaxY) or (Y < MinY) then
  begin
    Exit;
  end;
  if (ZoomPoint1.YCoord = ZoomPoint2.YCoord)
    or (ZoomPoint1.XCoord = ZoomPoint2.XCoord) then
  begin
    Result := True;
    Exit;
  end;
  Slope := (ZoomPoint2.Y - ZoomPoint1.Y)/(ZoomPoint2.X - ZoomPoint1.X);
  Intercept := ZoomPoint1.Y - Slope * ZoomPoint1.X;
  if (MaxY - MinY)* ZoomPoint1.Parent.VerticalExaggeration > (MaxX - MinX) then
  begin
    // Slope > 45 degrees: Get X
    YPos := ZoomPoint1.ZoomBox.Y(Y);
    XPos := (YPos - Intercept)/Slope;
    if Abs(ZoomPoint1.ZoomBox.XCoord(XPos) - X) <= SelectionWidth then
    begin
      Result := True;
      Exit;
    end;
  end
  else
  begin
    // Slope < 45 degrees
    XPos := ZoomPoint1.ZoomBox.X(X);
    YPos := Slope*XPos + Intercept;
    if Abs(ZoomPoint1.ZoomBox.YCoord(YPos) - Y) <= SelectionWidth then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TRbwZoomBox.SelectPolyLine(const X, Y: integer;
  const ZoomPointArray: TZBArray): boolean;
Var
  Index : integer;
  ZoomPoint1, ZoomPoint2 :TRbwZoomPoint;
begin
  result := False;
  for Index := 0 to Length(ZoomPointArray) -1 do
  begin
    ZoomPoint1 := ZoomPointArray[Index];
    result := self.SelectPoint(X, Y, ZoomPoint1);
    if Result then Exit;
  end;

  for Index := 0 to Length(ZoomPointArray) -2 do
  begin
    ZoomPoint1 := ZoomPointArray[Index];
    ZoomPoint2 := ZoomPointArray[Index+1];
    result := SelectSegment(X, Y, ZoomPoint1, ZoomPoint2);
    if result then Exit;
  end;
end;

function TRbwZoomBox.IsPointInside(const X, Y: extended;
  const ZoomPointArray: TZBArray): boolean;
var
  VertexIndex : integer;
  AZoomPoint, AnotherZoomPoint : TRbwZoomPoint;
begin   // based on CACM 112
  if Length(ZoomPointArray) < 4 then
  begin
    result := false;
    Exit;
  end;
  AZoomPoint := ZoomPointArray[0];
  AnotherZoomPoint := ZoomPointArray[Length(ZoomPointArray) -1];
  if (AZoomPoint.X <> AnotherZoomPoint.X) or
    (AZoomPoint.Y <> AnotherZoomPoint.Y) then
  begin
    result := False;
    Exit;
  end;

  result := true;
  For VertexIndex := 0 to Length(ZoomPointArray) -2 do
  begin
    AZoomPoint := ZoomPointArray[VertexIndex];
    AnotherZoomPoint := ZoomPointArray[VertexIndex+1];
    if ((Y <= AZoomPoint.Y) = (Y > AnotherZoomPoint.Y)) and
       (X - AZoomPoint.X - (Y - AZoomPoint.Y) *
         (AnotherZoomPoint.X - AZoomPoint.X)/
         (AnotherZoomPoint.Y - AZoomPoint.Y) < 0) then
    begin
      result := not result;
    end;
  end;
  result := not result;
end;

function TRbwZoomBox.SelectPolygon(const X, Y: integer;
  const ZoomPointArray: TZBArray): boolean;
begin
  result := SelectPolyLine(X, Y, ZoomPointArray);
  if result then Exit;
  result := IsPointInside(self.X(X), self.Y(Y), ZoomPointArray);
end;

procedure TRbwZoomBox.ScrollH(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnScrollHorizontal) then
  begin
    FOnScrollHorizontal(self);
  end;
end;

procedure TRbwZoomBox.ScrollV(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnScrollVertical) then
  begin
    FOnScrollVertical(self);
  end;
end;

{ TRbwZoomPoint }

constructor TRbwZoomPoint.Create(const An_RbwZoomBox: TRbwZoomBox);
begin
  inherited Create;
  Assert(An_RbwZoomBox <> nil);
  Parent := An_RbwZoomBox;
  UseForZoomOut := True;
end;

destructor TRbwZoomPoint.Destroy;
begin
  if Parent <> nil then
  begin
    Parent.PointsList.Remove(self);
  end;
  inherited Destroy;
end;

function TRbwZoomPoint.GetXCoord: Integer;
begin
  result := Parent.XCoord(FX);
end;

function TRbwZoomPoint.GetYCoord: Integer;
begin
  result := Parent.YCoord(FY);
end;

procedure TRbwZoomPoint.SetUseForZoomOut(const Value: boolean);
begin
  if FUseForZoomOut <> Value then
  begin
    FUseForZoomOut := Value;
    if FUseForZoomOut then
    begin
      Parent.PointsList.Add(self);
    end
    else
    begin
      Parent.PointsList.Remove(self);
    end;
  end;
end;

procedure TRbwZoomPoint.SetX(const Value: Extended);
begin
  FX := Value;
end;

procedure TRbwZoomPoint.SetXCoord(const AnX: Integer);
begin
  FX := Parent.X(AnX);
end;

procedure TRbwZoomPoint.SetY(const Value: Extended);
begin
  FY := Value;
end;

procedure TRbwZoomPoint.SetYCoord(const AY: Integer);
begin
  FY := Parent.Y(AY);
end;



end.
