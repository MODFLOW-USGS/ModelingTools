unit frameTopUnit;

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QZoomBox, QExtCtrls, QRbwRuler, QStdCtrls, SyncObjs, AbstractGridUnit,
  ContourUnit, SelectUnit, UndoItems, QMenus, QTypes, QRbwModelCube,
  QCustomRbwRuler;

type  
  TCursorGrid = (cgNone, cgTop, cgFront, cgSide);

type
  TframeView = class(TFrame)
  published
    rulVertical: TRbwRuler;
    ZoomBox: TQrbwZoomBox;
    OrderMenu: TPopupMenu;
    ToFront1: TMenuItem;
    ToBack1: TMenuItem;
    ForwardOne1: TMenuItem;
    BackOne1: TMenuItem;
    Panel1: TPanel;
    ModelCube: TRbwModelCube;
    rulHorizontal: TRbwRuler;
    procedure ZoomBoxHorzScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ZoomBoxVertScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ZoomBoxPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomBoxPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ZoomBoxPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomBoxPaintBoxPaint(Sender: TObject);
    procedure rulHorizontalMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ZoomBoxPaintBoxDblClick(Sender: TObject);
    procedure ZoomBoxExit(Sender: TObject);
    procedure ZoomBoxBottomPaintBoxPaint(Sender: TObject);
    procedure ToFront1Click(Sender: TObject);
    procedure ToBack1Click(Sender: TObject);
    procedure ForwardOne1Click(Sender: TObject);
    procedure BackOne1Click(Sender: TObject);
    procedure ModelCubeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ModelCubePaint(Sender: TObject);
    procedure ModelCubeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ModelCubeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OrderMenuPopup(Sender: TObject);
  private
    BitMap : TBitMap;
    CubeX, CubeY : integer;
    CurrentUndo : TCustomUndo;
    DoubleClicked : boolean;
    Drawing : boolean;
    MouseButtonIsDown : boolean;
    MovingContours : boolean;
    PreviousContours : TList;
    PriorCursorX, PriorCursorY : integer;
    Rotating : boolean;
    SelectBottomRight : TPoint;
    SelectedPointContour : T2DContour;
    SelectLine: TLine;
    SelectTopLeft : TPoint;
    StartAngle: double;
    StartX, StartY : integer;
    procedure AddColumnOrLayer(X, Y: Integer);
    procedure AddColumnOrRow(X, Y: Integer);
    procedure AddRowOrLayer(X, Y: Integer);
    function AreContoursSelected: boolean;
    procedure BeginMove(X, Y: Integer);
    procedure BeginTopMove(X, Y: Integer);
    procedure BeginSetSpacing(X, Y: Integer);
    procedure BeginSubdivide(X, Y: Integer);
    procedure ChangeLayer(const X, Y: Integer);
    procedure ContinueLineContour(X, Y: Integer);
    procedure ContinueRectangle(X, Y: Integer);
    procedure ContinueSetSpacing(X, Y: integer);
    procedure ContinueStraightLineContour(X, Y: Integer);
    procedure ContinueSubdivide(X, Y: integer);
    function GetContoursChanged: boolean;
    procedure CreatePointContour(X, Y: Integer);
    function CursorGrid: TCursorGrid;
    procedure DeleteColumnOrLayer(X, Y: Integer);
    procedure DeleteColumnOrRow(X, Y: Integer);
    procedure DeleteRowOrLayer(X, Y: Integer);
    procedure DeleteSegment(const X, Y : integer);
    procedure DrawBlueSelectedTopCells(FirstCol, LastCol, FirstRow,
      LastRow: integer);
    procedure DrawContours;
    procedure DrawGridAndContours;
    procedure DrawModifiedCurrentContour;
    procedure DrawMouseSelectionRectangle;
    procedure DrawRotatedGrid;
    procedure DrawSelectionRectangle;
    procedure GetColLayer(APoint: T2DRealPoint; out Col, Layer: integer);
    Procedure GetRowCol(APoint : T2DRealPoint; out Row, Column : integer);
    procedure GetRowLayer(APoint: T2DRealPoint; out Row, Layer: integer);
    function GridCenter: T2DRealPoint;
    procedure InsertPoint(const X, Y : integer);
    procedure InvalidateContours;
    function IsOnFrontColumn(const X, Y: integer): boolean;
    function IsOnFrontLayer(const X, Y: integer): boolean;
    function IsOnSideLayer(const X, Y: integer): boolean;
    function IsOnSideRow(const X, Y: integer): boolean;
    function IsOnTopColumn(const X, Y: integer): boolean;
    function IsOnTopRow(const X, Y: integer): boolean;
    procedure MoveColumnOrLayer(X, Y: Integer);
    procedure MoveColumnOrRow(X, Y: Integer);
    procedure MoveContours(const X, Y: integer);
    procedure MoveRowOrLayer(X, Y: Integer);
    function RecalculateTopCellColors: boolean;
    procedure RotatedGridCorners(Angle: double;
      out PointArray: array of TPoint);
    function SelectContours(const X, Y : integer;
      const SelectLowerContours, ToggleSelectedItem : boolean;
      const CanChangeSelection: boolean = True): boolean;
    function SelectContoursWithLine(const ToggleSelection : boolean) : boolean;
    function SelectPoints(const X, Y: integer; const AContour: T2DContour;
      const AddToSelection: boolean; var Changed: boolean): boolean;
    function SelectPointsOfAllSelectedContoursWithLine
      (const AddToSelection: boolean): boolean;
    function SelectPointsOfASelectedContour(const X, Y: integer;
      const AddToSelection: boolean): boolean;
    function SelectPointsWithLine(const AContour: T2DContour;
      const AddToSelection: boolean; out Changed: boolean): boolean;
    procedure SetTopCursor(X, Y: integer);
    procedure SetContoursChanged(const Value: boolean);
    procedure SetDeleteSegmentCursor(X, Y: Integer);
    procedure SetInsertPointCursor(X, Y: Integer);
    procedure SetSpacingFront(X, Y: Integer);
    procedure SetSpacingSide(X, Y: Integer);
    procedure SetSpacingTop(X, Y: Integer);
    procedure ShowMovedPoints;
    procedure ShowNewColumnOrRow;
    procedure StartTimer(Sender: TObject);
    procedure SubdivideFront(X, Y: Integer);
    procedure SubdivideSide(X, Y: Integer);
    procedure SubdivideTop(X, Y: Integer);
    procedure UpdateStatusBar(const X, Y: integer);
    procedure WarnTooBig;
    property ContoursChanged: boolean read GetContoursChanged
      write SetContoursChanged;
    function GetNeedToRecalculateCellColors: boolean;
    procedure SetNeedToRecalculateCellColors(const Value: boolean);
    property NeedToRecalculateCellColors : boolean
      read GetNeedToRecalculateCellColors
      write SetNeedToRecalculateCellColors;
    function GetGridChanged: boolean;
    procedure SetGridChanged(const Value: boolean);
    property GridChanged: boolean read GetGridChanged write SetGridChanged;
    procedure ChangeColumn(const X, Y: Integer);
    procedure ChangeRow(const X, Y: Integer);
    procedure BeginFrontMove(X, Y: Integer);
    procedure BeginSideMove(X, Y: Integer);
    procedure DrawBlueSelectedFrontCells(FirstCol, LastCol, FirstLayer,
      LastLayer: integer);
    procedure DrawBlueSelectedSideCells(FirstRow, LastRow, FirstLayer,
      LastLayer: integer);
    procedure DrawSpacing;
    procedure DrawSubdivide;
    procedure SetFrontCursor(X, Y: integer);
    procedure SetSideCursor(X, Y: integer);
    procedure ShowNewColumnOrLayer;
    procedure ShowNewRowOrLayer;
    function RecalculateCellColors: boolean;
    function RecalculateFrontCellColors: boolean;
    function RecalculateSideCellColors: boolean;
    { Private declarations }
  public
    CurrentContour : T2DContour;
    DrawSelectRect : boolean;
    ViewDirection : TViewDirection;
    procedure AdjustHorizontalScale;
    procedure AdjustVerticalScale;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure DisplayColumn;
    procedure DisplayLayer;
    procedure DisplayRow;
    procedure FinishContours;
    procedure LayerChange(Sender: TObject);
    procedure UpdateSelectRectangle;
    procedure ColumnChange(Sender: TObject);
    procedure RowChange(Sender: TObject);
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses frmGoPhastUnit, CursorsFoiledAgain, Math,
  frmContourPropertiesUnit, UndoItemsContours;


{ TframeTop }

procedure TframeView.AdjustHorizontalScale;
begin
  // set the ends of the horizontal ruler so that the scale
  // is displayed properly.
  rulHorizontal.RulerEnds.Lower := 10;

  // This accounts for the width of the vertical scroll bar (width = 18)
  // plus the space between the end of the ruler at the end
  // of the empty space in ZoomBox ( = 12)
  // The value of 30 comes from 18+12
  rulHorizontal.RulerEnds.Upper := rulHorizontal.Width-30;
  if rulHorizontal.RulerStart = sTopLeft then
  begin
    rulHorizontal.RulerValues.Lower := ZoomBox.X(10 + ZoomBox.HorzScrollBar.Position);
    rulHorizontal.RulerValues.Upper := ZoomBox.X(ZoomBox.HorzScrollBar.Width -12
      + ZoomBox.HorzScrollBar.Position);
  end
  else
  begin
    rulHorizontal.RulerValues.Upper := ZoomBox.X(10 + ZoomBox.HorzScrollBar.Position);
    rulHorizontal.RulerValues.Lower := ZoomBox.X(ZoomBox.HorzScrollBar.Width -12
      + ZoomBox.HorzScrollBar.Position);
  end;

end;

procedure TframeView.AdjustVerticalScale;
var
  NewLower, NewUpper : double;
begin
  // set the ends of the vertical ruler so that the scale
  // is displayed properly.
  rulVertical.RulerEnds.Upper := Height - rulHorizontal.Height -30;
  NewLower := ZoomBox.Y(ZoomBox.VertScrollBar.Height -10
    + ZoomBox.VertScrollBar.Position);
  NewUpper := ZoomBox.Y(12 + ZoomBox.VertScrollBar.Position);
  if NewLower < NewUpper then
  begin
    rulVertical.RulerValues.Lower := NewLower;
    rulVertical.RulerValues.Upper := NewUpper;
  end;
end;

procedure TframeView.ZoomBoxHorzScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  // Let the hurizontal ruler know that it needs to update
  // the scale.
  AdjustHorizontalScale;
end;

procedure TframeView.ZoomBoxVertScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  // Let the vertical ruler know that it needs to update
  // the scale.
  AdjustVerticalScale;
end;

procedure TframeView.BeginTopMove(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Row : integer;
begin
  // start to move a column or row.

  // First find the nearest column or row boundaries.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.PhastGrid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Column >= 0) and (Row >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    // The point is over the grid.
    if IsOnTopColumn(X,Y) then
    begin
      // If the point is over a column, set the cursor.
      // and set MovingColumn to true.
      // Store the column that is being moved.
      ZoomBox.Cursor := frmGoPhast.dcMoveColCursor.Cursor;
      ZoomBox.PaintBox.Cursor := frmGoPhast.dcMoveColCursor.Cursor;

      frmGoPhast.MovingColumn := True;
      frmGoPhast.MovingRow := False;
      frmGoPhast.ColumnBeingMoved := Column;
    end
    else if IsOnTopRow(X,Y) then
    begin
      // If the point is over a row, set the cursor.
      // and set MovingRow to true.
      // Store the row that is being moved.
      ZoomBox.Cursor := frmGoPhast.dcMoveRowCursor.Cursor;
      ZoomBox.PaintBox.Cursor := frmGoPhast.dcMoveRowCursor.Cursor;
      frmGoPhast.MovingRow := True;
      frmGoPhast.MovingColumn := False;
      frmGoPhast.RowBeingMoved := Row;
    end;
  end;
end;

procedure TframeView.BeginSetSpacing(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Row, Layer : integer;
begin
  // determine and store the first column, row, and layer that will be subdivided.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          frmGoPhast.FirstSpacingColumn := Column;
          frmGoPhast.FirstSpacingRow := Row;
          frmGoPhast.FirstSpacingLayer := -1;
          frmGoPhast.SettingSpacing := True;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.FirstSpacingColumn := Column;
          frmGoPhast.FirstSpacingRow := -1;
          frmGoPhast.FirstSpacingLayer := Layer;
          frmGoPhast.SettingSpacing := True;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.FirstSpacingColumn := -1;
          frmGoPhast.FirstSpacingRow := Row;
          frmGoPhast.FirstSpacingLayer := Layer;
          frmGoPhast.SettingSpacing := True;
        end;
      end;
  else Assert(False);
  end;

end;


procedure TframeView.BeginSubdivide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Row, Layer: integer;
begin
  // determine and store the first column and row that will be subdivided.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          frmGoPhast.FirstSubdivideColumn := Column;
          frmGoPhast.FirstSubdivideRow := Row;
          frmGoPhast.FirstSubdivideLayer := -1;
          frmGoPhast.Subdividing := True;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.FirstSubdivideColumn := Column;
          frmGoPhast.FirstSubdivideRow := -1;
          frmGoPhast.FirstSubdivideLayer := Layer;
          frmGoPhast.Subdividing := True;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.FirstSubdivideColumn := -1;
          frmGoPhast.FirstSubdivideRow := Row;
          frmGoPhast.FirstSubdivideLayer := Layer;
          frmGoPhast.Subdividing := True;
        end;
      end;
  else Assert(False);
  end;
end;

function TframeView.AreContoursSelected: boolean;
var
  Index : integer;
  AContour : T2DContour;
begin
  result := False;
  for Index := frmGoPhast.Contours.Count -1 downto 0 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected and (AContour.ViewDirection = ViewDirection) then
    begin
      result := True;
      Exit;
    end;
  end;

end;

procedure TframeView.ZoomBoxPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARealPoint : T2DRealPoint;
  Center : T2DRealPoint;
begin
  if ssRight in Shift then
  begin
    // for a right click, don't create objects
    Exit;
  end;
  MouseButtonIsDown := True;
  frmGoPhast.CursorGrid := CursorGrid;

  // Record the current cursor position.
  StartX := X;
  StartY := Y;
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  // Now see what button is down and respond appropriately
  if frmGoPhast.tbZoom.Down then
  begin
    // The user wants to zoom into a specific region.
    // record the first corner of the region.
    if ZoomBox.CanZoomIn then
    begin
      ZoomBox.BeginZoom(X, Y);
    end;
  end
  else if frmGoPhast.tbPan.Down then
  begin
    // The user wants to pan to a different position.
    // Start panning and change the cursor to indicate that panning has
    // begun.
    if ZoomBox.CanPan then
    begin
      ZoomBox.Panning := True;
      ZoomBox.Cursor := crHandGrab;
      ZoomBox.PaintBox.Cursor := crHandGrab;
      Screen.Cursor := crHandGrab;
    end;
  end
  else if frmGoPhast.tbMove.Down then
  begin
    // The user wants to move a grid line.
    BeginMove(X, Y);
  end
  else if frmGoPhast.tbSubdivide.Down then
  begin
    // The user wants to subdivide one or more rows or columns
    BeginSubdivide(X, Y);
  end
  else if frmGoPhast.tbSpacing.Down then
  begin
    // The user wants to subdivide one or more rows or columns
    BeginSetSpacing(X, Y);
  end
  else if frmGoPhast.tbGridAngle.Down then
  begin
    if ViewDirection = vdTop then
    begin
      ARealPoint.X := ZoomBox.X(X);
      ARealPoint.Y := ZoomBox.Y(Y);
      Center := GridCenter;
      StartAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
      Rotating := True;
    end;
  end
  else if frmGoPhast.tbSelect.Down then
  begin
    if ssDouble in Shift then
    begin
      // Don't change the selected contours when double-clicking.
      Exit;
    end;
    // The user wants to select contours and may want to move them too.
    MovingContours := SelectContours
      (X, Y, (ssCtrl in Shift), (ssShift in Shift),
      not AreContoursSelected or (ssCtrl in Shift)
      {or (ssShift in Shift)});
    if not MovingContours then
    begin
      // if the user didn't click on anything, the user must want to
      // draq a box around the objects to be selected.

      // SelectLine is used to define the box.  It will have 5 nodes because
      // it is rectangular and the first node will be at the same position as
      // the last one.
      SelectLine.Free;
      SelectLine := TLine.Create(5);
      SelectLine.AddPoint(Point(X,Y));
      ZoomBox.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbLasso.Down then
  begin
    // The user wants to select contours by dragging a lasso around them.
    // As a first approximation, we will say that the "lasso" will have
    // up to 1000 nodes.  However, if that is too small, the array holding
    // the nodes will be expanded to hold additional nodes.
    SelectLine.Free;
    SelectLine := TLine.Create(1000);
    SelectLine.AddPoint(Point(X,Y));
    ZoomBox.PaintBox.Invalidate;
  end
  else if frmGoPhast.tbSelectPoint.Down then
  begin
    // The user want to select a node on a contour.
    DrawSelectRect := False;
    // See if the user clicked on a node.
    MovingContours := SelectPointsOfASelectedContour(X, Y,
      True {(ssShift in Shift)});
    // define a box that will outline the nodes to be selected.
    SelectLine.Free;
    SelectLine := TLine.Create(5);
    SelectLine.AddPoint(Point(X,Y));
//    ZoomBox.PaintBox.Invalidate;
  end
  else if frmGoPhast.tbInsertPoint.Down or frmGoPhast.tbDeleteSegment.Down then
  begin
    // The user wants to either insert a node in an existing object or
    // delete a segment of an existing object.  In either case, you need
    // to select an object.
    SelectContours(X, Y, (ssCtrl in Shift), (ssShift in Shift));
  end;
end;

function TframeView.IsOnTopColumn(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Column : integer;
begin
  // Find the real world coordinates of the current location
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  APoint := frmGoPhast.PhastGrid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  // find the nearest column to that position.
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.PhastGrid.ColumnCount) then
  begin
    // offset the point to the position of the nearest column
    APoint.X := frmGoPhast.PhastGrid.ColumnPosition[Column];
    // rotate back to screen coordinates
    APoint := frmGoPhast.PhastGrid.RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
    if (Abs(ZoomBox.XCoord(APoint.X) - X) <= SelectionWidth)
      and (Abs(ZoomBox.YCoord(APoint.Y) - Y) <= SelectionWidth) then
    begin
      result := true;
    end;
  end;
end;

function TframeView.IsOnTopRow(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Row : integer;
begin
  // Find the real world coordinates of the current location
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  APoint := frmGoPhast.PhastGrid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  // find the nearest row to that position.
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    // offset the point to the position of the nearest row
    APoint.Y := frmGoPhast.PhastGrid.RowPosition[Row];
    // rotate back to screen coordinates
    APoint := frmGoPhast.PhastGrid.RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
    if (Abs(ZoomBox.XCoord(APoint.X) - X) <= SelectionWidth)
      and (Abs(ZoomBox.YCoord(APoint.Y) - Y) <= SelectionWidth) then
    begin
      result := true;
    end;
  end;
end;

procedure TframeView.SetTopCursor(X, Y: integer);
var
  APoint : T2DRealPoint;
  Column, Row : integer;
begin
  if frmGoPhast.MovingColumn
    or frmGoPhast.MovingRow
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
    // There is no need to change the cursor.
    // Just repaint and quit.
    ZoomBox.PaintBox.Invalidate;
    Exit;
  end;
  // Find the real world coordinates of the current location
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  APoint := frmGoPhast.PhastGrid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  // find the nearest column and row to that position.
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Column >= 0) and (Row >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    // The cursor is over the grid
    if IsOnTopColumn(X,Y) or IsOnTopRow(X,Y) then
    begin
      // The cursor is over a column or row boundary
      if frmGoPhast.tbDeleteColumnRow.Down then
      begin
        // Set the cursor for deleting columns or rows
        ZoomBox.PaintBox.Cursor := crDelete;
        ZoomBox.Cursor := crDelete;
      end
      else if IsOnTopColumn(X,Y) then
      begin
        // Set the cursor for moving columns
        ZoomBox.Cursor := frmGoPhast.dcMoveColCursor.Cursor;
        ZoomBox.PaintBox.Cursor := frmGoPhast.dcMoveColCursor.Cursor;
      end
      else if IsOnTopRow(X,Y) then
      begin
        // Set the cursor for moving rows
        ZoomBox.Cursor := frmGoPhast.dcMoveRowCursor.Cursor;
        ZoomBox.PaintBox.Cursor := frmGoPhast.dcMoveRowCursor.Cursor;
      end;
    end
    else
    begin
      // The cursor is not over a column or row boundary
      // so use the usual cursor.
      ZoomBox.PaintBox.Cursor := crArrow;
      ZoomBox.Cursor := crArrow;
    end;
  end
  else
  begin
      // The cursor is not over the grid
      // so use the usual cursor.
    ZoomBox.PaintBox.Cursor := crArrow;
    ZoomBox.Cursor := crArrow;
  end;
end;

procedure TframeView.ContinueSetSpacing(X, Y : integer);
var
  APoint : T2DRealPoint;
  Column, Row, Layer : integer;
begin
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
          frmGoPhast.LastSpacingColumn := Column;
          frmGoPhast.LastSpacingRow := Row;
          ZoomBox.PaintBox.Invalidate;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.LastSpacingLayer := Layer;
          frmGoPhast.LastSpacingColumn := Column;
          ZoomBox.PaintBox.Invalidate;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.LastSpacingLayer := Layer;
          frmGoPhast.LastSpacingRow := Row;
          ZoomBox.PaintBox.Invalidate;
        end;
      end;
  else ;
  end;

end;

procedure TframeView.ContinueSubdivide(X, Y : integer);
var
  APoint : T2DRealPoint;
  Column, Row, Layer : integer;
begin
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
          frmGoPhast.LastSubdivideColumn := Column;
          frmGoPhast.LastSubdivideRow := Row;
          ZoomBox.PaintBox.Invalidate;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.LastSubdivideLayer := Layer;
          frmGoPhast.LastSubdivideColumn := Column;
          ZoomBox.PaintBox.Invalidate;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          frmGoPhast.LastSubdivideLayer := Layer;
          frmGoPhast.LastSubdivideRow := Row;
          ZoomBox.PaintBox.Invalidate;
        end;
      end;
  else Assert(False);
  end;

end;

procedure TframeView.UpdateStatusBar(const X,Y: integer);
var
  APoint : T2DRealPoint;
  Column, Row, Layer : integer;
begin
  case ViewDirection of
    vdTop:
      begin
        frmGoPhast.StatusBar1.Panels[0].Text := 'X: ' + FloatToStrF(ZoomBox.X(X),
          ffGeneral, 7,1) + ', Y: ' + FloatToStrF(ZoomBox.Y(Y), ffGeneral, 7,1);
      end;
    vdFront:
      begin
        frmGoPhast.StatusBar1.Panels[0].Text := 'X: ' + FloatToStrF(ZoomBox.X(X), ffGeneral, 7,1)
        + ', Z: ' + FloatToStrF(ZoomBox.Y(Y), ffGeneral, 7,1);
      end;
    vdSide:
      begin
        frmGoPhast.StatusBar1.Panels[0].Text :=
          'Y: ' + FloatToStrF(ZoomBox.Y(Y), ffGeneral, 7,1)
          + ', Z: ' + FloatToStrF(ZoomBox.X(X), ffGeneral, 7,1);
      end;
  else Assert(False);
  end;


  // get the location in real-world coordinates of the current cursor location
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);


  case ViewDirection of
    vdTop:
      begin
        // rotate that position to the grid coordinate system.
        APoint := frmGoPhast.PhastGrid.
          RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
        // Get the column and row boundaries closest to the current cursor.
        Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
        Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
        if (Column >= 0) and (Row >= 0)
          and (Column <= frmGoPhast.PhastGrid.ColumnCount)
          and (Row <= frmGoPhast.PhastGrid.RowCount) then
        begin
          // If the cursor is over the grid, display the column and row number
          frmGoPhast.StatusBar1.Panels[1].Text := 'Col: ' + IntToStr(Column)
            + '; Row: ' + IntToStr(Row);
        end
        else
        begin
          // If the cursor is not over the grid, don't display
          // the column and row number
          frmGoPhast.StatusBar1.Panels[1].Text := '';
        end;
      end;
    vdFront:
      begin
        // Get the column and layer boundaries closest to the current cursor.
        Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
        Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
        if (Column >= 0) and (Layer >= 0)
          and (Column <= frmGoPhast.PhastGrid.ColumnCount)
          and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
        begin
          // If the cursor is over the grid, display the column and layer number
          frmGoPhast.StatusBar1.Panels[1].Text := 'Col: ' + IntToStr(Column)
            + '; Lay: ' + IntToStr(Layer);
        end
        else
        begin
          // If the cursor is not over the grid, don't display
          // the column and layer number
          frmGoPhast.StatusBar1.Panels[1].Text := '';
        end;
      end;
    vdSide:
      begin
        Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
        Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
        if (Row >= 0) and (Layer >= 0)
          and (Row <= frmGoPhast.PhastGrid.RowCount)
          and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
        begin
          frmGoPhast.StatusBar1.Panels[1].Text := 'Row: ' + IntToStr(Row)
            + '; Lay: ' + IntToStr(Layer);
        end
        else
        begin
          frmGoPhast.StatusBar1.Panels[1].Text := '';
        end;
      end;
  else Assert(False);
  end;
end;

procedure TframeView.ZoomBoxPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // record which grid the cursor is over
  frmGoPhast.CursorGrid := CursorGrid;
  // record the current cursor position
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;

  // display the current position
  UpdateStatusBar(X,Y);

  if frmGoPhast.tbDeleteColumnRow.Down
    or frmGoPhast.tbMove.Down
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
    // Set the correct cursor
    case ViewDirection of
      vdTop: SetTopCursor(X, Y);
      vdFront: SetFrontCursor(X, Y);
      vdSide: SetSideCursor(X, Y);
    else Assert(False);
    end;
    DrawSelectRect := False;
  end
  else if frmGoPhast.tbSubdivide.Down
    and frmGoPhast.Subdividing then
  begin
    // Determine the column and row under the cursor
    // and repaint to show the selected rows and columns.
    ContinueSubdivide(X, Y);
    DrawSelectRect := False;
  end
  else if frmGoPhast.tbSpacing.Down
    and frmGoPhast.SettingSpacing then
  begin
    // Determine the column and row under the cursor
    // and repaint to show the selected rows and columns.
    ContinueSetSpacing(X, Y);
    DrawSelectRect := False;
  end
  else if frmGoPhast.tbPolygon.Down
    or frmGoPhast.tbStraightLine.Down
    or frmGoPhast.tbLine.Down
    or frmGoPhast.tbRectangle.Down then
  begin
    // set the correct cursor
    ZoomBox.Cursor := crArrow;
    ZoomBox.PaintBox.Cursor := crArrow;
    DrawSelectRect := False;
    // redraw to indicate where the contour would be if you clicked now.
    if CurrentContour <> nil then
    begin
      ZoomBox.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbGridAngle.Down then
  begin
    if ssLeft in Shift then
    begin
      ZoomBox.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbPoint.Down then
  begin
    // set the correct cursor
    ZoomBox.Cursor := crArrow;
    ZoomBox.PaintBox.Cursor := crArrow;
  end
  else if frmGoPhast.tbLasso.Down and (SelectLine <> nil) then
  begin
    // set the correct cursor
    ZoomBox.Cursor := crArrow;
    ZoomBox.PaintBox.Cursor := crArrow;
    // If the cursor has moved far enough, add another point to the
    // lasso.
    if (SelectLine.Count = 0)
      or (Abs(SelectLine.Points[SelectLine.Count-1].X-X) > 10)
      or (Abs(SelectLine.Points[SelectLine.Count-1].Y-Y) > 10) then
    begin
      SelectLine.AddPoint(Point(X,Y));
      ZoomBox.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbSelect.Down then
  begin
    // set the correct cursor.
    ZoomBox.Cursor := crArrow;
    ZoomBox.PaintBox.Cursor := crArrow;
    // if something is happening, redraw.
    if (DrawSelectRect and (ssLeft in Shift)) or not MovingContours then
    begin
      ZoomBox.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbSelectPoint.Down then
  begin
    // if something is happening, redraw.
    if (SelectLine <> nil) and not MovingContours then
    begin
      ZoomBox.PaintBox.Invalidate;
    end
    else if (SelectedPointContour <> nil)
      and SelectedPointContour.Selected then
    begin
      ZoomBox.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbInsertPoint.Down then
  begin
    // The user wants to insert a point.
    // Check if the mouse is over the
    // edge of a selected object and change the cursor appropriately.
    SetInsertPointCursor(X,Y);
  end
  else if frmGoPhast.tbDeleteSegment.Down then
  begin
    // The user wants to delete on segment of an object.
    // Check if the mouse is over the
    // edge of an object and change the cursor appropriately.
    SetDeleteSegmentCursor(X,Y);
  end
  else
  begin
    DrawSelectRect := False;
  end;

end;

procedure TframeView.DeleteColumnOrRow(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Row : integer;
begin
  // Find the nearest column and row to the cursor position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.PhastGrid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Column >= 0) and (Row >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    // The cursor is over the grid.
    if IsOnTopColumn(X,Y) then
    begin
      // the cursor is over a column so delete it.
      frmGoPhast.UndoStack.Submit(TUndoDeleteColumn.Create(Column));
    end;
    if IsOnTopRow(X,Y) then
    begin
      // the cursor is over a row so delete it.
      frmGoPhast.UndoStack.Submit(TUndoDeleteRow.Create(Row));
    end;
  end;
end;

procedure TframeView.MoveContours(const X,Y: integer);
var
  XOffset, YOffset : double;
  UndoMoveContour : TUndoMoveContour;
begin
  // move the contours a distance specified by where the mouse was clicked
  // down and where it was clicked up.
  XOffset := ZoomBox.X(X)- ZoomBox.X(StartX);
  YOffset := ZoomBox.Y(Y)- ZoomBox.Y(StartY);
  if (XOffset <> 0) or (YOffset <> 0) then
  begin
    // if the cursor has moved, move the selected contours.
    UndoMoveContour := TUndoMoveContour.Create(XOffset, YOffset, ViewDirection);
    frmGoPhast.UndoStack.Submit(UndoMoveContour);
    StartX := X;
    StartY := Y;
  end;
end;

procedure TframeView.MoveColumnOrRow(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // If the user isn't moving a column or row, quit.
  if not (frmGoPhast.MovingColumn or frmGoPhast.MovingRow) then
  begin
    Exit;
  end;
  // transform the mouse coordinates to grid coordinates.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.PhastGrid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);

  if frmGoPhast.MovingColumn then
  begin
    // move a column
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create(
      frmGoPhast.ColumnBeingMoved, APoint.X));
  end
  else if frmGoPhast.MovingRow then
  begin
    // move a row.
    frmGoPhast.UndoStack.Submit(TUndoMoveRow.Create(
      frmGoPhast.RowBeingMoved, APoint.Y));
  end;
  // Stop moving columns and rows.
  frmGoPhast.MovingColumn := False;
  frmGoPhast.MovingRow := False;
end;

procedure TframeView.AddColumnOrRow(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // transform the mouse coordinates to grid coordinates.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.PhastGrid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  if frmGoPhast.tbAddColumn.Down then
  begin
    // Add a column.
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(APoint.X));
  end
  else if frmGoPhast.tbAddRow.Down then
  begin
    // Add a row.
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(APoint.Y));
  end;
end;

procedure TframeView.SubdivideTop(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Row : integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowCol(APoint, Row, Column);
  if (Row >= 0) and (Column >= 0) then
  begin
    // subdivide the column and row
    frmGoPhast.Subdivide(Column, Row, -1);
  end;
end;

procedure TframeView.SetSpacingTop(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Row : integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowCol(APoint, Row, Column);
  if (Row >= 0) and (Column >= 0) then
  begin
    frmGoPhast.LastSpacingColumn := Column;
    frmGoPhast.LastSpacingRow := Row;
    frmGoPhast.LastSpacingLayer := -1;
    // subdivide the column and row
    frmGoPhast.SetGridSpacing;
  end;
end;

procedure TframeView.ContinueRectangle(X, Y: Integer);
var
  APoint, NewPoint, PreviousPoint : T2DRealPoint;
  Angle, RotatedAngle : double;
  XR, YR, R : double;
  XPrime, YPrime : double;
begin
  // if the box hasn't been started yet, begin it.
  if CurrentContour = nil then
  begin
    CurrentContour := T2DContour.Create(ViewDirection, ctBox, CurrentUndo);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // get the real-world coordinates of the mouse.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Check and see whether this is a new box or not.
  if CurrentContour.count = 0 then
  begin
    // If it is a new box, add one point at the mouse position.
    CurrentContour.AddPoint(APoint);
  end
  else
  begin
    // If it is not a new box, add 4 points to finish the box.

    // The edges of box should be parallel to the grid.
    PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
    YR := APoint.Y - PreviousPoint.Y;
    XR := APoint.X - PreviousPoint.X;
    Angle := ArcTan2(YR, XR);
    RotatedAngle := Angle - frmGoPhast.PhastGrid.GridAngle;
    R := SqrT(Sqr(XR) + Sqr(YR));
    XPrime := Cos(RotatedAngle)*R;
    YPrime := Sin(RotatedAngle)*R;

    NewPoint.X := PreviousPoint.X + Cos(frmGoPhast.PhastGrid.GridAngle)*XPrime;
    NewPoint.Y := PreviousPoint.Y + Sin(frmGoPhast.PhastGrid.GridAngle)*XPrime;
    CurrentContour.AddPoint(NewPoint);
    CurrentContour.AddPoint(APoint);

    NewPoint.X := PreviousPoint.X - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrime;
    NewPoint.Y := PreviousPoint.Y + Cos(frmGoPhast.PhastGrid.GridAngle)*YPrime;
    CurrentContour.AddPoint(NewPoint);

    CurrentContour.AddPoint(CurrentContour.Points[0]);

    DrawSelectRect := False;
    // Get the object properties from the user.
    FinishContours;
  end;
end;

procedure TframeView.ContinueStraightLineContour(X, Y: Integer);
var
  APoint, PreviousPoint : T2DRealPoint;
  Angle, RotatedAngle : double;
  XR, YR, R : double;
  XPrimeD, YPrimeD : double;
  XPrimeI, YPrimeI : integer;
begin
  // if the straight line object hasn't been started yet, begin it.
  if CurrentContour = nil then
  begin
    CurrentContour := T2DContour.Create(ViewDirection, ctStraightLine, CurrentUndo);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // get the real-world coordinates of the mouse.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Check and see whether this is a new box or not.
  if CurrentContour.count = 0 then
  begin
    // If it is a new box, add one point at the mouse position.
    CurrentContour.AddPoint(APoint);
  end
  else
  begin
    // If it is not a new box, add another point in such a way that the
    // new edge is parallel to one of the edges of the grid.
    PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
    case ViewDirection of
      vdTop:
        begin
          YR := APoint.Y - PreviousPoint.Y;
          XR := APoint.X - PreviousPoint.X;
          Angle := ArcTan2(YR, XR);
          RotatedAngle := Angle - frmGoPhast.PhastGrid.GridAngle;
          R := Sqrt(Sqr(XR) + Sqr(YR));
          XPrimeD := Cos(RotatedAngle)*R;
          YPrimeD := Sin(RotatedAngle)*R;
          if Abs(XPrimeD) > Abs(YPrimeD) then
          begin
            APoint.X := PreviousPoint.X + Cos(frmGoPhast.PhastGrid.GridAngle)*XPrimeD;
            APoint.Y := PreviousPoint.Y + Sin(frmGoPhast.PhastGrid.GridAngle)*XPrimeD;
          end
          else
          begin
            APoint.X := PreviousPoint.X - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrimeD;
            APoint.Y := PreviousPoint.Y + Cos(frmGoPhast.PhastGrid.GridAngle)*YPrimeD;
          end;
        end;
      vdFront, vdSide:
        begin
          XPrimeI := X - ZoomBox.XCoord(PreviousPoint.X);
          YPrimeI := Y - ZoomBox.YCoord(PreviousPoint.Y);
          if Abs(XPrimeI) > Abs(YPrimeI) then
          begin
            APoint.X := ZoomBox.X(X);
            APoint.Y := PreviousPoint.Y ;
          end
          else
          begin
            APoint.X := PreviousPoint.X;
            APoint.Y := ZoomBox.Y(Y);
          end;
        end;
    else Assert(False);
    end;

    CurrentContour.AddPoint(APoint);
  end;
  DrawSelectRect := False;
end;


procedure TframeView.ContinueLineContour(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // If the contour hasn't been started yet, start it.
  if CurrentContour = nil then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      CurrentContour := T2DContour.Create(ViewDirection, ctLine, CurrentUndo);
    end
    else if frmGoPhast.tbPolygon.Down then
    begin
      CurrentContour := T2DContour.Create(ViewDirection, ctPolygon, CurrentUndo);
    end
    else Assert(False);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // Get the real-world coordinates of the mouse.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Add a point at the current mouse position.
  CurrentContour.AddPoint(APoint);
  DrawSelectRect := False;
end;

procedure TframeView.CreatePointContour(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // Get the real-world coordinates of the mouse.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // create the contour.
  CurrentContour := T2DContour.Create(ViewDirection, ctPoint, CurrentUndo);
  frmGoPhast.Contours.Add(CurrentContour);
  // Add a point at the current mouse position.
  CurrentContour.AddPoint(APoint);
  DrawSelectRect := False;
    // Get the object properties from the user.
  FinishContours;
end;

function TframeView.SelectPointsWithLine (const AContour : T2DContour;
  const AddToSelection : boolean; out Changed : boolean): boolean;
var
  PointIndex : integer;
  APoint : TPoint;
begin
  // This procedure selects nodes that are inside SelectLine.
  // if AddToSelection is not True, it deselects nodes that are not
  // inside SelectLine.  Changed is set to True if any nodes have been
  // changed from unselected to selected or vice versa.
  Changed := False;
  result := False;
  for PointIndex := 0 to AContour.Count -1 do
  begin
    APoint := AContour.CanvasCoordinates[PointIndex];
    if SelectLine.Inside(APoint) then
    begin
      result := True;
      if not AContour.SelectedNodes[PointIndex] then
      begin
        AContour.SelectedNodes[PointIndex] := True;
        Changed := True;
      end;
    end
    else
    begin
      if not AddToSelection and AContour.SelectedNodes[PointIndex] then
      begin
        AContour.SelectedNodes[PointIndex] := False;
        Changed := True;
      end;
    end;
  end;
end;

function TframeView.SelectContoursWithLine(const ToggleSelection : boolean) : boolean;
var
  ContourIndex: integer;
  AContour : T2DContour;
  APoint : TPoint;
  Update : boolean;
  UndoChangeSelection : TUndoChangeSelection;
  function PointInside: boolean;
  begin
    // PointInside  returns True if the first point in
    // AContour is inside SelectLine.
    result := false;
    if AContour.Count > 0 then
    begin
      APoint := AContour.CanvasCoordinates[0];
      if SelectLine.Inside(APoint) then
      begin
        result := True;
      end;
    end;
  end;
begin
  // SelectContoursWithLine returns true if an entire object is selected by
  // being inside SelectLine. If AddToSelection is true, contours inside
  // SelectLine are toggled from Selected to not Selected and vice-versa.
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := false;
    Update := False;
    try
      for ContourIndex := 0 to frmGoPhast.Contours.Count -1 do
      begin
        AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
        if (AContour.ViewDirection = ViewDirection)
          and not AContour.Deleted then
        begin
          // If at least one point of the contour is inside SelectLine
          // and SelectLine does not intersect SelectLine then
          // the entire object is inside SelectLine.
          if PointInside and not SelectLine.Intersect(AContour.SelectLine) then
          begin
            if ToggleSelection then
            begin
              AContour.Selected := not AContour.Selected;
              if AContour.Selected then
              begin
                result := True;
              end;
              Update := True;
            end
            else
            begin
              if not AContour.Selected then
              begin
                AContour.Selected := True;
                Update := True;
              end;
              result := True;
            end;
          end
          else if not ToggleSelection then
          begin
            if AContour.Selected then
            begin
              AContour.Selected := False;
              Update := True;
            end;
          end;
        end;
      end;
    finally
      UpdateSelectRectangle;
      if Update then
      begin
        ContoursChanged := True;
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

procedure TframeView.ZoomBoxPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  APoint : TPoint;
  ARealPoint : T2DRealPoint;
  Center : T2DRealPoint;
  NewAngle : double;
  procedure ShowMagnification;
  begin
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(ZoomBox.Magnification);
  end;
begin
  if ssRight in Shift then
  begin
    // for a right click, don't create objects
    Exit;
  end;
  MouseButtonIsDown := False;
  // Record the current cursor position.
  frmGoPhast.CursorGrid := CursorGrid;
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  if frmGoPhast.tbZoomOut.Down then
  begin
    // The user want to zoom out.
    ZoomBox.ZoomByAt(0.5, X, Y);
    ShowMagnification;
  end
  else if frmGoPhast.tbPan.Down then
  begin
    // The user has finished panning to a different position.
    // Stop panning and change the cursor to indicate that panning has
    // stopped.
    if ZoomBox.CanPan then
    begin
      ZoomBox.Panning := False;
      ZoomBox.Cursor := crHandFlat;
      ZoomBox.PaintBox.Cursor := crHandFlat;
      Screen.Cursor := crDefault;
    end;
  end
  else if frmGoPhast.tbZoom.Down then
  begin
    // The user has zoomed in to a specific region.  If further
    // zooming in is impossible, change the cursor.
    if not ZoomBox.CanZoomIn then
    begin
      ZoomBox.Cursor := crArrow;
      ZoomBox.PaintBox.Cursor := crArrow;
    end;
    ShowMagnification;
  end
  else if frmGoPhast.tbZoomIn.Down then
  begin
    // The user want to zoom in.
    ZoomBox.ZoomByAt(2, X, Y);
    // If further zooming in is impossible, change the cursor.
    if not ZoomBox.CanZoomIn then
    begin
      ZoomBox.Cursor := crArrow;
      ZoomBox.PaintBox.Cursor := crArrow;
    end;
    ShowMagnification;
  end
  else if frmGoPhast.tbDeleteColumnRow.Down then
  begin
    // delete the column, row, or layer under the cursor (if there is one).
    case ViewDirection of
      vdTop: DeleteColumnOrRow(X, Y);
      vdFront: DeleteColumnOrLayer(X,Y);
      vdSide: DeleteRowOrLayer(X,Y);
    else Assert(False);
    end;
  end
  else if frmGoPhast.tbMove.Down then
  begin
    // Move the selected column, row, or layer boundary.
    case ViewDirection of
      vdTop: MoveColumnOrRow(X, Y);
      vdFront: MoveColumnOrLayer(X,Y);
      vdSide: MoveRowOrLayer(X,Y);
    else Assert(False);
    end;
  end
  else if frmGoPhast.tbAddColumn.Down or frmGoPhast.tbAddRow.Down then
  begin
    // Add a column or row at the mouse position.
    case ViewDirection of
      vdTop: AddColumnOrRow(X, Y);
      vdFront: AddColumnOrLayer(X,Y);
      vdSide: AddRowOrLayer(X,Y);
    else Assert(False);
    end;
  end
  else if frmGoPhast.tbSubdivide.Down then
  begin
    // Subdivide the selected cells.
    case ViewDirection of
      vdTop: SubdivideTop(X, Y);
      vdFront: SubdivideFront(X,Y);
      vdSide: SubdivideSide(X,Y);
    else Assert(False);
    end;
  end
  else if frmGoPhast.tbSpacing.Down then
  begin
    // Set the spacing of the selected cells.
    case ViewDirection of
      vdTop: SetSpacingTop(X, Y);
      vdFront: SetSpacingFront(X,Y);
      vdSide: SetSpacingSide(X,Y);
    else Assert(False);
    end;
  end
  else if frmGoPhast.tbGridAngle.Down then
  begin
    if ViewDirection = vdTop then
    begin
      ARealPoint.X := ZoomBox.X(X);
      ARealPoint.Y := ZoomBox.Y(Y);
      Center := GridCenter;
      NewAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
      Rotating := False;
      if NewAngle <> StartAngle then
      begin
        frmGoPhast.UndoStack.Submit(TUndoSetAngle.Create(frmGoPhast.PhastGrid.GridAngle
          + NewAngle - StartAngle))
      end;
    end;
  end
  else if frmGoPhast.tbPoint.Down then
  begin
    // create a point object.
    CreatePointContour(X, Y);
  end
  else if frmGoPhast.tbLine.Down or frmGoPhast.tbPolygon.Down then
  begin
    // Continue (or create) a line or polygon.
    if (PriorCursorX <> X) or (PriorCursorY <> Y) then
    begin
      ContinueLineContour(X, Y);
    end;
  end
  else if frmGoPhast.tbStraightLine.Down then
  begin
    // continue (or create) a straight-line object.
    if (PriorCursorX <> X) or (PriorCursorY <> Y) then
    begin
      ContinueStraightLineContour(X, Y);
    end;
  end
  else if frmGoPhast.tbRectangle.Down then
  begin
    // start or finish a rectangle that is aligned with the grid.
    ContinueRectangle(X, Y);
  end
  else if frmGoPhast.tbSelect.Down then
  begin
    if  ((Abs(X - StartX) > SelectionWidth)
      or (Abs(Y - StartY) > SelectionWidth)) then
    begin
    // Don't do anything if the mouse hasn't moved much.
      // if you are moving contours, move them
      if MovingContours then
      begin
        MoveContours(X,Y);
      end
      else
      begin
        // otherwise select contours with a rectangle.
        // finish SelectLine
        APoint := SelectLine.Points[0];
        SelectLine.AddPoint(Point(APoint.X,Y));
        SelectLine.AddPoint(Point(X,Y));
        SelectLine.AddPoint(Point(X,APoint.Y));
        SelectLine.AddPoint(APoint);
        // Select contours with SelectLine.
        SelectContoursWithLine(ssShift in Shift);
        // redraw
        ZoomBox.PaintBox.Invalidate;
      end;
    end
    else if not MovingContours or (ssShift in Shift) then
    begin
      SelectContours(X, Y, (ssCtrl in Shift), (ssShift in Shift), True)
    end;
    // Get rid of SelectLine
    FreeAndNil(SelectLine);
  end
  else if frmGoPhast.tbLasso.Down and (SelectLine <> nil) then
  begin
    // finish SelectLine
    SelectLine.AddPoint(Point(X,Y));
    SelectLine.AddPoint(SelectLine.Points[0]);
    // Select contours with SelectLine.
    SelectContoursWithLine(ssShift in Shift);
    // Get rid of SelectLine
    FreeAndNil(SelectLine);
    // redraw
    ZoomBox.PaintBox.Invalidate;
    frmGoPhast.tbSelect.Down := true;
    frmGoPhast.tbSelect.OnMouseDown(frmGoPhast.tbSelect, mbLeft, [], 0,0);
    frmGoPhast.tbSelectClick(frmGoPhast.tbSelect);
  end
  else if frmGoPhast.tbSelectPoint.Down and (SelectLine <> nil) then
  begin
    // Don't do anything if the mouse hasn't moved much.
    if ((Abs(X - StartX) > SelectionWidth)
      or (Abs(Y - StartY) > SelectionWidth)) then
    begin
      // if you are moving nodes, move them
      if MovingContours then
      begin
        MoveContours(X,Y);
      end
      else
      begin
        // otherwise select contours with a rectangle.
        // finish SelectLine
        APoint := SelectLine.Points[0];
        SelectLine.AddPoint(Point(APoint.X,Y));
        SelectLine.AddPoint(Point(X,Y));
        SelectLine.AddPoint(Point(X,APoint.Y));
        SelectLine.AddPoint(APoint);
        // Select contours with SelectLine.
        SelectPointsOfAllSelectedContoursWithLine(ssShift in Shift);
      end;
    end;
    // Get rid of SelectLine
    FreeAndNil(SelectLine);
    // redraw
    ZoomBox.PaintBox.Invalidate;
  end
  else if frmGoPhast.tbInsertPoint.Down then
  begin
    // The user wants to insert a point
    SelectContours(X, Y, (ssCtrl in Shift), (ssShift in Shift));
    InsertPoint(X, Y);
  end
  else if frmGoPhast.tbDeleteSegment.Down then
  begin
    // The user wants to delete one edge of an object
    SelectContours(X, Y, (ssCtrl in Shift), (ssShift in Shift));
    DeleteSegment(X, Y);
  end;
  if DoubleClicked then
  begin
    if (frmGoPhast.tbLine.Down or frmGoPhast.tbPolygon.Down
      or frmGoPhast.tbStraightLine.Down) then
    begin
      // get the object properties from the user
      FinishContours;
    end
    else if frmGoPhast.tbSelect.Down or frmGoPhast.tbLasso.Down then
    begin
      // Edit the object properties.
      frmGoPhast.EditContours;
    end;
    DoubleClicked := False;
  end;
  PriorCursorX := X;
  PriorCursorY := Y;

end;

function TframeView.GetContoursChanged: boolean;
begin
  case ViewDirection of
    vdTop: result := frmGoPhast.TopContoursChanged;
    vdFront: result := frmGoPhast.FrontContoursChanged;
    vdSide: result := frmGoPhast.SideContoursChanged;
  else
    begin
      Assert(False);
      result := False;
    end;
  end;
end;

procedure TframeView.SetContoursChanged(const Value: boolean);
begin
  case ViewDirection of
    vdTop: frmGoPhast.TopContoursChanged := Value;
    vdFront: frmGoPhast.FrontContoursChanged := Value;
    vdSide: frmGoPhast.SideContoursChanged := Value;
  else
    begin
      Assert(False);
    end;
  end;
end;

function TframeView.GetNeedToRecalculateCellColors: boolean;
begin
  case ViewDirection of
    vdTop: result := frmGoPhast.PhastGrid.NeedToRecalculateTopCellColors;
    vdFront: result := frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors;
    vdSide: result := frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors;
  else
    begin
      Assert(False);
      result := False;
    end;
  end;
end;

procedure TframeView.SetNeedToRecalculateCellColors(const Value: boolean);
begin
  case ViewDirection of
    vdTop: frmGoPhast.PhastGrid.NeedToRecalculateTopCellColors := Value;
    vdFront: frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors := Value;
    vdSide: frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors := Value;
  else
    begin
      Assert(False);
    end;
  end;
end;

function TframeView.RecalculateTopCellColors: boolean;
var
  Index : integer;
  AContour: T2DContour;
begin
  result := False;
  frmGoPhast.PhastGrid.ResetTopCellColors;
  Application.ProcessMessages;
  if ContoursChanged then
  begin
    ContoursChanged := True;
    NeedToRecalculateCellColors := True;
    Exit;
  end;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.ViewDirection = ViewDirection then
    begin
      AContour.SetTopCellColor(frmGoPhast.PhastGrid);
    end;
    Application.ProcessMessages;
    if ContoursChanged then
    begin
      ContoursChanged := True;
      NeedToRecalculateCellColors := True;
      Exit;
    end;
  end;
  result := True;
end;

function TframeView.RecalculateFrontCellColors: boolean;
var
  Index : integer;
  AContour: T2DContour;
begin
  result := False;
  frmGoPhast.PhastGrid.ResetFrontCellColors;
  Application.ProcessMessages;
  if frmGoPhast.FrontContoursChanged then
  begin
    frmGoPhast.FrontContoursChanged := True;
    frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors := True;
    Exit;
  end;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.ViewDirection = vdFront then
    begin
      AContour.SetFrontCellColor(frmGoPhast.PhastGrid);
    end;
    Application.ProcessMessages;
    if frmGoPhast.FrontContoursChanged then
    begin
      frmGoPhast.FrontContoursChanged := True;
      frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors := True;
      Exit;
    end;
  end;
  result := True;
end;

function TframeView.RecalculateSideCellColors: boolean;
var
  Index : integer;
  AContour: T2DContour;
begin
  result := False;
  frmGoPhast.PhastGrid.ResetSideCellColors;
  Application.ProcessMessages;
  if frmGoPhast.SideContoursChanged then
  begin
    frmGoPhast.SideContoursChanged := True;
    frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors := True;
    Exit;
  end;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.ViewDirection = vdSide then
    begin
      AContour.SetSideCellColor(frmGoPhast.PhastGrid);
    end;
    Application.ProcessMessages;
    if frmGoPhast.SideContoursChanged then
    begin
      frmGoPhast.SideContoursChanged := True;
      frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors := True;
      Exit;
    end;
  end;
  result := True;
end;

function TframeView.RecalculateCellColors: boolean;
begin
  case ViewDirection of
    vdTop: result := RecalculateTopCellColors;
    vdFront: result := RecalculateFrontCellColors;
    vdSide: result := RecalculateSideCellColors;
  else Assert(False);
  end;
end;

function TframeView.GetGridChanged: boolean;
begin
  case ViewDirection of
    vdTop: result := frmGoPhast.TopGridChanged;
    vdFront: result := frmGoPhast.FrontGridChanged;
    vdSide: result := frmGoPhast.SideGridChanged;
  else
    begin
      Assert(False);
      result := False;
    end;
  end;
end;

procedure TframeView.SetGridChanged(const Value: boolean);
begin
  case ViewDirection of
    vdTop: frmGoPhast.TopGridChanged := Value;
    vdFront: frmGoPhast.FrontGridChanged := Value;
    vdSide: frmGoPhast.SideGridChanged := Value;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TframeView.DrawGridAndContours;
begin
  if  (frmContourProperties <> nil) then Exit;
  try

    BitMap.Free;
    BitMap := nil;
    BitMap := TBitMap.Create;
    BitMap.Height := ZoomBox.PaintBox.Height;
    BitMap.Width := ZoomBox.PaintBox.Width;

    // If the colors of the grid cells are out of date,
    // recalculate them.
    if NeedToRecalculateCellColors then
    begin
      if not RecalculateCellColors then Exit;
    end;

    // draw a box around the drawing area
    BitMap.Canvas.Brush.Color := clWhite;
    BitMap.Canvas.Pen.Color := clBlack;
    BitMap.Canvas.Rectangle(0,0,BitMap.Width, BitMap.Height);

    // draw the grid.
    frmGoPhast.PhastGrid.Draw(BitMap.Canvas, ViewDirection);

    // If the objects have been changed while drawing the grid
    // stop and start over.
    Application.ProcessMessages;
    if ContoursChanged then
    begin
      ContoursChanged := True;
      Exit;
    end;

    // If there are no objects, finish up and quit.
    if frmGoPhast.Contours.Count = 0 then
    begin
      GridChanged := False;
      ZoomBox.Image.Picture.Assign(BitMap);
      Exit;
    end;

    // Draw the contours.
    InvalidateContours;
    DrawContours;
  except on EInvalidGraphicOperation do
    begin
      // If the size is too big, make it smaller and start over.
      WarnTooBig;
    end;
  end;
end;

procedure TframeView.DrawSelectionRectangle;
var
  TL, BR : TPoint;
begin
  if DrawSelectRect then
  begin
    // draw a rectangle around the selected contours.
    TL := SelectTopLeft;
    BR := SelectBottomRight;
    if MovingContours then
    begin
      TL.X := TL.X + frmGoPhast.CursorX - StartX;
      BR.X := BR.X + frmGoPhast.CursorX - StartX;
      TL.Y := TL.Y + frmGoPhast.CursorY - StartY;
      BR.Y := BR.Y + frmGoPhast.CursorY - StartY;
    end;
    ZoomBox.PaintBox.Canvas.Brush.Style := bsClear;
    ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
    ZoomBox.PaintBox.Canvas.Pen.Width := 1;
    ZoomBox.PaintBox.Canvas.Pen.Color := clBlack;
    ZoomBox.PaintBox.Canvas.Rectangle(Rect(TL, BR));
  end;
end;

procedure TframeView.DrawMouseSelectionRectangle;
var
  TL, BR : TPoint;
  Temp : integer;
begin
  if MouseButtonIsDown and not MovingContours
    and (frmGoPhast.tbSelectPoint.Down or frmGoPhast.tbSelect.Down)
    and (SelectLine <> nil)
    and ((Abs(frmGoPhast.CursorX - StartX) > SelectionWidth)
    or (Abs(frmGoPhast.CursorY - StartY) > SelectionWidth)) then
  begin
    // draw a rectangle used to select points.
    TL := SelectLine.Points[0];
    BR.X := frmGoPhast.CursorX;
    BR.Y := frmGoPhast.CursorY;
    if TL.X > BR.X  then
    begin
      Temp := TL.X;
      TL.X := BR.X;
      BR.X := Temp;
    end;
    if TL.Y > BR.Y  then
    begin
      Temp := TL.Y;
      TL.Y := BR.Y;
      BR.Y := Temp;
    end;
    ZoomBox.PaintBox.Canvas.Brush.Style := bsClear;
    ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
    ZoomBox.PaintBox.Canvas.Pen.Width := 1;
    ZoomBox.PaintBox.Canvas.Pen.Color := clBlack;
    ZoomBox.PaintBox.Canvas.Rectangle(Rect(TL, BR));
  end;
end;

procedure TframeView.ShowMovedPoints;
var
  PointIndex : integer;
  PointArray : array of TPoint;
begin
  if MouseButtonIsDown and MovingContours
    and frmGoPhast.tbSelectPoint.Down
    and (SelectedPointContour <> nil)
    and SelectedPointContour.Selected then
  begin
    // show the new locations of the selected points.
    ZoomBox.PaintBox.Canvas.Brush.Style := bsClear;
    ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
    ZoomBox.PaintBox.Canvas.Pen.Width := 1;
    ZoomBox.PaintBox.Canvas.Pen.Color := clBlack;
    for PointIndex := 0 to SelectedPointContour.Count -1 do
    begin
      if SelectedPointContour.SelectedNodes[PointIndex] then
      begin
        if SelectedPointContour.Count = 1 then
        begin
          ZoomBox.PaintBox.Canvas.Rectangle(frmGoPhast.CursorX-3,
            frmGoPhast.CursorY-3,frmGoPhast.CursorX+4,
            frmGoPhast.CursorY+4);
        end
        else
        begin
          if PointIndex = 0 then
          begin
            SetLength(PointArray, 2);
            PointArray[0] := SelectedPointContour.CanvasCoordinates[0];
            PointArray[0].X := PointArray[0].X
               + frmGoPhast.CursorX - StartX;
            PointArray[0].Y := PointArray[0].Y
               + frmGoPhast.CursorY - StartY;

            PointArray[1] := SelectedPointContour.CanvasCoordinates[1];
            if SelectedPointContour.SelectedNodes[1] then
            begin
              PointArray[1].X := PointArray[1].X
                 + frmGoPhast.CursorX - StartX;
              PointArray[1].Y := PointArray[1].Y
                 + frmGoPhast.CursorY - StartY;
            end;
          end
          else if PointIndex = SelectedPointContour.Count -1 then
          begin
            SetLength(PointArray, 2);
            PointArray[0] := SelectedPointContour.
              CanvasCoordinates[PointIndex-1];
            if SelectedPointContour.SelectedNodes[PointIndex-1] then
            begin
              PointArray[0].X := PointArray[0].X
                 + frmGoPhast.CursorX - StartX;
              PointArray[0].Y := PointArray[0].Y
                 + frmGoPhast.CursorY - StartY;
            end;

            PointArray[1] := SelectedPointContour.
              CanvasCoordinates[PointIndex];
            PointArray[1].X := PointArray[1].X
               + frmGoPhast.CursorX - StartX;
            PointArray[1].Y := PointArray[1].Y
               + frmGoPhast.CursorY - StartY;
          end
          else
          begin
            SetLength(PointArray, 3);
            PointArray[0] := SelectedPointContour.
              CanvasCoordinates[PointIndex-1];
            if SelectedPointContour.SelectedNodes[PointIndex-1] then
            begin
              PointArray[0].X := PointArray[0].X
                 + frmGoPhast.CursorX - StartX;
              PointArray[0].Y := PointArray[0].Y
                 + frmGoPhast.CursorY - StartY;
            end;

            PointArray[1] := SelectedPointContour.
              CanvasCoordinates[PointIndex];
            PointArray[1].X := PointArray[1].X
               + frmGoPhast.CursorX - StartX;
            PointArray[1].Y := PointArray[1].Y
               + frmGoPhast.CursorY - StartY;

            PointArray[2] := SelectedPointContour.
              CanvasCoordinates[PointIndex+1];
            if SelectedPointContour.SelectedNodes[PointIndex+1] then
            begin
              PointArray[2].X := PointArray[2].X
                 + frmGoPhast.CursorX - StartX;
              PointArray[2].Y := PointArray[2].Y
                 + frmGoPhast.CursorY - StartY;
            end;

          end;

          ZoomBox.PaintBox.Canvas.PolyLine(PointArray);
        end;

      end;

    end;

  end;
end;

procedure TframeView.ShowNewColumnOrRow;
var
  APoint : T2DRealPoint;
  CursorPoint : T2DRealPoint;
begin
  // If you are editting the grid, show that.
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  CursorPoint := frmGoPhast.PhastGrid.RotateFromRealWorldCoordinatesToGridCoordinates(CursorPoint);
  ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
  try
    if (frmGoPhast.PhastGrid.ColumnCount > 0)
      and (frmGoPhast.PhastGrid.RowCount > 0)
      and (frmGoPhast.PhastGrid.LayerCount > 0) then
    begin
      if (frmGoPhast.tbMove.Down and frmGoPhast.MovingColumn)
        or frmGoPhast.tbAddColumn.Down then
      begin
        // moving or adding a column
        APoint.X := CursorPoint.X;
        APoint.Y := frmGoPhast.PhastGrid.RowPosition[0];
        APoint := frmGoPhast.PhastGrid.RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));

        APoint.X := CursorPoint.X;
        APoint.Y := frmGoPhast.PhastGrid.RowPosition[frmGoPhast.PhastGrid.RowCount];
        APoint := frmGoPhast.PhastGrid.RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));
      end
      else if (frmGoPhast.tbMove.Down and frmGoPhast.MovingRow)
        or frmGoPhast.tbAddRow.Down then
      begin
        // moving or adding a row
        APoint.X := frmGoPhast.PhastGrid.ColumnPosition[0];
        APoint.Y := CursorPoint.Y;
        APoint := frmGoPhast.PhastGrid.RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));

        APoint.X := frmGoPhast.PhastGrid.ColumnPosition[frmGoPhast.PhastGrid.ColumnCount];
        APoint.Y := CursorPoint.Y;
        APoint := frmGoPhast.PhastGrid.RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));
      end;
    end;
  finally
    ZoomBox.PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;

procedure TframeView.DrawBlueSelectedTopCells(FirstCol, LastCol, FirstRow, LastRow: integer);
var
  APoint : T2DRealPoint;
  R1, R2, C1, C2 : integer;
  Polygon : array[0..3] of TPoint;
  OldBrushStyle : TBrushStyle;
  OldColor : TColor;
begin
  if FirstCol < LastCol then
  begin
    C1 := FirstCol;
    C2 := LastCol+1;
  end
  else
  begin
    C1 := LastCol;
    C2 := FirstCol+1;
  end;
  if FirstRow < LastRow then
  begin
    R1 := FirstRow;
    R2 := LastRow+1;
  end
  else
  begin
    R1 := LastRow;
    R2 := FirstRow+1;
  end;

  APoint := frmGoPhast.PhastGrid.TwoDCellCorner(C1,R1);
  Polygon[0].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[0].Y := self.ZoomBox.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.TwoDCellCorner(C1,R2);
  Polygon[1].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[1].Y := self.ZoomBox.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.TwoDCellCorner(C2,R2);
  Polygon[2].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[2].Y := self.ZoomBox.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.TwoDCellCorner(C2,R1);
  Polygon[3].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[3].Y := self.ZoomBox.YCoord(APoint.Y);

  OldBrushStyle := ZoomBox.PaintBox.Canvas.Brush.Style;
  OldColor := ZoomBox.PaintBox.Canvas.Brush.Color;
  try
    ZoomBox.PaintBox.Canvas.Brush.Style := bsSolid;
    ZoomBox.PaintBox.Canvas.Brush.Color := clBlue;

    ZoomBox.PaintBox.Canvas.Polygon(Polygon);

  finally
    ZoomBox.PaintBox.Canvas.Brush.Style := OldBrushStyle;
    ZoomBox.PaintBox.Canvas.Brush.Color := OldColor;
  end;
end;

procedure TframeView.DrawRotatedGrid;
var
  OldBrushStyle : TBrushStyle;
  OldPenStyle : TPenStyle;
  ARealPoint : T2DRealPoint;
  Center : T2DRealPoint;
  GridOutline : array[0..4] of TPoint;
  NewAngle : double;
begin
  ARealPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  ARealPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  Center := self.GridCenter;
  NewAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
  RotatedGridCorners(NewAngle-StartAngle, GridOutline);
  OldPenStyle := ZoomBox.PaintBox.Canvas.Pen.Style;
  OldBrushStyle := ZoomBox.PaintBox.Canvas.Brush.Style;
  try
    ZoomBox.PaintBox.Canvas.Brush.Style := bsClear;
    ZoomBox.PaintBox.Canvas.Pen.Style := psDash;

    ZoomBox.PaintBox.Canvas.Polygon(GridOutline);

  finally
    ZoomBox.PaintBox.Canvas.Brush.Style := OldBrushStyle;
    ZoomBox.PaintBox.Canvas.Pen.Style := OldPenStyle;
  end;
end;

procedure TframeView.DrawModifiedCurrentContour;
var
  APoint : T2DRealPoint;
  PreviousPoint, NewPoint : T2DRealPoint;
  XR, YR : double;
  Angle, RotatedAngle, R : double;
  XPrimeD, YPrimeD : double;
  XPrimeI, YPrimeI : double;
begin
  // draw what the current object would look like if the mouse
  // button were depressed now.
  ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
  try
    if frmGoPhast.tbPolygon.Down then
    begin
      if CurrentContour.Count > 0 then
      begin
        ZoomBox.PaintBox.Canvas.MoveTo(CurrentContour.CanvasCoordinates[0].X,
          CurrentContour.CanvasCoordinates[0].Y);
        ZoomBox.PaintBox.Canvas.LineTo(frmGoPhast.CursorX,
          frmGoPhast.CursorY);
        ZoomBox.PaintBox.Canvas.LineTo(CurrentContour.CanvasCoordinates[CurrentContour.Count-1].X,
          CurrentContour.CanvasCoordinates[CurrentContour.Count-1].Y);
      end;
    end
    else if frmGoPhast.tbLine.Down then
    begin
      if CurrentContour.Count > 0 then
      begin
        ZoomBox.PaintBox.Canvas.MoveTo(CurrentContour.CanvasCoordinates[CurrentContour.Count-1].X,
          CurrentContour.CanvasCoordinates[CurrentContour.Count-1].Y);
        ZoomBox.PaintBox.Canvas.LineTo(frmGoPhast.CursorX,
          frmGoPhast.CursorY);
      end;
    end
    else if frmGoPhast.tbStraightLine.Down then
    begin
      APoint.X := ZoomBox.X(frmGoPhast.CursorX);
      APoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
      PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
      case ViewDirection of
        vdTop:
          begin
            YR := APoint.Y - PreviousPoint.Y;
            XR := APoint.X - PreviousPoint.X;
            Angle := ArcTan2(YR, XR);
            RotatedAngle := Angle - frmGoPhast.PhastGrid.GridAngle;
            R := Sqrt(Sqr(XR) + Sqr(YR));
            XPrimeD := Cos(RotatedAngle)*R;
            YPrimeD := Sin(RotatedAngle)*R;
            if Abs(XPrimeD) > Abs(YPrimeD) then
            begin
              APoint.X := PreviousPoint.X + Cos(frmGoPhast.PhastGrid.GridAngle)*XPrimeD;
              APoint.Y := PreviousPoint.Y + Sin(frmGoPhast.PhastGrid.GridAngle)*XPrimeD;
            end
            else
            begin
              APoint.X := PreviousPoint.X - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrimeD;
              APoint.Y := PreviousPoint.Y + Cos(frmGoPhast.PhastGrid.GridAngle)*YPrimeD;
            end;
          end;
        vdFront, vdSide:
          begin
            XPrimeI := frmGoPhast.CursorX - ZoomBox.XCoord(PreviousPoint.X);
            YPrimeI := frmGoPhast.CursorY - ZoomBox.YCoord(PreviousPoint.Y);
            if Abs(XPrimeI) > Abs(YPrimeI) then
            begin
              APoint.X := ZoomBox.X(frmGoPhast.CursorX);
              APoint.Y := PreviousPoint.Y ;
            end
            else
            begin
              APoint.X := PreviousPoint.X;
              APoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
            end;
          end;
      else Assert(False);
      end;
      ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(PreviousPoint.X), ZoomBox.YCoord(PreviousPoint.Y));
      ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));
    end
    else if frmGoPhast.tbRectangle.Down then
    begin
      PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
      case ViewDirection of
        vdTop:
          begin
            APoint.X := ZoomBox.X(frmGoPhast.CursorX);
            APoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
            YR := APoint.Y - PreviousPoint.Y;
            XR := APoint.X - PreviousPoint.X;
            Angle := ArcTan2(YR, XR);
            RotatedAngle := Angle - frmGoPhast.PhastGrid.GridAngle;
            R := SqrT(Sqr(XR) + Sqr(YR));
            XPrimeD := Cos(RotatedAngle)*R;
            YPrimeD := Sin(RotatedAngle)*R;

            NewPoint.X := PreviousPoint.X
              + Cos(frmGoPhast.PhastGrid.GridAngle)*XPrimeD;
            NewPoint.Y := PreviousPoint.Y
              + Sin(frmGoPhast.PhastGrid.GridAngle)*XPrimeD;
            ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(PreviousPoint.X),
              ZoomBox.YCoord(PreviousPoint.Y));
            ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(NewPoint.X),
              ZoomBox.YCoord(NewPoint.Y));
            ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X),
              ZoomBox.YCoord(APoint.Y));

            NewPoint.X := PreviousPoint.X
              - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrimeD;
            NewPoint.Y := PreviousPoint.Y
              + Cos(frmGoPhast.PhastGrid.GridAngle)*YPrimeD;
            ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(NewPoint.X),
              ZoomBox.YCoord(NewPoint.Y));
            ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(PreviousPoint.X),
              ZoomBox.YCoord(PreviousPoint.Y));
          end;
        vdFront, vdSide:
          begin
            ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(PreviousPoint.X), ZoomBox.YCoord(PreviousPoint.Y));
            ZoomBox.PaintBox.Canvas.LineTo(frmGoPhast.CursorX, ZoomBox.YCoord(PreviousPoint.Y));
            ZoomBox.PaintBox.Canvas.LineTo(frmGoPhast.CursorX, frmGoPhast.CursorY);
            ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(PreviousPoint.X), frmGoPhast.CursorY);
            ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(PreviousPoint.X), ZoomBox.YCoord(PreviousPoint.Y));
          end;
      else Assert(False);
      end;

    end;
  finally
    ZoomBox.PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;

procedure TframeView.ZoomBoxPaintBoxPaint(Sender: TObject);
var
  ShouldUpdate : boolean;
  PreviousCursor : TCursor;
begin
  if Drawing then Exit;
  Drawing := True;
  ShouldUpdate := ContoursChanged or NeedToRecalculateCellColors;
  ContoursChanged := False;
  try
    if (BitMap <> nil) then
    begin
      // if you need to redraw the bitmap, change the cursor to an hourglass,
      // redraw, and change the cursor back.
      if GridChanged
        or (BitMap.Height <> ZoomBox.PaintBox.Height)
        or (BitMap.Width <> ZoomBox.PaintBox.Width)
        or ShouldUpdate then
      begin
        PreviousCursor := ZoomBox.Cursor;
        try
          ZoomBox.Cursor := crHourGlass;
          DrawGridAndContours;
        finally
          ZoomBox.Cursor := PreviousCursor;
        end;
      end;

      // Nothing below here should take long to draw.
      if (frmGoPhast.CursorGrid = CursorGrid) then
      begin
        DrawSelectionRectangle;

        DrawMouseSelectionRectangle;

        ShowMovedPoints;

        if ((frmGoPhast.tbMove.Down
          and (frmGoPhast.MovingColumn or frmGoPhast.MovingRow))
          or frmGoPhast.tbAddColumn.Down or frmGoPhast.tbAddRow.Down) then
        begin
          case ViewDirection of
            vdTop: ShowNewColumnOrRow;
            vdFront: ShowNewColumnOrLayer;
            vdSide: ShowNewRowOrLayer;
          else Assert(False);
          end;
        end
        else if frmGoPhast.tbSubdivide.Down
          and frmGoPhast.Subdividing then
        begin
          DrawSubdivide;
        end
        else if frmGoPhast.tbSpacing.Down and frmGoPhast.SettingSpacing then
        begin
          DrawSpacing;
        end
        else if frmGoPhast.tbGridAngle.Down and Rotating then
        begin
          DrawRotatedGrid;
        end
        else if CurrentContour <> nil then
        begin
          // draw what the current object would look like if the mouse
          // button were depressed now.
          DrawModifiedCurrentContour;
        end
        else if SelectLine <> nil then
        begin
          SelectLine.Draw(ZoomBox.PaintBox.Canvas);
        end;
      end;

    end;
  finally
    Drawing := False;
  end;
end;

constructor TframeView.Create(AOwner: TComponent);
begin
  inherited;
  Panel1.ParentColor := True;
  PriorCursorX := 0;
  PriorCursorY := 0;
  BitMap := TBitMap.Create;
  PreviousContours := TList.Create;
end;

destructor TframeView.Destroy;
begin
  BitMap.Free;
  PreviousContours.Free;
  SelectLine.Free;
  inherited;
end;

procedure TframeView.rulHorizontalMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // This is called whenever the mouse is not over the top view.
  frmGoPhast.CursorGrid := cgNone;
end;

procedure TframeView.GetRowCol(APoint: T2DRealPoint; out Row, Column: integer);
var
  NeighborColumn, NeighborRow : integer;
begin
  // Get the row and column numbers of the cell containing APoint;
  APoint := frmGoPhast.PhastGrid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Column >= 0) and (Row >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    if APoint.X > frmGoPhast.PhastGrid.ColumnPosition[Column] then
    begin
      NeighborColumn := Column+1;
    end
    else
    begin
      NeighborColumn := Column-1;
    end;
    if APoint.Y > frmGoPhast.PhastGrid.RowPosition[Row] then
    begin
      NeighborRow := Row+1;
    end
    else
    begin
      NeighborRow := Row-1;
    end;
    if (NeighborColumn >= 0) and (NeighborRow >= 0)
      and (NeighborColumn <= frmGoPhast.PhastGrid.ColumnCount)
      and (NeighborRow <= frmGoPhast.PhastGrid.RowCount) then
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

procedure TframeView.WarnTooBig;
begin
  // zoom out a bit.
  if (ZoomBox.PaintBox.Width > ZoomBox.HorzScrollBar.Width) or
    (ZoomBox.PaintBox.Height > ZoomBox.VertScrollBar.Height) then
  begin
    ZoomBox.ZoomBy(0.8);
    ZoomBox.PaintBox.Invalidate;
    Beep;
  end;
  MessageDlg('Sorry; the view was zoomed in too far.  '
      + 'I''ve had to zoom out a bit', mtInformation, [mbOK], 0);
end;

procedure TframeView.InvalidateContours;
var
  ContourIndex : integer;
  AContour : T2DContour;
begin
  for ContourIndex := 0 to frmGoPhast.Contours.Count-1 do
  begin
    AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
    if AContour.ViewDirection = ViewDirection then
    begin
      AContour.Invalidate;
    end;
  end;
end;

procedure TframeView.DrawContours;
var
  ContourIndex : integer;
  AContour : T2DContour;
begin
//  if frmGoPhast.Contours.Count = 0 then Exit;
  if frmContourProperties <> nil then Exit;
  try
    BitMap.Canvas.Brush.Color := clBlack;
    for ContourIndex := 0 to frmGoPhast.Contours.Count-1 do
    begin
      Application.ProcessMessages;
      if ContoursChanged then
      begin
        ContoursChanged := True;
        Exit;
      end;
      AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
      if (AContour.ViewDirection = ViewDirection)
        and (AContour <> CurrentContour) then
      begin
        AContour.Draw(BitMap.Canvas);
      end;
    end;
    ZoomBox.Image.Picture.Assign(BitMap);
    GridChanged := False;

  except on EInvalidGraphicOperation do
    begin
      WarnTooBig;
    end;
  end;
  PreviousContours.Clear;
end;

procedure TframeView.ZoomBoxPaintBoxDblClick(Sender: TObject);
begin
  DoubleClicked := True;
end;

procedure TframeView.FinishContours;
var
  UndoCreate2DContour : TUndoCreate2DContour;
begin
  if CurrentContour <> nil then
  begin
    if (CurrentContour.ContourType = ctPolygon) then
    begin
      CurrentContour.AddPoint(CurrentContour.Points[0]);
    end;
    UndoCreate2DContour := CurrentUndo as TUndoCreate2DContour;
    try
      Application.CreateForm(TfrmContourProperties, frmContourProperties);
      try
        frmContourProperties.GetData(CurrentContour);
        if frmContourProperties.ShowModal <> mrOK then
        begin
          frmGoPhast.Contours.Remove(CurrentContour);
        end
        else
        begin
          UndoCreate2DContour.SetPostSelection;
          frmGoPhast.UndoStack.
            Submit(UndoCreate2DContour);

          // PreviousContours allows the contour to still be visible
          // in ZoomBox.BottomPaintbox while the grid is being
          // redrawn.
          PreviousContours.Add(CurrentContour);
        end;
        CurrentContour := nil;
      finally
        FreeAndNil(frmContourProperties);
        SetFocus;
      end;
    except
      FreeAndNil(CurrentContour);
      raise;
    end;
    DrawSelectRect := False;
    ContoursChanged := True;
  end;
end;

procedure TframeView.ZoomBoxExit(Sender: TObject);
begin
  // If the user clicks on a button or somewhere else, the contour
  // is terminated.
  FinishContours;
end;

function TframeView.SelectPoints(const X, Y: integer;
  Const AContour: T2DContour; const AddToSelection: boolean;
  var Changed : boolean) : boolean;
var
  PointIndex : integer;
  APoint : TPoint;
begin
  // This procedure is used to select individual nodes in an object.
  result := False;
  for PointIndex := 0 to AContour.Count -1 do
  begin
    APoint := AContour.CanvasCoordinates[PointIndex];
    if (Abs(X-APoint.X) < SelectionWidth)
      and(Abs(Y-APoint.Y) < SelectionWidth) then
    begin
      result := True;
      if AddToSelection then
      begin
        AContour.SelectedNodes[PointIndex] :=
          not AContour.SelectedNodes[PointIndex];
        Changed := True;
      end
      else
      begin
        if not AContour.SelectedNodes[PointIndex] then
        begin
          AContour.SelectedNodes[PointIndex] := True;
          Changed := True;
        end;
      end;
    end;
  end;
end;

function TframeView.SelectPointsOfAllSelectedContoursWithLine
  (const AddToSelection: boolean) : boolean;
var
  Index : integer;
  AContour : T2DContour;
  Update : boolean;
  Changed : boolean;
  UndoChangeSelection : TUndoChangeSelection;
begin
  // This procedure uses a "lasso" to select nodes in one selected object.
  // All other selected objects are deselected.
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    try
      for Index := frmGoPhast.Contours.Count -1 downto 0 do
      begin
        AContour := frmGoPhast.Contours[Index] as T2DContour;
        if result then
        begin
          if AContour.Selected then
          begin
            AContour.Selected := False;
            Update := True;
          end;
          Continue;
        end;
        if (AContour.ViewDirection = ViewDirection) and AContour.Selected then
        begin
          Changed := False;
          if SelectPointsWithLine(AContour, AddToSelection, Changed) then
          begin
            result := True;
            SelectedPointContour := AContour;
          end
          else
          begin
            AContour.Selected := false;
            Update := True;
          end;
          if Changed then
          begin
            Update := True;
          end;
        end;
      end;
    finally
      if Update then
      begin
        ContoursChanged := True;
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

function TframeView.SelectPointsOfASelectedContour(const X, Y: integer;
  const AddToSelection: boolean) : boolean;
var
  Index : integer;
  AContour, SelectedContour : T2DContour;
  Update : boolean;
  Changed : boolean;
  UndoChangeSelection : TUndoChangeSelection;
begin
  // This procedure selects the node of a selected contour
  // that is at or near (X,Y).
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    SelectedContour := nil;
    try
      for Index := frmGoPhast.Contours.Count -1 downto 0 do
      begin
        AContour := frmGoPhast.Contours[Index] as T2DContour;
        if result then
        begin
          if AContour.Selected then
          begin
            AContour.Selected := False;
            Update := True;
          end;
          Continue;
        end;

        if (AContour.ViewDirection = ViewDirection) and AContour.Selected then
        begin
          Changed := False;
          if SelectPoints(X, Y, AContour, AddToSelection, Changed) then
          begin
            result := True;
            SelectedContour := AContour;
            SelectedPointContour := AContour;
          end;
          if Changed then
          begin
            Update := True;
          end;
        end;
      end;
    finally
      if result then
      begin
        for Index := frmGoPhast.Contours.Count -1 downto 0 do
        begin
          AContour := frmGoPhast.Contours[Index] as T2DContour;
          if result and (AContour <> SelectedContour) then
          begin
            AContour.Selected := False;
            Update := True;
          end;
        end;
      end;
      if Update then
      begin
        ContoursChanged := True;
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


function TframeView.SelectContours(const X, Y: integer;
  const SelectLowerContours, ToggleSelectedItem: boolean;
  const CanChangeSelection: boolean = True) : boolean;
var
  Index : integer;
  AContour : T2DContour;
  Update : boolean;
  Start : integer;
  UndoChangeSelection : TUndoChangeSelection;
begin
  // This procedure selects contours that are at or near (X,Y).
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    if SelectLowerContours then
    begin
      Start := frmGoPhast.Contours.Count -1;
      for Index := frmGoPhast.Contours.Count -1 downto 0 do
      begin
        AContour := frmGoPhast.Contours[Index] as T2DContour;
        if (AContour.ViewDirection = ViewDirection)then
        begin
          if AContour.Select(X, Y) then
          begin
            if AContour.Selected then
            begin
              Start := Index -1;
              if CanChangeSelection and not ToggleSelectedItem then
              begin
                AContour.Selected := False;
              end;
              if Start = -1 then
              begin
                Start := frmGoPhast.Contours.Count -1
              end;

            end
            else
            begin
//              break;
            end;
          end;
        end;
      end;
    end
    else
    begin
      Start := frmGoPhast.Contours.Count -1;
    end;

    try
      for Index := Start downto 0 do
      begin
        AContour := frmGoPhast.Contours[Index] as T2DContour;
        if {not ToggleSelectedItem and} result then
        begin
          if CanChangeSelection and AContour.Selected
            and not ToggleSelectedItem then
          begin
            AContour.Selected := False;
            Update := True;
          end;
          continue;
        end;
        if (AContour.ViewDirection = ViewDirection) then
        begin
          if AContour.Select(X, Y) then
          begin
            if not CanChangeSelection then
            begin
              result := AContour.Selected;
              Exit;
            end;
            if ToggleSelectedItem then
            begin
              AContour.Selected := not AContour.Selected ;
              Update := True;
              if AContour.Selected then
              begin
                result := True;
              end;
            end
            else
            begin
              if not AContour.Selected then
              begin
                AContour.Selected := True;
                Update := True;
              end;
              result := True;
            end;
          end
          else
          begin
            if not CanChangeSelection then Continue;
            if not ToggleSelectedItem then
            begin
              if AContour.Selected then
              begin
                AContour.Selected := False;
                Update := True;
              end;
            end;
          end;
        end;
      end;
    finally
      UpdateSelectRectangle;
      if Update then
      begin
        ContoursChanged := True;
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

procedure TframeView.ZoomBoxBottomPaintBoxPaint(Sender: TObject);
var
  Index : integer;
  Contour : T2DContour;
begin
  // This allows for the current contour and previous contours to
  // be drawn with relatively little flickering until the ZoomBox.Image
  // has been updated.
  for Index := 0 to PreviousContours.Count -1 do
  begin
    Contour := PreviousContours[Index];
    Contour.Draw(ZoomBox.BottomPaintBox.Canvas);
  end;

  if (BitMap <> nil) and (CurrentContour <> nil) then
  begin
    CurrentContour.Draw(ZoomBox.BottomPaintBox.Canvas);
  end;  
end;

{procedure TframeTop.EditContours;
var
  AList : TList;
  Index : Integer;
  AContour : T2DContour;
begin
  frmGoPhast.EditContours;
{  // This procedure allows the user to edit the properties of the selected
  // contours.
  AList := TList.Create;
  try
    for Index := 0 to frmGoPhast.Contours.Count -1 do
    begin
      AContour := frmGoPhast.Contours[Index] as T2DContour;
      if AContour.Selected then
      begin
        AList.Add(AContour);
      end;
    end;
    if AList.Count > 0 then
    begin
      Application.CreateForm(TfrmContourProperties, frmContourProperties);
      try
        frmContourProperties.GetDataForMultipleContours(AList);
        if frmContourProperties.ShowModal = mrOK then
        begin
          if frmContourProperties.UndoContourProperties <> nil then
          begin
            frmGoPhast.UndoStack.Submit(frmContourProperties.UndoContourProperties);
            frmContourProperties.UndoContourProperties := nil;
          end;
        end;
      finally
        FreeAndNil(frmContourProperties);
      end;
    end;
  finally
    AList.Free;
  end; }
//end;

procedure TframeView.UpdateSelectRectangle;
var
  Index : Integer;
  AContour : T2DContour;
  First : boolean;
  Temp, TempL, TempH: integer;
begin
  // This procedure calculates the coordinates of a rectangle that surrounds
  // the selected objects.
  First := True;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if (AContour.ViewDirection = ViewDirection) and AContour.Selected then
    begin
      if First then
      begin
        First := False;
        SelectTopLeft.X := ZoomBox.XCoord(AContour.MaxX);
        SelectTopLeft.Y := ZoomBox.YCoord(AContour.MaxY);
        SelectBottomRight.X := ZoomBox.XCoord(AContour.MinX);
        SelectBottomRight.Y := ZoomBox.YCoord(AContour.MinY);
        if SelectTopLeft.X > SelectBottomRight.X then
        begin
          Temp := SelectTopLeft.X;
          SelectTopLeft.X := SelectBottomRight.X;
          SelectBottomRight.X := Temp;
        end;
        if SelectTopLeft.Y > SelectBottomRight.Y then
        begin
          Temp := SelectTopLeft.Y;
          SelectTopLeft.Y := SelectBottomRight.Y;
          SelectBottomRight.Y := Temp;
        end;
      end
      else
      begin
        TempL := ZoomBox.XCoord(AContour.MinX);
        TempH := ZoomBox.XCoord(AContour.MaxX);
        if TempH < TempL then
        begin
          Temp := TempH;
          TempH := TempL;
          TempL := Temp;
        end;
        if SelectTopLeft.X > TempL then
        begin
          SelectTopLeft.X := TempL
        end;
        if SelectBottomRight.X < TempH then
        begin
          SelectBottomRight.X := TempH
        end;

        TempL := ZoomBox.YCoord(AContour.MinY);
        TempH := ZoomBox.YCoord(AContour.MaxY);
        if TempH < TempL then
        begin
          Temp := TempH;
          TempH := TempL;
          TempL := Temp;
        end;
        if SelectTopLeft.Y > TempL then
        begin
          SelectTopLeft.Y := TempL
        end;
        if SelectBottomRight.Y < TempH then
        begin
          SelectBottomRight.Y := TempH
        end;
      end;
    end;
  end;
  if DrawSelectRect = First then
  begin
    DrawSelectRect := not First and frmGoPhast.tbSelect.Down;
    ZoomBox.PaintBox.Invalidate;
  end;
end;

procedure TframeView.InsertPoint(const X, Y: integer);
var
  Index : integer;
  AContour : T2DContour;
  Edge : integer;
  APoint : T2DRealPoint;
  UndoInsertPoint : TUndoInsertPoint;
begin
  // This procedure allows the user to insert a node into a selected object.
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected and (AContour.ViewDirection = ViewDirection) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        APoint.X := ZoomBox.X(X);
        APoint.Y := ZoomBox.Y(Y);
        UndoInsertPoint := TUndoInsertPoint.Create(AContour, Edge+1, APoint);
        frmGoPhast.UndoStack.Submit(UndoInsertPoint);
        UndoInsertPoint.SetPostSelection;
        Exit;
      end;
    end;
  end;

end;

procedure TframeView.DeleteSegment(const X, Y: integer);
var
  Index : integer;
  AContour : T2DContour;
  Edge : integer;
  UndoDeleteSegment : TUndoDeleteSegment;
begin
  // This procedure allows the user to delete one edge of a selected object.
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected and (AContour.ViewDirection = ViewDirection) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        UndoDeleteSegment := TUndoDeleteSegment.Create(AContour, Edge);
        frmGoPhast.UndoStack.Submit(UndoDeleteSegment);
        UndoDeleteSegment.SetPostSelection;
        Exit;
      end;
    end;
  end;
end;

procedure TframeView.ToFront1Click(Sender: TObject);
var
  UndoToFront : TUndoToFront;
begin
  // This procedure allows the user to move selected objects in front
  // all other objects.
  UndoToFront := TUndoToFront.Create(ViewDirection);
  UndoToFront.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoToFront);
end;

procedure TframeView.ToBack1Click(Sender: TObject);
var
  UndoToBack : TUndoToBack;
begin
  // This procedure allows the user to move selected objects in behind
  // all other objects.
  UndoToBack := TUndoToBack.Create(ViewDirection);
  UndoToBack.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoToBack);
end;

procedure TframeView.ForwardOne1Click(Sender: TObject);
var
  UndoMoveUp : TUndoMoveUp;
begin
  // This procedure allows the user to move selected objects forward one
  // in the list of objects so that they move forward of one other object.
  UndoMoveUp := TUndoMoveUp.Create(ViewDirection);
  UndoMoveUp.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoMoveUp);
end;

procedure TframeView.BackOne1Click(Sender: TObject);
var
  UndoMoveDown : TUndoMoveDown;
begin
  // This procedure allows the user to move selected objects back one
  // in the list of objects so that they move behind one other object.
  UndoMoveDown := TUndoMoveDown.Create(ViewDirection);
  UndoMoveDown.SetPostSelection;
  frmGoPhast.UndoStack.Submit(UndoMoveDown);
end;

function TframeView.GridCenter: T2DRealPoint;
var
  Corners: array[0..3] of T2DRealPoint;
  Index : integer;
begin
  if (frmGoPhast.PhastGrid.RowCount > 0) and
    (frmGoPhast.PhastGrid.ColumnCount > 0) then
  begin
    Corners[0] := frmGoPhast.PhastGrid.TwoDCellCorner(0,0);
    Corners[1] := frmGoPhast.PhastGrid.TwoDCellCorner(0,frmGoPhast.PhastGrid.RowCount);
    Corners[2] := frmGoPhast.PhastGrid.TwoDCellCorner(frmGoPhast.PhastGrid.ColumnCount,frmGoPhast.PhastGrid.RowCount);
    Corners[3] := frmGoPhast.PhastGrid.TwoDCellCorner(frmGoPhast.PhastGrid.ColumnCount,0);
  end;
  result.X := 0;
  result.Y := 0;
  for Index := 0 to 3 do
  begin
    result.X := Corners[Index].X + result.X;
    result.Y := Corners[Index].Y + result.Y;
  end;
  result.X := result.X/4;
  result.Y := result.Y/4;
end;

procedure TframeView.RotatedGridCorners(Angle: double;
  out PointArray : array of TPoint);
var
  Center : T2DRealPoint;
  Corners: array[0..3] of T2DRealPoint;
  Index : integer;
  PointAngle, PointDistance : double;
  X, Y: double;
begin
  Assert(Length(PointArray) = 5);
  if (frmGoPhast.PhastGrid.RowCount > 0) and
    (frmGoPhast.PhastGrid.ColumnCount > 0) then
  begin
    Corners[0] := frmGoPhast.PhastGrid.TwoDCellCorner(0,0);
    Corners[1] := frmGoPhast.PhastGrid.TwoDCellCorner(0,frmGoPhast.PhastGrid.RowCount);
    Corners[2] := frmGoPhast.PhastGrid.TwoDCellCorner(frmGoPhast.PhastGrid.ColumnCount,frmGoPhast.PhastGrid.RowCount);
    Corners[3] := frmGoPhast.PhastGrid.TwoDCellCorner(frmGoPhast.PhastGrid.ColumnCount,0);
  end;
  Center := GridCenter;

  for Index := 0 to 3 do
  begin
    PointAngle := ArcTan2(Corners[Index].Y-Center.Y,
      Corners[Index].X-Center.X);
    PointDistance := Sqrt(Sqr(Corners[Index].X-Center.X)
      + Sqr(Corners[Index].Y-Center.Y));
    PointAngle := PointAngle + Angle;
    X := Center.X + PointDistance*Cos(PointAngle);
    Y := Center.Y + PointDistance*Sin(PointAngle);
    PointArray[Index].X := ZoomBox.XCoord(X);
    PointArray[Index].Y := ZoomBox.YCoord(Y);
  end;

  PointArray[4] := PointArray[0];

end;

procedure TframeView.LayerChange(Sender: TObject);
begin
  with frmGoPhast.PhastGrid do
  begin
    ModelCube.Selection2 := (SelectedLayer+1)/(LayerCount);
    ModelCube.Selection1 := (SelectedLayer)/(LayerCount);
  end;
end;

procedure TframeView.ChangeLayer(Const X, Y: Integer);
var
  ClickDirection : TRbwClickDirection;
begin
  ClickDirection := ModelCube.ClickDirection(X,Y);
  if ModelCube.ZOrigin = zoBottom then
  begin
    if ClickDirection = cdUp then
    begin
      frmGoPhast.PhastGrid.SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer+1;
    end
    else if ClickDirection = cdDown then
    begin
      frmGoPhast.PhastGrid.SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer-1;
    end;
  end
  else
  begin
    if ClickDirection = cdUp then
    begin
      frmGoPhast.PhastGrid.SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer-1;
    end
    else if ClickDirection = cdDown then
    begin
      frmGoPhast.PhastGrid.SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer+1;
    end;
  end;
  DisplayLayer;
end;


procedure TframeView.ModelCubeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  frmGoPhast.Timer1.Enabled := False;
  if frmGoPhast.Timer1.Interval = 1000 then
  begin
    case ViewDirection of
      vdTop: ChangeLayer(X,Y);
      vdFront: ChangeRow(X,Y);
      vdSide: ChangeColumn(X,Y);
    else Assert(False);
    end;
  end;
end;

procedure TframeView.ModelCubePaint(Sender: TObject);
begin
  ModelCube.Canvas.Rectangle(0,0,ModelCube.Width, ModelCube.Height);
end;

procedure TframeView.DisplayLayer;
begin
  frmGoPhast.StatusBar1.Panels[1].Text := 'Selected Layer: '
    + IntToStr(frmGoPhast.PhastGrid.SelectedLayer+1);
end;

procedure TframeView.ModelCubeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  case ViewDirection of
    vdTop: DisplayLayer;
    vdFront: DisplayRow;
    vdSide: DisplayColumn;
  else Assert(False);
  end;
  CubeX := X;
  CubeY := Y;
end;

procedure TframeView.StartTimer(Sender: TObject);
begin
  case ViewDirection of
    vdTop: ChangeLayer(CubeX,CubeY);
    vdFront: ChangeRow(CubeX,CubeY);
    vdSide: ChangeColumn(CubeX,CubeY);
  else Assert(False);
  end;
  frmGoPhast.Timer1.Interval := 100;
end;

procedure TframeView.ModelCubeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CubeX := X;
  CubeY := Y;
  frmGoPhast.Timer1.Interval := 1000;
  frmGoPhast.Timer1.OnTimer := StartTimer;
  frmGoPhast.Timer1.Enabled := True;
end;

procedure TframeView.OrderMenuPopup(Sender: TObject);
var
  Index : integer;
  AContour : T2DContour;
  notSelectedFound : boolean;
begin
  ToBack1.Enabled := False;
  BackOne1.Enabled := False;
  ToFront1.Enabled := False;
  ForwardOne1.Enabled := False;

  notSelectedFound := False;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected then
    begin
      if notSelectedFound then
      begin
        ToBack1.Enabled := True;
        BackOne1.Enabled := True;
        break;
      end;
    end
    else
    begin
      notSelectedFound := True;
    end;
  end;

  notSelectedFound := False;
  for Index := frmGoPhast.Contours.Count -1 downto 0 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected then
    begin
      if notSelectedFound then
      begin
        ToFront1.Enabled := True;
        ForwardOne1.Enabled := True;
        break;
      end;
    end
    else
    begin
      notSelectedFound := True;
    end;
  end;
end;

procedure TframeView.SetInsertPointCursor(X, Y: Integer);
var
  Index : integer;
  AContour : T2DContour;
  Edge : integer;
begin
  // The user wants to insert a point.  Check if the mouse is over the
  // edge of a selected object.
  Edge := -1;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected and (AContour.ViewDirection = ViewDirection) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      If Edge >= 0 then break;
    end
  end;
  // set the Cursor based on whether or not the mouse is over an
  // edge of a selected object.
  if Edge >= 0 then
  begin
    ZoomBox.Cursor := crInsertPoint;
    ZoomBox.PaintBox.Cursor := crInsertPoint;
  end
  else
  begin
    ZoomBox.Cursor := crDisabledInsertPoint;
    ZoomBox.PaintBox.Cursor := crDisabledInsertPoint;
  end;
end;

procedure TframeView.SetDeleteSegmentCursor(X, Y: Integer);
var
  Index : integer;
  AContour : T2DContour;
  Edge : integer;
begin
  // The user wants to delete on segment of an object.  Check if the
  // mouse is over the edge of an object.
  Edge := -1;
  for Index := 0 to frmGoPhast.Contours.Count -1 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if (AContour.ViewDirection = ViewDirection) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      If Edge >= 0 then break;
    end
  end;
  // set the Cursor based on whether or not the mouse is over an
  // edge of an object.
  if Edge >= 0 then
  begin
    ZoomBox.Cursor := crDeleteSegment;
    ZoomBox.PaintBox.Cursor := crDeleteSegment;
  end
  else
  begin
    ZoomBox.Cursor := crDisabledDeleteSegment;
    ZoomBox.PaintBox.Cursor := crDisabledDeleteSegment;
  end;

end;

function TframeView.IsOnFrontColumn(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Column : integer;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.PhastGrid.ColumnCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.ColumnPosition[Column];
    if (Abs(ZoomBox.XCoord(APoint.X) - X) <= 5)
      and (Abs(ZoomBox.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

function TframeView.IsOnFrontLayer(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Layer : integer;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if (Abs(ZoomBox.XCoord(APoint.X) - X) <= 5)
      and (Abs(ZoomBox.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

function TframeView.IsOnSideLayer(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Layer : integer;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if (Abs(ZoomBox.XCoord(APoint.X) - X) <= 5)
      and (Abs(ZoomBox.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

function TframeView.IsOnSideRow(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Row : integer;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.RowPosition[Row];
    if (Abs(ZoomBox.XCoord(APoint.X) - X) <= 5)
      and (Abs(ZoomBox.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;


procedure TframeView.DeleteColumnOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
  UndoDeleteColumn : TUndoDeleteColumn;
  UndoDeleteLayer : TUndoDeleteLayer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Column >= 0) and (Layer >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    if IsOnFrontColumn(X, Y) then
    begin
      UndoDeleteColumn := TUndoDeleteColumn.Create(Column);
      frmGoPhast.UndoStack.Submit(UndoDeleteColumn);
    end;
    if IsOnFrontLayer(X, Y) then
    begin
      UndoDeleteLayer := TUndoDeleteLayer.Create(Layer);
      frmGoPhast.UndoStack.Submit(UndoDeleteLayer);
    end;
  end;
end;

procedure TframeView.DeleteRowOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
  UndoDeleteRow : TUndoDeleteRow;
  UndoDeleteLayer : TUndoDeleteLayer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Layer >= 0)
    and (Row <= frmGoPhast.PhastGrid.RowCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.LayerElevation[Layer];
    APoint.Y := frmGoPhast.PhastGrid.RowPosition[Row];
    if IsOnSideLayer(X,Y) then
    begin
      UndoDeleteLayer := TUndoDeleteLayer.Create(Layer);
      frmGoPhast.UndoStack.Submit(UndoDeleteLayer);
    end;
    if IsOnSideRow(X,Y) then
    begin
      UndoDeleteRow := TUndoDeleteRow.Create(Row);
      frmGoPhast.UndoStack.Submit(UndoDeleteRow);
    end;
  end;
end;

procedure TframeView.MoveColumnOrLayer(X, Y: Integer);
begin
  if not (frmGoPhast.MovingColumn or frmGoPhast.MovingLayer) then
  begin
    Exit;
  end;
  if frmGoPhast.MovingColumn then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create
      (frmGoPhast.ColumnBeingMoved, ZoomBox.X(X)));
  end
  else if frmGoPhast.MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create
      (frmGoPhast.LayerBeingMoved, ZoomBox.Y(Y)));
  end;
  frmGoPhast.MovingColumn := False;
  frmGoPhast.MovingLayer := False;
end;

procedure TframeView.MoveRowOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  if not (frmGoPhast.MovingRow or frmGoPhast.MovingLayer) then
  begin
    Exit;
  end;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  if frmGoPhast.MovingRow then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveRow.Create(frmGoPhast.RowBeingMoved, APoint.Y));
  end
  else if frmGoPhast.MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create(frmGoPhast.LayerBeingMoved, APoint.X));
  end;
  frmGoPhast.MovingRow := False;
  frmGoPhast.MovingLayer := False;
end;

procedure TframeView.AddColumnOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddColumn.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(ZoomBox.X(X)));
  end
  else if frmGoPhast.tbAddRow.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(ZoomBox.Y(Y)));
  end;
end;

procedure TframeView.AddRowOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddColumn.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(ZoomBox.X(X)));
  end
  else if frmGoPhast.tbAddRow.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(ZoomBox.Y(Y)));
  end;
end;

procedure TframeView.SubdivideFront(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetColLayer(APoint, Column, Layer);
  if (Column >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.Subdivide(Column, -1, Layer);
  end;
end;

procedure TframeView.GetColLayer(APoint: T2DRealPoint; out Col, Layer: integer);
var
  NeighborCol, NeighborLayer : integer;
begin
  // Get the column and layer numbers of the cell containing APoint;
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  Col := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  if (Col >= 0) and (Layer >= 0)
    and (Col <= frmGoPhast.PhastGrid.ColumnCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    if APoint.X > frmGoPhast.PhastGrid.ColumnPosition[Col] then
    begin
      NeighborCol := Col+1;
    end
    else
    begin
      NeighborCol := Col-1;
    end;
    if APoint.Y > frmGoPhast.PhastGrid.LayerElevation[Layer] then
    begin
      NeighborLayer := Layer+1;
    end
    else
    begin
      NeighborLayer := Layer-1;
    end;
    if (NeighborCol >= 0) and (NeighborLayer >= 0)
      and (NeighborLayer <= frmGoPhast.PhastGrid.LayerCount)
      and (NeighborCol <= frmGoPhast.PhastGrid.ColumnCount) then
    begin
      if NeighborLayer < Layer then
      begin
        Layer := NeighborLayer;
      end;
      if NeighborCol < Col then
      begin
        Col := NeighborCol;
      end;
    end
    else
    begin
      Layer := -1;
      Col := -1;
    end;
  end
  else
  begin
    Layer := -1;
    Col := -1;
  end;

end;

procedure TframeView.SubdivideSide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.Subdivide(-1, Row, Layer);
  end;
end;

procedure TframeView.GetRowLayer(APoint: T2DRealPoint; out Row, Layer: integer);
var
  NeighborRow, NeighborLayer : integer;
begin
  // Get the row and layer numbers of the cell containing APoint;
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Layer >= 0)
    and (Row <= frmGoPhast.PhastGrid.RowCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    if APoint.X > frmGoPhast.PhastGrid.LayerElevation[Layer] then
    begin
      NeighborLayer := Layer+1;
    end
    else
    begin
      NeighborLayer := Layer-1;
    end;
    if APoint.Y > frmGoPhast.PhastGrid.RowPosition[Row] then
    begin
      NeighborRow := Row+1;
    end
    else
    begin
      NeighborRow := Row-1;
    end;
    if (NeighborRow >= 0) and (NeighborLayer >= 0)
      and (NeighborLayer <= frmGoPhast.PhastGrid.LayerCount)
      and (NeighborRow <= frmGoPhast.PhastGrid.RowCount) then
    begin
      if NeighborLayer < Layer then
      begin
        Layer := NeighborLayer;
      end;
      if NeighborRow < Row then
      begin
        Row := NeighborRow;
      end;
    end
    else
    begin
      Layer := -1;
      Row := -1;
    end;
  end
  else
  begin
    Layer := -1;
    Row := -1;
  end;

end;

procedure TframeView.SetSpacingFront(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Layer, Col : integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetColLayer(APoint, Col, Layer);
  if (Col >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.LastSpacingColumn := Col;
    frmGoPhast.LastSpacingRow := -1;
    frmGoPhast.LastSpacingLayer := Layer;
    // subdivide the column and row
    frmGoPhast.SetGridSpacing;
  end;
end;

procedure TframeView.SetSpacingSide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Layer, Row : integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.LastSpacingColumn := -1;
    frmGoPhast.LastSpacingRow := Row;
    frmGoPhast.LastSpacingLayer := Layer;
    // subdivide the column and row
    frmGoPhast.SetGridSpacing;
  end;
end;


function TframeView.CursorGrid: TCursorGrid;
begin
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

procedure TframeView.ChangeColumn(Const X, Y: Integer);
var
  ClickDirection : TRbwClickDirection;
begin
  ClickDirection := ModelCube.ClickDirection(X,Y);
  if ModelCube.XOrigin = xoWest then
  begin
    if ClickDirection = cdWest then
    begin
      frmGoPhast.PhastGrid.SelectedColumn := frmGoPhast.PhastGrid.SelectedColumn-1;
    end
    else if ClickDirection = cdEast then
    begin
      frmGoPhast.PhastGrid.SelectedColumn := frmGoPhast.PhastGrid.SelectedColumn+1;
    end;
  end
  else
  begin
    if ClickDirection = cdWest then
    begin
      frmGoPhast.PhastGrid.SelectedColumn := frmGoPhast.PhastGrid.SelectedColumn+1;
    end
    else if ClickDirection = cdEast then
    begin
      frmGoPhast.PhastGrid.SelectedColumn := frmGoPhast.PhastGrid.SelectedColumn-1;
    end;
  end;
  DisplayColumn;
end;

procedure TframeView.DisplayColumn;
begin
  frmGoPhast.StatusBar1.Panels[1].Text := 'Selected Col: '
    + IntToStr(frmGoPhast.PhastGrid.SelectedColumn+1);
end;

procedure TframeView.ChangeRow(Const X, Y: Integer);
var
  ClickDirection : TRbwClickDirection;
begin
  ClickDirection := ModelCube.ClickDirection(X,Y);
  if ModelCube.YOrigin = yoSouth then
  begin
    if ClickDirection = cdNorth then
    begin
      frmGoPhast.PhastGrid.SelectedRow := frmGoPhast.PhastGrid.SelectedRow+1;
    end
    else if ClickDirection = cdSouth then
    begin
      frmGoPhast.PhastGrid.SelectedRow := frmGoPhast.PhastGrid.SelectedRow-1;
    end;
  end
  else
  begin
    if ClickDirection = cdNorth then
    begin
      frmGoPhast.PhastGrid.SelectedRow := frmGoPhast.PhastGrid.SelectedRow-1;
    end
    else if ClickDirection = cdSouth then
    begin
      frmGoPhast.PhastGrid.SelectedRow := frmGoPhast.PhastGrid.SelectedRow+1;
    end;
  end;
  DisplayRow;
end;

procedure TframeView.DisplayRow;
begin
  frmGoPhast.StatusBar1.Panels[1].Text := 'Selected Row: '
    + IntToStr(frmGoPhast.PhastGrid.SelectedRow+1);
end;

procedure TframeView.BeginFrontMove(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  // start to move a column or layer.

  // First find the nearest column or layer boundaries.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Column >= 0) and (Layer >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    // The point is over the grid.
    APoint.X := frmGoPhast.PhastGrid.ColumnPosition[Column];
    APoint.Y := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if IsOnFrontColumn(X,Y) then
    begin
      // If the point is over a column, set the cursor.
      // and set MovingColumn to true.
      // Store the column that is being moved.
      ZoomBox.Cursor := crMoveColumn;
      ZoomBox.PaintBox.Cursor := crMoveColumn;
      frmGoPhast.MovingColumn := True;
      frmGoPhast.MovingLayer := False;
      frmGoPhast.ColumnBeingMoved := Column;
    end
    else if IsOnFrontLayer(X,Y) then
    begin
      // If the point is over a layer, set the cursor.
      // and set MovingLayer to true.
      // Store the row that is being moved.
      ZoomBox.Cursor := crMoveRow;
      ZoomBox.PaintBox.Cursor := crMoveRow;
      frmGoPhast.MovingLayer := True;
      frmGoPhast.MovingColumn := False;
      frmGoPhast.LayerBeingMoved := Layer;
    end;
  end;

end;

procedure TframeView.BeginSideMove(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  if (Row >= 0) and (Layer >= 0)
    and (Row <= frmGoPhast.PhastGrid.RowCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.RowPosition[Row];
    APoint.X := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if IsOnSideLayer(X,Y)  then
    begin
      ZoomBox.Cursor := crMoveColumn;
      frmGoPhast.MovingLayer := True;
      frmGoPhast.MovingRow := False;
      frmGoPhast.LayerBeingMoved := Layer;
    end
    else if IsOnSideRow(X,Y) then
    begin
      ZoomBox.Cursor := crMoveRow;
      frmGoPhast.MovingRow := True;
      frmGoPhast.MovingLayer := False;
      frmGoPhast.RowBeingMoved := Row;
    end;
  end;
end;

procedure TframeView.BeginMove(X, Y: Integer);
begin
  case ViewDirection of
    vdTop: BeginTopMove(X,Y);
    vdFront: BeginFrontMove(X,Y);
    vdSide: BeginSideMove(X,Y);
  else Assert(False);
  end;
end;

procedure TframeView.DrawBlueSelectedFrontCells(FirstCol, LastCol,
  FirstLayer, LastLayer: integer);
var
  APoint : T3DRealPoint;
  L1, L2, C1, C2 : integer;
  Polygon : array[0..3] of TPoint;
  OldBrushStyle : TBrushStyle;
  OldColor : TColor;
begin
  if FirstCol < LastCol then
  begin
    C1 := FirstCol;
    C2 := LastCol+1;
  end
  else
  begin
    C1 := LastCol;
    C2 := FirstCol+1;
  end;
  if FirstLayer < LastLayer then
  begin
    L1 := FirstLayer;
    L2 := LastLayer+1;
  end
  else
  begin
    L1 := LastLayer;
    L2 := FirstLayer+1;
  end;

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L1);
  Polygon[0].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[0].Y := self.ZoomBox.YCoord(APoint.Z);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2 ,0, L1);
  Polygon[1].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[1].Y := self.ZoomBox.YCoord(APoint.Z);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2, 0, L2);
  Polygon[2].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[2].Y := self.ZoomBox.YCoord(APoint.Z);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L2);
  Polygon[3].X := self.ZoomBox.XCoord(APoint.X);
  Polygon[3].Y := self.ZoomBox.YCoord(APoint.Z);

  OldBrushStyle := ZoomBox.PaintBox.Canvas.Brush.Style;
  OldColor := ZoomBox.PaintBox.Canvas.Brush.Color;
  try
    ZoomBox.PaintBox.Canvas.Brush.Style := bsSolid;
    ZoomBox.PaintBox.Canvas.Brush.Color := clBlue;

    ZoomBox.PaintBox.Canvas.Polygon(Polygon);

  finally
    ZoomBox.PaintBox.Canvas.Brush.Style := OldBrushStyle;
    ZoomBox.PaintBox.Canvas.Brush.Color := OldColor;
  end;
end;

procedure TframeView.DrawBlueSelectedSideCells(FirstRow, LastRow, FirstLayer,
  LastLayer: integer);
var
  APoint : T3DRealPoint;
  L1, L2, R1, R2 : integer;
  Polygon : array[0..3] of TPoint;
  OldBrushStyle : TBrushStyle;
  OldColor : TColor;
begin
  if FirstRow < LastRow then
  begin
    R1 := FirstRow;
    R2 := LastRow+1;
  end
  else
  begin
    R1 := LastRow;
    R2 := FirstRow+1;
  end;
  if FirstLayer < LastLayer then
  begin
    L1 := FirstLayer;
    L2 := LastLayer+1;
  end
  else
  begin
    L1 := LastLayer;
    L2 := FirstLayer+1;
  end;

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L1);
  Polygon[0].X := ZoomBox.XCoord(APoint.Z);
  Polygon[0].Y := ZoomBox.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0 ,R2, L1);
  Polygon[1].X := ZoomBox.XCoord(APoint.Z);
  Polygon[1].Y := ZoomBox.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R2, L2);
  Polygon[2].X := ZoomBox.XCoord(APoint.Z);
  Polygon[2].Y := ZoomBox.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L2);
  Polygon[3].X := ZoomBox.XCoord(APoint.Z);
  Polygon[3].Y := ZoomBox.YCoord(APoint.Y);

  OldBrushStyle := ZoomBox.PaintBox.Canvas.Brush.Style;
  OldColor := ZoomBox.PaintBox.Canvas.Brush.Color;
  try
    ZoomBox.PaintBox.Canvas.Brush.Style := bsSolid;
    ZoomBox.PaintBox.Canvas.Brush.Color := clBlue;

    ZoomBox.PaintBox.Canvas.Polygon(Polygon);

  finally
    ZoomBox.PaintBox.Canvas.Brush.Style := OldBrushStyle;
    ZoomBox.PaintBox.Canvas.Brush.Color := OldColor;
  end;
end;

procedure TframeView.DrawSubdivide;
begin
  case ViewDirection of
    vdTop:
      begin
        DrawBlueSelectedTopCells(frmGoPhast.FirstSubdivideColumn,
          frmGoPhast.LastSubdivideColumn,
          frmGoPhast.FirstSubdivideRow,
          frmGoPhast.LastSubdivideRow);
      end;
    vdFront:
      begin
        DrawBlueSelectedFrontCells(frmGoPhast.FirstSubdivideColumn,
          frmGoPhast.LastSubdivideColumn,
          frmGoPhast.FirstSubdivideLayer,
          frmGoPhast.LastSubdivideLayer);
      end;
    vdSide:
      begin
        DrawBlueSelectedSideCells(frmGoPhast.FirstSubdivideRow,
          frmGoPhast.LastSubdivideRow,
          frmGoPhast.FirstSubdivideLayer,
          frmGoPhast.LastSubdivideLayer);
      end;
  else Assert(False);
  end;
end;

procedure TframeView.DrawSpacing;
begin
  case ViewDirection of
    vdTop:
      begin
          DrawBlueSelectedTopCells(frmGoPhast.FirstSpacingColumn,
            frmGoPhast.LastSpacingColumn,
            frmGoPhast.FirstSpacingRow,
            frmGoPhast.LastSpacingRow);
      end;
    vdFront:
      begin
          DrawBlueSelectedFrontCells(frmGoPhast.FirstSpacingColumn,
            frmGoPhast.LastSpacingColumn,
            frmGoPhast.FirstSpacingLayer,
            frmGoPhast.LastSpacingLayer);
      end;
    vdSide:
      begin
          DrawBlueSelectedSideCells(frmGoPhast.FirstSpacingRow,
            frmGoPhast.LastSpacingRow,
            frmGoPhast.FirstSpacingLayer,
            frmGoPhast.LastSpacingLayer);
      end;
  else Assert(False);
  end;
end;

procedure TframeView.SetFrontCursor(X, Y: integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  if frmGoPhast.MovingColumn
    or frmGoPhast.MovingLayer
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
    // There is no need to change the cursor.
    // Just repaint and quit.
    ZoomBox.PaintBox.Invalidate;
    Exit;
  end;

  // get the location in real-world coordinates of the current cursor location
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  // Get the column and layer boundaries closest to the current cursor.
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Column >= 0) and (Layer >= 0)
    and (Column <= frmGoPhast.PhastGrid.ColumnCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    // The cursor is over the grid
    if IsOnFrontColumn(X,Y) or IsOnFrontLayer(X,Y) then
    begin
      // The cursor is over a column or layer boundary
      if frmGoPhast.tbDeleteColumnRow.Down then
      begin
        // Set the cursor for deleting columns or layers
        ZoomBox.Cursor := crDelete;
        ZoomBox.PaintBox.Cursor := crDelete;
      end
      else if IsOnFrontColumn(X,Y) then
      begin
        // Set the cursor for moving columns
        ZoomBox.Cursor := crMoveColumn;
        ZoomBox.PaintBox.Cursor := crMoveColumn;
      end
      else if IsOnFrontLayer(X,Y) then
      begin
        // Set the cursor for moving layers
        ZoomBox.Cursor := crMoveRow;
        ZoomBox.PaintBox.Cursor := crMoveRow;
      end;
    end
    else
    begin
      ZoomBox.Cursor := crArrow;
      ZoomBox.PaintBox.Cursor := crArrow;
    end;
  end
  else
  begin
    ZoomBox.Cursor := crArrow;
    ZoomBox.PaintBox.Cursor := crArrow;
  end;
end;

procedure TframeView.SetSideCursor(X, Y: integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  if frmGoPhast.MovingRow
    or frmGoPhast.MovingLayer
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
    ZoomBox.PaintBox.Invalidate;
    Exit;
  end;

  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Layer >= 0)
    and (Row <= frmGoPhast.PhastGrid.RowCount)
    and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    if IsOnSideRow(X,Y) or IsOnSideLayer(X,Y) then
    begin
      if frmGoPhast.tbDeleteColumnRow.Down then
      begin
        ZoomBox.Cursor := crDelete;
      end
      else if IsOnSideLayer(X,Y) then
      begin
        ZoomBox.Cursor := crMoveColumn;
      end
      else if IsOnSideRow(X,Y) then
      begin
        ZoomBox.Cursor := crMoveRow;
      end;
    end
    else
    begin
      ZoomBox.Cursor := crArrow;
    end;
  end
  else
  begin
    ZoomBox.Cursor := crArrow;
  end;
end;

procedure TframeView.ShowNewColumnOrLayer;
var
  APoint : T3DRealPoint;
  CursorPoint : T2DRealPoint;
begin
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
  try
    if (frmGoPhast.PhastGrid.ColumnCount > 0)
      and (frmGoPhast.PhastGrid.RowCount > 0)
      and (frmGoPhast.PhastGrid.LayerCount > 0) then
    begin
      if (frmGoPhast.tbMove.Down and frmGoPhast.MovingColumn) or frmGoPhast.tbAddColumn.Down then
      begin
        APoint.X := CursorPoint.X;
        APoint.Y := frmGoPhast.PhastGrid.LayerElevation[0];
        ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));

        APoint.X := CursorPoint.X;
        APoint.Y := frmGoPhast.PhastGrid.LayerElevation[frmGoPhast.PhastGrid.LayerCount];
        ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));
      end
      else if (frmGoPhast.tbMove.Down and frmGoPhast.MovingLayer) or frmGoPhast.tbAddRow.Down then
      begin
        APoint.X := frmGoPhast.PhastGrid.ColumnPosition[0];
        APoint.Y := CursorPoint.Y;
        ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));

        APoint.X := frmGoPhast.PhastGrid.ColumnPosition[frmGoPhast.PhastGrid.ColumnCount];
        APoint.Y := CursorPoint.Y;
        ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X), ZoomBox.YCoord(APoint.Y));
      end;
    end;
  finally
    ZoomBox.PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;

procedure TframeView.ShowNewRowOrLayer;
var
  APoint : T3DRealPoint;
  CursorPoint : T2DRealPoint;
begin
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  ZoomBox.PaintBox.Canvas.Pen.Style := psDash;
  try
    if (frmGoPhast.PhastGrid.ColumnCount > 0)
      and (frmGoPhast.PhastGrid.RowCount > 0)
      and (frmGoPhast.PhastGrid.LayerCount > 0) then
    begin
      if (frmGoPhast.tbMove.Down and frmGoPhast.MovingLayer)
        or frmGoPhast.tbAddColumn.Down then
      begin
        APoint.Y := frmGoPhast.PhastGrid.RowPosition[0];
        APoint.X := CursorPoint.X;
        ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(APoint.X),
          ZoomBox.YCoord(APoint.Y));

        APoint.Y := frmGoPhast.PhastGrid.RowPosition
          [frmGoPhast.PhastGrid.RowCount];
        APoint.X := CursorPoint.X;
        ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X),
          ZoomBox.YCoord(APoint.Y));
      end
      else if (frmGoPhast.tbMove.Down and frmGoPhast.MovingRow)
        or frmGoPhast.tbAddRow.Down then
      begin
        APoint.Y := CursorPoint.Y;
        APoint.X := frmGoPhast.PhastGrid.LayerElevation[0];
        ZoomBox.PaintBox.Canvas.MoveTo(ZoomBox.XCoord(APoint.X),
          ZoomBox.YCoord(APoint.Y));

        APoint.Y := CursorPoint.Y;
        APoint.X := frmGoPhast.PhastGrid.LayerElevation
          [frmGoPhast.PhastGrid.LayerCount];
        ZoomBox.PaintBox.Canvas.LineTo(ZoomBox.XCoord(APoint.X),
          ZoomBox.YCoord(APoint.Y));
      end;
    end;
  finally
    ZoomBox.PaintBox.Canvas.Pen.Style := psSolid;
  end;

end;

procedure TframeView.ColumnChange(Sender: TObject);
begin
  with frmGoPhast.PhastGrid do
  begin
    ModelCube.Selection2 := (SelectedColumn+1)/(ColumnCount);
    ModelCube.Selection1 := (SelectedColumn)/(ColumnCount);
  end;
end;

procedure TframeView.RowChange(Sender: TObject);
begin
  with frmGoPhast.PhastGrid do
  begin
    ModelCube.Selection2 := (SelectedRow+1)/(RowCount);
    ModelCube.Selection1 := (SelectedRow)/(RowCount);
  end;
end;

end.
