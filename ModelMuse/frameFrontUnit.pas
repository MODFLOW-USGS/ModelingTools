unit frameFrontUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QZoomBox, QExtCtrls, QRbwRuler, QStdCtrls, QRbwModelCube, AbstractGridUnit,
  ContourUnit, UndoItems, SelectUnit, QCustomRbwRuler;

type
  TframeFront = class(TFrame)
    rulerFrontVertical: TRbwRuler;
    zbFront: TQrbwZoomBox;
    Panel1: TPanel;
    ModelCube: TRbwModelCube;
    rulerFrontHoriz: TRbwRuler;
    procedure zbFrontHorzScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbFrontVertScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbFrontPaintBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure zbFrontPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure zbFrontPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure zbFrontPaintBoxPaint(Sender: TObject);
    procedure pnlWhiteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ModelCubePaint(Sender: TObject);
    procedure ModelCubeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ModelCubeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ModelCubeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure zbFrontBottomPaintBoxPaint(Sender: TObject);
    procedure zbFrontPaintBoxDblClick(Sender: TObject);
  private
    bmpFront : TBitmap;
    CubeX, CubeY : integer;
    CurrentUndo : TCustomUndo;
    DoubleClicked : boolean;
    Drawing : boolean;
    MouseButtonIsDown : boolean;
    MovingContours : boolean;
    PreviousContours : TList;
    PriorCursorX, PriorCursorY : integer;
    SelectBottomRight : TPoint;
    SelectedPointContour : T2DContour;
    SelectLine: TLine;
    SelectTopLeft : TPoint;
    StartX, StartY : integer;
    procedure AddColumnOrLayer(X, Y: Integer);
    function AreContoursSelected: boolean;
    procedure BeginMove(X, Y: Integer);
    procedure BeginSetSpacing(X, Y: Integer);
    procedure BeginSubdivide(X, Y: Integer);
    procedure ChangeRow(Const X, Y: Integer);
    procedure ContinueLineContour(X, Y: Integer);
    procedure ContinueRectangle(X, Y: Integer);
    procedure ContinueSetSpacing(X, Y: integer);
    procedure ContinueStraightLineContour(X, Y: Integer);
    procedure ContinueSubdivide(X, Y: integer);
    procedure CreatePointContour(X, Y: Integer);
    procedure DeleteColumnOrLayer(X, Y: Integer);
    procedure DeleteSegment(const X, Y: integer);
    procedure DrawBlueSelectedCells(FirstCol, LastCol, FirstLayer,
      LastLayer: integer);
    procedure DrawContours;
    procedure DrawGridAndContours;
    procedure DrawModifiedCurrentContour;
    procedure DrawMouseSelectionRectangle;
    procedure DrawSelectionRectangle;
    procedure GetColLayer(APoint: T2DRealPoint; out Col, Layer: integer);
    procedure InsertPoint(const X, Y: integer);
    procedure InvalidateContours;
    function IsOnFrontColumn(const X, Y: integer): boolean;
    function IsOnFrontLayer(const X, Y: integer): boolean;
    procedure MoveColumnOrLayer(X, Y: Integer);
    procedure MoveContours(const X, Y: integer);
    function SelectContours(const X, Y: integer; const SelectLowerContours,
      ToggleSelectedItem: boolean;
      const CanChangeSelection: boolean = True): boolean;
    function SelectContoursWithLine(
      const ToggleSelection: boolean): boolean;
    function SelectPoints(const X, Y: integer; const AContour: T2DContour;
      const AddToSelection: boolean; var Changed: boolean): boolean;
    function SelectPointsOfAllSelectedContoursWithLine(
      const AddToSelection: boolean): boolean;
    function SelectPointsOfASelectedContour(const X, Y: integer;
      const AddToSelection: boolean): boolean;
    function SelectPointsWithLine(const AContour: T2DContour;
      const AddToSelection: boolean; out Changed: boolean): boolean;
    procedure SetACursor(X, Y: integer);
    procedure SetDeleteSegmentCursor(X, Y: Integer);
    procedure SetInsertPointCursor(X, Y: Integer);
    procedure SetSpacing(X, Y: Integer);
    procedure ShowMovedPoints;
    procedure ShowNewColumnOrLayer;
    procedure StartRowTimer(Sender: TObject);
    procedure Subdivide(X, Y: Integer);
    procedure WarnTooBig;
    { Private declarations }
  public
    CurrentContour : T2DContour;
    DrawSelectRect : boolean;
    ViewDirection : TViewDirection;
    procedure AdjustHorizontalScale;
    procedure AdjustVerticalScale;
    constructor Create(AOwner: TComponent); override;
    procedure DisplayRow;
    Destructor Destroy; override;
    procedure FinishContours;
    procedure RowChange(Sender: TObject);
    procedure UpdateSelectRectangle;
    { Public declarations }
  end;

implementation

uses CursorsFoiledAgain, frmGoPhastUnit, UndoItemsContours,
  frmContourPropertiesUnit, frameTopUnit;

{$R *.dfm}

{ TframeFront }

procedure TframeFront.AdjustHorizontalScale;
begin
  rulerFrontHoriz.RulerEnds.Lower := 10;
  rulerFrontHoriz.RulerEnds.Upper := rulerFrontHoriz.Width-30;
  rulerFrontHoriz.RulerValues.Lower := zbFront.X(10 + zbFront.HorzScrollBar.Position);
  rulerFrontHoriz.RulerValues.Upper := zbFront.X(zbFront.HorzScrollBar.Width
    -12 + zbFront.HorzScrollBar.Position);
end;

procedure TframeFront.AdjustVerticalScale;
var
  NewLower, NewUpper : double;
begin
  rulerFrontVertical.RulerEnds.Upper := Height - rulerFrontHoriz.Height -30;
  NewLower := zbFront.Y(zbFront.VertScrollBar.Height -10
    + zbFront.VertScrollBar.Position);
  NewUpper := zbFront.Y(12 + zbFront.VertScrollBar.Position);
  if NewLower < NewUpper then
  begin
    rulerFrontVertical.RulerValues.Lower := NewLower;
    rulerFrontVertical.RulerValues.Upper := NewUpper;
  end;
end;

procedure TframeFront.zbFrontHorzScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScale;
end;

procedure TframeFront.zbFrontVertScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustVerticalScale;
end;

procedure TframeFront.zbFrontPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssRight in Shift then
  begin
    // for a right click, don't create objects
    Exit;
  end;

  MouseButtonIsDown := True;
  frmGoPhast.CursorGrid := cgFront;
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
    if zbFront.CanZoomIn then
    begin
      zbFront.BeginZoom(X, Y);
    end;
  end
  else if frmGoPhast.tbPan.Down then
  begin
    // The user wants to pan to a different position.
    // Start panning and change the cursor to indicate that panning has
    // begun.
    if zbFront.CanPan then
    begin
      zbFront.Panning := True;
      zbFront.Cursor := crHandGrab;
      zbFront.PaintBox.Cursor := crHandGrab;
      Screen.Cursor := crHandGrab;
    end;
  end
  else if frmGoPhast.tbMove.Down then
  begin
    // The user wants to move a grid line.
    BeginMove(X,Y);
  end
  else if frmGoPhast.tbSubdivide.Down then
  begin
    // The user wants to subdivide one or more rows or columns
    BeginSubdivide(X,Y);
  end
  else if frmGoPhast.tbSpacing.Down then
  begin
    // The user wants to subdivide one or more rows or columns
    BeginSetSpacing(X, Y);
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
      zbFront.PaintBox.Invalidate;
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
    zbFront.PaintBox.Invalidate;
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
//    zbTop.PaintBox.Invalidate;
  end
  else if frmGoPhast.tbInsertPoint.Down or frmGoPhast.tbDeleteSegment.Down then
  begin
    // The user wants to either insert a node in an existing object or
    // delete a segment of an existing object.  In either case, you need
    // to select an object.
    SelectContours(X, Y, (ssCtrl in Shift), (ssShift in Shift));
  end;
end;

function TframeFront.IsOnFrontColumn(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Column : integer;
begin
  result := false;
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  Column := frmGoPhast.PhastGrid.NearestColumnPosition(APoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.PhastGrid.ColumnCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.ColumnPosition[Column];
    if (Abs(zbFront.XCoord(APoint.X) - X) <= 5)
      and (Abs(zbFront.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

function TframeFront.IsOnFrontLayer(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Layer : integer;
begin
  result := false;
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if (Abs(zbFront.XCoord(APoint.X) - X) <= 5)
      and (Abs(zbFront.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

procedure TframeFront.zbFrontPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  // record which grid the cursor is over
  frmGoPhast.CursorGrid := cgFront;
  // record the current cursor position
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  // display the current position
  frmGoPhast.StatusBar1.Panels[0].Text := 'X: ' + FloatToStrF(zbFront.X(X), ffGeneral, 7,1)
  + ', Z: ' + FloatToStrF(zbFront.Y(Y), ffGeneral, 7,1);

  // get the location in real-world coordinates of the current cursor location
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);

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

  if frmGoPhast.tbDeleteColumnRow.Down
    or frmGoPhast.tbMove.Down
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
    // Set the correct cursor
    SetACursor(X,Y);
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
    zbFront.Cursor := crArrow;
    zbFront.PaintBox.Cursor := crArrow;
    DrawSelectRect := False;
    // redraw to indicate where the contour would be if you clicked now.
    if CurrentContour <> nil then
    begin
      zbFront.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbPoint.Down then
  begin
    // set the correct cursor
    zbFront.Cursor := crArrow;
    zbFront.PaintBox.Cursor := crArrow;
  end
  else if frmGoPhast.tbLasso.Down and (SelectLine <> nil) then
  begin
    // set the correct cursor
    zbFront.Cursor := crArrow;
    zbFront.PaintBox.Cursor := crArrow;
    // If the cursor has moved far enough, add another point to the
    // lasso.
    if (SelectLine.Count = 0)
      or (Abs(SelectLine.Points[SelectLine.Count-1].X-X) > 10)
      or (Abs(SelectLine.Points[SelectLine.Count-1].Y-Y) > 10) then
    begin
      SelectLine.AddPoint(Point(X,Y));
      zbFront.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbSelect.Down then
  begin
    // set the correct cursor.
    zbFront.Cursor := crArrow;
    zbFront.PaintBox.Cursor := crArrow;
    // if something is happening, redraw.
    if (DrawSelectRect and (ssLeft in Shift)) or not MovingContours then
    begin
      zbFront.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbSelectPoint.Down then
  begin
    // if something is happening, redraw.
    if (SelectLine <> nil) and not MovingContours then
    begin
      zbFront.PaintBox.Invalidate;
    end
    else if (SelectedPointContour <> nil)
      and SelectedPointContour.Selected then
    begin
      zbFront.PaintBox.Invalidate;
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

procedure TframeFront.zbFrontPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var  
  APoint : TPoint;
begin
  if ssRight in Shift then
  begin
    // for a right click, don't create objects
    Exit;
  end;

  MouseButtonIsDown := False;
  // Record the current cursor position.
  frmGoPhast.CursorGrid := cgFront;
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  if frmGoPhast.tbZoomOut.Down then
  begin
    // The user want to zoom out.
    zbFront.ZoomByAt(0.5, X, Y);
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(zbFront.Magnification);
  end
  else if frmGoPhast.tbPan.Down then
  begin
    // The user has finished panning to a different position.
    // Stop panning and change the cursor to indicate that panning has
    // stopped.
    if zbFront.CanPan then
    begin
      zbFront.Panning := False;
      zbFront.Cursor := crHandFlat;
      zbFront.PaintBox.Cursor := crHandFlat;
      Screen.Cursor := crDefault;
    end;
  end
  else if frmGoPhast.tbZoom.Down then
  begin
    // The user has zoomed in to a specific region.  If further
    // zooming in is impossible, change the cursor.
    if not zbFront.CanZoomIn then
    begin
      zbFront.Cursor := crArrow;
      zbFront.PaintBox.Cursor := crArrow;
    end;
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(zbFront.Magnification);
  end
  else if frmGoPhast.tbZoomIn.Down then
  begin
    // The user want to zoom in.
    zbFront.ZoomByAt(2, X, Y);
    // If further zooming in is impossible, change the cursor.
    if not zbFront.CanZoomIn then
    begin
      zbFront.Cursor := crArrow;
      zbFront.PaintBox.Cursor := crArrow;
    end;
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(zbFront.Magnification);
  end
  else if frmGoPhast.tbDeleteColumnRow.Down then
  begin
    DeleteColumnOrLayer(X,Y);
  end
  else if frmGoPhast.tbMove.Down then
  begin
    MoveColumnOrLayer(X,Y);
  end
  else if frmGoPhast.tbAddColumn.Down or frmGoPhast.tbAddRow.Down then
  begin
    AddColumnOrLayer(X,Y);
  end
  else if frmGoPhast.tbSubdivide.Down then
  begin
    SubDivide(X,Y);
  end
  else if frmGoPhast.tbSpacing.Down then
  begin
    // Set the spacing of the selected cells.
    SetSpacing(X, Y);
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
        zbFront.PaintBox.Invalidate;
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
    zbFront.PaintBox.Invalidate;
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
    zbFront.PaintBox.Invalidate;
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

procedure TframeFront.ShowNewColumnOrLayer;
var
  APoint : T3DRealPoint;
  CursorPoint : T2DRealPoint;
begin
  CursorPoint.X := zbFront.X(frmGoPhast.CursorX);
  CursorPoint.Y := zbFront.Y(frmGoPhast.CursorY);
  zbFront.PaintBox.Canvas.Pen.Style := psDash;
  try
    if (frmGoPhast.PhastGrid.ColumnCount > 0)
      and (frmGoPhast.PhastGrid.RowCount > 0)
      and (frmGoPhast.PhastGrid.LayerCount > 0) then
    begin
      if (frmGoPhast.tbMove.Down and frmGoPhast.MovingColumn) or frmGoPhast.tbAddColumn.Down then
      begin
        APoint.X := CursorPoint.X;
        APoint.Y := frmGoPhast.PhastGrid.LayerElevation[0];
        zbFront.PaintBox.Canvas.MoveTo(zbFront.XCoord(APoint.X), zbFront.YCoord(APoint.Y));

        APoint.X := CursorPoint.X;
        APoint.Y := frmGoPhast.PhastGrid.LayerElevation[frmGoPhast.PhastGrid.LayerCount];
        zbFront.PaintBox.Canvas.LineTo(zbFront.XCoord(APoint.X), zbFront.YCoord(APoint.Y));
      end
      else if (frmGoPhast.tbMove.Down and frmGoPhast.MovingLayer) or frmGoPhast.tbAddRow.Down then
      begin
        APoint.X := frmGoPhast.PhastGrid.ColumnPosition[0];
        APoint.Y := CursorPoint.Y;
        zbFront.PaintBox.Canvas.MoveTo(zbFront.XCoord(APoint.X), zbFront.YCoord(APoint.Y));

        APoint.X := frmGoPhast.PhastGrid.ColumnPosition[frmGoPhast.PhastGrid.ColumnCount];
        APoint.Y := CursorPoint.Y;
        zbFront.PaintBox.Canvas.LineTo(zbFront.XCoord(APoint.X), zbFront.YCoord(APoint.Y));
      end;
    end;
  finally
    zbFront.PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;

procedure TframeFront.DrawBlueSelectedCells(FirstCol, LastCol,
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
  Polygon[0].X := self.zbFront.XCoord(APoint.X);
  Polygon[0].Y := self.zbFront.YCoord(APoint.Z);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2 ,0, L1);
  Polygon[1].X := self.zbFront.XCoord(APoint.X);
  Polygon[1].Y := self.zbFront.YCoord(APoint.Z);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2, 0, L2);
  Polygon[2].X := self.zbFront.XCoord(APoint.X);
  Polygon[2].Y := self.zbFront.YCoord(APoint.Z);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L2);
  Polygon[3].X := self.zbFront.XCoord(APoint.X);
  Polygon[3].Y := self.zbFront.YCoord(APoint.Z);

  OldBrushStyle := zbFront.PaintBox.Canvas.Brush.Style;
  OldColor := zbFront.PaintBox.Canvas.Brush.Color;
  try
    zbFront.PaintBox.Canvas.Brush.Style := bsSolid;
    zbFront.PaintBox.Canvas.Brush.Color := clBlue;

    zbFront.PaintBox.Canvas.Polygon(Polygon);

  finally
    zbFront.PaintBox.Canvas.Brush.Style := OldBrushStyle;
    zbFront.PaintBox.Canvas.Brush.Color := OldColor;
  end;
end;

procedure TframeFront.zbFrontPaintBoxPaint(Sender: TObject);
var
  ShouldUpdate : boolean;
  PreviousCursor : TCursor;
begin
  if Drawing then Exit;
  Drawing := True;
  ShouldUpdate := frmGoPhast.FrontContoursChanged
    or frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors;
  frmGoPhast.FrontContoursChanged := False;
  try
    if (bmpFront <> nil) then
    begin
      // if you need to redraw the bitmap, change the cursor to an hourglass,
      // redraw, and change the cursor back.
      if frmGoPhast.FrontGridChanged
        or (bmpFront.Height <> zbFront.PaintBox.Height)
        or (bmpFront.Width <> zbFront.PaintBox.Width)
        or ShouldUpdate then
      begin
        PreviousCursor := zbFront.Cursor;
        try
          zbFront.Cursor := crHourGlass;
          DrawGridAndContours;
        finally
          zbFront.Cursor := PreviousCursor;
        end;
      end;

      if (frmGoPhast.CursorGrid = cgFront) then
      begin
        DrawSelectionRectangle;

        DrawMouseSelectionRectangle;

        ShowMovedPoints;

        if (frmGoPhast.tbMove.Down
          and (frmGoPhast.MovingColumn or frmGoPhast.MovingLayer)
          or frmGoPhast.tbAddColumn.Down or frmGoPhast.tbAddRow.Down) then
        begin
          ShowNewColumnOrLayer;
        end
        else if frmGoPhast.tbSubdivide.Down and frmGoPhast.Subdividing then
        begin
          DrawBlueSelectedCells(frmGoPhast.FirstSubdivideColumn,
            frmGoPhast.LastSubdivideColumn,
            frmGoPhast.FirstSubdivideLayer,
            frmGoPhast.LastSubdivideLayer);
        end
        else if frmGoPhast.tbSpacing.Down and frmGoPhast.SettingSpacing then
        begin
          DrawBlueSelectedCells(frmGoPhast.FirstSpacingColumn,
            frmGoPhast.LastSpacingColumn,
            frmGoPhast.FirstSpacingLayer,
            frmGoPhast.LastSpacingLayer);
        end
        else if CurrentContour <> nil then
        begin
          // draw what the current object would look like if the mouse
          // button were depressed now.
          DrawModifiedCurrentContour;
        end
        else if SelectLine <> nil then
        begin
          SelectLine.Draw(zbFront.PaintBox.Canvas);
        end;
      end;
    end;
  finally
    Drawing := False;
  end;
end;

constructor TframeFront.Create(AOwner: TComponent);
begin
  inherited;
  Panel1.ParentColor := True;
  bmpFront := TBitMap.Create;
  PreviousContours := TList.Create;
end;

destructor TframeFront.Destroy;
begin
  bmpFront.Free;
  PreviousContours.Free;
  inherited;
end;

procedure TframeFront.pnlWhiteMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  frmGoPhast.CursorGrid := cgNone;
end;

procedure TframeFront.ModelCubePaint(Sender: TObject);
begin
  ModelCube.Canvas.Rectangle(0,0,ModelCube.Width, ModelCube.Height);
end;

procedure TframeFront.RowChange(Sender: TObject);
begin
  with frmGoPhast.PhastGrid do
  begin
    ModelCube.Selection2 := (SelectedRow+1)/(RowCount);
    ModelCube.Selection1 := (SelectedRow)/(RowCount);
  end;
end;

procedure TframeFront.ChangeRow(Const X, Y: Integer);
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

procedure TframeFront.ModelCubeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  frmGoPhast.Timer1.Enabled := False;
  if frmGoPhast.Timer1.Interval = 1000 then
  begin
    ChangeRow(X,Y);
  end;
end;

procedure TframeFront.DisplayRow;
begin
  frmGoPhast.StatusBar1.Panels[1].Text := 'Selected Row: '
    + IntToStr(frmGoPhast.PhastGrid.SelectedRow+1);
end;

procedure TframeFront.ModelCubeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CubeX := X;
  CubeY := Y;
  DisplayRow;
end;

procedure TframeFront.StartRowTimer(Sender: TObject);
begin
  ChangeRow(CubeX,CubeY);
  frmGoPhast.Timer1.Interval := 100;
end;

procedure TframeFront.ModelCubeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CubeX := X;
  CubeY := Y;
  frmGoPhast.Timer1.Interval := 1000;
  frmGoPhast.Timer1.OnTimer := StartRowTimer;
  frmGoPhast.Timer1.Enabled := True;

end;

procedure TframeFront.BeginSetSpacing(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Layer, Col : integer;
begin
  // determine and store the first column and row that will be subdivided.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  GetColLayer(APoint, Col, Layer);
  if (Col >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.FirstSpacingColumn := Col;
    frmGoPhast.FirstSpacingRow := -1;
    frmGoPhast.FirstSpacingLayer := Layer;
    frmGoPhast.SettingSpacing := True;
  end;
end;

procedure TframeFront.GetColLayer(APoint: T2DRealPoint; out Col, Layer: integer);
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

procedure TframeFront.ContinueSubdivide(X, Y : integer);
var
  APoint : T2DRealPoint;
  Layer, Col : integer;
begin
  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  GetColLayer(APoint, Col, Layer);
  if (Col >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.LastSubdivideLayer := Layer;
    frmGoPhast.LastSubdivideColumn := Col;
    zbFront.PaintBox.Invalidate;
  end;
end;

procedure TframeFront.ContinueSetSpacing(X, Y : integer);
var
  APoint : T2DRealPoint;
  Layer, Col : integer;
begin
  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  GetColLayer(APoint, Col, Layer);
  if (Col >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.LastSpacingLayer := Layer;
    frmGoPhast.LastSpacingColumn := Col;
    zbFront.PaintBox.Invalidate;
  end;
end;

procedure TframeFront.SetSpacing(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Layer, Col : integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
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

procedure TframeFront.BeginMove(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  // start to move a column or layer.

  // First find the nearest column or layer boundaries.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
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
      zbFront.Cursor := crMoveColumn;
      zbFront.PaintBox.Cursor := crMoveColumn;
      frmGoPhast.MovingColumn := True;
      frmGoPhast.MovingLayer := False;
      frmGoPhast.ColumnBeingMoved := Column;
    end
    else if IsOnFrontLayer(X,Y) then
    begin
      // If the point is over a layer, set the cursor.
      // and set MovingLayer to true.
      // Store the row that is being moved.
      zbFront.Cursor := crMoveRow;
      zbFront.PaintBox.Cursor := crMoveRow;
      frmGoPhast.MovingLayer := True;
      frmGoPhast.MovingColumn := False;
      frmGoPhast.LayerBeingMoved := Layer;
    end;
  end;

end;

procedure TframeFront.BeginSubdivide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  // determine and store the first column and layer that will be subdivided.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  GetColLayer(APoint, Column, Layer);
  if (Column >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.FirstSubdivideColumn := Column;
    frmGoPhast.FirstSubdivideRow := -1;
    frmGoPhast.FirstSubdivideLayer := Layer;
    frmGoPhast.Subdividing := True;
  end;
end;

procedure TframeFront.SetACursor(X, Y: integer);
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
    zbFront.PaintBox.Invalidate;
    Exit;
  end;

  // get the location in real-world coordinates of the current cursor location
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);

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
        zbFront.Cursor := crDelete;
        zbFront.PaintBox.Cursor := crDelete;
      end
      else if IsOnFrontColumn(X,Y) then
      begin
        // Set the cursor for moving columns
        zbFront.Cursor := crMoveColumn;
        zbFront.PaintBox.Cursor := crMoveColumn;
      end
      else if IsOnFrontLayer(X,Y) then
      begin
        // Set the cursor for moving layers
        zbFront.Cursor := crMoveRow;
        zbFront.PaintBox.Cursor := crMoveRow;
      end;
    end
    else
    begin
      zbFront.Cursor := crArrow;
      zbFront.PaintBox.Cursor := crArrow;
    end;
  end
  else
  begin
    zbFront.Cursor := crArrow;
    zbFront.PaintBox.Cursor := crArrow;
  end;
end;

procedure TframeFront.DeleteColumnOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
  UndoDeleteColumn : TUndoDeleteColumn;
  UndoDeleteLayer : TUndoDeleteLayer;
begin
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
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

procedure TframeFront.MoveColumnOrLayer(X, Y: Integer);
begin
  if not (frmGoPhast.MovingColumn or frmGoPhast.MovingLayer) then
  begin
    Exit;
  end;
  if frmGoPhast.MovingColumn then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create
      (frmGoPhast.ColumnBeingMoved, zbFront.X(X)));
  end
  else if frmGoPhast.MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create
      (frmGoPhast.LayerBeingMoved, zbFront.Y(Y)));
  end;
  frmGoPhast.MovingColumn := False;
  frmGoPhast.MovingLayer := False;
end;

procedure TframeFront.AddColumnOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddColumn.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(zbFront.X(X)));
  end
  else if frmGoPhast.tbAddRow.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(zbFront.Y(Y)));
  end;
end;

procedure TframeFront.Subdivide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Column, Layer : integer;
begin
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  self.GetColLayer(APoint, Column, Layer);
  if (Column >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.Subdivide(Column, -1, Layer);
  end;
end;

procedure TframeFront.zbFrontBottomPaintBoxPaint(Sender: TObject);
var
  Index : integer;
  Contour : T2DContour;
begin
  // This allows for the current contour and previous contours to
  // be drawn with relatively little flickering until the zbFront.Image
  // has been updated.
  for Index := 0 to PreviousContours.Count -1 do
  begin
    Contour := PreviousContours[Index];
    Contour.Draw(zbFront.BottomPaintBox.Canvas);
  end;

  if (bmpFront <> nil) and (CurrentContour <> nil) then
  begin
    CurrentContour.Draw(zbFront.BottomPaintBox.Canvas);
  end;  
end;

procedure TframeFront.CreatePointContour(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // Get the real-world coordinates of the mouse.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  // create the contour.
  CurrentContour := T2DContour.Create(vdFront, ctPoint, CurrentUndo);
  frmGoPhast.Contours.Add(CurrentContour);
  // Add a point at the current mouse position.
  CurrentContour.AddPoint(APoint);
  DrawSelectRect := False;
    // Get the object properties from the user.
  FinishContours;
end;

procedure TframeFront.FinishContours;
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
          // in zbTop.BottomPaintbox while the grid is being
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
    frmGoPhast.FrontContoursChanged := True;
  end;
end;

procedure TframeFront.ContinueLineContour(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // If the contour hasn't been started yet, start it.
  if CurrentContour = nil then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      CurrentContour := T2DContour.Create(vdFront, ctLine, CurrentUndo);
    end
    else if frmGoPhast.tbPolygon.Down then
    begin
      CurrentContour := T2DContour.Create(vdFront, ctPolygon, CurrentUndo);
    end
    else Assert(False);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // Get the real-world coordinates of the mouse.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
  // Add a point at the current mouse position.
  CurrentContour.AddPoint(APoint);
  DrawSelectRect := False;
end;

procedure TframeFront.ContinueStraightLineContour(X, Y: Integer);
var
  APoint, PreviousPoint : T2DRealPoint;
  XPrime, YPrime : integer;
begin
  // if the straight line object hasn't been started yet, begin it.
  if CurrentContour = nil then
  begin
    CurrentContour := T2DContour.Create(vdFront, ctStraightLine, CurrentUndo);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // get the real-world coordinates of the mouse.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
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
    XPrime := X - zbFront.XCoord(PreviousPoint.X);//Sin(RotatedAngle)*R;
    YPrime := Y - zbFront.YCoord(PreviousPoint.Y);//Cos(RotatedAngle)*R;
    if Abs(XPrime) > Abs(YPrime) then
    begin
      APoint.X := zbFront.X(X);//Cos(frmGoPhast.PhastGrid.GridAngle)*XPrime;
      APoint.Y := PreviousPoint.Y ;//+ Sin(frmGoPhast.PhastGrid.GridAngle)*XPrime;
    end
    else
    begin
      APoint.X := PreviousPoint.X;// - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrime;
      APoint.Y := zbFront.Y(Y);//Cos(frmGoPhast.PhastGrid.GridAngle)*YPrime;
    end;
    CurrentContour.AddPoint(APoint);
  end;
  DrawSelectRect := False;
end;

procedure TframeFront.ContinueRectangle(X, Y: Integer);
var
  APoint, NewPoint, PreviousPoint : T2DRealPoint;
begin
  // if the box hasn't been started yet, begin it.
  if CurrentContour = nil then
  begin
    CurrentContour := T2DContour.Create(vdFront, ctBox, CurrentUndo);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // get the real-world coordinates of the mouse.
  APoint.X := zbFront.X(X);
  APoint.Y := zbFront.Y(Y);
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

    NewPoint.X := PreviousPoint.X ;//+ Cos(frmGoPhast.PhastGrid.GridAngle)*XPrime;
    NewPoint.Y := APoint.Y;// + Sin(frmGoPhast.PhastGrid.GridAngle)*XPrime;
    CurrentContour.AddPoint(NewPoint);
    CurrentContour.AddPoint(APoint);

    NewPoint.X := APoint.X;// - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrime;
    NewPoint.Y := PreviousPoint.Y;// + Cos(frmGoPhast.PhastGrid.GridAngle)*YPrime;
    CurrentContour.AddPoint(NewPoint);

    CurrentContour.AddPoint(CurrentContour.Points[0]);

    DrawSelectRect := False;
    // Get the object properties from the user.
    FinishContours;
  end;
end;


procedure TframeFront.DrawContours;
var
  ContourIndex : integer;
  AContour : T2DContour;
begin
//  if frmGoPhast.Contours.Count = 0 then Exit;
  if frmContourProperties <> nil then Exit;
  try
    bmpFront.Canvas.Brush.Color := clBlack;
    for ContourIndex := 0 to frmGoPhast.Contours.Count-1 do
    begin
      Application.ProcessMessages;
      if frmGoPhast.FrontContoursChanged then
      begin
        frmGoPhast.FrontContoursChanged := True;
        Exit;
      end;
      AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
      if (AContour.ViewDirection = vdFront) and (AContour <> CurrentContour) then
      begin
        AContour.Draw(bmpFront.Canvas);
      end;
    end;
    zbFront.Image.Picture.Assign(bmpFront);
    frmGoPhast.FrontGridChanged := False;

  except on EInvalidGraphicOperation do
    begin
      WarnTooBig;
    end;
  end;
  PreviousContours.Clear;
end;

procedure TframeFront.WarnTooBig;
begin
  // zoom out a bit.
  if (zbFront.PaintBox.Width > zbFront.HorzScrollBar.Width) or
    (zbFront.PaintBox.Height > zbFront.VertScrollBar.Height) then
  begin
    zbFront.ZoomBy(0.8);
    zbFront.PaintBox.Invalidate;
    Beep;
  end;
  MessageDlg('Sorry; the view was zoomed in too far.  '
      + 'I''ve had to zoom out a bit', mtInformation, [mbOK], 0);
end;

procedure TframeFront.DrawGridAndContours;
var
  Index : integer;
  AContour: T2DContour;
begin
  if  (frmContourProperties <> nil) then Exit;
  try
    bmpFront.Free;
    bmpFront := nil;
    bmpFront := TBitMap.Create;
    bmpFront.Height := zbFront.PaintBox.Height;
    bmpFront.Width := zbFront.PaintBox.Width;

    // If the colors of the grid cells are out of date,
    // recalculate them.
    if frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors then
    begin
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
    end;

    With bmpFront do
    begin
      With Canvas do
      begin
    // draw the grid.
        Brush.Color := clWhite;
        Pen.Color := clBlack;
        Rectangle(0,0,bmpFront.Width, bmpFront.Height);
        frmGoPhast.PhastGrid.Draw(bmpFront.Canvas, vdFront);

        Application.ProcessMessages;
        if frmGoPhast.FrontContoursChanged then
        begin
          frmGoPhast.FrontContoursChanged := True;
          Exit;
        end;

        // If there are no objects, finish up and quit.
        if frmGoPhast.Contours.Count = 0 then
        begin
          frmGoPhast.FrontGridChanged := False;
          zbFront.Image.Picture.Assign(bmpFront);
          Exit;
        end;

        // Draw the contours.
        InvalidateContours;
        DrawContours;
      end;
    end;
  except on EInvalidGraphicOperation do
    begin
      WarnTooBig;
    end;
  end;
end;

procedure TframeFront.InvalidateContours;
var
  ContourIndex : integer;
  AContour : T2DContour;
begin
  for ContourIndex := 0 to frmGoPhast.Contours.Count-1 do
  begin
    AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
    if AContour.ViewDirection = vdFront then
    begin
      AContour.Invalidate;
    end;
  end;
end;

procedure TframeFront.zbFrontPaintBoxDblClick(Sender: TObject);
begin
  DoubleClicked := True;
end;

procedure TframeFront.DrawMouseSelectionRectangle;
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
    zbFront.PaintBox.Canvas.Brush.Style := bsClear;
    zbFront.PaintBox.Canvas.Pen.Style := psDash;
    zbFront.PaintBox.Canvas.Pen.Width := 1;
    zbFront.PaintBox.Canvas.Pen.Color := clBlack;
    zbFront.PaintBox.Canvas.Rectangle(Rect(TL, BR));
  end;
end;

procedure TframeFront.DrawSelectionRectangle;
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
    zbFront.PaintBox.Canvas.Brush.Style := bsClear;
    zbFront.PaintBox.Canvas.Pen.Style := psDash;
    zbFront.PaintBox.Canvas.Pen.Width := 1;
    zbFront.PaintBox.Canvas.Pen.Color := clBlack;
    zbFront.PaintBox.Canvas.Rectangle(Rect(TL, BR));
  end;
end;

procedure TframeFront.ShowMovedPoints;
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
    zbFront.PaintBox.Canvas.Brush.Style := bsClear;
    zbFront.PaintBox.Canvas.Pen.Style := psDash;
    zbFront.PaintBox.Canvas.Pen.Width := 1;
    zbFront.PaintBox.Canvas.Pen.Color := clBlack;
    for PointIndex := 0 to SelectedPointContour.Count -1 do
    begin
      if SelectedPointContour.SelectedNodes[PointIndex] then
      begin
        if SelectedPointContour.Count = 1 then
        begin
          zbFront.PaintBox.Canvas.Rectangle(frmGoPhast.CursorX-3,
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
          zbFront.PaintBox.Canvas.PolyLine(PointArray);
        end;
      end;
    end;
  end;
end;

procedure TframeFront.UpdateSelectRectangle;
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
    if (AContour.ViewDirection = vdFront) and AContour.Selected then
    begin
      if First then
      begin
        First := False;
        SelectTopLeft.X := zbFront.XCoord(AContour.MaxX);
        SelectTopLeft.Y := zbFront.YCoord(AContour.MaxY);
        SelectBottomRight.X := zbFront.XCoord(AContour.MinX);
        SelectBottomRight.Y := zbFront.YCoord(AContour.MinY);
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
        TempL := zbFront.XCoord(AContour.MinX);
        TempH := zbFront.XCoord(AContour.MaxX);
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

        TempL := zbFront.YCoord(AContour.MinY);
        TempH := zbFront.YCoord(AContour.MaxY);
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
    zbFront.PaintBox.Invalidate;
  end;
end;

function TframeFront.SelectPoints(const X, Y: integer;
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

function TframeFront.SelectPointsOfAllSelectedContoursWithLine
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
        if (AContour.ViewDirection = vdFront) and AContour.Selected then
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
        frmGoPhast.FrontContoursChanged := True;
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

function TframeFront.SelectPointsOfASelectedContour(const X, Y: integer;
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

        if (AContour.ViewDirection = vdFront) and AContour.Selected then
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
        frmGoPhast.FrontContoursChanged := True;
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


function TframeFront.SelectContours(const X, Y: integer;
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
        if (AContour.ViewDirection = vdFront)then
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
        if (AContour.ViewDirection = vdFront) then
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
        frmGoPhast.FrontContoursChanged := True;
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

function TframeFront.SelectPointsWithLine (const AContour : T2DContour;
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

function TframeFront.AreContoursSelected: boolean;
var
  Index : integer;
  AContour : T2DContour;
begin
  result := False;
  for Index := frmGoPhast.Contours.Count -1 downto 0 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected and (AContour.ViewDirection = vdFront) then
    begin
      result := True;
      Exit;
    end;
  end;

end;

procedure TframeFront.SetInsertPointCursor(X, Y: Integer);
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
    if AContour.Selected and (AContour.ViewDirection = vdFront) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      If Edge >= 0 then break;
    end
  end;
  // set the Cursor based on whether or not the mouse is over an
  // edge of a selected object.
  if Edge >= 0 then
  begin
    zbFront.Cursor := crInsertPoint;
    zbFront.PaintBox.Cursor := crInsertPoint;
  end
  else
  begin
    zbFront.Cursor := crDisabledInsertPoint;
    zbFront.PaintBox.Cursor := crDisabledInsertPoint;
  end;
end;

procedure TframeFront.SetDeleteSegmentCursor(X, Y: Integer);
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
    if (AContour.ViewDirection = vdFront) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      If Edge >= 0 then break;
    end
  end;
  // set the Cursor based on whether or not the mouse is over an
  // edge of an object.
  if Edge >= 0 then
  begin
    zbFront.Cursor := crDeleteSegment;
    zbFront.PaintBox.Cursor := crDeleteSegment;
  end
  else
  begin
    zbFront.Cursor := crDisabledDeleteSegment;
    zbFront.PaintBox.Cursor := crDisabledDeleteSegment;
  end;

end;

procedure TframeFront.MoveContours(const X,Y: integer);
var
  XOffset, YOffset : double;
  UndoMoveContour : TUndoMoveContour;
begin
  // move the contours a distance specified by where the mouse was clicked
  // down and where it was clicked up.
  XOffset := zbFront.X(X)- zbFront.X(StartX);
  YOffset := zbFront.Y(Y)- zbFront.Y(StartY);
  if (XOffset <> 0) or (YOffset <> 0) then
  begin
    // if the cursor has moved, move the selected contours.
    UndoMoveContour := TUndoMoveContour.Create(XOffset, YOffset, vdFront);
    frmGoPhast.UndoStack.Submit(UndoMoveContour);
    StartX := X;
    StartY := Y;
  end;
end;

function TframeFront.SelectContoursWithLine(const ToggleSelection : boolean) : boolean;
var
  ContourIndex: integer;
  AContour : T2DContour;
  APoint : TPoint;
  Update : boolean;
  UndoChangeSelection : TUndoChangeSelection;
  function PointInside: boolean;
{  var
    PointIndex : integer;}
  begin
    // PointInside  returns True if the first point in
    // AContour is inside SelectLine.
    result := false;
    if AContour.Count > 0 then
    begin
{      for PointIndex := 0 to AContour.Count -1 do
      begin
        APoint := AContour.CanvasCoordinates[PointIndex];}
        APoint := AContour.CanvasCoordinates[0];
        if SelectLine.Inside(APoint) then
        begin
          result := True;
//          Exit;
        end;
//      end;
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
        if (AContour.ViewDirection = vdFront) and not AContour.Deleted then
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
        frmGoPhast.FrontContoursChanged := True;
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

procedure TframeFront.InsertPoint(const X, Y: integer);
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
    if AContour.Selected and (AContour.ViewDirection = vdFront) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        APoint.X := zbFront.X(X);
        APoint.Y := zbFront.Y(Y);
        UndoInsertPoint := TUndoInsertPoint.Create(AContour, Edge+1, APoint);
        frmGoPhast.UndoStack.Submit(UndoInsertPoint);
        UndoInsertPoint.SetPostSelection;
        Exit;
      end;
    end;
  end;

end;

procedure TframeFront.DeleteSegment(const X, Y: integer);
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
    if AContour.Selected and (AContour.ViewDirection = vdFront) then
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

procedure TframeFront.DrawModifiedCurrentContour;
var
  APoint : T2DRealPoint;
  PreviousPoint {, NewPoint} : T2DRealPoint;
//  XR, YR : double;
//  Angle, RotatedAngle, R : double;
  XPrime, YPrime : integer;
begin
  // draw what the current object would look like if the mouse
  // button were depressed now.
  zbFront.PaintBox.Canvas.Pen.Style := psDash;
  try
    if frmGoPhast.tbPolygon.Down then
    begin
      if CurrentContour.Count > 0 then
      begin
        zbFront.PaintBox.Canvas.MoveTo(CurrentContour.CanvasCoordinates[0].X,
          CurrentContour.CanvasCoordinates[0].Y);
        zbFront.PaintBox.Canvas.LineTo(frmGoPhast.CursorX,
          frmGoPhast.CursorY);
        zbFront.PaintBox.Canvas.LineTo(CurrentContour.CanvasCoordinates[CurrentContour.Count-1].X,
          CurrentContour.CanvasCoordinates[CurrentContour.Count-1].Y);
      end;
    end
    else if frmGoPhast.tbLine.Down then
    begin
      if CurrentContour.Count > 0 then
      begin
        zbFront.PaintBox.Canvas.MoveTo(CurrentContour.CanvasCoordinates[CurrentContour.Count-1].X,
          CurrentContour.CanvasCoordinates[CurrentContour.Count-1].Y);
        zbFront.PaintBox.Canvas.LineTo(frmGoPhast.CursorX,
          frmGoPhast.CursorY);
      end;
    end
    else if frmGoPhast.tbStraightLine.Down then
    begin
      APoint.X := zbFront.X(frmGoPhast.CursorX);
      APoint.Y := zbFront.Y(frmGoPhast.CursorY);
      PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
      XPrime := frmGoPhast.CursorX - zbFront.XCoord(PreviousPoint.X);//Sin(RotatedAngle)*R;
      YPrime := frmGoPhast.CursorY - zbFront.YCoord(PreviousPoint.Y);//Cos(RotatedAngle)*R;
      if Abs(XPrime) > Abs(YPrime) then
      begin
        APoint.X := zbFront.X(frmGoPhast.CursorX);//Cos(frmGoPhast.PhastGrid.GridAngle)*XPrime;
        APoint.Y := PreviousPoint.Y ;//+ Sin(frmGoPhast.PhastGrid.GridAngle)*XPrime;
      end
      else
      begin
        APoint.X := PreviousPoint.X;// - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrime;
        APoint.Y := zbFront.Y(frmGoPhast.CursorY);//Cos(frmGoPhast.PhastGrid.GridAngle)*YPrime;
      end;
      zbFront.PaintBox.Canvas.MoveTo(zbFront.XCoord(PreviousPoint.X), zbFront.YCoord(PreviousPoint.Y));
      zbFront.PaintBox.Canvas.LineTo(zbFront.XCoord(APoint.X), zbFront.YCoord(APoint.Y));
    end
    else if frmGoPhast.tbRectangle.Down then
    begin
      PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
      zbFront.PaintBox.Canvas.MoveTo(zbFront.XCoord(PreviousPoint.X), zbFront.YCoord(PreviousPoint.Y));
      zbFront.PaintBox.Canvas.LineTo(frmGoPhast.CursorX, zbFront.YCoord(PreviousPoint.Y));
      zbFront.PaintBox.Canvas.LineTo(frmGoPhast.CursorX, frmGoPhast.CursorY);
      zbFront.PaintBox.Canvas.LineTo(zbFront.XCoord(PreviousPoint.X), frmGoPhast.CursorY);
      zbFront.PaintBox.Canvas.LineTo(zbFront.XCoord(PreviousPoint.X), zbFront.YCoord(PreviousPoint.Y));

    end;
  finally
    zbFront.PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;


end.

