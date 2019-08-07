unit frameSideUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QZoomBox, QExtCtrls, QRbwRuler, QStdCtrls, QRbwModelCube, AbstractGridUnit,
  ContourUnit, SelectUnit, UndoItems, QCustomRbwRuler;

type
  TframeSide = class(TFrame)
    rulerSideHoriz: TRbwRuler;
    zbSide: TQrbwZoomBox;
    Panel1: TPanel;
    ModelCube: TRbwModelCube;
    rulerSideVertical: TRbwRuler;
    procedure zbSideHorzScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbSideVertScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbSidePaintBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure zbSidePaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure zbSidePaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure zbSidePaintBoxPaint(Sender: TObject);
    procedure rulerSideVerticalMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ModelCubePaint(Sender: TObject);
    procedure ModelCubeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ModelCubeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ModelCubeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure zbSidePaintBoxDblClick(Sender: TObject);
    procedure zbSideBottomPaintBoxPaint(Sender: TObject);
  private
    bmpSide : TBitmap;
    CubeX, CubeY : integer;
    MouseButtonIsDown : boolean;
    StartX, StartY: integer;
    MovingContours : boolean;
    SelectLine: TLine;
    SelectedPointContour : T2DContour;
    SelectTopLeft : TPoint;
    SelectBottomRight : TPoint;
    PriorCursorX, PriorCursorY : integer;
    DoubleClicked : boolean;
    CurrentUndo : TCustomUndo;
    PreviousContours : TList;
    Drawing : boolean;
    procedure BeginMove(X, Y: Integer);
    procedure BeginSubdivide(X, Y: Integer);
    function IsOnSideLayer(const X, Y: integer): boolean;
    function IsOnSideRow(const X, Y: integer): boolean;
    procedure StartColumnTimer(Sender: TObject);
    procedure BeginSetSpacing(X, Y: Integer);
    procedure ChangeColumn(const X, Y: Integer);
    procedure ContinueSetSpacing(X, Y: integer);
    procedure ContinueSubdivide(X, Y: integer);
    procedure CreatePointContour(X, Y: Integer);
    procedure GetRowLayer(APoint: T2DRealPoint; out Row, Layer: integer);
    procedure SetSpacing(X, Y: Integer);
    function SelectContours(const X, Y: integer;
      const SelectLowerContours, ToggleSelectedItem: boolean;
      const CanChangeSelection: boolean = True): boolean;
    procedure SetACursor(X, Y: integer);
    function AreContoursSelected: boolean;
    function SelectPointsOfASelectedContour(const X, Y: integer;
      const AddToSelection: boolean): boolean;
    function SelectPoints(const X, Y: integer; const AContour: T2DContour;
      const AddToSelection: boolean; var Changed: boolean): boolean;
    procedure SetInsertPointCursor(X, Y: Integer);
    procedure SetDeleteSegmentCursor(X, Y: Integer);
    procedure DeleteRowOrLayer(X, Y: Integer);
    procedure MoveRowOrLayer(X, Y: Integer);
    procedure AddRowOrLayer(X, Y: Integer);
    procedure Subdivide(X, Y: Integer);
    procedure ContinueLineContour(X, Y: Integer);
    procedure ContinueStraightLineContour(X, Y: Integer);
    procedure ContinueRectangle(X, Y: Integer);
    procedure MoveContours(const X, Y: integer);
    function SelectContoursWithLine(
      const ToggleSelection: boolean): boolean;
    function SelectPointsOfAllSelectedContoursWithLine(
      const AddToSelection: boolean): boolean;
    procedure InsertPoint(const X, Y: integer);
    procedure DeleteSegment(const X, Y: integer);
    function SelectPointsWithLine(const AContour: T2DContour;
      const AddToSelection: boolean; out Changed: boolean): boolean;
    procedure DrawGridAndContours;
    procedure DrawContours;
    procedure InvalidateContours;
    procedure WarnTooBig;
    procedure DrawSelectionRectangle;
    procedure DrawMouseSelectionRectangle;
    procedure ShowMovedPoints;
    procedure ShowNewRowOrLayer;
    procedure DrawBlueSelectedCells(FirstRow, LastRow, FirstLayer,
      LastLayer: integer);
    procedure DrawModifiedCurrentContour;
    { Private declarations }
  public
    CurrentContour : T2DContour;
    DrawSelectRect : boolean;
    ViewDirection : TViewDirection;
    procedure AdjustHorizontalScale;
    procedure AdjustVerticalScale;
    procedure ColumnChange(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure DisplayColumn;
    procedure FinishContours;
    procedure UpdateSelectRectangle;
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, CursorsFoiledAgain, frmContourPropertiesUnit,
  UndoItemsContours, frameTopUnit;

{$R *.dfm}

{ TframeSide }

procedure TframeSide.AdjustHorizontalScale;
begin
  // set the ends of the horizontal ruler so that the scale
  // is displayed properly.
  rulerSideVertical.RulerEnds.Lower := 10;

  // This accounts for the width of the vertical scroll bar (width = 18)
  // plus the space between the end of the ruler at the end
  // of the empty space in ZoomBox ( = 12)
  // The value of 30 comes from 18+12
  rulerSideVertical.RulerEnds.Upper := rulerSideVertical.Width -30;
  rulerSideVertical.RulerValues.Upper := zbSide.X(10 + zbSide.HorzScrollBar.Position);
  rulerSideVertical.RulerValues.Lower := zbSide.X(zbSide.HorzScrollBar.Width -12 + zbSide.HorzScrollBar.Position);
end;

procedure TframeSide.AdjustVerticalScale;
var
  NewLower, NewUpper : double;
begin
  rulerSideHoriz.RulerEnds.Upper := Height - rulerSideVertical.Height -30;
  NewLower := zbSide.Y(zbSide.VertScrollBar.Height -10 + zbSide.VertScrollBar.Position);
  NewUpper := zbSide.Y(12 + zbSide.VertScrollBar.Position);
  if NewLower < NewUpper then
  begin
    rulerSideHoriz.RulerValues.Lower := NewLower;
    rulerSideHoriz.RulerValues.Upper := NewUpper;
  end;
end;

procedure TframeSide.zbSideHorzScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScale;
end;

procedure TframeSide.zbSideVertScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustVerticalScale
end;

procedure TframeSide.zbSidePaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssRight in Shift then
  begin
    // for a right click, don't create objects
    Exit;
  end;

  MouseButtonIsDown := True;
  frmGoPhast.CursorGrid := cgSide;
  // Record the current cursor position.
  StartX := X;
  StartY := Y;
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  if frmGoPhast.tbZoom.Down then
  begin
    if zbSide.CanZoomIn then
    begin
      zbSide.BeginZoom(X, Y);
    end;
  end
  else if frmGoPhast.tbPan.Down then
  begin
    if zbSide.CanPan then
    begin
      zbSide.Panning := True;
      zbSide.Cursor := crHandGrab;
      zbSide.PaintBox.Cursor := crHandGrab;
      Screen.Cursor := crHandGrab;
    end;
  end
  else if frmGoPhast.tbMove.Down then
  begin
    BeginMove(X,Y);
  end
  else if frmGoPhast.tbSubdivide.Down then
  begin
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
      zbSide.PaintBox.Invalidate;
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
    zbSide.PaintBox.Invalidate;
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

function TframeSide.IsOnSideLayer(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Layer : integer;
begin
  result := false;
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if (Abs(zbSide.XCoord(APoint.X) - X) <= 5)
      and (Abs(zbSide.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

function TframeSide.IsOnSideRow(Const X, Y: integer): boolean;
var
  APoint : T2DRealPoint;
  Row : integer;
begin
  result := false;
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  Row := frmGoPhast.PhastGrid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.PhastGrid.RowCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.RowPosition[Row];
    if (Abs(zbSide.XCoord(APoint.X) - X) <= 5)
      and (Abs(zbSide.YCoord(APoint.Y) - Y) <= 5) then
    begin
      result := true;
    end;
  end;
end;

procedure TframeSide.zbSidePaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  frmGoPhast.CursorGrid := cgSide;
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  frmGoPhast.StatusBar1.Panels[0].Text :=
    'Y: ' + FloatToStrF(zbSide.Y(Y), ffGeneral, 7,1)
    + ', Z: ' + FloatToStrF(zbSide.X(X), ffGeneral, 7,1);

  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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

  if frmGoPhast.tbDeleteColumnRow.Down
    or frmGoPhast.tbMove.Down
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
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
    zbSide.Cursor := crArrow;
    zbSide.PaintBox.Cursor := crArrow;
    DrawSelectRect := False;
    // redraw to indicate where the contour would be if you clicked now.
    if CurrentContour <> nil then
    begin
      zbSide.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbPoint.Down then
  begin
    // set the correct cursor
    zbSide.Cursor := crArrow;
    zbSide.PaintBox.Cursor := crArrow;
  end
  else if frmGoPhast.tbLasso.Down and (SelectLine <> nil) then
  begin
    // set the correct cursor
    zbSide.Cursor := crArrow;
    zbSide.PaintBox.Cursor := crArrow;
    // If the cursor has moved far enough, add another point to the
    // lasso.
    if (SelectLine.Count = 0)
      or (Abs(SelectLine.Points[SelectLine.Count-1].X-X) > 10)
      or (Abs(SelectLine.Points[SelectLine.Count-1].Y-Y) > 10) then
    begin
      SelectLine.AddPoint(Point(X,Y));
      zbSide.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbSelect.Down then
  begin
    // set the correct cursor.
    zbSide.Cursor := crArrow;
    zbSide.PaintBox.Cursor := crArrow;
    // if something is happening, redraw.
    if (DrawSelectRect and (ssLeft in Shift)) or not MovingContours then
    begin
      zbSide.PaintBox.Invalidate;
    end;
  end
  else if frmGoPhast.tbSelectPoint.Down then
  begin
    // if something is happening, redraw.
    if (SelectLine <> nil) and not MovingContours then
    begin
      zbSide.PaintBox.Invalidate;
    end
    else if (SelectedPointContour <> nil)
      and SelectedPointContour.Selected then
    begin
      zbSide.PaintBox.Invalidate;
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

procedure TframeSide.zbSidePaintBoxMouseUp(Sender: TObject;
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

  frmGoPhast.CursorGrid := cgSide;
  frmGoPhast.CursorX := X;
  frmGoPhast.CursorY := Y;
  if frmGoPhast.tbZoomOut.Down then
  begin
    zbSide.ZoomByAt(0.5, X, Y);
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(zbSide.Magnification);
  end
  else if frmGoPhast.tbPan.Down then
  begin
    if zbSide.CanPan then
    begin
      zbSide.Panning := False;
      zbSide.Cursor := crHandFlat;
      zbSide.PaintBox.Cursor := crHandFlat;
      Screen.Cursor := crDefault;
    end;
  end
  else if frmGoPhast.tbZoom.Down then
  begin
    if not zbSide.CanZoomIn then
    begin
      zbSide.Cursor := crArrow;
      zbSide.PaintBox.Cursor := crArrow;
    end;
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(zbSide.Magnification);
  end
  else if frmGoPhast.tbZoomIn.Down then
  begin
    zbSide.ZoomByAt(2, X, Y);
    if not zbSide.CanZoomIn then
    begin
      zbSide.Cursor := crArrow;
      zbSide.PaintBox.Cursor := crArrow;
    end;
    frmGoPhast.StatusBar1.Panels[0].Text :=
      'Magnification = ' + FloatToStr(zbSide.Magnification);
  end
  else if frmGoPhast.tbDeleteColumnRow.Down then
  begin
    DeleteRowOrLayer(X,Y);
  end
  else if frmGoPhast.tbMove.Down then
  begin
    MoveRowOrLayer(X, Y);
  end
  else if frmGoPhast.tbAddColumn.Down or frmGoPhast.tbAddRow.Down then
  begin
    AddRowOrLayer(X,Y);
  end
  else if frmGoPhast.tbSubdivide.Down then
  begin
    Subdivide(X, Y);
  end
  else if frmGoPhast.tbSpacing.Down then
  begin
    // Subdivide the selected cells.
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
        zbSide.PaintBox.Invalidate;
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
    zbSide.PaintBox.Invalidate;
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
    zbSide.PaintBox.Invalidate;
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

procedure TframeSide.zbSidePaintBoxPaint(Sender: TObject);
var
  ShouldUpdate : boolean;
  PreviousCursor : TCursor;
begin
  if Drawing then Exit;
  Drawing := True;
  ShouldUpdate := frmGoPhast.SideContoursChanged
    or frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors;
  frmGoPhast.SideContoursChanged := False;
  try
    if bmpSide <> nil then
    begin
      if frmGoPhast.SideGridChanged
        or (bmpSide.Height <> zbSide.PaintBox.Height)
        or (bmpSide.Width <> zbSide.PaintBox.Width)
        or ShouldUpdate then
      begin
        PreviousCursor := zbSide.Cursor;
        try
          zbSide.Cursor := crHourGlass;
          DrawGridAndContours;
        finally
          zbSide.Cursor := PreviousCursor;
        end;
      end;

      if (frmGoPhast.CursorGrid = cgSide) then
      begin
        DrawSelectionRectangle;

        DrawMouseSelectionRectangle;

        ShowMovedPoints;


        if (frmGoPhast.tbMove.Down
          and (frmGoPhast.MovingRow or frmGoPhast.MovingLayer)
          or frmGoPhast.tbAddColumn.Down or frmGoPhast.tbAddRow.Down) then
        begin
          ShowNewRowOrLayer;
        end
        else if frmGoPhast.tbSubdivide.Down and frmGoPhast.Subdividing then
        begin
          DrawBlueSelectedCells(frmGoPhast.FirstSubdivideRow,
            frmGoPhast.LastSubdivideRow,
            frmGoPhast.FirstSubdivideLayer,
            frmGoPhast.LastSubdivideLayer);
        end
        else if frmGoPhast.tbSpacing.Down and frmGoPhast.SettingSpacing then
        begin
          DrawBlueSelectedCells(frmGoPhast.FirstSpacingRow,
            frmGoPhast.LastSpacingRow,
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
          SelectLine.Draw(zbSide.PaintBox.Canvas);
        end;

      end;
    end;
  finally
    Drawing := False;
  end;
end;

constructor TframeSide.Create(AOwner: TComponent);
begin
  inherited;
  PreviousContours := TList.Create;
  Panel1.ParentColor := True;
  bmpSide := TBitmap.Create;
end;

destructor TframeSide.Destroy;
begin
  PreviousContours.Free;
  bmpSide.Free;
  inherited;
end;

procedure TframeSide.rulerSideVerticalMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  frmGoPhast.CursorGrid := cgNone;

end;

procedure TframeSide.ModelCubePaint(Sender: TObject);
begin
  ModelCube.Canvas.Rectangle(0,0,ModelCube.Width, ModelCube.Height);
end;

procedure TframeSide.ColumnChange(Sender: TObject);
begin
  with frmGoPhast.PhastGrid do
  begin
    ModelCube.Selection2 := (SelectedColumn+1)/(ColumnCount);
    ModelCube.Selection1 := (SelectedColumn)/(ColumnCount);
  end;
end;

procedure TframeSide.ChangeColumn(Const X, Y: Integer);
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

procedure TframeSide.ModelCubeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  frmGoPhast.Timer1.Enabled := False;
  if frmGoPhast.Timer1.Interval = 1000 then
  begin
    ChangeColumn(X,Y);
  end;
end;

procedure TframeSide.DisplayColumn;
begin
  frmGoPhast.StatusBar1.Panels[1].Text := 'Selected Col: '
    + IntToStr(frmGoPhast.PhastGrid.SelectedColumn+1);
end;

procedure TframeSide.ModelCubeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CubeX := X;
  CubeY := Y;
  DisplayColumn
end;

procedure TframeSide.ModelCubeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CubeX := X;
  CubeY := Y;
  frmGoPhast.Timer1.Interval := 1000;
  frmGoPhast.Timer1.OnTimer := StartColumnTimer;
  frmGoPhast.Timer1.Enabled := True;
end;

procedure TframeSide.StartColumnTimer(Sender: TObject);
begin
  ChangeColumn(CubeX,CubeY);
  frmGoPhast.Timer1.Interval := 100;
end;

procedure TframeSide.BeginSetSpacing(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Layer, Row : integer;
begin
  // determine and store the first column and row that will be subdivided.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.FirstSpacingColumn := -1;
    frmGoPhast.FirstSpacingRow := Row;
    frmGoPhast.FirstSpacingLayer := Layer;
    frmGoPhast.SettingSpacing := True;
  end;
end;

procedure TframeSide.GetRowLayer(APoint: T2DRealPoint; out Row, Layer: integer);
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

procedure TframeSide.ContinueSubdivide(X, Y : integer);
var
  APoint : T2DRealPoint;
  Layer, Row : integer;
begin
  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.LastSubdivideLayer := Layer;
    frmGoPhast.LastSubdivideRow := Row;
    zbSide.PaintBox.Invalidate;
  end;
end;

procedure TframeSide.ContinueSetSpacing(X, Y : integer);
var
  APoint : T2DRealPoint;
  Layer, Row : integer;
begin
  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.LastSpacingLayer := Layer;
    frmGoPhast.LastSpacingRow := Row;
    zbSide.PaintBox.Invalidate;
  end;
end;

procedure TframeSide.SetSpacing(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Layer, Row : integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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
procedure TframeSide.BeginMove(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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
      zbSide.Cursor := crMoveColumn;
      frmGoPhast.MovingLayer := True;
      frmGoPhast.MovingRow := False;
      frmGoPhast.LayerBeingMoved := Layer;
    end
    else if IsOnSideRow(X,Y) then
    begin
      zbSide.Cursor := crMoveRow;
      frmGoPhast.MovingRow := True;
      frmGoPhast.MovingLayer := False;
      frmGoPhast.RowBeingMoved := Row;
    end;
  end;
end;

procedure TframeSide.BeginSubdivide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.FirstSubdivideColumn := -1;
    frmGoPhast.FirstSubdivideRow := Row;
    frmGoPhast.FirstSubdivideLayer := Layer;
    frmGoPhast.Subdividing := True;
  end;
end;

function TframeSide.SelectContours(const X, Y: integer;
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
        if (AContour.ViewDirection = vdside)then
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
        if (AContour.ViewDirection = vdSide) then
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
        frmGoPhast.SideContoursChanged := True;
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

function TframeSide.AreContoursSelected: boolean;
var
  Index : integer;
  AContour : T2DContour;
begin
  result := False;
  for Index := frmGoPhast.Contours.Count -1 downto 0 do
  begin
    AContour := frmGoPhast.Contours[Index] as T2DContour;
    if AContour.Selected and (AContour.ViewDirection = vdSide) then
    begin
      result := True;
      Exit;
    end;
  end;

end;

function TframeSide.SelectPointsOfASelectedContour(const X, Y: integer;
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

        if (AContour.ViewDirection = vdSide) and AContour.Selected then
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
        frmGoPhast.SideContoursChanged := True;
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

procedure TframeSide.UpdateSelectRectangle;
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
    if (AContour.ViewDirection = vdSide) and AContour.Selected then
    begin
      if First then
      begin
        First := False;
        SelectTopLeft.X := zbSide.XCoord(AContour.MaxX);
        SelectTopLeft.Y := zbSide.YCoord(AContour.MaxY);
        SelectBottomRight.X := zbSide.XCoord(AContour.MinX);
        SelectBottomRight.Y := zbSide.YCoord(AContour.MinY);
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
        TempL := zbSide.XCoord(AContour.MinX);
        TempH := zbSide.XCoord(AContour.MaxX);
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

        TempL := zbSide.YCoord(AContour.MinY);
        TempH := zbSide.YCoord(AContour.MaxY);
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
    zbSide.PaintBox.Invalidate;
  end;
end;

function TframeSide.SelectPoints(const X, Y: integer;
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


procedure TframeSide.SetACursor(X, Y: integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  if frmGoPhast.MovingRow
    or frmGoPhast.MovingLayer
    or frmGoPhast.tbAddColumn.Down
    or frmGoPhast.tbAddRow.Down then
  begin
    zbSide.PaintBox.Invalidate;
    Exit;
  end;

  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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
        zbSide.Cursor := crDelete;
      end
      else if IsOnSideLayer(X,Y) then
      begin
        zbSide.Cursor := crMoveColumn;
      end
      else if IsOnSideRow(X,Y) then
      begin
        zbSide.Cursor := crMoveRow;
      end;
    end
    else
    begin
      zbSide.Cursor := crArrow;
    end;
  end
  else
  begin
    zbSide.Cursor := crArrow;
  end;
end;

procedure TframeSide.SetInsertPointCursor(X, Y: Integer);
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
    if AContour.Selected and (AContour.ViewDirection = vdSide) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      If Edge >= 0 then break;
    end
  end;
  // set the Cursor based on whether or not the mouse is over an
  // edge of a selected object.
  if Edge >= 0 then
  begin
    zbSide.Cursor := crInsertPoint;
    zbSide.PaintBox.Cursor := crInsertPoint;
  end
  else
  begin
    zbSide.Cursor := crDisabledInsertPoint;
    zbSide.PaintBox.Cursor := crDisabledInsertPoint;
  end;
end;

procedure TframeSide.SetDeleteSegmentCursor(X, Y: Integer);
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
    if (AContour.ViewDirection = vdSide) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      If Edge >= 0 then break;
    end
  end;
  // set the Cursor based on whether or not the mouse is over an
  // edge of an object.
  if Edge >= 0 then
  begin
    zbSide.Cursor := crDeleteSegment;
    zbSide.PaintBox.Cursor := crDeleteSegment;
  end
  else
  begin
    zbSide.Cursor := crDisabledDeleteSegment;
    zbSide.PaintBox.Cursor := crDisabledDeleteSegment;
  end;

end;

procedure TframeSide.DeleteRowOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
  UndoDeleteRow : TUndoDeleteRow;
  UndoDeleteLayer : TUndoDeleteLayer;
begin
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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

procedure TframeSide.MoveRowOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  if not (frmGoPhast.MovingRow or frmGoPhast.MovingLayer) then
  begin
    Exit;
  end;
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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

procedure TframeSide.AddRowOrLayer(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  if frmGoPhast.tbAddColumn.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(APoint.X));
  end
  else if frmGoPhast.tbAddRow.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(APoint.Y));
  end;
end;

procedure TframeSide.Subdivide(X, Y: Integer);
var
  APoint : T2DRealPoint;
  Row, Layer : integer;
begin
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    frmGoPhast.Subdivide(-1, Row, Layer);
  end;
end;

procedure TframeSide.CreatePointContour(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // Get the real-world coordinates of the mouse.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  // create the contour.
  CurrentContour := T2DContour.Create(vdSide, ctPoint, CurrentUndo);
  frmGoPhast.Contours.Add(CurrentContour);
  // Add a point at the current mouse position.
  CurrentContour.AddPoint(APoint);
  DrawSelectRect := False;
    // Get the object properties from the user.
  FinishContours;
end;

procedure TframeSide.ContinueLineContour(X, Y: Integer);
var
  APoint : T2DRealPoint;
begin
  // If the contour hasn't been started yet, start it.
  if CurrentContour = nil then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      CurrentContour := T2DContour.Create(vdSide, ctLine, CurrentUndo);
    end
    else if frmGoPhast.tbPolygon.Down then
    begin
      CurrentContour := T2DContour.Create(vdSide, ctPolygon, CurrentUndo);
    end
    else Assert(False);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // Get the real-world coordinates of the mouse.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
  // Add a point at the current mouse position.
  CurrentContour.AddPoint(APoint);
  DrawSelectRect := False;
end;

procedure TframeSide.ContinueStraightLineContour(X, Y: Integer);
var
  APoint, PreviousPoint : T2DRealPoint;
  XPrime, YPrime : integer;
begin
  // if the straight line object hasn't been started yet, begin it.
  if CurrentContour = nil then
  begin
    CurrentContour := T2DContour.Create(vdSide, ctStraightLine, CurrentUndo);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // get the real-world coordinates of the mouse.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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
    XPrime := X - zbSide.XCoord(PreviousPoint.X);//Sin(RotatedAngle)*R;
    YPrime := Y - zbSide.YCoord(PreviousPoint.Y);//Cos(RotatedAngle)*R;
    if Abs(XPrime) > Abs(YPrime) then
    begin
      APoint.X := zbSide.X(X);//Cos(frmGoPhast.PhastGrid.GridAngle)*XPrime;
      APoint.Y := PreviousPoint.Y ;//+ Sin(frmGoPhast.PhastGrid.GridAngle)*XPrime;
    end
    else
    begin
      APoint.X := PreviousPoint.X;// - Sin(frmGoPhast.PhastGrid.GridAngle)*YPrime;
      APoint.Y := zbSide.Y(Y);//Cos(frmGoPhast.PhastGrid.GridAngle)*YPrime;
    end;
    CurrentContour.AddPoint(APoint);
  end;
  DrawSelectRect := False;
end;

procedure TframeSide.ContinueRectangle(X, Y: Integer);
var
  APoint, NewPoint, PreviousPoint : T2DRealPoint;
begin
  // if the box hasn't been started yet, begin it.
  if CurrentContour = nil then
  begin
    CurrentContour := T2DContour.Create(vdSide, ctBox, CurrentUndo);
    frmGoPhast.Contours.Add(CurrentContour);
  end;
  // get the real-world coordinates of the mouse.
  APoint.X := zbSide.X(X);
  APoint.Y := zbSide.Y(Y);
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

procedure TframeSide.MoveContours(const X,Y: integer);
var
  XOffset, YOffset : double;
  UndoMoveContour : TUndoMoveContour;
begin
  // move the contours a distance specified by where the mouse was clicked
  // down and where it was clicked up.
  XOffset := zbSide.X(X)- zbSide.X(StartX);
  YOffset := zbSide.Y(Y)- zbSide.Y(StartY);
  if (XOffset <> 0) or (YOffset <> 0) then
  begin
    // if the cursor has moved, move the selected contours.
    UndoMoveContour := TUndoMoveContour.Create(XOffset, YOffset, vdSide);
    frmGoPhast.UndoStack.Submit(UndoMoveContour);
    StartX := X;
    StartY := Y;
  end;
end;

function TframeSide.SelectContoursWithLine(const ToggleSelection : boolean) : boolean;
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
        if (AContour.ViewDirection = vdSide) and not AContour.Deleted then
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
        frmGoPhast.SideContoursChanged := True;
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

function TframeSide.SelectPointsOfAllSelectedContoursWithLine
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
        if (AContour.ViewDirection = vdSide) and AContour.Selected then
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
        frmGoPhast.SideContoursChanged := True;
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

procedure TframeSide.InsertPoint(const X, Y: integer);
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
    if AContour.Selected and (AContour.ViewDirection = vdSide) then
    begin
      Edge := AContour.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        APoint.X := zbSide.X(X);
        APoint.Y := zbSide.Y(Y);
        UndoInsertPoint := TUndoInsertPoint.Create(AContour, Edge+1, APoint);
        frmGoPhast.UndoStack.Submit(UndoInsertPoint);
        UndoInsertPoint.SetPostSelection;
        Exit;
      end;
    end;
  end;
end;

procedure TframeSide.DeleteSegment(const X, Y: integer);
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
    if AContour.Selected and (AContour.ViewDirection = vdSide) then
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

procedure TframeSide.FinishContours;
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
    frmGoPhast.SideContoursChanged := True;
  end;
end;


function TframeSide.SelectPointsWithLine(const AContour: T2DContour;
  const AddToSelection: boolean; out Changed: boolean): boolean;
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

procedure TframeSide.DrawGridAndContours;
var
  Index : integer;
  AContour: T2DContour;
begin
  if  (frmContourProperties <> nil) then Exit;
  try
    bmpSide.Free;
    bmpSide := nil;
    bmpSide := TBitMap.Create;
    bmpSide.Height := zbSide.PaintBox.Height;
    bmpSide.Width := zbSide.PaintBox.Width;

    // If the colors of the grid cells are out of date,
    // recalculate them.
    if frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors then
    begin
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
    end;

    With bmpSide do
    begin
      With Canvas do
      begin
    // draw the grid.
        Brush.Color := clWhite;
        Pen.Color := clBlack;
        Rectangle(0,0,bmpSide.Width, bmpSide.Height);
        frmGoPhast.PhastGrid.Draw(bmpSide.Canvas, vdSide);

        Application.ProcessMessages;
        if frmGoPhast.SideContoursChanged then
        begin
          frmGoPhast.SideContoursChanged := True;
          Exit;
        end;

        // If there are no objects, finish up and quit.
        if frmGoPhast.Contours.Count = 0 then
        begin
          frmGoPhast.SideGridChanged := False;
          zbSide.Image.Picture.Assign(bmpSide);
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

procedure TframeSide.InvalidateContours;
var
  ContourIndex : integer;
  AContour : T2DContour;
begin
  for ContourIndex := 0 to frmGoPhast.Contours.Count-1 do
  begin
    AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
    if AContour.ViewDirection = vdSide then
    begin
      AContour.Invalidate;
    end;
  end;
end;

procedure TframeSide.DrawContours;
var
  ContourIndex : integer;
  AContour : T2DContour;
begin
//  if frmGoPhast.Contours.Count = 0 then Exit;
  if frmContourProperties <> nil then Exit;
  try
    bmpSide.Canvas.Brush.Color := clBlack;
    for ContourIndex := 0 to frmGoPhast.Contours.Count-1 do
    begin
      Application.ProcessMessages;
      if frmGoPhast.SideContoursChanged then
      begin
        frmGoPhast.SideContoursChanged := True;
        Exit;
      end;
      AContour := frmGoPhast.Contours[ContourIndex] as T2DContour;
      if (AContour.ViewDirection = vdSide) and (AContour <> CurrentContour) then
      begin
        AContour.Draw(bmpSide.Canvas);
      end;
    end;
    zbSide.Image.Picture.Assign(bmpSide);
    frmGoPhast.SideGridChanged := False;

  except on EInvalidGraphicOperation do
    begin
      WarnTooBig;
    end;
  end;
  PreviousContours.Clear;
end;

procedure TframeSide.WarnTooBig;
begin
  // zoom out a bit.
  if (zbSide.PaintBox.Width > zbSide.HorzScrollBar.Width) or
    (zbSide.PaintBox.Height > zbSide.VertScrollBar.Height) then
  begin
    zbSide.ZoomBy(0.8);
    zbSide.PaintBox.Invalidate;
    Beep;
  end;
  MessageDlg('Sorry; the view was zoomed in too far.  '
      + 'I''ve had to zoom out a bit', mtInformation, [mbOK], 0);
end;

procedure TframeSide.zbSidePaintBoxDblClick(Sender: TObject);
begin
  DoubleClicked := True;
end;

procedure TframeSide.zbSideBottomPaintBoxPaint(Sender: TObject);
var
  Index : integer;
  Contour : T2DContour;
begin
  // This allows for the current contour and previous contours to
  // be drawn with relatively little flickering until the zbSide.Image
  // has been updated.
  for Index := 0 to PreviousContours.Count -1 do
  begin
    Contour := PreviousContours[Index];
    Contour.Draw(zbSide.BottomPaintBox.Canvas);
  end;

  if (bmpSide <> nil) and (CurrentContour <> nil) then
  begin
    CurrentContour.Draw(zbSide.BottomPaintBox.Canvas);
  end;  
end;

procedure TframeSide.DrawMouseSelectionRectangle;
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
    zbSide.PaintBox.Canvas.Brush.Style := bsClear;
    zbSide.PaintBox.Canvas.Pen.Style := psDash;
    zbSide.PaintBox.Canvas.Pen.Width := 1;
    zbSide.PaintBox.Canvas.Pen.Color := clBlack;
    zbSide.PaintBox.Canvas.Rectangle(Rect(TL, BR));
  end;
end;

procedure TframeSide.DrawSelectionRectangle;
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
    zbSide.PaintBox.Canvas.Brush.Style := bsClear;
    zbSide.PaintBox.Canvas.Pen.Style := psDash;
    zbSide.PaintBox.Canvas.Pen.Width := 1;
    zbSide.PaintBox.Canvas.Pen.Color := clBlack;
    zbSide.PaintBox.Canvas.Rectangle(Rect(TL, BR));
  end;
end;

procedure TframeSide.ShowMovedPoints;
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
    zbSide.PaintBox.Canvas.Brush.Style := bsClear;
    zbSide.PaintBox.Canvas.Pen.Style := psDash;
    zbSide.PaintBox.Canvas.Pen.Width := 1;
    zbSide.PaintBox.Canvas.Pen.Color := clBlack;
    for PointIndex := 0 to SelectedPointContour.Count -1 do
    begin
      if SelectedPointContour.SelectedNodes[PointIndex] then
      begin
        if SelectedPointContour.Count = 1 then
        begin
          zbSide.PaintBox.Canvas.Rectangle(frmGoPhast.CursorX-3,
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
          zbSide.PaintBox.Canvas.PolyLine(PointArray);
        end;
      end;
    end;
  end;
end;
procedure TframeSide.DrawBlueSelectedCells(FirstRow, LastRow, FirstLayer,
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
  Polygon[0].X := self.zbSide.XCoord(APoint.Z);
  Polygon[0].Y := self.zbSide.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0 ,R2, L1);
  Polygon[1].X := self.zbSide.XCoord(APoint.Z);
  Polygon[1].Y := self.zbSide.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R2, L2);
  Polygon[2].X := self.zbSide.XCoord(APoint.Z);
  Polygon[2].Y := self.zbSide.YCoord(APoint.Y);

  APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L2);
  Polygon[3].X := self.zbSide.XCoord(APoint.Z);
  Polygon[3].Y := self.zbSide.YCoord(APoint.Y);

  OldBrushStyle := zbSide.PaintBox.Canvas.Brush.Style;
  OldColor := zbSide.PaintBox.Canvas.Brush.Color;
  try
    zbSide.PaintBox.Canvas.Brush.Style := bsSolid;
    zbSide.PaintBox.Canvas.Brush.Color := clBlue;

    zbSide.PaintBox.Canvas.Polygon(Polygon);

  finally
    zbSide.PaintBox.Canvas.Brush.Style := OldBrushStyle;
    zbSide.PaintBox.Canvas.Brush.Color := OldColor;
  end;
end;

procedure TframeSide.ShowNewRowOrLayer;
var
  APoint : T3DRealPoint;
  CursorPoint : T2DRealPoint;
begin
  CursorPoint.X := zbSide.X(frmGoPhast.CursorX);
  CursorPoint.Y := zbSide.Y(frmGoPhast.CursorY);
  zbSide.PaintBox.Canvas.Pen.Style := psDash;
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
        zbSide.PaintBox.Canvas.MoveTo(zbSide.XCoord(APoint.X),
          zbSide.YCoord(APoint.Y));

        APoint.Y := frmGoPhast.PhastGrid.RowPosition
          [frmGoPhast.PhastGrid.RowCount];
        APoint.X := CursorPoint.X;
        zbSide.PaintBox.Canvas.LineTo(zbSide.XCoord(APoint.X),
          zbSide.YCoord(APoint.Y));
      end
      else if (frmGoPhast.tbMove.Down and frmGoPhast.MovingRow)
        or frmGoPhast.tbAddRow.Down then
      begin
        APoint.Y := CursorPoint.Y;
        APoint.X := frmGoPhast.PhastGrid.LayerElevation[0];
        zbSide.PaintBox.Canvas.MoveTo(zbSide.XCoord(APoint.X),
          zbSide.YCoord(APoint.Y));

        APoint.Y := CursorPoint.Y;
        APoint.X := frmGoPhast.PhastGrid.LayerElevation
          [frmGoPhast.PhastGrid.LayerCount];
        zbSide.PaintBox.Canvas.LineTo(zbSide.XCoord(APoint.X),
          zbSide.YCoord(APoint.Y));
      end;
    end;
  finally
    zbSide.PaintBox.Canvas.Pen.Style := psSolid;
  end;

end;

procedure TframeSide.DrawModifiedCurrentContour;
var
  APoint : T2DRealPoint;
  PreviousPoint : T2DRealPoint;
  XPrime, YPrime : integer;
begin
  // draw what the current object would look like if the mouse
  // button were depressed now.
  zbSide.PaintBox.Canvas.Pen.Style := psDash;
  try
    if frmGoPhast.tbPolygon.Down then
    begin
      if CurrentContour.Count > 0 then
      begin
        zbSide.PaintBox.Canvas.MoveTo(CurrentContour.CanvasCoordinates[0].X,
          CurrentContour.CanvasCoordinates[0].Y);
        zbSide.PaintBox.Canvas.LineTo(frmGoPhast.CursorX,
          frmGoPhast.CursorY);
        zbSide.PaintBox.Canvas.LineTo(CurrentContour.CanvasCoordinates[CurrentContour.Count-1].X,
          CurrentContour.CanvasCoordinates[CurrentContour.Count-1].Y);
      end;
    end
    else if frmGoPhast.tbLine.Down then
    begin
      if CurrentContour.Count > 0 then
      begin
        zbSide.PaintBox.Canvas.MoveTo(CurrentContour.CanvasCoordinates[CurrentContour.Count-1].X,
          CurrentContour.CanvasCoordinates[CurrentContour.Count-1].Y);
        zbSide.PaintBox.Canvas.LineTo(frmGoPhast.CursorX,
          frmGoPhast.CursorY);
      end;
    end
    else if frmGoPhast.tbStraightLine.Down then
    begin
      APoint.X := zbSide.X(frmGoPhast.CursorX);
      APoint.Y := zbSide.Y(frmGoPhast.CursorY);
      PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];
      XPrime := frmGoPhast.CursorX - zbSide.XCoord(PreviousPoint.X);
      YPrime := frmGoPhast.CursorY - zbSide.YCoord(PreviousPoint.Y);
      if Abs(XPrime) > Abs(YPrime) then
      begin
        APoint.X := zbSide.X(frmGoPhast.CursorX);
        APoint.Y := PreviousPoint.Y ;
      end
      else
      begin
        APoint.X := PreviousPoint.X;
        APoint.Y := zbSide.Y(frmGoPhast.CursorY);
      end;
      zbSide.PaintBox.Canvas.MoveTo(zbSide.XCoord(PreviousPoint.X), zbSide.YCoord(PreviousPoint.Y));
      zbSide.PaintBox.Canvas.LineTo(zbSide.XCoord(APoint.X), zbSide.YCoord(APoint.Y));
    end
    else if frmGoPhast.tbRectangle.Down then
    begin
      PreviousPoint := CurrentContour.Points[CurrentContour.Count -1];

      zbSide.PaintBox.Canvas.MoveTo(zbSide.XCoord(PreviousPoint.X), zbSide.YCoord(PreviousPoint.Y));
      zbSide.PaintBox.Canvas.LineTo(frmGoPhast.CursorX, zbSide.YCoord(PreviousPoint.Y));
      zbSide.PaintBox.Canvas.LineTo(frmGoPhast.CursorX, frmGoPhast.CursorY);
      zbSide.PaintBox.Canvas.LineTo(zbSide.XCoord(PreviousPoint.X), frmGoPhast.CursorY);
      zbSide.PaintBox.Canvas.LineTo(zbSide.XCoord(PreviousPoint.X), zbSide.YCoord(PreviousPoint.Y));

    end;
  finally
    zbSide.PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;

end.

