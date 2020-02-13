unit InteractiveTools;

interface

uses
  frmCustomBasinUnit, Controls, Classes, GR32;

type
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
    procedure Activate; override;
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

  TCreateNewBoundaryTool = class(TCustomInteractiveTool)
  private
    FBasin: TCustomBasin;
  protected
    // @name returns the cursor to the class.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    procedure Deactivate; override;
    // @name causes the magnification to decrease by a factor of two.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TInsertPointTool = class(TCustomInteractiveTool)
  private
    FBasin: TCustomBasin;
  protected
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TSelectAndDragTool = class(TCustomInteractiveTool)
  private
    FBasin: TCustomBasin;
    FCursorX: Integer;
    FCursorY: Integer;
    FStartX: Integer;
    FStartY: Integer;
    FMouseButtonIsDown: Boolean;
    FMovingScreenObjects: Boolean;
    FClickedOnPoint: Boolean;
    procedure ShowMovedPoints(const BitMap: TBitmap32);
    procedure DrawSelectionRectangle32(BitMap: TBitmap32);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    function GetCursor: TCursor; override;
    function GetHint: string; override;
  public
    procedure Activate; override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); override;
  end;

  var
    // @name is the instance of @link(TZoomTool) used in ModelMuse.
    ZoomTool: TZoomTool;
    // @name is the instance of @link(TZoomInTool) used in ModelMuse.
    ZoomInTool: TZoomInTool;
    // @name is the instance of @link(TZoomOutTool) used in ModelMuse.
    ZoomOutTool: TZoomOutTool;
    // @name is the instance of @link(TPanTool) used in ModelMuse.
    PanTool: TPanTool;
    NewBoundary : TCreateNewBoundaryTool;
    InsertPointTool: TInsertPointTool;
    SelectAndDragTool: TSelectAndDragTool;


implementation

uses
  CursorsFoiledAgain, Forms, FastGEO, BigCanvasMethods, Math;

resourcestring
  StrClickAndDragToZo = 'Click and drag to zoom in';
  StrClickToZoomIn = 'Click to zoom in';
  StrClickToZoomOut = 'Click to zoom out';
  StrClickAndDragToMo = 'Click and drag to move view';
  StrClickOnASegmentT = 'Click on a segment to create a new node there.';
  StrClickAndDragToMoveP = 'Click and drag to move points';

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
  ZoomBox.BeginZoom(X, Y);
end;

procedure TZoomTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateCursors;
  with frmCustomBasin do
  begin
    AdjustHorizontalScale(0);
    AdjustVerticalScale(0);
  end;
  UpdateCursors;
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
  if Button <> mbRight then
  begin
    ZoomBox.ZoomByAt(2, X, Y);
    // If further zooming in is impossible, change the cursor.
    If not ZoomBox.CanZoomIn then
    begin
      UpdateCursors;
    end;
    with frmCustomBasin do
    begin
      AdjustHorizontalScale(0);
      AdjustVerticalScale(0);
    end;
    UpdateCursors;
  end;
  inherited;
end;

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
  if Button <> mbRight then
  begin
    ZoomBox.ZoomByAt(0.5, X, Y);
    with frmCustomBasin do
    begin
      AdjustHorizontalScale(0);
      AdjustVerticalScale(0);
    end;
    UpdateCursors;
  end;
  inherited;
end;

{ TPanTool }

procedure TPanTool.Activate;
begin
  inherited;
  frmCustomBasin.zmbx1.Cursor := crHandFlat;
  frmCustomBasin.zmbx1.Image32.Cursor := crHandFlat;
end;

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
  if Button <> mbLeft then Exit;
  ZoomBox.Panning := True;
  Cursor := crHandGrab;
  Screen.Cursor := crHandGrab;
end;

procedure TPanTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // The user has finished panning to a different position.
  // Stop panning and change the cursor to indicate that panning has
  // stopped.
  ZoomBox.Panning := False;
  Cursor := crHandFlat;
  Screen.Cursor := crDefault;
  with frmCustomBasin do
  begin
    AdjustHorizontalScale(0);
    AdjustVerticalScale(0);
  end;
  inherited;
end;

procedure TPanTool.RightClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseUp(Sender, Button, Shift, X, Y);
end;

{ TCreateNewBoundaryTool }

procedure TCreateNewBoundaryTool.Deactivate;
begin
  inherited;
  FBasin := nil;
end;

function TCreateNewBoundaryTool.GetCursor: TCursor;
begin
  Result := crPolygonArrow
end;

function TCreateNewBoundaryTool.GetHint: string;
begin
  result := 'Click to draw a custom basin';
end;

procedure TCreateNewBoundaryTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
begin
  inherited;
  if FBasin = nil then
  begin
    FBasin := frmCustomBasin.CustomBasin;
    FBasin.Clear;
  end;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  FBasin.AddPoint(APoint);
end;

{ TInsertPointTool }

function TInsertPointTool.GetHint: string;
begin
  result := StrClickOnASegmentT;
end;

procedure TInsertPointTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Edge: Integer;
begin
  if FBasin = nil then
  begin
    FBasin := frmCustomBasin.CustomBasin;
  end;
  Edge := FBasin.SelectEdge(X, Y);
  if Edge < 0 then
  begin
    Cursor := crDisabledInsertPoint;
  end
  else
  begin
    Cursor := crInsertPoint;
  end;
end;

procedure TInsertPointTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Edge: Integer;
  APoint: TPoint2D;
begin
  inherited;
  if FBasin = nil then
  begin
    FBasin := frmCustomBasin.CustomBasin;
  end;
  Edge := FBasin.SelectEdge(X, Y);
  if Edge >= 0 then
  begin
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    FBasin.InsertPoint(Edge+1, APoint);
  end;
end;

{ TSelectAndDragTool }

procedure TSelectAndDragTool.Activate;
begin
  inherited;
  FBasin := frmCustomBasin.CustomBasin;
end;

procedure TSelectAndDragTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if frmCustomBasin.CurrentTool <> self then Exit;
  Layer32.BringToFront;
  Buffer.BeginUpdate;
  try
    ShowMovedPoints(Buffer);
  finally
    Buffer.EndUpdate;
  end;
end;

procedure TSelectAndDragTool.DrawSelectionRectangle32(BitMap: TBitmap32);
//var
//  TopLeft: TPoint;
begin
  Layer32.BringToFront;
  BitMap.BeginUpdate;
  try
    DrawBigRectangle32(BitMap, clBlack32, clTransparent32, 1,
      FStartX, FStartY, FCursorX, FCursorY, True);
  finally
    BitMap.EndUpdate;
  end;
end;

function TSelectAndDragTool.GetCursor: TCursor;
begin
  result := crSelectPoint;
end;

function TSelectAndDragTool.GetHint: string;
begin
  result := StrClickAndDragToMoveP;
end;

procedure TSelectAndDragTool.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//var
//  Index: Integer;
begin
  if Key = 46 then
  begin
    // delete key;
    FBasin.DeleteSelected;
  end;
end;

procedure TSelectAndDragTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CanvasCoordinates: TPointArray;
  index: Integer;
begin
  inherited;
  FStartX := X;
  FStartY := Y;
  FMouseButtonIsDown := True;

  FClickedOnPoint := False;
  CanvasCoordinates := FBasin.CanvasCoordinates;
  for index := 0 to Length(CanvasCoordinates) - 1 do
  begin
    if (Abs(CanvasCoordinates[index].X-X) <= SquareOffset)
      and (Abs(CanvasCoordinates[index].Y-Y) <= SquareOffset) then
    begin
      FClickedOnPoint := True;
      if ssShift in Shift then
      begin
        FBasin.Selected[index] := not FBasin.Selected[index];
      end
      else
      begin
        if not FMovingScreenObjects or not FBasin.Selected[index] then
        begin

          FBasin.ClearSelected;
          FBasin.Selected[index] := True;
        end;
      end;
      break;
    end;
  end;
  if not FClickedOnPoint and not FMovingScreenObjects then
  begin
    FBasin.ClearSelected;
  end;

  FMovingScreenObjects := FBasin.SelectionCount > 0;
end;

procedure TSelectAndDragTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FCursorX := X;
  FCursorY := Y;
  if FMouseButtonIsDown then
  begin
    ZoomBox.Image32.Invalidate;
  end
end;

procedure TSelectAndDragTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CanvasCoordinates: TPointArray;
  Index: Integer;
  MinX: integer;
  MaxX: Integer;
  MinY: Integer;
  MaxY: Integer;
  DeltaX: Extended;
  DeltaY: Extended;
  APoint: TPoint2D;
begin
  inherited;
  FMouseButtonIsDown := False;
  if (Abs(X-FStartX) > SquareOffset) or (Abs(Y-FStartY) > SquareOffset) then
  begin
    if FMovingScreenObjects and FClickedOnPoint then
    begin
      Assert(FBasin.SelectionCount > 0);
      FMovingScreenObjects := False;
      DeltaX := ZoomBox.X(X)-ZoomBox.X(FStartX);
      DeltaY := ZoomBox.Y(Y)-ZoomBox.Y(FStartY);
      for Index := 0 to FBasin.Count - 1 do
      begin
        if FBasin.Selected[Index] then
        begin
          APoint := FBasin.Points[Index];
          APoint.x := APoint.x + DeltaX;
          APoint.y := APoint.y + DeltaY;
          FBasin.Points[Index] := APoint;
        end;
      end;
      FBasin.ClearSelected;
    end
    else
    begin
      CanvasCoordinates := FBasin.CanvasCoordinates;
      MinX := Min(X, FStartX);
      MaxX := Max(X, FStartX);
      MinY := Min(Y, FStartY);
      MaxY := Max(Y, FStartY);
      if not (ssShift in Shift) then
      begin
        FBasin.ClearSelected;
      end;
      for Index := 0 to Length(CanvasCoordinates) - 1 do
      begin
        if (MinX <= CanvasCoordinates[Index].X)
          and (MaxX >= CanvasCoordinates[Index].X)
          and (MinY <= CanvasCoordinates[Index].Y)
          and (MaxY >= CanvasCoordinates[Index].Y) then
        begin
          if ssShift in Shift then
          begin
            FBasin.Selected[index] := not FBasin.Selected[index];
          end
          else
          begin

            FBasin.Selected[index] := True;
          end;
        end;
      end;
      FMovingScreenObjects := FBasin.SelectionCount > 0;
      ZoomBox.Image32.Invalidate;
    end;
  end;
end;

procedure TSelectAndDragTool.ShowMovedPoints(const BitMap: TBitmap32);
var
  PointIndex: Integer;
  PointArray: array of TPoint;
  CanvasCoordinates: TPointArray;
  PMinus1: integer;
  PPlus1: integer;
begin
  if FMouseButtonIsDown  then
  begin
    if  FMovingScreenObjects and FClickedOnPoint then
    begin
      for PointIndex := 0 to FBasin.Count - 1 do
      begin
        if FBasin.Selected[PointIndex] then
        begin
          if FBasin.Count = 1 then
          begin
            DrawBigRectangle32(BitMap, clBlack32, clTransparent32, 1,
              FCursorX - SquareOffset,
              FCursorY - SquareOffset,
              FCursorX + SquareOffset,
              FCursorY + SquareOffset);
          end
          else
          begin
            SetLength(PointArray, 3);

            if PointIndex > 0 then
            begin
              PMinus1 := PointIndex-1;
            end
            else
            begin
              PMinus1 := FBasin.Count-1;
            end;

            if PointIndex < FBasin.Count-1 then
            begin
              PPlus1 := PointIndex+1;
            end
            else
            begin
              PPlus1 := 0;
            end;

            CanvasCoordinates := FBasin.CanvasCoordinates;

            PointArray[0] :=
              CanvasCoordinates[PMinus1];
            if FBasin.Selected[PMinus1] then
            begin
              PointArray[0].X := PointArray[0].X
                + FCursorX - FStartX;
              PointArray[0].Y := PointArray[0].Y
                + FCursorY - FStartY;
            end;

            PointArray[1] :=
              CanvasCoordinates[PointIndex];
            PointArray[1].X := PointArray[1].X
              + FCursorX - FStartX;
            PointArray[1].Y := PointArray[1].Y
              + FCursorY - FStartY;

            PointArray[2] :=
              CanvasCoordinates[PPlus1];
            if FBasin.Selected[PPlus1] then
            begin
              PointArray[2].X := PointArray[2].X
                + FCursorX - FStartX;
              PointArray[2].Y := PointArray[2].Y
                + FCursorY - FStartY;
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

end;

initialization
  ZoomTool := TZoomTool.Create(nil);
  ZoomInTool := TZoomInTool.Create(nil);
  ZoomOutTool := TZoomOutTool.Create(nil);
  PanTool := TPanTool.Create(nil);
  NewBoundary := TCreateNewBoundaryTool.Create(nil);
  InsertPointTool := TInsertPointTool.Create(nil);
  SelectAndDragTool := TSelectAndDragTool.Create(nil);

finalization
  ZoomTool.Free;
  ZoomInTool.Free;
  ZoomOutTool.Free;
  PanTool.Free;
  NewBoundary.Free;
  InsertPointTool.Free;
  SelectAndDragTool.Free;

end.
