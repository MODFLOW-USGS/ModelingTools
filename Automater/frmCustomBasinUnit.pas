unit frmCustomBasinUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RbwRuler, ZoomBox2, FastGEO, Generics.Collections, GR32,
  BigCanvasMethods, ComCtrls, ToolWin, ImgList, GR32_Layers, JvExComCtrls,
  JvStatusBar, StdCtrls, Buttons, OleCtrls, SHDocVw, frmAncestorUnit;

type
  TZoomAction = (zaAdd, zaInsert, zaSelectPoints, zaMovePoints,
    zaZoomIn, zaZoomOut);

  TPointList = TList<TPoint2D>;
  TIntList = TList<Integer>;

  TCustomBasin = class(TObject)
  private
    FPoints: TPointList;
    FSelected: TIntList;
    FZoomBox: TQRbwZoomBox2;
    procedure PointsChanged(Sender: TObject; const Item: TPoint2D;
      Action: TCollectionNotification);
    procedure SetPoints(const Value: TPointList);
    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; const Value: Boolean);
    function GetCount: Integer;
    function GetSelectionCount: Integer;
    function GetPoint(Index: Integer): TPoint2D;
    procedure SetPoint(Index: Integer; const Value: TPoint2D);
    property PointList: TPointList read FPoints write SetPoints;
  public
    constructor Create(ZoomBox: TQRbwZoomBox2);
    destructor Destroy; override;
    procedure Draw(Const Bitmap32: TBitmap32);
    function CanvasCoordinates: TPointArray;
    procedure AddPoint(APoint: TPoint2D);
    procedure InsertPoint(Index: Integer; APoint: TPoint2D);
    procedure DeletePoint(Index: Integer);
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    procedure Clear;
    function SelectEdge(const X, Y: integer): integer;
    property Count: Integer read GetCount;
    function SelectPoint(const X, Y: integer): integer;
    property SelectionCount: Integer read GetSelectionCount;
    procedure ClearSelected;
    property Points[Index: Integer]: TPoint2D read GetPoint write SetPoint;
    procedure DeleteSelected;
  end;

  TCustomInteractiveTool = class(TComponent)
  private
    // See @link(Cursor).
    FCursor: TCursor;
    // Getter for @link(MouseIsDown)
    function GetMouseIsDown: boolean;

  protected
    FTopLayer: TPositionedLayer;
    procedure CreateLayers; virtual;
    function Layer32: TPositionedLayer;
    // See @link(Cursor).
    function GetCursor: TCursor; virtual;
    // See @link(Hint)
    function GetHint: string; virtual;
    // See @link(Cursor).
    procedure SetCursor(const Value: TCursor); virtual;
    // @name sets the cursors of
    // TQRbwZoomBox2.Image32
    // and TframeView.@link(TframeView.ZoomBox).
    procedure UpdateCursors; virtual;
    // @name is the TQRbwZoomBox2 of @link(View).
    function ZoomBox: TQRbwZoomBox2;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); virtual;
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
    procedure KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
  end;


  TfrmCustomBasin = class(TfrmAncestor)
    rulVertical: TRbwRuler;
    pnl1: TPanel;
    rulHorizontal: TRbwRuler;
    zmbx1: TQRbwZoomBox2;
    pnl2: TPanel;
    ilImageList: TImageList;
    ilDisabledImageList: TImageList;
    ctrlbrControlBar: TControlBar;
    tlbEdit: TToolBar;
    btnInsertPoint: TToolButton;
    btnZoom: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    btnPan: TToolButton;
    btnDrawBasin: TToolButton;
    btnEditVertex: TToolButton;
    sb1: TJvStatusBar;
    tmrTimer: TTimer;
    btnDefaultPosition: TToolButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlHelp: TPanel;
    wbHelp: TWebBrowser;
    pnl3: TPanel;
    btn1: TToolButton;
    pnl4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure zmbx1Image32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure btnZoomClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnPanClick(Sender: TObject);
    procedure zmbx1Image32DblClick(Sender: TObject);
    procedure zmbx1Image32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure zmbx1Image32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure zmbx1Resize(Sender: TObject);
    procedure zmbx1Pan(Sender: TObject; DeltaX, DeltaY: Real);
    procedure btnDefaultPositionClick(Sender: TObject);
    procedure btnDrawBasinClick(Sender: TObject);
    procedure btnInsertPointClick(Sender: TObject);
    procedure btnEditVertexClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ControlEnter(Sender: TObject);
  private
    FTopLayer: TPositionedLayer;
    FCustomBasin: TCustomBasin;
    FCurrentTool: TCustomInteractiveTool;
    FBusy: Boolean;
    FIsResizing: Boolean;
    FPositionedLayer: TPositionedLayer;
    FMouseStartX: integer;
    FMouseStartY: integer;
    FBitMap32: TBitmap32;
    FPaintingNeeded: Boolean;
    FDrawing: Boolean;
//    MagnificationChanged: Boolean;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); virtual;
    procedure SetTopPosition(const XCoordinate, YCoordinate: real);
    procedure SetCurrentTool(const Value: TCustomInteractiveTool);
    procedure UpdateStatusBar(X, Y: Integer);
    // @name causes the @link(TFrameView.ZoomBox)es to be resized.
    // @name is set to be the OnTimer event handler of @link(timTimer)
    // in TframeView.@link(TframeView.ZoomBoxResize).
    procedure ResizeZoomBoxes(Sender: TObject);
    procedure ZoomBoxHitTest(Sender: TObject; X, Y: Integer;
      var Passed: Boolean);
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    procedure DrawCustomBasin;
    procedure PaintView(Sender: TObject);
    procedure SetDefaultScale(Points: TPointList);
    procedure SetData;
    function GetHelpUrl(Keyword: string): string;
    { Private declarations }
  public
    procedure GetData(Points: TPointList);
    procedure AdjustHorizontalScale(const NewX: integer);
    procedure AdjustVerticalScale(const NewY: integer);
    property CurrentTool: TCustomInteractiveTool read FCurrentTool
      write SetCurrentTool;
    property CustomBasin: TCustomBasin read FCustomBasin;
    { Public declarations }
  end;

var
  frmCustomBasin: TfrmCustomBasin;
const
  SquareOffset = 3;

implementation

uses
  GR32_Polygons, Math, InteractiveTools, frmGwMoundUnit, RbwDataGrid4;

resourcestring
  StrChooseAToolAndCl = 'Choose a tool and click to edit custom basin';
  StrDefaultView = 'Default view';
  StrDrawCustomBasin = 'Draw custom basin';
  StrSelectPoints = 'Select points';
  StrInsertPoint = 'Insert point';
  StrZoom = 'Zoom';
  StrZoomIn = 'Zoom in';
  StrZoomOut = 'Zoom out';
  StrMoveView = 'Move view';

{$R *.dfm}

var
  GlobalMouseIsDown: boolean;
const
  SelectEpsilon = 5;

function IsValueInside(const First, Middle, Last: real): boolean;
var
  Epsilon: double;
begin
  Epsilon := Abs(First - Last) / 1E8;
  // This function returns True if Middle is between First and Last.
  result := ((First - Epsilon <= Middle) and (Middle <= Last + Epsilon))
    or ((First + Epsilon >= Middle) and (Middle >= Last - Epsilon));
end;


{ TCustomBasin }

procedure TCustomBasin.AddPoint(APoint: TPoint2D);
begin
  PointList.Add(APoint);
end;

function TCustomBasin.CanvasCoordinates: TPointArray;
var
  PointIndex: Integer;
begin
  SetLength(Result, PointList.Count);
  for PointIndex := 0 to PointList.Count - 1 do
  begin
    Result[PointIndex].X := FZoomBox.XCoord(PointList[PointIndex].x);
    Result[PointIndex].Y := FZoomBox.YCoord(PointList[PointIndex].y);
  end;
end;

procedure TCustomBasin.Clear;
begin
  PointList.Clear;
  FSelected.Clear;
end;

procedure TCustomBasin.ClearSelected;
begin
  FSelected.Clear;
end;

constructor TCustomBasin.Create(ZoomBox: TQRbwZoomBox2);
begin
  FZoomBox := ZoomBox;
  FPoints:= TPointList.Create;
  FPoints.OnNotify := PointsChanged;
  FSelected := TIntList.Create;
end;

procedure TCustomBasin.DeletePoint(Index: Integer);
var
  SelectedIndex: Integer;
begin
  PointList.Delete(Index);
  for SelectedIndex := FSelected.Count - 1 downto 0 do
  begin
    if FSelected[SelectedIndex] > Index then
    begin
      FSelected[SelectedIndex] := FSelected[SelectedIndex]-1;
    end
    else if FSelected[SelectedIndex] = Index then
    begin
      FSelected.Delete(SelectedIndex);
    end;
  end;
end;

procedure TCustomBasin.DeleteSelected;
var
  index: Integer;
begin
  for index := PointList.Count - 1 downto 0 do
  begin
    if Selected[index] then
    begin
      PointList.Delete(index);
    end;
  end;
  FSelected.Clear;
end;

destructor TCustomBasin.Destroy;
begin
  FSelected.Free;
  FPoints.Free;
  inherited;
end;

procedure TCustomBasin.Draw(Const Bitmap32: TBitmap32);
var
  FillColor32, LineColor32: TColor32;
  LineWidth: single;
  MultiplePolygons: Boolean;
  SectionPoints: TPointArray;
  P: TPolygon32;
  index: Integer;
begin
  SectionPoints := CanvasCoordinates;
  if Length(SectionPoints) > 0 then
  begin
    LineColor32 := clBlack32;
    FillColor32 := clTransparent32;
    LineWidth := 1.5;
    MultiplePolygons := False;
    P := nil;
    if Length(SectionPoints) = 1 then
    begin
    end
    else if Length(SectionPoints) = 2 then
    begin
      DrawBigPolyline32(Bitmap32,LineColor32,LineWidth, SectionPoints, True);
    end
    else
    begin
      DrawBigPolygon32(Bitmap32, LineColor32, FillColor32, LineWidth,
        SectionPoints, P, MultiplePolygons,
        True);
    end;
    for Index := 0 to PointList.Count - 1 do
    begin
      if Selected[Index] then
      begin
        DrawBigRectangle32(Bitmap32,LineColor32, clBlack32, 1,
          SectionPoints[Index].X-SquareOffset, SectionPoints[Index].Y-SquareOffset,
          SectionPoints[Index].X+SquareOffset, SectionPoints[Index].Y+SquareOffset)
      end
      else
      begin
        DrawBigRectangle32(Bitmap32,LineColor32, FillColor32, 1,
          SectionPoints[Index].X-SquareOffset, SectionPoints[Index].Y-SquareOffset,
          SectionPoints[Index].X+SquareOffset, SectionPoints[Index].Y+SquareOffset)
      end;
    end;
  end;
end;

function TCustomBasin.GetCount: Integer;
begin
  result := PointList.Count;
end;

function TCustomBasin.GetPoint(Index: Integer): TPoint2D;
begin
  result := PointList[Index];
end;

function TCustomBasin.GetSelected(Index: Integer): Boolean;
begin
  result := FSelected.IndexOf(Index) >= 0;
end;

function TCustomBasin.GetSelectionCount: Integer;
begin
  result := FSelected.Count;
end;

procedure TCustomBasin.InsertPoint(Index: Integer; APoint: TPoint2D);
var
  SelectedIndex: Integer;
begin
  PointList.Insert(Index, APoint);
  for SelectedIndex := 0 to FSelected.Count - 1 do
  begin
    if FSelected[SelectedIndex] >= Index then
    begin
      FSelected[SelectedIndex] := FSelected[SelectedIndex]+1;
    end;
  end;
end;

procedure TCustomBasin.PointsChanged(Sender: TObject; const Item: TPoint2D;
  Action: TCollectionNotification);
begin
  frmCustomBasin.zmbx1.Image32.Invalidate;
end;

function TCustomBasin.SelectEdge(const X, Y: integer): integer;
var
  Index: Integer;
  Coordinates: TPointArray;
  X1: Integer;
  X2: Integer;
  Y1: Integer;
  Y2: Integer;
  MinX: Int64;
  MaxX: Int64;
  MinY: Int64;
  MaxY: Int64;
  Prior: Integer;
begin
  result := -1;
  Prior := Count - 1;
  Coordinates := CanvasCoordinates;
  for Index := 0 to Count - 1 do
  begin
    X1 := Coordinates[Prior].X;
    X2 := Coordinates[Index].X;
    Y1 := Coordinates[Prior].Y;
    Y2 := Coordinates[Index].Y;
    MinX := Int64(Min(X1, X2)) - SelectEpsilon;
    MaxX := Int64(Max(X1, X2)) + SelectEpsilon;
    MinY := Int64(Min(Y1, Y2)) - SelectEpsilon;
    MaxY := Int64(Max(Y1, Y2)) + SelectEpsilon;

    if IsValueInside(MinX, X, MaxX) and IsValueInside(MinY, Y, MaxY) then
    begin
      if (Coordinates[Prior].X = Coordinates[Index].X)
        or (Coordinates[Prior].Y = Coordinates[Index].Y) then
      begin
        // special case: horizontal or vertical line.
        result := Prior;
        Exit;
      end
      else
      begin

        if Abs(X2 - X1) > Abs(Y2 - Y1) then
        begin
          if Abs((X - X1) / (X2 - X1) * (Y2 - Y1) + Y1 - Y) < SelectEpsilon then
          begin
            result := Prior;
            Exit;
          end
        end
        else
        begin
          if Abs((Y - Y1) / (Y2 - Y1) * (X2 - X1) + X1 - X) < SelectEpsilon then
          begin
            result := Prior;
            Exit;
          end
        end;
      end;
    end;
    Prior := Index;
  end;

end;

function TCustomBasin.SelectPoint(const X, Y: integer): integer;
var
  Coordinates: TPointArray;
  Index: Integer;
  X1: Integer;
  Y1: Integer;
  MinX: Int64;
  MaxX: Int64;
  MinY: Int64;
  MaxY: Int64;
begin
  result := -1;
  Coordinates := CanvasCoordinates;
  for Index := 0 to Count - 1 do
  begin
    X1 := Coordinates[Index].X;
    Y1 := Coordinates[Index].Y;
    MinX := Int64(X1) - SelectEpsilon;
    MaxX := Int64(X1) + SelectEpsilon;
    MinY := Int64(Y1) - SelectEpsilon;
    MaxY := Int64(Y1) + SelectEpsilon;
    if IsValueInside(MinX, X, MaxX)
      and IsValueInside(MinY, Y, MaxY) then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

procedure TCustomBasin.SetPoint(Index: Integer; const Value: TPoint2D);
begin
  PointList[Index] := Value;
end;

procedure TCustomBasin.SetPoints(const Value: TPointList);
begin
  FPoints.Clear;
  FPoints.AddRange(Value);
end;

procedure TCustomBasin.SetSelected(Index: Integer; const Value: Boolean);
var
  SelectPos: Integer;
begin
  SelectPos := FSelected.IndexOf(Index);
  if (SelectPos >= 0) <> Value then
  begin
    if Value then
    begin
      FSelected.Add(Index);
    end
    else
    begin
      FSelected.Delete(SelectPos);
    end;
    frmCustomBasin.zmbx1.Image32.Invalidate;
  end;
end;

procedure TfrmCustomBasin.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
//  FCustomBasin.Draw(Buffer);
end;

procedure TfrmCustomBasin.ZoomBoxHitTest(Sender: TObject; X, Y: Integer; var
  Passed: Boolean);
begin
  Passed := True;
end;

procedure TfrmCustomBasin.PaintView(Sender: TObject);
var
//  ShouldUpdate: boolean;
  PreviousCursor: TCursor;
begin
  // calling Application.ProcessMessages here causes the zmbx1 to redraw
  // properly but also can be a bottleneck.
//  Application.ProcessMessages;
  // @name is draws the grid @link(TfrmGoPhast.Grid) and
  // @link(TScreenObject)s.
  if FDrawing {or not frmGoPhast.CanDraw} then
    Exit;
  FDrawing := True;
//  ShouldUpdate := ScreenObjectsHaveChanged or NeedToRecalculateCellColors;
//  ScreenObjectsHaveChanged := False;
  try
    if (FBitMap32 <> nil) then
    begin
      // if you need to redraw the FBitMap32,
      // change the cursor to an hourglass,
      // redraw, and change the cursor back.
//      if {GridChanged or ModelChanged
//        or (FBitMap32.Height <> zmbx1.Image32.Height)
//        or (FBitMap32.Width <> zmbx1.Image32.Width)
//        or ShouldUpdate or} MagnificationChanged then
      begin
//        MagnificationChanged := False;
        PreviousCursor := zmbx1.Cursor;
        try
          zmbx1.Cursor := crHourGlass;
          try
            DrawCustomBasin;
          except on EInvalidOp do
            begin
              zmbx1.ZoomBy(0.01);
//              GridChanged := True;
              zmbx1.Image32.Invalidate;
              Exit;
            end;
          end;

          SelectAndDragTool.DrawOnBitMap32(nil, FBitMap32);

        finally
          zmbx1.Cursor := PreviousCursor;
        end;
      end;
    end;
  finally
//    ShouldUpdate := ScreenObjectsHaveChanged or NeedToRecalculateCellColors;
//    ScreenObjectsHaveChanged := False;
//    if ShouldUpdate then
//    begin
//      zmbx1.Image32.Invalidate;
//    end;
    FDrawing := False;
  end;
end;

procedure TfrmCustomBasin.PaintLayer(Sender: TObject; Buffer: TBitmap32);
begin
  Buffer.BeginUpdate;
  try
    PaintView(Sender);
    if zmbx1.Panning then
    begin
      Buffer.Draw(
        Round(FPositionedLayer.Location.Left) - FMouseStartX,
        Round(FPositionedLayer.Location.Top) - FMouseStartY,
        FBitMap32);
    end
    else
    begin
      Buffer.Draw(0, 0, FBitMap32);
    end;
  finally
    Buffer.EndUpdate;
  end
end;

procedure TfrmCustomBasin.FormCreate(Sender: TObject);
  function CreateLayer(ZoomBox: TQRbwZoomBox2): TPositionedLayer;
  begin
    result := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
      TPositionedLayer;
    result.OnPaint := DrawOnBitMap32;
  end;
var
  Url: string;
begin
  FBitMap32 := TBitmap32.Create;
  FCustomBasin := TCustomBasin.Create(zmbx1);

  FPositionedLayer := zmbx1.Image32.Layers.Add(TPositionedLayer) as
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

  if FTopLayer = nil then
  begin
    FTopLayer := CreateLayer(zmbx1);
  end;

  btnDrawBasin.Hint := StrDrawCustomBasin;
  btnEditVertex.Hint := StrSelectPoints;
  btnInsertPoint.Hint := StrInsertPoint;
  btnZoom.Hint := StrZoom;
  btnZoomIn.Hint := StrZoomIn;
  btnZoomOut.Hint := StrZoomOut;
  btnPan.Hint := StrMoveView;
  btnDefaultPosition.Hint := StrDefaultView;

  Url := GetHelpUrl('');
  wbHelp.Navigate(Url);
end;

procedure TfrmCustomBasin.FormDestroy(Sender: TObject);
begin
  FTopLayer.Free;
  FCustomBasin.Free;
end;

procedure TfrmCustomBasin.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CurrentTool <> nil then
  begin
    CurrentTool.KeyUp(Sender, Key, Shift);
  end;
end;

procedure TfrmCustomBasin.SetCurrentTool(const Value: TCustomInteractiveTool);
begin
  if CurrentTool <> Value then
  begin
    if CurrentTool <> nil then
    begin
      CurrentTool.Deactivate;
    end;
    FCurrentTool := Value;
    if CurrentTool <> nil then
    begin
      CurrentTool.Activate;
    end;
  end;
end;

procedure TfrmCustomBasin.SetTopPosition(const XCoordinate, YCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with zmbx1 do
  begin
    DeltaX := (X(Image32.Width) - X(0)) / 2;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := XCoordinate - DeltaX;
    OriginY := YCoordinate - DeltaY;
    Image32.Invalidate;
//    .AdjustScales;
  end;
end;


procedure TfrmCustomBasin.GetData(Points: TPointList);
begin
  SetDefaultScale(Points);
  FCustomBasin.PointList := Points;
end;

function TfrmCustomBasin.GetHelpUrl(Keyword: string): string;
var
  FileName: string;
begin
  FileName := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));

{$IFDEF CHM}
  FileName := FileName + 'Help\GwMounding.chm';
  FileName := FileName + '::/custom_basin.htm';
  result := 'mk:@MSITStore:' + FileName;
{$ELSE}
  FileName := FileName + 'Help\';
  FileName := FileName + 'custom_basin.htm';
  result := 'file:\\' + FileName;
{$ENDIF}

  if Keyword <> '' then
  begin
    result := result + '#' + Keyword;
  end;
end;

procedure TfrmCustomBasin.ResizeZoomBoxes(Sender: TObject);
begin
  tmrTimer.Enabled := False;
  if not zmbx1.ImmediateResize then
  begin
    zmbx1.ImmediateResize := True;
    FIsResizing := False;
    AdjustHorizontalScale(0);
    AdjustVerticalScale(0);
  end;
end;

procedure TfrmCustomBasin.zmbx1Image32DblClick(Sender: TObject);
begin
  if (CurrentTool <> nil) then
  begin
    CurrentTool.DoubleClick(Sender);
  end;
end;

procedure TfrmCustomBasin.zmbx1Image32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FMouseStartX := X;
  FMouseStartY := Y;
  if CurrentTool <> nil then
  begin
    CurrentTool.MouseDown(Sender, Button, Shift, X, Y);
  end;
end;

procedure TfrmCustomBasin.DrawCustomBasin;
begin
  FPaintingNeeded := True;
  try
    try
      FBitMap32.Free;
      FBitMap32 := nil;
      FBitMap32 := TBitmap32.Create;

      FBitMap32.Height := zmbx1.Image32.Height;
      FBitMap32.Width := zmbx1.Image32.Width;

      // draw a box around the drawing area
      DrawBigRectangle32(FBitMap32, clBlack32, clWhite32, 1.0, 0, 0,
        FBitMap32.Width - 1, FBitMap32.Height - 1);

      // draw the custom basin
      FCustomBasin.Draw(FBitMap32);



      // Do not call Application.ProcessMessages.
      // Calling Application.ProcessMessages causes all the views to
      // be redrawn.  If that is done, PaintLayer has to deal with the
      // case where it is called within PaintLayer from another view.
      // In addition, this may be the cause of some access violations
      // within Graphics32.
      // See the compiler definition ExtraDrawingNeeded in the TFrameView
      // declaration.

//      Application.ProcessMessages;



    except on EInvalidGraphicOperation do
      begin
        // If the size is too big, make it smaller and start over.
//        WarnTooBig;
      end;
    end;
  finally
//    ModelChanged := False;
//    if frmGoPhast.Grid <> nil then
//    begin
//      frmGoPhast.Grid.Draw3DAllowed := True;
//      frmGoPhast.frame3DView.glWidModelView.Invalidate;
//    end;
  end;
end;

procedure TfrmCustomBasin.zmbx1Image32MouseMove(Sender: TObject;
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
  if zmbx1.Panning then
  begin
    FPositionedLayer.Location := FloatRect(X, Y, X, Y);
  end;

  // Record which grid the cursor is over

  // display the current position and other related data.
  UpdateStatusBar(X, Y);

  if CurrentTool <> nil then
  begin

    CurrentTool.MouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TfrmCustomBasin.zmbx1Image32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
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
      if CurrentTool <> nil then
      begin
        CurrentTool.RightClick(Sender, Button, Shift, X, Y);
      end;
      // for a right click, don't create objects
      Exit;
    end;
    // Record the current cursor position.
//    frmGoPhast.CursorGrid := CursorGrid;
//    frmGoPhast.CursorX := X;
//    frmGoPhast.CursorY := Y;

    if CurrentTool <> nil then
    begin
      CurrentTool.MouseUp(Sender, Button, Shift, X, Y);
    end;

  finally
    FBusy := False;
  end;
end;

procedure TfrmCustomBasin.zmbx1Pan(Sender: TObject; DeltaX, DeltaY: Real);

var
  X, Y: integer;
begin
    // @name is the event handler for @link(ZoomBox).OnPan.
    // It updates @link(rulHorizontal) and @link(rulVertical) with
    // its current position.
  X := zmbx1.XCoord(zmbx1.OriginX) - zmbx1.XCoord(zmbx1.OriginX +
    DeltaX);
  Y := zmbx1.YCoord(zmbx1.OriginY) - zmbx1.YCoord(zmbx1.OriginY +
    DeltaY);
  AdjustHorizontalScale(X);
  AdjustVerticalScale(Y);
end;

procedure TfrmCustomBasin.zmbx1Resize(Sender: TObject);
begin
    // @name is the event handler for @link(ZoomBox).OnResize.
    // @name sets frmGoPhast.@link(TfrmGoPhast.Timer).OnTimer to
    // frmGoPhast.@link(TfrmGoPhast.ResizeZoomBoxes) with a delay of 100 ms.
    // The effect is to prevent the control from redrawing until
    // the user has finished resizing it.
  if (tmrTimer <> nil) and not FIsResizing then
  begin
    FIsResizing := True;
    zmbx1.ImmediateResize := False;
    tmrTimer.Enabled := False;
    tmrTimer.OnTimer := ResizeZoomBoxes;
    tmrTimer.Interval := 100;
    tmrTimer.Enabled := True;
  end;
end;

procedure TfrmCustomBasin.AdjustHorizontalScale(const NewX: integer);
begin
    // @name is used to redraw the horizontal scale.
    // @param(NewX NewX indicates how far the image in @link(zmbx1)
    // has moved in the X direction in pixels.)

  // set the ends of the horizontal ruler so that the scale
  // is displayed properly.
  rulHorizontal.RulerEnds.Lower := 11;

  // This accounts for the width of the vertical scroll bar (width = 18)
  // plus the space between the end of the ruler at the end
  // of the empty space in zmbx1 ( = 12)
  // The value of 30 comes from 18+12

  // The ends must be adjusted by 1 to account for Panel1.BevelWidth = 1;
  rulHorizontal.RulerEnds.Upper := rulHorizontal.Width - 11;
  if rulHorizontal.RulerStart = sTopLeft then
  begin
    rulHorizontal.RulerValues.Lower := zmbx1.X(10 + NewX);
    rulHorizontal.RulerValues.Upper := zmbx1.X(zmbx1.Width - 12
      + NewX);
  end
  else
  begin
    rulHorizontal.RulerValues.Upper := zmbx1.X(10 + NewX);
    rulHorizontal.RulerValues.Lower := zmbx1.X(zmbx1.Width - 12
      + NewX);
  end;
  rulHorizontal.Invalidate;
end;

procedure TfrmCustomBasin.AdjustVerticalScale(const NewY: integer);
begin
    // @name is used to redraw the vertical scale.
    // @param(NewY NewY indicates how far the image in @link(zmbx1)
    // has moved in the Y direction in pixels.)

  // set the ends of the horizontal ruler so that the scale
  // is displayed properly.
  rulVertical.RulerEnds.Lower := 11;

  // This accounts for the width of the vertical scroll bar (width = 18)
  // plus the space between the end of the ruler at the end
  // of the empty space in zmbx1 ( = 12)
  // The value of 30 comes from 18+12

  // The ends must be adjusted by 1 to account for Panel1.BevelWidth = 1;
  rulVertical.RulerEnds.Upper := rulVertical.Height - 11;
  if rulVertical.RulerStart = sTopLeft then
  begin
    rulVertical.RulerValues.Lower := zmbx1.Y(10 + NewY);
    rulVertical.RulerValues.Upper := zmbx1.Y(zmbx1.Height - 12
      + NewY);
  end
  else
  begin
    rulVertical.RulerValues.Upper := zmbx1.Y(10 + NewY);
    rulVertical.RulerValues.Lower := zmbx1.Y(zmbx1.Height - 12
      + NewY);
  end;
  rulVertical.Invalidate;
end;

procedure TfrmCustomBasin.SetData;
var
  Grid: TRbwDataGrid4;
  PointIndex: Integer;
begin
  Grid := frmGwMound.rdgBasinCoordinates;
  Grid.BeginUpdate;
  try
    Grid.RowCount := Max(2, CustomBasin.Count+1);
    for PointIndex := 0 to CustomBasin.Count - 1 do
    begin
      Grid.Cells[0,PointIndex+1] := FloatToStr(CustomBasin.Points[PointIndex].x);
      Grid.Cells[1,PointIndex+1] := FloatToStr(CustomBasin.Points[PointIndex].y);
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrmCustomBasin.SetDefaultScale(Points: TPointList);
var
  YMax: Extended;
  XMax: Extended;
  ModelXWidth: Double;
  XMin: Extended;
  ModelYWidth: Double;
  YMin: Extended;
  index: Integer;
begin
  if Points.Count > 0 then
  begin
    XMax := Points[0].x;
    XMin := XMax;
    YMax := Points[0].y;
    YMin := YMax;
    for index := 1 to Points.Count - 1 do
    begin
      if Points[index].x > XMax then
      begin
        XMax := Points[index].x;
      end
      else if Points[index].x < XMin then
      begin
        XMin := Points[index].x;
      end;
      if Points[index].y > YMax then
      begin
        YMax := Points[index].y;
      end
      else if Points[index].y < YMin then
      begin
        YMin := Points[index].y;
      end;
    end;
    ModelXWidth := XMax - XMin;
    ModelYWidth := YMax - YMin;
    zmbx1.Magnification := 0.9 * Min(zmbx1.Width / ModelXWidth, zmbx1.Height / ModelYWidth);
    SetTopPosition((XMax + XMin) / 2, (YMax + YMin) / 2);
    AdjustHorizontalScale(0);
    AdjustVerticalScale(0);
  end;
end;

procedure TfrmCustomBasin.UpdateStatusBar(X, Y: Integer);
var
  XValue: Extended;
  YValue: Extended;
begin
  XValue := zmbx1.X(X);
  YValue := zmbx1.Y(Y);
  sb1.SimpleText := Format('(X,Y) = (%0:n, %1:n)', [XValue, YValue]);
end;


procedure TfrmCustomBasin.btnDrawBasinClick(Sender: TObject);
begin
  CurrentTool := NewBoundary;
end;

procedure TfrmCustomBasin.ControlEnter(Sender: TObject);
var
  Control: TControl;
  Url: string;
begin
  Control := Sender as TControl;
  while (Control <> nil) and (Control.HelpKeyword = '') do
  begin
    Control := Control.Parent;
  end;
  if Control <> nil then
  begin
    Url := GetHelpUrl(Control.HelpKeyword);
    wbHelp.Navigate(Url);
  end;
end;

procedure TfrmCustomBasin.btnEditVertexClick(Sender: TObject);
begin
  CurrentTool := SelectAndDragTool;
end;

procedure TfrmCustomBasin.btnDefaultPositionClick(Sender: TObject);
begin
  SetDefaultScale(FCustomBasin.PointList);
end;

procedure TfrmCustomBasin.btnOKClick(Sender: TObject);
begin
  SetData;
end;

procedure TfrmCustomBasin.btnInsertPointClick(Sender: TObject);
begin
  CurrentTool := InsertPointTool;
end;

procedure TfrmCustomBasin.btnPanClick(Sender: TObject);
begin
  CurrentTool := PanTool;
end;

procedure TfrmCustomBasin.btnZoomClick(Sender: TObject);
begin
  CurrentTool := ZoomTool;
end;

procedure TfrmCustomBasin.btnZoomInClick(Sender: TObject);
begin
  CurrentTool := ZoomInTool;
end;

procedure TfrmCustomBasin.btnZoomOutClick(Sender: TObject);
begin
  CurrentTool := ZoomOutTool;
end;

{ TCustomInteractiveTool }

procedure TCustomInteractiveTool.Activate;
begin
  if Hint = '' then
  begin
    frmCustomBasin.zmbx1.Hint := StrChooseAToolAndCl;
  end
  else
  begin
    frmCustomBasin.zmbx1.Hint := Hint;
  end;
  frmCustomBasin.zmbx1.Cursor := GetCursor;
  frmCustomBasin.zmbx1.Image32.Cursor := GetCursor;
  UpdateAllViews;
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
    FTopLayer := CreateLayer(frmCustomBasin.zmbx1);
  end;
end;

procedure TCustomInteractiveTool.Deactivate;
begin
  ZoomBox.Hint := StrChooseAToolAndCl;
  UpdateAllViews;
end;

procedure TCustomInteractiveTool.DoubleClick(Sender: TObject);
begin
  // do nothing
end;

procedure TCustomInteractiveTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  // do nothing
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

procedure TCustomInteractiveTool.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

function TCustomInteractiveTool.Layer32: TPositionedLayer;
  function CreateLayer: TPositionedLayer;
  begin
    result := ZoomBox.Image32.Layers.Add(TPositionedLayer) as
      TPositionedLayer;
    result.OnPaint := DrawOnBitMap32;
  end;
begin
  if FTopLayer = nil then
  begin
    FTopLayer := CreateLayer;
  end;
  result := FTopLayer;
  result.BringToFront;
end;

procedure TCustomInteractiveTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GlobalMouseIsDown := True;
end;

procedure TCustomInteractiveTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // do nothing.
end;

procedure TCustomInteractiveTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GlobalMouseIsDown := False;
end;

procedure TCustomInteractiveTool.RightClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // do nothing.
end;

procedure TCustomInteractiveTool.SetAllCursors(const Value: TCursor);
begin
  ZoomBox.Cursor := Value;
  ZoomBox.Image32.Cursor := Value;
end;

procedure TCustomInteractiveTool.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  UpdateCursors;
end;

procedure TCustomInteractiveTool.UpdateAllViews;
begin
  if FTopLayer <> nil then
  begin
    FTopLayer.Changed;
    ZoomBox.Image32.Invalidate;
  end;
end;

procedure TCustomInteractiveTool.UpdateCursors;
begin
  frmCustomBasin.zmbx1.Cursor := GetCursor;
  frmCustomBasin.zmbx1.Image32.Cursor := GetCursor;
  // This is needed to update the cursor for TSelectPointTool when dragging
  // a vertex.
  Screen.Cursor := GetCursor;
end;

function TCustomInteractiveTool.ZoomBox: TQRbwZoomBox2;
begin
  result := frmCustomBasin.zmbx1;
end;

end.
