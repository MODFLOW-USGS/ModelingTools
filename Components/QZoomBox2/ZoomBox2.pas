{@abstract(@name registers @link(TQRbwZoomBox2) which is used
  as an interface for handling drawing with real number coordinates.)

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit ZoomBox2;

interface

uses Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms, GR32_Image,
  GR32_Polygons, // TPolygon32 is declared in GR32_Polygons.
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32; // TBitmap32, and TFloatRect are declared in GR32.

type
  // @name indicates whether exaggeration is applied in
  // a vertical or horizontal direction.
  // See TQRbwZoomBox2.@link(TQRbwZoomBox2.ExaggerationDirection).
  TExaggerationDirection = (edVertical, edHorizontal);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.VerticalDirection).}
  TVerticalDirection = (vdDown, vdUp);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.HorizontalDirection).}
  THorizontalDirection = (hdRight, hdLeft);


  {
    @abstract(@name is used to respond to changes in the origin
    of a @link(TQRbwZoomBox2).)

    @name = procedure(Sender: TObject; DeltaX, DeltaY: real) of object;
    See @link(TQRbwZoomBox2.OnPan)

    Sender is the @link(TQRbwZoomBox2) whose origin is being changed.
    DeltaX is the amount by which the X-Coorinate
    of the origin is being changed.
    DeltaY is the amount by which the Y-Coorinate
    of the origin is being changed.
  }
  TPanEvent = procedure(Sender: TObject; DeltaX, DeltaY: real) of object;



  { @abstract(@name is the type of TQRbwZoomBox2.@link(TQRbwZoomBox2.Image32).)
    It overrides the protected @link(TzbImage32.MouseMove) and
    @link(TzbImage32.MouseUp)
    procedures and adds two protected events:
    @link(TzbImage32.OnZbMove),
    and @link(TzbImage32.OnZbUp).
    The event handlers for the protected events are provided by
    @link(TQRbwZoomBox2).
  }
  TZbImage32 = class(TImage32)
  private
    // See @link(OnZbMove).
    FOnZbMove: TMouseMoveEvent;
    // See @link(OnZbUp).
    FOnZbUp: TMouseEvent;
  protected
    // @name calls inherited @name and then calls @link(OnZbMove).
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // @name calls @link(OnZbUp) and then calls inherited @name.
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    // @name is used to respond to mouse move events.
    // The event handler for @name is
    // TQRbwZoomBox2.@link(TQRbwZoomBox2.PanMove).
    property OnZbMove: TMouseMoveEvent read FOnZbMove write FOnZbMove;
    // @name is used to respond to mouse up events.
    // The event handler for @name is
    // TQRbwZoomBox2.@link(TQRbwZoomBox2.PanUp).
    property OnZbUp: TMouseEvent read FOnZbUp write FOnZbUp;
  end;

  {
    @abstract(@name is used as an interface for handling drawing
    with real number coordinates.) The functions
    @link(TQRbwZoomBox2.X), @link(TQRbwZoomBox2.XCoord),
    @link(TQRbwZoomBox2.Y), and @link(TQRbwZoomBox2.YCoord)
    can be used to convert between
    real-world and screen coordinates.  By default, the real-world
    coordinates increase to the left and upward but these directions
    can be reversed with @link(TQRbwZoomBox2.HorizontalDirection) and
    @link(TQRbwZoomBox2.VerticalDirection).

    Zooming operations are performed with @link(TQRbwZoomBox2.BeginZoom)
    and @link(TQRbwZoomBox2.FinishZoom)

    Panning can be turned on and off with @link(TQRbwZoomBox2.Panning).

    Drawing should be done on the subcomponent.
  }
  TQRbwZoomBox2 = class(TPanel)
  private
    // @name is set to the mouse X coordinate in @link(BeginZoom),
    // @link(ContinueZoom), and @link(FinishZoom).
    // See @link(FStartX).
    FEndX: Integer;
    // @name is set to the mouse Y coordinate in @link(BeginZoom),
    // @link(ContinueZoom), and @link(FinishZoom).
    // See @link(FStartY).
    FEndY: Integer;
    // See @link(Exaggeration).
    FExaggeration: real;
    // See @link(ExaggerationDirection).
    FExaggerationDirection: TExaggerationDirection;
    // See @link(HorizontalDirection).
    FHorizontalDirection: THorizontalDirection;
    // @name is used for the actual drawing in @classname
    FImage32: TzbImage32;
    // See @link(ImmediateResize).
    FImmediateResize: boolean;
    // See @link(Magnification).
    FMagnification: real;
    // See @link(OnPan).
    FOnPan: TPanEvent;
    // See @link(OriginX).
    FOriginX: real;
    // See @link(OriginY).
    FOriginY: real;
    // See @link(Panning).
    FPanning: boolean;
    // @name is used for drawing the zoom window during zooming operations.
    FPositionedLayer: TPositionedLayer;
    // @name is set to the mouse X coordinate in @link(BeginZoom).
    // See @link(FEndX).
    FStartX: Integer;
    // @name is set to the mouse Y coordinate in @link(BeginZoom).
    // See @link(FEndY).
    FStartY: Integer;
    // @name is set to the mouse X coordinate in @link(SetPanning).
    FStartXPosition: integer;
    // @name is set to the mouse Y coordinate in @link(SetPanning).
    FStartYPosition: integer;
    // @name is set to the mouse X coordinate in @link(Pan).
    FStopXPosition: integer;
    // @name is set to the mouse Y coordinate in @link(Pan).
    FStopYPosition: integer;
    // See @link(VerticalDirection).
    FVerticalDirection: TVerticalDirection;
    // @name is @true when a zooming operation is underway.
    FZooming: boolean;
    FOnMagnificationChanged: TNotifyEvent;
    // @name returns true if @link(Magnification)
    // is less than the maximum magnification.
    function GetCanZoomIn: boolean;
    // @name returns true if @link(Magnification)
    // is greater than the minimum magnification.
    function GetCanZoomOut: boolean;
    // @name is responsible for painting the zoom window during zooming.
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    // @name is the event handler for @link(TzbImage32.OnZbMove).
    // If @link(FZooming) is @true, @link(FEndX) and @link(FEndY)
    // are updated and @link(Image32) is redrawn.
    procedure PanMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // @name is the event handler for @link(TzbImage32.OnZbUp).
    // If @link(FZooming) is @true, the zooming operation is either
    // aborted or finished. See @link(AbortZoom) and @link(FinishZoom).
    // It is aborted if the mouse is not over the @link(TQRbwZoomBox2).
    procedure PanUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    // See @link(Exaggeration).
    procedure SetExaggeration(const Value: real);
    // See @link(ExaggerationDirection).
    procedure SetExaggerationDirection(const Value: TExaggerationDirection);
    // See @link(HorizontalDirection).
    procedure SetHorizontalDirection(const Value: THorizontalDirection);
    // See @link(ImmediateResize).
    procedure SetImmediateResize(const Value: boolean);
    // See @link(Magnification).
    procedure SetMagnification(Value: real);
    // See @link(OriginX).
    procedure SetOriginX(const Value: real);
    // See @link(OriginY).
    procedure SetOriginY(const Value: real);
    // See @link(Panning).
    procedure SetPanning(const Value: boolean);
    // See @link(VerticalDirection).
    procedure SetVerticalDirection(const Value: TVerticalDirection);
    // @name changes Passed to @True so that a mouse down event in
    // @link(FPositionedLayer) will always succeed.
    procedure ZoomBoxHitTest(Sender: TObject; X, Y: Integer;
      var Passed: Boolean);
  protected
    {If @link(BeginZoom) has been called, and neither @link(AbortZoom)
    nor @link(FinishZoom) have
    yet been called, @name will invalidate @link(Image32) so that
    the zoome window will be drawn.
    One of the corners of the shape will be at the location
    where the zoom began. The other will be at X, and Y.
    X and Y are screen coordinates in the @link(Image32)
    in the @link(TQrbwZoomBox2). You can get X and Y in OnMouseMove.
    @name is called in the private method @link(PanMove).}
    procedure ContinueZoom(X, Y: Integer);
    {@name terminates a zoom operation.
    X, and Y are the X and Y screen
    coordinates of the @link(Image32) in the @link(TQrbwZoomBox2).
    You can get X and Y in
    OnMouseUp. FinishZoom sets the zoom level so that
    the area outlined by the X, and Y parameters in BeginZoom and the
    X, and Y parameters in FinishZoom is displayed in the @link(TQrbwZoomBox2). }
    procedure FinishZoom(X, Y: Integer);
    // @name sets the Parent, Align, and Anchors properties of
    // @link(Image32).
    // It also assigns event handlers for @link(TzbImage32.OnZbMove) and
    // @link(TzbImage32.OnZbUp).
    procedure InitWidget;
    {@name calls the inherited @name and then calls @link(Pan).}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {@name calls the inherited @name and then calls @link(Pan).}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    { If @link(Panning) is true, @link(FStopXPosition)
      and @link(FStopYPosition) are moved to follow the mouse.  Then
      @link(OnPan) is called.
    }
    procedure Pan; virtual;
    // If @link(ImmediateResize) is @true, @name causes
    // @link(Image32)
    // to be resized to fit ClientWidth and ClientHeight.
    procedure Resize; override;
  public
    {If for some reason you want to terminate a zoom operation without doing
    the zoom, call @name instead of @link(FinishZoom). @name is called from
    @link(PanUp).}
    procedure AbortZoom;
    {@name starts a zooming operation. X, and Y are the X and Y screen
    coordinates of the @link(Image32) in the @link(TQrbwZoomBox2).
    You can get those in OnMouseDown.}
    procedure BeginZoom(X, Y: Integer);
    // Read @name to determine whether it is possible to zoom in.
    property CanZoomIn: boolean read GetCanZoomIn;
    // Read @name to determine whether it is possible to zoom out.
    property CanZoomOut: boolean read GetCanZoomOut;
    // @name creates an instance of @link(TQRbwZoomBox2).
    constructor Create(AOwner: TComponent); override;
    // @name checks that @link(Image32)
    // can be drawn and, if so, calls Invalidate
    // for it.
    procedure InvalidateImage32;
    // Set @name to @true to start a panning operation.
    // Set @name to @false to stop a panning operation.
    // See @link(Pan) for what happens during a panning operation.
    property Panning: boolean read FPanning write SetPanning;
    {X converts a screen coordinate into a real-number X coordinate.}
    function X(XCoord: integer): extended;
    {XCoord converts a real-number X coordinate into a screen coordinate.}
    function XCoord(X: extended): integer;
    {Y converts a screen coordinate into a real-number Y coordinate.}
    function Y(YCoord: integer): extended;
    {YCoord converts a real-number Y coordinate into a screen coordinate.}
    function YCoord(Y: extended): integer;
    // @name increase the magnification by Factor by calling
    // @link(ZoomByAt) and using the center of the @link(TQRbwZoomBox2)
    // as (CenterX, CenterY).
    procedure ZoomBy(Factor: extended);
    // @name changes the magnification by Factor and then adjusts
    // @link(OriginX) and @link(OriginY) so that the point represented
    // by the screen coordinates (CenterX, CenterY) will still have
    // screen coordinates (CenterX, CenterY).  For example, suppose there
    // is a point with real-world coordinates of (1,1) and screen coordinates
    // of (100,100).  If the OnMouseUp event was used to call ZoomByAt
    // with (CenterX,CenterY) = (100, 100), the point will still have screen
    // coordinates of (100, 100) after applying ZoomByAt.
    procedure ZoomByAt(Factor: extended; CenterX, CenterY: integer);
  published
    // @name is used to allow either vertical or horizontal exaggeration.
    // @name affects how screen coordinates map to real-world coordinates.
    // See @link(ExaggerationDirection).
    property Exaggeration: real read FExaggeration write SetExaggeration;
    // @name controls the direction in which @link(Exaggeration) is applied.
    property ExaggerationDirection: TExaggerationDirection
      read FExaggerationDirection write SetExaggerationDirection default
      edVertical;
    { @name indicates the direction in which the X-coordinates increase.
      hdRight indicates they increase towards the right,
      hdLeft indicates they increase towards the left.}
    property HorizontalDirection: THorizontalDirection
      read FHorizontalDirection write SetHorizontalDirection;
    // @name is used for the actual drawing in @classname
    property Image32: TzbImage32 read FImage32;
    // See @link(Resize).
    property ImmediateResize: boolean read FImmediateResize write SetImmediateResize;
    // @name is the ratio of distances in screen coordinates to
    // corresponding distances in real-world coordinates.
    // Usually @name is not set directly. See @link(BeginZoom),
    // @link(FinishZoom), @link(ZoomBy) and @link(ZoomByAt).
    property Magnification: real read FMagnification write SetMagnification;
    // @name is the screen X-coordinate that corresponds to a
    // real-world X-coordinate of zero.
    property OriginX: real read FOriginX write SetOriginX;
    // @name is the screen Y-coordinate that corresponds to a
    // real-world Y-coordinate of zero.
    property OriginY: real read FOriginY write SetOriginY;
    { @name indicates the direction in which the Y-coordinates increase.
      vdDown indicates they increase downward,
      vdUp indicates they increase upward.}
    property VerticalDirection: TVerticalDirection read FVerticalDirection write
      SetVerticalDirection;
    // @name is called during panning operations to indicate the
    // amount of panning.  See @link(Pan).
    property OnPan: TPanEvent read FOnPan write FOnPan;
    property OnMagnificationChanged: TNotifyEvent
      read FOnMagnificationChanged write FOnMagnificationChanged;
  end;

// @name registers @link(TQRbwZoomBox2).
procedure Register;

implementation

{ $R QZoomBox2.dcr}

const
  // When the magnification gets outside the range or
  // MinMagnification..MaxMagnification,
  // you start to get errors due to insufficient
  // precision in doubles.
  MaxMagnification : real = 200000;
  MinMagnification : real = 1/200000000;

procedure Register;
begin
  RegisterComponents('RBW', [TQRbwZoomBox2]);
end;

{ TQRbwZoomBox2 }

procedure TQRbwZoomBox2.AbortZoom;
begin
  FZooming := False;
  Screen.Cursor := crDefault;
  Invalidate;
end;

procedure TQRbwZoomBox2.BeginZoom(X, Y: Integer);
begin
  FPositionedLayer.BringToFront;
  FStartX := X;
  FStartY := Y;
  FEndX := FStartX;
  FEndY := FStartY;
  Screen.Cursor := crCross;
  FZooming := True;
end;

procedure TQRbwZoomBox2.ContinueZoom(X, Y: Integer);
begin
  if FZooming then
  begin
    FEndX := X;
    FEndY := Y;
    Image32.Invalidate;
  end;
end;

procedure TQRbwZoomBox2.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  Polygon : TPolygon32;
  LowX, HighX, LowY, HighY: integer;
begin

  if FZooming then
  begin
    Buffer.BeginUpdate;
      try
        Polygon := TPolygon32.Create;
        try
          Polygon.Add(FixedPoint(FStartX, FStartY));
          Polygon.Add(FixedPoint(FStartX, FEndY));
          Polygon.Add(FixedPoint(FEndX, FEndY));
          Polygon.Add(FixedPoint(FEndX, FStartY));
          Polygon.Add(FixedPoint(FStartX, FStartY));

          Polygon.DrawEdge(Buffer, clBlack32);

          Polygon.Clear;
          if FStartX < FEndX then
          begin
            LowX := FStartX;
            HighX := FEndX;
          end
          else
          begin
            LowX := FEndX;
            HighX := FStartX;
          end;
          if FStartY < FEndY then
          begin
            LowY := FStartY;
            HighY := FEndY;
          end
          else
          begin
            LowY := FEndY;
            HighY := FStartY;
          end;

          Polygon.Add(FixedPoint(LowX - 1, LowY - 1));
          Polygon.Add(FixedPoint(LowX - 1, HighY + 1));
          Polygon.Add(FixedPoint(HighX + 1, HighY + 1));
          Polygon.Add(FixedPoint(HighX + 1, LowY - 1));
          Polygon.Add(FixedPoint(LowX - 1, LowY - 1));

          Polygon.NewLine;

          Polygon.Add(FixedPoint(LowX + 1, LowY + 1));
          Polygon.Add(FixedPoint(LowX + 1, HighY - 1));
          Polygon.Add(FixedPoint(HighX - 1, HighY - 1));
          Polygon.Add(FixedPoint(HighX - 1, LowY + 1));
          Polygon.Add(FixedPoint(LowX + 1, LowY + 1));

          Polygon.DrawEdge(Buffer, clWhite32);
        finally
          Polygon.Free;
        end;
      finally
        Buffer.EndUpdate;
      end;  
  end;
end;

procedure TQRbwZoomBox2.ZoomBoxHitTest(Sender: TObject; X, Y: Integer; var Passed: Boolean);
begin
  Passed := True;
end;

constructor TQRbwZoomBox2.Create(AOwner: TComponent);
begin
  inherited;
  OriginX := 0;
  OriginY := 0;
  FMagnification := 1;
  FExaggeration := 1;
  FVerticalDirection := vdUp;

  FImage32 := TzbImage32.Create(self);
  FImage32.SetSubComponent(True);
  FImage32.Name := 'Image32';
  FImage32.Align := alClient;
  FImage32.Color := clWhite;

  // Image32.Layers is a TCollection.
  // You can have it hold different sorts of TCustomLayer
  // by passing it a descendent of TCustomLayer.
  // Here we use TPositionedLayer.
  FPositionedLayer := Image32.Layers.Add(TPositionedLayer) as TPositionedLayer;
  // Assign an event handler for the OnPaint event.
  FPositionedLayer.OnPaint := PaintLayer;
  // Set the location of the TPositionedLayer

  // Tell the layer to respond to mouse events
  FPositionedLayer.LayerOptions := FPositionedLayer.LayerOptions or LOB_MOUSE_EVENTS;

  // ZoomBoxHitTest means that the hit test always says the hit test
  // succeeds.
  FPositionedLayer.OnHitTest := ZoomBoxHitTest;

  FImmediateResize := True;

  InitWidget;
end;

procedure TQRbwZoomBox2.FinishZoom(X, Y: Integer);
var
  XMult, YMult, LocalMult: Extended;
  LeftX, RightX, TopY, BottomY: integer;
  CenterX, CenterY: integer;
  RealCenterX, RealCenterY: real;
  NewRealCenterX, NewRealCenterY: real;
  Delta: real;
begin
  Screen.Cursor := crDefault;
  FZooming := False;
  FEndX := X;
  FEndY := Y;

  if FStartX < FEndX then
  begin
    LeftX := FStartX;
    RightX := FEndX;
  end
  else
  begin
    LeftX := FEndX;
    RightX := FStartX;
  end;
  if FStartY < FEndY then
  begin
    TopY := FStartY;
    BottomY := FEndY;
  end
  else
  begin
    TopY := FEndY;
    BottomY := FStartY;
  end;

  if (RightX > LeftX) and (BottomY > TopY) then
  begin
    XMult := ClientWidth / (RightX - LeftX);
    YMult := ClientHeight / (BottomY - TopY);
    if XMult > YMult then
    begin
      LocalMult := YMult;
    end
    else
    begin
      LocalMult := XMult;
    end;

    CenterX := (RightX + LeftX) div 2;
    CenterY := (BottomY + TopY) div 2;
    RealCenterX := self.X(CenterX);
    RealCenterY := self.Y(CenterY);

    ZoomByAt(LocalMult, CenterX, CenterY);

    NewRealCenterX := self.X(ClientWidth div 2);
    NewRealCenterY := self.Y(ClientHeight div 2);

    Delta := NewRealCenterX - RealCenterX;
    FOriginX := OriginX - Delta;
    Delta := NewRealCenterY - RealCenterY;
    FOriginY := OriginY - Delta;
    InvalidateImage32;
  end;
end;

function TQRbwZoomBox2.GetCanZoomIn: boolean;
begin
  result := Magnification < MaxMagnification;
end;

function TQRbwZoomBox2.GetCanZoomOut: boolean;
begin
  result := Magnification > MinMagnification;
end;

procedure TQRbwZoomBox2.InitWidget;
begin
  inherited;
  if FImage32 <> nil then
  begin
    FImage32.Parent := self;
    FImage32.Align := alClient;
    FImage32.Align := alNone;
    FImage32.Anchors := [akLeft, akBottom];
  end;

  if FImage32 <> nil then
  begin
    FImage32.OnZbMove := PanMove;
    FImage32.OnZbUp := PanUp;
  end;

end;

procedure TQRbwZoomBox2.InvalidateImage32;
begin
  if (Image32 <> nil) and (Image32.Parent <> nil) then
  begin
    Image32.Invalidate;
  end;
end;

procedure TQRbwZoomBox2.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Pan;
end;

procedure TQRbwZoomBox2.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  Pan;
end;

procedure TQRbwZoomBox2.Pan;
var
  DeltaX: real;
  DeltaY: real;
begin
  if Panning then
  begin
    FStopXPosition := Mouse.CursorPos.x;
    FStopYPosition := Mouse.CursorPos.y;

    DeltaX := X(FStopXPosition) - X(FStartXPosition);
    DeltaY := Y(FStopYPosition) - Y(FStartYPosition);

    if Assigned(OnPan) then
    begin
      OnPan(self, DeltaX, DeltaY);
    end;
  end;
end;

procedure TQRbwZoomBox2.PanMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Pan;
  if FZooming then
  begin
    if (X < 0)
      or (X > Width)
      or (Y < 0)
      or (Y > Height) then
    begin
      Screen.Cursor := crDefault;
    end
    else
    begin
      Screen.Cursor := crCross;
      ContinueZoom(X, Y);
    end;
  end;

end;

procedure TQRbwZoomBox2.PanUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Pan;
  if FZooming then
  begin
    if (X < 0)
      or (X > Width)
      or (Y < 0)
      or (Y > Height) then
    begin
      AbortZoom;
    end
    else
    begin
      FinishZoom(X, Y);
    end;
  end;
end;

procedure TQRbwZoomBox2.Resize;
begin
  inherited;
  if FImmediateResize then
  begin
    Image32.Width := Image32.Parent.ClientWidth;
    Image32.Height := Image32.Parent.ClientHeight;
    Image32.Top := 0;
    Image32.Left := 0;
    InvalidateImage32;
  end;
end;

procedure TQRbwZoomBox2.SetExaggeration(const Value: real);
begin
  if FExaggeration <> Value then
  begin
    FExaggeration := Value;
    InvalidateImage32;
  end;
end;

procedure TQRbwZoomBox2.SetExaggerationDirection(
  const Value: TExaggerationDirection);
begin
  if FExaggerationDirection <> Value then
  begin
    FExaggerationDirection := Value;
    InvalidateImage32;
  end;
end;

procedure TQRbwZoomBox2.SetHorizontalDirection(
  const Value: THorizontalDirection);
begin
  if FHorizontalDirection <> Value then
  begin
    FHorizontalDirection := Value;
    InvalidateImage32;
  end;
end;

procedure TQRbwZoomBox2.SetImmediateResize(const Value: boolean);
begin
  FImmediateResize := Value;
  if FImmediateResize then
  begin
    Resize;
  end;
end;

procedure TQRbwZoomBox2.SetMagnification(Value: real);
begin
  if Value > MaxMagnification then
  begin
    Value := MaxMagnification
  end;
  if Value < MinMagnification then
  begin
    Value := MinMagnification
  end;
  if FMagnification <> Value then
  begin
    FMagnification := Value;
    InvalidateImage32;
    if Assigned(OnMagnificationChanged) then
    begin
      OnMagnificationChanged(self);
    end;
  end;
end;

procedure TQRbwZoomBox2.SetOriginX(const Value: real);
begin
  if FOriginX <> Value then
  begin
    FOriginX := Value;
    InvalidateImage32;
  end;
end;

procedure TQRbwZoomBox2.SetOriginY(const Value: real);
begin
  if FOriginY <> Value then
  begin
    FOriginY := Value;
    InvalidateImage32;
  end;
end;

procedure TQRbwZoomBox2.SetPanning(const Value: boolean);
var
  Delta: real;
begin
  if FPanning <> Value then
  begin
    if Value then
    begin
      FStartXPosition := Mouse.CursorPos.X;
      FStartYPosition := Mouse.CursorPos.Y;
    end
    else
    begin
      Delta := X(FStopXPosition) - X(FStartXPosition);
      FOriginX := OriginX - Delta;
      Delta := Y(FStopYPosition) - Y(FStartYPosition);
      FOriginY := OriginY - Delta;
      InvalidateImage32;
    end;
    FPanning := Value;
  end;
end;

procedure TQRbwZoomBox2.SetVerticalDirection(
  const Value: TVerticalDirection);
begin
  if FVerticalDirection <> Value then
  begin
    FVerticalDirection := Value;
    InvalidateImage32;
  end;
end;

function TQRbwZoomBox2.X(XCoord: integer): extended;
begin
  if HorizontalDirection = hdLeft then
  begin
    XCoord := ClientWidth - XCoord;
  end;
  if ExaggerationDirection = edHorizontal then
  begin
    result := XCoord / Magnification / FExaggeration + OriginX;
  end
  else
  begin
    result := XCoord / Magnification + OriginX;
  end;
end;

function TQRbwZoomBox2.XCoord(X: extended): integer;
var
  temp: extended;
begin
  try
    if ExaggerationDirection = edHorizontal then
    begin
      temp := (X - OriginX) * FExaggeration * Magnification;
    end
    else
    begin
      temp := (X - OriginX) * Magnification;
    end;
    if HorizontalDirection = hdLeft then
    begin
      temp := ClientWidth - temp;
    end;
    if temp > High(Integer) then
    begin
      result := High(Integer);
    end
    else if temp < Low(Integer) then
    begin
      result := Low(Integer);
    end
    else
    begin
      result := Round(temp);
    end;
  except
    on EOverflow do
    begin
      if X > OriginX then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
    on EInvalidOp do
    begin
      if X > OriginX then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
  end;
end;

function TQRbwZoomBox2.Y(YCoord: integer): extended;
begin
  if VerticalDirection = vdUp then
  begin
    YCoord := ClientHeight - YCoord;
  end;

  if ExaggerationDirection = edVertical then
  begin
    result := YCoord / Magnification / FExaggeration + OriginY;
  end
  else
  begin
    result := YCoord / Magnification + OriginY;
  end;
end;

function TQRbwZoomBox2.YCoord(Y: extended): integer;
var
  temp: extended;
begin
  try
    if ExaggerationDirection = edVertical then
    begin
      temp := (Y - OriginY) * FExaggeration * Magnification
    end
    else
    begin
      temp := (Y - OriginY) * Magnification;
    end;
    if VerticalDirection = vdUp then
    begin
      temp := ClientHeight - temp;
    end;
    if temp > High(Integer) then
    begin
      result := High(Integer);
    end
    else if temp < Low(Integer) then
    begin
      result := Low(Integer);
    end
    else
    begin
      result := Round(temp);
    end;
  except on EOverflow do
    begin
      if Y > OriginY then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
    on EInvalidOp do
    begin
      if Y > OriginY then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
  end;
end;

procedure TQRbwZoomBox2.ZoomBy(Factor: extended);
begin
  ZoomByAt(Factor, ClientWidth div 2, ClientHeight div 2);
end;

procedure TQRbwZoomBox2.ZoomByAt(Factor: extended; CenterX,
  CenterY: integer);
var
  NewMagnification: real;
  AnX: real;
  AY: real;
  NewOriginX: real;
  NewOriginY: real;
begin
  if (Factor <= 0) or (Factor = 1) then
  begin
    Exit;
  end;
  NewMagnification := Magnification * Factor;
  if NewMagnification > MaxMagnification then
  begin
    if Magnification = MaxMagnification then
    begin
      Exit;
    end;
    NewMagnification := MaxMagnification;
    Factor := NewMagnification/Magnification;
  end;
  if NewMagnification < MinMagnification then
  begin
    if Magnification = MinMagnification then
    begin
      Exit;
    end;
    NewMagnification := MinMagnification;
    Factor := NewMagnification/Magnification;
  end;
  AnX := X(CenterX);
  AY := Y(CenterY);
  NewOriginX := AnX - (AnX - OriginX) / Factor;
  NewOriginY := AY - (AY - OriginY) / Factor;
  FOriginX := NewOriginX;
  FOriginY := NewOriginY;
  Magnification := NewMagnification;
//  InvalidateImage32;
end;


{ TZbImage32 }

procedure TZbImage32.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if assigned(OnZbMove) then
  begin
    OnZbMove(self, Shift, X, Y);
  end;
end;

procedure TZbImage32.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if assigned(OnZbUp) then
  begin
    OnZbUp(self, Button, Shift, X, Y);
  end;
  inherited;

end;

end.

