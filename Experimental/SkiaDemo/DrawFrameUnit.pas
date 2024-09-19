unit DrawFrameUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Skia, FMX.Skia, FastGeo;

type
  // @name indicates whether exaggeration is applied in
  // a vertical or horizontal direction.
  // See TQRbwZoomBox2.@link(TQRbwZoomBox2.ExaggerationDirection).
  TExaggerationDirection = (edVertical, edHorizontal);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.VerticalDirection).}
  TVerticalDirection = (vdDown, vdUp);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.HorizontalDirection).}
//  THorizontalDirection = (hdRight, hdLeft);


  TDrawFrame = class(TFrame)
    SkPaintBox1: TSkPaintBox;
  private
//    FHorizontalDirection: THorizontalDirection;
    FVerticalDirection: TVerticalDirection;
    FExaggerationDirection: TExaggerationDirection;
    FMagnification: double;
    FExaggeration: Double;
    FMove: TPointF;
//    procedure SetHorizontalDirection(const Value: THorizontalDirection);
    procedure SetVerticalDirection(const Value: TVerticalDirection);
    procedure SetExaggerationDirection(const Value: TExaggerationDirection);
    procedure SetExaggeration(const Value: Double);
    procedure SetMagnification(const Value: double);
    procedure SetMove(const Value: TPointF);
    { Private declarations }
  public
    {X converts a screen coordinate into a real-number X coordinate.}
    function X(XCoord: single): extended;
    {XCoord converts a real-number X coordinate into a screen coordinate.}
    function XCoord(X: extended): single;
    {Y converts a screen coordinate into a real-number Y coordinate.}
    function Y(YCoord: single): extended;
    {YCoord converts a real-number Y coordinate into a screen coordinate.}
    function YCoord(Y: extended): single;
    function ScreenToReal(APoint: TPoint2D): TPoint2D;
    function RealToScreen(APoint: TPoint2D): TPoint2D;
    // @name is the ratio of distances in screen coordinates to
    // corresponding distances in real-world coordinates.
    // Usually @name is not set directly. See @link(BeginZoom),
    // @link(FinishZoom), @link(ZoomBy) and @link(ZoomByAt).
    property Magnification: double read FMagnification write SetMagnification;
    // @name is used to allow either vertical or horizontal exaggeration.
    // @name affects how screen coordinates map to real-world coordinates.
    // See @link(ExaggerationDirection).
    property Exaggeration: Double read FExaggeration write SetExaggeration;
    // @name controls the direction in which @link(Exaggeration) is applied.
    { Public declarations }
    property ModelMove: TPointF read FMove write SetMove;
  published
    property ExaggerationDirection: TExaggerationDirection
      read FExaggerationDirection write SetExaggerationDirection default
      edVertical;
    { @name indicates the direction in which the X-coordinates increase.
      hdRight indicates they increase towards the right,
      hdLeft indicates they increase towards the left.}
//    property HorizontalDirection: THorizontalDirection
//      read FHorizontalDirection write SetHorizontalDirection;
    { @name indicates the direction in which the Y-coordinates increase.
      vdDown indicates they increase downward,
      vdUp indicates they increase upward.}
    property VerticalDirection: TVerticalDirection read FVerticalDirection write
      SetVerticalDirection;
  end;

implementation

{$R *.fmx}



{ TDrawFrame }

function TDrawFrame.RealToScreen(APoint: TPoint2D): TPoint2D;
begin
  result.x := XCoord(APoint.x);
  result.y := YCoord(APoint.y);
end;

function TDrawFrame.ScreenToReal(APoint: TPoint2D): TPoint2D;
begin
  result.x := X(APoint.x);
  result.y := Y(APoint.y);
end;

procedure TDrawFrame.SetExaggeration(const Value: Double);
begin
  FExaggeration := Value;
end;

procedure TDrawFrame.SetExaggerationDirection(
  const Value: TExaggerationDirection);
begin
  FExaggerationDirection := Value;
end;

//procedure TDrawFrame.SetHorizontalDirection(const Value: THorizontalDirection);
//begin
//  FHorizontalDirection := Value;
//end;

procedure TDrawFrame.SetMagnification(const Value: double);
begin
  FMagnification := Value;
end;

procedure TDrawFrame.SetMove(const Value: TPointF);
begin
  FMove := Value;
end;

procedure TDrawFrame.SetVerticalDirection(const Value: TVerticalDirection);
begin
  FVerticalDirection := Value;
end;

function TDrawFrame.X(XCoord: single): extended;
begin
//  if HorizontalDirection = hdLeft then
//  begin
//    XCoord := SkPaintBox1.Width - XCoord;
//  end;
  if ExaggerationDirection = edHorizontal then
  begin
    result := (XCoord-FMove.X)/Magnification/ FExaggeration
  end
  else
  begin
    result := (XCoord-FMove.X)/Magnification
  end;
end;

function TDrawFrame.XCoord(X: extended): single;
begin
  if ExaggerationDirection = edHorizontal then
  begin
    result := X * FExaggeration  *Magnification + FMove.X;
  end
  else
  begin
    result := x*Magnification + FMove.X;
  end;
end;

function TDrawFrame.Y(YCoord: single): extended;
begin
  if ExaggerationDirection = edVertical then
  begin
    result := (-YCoord+FMove.Y)/Magnification/FExaggeration;
  end
  else
  begin
    result := (-YCoord+FMove.Y)/Magnification;
  end;
end;

function TDrawFrame.YCoord(Y: extended): single;
begin
  if ExaggerationDirection = edVertical then
  begin
    result := -(Y * FExaggeration * Magnification - FMove.Y);
  end
  else
  begin
    result := -(Y * Magnification - FMove.Y);
  end;
end;

end.
