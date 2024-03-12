{@abstract(@name defines @link(TRbwModelCube).)}
unit RbwModelCube;

interface

uses
  System.UITypes, Types, SysUtils, Classes, Controls, ExtCtrls, Graphics;

type
  // See @link(TRbwModelCube.SelectedFace).
  TRbwFace = (faNone, faTop, faFront, faSide);
  // See @link(TRbwModelCube.XOrigin).
  TRbwXOrigin = (xoWest, xoEast);
  // See @link(TRbwModelCube.YOrigin).
  TRbwYOrigin = (yoSouth, yoNorth);
  // See @link(TRbwModelCube.ZOrigin).
  TRbwZOrigin = (zoBottom, zoTop);
  // See @link(TRbwModelCube.ClickDirection).
  TRbwClickDirection = (cdNone, cdDown, cdUp, cdWest, cdEast, cdNorth, cdSouth);

  TBreakPosition = class(TCollectionItem)
  private
    FLowerFraction: double;
    FHigherFraction: double;
    procedure SetLowerFraction(const Value: double);
    procedure SetHigherFraction(const Value: double);
  published
    property LowerFraction: double read FLowerFraction write SetLowerFraction;
    property HigherFraction: double read FHigherFraction write SetHigherFraction;
  end;

  TBreakCollection = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TBreakPosition;
    procedure SetItem(Index: Integer; const Value: TBreakPosition);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    function Add: TBreakPosition;
    procedure Sort;
    property Items[Index: Integer]: TBreakPosition read GetItem
      write SetItem; default;
  end;

type
  { @abstract(@name) is a TPaintBox that draws
    a block on itself.) Typically, the block is drawn as
    if all the sides were the same length so the block is a
    cube. The cube can be drawn as either transparent or opaque.
    A selected area can be drawn on the cube.
    (See @link(TRbwModelCube.Selection1)
    and @link(TRbwModelCube.Selection2).) If the user clicks on the cube,
    @link(TRbwModelCube.ClickDirection)
    can be used to determine where the user
    clicked with respect to the selected area.}
  TRbwModelCube = class(TPaintBox)
  private
    // @name : integer;
    // The bottom edge of the front of the cube
    FBottomY : integer;
    // @name : boolean;
    // See @link(CanClickFace)
    FCanClickFace: boolean;
    // @name : double;
    // @name is the Fraction of the distance between the front
    // of the cube and vanishing point at which
    // the back of the cube is located.
    // See @link(CubeFraction).
    FFraction : double;
    // @name : integer;
    // left edge of the front of the cube
    FLeftX : integer;
    // @name : integer;
    // @name is set to the mouse X coordinate in @link(MouseUp).
    FMouseX : integer;
    // @name : integer;
    // @name is set to the mouse Y coordinate in @link(MouseUp).
    FMouseY : integer;
    // @name : TNotifyEvent;
    // See @link(OnSelectFace).
    FOnSelectFace: TNotifyEvent;
    // @name : boolean;
    // See @link(Opaque).
    FOpaque: boolean;
    // @name : integer;
    // right edge of the front of the cube
    FRightX : integer;
    // @name : @link(TRbwFace);
    // See @link(SelectedFace).
    FSelectedFace: TRbwFace;
    // @name : double;
    // See @link(Selection1).
    FSelection1: double;
    // @name : double;
    // See @link(Selection2).
    FSelection2: double;
    // @name : boolean;
    // See @link(ShowSelection).
    FShowSelection: boolean;
    // @name : integer;
    // top edge of the front of the cube
    FTopY : integer;
    // @name : integer;
    // X coordinate of the vanishing point
    FVanishingPointX : integer;
    // @name : integer;
    // Y coordinate of the vanishing point
    FVanishingPointY : integer;
    // @name : TRbwXOrigin;
    // See @link(XOrigin).
    FXOrigin: TRbwXOrigin;
    // @name : FYOrigin;
    // See @link(YOrigin).
    FYOrigin: TRbwYOrigin;
    // @name : TRbwZOrigin;
    // See @link(ZOrigin).
    FZOrigin: TRbwZOrigin;
    FSelectionColor: TColor;
    FBreaks: TBreakCollection;
    // See @link(CanClickFace).
    procedure SetCanClickFace(const Value: boolean);
    // See @link(SelectedFace).
    procedure SetSelectedFace(const Value: TRbwFace);
    // See @link(Selection1).
    procedure SetSelection1(const Value: double);
    // See @link(Selection2).
    procedure SetSelection2(const Value: double);
    // See @link(ShowSelection).
    procedure SetShowSelection(const Value: boolean);
    // See @link(Opaque).
    procedure SetOpaque(const Value: boolean);
    // See @link(XOrigin).
    procedure SetXOrigin(const Value: TRbwXOrigin);
    // See @link(YOrigin).
    procedure SetYOrigin(const Value: TRbwYOrigin);
    // See @link(ZOrigin).
    procedure SetZOrigin(const Value: TRbwZOrigin);
    // See @link(CubeFraction).
    procedure SetFraction(const Value: double);
    // See @link(CubeLeftX).
    procedure SetLeftX(const Value: integer);
    // See @link(CubeTopY).
    procedure SetTopY(const Value: integer);
    // See @link(CubeVanishingPointX).
    procedure SetVanishingPointX(const Value: integer);
    // See @link(CubeVanishingPointY).
    procedure SetVanishingPointY(const Value: integer);
    // See @link(CubeHeight).
    function GetCubeHeight: integer;
    // See @link(CubeWidth).
    function GetCubeWidth: integer;
    // See @link(CubeHeight).
    procedure SetCubeHeight(const Value: integer);
    // See @link(CubeWidth).
    procedure SetCubeWidth(const Value: integer);
    procedure SetSelectionColor(const Value: TColor);
    procedure DrawSelection(BackTopY, BackBottomY, BackLeftX,
      BackRightX: Integer);
    procedure BreakChanged(Sender: TObject);
    { Private declarations }
  protected
    // If @link(CanClickFace) is true,
    // @name sets @link(SelectedFace) to the face on which
    // the user double-clicked.
    procedure DblClick; override;
    // @name draws a perspective drawing of a cube
    // with a selected
    // section indicated by @link(Selection1) and @link(Selection2).
    //
    // @link(SelectedFace) indicates the direction of the slice of the
    // cube that is selected.
    // faNone indicates it is undetermined.
    // faTop indicates it is parallel to the top surface.
    // faFront indicates it is parallel to the front surface.
    // faSide indicates it is parallel to the side surface.
    //
    // @link(Selection1) and @link(Selection2) give the fraction
    // of the side of the cube along
    // which the colored section is to be shown.
    // The origin is determined by @link(XOrigin), @link(YOrigin),
    // and @link(ZOrigin).
    // @link(Selection2) should be greater than or equal to
    // @link(Selection1).
    // Both @link(Selection1) and @link(Selection2) should be
    // greater than or equal to 0 and
    // less than or equal to 1.
    procedure DrawCube;
    // @name returns the face of the cube, if any,
    // on which the user clicked.  See @link(DblClick).
    function GetFaceOfCube: TRbwFace;
    // @name sets @link(FMouseX) and @link(FMouseY).
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    // @name calls @link(DrawCube).
    procedure Paint; override;
    { Protected declarations }
  public
    property Breaks: TBreakCollection read FBreaks;
    // The result of @name depend on @link(SelectedFace).
    //
    // If @link(SelectedFace) = faNone, @name returns cdNone.
    //
    // If @link(SelectedFace) = faTop,  @name returns cdDown, cdUp or cdNone
    // depending on whether the Point X,Y appear to be below, above, or between
    // Selection1 and Selection2.
    //
    // If @link(SelectedFace) = faFront,  @name returns cdNorth,
    // cdSouth or cdNone depending on whether the Point X,Y appear to be
    // behind, in front of, or between Selection1 and Selection2.
    //
    // If @link(SelectedFace) = faSide,  @name returns cdWest,
    // cdEast or cdNone depending on whether the Point X,Y appear to be
    // to the right of, to the left of, or between Selection1 and Selection2.
    function ClickDirection(const X, Y: integer): TRbwClickDirection;
    // @name creates and instance of @link(TRbwModelCube).
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    // If @name is true, the user can change @link(SelectedFace)
    // by double clicking on a face.
    property CanClickFace : boolean read FCanClickFace write SetCanClickFace;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clRed;
    // @name is the fraction of the distance between the front
    // of the cube and vanishing point at which
    // the back of the cube is located.
    //
    // See @link(CubeVanishingPointX) and @link(CubeVanishingPointY).
    property CubeFraction : double read FFraction write SetFraction;
    // @name is the height of the front face of the cube in pixels.
    property CubeHeight: integer read GetCubeHeight write SetCubeHeight;
    // @name is the X coordinate of the left side of
    // the front face of the cube in pixels.
    property CubeLeftX : integer read FLeftX write SetLeftX;
    // @name is the Y coordinate of the top side of
    // the front face of the cube in pixels.
    property CubeTopY : integer read FTopY write SetTopY;
    // @name and @link(CubeVanishingPointY) define a vanishing point.
    // The side faces of the cube are drawn by drawing lines
    // @link(CubeFraction) of the distance between the front face and the
    // vanishing point.
    property CubeVanishingPointX : integer read FVanishingPointX
      write SetVanishingPointX;
    // @link(CubeVanishingPointX) and @name define a vanishing point.
    // The side faces of the cube are drawn by drawing lines
    // @link(CubeFraction) of the distance between the front face and the
    // vanishing point.
    property CubeVanishingPointY : integer read FVanishingPointY
      write SetVanishingPointY;
    // @name is the width of the front face of the cube in pixels.
    property CubeWidth: integer read GetCubeWidth write SetCubeWidth;
    // If @name is true, only the outside of the cube is drawn.
    property Opaque : boolean read FOpaque write SetOpaque;
    // @name determines which face of the cube the selected area represents.
    // The selected area is drawn in red.
    //
    // faNone indicates it is undetermined.
    // faTop indicates it is parallel to the top surface.
    // faFront indicates it is parallel to the front surface.
    // faSide indicates it is parallel to the side surface.
    //
    // See @link(OnSelectFace).
    property SelectedFace : TRbwFace read FSelectedFace write SetSelectedFace;
    // @name and @link(Selection2) give the fraction
    // of the side of the cube along
    // which the colored section is to be shown.
    // The origin is determined by @link(XOrigin), @link(YOrigin),
    // and @link(ZOrigin).
    // @link(Selection2) should be greater than or equal to
    // @name.
    // Both @name and @link(Selection2) should be
    // greater than or equal to 0 and
    // less than or equal to 1.
    property Selection1 : double read FSelection1 write SetSelection1;
    // @link(Selection1) and @name give the fraction
    // of the side of the cube along
    // which the colored section is to be shown.
    // The origin is determined by @link(XOrigin), @link(YOrigin),
    // and @link(ZOrigin).
    // @name should be greater than or equal to
    // @link(Selection1).
    // Both @link(Selection1) and @name should be
    // greater than or equal to 0 and
    // less than or equal to 1.
    property Selection2 : double read FSelection2 write SetSelection2;
    // @name indicates whether the selected area should be drawn on not.
    property ShowSelection : boolean read FShowSelection write SetShowSelection;
    // @name indicates whether the origin of the cube is on the east or west.
    property XOrigin : TRbwXOrigin read FXOrigin write SetXOrigin;
    // @name indicates whether the origin of the cube is on the north or south.
    property YOrigin : TRbwYOrigin read FYOrigin write SetYOrigin;
    // @name indicates whether the origin of the cube is on the top or bottom.
    property ZOrigin : TRbwZOrigin read FZOrigin write SetZOrigin;
    // @name is called when @link(SelectedFace) is changed.
    property OnSelectFace : TNotifyEvent read FOnSelectFace
      write FOnSelectFace;
    { Published declarations }
  end;

procedure Register;

implementation

{#BACKUP *.ICO}

uses
  Math;

procedure Register;
begin
  RegisterComponents('RBW', [TRbwModelCube]);
end;

function Interpolate(LowValue, HighValue: integer; Frac : double) : integer;
begin
  result := Round(Frac*(HighValue - LowValue) + LowValue)
end;

function IsPointInside(const X, Y: integer;
  const PointArray: array of TPoint): boolean;
var
  VertexIndex : integer;
  Point1, Point2 : TPoint;
begin   // based on CACM 112
  result := false;
  if Length(PointArray) < 4 then
  begin
    Exit;
  end;

  Point1 := PointArray[0];
  Point2 := PointArray[Length(PointArray) -1];
  if (Point1.X <> Point2.X) or
    (Point1.Y <> Point2.Y) then
  begin
    Exit;
  end;

  For VertexIndex := 0 to Length(PointArray) -2 do
  begin
    Point1 := PointArray[VertexIndex];
    Point2 := PointArray[VertexIndex+1];
    if ((Y <= Point1.Y) = (Y > Point2.Y)) and
      (X - Point1.X - (Y - Point1.Y) *
      (Point2.X - Point1.X)/(Point2.Y - Point1.Y) < 0) then
    begin
      result := not result;
    end;
  end;
end;

{ TRbwModelCube }

procedure TRbwModelCube.BreakChanged(Sender: TObject);
begin
  Invalidate;
end;

function TRbwModelCube.ClickDirection(const X,
  Y: integer): TRbwClickDirection;
var
  LayerTop, LayerBottom: integer;
  LayerLeft, LayerRight: integer;
  BackLayerX, BackLayerY, FrontLayerX, FrontLayerY : integer;
  CubeDepthY, CubeDepthX : integer;
  BackLeftX, BackRightX, BackTopY, BackBottomY : integer;
  Point2X, Point3X, Point6X, Point7X: integer;
  Point3Y, Point4Y, Point7Y, Point8Y: integer;
begin
  case SelectedFace of
    faNone:
      begin
        result := cdNone
      end;
    faTop:
      begin
        if X <= FRightX then
        begin
          if ZOrigin = zoBottom then
          begin
            LayerTop := CubeTopY + CubeHeight - Round(Selection2*CubeHeight);
            LayerBottom := CubeTopY + CubeHeight - Round(Selection1*CubeHeight);
          end
          else
          begin
            LayerTop := CubeTopY + Round(Selection1*CubeHeight);
            LayerBottom := CubeTopY + Round(Selection2*CubeHeight);
          end;
        end
        else
        begin
          BackRightX := Interpolate(FRightX, FVanishingPointX, FFraction);
          BackTopY := Interpolate(FTopY, FVanishingPointY, FFraction);
          BackBottomY := Interpolate(FBottomY, FVanishingPointY, FFraction);

          if ZOrigin = zoBottom then
          begin
            Point4Y := Interpolate(FBottomY,FTopY,  Selection2);
            Point3Y := Interpolate(BackBottomY,BackTopY, Selection2);
          end
          else
          begin
            Point4Y := Interpolate(FBottomY,FTopY,  1-Selection2);
            Point3Y := Interpolate(BackBottomY,BackTopY, 1-Selection2);
          end;

          if ZOrigin = zoBottom then
          begin
            Point8Y := Interpolate(FBottomY,FTopY, Selection1);
            Point7Y := Interpolate(BackBottomY,BackTopY, Selection1);
          end
          else
          begin
            Point8Y := Interpolate(FBottomY,FTopY, 1-Selection1);
            Point7Y := Interpolate(BackBottomY,BackTopY, 1-Selection1);
          end;

          if ZOrigin = zoBottom then
          begin
            LayerTop := Interpolate(Point4Y, Point3Y, (X-FRightX)/(BackRightX-FRightX));
            LayerBottom := Interpolate(Point8Y, Point7Y, (X-FRightX)/(BackRightX-FRightX));
          end
          else
          begin
            LayerTop := Interpolate(Point8Y, Point7Y, (X-FRightX)/(BackRightX-FRightX));
            LayerBottom := Interpolate(Point4Y, Point3Y, (X-FRightX)/(BackRightX-FRightX));
          end;
        end;


        if Y >= LayerBottom then
        begin
          result := cdDown;
        end
        else if Y <= LayerTop then
        begin
          result := cdUp;
        end
        else
        begin
          result := cdNone
        end;
      end;
    faFront:
      begin
        CubeDepthY := Round(FFraction*(FTopY - FVanishingPointY));
        CubeDepthX := Round(FFraction*(FVanishingPointX - FRightX));
        if YOrigin = yoSouth then
        begin
          BackLayerY := FTopY  - Round(Selection2*CubeDepthY);
          FrontLayerY := FTopY - Round(Selection1*CubeDepthY);
          BackLayerX := FRightX + Round(Selection2*CubeDepthX);
          FrontLayerX := FRightX + Round(Selection1*CubeDepthX);
        end
        else
        begin
          BackLayerY := FTopY -CubeDepthY + Round(Selection2*CubeDepthY);
          FrontLayerY := FTopY -CubeDepthY + Round(Selection1*CubeDepthY);
          BackLayerX := FRightX +CubeDepthX - Round(Selection2*CubeDepthX);
          FrontLayerX := FRightX +CubeDepthX - Round(Selection1*CubeDepthX);
        end;

        if (Y <= BackLayerY) or (X >= BackLayerX) then
        begin
          result := cdNorth
        end
        else if (Y >= FrontLayerY) or (X <= FrontLayerX) then
        begin
          result := cdSouth;
        end
        else
        begin
          result := cdNone
        end;
      end;
    faSide:
      begin
        if Y > FTopY then
        begin
          if XOrigin = xoWest then
          begin
            LayerLeft := CubeLeftX + Round(Selection1*CubeWidth);
            LayerRight := CubeLeftX + Round(Selection2*CubeWidth);
          end
          else
          begin
            LayerLeft := CubeLeftX + CubeWidth - Round(Selection2*CubeWidth);
            LayerRight := CubeLeftX + CubeWidth - Round(Selection1*CubeWidth);
          end;
        end
        else
        begin
          BackLeftX := Interpolate(FLeftX, FVanishingPointX, FFraction);
          BackRightX := Interpolate(FRightX, FVanishingPointX, FFraction);

          if XOrigin = xoWest then
          begin
            Point2X := Interpolate(FLeftX, FRightX, Selection2);
          end
          else
          begin
            Point2X := Interpolate(FLeftX, FRightX, 1-Selection2);
          end;

          if XOrigin = xoWest then
          begin
            Point3X := Interpolate(BackLeftX, BackRightX, Selection2);
          end
          else
          begin
            Point3X := Interpolate(BackLeftX, BackRightX, 1-Selection2);
          end;

          if XOrigin = xoWest then
          begin
            Point6X := Interpolate(FLeftX, FRightX, Selection1);
          end
          else
          begin
            Point6X := Interpolate(FLeftX, FRightX, 1-Selection1);
          end;
          if XOrigin = xoWest then
          begin
            Point7X := Interpolate(BackLeftX, BackRightX, Selection1);
          end
          else
          begin
            Point7X := Interpolate(BackLeftX, BackRightX, 1-Selection1);
          end;

          if XOrigin = xoWest then
          begin
            LayerRight := Interpolate(Point3X, Point2X,
              (X-Point2X)/(Point3X-Point2X));
            LayerLeft  := Interpolate(Point7X, Point6X,
              (X-Point6X)/(Point7X-Point6X));
          end
          else
          begin
            LayerRight := Interpolate(Point6X, Point7X,
              (X-Point6X)/(Point7X-Point6X));
            LayerLeft  := Interpolate(Point2X, Point3X,
              (X-Point2X)/(Point3X-Point2X));
          end;


        end;

        if X >= LayerRight then
        begin
          result := cdEast;
        end
        else if X <= LayerLeft then
        begin
          result := cdWest;
        end
        else
        begin
          result := cdNone
        end;
      end;
  else
    begin
      result := cdNone;
      Assert(False);
    end;
  end;

end;

constructor TRbwModelCube.Create(AOwner: TComponent);
begin
  inherited;
  FBreaks := TBreakCollection.Create;
  FBreaks.OnChange := BreakChanged;
  FSelectionColor := clRed;
  Width := 49;
  Height := 52;
  SelectedFace := faTop;
  ShowSelection := True;
  FLeftX := 5;
  FRightX := 30;
  FTopY := 20;              // top edge of the front of the cube
  FBottomY := 45;           // bottom edge of the front of the cube
  FVanishingPointX := 118;  // X coordinate of the vanishing point
  FVanishingPointY := -40;  // Y coordinate of the vanishing point
  FFraction := 0.15;        // FFraction of the distance between the front
                           // of the cube and vanishing point at which
                           // the back of the cube is located.
end;

procedure TRbwModelCube.DblClick;
var
  AFace : TRbwFace;
begin
  inherited;
  if CanClickFace then
  begin
    AFace := GetFaceOfCube;
    if AFace <> faNone then
    begin
      SelectedFace := AFace;
    end;
  end;
end;

destructor TRbwModelCube.Destroy;
begin
  FBreaks.Free;
  inherited;
end;

procedure TRbwModelCube.DrawSelection(BackTopY, BackBottomY, BackLeftX,
  BackRightX: Integer);
var
  Point6: TPoint;
  Point5: TPoint;
  Point2: TPoint;
  Point1: TPoint;
  BrushColor: Integer;
  Point3: TPoint;
  Point7: TPoint;
  Point4: TPoint;
  Point8: TPoint;
begin
  if ShowSelection then
  begin
    BrushColor := Canvas.Brush.Color;
    try
      Canvas.Brush.Color := SelectionColor;
      case SelectedFace of
        faTop:
          // Top
          begin
            Point1.X := FLeftX;
            Point2.X := BackLeftX;
            Point3.X := BackRightX;
            Point4.X := FRightX;
            if ZOrigin = zoBottom then
            begin
              Point1.Y := Interpolate(FBottomY, FTopY, Selection2);
              Point2.Y := Interpolate(BackBottomY, BackTopY, Selection2);
            end
            else
            begin
              Point1.Y := Interpolate(FBottomY, FTopY, 1 - Selection2);
              Point2.Y := Interpolate(BackBottomY, BackTopY, 1 - Selection2);
            end;
            Point3.Y := Point2.Y;
            Point4.Y := Point1.Y;
            Point5.X := FLeftX;
            Point6.X := BackLeftX;
            Point7.X := BackRightX;
            Point8.X := FRightX;
            if ZOrigin = zoBottom then
            begin
              Point5.Y := Interpolate(FBottomY, FTopY, Selection1);
              Point6.Y := Interpolate(BackBottomY, BackTopY, Selection1);
            end
            else
            begin
              Point5.Y := Interpolate(FBottomY, FTopY, 1 - Selection1);
              Point6.Y := Interpolate(BackBottomY, BackTopY, 1 - Selection1);
            end;
            Point7.Y := Point6.Y;
            Point8.Y := Point5.Y;
            if not Opaque or ((ZOrigin = zoBottom) and (Selection2 = 1)) or ((ZOrigin = zoTop) and (Selection1 = 0)) then
            begin
              if (ZOrigin = zoBottom) then
              begin
                Canvas.Polygon([Point1, Point2, Point3, Point4]);
              end
              else
              begin
                Canvas.Polygon([Point5, Point6, Point7, Point8]);
              end;
            end;
            Canvas.Polygon([Point1, Point4, Point8, Point5]);
            Canvas.Polygon([Point3, Point7, Point8, Point4]);
          end;
        faFront:
          // Front
          begin
            if YOrigin = yoSouth then
            begin
              Point1.X := Interpolate(FLeftX, BackLeftX, Selection1);
            end
            else
            begin
              Point1.X := Interpolate(FLeftX, BackLeftX, 1 - Selection1);
            end;
            Point2.X := Point1.X;
            if YOrigin = yoSouth then
            begin
              Point3.X := Interpolate(FRightX, BackRightX, Selection1);
            end
            else
            begin
              Point3.X := Interpolate(FRightX, BackRightX, 1 - Selection1);
            end;
            Point4.X := Point3.X;
            if YOrigin = yoSouth then
            begin
              Point1.Y := Interpolate(FBottomY, BackBottomY, Selection1);
              Point2.Y := Interpolate(FTopY, BackTopY, Selection1);
            end
            else
            begin
              Point1.Y := Interpolate(FBottomY, BackBottomY, 1 - Selection1);
              Point2.Y := Interpolate(FTopY, BackTopY, 1 - Selection1);
            end;
            Point3.Y := Point2.Y;
            Point4.Y := Point1.Y;
            if YOrigin = yoSouth then
            begin
              Point5.X := Interpolate(FLeftX, BackLeftX, Selection2);
            end
            else
            begin
              Point5.X := Interpolate(FLeftX, BackLeftX, 1 - Selection2);
            end;
            Point6.X := Point5.X;
            if YOrigin = yoSouth then
            begin
              Point7.X := Interpolate(FRightX, BackRightX, Selection2);
            end
            else
            begin
              Point7.X := Interpolate(FRightX, BackRightX, 1 - Selection2);
            end;
            Point8.X := Point7.X;
            if YOrigin = yoSouth then
            begin
              Point5.Y := Interpolate(FBottomY, BackBottomY, Selection2);
              Point6.Y := Interpolate(FTopY, BackTopY, Selection2);
            end
            else
            begin
              Point5.Y := Interpolate(FBottomY, BackBottomY, 1 - Selection2);
              Point6.Y := Interpolate(FTopY, BackTopY, 1 - Selection2);
            end;
            Point7.Y := Point6.Y;
            Point8.Y := Point5.Y;
            if not Opaque or ((YOrigin = yoSouth) and (Selection1 = 0)) or ((YOrigin = yoNorth) and (Selection2 = 1)) then
            begin
              if YOrigin = yoSouth then
              begin
                Canvas.Polygon([Point1, Point2, Point3, Point4]);
              end
              else
              begin
                Canvas.Polygon([Point5, Point6, Point7, Point8]);
              end;
            end;
            Canvas.Polygon([Point2, Point3, Point7, Point6]);
            Canvas.Polygon([Point3, Point7, Point8, Point4]);
          end;
        faSide:
          // side
          begin
            if XOrigin = xoWest then
            begin
              Point1.X := Interpolate(FLeftX, FRightX, Selection2);
            end
            else
            begin
              Point1.X := Interpolate(FLeftX, FRightX, 1 - Selection2);
            end;
            Point2.X := Point1.X;
            if XOrigin = xoWest then
            begin
              Point3.X := Interpolate(BackLeftX, BackRightX, Selection2);
            end
            else
            begin
              Point3.X := Interpolate(BackLeftX, BackRightX, 1 - Selection2);
            end;
            Point4.X := Point3.X;
            Point1.Y := FBottomY;
            Point2.Y := FTopY;
            Point3.Y := BackTopY;
            Point4.Y := BackBottomY;
            if XOrigin = xoWest then
            begin
              Point5.X := Interpolate(FLeftX, FRightX, Selection1);
            end
            else
            begin
              Point5.X := Interpolate(FLeftX, FRightX, 1 - Selection1);
            end;
            Point6.X := Point5.X;
            if XOrigin = xoWest then
            begin
              Point7.X := Interpolate(BackLeftX, BackRightX, Selection1);
            end
            else
            begin
              Point7.X := Interpolate(BackLeftX, BackRightX, 1 - Selection1);
            end;
            Point8.X := Point7.X;
            Point5.Y := FBottomY;
            Point6.Y := FTopY;
            Point7.Y := BackTopY;
            Point8.Y := BackBottomY;
            if not Opaque or ((XOrigin = xoWest) and (Selection2 = 1)) or ((XOrigin = xoEast) and (Selection1 = 0)) then
            begin
              if (XOrigin = xoWest) then
              begin
                Canvas.Polygon([Point1, Point2, Point3, Point4]);
              end
              else
              begin
                Canvas.Polygon([Point5, Point6, Point7, Point8]);
              end;
            end;
            Canvas.Polygon([Point2, Point3, Point7, Point6]);
            Canvas.Polygon([Point1, Point2, Point6, Point5]);
          end;
      end;
    finally
      Canvas.Brush.Color := BrushColor;
    end;
  end;
end;

// adapted from http://www.delphipages.com/threads/thread.cfm?ID=109695&G=109693
procedure ThickLine( canvas:TCanvas; p1, p2 : TPoint );
{ delphi/windows can not print dashes and dots for thick lines (width>1),
  and there is a bug when printing dash or dot lines with width=1 }
const Dashmm = 3{mm}; Holemm = 1{mm}; Dotmm = 0.5{mm};
      Styles : array[TPenStyle,1..7] of TPenStyle = (
{ psSolid } ( psSolid, psSolid, psSolid, psSolid, psSolid, psSolid, psSolid ),
{ psDash } ( psDash, psClear, psSolid, psSolid, psSolid, psSolid, psSolid ),
{ psDot } ( psDot, psDot, psClear, psSolid, psSolid, psSolid, psSolid ),
{ psDashDot } ( psDash, psClear, psDot, psClear, psSolid, psSolid, psSolid ),
{ psDashDotDot } ( psDash, psClear, psDot, psClear, psDot, psClear, psSolid ),
{ psClear } ( psClear, psSolid, psSolid, psSolid, psSolid, psSolid, psSolid ),
{ psInsideFrame }( psSolid, psSolid, psSolid, psSolid, psSolid, psSolid, psSolid),
{ psUserStyle }( psSolid, psSolid, psSolid, psSolid, psSolid, psSolid, psSolid),
{ psAlternate }( psSolid, psSolid, psSolid, psSolid, psSolid, psSolid, psSolid)
 );

var dx, dy, dirx, diry, p : integer;
    x1, x2, y1, y2, absdxdy, absdydx : Extended;
    OldStyle, Style : TPenStyle;
    DimStyle : array[TPenStyle] of integer;
    stop : boolean;
begin
  with canvas do begin
    if not (Pen.style in [psDash,psDot,psDashDot,psDashDotdot]) then begin
      Polyline( [P1,P2] ); exit;
    end;
    stop:=(p2.x=P1.x) and (p2.y=p1.y);
    if stop then exit;
{ DimLine[psSolid]:=maxint; }
    DimStyle[psDash]:=round( font.pixelsPerInch/25.4*dashmm );
    DimStyle[psClear]:=round( font.pixelsPerInch/25.4*Holemm );
    DimStyle[psDot]:=round( font.pixelsPerInch/25.4*Dotmm );
    dx:=p2.x-p1.x; dy:=p2.y-p1.y;
    if dx<0 then dirx:=-1 else if dx=0 then dirx:=0 else dirx:=1;
    if dy<0 then diry:=-1 else if dy=0 then diry:=0 else diry:=1;
    OldStyle:=Pen.Style; Pen.style:=psSolid;
    x2:=p1.x; y2:=p1.y; p:=1; MoveTo( p1.x, p1.y );
    if abs(dx)>abs(dy) then begin absdxdy:=1; absdydx:=abs(dy/dx); end
                       else begin absdydx:=1; absdxdy:=abs(dx/dy); end;
    while not stop do begin
      Style:=Styles[OldStyle,p]; inc(p);
      if Styles[OldStyle,p]=psSolid then p:=1;
      x1:=x2; y1:=y2;
      x2:=x1+dirx*dimStyle[Style]*absdxdy;
      y2:=y1+diry*dimStyle[Style]*absdydx;
      if (x2*dirx>p2.x*dirx) or (y2*diry>p2.y*diry) then begin
        x2:=p2.x; y2:=p2.y; stop:=true;
      end;
      if (style<>psClear) or stop then LineTo( round(x2),round(y2) )
                                  else MoveTo( round(x2),round(y2) );
    end;
    Pen.style:=OldStyle;
  end;
end;

procedure TRbwModelCube.DrawCube;
var
  // This procedure draws a perspective drawing of a cube with a selected
  // section indicated by Selection1 and Selection2.

  // The size of PaintBox must be Width = 67, Height = 58.
  // Face indicates the direction of the slice of the cube that is
  // selected.
  // faNone indicates it is undetermined.
  // faTop indicates it is parallel to the top surface.
  // faFront indicates it is parallel to the front surface.
  // faSide indicates it is parallel to the side surface.
  //
  // Selection1 and Selection2 give the Fraction of the
  // side of the cube along
  // which the colored section is to be shown.
  // The origin is considered to be at the front, left, bottom corner.
  // Selection2 should be greater than or equal to Selection1.
  // Both Selection1 and Selection2 should be greater than or equal to 0 and
  // less than or equal to 1.
  BackLeftX, BackRightX, BackTopY, BackBottomY : integer;
  PenStyle : TPenStyle;
  FrontLeftCoordinates: array of TPoint;
  FrontRightCoordinates: array of TPoint;
  BackLeftCoordinates: array of TPoint;
  BackRightCoordinates: array of TPoint;
  PointIndex: Integer;
  ABreak: TBreakPosition;
  Frac1: double;
  Frac2: double;
begin
  if SelectedFace = faNone then
  begin
    Exit;
  end;
  BackLeftX := Interpolate(FLeftX, FVanishingPointX, FFraction);
  BackRightX := Interpolate(FRightX, FVanishingPointX, FFraction);
  BackTopY := Interpolate(FTopY, FVanishingPointY, FFraction);
  BackBottomY := Interpolate(FBottomY, FVanishingPointY, FFraction);

  // draw hidden part of cube
//  With Canvas do
  begin
    PenStyle := Canvas.Pen.Style;
    try
      if not Opaque then
      begin
        if SelectedFace <> faTop then
        begin
          Canvas.Pen.Style := psDot;

          ThickLine(Canvas, Point(FLeftX,FBottomY),
            Point(BackLeftX,BackBottomY));
          ThickLine(Canvas, Point(BackLeftX,BackBottomY),
            Point(BackRightX,BackBottomY));
          ThickLine(Canvas, Point(BackLeftX,BackBottomY),
            Point(BackLeftX,BackTopY));
        end;
      end;

      Canvas.Pen.Style := psSolid;

      DrawSelection(BackTopY, BackBottomY, BackLeftX, BackRightX);

    // draw front of cube

      if SelectedFace = faTop then
      begin
        SetLength(FrontLeftCoordinates, 2+Breaks.Count*2);
        SetLength(FrontRightCoordinates, 2+Breaks.Count*2);
        SetLength(BackLeftCoordinates, 2+Breaks.Count*2);
        SetLength(BackRightCoordinates, 2+Breaks.Count*2);
        for PointIndex := 0 to Length(FrontLeftCoordinates) - 1 do
        begin
          FrontLeftCoordinates[PointIndex].X := FLeftX;
          BackLeftCoordinates[PointIndex].X := BackLeftX;
          FrontRightCoordinates[PointIndex].X := FRightX;
          BackRightCoordinates[PointIndex].X := BackRightX;
        end;

        FrontLeftCoordinates[0].Y := FBottomY;
        FrontLeftCoordinates[1+Breaks.Count*2].Y := FTopY;
        BackLeftCoordinates[0].Y := BackBottomY;
        BackLeftCoordinates[1+Breaks.Count*2].Y := BackTopY;
        FrontRightCoordinates[0].Y := FBottomY;
        FrontRightCoordinates[1+Breaks.Count*2].Y := FTopY;
        BackRightCoordinates[0].Y := BackBottomY;
        BackRightCoordinates[1+Breaks.Count*2].Y := BackTopY;

        Breaks.Sort;
        for PointIndex := 0 to Breaks.Count - 1 do
        begin
          ABreak := Breaks[PointIndex];
          FrontLeftCoordinates[PointIndex*2+1].Y :=
            Interpolate(FBottomY, FTopY, ABreak.LowerFraction);
          FrontLeftCoordinates[PointIndex*2+2].Y :=
            Interpolate(FBottomY, FTopY, ABreak.HigherFraction);
          FrontRightCoordinates[PointIndex*2+1].Y :=
            FrontLeftCoordinates[PointIndex*2+1].Y;
          FrontRightCoordinates[PointIndex*2+2].Y :=
            FrontLeftCoordinates[PointIndex*2+2].Y;
          BackLeftCoordinates[PointIndex*2+1].Y :=
            Interpolate(BackBottomY, BackTopY, ABreak.LowerFraction);
          BackLeftCoordinates[PointIndex*2+2].Y :=
            Interpolate(BackBottomY, BackTopY, ABreak.HigherFraction);
          BackRightCoordinates[PointIndex*2+1].Y :=
            BackLeftCoordinates[PointIndex*2+1].Y;
          BackRightCoordinates[PointIndex*2+2].Y :=
            BackLeftCoordinates[PointIndex*2+2].Y;
        end;

        for PointIndex := 0 to Length(FrontLeftCoordinates) div 2 - 1 do
        begin
          if not Opaque then
          begin
            Canvas.Pen.Style := psDot;

            ThickLine(Canvas, FrontLeftCoordinates[PointIndex*2],
              BackLeftCoordinates[PointIndex*2]);
            ThickLine(Canvas, BackLeftCoordinates[PointIndex*2],
              BackRightCoordinates[PointIndex*2]);
            ThickLine(Canvas, BackLeftCoordinates[PointIndex*2],
              BackLeftCoordinates[PointIndex*2+1]);
          end;

          Canvas.Pen.Style := psSolid;
          // Front Face
          Canvas.MoveTo(FrontLeftCoordinates[PointIndex*2+1].X,
            FrontLeftCoordinates[PointIndex*2+1].Y);
          Canvas.LineTo(FrontLeftCoordinates[PointIndex*2].X,
            FrontLeftCoordinates[PointIndex*2].Y);
          Canvas.LineTo(FrontRightCoordinates[PointIndex*2].X,
            FrontRightCoordinates[PointIndex*2].Y);
          Canvas.LineTo(FrontRightCoordinates[PointIndex*2+1].X,
            FrontRightCoordinates[PointIndex*2+1].Y);
          Canvas.LineTo(FrontLeftCoordinates[PointIndex*2+1].X,
            FrontLeftCoordinates[PointIndex*2+1].Y);

          // Top Face
          Canvas.LineTo(BackLeftCoordinates[PointIndex*2+1].X,
            BackLeftCoordinates[PointIndex*2+1].Y);
          Canvas.LineTo(BackRightCoordinates[PointIndex*2+1].X,
            BackRightCoordinates[PointIndex*2+1].Y);
          Canvas.LineTo(FrontRightCoordinates[PointIndex*2+1].X,
            FrontRightCoordinates[PointIndex*2+1].Y);

          // Side Face
          Canvas.MoveTo(FrontRightCoordinates[PointIndex*2].X,
            FrontRightCoordinates[PointIndex*2].Y);
          Canvas.LineTo(BackRightCoordinates[PointIndex*2].X,
            BackRightCoordinates[PointIndex*2].Y);
          Canvas.LineTo(BackRightCoordinates[PointIndex*2+1].X,
            BackRightCoordinates[PointIndex*2+1].Y);

          Frac1 := (PointIndex * 2)/Length(FrontLeftCoordinates);
          Frac2 := ((PointIndex * 2)+1)/Length(FrontLeftCoordinates);

          if (Frac2 > Selection1) and (Frac1 < Selection2) then
          begin
            DrawSelection(BackTopY, BackBottomY, BackLeftX, BackRightX);
          end;
        end;
      end
      else
      begin
        // front face
        Canvas.MoveTo(FLeftX,FTopY);
        Canvas.LineTo(FRightX,FTopY);
        Canvas.LineTo(FRightX,FBottomY);
        Canvas.LineTo(FLeftX,FBottomY);
        Canvas.LineTo(FLeftX,FTopY);

        // top face
        Canvas.LineTo(BackLeftX,BackTopY);
        Canvas.LineTo(BackRightX,BackTopY);
        Canvas.LineTo(FRightX,FTopY);

        // right face
        Canvas.MoveTo(FRightX,FBottomY);
        Canvas.LineTo(BackRightX,BackBottomY);
        Canvas.LineTo(BackRightX,BackTopY);
      end;

    finally
      Canvas.Pen.Style := PenStyle;
    end;

  end;
end;

function TRbwModelCube.GetCubeHeight: integer;
begin
  result := FBottomY - FTopY;
end;

function TRbwModelCube.GetCubeWidth: integer;
begin
  result := FRightX - FLeftX;
end;

function TRbwModelCube.GetFaceOfCube: TRbwFace;
var
  // The size of PaintBox must be Width = 67, Height = 58.
  BackLeftX, BackRightX, BackTopY, BackBottomY : integer;
  PointArray : array[0..4] of TPoint;
begin
  result := faNone;

  BackLeftX := Interpolate(FLeftX, FVanishingPointX, FFraction);
  BackRightX := Interpolate(FRightX, FVanishingPointX, FFraction);
  BackTopY := Interpolate(FTopY, FVanishingPointY, FFraction);
  BackBottomY := Interpolate(FBottomY, FVanishingPointY, FFraction);

  // Front Face
  PointArray[0].X := FLeftX;
  PointArray[0].Y := FTopY;
  PointArray[1].X := FRightX;
  PointArray[1].Y := FTopY;
  PointArray[2].X := FRightX;
  PointArray[2].Y := FBottomY;
  PointArray[3].X := FLeftX;
  PointArray[3].Y := FBottomY;
  PointArray[4].X := FLeftX;
  PointArray[4].Y := FTopY;

  if IsPointInside(FMouseX, FMouseY, PointArray) then
  begin
    result := faFront;
  end
  else
  begin
    // Top Face
    PointArray[0].X := FLeftX;
    PointArray[0].Y := FTopY;
    PointArray[1].X := BackLeftX;
    PointArray[1].Y := BackTopY;
    PointArray[2].X := BackRightX;
    PointArray[2].Y := BackTopY;
    PointArray[3].X := FRightX;
    PointArray[3].Y := FTopY;
    PointArray[4].X := FLeftX;
    PointArray[4].Y := FTopY;
    if IsPointInside(FMouseX, FMouseY, PointArray) then
    begin
      result := faTop;
    end
    else
    begin
      // Side Face
      PointArray[0].X := FRightX;
      PointArray[0].Y := FTopY;
      PointArray[1].X := BackRightX;
      PointArray[1].Y := BackTopY;
      PointArray[2].X := BackRightX;
      PointArray[2].Y := BackBottomY;
      PointArray[3].X := FRightX;
      PointArray[3].Y := FBottomY;
      PointArray[4].X := FRightX;
      PointArray[4].Y := FTopY;
      if IsPointInside(FMouseX, FMouseY, PointArray) then
      begin
        result := faSide;
      end;
    end;
  end;
end;


procedure TRbwModelCube.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseX := X;
  FMouseY := Y;
end;

procedure TRbwModelCube.Paint;
begin
  inherited;
  DrawCube;
end;

procedure TRbwModelCube.SetCanClickFace(const Value: boolean);
begin
  FCanClickFace := Value;
end;

procedure TRbwModelCube.SetSelectionColor(const Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetCubeHeight(const Value: integer);
begin
  FBottomY := FTopY + value;
  Invalidate;
end;

procedure TRbwModelCube.SetCubeWidth(const Value: integer);
begin
  FRightX := FLeftX + Value;
  Invalidate;
end;

procedure TRbwModelCube.SetFraction(const Value: double);
begin
  FFraction := Value;
  Invalidate;
end;

procedure TRbwModelCube.SetLeftX(const Value: integer);
begin
  FLeftX := Value;
  Invalidate;
end;

procedure TRbwModelCube.SetOpaque(const Value: boolean);
begin
  if FOpaque <> Value then
  begin
    FOpaque := Value;
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetSelectedFace(const Value: TRbwFace);
begin
  if FSelectedFace <> Value then
  begin
    FSelectedFace := Value;
    if ShowSelection then
    begin
      Invalidate;
    end;
    if Assigned(FOnSelectFace) then
    begin
      FOnSelectFace(self);
    end;
  end;
end;

procedure TRbwModelCube.SetSelection1(const Value: double);
begin
  if Value < 0 then
  begin
    FSelection1 := 0;
  end
  else if Value > 1 then
  begin
    FSelection1 := 1;
  end
  else
  begin
    FSelection1 := Value;
  end;
  if FSelection2 < FSelection1 then
  begin
    FSelection2 := FSelection1;
  end;
  if ShowSelection then
  begin
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetSelection2(const Value: double);
begin
  if Value < 0 then
  begin
    FSelection2 := 0;
  end
  else if Value > 1 then
  begin
    FSelection2 := 1;
  end
  else
  begin
    FSelection2 := Value;
  end;
  if FSelection2 < FSelection1 then
  begin
    FSelection1 := FSelection2;
  end;
  if ShowSelection then
  begin
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetShowSelection(const Value: boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetTopY(const Value: integer);
begin
  FTopY := Value;
  Invalidate;
end;

procedure TRbwModelCube.SetVanishingPointX(const Value: integer);
begin
  FVanishingPointX := Value;
  Invalidate;
end;

procedure TRbwModelCube.SetVanishingPointY(const Value: integer);
begin
  FVanishingPointY := Value;
  Invalidate;
end;

procedure TRbwModelCube.SetXOrigin(const Value: TRbwXOrigin);
begin
  if FXOrigin <> Value then
  begin
    FXOrigin := Value;
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetYOrigin(const Value: TRbwYOrigin);
begin
  if FYOrigin <> Value then
  begin
    FYOrigin := Value;
    Invalidate;
  end;
end;

procedure TRbwModelCube.SetZOrigin(const Value: TRbwZOrigin);
begin
  if FZOrigin <> Value then
  begin
    FZOrigin := Value;
    Invalidate;
  end;
end;

{ TBreakPosition }

procedure TBreakPosition.SetLowerFraction(const Value: double);
begin
  FLowerFraction := Value;
  (Collection as TBreakCollection).Changed;
end;

procedure TBreakPosition.SetHigherFraction(const Value: double);
begin
  FHigherFraction := Value;
  (Collection as TBreakCollection).Changed;
end;

{ TBreakCollection }

function TBreakCollection.Add: TBreakPosition;
begin
  Result := inherited Add as TBreakPosition;
  Changed;
end;

constructor TBreakCollection.Create;
begin
  inherited Create(TBreakPosition);
end;

function TBreakCollection.GetItem(Index: Integer): TBreakPosition;
begin
  result := inherited Items[Index] as TBreakPosition;
end;

procedure TBreakCollection.SetItem(Index: Integer; const Value: TBreakPosition);
begin
  inherited Items[Index] := Value;
end;

function CompareBreaks(Item1, Item2: Pointer): integer;
var
  Break1, Break2: TBreakPosition;
begin
  Break1 := Item1;
  Break2 := Item2;
  result := Sign(Break1.LowerFraction - Break2.LowerFraction);
  if result = 0 then
  begin
    result := Sign(Break1.HigherFraction - Break2.HigherFraction);
  end;
end;

procedure TBreakCollection.Sort;
var
  List: TList;
  index: Integer;
  ABreak: TBreakPosition;
begin
  List := TList.Create;
  try
    List.Capacity := Count;
    for index := 0 to Count - 1 do
    begin
      List.Add(Items[index])
    end;
    List.Sort(CompareBreaks);
    for index := 0 to List.Count - 1 do
    begin
      ABreak := List[index];
      ABreak.Index := index;
    end;
  finally
    List.Free;
  end;
end;

procedure TBreakCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

end.
