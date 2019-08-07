unit DrawMeshTypesUnit;

interface


uses
  System.Classes, Vcl.Graphics, FastGEO, GoPhastTypes, MeshRenumberingTypes,
  SubPolygonUnit, DataSetUnit;

type
  TMeshCrossSectionLine = class;

  IDrawMesh = interface(IMesh)
    function Is3DMesh: Boolean;
    function GetCanDraw: boolean;
    function GetSelectedLayer: Integer;
    procedure SetSelectedLayer(Value: Integer);
    function GetCrossSection: TMeshCrossSectionLine;
    function GetCanDraw3D: Boolean;
    function GetMesh2DI: IMesh2D;
    function GetLayerCount: Integer;
    function GetElementArrayMemberI(Layer, Col: Integer): IElement3D;
    function GetNodeArrayMemberI(Layer, Col: Integer): INode3D;
    function GetTopDataSet: TDataArray;
    function GetTopContourDataSet: TDataArray;
    function GetThreeDDataSet: TDataArray;
    function GetThreeDContourDataSet: TDataArray;
    procedure SetCanDraw(const Value: boolean);
    procedure SetCrossSection(const Value: TMeshCrossSectionLine);
    procedure SetTopDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    procedure Draw(const BitMap: TPersistent;
      const ViewDirection: TViewDirection);
    property SelectedLayer: Integer read GetSelectedLayer write SetSelectedLayer;
    property CrossSection: TMeshCrossSectionLine read GetCrossSection
      write SetCrossSection;
    function RotateFromRealWorldCoordinatesToMeshCoordinates
      (const APoint: TPoint2D): TPoint2D;
    function RotateFromMeshCoordinatesToRealWorldCoordinates
      (const APoint: TPoint2D): TPoint2D;  overload;
    function MeshLimits(ViewDirection: TViewDirection; Angle: double): TGridLimit;
    procedure Draw3D;
    property CanDraw3D: Boolean read GetCanDraw3D;
    procedure GetElementsIntfOnCrossSection(ElementList: TIElement2DList);
    procedure GetNodesIntfOnCrossSection(NodeList: TINode2DList);
    function IsFishnetMesh: Boolean;
    function TopOutline(Layer: integer): TOutline;
    function FrontOutline: TOutline;
    property Mesh2DI: IMesh2D read GetMesh2DI;
    property LayerCount: Integer read GetLayerCount;
    property ElementArrayI[Layer: Integer; Col: Integer]: IElement3D
      read GetElementArrayMemberI;
    property NodeArrayI[Layer: Integer; Col: Integer]: INode3D
      read GetNodeArrayMemberI;
    property TopDataSet: TDataArray read GetTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read GetTopContourDataSet write SetTopContourDataSet;
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property ThreeDContourDataSet: TDataArray read GetThreeDContourDataSet write SetThreeDContourDataSet;
    procedure MeshChanged;
    function DefaultCrossSectionLocation: TSegment2D;
  end;

  TMeshCrossSectionLine = class(TGoPhastPersistent)
  private
    FStartX: Double;
    FStartY: double;
    FEndX: Double;
    FEndY: double;
    FColor: TColor;
    FEditing: Boolean;
    FOnMoved: TNotifyEvent;
    procedure Moved;
    procedure SetEndPointValue(var AField: double; const NewValue: double);
    procedure SetEndX(const Value: Double);
    procedure SetEndY(const Value: double);
    procedure SetStartX(const Value: Double);
    procedure SetStartY(const Value: double);
    function GetEndPoint: TPoint2D;
    function GetStartPoint: TPoint2D;
    procedure SetEndPoint(const Value: TPoint2D);
    procedure SetStartPoint(const Value: TPoint2D);
    procedure SetColor(const Value: TColor);
    function GetSegment: TSegment2D;
    procedure SetSegment(const Value: TSegment2D);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    property StartPoint: TPoint2D read GetStartPoint write SetStartPoint;
    property EndPoint: TPoint2D read GetEndPoint write SetEndPoint;
    property Segment: TSegment2D read GetSegment write SetSegment;
    procedure Draw(const BitMap: TPersistent);
    // cross section angle in radians.
    function Angle: Double;
    property Editing: Boolean read FEditing write FEditing;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  published
    property StartX: Double read FStartX write SetStartX;
    property EndX: Double read FEndX write SetEndX;
    property StartY: double read FStartY write SetStartY;
    property EndY: double read FEndY write SetEndY;
    property Color: TColor read FColor write SetColor default clFuchsia;
  end;

implementation

uses
  System.Math, frmGoPhastUnit, ZoomBox2, AbstractGridUnit, BigCanvasMethods,
  GR32;

{ TCrossSection }

function TMeshCrossSectionLine.Angle: Double;
begin
  result := ArcTan2(EndY-StartY, EndX-StartX);
end;

procedure TMeshCrossSectionLine.Assign(Source: TPersistent);
var
  SourceCS: TMeshCrossSectionLine;
begin
  if Source is TMeshCrossSectionLine then
  begin
    SourceCS := TMeshCrossSectionLine(Source);
    Segment := SourceCS.Segment;
    Color := SourceCS.Color;
  end
  else
  begin
    inherited;
  end;
end;

constructor TMeshCrossSectionLine.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(InvalidateModelEvent);
  FColor := clPurple;
end;

procedure TMeshCrossSectionLine.Draw(const BitMap: TPersistent);
const
  SquareSize = 3;
var
  Points: GoPhastTypes.TPointArray;
  ZoomBox: TQRbwZoomBox2;
begin
  SetLength(Points, 2);
  ZoomBox := frmGoPhast.frameTopView.ZoomBox;
  Points[0] := ConvertTop2D_Point(ZoomBox, StartPoint);
  Points[1] := ConvertTop2D_Point(ZoomBox, EndPoint);
  if Editing then
  begin
    DrawBigPolyline32(BitMap, Color32(Color), ThickGridLineThickness,
      Points, True);
    DrawBigRectangle32(BitMap, Color32(Color), Color32(Color),
      OrdinaryGridLineThickness,
      Points[0].x -SquareSize, Points[0].y -SquareSize,
      Points[0].x +SquareSize, Points[0].y +SquareSize);
    DrawBigRectangle32(BitMap, Color32(Color), Color32(Color),
      OrdinaryGridLineThickness,
      Points[1].x -SquareSize, Points[1].y -SquareSize,
      Points[1].x +SquareSize, Points[1].y +SquareSize);
  end
  else
  begin
    DrawBigPolyline32(BitMap, Color32(Color), OrdinaryGridLineThickness,
      Points, True);
  end;
end;

function TMeshCrossSectionLine.GetEndPoint: TPoint2D;
begin
  Result.x := EndX;
  Result.y := EndY;
end;

function TMeshCrossSectionLine.GetSegment: TSegment2D;
begin
  result[1] := StartPoint;
  result[2] := EndPoint;
end;

function TMeshCrossSectionLine.GetStartPoint: TPoint2D;
begin
  Result.x := StartX;
  Result.y := StartY;
end;

procedure TMeshCrossSectionLine.Moved;
begin
  if Assigned(FOnMoved) then
  begin
    FOnMoved(Self);
  end;
end;

procedure TMeshCrossSectionLine.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    InvalidateModel;
  end;
end;

procedure TMeshCrossSectionLine.SetEndPoint(const Value: TPoint2D);
begin
  EndX := Value.x;
  EndY := Value.y;
end;

procedure TMeshCrossSectionLine.SetEndPointValue(var AField: double;
  const NewValue: double);
begin
  if AField <> NewValue then
  begin
    Moved;
  end;
  SetRealProperty(AField, NewValue);
end;

procedure TMeshCrossSectionLine.SetEndX(const Value: Double);
begin
  SetEndPointValue(FEndX, Value);
end;

procedure TMeshCrossSectionLine.SetEndY(const Value: double);
begin
  SetEndPointValue(FEndY, Value);
end;

procedure TMeshCrossSectionLine.SetSegment(const Value: TSegment2D);
begin
  StartPoint := Value[1];
  EndPoint := Value[2];
end;

procedure TMeshCrossSectionLine.SetStartPoint(const Value: TPoint2D);
begin
  StartX := Value.x;
  StartY := Value.y;
end;

procedure TMeshCrossSectionLine.SetStartX(const Value: Double);
begin
  SetEndPointValue(FStartX, Value);
end;

procedure TMeshCrossSectionLine.SetStartY(const Value: double);
begin
  SetEndPointValue(FStartY, Value);
end;

end.
