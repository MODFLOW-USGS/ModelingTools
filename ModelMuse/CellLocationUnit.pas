unit CellLocationUnit;

interface

// TCellLocation is declared in ModflowCellUnit.

uses
  GoPhastTypes, FastGEO, ScreenObjectInterfaceUnit, System.Classes, System.ZLib,
  PhastModelInterfaceUnit, AbstractGridInterfaceUnit, ModflowCellUnit;

type

  // @abstract(@name stores information about where
  // a @link(TScreenObject) is intersecting
  // the grid.)
  //
  // See @link(TEdgePoint) and @link(TCellElementSegment).
  TEdgePosition = (epFirst, epMiddle, epLast);

  // @abstract(@name represents the 2D intersection of
  // one segment of a @link(TScreenObject)
  // with a cell or element in the grid.)
  TCellElementSegment = class(TObject)
  private
    // See @link(X1), @link(X2), @link(Y1), and @link(Y2).
    FSegment: TSegment2D;
    // See @link(Col).
    FCol: integer;
    // See @link(EndPosition).
    FEndPosition: TEdgePosition;
    // See @link(Layer).
    FLayer: integer;
    // See @link(VertexIndex).
    FVertexIndex: integer;
    // See @link(Row).
    FRow: integer;
    // See @link(StartPosition).
    FStartPosition: TEdgePosition;
    FSectionIndex: integer;
    FSubSegments: TSegment2DArray;
    FLgrEdge: boolean;
    FScreenObject: IScreenObject;
    FPositionInSegmentList: integer;
  public
    procedure Store(Stream: TStream);
    procedure Restore(Stream: TDecompressionStream);
    // @name is the column of the grid which this @classname intersects.
    // In this context, column can represent a column of elements or a column
    // of cells depending on which this @classname is intersecting.
    property Col: integer read FCol write FCol;
    // If the start of this @classname is at a vertex of a @link(TScreenObject),
    // @name is epFirst or epLast.  If it is at a location where the
    // the @link(TScreenObject) crosses the edge of a cell or element,
    //  it is epMiddle.
    // @name is used in exporting rivers in PHAST.
    property EndPosition: TEdgePosition read FEndPosition write FEndPosition;
    // @name is the layer of the grid which this @classname intersects.
    // In this context, layer can represent a layer of elements or a layer
    // of cells depending on which this @classname is intersecting.
    property Layer: integer read FLayer write FLayer;
    // @name returns the length of this @classname.
    function SegmentLength: double;
    // @name is the index of the point in @link(TScreenObject.Points)
    // that comes before this @classname.
    property VertexIndex: integer read FVertexIndex write FVertexIndex;
    // @name is the row of the grid which this @classname intersects.
    // In this context, row can represent a row of elements or a row
    // of cells depending on which this @classname is intersecting.
    property Row: integer read FRow write FRow;
    // If the start of this @classname is at a vertex of a @link(TScreenObject),
    // @name is epFirst or epLast.  If it is at a location where the
    // the @link(TScreenObject) crosses the edge of a cell or element,
    //  it is epMiddle.
    // @name is used in exporting rivers in PHAST.
    property StartPosition: TEdgePosition read FStartPosition
      write FStartPosition;
    // @name is the X-coordinate of the start of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property X1: double read FSegment[1].x write FSegment[1].x;
    // @name is the X-coordinate of the end of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property X2: double read FSegment[2].x write FSegment[2].x;
    // @name is the Y-coordinate of the start of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property Y1: double read FSegment[1].y write FSegment[1].y;
    // @name is the Y-coordinate of the end of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property Y2: double read FSegment[2].y write FSegment[2].y;
    Property StartPoint: TPoint2D read FSegment[1];
    Property EndPoint: TPoint2D read FSegment[2];
    function FirstPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
    function SecondPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
    property SectionIndex: integer read FSectionIndex write FSectionIndex;
    property LgrEdge: boolean read FLgrEdge write FLgrEdge;
    constructor Create(ScreenObject: IScreenObject);
    function IsSame(AnotherSegment: TCellElementSegment): boolean;
    property Segment: TSegment2D read FSegment write FSegment;
    property PositionInSegmentList: integer read FPositionInSegmentList
      write FPositionInSegmentList;
    property ScreenObject: IScreenObject read FScreenObject;
    property SubSegments: TSegment2DArray read FSubSegments write FSubSegments;
    procedure SetSubsegmentLength(NewLength: Integer);
  end;

  TAssignmentMethod = (amEnclose, amIntersect);

  TCellAssignment = class(TObject)
  strict private
    FAnnotation: string;
    FAssignmentMethod: TAssignmentMethod;
    FOwnedSegment: TCellElementSegment;
    FSegment: TCellElementSegment;
    FLayer: integer;
    FSection: integer;
    FRow: integer;
    FColumn: integer;
    FLgrEdge: Boolean;
  private
    FScreenObject: IScreenObject;
    FSutraX: double;
    FSutraY: double;
    FSutraZ: Double;
    procedure Assign(Cell: TCellAssignment);
    function GetSection: integer;
    function GetCell: TCellLocation;
  public
    procedure Store(Stream: TStream);
    procedure Restore(Stream: TDecompressionStream;
      const EncloseAnnotation, IntersectAnnotation: string);
    constructor CreateFromStream(Stream: TDecompressionStream;
      const EncloseAnnotation, IntersectAnnotation: string);
    constructor CreateFromCell(Cell: TCellAssignment);
    property Cell: TCellLocation read GetCell;
    property AssignmentMethod: TAssignmentMethod read FAssignmentMethod;
    // @name starts at zero.
    property Layer: integer read FLayer write FLayer;
    // @name starts at zero.
    property Row: integer read FRow write FRow;
    // @name starts at zero.
    property Column: integer read FColumn write FColumn;
    property Segment: TCellElementSegment read FSegment;
    property Section: integer read GetSection;
    property Annotation: string read FAnnotation;
    Constructor Create(ALayer, ARow, ACol: integer;
      ASegment: TCellElementSegment; ASection: integer;
      const AnAnnotation: string; AnAssignmentMethod: TAssignmentMethod);
    Destructor Destroy; override;
    property LgrEdge: Boolean read FLgrEdge;
    // @name is used for the location of SUTRA observations when those
    // locations are defined by the location of the object rather than
    // the location of the node or element intersected by the object.
    Property SutraX: double read FSutraX write FSutraX;
    // @name is used for the location of SUTRA observations when those
    // locations are defined by the location of the object rather than
    // the location of the node or element intersected by the object.
    property SutraY: double read FSutraY write FSutraY;
    // @name is used for the location of SUTRA observations when those
    // locations are defined by the location of the object rather than
    // the location of the node or element intersected by the object.
    property SutraZ: Double read FSutraZ write FSutraZ;
    // If any new properties are added, be sure to update Assign too.
    property ScreenObject: IScreenObject read FScreenObject;
    function ZeroBasedID: TZeroBasedID;
  end;

implementation

{ TCellElementSegment }

constructor TCellElementSegment.Create(ScreenObject: IScreenObject);
begin
  inherited Create;
  FPositionInSegmentList := -1;
  FScreenObject := ScreenObject;
end;

function TCellElementSegment.FirstPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
var
  LocalModel: IModelForCustomModelGrid;
begin
  result := FSegment[1];
  if IGlobalModel.QueryInterface(IModelForCustomModelGrid, LocalModel) <> 0 then
  begin
    Assert(False);
  end;
  if (ViewDirection = vdTop) and (LocalModel.GridI <> nil) then
  begin
    result := LocalModel.GridI.
      RotateFromGridCoordinatesToRealWorldCoordinates(result);
  end;
end;

function TCellElementSegment.IsSame(
  AnotherSegment: TCellElementSegment): boolean;
begin
  result := (Col = AnotherSegment.Col)
    and (EndPosition = AnotherSegment.EndPosition)
    and (Layer = AnotherSegment.Layer)
    and (VertexIndex = AnotherSegment.VertexIndex)
    and (Row = AnotherSegment.Row)
    and (StartPosition = AnotherSegment.StartPosition)
    and (X1 = AnotherSegment.X1)
    and (X2 = AnotherSegment.X2)
    and (Y1 = AnotherSegment.Y1)
    and (Y2 = AnotherSegment.Y2)
    and (SectionIndex = AnotherSegment.SectionIndex)
end;

function TCellElementSegment.SegmentLength: double;
var
  SubIndex: Integer;
begin
  if (Length(FSubSegments) = 0)
  or ((FScreenObject <> nil)
    and (FScreenObject.FullObjectIntersectLength)) then
  begin
    result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
  end
  else
  begin
    result := 0;
    for SubIndex := 0 to Length(FSubSegments) - 1 do
    begin
      result := result
        + Sqrt(Sqr(FSubSegments[SubIndex][1].x - FSubSegments[SubIndex][2].x)
        + Sqr(FSubSegments[SubIndex][1].y - FSubSegments[SubIndex][2].y));
    end;
  end;
end;

procedure TCellElementSegment.SetSubsegmentLength(NewLength: Integer);
begin
  SetLength(FSubSegments, NewLength);
end;

procedure TCellElementSegment.Restore(Stream: TDecompressionStream);
var
  SubSegLength: integer;
begin
  FCol := ReadCompInt(Stream);
  Stream.Read(FEndPosition, SizeOf(FEndPosition));
  FLayer := ReadCompInt(Stream);
  FVertexIndex := ReadCompInt(Stream);
  FRow := ReadCompInt(Stream);
  Stream.Read(FStartPosition, SizeOf(FStartPosition));
  Stream.Read(FSegment, SizeOf(FSegment));
  FSectionIndex := ReadCompInt(Stream);
  LgrEdge := ReadCompBoolean(Stream);
  FPositionInSegmentList := ReadCompInt(Stream);

  SubSegLength := ReadCompInt(Stream);
  SetLength(FSubSegments, SubSegLength);
  if SubSegLength > 0 then
  begin
    Stream.Read(FSubSegments[0], SubSegLength*SizeOf(TSegment2D));
  end;
end;

function TCellElementSegment.SecondPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
var
  LocalModel: IModelForCustomModelGrid;
begin
  result := FSegment[2];
  if IGlobalModel.QueryInterface(IModelForCustomModelGrid, LocalModel) <> 0 then
  begin
    Assert(False);
  end;
  if (ViewDirection = vdTop) and (LocalModel.GridI <> nil) then
  begin
    result := LocalModel.GridI.
      RotateFromGridCoordinatesToRealWorldCoordinates(result);
  end;
end;

procedure TCellElementSegment.Store(Stream: TStream);
var
  SubSegLength: integer;
begin
  WriteCompInt(Stream, FCol);
  Stream.Write(FEndPosition, SizeOf(FEndPosition));
  WriteCompInt(Stream, FLayer);
  WriteCompInt(Stream, FVertexIndex);
  WriteCompInt(Stream, FRow);
  Stream.Write(FStartPosition, SizeOf(FStartPosition));
  Stream.Write(FSegment, SizeOf(FSegment));
  WriteCompInt(Stream, FSectionIndex);
  WriteCompBoolean(Stream, LgrEdge);
  WriteCompInt(Stream, FPositionInSegmentList);

  SubSegLength := System.Length(FSubSegments);
  WriteCompInt(Stream, SubSegLength);
  if SubSegLength > 0 then
  begin
    Stream.Write(FSubSegments[0], SubSegLength*SizeOf(TSegment2D));
  end;
end;

{ TCellAssignment }

procedure TCellAssignment.Assign(Cell: TCellAssignment);
begin
  FLayer := Cell.FLayer;
  FRow := Cell.FRow;
  FColumn := Cell.FColumn;
  FSegment := Cell.FSegment;
  FSection := Cell.FSection;
  FAnnotation := Cell.FAnnotation;
  FAssignmentMethod := Cell.FAssignmentMethod;
  FLgrEdge := Cell.FLgrEdge;
  FSutraX := Cell.FSutraX;
  FSutraY := Cell.FSutraY;
  FSutraZ := Cell.FSutraZ;
end;

constructor TCellAssignment.Create(ALayer, ARow, ACol: integer;
      ASegment: TCellElementSegment; ASection: integer;
      const AnAnnotation: string; AnAssignmentMethod: TAssignmentMethod);
begin
  FLayer := ALayer;
  FRow := ARow;
  FColumn := ACol;
  FSegment := ASegment;
  FSection := ASection;
  FAnnotation := AnAnnotation;
  FAssignmentMethod := AnAssignmentMethod;
  if ASegment <> nil then
  begin
    FScreenObject := ASegment.FScreenObject;
    FLgrEdge := ASegment.LgrEdge;
  end
  else
  begin
    FLgrEdge := False;
    FScreenObject := nil;
  end;
end;

constructor TCellAssignment.CreateFromCell(Cell: TCellAssignment);
begin
  Assign(Cell);
end;

constructor TCellAssignment.CreateFromStream(Stream: TDecompressionStream;
  const EncloseAnnotation, IntersectAnnotation: string);
begin
  Restore(Stream, EncloseAnnotation, IntersectAnnotation);
end;

destructor TCellAssignment.Destroy;
begin
  FOwnedSegment.Free;
  inherited;
end;

function TCellAssignment.GetCell: TCellLocation;
begin
  result.Layer := Layer;
  result.Row := Row;
  result.Column := Column;
  result.Section := Section;
end;

function TCellAssignment.GetSection: integer;
begin
  if Segment = nil then
  begin
    result := FSection;
  end
  else
  begin
    result := Segment.SectionIndex;
  end;
end;

procedure TCellAssignment.Restore(Stream: TDecompressionStream;
  const EncloseAnnotation, IntersectAnnotation: string);
var
  ReadSegment: boolean;
begin
  Stream.Read(FAssignmentMethod, SizeOf(FAssignmentMethod));
  case FAssignmentMethod of
    amEnclose: FAnnotation := EncloseAnnotation;
    amIntersect: FAnnotation := IntersectAnnotation;
    else Assert(False);
  end;
  Stream.Read(ReadSegment, SizeOf(ReadSegment));
  if ReadSegment then
  begin
    FOwnedSegment.Free;
    FOwnedSegment := TCellElementSegment.Create(FScreenObject);
    FOwnedSegment.Restore(Stream);
    FSegment := FOwnedSegment;
  end
  else
  begin
    FSegment := nil;
  end;
  FLayer := ReadCompInt(Stream);
  FSection := ReadCompInt(Stream);
  FRow := ReadCompInt(Stream);
  FColumn := ReadCompInt(Stream);
  FLgrEdge := ReadCompBoolean(Stream);
  FSutraX := ReadCompReal(Stream);
  FSutraY := ReadCompReal(Stream);
  FSutraZ := ReadCompReal(Stream);
end;

procedure TCellAssignment.Store(Stream: TStream);
var
  StoreSegment: boolean;
begin
  Stream.Write(FAssignmentMethod, SizeOf(FAssignmentMethod));
  StoreSegment := FSegment <> nil;
  Stream.Write(StoreSegment, SizeOf(StoreSegment));
  if StoreSegment then
  begin
    FSegment.Store(Stream);
  end;
  WriteCompInt(Stream, FLayer);
  WriteCompInt(Stream, FSection);
  WriteCompInt(Stream, FRow);
  WriteCompInt(Stream, FColumn);
  WriteCompBoolean(Stream, FLgrEdge);
  WriteCompReal(Stream, FSutraX);
  WriteCompReal(Stream, FSutraY);
  WriteCompReal(Stream, FSutraZ);
end;



function TCellAssignment.ZeroBasedID: TZeroBasedID;
begin
  result.Column := Column;
  result.Row := Row;
  result.Layer := Layer;
end;

end.
