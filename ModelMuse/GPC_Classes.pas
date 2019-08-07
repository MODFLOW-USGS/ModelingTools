unit GPC_Classes;

interface

uses SysUtils, gpc;

type
  EGpcException = class(Exception);

  TGpcPolygonClass = class(TObject)
  private
    FPolygon: Tgpc_polygon;
    function GetContours(ContourIndex: integer): Tgpc_vertex_list;
    function GetHoles(ContourIndex: integer): boolean;
    function GetNumberOfContours: integer;
    function GetVertexCount(ContourIndex: integer): integer;
    function GetVertices(ContourIndex, VertexIndex: integer): Tgpc_vertex;
    procedure SetContours(ContourIndex: integer; const Value: Tgpc_vertex_list);
    procedure SetHoles(ContourIndex: integer; const Value: boolean);
    procedure SetNumberOfContours(const Value: integer);
    procedure SetVertexCount(ContourIndex: integer; const Value: integer);
    procedure SetVertices(ContourIndex, VertexIndex: integer;
      const Value: Tgpc_vertex);
  public
    {Setting @name erases all existing contours.}
    property NumberOfContours: integer read GetNumberOfContours
      write SetNumberOfContours;
    property Contours[ContourIndex: integer]: Tgpc_vertex_list read GetContours
      write SetContours;
    property Holes[ContourIndex: integer]: boolean read GetHoles write SetHoles;
    {Setting @name erases all vertices in the specified contour.}
    property VertexCount[ContourIndex: integer]: integer read GetVertexCount
      write SetVertexCount;
    property Vertices[ContourIndex, VertexIndex: integer]: Tgpc_vertex
      read GetVertices write SetVertices;
    Constructor Create;
    Constructor CreateFromOperation(Operation: Tgpc_op;
      Subject, Clip: TGpcPolygonClass);
    Destructor Destroy; override;
    procedure ReadFromFile(FileName: string; ReadHoles: boolean);
    procedure WriteToFile(FileName: string; WriteHoles: boolean);
    function ContourArea(ContourIndex: integer): double;
    procedure AddContour(const contour: Tgpc_vertex_list; Hole: boolean);
    function TotalVertexCount: integer;
  end;

  TGpcTristripClass = class(TObject)
  private
    FTristrip: Tgpc_tristrip;
    function GetNumberOfStrips: integer;
    function GetNumberOfVerticies(StripIndex: integer): integer;
    function GetTristrips(StripIndex: integer): Tgpc_vertex_list;
    function GetVertices(StripIndex, VertexIndex: integer): Tgpc_vertex;
  public
    property NumberOfStrips: integer read GetNumberOfStrips;
    property Tristrips[StripIndex: integer]: Tgpc_vertex_list read GetTristrips;
    property NumberOfVerticies[StripIndex: integer]: integer
      read GetNumberOfVerticies;
    property Vertices[StripIndex, VertexIndex: integer]: Tgpc_vertex
      read GetVertices;
    Constructor Create(Subject: TGpcPolygonClass);
    Constructor CreateFromOperation(Subject, Clip: TGpcPolygonClass;
      Operation: Tgpc_op);
    Destructor Destroy; override;
  end;

implementation

resourcestring
  StrGpcMallocFailure = 'gpc malloc failure: %s';
  StrSDoesNotExist = '%s does not exist.';

procedure MALLOC(var p : pointer; b : integer; s : string);
begin
  GetMem(p, b);
  if (p = nil) and (b <> 0) then
  begin
    raise EGpcException.Create(Format(StrGpcMallocFailure, [s]));
  end;
end;
{ TGpcPolygonClass }

procedure TGpcPolygonClass.AddContour(const contour: Tgpc_vertex_list;
  Hole: boolean);
begin
  gpc_add_contour(@FPolygon, @contour, Ord(Hole));
end;

function TGpcPolygonClass.ContourArea(ContourIndex: integer): double;
var
  j: Integer;
  i: Integer;
  Contour: Tgpc_vertex_list;
begin
  // http://www.acm.org/jgt/papers/Sunday02/FastArea.html

  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  Result := 0.0;
  Contour := FPolygon.contour[ContourIndex];
  if Contour.num_vertices < 3 then Exit;

  j := Contour.num_vertices - 1;
  for i := 0 to Contour.num_vertices - 1 do
  begin
    Result := Result + ((Contour.vertex[j].x * Contour.vertex[i].y)
      - (Contour.vertex[j].y * Contour.vertex[i].x));
    j := i;
  end;
  Result := Result * 0.5;
end;

constructor TGpcPolygonClass.Create;
begin
  FPolygon.hole := nil;
  FPolygon.contour := nil;
  FPolygon.num_contours := 0;
end;

constructor TGpcPolygonClass.CreateFromOperation(Operation: Tgpc_op;
  Subject, Clip: TGpcPolygonClass);
begin
  Create;
  gpc_free_polygon(@FPolygon);
  gpc_polygon_clip(Operation, @Subject.FPolygon, @Clip.FPolygon, @FPolygon);
end;

destructor TGpcPolygonClass.Destroy;
begin
  gpc_free_polygon(@FPolygon);
  inherited;
end;

function TGpcPolygonClass.GetContours(ContourIndex: integer): Tgpc_vertex_list;
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  result := FPolygon.contour[ContourIndex];
end;

function TGpcPolygonClass.GetHoles(ContourIndex: integer): boolean;
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  result := FPolygon.hole[ContourIndex] = 1;
end;

function TGpcPolygonClass.GetNumberOfContours: integer;
begin
  result := FPolygon.num_contours;
end;

function TGpcPolygonClass.GetVertexCount(ContourIndex: integer): integer;
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  result := FPolygon.contour[ContourIndex].num_vertices;
end;

function TGpcPolygonClass.GetVertices(ContourIndex,
  VertexIndex: integer): Tgpc_vertex;
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  Assert((VertexIndex >= 0) and (VertexIndex < VertexCount[ContourIndex]));
  result := FPolygon.contour[ContourIndex].vertex[VertexIndex];
end;

procedure TGpcPolygonClass.ReadFromFile(FileName: string; ReadHoles: boolean);
var
  AFile: TextFile;
begin
  if FileExists(FileName) then
  begin
    AssignFile(AFile, FileName);
    try
      Reset(AFile);
      gpc_free_polygon(@FPolygon);
      gpc_read_polygon(AFile, Ord(ReadHoles), @FPolygon);
    finally
      CloseFile(AFile);
    end;
  end
  else
  begin
    raise EGpcException.Create(Format(StrSDoesNotExist, [FileName]));
  end;
end;

procedure TGpcPolygonClass.SetContours(ContourIndex: integer;
  const Value: Tgpc_vertex_list);
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  FPolygon.contour[ContourIndex] := Value;
end;

procedure TGpcPolygonClass.SetHoles(ContourIndex: integer;
  const Value: boolean);
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  FPolygon.hole[ContourIndex] := Ord(Value);
end;

procedure TGpcPolygonClass.SetNumberOfContours(const Value: integer);
var
  Index: Integer;
begin
  if NumberOfContours <> Value then
  begin
    if NumberOfContours <> 0 then
    begin
      gpc_free_polygon(@FPolygon);
    end;

    FPolygon.num_contours := Value;

    MALLOC(Pointer(FPolygon.hole), FPolygon.num_contours * sizeof(integer),
           'hole flag array creation');
    MALLOC(pointer(FPolygon.contour), FPolygon.num_contours * sizeof(Tgpc_vertex_list),
      'contour creation');

    for Index := 0 to FPolygon.num_contours - 1 do
    begin
      FPolygon.hole[Index] := 0;
      FPolygon.contour[Index].num_vertices := 0;
      FPolygon.contour[Index].vertex := nil;
    end;
  end;
end;

procedure TGpcPolygonClass.SetVertexCount(ContourIndex: integer;
  const Value: integer);
var
  Index: Integer;
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  if VertexCount[ContourIndex] <> Value then
  begin
    if VertexCount[ContourIndex] > 0 then
    begin
      FreeMem(FPolygon.contour[ContourIndex].vertex);
      FPolygon.contour[ContourIndex].vertex := nil;
    end;

    FPolygon.contour[ContourIndex].num_vertices := Value;

    MALLOC(pointer(FPolygon.contour[ContourIndex].vertex),
      FPolygon.contour[ContourIndex].num_vertices * sizeof(Tgpc_vertex),
      'vertex creation');
    for Index := 0 to FPolygon.contour[ContourIndex].num_vertices - 1 do
    begin
      FPolygon.contour[ContourIndex].vertex[Index].x := 0;
      FPolygon.contour[ContourIndex].vertex[Index].Y := 0;
    end;
  end;
end;

procedure TGpcPolygonClass.SetVertices(ContourIndex, VertexIndex: integer;
  const Value: Tgpc_vertex);
begin
  Assert((ContourIndex >= 0) and (ContourIndex < NumberOfContours));
  Assert((VertexIndex >= 0) and (VertexIndex < VertexCount[ContourIndex]));
  FPolygon.contour[ContourIndex].vertex[VertexIndex] := Value;
end;

function TGpcPolygonClass.TotalVertexCount: integer;
var
  Index: Integer;
begin
  result := 0;
  for Index := 0 to NumberOfContours - 1 do
  begin
    result := result + VertexCount[Index];
  end;
end;

procedure TGpcPolygonClass.WriteToFile(FileName: string; WriteHoles: boolean);
var
  AFile: TextFile;
begin
  AssignFile(AFile, FileName);
  try
    ReWrite(AFile);
    gpc_write_polygon(AFile, Ord(WriteHoles), @FPolygon);
  finally
    CloseFile(AFile);
  end;
end;

{ TGpcTristripClass }

constructor TGpcTristripClass.Create(Subject: TGpcPolygonClass);
begin
  FTristrip.strip := nil;
  FTristrip.num_strips := 0;
  gpc_polygon_to_tristrip(@Subject.FPolygon, @FTristrip);
end;

constructor TGpcTristripClass.CreateFromOperation(Subject,
  Clip: TGpcPolygonClass; Operation: Tgpc_op);
begin
  FTristrip.strip := nil;
  FTristrip.num_strips := 0;
  gpc_tristrip_clip(Operation, @Subject.FPolygon, @Clip.FPolygon, @FTristrip);
end;

destructor TGpcTristripClass.Destroy;
begin
  gpc_free_tristrip(@FTristrip);
  inherited;
end;

function TGpcTristripClass.GetNumberOfStrips: integer;
begin
  result := FTristrip.num_strips;
end;

function TGpcTristripClass.GetNumberOfVerticies(StripIndex: integer): integer;
begin
  Assert((StripIndex >= 0) and (StripIndex < NumberOfStrips));
  result := FTristrip.strip[StripIndex].num_vertices;
end;

function TGpcTristripClass.GetTristrips(StripIndex: integer): Tgpc_vertex_list;
begin
  Assert((StripIndex >= 0) and (StripIndex < NumberOfStrips));
  result := FTristrip.strip[StripIndex];
end;

function TGpcTristripClass.GetVertices(StripIndex,
  VertexIndex: integer): Tgpc_vertex;
begin
  Assert((StripIndex >= 0) and (StripIndex < NumberOfStrips));
  Assert((VertexIndex >= 0) and (VertexIndex < NumberOfVerticies[StripIndex]));
  result := FTristrip.strip[StripIndex].vertex[VertexIndex];
end;

end.
