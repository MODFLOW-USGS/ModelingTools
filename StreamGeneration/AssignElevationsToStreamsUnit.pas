unit AssignElevationsToStreamsUnit;

interface

uses
  SurferGridFileReaderUnit, ShapefileUnit, System.Classes, System.SysUtils,
  FastGEO;

type
  TProgressEvent = procedure(Sender: TObject; Position, Max: integer) of object;

  TElevationAssigner = class(TObject)
  private
    FInputShapeFile: string;
    FMeasuredValueGridFiles: TStrings;
    FElevationGridFiles: TStrings;
    FOutputShapeFile: string;
    FShapeReader: TShapefileGeometryReader;
    FElevationFiles: TSurferRaster7File2ObjectList;
    FMeasuredValueFiles: TSurferRaster7File2ObjectList;
    FOutputShapeFiles: TShapefileGeometryWriter;
    FCachedElevGrid: TSurferRaster7File2;
    FCachedMeasValueGrid: TSurferRaster7File2;
    FOnProgress: TProgressEvent;
    procedure SetInputShapeFile(const Value: string);
    procedure SetElevationGridFiles(const Value: TStrings);
    procedure SetMeasuredValueGridFiles(const Value: TStrings);
    procedure SetOutputShapeFile(const Value: string);
    procedure AssignElevations;
    function LocateElevationGrid(Point: TShapePoint): TSurferRaster7File2;
    function LocateMeasuredValueGrid(Point: TShapePoint): TSurferRaster7File2;
    function InGrid(Grid: TSurferRaster7File2; Point: TShapePoint): Boolean;
    function GridValue(Grid: TSurferRaster7File2; Point: TShapePoint): double;
  public
    constructor Create;
    destructor Destroy; override;
    property InputShapeFile: string read FInputShapeFile write SetInputShapeFile;
    property OutputShapeFile: string read FOutputShapeFile write SetOutputShapeFile;
    property ElevationGridFiles: TStrings read FElevationGridFiles write SetElevationGridFiles;
    property MeasuredValueGridFiles: TStrings read FMeasuredValueGridFiles write SetMeasuredValueGridFiles;
    procedure PerformConversion;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TElevationAssigner }

procedure TElevationAssigner.AssignElevations;
var
  ShapeIndex: Integer;
  AShape: TShapeObject;
  PointIndex: Integer;
  APoint: TShapePoint;
  AGrid: TSurferRaster7File2;
begin
  for ShapeIndex := 0 to FShapeReader.Count - 1 do
  begin
    AShape := FShapeReader[ShapeIndex];
    AShape.FShapeType := stPolyLineZ;
    SetLength(AShape.FMArray, AShape.FNumPoints);
    SetLength(AShape.FZArray, AShape.FNumPoints);
    for PointIndex := 0 to AShape.FNumPoints - 1 do
    begin
      APoint := AShape.FPoints[PointIndex];
      AGrid := LocateElevationGrid(APoint);
      if AGrid <> nil then
      begin
        AShape.FZArray[PointIndex] := GridValue(AGrid, APoint)
      end
      else
      begin
        AShape.FZArray[PointIndex] := 0;
      end;
      AGrid := LocateMeasuredValueGrid(APoint);
      if AGrid <> nil then
      begin
        AShape.FMArray[PointIndex] := GridValue(AGrid, APoint)
      end
      else
      begin
        AShape.FMArray[PointIndex] := 0;
      end;
    end;
    FOutputShapeFiles.AddShape(AShape);
    if Assigned(FOnProgress) then
    begin
      FOnProgress(Self, ShapeIndex+1, FShapeReader.Count);
    end;
  end;
end;

constructor TElevationAssigner.Create;
begin
  inherited;
  FMeasuredValueGridFiles := TStringList.Create;
  FElevationGridFiles := TStringList.Create;
end;

destructor TElevationAssigner.Destroy;
begin
  FMeasuredValueGridFiles.Free;
  FElevationGridFiles.Free;
  inherited;
end;

function TElevationAssigner.GridValue(Grid: TSurferRaster7File2;
  Point: TShapePoint): double;
var
  XIndex: integer;
  YIndex: Integer;
begin
  XIndex := Trunc((Point.X - Grid.LowerLeft.x)/Grid.XSpacing);
  YIndex := Trunc((Point.Y - Grid.LowerLeft.Y)/Grid.YSpacing);
  result := Grid.Z[XIndex, YIndex]
end;

function TElevationAssigner.InGrid(Grid: TSurferRaster7File2;
  Point: TShapePoint): Boolean;
var
  LowerLeft: TPoint2D;
  UpperRight: TPoint2D;
begin
  LowerLeft := Grid.LowerLeft;
  UpperRight.x := LowerLeft.x + Grid.XCount * Grid.XSpacing;
  Result := (LowerLeft.x <= Point.X) and (Point.X <= UpperRight.x);
  if result then
  begin
    UpperRight.y := LowerLeft.y + Grid.YCount * Grid.YSpacing;
    Result := (LowerLeft.y <= Point.y) and (Point.y <= UpperRight.y);
  end;
end;

function TElevationAssigner.LocateElevationGrid(Point: TShapePoint): TSurferRaster7File2;
var
  GridIndex: Integer;
  AGrid: TSurferRaster7File2;
begin
  Result := nil;
  if (FCachedElevGrid <> nil) and InGrid(FCachedElevGrid, Point) then
  begin
    Result := FCachedElevGrid;
  end
  else
  begin
    for GridIndex := 0 to FElevationFiles.Count - 1 do
    begin
      AGrid := FElevationFiles[GridIndex];
      if InGrid(AGrid, Point) then
      begin
        Result := AGrid;
        FCachedElevGrid := AGrid;
        Exit;
      end;
    end;
  end;
end;

function TElevationAssigner.LocateMeasuredValueGrid(
  Point: TShapePoint): TSurferRaster7File2;
var
  GridIndex: Integer;
  AGrid: TSurferRaster7File2;
begin
  Result := nil;
  if (FCachedMeasValueGrid <> nil) and InGrid(FCachedMeasValueGrid, Point) then
  begin
    Result := FCachedMeasValueGrid;
  end
  else
  begin
    for GridIndex := 0 to FMeasuredValueFiles.Count - 1 do
    begin
      AGrid := FMeasuredValueFiles[GridIndex];
      if InGrid(AGrid, Point) then
      begin
        Result := AGrid;
        FCachedMeasValueGrid := AGrid;
        Exit;
      end;
    end;
  end;
end;

procedure TElevationAssigner.PerformConversion;
var
  FileIndex: Integer;
begin
  FShapeReader := TShapefileGeometryReader.Create;
  try
    FShapeReader.ReadFromFile(InputShapeFile,
      ChangeFileExt(InputShapeFile, '.shx'));

    if Assigned(FOnProgress) then
    begin
      FOnProgress(Self, 0, FShapeReader.Count);
    end;

    FElevationFiles := TSurferRaster7File2ObjectList.Create;
    FMeasuredValueFiles := TSurferRaster7File2ObjectList.Create;
    try
      for FileIndex := 0 to ElevationGridFiles.Count - 1 do
      begin
        FElevationFiles.Add(TSurferRaster7File2.Create(ElevationGridFiles[FileIndex]));
      end;
      for FileIndex := 0 to MeasuredValueGridFiles.Count - 1 do
      begin
        FMeasuredValueFiles.Add(TSurferRaster7File2.Create(MeasuredValueGridFiles[FileIndex]));
      end;

      FOutputShapeFiles := TShapefileGeometryWriter.Create(stPolyLineZ, False);
      try
        AssignElevations;

        FOutputShapeFiles.WriteToFile(OutputShapeFile,
          ChangeFileExt(OutputShapeFile, '.shx'));
      finally
        FreeAndNil(FOutputShapeFiles);
      end;

    finally
      FreeAndNil(FElevationFiles);
      FreeAndNil(FMeasuredValueFiles);
    end;
  finally
    FreeAndNil(FShapeReader);
  end;
end;

procedure TElevationAssigner.SetElevationGridFiles(const Value: TStrings);
begin
  FElevationGridFiles.Assign(Value);
end;

procedure TElevationAssigner.SetInputShapeFile(const Value: string);
begin
  FInputShapeFile := Value;
end;

procedure TElevationAssigner.SetMeasuredValueGridFiles(const Value: TStrings);
begin
  FMeasuredValueGridFiles.Assign(Value);
end;

procedure TElevationAssigner.SetOutputShapeFile(const Value: string);
begin
  FOutputShapeFile := Value;
end;

end.
